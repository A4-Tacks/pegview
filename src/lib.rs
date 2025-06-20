use std::{
    borrow::Cow,
    collections::BTreeSet,
    fmt::{Debug, Display},
    iter::{once, repeat, repeat_n},
    mem::take,
    ops::{BitAndAssign, BitOrAssign, ControlFlow},
    str::FromStr,
    sync::atomic::{AtomicBool, Ordering::*},
};

use itermaps::short_funcs::{default, into};
use unicode_width::{UnicodeWidthChar, UnicodeWidthStr};

/// simple checked try into
pub trait CInto {
    #[track_caller]
    fn cinto<U>(self) -> U
    where Self: TryInto<U>,
          Self::Error: Debug,
    {
        self.try_into().unwrap()
    }
}
impl<T> CInto for T {}

trait IterExt: Iterator + Sized {
    /// 寻找最后一个符合条件的
    fn find_last<P>(mut self, mut predicate: P) -> Option<Self::Item>
    where P: FnMut(&Self::Item) -> bool,
    {
        let result =
            self.try_fold(None, |acc, ele|
        {
            if predicate(&ele) {
                ControlFlow::Continue(Some(ele))
            } else {
                ControlFlow::Break(acc)
            }
        });
        match result {
            ControlFlow::Continue(x)
            | ControlFlow::Break(x)
            => x,
        }
    }
}
impl<T: Iterator> IterExt for T { }

trait AssignAble {
    type Item;
    fn assign_able(&mut self, index: usize) -> &mut Self::Item;
}
impl<T: Default> AssignAble for Vec<T> {
    type Item = T;

    fn assign_able(&mut self, index: usize) -> &mut T {
        self.extend((self.len()..=index).map(|_| default()));
        debug_assert!(self.len() >= index);
        &mut self[index]
    }
}

macro_rules! do_op {
    ($self:ident, $oth:ident $oper:tt $($field:tt)+) => {
        $(
            $self.$field $oper $oth.$field;
        )+
    };
}

pub static CENTER_NAME: AtomicBool = AtomicBool::new(false);
pub static FULL_WIDTH_TAB: AtomicBool = AtomicBool::new(false);
pub static UNQUOTE_SPACE: AtomicBool = AtomicBool::new(false);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Loc {
    /// by zero is index mode
    pub line: u32,
    pub column: u32,
}
impl Default for Loc {
    fn default() -> Self {
        Self::new(1, 1)
    }
}
impl Display for Loc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.line == 0 {
            write!(f, "#{}", self.column)
        } else {
            write!(f, "{}:{}", self.line, self.column)
        }
    }
}
impl Loc {
    pub fn new(line: u32, column: u32) -> Self {
        assert_ne!(line, 0);
        assert_ne!(column, 0);
        Self { line, column }
    }

    pub fn is_zero(&self) -> bool {
        matches!(self,
            | Self { line: 0, column: 0 }
            | Self { line: 1, column: 1 })
    }

    /// New char index location
    pub fn new_index(index: u32) -> Self {
        Self { line: 0, column: index }
    }

    #[track_caller]
    pub fn get_char_index(&self, src: &str) -> usize {
        if self.line == 0 {
            return self.column.cinto();
        }
        line_column::char_index(src, self.line, self.column)
    }

    #[track_caller]
    pub fn to_char_index(&mut self, src: &str) {
        self.column = self.get_char_index(src).cinto();
        self.line = 0;
    }

    pub fn oneline_col(&self) -> Option<u32> {
        match self.line {
            0 => Some(self.column+1),
            1 => Some(self.column),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Default)]
pub struct Side {
    up: bool,
    down: bool,
    left: bool,
    right: bool,
}
impl BitAndAssign for Side {
    fn bitand_assign(&mut self, rhs: Self) {
        do_op!(self, rhs &= up down left right);
    }
}
impl BitOrAssign for Side {
    fn bitor_assign(&mut self, rhs: Self) {
        do_op!(self, rhs |= up down left right);
    }
}
impl Side {
    pub const fn new(up: bool, down: bool, left: bool, right: bool) -> Self {
        Self { up, down, left, right }
    }

    pub const EMPTY: Self = Self::bit_new(0b0000);
    pub const JOINT: Self = Self::bit_new(0b0011);
    pub const EXT: Self = Self::bit_new(0b1100);
    pub const LD: Self = Self::bit_new(0b0110);
    pub const RD: Self = Self::bit_new(0b0101);

    /// high -> low: up, down, left, right
    pub const fn bit_new(n: u8) -> Self {
        Self {
            up:     n>>3&1 != 0,
            down:   n>>2&1 != 0,
            left:   n>>1&1 != 0,
            right:  n&1 != 0,
        }
    }

    pub fn get(&self) -> Option<char> {
        match (self.down, self.up, self.left, self.right) {
            (false, false, false, false) => None?,
            (true, true, true, true)   => '┼',
            (true, true, true, false)   => '┤',
            (true, true, false, true)  => '├',
            (_, _, false, false)        => '│',
            (true, false, true, false) => '┘',
            (true, false, false, true)  => '└',
            (false, true, true, false) => '┐',
            (false, true, false, true)  => '┌',
            (false, false, _, _)       => '─',
            (false, true, true, true)   => '┬',
            (true, false, true, true)  => '┴',
        }.into()
    }

    pub fn is_empty(&self) -> bool {
        self.get().is_none()
    }

    pub fn width(&self) -> usize {
        let full = FULL_WIDTH_TAB.load(Acquire);
        let basic = full as usize + 1;
        usize::from(!self.is_empty()) * basic
    }

    pub fn concat(&mut self, parent: &Self) {
        if parent.down {
            self.up = true;
            self.down = true;
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Default)]
pub struct Sides(pub Side, pub Side);
impl BitOrAssign for Sides {
    fn bitor_assign(&mut self, rhs: Self) {
        do_op!(self, rhs |= 0 1);
    }
}
impl BitAndAssign for Sides {
    fn bitand_assign(&mut self, rhs: Self) {
        do_op!(self, rhs &= 0 1);
    }
}
impl Sides {
    const EMPTY: Self = Self(Side::EMPTY, Side::EMPTY);

    /// high -> low: left, right
    pub const fn bit_new(n: u8) -> Self {
        Self(Side::bit_new(n>>4), Side::bit_new(n))
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty() && self.1.is_empty()
    }

    pub fn width(&self) -> usize {
        self.0.width() + self.1.width()
    }

    pub fn sides(&self) -> (Option<char>, Option<char>) {
        (self.0.get(), self.1.get())
    }

    pub fn to_hang(self) -> Self {
        let f = |x: Side| if x.down {
            Side::EXT
        } else {
            Side::EMPTY
        };
        Self(f(self.0), f(self.1))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Attr {
    pub zero_width: bool,
    pub cached_match: bool,
    pub cached_fail: bool,
}
impl Attr {
    pub fn width(&self) -> usize {
        [
            self.zero_width,
            self.cached_match,
            self.cached_fail,
        ].into_iter().map(into::<_, usize>).sum()
    }
    pub fn is_empty(&self) -> bool {
        self.width() == 0
    }
}
impl Display for Attr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.zero_width { write!(f, "*")?; }
        if self.cached_match { write!(f, "&")?; }
        if self.cached_fail { write!(f, "!")?; }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Entry<'a> {
    Str(&'a str, Attr),
    Char(DChar),
}
impl<'a> Entry<'a> {
    /// No attribute str
    pub fn str(s: &'a str) -> Self {
        Self::Str(s, default())
    }

    pub fn is_empty(&self) -> bool {
        match self {
            Entry::Str(s, attr) => s.is_empty() && attr.is_empty(),
            Entry::Char(_) => false,
        }
    }

    pub fn width(&self) -> usize {
        match self {
            Entry::Str(s, attr) => s.width() + attr.width(),
            Entry::Char(dchar) => dchar.width(),
        }
    }

    /// Returns `true` if the entry is [`Char`].
    ///
    /// [`Char`]: Entry::Char
    #[must_use]
    pub fn is_char(&self) -> bool {
        matches!(self, Self::Char(..))
    }

    /// Returns `true` if the entry is [`Str`].
    ///
    /// [`Str`]: Entry::Str
    #[must_use]
    pub fn is_str(&self) -> bool {
        matches!(self, Self::Str(..))
    }
}
impl Display for Entry<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Entry::Str(s, attr) => write!(f, "{s}{attr}"),
            Entry::Char(dchar) => Display::fmt(dchar, f),
        }
    }
}
impl Default for Entry<'_> {
    fn default() -> Self {
        Self::Str("", default())
    }
}
impl From<char> for Entry<'_> {
    fn from(v: char) -> Self {
        Self::Char(v.into())
    }
}
impl From<DChar> for Entry<'_> {
    fn from(v: DChar) -> Self {
        Self::Char(v)
    }
}
impl<'a> From<&'a str> for Entry<'a> {
    fn from(v: &'a str) -> Self {
        Self::Str(v, default())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Elem<'a> {
    left: Entry<'a>,
    right: Entry<'a>,
    sides: Sides,
    fill: char,
    exts: usize,
}
impl<'a> Elem<'a> {
    const JOINT_CH: char = '─';

    fn joint_ch() -> char {
        if FULL_WIDTH_TAB.load(Acquire) {
            '-'
        } else {
            Self::JOINT_CH
        }
    }

    pub fn new_region(
        left: impl Into<Entry<'a>>,
        right: impl Into<Entry<'a>>,
        sides: Sides,
    ) -> Self {
        Self {
            left: left.into(),
            right: right.into(),
            sides,
            fill: Self::joint_ch(),
            exts: 0,
        }
    }
    pub fn new(
        left: impl Into<Entry<'a>>,
        right: impl Into<Entry<'a>>,
        sides: Sides,
        fill: char,
    ) -> Self {
        Self {
            left: left.into(),
            right: right.into(),
            sides,
            fill,
            exts: 0,
        }
    }
    pub fn new_left(s: impl Into<Entry<'a>>) -> Self {
        Self {
            left: s.into(),
            right: "".into(),
            sides: Sides::EMPTY,
            fill: ' ',
            exts: 0,
        }
    }
    pub fn new_joint(
        left: impl Into<Entry<'a>>,
        right: impl Into<Entry<'a>>,
    ) -> Self {
        Self::new_region(left, right, Sides::bit_new(0b0101_0110))
    }
    pub fn new_fill() -> Self {
        Self::new_left("")
    }
    pub fn new_fill_hang(sides: Sides) -> Self {
        debug_assert_ne!(sides, Sides::EMPTY);
        Self { sides, ..Self::new_fill() }
    }

    pub fn split_iter(mut self, cols: usize) -> impl Iterator<Item = Self> {
        assert_ne!(cols, 0);
        let exts = cols-1;
        self.exts = exts;
        let mut tail = Self::new_fill();
        if exts != 0 { tail.sides.1 = take(&mut self.sides.1) }
        once(self).chain((exts != 0).then(|| {
            repeat_n(Self::new_fill(), exts-1)
                .chain(once(tail))
        }).into_iter().flatten())
    }

    pub fn width(&self) -> usize {
        self.sides.width() + self.left.width() + self.right.width()
    }

    /// result: skip cols
    pub fn output(
        &self,
        widths: &[usize],
        tail: &Elem<'a>,
        share_width: &ShareWidth,
    ) -> usize {
        fn center_prefix(fill: char, fillc: &mut usize) {
            let pre_fillc = *fillc >> 1;
            *fillc -= pre_fillc;
            for _ in 0..pre_fillc { print!("{}", fill) }
        }

        let col_width = widths[..=self.exts].iter().sum::<usize>();
        let mut fillc = col_width - self.width();
        let (ls, _) = self.sides.sides();
        let (_, rs) = tail.sides.sides();
        if self.exts != 0 { fillc -= tail.width() }

        if let Some(ls) = ls { print!("{ls}") }
        if self.right.is_empty() {
            if self.left.is_char() {
                // src mode
                match share_width {
                    ShareWidth::Mixed => center_prefix(self.fill, &mut fillc),
                    ShareWidth::First => {
                        for _ in 0..fillc { print!("{}", self.fill) }
                        fillc = 0;
                    },
                    ShareWidth::Last => (),
                }
            } else if CENTER_NAME.load(Acquire) {
                // one side output, center mode
                center_prefix(self.fill, &mut fillc);
            }
        }

        print!("{}", self.left);

        for _ in 0..fillc { print!("{}", self.fill) }

        print!("{}", self.right);
        if let Some(rs) = rs { print!("{rs}") }

        self.exts
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub enum ShareWidth {
    Mixed,
    First,
    Last,
}
impl Default for ShareWidth {
    fn default() -> Self {
        Self::Mixed
    }
}
impl FromStr for ShareWidth {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "mixed" => Ok(Self::Mixed),
            "first" => Ok(Self::First),
            "last" => Ok(Self::Last),
            e => Err(format!("invalid share width style: {e:?}")),
        }
    }
}
impl ShareWidth {
    fn run_mixed(span: &mut [usize], rem_width: usize) {
        // 较为均匀的将可扩展元素的长度匀入多列中
        let cols = span.len();
        let point = rem_width % cols;
        let base_width = rem_width / cols;
        for col_width in &mut span[..point] {
            *col_width += base_width+1;
        }
        for col_width in &mut span[point..] {
            *col_width += base_width;
        }
    }

    fn run_first(span: &mut [usize], rem_width: usize) {
        span[0] += rem_width;
    }

    fn run_last(span: &mut [usize], rem_width: usize) {
        *span.last_mut().unwrap() += rem_width;
    }

    /// extendeds: `&[(i, cols, width)]`
    fn run(&self,
        widths: &mut [usize],
        extendeds: &[(usize, usize, usize)],
    ) {
        for &(i, cols, width) in extendeds {
            let span = &mut widths[i..][..cols];
            let orig_width = span.iter().sum::<usize>();
            if width <= orig_width { continue }
            let rem_width = width - orig_width;

            assert_eq!(span.len(), cols);
            debug_assert_ne!(cols, 0);

            match self {
                ShareWidth::Mixed => Self::run_mixed(span, rem_width),
                ShareWidth::First => Self::run_first(span, rem_width),
                ShareWidth::Last => Self::run_last(span, rem_width),
            }
        }
    }
}

#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub struct Config {
    pub uniq: bool,
    pub share_width: ShareWidth,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ColLine<'a> {
    lines: Vec<Vec<Option<Elem<'a>>>>,
    cfg: Config,
}
impl Default for ColLine<'_> {
    fn default() -> Self {
        Self::new(default())
    }
}

impl<'a> ColLine<'a> {
    pub fn new(cfg: Config) -> Self {
        Self {
            lines: vec![vec![]],
            cfg,
        }
    }

    pub fn push_solid(&mut self, elem: Elem<'a>, coli: u32, cols: u32) {
        let uniq = take(&mut self.cfg.uniq);
        self.push(elem, coli, cols);
        self.cfg.uniq = uniq;
    }

    pub fn push(&mut self, elem: Elem<'a>, coli: u32, cols: u32) {
        assert_ne!(cols, 0);
        let coli = coli.cinto::<usize>();
        let cols = cols.cinto::<usize>();
        // 预留
        if !self.lines.last().unwrap().is_empty() {
            self.lines.push(vec![]);
        }

        let end = coli + cols;
        let line = self.lines.iter_mut().rev()
            .find_last(|line|
        {
            line.is_empty()
                || !self.cfg.uniq
                && line.iter().skip(coli).take(cols).all(Option::is_none)
        }).unwrap();
        line.assign_able(end-1);
        let iter
            = elem.split_iter(cols).map(Some);
        line.splice(coli..end, iter);
    }

    pub fn max_widths(&self) -> Vec<usize> {
        let mut skips = BTreeSet::new();
        let mut extendeds = vec![];
        let mut widths: Vec<usize> = (0..self.max_col())
            .map(|col| self.lines.iter()
                .enumerate()
                .map(|(i, line)| line.get(col)
                    .and_then(Option::as_ref)
                    .filter(|_| !skips.contains(&(i, col)))
                    .filter(|elem| if elem.exts != 0 {
                        let tail = line[col+elem.exts].as_ref().unwrap();
                        extendeds.push((
                                col,
                                elem.exts+1,
                                elem.width() + tail.width(),
                        ));

                        skips.extend(repeat(i)
                            .zip(col..)
                            .take(elem.exts+1));
                        false
                    } else { true })
                    .map_or(0, Elem::width))
                .max()
                .unwrap_or_default())
            .collect();
        self.cfg.share_width.run(&mut widths, &extendeds);
        widths
    }

    pub fn max_col(&self) -> usize {
        self.lines.iter()
            .map(Vec::len)
            .max()
            .unwrap_or_default()
    }

    pub fn fill_hangs(&mut self) {
        self.lines.iter_mut().rev().reduce(|up, down| {
            // 如果下面的是空的, 那么把上面的sides复制到下面
            for (i, elem) in up.iter().enumerate() {
                let Some(elem) = elem else { continue };
                if elem.sides.is_empty()
                || down.get(i)
                    .and_then(Option::as_ref)
                    .is_some()
                { continue }

                down.assign_able(i);
                down[i] = Elem::new_fill_hang(elem.sides.to_hang()).into();
            }
            down
        });
    }

    pub fn concat_hangs(&mut self) {
        self.lines.iter_mut().skip(1).rev()
            .reduce(|up, down|
        {
            // 连接上下线条
            let mut skips = 0;
            up.iter_mut()
                .zip(&mut *down)
                .filter_map(|(up, down)| {
                    if skips != 0 { skips -= 1; return None; }
                    let down = down.as_mut()?;
                    skips = down.exts.saturating_sub(1);
                    let up = up.as_mut()?;
                    Some((up, down))
                })
                .for_each(|(up, down)| {
                    if down.sides.0.down {
                        down.sides.0.concat(&up.sides.0);
                    }
                    if up.exts == 0 && down.exts == 0 {
                        down.sides.1.concat(&up.sides.1);
                    }
                });
            down
        });
    }

    pub fn output(&self) {
        let widths = self.max_widths();
        let fill = Elem::new_fill();

        self.lines.iter().for_each(|line| {
            let mut skips = 0;
            for (i, elem) in line.iter().enumerate() {
                if skips != 0 { skips -= 1; continue }
                let elem = elem.as_ref().unwrap_or(&fill);

                let tail = line[i+elem.exts].as_ref();
                if tail.is_none() { debug_assert_eq!(elem.exts, 0); }
                skips = elem.output(
                    &widths[i..],
                    tail.unwrap_or(elem),
                    &self.cfg.share_width,
                );
            }
            println!();
        });
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct DChar(pub char);
impl From<char> for DChar {
    fn from(value: char) -> Self {
        Self(value)
    }
}
impl DChar {
    pub fn width(&self) -> usize {
        match self.0 {
            '\'' => 4,
            ' ' if UNQUOTE_SPACE.load(Acquire) => 1,
            ' ' => 3,
            ch if ch.is_control() => {
                ch.escape_debug().count()+2
            },
            ch => {
                ch.width().unwrap_or(1)
            },
        }
    }
}
impl Display for DChar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let &Self(ch) = self;
        let printable = (ch != ' ' || UNQUOTE_SPACE.load(Acquire))
            && ch != '\''
            && !ch.is_control();

        if printable {
            write!(f, "{ch}")
        } else {
            write!(f, "{ch:?}")
        }
    }
}

pub struct Repeat<'a>(pub &'a str, pub u32);
impl Display for Repeat<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for _ in 0..self.1 {
            f.write_str(self.0)?;
        }
        Ok(())
    }
}

/// 仅保留成功树的解析路径
pub fn filter_fails<'a, I>(iter: I) -> Vec<Action<'a>>
where I: IntoIterator<Item = Action<'a>>,
{
    let iter = iter.into_iter();
    let capacity = iter.size_hint().0 >> 2;
    let mut first = Vec::with_capacity(capacity);
    let rest = &mut Vec::with_capacity(capacity);
    for action in iter {
        match action {
            Action::Attempting { .. }
            | Action::Matched { .. }
            | Action::CachedMatch { .. }
            | Action::CachedFail { .. }
            | Action::Entering { .. }
            | Action::Leaving { .. }
            => {
                rest.push(action)
            },
            Action::Failed { .. } => {
                let mut count = 1usize;
                while count != 0 {
                    match rest.pop().unwrap() {
                        Action::Attempting { .. }
                        | Action::Entering { .. }
                        => {
                            count -= 1;
                        },
                        Action::Matched { .. }
                        | Action::Leaving { .. }
                        => {
                            count += 1;
                        },
                        Action::Failed { .. } => {
                            unreachable!("pop fails unexpected fail: {action}")
                        },
                        Action::CachedMatch { .. }
                        | Action::CachedFail { .. }
                        => (),
                        Action::Begin { .. }
                        | Action::End
                        | Action::Other { .. }
                        => {
                            unreachable!("pop fails unexpected: {action}")
                        },
                    }
                }
            },
            Action::Begin { .. }
            | Action::End
            | Action::Other { .. }
            => first.push(action),
        }
    }
    first.append(rest);
    first
}

pub fn quiet_rules(map: &BTreeSet<String>, actions: &mut Vec<Action<'_>>) {
    let mut level = 0usize;
    actions.retain(|action| {
        let old_quiet_stat = level == 0;
        match action {
            Action::Attempting { name, .. }
            => if map.contains(*name)
            {
                level += 1;
            },
            Action::Matched { name, .. }
            | Action::Failed { name, .. }
            => if map.contains(*name)
            {
                level -= 1;
                return level == 0;
            },
            | Action::CachedMatch { .. }
            | Action::CachedFail { .. }
            | Action::Entering { .. }
            | Action::Leaving { .. }
            | Action::Begin { .. }
            | Action::End
            | Action::Other { .. } => (),
        }
        old_quiet_stat
    });
}

pub fn pair_fails(actions: &mut Vec<Action<'_>>) {
    let mut attempted = vec![];
    for action in &mut *actions {
        match *action {
            Action::Attempting { name, ref start } => {
                let start = start.clone();
                attempted.push(Action::Failed { name, start });
            },
            Action::Matched  { name, ref start, .. }
            | Action::Failed { name, ref start }
            => {
                let start = start.clone();
                let poped = attempted.pop();
                assert_eq!(poped, Some(Action::Failed { name, start }),
                           "pair_fails pop attempted no match");
            },
            _ => {},
        }
    }
    actions.extend(attempted.into_iter().rev());
}

pub fn split_regions<'a, I>(iter: I) -> Vec<Vec<Action<'a>>>
where I: IntoIterator<Item = Action<'a>>,
{
    let stack = &mut vec![vec![]];
    let mut regions = vec![];
    let others = &mut vec![];

    for action in iter {
        if !action.is_other() && !others.is_empty() {
            regions.push(take(others));
        }
        match action {
            Action::Attempting { .. }
            | Action::Matched { .. }
            | Action::Failed { .. }
            | Action::Entering { .. }
            | Action::Leaving { .. }
            | Action::CachedMatch { .. }
            | Action::CachedFail { .. }
            => stack.last_mut().unwrap().push(action),
            Action::Begin { .. } => stack.push(vec![action]),
            Action::End => regions.extend(stack.pop()),
            Action::Other { .. } => others.push(action),
        }
    }

    if !others.is_empty() {
        regions.push(take(others));
    }

    while let Some(top) = stack.pop() {
        if !top.is_empty() {
            regions.push(top);
        }
    }

    regions
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Action<'a> {
    Attempting {
        name: &'a str,
        start: Loc,
    },
    Matched {
        name: &'a str,
        start: Loc,
        stop: Loc,
    },
    Failed {
        name: &'a str,
        start: Loc,
    },
    CachedMatch {
        name: &'a str,
        start: Loc,
    },
    CachedFail {
        name: &'a str,
        start: Loc,
    },
    Entering {
        level: u32,
    },
    Leaving {
        level: u32,
    },
    Begin {
        source: Cow<'a, str>,
        from: Loc,
    },
    End,
    Other {
        text: &'a str,
    },
}
impl Action<'_> {
    pub fn locs(&self) -> Option<(Loc, Option<Loc>)> {
        Some(match self {
            Action::Matched { start, stop, .. }
            => (start.clone(), stop.clone().into()),
            Action::Attempting { start, .. }
            | Action::Failed { start, .. }
            | Action::CachedMatch { start, .. }
            | Action::CachedFail { start, .. }
            => (start.clone(), None),

            | Action::Entering { .. }
            | Action::Leaving { .. }
            | Action::Begin { .. }
            | Action::End
            | Action::Other { .. }
            => None?,
        })
    }

    /// Returns `true` if the action is [`Attempting`].
    ///
    /// [`Attempting`]: Action::Attempting
    #[must_use]
    pub fn is_attempting(&self) -> bool {
        matches!(self, Self::Attempting { .. })
    }

    /// Returns `true` if the action is [`Matched`].
    ///
    /// [`Matched`]: Action::Matched
    #[must_use]
    pub fn is_matched(&self) -> bool {
        matches!(self, Self::Matched { .. })
    }

    /// Returns `true` if the action is [`Failed`].
    ///
    /// [`Failed`]: Action::Failed
    #[must_use]
    pub fn is_failed(&self) -> bool {
        matches!(self, Self::Failed { .. })
    }

    /// Returns `true` if the action is [`Begin`].
    ///
    /// [`Begin`]: Action::Begin
    #[must_use]
    pub fn is_begin(&self) -> bool {
        matches!(self, Self::Begin { .. })
    }

    /// Returns `true` if the action is [`End`].
    ///
    /// [`End`]: Action::End
    #[must_use]
    pub fn is_end(&self) -> bool {
        matches!(self, Self::End)
    }

    /// Returns `true` if the action is [`Other`].
    ///
    /// [`Other`]: Action::Other
    #[must_use]
    pub fn is_other(&self) -> bool {
        matches!(self, Self::Other { .. })
    }

    #[must_use]
    pub fn is_tracing(&self) -> bool {
        matches!(self,
            | Self::Matched { .. }
            | Self::Failed { .. }
            | Self::CachedMatch { .. }
            | Self::CachedFail { .. }
            | Self::Entering { .. }
            | Self::Leaving { .. })
    }

    pub fn as_begin(&self) -> Option<(&str, &Loc)> {
        if let Self::Begin { source, from } = self {
            Some((source, from))
        } else {
            None
        }
    }
}

peg::parser!(pub grammar parser() for str {
    rule _() = [' ' | '\t']+
    rule nl() = quiet!{"\n" / "\r\n"} / expected!("newline")
    rule num() -> u32
        = quiet!{s:$(['0'..='9']+) { s.parse().unwrap() }}
        / expected!("num")
    rule rule_name() -> &'input str
        = "`" s:$((!_ [^'`' | '\r' | '\n'])+) "`" { s }
        / $([^'`' | ' ' | '\t' | '\r' | '\n']+)
    rule loc() -> Loc
        = "#" index:num()               { Loc::new_index(index) }
        / line:num() ":" column:num()   { Loc { line, column } }

    rule peg_trace() -> Action<'input>
        = "Attempting to match rule"
            _ name:rule_name()
            _ "at"
            _ start:loc()
            { Action::Attempting { name, start } }
        / "Matched rule"
            _ name:rule_name()
            _ "at"
            _ start:loc()
            _ "to"
            _ stop:loc()
            { Action::Matched { name, start, stop } }
        / "Failed to match rule"
            _ name:rule_name()
            _ "at"
            _ start:loc()
            { Action::Failed { name, start } }
        / "Cached match of rule"
            _ name:rule_name()
            _ "at"
            _ start:loc()
            { Action::CachedMatch { name, start } }
        / "Cached fail of rule"
            _ name:rule_name()
            _ "at"
            _ start:loc()
            { Action::CachedFail { name, start } }
        / "Entering level"
            _ level:num()
            { Action::Entering { level } }
        / "Leaving level"
            _ level:num()
            { Action::Leaving { level } }
    pub
    rule content() -> Action<'input>
        = "[PEG_TRACE]" _ t:peg_trace()
            { t }
        / "[PEG_INPUT_START]" from:(_? "from" _
                n:(loc() / n:num() {Loc::new_index(n)}) {n}
            )? nl()
            source:$( (!"[PEG_TRACE_START]" [^'\r' | '\n']*)**nl() )
            nl()?
            "[PEG_TRACE_START]"
            { Action::Begin { source: source.into(), from: from.unwrap_or_default() } }
        / "[PEG_TRACE_STOP]"
            { Action::End }
        / !("[PEG_TRACE]" / "[PEG_INPUT_START]" / "[PEG_TRACE_STOP]" / ![_])
            text:$([^'\r' | '\n']*)
            { Action::Other { text } }
    pub
    rule lines() -> Vec<Action<'input>>
        = x:content()**nl() nl()? {x}
});
impl Display for Action<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Action::Attempting { name, start } => {
                write!(f, "[PEG_TRACE] ")?;
                write!(f, "Attempting to match rule `{name}` at {start}")
            },
            Action::Matched { name, start, stop } => {
                write!(f, "[PEG_TRACE] ")?;
                write!(f, "Matched rule `{name}` at {start} to {stop}")
            },
            Action::Failed { name, start } => {
                write!(f, "[PEG_TRACE] ")?;
                write!(f, "Failed to match rule `{name}` at {start}")
            },
            Action::CachedMatch { name, start } => {
                write!(f, "[PEG_TRACE] ")?;
                write!(f, "Cached match of rule `{name}` at {start}")
            },
            Action::CachedFail { name, start } => {
                write!(f, "[PEG_TRACE] ")?;
                write!(f, "Cached fail of rule `{name}` at {start}")
            },
            Action::Entering { level } => {
                write!(f, "[PEG_TRACE] ")?;
                write!(f, "Entering level {level}")
            },
            Action::Leaving { level } => {
                write!(f, "[PEG_TRACE] ")?;
                write!(f, "Leaving level {level}")
            },
            Action::Begin { source, from } if from.is_zero()  => {
                write!(f, "[PEG_INPUT_START]\n{source}\n[PEG_TRACE_START]")
            },
            Action::Begin { source, from } => {
                write!(f, "[PEG_INPUT_START] from {from}\n{source}\n[PEG_TRACE_START]")
            },
            Action::End => {
                write!(f, "[PEG_TRACE_STOP]")
            },
            Action::Other { text } => {
                write!(f, "{text}")
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! gen_actions {
        ($($l:tt $n:ident)+) => {
            vec![$(gen_actions!(@to $l stringify!($n))),+]
        };
        (@to + $n:expr) => {Action::Attempting { name: $n, start: Loc::new(1, 1) }};
        (@to ! $n:expr) => {Action::Failed { name: $n, start: Loc::new(1, 1) }};
        (@to @ $n:expr) => {Action::Matched { name: $n, start: Loc::new(1, 1), stop: Loc::new(1, 1) }};
        (@to & $n:expr) => {Action::Other { text: "" }};
    }

    #[test]
    fn filter_fails_test() {
        let axtions = gen_actions![
            +a
            +b
            +c
            !c
            +d
            @d
            !b
            &ext
            +e
            @e
            @a
        ];
        let filted = filter_fails(axtions);
        assert_eq!(filted, gen_actions![
            &ext
            +a
            +e
            @e
            @a
        ]);
    }

    #[test]
    fn char_printable() {
        assert_eq!(DChar('a').to_string(), "a");
        assert_eq!(DChar(' ').to_string(), "' '");
        assert_eq!(DChar('\n').to_string(), "'\\n'");
        assert_eq!(DChar('\'').to_string(), "'\\''");
        assert_eq!(DChar('"').to_string(), "\"");
    }

    #[test]
    fn lines_test() {
        assert_eq!("".lines().collect::<Vec<_>>(), Vec::<&str>::new());
        assert_eq!("a".lines().collect::<Vec<_>>(), vec!["a"]);
        assert_eq!("a\n".lines().collect::<Vec<_>>(), vec!["a"]);
        assert_eq!("a\nb".lines().collect::<Vec<_>>(), vec!["a", "b"]);
        assert_eq!("a\n\nb".lines().collect::<Vec<_>>(), vec!["a", "", "b"]);
    }

    #[test]
    fn split_inclusive_once_test() {
        assert_eq!("a,b".split_inclusive(",").collect::<Vec<_>>(), vec!["a,","b"]);
        assert_eq!("a,,b".split_inclusive(",").collect::<Vec<_>>(), vec!["a,", ",","b"]);
        assert_eq!("a,".split_inclusive(",").collect::<Vec<_>>(), vec!["a,"]);
        assert_eq!("a".split_inclusive(",").collect::<Vec<_>>(), vec!["a"]);
        assert_eq!(",".split_inclusive(",").collect::<Vec<_>>(), vec![","]);
        assert_eq!("".split_inclusive(",").collect::<Vec<_>>(), Vec::<&str>::new());
    }

    #[test]
    fn to_index_test() {
        let tests = [
            (1, 1, "", 0),
            (1, 1, "a", 0),
            (1, 2, "a", 1),
            (1, 2, "ab", 1),
            (2, 3, "\nab\n ", 3),
            (3, 1, "\nab\n ", 4),
        ];
        for (line, col, src, expected) in tests {
            let loc = Loc::new(line, col);
            assert_eq!(loc.get_char_index(src), expected);
        }
    }

    #[test]
    fn find_last_test() {
        let tests = [
            (0..0, (|&n| n<3) as fn(&_) -> _, None),
            (0..1, |&n| n<3, Some(0)),
            (0..2, |&n| n<3, Some(1)),
            (0..3, |&n| n<3, Some(2)),
            (0..4, |&n| n<3, Some(2)),
            (2..4, |&n| n<3, Some(2)),
            (3..4, |&n| n<3, None),
        ];
        for (iter, predicate, expected) in tests {
            assert_eq!(iter.find_last(predicate), expected);
        }
    }

    #[test]
    fn fill_width_test() {
        let fill = Elem::new_fill();
        assert_eq!(fill.width(), 0);
    }

    #[test]
    fn collines_test() {
        let mut colline = ColLine::default();
        colline.push(Elem::new_left('1'), 0, 1);
        colline.push(Elem::new_left('+'), 1, 1);
        colline.push(Elem::new_left('2'), 2, 1);
        colline.push(Elem::new_left('+'), 3, 1);
        colline.push(Elem::new_left('3'), 4, 1);
        colline.push(Elem::new_left('+'), 6, 1);
        colline.push(Elem::new_left('4'), 7, 1);
        colline.push(Elem::new_left(""),  5, 1);
        colline.push(
            Elem::new(
                "baz",
                " ",
                Sides::bit_new(0b0101_0000),
                ' ',
            ),
            0,
            1,
        );
        colline.push(
            Elem::new_joint("exp", ""),
            0,
            1,
        );
        colline.fill_hangs();
        colline.concat_hangs();
        colline.output();
    }
}
