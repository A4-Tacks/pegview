use itermaps::{fields, short_funcs::default};
use std::{
    collections::BTreeSet,
    env::args,
    fs,
    io::{stdin, Read},
    process::exit,
    sync::atomic::Ordering,
};

use pegview::*;

const BINNAME: &str = env!("CARGO_BIN_NAME");
const EXAMPLE_MSG: &str =
{r#"Grammar:
    add    = atom *("+" atom)
    atom   = number
           / "(" add ")"
    number = ;builtin-token;

Location:
    <num>:<num>         Line and column number, based 1, e.g 1:1
    #<num>              Char index, based 0, e.g #0

Let your parser output the following trace format:

$ pegview <<\EOF
[PEG_INPUT_START]
(1+2)
[PEG_TRACE_START]
[PEG_TRACE] Attempting to match rule `add` at 1:1
[PEG_TRACE] Attempting to match rule `atom` at 1:1
[PEG_TRACE] Attempting to match rule `number` at 1:1
[PEG_TRACE] Failed to match rule `number` at 1:1
[PEG_TRACE] Attempting to match rule `add` at 1:2
[PEG_TRACE] Attempting to match rule `atom` at 1:2
[PEG_TRACE] Attempting to match rule `number` at 1:2
[PEG_TRACE] Matched rule `number` at 1:2 to 1:3
[PEG_TRACE] Matched rule `atom` at 1:2 to 1:3
[PEG_TRACE] Attempting to match rule `add` at 1:4
[PEG_TRACE] Attempting to match rule `atom` at 1:4
[PEG_TRACE] Matched rule `number` at 1:4 to 1:5
[PEG_TRACE] Matched rule `atom` at 1:4 to 1:5
[PEG_TRACE] Matched rule `add` at 1:2 to 1:5
[PEG_TRACE] Matched rule `atom` at 1:1 to 1:6
[PEG_TRACE] Matched rule `add` at 1:1 to 1:6
[PEG_TRACE_STOP]
EOF"#};

fn colline_from_src(src: &str, cfg: Config) -> ColLine<'_> {
    let mut colline = ColLine::new(cfg);
    let mut i = 0;
    for ch in src.chars() {
        let elem = Elem::new_left(ch);
        colline.push_solid(elem, i, 1);
        i += 1;
    }
    // 填充末尾, 防止末尾匹配下降过头
    let elem = Elem::new_fill();
    colline.push_solid(elem, i, 1);
    colline
}

fn trace_regions<'a, 'b, I>(iter: I) -> impl Iterator<Item = &'b mut Vec<Action<'a>>>
where I: IntoIterator<Item = &'b mut Vec<Action<'a>>>,
      I::IntoIter: 'b,
      'a: 'b,
{
    iter.into_iter()
        .filter(move |region| region.iter().any(|action| {
            action.is_begin() || action.is_end()
        }))
}

fn fake_src(regions: &mut Vec<Vec<Action<'_>>>, mut source: String) {
    if trace_regions(&mut *regions).next().is_some() {
        eprintln!("Only support one region \
                (no PEG_INPUT_START PEG_INPUT_START PEG_TRACE_STOP)");
        exit(3);
    }
    let region = regions.iter_mut()
        .find(|region| region.iter().any(Action::is_tracing))
        .unwrap_or_else(|| {
            eprintln!("Cannot find valided tracings ([PEG_TRACE] Matched ...)");
            exit(3);
        });
    let compat_locs = region.iter()
        .filter_map(Action::locs)
        .map(|(start, stop)| {
            let f = |mut loc: Loc| {
                if loc.line == 0 {
                    loc.column += 1;
                }
                loc
            };
            (f(start), stop.map(f))
        });
    let max_col = compat_locs.clone()
        .map(|(start, stop)| stop
            .filter(|stop| stop.line <= 1)
            .unwrap_or(start))
        .filter(|loc| loc.line <= 1)
        .map(fields!(column))
        .max()
        .unwrap_or(1);

    let mut eof = '$';
    let Loc { line, column } = compat_locs
        .map(|(start, stop)| stop.unwrap_or(start))
        .map(|mut loc| {
            if loc.line > 1 {
                eof = '\n';
                (loc.line, loc.column) = (1, max_col);
            }
            loc
        })
        .max_by_key(fields!(column))
        .expect("locations by empty");
    assert!(line <= 1, "{line}");

    for _ in source.chars().count()..(column-1).cinto() {
        source.push('.')
    }
    source.push(eof);

    region.push(Action::Begin { source: source.into(), from: default() });
}

fn check_width(action: &Action, start: u32, stop: u32) {
    if stop < start {
        eprintln!("Invalid span, stop ({stop}) < start ({start}):\n    {action}");
        exit(1)
    }
}

fn preset_matched(args: impl IntoIterator<Item = String>) -> getopts::Matches {
    let options = getopts_macro::getopts_options! {
        -c --center-rule                "Rule name to centered";
        -i --ignore*=NAME               "Ignore a rule";
        -I --ignore-partial*=NAME       "Ignore a rule, support partial pattern";
        -z --zero-width-ignore*=NAME    "Ignore a rule, when zero width matched";
        -Q --quiet*=NAME                "Quiet a rule sub tree";
        -u --unique-line                "One rule one line";
        -F --first-width                "Share width to first";
        -L --last-width                 "Share width to last";
           --share-width=STYLE          "Share width style (mixed, first, last)";
        -e --exclude-fails              "Exclude failed matches";
        -r --pair-fails                 "Add unpaired failed matches";
        -w --full-width-tab-chars       "Full-width tab chars";
        -s --fake-source                "Using oneline fake source";
        -S --fake-source-from=SRC       "Using fake source";
        -q --unquote-space              "Unquote space";
        -C --show-cached                "Show cached match and fail";
        -h --help*                      "Show help messages";
           --example                    "Show example usage";
        -v --version                    "Show version";
        .parsing_style(getopts::ParsingStyle::FloatingFrees)
    };

    let result = options.parse(args);
    let matched = match result {
        Ok(matched) => matched,
        Err(e) => {
            eprintln!("{e}");
            exit(2);
        },
    };
    if matched.opt_present("h") {
        let about = "Parse rust-peg traces, convert to tree view";
        let info = format!("Usage: {BINNAME} [Options].. [FILES]..\n{about}");
        let usage = options.usage(&info);
        println!("{usage}");
        println!("Report bugs from {} issues", env!("CARGO_PKG_REPOSITORY"));
        exit(0)
    }
    if matched.opt_present("example") {
        println!("{}", EXAMPLE_MSG);
        exit(0)
    }
    if matched.opt_present("v") {
        println!("{}", env!("CARGO_PKG_VERSION"));
        exit(0)
    }

    matched
}

#[derive(Debug, Default, Clone)]
struct InputConfig<'a> {
    uniq_line:          bool,
    exclude_fail:       bool,
    pair_fail:          bool,
    fake_source:        Option<String>,
    ignore_set:         BTreeSet<String>,
    ignore_zw_set:      BTreeSet<String>,
    ignore_part_list:   Vec<String>,
    quiet_set:          BTreeSet<String>,
    show_cached:        bool,
    share_width_style:  ShareWidth,
    files: &'a [String],
}

impl<'a> From<&'a getopts::Matches> for InputConfig<'a> {
    fn from(matched: &'a getopts::Matches) -> Self {
        let input_cfg = Self {
            uniq_line: matched.opt_present("u"),
            exclude_fail: matched.opt_present("e"),
            pair_fail: matched.opt_present("r"),
            fake_source: matched.opt_str("S")
                .or_else(|| matched.opt_present("s").then(String::new)),
            ignore_set: BTreeSet::from_iter(matched.opt_strs("i")),
            ignore_zw_set: BTreeSet::from_iter(matched.opt_strs("z")),
            ignore_part_list: matched.opt_strs("I"),
            quiet_set: BTreeSet::from_iter(matched.opt_strs("Q")),
            show_cached: matched.opt_present("C"),
            share_width_style: matched
                .opt_get_default("share-width", matched.opt_present("L")
                    .then_some(ShareWidth::Last)
                    .or_else(|| matched.opt_present("F").then_some(ShareWidth::First))
                    .unwrap_or_default())
                .unwrap_or_else(|e| {
                    eprintln!("error: {e}");
                    exit(2)
                }),
            files: &matched.free,
        };

        CENTER_NAME.store(matched.opt_present("c"), Ordering::Release);
        FULL_WIDTH_TAB.store(matched.opt_present("w"), Ordering::Release);
        UNQUOTE_SPACE.store(matched.opt_present("q"), Ordering::Release);

        input_cfg
    }
}

impl InputConfig<'_> {
    fn ignored(&self, s: &str) -> bool {
        self.ignore_set.contains(s)
            || self.ignore_part_list.iter()
                .any(|p| str::contains(s, p))
    }

    fn read_input_files(&self) -> String {
        let mut buf = String::new();

        if self.files.is_empty() {
            if atty::is(atty::Stream::Stdin) {
                eprintln!("Warning: Reading PEG traces from tty");
            }
            stdin().read_to_string(&mut buf).unwrap();
        } else {
            for path in self.files {
                let mut f = match fs::File::open(path) {
                    Ok(f) => f,
                    Err(e) => {
                        eprintln!("error {path:?}: {e}");
                        exit(e.raw_os_error().unwrap_or(3));
                    },
                };
                f.read_to_string(&mut buf).unwrap();
                if buf.chars().next_back().unwrap_or('\n') != '\n' {
                    buf.push('\n')
                }
            }
        }

        buf
    }

    fn parse_regions<'a>(&mut self, source: &'a str) -> Vec<Vec<Action<'a>>> {
        let actions = match parser::lines(source) {
            Ok(x) => x,
            Err(e) => {
                eprintln!("{e}");
                exit(3);
            },
        };

        let mut regions = split_regions(actions);

        if self.pair_fail {
            trace_regions(&mut regions)
                .for_each(pair_fails)
        }

        for actions in &mut regions {
            quiet_rules(&self.quiet_set, actions);
        }

        if let Some(source) = self.fake_source.take() {
            fake_src(&mut regions, source);
        }

        if self.exclude_fail {
            for actions in &mut regions {
                *actions = filter_fails(actions.drain(..));
            }
        }

        regions
    }

    fn run_regions(&self, regions: &[Vec<Action<'_>>]) {
        for region in regions {
            println!("----------------------------------------------------------");
            self.run_region(region);
        }
    }

    fn run_region(&self, region: &[Action<'_>]) {
        let cfg = Config {
            uniq: self.uniq_line,
            share_width: self.share_width_style,
        };

        let Some((src, from)) = region.iter().find_map(Action::as_begin) else {
            for action in region {
                println!("{action}");
            }
            return;
        };

        let from = from.get_char_index(src);
        let mut colline = colline_from_src(&src[from..], cfg);
        let tidx = |loc: &Loc| {
            let ridx = loc.get_char_index(src)
                .checked_sub(from)
                .unwrap_or_else(|| {
                    panic!("Trace location {loc} less than `from {from}`")
                });
            ridx.cinto::<u32>()
        };

        println!("Trace Source: {:?}", &src[from..]);

        for action in region {
            match action {
                Action::Other { text } => {
                    println!("{text}");
                },
                Action::Begin { source, from: _ } => {
                    debug_assert_eq!(src, *source,
                                    "Possible duplicate [PEG_INPUT_START]");
                },
                _ => (),
            }
        }

        for action in region {
            match action {
                Action::Matched { name, start, stop } => {
                    if self.ignored(name) { continue; }
                    let [start, stop]: [u32; 2] = [start, stop].map(tidx);
                    check_width(action, start, stop);
                    let len = stop-start;
                    if len == 0 && self.ignore_zw_set.contains(*name) { continue }
                    let name = if len == 0 {
                        let attr = Attr {
                            zero_width: true,
                            ..default()
                        };
                        Entry::Str(name, attr)
                    } else {
                        (*name).into()
                    };
                    let cols = len.max(1);
                    let elem = Elem::new_joint(name, "");
                    colline.push(elem, start, cols);
                },
                Action::Failed { name, start } => {
                    if self.ignored(name) { continue; }
                    let start = tidx(start);
                    let elem = Elem::new(
                        *name,
                        " ",
                        Sides::bit_new(0b0101_0000),
                        ' ',
                    );
                    colline.push(elem, start, 1);
                },
                Action::CachedMatch { name, start } if self.show_cached => {
                    if self.ignored(name) { continue; }
                    let start = tidx(start);
                    let elem = Elem::new(
                        Entry::Str(name, Attr {
                            cached_match: true,
                            ..default()
                        }),
                        " ",
                        Sides::bit_new(0b0101_0000),
                        ' ',
                    );
                    colline.push(elem, start, 1);
                },
                Action::CachedFail { name, start } if self.show_cached => {
                    if self.ignored(name) { continue; }
                    let start = tidx(start);
                    let elem = Elem::new(
                        Entry::Str(name, Attr {
                            cached_fail: true,
                            ..default()
                        }),
                        " ",
                        Sides::bit_new(0b0101_0000),
                        ' ',
                    );
                    colline.push(elem, start, 1);
                },
                _ => (),
            }
        }
        colline.fill_hangs();
        colline.concat_hangs();
        colline.output();
    }
}

fn main() {
    let matched = preset_matched(args().skip(1));

    let mut input_cfg = InputConfig::from(&matched);
    let source = input_cfg.read_input_files();

    let regions = input_cfg.parse_regions(&source);

    input_cfg.run_regions(&regions);
}
