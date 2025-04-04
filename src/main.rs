use itermaps::{fields, short_funcs::default};
use std::{
    collections::BTreeSet,
    env::args,
    fs,
    io::{stdin, Read},
    process::exit,
    sync::atomic::Ordering,
};

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

use pegview::*;

fn colline_from_src(src: &str) -> ColLine<'_> {
    let mut colline = ColLine::new();
    let mut i = 0;
    for ch in src.chars() {
        let elem = Elem::new_left(ch);
        colline.push(elem, i, 1, false);
        i += 1;
    }
    // 填充末尾, 防止末尾匹配下降过头
    let elem = Elem::new_fill();
    colline.push(elem, i, 1, false);
    colline
}

fn fake_src<'a>(regions: &mut Vec<Vec<Action<'a>>>, source: &'a mut String) {
    if regions.len() != 1 || regions.iter()
        .any(|region| region.iter()
            .any(|action|
                action.is_begin() || action.is_end()))
    {
        eprintln!("Only support one region \
                (no PEG_INPUT_START PEG_INPUT_START PEG_TRACE_STOP)");
        exit(3);
    }
    let region = &mut regions[0];
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

    region.push(Action::Begin { source, from: default() });
}

fn main() {
    let options = getopts_macro::getopts_options! {
        -c --center-rule            "Rule name to centered";
        -i --ignore*=NAME           "Ignore a rule";
        -I --ignore-partial*=NAME   "Ignore a rule, support partial pattern";
        -u --unique-line            "One rule one line";
        -e --exclude-fails          "Exclude failed matches";
        -r --pair-fails             "Add unpaired failed matches";
        -w --full-width-tab-chars   "Full-width tab chars";
        -s --fake-source            "Using oneline fake source";
        -S --fake-source-from=SRC   "Using oneline fake source";
        -q --unquote-space          "Unquote space";
        -C --show-cached            "Show cached match and fail";
        -h --help*                  "Show help messages";
           --example                "Show example usage";
        -v --version                "Show version";
        .parsing_style(getopts::ParsingStyle::FloatingFrees)
    };

    let result = options.parse(args().skip(1));
    let matched = match result {
        Ok(matched) => matched,
        Err(e) => {
            eprintln!("{e}");
            exit(2);
        },
    };
    if matched.opt_present("h") {
        let biref = options.short_usage(BINNAME);
        let about = "Parse rust-peg traces, convert to tree";
        let info = format!("{biref} [FILES..]\n{about}");
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

    let uniq_line = matched.opt_present("u");
    let exclude_fail = matched.opt_present("e");
    let pair_fail = matched.opt_present("r");
    let mut fake_source = matched.opt_str("S")
        .or_else(|| matched.opt_present("s").then(String::new));
    let ignore_set = BTreeSet::from_iter(matched.opt_strs("i"));
    let ignore_part_list = matched.opt_strs("I");
    let show_cached = matched.opt_present("C");

    CENTER_NAME.store(matched.opt_present("c"), Ordering::Release);
    FULL_WIDTH_TAB.store(matched.opt_present("w"), Ordering::Release);
    UNQUOTE_SPACE.store(matched.opt_present("q"), Ordering::Release);

    let ignored = |s| {
        ignore_set.contains(s)
            || ignore_part_list.iter()
                .any(|p| str::contains(s, p))
    };

    let buf = &mut String::new();
    if matched.free.is_empty() {
        stdin().read_to_string(buf).unwrap();
    } else {
        for path in &matched.free {
            let mut f = match fs::File::open(path) {
                Ok(f) => f,
                Err(e) => {
                    eprintln!("error {path:?}: {e}");
                    exit(e.raw_os_error().unwrap_or(3));
                },
            };
            f.read_to_string(buf).unwrap();
            if buf.chars().next_back().unwrap_or('\n') != '\n' {
                buf.push('\n')
            }
        }
    }

    let actions = match parser::lines(buf) {
        Ok(x) => x,
        Err(e) => {
            eprintln!("{e}");
            exit(3);
        },
    };

    let mut regions = split_regions(actions);

    if pair_fail {
        regions.iter_mut()
            .filter(|region| region.iter()
                .any(|action| action.is_begin()))
            .for_each(pair_fails)
    }

    if exclude_fail {
        for actions in &mut regions {
            let new = filter_fails(actions.drain(..));
            actions.extend(new);
        }
    }

    if let Some(source) = &mut fake_source {
        fake_src(&mut regions, source);
    }

    for actions in &regions {
        println!("----------------------------------------------------------");
        if let Some((src, from)) = actions.iter().find_map(Action::as_begin) {
            let from = from.get_char_index(src);
            let mut colline = colline_from_src(&src[from..]);
            let tidx = |loc: &Loc| {
                let ridx = loc.get_char_index(src)
                    .checked_sub(from)
                    .unwrap_or_else(|| {
                        panic!("Trace location {loc} less than `from {from}`")
                    });
                ridx.cinto::<u32>()
            };

            println!("Trace Source: {:?}", &src[from..]);

            for action in actions {
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

            for action in actions {
                match action {
                    Action::Matched { name, start, stop } => {
                        if ignored(*name) { continue; }
                        let [start, stop]: [u32; 2] = [start, stop].map(tidx);
                        let len = stop-start;
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
                        colline.push(elem, start, cols, uniq_line);
                    },
                    Action::Failed { name, start } => {
                        if ignored(*name) { continue; }
                        let start = tidx(start);
                        let elem = Elem::new(
                            *name,
                            " ",
                            Sides::bit_new(0b0101_0000),
                            ' ',
                        );
                        colline.push(elem, start, 1, uniq_line);
                    },
                    Action::CachedMatch { name, start } if show_cached => {
                        if ignored(*name) { continue; }
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
                        colline.push(elem, start, 1, uniq_line);
                    },
                    Action::CachedFail { name, start } if show_cached => {
                        if ignored(*name) { continue; }
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
                        colline.push(elem, start, 1, uniq_line);
                    },
                    _ => (),
                }
            }
            colline.fill_hangs();
            colline.concat_hangs();
            colline.output();
        } else {
            for action in actions {
                println!("{action}");
            }
        }
    }
}
