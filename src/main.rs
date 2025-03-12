use itermaps::short_funcs::default;
use std::{
    collections::BTreeSet, env::args, fs, io::{stdin, Read}, process::exit, sync::atomic::Ordering
};

const BINNAME: &str = env!("CARGO_BIN_NAME");

use pegview::*;

fn main() {
    let mut options = getopts::Options::new();
    options
        .optflag("c", "center-rule", "Rule name to centered")
        .optmulti("i", "ignore", "Ignore a rule", "NAME")
        .optflag("u", "unique-line", "One rule one line")
        .optflag("e", "exclude-fails", "Exclude failed matches")
        .optflag("h", "help", "Show help messages")
        .optflag("v", "version", "Show version")
        .parsing_style(getopts::ParsingStyle::FloatingFrees);

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
        let usage = options.usage(&format!("{biref} [FILES...]"));
        println!("{usage}");
        println!("Report bugs from {} issues", env!("CARGO_PKG_REPOSITORY"));
        exit(0)
    }
    if matched.opt_present("v") {
        println!("{}", env!("CARGO_PKG_VERSION"));
        exit(0)
    }

    let uniq_line = matched.opt_present("u");
    let exclude_fail = matched.opt_present("e");
    let ignore_set = BTreeSet::from_iter(matched.opt_strs("i"));
    CENTER_NAME.store(matched.opt_present("c"), Ordering::Release);

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
    if exclude_fail {
        for actions in &mut regions {
            let new = filter_fails(actions.drain(..));
            actions.extend(new);
        }
    }

    for actions in &regions {
        println!("----------------------------------------------------------");
        if let Some(src) = actions.iter().find_map(Action::as_begin) {
            println!("Trace Source: {src:?}");
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

            for action in actions {
                match action {
                    Action::Matched { name, start, stop } => {
                        if ignore_set.contains(*name) { continue; }
                        let [start, stop]: [u32; 2] = [
                            start.to_index(src).cinto(),
                            stop.to_index(src).cinto(),
                        ];
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
                        if ignore_set.contains(*name) { continue; }
                        let start = start.to_index(src).cinto();
                        let elem = Elem::new(
                            *name,
                            " ",
                            Sides::bit_new(0b1101_0000),
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
