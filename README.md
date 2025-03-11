Tree visualization of [rust-peg] trace

# Example
```sh
$ cargo build
$ cd example/
$ ../target/debug/pegview -h
Usage: pegview [-c] [-i NAME].. [-u] [-e] [-h]

Options:
    -c, --center-rule   Rule name to centered
    -i, --ignore NAME   Ignore a rule
    -u, --unique-line   One rule one line
    -e, --exclude-fails 
                        Exclude failed matches
    -h, --help          Show help messages
$ cargo run --features=trace | ../target/debug/pegview 
    Finished `dev` profile [unoptimized + debuginfo] target(s) in 0.09s
     Running `target/debug/example`
[src/main.rs:47:13] p::top_level("1*(2+3)+4*5^6") = Ok(
    62505.0,
)
----------------------------------------------------------
[PEG_TRACE] Attempting to match rule `top_level` at 1:1
[PEG_TRACE] Attempting to match rule `trace` at 1:1
----------------------------------------------------------
Trace Source: "1*(2+3)+4*5^6"
   1    *   (       2    +   3    )+   4    *   5    ^   6    
├number┤ ├number ├number┤ ├number┤│ ├number┤ ├number┤ ├number┤
├atom──┤ │       ├atom──┤ ├atom──┤│ ├atom──┤ ├atom──┘ └atom──┤
├pow───┤ │       ├pow───┤ ├pow───┤│ ├pow───┤ ├pow────────────┤
├neg───┘ │       ├neg───┤ ├neg───┤│ ├neg───┘ └neg────────────┤
│        │       ├md────┘ └md────┤│ └md──────────────────────┤
│        │       └expr───────────┘│                          │
│        ├atom────────────────────┤                          │
│        ├pow─────────────────────┤                          │
│        └neg─────────────────────┤                          │
├md───────────────────────────────┘                          │
└expr────────────────────────────────────────────────────────┘
----------------------------------------------------------
[PEG_TRACE] Matched rule `trace` at 1:1 to 1:14
[PEG_TRACE] Matched rule `top_level` at 1:1 to 1:14
```

[rust-peg]: https://github.com/kevinmehall/rust-peg/
