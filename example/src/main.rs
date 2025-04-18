use std::{iter::once, ops};

// atom = number / "(" expr ")"
// pow  = atom ("^" atom)*
// neg  = "-"* pow
// md   = neg ("*" neg)*
// expr = md ("+" md)*

peg::parser!(grammar p() for str {
    rule number() -> f64 = s:$(['0'..='9']) { s.parse().unwrap() }
    rule atom() -> f64
        = number()
        / "(" e:expr() ")" {e}
    rule pow() -> f64
        = a:atom() b:("^" b:atom() {b})*
        {
            once(a).chain(b).rev().reduce(|b, a| a.powf(b)).unwrap()
        }
    rule neg() -> f64
        = s:"-"* n:pow()
        { if s.len() % 2 == 1 { -n } else { n } }
    rule md() -> f64
        = a:neg() b:("*" b:neg() {b})*
        {
            once(a).chain(b).reduce(ops::Mul::mul).unwrap()
        }
    pub rule expr() -> f64
        = a:md() b:("+" b:md() {b})*
        {
            once(a).chain(b).reduce(ops::Add::add).unwrap()
        }
    rule trace<T>(r: rule<T>) -> T
        = #{|input, pos| {
            // PEG 0.8.5 feature #{|input, pos| ...}
            #[cfg(feature = "trace")] {
                print!("[PEG_INPUT_START]");
                if pos != 0 { print!(" from {pos}") }
                println!("\n{input}\n[PEG_TRACE_START]");
            }
            peg::RuleResult::Matched(pos, ())
        }}
        r:r()? {?
            #[cfg(feature = "trace")]
            println!("[PEG_TRACE_STOP]");
            r.ok_or("")
        }
    pub rule top_level() -> f64
        = "> " r:trace(<expr()>) {r}
});

fn main() {
    println!("Result: {:?}", p::top_level("> 1*(2+3)+4*5^6"));
}
