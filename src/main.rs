use std::io;
use std::fmt::Write as _;
use std::io::prelude::*;

fn main() {
    //let text = "a = 0";
    //let text = "a = \"some string with a \\\"quote\\\" in the middle \"";
    //let text = "a = 3\nb = 4\nc = 5";
    //let text = "a = b + (c + d)";
    //let text = "a = b * c + d\ne = 3 + 4 + \"abc\"\n\nf = a + b + c*d\nh+34";
    //let text = "a = 3 * 4\nb = a + 5\nc = a*b\nd = c * a + b * b\ne = d + 1\ne";//e = 3 + 4 + \"abc\"\n\nf = a + b + c*d\nh+34";
    //let text = "a = (b + c)";
    //let nodes = ast::parser::Parser::new(text).unwrap().parse().unwrap();
    //let nodes = ast::evaluator::Evaluator::new(text).unwrap().evaluate().unwrap();
    //for node in nodes {
    //    println!("{}", node);
    //}

    let mut e = ast::evaluator::Evaluator::new_interactive();
    loop {
        // output
        let mut output = String::new();
        let _ = write!(output, ">> ");
        let _ = std::io::stdout().write_all(output.as_bytes());
        std::io::stdout().flush().unwrap();

        // input
        let mut buffer = String::new();
        io::stdin().read_line(&mut buffer).unwrap();

        // do stuff
        if buffer == "exit\n" {
            break;
        }
        match e.evaluate_interactive(&buffer) {
            Ok(eval) => println!("{}", eval),
            Err(e) => println!("{}", e),
        }
    }
}
