use std::io::{self, Write};

use ipso_diagnostic::Source;

pub fn run() -> io::Result<()> {
    let mut stdout = io::stdout();
    let stdin = io::stdin();
    let repl = ipso_repl::Repl::new();

    writeln!(
        stdout,
        "Welcome to the ipso repl. Type :quit<ENTER> to quit."
    )?;

    let mut buffer = String::new();

    let mut quit = false;
    while !quit {
        buffer.clear();

        write!(stdout, "\n> ")?;
        stdout.flush()?;

        stdin.read_line(&mut buffer)?;
        if buffer == ":quit\n" {
            quit = true;
        } else {
            {
                let mut parser = ipso_parse::Parser::new(
                    Source::Interactive {
                        label: String::from("repl"),
                    },
                    ipso_lex::Lexer::new(&buffer),
                );
                let parsed = ipso_parse::grammar::expr::expr(&mut parser);
                match parser.into_parse_error(parsed.result) {
                    Err(err) => println!("{:?}", err),
                    Ok(expr) => match repl.eval(expr) {
                        Err(err) => println!("{:?}", err),
                        Ok(value) => println!("{:?}", value),
                    },
                }
            }
        }
    }

    Ok(())
}
