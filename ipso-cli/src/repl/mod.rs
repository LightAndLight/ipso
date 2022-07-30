pub mod circular_buffer;

use ipso_diagnostic::Source;
use std::io::{self, Stdout, Write};
use termion::{
    cursor::DetectCursorPos,
    event::{Event, Key},
    input::TermRead,
    raw::{IntoRawMode, RawTerminal},
};

use crate::version::VERSION;

use self::circular_buffer::CircularBuffer;

const IPSO_BANNER: &str = r#" _
(_)
 _ _ __  ___  ___
| | '_ \/ __|/ _ \
| | |_) \__ \ (_) |
|_| .__/|___/\___/
  | |
  |_|"#;

struct InputState {
    terminal_size: (u16, u16),
    cursor_row: u16,
    buffer_index: usize,
    buffer: String,
}

impl InputState {
    fn new(cursor_current_row: u16) -> io::Result<Self> {
        let terminal_size = termion::terminal_size()?;
        Ok(InputState {
            terminal_size,
            cursor_row: cursor_current_row,
            buffer_index: 0,
            buffer: String::new(),
        })
    }

    #[allow(clippy::ptr_arg)]
    fn set(&mut self, value: &String) {
        self.buffer_index = value.len();
        self.buffer.clone_from(value)
    }

    fn reset(&mut self) {
        self.buffer_index = 0;
        self.buffer.clear();
    }

    fn left(&mut self, stdout: &mut dyn Write, count: u16) -> io::Result<()> {
        if count > 0 && self.buffer_index >= count as usize {
            write!(stdout, "{}", termion::cursor::Left(count))?;
            stdout.flush()?;
            self.buffer_index -= count as usize;
            Ok(())
        } else {
            Ok(())
        }
    }

    fn right(&mut self, stdout: &mut dyn Write, count: u16) -> io::Result<()> {
        if count > 0 && self.buffer_index + count as usize <= self.buffer.len() {
            write!(stdout, "{}", termion::cursor::Right(count))?;
            stdout.flush()?;
            self.buffer_index += count as usize;
            Ok(())
        } else {
            Ok(())
        }
    }

    fn insert(&mut self, c: char) -> io::Result<()> {
        self.buffer.insert(self.buffer_index, c);
        self.buffer_index += 1;

        Ok(())
    }

    fn delete(&mut self) {
        if self.buffer_index < self.buffer.len() {
            self.buffer.remove(self.buffer_index);
        }
    }

    fn backspace(&mut self, stdout: &mut dyn Write) -> io::Result<()> {
        if self.buffer_index >= 1 {
            self.buffer.remove(self.buffer_index - 1);
            self.left(stdout, 1)
        } else {
            Ok(())
        }
    }

    fn newline(&mut self, stdout: &mut dyn Write) -> io::Result<()> {
        let at_bottom_row = self.cursor_row == self.terminal_size.1;
        writeln!(stdout)?;
        if !at_bottom_row {
            self.cursor_row += 1;
        }
        write!(stdout, "{}", termion::cursor::Goto(1, self.cursor_row))?;
        stdout.flush()?;
        Ok(())
    }

    fn draw(&self, stdout: &mut dyn Write, prompt: &str) -> io::Result<()> {
        write!(
            stdout,
            "{}{}{}{}{}",
            termion::cursor::Goto(1, self.cursor_row),
            termion::clear::AfterCursor,
            prompt,
            self.buffer,
            termion::cursor::Goto(
                prompt.len() as u16 + self.buffer_index as u16 + 1,
                self.cursor_row
            )
        )?;
        stdout.flush()
    }
}

/*
Whenever we write a '\n' to stdout, we need to update the input cursor.

`Newliner`'s write instance calls `InputState::newline` for each '\n' written.

Without this, the terminal UI will only render the first line of a multi-line
string. See https://github.com/LightAndLight/ipso/issues/203 for examples of
how it breaks.
*/
struct Newliner<'a> {
    input_state: &'a mut InputState,
    stdout: &'a mut RawTerminal<Stdout>,
}

impl<'a> Write for Newliner<'a> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let mut lines = buf.split(|b| *b == b'\n');
        let mut bytes_written = 0;
        if let Some(line) = lines.next() {
            match self.stdout.write(line) {
                Err(err) => return Err(err),
                Ok(count) => {
                    bytes_written += count;
                }
            }
        }
        lines.try_for_each(|line| {
            self.input_state.newline(self.stdout)?;
            bytes_written += 1;
            let count = self.stdout.write(line)?;
            bytes_written += count;
            Ok::<(), io::Error>(())
        })?;
        Ok(bytes_written)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.stdout.flush()
    }
}

fn render_error(
    stdout: &mut Newliner,
    prompt: &str,
    error_position: usize,
    error_message: String,
) -> Result<(), io::Error> {
    writeln!(stdout, "{}^", " ".repeat(prompt.len() + error_position))?;
    write!(stdout, "error: {}", error_message)?;
    Ok(())
}

pub fn run() -> io::Result<()> {
    let source = Source::Interactive {
        label: String::from("repl"),
    };
    let prompt = "> ";

    let mut stdout = io::stdout();
    let repl = ipso_repl::Repl::new(source.clone());

    let mut history: CircularBuffer<String> = CircularBuffer::new(100);
    let mut history_index = 0;

    writeln!(
        stdout,
        "{} {}\n\nType :quit<ENTER> to quit.\n",
        IPSO_BANNER, VERSION
    )?;

    let mut stdout = stdout.into_raw_mode()?;
    let (_, cursor_current_row) = stdout.cursor_pos()?;
    let mut input_state = InputState::new(cursor_current_row)?;

    input_state.draw(&mut stdout, prompt)?;

    let stdin = io::stdin();
    for event in stdin.events() {
        if let Event::Key(key) = event? {
            match key {
                Key::Backspace => {
                    input_state.backspace(&mut stdout)?;
                }
                Key::Delete => {
                    input_state.delete();
                }
                Key::Left => {
                    input_state.left(&mut stdout, 1)?;
                }
                Key::Right => {
                    input_state.right(&mut stdout, 1)?;
                }
                Key::Up => {
                    if history_index > 0 {
                        history_index -= 1;
                        input_state.set(&history[history_index]);
                    }
                }
                Key::Down => {
                    if history_index < history.len() {
                        history_index += 1;
                        if history_index < history.len() {
                            input_state.set(&history[history_index]);
                        } else {
                            input_state.reset();
                        }
                    }
                }
                Key::Ctrl('c') => input_state.reset(),
                Key::Char('\n') => {
                    if input_state.buffer == ":quit" {
                        break;
                    } else if input_state.buffer.starts_with(":type") {
                        history.push(input_state.buffer.clone());
                        history_index = history.len();

                        input_state.newline(&mut stdout)?;

                        let prefix = ":type";
                        let input = input_state.buffer.trim_start_matches(prefix);
                        let mut error_offset = prefix.as_bytes().len();

                        let new_input = input.trim_start();
                        error_offset += input.as_bytes().len() - new_input.as_bytes().len();

                        let input = new_input;

                        let mut parser =
                            ipso_parse::Parser::new(source.clone(), ipso_lex::Lexer::new(input));
                        let parsed = ipso_parse::grammar::expr::expr(&mut parser);
                        let parse_result = parser.into_parse_error(parsed.result);

                        {
                            let mut stdout = Newliner {
                                input_state: &mut input_state,
                                stdout: &mut stdout,
                            };
                            match parse_result {
                                Err(err) => render_error(
                                    &mut stdout,
                                    prompt,
                                    error_offset + err.position(),
                                    err.message(),
                                ),
                                Ok(expr) => match repl.type_of(expr) {
                                    Err(err) => render_error(
                                        &mut stdout,
                                        prompt,
                                        error_offset + err.position(),
                                        err.message(),
                                    ),
                                    Ok(value) => stdout.write_all(value.render().as_bytes()),
                                },
                            }
                        }?;

                        input_state.newline(&mut stdout)?;
                        input_state.reset();
                    } else {
                        history.push(input_state.buffer.clone());
                        history_index = history.len();

                        input_state.newline(&mut stdout)?;

                        let mut parser = ipso_parse::Parser::new(
                            source.clone(),
                            ipso_lex::Lexer::new(&input_state.buffer),
                        );
                        let parsed = ipso_parse::grammar::expr::expr(&mut parser);
                        let parse_result = parser.into_parse_error(parsed.result);

                        {
                            let mut stdout = Newliner {
                                input_state: &mut input_state,
                                stdout: &mut stdout,
                            };
                            match parse_result {
                                Err(err) => {
                                    render_error(&mut stdout, prompt, err.position(), err.message())
                                }
                                Ok(expr) => match repl.eval_show(&mut stdout, expr) {
                                    Err(err) => render_error(
                                        &mut stdout,
                                        prompt,
                                        err.position(),
                                        err.message(),
                                    ),
                                    Ok(value) => {
                                        if let Some(value) = value {
                                            stdout.write_all(value.as_bytes())
                                        } else {
                                            Ok(())
                                        }
                                    }
                                },
                            }
                        }?;

                        input_state.newline(&mut stdout)?;
                        input_state.reset();
                    }
                }
                Key::Char(c) => {
                    input_state.insert(c)?;
                }
                _ => {}
            }
        }

        input_state.draw(&mut stdout, prompt)?;
    }

    Ok(())
}
