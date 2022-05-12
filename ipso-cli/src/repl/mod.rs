pub mod circular_buffer;

use ipso_diagnostic::Source;
use std::io::{self, Write};
use termion::{
    cursor::DetectCursorPos,
    event::{Event, Key},
    input::TermRead,
    raw::IntoRawMode,
};

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
        self.buffer.push(c);
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

fn render_error(
    stdout: &mut termion::raw::RawTerminal<io::Stdout>,
    prompt: &str,
    error_position: usize,
    error_message: String,
) -> Result<(), io::Error> {
    writeln!(stdout, "{}^", " ".repeat(prompt.len() + error_position))?;
    writeln!(stdout, "error: {}", error_message)?;
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

    writeln!(stdout, "{}\n\nType :quit<ENTER> to quit.\n", IPSO_BANNER)?;

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
                        input_state.draw(&mut stdout, prompt)?;
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
                        input_state.draw(&mut stdout, prompt)?;
                    }
                }
                Key::Char('\n') => {
                    if input_state.buffer == ":quit" {
                        break;
                    } else {
                        history.push(input_state.buffer.clone());
                        history_index = history.len();

                        input_state.newline(&mut stdout)?;

                        stdout.suspend_raw_mode()?;
                        let mut parser = ipso_parse::Parser::new(
                            source.clone(),
                            ipso_lex::Lexer::new(&input_state.buffer),
                        );
                        let parsed = ipso_parse::grammar::expr::expr(&mut parser);
                        match parser.into_parse_error(parsed.result) {
                            Err(err) => {
                                render_error(&mut stdout, prompt, err.position(), err.message())?;
                            }
                            Ok(expr) => match repl.eval_show(expr) {
                                Err(err) => {
                                    render_error(
                                        &mut stdout,
                                        prompt,
                                        err.position(),
                                        err.message(),
                                    )?;
                                }
                                Ok(value) => {
                                    if let Some(value) = value {
                                        writeln!(stdout, "{}", value)
                                    } else {
                                        Ok(())
                                    }?
                                }
                            },
                        }
                        stdout.activate_raw_mode()?;

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