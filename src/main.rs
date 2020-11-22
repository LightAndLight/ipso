use std::env;
mod parse;

#[derive(Debug)]
struct Config {
    filename: String,
    entrypoint: Option<String>,
}

#[derive(Debug)]
enum ConfigError {
    MissingFilename,
    MissingEntrypoint,
}

fn get_filename(args: &Vec<String>) -> Result<String, ConfigError> {
    if args.len() > 1 {
        return Ok(args[1].clone());
    } else {
        return Err(ConfigError::MissingFilename);
    }
}

fn get_entrypoint(args: &Vec<String>) -> Result<Option<String>, ConfigError> {
    match args
        .iter()
        .enumerate()
        .find_map(|(ix, val)| if val == "--run" { Some(ix) } else { None })
    {
        None => Ok(None),
        Some(option_ix) => match args.get(option_ix + 1) {
            None => Err(ConfigError::MissingEntrypoint),
            Some(entrypoint) => Ok(Some(entrypoint.clone())),
        },
    }
}

fn get_config(args: &Vec<String>) -> Result<Config, ConfigError> {
    let filename = get_filename(args)?;
    let entrypoint = get_entrypoint(args)?;
    Ok(Config {
        filename,
        entrypoint,
    })
}

fn parse_args() -> Result<Config, ConfigError> {
    let args = env::args().collect();
    get_config(&args)
}

fn report_config_error(err: ConfigError) {
    println!("{:?}", err)
}

#[derive(Debug)]
enum InterpreterError {
    ParseError(parse::ParseError),
}

impl From<parse::ParseError> for InterpreterError {
    fn from(err: parse::ParseError) -> Self {
        InterpreterError::ParseError(err)
    }
}

fn report_interpreter_error(err: InterpreterError) {
    println!("{:?}", err)
}

fn run_interpreter(config: Config) -> Result<(), InterpreterError> {
    let filename: String = config.filename;
    let entrypoint: String = config.entrypoint.unwrap_or(String::from("main"));
    let ast: () = parse::parse_file(&filename)?;
    panic!("{:?} {:?} {:?}", filename, entrypoint, ast)
}

fn main() {
    match parse_args() {
        Err(err) => {
            report_config_error(err);
            std::process::exit(1);
        }
        Ok(config) => match run_interpreter(config) {
            Ok(()) => {}
            Err(err) => {
                report_interpreter_error(err);
                std::process::exit(1);
            }
        },
    }
}
