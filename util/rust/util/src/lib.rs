use std::env;
use std::fs;
use std::option::Option;
use std::path::Path;

pub struct AOCParams {
    pub testing: bool,
    pub input: Option<String>,
}

impl AOCParams {
    pub fn new(testing: bool, input: Option<String>) -> AOCParams {
        AOCParams {
            testing: testing,
            input: input,
        }
    }
}

pub fn aoc(p1: &dyn Fn(String) -> (), p2: &dyn Fn(String) -> (), params: AOCParams) {
    let args: Vec<String> = env::args().collect();
    let part = &args[1];

    let input = match params.input {
        Some(ref r#override) => r#override.to_string(),
        None => {
            if params.testing {
                get_test_input()
            } else {
                get_input()
            }
        }
    };

    match part.as_str() {
        "1" => p1(input),
        "2" => p2(input),
        _ => println!("Only parts 1 and 2 exist..."),
    }
}

fn get_test_input() -> String {
    read_file("../test.txt")
}

fn get_input() -> String {
    read_file("../input.txt")
}

pub fn read_file(file: &str) -> String {
    let input_file = Path::new(&file);

    if input_file.exists() {
        return fs::read_to_string(input_file).unwrap();
    }

    String::new()
}
