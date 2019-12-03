use std::env;
use std::fs;
use std::path::Path;
use std::collections::HashSet;

/* Part 1
================================================= */

fn part_one(input: String) {
    let frequency: i32 = input.lines()
        .map(|x| x.parse::<i32>().unwrap())
        .sum();

    println!("The resulting frequency is {}", frequency);
}

/* Part 2
================================================= */

fn part_two(input: String) {
    let mut frequency = 0;
    let mut frequencies: HashSet<i32> = HashSet::new();
    let repeated = 'repeatLoop: loop {
        for modifier in input.lines().map(|x| x.parse::<i32>().unwrap()) {
            frequency += modifier;
            if frequencies.contains(&frequency) {
                break 'repeatLoop frequency;
            } else {
                frequencies.insert(frequency);
            }
        }
    };

    println!("The first repeated frequency is {}", repeated);
}

/* Main + Input
================================================= */

fn main() {
    let args: Vec<String> = env::args().collect();
    let part = &args[1];
    let input = get_input();
    match part.as_str() {
        "1" => part_one(input),
        "2" => part_two(input),
        _ => println!("Only parts 1 and 2 exist...")
    }
}

fn get_input() -> String {
    let input_file = Path::new("../input.txt");

    if input_file.exists() {
        return fs::read_to_string(input_file).unwrap();
    }

    String::new()
}
