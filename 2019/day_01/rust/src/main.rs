use std::env;
use std::fs;
use std::path::Path;

/* Part 1
================================================= */

fn part_one(input: String) {
    let total_fuel: i32 = input.split_whitespace()
        .map(|x| x.parse::<i32>().unwrap() / 3 - 2)
        .sum();

    println!("The total fuel necessary is {}", total_fuel);
}

/* Part 2
================================================= */

fn part_two(input: String) {
    let total_fuel: i32 = input.split_whitespace()
        .map(|x| fuel_for_module(x.parse::<i32>().unwrap()))
        .sum();

    println!("The total fuel necessary is {}", total_fuel);
}

fn fuel_for_module(mass: i32) -> i32 {
    let fuel = mass / 3 - 2;
    if fuel <= 0 {
        return 0;
    }

    return fuel + fuel_for_module(fuel);
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
