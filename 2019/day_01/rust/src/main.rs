/* Part 1
================================================= */

fn part_one(input: String) {
    let total_fuel: i32 = input
        .split_whitespace()
        .map(|x| x.parse::<i32>().unwrap() / 3 - 2)
        .sum();

    println!("The total fuel necessary is {}", total_fuel);
}

/* Part 2
================================================= */

fn part_two(input: String) {
    let total_fuel: i32 = input
        .split_whitespace()
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

use aoc_util::{aoc, AOCParams};

fn main() {
    aoc(&part_one, &part_two, AOCParams::new(false, None));
}
