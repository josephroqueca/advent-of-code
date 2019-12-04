use std::env;

/* Part 1
================================================= */

fn has_pair(candidate: i32) -> bool {
    let mut previous_digit = candidate % 10;
    let mut value = candidate;
    while value >= 10 {
        value = value / 10;
        if value % 10 == previous_digit {
            return true;
        }
        previous_digit = value % 10;
    }
    false
}

fn is_incrementing(candidate: i32) -> bool {
    let mut previous_digit = candidate % 10;
    let mut value = candidate;
    while value >= 10 {
        value = value / 10;
        if value % 10 > previous_digit {
            return false;
        }
        previous_digit = value % 10;
    }
    true
}

fn part_one(range: std::ops::Range<i32>) {
    let passwords = range
        .into_iter()
        .filter(|&candidate| has_pair(candidate) && is_incrementing(candidate))
        .count();
    println!("{} passwords meet the criteria", passwords);
}

/* Part 2
================================================= */

fn has_isolated_pair(candidate: i32) -> bool {
    let mut previous_digit = candidate % 10;
    let mut group_size = 1;
    let mut value = candidate;
    while value >= 10 {
        value = value / 10;
        if value % 10 == previous_digit {
            group_size += 1;
        } else {
            if group_size == 2 {
                return true;
            } else {
                group_size = 1;
            }
        }
        previous_digit = value % 10;
    }
    group_size == 2
}

fn part_two(range: std::ops::Range<i32>) {
    let passwords = range
        .into_iter()
        .filter(|&candidate| has_isolated_pair(candidate) && is_incrementing(candidate))
        .count();
    println!("{} passwords meet the criteria", passwords);
}

/* Main + Input
================================================= */

fn main() {
    let args: Vec<String> = env::args().collect();
    let part = &args[1];

    let range = 256310..732736;

    match part.as_str() {
        "1" => part_one(range),
        "2" => part_two(range),
        _ => println!("Only parts 1 and 2 exist..."),
    }
}
