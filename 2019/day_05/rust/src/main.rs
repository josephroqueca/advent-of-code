use opcode_computer::Program;

/* Part 1
================================================= */

fn part_one(input: String) {
    println!(
        "The program produces diagnostic code {}",
        Program::from_str(&input)
            .push(1)
            .run()
            .output()
            .pop()
            .unwrap()
    );
}

/* Part 2
================================================= */

fn part_two(input: String) {
    let mut program = Program::from_str(&input);
    let _ = program.push(5).run();

    println!(
        "The program produces diagnostic code {}",
        Program::from_str(&input)
            .push(5)
            .run()
            .output()
            .pop()
            .unwrap()
    )
}

/* Main + Input
================================================= */

use aoc_util::{aoc, AOCParams};

fn main() {
    aoc(&part_one, &part_two, AOCParams::new(false, None));
}
