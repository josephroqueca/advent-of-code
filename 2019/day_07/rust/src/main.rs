use opcode_computer::Program;
use std::cmp::max;
use std::collections::HashSet;
use std::iter::repeat;

/* Part 1
================================================= */

fn part_one(input: String) {
    let mut max_thruster_signal = 0;

    for a in 0..5 {
        for b in 0..5 {
            for c in 0..5 {
                for d in 0..5 {
                    for e in 0..5 {
                        let phase_sequence = vec![a, b, c, d, e];
                        if phase_sequence
                            .clone()
                            .into_iter()
                            .collect::<HashSet<i32>>()
                            .len()
                            != 5
                        {
                            continue;
                        }

                        let thruster_signal =
                            phase_sequence.iter().fold(0, |acc, &phase_setting| {
                                Program::from_str(&input)
                                    .push(phase_setting)
                                    .push(acc)
                                    .run()
                                    .output()
                                    .pop()
                                    .unwrap()
                            });

                        max_thruster_signal = max(max_thruster_signal, thruster_signal);
                    }
                }
            }
        }
    }

    println!(
        "The max thruster signal produced is {}",
        max_thruster_signal
    );
}

/* Part 2
================================================= */

fn part_two(input: String) {
    let mut max_thruster_signal = 0;

    for a in 5..=9 {
        for b in 5..=9 {
            for c in 5..=9 {
                for d in 5..=9 {
                    for e in 5..=9 {
                        let mut phase_sequence = vec![a, b, c, d, e];
                        phase_sequence.reverse();
                        if phase_sequence
                            .clone()
                            .into_iter()
                            .collect::<HashSet<i32>>()
                            .len()
                            != 5
                        {
                            continue;
                        }

                        let base_program = Program::from_str(&input);
                        let mut state = repeat(base_program.state).take(5).collect::<Vec<_>>();

                        let mut current_program = 0;
                        let mut next_input = 0;

                        let thruster_signal = loop {
                            let mut program = Program::from_str("0");

                            if let Some(i) = phase_sequence.pop() {
                                program.push(i);
                            }

                            program
                                .set_state(&state[current_program])
                                .push(next_input)
                                .run();

                            let output = program.output().pop().unwrap();
                            if current_program == 4 && program.halted {
                                break output;
                            } else {
                                state[current_program] = program.state.clone();
                                current_program = (current_program + 1) % 5;
                                next_input = output;
                            }
                        };

                        max_thruster_signal = max(max_thruster_signal, thruster_signal);
                    }
                }
            }
        }
    }

    println!(
        "The max thruster signal produced is {}",
        max_thruster_signal
    );
}

/* Main + Input
================================================= */

use aoc_util::{aoc, AOCParams};

fn main() {
    aoc(&part_one, &part_two, AOCParams::new(false, None));
}
