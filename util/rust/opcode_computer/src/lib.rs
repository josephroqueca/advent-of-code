mod instruction;

use instruction::Instruction;
use instruction::OpCode;
use instruction::ParameterMode;

use std::collections::VecDeque;
use std::iter::FromIterator;

#[derive(Debug, Clone)]
pub struct State {
    original_memory: Vec<i32>,
    memory: Vec<i32>,
    position: usize,
}

impl State {
    fn reset(&mut self) {
        self.memory = self.original_memory.clone();
        self.position = 0;
    }
}

#[derive(Debug, Clone)]
pub struct Program {
    // pub memory: Vec<i32>,
    pub halted: bool,
    pub state: State,
    // position: usize,
    input: VecDeque<i32>,
    output: VecDeque<i32>,
}

impl Program {
    pub fn from_str(s: &str) -> Program {
        let parsed: Vec<i32> = s.split(",").map(|x| x.parse::<i32>().unwrap()).collect();
        Program {
            // memory: parsed,
            // position: 0,
            input: VecDeque::new(),
            output: VecDeque::new(),
            halted: false,
            state: State {
                original_memory: parsed.clone(),
                memory: parsed,
                position: 0,
            },
        }
    }

    pub fn push(&mut self, input: i32) -> &mut Program {
        self.input.push_back(input);
        self
    }

    pub fn output(&self) -> Vec<i32> {
        Vec::from_iter(self.output.iter().map(|x| x.clone()))
    }

    pub fn reset(&mut self) -> &mut Program {
        self.state.reset();
        self.output.clear();
        self.halted = false;
        self
    }

    pub fn run(&mut self) -> &mut Program {
        if self.halted {
            return self;
        }

        loop {
            let position = self.state.position;
            let instruction =
                Instruction::from(self.get_internal(position, &ParameterMode::Immediate));

            // println!(
            //     "Position: {}\nInstruction: {:?}\nMemory: {:?}",
            //     position, instruction, self.memory
            // );

            match instruction.opcode {
                OpCode::Add => {
                    self.perform_basic_operation(position, &instruction, |x, y| x + y);
                }
                OpCode::Multiply => {
                    self.perform_basic_operation(position, &instruction, |x, y| x * y);
                }
                OpCode::JumpIfFalse => {
                    self.jump(position, &instruction, false);
                }
                OpCode::JumpIfTrue => {
                    self.jump(position, &instruction, true);
                }
                OpCode::LessThan => {
                    self.perform_basic_operation(
                        position,
                        &instruction,
                        |x, y| {
                            if x < y {
                                1
                            } else {
                                0
                            }
                        },
                    )
                }
                OpCode::EqualTo => {
                    self.perform_basic_operation(
                        position,
                        &instruction,
                        |x, y| {
                            if x == y {
                                1
                            } else {
                                0
                            }
                        },
                    )
                }
                OpCode::Input => match self.read_input() {
                    Some(ref i) => {
                        self.set_internal(position + 1, &instruction.parameter_mode.0, *i);
                    }
                    None => break,
                },
                OpCode::Output => {
                    self.print_output(position, &instruction);
                }
                OpCode::Halt => {
                    self.halted = true;
                    break;
                }
            };

            self.state.position += instruction.opcode.jump_after_instruction();
        }

        self
    }

    fn read_input(&mut self) -> Option<i32> {
        match self.input.pop_front() {
            Some(ref i) => Some(*i),
            None => None,
        }
    }

    fn print_output(&mut self, position: usize, instruction: &Instruction) {
        self.output
            .push_back(self.get_internal(position + 1, &instruction.parameter_mode.0));
    }

    fn jump(&mut self, position: usize, instruction: &Instruction, jump_if_true: bool) {
        let value = self.get_internal(position + 1, &instruction.parameter_mode.0);
        if (jump_if_true && value != 0) || (!jump_if_true && value == 0) {
            self.state.position =
                self.get_internal(position + 2, &instruction.parameter_mode.1) as usize;
        } else {
            self.state.position += 3;
        }
    }

    fn perform_basic_operation<P>(
        &mut self,
        position: usize,
        instruction: &Instruction,
        operation: P,
    ) where
        P: Fn(i32, i32) -> i32,
    {
        self.set_internal(
            position + 3,
            &instruction.parameter_mode.2,
            operation(
                self.get_internal(position + 1, &instruction.parameter_mode.0),
                self.get_internal(position + 2, &instruction.parameter_mode.1),
            ),
        );
    }

    pub fn set(&mut self, position: usize, value: i32) -> &mut Program {
        self.set_internal(position, &ParameterMode::Immediate, value)
    }

    fn set_internal(&mut self, position: usize, mode: &ParameterMode, value: i32) -> &mut Program {
        match mode {
            ParameterMode::Position => {
                let immediate_position =
                    self.get_internal(position, &ParameterMode::Immediate) as usize;
                self.state.memory[immediate_position] = value
            }
            ParameterMode::Immediate => self.state.memory[position] = value,
        }
        self
    }

    pub fn get(&self, position: usize) -> i32 {
        self.get_internal(position, &ParameterMode::Immediate)
    }

    fn get_internal(&self, position: usize, mode: &ParameterMode) -> i32 {
        match mode {
            ParameterMode::Position => {
                self.state.memory[self.get_internal(position, &ParameterMode::Immediate) as usize]
            }
            ParameterMode::Immediate => self.state.memory[position],
        }
    }

    pub fn set_state(&mut self, state: &State) -> &mut Program {
        self.state = state.clone();
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use aoc_util::read_file;

    #[test]
    fn test_small_programs() {
        [
            (
                "1,9,10,3,2,3,11,0,99,30,40,50",
                vec![3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50],
            ),
            ("1,0,0,0,99", vec![2, 0, 0, 0, 99]),
            ("2,3,0,3,99", vec![2, 3, 0, 6, 99]),
            ("2,4,4,5,99,0", vec![2, 4, 4, 5, 99, 9801]),
            ("1,1,1,4,99,5,6,0,99", vec![30, 1, 1, 4, 2, 5, 6, 0, 99]),
            ("1002,4,3,4,33", vec![1002, 4, 3, 4, 99]),
            ("1101,100,-1,4,0", vec![1101, 100, -1, 4, 99]),
        ]
        .iter()
        .for_each(|(inital_state, final_state)| {
            assert_eq!(
                Program::from_str(inital_state).run().state.memory,
                final_state.to_vec()
            );
        })
    }

    #[test]
    fn test_set_memory() {
        let program = Program::from_str("1,0,0,0,99");

        assert_eq!(
            Program::from_str("2,3,0,3,99")
                .set_state(&program.state)
                .state
                .memory,
            vec![1, 0, 0, 0, 99]
        );
    }

    #[test]
    fn test_halting() {
        assert_eq!(Program::from_str("1,0,0,0,99").run().halted, true);
        assert_eq!(Program::from_str("1,0,0,0,99").run().reset().halted, false);
    }

    #[test]
    fn test_day_2_part_1() {
        assert_eq!(
            Program::from_str(&read_file("./fixtures/day2.txt"))
                .set(1, 12)
                .set(2, 2)
                .run()
                .get(0),
            3562624
        )
    }

    #[test]
    fn test_day_2_part_2() {
        assert_eq!(
            Program::from_str(&read_file("./fixtures/day2.txt"))
                .set(1, 82)
                .set(2, 98)
                .run()
                .get(0),
            19690720
        )
    }

    #[test]
    fn test_day_5_part_1() {
        assert_eq!(
            Program::from_str(&read_file("./fixtures/day5.txt"))
                .push(1)
                .run()
                .output()
                .pop()
                .unwrap(),
            2845163
        )
    }

    #[test]
    fn test_day_5_part_2() {
        assert_eq!(
            Program::from_str(&read_file("./fixtures/day5.txt"))
                .push(5)
                .run()
                .output()
                .pop()
                .unwrap(),
            9436229
        )
    }

    #[test]
    fn setting_memory_not_reset() {
        let mut program = Program::from_str("1,0,0,0,99");

        assert_eq!(program.run().get(0), 2);
        assert_eq!(program.reset().set(0, 2).run().get(0), 4);
    }

    #[test]
    fn test_equals_eight_position_mode() {
        let mut equals_eight = Program::from_str("3,9,8,9,10,9,4,9,99,-1,8");

        assert_eq!(equals_eight.push(1).run().output(), vec![0]);
        assert_eq!(equals_eight.reset().push(8).run().output(), vec![1]);
    }

    #[test]
    fn test_equals_eight_immediate_mode() {
        let mut equals_eight = Program::from_str("3,3,1108,-1,8,3,4,3,99");

        assert_eq!(equals_eight.push(1).run().output(), vec![0]);
        assert_eq!(equals_eight.reset().push(8).run().output(), vec![1]);
    }

    #[test]
    fn test_less_than_eight_position_mode() {
        let mut less_than_eight = Program::from_str("3,9,7,9,10,9,4,9,99,-1,8");

        assert_eq!(less_than_eight.push(1).run().output(), vec![1]);
        assert_eq!(less_than_eight.reset().push(10).run().output(), vec![0]);
    }

    #[test]
    fn test_less_than_eight_immediate_mode() {
        let mut less_than_eight = Program::from_str("3,3,1107,-1,8,3,4,3,99");

        assert_eq!(less_than_eight.push(1).run().output(), vec![1]);
        assert_eq!(less_than_eight.reset().push(10).run().output(), vec![0]);
    }

    #[test]
    fn test_jump_if_zero_position_mode() {
        let mut jump_if_zero = Program::from_str("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9");

        assert_eq!(jump_if_zero.push(1).run().output(), vec![1]);
        assert_eq!(jump_if_zero.reset().push(0).run().output(), vec![0]);
    }

    #[test]
    fn test_jump_if_zero_immediate_mode() {
        let mut jump_if_zero = Program::from_str("3,3,1105,-1,9,1101,0,0,12,4,12,99,1");

        assert_eq!(jump_if_zero.push(1).run().output(), vec![1]);
        assert_eq!(jump_if_zero.reset().push(0).run().output(), vec![0]);
    }

    #[test]
    fn compare_to_eight() {
        let mut compare_to_eight = Program::from_str("3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99");

        assert_eq!(compare_to_eight.push(7).run().output(), vec![999]);
        assert_eq!(compare_to_eight.reset().push(8).run().output(), vec![1000]);
        assert_eq!(compare_to_eight.reset().push(9).run().output(), vec![1001]);
    }

    #[test]
    fn max_thruster_signal_1() {
        let phase_sequence = [4, 3, 2, 1, 0];

        let max_thruster_signal = phase_sequence.iter().fold(0, |acc, &phase_setting| {
            Program::from_str("3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0")
                .push(phase_setting)
                .push(acc)
                .run()
                .output()
                .pop()
                .unwrap()
        });

        assert_eq!(max_thruster_signal, 43210);
    }

    #[test]
    fn max_thruster_signal_2() {
        let phase_sequence = [0, 1, 2, 3, 4];

        let max_thruster_signal = phase_sequence.iter().fold(0, |acc, &phase_setting| {
            Program::from_str(
                "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0",
            )
            .push(phase_setting)
            .push(acc)
            .run()
            .output()
            .pop()
            .unwrap()
        });

        assert_eq!(max_thruster_signal, 54321);
    }

    #[test]
    fn max_thruster_signal_3() {
        let phase_sequence = [1, 0, 4, 3, 2];

        let max_thruster_signal = phase_sequence.iter().fold(0, |acc, &phase_setting| {
            Program::from_str(
                "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0",
            )
            .push(phase_setting)
            .push(acc)
            .run()
            .output()
            .pop()
            .unwrap()
        });

        assert_eq!(max_thruster_signal, 65210);
    }
}
