mod instruction;

use instruction::Instruction;
use instruction::OpCode;
use instruction::ParameterMode;

use std::collections::VecDeque;
use std::iter::FromIterator;

#[derive(Debug)]
pub struct Program {
    original_memory: Vec<i32>,
    pub memory: Vec<i32>,
    position: usize,
    input: VecDeque<i32>,
    output: VecDeque<i32>,
}

impl Program {
    pub fn from_str(s: &str) -> Program {
        let parsed: Vec<i32> = s.split(",").map(|x| x.parse::<i32>().unwrap()).collect();
        Program {
            memory: parsed.clone(),
            original_memory: parsed.clone(),
            position: 0,
            input: VecDeque::new(),
            output: VecDeque::new(),
        }
    }

    pub fn push(&mut self, input: i32) -> &mut Program {
        self.input.push_back(input);
        self
    }

    pub fn output(&self) -> Vec<i32> {
        Vec::from_iter(self.output.iter().map(|x| x.clone()))
    }

    pub fn run(&mut self) -> Result<i32, ()> {
        self.memory = self.original_memory.clone();
        self.output.clear();
        self.position = 0;

        loop {
            let position = self.position;
            let instruction = Instruction::from(self.get(position, &ParameterMode::Immediate));

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
                OpCode::Input => {
                    self.read_input(position, &instruction);
                }
                OpCode::Output => {
                    self.print_output(position, &instruction);
                }
                OpCode::Halt => break,
            };

            self.position += instruction.opcode.jump_after_instruction();
        }

        Ok(self.get(0, &ParameterMode::Immediate))
    }

    fn read_input(&mut self, position: usize, instruction: &Instruction) {
        let next = self.input.pop_front().unwrap();
        self.set_internal(position + 1, &instruction.parameter_mode.0, next);
    }

    fn print_output(&mut self, position: usize, instruction: &Instruction) {
        self.output
            .push_back(self.get(position + 1, &instruction.parameter_mode.0));
    }

    fn jump(&mut self, position: usize, instruction: &Instruction, jump_if_true: bool) {
        let value = self.get(position + 1, &instruction.parameter_mode.0);
        if (jump_if_true && value != 0) || (!jump_if_true && value == 0) {
            self.position = self.get(position + 2, &instruction.parameter_mode.1) as usize;
        } else {
            self.position += 3;
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
                self.get(position + 1, &instruction.parameter_mode.0),
                self.get(position + 2, &instruction.parameter_mode.1),
            ),
        );
    }

    pub fn set(&mut self, position: usize, value: i32) -> &mut Program {
        self.set_internal(position, &ParameterMode::Immediate, value)
    }

    fn set_internal(&mut self, position: usize, mode: &ParameterMode, value: i32) -> &mut Program {
        match mode {
            ParameterMode::Position => {
                let immediate_position = self.get(position, &ParameterMode::Immediate) as usize;
                self.memory[immediate_position] = value
            }
            ParameterMode::Immediate => self.memory[position] = value,
        }
        self
    }

    fn get(&self, position: usize, mode: &ParameterMode) -> i32 {
        match mode {
            ParameterMode::Position => {
                self.memory[self.get(position, &ParameterMode::Immediate) as usize]
            }
            ParameterMode::Immediate => self.memory[position],
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
            let mut program = Program::from_str(inital_state);
            let _ = program.run();
            assert_eq!(program.memory, final_state.to_vec());
        })
    }

    #[test]
    fn test_equals_eight_position_mode() {
        let mut equals_eight = Program::from_str("3,9,8,9,10,9,4,9,99,-1,8");
        let _ = equals_eight.push(1).run();
        assert_eq!(equals_eight.output(), vec![0]);

        let _ = equals_eight.push(8).run();
        assert_eq!(equals_eight.output(), vec![1]);
    }

    #[test]
    fn test_equals_eight_immediate_mode() {
        let mut equals_eight = Program::from_str("3,3,1108,-1,8,3,4,3,99");
        let _ = equals_eight.push(1).run();
        assert_eq!(equals_eight.output(), vec![0]);

        let _ = equals_eight.push(8).run();
        assert_eq!(equals_eight.output(), vec![1]);
    }

    #[test]
    fn test_less_than_eight_position_mode() {
        let mut less_than_eight = Program::from_str("3,9,7,9,10,9,4,9,99,-1,8");
        let _ = less_than_eight.push(1).run();
        assert_eq!(less_than_eight.output(), vec![1]);

        let _ = less_than_eight.push(10).run();
        assert_eq!(less_than_eight.output(), vec![0]);
    }

    #[test]
    fn test_less_than_eight_immediate_mode() {
        let mut less_than_eight = Program::from_str("3,3,1107,-1,8,3,4,3,99");
        let _ = less_than_eight.push(1).run();
        assert_eq!(less_than_eight.output(), vec![1]);

        let _ = less_than_eight.push(10).run();
        assert_eq!(less_than_eight.output(), vec![0]);
    }

    #[test]
    fn test_jump_if_zero_position_mode() {
        let mut jump_if_zero = Program::from_str("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9");
        let _ = jump_if_zero.push(1).run();
        assert_eq!(jump_if_zero.output(), vec![1]);

        let _ = jump_if_zero.push(0).run();
        assert_eq!(jump_if_zero.output(), vec![0]);
    }

    #[test]
    fn test_jump_if_zero_immediate_mode() {
        let mut jump_if_zero = Program::from_str("3,3,1105,-1,9,1101,0,0,12,4,12,99,1");
        let _ = jump_if_zero.push(1).run();
        assert_eq!(jump_if_zero.output(), vec![1]);

        let _ = jump_if_zero.push(0).run();
        assert_eq!(jump_if_zero.output(), vec![0]);
    }

    #[test]
    fn compare_to_eight() {
        let mut compare_to_eight = Program::from_str("3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99");
        let _ = compare_to_eight.push(7).run();
        assert_eq!(compare_to_eight.output(), vec![999]);

        let _ = compare_to_eight.push(8).run();
        assert_eq!(compare_to_eight.output(), vec![1000]);

        let _ = compare_to_eight.push(9).run();
        assert_eq!(compare_to_eight.output(), vec![1001]);
    }
}
