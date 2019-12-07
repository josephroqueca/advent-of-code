use std::collections::VecDeque;
use std::iter::FromIterator;

#[derive(PartialEq, Debug)]
enum OpCode {
    Add,
    Multiply,
    Input,
    Output,
    JumpIfTrue,
    JumpIfFalse,
    LessThan,
    EqualTo,
    Halt,
}

impl OpCode {
    fn from(code: i32) -> OpCode {
        match code {
            1 => OpCode::Add,
            2 => OpCode::Multiply,
            3 => OpCode::Input,
            4 => OpCode::Output,
            5 => OpCode::JumpIfTrue,
            6 => OpCode::JumpIfFalse,
            7 => OpCode::LessThan,
            8 => OpCode::EqualTo,
            99 => OpCode::Halt,
            _ => unreachable!(),
        }
    }

    fn jump_after_instruction(&self) -> usize {
        match self {
            OpCode::Add | OpCode::Multiply | OpCode::LessThan | OpCode::EqualTo => 4,
            OpCode::Input | OpCode::Output => 2,
            OpCode::JumpIfTrue | OpCode::JumpIfFalse => 0,
            OpCode::Halt => 0,
        }
    }
}

#[derive(Debug)]
enum ParameterMode {
    Position,
    Immediate,
}

impl ParameterMode {
    fn from(code: i32) -> ParameterMode {
        match code {
            0 => ParameterMode::Position,
            1 => ParameterMode::Immediate,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
struct Instruction {
    opcode: OpCode,
    parameter_mode: (ParameterMode, ParameterMode, ParameterMode),
}

impl Instruction {
    fn from(code: i32) -> Instruction {
        Instruction {
            opcode: OpCode::from(code % 100),
            parameter_mode: (
                ParameterMode::from(code % 1000 / 100),
                ParameterMode::from(code % 10000 / 1000),
                ParameterMode::from(code % 100000 / 10000),
            ),
        }
    }
}

#[derive(Debug)]
pub struct Program {
    pub memory: Vec<i32>,
    position: usize,
    input: VecDeque<i32>,
    output: VecDeque<i32>,
    total_run: i32,
}

impl Program {
    pub fn from_str(s: &str) -> Program {
        Program {
            memory: s.split(",").map(|x| x.parse::<i32>().unwrap()).collect(),
            position: 0,
            input: VecDeque::new(),
            output: VecDeque::new(),
            total_run: 0,
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
        loop {
            let position = self.position;
            self.total_run += 1;

            // println!("{:?}", self.memory);
            // if self.total_run > 20 {
            //     return Ok(0);
            // }

            let instruction = Instruction::from(self.get(position, &ParameterMode::Immediate));

            // println!("{:?}", instruction);
            // println!("{:?}", self);

            match instruction.opcode {
                OpCode::Add => {
                    self.perform_basic_operation(position, &instruction, |x, y| Some(x + y));
                }
                OpCode::Multiply => {
                    self.perform_basic_operation(position, &instruction, |x, y| Some(x * y));
                }
                OpCode::JumpIfFalse => {
                    self.jump(position, &instruction, false);
                }
                OpCode::JumpIfTrue => {
                    self.jump(position, &instruction, true);
                }
                OpCode::LessThan => self.perform_basic_operation(position, &instruction, |x, y| {
                    if x < y {
                        Some(1)
                    } else {
                        None
                    }
                }),
                OpCode::EqualTo => self.perform_basic_operation(position, &instruction, |x, y| {
                    if x == y {
                        Some(1)
                    } else {
                        None
                    }
                }),
                OpCode::Input => {
                    self.read_input(position, &instruction);
                }
                OpCode::Output => {
                    self.print_output(position, &instruction);
                }
                OpCode::Halt => break,
            };

            // println!("Jumping {:}", instruction.opcode.jump_after_instruction());
            self.position += instruction.opcode.jump_after_instruction();
        }

        Ok(self.get(0, &ParameterMode::Immediate))
    }

    fn read_input(&mut self, position: usize, instruction: &Instruction) {
        let next = self.input.pop_front().unwrap();
        self.set_internal(position + 1, &instruction.parameter_mode.0, next);
    }

    fn print_output(&mut self, position: usize, _instruction: &Instruction) {
        self.output
            .push_back(self.get(position + 1, &ParameterMode::Position));
    }

    fn jump(&mut self, position: usize, instruction: &Instruction, jump_if_true: bool) {
        let value = self.get(position + 1, &instruction.parameter_mode.0);
        if jump_if_true && value != 0 {
            self.position = self.get(position + 2, &instruction.parameter_mode.1) as usize;
        } else if jump_if_true == false && value == 0 {
            self.position = self.get(position + 2, &instruction.parameter_mode.1) as usize;
        }
    }

    fn perform_basic_operation<P>(
        &mut self,
        position: usize,
        instruction: &Instruction,
        operation: P,
    ) where
        P: Fn(i32, i32) -> Option<i32>,
    {
        // println!("{:}", position);

        // println!("{:}", self.get(position + 1, &instruction.parameter_mode.0));
        // println!("{:}", self.get(position + 2, &instruction.parameter_mode.1));
        // println!("{:}", self.get(position + 3, &instruction.parameter_mode.2));

        match operation(
            self.get(position + 1, &instruction.parameter_mode.0),
            self.get(position + 2, &instruction.parameter_mode.1),
        ) {
            Some(ref value) => {
                self.set_internal(position + 3, &instruction.parameter_mode.2, *value)
            }
            None => self,
        };
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
