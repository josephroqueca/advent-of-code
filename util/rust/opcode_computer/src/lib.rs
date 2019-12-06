#[derive(Debug)]
enum OpCode {
    Add,
    Multiply,
    Input,
    Output,
    Halt,
}

impl OpCode {
    fn from(code: i32) -> OpCode {
        match code {
            1 => OpCode::Add,
            2 => OpCode::Multiply,
            3 => OpCode::Input,
            4 => OpCode::Output,
            99 => OpCode::Halt,
            _ => unreachable!(),
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

pub struct Program {
    pub memory: Vec<i32>,
}

impl Program {
    pub fn from_str(s: &str) -> Program {
        Program {
            memory: s.split(",").map(|x| x.parse::<i32>().unwrap()).collect(),
        }
    }

    pub fn run(&mut self) -> Result<i32, ()> {
        let mut position = 0;

        loop {
            let instruction = Instruction::from(self.get(position, ParameterMode::Immediate));
            match instruction.opcode {
                OpCode::Add => {
                    self.perform_basic_operation(position, instruction, |x, y| x + y);
                    position += 4
                }
                OpCode::Multiply => {
                    self.perform_basic_operation(position, instruction, |x, y| x * y);
                    position += 4
                }
                OpCode::Input => (),
                OpCode::Output => (),
                OpCode::Halt => break Ok(self.get(0, ParameterMode::Immediate)),
            }
        }
    }

    fn perform_basic_operation<P>(
        &mut self,
        position: usize,
        instruction: Instruction,
        operation: P,
    ) where
        P: Fn(i32, i32) -> i32,
    {
        self.set_internal(
            position + 3,
            instruction.parameter_mode.2,
            operation(
                self.get(position + 1, instruction.parameter_mode.0),
                self.get(position + 2, instruction.parameter_mode.1),
            ),
        );
    }

    pub fn set(&mut self, position: usize, value: i32) -> &mut Program {
        self.set_internal(position, ParameterMode::Immediate, value)
    }

    fn set_internal(&mut self, position: usize, mode: ParameterMode, value: i32) -> &mut Program {
        match mode {
            ParameterMode::Position => {
                let immediate_position = self.get(position, ParameterMode::Immediate) as usize;
                self.memory[immediate_position] = value
            }
            ParameterMode::Immediate => self.memory[position] = value,
        }
        self
    }

    fn get(&self, position: usize, mode: ParameterMode) -> i32 {
        match mode {
            ParameterMode::Position => {
                self.memory[self.get(position, ParameterMode::Immediate) as usize]
            }
            ParameterMode::Immediate => self.memory[position],
        }
    }
}
