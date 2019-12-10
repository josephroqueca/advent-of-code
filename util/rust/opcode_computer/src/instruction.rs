#[derive(PartialEq, Debug)]
pub enum OpCode {
    Add,
    Multiply,
    Input,
    Output,
    JumpIfTrue,
    JumpIfFalse,
    LessThan,
    EqualTo,
    RelativeBaseOffset,
    Halt,
}

impl OpCode {
    pub fn from(code: i32) -> OpCode {
        match code {
            1 => OpCode::Add,
            2 => OpCode::Multiply,
            3 => OpCode::Input,
            4 => OpCode::Output,
            5 => OpCode::JumpIfTrue,
            6 => OpCode::JumpIfFalse,
            7 => OpCode::LessThan,
            8 => OpCode::EqualTo,
            9 => OpCode::RelativeBaseOffset,
            99 => OpCode::Halt,
            _ => unreachable!(),
        }
    }

    pub fn jump_after_instruction(&self) -> usize {
        match self {
            OpCode::Add | OpCode::Multiply | OpCode::LessThan | OpCode::EqualTo => 4,
            OpCode::Input | OpCode::Output | OpCode::RelativeBaseOffset => 2,
            OpCode::JumpIfTrue | OpCode::JumpIfFalse => 0,
            OpCode::Halt => 0,
        }
    }
}

#[derive(Debug)]
pub enum ParameterMode {
    Position,
    Immediate,
    Relative,
}

impl ParameterMode {
    pub fn from(code: i32) -> ParameterMode {
        match code {
            0 => ParameterMode::Position,
            1 => ParameterMode::Immediate,
            2 => ParameterMode::Relative,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub struct Instruction {
    pub opcode: OpCode,
    pub parameter_mode: (ParameterMode, ParameterMode, ParameterMode),
}

impl Instruction {
    pub fn from(code: i32) -> Instruction {
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
