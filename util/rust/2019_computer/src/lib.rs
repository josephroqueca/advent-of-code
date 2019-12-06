enum OpCode {
    Add,
    Multiply,
    Halt,
}

impl OpCode {
    fn from_i32(code: i32) -> OpCode {
        match code {
            1 => OpCode::Add,
            2 => OpCode::Multiply,
            99 => OpCode::Halt,
            _ => unreachable!(),
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
            match OpCode::from_i32(self.get(position)) {
                OpCode::Add => {
                    self.set(
                        self.get(position + 3) as usize,
                        self.get(self.get(position + 1) as usize)
                            + self.get(self.get(position + 2) as usize),
                    );
                    position += 4
                }
                OpCode::Multiply => {
                    self.set(
                        self.get(position + 3) as usize,
                        self.get(self.get(position + 1) as usize)
                            * self.get(self.get(position + 2) as usize),
                    );
                    position += 4
                }
                OpCode::Halt => break Ok(self.get(0)),
            }
        }
    }

    pub fn set(&mut self, position: usize, value: i32) -> &mut Program {
        self.memory[position] = value;
        self
    }

    pub fn get(&self, position: usize) -> i32 {
        self.memory[position]
    }
}
