use byteorder::{BigEndian, ByteOrder, WriteBytesExt};

/// Instructions is a stream of bytecode instructions
pub type Instructions = Vec<u8>;

/// InstructionsFns is a collection of functions for working with Instructions.
/// We can't implement fmt::Display for a type, so we have this instead.
trait InstructionsFns {
    fn to_string(&self) -> String;
}

impl InstructionsFns for Instructions {
    fn to_string(&self) -> String {
        let mut result = String::new();

        let mut i = 0;

        while i < self.len() {
            let op_code = self[i];
            if let Some(op) = Op::lookup_op(op_code) {
                if i > 0 {
                    result.push('\n');
                }

                result.push_str(&format!("{:04} ", i));

                i += 1; // slurp the op_code

                let (operands, offset) = read_operands(&op, &self[i..]);
                result.push_str(op.name());

                for operand in operands {
                    result.push_str(&format!(" {}", operand));
                }

                i += offset; // slurp the operands
            } else {
                return format!("Unknown op code {} at {}", op_code, i);
            }
        }

        result
    }
}

/// Op is the first byte in an Instruction, followd by an optional number of variable-width operands.
#[repr(u8)]
#[derive(Debug)]
pub enum Op {
    Constant,
    Add,
    Sub,
    Mul,
    Div,
    True,
    False,
    Pop,
}

impl Op {
    /// Returns the human-readable name of the Op code
    pub fn name(&self) -> &str {
        match self {
            Op::Constant => "OpConstant",
            Op::Add => "OpAdd",
            Op::Sub => "OpSub",
            Op::Mul => "OpMul",
            Op::Div => "OpDiv",
            Op::True => "OpTrue",
            Op::False => "OpFalse",
            Op::Pop => "OpPop",
        }
    }

    /// Returns the number of operands (by the length of the result). Each entry is the width of the operands
    pub fn operand_widths(&self) -> Vec<u8> {
        match self {
            Op::Constant => vec![2],
            Op::Add => vec![],
            Op::Sub => vec![],
            Op::Mul => vec![],
            Op::Div => vec![],
            Op::True => vec![],
            Op::False => vec![],
            Op::Pop => vec![],
        }
    }

    pub fn lookup_op(op_code: u8) -> Option<Op> {
        match op_code {
            0 => Some(Op::Constant),
            1 => Some(Op::Add),
            2 => Some(Op::Sub),
            3 => Some(Op::Mul),
            4 => Some(Op::Div),
            5 => Some(Op::True),
            6 => Some(Op::False),
            7 => Some(Op::Pop),
            _ => None,
        }
    }
}

fn read_operands(op: &Op, instructions: &[u8]) -> (Vec<usize>, usize) {
    let widths = op.operand_widths();
    let mut operands = Vec::with_capacity(widths.len());
    let mut offset = 0;

    for width in widths {
        match width {
            2 => {
                operands.push(BigEndian::read_u16(&instructions[offset..offset + 2]) as usize);
                offset += 2;
            }
            _ => panic!("width {} not supported for operand", width),
        }
    }

    (operands, offset)
}

pub fn make_instruction(op: Op, operands: &[usize]) -> Vec<u8> {
    let mut instruction = vec![];
    let widths = op.operand_widths();
    instruction.push(op as u8);

    for (o, width) in operands.iter().zip(widths) {
        match width {
            2 => instruction.write_u16::<BigEndian>(*o as u16).unwrap(),
            _ => panic!("unsupported operand width {}", width),
        };
    }

    instruction
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn instructions_string() {
        let instructions: Instructions = vec![
            make_instruction(Op::Add, &vec![]),
            make_instruction(Op::Constant, &vec![2]),
            make_instruction(Op::Constant, &vec![65535]),
        ]
        .concat();

        assert_eq!(
            "0000 OpAdd\n\
                    0001 OpConstant 2\n\
                    0004 OpConstant 65535",
            instructions.to_string()
        );
    }

    #[test]
    fn make() {
        let tests = vec![
            (
                Op::Constant,
                vec![65534],
                vec![Op::Constant as u8, 255u8, 254u8],
            ),
            (Op::Add, vec![], vec![Op::Add as u8]),
        ];

        for (op, operands, expected) in tests {
            let instruction = make_instruction(op, &operands);

            assert_eq!(
                expected.len(),
                instruction.len(),
                "instruction has wrong length. want={}, got={}",
                expected.len(),
                instruction.len()
            );

            for (i, b) in expected.iter().enumerate() {
                assert_eq!(
                    expected[i], instruction[i],
                    "wrong byte at pos {}. want={}, got={}",
                    i, b, instruction[i]
                );
            }
        }
    }
}
