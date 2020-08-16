use byteorder::{BigEndian, WriteBytesExt};

/// Instructions is a stream of bytecode instructions
pub type Instructions = Vec<u8>;

/// Op is the first byte in an Instruction, followd by an optional number of variable-width operands.
#[repr(u8)]
#[derive(Debug)]
pub enum Op {
    Constant,
}

impl Op {
    /// Returns the human-readable name of the Op code
    pub fn name(&self) -> &str {
        match self {
            Op::Constant => "OpConstant",
        }
    }

    /// Returns the number of operands (by the length of the result). Each entry is the width of the operands
    pub fn operand_widths(&self) -> Vec<u8> {
        match self {
            Op::Constant => vec![2],
        }
    }
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
    fn make() {
        let tests = vec![(
            Op::Constant,
            vec![65534],
            vec![Op::Constant as u8, 255u8, 254u8],
        )];

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
