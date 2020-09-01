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

macro_rules! byte_enum {

    (@step $_idx:expr, $name:ident, $_byte:ident, []) => {
        None as Option<$name>
    };

    (@step $idx:expr, $name:ident, $byte:ident, [$head:ident, $($tail:ident,)*]) => {
        if $byte == $idx {
            return Some($name::$head);
        }
        byte_enum!(@step $idx + 1u8, $name, $byte, [$($tail,)*]);
    };

    ($name:ident, [ $( $var: ident ),+ ] ) => {
        /// Op is the first byte in an Instruction, followd by an optional number of variable-width operands.
        #[derive(Debug, Clone, Copy, PartialEq)]
        #[repr(u8)]
        pub enum $name {
            $($var,)+
        }

        impl $name {
            pub fn lookup_op(byte: u8) -> Option<$name> {
                byte_enum!(@step 0u8, $name, byte, [$($var,)+]);
                None // need to have this because it can't quite expand the macro at this point.
            }
         }
    };
}

byte_enum!(
    Op,
    [
        Constant,
        Add,
        Sub,
        Mul,
        Div,
        True,
        False,
        Equal,
        NotEqual,
        GreaterThan,
        Minus,
        Bang,
        JumpNotTruthy,
        Jump,
        Pop,
        Null,
        SetGlobal,
        GetGlobal,
        Array,
        Hash,
        Index
    ]
);

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
            Op::Equal => "OpEqual",
            Op::NotEqual => "OpNotEqual",
            Op::GreaterThan => "OpGreaterThan",
            Op::Minus => "OpMinus",
            Op::Bang => "OpBang",
            Op::JumpNotTruthy => "OpJumpNotTruthy",
            Op::Jump => "OpJump",
            Op::Pop => "OpPop",
            Op::Null => "OpNull",
            Op::SetGlobal => "OpSetGlobal",
            Op::GetGlobal => "OpGetGlobal",
            Op::Array => "OpArray",
            Op::Hash => "OpHash",
            Op::Index => "OpIndex",
        }
    }

    /// Returns the number of operands (by the length of the result). Each entry is the width of the operands
    pub fn operand_widths(&self) -> Vec<u8> {
        match self {
            Op::Constant
            | Op::JumpNotTruthy
            | Op::Jump
            | Op::SetGlobal
            | Op::GetGlobal
            | Op::Array // This limits an Array to only contain (1 << 16) -1 = 65535 elements in an array
            | Op::Hash => vec![2],
            Op::Add
            | Op::Sub
            | Op::Mul
            | Op::Div
            | Op::True
            | Op::False
            | Op::Equal
            | Op::NotEqual
            | Op::GreaterThan
            | Op::Minus
            | Op::Bang
            | Op::Pop
            | Op::Null
            | Op::Index => vec![],
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
