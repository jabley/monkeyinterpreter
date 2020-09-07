use byteorder::{BigEndian, ByteOrder, WriteBytesExt};

/// Instructions is a stream of bytecode instructions
pub type Instructions = Vec<u8>;

/// InstructionsFns is a collection of functions for working with Instructions.
/// We can't implement fmt::Display for a type, so we have this instead.
pub trait InstructionsFns {
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
        Index,
        Call, // tell the VM to start executing the Object::CompiledFunction sitting on top of the stack
        ReturnValue, // tell the VM to return the value on top of the stack to the calling context and to resume execution there
        Return, // similar to ReturnValue except there is no explicit return value to return but an implicit Object::Null
        SetLocal,
        GetLocal,
        GetBuiltIn,
        Closure,
        GetFree
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
            Op::Call => "OpCall",
            Op::ReturnValue => "OpReturnValue",
            Op::Return => "OpReturn",
            Op::SetLocal => "OpSetLocal",
            Op::GetLocal => "OpGetLocal",
            Op::GetBuiltIn => "OpGetBuiltIn",
            Op::Closure => "OpClosure",
            Op::GetFree => "OpGetFree",
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
            Op::GetLocal // This limits local bindings to only 1 << 8 == 256 per function.
            | Op::SetLocal
            | Op::Call
            | Op::GetBuiltIn
            | Op::GetFree
            => vec![1],
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
            | Op::Index
            | Op::ReturnValue
            | Op::Return => vec![],
            // Closure has 2 operands. The first (2 bytes wide) is the constant index. This describes
            // where we can find the *object.CompiledFunction that's to be converted into a closure.
            // The second operand (1 byte wide) is the number of free variables for the closure. This
            // limits us to 256 free variables per closure. If you have a closure that has more than 256
            // free variables, you might be writing code which is hard to understand :D
            Op::Closure
            => vec![2, 1],
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
                operands.push(read_u16(instructions, offset));
                offset += 2;
            }
            1 => {
                operands.push(read_u8(instructions, offset));
                offset += 1;
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
            1 => instruction.write_u8(*o as u8).unwrap(),
            _ => panic!("unsupported operand width {}", width),
        };
    }

    instruction
}

pub fn read_u16(instructions: &[u8], start: usize) -> usize {
    BigEndian::read_u16(&instructions[start..start + 2]) as usize
}

pub fn read_u8(instructions: &[u8], start: usize) -> usize {
    instructions[start] as usize
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn instructions_string() {
        let instructions: Instructions = vec![
            make_instruction(Op::Add, &vec![]),
            make_instruction(Op::GetLocal, &[1]),
            make_instruction(Op::Constant, &vec![2]),
            make_instruction(Op::Constant, &vec![65535]),
            make_instruction(Op::Closure, &vec![65535, 255]),
        ]
        .concat();

        assert_eq!(
            "0000 OpAdd\n\
             0001 OpGetLocal 1\n\
             0003 OpConstant 2\n\
             0006 OpConstant 65535\n\
             0009 OpClosure 65535 255",
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
            (Op::GetLocal, vec![255], vec![Op::GetLocal as u8, 255u8]),
            (
                Op::Closure,
                vec![65534, 255],
                vec![Op::Closure as u8, 255, 254, 255],
            ),
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

    #[test]
    fn test_read_operands() {
        let tests = vec![
            (Op::Constant, vec![65535], 2),
            (Op::GetLocal, vec![255], 1),
            (Op::Closure, vec![65535, 255], 3),
        ];

        for (op, operands, bytes_read) in tests {
            let instruction = make_instruction(op, &operands);

            let (operands_read, n) = read_operands(&op, &instruction[1..]);
            assert_eq!(bytes_read, n, "number of bytes read");
            assert_eq!(operands, operands_read, "operand bytes read");
        }
    }
}
