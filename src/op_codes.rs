use crate::cpu::AddressingMode;
use std::collections::HashMap;

pub struct OpCode {
    pub code: u8,
    pub mnemonic: &'static str,
    pub len: u8,
    pub cycles: u8,
    pub address_mode: AddressingMode,
}

impl OpCode {
    pub fn new(
        code: u8,
        mnemonic: &'static str,
        len: u8,
        cycles: u8,
        address_mode: AddressingMode,
    ) -> Self {
        OpCode {
            code,
            mnemonic,
            len,
            cycles,
            address_mode,
        }
    }
}

lazy_static! {
  pub static ref CPU_OPS_CODES: Vec<OpCode> = vec![
    OpCode::new(0x00, "BRK", 1, 7, AddressingMode::None),
    OpCode::new(0xaa, "TAX", 1, 2, AddressingMode::None),
    OpCode::new(0xe8, "INX", 1, 2, AddressingMode::None),

    OpCode::new(0xa9, "LDA", 2, 2, AddressingMode::Immediate),
    OpCode::new(0xa5, "LDA", 2, 3, AddressingMode::ZeroPage),
    OpCode::new(0xb5, "LDA", 2, 4, AddressingMode::ZeroPage_X),
    OpCode::new(0xad, "LDA", 3, 4, AddressingMode::Absolute),
    OpCode::new(0xbd, "LDA", 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_X),
    OpCode::new(0xb9, "LDA", 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_Y),
    OpCode::new(0xa1, "LDA", 2, 6, AddressingMode::Indirect_X),
    OpCode::new(0xb1, "LDA", 2, 5/*+1 if page crossed*/, AddressingMode::Indirect_Y),

    OpCode::new(0x85, "STA", 2, 3, AddressingMode::ZeroPage),
    OpCode::new(0x95, "STA", 2, 4, AddressingMode::ZeroPage_X),
    OpCode::new(0x8d, "STA", 3, 4, AddressingMode::Absolute),
    OpCode::new(0x9d, "STA", 3, 5, AddressingMode::Absolute_X),
    OpCode::new(0x99, "STA", 3, 5, AddressingMode::Absolute_Y),
    OpCode::new(0x81, "STA", 2, 6, AddressingMode::Indirect_X),
    OpCode::new(0x91, "STA", 2, 6, AddressingMode::Indirect_Y),

    OpCode::new(0x29, "AND", 2, 2, AddressingMode::Immediate),
    OpCode::new(0x25, "AND", 2, 3, AddressingMode::ZeroPage),
    OpCode::new(0x35, "AND", 2, 4, AddressingMode::ZeroPage_X),
    OpCode::new(0x2D, "AND", 3, 4, AddressingMode::Absolute),
    OpCode::new(0x3D, "AND", 3, 4, /* (+1 if page crossed) */	AddressingMode::Absolute_X),
    OpCode::new(0x39, "AND", 3, 4, /* (+1 if page crossed) */	AddressingMode::Absolute_Y),
    OpCode::new(0x21, "AND", 2, 6, AddressingMode::Indirect_X),
    OpCode::new(0x31, "AND", 2, 5, /* (+1 if page crossed) */	AddressingMode::Indirect_Y),

    OpCode::new(0x0A, "ASL", 1, 2, Accumulator),
    OpCode::new(0x06, "ASL", 2, 5, Zero Page),
    OpCode::new(0x16, "ASL", 2, 6, Zero Page,X),
    OpCode::new(0x0E, "ASL", 3, 6, Absolute),
    OpCode::new(0x1E, "ASL", 3, 7, Absolute,X),
    
  ];

  pub static ref OPCODES_MAP: HashMap<u8, &'static OpCode> = {
    let mut map = HashMap::new();
    for cpuop in &*CPU_OPS_CODES {
      map.insert(cpuop.code, cpuop);
    }
    map
  };
}
