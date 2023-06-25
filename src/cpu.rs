use crate::op_codes;
use std::collections::HashMap;
use bitflags::bitflags;

const STACK: u16 = 0x0100;
// static PROG_BEGIN: u16 = 0x8000;

bitflags! {
  /// # Status Register (P) http://wiki.nesdev.com/w/index.php/Status_flags
  ///
  ///  7 6 5 4 3 2 1 0
  ///  N V _ B D I Z C
  ///  | |   | | | | +--- Carry Flag
  ///  | |   | | | +----- Zero Flag
  ///  | |   | | +------- Interrupt Disable
  ///  | |   | +--------- Decimal Mode (not used on NES)
  ///  | |   +----------- Break Command
  ///  | +--------------- Overflow Flag
  ///  +----------------- Negative Flag
  ///
  #[derive(Clone, Copy, Debug, PartialEq, Eq)]
  pub struct CpuFlags: u8 {
    const CARRY = 0x01;
    const ZERO = 0x02;
    const INTERRUPT_DISABLE = 0x04;
    const DECIMAL_MODE = 0x08;
    const BREAK = 0x10;
    const BREAK2 = 0x20;
    const OVERFLOW = 0x40;
    const NEGATIVE = 0x80;
  } // 0 0 0 0 _ 0 0 0 0 
}

pub enum StoreSrc {
  RegisterA,
  RegisterX,
  RegisterY
}

pub enum TransferADest {
  RegisterX,
  RegisterY
}

#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum AddressingMode {
    Immediate,
    ZeroPage,
    ZeroPage_X,
    ZeroPage_Y,
    Absolute,
    Absolute_X,
    Absolute_Y,
    Indirect_X,
    Indirect_Y,
    Relative,
    None,
}

#[derive(Clone, Debug)]
pub struct Cpu {
    pub register_x: u8,
    pub register_a: u8,
    pub register_y: u8,
    pub stack_pointer: u8,
    pub program_counter: u16,
    pub status: CpuFlags,
    pub memory: [u8; 0xFFFF], // pub memory: DebugIgnore<[u8; 0xFFFF]>
}

impl Default for Cpu {
    fn default() -> Self {
        Self::new()
    }
}

impl Cpu {
    pub fn new() -> Self {
        Cpu {
            register_x: 0,
            register_a: 0,
            register_y: 0,
            stack_pointer: 0xFD,
            program_counter: 0,
            status: CpuFlags::from_bits_truncate(0b100100),
            memory: [0; 0xFFFF],
        }
    }

    pub fn get_operand_address(&mut self, mode: &AddressingMode) -> u16 {
      match mode {
        AddressingMode::Immediate => self.program_counter,
        AddressingMode::ZeroPage => self.mem_read(self.program_counter) as u16,
        AddressingMode::Absolute => self.mem_read_u16(self.program_counter),
        AddressingMode::ZeroPage_X => {
            let addr = self.mem_read(self.program_counter);
            let addr = addr.wrapping_add(self.register_x) as u16;

            addr
        }
        AddressingMode::ZeroPage_Y => {
            let addr = self.mem_read(self.program_counter);
            let addr = addr.wrapping_add(self.register_y) as u16;

            addr
        }
        AddressingMode::Absolute_X => {
            let base = self.mem_read_u16(self.program_counter);
            let addr = base.wrapping_add(self.register_x as u16);

            addr
        }
        AddressingMode::Absolute_Y => {
            let base = self.mem_read_u16(self.program_counter);
            let addr = base.wrapping_add(self.register_y as u16);

            addr
        }
        AddressingMode::Indirect_X => {
            let base = self.mem_read(self.program_counter);
            let ptr: u8 = base.wrapping_add(self.register_x);
            let lo = self.mem_read(ptr as u16);
            let hi = self.mem_read(ptr.wrapping_add(1) as u16);

            (hi as u16) << 8 | (lo as u16)
        }
        AddressingMode::Indirect_Y => {
            let base = self.mem_read(self.program_counter);

            let lo = self.mem_read(base as u16);
            let hi = self.mem_read((base).wrapping_add(1) as u16);
            let deref_base = (hi as u16) << 8 | (lo as u16);
            let deref = deref_base.wrapping_add(self.register_y as u16);

            deref
        }
        _ => {
            panic!("mode {:?} is not supported", mode);
        }
      }
    }

    pub fn mem_read(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    pub fn mem_write(&mut self, addr: u16, data: u8) {
        self.memory[addr as usize] = data;
    }

    pub fn mem_read_u16(&mut self, addr: u16) -> u16 {
        let lo = self.mem_read(addr) as u16;
        let hi = self.mem_read(addr + 1) as u16;
        (hi << 8) | lo
    }

    pub fn mem_write_u16(&mut self, addr: u16, data: u16) {
        let hi = (data >> 8) as u8;
        let lo = (data & 0xff) as u8;
        self.mem_write(addr, lo);
        self.mem_write(addr + 1, hi);
    }

    pub fn update_zero_neg_flags(&mut self, result: u8) {
      if result == 0 {
        self.status.insert(CpuFlags::ZERO);
      } else {
        self.status.remove(CpuFlags::ZERO);
      }

      if result & CpuFlags::NEGATIVE.bits() != 0 {
        self.status.insert(CpuFlags::NEGATIVE);
      } else {
        self.status.remove(CpuFlags::NEGATIVE);
      }
    }

    pub fn update_carry_flag(&mut self, set: u8) {
      if set > 0 {
        self.status.insert(CpuFlags::CARRY);
      } else {
        self.status.remove(CpuFlags::CARRY);
      }
    }

    pub fn reset(&mut self) {
      self.register_a = 0;
      self.register_x = 0;
      self.register_y = 0;
      self.status = CpuFlags::from_bits_truncate(0b100100);

      self.program_counter = self.mem_read_u16(0xFFFC);
    }

    pub fn load_and_run(&mut self, program: Vec<u8>) {
        self.load(program);
        self.reset();
        self.run();
    }

    pub fn load(&mut self, program: Vec<u8>) {
      // self.memory[0x8000..(0x8000 + program.len())].copy_from_slice(&program);
      // self.mem_write_u16(0xFFFC, 0x8000);
      
      let program_start = 0x0600;
      self.memory[program_start..(program_start + program.len())].copy_from_slice(&program[..]);
      self.mem_write_u16(0xFFFC, program_start as u16);
    }

    pub fn lda(&mut self, mode: &AddressingMode) {
      let addr = self.get_operand_address(mode);
      let data = self.mem_read(addr);

      self.register_a = data;
      self.update_zero_neg_flags(self.register_a);
    }

    pub fn load_into(&mut self, mode: &AddressingMode) -> u8 {
      let addr = self.get_operand_address(mode);
      let data = self.mem_read(addr);

      self.update_zero_neg_flags(data);

      data
    }

    pub fn transfer_a(&mut self, dest: TransferADest) {
      match dest {
        TransferADest::RegisterX => {
          self.register_x = self.register_a;
          self.update_zero_neg_flags(self.register_x);
        }
        TransferADest::RegisterY => {
          self.register_y = self.register_a;
          self.update_zero_neg_flags(self.register_y);
        }
      };
    }

    pub fn and(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);

        self.register_a &= data;

        self.update_zero_neg_flags(self.register_a);
    }

    pub fn lsr_accumlator(&mut self) {
      let val = self.register_a >> 1;

      self.update_carry_flag(self.register_a & 1);
      
      self.register_a = val;
      self.update_zero_neg_flags(self.register_a);
    }

    pub fn lsr(&mut self, mode: &AddressingMode) {
      let addr = self.get_operand_address(mode);
      let data = self.mem_read(addr);

      let val = data >> 1;
      self.update_carry_flag(data & 1);
      self.update_zero_neg_flags(val);

      self.mem_write(addr, val);
    }

    pub fn asl_accumlator(&mut self) {
      let val = self.register_a << 1;
      
      self.update_carry_flag(self.register_a & 0x80);
      
      self.register_a = val;
      self.update_zero_neg_flags(self.register_a);
    }

    pub fn asl(&mut self, mode: &AddressingMode) {
      let addr = self.get_operand_address(mode);
      let data = self.mem_read(addr);

      self.update_carry_flag(data & 0x80);
      let val = data << 1;
      self.update_zero_neg_flags(val);

      self.mem_write(addr, val);
    }

    pub fn branch(&mut self, do_branch: bool) {
      if do_branch {
        let relative_offset = self.mem_read(self.program_counter) as i8;
        let jump_address = self.program_counter
          .wrapping_add(1)
          .wrapping_add(relative_offset as u16);

        self.program_counter = jump_address;
      }
    }

    pub fn bit(&mut self, mode: &AddressingMode) {
      let addr = self.get_operand_address(mode);
      let data = self.mem_read(addr);

      if self.register_a & data == 0 {
        self.status.insert(CpuFlags::ZERO);
      } else {
        self.status.remove(CpuFlags::ZERO);
      }
      
      if data >> 6 == 1 {
        self.status.insert(CpuFlags::OVERFLOW);
      } else {
        self.status.remove(CpuFlags::OVERFLOW);
      }
      
      if data >> 7 == 1 {
        self.status.insert(CpuFlags::NEGATIVE);
      } else {
        self.status.remove(CpuFlags::NEGATIVE);
      }
    }

    pub fn clear(&mut self, flag: CpuFlags) {
      self.status.remove(flag);
    }

    pub fn sbc(&mut self, mode: &AddressingMode) {
      let addr = self.get_operand_address(mode);
      let data = self.mem_read(addr);
      // self.register_a = self.add_to_reg_a((data as i8).wrapping_neg() as u8);
      let not_carry_sb = if self.status.contains(CpuFlags::CARRY) { 0 } else { 1 } as u8;
      self.register_a = self.add_to_reg_a(!data + not_carry_sb)
    }

    fn add_to_reg_a(&mut self, data: u8) -> u8 {
      let sum = data as u16 + self.register_a as u16 + if self.status.contains(CpuFlags::CARRY) { 1 } else { 0 } as u16;

      if sum & 0xFF00 != 0 {
        self.status.insert(CpuFlags::CARRY);
      } else {
        self.status.remove(CpuFlags::CARRY);
      }

      let ops_sign = data >> 7;
      let same_sign = ops_sign & (self.register_a >> 7);

      let result = sum as u8;
      if same_sign == 1 && ops_sign != result >> 7 {
          self.status.insert(CpuFlags::OVERFLOW);
        } else {
          self.status.remove(CpuFlags::OVERFLOW);
      }

      self.update_zero_neg_flags(result);
      
      result
    }

    pub fn ora(&mut self, mode: &AddressingMode) {
      let addr = self.get_operand_address(mode);
      let data = self.mem_read(addr);

      let new_data = self.register_a | data;
      self.update_zero_neg_flags(new_data);

      self.register_a = new_data;
    }

    pub fn adc(&mut self, mode: &AddressingMode) {
      let addr = self.get_operand_address(mode);
      let data = self.mem_read(addr);

      self.register_a = self.add_to_reg_a(data);
    }
    
    pub fn ror(&mut self, mode: &AddressingMode) {
      let addr = self.get_operand_address(mode);
      let mut data = self.mem_read(addr);

      let prev_carry_flag = self.status.contains(CpuFlags::CARRY);
      self.update_carry_flag(data & 1);

      data >>= 1;

      if prev_carry_flag {
        data |= 0x80;
      }
      
      self.update_zero_neg_flags(data);

      self.mem_write(addr, data);
    }
    
    pub fn rol(&mut self, mode: &AddressingMode) {
      let addr = self.get_operand_address(mode);
      let mut data = self.mem_read(addr);

      let prev_carry_flag = self.status.contains(CpuFlags::CARRY);
      
      self.update_carry_flag(data & 0x80); // order here is *not* arbitrary https://www.nesdev.org/obelisk-6502-guide/reference.html#ROL

      data <<= 1;

      if prev_carry_flag {
        data |= 1;
      }
      
      // data = data.rotate_left(1);
      self.update_zero_neg_flags(data);
      self.mem_write(addr, data);
    }

    pub fn cmp(&mut self, mode: &AddressingMode, other: u8) {
      let addr = self.get_operand_address(mode);
      let data = self.mem_read(addr);
      
      let set_carry = if other >= data {
        1
      } else {
        0
      };
      
      self.update_carry_flag(set_carry);
      self.update_zero_neg_flags(other.wrapping_sub(data));
    }

    // fn stack_pop(&mut self) -> u8 {
    //   self.stack_pointer = self.stack_pointer.wrapping_add(1);
    //   self.mem_read((STACK as u16) + self.stack_pointer as u16)
    // }

    // fn stack_push(&mut self, data: u8) {
    //   self.mem_write((STACK as u16) + self.stack_pointer as u16, data);
    //   self.stack_pointer = self.stack_pointer.wrapping_sub(1)
    // }

    pub fn stack_pop(&mut self) -> u8{
      self.stack_pointer = self.stack_pointer.wrapping_add(1);
      // println!("Stack pop: {}", STACK + self.stack_pointer as u16);
      
      self.mem_read(STACK + (self.stack_pointer as u16))
    }
    
    pub fn stack_push(&mut self, data: u8) {
      // println!("Stack push: {}", STACK + self.stack_pointer as u16);
      self.mem_write(STACK + (self.stack_pointer as u16), data);
      self.stack_pointer = self.stack_pointer.wrapping_sub(1);
    }
    
    pub fn stack_pop_u16(&mut self) -> u16 {
      let lo = self.stack_pop() as u16;
      let hi = self.stack_pop() as u16;

      hi << 8 | lo
    }

    pub fn stack_push_u16(&mut self, data: u16) {
      let hi = data >> 8;
      let lo = data & 0xFF;
      self.stack_push(hi as u8);
      self.stack_push(lo as u8);
    }

    pub fn store(&mut self, mode: &AddressingMode, src: StoreSrc) {
      let addr = self.get_operand_address(mode);

      let data = match src {
        StoreSrc::RegisterA => {
          self.register_a
        }
        StoreSrc::RegisterX => {
          self.register_x
        }
        StoreSrc::RegisterY => {
          self.register_y
        }
      };

      self.mem_write(addr, data);
    }

    pub fn run_with_callback<F>(&mut self, mut callback: F)
    where
      F: FnMut(&mut Cpu),
    {
      let opcodes: &HashMap<u8, &'static op_codes::OpCode> = &op_codes::OPCODES_MAP;

      loop {
        let op_code = self.mem_read(self.program_counter);

        // println!("PC: {:#x}, PC byte: {:#x}", &self.program_counter, &op_code);

        self.program_counter += 1;

        let op_code_data = opcodes.get(&op_code)
          .unwrap_or_else(|| panic!("Unknown OPCODE: {:x}.", &op_code));

        let program_counter_state = self.program_counter;

        match op_code {
          0x98 => {
            // TYA
            self.register_a = self.register_y;
            self.update_zero_neg_flags(self.register_a);
          }
          0x9A => {
            // TXS
            self.stack_pointer = self.register_x;
          }
          0x8A => {
            // TXA
            self.register_a = self.register_x;
            self.update_zero_neg_flags(self.register_a);
          }
          0xBA => {
            // TSX
            self.register_x = self.stack_pointer;
            self.update_zero_neg_flags(self.register_x);
          }
          0xAA => { // TAX
            self.transfer_a(TransferADest::RegisterX);
          }
          0xA8 => { // TAY
            self.transfer_a(TransferADest::RegisterY);
          }
          0x86 | 0x96 | 0x8E => {
            // STX
            self.store(&op_code_data.address_mode, StoreSrc::RegisterX);
          }
          0x84 | 0x94 | 0x8C => {
            // STY
            self.store(&op_code_data.address_mode, StoreSrc::RegisterY);
          }
          0x85 | 0x95 | 0x8D | 0x9D | 0x99 | 0x81 | 0x91 => {
            // STA
            self.store(&op_code_data.address_mode, StoreSrc::RegisterA);
          }
          0x78 => {
            // SEI
            self.status.insert(CpuFlags::INTERRUPT_DISABLE);
          }
          0xF8 => {
            // SED
            self.status.insert(CpuFlags::DECIMAL_MODE);
          }
          0x38 => {
            // SEC
            self.status.insert(CpuFlags::CARRY);
          }
          0x60 => {
            // RTS
            self.program_counter = self.stack_pop_u16() + 1;
          }
          0x40 => {
            // RTI
            self.status = CpuFlags::from_bits_truncate(self.stack_pop());
            self.status.remove(CpuFlags::BREAK);
            self.status.insert(CpuFlags::BREAK2);

            self.program_counter = self.stack_pop_u16();
          }
          0x6A => {
            // ROR
            let prev_carry_flag = self.status.contains(CpuFlags::CARRY);

            let mut data = self.register_a;
            self.update_carry_flag(data & 1);

            data >>= 1;

            if prev_carry_flag {
              data |= 0x80;
            }

            self.register_a = data;

            self.update_zero_neg_flags(self.register_a);
          }
          0x66 | 0x76 | 0x6E | 0x7E => {
            self.ror(&op_code_data.address_mode);
          }
          0x2A => {
            // ROL acc
            let prev_carry_flag = self.status.contains(CpuFlags::CARRY);
            let mut data = self.register_a;

            self.update_carry_flag(data & 0x80);

            data <<= 1;
            if prev_carry_flag {
              data |= 1;
            }

            self.register_a = data;
            self.update_zero_neg_flags(self.register_a);
          }
          0x26 | 0x36 | 0x2E | 0x3E => {
            // ROL
            self.rol(&op_code_data.address_mode);
          }
          0x28 => {
            // PLP
            self.status = CpuFlags::from_bits_truncate(self.stack_pop());
            self.status.remove(CpuFlags::BREAK);
            self.status.insert(CpuFlags::BREAK2);
          }
          0x68 => {
            // PLA
            self.register_a = self.stack_pop();
            self.update_zero_neg_flags(self.register_a);
          }
          0x08 => {
            // PHP
            self.status.insert(CpuFlags::BREAK);
            self.status.insert(CpuFlags::BREAK2);
            self.stack_push(self.status.bits());
          }
          0x48 => {
            // PHA
            self.stack_push(self.register_a);
          }
          0xEA => {}
          0x09 | 0x05 | 0x15 | 0x0D | 0x1D | 0x19 | 0x01 | 0x11 => {
            // ORA
            self.ora(&op_code_data.address_mode);
          }
          0xA2 | 0xA6 | 0xB6 | 0xAE | 0xBE => {
            // LDX
            self.register_x = self.load_into(&op_code_data.address_mode)
          }
          0xA0 | 0xA4 | 0xB4 | 0xAC | 0xBC => {
            self.register_y = self.load_into(&op_code_data.address_mode)
          }
          0xA9 | 0xA5 | 0xB5 | 0xAD | 0xBD | 0xB9 | 0xA1 | 0xB1 => {
            // LDA
            self.register_a = self.load_into(&op_code_data.address_mode)
          }
          0x20 => {
            // JSR
            self.stack_push_u16(self.program_counter + 2 - 1);
            let target_address = self.mem_read_u16(self.program_counter);

            self.program_counter = target_address;
          }
          0x4C => {
            // JMP absolute
            self.program_counter = self.mem_read_u16(self.program_counter);
          }
          0x6C => {
            // JMP indirect
            let jump_memory = self.mem_read_u16(self.program_counter);
            let jump_dest = self.mem_read_u16(jump_memory);
            self.program_counter = jump_dest;
          }
          0x49 | 0x45 | 0x55 | 0x4D | 0x5D | 0x59 | 0x41 | 0x51 => {
            // EOR
            let addr = self.get_operand_address(&op_code_data.address_mode);
            let data = self.mem_read(addr);

            self.register_a ^= data;

            self.update_zero_neg_flags(self.register_a);
          }
          0xE6 | 0xF6 | 0xEE | 0xFE => {
            // INC
            let addr = self.get_operand_address(&op_code_data.address_mode);
            let mut data = self.mem_read(addr);

            data = data.wrapping_add(1);
            self.mem_write(addr, data);
            self.update_zero_neg_flags(data);
          }
          0xE8 => {
            // INX
            self.register_x = self.register_x.wrapping_add(1);
            self.update_zero_neg_flags(self.register_x);
          }
          0xC8 => {
            // INY
            self.register_y = self.register_y.wrapping_add(1);
            self.update_zero_neg_flags(self.register_y);
          }
          0xC6 | 0xD6 | 0xCE | 0xDE => {
            // DEC
            let addr = self.get_operand_address(&op_code_data.address_mode);
            let mut data = self.mem_read(addr);

            data = data.wrapping_sub(1);
            self.mem_write(addr, data);
            self.update_zero_neg_flags(data);
          }
          0xCA => {
            // DEX
            self.register_x = self.register_x.wrapping_sub(1);
            self.update_zero_neg_flags(self.register_x);
          }
          0x88 => {
            // DEY
            self.register_y = self.register_y.wrapping_sub(1);
            self.update_zero_neg_flags(self.register_y);
          }
          0x4A => {
            // LSR
            self.lsr_accumlator();
          }
          0x46 | 0x56 | 0x4E | 0x5E => {
            // LSR
            self.lsr(&op_code_data.address_mode);
          }
          0x0A => {
            // ASL
            self.asl_accumlator();
          }
          0x06 | 0x16 | 0x0E | 0x1E => {
            // ASL
            self.asl(&op_code_data.address_mode);
          }
          0x29 | 0x25 | 0x35 | 0x2D | 0x3D | 0x39 | 0x21 | 0x31 => {
            // AND
            self.and(&op_code_data.address_mode);
          }
          0xC9 | 0xC5 | 0xD5 | 0xCD | 0xDD | 0xD9 | 0xC1 | 0xD1 => {
            // CMP
            self.cmp(&op_code_data.address_mode, self.register_a);
          }
          0xE0 | 0xE4 | 0xEC => {
            // CPX
            self.cmp(&op_code_data.address_mode, self.register_x);
          }
          0xC0 | 0xC4 | 0xCC => {
            // CPY
            self.cmp(&op_code_data.address_mode, self.register_y);
          }
          0x24 | 0x2C => {
            // BIT
            self.bit(&op_code_data.address_mode);
          }
          0x50 => {
            // BVC
            self.branch(!self.status.contains(CpuFlags::OVERFLOW));
          }
          0x70 => {
            // BVS
            self.branch(self.status.contains(CpuFlags::OVERFLOW));
          }
          0x30 => {
            // BMI
            self.branch(self.status.contains(CpuFlags::NEGATIVE));
          }
          0x10 => {
            // BPL
            self.branch(!self.status.contains(CpuFlags::NEGATIVE));
          }
          0xD0 => {
            // BNE
            self.branch(!self.status.contains(CpuFlags::ZERO));
          }
          0xF0 => {
            // BEQ
            self.branch(self.status.contains(CpuFlags::ZERO));
          }
          0xB0 => {
            // BCS
            self.branch(self.status.contains(CpuFlags::CARRY));
          }
          0x90 => {
            // BCC
            self.branch(!self.status.contains(CpuFlags::CARRY));
          }
          0x69 | 0x65 | 0x75 | 0x6D | 0x7D | 0x79 | 0x61 | 0x71 => {
            self.adc(&op_code_data.address_mode);
          }
          0xE9 | 0xE5 | 0xF5 | 0xED | 0xFD | 0xF9 | 0xE1 | 0xF1 => {
            self.sbc(&op_code_data.address_mode);
          }
          0x18 => {
            self.clear(CpuFlags::CARRY);
          }
          0xD8 => {
            self.clear(CpuFlags::DECIMAL_MODE);
          }
          0x58 => {
            self.clear(CpuFlags::INTERRUPT_DISABLE);
          }
          0xB8 => {
            self.clear(CpuFlags::OVERFLOW);
          }
          0x00 => {
            // BRK
            return;
          }
          _ => (),
        }

        // println!("\t Exec: {:?}, \n\t Interim PC: {:#x}, New PC: {:#x}", &op_code_data, &self.program_counter, self.program_counter + (op_code_data.len - 1) as u16);

        if program_counter_state == self.program_counter {
            self.program_counter += (op_code_data.len - 1) as u16;
        }

        // println!("\t Final PC: {:#x}", self.program_counter);
        // println!("");
        callback(self);
      }
    }

    pub fn run(&mut self) {
      self.run_with_callback(|_| {});
    }
}

#[cfg(test)]
mod test {
    use super::*;

    // #[test]
    // #[ignore]
    // fn test_bcc() {
    //     let mut cpu = Cpu::new();

    //     cpu.load_and_run(vec![0xa9, 0xFF, 0x69, 0x01, 0x90, 0x42, 0x00])
    // }


    #[test]
    fn test_tsx() {
      let mut cpu = Cpu::new();

      cpu.stack_pointer = 101;

      cpu.load(vec![0xBA, 0x00]);
      cpu.program_counter = cpu.mem_read_u16(0xFFFC);
      cpu.run();

      assert_eq!(cpu.register_x, 101);
    }

    #[test]
    fn test_sta_stx_sty() {
      let mut cpu = Cpu::new();

      cpu.register_a = 1;
      cpu.register_x = 2;
      cpu.register_y = 4;

      cpu.load(vec![0x85, 0x50, 0x86, 0x51, 0x84, 0x52, 0x00]);
      cpu.program_counter = cpu.mem_read_u16(0xFFFC);
      cpu.run();

      assert_eq!(cpu.mem_read(0x50), 1);
      assert_eq!(cpu.mem_read(0x51), 2);
      assert_eq!(cpu.mem_read(0x52), 4);
    }

    #[test]
    fn test_set_flags() {
      let mut cpu = Cpu::new();

      cpu.load_and_run(vec![0x78, 0xF8, 0x38, 0x00]);

      assert!(cpu.status.contains(CpuFlags::DECIMAL_MODE | CpuFlags::INTERRUPT_DISABLE | CpuFlags::CARRY));
    }

    #[test]
    fn test_rts() {
      let mut cpu = Cpu::new();

      cpu.stack_push_u16(0xFF00);
      
      cpu.load(vec![0x60, 0x00]);
      cpu.program_counter = cpu.mem_read_u16(0xFFFC);
      cpu.run();

      assert_eq!(cpu.program_counter, 0xFF02); // this is 2 over PC due to last BRK instruction
    }

    #[test]
    fn test_rti() {
      let mut cpu = Cpu::new();

      cpu.status = CpuFlags::from_bits_truncate(0);
      cpu.stack_push_u16(0xFF00);
      cpu.stack_push(CpuFlags::all().bits());
      
      cpu.load(vec![0x40, 0x00]);
      cpu.program_counter = cpu.mem_read_u16(0xFFFC);
      cpu.run();

      // assert!(cpu.status.contains(CpuFlags::all()));
      assert_eq!(cpu.program_counter, 0xFF00 + 1); // PC will be +1 by program-end due to last BRK instruction
    }

    #[test]
    fn test_ror_acc() {
      let mut cpu = Cpu::new();

      // LDA 0x05, STA 0x50, ROR 0x50, LDA @ 0x50
      cpu.load_and_run(vec![0xA9, 0x05, 0x6A, 0x00]);

      assert!(cpu.status.contains(CpuFlags::CARRY));
      assert!(!cpu.status.contains(CpuFlags::NEGATIVE));
      assert_eq!(cpu.register_a, 0x02);
    }

    #[test]
    fn test_ror() {
      let mut cpu = Cpu::new();

      // LDA 0x22, STA 0x50, ROR 0x50, LDA @ 0x50
      cpu.load_and_run(vec![0xA9, 0x22, 0x85, 0x50, 0x66, 0x50, 0x66, 0x50, 0xA5, 0x50, 0x00]);

      assert!(cpu.status.contains(CpuFlags::CARRY));
      assert!(!cpu.status.contains(CpuFlags::NEGATIVE));
      assert_eq!(cpu.register_a, 0x08);
    }

    #[test]
    fn test_rol_acc() {
      let mut cpu = Cpu::new();

      // LDA 0x41, STA 0x50, ROL 0x50, LDA @ 0x50
      cpu.load_and_run(vec![0xA9, 0x41, 0x2A, 0x00]);

      assert!(!cpu.status.contains(CpuFlags::CARRY));
      assert!(cpu.status.contains(CpuFlags::NEGATIVE));
      assert_eq!(cpu.register_a, 0x82);
    }

    #[test]
    fn test_rol() {
      let mut cpu = Cpu::new();

      // LDA 0x82, STA 0x50, ROL 0x50, LDA @ 0x50
      cpu.load_and_run(vec![0xA9, 0x82, 0x85, 0x50, 0x26, 0x50, 0xA5, 0x50, 0x00]);

      assert!(cpu.status.contains(CpuFlags::CARRY));
      assert!(!cpu.status.contains(CpuFlags::NEGATIVE));
      assert_eq!(cpu.register_a, 0x04);
    }

    #[test]
    fn test_plp() {
      let mut cpu = Cpu::new();

      cpu.status = CpuFlags::from_bits_truncate(0);
      cpu.stack_push(CpuFlags::all().bits());
      
      cpu.load(vec![0x28, 0x00]);
      cpu.program_counter = cpu.mem_read_u16(0xFFFC);
      cpu.run();

      println!("Flags: {:?}", &cpu.status);

      assert!(cpu.status.contains(CpuFlags::from_bits_truncate(0b1110_1111)));
    }

    #[test]
    fn test_pla() {
      let mut cpu = Cpu::new();

      cpu.load_and_run(vec![0xA9, 0xFF, 0x48, 0xA9, 0x00, 0x68, 0x00]);

      assert!(cpu.status.contains(CpuFlags::NEGATIVE));
      assert_eq!(cpu.register_a, 0xFF);
    }

    #[test]
    fn test_php() {
      let mut cpu = Cpu::new();

      cpu.status = CpuFlags::from_bits_truncate(0b1000_0001);
      cpu.register_a = 0;
      cpu.register_x = 0;
      cpu.register_y = 0;
      cpu.load(vec![0x08, 0x00]);
      cpu.program_counter = cpu.mem_read_u16(0xFFFC);
      cpu.run();

      assert_eq!(cpu.stack_pop(), 0b1011_0001 as u8);
    }

    #[test]
    fn test_pha() {
      let mut cpu = Cpu::new();

      cpu.load_and_run(vec![0xA9, 0x04, 0x48, 0x00]);

      assert_eq!(cpu.stack_pop(), 0x04);
    }

    #[test]
    fn test_ora() {
      let mut cpu = Cpu::new();

      cpu.load_and_run(vec![0xA9, 0x81, 0x09, 0x0e, 0x00]);

      assert!(!cpu.status.contains(CpuFlags::OVERFLOW));
      assert!(cpu.status.contains(CpuFlags::NEGATIVE));
      assert_eq!(cpu.register_a, 0x8F);
    }

    #[test]
    fn test_sbc_normal() {
      let mut cpu = Cpu::new();

      // 3 - 1 == 2
      cpu.load_and_run(vec![0xA9, 0x3, 0xE9, 0x01, 0x00]);

      assert!(!cpu.status.contains(CpuFlags::OVERFLOW));
      assert!(!cpu.status.contains(CpuFlags::NEGATIVE));
      assert_eq!(cpu.register_a, 0x02);
    }

    #[test]
    fn test_sbc() {
      let mut cpu = Cpu::new();

      // 0 - 1 == -1
      cpu.load_and_run(vec![0xA9, 0x00, 0xE9, 0x01, 0x00]);

      assert!(!cpu.status.contains(CpuFlags::OVERFLOW));
      assert!(cpu.status.contains(CpuFlags::NEGATIVE));
      assert_eq!(cpu.register_a, 0xFF);
    }

    #[test]
    fn test_sbc_overflow() {
      let mut cpu = Cpu::new();

      // -128 - 1 == 127
      cpu.load_and_run(vec![0xA9, 0x80, 0xE9, 0x01, 0x00]);

      assert!(cpu.status.contains(CpuFlags::OVERFLOW));
      assert!(!cpu.status.contains(CpuFlags::NEGATIVE));
      assert_eq!(cpu.register_a, 127);
    }

    #[test]
    fn test_ldy_ldx_lda() {
      let mut cpu = Cpu::new();

      cpu.load_and_run(vec![0xA9, 0x2, 0xA2, 0x8, 0xA0, 0x6, 0x00]);

      assert_eq!(cpu.register_a, 2);
      assert_eq!(cpu.register_x, 8);
      assert_eq!(cpu.register_y, 6);
    }

    #[test]
    fn test_adc_overflow() {
      let mut cpu = Cpu::new();

      // -128 + -1 == 127
      cpu.load_and_run(vec![0xA9, 0x80, 0x69, (!0x01 as u8)+1, 0x00]);

      assert!(cpu.status.contains(CpuFlags::OVERFLOW));
      assert_eq!(cpu.register_a, 127);
    }

    #[test]
    fn test_adc_carry() {
      let mut cpu = Cpu::new();

      // 208 + 80 == 32
      cpu.load_and_run(vec![0xA9, 0xD0, 0x69, 0x50, 0x00]);

      assert!(cpu.status.contains(CpuFlags::CARRY));
      assert_eq!(cpu.register_a, 0x20);
    }

    #[test]
    fn test_adc() {
      let mut cpu = Cpu::new();

      // 2 + 1 == 3
      cpu.load_and_run(vec![0xa9, 0x02, 0x69, 0x01, 0x00]);
      
      assert!(!cpu.status.contains(CpuFlags::CARRY));
      assert_eq!(cpu.register_a, 0x03);
    }

    #[test]
    fn test_lsr_accumulator() {
        let mut cpu = Cpu::new();

        assert!(!cpu.status.contains(CpuFlags::CARRY));

        // LDA 0x0F, TAX 0x0050, 0xF4 & register_A
        cpu.load_and_run(vec![0xa9, 0x81, 0x4a, 0x00]);

        assert_eq!(cpu.register_a, 0x40);
        assert!(cpu.status.contains(CpuFlags::CARRY));
    }

    #[test]
    fn test_lsr() {
        let mut cpu = Cpu::new();

        assert!(!cpu.status.contains(CpuFlags::CARRY));

        cpu.load_and_run(vec![0xA9, 0x02, 0x85, 0x50, 0x46, 0x50, 0xA5, 0x50, 0x00]);

        assert_eq!(cpu.register_a, 1);
        assert_eq!(cpu.mem_read(0x50), 1);
        assert_eq!(cpu.memory[0x50], 1);
        assert!(!cpu.status.contains(CpuFlags::CARRY));
    }

    #[test]
    fn test_asl_accumulator() {
        let mut cpu = Cpu::new();

        // LDA 0x0F, TAX 0x0050, 0xF4 & register_A
        cpu.load_and_run(vec![0xa9, 0x40, 0x0a, 0x00]);

        assert_eq!(cpu.register_a, 0x80);
    }

    #[test]
    fn test_asl() {
        let mut cpu = Cpu::new();

        // LDA 0x0F, TAX 0x0050, 0xF4 & register_A
        cpu.load_and_run(vec![0xa9, 0x10, 0x85, 0x50, 0x06, 0x50, 0xa5, 0x50, 0x00]);

        assert_eq!(cpu.register_a, 0x20);
        assert_eq!(cpu.mem_read(0x50), 0x20);
        assert_eq!(cpu.memory[0x50], 0x20);
    }

    #[test]
    fn test_and_immediate() {
        let mut cpu = Cpu::new();

        // LDA 0x0F, TAX 0x0050, 0xF4 & register_A
        cpu.load_and_run(vec![0xa9, 0xf4, 0x85, 0x50, 0xa9, 0x0f, 0x2D, 0x50, 0x00]);

        assert_eq!(cpu.register_a, 0x04);
    }

    #[test]
    fn test_sta_to_memory() {
        let mut cpu = Cpu::new();
        let memory_addr = 0x50;

        cpu.load_and_run(vec![0xa9, 0xff, 0x85, 0x50, 0x00]);

        let read_value = cpu.mem_read(memory_addr);

        assert_eq!(read_value, 0xff);
    }

    #[test]
    fn test_lda_from_memory() {
        let mut cpu = Cpu::new();
        cpu.mem_write(0x10, 0x55);

        cpu.load_and_run(vec![0xa5, 0x10, 0x00]);

        assert_eq!(cpu.register_a, 0x55);
    }

    #[test]
    fn test_0xa9_lda_immediate() {
      let mut cpu = Cpu::new();
      cpu.load_and_run(vec![0xa9, 0x04, 0x00]);

      // print!("{:#?}", &cpu);

      assert!(cpu.register_a == 0x04);

      // assert!(cpu.program_counter == 0);
      // assert!(cpu.status & 0b0000_0100 == 0b0000_0100);

      assert!(!cpu.status.contains(CpuFlags::ZERO | CpuFlags::NEGATIVE));
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
      let mut cpu = Cpu::new();
      cpu.load_and_run(vec![0xa9, 0x00, 0x00]);

      assert!(cpu.register_a == 0x00);
      assert!(!cpu.status.contains(CpuFlags::ZERO | CpuFlags::NEGATIVE));
    }

    #[test]
    fn test_0xa9_lda_neg_flag() {
        let mut cpu = Cpu::new();
        cpu.load_and_run(vec![0xa9, 0b1000_0010, 0x00]);

        assert!(cpu.register_a == 0b1000_0010);
        assert!(!cpu.status.contains(CpuFlags::ZERO | CpuFlags::NEGATIVE));
    }

    #[test]
    fn test_0xaa_tax_move_a_to_x() {
      let mut cpu = Cpu::new();
      cpu.load_and_run(vec![0xa9, 0x01, 0xAA, 0x00]);

      assert!(cpu.register_x == 1);
      assert!(!cpu.status.contains(CpuFlags::ZERO | CpuFlags::NEGATIVE));
    }

    #[test]
    fn test_0xe8_inx_increment_x() {
      let mut cpu = Cpu::new();
      cpu.load_and_run(vec![0xa9, 0x01, 0xaa, 0xe8, 0x00]);

      assert!(cpu.register_x == 2);
      assert!(!cpu.status.contains(CpuFlags::ZERO | CpuFlags::NEGATIVE));
    }

    #[test]
    fn test_5_ops_working_together() {
      let mut cpu = Cpu::new();
      cpu.load_and_run(vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00]);

      assert_eq!(cpu.register_x, 0xc1)
    }

    #[test]
    fn test_inx_overflow() {
      let mut cpu = Cpu::new();
      cpu.load_and_run(vec![0xa9, 0xff, 0xaa, 0xe8, 0xe8, 0x00]);

      assert_eq!(cpu.register_x, 1)
    }

    #[test]
    fn test_stack_push_16_pop_16() {
      let mut cpu = Cpu::new();

      cpu.stack_push_u16(0x0102);
      cpu.stack_push_u16(0x0407);
      cpu.stack_push_u16(0x8012);
      
      assert_eq!(cpu.stack_pop_u16(), 0x8012);
      assert_eq!(cpu.stack_pop_u16(), 0x0407);
      assert_eq!(cpu.stack_pop_u16(), 0x0102);
    }

    #[test]
    fn test_stack_push_pop() {
      let mut cpu = Cpu::new();

      cpu.stack_push(2);
      cpu.stack_push(4);
      cpu.stack_push(8);

      // println!("{:?}", &cpu.memory[250..256]);

      assert_eq!(cpu.stack_pop(), 8);
      assert_eq!(cpu.stack_pop(), 4);
      assert_eq!(cpu.stack_pop(), 2);
    }

    #[ignore]
    #[test]
    fn _test_log_memory_value_shifting() {
      let mut cpu = Cpu::new();
      // cpu.mem_write(0x8000, 0b0001);

      let addr: u16 = 0x8000;

      cpu.mem_write_u16(addr, 0x1000);
      let read_value = cpu.mem_read_u16(addr);

      println!(
          "Memory {:x}, val: {:b}, read value: {:b}",
          0x8000, &cpu.memory[addr as usize], &read_value
      );
      println!(
          "Memory 0x8001-0x8000 hi shifted | lo: {:b}",
          (cpu.memory[(addr + 1) as usize] as u16) << 8 | cpu.memory[addr as usize] as u16
      );
      println!(
          "Memory 0x8001-0x8000 hi part only: {:b}",
          cpu.memory[(addr + 1) as usize]
      );
      // print!("{:?}", &cpu.memory[8000..]);
    }
}

