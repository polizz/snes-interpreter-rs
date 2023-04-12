// use std::fmt;
use crate::op_codes;
use std::collections::HashMap;
// use std::fmt::Binary;

// static PROG_BEGIN: u16 = 0x8000;

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
    None,
}

#[derive(Clone, Debug)]
pub struct Cpu {
    pub register_x: u8,
    pub register_a: u8,
    pub register_y: u8,
    pub stack_pointer: u8,
    pub program_counter: u16,
    pub status: u8,
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
            stack_pointer: 0,
            program_counter: 0,
            status: 0,
            memory: [0; 0xFFFF], // memory: ([0; 0xFFFF]).into()
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
                let ptr: u8 = (base as u8).wrapping_add(self.register_x);
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

    pub fn update_result_flags(&mut self, result: u8) {
      if result == 0 {
          self.status |= 0b0000_0010;
      } else {
          self.status &= 0b1111_1101;
      }

      if result & 0b1000_0000 != 0 {
          self.status |= 0b1000_0000;
      } else {
          self.status &= 0b0111_1111;
      }
  }

    pub fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.register_y = 0;
        self.status = 0;

        self.program_counter = self.mem_read_u16(0xFFFC);
    }

    pub fn load_and_run(&mut self, program: Vec<u8>) {
        self.load(program);
        self.reset();
        self.run();
    }

    pub fn load(&mut self, program: Vec<u8>) {
        self.memory[0x8000..(0x8000 + program.len())].copy_from_slice(&program);
        self.mem_write_u16(0xFFFC, 0x8000);
    }

    pub fn lda(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);

        self.register_a = data;
        self.update_result_flags(self.register_a);
    }

    pub fn tax(&mut self) {
        self.register_x = self.register_a;
        self.update_result_flags(self.register_x);
    }

    pub fn inx(&mut self) {
        if self.register_x == 0xff {
            self.register_x = 0;
        } else {
            self.register_x += 1;
        }

        self.update_result_flags(self.register_x);
    }

    pub fn sta(&mut self, mode: &AddressingMode) {
      let addr = self.get_operand_address(mode);
      self.mem_write(addr, self.register_a);
    }

    pub fn and(&mut self, mode: &AddressingMode) {
      let addr = self.get_operand_address(mode);
      let data = self.mem_read(addr);
      
      self.register_a &= data;

      self.update_result_flags(self.register_a);
    }

    pub fn run(&mut self) {
      let ref opcodes: HashMap<u8, &'static op_codes::OpCode> = *op_codes::OPCODES_MAP;

      loop {
          let op_code = self.mem_read(self.program_counter);
          self.program_counter += 1;

          let op_code_data = opcodes.get(&op_code).expect("Unknown OPCODE memory mode.");
          let program_counter_state = self.program_counter;

          match op_code {
              0xA9 | 0xA5 | 0xB5 | 0xAD | 0xBD | 0xB9 | 0xA1 | 0xB1 => {
                // LDA
                self.lda(&op_code_data.address_mode);
              }
              0x85 | 0x95 | 0x8d | 0x9d | 0x99 | 0x81 | 0x91 => {
                // STA
                self.sta(&op_code_data.address_mode)
              }
              0x29| 0x25| 0x35| 0x2D| 0x3D| 0x39| 0x21| 0x31 => {
                // AND
                self.and(&op_code_data.address_mode)
              }
              0xE8 => {
                // INX
                self.inx();
              }
              0xAA => {
                  // TAX
                  self.tax();
              }
              0x00 => {
                // BRK
                return;
            }
              _ => (),
          }

          if program_counter_state == self.program_counter {
            self.program_counter += (op_code_data.len - 1) as u16;
          }
      }
    }
}

#[cfg(test)]
mod test {
  use super::*;

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

      assert!(cpu.status & 0b0000_0010 == 0);
      assert!(cpu.status & 0b1000_0000 == 0);
  }

  #[test]
  fn test_0xa9_lda_zero_flag() {
      let mut cpu = Cpu::new();
      cpu.load_and_run(vec![0xa9, 0x00, 0x00]);

      assert!(cpu.register_a == 0x00);
      assert!(cpu.status & 0b0000_0010 == 0b0000_0010);
      assert!(cpu.status & 0b1000_0000 == 0);
  }

  #[test]
  fn test_0xa9_lda_neg_flag() {
      let mut cpu = Cpu::new();
      cpu.load_and_run(vec![0xa9, 0b1000_0010, 0x00]);

      assert!(cpu.register_a == 0b1000_0010);
      assert!(cpu.status & 0b0000_0010 == 0);
      assert!(cpu.status & 0b1000_0000 == 0b1000_0000);
  }

  #[test]
  fn test_0xaa_tax_move_a_to_x() {
      let mut cpu = Cpu::new();
      cpu.load_and_run(vec![0xa9, 0x01, 0xaa, 0x00]);

      assert!(cpu.register_x == 1);
      assert!(cpu.status & 0b0000_0010 == 0);
      assert!(cpu.status & 0b1000_0000 == 0);
  }

  #[test]
  fn test_0xe8_inx_increment_x() {
      let mut cpu = Cpu::new();
      cpu.load_and_run(vec![0xa9, 0x01, 0xaa, 0xe8, 0x00]);

      assert!(cpu.register_x == 2);
      assert!(cpu.status & 0b0000_0010 == 0);
      assert!(cpu.status & 0b1000_0000 == 0);
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

  #[ignore]
  #[test]
  fn _test_log_memory_value_shifting() {
      let mut cpu = Cpu::new();
      // cpu.mem_write(0x8000, 0b0001);

      let addr: u16 = 0x8000;

      cpu.mem_write_u16(addr, 0x1000);
      let read_value = cpu.mem_read_u16(addr);

      println!("Memory {:x}, val: {:b}, read value: {:b}",  0x8000, &cpu.memory[addr as usize], &read_value);
      println!("Memory 0x8001-0x8000 hi shifted | lo: {:b}", (cpu.memory[(addr + 1) as usize] as u16) << 8 | cpu.memory[addr as usize] as u16);
      println!("Memory 0x8001-0x8000 hi part only: {:b}", cpu.memory[(addr + 1) as usize]);
      // print!("{:?}", &cpu.memory[8000..]);
    }

}
