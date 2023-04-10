#[derive(Clone, Debug)]
pub struct Cpu {
  pub register_x: u8,
  pub register_a: u8,
  pub register_y: u8,
  pub stack_pointer: u8,
  pub program_counter: u16,
  pub status: u8
}

// enum OpCodes {
//   LDA = 0xA9,
//   TAX = 0xAA,
//   BRK = 0x00,
//   INX = 0xE8
// }

impl Cpu {
  pub fn new() -> Self {
    Cpu {
      register_x: 0,
      register_a: 0,
      register_y: 0,
      stack_pointer: 0,
      program_counter: 0,
      status: 0
    }
  }

  #[allow(dead_code)]
  pub fn lda(&mut self, param: u8) {
    self.register_a = param;
    self.update_result_flags(self.register_a);
  }

  #[allow(dead_code)]
  pub fn tax(&mut self) {
    self.register_x = self.register_a;
    self.update_result_flags(self.register_x);
  }

  #[allow(dead_code)]
  pub fn inx(&mut self) {
    if self.register_x == 0xff {
      self.register_x = 0;
    } else {
      self.register_x = self.register_x + 1;
    }

    self.update_result_flags(self.register_x);
  }

  #[allow(dead_code)]
  pub fn update_result_flags(&mut self, result: u8) {
    if result == 0 {
      self.status = self.status | 0b0000_0010;
    } else {
      self.status = self.status & 0b1111_1101;
    }

    if result & 0b1000_0000 != 0 {
      self.status = self.status | 0b1000_0000;
    } else {
      self.status = self.status & 0b0111_1111;
    }
  }

  #[allow(dead_code)]
  pub fn interpret(&mut self, program: Vec<u8>) {
    self.program_counter = 0;

    loop {
      let op_code = program[self.program_counter as usize];
      self.program_counter += 1;

      match op_code {
        0x00 => { // BRK
          self.program_counter = 0;
          self.status = self.status | 0b0000_0100;

          return
        },
        0xAA => { // TAX
          self.tax();
        },
        0xA9 => { // LDA
          let param = program[self.program_counter as usize];
          self.program_counter += 1;

          self.lda(param);
        },
        0xE8 => { // INX
          self.inx();
        }
        _ => ()
      }
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn test_0xa9_lda_immediate() {
    let mut cpu = Cpu::new();
    cpu.interpret(vec![0xa9, 0x04, 0x00]);

    assert!(cpu.register_a == 0x04);

    assert!(cpu.program_counter == 0);
    assert!(cpu.status & 0b0000_0100 == 0b0000_0100);

    assert!(cpu.status & 0b0000_0010 == 0);
    assert!(cpu.status & 0b1000_0000 == 0);
  }

  #[test]
  fn test_0xa9_lda_zero_flag() {
    let mut cpu = Cpu::new();
    cpu.interpret(vec![0xa9, 0x00, 0x00]);

    assert!(cpu.register_a == 0x00);
    assert!(cpu.status & 0b0000_0010 == 0b0000_0010);
    assert!(cpu.status & 0b1000_0000 == 0);
  }

  #[test]
  fn test_0xa9_lda_neg_flag() {
    let mut cpu = Cpu::new();
    cpu.interpret(vec![0xa9, 0b1000_0010, 0x00]);

    assert!(cpu.register_a == 0b1000_0010);
    assert!(cpu.status & 0b0000_0010 == 0);
    assert!(cpu.status & 0b1000_0000 == 0b1000_0000);
  }

  #[test]
  fn test_0xaa_tax_move_a_to_x() {
    let mut cpu = Cpu::new();
    cpu.register_a = 0x01;

    cpu.interpret(vec![0xaa, 0x00]);

    assert!(cpu.register_x == 1);
    assert!(cpu.status & 0b0000_0010 == 0);
    assert!(cpu.status & 0b1000_0000 == 0);
  }

  #[test]
  fn test_0xe8_inx_increment_x() {
    let mut cpu = Cpu::new();
    cpu.register_x = 1;
    cpu.interpret(vec![0xe8, 0x00]);

    assert!(cpu.register_x == 2);
    assert!(cpu.status & 0b0000_0010 == 0);
    assert!(cpu.status & 0b1000_0000 == 0);
  }

  #[test]
  fn test_5_ops_working_together() {
      let mut cpu = Cpu::new();
      cpu.interpret(vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00]);

      assert_eq!(cpu.register_x, 0xc1)
  }

   #[test]
   fn test_inx_overflow() {
       let mut cpu = Cpu::new();
       cpu.register_x = 0xff;
       cpu.interpret(vec![0xe8, 0xe8, 0x00]);

       assert_eq!(cpu.register_x, 1)
   }
}