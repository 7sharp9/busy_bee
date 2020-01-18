#![feature(exclusive_range_pattern)]
#![feature(trace_macros)]

#[macro_use]
extern crate num_derive;

use self::TraceMode::*;
use num_traits::FromPrimitive;
use std::convert::TryInto;
use std::fmt;

use quark::BitIndex;
use quark::BitMask;
use quark::Signs;

pub mod mmu;

macro_rules! n_flag {
    ($e:ident) => {
        $e.ccr
    };
}

pub enum TraceMode {
    NoTrace,
    TraceOnAny,
    TraceOnFlow,
    Undefined,
}

pub enum ActiveStack {
    Usp,
    Isp,
    Msp,
}

pub enum OperationSize {
    Byte,
    Word,
    Long,
}

#[derive(Debug)]
pub enum AddressingMode {
    DataRegister(u8),
    AddressRegister(u8),
    Address(u8),
    AddressWithPostincrement(u8),
    AddressWithPredecrement(u8),
    AddressWithDisplacement(u8),
    AddressWithIndex(u8),
    ProgramCounterWithDisplacement,
    ProgramCounterWithIndex,
    AbsoluteShort,
    AbsoluteLong,
    Immediate,
}

impl AddressingMode {
    fn parse(mode: u8, reg: u8) -> AddressingMode {
        match (mode, reg) {
            (0b000, 0..=7) => AddressingMode::DataRegister(reg),
            (0b001, 0..=7) => AddressingMode::AddressRegister(reg),
            (0b010, 0..=7) => AddressingMode::Address(reg),
            (0b011, 0..=7) => AddressingMode::AddressWithPostincrement(reg),
            (0b100, 0..=7) => AddressingMode::AddressWithPredecrement(reg),
            (0b101, 0..=7) => AddressingMode::AddressWithDisplacement(reg),
            (0b110, 0..=7) => AddressingMode::AddressWithIndex(reg),
            (0b111, 0b010) => AddressingMode::ProgramCounterWithDisplacement,
            (0b111, 0b011) => AddressingMode::ProgramCounterWithIndex,
            (0b111, 0b000) => AddressingMode::AbsoluteShort,
            (0b111, 0b001) => AddressingMode::AbsoluteLong,
            (0b111, 0b100) => AddressingMode::Immediate,
            _ => panic!("Invalid Addressing Mode: {} {}", mode, reg),
        }
    }
}

impl std::fmt::Display for OperationSize {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let display = match self {
            OperationSize::Byte => "b",
            OperationSize::Word => "w",
            OperationSize::Long => "l",
        };
        write!(f, "{}", display)
    }
}

impl OperationSize {
    fn from_u16(element: u16) -> OperationSize {
        match element {
            0xFF => OperationSize::Long,
            0x00 => OperationSize::Word,
            _other => OperationSize::Byte,
        }
    }
}

#[derive(FromPrimitive, ToPrimitive)]
pub enum Condition {
    T = 0b0000,
    F = 0b0001,
    HI = 0b0010,
    LS = 0b0011,
    CC = 0b0100,
    CS = 0b0101,
    NE = 0b0110,
    EQ = 0b0111,
    VC = 0b1000,
    VS = 0b1001,
    PL = 0b1010,
    MI = 0b1011,
    GE = 0b1100,
    LT = 0b1101,
    GT = 0b1110,
    LE = 0b1111,
}

impl fmt::Display for Condition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let text = match self {
            Condition::T => "t",
            Condition::F => "f",
            Condition::HI => "hi",
            Condition::LS => "ls",
            Condition::CC => "cc",
            Condition::CS => "cs",
            Condition::NE => "ne",
            Condition::EQ => "eq",
            Condition::VC => "vc",
            Condition::VS => "vs",
            Condition::PL => "pl",
            Condition::MI => "mi",
            Condition::GE => "ge",
            Condition::LT => "lt",
            Condition::GT => "gt",
            Condition::LE => "le",
        };
        write!(f, "{}", text)
    }
}

impl fmt::Debug for Condition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let text = match self {
            Condition::T => "True",
            Condition::F => "False",
            Condition::HI => "Higher",
            Condition::LS => "Lower or Same",
            Condition::CC => "Carry Clear",
            Condition::CS => "Carry Set",
            Condition::NE => "Not Equal",
            Condition::EQ => "Equal",
            Condition::VC => "Overflow Clear",
            Condition::VS => "Overflow Set",
            Condition::PL => "Plus",
            Condition::MI => "Minus",
            Condition::GE => "Greater or Equal",
            Condition::LT => "Less Than",
            Condition::GT => "Greater Than",
            Condition::LE => "Less or Equal",
        };
        write!(f, "{}", text)
    }
}

use Condition::*;
impl Condition {
    pub fn is_true(&self, cpu: &CPU) -> bool {
        match self {
            T => true,
            F => false,
            HI => !cpu.c() & !cpu.z(),
            LS => cpu.c() | cpu.z(),
            CC => !cpu.c(),
            CS => cpu.c(),
            NE => !cpu.z(),
            EQ => cpu.z(),
            VC => !cpu.v(),
            VS => cpu.v(),
            PL => !cpu.n(),
            MI => cpu.n(),
            GE => (cpu.n() & cpu.v()) | (!cpu.n() & !cpu.v()),
            LT => (cpu.n() & !cpu.v()) | (!cpu.n() & cpu.v()),
            GT => (cpu.n() & cpu.v()) | (!cpu.n() & !cpu.v()) & !cpu.z(),
            LE => cpu.z() | (cpu.n() & !cpu.v()) | (!cpu.n() & cpu.v()),
        }
    }
}

#[derive(Clone)]
pub struct CPU {
    pub d: [u32; 8],
    pub a: [u32; 8],
    pub pc: u32,
    pub ccr: u16,
    pub mmu: mmu::Mmu,
}

//println!("TTSM IPM   XNZVC");
//println!("{:016b}", cpu.ccr);

use colored::Colorize;
impl fmt::Debug for CPU {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "{}",
            (format!(
                "D0 {:08x}   D1 {:08x}   D2 {:08x}   D3 {:08x}",
                self.d[0], self.d[1], self.d[2], self.d[3]
            ))
            .red()
        )?;
        writeln!(
            f,
            "{}",
            (format!(
                "D4 {:08x}   D5 {:08x}   D6 {:08x}   D7 {:08x}",
                self.d[4], self.d[5], self.d[6], self.d[7]
            ))
            .red()
        )?;
        writeln!(
            f,
            "A0 {:08x}   A1 {:08x}   A2 {:08x}   A3 {:08x}",
            self.a[0], self.a[1], self.a[2], self.a[3]
        )?;
        writeln!(
            f,
            "A4 {:08x}   A5 {:08x}   A6 {:08x}   A7 {:08x}",
            self.a[4], self.a[5], self.a[6], self.a[7]
        )?;
        writeln!(f, "USP   {:08x}   ISP   {:08x}", self.a[7], self.a[7])?;
        writeln!(
            f,
            "T={:02b} S={} M={} X={} N={} Z={} V={} C={} IMASK={:0x} STP={}",
            (self.ccr >> 14),
            self.s() as u32,
            self.m() as u32,
            self.x() as u32,
            self.n() as u32,
            self.z() as u32,
            self.v() as u32,
            self.c() as u32,
            self.interrupt_mask(),
            0
        )?;
        writeln!(f, "PC: {:08x}", self.pc)
    }
}
// D0 FFFF00E4   D1 00FC4C25   D2 00000023   D3 0000FFFF
// D4 00000400   D5 00100000   D6 00000017   D7 00015F96
// A0 0000187A   A1 00000A06   A2 00FC2318   A3 00FC2356
// A4 00FC05C0   A5 00000000   A6 00FC0044   A7 601E0100
// USP  00006188 ISP  601E0100
// T=00 S=1 M=0 X=0 N=0 Z=0 V=0 C=0 IMASK=7 STP=0
// Prefetch fa52 (ILLEGAL) 0cb9 (CMP) Chip latch 00000000
// 00FC0026 0cb9 fa52 235f 00fa 0000 CMP.L #$fa52235f,$00fa0000 [ffffffff]
// Next PC: 00fc0030

pub mod ccr {
    pub const C: u16 = 0x1;
    pub const V: u16 = 0x2;
    pub const Z: u16 = 0x4;
    pub const N: u16 = 0x8;
    pub const X: u16 = 0x10;
    pub const I: u16 = 0x700;
    pub const M: u16 = 0x1000;
    pub const S: u16 = 0x2000;
    pub const T0: u16 = 0x4000;
    pub const T1: u16 = 0x8000;

    pub const XC: u16 = X | C;
    pub const NZVC: u16 = N | Z | V | C;
    pub const XNVC: u16 = X | N | V | C;
    pub const XNZVC: u16 = X | N | Z | V | C;

    pub const MASK: u16 = X | N | Z | V | C;
    pub const E68_SR_MASK: u16 = MASK | S | T0 | T1 | I;
}

use ccr::*;
impl CPU {
    pub fn c(&self) -> bool {
        !self.ccr & C == 0
    }

    pub fn v(&self) -> bool {
        !self.ccr & V == 0
    }
    pub fn z(&self) -> bool {
        !self.ccr & Z == 0
    }
    pub fn n(&self) -> bool {
        !self.ccr & N == 0
    }
    pub fn x(&self) -> bool {
        !self.ccr & X == 0
    }
    pub fn interrupt_mask(&self) -> u16 {
        self.ccr & I
    }
    pub fn m(&self) -> bool {
        !self.ccr & M == 0
    }
    pub fn s(&self) -> bool {
        !self.ccr & S == 0
    }
    pub fn t0(&self) -> bool {
        !self.ccr & T0 == 0
    }
    pub fn t1(&self) -> bool {
        !self.ccr & T1 == 0
    }

    pub fn trace_mode(&self) -> TraceMode {
        match (self.t0(), self.t1()) {
            (false, false) => NoTrace,
            (true, false) => TraceOnAny,
            (false, true) => TraceOnFlow,
            (true, true) => Undefined,
        }
    }

    pub fn reset(&mut self) {
        self.a[7] = self.mmu.read_long(0) as u32;
        self.pc = self.mmu.read_long(4) as u32;
    }

    #[inline]
    fn byte_word_or_long(op: u16) -> bool {
        match op {
            0b00 => false,
            _ => true,
        }
    }

    #[inline]
    fn size_from_two_bits_one_indexed(size: u16) -> Option<OperationSize> {
        match size & 0b011 {
            0b00 => None,
            0b01 => Some(OperationSize::Byte),
            0b11 => Some(OperationSize::Word),
            0b10 => Some(OperationSize::Long),
            _ => None,
        }
    }

    pub fn step(&mut self) {
        let opcode = self.mmu.read_word(self.pc);
        let op_1 = (opcode & 0xF000) >> 12;
        let op_2 = (opcode & 0x0F00) >> 8;
        let op_3 = (opcode & 0x00F0) >> 4;
        let op_4 = opcode & 0x000F;
        print!("${:08x} : ", self.pc);
        match (op_1, op_2, op_3, op_4) {
            //(0b0000, 0b0000, _, _) => println!("ori to ccr"),

            //move <ea>, <ea>
            // CC NZVC V/C are cleared, NZ as per result
            (..) if op_1 & 0b1100 == 0 && CPU::byte_word_or_long(op_1) => {
                let size = CPU::size_from_two_bits_one_indexed(op_1).expect("invalid size");

                let destination_reg = opcode >> 9 & 0b111;
                let destination_mode = opcode >> 6 & 0b111;
                let source_mode = opcode >> 3 & 0b111;
                let source_reg = opcode & 0b111;
                let mut pc_increment = 2;

                let (source, source_format) =
                    match AddressingMode::parse(source_mode as u8, source_reg as u8) {
                        AddressingMode::DataRegister(_reg) => unimplemented!(),
                        AddressingMode::AddressRegister(_reg) => unimplemented!(),
                        AddressingMode::Address(_reg) => unimplemented!(),
                        AddressingMode::AddressWithPostincrement(reg) => {
                            let address = self.a[reg as usize];
                            let format = format!("(a{})+", reg);
                            match size {
                                OperationSize::Byte => {
                                    self.a[reg as usize] += 1;
                                    (self.mmu.read_byte(address) as u32, format)
                                }
                                OperationSize::Word => {
                                    self.a[reg as usize] += 2;
                                    (self.mmu.read_word(address) as u32, format)
                                }
                                OperationSize::Long => {
                                    self.a[reg as usize] += 4;
                                    (self.mmu.read_long(address), format)
                                }
                            }
                        }
                        AddressingMode::AddressWithPredecrement(_reg) => unimplemented!(),
                        AddressingMode::AddressWithDisplacement(reg) => {
                            let displacement =
                                (self.mmu.read_word(self.pc + pc_increment) as i32).sign_extend(16);
                            let format = format!("${:04x}(a{})", displacement, reg);
                            pc_increment += 2;
                            let address = (self.a[reg as usize] as i32) + displacement;
                            let value = (self.mmu.read_byte(address as u32) & 0xff) as u32;
                            (value, format)
                        }
                        AddressingMode::AddressWithIndex(_reg) => unimplemented!(),
                        AddressingMode::ProgramCounterWithDisplacement => unimplemented!(),
                        AddressingMode::ProgramCounterWithIndex => unimplemented!(),
                        AddressingMode::AbsoluteShort => unimplemented!(),
                        AddressingMode::AbsoluteLong => unimplemented!(),
                        AddressingMode::Immediate => {
                            let imm = match size {
                                //only read lower byte information of the word
                                OperationSize::Byte => {
                                    pc_increment += 2;
                                    (self.mmu.read_word(self.pc + 2) & 0xff) as u32
                                }
                                OperationSize::Word => {
                                    pc_increment += 2;
                                    self.mmu.read_word(self.pc + 2) as u32
                                }
                                OperationSize::Long => {
                                    pc_increment += 4;
                                    self.mmu.read_long(self.pc + 2)
                                }
                            };
                            (imm, format!("#{}", imm))
                        }
                    };

                match AddressingMode::parse(destination_mode as u8, destination_reg as u8) {
                    AddressingMode::DataRegister(reg) => {
                        println!("move.{} {},d{}", size, source_format, reg);
                        match size {
                            OperationSize::Byte => self.d[reg as usize] |= source & 0xff,
                            OperationSize::Word => self.d[reg as usize] |= source & 0xffff,
                            OperationSize::Long => self.d[reg as usize] = source,
                        }
                    }
                    AddressingMode::Address(reg) => {
                        let destination = self.a[reg as usize];
                        println!("move.{} {},(a{})", size, source_format, reg);
                        match size {
                            OperationSize::Byte => self.mmu.write_byte(destination, source as u8),
                            OperationSize::Word => unimplemented!(),
                            OperationSize::Long => unimplemented!(),
                        }
                    }
                    AddressingMode::AddressWithPostincrement(reg) => {
                        println!("move.{} {},(a{})+", size, source_format, destination_reg);
                        match size {
                            OperationSize::Byte => {
                                self.mmu
                                    .write_byte(self.a[reg as usize], (source & 0xff) as u8);
                                self.a[reg as usize] += 1
                            }
                            OperationSize::Word => {
                                self.mmu
                                    .write_word(self.a[reg as usize], (source & 0xffff) as u16);
                                self.a[reg as usize] += 2
                            }
                            OperationSize::Long => {
                                self.mmu.write_long(self.a[reg as usize], source);
                                self.a[reg as usize] += 4
                            }
                        };
                    }
                    AddressingMode::AddressWithPredecrement(_reg) => unimplemented!(),
                    AddressingMode::AddressWithDisplacement(reg) => {
                        //sign extend the displacement
                        let displacement =
                            (self.mmu.read_word(self.pc + pc_increment) as u32).sign_extend(16);
                        pc_increment += 2;
                        let destination = self.a[reg as usize] + displacement;

                        println!("move.{} {},{}({})", size, source_format, displacement, reg);
                        match size {
                            OperationSize::Byte => self.mmu.write_byte(destination, source as u8),
                            OperationSize::Word => unimplemented!(),
                            OperationSize::Long => unimplemented!(),
                        }
                    }
                    AddressingMode::AddressWithIndex(_reg) => unimplemented!(),
                    AddressingMode::AbsoluteShort => unimplemented!(),
                    AddressingMode::AbsoluteLong => {
                        let destination_address = self.mmu.read_long(self.pc + pc_increment);
                        println!(
                            "move.{} {},${:08x}",
                            size, source_format, destination_address
                        );
                        pc_increment += 4;
                        match size {
                            OperationSize::Byte => self
                                .mmu
                                .write_byte(destination_address, (source & 0xff) as u8),
                            OperationSize::Word => self
                                .mmu
                                .write_word(destination_address, (source & 0xffff) as u16),
                            OperationSize::Long => self.mmu.write_long(destination_address, source),
                        }
                    }
                    _ => panic!("invalid addressing mode: {:0b} {:0b}",),
                };

                //Should be the same for:
                //AND, ANDI, OR, EOR, EORI, MOVE, MOVEQ, EXT, NOT, TST
                let mut set = 0;
                if source
                    & match size {
                        OperationSize::Byte => 0xff,
                        OperationSize::Word => 0xffff,
                        OperationSize::Long => 0xffffffff,
                    }
                    == 0
                {
                    set |= ccr::Z
                } else if source
                    & match size {
                        OperationSize::Byte => 0x80,
                        OperationSize::Word => 0x8000,
                        OperationSize::Long => 0x80000000,
                    }
                    != 0
                {
                    set |= ccr::N
                }
                self.ccr &= !ccr::NZVC;
                self.ccr |= set & ccr::NZVC;

                self.pc += pc_increment
            }
            //BTST #<data>, <ea>
            //CC Z is bit tested is zero
            (0b0000, ..) if opcode & 0b0000000111000000 == 0b0000000100000000 => {
                let bit_register = (opcode >> 9) & 0b111;
                let mode = (opcode >> 3) & 0b111;
                let register = opcode & 0b111;
                todo!()
            }
            //BTST Immediate #, <ea>
            //CC Z is bit tested is zero
            (0b0000, 0b1000, ..) if opcode & 0b0000000011000000 == 0 => {
                let mode = (opcode >> 3) & 0b111;
                let reg = opcode & 0b111;
                let bit_index = self.mmu.read_word(self.pc + 2) & 0xff;
                match AddressingMode::parse(mode as u8, reg as u8) {
                    AddressingMode::ProgramCounterWithDisplacement => {
                        //let displacement1 = (self.mmu.read_word(self.pc + 4) as u32).sign_extend(16);
                        let displacement = (self.mmu.read_word(self.pc + 4) as i32).sign_extend(16);
                        let address = self.pc as i32 + displacement + 4;
                        let bit_set = address.bit(bit_index as usize);
                        println!(
                            "btst.b #${:04}, (PC, {:08x}) == {:08x}",
                            bit_index, displacement, address
                        );
                        if bit_set {
                            self.ccr |= ccr::Z
                        } else {
                            self.ccr &= !ccr::Z
                        }
                        self.pc += 6
                    }
                    other => panic!(
                        "BTST address mode {:?} not implemented mode {:03b} reg: {:03b}",
                        other, mode, reg
                    ),
                }
            }
            //DBcc
            (..) if opcode & 0b1111000011111000 == 0b0101000011001000 => {
                let condition = opcode >> 8 & 0b1111;
                let reg = opcode & 0b111;
                let condition = Condition::from_u16(condition).unwrap();
                let displacement = (self.mmu.read_word(self.pc + 2) as i32).sign_extend(16);
                let condition_true = condition.is_true(self);
                if condition_true {
                    self.pc += 4
                } else {
                    let new_counter = self.d[reg as usize] as i32 - 1;
                    self.d[reg as usize] =
                        (self.d[reg as usize] & 0xffff0000) | new_counter as u32 & 0xffff;

                    if new_counter == -1 {
                        self.pc += 4
                    } else {
                        let new_pc = (self.pc as i32) + displacement + 2;
                        self.pc = new_pc as u32
                    }
                }

                println!(
                    "db{} d{},${:08x} == ${:08x}",
                    condition, reg, displacement, self.pc
                );
            }
            //Bra, Bcc, Bsr
            //CC none
            (0b0110, condition, ..) => {
                match condition {
                    //Bra
                    0b0000 => {
                        match opcode & 0xFF {
                            0xFF => {
                                //long displacement
                                self.pc += self.mmu.read_long(self.pc + 2) + 2;
                                println!("bra.l ${:06x}", self.pc)
                            }
                            0x00 => {
                                //word displacement, sign extended
                                self.pc +=
                                    (self.mmu.read_word(self.pc + 2) as u32 + 2).sign_extend(16);
                                println!("bra.w ${:06x}", self.pc)
                            }
                            byte => {
                                //byte displacement, sign extend
                                self.pc += ((byte as u32) + 2).sign_extend(24);
                                println!("bra.s ${:06x}", self.pc)
                            }
                        }
                    }
                    //Bsr
                    0b0001 => unimplemented!("Bsr"),
                    //Bcc
                    bcc => {
                        let mut pc_increment = 2;
                        let condition = Condition::from_u16(bcc).unwrap();
                        let condition_true = condition.is_true(self);
                        let displacement = opcode & 0xFF;
                        let displacement_size = OperationSize::from_u16(displacement);
                        //this is the same as Bra

                        let displacement = match displacement_size {
                            OperationSize::Long => {
                                let displacement = self.mmu.read_long(self.pc + pc_increment);
                                pc_increment += 4;
                                displacement
                            }
                            OperationSize::Word => {
                                let displacement =
                                    self.mmu.read_word(self.pc + pc_increment) as u32;
                                let sign_extended = displacement.sign_extend(16);
                                pc_increment += 2;
                                sign_extended
                            }
                            OperationSize::Byte => (displacement as u32).sign_extend(24),
                        };
                        println!(
                            "b{}.{} #${:x} == {:08x} ({})",
                            condition,
                            displacement_size,
                            displacement,
                            (self.pc + displacement + 2),
                            condition_true
                        );
                        if condition_true {
                            self.pc += displacement + 2
                        } else {
                            self.pc += pc_increment
                        }
                    }
                }
            }

            //Move to SR
            //All CC bits affected as this is moving a word to CCR
            (0b0100, 0b0110, part, _) if part & 0b1100 == 0b1100 => {
                let mode = (opcode & 0b0000000000111000) >> 3;
                let reg = opcode & 0b0000000000000111;
                match AddressingMode::parse(mode as u8, reg as u8) {
                    AddressingMode::Immediate => {
                        let imm = self.mmu.read_word(self.pc + 2);
                        self.ccr = imm;
                        self.pc += 4;
                        println!("move #{:0x},sr", imm)
                    }
                    _ => unimplemented!("move      mode: {:03b} reg: {:03b}", mode, reg),
                }
            }
            //reset
            //CC none
            (0b0100, 0b1110, 0b0111, 0b0000) => {
                //124 clock cycles
                self.pc += 2;
                println!("reset")
            }

            //jmp <ea>
            //CC none
            (..) if opcode & 0b1111111111000000 == 0b0100111011000000 => {
                let mode = (opcode & 0b0000000000111000) >> 3;
                let reg = opcode & 0b0000000000000111;
                match AddressingMode::parse(mode as u8, reg as u8) {
                    AddressingMode::Address(reg) => {
                        let jump = self.a[reg as usize];
                        println!("jmp A{:x}", reg);
                        self.pc = jump
                    }
                    AddressingMode::AddressWithDisplacement(_reg) => unimplemented!(),
                    AddressingMode::AddressWithIndex(_reg) => unimplemented!(),
                    AddressingMode::AbsoluteShort => unimplemented!(),
                    AddressingMode::AbsoluteLong => unimplemented!(),
                    AddressingMode::ProgramCounterWithDisplacement => unimplemented!(),
                    AddressingMode::ProgramCounterWithIndex => unimplemented!(),
                    _ => panic!("invalid jmp mode {:03b} reg: {:03b}", mode, reg),
                }
            }
            //cmpi #<data>, <ea>
            //CC NZVC
            //Destination - Immediate Data
            (0b0000, 0b1100, ..) => {
                let size = (opcode & 0b0000000011000000) >> 6;
                let mode = (opcode & 0b0000000000111000) >> 3;
                let reg = opcode & 0b0000000000000111;
                match (size, mode, reg) {
                    //byte
                    (0b00, ..) => unimplemented!(),
                    //word
                    (0b01, ..) => unimplemented!(),
                    //long
                    (0b10, ..) => match (mode, reg) {
                        //(d16,An)
                        (0b101, an_reg) => {
                            let immediate = self.mmu.read_long(self.pc + 2);
                            let displacement = self.mmu.read_word(self.pc + 6) as u32;
                            let destination = self.a[an_reg as usize] + displacement;
                            let result = destination.wrapping_sub(immediate);
                            println!(
                                "cmpi.l #${:x},(A{},${:x}) == ${:x}",
                                immediate, an_reg, displacement, destination
                            );

                            //CMP, CMPA, CMPI, CMPM
                            //currently only long flags are calculated, byte being val & ! << 7 != 0 etc
                            let sn = immediate & 1 << 31 != 0;
                            let dn = destination & 1 << 31 != 0;
                            let rn = result & 1 << 31 != 0;

                            let mut set: u16 = 0;
                            //overflow
                            if (!sn && dn && !rn) || (sn && !dn && rn) {
                                set |= ccr::V;
                            }
                            //carry
                            if (sn && !dn) || (rn && !dn) || (sn && rn) {
                                set |= ccr::C | ccr::X;
                            }
                            //neg
                            if rn {
                                set |= ccr::N
                            }
                            //zero
                            if result == 0 {
                                set |= ccr::Z
                            }
                            self.ccr &= !ccr::NZVC;
                            self.ccr |= set & ccr::NZVC;

                            self.pc += 8
                        }
                        //Absolute Long
                        (0b111, 0b001) => {
                            let immediate = self.mmu.read_long(self.pc + 2);
                            let destination_register = self.mmu.read_long(self.pc + 6);
                            let destination = self.mmu.read_long(destination_register);
                            //let result = dest.wrapping_sub(immediate);
                            let result = destination - immediate;

                            //CMP, CMPA, CMPI, CMPM
                            //currently only long flags are calculated, byte being val & ! << 7 != 0 etc
                            let sn = immediate & 1 << 31 != 0;
                            let dn = destination & 1 << 31 != 0;
                            let rn = result & 1 << 31 != 0;

                            let mut set: u16 = 0;
                            //overflow
                            if (!sn && dn && !rn) || (sn && !dn && rn) {
                                set |= ccr::V;
                            }
                            //carry
                            if (sn && !dn) || (rn && !dn) || (sn && rn) {
                                set |= ccr::C | ccr::X;
                            }
                            //neg
                            if rn {
                                set |= ccr::N
                            }
                            //zero
                            if result == 0 {
                                set |= ccr::Z
                            }
                            self.ccr &= !ccr::NZVC;
                            self.ccr |= set & ccr::NZVC;

                            //self.clock +=12;
                            self.pc += 10;
                            println!("cmpi.l #${:0x}, ${:0x}", immediate, destination_register)
                        }
                        _ => unimplemented!("other cmpi mode{:03b} ea_reg{:03b}", mode, reg),
                    },
                    //should be impossible size is only 2 bit
                    _ => panic!("invalid size"),
                }
            }
            //lea
            //CC none
            (..) if opcode & 0b1111000111000000 == 0b0100000111000000 => {
                let an = (opcode >> 9) & 0b111;
                let mode = (opcode >> 3) & 0b111;
                let reg = opcode & 0b111;

                match AddressingMode::parse(mode as u8, reg as u8) {
                    AddressingMode::Address(_reg) => todo!(),
                    AddressingMode::AddressWithDisplacement(_reg) => todo!(),
                    AddressingMode::AddressWithIndex(_reg) => todo!(),
                    AddressingMode::AbsoluteShort => todo!(),
                    AddressingMode::AbsoluteLong => {
                        let address = self.mmu.read_long(self.pc + 2);
                        self.a[an as usize] = address;
                        println!("lea.l {:08x},a{}", address, an);
                        self.pc += 6
                    }
                    AddressingMode::ProgramCounterWithDisplacement => {
                        let displacement = (self.mmu.read_word(self.pc + 2) as i32).sign_extend(16);
                        self.a[an as usize] =
                            (self.pc as i32 + 2 + displacement).try_into().unwrap();
                        println!("lea.l (PC,${:04x}), a{}", displacement, an);
                        self.pc += 4;
                    }
                    AddressingMode::ProgramCounterWithIndex => todo!(),
                    _ => panic!("invalid mode/reg for lea"),
                }
            }
            //suba
            //CC none
            (0b1001, ..) if opcode & 0b000000011000000 == 0b0000000011000000 => {
                let register = opcode >> 9 & 0b111;
                let long_mode = opcode.bit(9);
                let mode = opcode >> 3 & 0b111;
                let reg = opcode & 0b111;
                match AddressingMode::parse(mode as u8, reg as u8) {
                    AddressingMode::DataRegister(_reg) => todo!(),
                    AddressingMode::AddressRegister(reg) => {
                        if long_mode {
                            //suba.l
                            let dest = self.a[register as usize];
                            let source = self.a[reg as usize];
                            let result = dest - source;
                            println!("suba.l a{},a{}", reg, register);
                            self.a[register as usize] = result;
                            self.pc += 2;
                        } else {
                            //suba.w
                            //note: Word operation. The source operand is sign-extended to a long operand and
                            //the operation is performed on the address register using all 32 bits.
                            todo!();
                        }
                    }
                    AddressingMode::Address(_reg) => todo!(),
                    AddressingMode::AddressWithPostincrement(_reg) => todo!(),
                    AddressingMode::AddressWithPredecrement(_reg) => todo!(),
                    AddressingMode::AddressWithDisplacement(_reg) => todo!(),
                    AddressingMode::AddressWithIndex(_reg) => todo!(),
                    AddressingMode::ProgramCounterWithDisplacement => todo!(),
                    AddressingMode::ProgramCounterWithIndex => todo!(),
                    AddressingMode::AbsoluteShort => todo!(),
                    AddressingMode::AbsoluteLong => todo!(),
                    AddressingMode::Immediate => todo!(),
                }
            }
            _ => panic!(
                "unknown {:04b} {:04b} {:04b} {:04b} {:04x}",
                op_1, op_2, op_3, op_4, opcode
            ),
        }
    }
}
