#![feature(exclusive_range_pattern)]
#![feature(trace_macros)]

#[macro_use]
extern crate num_derive;

use self::TraceMode::*;
use num::traits::AsPrimitive;
use num_traits::FromPrimitive;
use std::convert::TryInto;
use std::fmt;

use quark::BitIndex;
use quark::BitMask;
use quark::Signs;

use num::{PrimInt, Unsigned};
use std::ops::{Add, BitAnd, BitOr, Not, Shl, Sub};

pub trait FlagMsb<T> {
    /// The most significant bit for the type.
    fn msb() -> T;
}

macro_rules! bit_size_impl {
    ($t:ty, $v:expr) => {
        impl FlagMsb<$t> for $t {
            #[inline]
            fn msb() -> $t {
                $v
            }
        }
    };
}

bit_size_impl!(u8, 0x80);
bit_size_impl!(u16, 0x8000);
bit_size_impl!(u32, 0x80000000);

pub mod mmu;

#[derive(Debug)]
pub enum TraceMode {
    NoTrace,
    TraceOnAny,
    TraceOnFlow,
    Undefined,
}

#[derive(Debug)]
pub enum ActiveStack {
    Usp,
    Isp,
    Msp,
}

#[derive(Debug)]
pub enum OperationSize {
    Byte,
    Word,
    Long,
}

#[derive(Debug)]
pub enum Destination {
    EA,
    Dn,
}

#[derive(Debug)]
pub enum ShiftDirection {
    Left,
    Right,
}

impl std::fmt::Display for ShiftDirection {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let display = match self {
            ShiftDirection::Left => "l",
            ShiftDirection::Right => "r",
        };
        write!(f, "{}", display)
    }
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
            HI => !cpu.c_flag & !cpu.z_flag,
            LS => cpu.c_flag | cpu.z_flag,
            CC => !cpu.c_flag,
            CS => cpu.c_flag,
            NE => !cpu.z_flag,
            EQ => cpu.z_flag,
            VC => !cpu.v_flag,
            VS => cpu.v_flag,
            PL => !cpu.n_flag,
            MI => cpu.n_flag,
            GE => (cpu.n_flag & cpu.v_flag) | (!cpu.n_flag & !cpu.v_flag),
            LT => (cpu.n_flag & !cpu.v_flag) | (!cpu.n_flag & cpu.v_flag),
            GT => (cpu.n_flag & cpu.v_flag) | (!cpu.n_flag & !cpu.v_flag) & !cpu.z_flag,
            LE => cpu.z_flag | (cpu.n_flag & !cpu.v_flag) | (!cpu.n_flag & cpu.v_flag),
        }
    }
}

#[derive(Clone)]
pub struct CPU {
    pub d: [u32; 8],
    pub a: [u32; 8],
    pub pc: u32,

    pub t1_flag: bool,
    pub t0_flag: bool,
    pub s_flag: bool,
    pub m_flag: bool,

    pub irq_level: u8,
    pub int_mask: u16,

    pub x_flag: bool,
    pub n_flag: bool,
    pub z_flag: bool,
    pub v_flag: bool,
    pub c_flag: bool,
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
            "T1={} T0={} S={} M={} X={} N={} Z={} V={} C={} IMASK={:0x} STP={}",
            self.t1_flag as u32,
            self.t0_flag as u32,
            self.s_flag as u32,
            self.m_flag as u32,
            self.x_flag as u32,
            self.n_flag as u32,
            self.z_flag as u32,
            self.v_flag as u32,
            self.c_flag as u32,
            self.int_mask,
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

impl CPU {
    // pub fn trace_mode(&self) -> TraceMode {
    //     match (self.t1_flag, self.t0_flag) {
    //         (false, false) => NoTrace,
    //         (true, false) => TraceOnAny,
    //         (false, true) => TraceOnFlow,
    //         (true, true) => Undefined,
    //     }
    // }

    pub fn reset(&mut self) {
        self.a[7] = self.mmu.read_long(0) as u32;
        self.pc = self.mmu.read_long(4) as u32;
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

    #[inline]
    fn size_from_two_bits_zero_indexed(size: u16) -> Option<OperationSize> {
        match size & 0b011 {
            0b00 => Some(OperationSize::Byte),
            0b01 => Some(OperationSize::Word),
            0b10 => Some(OperationSize::Long),
            _ => None,
        }
    }

    fn get_dreg8(&self, reg: u8) -> u8 {
        (self.d[(reg & 7) as usize] & 0xff) as u8
    }

    fn get_dreg16(&self, reg: u8) -> u16 {
        (self.d[(reg & 7) as usize] & 0xffff) as u16
    }

    fn get_dreg32(&self, reg: u8) -> u32 {
        self.d[(reg & 7) as usize]
    }

    fn get_areg16(&self, reg: u8) -> u16 {
        (self.a[(reg & 7) as usize] & 0xffff) as u16
    }

    fn get_areg32(&self, reg: u8) -> u32 {
        self.a[(reg & 7) as usize]
    }

    fn set_dreg8(&mut self, reg: u8, val: u8) {
        let reg = reg & 7;
        self.d[reg as usize] = (self.d[reg as usize] & 0xffffff00) | (val as u32 & 0x000000ff)
    }

    fn set_dreg16(&mut self, reg: u8, val: u16) {
        let reg = reg & 7;
        self.d[reg as usize] = (self.d[reg as usize] & 0xffff0000) | (val as u32 & 0x0000ffff)
    }

    fn set_dreg8_clear(&mut self, reg: u8) {
        let reg = reg & 7;
        self.d[reg as usize] = self.d[reg as usize] & 0xffffff00
    }

    fn set_dreg16_clear(&mut self, reg: u8) {
        let reg = reg & 7;
        self.d[reg as usize] = self.d[reg as usize] & 0xffff0000
    }

    fn set_dreg32(&mut self, reg: u8, val: u32) {
        self.d[(reg & 7) as usize] = val
    }

    fn set_dreg32_clear(&mut self, reg: u8) {
        self.d[(reg & 7) as usize] = 0
    }

    fn set_areg16(&mut self, reg: u8, val: u16) {
        let reg = reg & 7;
        self.a[reg as usize] = (val as u32 & 0xffff).sign_extend(16);
    }

    fn set_areg32(&mut self, reg: u8, val: u32) {
        let reg = reg & 7;
        self.a[reg as usize] = val
    }

    pub const NFLAG_SET: u32 = 0x80;
    pub const NFLAG_CLEAR: u32 = 0;
    pub const CFLAG_SET: u32 = 0x100;
    pub const CFLAG_CLEAR: u32 = 0;
    pub const XFLAG_SET: u32 = 0x100;
    pub const XFLAG_CLEAR: u32 = 0;
    pub const VFLAG_SET: u32 = 0x80;
    pub const VFLAG_CLEAR: u32 = 0;
    pub const ZFLAG_SET: u32 = 0;
    pub const ZFLAG_CLEAR: u32 = 0xffffffff;

    //and, andi, or, eor, eori, move, moveq, ext, not, tst,
    pub fn flag_logical(&mut self, result: u32, size: OperationSize) {
        match size {
            OperationSize::Byte => self.n_flag = result & 0x80 != 0,
            OperationSize::Word => self.n_flag = result & 0x8000 != 0,
            OperationSize::Long => self.n_flag = result & 0x80000000 != 0,
        }
        self.z_flag = result == 0;
        self.v_flag = false;
        self.c_flag = false
    }

    pub fn flag_cmp(&mut self, source: u32, destination: u32, result: u32, size: OperationSize) {
        let sm: bool;
        let dm: bool;
        let rm: bool;

        match size {
            OperationSize::Byte => {
                sm = (source & 0x80) != 0;
                dm = (destination & 0x80) != 0;
                rm = (result & 0x80) != 0
            }
            OperationSize::Word => {
                sm = (source & 0x8000) != 0;
                dm = (destination & 0x8000) != 0;
                rm = (result & 0x8000) != 0
            }
            OperationSize::Long => {
                sm = (source & 0x80000000) != 0;
                dm = (destination & 0x80000000) != 0;
                rm = (result & 0x80000000) != 0
            }
        }
        self.v_flag = (!sm && dm && !rm) || (sm && !dm && rm);
        self.c_flag = (sm && !dm) || (rm && !dm) || (sm && rm);
        self.n_flag = rm;
        self.z_flag = result == 0
    }

    //ADD, ADDI, ADDQ, ADDX
    pub fn flg_add<T>(&mut self, source: T, destination: T, result: T, isADDX: bool)
    where
        T: PrimInt + FlagMsb<T>,
    {
        let sm = (source & T::msb()) != T::zero();
        let dm = (destination & T::msb()) != T::zero();
        let rm = (result & T::msb()) != T::zero();

        self.v_flag = (sm && dm && !rm) || (!sm && !dm && rm);
        self.c_flag = (sm && dm) || (!rm && dm) || (sm && !rm);
        self.x_flag = self.c_flag;
        self.n_flag = rm;
        if isADDX {
            if result != T::zero() {
                self.z_flag = false
            }
        } else {
            self.z_flag = result == T::zero()
        }
    }

    pub fn step(&mut self) {
        let opcode = self.mmu.read_word(self.pc);
        print!("${:08x} : ", self.pc);
        match opcode {
            //(0b0000, 0b0000, _, _) => println!("ori to ccr"),

            //movea
            // cc NA
            _ if opcode & 0b1100000111000000 == 0b0000000001000000 && opcode >> 12 & 0b11 != 0 => {
                let mut pc_increment = 2;
                let size = match (opcode >> 12) & 0b11 {
                    0b11 => OperationSize::Word,
                    0b10 => OperationSize::Long,
                    _ => panic!("Invalid size"),
                };
                let source_mode = opcode >> 3 & 0b111;
                let source_reg = opcode & 0b111;
                let (source, source_format): (u32, String) =
                    match AddressingMode::parse(source_mode as u8, source_reg as u8) {
                        AddressingMode::Immediate => {
                            let (imm, f) = match size {
                                OperationSize::Word => {
                                    let imm = self.mmu.read_word(self.pc + pc_increment) as u32;
                                    pc_increment += 2;
                                    (imm, format!("#${:04x}", imm))
                                }
                                OperationSize::Long => {
                                    let imm = self.mmu.read_long(self.pc + pc_increment);
                                    pc_increment += 4;
                                    (imm, format!("#${:08x}", imm))
                                }
                                _ => panic!("Invalid size"),
                            };
                            (imm, f)
                        }
                        _ => todo!(),
                    };
                let destination_reg = opcode >> 9 & 0b111;
                match size {
                    OperationSize::Word => {
                        self.a[destination_reg as usize] = source.sign_extend(16)
                    }
                    OperationSize::Long => self.a[destination_reg as usize] = source,
                    _ => panic!("Invalid size"),
                }
                println!("movea.{} {},a{}", size, source_format, destination_reg);
                self.pc += pc_increment
            }
            //move <ea>, <ea>
            // CC NZVC V/C are cleared, NZ as per result
            _ if opcode & 0b1100000000000000 == 0b0000000000000000 && opcode >> 12 & 0b11 != 0 => {
                let size = match opcode >> 12 & 0b11 {
                    0b01 => OperationSize::Byte,
                    0b11 => OperationSize::Word,
                    0b10 => OperationSize::Long,
                    _ => panic!("Invalid"),
                };

                let destination_reg = (opcode >> 9) & 0b111;
                let destination_mode = (opcode >> 6) & 0b111;
                let source_mode = (opcode >> 3) & 0b111;
                let source_reg = opcode & 0b111;
                let mut pc_increment = 2;

                let (source, source_format) =
                    match AddressingMode::parse(source_mode as u8, source_reg as u8) {
                        AddressingMode::DataRegister(reg) => {
                            let address = match size {
                                OperationSize::Byte => self.d[reg as usize] & 0xFF,
                                OperationSize::Word => self.d[reg as usize] & 0xFFFF,
                                OperationSize::Long => self.d[reg as usize],
                            };
                            (address, format!("d{}", reg))
                        }
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
                            (imm, format!("#&{:x}", imm))
                        }
                    };

                match AddressingMode::parse(destination_mode as u8, destination_reg as u8) {
                    AddressingMode::AddressRegister(reg) => {
                        println!("move.{} {},a{}", size, source_format, reg);
                        match size {
                            OperationSize::Byte => self.a[reg as usize] |= source & 0xff, //sign extend?
                            OperationSize::Word => self.a[reg as usize] |= source & 0xffff, //sign extend?
                            OperationSize::Long => self.a[reg as usize] = source,
                        }
                    }
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
                            OperationSize::Byte => self.mmu.write_byte(destination, source as u8), //sign extend?
                            OperationSize::Word => unimplemented!(), //sign extend?
                            OperationSize::Long => unimplemented!(),
                        }
                    }
                    AddressingMode::AddressWithPostincrement(reg) => {
                        println!("move.{} {},(a{})+", size, source_format, destination_reg);
                        match size {
                            OperationSize::Byte => {
                                self.mmu
                                    .write_byte(self.a[reg as usize], (source & 0xff) as u8); //sign extend?
                                self.a[reg as usize] += 1
                            }
                            OperationSize::Word => {
                                self.mmu
                                    .write_word(self.a[reg as usize], (source & 0xffff) as u16); //sign extend?
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
                            OperationSize::Byte => self.mmu.write_byte(destination, source as u8), //sign extend?
                            OperationSize::Word => unimplemented!(), //sign extend?
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
                                .write_byte(destination_address, (source & 0xff) as u8), //sign extend?
                            OperationSize::Word => self
                                .mmu
                                .write_word(destination_address, (source & 0xffff) as u16), //sign extend?
                            OperationSize::Long => self.mmu.write_long(destination_address, source),
                        }
                    }
                    _ => panic!("invalid addressing mode: {:0b} {:0b}",),
                };

                self.flag_logical(source, size);
                self.pc += pc_increment
            }
            //BTST #<data>, <ea>
            //CC Z is bit tested is zero
            _ if opcode & 0b1111000111000000 == 0b0000000100000000 => {
                let bit_register = (opcode >> 9) & 0b111;
                let mode = (opcode >> 3) & 0b111;
                let register = opcode & 0b111;
                todo!()
            }
            //BTST Immediate #, <ea>
            //CC Z is bit tested is zero
            _ if opcode & 0b1111111111000000 == 0b0000100000000000 => {
                let mode = (opcode >> 3) & 0b111;
                let reg = opcode & 0b111;
                let test_bit = self.mmu.read_word(self.pc + 2) & 0xff;
                match AddressingMode::parse(mode as u8, reg as u8) {
                    AddressingMode::ProgramCounterWithDisplacement => {
                        let displacement = (self.mmu.read_word(self.pc + 4) as i32).sign_extend(16);
                        let address = self.pc as i32 + displacement + 4;
                        let address_contents = self.mmu.read_byte(address as u32);
                        let bit_set = address_contents.bit(test_bit as usize);
                        println!(
                            "btst.b #${:04}, (PC, {:04x}) == {:08x}",
                            test_bit, displacement as i16, address
                        );
                        self.z_flag = !bit_set;
                        self.pc += 6
                    }
                    other => panic!(
                        "BTST address mode {:?} not implemented mode {:03b} reg: {:03b}",
                        other, mode, reg
                    ),
                }
            }
            //DBcc
            _ if opcode & 0b1111000011111000 == 0b0101000011001000 => {
                let condition = (opcode >> 8) & 0b1111;
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
            //Bra
            //CC none
            _ if opcode & 0b1111111100000000 == 0b0110000000000000 => {
                match opcode & 0xFF {
                    0xFF => {
                        //long displacement
                        self.pc += self.mmu.read_long(self.pc + 2) + 2;
                        println!("bra.l ${:06x}", self.pc)
                    }
                    0x00 => {
                        //word displacement, sign extended
                        self.pc += (self.mmu.read_word(self.pc + 2) as u32 + 2).sign_extend(16);
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
            //CC none
            _ if opcode & 0b1111111100000000 == 0b0110000100000000 => unimplemented!("Bsr"),

            //Bcc
            //CC none
            _ if opcode & 0b1111000000000000 == 0b0110000000000000 => {
                let bcc = (opcode >> 8) & 0b1111;
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
                        let displacement = self.mmu.read_word(self.pc + pc_increment) as u32;
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
                    (self.pc.wrapping_add(displacement + 2)),
                    condition_true
                );
                if condition_true {
                    self.pc = self.pc.wrapping_add(displacement + 2)
                } else {
                    self.pc = self.pc.wrapping_add(pc_increment)
                }
            }

            //CLR
            //CCR: n|v|c = 0 z=1
            _ if opcode & 0b1111111100000000 == 0b0100001000000000 => {
                let size = opcode >> 6 & 0b11;
                let size = CPU::size_from_two_bits_zero_indexed(size).unwrap();
                let mode = (opcode >> 3) & 0b111;
                let reg = opcode & 0b111;

                match AddressingMode::parse(mode as u8, reg as u8) {
                    AddressingMode::DataRegister(reg) => {
                        match size {
                            OperationSize::Byte => (self.d[reg as usize] &= 0xffffff00),
                            OperationSize::Word => self.d[reg as usize] &= 0xffff0000,
                            OperationSize::Long => self.d[reg as usize] = 0,
                        }
                        println!("clr.{} d{}", size, reg);
                        self.pc += 2
                    }
                    AddressingMode::Address(reg) => todo!(),
                    AddressingMode::AddressWithPostincrement(reg) => todo!(),
                    AddressingMode::AddressWithPredecrement(reg) => todo!(),
                    AddressingMode::AddressWithDisplacement(reg) => todo!(),
                    AddressingMode::AddressWithIndex(reg) => todo!(),
                    AddressingMode::AbsoluteShort => todo!(),
                    AddressingMode::AbsoluteLong => todo!(),
                    other => panic!("invalid addressing mode: {:?}", other),
                }
                //clear n|v|c
                self.n_flag = false;
                self.v_flag = false;
                self.c_flag = false;
                //set z
                self.z_flag = true
            }

            //Move to SR
            //All CC bits affected as this is moving a word to CCR
            _ if opcode & 0b1111111111000000 == 0b0100011011000000 => {
                let mode = (opcode & 0b0000000000111000) >> 3;
                let reg = opcode & 0b0000000000000111;
                match AddressingMode::parse(mode as u8, reg as u8) {
                    AddressingMode::Immediate => {
                        let imm = self.mmu.read_word(self.pc + 2);
                        let int_mask = imm.bits(8..11);
                        self.c_flag = imm.bit(0);
                        self.v_flag = imm.bit(1);
                        self.z_flag = imm.bit(2);
                        self.n_flag = imm.bit(3);
                        self.x_flag = imm.bit(4);
                        self.int_mask = int_mask;
                        self.m_flag = imm.bit(12);
                        self.s_flag = imm.bit(13);
                        self.t0_flag = imm.bit(14);
                        self.t1_flag = imm.bit(15);
                        self.pc += 4;
                        println!("move #{:0x},sr", imm)
                    }
                    _ => unimplemented!("move      mode: {:03b} reg: {:03b}", mode, reg),
                }
            }
            //reset
            //CC none
            _ if opcode & 0b1111111111111111 == 0b0100111001110000 => {
                //124 clock cycles
                self.pc += 2;
                println!("reset")
            }

            //jmp <ea>
            //CC none
            _ if opcode & 0b1111111111000000 == 0b0100111011000000 => {
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
            _ if opcode & 0b1111111100000000 == 0b0000110000000000 => {
                let size = CPU::size_from_two_bits_zero_indexed((opcode & 0b0000000011000000) >> 6)
                    .unwrap();
                let mode = ((opcode & 0b0000000000111000) >> 3) as u8;
                let reg = (opcode & 0b111) as u8;
                let addressing_mode = AddressingMode::parse(mode, reg);
                match (&size, addressing_mode) {
                    (OperationSize::Byte, ..) => unimplemented!(),
                    (OperationSize::Word, ..) => unimplemented!(),
                    (OperationSize::Long, AddressingMode::AddressWithDisplacement(reg)) => {
                        let source = self.mmu.read_long(self.pc + 2);
                        let displacement = (self.mmu.read_word(self.pc + 6) as i32).sign_extend(16);
                        let destination = (self.get_areg32(reg) as i32).wrapping_add(displacement);
                        let result = destination.wrapping_sub(source as i32);
                        println!(
                            "cmpi.l #${:x},(A{},${:x}) == ${:x}",
                            source, reg, displacement, destination
                        );

                        self.flag_cmp(source, destination as u32, result as u32, size);
                        self.pc += 8
                    }
                    (OperationSize::Long, AddressingMode::AbsoluteLong) => {
                        let immediate = self.mmu.read_long(self.pc + 2);
                        let destination_reg = self.mmu.read_long(self.pc + 6);
                        let destination = self.mmu.read_long(destination_reg);
                        let result = destination.wrapping_sub(immediate);
                        println!("cmpi.l #${:0x}, ${:0x}", immediate, destination_reg);

                        self.flag_cmp(immediate, destination, result, size);
                        self.pc += 10;
                    }
                    (a, b) => unimplemented!("Unimplemented cmpi - size: {:?} ea:{:?}", a, b),
                }
            }
            //lea
            //CC none
            _ if opcode & 0b1111000111000000 == 0b0100000111000000 => {
                let an = (opcode >> 9) & 0b111;
                let mode = (opcode >> 3) & 0b111;
                let reg = opcode & 0b111;

                match AddressingMode::parse(mode as u8, reg as u8) {
                    AddressingMode::Address(_reg) => todo!(),
                    AddressingMode::AddressWithDisplacement(_reg) => todo!(),
                    AddressingMode::AddressWithIndex(_reg) => todo!(),
                    AddressingMode::AbsoluteShort => todo!(), //no sign extension for lea?  check!
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
            _ if opcode & 0b1111000011000000 == 0b1001000011000000 => {
                let register = (opcode >> 9) & 0b111;
                let long_mode = opcode.bit(8);
                let mode = ((opcode >> 3) & 0b111) as u8;
                let reg = (opcode & 0b111) as u8;
                match AddressingMode::parse(mode, reg) {
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
            //cmpa
            _ if opcode & 0b1111000000000000 == 0b1011000000000000
                && (opcode >> 6) & 0b11 == 0b11 =>
            {
                let mut pc_increment = 2;
                let long_mode = opcode.bit(8);
                let register = ((opcode >> 9) & 0b111) as u8;
                let ea_mode = ((opcode >> 3) & 0b111) as u8;
                let ea_reg = (opcode & 0b111) as u8;
                let address_mode = AddressingMode::parse(ea_mode, ea_reg);

                if long_mode {
                    match address_mode {
                        AddressingMode::DataRegister(_reg) => todo!(),
                        AddressingMode::AddressRegister(reg) => todo!(),
                        AddressingMode::Address(_reg) => todo!(),
                        AddressingMode::AddressWithPostincrement(_reg) => todo!(),
                        AddressingMode::AddressWithPredecrement(_reg) => todo!(),
                        AddressingMode::AddressWithDisplacement(_reg) => todo!(),
                        AddressingMode::AddressWithIndex(_reg) => todo!(),
                        AddressingMode::ProgramCounterWithDisplacement => todo!(),
                        AddressingMode::ProgramCounterWithIndex => todo!(),
                        AddressingMode::AbsoluteShort => todo!(),
                        AddressingMode::AbsoluteLong => todo!(),
                        AddressingMode::Immediate => {
                            let imm = self.mmu.read_long(self.pc + pc_increment);
                            pc_increment += 4;
                            let dest_address = self.get_areg32(register);
                            let result = dest_address.wrapping_sub(imm);
                            self.flag_cmp(imm, dest_address, result, OperationSize::Long);
                            println!(
                                "cmpa.{} #${:08x},a{}",
                                (if long_mode { "l" } else { "w" }),
                                imm,
                                register
                            );
                            self.pc += pc_increment
                        }
                    }
                } else {
                    todo!()
                }
            }

            //addx
            _ if opcode & 0b1111000100110000 == 0b1101000100000000
                && opcode >> 6 & 0b11 != 0b11 =>
            {
                unimplemented!("addx")
            }
            //adda
            _ if opcode & 0b1111000011000000 == 0b1101000011000000 => unimplemented!("adda"),
            //add
            _ if opcode & 0b1111000000000000 == 0b1101000000000000
                && opcode >> 6 & 0b11 != 0b11 =>
            {
                let mut pc_increment = 2;
                let dn = ((opcode >> 9) & 0b111) as u8;
                let ea_mode = ((opcode >> 3) & 0b111) as u8;
                let ea_reg = (opcode & 0b111) as u8;

                let (size, operation_mode) = match opcode >> 6 & 0b111 {
                    0b000 => (OperationSize::Byte, Destination::Dn), //byte: <ea> + Dn -> Dn
                    0b001 => (OperationSize::Word, Destination::Dn), //word: <ea> + Dn -> Dn
                    0b010 => (OperationSize::Long, Destination::Dn), //long: <ea> + Dn -> Dn

                    0b100 => (OperationSize::Byte, Destination::EA), //byte: Dn + <ea> -> <ea>
                    0b101 => (OperationSize::Word, Destination::EA), //word: Dn + <ea> -> <ea>
                    0b110 => (OperationSize::Long, Destination::EA), //long: Dn + <ea> -> <ea>
                    _ => panic!("Invalid operation mode"),
                };
                match (AddressingMode::parse(ea_mode, ea_reg), operation_mode) {
                    (AddressingMode::DataRegister(_reg), Destination::Dn) => todo!(),
                    (AddressingMode::AddressRegister(_reg), Destination::Dn) => todo!(),
                    (AddressingMode::Address(_reg), _destination) => todo!(),
                    (AddressingMode::AddressWithPostincrement(_reg), _) => todo!(),
                    (AddressingMode::AddressWithPredecrement(_reg), _) => todo!(),
                    (AddressingMode::AddressWithDisplacement(_reg), _) => todo!(),
                    (AddressingMode::AddressWithIndex(_reg), _) => todo!(),
                    (AddressingMode::ProgramCounterWithDisplacement, Destination::Dn) => todo!(),
                    (AddressingMode::ProgramCounterWithIndex, Destination::Dn) => todo!(),
                    (AddressingMode::AbsoluteShort, _) => todo!(),
                    (AddressingMode::AbsoluteLong, _) => todo!(),
                    (AddressingMode::Immediate, Destination::Dn) => {
                        //sign extension not needed as immediate can only be the source, which means Dn is always the destination
                        match size {
                            OperationSize::Byte => {
                                let source = self.mmu.read_word(self.pc + pc_increment) & 0xff;
                                pc_increment += 2;
                                todo!()
                            }
                            OperationSize::Word => {
                                let source = self.mmu.read_word(self.pc + pc_increment);
                                pc_increment += 2;
                                let destination = self.get_dreg16(dn);
                                let result = destination.wrapping_add(source);
                                self.set_dreg16(dn, result);

                                self.flg_add(source, destination, result, false);

                                println!("add.{} #${:04x},d{}", size, source, dn)
                            }
                            OperationSize::Long => {
                                self.mmu.read_long(self.pc + pc_increment);
                                todo!()
                            }
                        };
                        self.pc += pc_increment
                    }

                    //These all panic as only memory alterable addressing modes can be used if
                    //the destination operand is the <ea>
                    (AddressingMode::DataRegister(_reg), Destination::EA) => panic!(),
                    (AddressingMode::AddressRegister(_reg), Destination::EA) => panic!(),
                    (AddressingMode::ProgramCounterWithDisplacement, Destination::EA) => panic!(),
                    (AddressingMode::ProgramCounterWithIndex, Destination::EA) => panic!(),
                    (AddressingMode::Immediate, Destination::EA) => panic!(),
                }
            }
            //LSR - register
            _ if opcode & 0b1111000100011000 == 0b1110000000001000 => {
                let count_or_reg = ((opcode >> 9) & 0b111) as u8;
                let size = CPU::size_from_two_bits_zero_indexed((opcode >> 6) & 0b111).unwrap();
                let use_register_for_count = opcode.bit(5);
                let reg = (opcode & 0b111) as u8;

                let shift = {
                    if use_register_for_count {
                        match size {
                            OperationSize::Byte => self.get_dreg8(count_or_reg) as u32,
                            OperationSize::Word => self.get_dreg16(count_or_reg) as u32,
                            OperationSize::Long => self.get_dreg32(count_or_reg),
                        }
                    } else {
                        match count_or_reg {
                            //0 is encoded as a shift of 8
                            0 => 8,
                            other => other as u32,
                        }
                    }
                };
                let source = {
                    match size {
                        OperationSize::Byte => self.get_dreg8(reg) as u32,
                        OperationSize::Word => self.get_dreg16(reg) as u32,
                        OperationSize::Long => self.get_dreg32(reg),
                    }
                };
                let result = source >> shift;

                //flags and d reg assignment
                match size {
                    OperationSize::Byte => {
                        if shift != 0 {
                            if shift <= 8 {
                                self.set_dreg8(reg, result as u8);
                                self.x_flag = source << (9 - shift) != 0;
                                self.c_flag = self.x_flag;
                                self.n_flag = false;
                                self.z_flag = result == 0;
                                self.v_flag = false;
                            } else {
                                self.set_dreg8_clear(reg); //self.d[reg] &= 0xffffff00
                                self.x_flag = false;
                                self.c_flag = false;
                                self.n_flag = false;
                                self.z_flag = true;
                                self.v_flag = false;
                            }
                        } else {
                            self.c_flag = false;
                            self.n_flag = (source as u8).sign_bit();
                            self.z_flag = result == 0;
                            self.v_flag = false;
                        }
                    }
                    OperationSize::Word => {
                        if shift != 0 {
                            if shift <= 16 {
                                self.set_dreg16(reg, result as u16);
                                self.x_flag = ((source >> (shift - 1)) << 8) != 0;
                                self.c_flag = self.x_flag;
                                self.n_flag = false;
                                self.z_flag = result == 0;
                                self.v_flag = false;
                            } else {
                                self.set_dreg16_clear(reg); //self.d[reg] &= 0xffff0000
                                self.x_flag = false;
                                self.c_flag = false;
                                self.n_flag = false;
                                self.z_flag = true;
                                self.v_flag = false;
                            }
                        } else {
                            self.c_flag = false;
                            self.n_flag = (source as u16).sign_bit();
                            self.z_flag = result == 0;
                            self.v_flag = false;
                        }
                    }
                    OperationSize::Long => {
                        if shift != 0 {
                            if shift <= 32 {
                                self.set_dreg32(reg, result);
                                self.x_flag = ((source >> (shift - 1)) << 8) != 0;
                                self.c_flag = self.x_flag;
                                self.n_flag = false;
                                self.z_flag = result == 0;
                                self.v_flag = false;
                            } else {
                                self.set_dreg32_clear(reg); // self.d[reg] = 0
                                self.x_flag = if shift == 32 {
                                    (source & 0x80000000) >> 23 != 0
                                } else {
                                    false
                                };
                                self.c_flag = self.x_flag;
                                self.n_flag = false;
                                self.z_flag = true;
                                self.v_flag = false;
                            }
                        } else {
                            self.c_flag = false;
                            self.n_flag = source.sign_bit();
                            self.z_flag = result == 0;
                            self.v_flag = false;
                        }
                    }
                }
                println!("lsr.{} #{:02},d{}", size, shift, reg);
                println!("\r\n{:?}", self);
                self.pc += 2
            }
            _ => panic!("pc: {:08x} unknown {1:04x} {1:016b}", self.pc, opcode),
        }
    }
}
