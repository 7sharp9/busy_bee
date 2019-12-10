use std::fmt;

pub mod mmu;

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
    Long
}
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
#[derive(Clone)]
pub struct CPU {
    pub d: [u32; 8],
    pub a: [u32; 8],
    pub pc: u32,
    pub ccr: u16,
    pub mmu: mmu::Mmu,
}
