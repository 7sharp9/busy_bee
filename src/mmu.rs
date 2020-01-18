use std::fmt;
use std::io::Cursor;
extern crate byteorder;
use byteorder::{BigEndian, ByteOrder};

const MEMORY_CONFIGURATION: u32 = 0xFF8001;
const VIDEO_DISPLAY_REGISTER_START: u32 = 0xff8200;
const VIDEO_DISPLAY_REGISTER_END: u32 = 0xFF8260;

const RESERVED: u32 = 0xFF8400;
const DMA_DISK_CONTROLLER: u32 = 0xFF8600;

const PSG_REGISTER_SELECT: u32 = 0xFF8800;
const PSG_READ: u32 = PSG_REGISTER_SELECT;
const PSG_WRITE: u32 = 0xFF8802;

//Multi Function Peripheral
const MPF68901: u32 = 0xFFFA00;

//Asynchronous Communications Interface Adaptor
const MC6850: u32 = 0xFFFC00;

const ROM_START: u32 = 0xfc0000;
const ROM_END: u32 = 0xfeffff;

const CART_START: u32 = 0xfa0000;
const CART_END: u32 = 0xFBFFFF;

const MAX_MEMORY: u32 = 0xffffff;

const MAX_ROM: u32 = 0x3ffff;
const MAX_CART: u32 = 0xFBFFFF;

pub mod Video;

#[derive(Clone)]
pub struct Mmu {
    pub memory_configuration: u8,
    pub rom: Vec<u8>,
    pub cart: Vec<u8>,
    pub memory: Vec<u8>,
    pub sound: Sound,
    pub video: Video::Video,
}

#[derive(Clone)]
pub struct Sound {
    reg_select: u8,
    registers: [u8; 16],
}

impl Sound {
    pub fn select_register(&mut self, reg: u8) {
        self.reg_select = reg
    }
    pub fn read_data(&self) -> u8 {
        self.registers[self.reg_select as usize]
    }
    pub fn write_data(&mut self, data: u8) {
        self.registers[self.reg_select as usize] = data
    }

    pub fn new() -> Sound {
        Sound {
            reg_select: 0,
            registers: [0; 16],
        }
    }
    //0,1 pitch of analogue A 0-7 frequency lower 4 bits of 1 = step size
    //2,3 pitch of analogue B "
    //4,5 pitch of analogue C ""
    //6 lowest 5 bits noise generator, smaller value = higher pitch
    //7 0 - channel a tone on/off 0 = on
    //  1 - channel b "
    //  2 - channel c "
    //  3 - channel a noise on/off 0 = on
    //  4 - channel b "
    //  5 - channel c "
    //  6 - port a in/output 0 = in 1 = out
    //  7 - port b "
    //8 channel a 0-3 register control bit 4 = envelope register is being used and bits 0-3 ignored
    //9 channel b
    //10 channel c
    //11,12 low/high byte of sustain
    //13 bits 0-3 envelope generator
    //14,15 PortA / Port B set via bits 7/8 of register 7
}

impl fmt::Debug for Mmu {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "mmu {{ rom: {}, cart: {} memory: {} }}",
            self.rom.len(),
            self.cart.len(),
            self.memory.len()
        )
    }
}

fn word_from_slice(buffer: &[u8], address: u32) -> u16 {
    (buffer[address as usize] as u16) << 8 | (buffer[(address.wrapping_add(1)) as usize] as u16)
}

fn long_from_slice(buffer: &[u8], address: u32) -> u32 {
    (buffer[address as usize] as u32) << 24
        | (buffer[address.wrapping_add(1) as usize] as u32) << 16
        | (buffer[address.wrapping_add(2) as usize] as u32) << 8
        | (buffer[address.wrapping_add(3) as usize] as u32)
}

impl Mmu {
    pub fn read_byte(&self, address: u32) -> u8 {
        let address = address & 0xffffff; //clip to max mem
        match address {
            0..=7 => self.rom[address as usize],
            ROM_START..=ROM_END => {
                let address = address & 0x3ffff;
                self.rom[(address) as usize]
            }
            CART_START..=CART_END => {
                //TODO
                0xff
            }
            PSG_READ => self.sound.read_data(),
            VIDEO_DISPLAY_REGISTER_START..=VIDEO_DISPLAY_REGISTER_END => {
                unimplemented!("video read");
                //self.video_display[(address) as usize]
            }
            MEMORY_CONFIGURATION => self.memory_configuration,
            a => self.memory[a as usize],
        }
    }

    pub fn write_byte(&mut self, destination: u32, data: u8) {
        let address = destination & 0xffffff; //clip to max mem
        match address {
            a if a < 8 => panic!("Memory error:${0:08x}, {0}, {0:024b}", destination),
            ROM_START..ROM_END => panic!("Attempt to write to Rom: ${:08x}", destination),
            CART_START..CART_END => panic!("Attempt to write to Cart: ${:08x}", destination),
            PSG_REGISTER_SELECT => self.sound.select_register(data),
            PSG_WRITE => self.sound.write_data(data),
            VIDEO_DISPLAY_REGISTER_START..=VIDEO_DISPLAY_REGISTER_END => {
                self.video.write_byte(address, data)
            }
            _ => self.memory[destination as usize] = data,
        }
    }

    pub fn read_word(&self, address: u32) -> u16 {
        let address = address & 0xffffff; //clip to max mem
        match address {
            0..7 => word_from_slice(&self.rom, address),
            ROM_START..ROM_END => {
                let address = address & 0x3ffff;
                let rom1 = word_from_slice(&self.rom, address);
                let rom2 = BigEndian::read_u16(&self.rom[address as usize..]);

                //let temp2 = BigEndian::read_u16(temp);
                rom1
            }
            CART_START..CART_END => {
                //TODO
                0xffff
            }
            VIDEO_DISPLAY_REGISTER_START..=VIDEO_DISPLAY_REGISTER_END => {
                self.video.read_word(address)
            }
            a => word_from_slice(&self.memory, a),
        }
    }

    pub fn write_word(&mut self, address: u32, data: u16) {
        let address = address & 0xffffff; //clip to max mem
        match address {
            a if a < 8 => panic!("Memory error:${0:08x}, {0}, {0:024b}", address),
            ROM_START..ROM_END => panic!("Attempt to write to Rom: ${:08x}", address),
            CART_START..CART_END => panic!("Attempt to write to Cart: ${:08x}", address),
            VIDEO_DISPLAY_REGISTER_START..VIDEO_DISPLAY_REGISTER_END => {
                self.video.write_word(address, data)
            }
            //YM2149 {
            //     ym2149IOMemory[(destination-ym2149Start)] = (data >> 8)
            //     ym2149IOMemory[(destination-ym2149Start + 1)] = data
            //}
            _ => {
                self.memory[address as usize] = (data >> 8) as u8;
                self.memory[(address + 1) as usize] = (data & 0xff) as u8
            }
        }
    }

    pub fn read_long(&self, address: u32) -> u32 {
        let address = address & 0xffffff; //clip to max mem
        match address {
            0..7 => long_from_slice(&self.rom, address),
            ROM_START..ROM_END => {
                let address = address & 0x3ffff;
                let rom1 = long_from_slice(&self.rom, address);
                //let temp = &self.rom[(address as usize)..(address.wrapping_add(4) as usize)];
                //let temp2 = BigEndian::read_u32(temp);
                rom1
            }
            CART_START..CART_END => 0xffffffff,
            VIDEO_DISPLAY_REGISTER_START..VIDEO_DISPLAY_REGISTER_END => {
                unimplemented!("video read")
                //long_from_slice(&self.video_display, address)
            }
            a => long_from_slice(&self.memory, a),
        }
    }

    pub fn write_long(&self, address: u32, data: u32) {
        let address = address & 0xffffff; //clip to max mem
        todo!()
    }
}
