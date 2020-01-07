use std::fmt;
use std::io::Cursor;
extern crate byteorder;
use byteorder::{BigEndian, ByteOrder};

const VIDEO_DISPLAY_REGISTER_START: u32 = 0xff8200;
const VIDEO_DISPLAY_REGISTER_END: u32 = 0xFF8260;

const RESERVED: u32 = 0xFF8400;
const DMA_DISK_CONTROLLER: u32 = 0xFF8600;

const YM2149_START: u32 = 0xFF8800;
const YM2149_END: u32 = 0xFF8804;

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

#[derive(Clone)]
pub struct Mmu {
    pub rom: Vec<u8>,
    pub cart: Vec<u8>,
    pub memory: Vec<u8>,
    pub ym2149: [u8; 4],
    pub video_display: [u8; 96],
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
            VIDEO_DISPLAY_REGISTER_START..=VIDEO_DISPLAY_REGISTER_END => {
                unimplemented!("video read");
                //self.video_display[(address) as usize]
            }
            a => self.memory[a as usize],
        }
    }

    pub fn write_byte(& mut self, destination: u32, data: u8) {
        let address = destination & 0xffffff; //clip to max mem
        match address {
            a if a < 8 => panic!("Memory error:${0:08x}, {0}, {0:024b}", destination),
            ROM_START..ROM_END => panic!("Attempt to write to Rom: ${:08x}", destination),
            CART_START..CART_END => panic!("Attempt to write to Cart: ${:08x}", destination),
            VIDEO_DISPLAY_REGISTER_START..VIDEO_DISPLAY_REGISTER_END => unimplemented!("write to Video Display Register: {:0x}", address),
            //YM2149 => ym2149IOMemory[(destination-ym2149Start)] = data
            _ => self.memory[destination as usize] = data,
        }
    }

    pub fn read_word(& mut self, address: u32) -> u16 {
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
            VIDEO_DISPLAY_REGISTER_START..VIDEO_DISPLAY_REGISTER_END => {
                unimplemented!("video read")
                //word_from_slice(&self.video_display, address)
            }
            a => word_from_slice(&self.memory, a),
        }
    }

    pub fn write_word(& mut self, destination: u32, data: u16) {
        let address = destination & 0xffffff; //clip to max mem
        match address {
            a if a < 8 => panic!("Memory error:${0:08x}, {0}, {0:024b}", destination),
            ROM_START..ROM_END => panic!("Attempt to write to Rom: ${:08x}", destination),
            CART_START..CART_END => panic!("Attempt to write to Cart: ${:08x}", destination),
            VIDEO_DISPLAY_REGISTER_START..VIDEO_DISPLAY_REGISTER_END => unimplemented!(
                "not implemented write to Video Display Register: {:0x}",
                address
            ),
            //YM2149 {
            //     ym2149IOMemory[(destination-ym2149Start)] = (data >> 8)
            //     ym2149IOMemory[(destination-ym2149Start + 1)] = data
            //}
            _ => {
                self.memory[destination as usize] = (data >> 8) as u8;
                self.memory[(destination + 1) as usize] = (data & 0xff) as u8
            }
        }
    }

    pub fn read_long(&self, address: u32) -> u32 {
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
}
