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
                self.video_display[(address) as usize]
            }
            a => self.memory[a as usize],
        }
    }

    pub fn read_word(&self, address: u32) -> u16 {
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
                unimplemented!("video read");
                word_from_slice(&self.video_display, address)
            }
            a => word_from_slice(&self.memory, a),
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
            CART_START..CART_END => {
                //todo!();
                0xffffffff
            }
            VIDEO_DISPLAY_REGISTER_START..VIDEO_DISPLAY_REGISTER_END => {
                unimplemented!("video read");
                long_from_slice(&self.video_display, address)
            }
            a => long_from_slice(&self.memory, a),
        }
    }
}
