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

#[derive(Clone)]
pub struct Mmu {
    pub memory_configuration: u8,
    pub rom: Vec<u8>,
    pub cart: Vec<u8>,
    pub memory: Vec<u8>,
    pub sound: Sound,
    pub video: Video,
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
            MEMORY_CONFIGURATION => self.memory_configuration = data,
            _ => self.memory[address as usize] = data,
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
                if address > self.memory.len() as u32 {
                    println!("not writing to ${:08x} as its not in memory", address)
                } else {
                    self.memory[address as usize] = (data >> 8) as u8;
                    self.memory[(address + 1) as usize] = (data & 0xff) as u8
                }
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

#[derive(Clone)]
pub struct Video {
    video_base_high: u8,
    video_base_medium: u8,
    video_address_counter_high: u8,
    video_address_counter_medium: u8,
    video_address_counter_low: u8,
    sync_mode: u8,
    palette_colour: [u8; 32],
    screen_resolution: u8,
}

impl Video {
    pub fn new() -> Video {
        Video {
            video_base_high: 0,
            video_base_medium: 0,
            video_address_counter_high: 0,
            video_address_counter_medium: 0,
            video_address_counter_low: 0,
            sync_mode: 0,
            palette_colour: [0; 32],
            screen_resolution: 0
        }
    }

    pub fn print_palette_registers(&self) {
        println!("{:04x}", self.read_word(0xFF8240));
        println!("{:04x}", self.read_word(0xFF8242));
        println!("{:04x}", self.read_word(0xFF8244));
        println!("{:04x}", self.read_word(0xFF8246));
        println!("{:04x}", self.read_word(0xFF8248));
        println!("{:04x}", self.read_word(0xFF824a));
        println!("{:04x}", self.read_word(0xFF824c));
        println!("{:04x}", self.read_word(0xFF824e));
        println!("{:04x}", self.read_word(0xFF8250));
        println!("{:04x}", self.read_word(0xFF8252));
        println!("{:04x}", self.read_word(0xFF8254));
        println!("{:04x}", self.read_word(0xFF8256));
        println!("{:04x}", self.read_word(0xFF8258));
        println!("{:04x}", self.read_word(0xFF825a));
        println!("{:04x}", self.read_word(0xFF825c));
        println!("{:04x}", self.read_word(0xFF825e));
    }

    pub fn read_byte(&self, address: u32) -> u8 {
        match address {
            0xFF8201 => self.video_base_high,
            0xFF8203 => self.video_base_medium,
            0xFF8205 => self.video_address_counter_high & 0x3f,
            0xFF8207 => self.video_address_counter_medium,
            0xFF8209 => self.video_address_counter_low & 0xfe,
            0xFF820A => self.sync_mode & 0x3,
            0xFF8240..0xFF8260 => {
                self.palette_colour[ (address - 0xFF8240) as usize] as u8
            },
            0xFF8260 => self.screen_resolution & 0x3,
            _ => panic!("invalid video register")
        }
    }

    pub fn write_byte(& mut self, address: u32, data: u8) {
        match address {
            0xFF8201 => self.video_base_high = data,
            0xFF8203 => self.video_base_medium = data,
            0xFF8205 => panic!("read only video_address_counter_high"),
            0xFF8207 => panic!("read only video_address_counter_medium"),
            0xFF8209 => panic!("read only video_address_counter_low"),
            0xFF820A => self.sync_mode = data,
            0xFF8240..0xFF8260 => self.palette_colour[ (address - 0xFF8240) as usize] = data,
            0xFF8260 => self.screen_resolution = data,
            _ => panic!("invalid video register")
        }
    }

    pub fn read_word(&self, address: u32) -> u16 {
        match address {
            0xFF8201 => (self.video_base_high as u16) << 8 | 0x00,
            0xFF8203 => (self.video_base_medium as u16) << 8 | 0x00,
            0xFF8205 => (self.video_address_counter_high as u16) << 8 | 0x00,
            0xFF8207 => (self.video_address_counter_medium as u16) << 8 | 0x00,
            0xFF8209 => (self.video_address_counter_low as u16) << 8 | 0x00,
            0xFF820A => (self.sync_mode << 2) as u16 | 0x00,
            0xFF8240..0xFF8260 => {
                let b1 = (self.read_byte(address) as u16) << 8;
                let b2 = self.read_byte(address + 1) as u16;
                let result = b1 | b2;
                result

            },
            0xFFFF8260 => (self.screen_resolution as u16) << 8 | 0x00,
            _ => panic!("invalid video register")
        }
    }

    pub fn write_word(& mut self, address: u32, data: u16) {
        match address {
            0xff8201 => todo!(), // self.video_base_high,
            0xFF8203 => todo!(), //self.video_base_medium,
            0xFF8205 => todo!(), //self.video_address_counter_high & 0x3f,
            0xFF8207 => todo!(), //self.video_address_counter_medium,
            0xFF8209 => todo!(), //self.video_address_counter_low & 0xfe,
            0xFF820A => todo!(), //self.sync_mode & 0x3,
            0xFF8240..0xFF8260 => {
                self.write_byte(address, (data >> 8) as u8);
                self.write_byte(address + 1, (data & 0xff) as u8)

            },
            0xFF8260 => todo!(), //self.screen_resolution & 0x3,
            _ => panic!("invalid video register")
        }
    }
}
