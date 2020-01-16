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
            0xFF8201 => (self.video_base_high << 8) as u16 | 0x00,
            0xFF8203 => (self.video_base_medium<< 8) as u16 | 0x00,
            0xFF8205 => (self.video_address_counter_high << 8) as u16 | 0x00,
            0xFF8207 => (self.video_address_counter_medium << 8) as u16 | 0x00,
            0xFF8209 => (self.video_address_counter_low << 8) as u16 | 0x00,
            0xFF820A => (self.sync_mode << 2) as u16 | 0x00,
            0xFF8240..0xFF8260 => {
                let b1 = (self.read_byte(address) as u16) << 8;
                let b2 = self.read_byte(address + 1) as u16;
                let result = b1 | b2;
                result

            },
            0xFFFF8260 => (self.screen_resolution << 8) as u16 | 0x00,
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