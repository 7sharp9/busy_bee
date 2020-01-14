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
            0xffff8201 => self.video_base_high,
            0xFFFF8203 => self.video_base_medium,
            0xFFFF8205 => self.video_address_counter_high & 0x3f,
            0xFFFF8207 => self.video_address_counter_medium,
            0xFFFF8209 => self.video_address_counter_low & 0xfe,
            0xFFFF820A => self.sync_mode & 0x3,
            0xFFFF8240..0xFFFF8260 => {
                self.palette_colour[ (address - 0xFF8240) as usize] as u8
            },
            0xFFFF8260 => self.screen_resolution & 0x3,
            _ => panic!("invalid video register")
        }
    }

    pub fn write_byte(& mut self, address: u32, data: u8) {
        match address {
            0xFFFF8201 => self.video_base_high = data,
            0xFFFF8203 => self.video_base_medium = data,
            0xFFFF8205 => self.video_address_counter_high = data,
            0xFFFF8207 => self.video_address_counter_medium = data,
            0xFFFF8209 => self.video_address_counter_low = data,
            0xFFFF820A => self.sync_mode = data,
            0xFFFF8240..0xFFFF8260 => self.palette_colour[ (address - 0xFFFF8240) as usize] = data,
            0xFFFF8260 => self.screen_resolution = data,
            _ => panic!("invalid video register")
        }
    }

    pub fn write_word(& mut self, address: u32, data: u16) {
        match address {
            0xffff8201 => todo!(), // self.video_base_high,
            0xFFFF8203 => todo!(), //self.video_base_medium,
            0xFFFF8205 => todo!(), //self.video_address_counter_high & 0x3f,
            0xFFFF8207 => todo!(), //self.video_address_counter_medium,
            0xFFFF8209 => todo!(), //self.video_address_counter_low & 0xfe,
            0xFFFF820A => todo!(), //self.sync_mode & 0x3,
            0xFFFF8240..0xFFFF8260 => {
                self.write_byte(address, (data >> 8) as u8);
                self.write_byte(address + 1, (data & 0xff) as u8)

            },
            0xFFFF8260 => todo!(), //self.screen_resolution & 0x3,
            _ => panic!("invalid video register")
        }
    }
}