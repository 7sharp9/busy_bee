use busy_bee::mmu::{Mmu, Sound, Video};
use busy_bee::CPU;
use std::fs;
use std::io;

//I forget if this was just another way of doing the read_word function or if there was something notable about it?
fn read_word(memory: [u8; 4096], index: u16) -> u16 {
    (memory[index as usize] as u16) << 8 | (memory[(index + 1) as usize] as u16)
}

fn main() -> Result<(), io::Error> {
    let rom = fs::read("/Users/davethomas/Documents/68000/TOS100UK.IMG")?;

    let mut cpu = CPU {
        a: [0; 8],
        d: [0; 8],
        t1_flag: false,
        t0_flag: false,
        s_flag: false,
        m_flag: false,
        int_mask: 0,
        irq_level: 0,
        x_flag: false,
        n_flag: false,
        z_flag: false,
        v_flag: false,
        c_flag: false,

        pc: 0,
        mmu: Mmu {
            rom: rom,
            cart: vec![0],
            memory: vec![0; 1048576],
            memory_configuration: 0b0101, //101 = 512/512
            sound: Sound::new(),
            video: Video::new()
        },
    };

    //cpu.reset();
    cpu.reset();
    for _ in 1..=2000 {
        cpu.step();
    }
    println!("\r\n{:?}", cpu);

    cpu.step();

    println!("\r\n{:?}", cpu);
    Ok(())
}
