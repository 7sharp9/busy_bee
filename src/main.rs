use std::fs;
use std::io;

fn main() -> Result<(), io::Error> {
    let rom = fs::read("/Users/davethomas/Documents/68000/TOS100UK.IMG")?;

    let mut cpu = CPU {
        a: [0; 8],
        d: [0; 8],
        ccr: 0,
        pc: 0,
        mmu: Mmu {
            rom: rom,
            cart: vec![0],
            memory: vec![0; 1048576],
            ym2149: [0; 4],
            video_display: [0; 96],
        },
    };

    //cpu.reset();
    cpu.reset();
    println!("\r\n{:?}", cpu);
    Ok(())
}
