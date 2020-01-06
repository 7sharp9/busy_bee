#[cfg(test)]
mod tests {
    use busy_bee::{mmu::Mmu, CPU};
    #[test]

    fn reset() {
        let mut cpu = CPU {
            a: [0; 8],
            d: [0; 8],
            ccr: 0,
            pc: 0,
            mmu: Mmu {
                rom: [0x60, 0x1E, 0x01, 0x00, 0x00, 0xFC, 0x00, 0x20].to_vec(),
                cart: vec![0],
                memory: vec![0; 1048576],
                ym2149: [0; 4],
                video_display: [0; 96],
            },
        };
        cpu.reset();
        assert_eq!(
            cpu.a[7],
            0x0601E0100,
            "we are testing that a7:{} is set with the value mirrored from location 0: {}",
            cpu.a[7],
            cpu.mmu.read_long(0)
        );

        assert_eq!(
            cpu.pc,
            0xFC0020,
            "We are testing that the pc:{} is reset with the value mirrored from location 4: {}",
            cpu.pc,
            cpu.mmu.read_long(4)
        );
    }
}