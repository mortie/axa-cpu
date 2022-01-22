use std::cell::Cell;
use super::emulator;

const DISPLAY_WIDTH: usize = 32;
const DISPLAY_HEIGHT: usize = 32;

pub struct DisplayDevice {
    data: Vec<Cell<u8>>,
    x: Cell<Option<u8>>,
}

impl DisplayDevice {
    fn new() -> Self {
        let mut data: Vec<Cell<u8>> = Vec::new();
        data.resize((DISPLAY_WIDTH / 8) * DISPLAY_HEIGHT, Cell::new(0));
        Self {
            data,
            x: Cell::new(None),
        }
    }

    fn get_pixel(&self, x: usize, y: usize) -> bool {
        let xbyte = x / 8;
        let xbit = x % 8;
        let cell = &self.data[y * (DISPLAY_WIDTH / 8) + xbyte];
        let byte = cell.get();
        match byte & (1 << xbit) {
            0 => false,
            _ => true,
        }
    }

    fn set_pixel(&self, x: usize, y: usize) {
        let xbyte = x / 8;
        let xbit = x % 8;
        let cell = &self.data[y * (DISPLAY_WIDTH / 8) + xbyte];
        let mut byte = cell.get();
        byte |= 1 << xbit;
        cell.set(byte);
    }

    fn render(&self) {
        print!("\x1bc");
        print!("╔");
        for _ in 0..DISPLAY_WIDTH {
            print!("═");
        }
        println!("╗");

        for y in 0..(DISPLAY_HEIGHT / 2) {
            print!("║");
            let y = y * 2;
            for x in 0..DISPLAY_WIDTH {
                let a = self.get_pixel(x, y);
                let b = self.get_pixel(x, y + 1);

                if a && b {
                    print!("█");
                } else if a && !b {
                    print!("▀");
                } else if !a && b {
                    print!("▄");
                } else {
                    print!(" ");
                }
            }
            print!("║\n");
        }

        print!("╚");
        for _ in 0..DISPLAY_WIDTH {
            print!("═");
        }
        println!("╝");
    }

    fn store(&self, _offset: u16, value: u8) {
        if value == 0xffu8 {
            self.render();
            for cell in &self.data {
                cell.set(0);
            }
            self.x.set(None);
        } else if let Some(x) = self.x.get() {
            self.set_pixel(x as usize, value as usize);
            self.x.set(None);
        } else {
            self.x.set(Some(value));
        }
    }
}

pub struct Memory {
    pub data: Vec<Cell<u8>>,
    pub halt: Cell<bool>,
    pub display: DisplayDevice,
}

impl Memory {
    pub fn new() -> Self {
        let mut data: Vec<Cell<u8>> = Vec::new();
        data.resize(1024, Cell::new(0));
        Self {
            data,
            halt: Cell::new(false),
            display: DisplayDevice::new(),
        }
    }
}

impl emulator::Memory for Memory {
    fn load(&self, addr: u16) -> u8 {
        if (addr as usize) < self.data.len() {
            return self.data[addr as usize].get();
        }

        if addr == 0x8000 {
            return 0;
        }

        if addr == 0xff00 {
            return 0;
        }

        if addr == 0xffff {
            return 0;
        }

        println!("Load from wild address {}!", addr);
        0
    }

    fn store(&self, addr: u16, value: u8) {
        if (addr as usize) < self.data.len() {
            self.data[addr as usize].set(value);
            return;
        }

        if addr == 0x8000 {
            return self.display.store(0, value);
        }

        if addr == 0xff00 {
            println!("Debug store: {}", value);
            return;
        }

        if addr == 0xffff {
            self.halt.set(true);
            return;
        }
    }
}
