use std::{
    fs::File,
    io::{BufReader, Read},
};

fn main() {
    let mut buf: [u8; 1024 * 64] = [0; 1024 * 64];
    let mut pos: usize = 0;

    let file = File::open("examples/hello.b").unwrap();
    let mut reader = BufReader::new(file);
    let mut source = Vec::new();
    reader.read_to_end(&mut source).unwrap();

    let mut pc: usize = 0;

    while pc < source.len() {
        let ch = source[pc] as char;
        match ch {
            '+' => buf[pos] = buf[pos].wrapping_add(1),
            '-' => buf[pos] = buf[pos].wrapping_sub(1),
            '<' => pos -= 1,
            '>' => pos += 1,
            '.' => print!("{}", buf[pos] as char),
            ',' => buf[pos] = std::io::stdin().bytes().next().unwrap().unwrap(),
            '[' => {
                if buf[pos] == 0 {
                    let mut depth = 1;
                    while depth > 0 {
                        pc += 1;
                        match source[pc] as char {
                            '[' => depth += 1,
                            ']' => depth -= 1,
                            _ => {}
                        }
                    }
                }
            }
            ']' => {
                if buf[pos] != 0 {
                    let mut depth = 1;
                    while depth > 0 {
                        pc -= 1;
                        match source[pc] as char {
                            ']' => depth += 1,
                            '[' => depth -= 1,
                            _ => {}
                        }
                    }
                }
            }
            _ => {}
        }
        pc += 1;
    }
}
