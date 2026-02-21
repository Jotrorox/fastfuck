use std::{
    fs::File,
    io::{self, BufReader, BufWriter, Read, Write},
};

fn build_jump_table(source: &[u8]) -> Vec<usize> {
    let mut table = vec![0usize; source.len()];
    let mut stack = Vec::new();
    for (i, &byte) in source.iter().enumerate() {
        match byte as char {
            '[' => stack.push(i),
            ']' => {
                let open = stack.pop().expect("unmatched ]");
                table[open] = i;
                table[i] = open;
            }
            _ => {}
        }
    }
    if !stack.is_empty() {
        panic!("unmatched [");
    }
    table
}

fn main() {
    let mut buf: [u8; 1024 * 64] = [0; 1024 * 64];
    let mut pos: usize = 0;

    let path = std::env::args().nth(1).expect("usage: ff <file>");
    let file = File::open(&path).unwrap();

    let mut reader = BufReader::new(file);
    let mut source = Vec::new();
    reader.read_to_end(&mut source).unwrap();

    let stdout = io::stdout();
    let mut out = BufWriter::new(stdout.lock());

    let mut pc: usize = 0;
    let jump_table = build_jump_table(&source);

    while pc < source.len() {
        let ch = source[pc] as char;
        match ch {
            '+' => buf[pos] = buf[pos].wrapping_add(1),
            '-' => buf[pos] = buf[pos].wrapping_sub(1),
            '<' => pos -= 1,
            '>' => pos += 1,
            '.' => out.write_all(&[buf[pos]]).unwrap(),
            ',' => buf[pos] = std::io::stdin().bytes().next().unwrap().unwrap(),
            '[' => {
                if buf[pos] == 0 {
                    pc = jump_table[pc];
                }
            }
            ']' => {
                if buf[pos] != 0 {
                    pc = jump_table[pc];
                }
            }
            _ => {}
        }
        pc += 1;
    }
}
