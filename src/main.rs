use std::{
    collections::HashMap,
    fs::File,
    io::{self, BufReader, BufWriter, Read, Write},
};

/// Compiled instruction set
#[derive(Debug, Clone)]
enum Op {
    Add(u8),
    Sub(u8),
    Right(usize),
    Left(usize),
    Output,
    Input,
    JumpForward(usize),
    JumpBack(usize),
    Clear,
    MulLoop(Vec<(isize, u8)>),
}

fn parse_rle(source: &[u8]) -> Vec<Op> {
    let mut ops = Vec::new();
    let mut i = 0;
    while i < source.len() {
        let b = source[i];
        let run = |ch: u8| source[i..].iter().take_while(|&&x| x == ch).count();
        match b {
            b'+' => {
                let n = run(b'+');
                ops.push(Op::Add(n as u8));
                i += n;
            }
            b'-' => {
                let n = run(b'-');
                ops.push(Op::Sub(n as u8));
                i += n;
            }
            b'>' => {
                let n = run(b'>');
                ops.push(Op::Right(n));
                i += n;
            }
            b'<' => {
                let n = run(b'<');
                ops.push(Op::Left(n));
                i += n;
            }
            b'.' => {
                ops.push(Op::Output);
                i += 1;
            }
            b',' => {
                ops.push(Op::Input);
                i += 1;
            }
            b'[' => {
                ops.push(Op::JumpForward(0));
                i += 1;
            }
            b']' => {
                ops.push(Op::JumpBack(0));
                i += 1;
            }
            _ => {
                i += 1;
            }
        }
    }
    ops
}

fn patch_jumps(ops: &mut Vec<Op>) {
    let mut stack: Vec<usize> = Vec::new();
    for i in 0..ops.len() {
        match ops[i] {
            Op::JumpForward(_) => stack.push(i),
            Op::JumpBack(_) => {
                let open = stack.pop().expect("unmatched ]");
                ops[open] = Op::JumpForward(i);
                ops[i] = Op::JumpBack(open);
            }
            _ => {}
        }
    }
    if !stack.is_empty() {
        panic!("unmatched [");
    }
}

fn try_mul_loop(body: &[Op]) -> Option<Vec<(isize, u8)>> {
    let mut offset: isize = 0;
    let mut changes: HashMap<isize, i32> = HashMap::new();

    for op in body {
        match op {
            Op::Add(n) => *changes.entry(offset).or_insert(0) += *n as i32,
            Op::Sub(n) => *changes.entry(offset).or_insert(0) -= *n as i32,
            Op::Right(n) => offset += *n as isize,
            Op::Left(n) => offset -= *n as isize,
            _ => return None,
        }
    }

    if offset != 0 {
        return None;
    } // pointer must return to start
    if *changes.get(&0).unwrap_or(&0) != -1 {
        return None;
    } // cell[0] must be -1/iter

    let muls: Vec<(isize, u8)> = changes
        .into_iter()
        .filter(|(off, _)| *off != 0)
        .map(|(off, delta)| (off, delta as u8))
        .collect();

    Some(muls)
}

fn optimize(ops: Vec<Op>) -> Vec<Op> {
    let mut result: Vec<Op> = Vec::with_capacity(ops.len());
    let mut i = 0;

    while i < ops.len() {
        if let Op::JumpForward(close) = ops[i] {
            let body = &ops[i + 1..close];

            // [-] or [+]  →  Clear
            if body.len() == 1 {
                match body[0] {
                    Op::Sub(1) | Op::Add(1) => {
                        result.push(Op::Clear);
                        i = close + 1;
                        continue;
                    }
                    _ => {}
                }
            }

            // multiply / copy loop
            if let Some(muls) = try_mul_loop(body) {
                result.push(Op::MulLoop(muls));
                i = close + 1;
                continue;
            }

            result.push(Op::JumpForward(0));
            i += 1;
        } else {
            result.push(ops[i].clone());
            i += 1;
        }
    }

    patch_jumps(&mut result);
    result
}

fn compile(source: &[u8]) -> Vec<Op> {
    let mut ops = parse_rle(source);
    patch_jumps(&mut ops);
    optimize(ops)
}

fn run(ops: &[Op], out: &mut impl Write) {
    let mut buf = vec![0u8; 65536];
    let mut pos: usize = 0;
    let mut pc: usize = 0;

    while pc < ops.len() {
        match &ops[pc] {
            Op::Add(n) => buf[pos] = buf[pos].wrapping_add(*n),
            Op::Sub(n) => buf[pos] = buf[pos].wrapping_sub(*n),
            Op::Right(n) => pos += n,
            Op::Left(n) => pos -= n,
            Op::Output => out.write_all(&[buf[pos]]).unwrap(),
            Op::Input => buf[pos] = io::stdin().bytes().next().unwrap().unwrap(),
            Op::JumpForward(target) => {
                if buf[pos] == 0 {
                    pc = *target;
                }
            }
            Op::JumpBack(target) => {
                if buf[pos] != 0 {
                    pc = *target;
                }
            }
            Op::Clear => buf[pos] = 0,
            Op::MulLoop(muls) => {
                if buf[pos] != 0 {
                    let val = buf[pos] as u32;
                    for &(offset, factor) in muls {
                        let t = (pos as isize + offset) as usize;
                        buf[t] = buf[t].wrapping_add((val.wrapping_mul(factor as u32)) as u8);
                    }
                    buf[pos] = 0;
                }
            }
        }
        pc += 1;
    }
}

fn main() {
    let path = std::env::args().nth(1).expect("usage: ff <file>");
    let source = {
        let file = File::open(&path).unwrap();
        let mut reader = BufReader::new(file);
        let mut src = Vec::new();
        reader.read_to_end(&mut src).unwrap();
        src
    };

    let ops = compile(&source);

    let stdout = io::stdout();
    let mut out = BufWriter::new(stdout.lock());
    run(&ops, &mut out);
}
