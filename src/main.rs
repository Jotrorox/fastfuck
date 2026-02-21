use std::{
    collections::HashMap,
    fs::File,
    io::{self, BufReader, BufWriter, Read, Write},
};

const TAPE_SIZE: usize = 65536;

#[derive(Debug, Clone)]
enum IrOp {
    Add(u8),
    Sub(u8),
    Right(usize),
    Left(usize),
    Output,
    Input,
    JumpForward(usize),
    JumpBack(usize),
    Clear,
    MulLoop { step: u8, muls: Vec<(isize, i32)> },
    ScanRight,
    ScanLeft,
}

#[derive(Debug, Clone)]
enum Op {
    Add(isize, u8),
    Sub(isize, u8),
    Clear(isize),
    Output(isize),
    Input(isize),
    Move(isize),
    JumpForward(usize),
    JumpBack(usize),
    MulLoop { step: u8, muls: Vec<(isize, i32)> },
    ScanRight,
    ScanLeft,
}

fn parse_rle(source: &[u8]) -> Vec<IrOp> {
    let mut ops = Vec::new();
    let mut i = 0;
    while i < source.len() {
        let b = source[i];
        let run = |ch: u8| source[i..].iter().take_while(|&&x| x == ch).count();
        match b {
            b'+' => {
                let n = run(b'+');
                ops.push(IrOp::Add(n as u8));
                i += n;
            }
            b'-' => {
                let n = run(b'-');
                ops.push(IrOp::Sub(n as u8));
                i += n;
            }
            b'>' => {
                let n = run(b'>');
                ops.push(IrOp::Right(n));
                i += n;
            }
            b'<' => {
                let n = run(b'<');
                ops.push(IrOp::Left(n));
                i += n;
            }
            b'.' => {
                ops.push(IrOp::Output);
                i += 1;
            }
            b',' => {
                ops.push(IrOp::Input);
                i += 1;
            }
            b'[' => {
                ops.push(IrOp::JumpForward(0));
                i += 1;
            }
            b']' => {
                ops.push(IrOp::JumpBack(0));
                i += 1;
            }
            _ => {
                i += 1;
            }
        }
    }
    ops
}

fn patch_ir_jumps(ops: &mut [IrOp]) {
    let mut stack: Vec<usize> = Vec::new();
    for i in 0..ops.len() {
        match ops[i] {
            IrOp::JumpForward(_) => stack.push(i),
            IrOp::JumpBack(_) => {
                let open = stack.pop().expect("unmatched ]");
                ops[open] = IrOp::JumpForward(i);
                ops[i] = IrOp::JumpBack(open);
            }
            _ => {}
        }
    }
    assert!(stack.is_empty(), "unmatched [");
}

fn patch_op_jumps(ops: &mut [Op]) {
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
    assert!(stack.is_empty(), "unmatched [");
}

fn try_mul_loop(body: &[IrOp]) -> Option<(u8, Vec<(isize, i32)>)> {
    let mut ptr: isize = 0;
    let mut changes: HashMap<isize, i32> = HashMap::new();

    for op in body {
        match op {
            IrOp::Add(n) => *changes.entry(ptr).or_insert(0) += *n as i32,
            IrOp::Sub(n) => *changes.entry(ptr).or_insert(0) -= *n as i32,
            IrOp::Right(n) => ptr += *n as isize,
            IrOp::Left(n) => ptr -= *n as isize,
            _ => return None,
        }
    }

    if ptr != 0 {
        return None;
    }

    let step_neg = *changes.get(&0).unwrap_or(&0);
    if step_neg >= 0 {
        return None;
    } // cell[0] must decrease

    let step = (-step_neg) as u8; // 1, 2, 3 …

    let muls: Vec<(isize, i32)> = changes.into_iter().filter(|&(off, _)| off != 0).collect();

    Some((step, muls))
}

fn optimize(ops: Vec<IrOp>) -> Vec<IrOp> {
    let mut result: Vec<IrOp> = Vec::with_capacity(ops.len());
    let mut i = 0;

    while i < ops.len() {
        if let IrOp::JumpForward(close) = ops[i] {
            let body = &ops[i + 1..close];

            if body.len() == 1 {
                match body[0] {
                    IrOp::Sub(1) | IrOp::Add(1) => {
                        result.push(IrOp::Clear);
                        i = close + 1;
                        continue;
                    }
                    IrOp::Right(1) => {
                        result.push(IrOp::ScanRight);
                        i = close + 1;
                        continue;
                    }
                    IrOp::Left(1) => {
                        result.push(IrOp::ScanLeft);
                        i = close + 1;
                        continue;
                    }
                    _ => {}
                }
            }

            if let Some((step, muls)) = try_mul_loop(body) {
                result.push(IrOp::MulLoop { step, muls });
                i = close + 1;
                continue;
            }

            result.push(IrOp::JumpForward(0));
            i += 1;
        } else {
            result.push(ops[i].clone());
            i += 1;
        }
    }

    patch_ir_jumps(&mut result);
    result
}

fn fold_offsets(ir: Vec<IrOp>) -> Vec<Op> {
    let mut ops: Vec<Op> = Vec::with_capacity(ir.len());
    let mut pending: isize = 0;

    for irop in ir {
        match irop {
            IrOp::Right(n) => pending += n as isize,
            IrOp::Left(n) => pending -= n as isize,

            IrOp::Add(n) => ops.push(Op::Add(pending, n)),
            IrOp::Sub(n) => ops.push(Op::Sub(pending, n)),
            IrOp::Clear => ops.push(Op::Clear(pending)),
            IrOp::Output => ops.push(Op::Output(pending)),
            IrOp::Input => ops.push(Op::Input(pending)),

            IrOp::JumpForward(_) => {
                if pending != 0 {
                    ops.push(Op::Move(pending));
                    pending = 0;
                }
                ops.push(Op::JumpForward(0));
            }
            IrOp::JumpBack(_) => {
                if pending != 0 {
                    ops.push(Op::Move(pending));
                    pending = 0;
                }
                ops.push(Op::JumpBack(0));
            }

            IrOp::MulLoop { step, muls } => {
                if pending != 0 {
                    ops.push(Op::Move(pending));
                    pending = 0;
                }
                ops.push(Op::MulLoop { step, muls });
            }
            IrOp::ScanRight => {
                if pending != 0 {
                    ops.push(Op::Move(pending));
                    pending = 0;
                }
                ops.push(Op::ScanRight);
            }
            IrOp::ScanLeft => {
                if pending != 0 {
                    ops.push(Op::Move(pending));
                    pending = 0;
                }
                ops.push(Op::ScanLeft);
            }
        }
    }

    if pending != 0 {
        ops.push(Op::Move(pending));
    }

    ops
}

fn compile(source: &[u8]) -> Vec<Op> {
    let mut ir = parse_rle(source);
    patch_ir_jumps(&mut ir);
    let ir = optimize(ir);
    let mut ops = fold_offsets(ir);
    patch_op_jumps(&mut ops);
    ops
}

#[inline(always)]
unsafe fn rd(buf: &[u8], pos: usize, off: isize) -> u8 {
    *buf.get_unchecked((pos as isize + off) as usize)
}

#[inline(always)]
unsafe fn wr(buf: &mut [u8], pos: usize, off: isize) -> &mut u8 {
    buf.get_unchecked_mut((pos as isize + off) as usize)
}

fn run(ops: &[Op], out: &mut impl Write) {
    let mut buf = vec![0u8; TAPE_SIZE];
    let mut pos: usize = 0;
    let mut pc: usize = 0;

    while pc < ops.len() {
        match unsafe { ops.get_unchecked(pc) } {
            Op::Add(off, n) => unsafe {
                let c = wr(&mut buf, pos, *off);
                *c = c.wrapping_add(*n);
            },
            Op::Sub(off, n) => unsafe {
                let c = wr(&mut buf, pos, *off);
                *c = c.wrapping_sub(*n);
            },
            Op::Clear(off) => unsafe {
                *wr(&mut buf, pos, *off) = 0;
            },
            Op::Output(off) => {
                out.write_all(&[unsafe { rd(&buf, pos, *off) }]).unwrap();
            }
            Op::Input(off) => unsafe {
                *wr(&mut buf, pos, *off) = io::stdin().bytes().next().unwrap().unwrap();
            },

            Op::Move(delta) => {
                pos = (pos as isize + delta) as usize;
            }

            Op::JumpForward(target) => {
                if unsafe { rd(&buf, pos, 0) } == 0 {
                    pc = *target;
                }
            }
            Op::JumpBack(target) => {
                if unsafe { rd(&buf, pos, 0) } != 0 {
                    pc = *target;
                }
            }

            Op::MulLoop { step, muls } => {
                let val = unsafe { rd(&buf, pos, 0) };
                if val != 0 {
                    let count = val as i32 / *step as i32;
                    for &(off, delta) in muls {
                        unsafe {
                            let c = wr(&mut buf, pos, off);
                            *c = c.wrapping_add((count * delta) as u8);
                        }
                    }
                    unsafe {
                        *wr(&mut buf, pos, 0) = 0;
                    }
                }
            }

            Op::ScanRight => {
                let slice = unsafe { buf.get_unchecked(pos..) };
                pos += slice
                    .iter()
                    .position(|&b| b == 0)
                    .expect("ScanRight: no zero on tape");
            }
            Op::ScanLeft => {
                let slice = unsafe { buf.get_unchecked(..=pos) };
                pos -= slice
                    .iter()
                    .rev()
                    .position(|&b| b == 0)
                    .expect("ScanLeft: no zero on tape");
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
