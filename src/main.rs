use cranelift::{
    jit::{JITBuilder, JITModule},
    module::{Linkage, Module},
    prelude::*,
};
use mimalloc::MiMalloc;
use std::{
    collections::HashMap,
    fs::File,
    io::{self, BufReader, Read, Write},
    mem,
};

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

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
    JumpBack,
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
                ops.push(IrOp::JumpBack);
                i += 1;
            }
            _ => i += 1,
        }
    }
    ops
}

fn patch_ir_jumps(ops: &mut [IrOp]) {
    let mut stack = Vec::new();
    for i in 0..ops.len() {
        match ops[i] {
            IrOp::JumpForward(_) => stack.push(i),
            IrOp::JumpBack => {
                let open = stack.pop().expect("unmatched ]");
                ops[open] = IrOp::JumpForward(i);
                ops[i] = IrOp::JumpBack;
            }
            _ => {}
        }
    }
    assert!(stack.is_empty(), "unmatched [");
}

fn patch_op_jumps(ops: &mut [Op]) {
    let mut stack = Vec::new();
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
    let mut ptr = 0;
    let mut changes = HashMap::new();

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
    }

    let step = (-step_neg) as u8;
    let muls: Vec<_> = changes.into_iter().filter(|&(off, _)| off != 0).collect();

    Some((step, muls))
}

fn optimize(ops: Vec<IrOp>) -> Vec<IrOp> {
    let mut result = Vec::with_capacity(ops.len());
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
    let mut ops = Vec::with_capacity(ir.len());
    let mut pending: isize = 0;

    let flush = |ops: &mut Vec<Op>, pending: &mut isize| {
        if *pending != 0 {
            ops.push(Op::Move(*pending));
            *pending = 0;
        }
    };

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
                flush(&mut ops, &mut pending);
                ops.push(Op::JumpForward(0));
            }
            IrOp::JumpBack => {
                flush(&mut ops, &mut pending);
                ops.push(Op::JumpBack(0));
            }
            IrOp::MulLoop { step, muls } => {
                flush(&mut ops, &mut pending);
                ops.push(Op::MulLoop { step, muls });
            }
            IrOp::ScanRight => {
                flush(&mut ops, &mut pending);
                ops.push(Op::ScanRight);
            }
            IrOp::ScanLeft => {
                flush(&mut ops, &mut pending);
                ops.push(Op::ScanLeft);
            }
        }
    }

    flush(&mut ops, &mut pending);
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

extern "C" fn bf_putchar(c: u8) {
    io::stdout().write_all(&[c]).unwrap();
    io::stdout().flush().unwrap();
}

extern "C" fn bf_getchar() -> u8 {
    io::stdin().bytes().next().unwrap_or(Ok(0)).unwrap_or(0)
}

fn run_jit(ops: &[Op]) {
    let mut flag_builder = settings::builder();
    flag_builder.set("use_colocated_libcalls", "false").unwrap();
    flag_builder.set("is_pic", "false").unwrap();
    flag_builder.set("opt_level", "speed").unwrap();

    let isa_builder = cranelift::native::builder().expect("Host machine is not supported");
    let isa = isa_builder
        .finish(settings::Flags::new(flag_builder))
        .unwrap();
    let mut builder = JITBuilder::with_isa(isa, cranelift::module::default_libcall_names());

    builder.symbol("bf_putchar", bf_putchar as *const u8);
    builder.symbol("bf_getchar", bf_getchar as *const u8);

    let mut module = JITModule::new(builder);
    let mut ctx = cranelift::module::Module::make_context(&module);
    let mut builder_ctx = FunctionBuilderContext::new();

    let pointer_type = cranelift::module::Module::target_config(&module).pointer_type();
    let mut sig = cranelift::module::Module::make_signature(&module);
    sig.params.push(AbiParam::new(pointer_type));
    ctx.func.signature = sig;

    let func_id = cranelift::module::Module::declare_function(
        &mut module,
        "run",
        Linkage::Export,
        &ctx.func.signature,
    )
    .unwrap();

    {
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_ctx);
        let entry_block = builder.create_block();
        builder.append_block_param(entry_block, pointer_type);
        builder.switch_to_block(entry_block);

        let tape_ptr_val = builder.block_params(entry_block)[0];

        let pos_var = builder.declare_var(pointer_type);
        let zero = builder.ins().iconst(pointer_type, 0);
        builder.def_var(pos_var, zero);

        let mut putchar_sig = module.make_signature();
        putchar_sig.params.push(AbiParam::new(types::I8));
        let putchar_func = module
            .declare_function("bf_putchar", Linkage::Import, &putchar_sig)
            .unwrap();
        let local_putchar = module.declare_func_in_func(putchar_func, builder.func);

        let mut getchar_sig = module.make_signature();
        getchar_sig.returns.push(AbiParam::new(types::I8));
        let getchar_func = module
            .declare_function("bf_getchar", Linkage::Import, &getchar_sig)
            .unwrap();
        let local_getchar = module.declare_func_in_func(getchar_func, builder.func);

        // entry -> blocks[0]
        // pc = 0 -> blocks[1]
        // pc = N -> blocks[N+1]
        let mut blocks = vec![entry_block];
        for _ in 0..=ops.len() {
            blocks.push(builder.create_block());
        }

        builder.ins().jump(blocks[1], &[]);

        // `tape_base + ((pos + offset) & (TAPE_SIZE - 1))`
        let get_cell_addr = |builder: &mut FunctionBuilder, tape_base, pos_var, off: isize| {
            let pos_val = builder.use_var(pos_var);
            let off_val = builder.ins().iconst(pointer_type, off as i64);
            let raw_idx = builder.ins().iadd(pos_val, off_val);
            let masked_idx = builder.ins().band_imm(raw_idx, (TAPE_SIZE - 1) as i64); // Wrap within tape!
            builder.ins().iadd(tape_base, masked_idx)
        };

        for (pc, op) in ops.iter().enumerate() {
            builder.switch_to_block(blocks[pc + 1]);

            match op {
                Op::Add(off, n) => {
                    let addr = get_cell_addr(&mut builder, tape_ptr_val, pos_var, *off);
                    let val = builder.ins().load(types::I8, MemFlags::new(), addr, 0);
                    let new_val = builder.ins().iadd_imm(val, *n as i64);
                    builder.ins().store(MemFlags::new(), new_val, addr, 0);
                    builder.ins().jump(blocks[pc + 2], &[]);
                }
                Op::Sub(off, n) => {
                    let addr = get_cell_addr(&mut builder, tape_ptr_val, pos_var, *off);
                    let val = builder.ins().load(types::I8, MemFlags::new(), addr, 0);
                    let n_neg = (*n as i8).wrapping_neg() as i64;
                    let new_val = builder.ins().iadd_imm(val, n_neg);
                    builder.ins().store(MemFlags::new(), new_val, addr, 0);
                    builder.ins().jump(blocks[pc + 2], &[]);
                }
                Op::Clear(off) => {
                    let addr = get_cell_addr(&mut builder, tape_ptr_val, pos_var, *off);
                    let zero = builder.ins().iconst(types::I8, 0);
                    builder.ins().store(MemFlags::new(), zero, addr, 0);
                    builder.ins().jump(blocks[pc + 2], &[]);
                }
                Op::Output(off) => {
                    let addr = get_cell_addr(&mut builder, tape_ptr_val, pos_var, *off);
                    let val = builder.ins().load(types::I8, MemFlags::new(), addr, 0);
                    builder.ins().call(local_putchar, &[val]);
                    builder.ins().jump(blocks[pc + 2], &[]);
                }
                Op::Input(off) => {
                    let call_inst = builder.ins().call(local_getchar, &[]);
                    let val = builder.inst_results(call_inst)[0];
                    let addr = get_cell_addr(&mut builder, tape_ptr_val, pos_var, *off);
                    builder.ins().store(MemFlags::new(), val, addr, 0);
                    builder.ins().jump(blocks[pc + 2], &[]);
                }
                Op::Move(delta) => {
                    let pos_val = builder.use_var(pos_var);
                    let new_pos = builder.ins().iadd_imm(pos_val, *delta as i64);
                    builder.def_var(pos_var, new_pos);
                    builder.ins().jump(blocks[pc + 2], &[]);
                }
                Op::JumpForward(target) => {
                    let addr = get_cell_addr(&mut builder, tape_ptr_val, pos_var, 0);
                    let val = builder.ins().load(types::I8, MemFlags::new(), addr, 0);
                    // If true (val != 0), go next. If false (val == 0), jump to target
                    builder
                        .ins()
                        .brif(val, blocks[pc + 2], &[], blocks[*target + 1], &[]);
                }
                Op::JumpBack(target) => {
                    let addr = get_cell_addr(&mut builder, tape_ptr_val, pos_var, 0);
                    let val = builder.ins().load(types::I8, MemFlags::new(), addr, 0);
                    // If true (val != 0), jump back to target. If false, go next
                    builder
                        .ins()
                        .brif(val, blocks[*target + 1], &[], blocks[pc + 2], &[]);
                }
                Op::MulLoop { step, muls } => {
                    let addr = get_cell_addr(&mut builder, tape_ptr_val, pos_var, 0);
                    let val = builder.ins().load(types::I8, MemFlags::new(), addr, 0);
                    let skip_block = builder.create_block();
                    let do_block = builder.create_block();

                    blocks.push(skip_block);
                    blocks.push(do_block);

                    builder.ins().brif(val, do_block, &[], skip_block, &[]);

                    builder.switch_to_block(do_block);
                    let val_32 = builder.ins().uextend(types::I32, val);
                    let step_32 = builder.ins().iconst(types::I32, *step as i64);
                    let count = builder.ins().udiv(val_32, step_32);

                    for (off, delta) in muls {
                        let inner_addr = get_cell_addr(&mut builder, tape_ptr_val, pos_var, *off);
                        let orig = builder
                            .ins()
                            .load(types::I8, MemFlags::new(), inner_addr, 0);
                        let count_8 = builder.ins().ireduce(types::I8, count);
                        let delta_8 = builder.ins().iconst(types::I8, *delta as i64);
                        let change = builder.ins().imul(count_8, delta_8);
                        let new_val = builder.ins().iadd(orig, change);
                        builder.ins().store(MemFlags::new(), new_val, inner_addr, 0);
                    }

                    let zero = builder.ins().iconst(types::I8, 0);
                    builder.ins().store(MemFlags::new(), zero, addr, 0);
                    builder.ins().jump(skip_block, &[]);

                    builder.switch_to_block(skip_block);
                    builder.ins().jump(blocks[pc + 2], &[]);
                }
                Op::ScanRight | Op::ScanLeft => {
                    let is_right = matches!(op, Op::ScanRight);
                    let loop_hdr = builder.create_block();
                    let loop_body = builder.create_block();

                    blocks.push(loop_hdr);
                    blocks.push(loop_body);

                    builder.ins().jump(loop_hdr, &[]);

                    builder.switch_to_block(loop_hdr);
                    let addr = get_cell_addr(&mut builder, tape_ptr_val, pos_var, 0);
                    let val = builder.ins().load(types::I8, MemFlags::new(), addr, 0);
                    // If val != 0, continue loop. Else, break loop (jump next)
                    builder.ins().brif(val, loop_body, &[], blocks[pc + 2], &[]);

                    builder.switch_to_block(loop_body);
                    let pos_val = builder.use_var(pos_var);
                    let inc = if is_right { 1 } else { -1 };
                    let new_pos = builder.ins().iadd_imm(pos_val, inc);
                    builder.def_var(pos_var, new_pos);
                    builder.ins().jump(loop_hdr, &[]);
                }
            }
        }

        builder.switch_to_block(blocks[ops.len() + 1]);
        builder.ins().return_(&[]);

        for block in blocks {
            builder.seal_block(block);
        }
        builder.finalize();
    }

    module.define_function(func_id, &mut ctx).unwrap();
    module.clear_context(&mut ctx);
    module.finalize_definitions().unwrap();

    let code_ptr = module.get_finalized_function(func_id);

    let run_fn: extern "C" fn(*mut u8) = unsafe { mem::transmute(code_ptr) };

    let mut tape = vec![0u8; TAPE_SIZE];
    run_fn(tape.as_mut_ptr());
}

fn main() {
    let path = std::env::args().nth(1).expect("usage: fastfuck <file>");
    let source = {
        let file = File::open(&path).unwrap();
        let mut reader = BufReader::new(file);
        let mut src = Vec::new();
        reader.read_to_end(&mut src).unwrap();
        src
    };

    let ops = compile(&source);
    run_jit(&ops);
}
