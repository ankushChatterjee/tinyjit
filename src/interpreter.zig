const std = @import("std");
const JitCompiler = @import("jit.zig").JitCompiler;
const CompiledFunction = @import("jit.zig").CompiledFunction;
const pthread = @cImport(@cInclude("pthread.h"));

pub const OpCode = enum(u8) {
    push, // pushes a value onto the stack
    pushc, // pushes a constant onto the stack
    pop, // pops a value from the stack
    add, // pops two values from the stack and pushes the sum
    eq, // pops two values from the stack and pushes 1 if they are equal, 0 otherwise
    jmp, // jumps to a block
    jmp_nz, // pops a value from the stack and jumps to a block if it is not zero
    pushv, // pushes a variable's value onto the stack
    stvar, // pops a value from the stack and stores it in a variable
};

pub const Block = struct {
    bytecode: []const u8,
    constants: []const i64,
    variables: []i64,
    allocator: std.mem.Allocator,
    execution_count: u32,
    compiled_function: ?CompiledFunction,

    pub fn init(allocator: std.mem.Allocator, bytecode: []const u8, constants: []const i64, initial_variables: []i64) !Block {
        const variables = try allocator.alloc(i64, initial_variables.len);
        // Copy initial values to variables
        @memcpy(variables, initial_variables);

        return Block{
            .bytecode = bytecode,
            .constants = constants,
            .variables = variables,
            .allocator = allocator,
            .execution_count = 0,
            .compiled_function = null,
        };
    }

    pub fn deinit(self: *Block) void {
        self.allocator.free(self.variables);
        if (self.compiled_function) |*func| {
            func.deinit();
        }
    }
};

pub fn Op(op_code: OpCode) u8 {
    return @intFromEnum(op_code);
}

pub const Interpreter = struct {
    allocator: std.mem.Allocator,
    blocks: []Block,
    stack: []i64,
    pc: u32,
    sp: u32,
    current_block: u32,
    jit_compiler: JitCompiler,
    jit_enabled: bool,

    pub fn init(allocator: std.mem.Allocator, blocks: []Block, jit_enabled: bool) !Interpreter {
        var interpreter = Interpreter{
            .allocator = allocator,
            .blocks = blocks,
            .stack = try allocator.alloc(i64, 132000),
            .pc = 0,
            .sp = 0,
            .current_block = 0,
            .jit_compiler = undefined,
            .jit_enabled = jit_enabled,
        };

        interpreter.jit_compiler = try JitCompiler.init(allocator, &interpreter);
        return interpreter;
    }

    pub fn deinit(self: *Interpreter) void {
        // for (self.blocks) |*block| {
        //     block.deinit();
        // }
        self.jit_compiler.deinit();
        self.allocator.free(self.stack);
    }

    inline fn push(self: *Interpreter, value: i64) void {
        self.stack[self.sp] = value;
        self.sp += 1;
    }

    inline fn pop(self: *Interpreter) i64 {
        self.sp -= 1;
        return self.stack[self.sp];
    }

    pub inline fn top(self: *Interpreter) i64 {
        return self.stack[self.sp - 1];
    }

    inline fn nextOpCode(self: *Interpreter) OpCode {
        const op_code = @as(OpCode, @enumFromInt(self.blocks[self.current_block].bytecode[self.pc]));
        self.pc += 1;
        return op_code;
    }

    inline fn loadConstant(self: *Interpreter) i64 {
        const constant_index = self.blocks[self.current_block].bytecode[self.pc];
        self.pc += 1;
        if (constant_index >= self.blocks[self.current_block].constants.len) {
            return 0;
        }
        return self.blocks[self.current_block].constants[constant_index];
    }

    pub fn run(self: *Interpreter) !void {
        // Increment execution count for the initial block
        self.blocks[self.current_block].execution_count += 1;

        while (self.pc < self.blocks[self.current_block].bytecode.len) {
            // Check if we should JIT compile this block
            if (self.jit_enabled and
                self.blocks[self.current_block].compiled_function == null and
                self.blocks[self.current_block].execution_count >= 1000)
            {
                // Print professional JIT compilation notice

                // Time the JIT compilation process
                const compile_start = std.time.nanoTimestamp();
                const compiled_func = try self.jit_compiler.compile(self.current_block, &self.blocks[self.current_block]);
                const compile_end = std.time.nanoTimestamp();
                const compile_time = @as(u64, @intCast(compile_end - compile_start));

                self.blocks[self.current_block].compiled_function = compiled_func;

                std.debug.print("⚡ JIT Compilation completed in {d:.2}ms\n", .{@as(f64, @floatFromInt(compile_time)) / 1_000_000.0});
            }

            // If we have a compiled function, use it
            if (self.blocks[self.current_block].compiled_function) |compiled_func| {
                var pc_usize: usize = self.pc;
                var sp_usize: usize = self.sp;
                var current_block_usize: usize = self.current_block;

                compiled_func.function(
                    self.stack.ptr,
                    self.blocks[self.current_block].bytecode.ptr,
                    &pc_usize,
                    &sp_usize,
                    &current_block_usize,
                    self.blocks[self.current_block].constants.ptr,
                    self.blocks[self.current_block].variables.ptr,
                );

                self.pc = @intCast(pc_usize);
                self.sp = @intCast(sp_usize);
                self.current_block = @intCast(current_block_usize);

                // If we jumped to a different block, continue from there
                if (self.current_block >= self.blocks.len) return;
                continue;
            }

            // Fallback to interpretation
            const op_code = self.nextOpCode();
            switch (op_code) {
                .pushc => {
                    const value = self.loadConstant();
                    self.push(value);
                },
                .push => {
                    const value = self.blocks[self.current_block].bytecode[self.pc];
                    self.pc += 1;
                    self.push(value);
                },
                .pop => {
                    _ = self.pop();
                },
                .add => {
                    const a = self.pop();
                    const b = self.pop();
                    self.push(a + b);
                },
                .eq => {
                    const a = self.pop();
                    const b = self.pop();
                    self.push(if (a == b) 1 else 0);
                },
                .jmp => {
                    const block_index = self.blocks[self.current_block].bytecode[self.pc];
                    self.pc += 1;
                    if (block_index >= self.blocks.len) return;
                    self.current_block = @intCast(block_index);
                    self.pc = 0;
                    // Increment execution count for the target block
                    self.blocks[self.current_block].execution_count += 1;
                    continue;
                },
                .jmp_nz => {
                    const offset = self.blocks[self.current_block].bytecode[self.pc];
                    self.pc += 1;
                    if (self.top() != 0) {
                        if (offset >= self.blocks.len) return;
                        self.current_block = @intCast(offset);
                        self.pc = 0;
                        // Increment execution count for the target block
                        self.blocks[self.current_block].execution_count += 1;
                        continue;
                    }
                },
                .pushv => {
                    const variable_index = self.blocks[self.current_block].bytecode[self.pc];
                    self.pc += 1;
                    self.push(self.blocks[self.current_block].variables[@intCast(variable_index)]);
                },
                .stvar => {
                    const variable_index = self.blocks[self.current_block].bytecode[self.pc];
                    self.pc += 1;
                    const value = self.pop();
                    self.blocks[self.current_block].variables[@intCast(variable_index)] = value;
                },
            }
        }
    }
};

test "interpreter" {
    var initial_variables = [_]i64{0};
    var blocks = [_]Block{
        try Block.init(std.testing.allocator, &[_]u8{ Op(.pushc), 0 }, &[_]i64{1}, &initial_variables),
    };
    defer for (&blocks) |*block| {
        block.deinit();
    };

    var interpreter = try Interpreter.init(std.testing.allocator, &blocks, true);
    defer interpreter.deinit();
    try interpreter.run();
    try std.testing.expect(interpreter.top() == 1);
}

test "interpreter loop JIT enabled" {
    var initial_variables = [_]i64{ 0, 0 };
    var blocks = [_]Block{
        try Block.init(std.testing.allocator, &[_]u8{
            Op(.pushv), 0, // loop start: load i (pops index from stack)
            Op(.pushc), 0, // load 1000
            Op(.eq), // if i == 1000
            Op(.jmp_nz), 1, // exit if equal - jump to exit block
            Op(.push),   1,
            Op(.pushv), 1, // x
            Op(.add), // x + 1
            Op(.stvar), 1, // x = x + 1
            Op(.pushv), 0, // i
            Op(.push), 1, // 1
            Op(.add), // i + 1
            Op(.stvar), 0, // i = i + 1
            Op(.jmp), 0, // loop back to same block
        }, &[_]i64{10000}, &initial_variables),
    };

    defer for (&blocks) |*block| {
        block.deinit();
    };

    var interpreter = try Interpreter.init(std.testing.allocator, &blocks, true);
    defer interpreter.deinit();
    try interpreter.run();
    try std.testing.expect(interpreter.blocks[0].variables[1] == 10000);
}

test "interpreter loop JIT disabled" {
    var initial_variables = [_]i64{ 0, 0 };
    var blocks = [_]Block{
        try Block.init(std.testing.allocator, &[_]u8{
            Op(.pushv), 0, // loop start: load i (pops index from stack)
            Op(.pushc), 0, // load 1000
            Op(.eq), // if i == 1000
            Op(.jmp_nz), 1, // exit if equal - jump to exit block
            Op(.push),   1,
            Op(.pushv), 1, // x
            Op(.add), // x + 1
            Op(.stvar), 1, // x = x + 1
            Op(.pushv), 0, // i
            Op(.push), 1, // 1
            Op(.add), // i + 1
            Op(.stvar), 0, // i = i + 1
            Op(.jmp), 0, // loop back to same block
        }, &[_]i64{10000}, &initial_variables),
    };

    defer for (&blocks) |*block| {
        block.deinit();
    };

    var interpreter = try Interpreter.init(std.testing.allocator, &blocks, false);
    defer interpreter.deinit();
    try interpreter.run();
    try std.testing.expect(interpreter.blocks[0].variables[1] == 10000);
}

test "JIT performance comparison" {
    // JIT enabled measurement
    var initial_variables_jit = [_]i64{ 0, 0 };
    var blocks_jit = [_]Block{
        try Block.init(std.testing.allocator, &[_]u8{
            Op(.pushv), 0, // loop start: load i (pops index from stack)
            Op(.pushc), 0, // load 1000
            Op(.eq), // if i == 1000
            Op(.jmp_nz), 1, // exit if equal - jump to exit block
            Op(.push),   1,
            Op(.pushv), 1, // x
            Op(.add), // x + 1
            Op(.stvar), 1, // x = x + 1
            Op(.pushv), 0, // i
            Op(.push), 1, // 1
            Op(.add), // i + 1
            Op(.stvar), 0, // i = i + 1
            Op(.jmp), 0, // loop back to same block
        }, &[_]i64{100000}, &initial_variables_jit),
    };

    defer for (&blocks_jit) |*block| {
        block.deinit();
    };

    var interpreter_jit = try Interpreter.init(std.testing.allocator, &blocks_jit, true);
    defer interpreter_jit.deinit();
    // measure
    const start_jit = std.time.nanoTimestamp();
    try interpreter_jit.run();
    const end_jit = std.time.nanoTimestamp();
    const time_jit = @as(u64, @intCast(end_jit - start_jit));

    // JIT disabled measurement
    var initial_variables_no_jit = [_]i64{ 0, 0 };
    var blocks_no_jit = [_]Block{
        try Block.init(std.testing.allocator, &[_]u8{
            Op(.pushv), 0, // loop start: load i (pops index from stack)
            Op(.pushc), 0, // load 1000
            Op(.eq), // if i == 1000
            Op(.jmp_nz), 1, // exit if equal - jump to exit block
            Op(.push),   1,
            Op(.pushv), 1, // x
            Op(.add), // x + 1
            Op(.stvar), 1, // x = x + 1
            Op(.pushv), 0, // i
            Op(.push), 1, // 1
            Op(.add), // i + 1
            Op(.stvar), 0, // i = i + 1
            Op(.jmp), 0, // loop back to same block
        }, &[_]i64{100000}, &initial_variables_no_jit),
    };

    defer for (&blocks_no_jit) |*block| {
        block.deinit();
    };

    var interpreter_no_jit = try Interpreter.init(std.testing.allocator, &blocks_no_jit, false);
    defer interpreter_no_jit.deinit();
    const start_no_jit = std.time.nanoTimestamp();
    try interpreter_no_jit.run();
    const end_no_jit = std.time.nanoTimestamp();
    const time_no_jit = @as(u64, @intCast(end_no_jit - start_no_jit));

    // Professional performance report
    const jit_time_ns = @as(f64, @floatFromInt(time_jit));
    const no_jit_time_ns = @as(f64, @floatFromInt(time_no_jit));
    const speedup = no_jit_time_ns / jit_time_ns;

    std.debug.print("\n" ++ "─" ** 50 ++ "\n", .{});
    std.debug.print("  PERFORMANCE BENCHMARK RESULTS\n", .{});
    std.debug.print("─" ** 50 ++ "\n", .{});
    std.debug.print("  Interpreter Mode    │ Execution Time\n", .{});
    std.debug.print("  ─────────────────── │ ──────────────\n", .{});
    std.debug.print("  JIT Enabled         │ {d:.2} ns\n", .{jit_time_ns});
    std.debug.print("  JIT Disabled        │ {d:.2} ns\n", .{no_jit_time_ns});
    std.debug.print("  ─────────────────── │ ──────────────\n", .{});
    if (speedup > 1.0) {
        std.debug.print("  Performance Gain    │ \x1b[32m{d:.2}x\x1b[0m\n", .{speedup});
    } else {
        std.debug.print("  Performance Gain    │ {d:.2}x\n", .{speedup});
    }
    std.debug.print("─" ** 50 ++ "\n", .{});
}
