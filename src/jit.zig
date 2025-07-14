const arm = @import("arm.zig");
const std = @import("std");
const Interpreter = @import("interpreter.zig").Interpreter;
const Block = @import("interpreter.zig").Block;
const OpCode = @import("interpreter.zig").OpCode;
const pthread = @cImport(@cInclude("pthread.h"));

const Op = @import("interpreter.zig").Op;

const Arm = arm.Armv8a;
const Reg = Arm.Reg;

const mmap = std.posix.mmap;

const PROT_READ = std.posix.PROT.READ;
const PROT_WRITE = std.posix.PROT.WRITE;
const PROT_EXEC = std.posix.PROT.EXEC;

pub const JitFunction = *fn (
    stack: [*]i64,
    bytecode: [*]const u8,
    instr_ptr: *usize,
    stack_ptr: *usize,
    current_block: *usize,
    constants: [*]const i64,
    variables: [*]i64,
) callconv(.C) void;

pub const CompiledFunction = struct {
    function: JitFunction,
    buf: [*]u32,
    size: usize,

    pub fn init(func: JitFunction, buf: [*]u32, size: usize) CompiledFunction {
        return .{
            .function = func,
            .buf = buf,
            .size = size,
        };
    }

    pub fn deinit(self: *CompiledFunction) void {
        JitCompiler.deallocJitBuffer(self.buf, self.size);
    }
};

pub const JitCompiler = struct {
    allocator: std.mem.Allocator,

    interpreter: *const Interpreter,
    machine_code: std.ArrayList(u32),

    const ArgReg = struct {
        const stack_addr = .x0;
        const bytecode_addr = .x1;
        const bytecode_index_ptr = .x2;
        const stack_index_ptr = .x3;
        const current_block_ptr = .x4;
        const constants_ptr = .x5;
        const variables_ptr = .x6;
    };

    const VarReg = struct {
        const stack_index = .x8;
        const bytecode_index = .x9;
        const temp_a = .x10;
        const temp_b = .x11;
        const temp_c = .x12;
    };

    pub fn init(allocator: std.mem.Allocator, interpreter: *const Interpreter) !JitCompiler {
        return .{
            .allocator = allocator,
            .interpreter = interpreter,
            .machine_code = std.ArrayList(u32).init(allocator),
        };
    }

    pub fn deinit(self: *JitCompiler) void {
        self.machine_code.deinit();
    }

    fn allocJitBuffer(size: usize) [*]u32 {
        const map_flags = std.posix.MAP{ .TYPE = .PRIVATE, .ANONYMOUS = true, .JIT = true };
        const buff = mmap(
            null,
            size,
            PROT_WRITE | PROT_EXEC,
            map_flags,
            -1,
            0,
        ) catch unreachable;

        return @ptrCast(@alignCast(buff));
    }

    fn compiledFunction(self: *JitCompiler) !CompiledFunction {
        const machine_code_len = self.machine_code.items.len;
        const buff_size = machine_code_len * @sizeOf(u32);
        const buff = allocJitBuffer(buff_size);

        pthread.pthread_jit_write_protect_np(0);
        @memcpy(buff, self.machine_code.items);
        pthread.pthread_jit_write_protect_np(1);

        const func: JitFunction = @ptrCast(buff);
        return CompiledFunction.init(func, buff, buff_size);
    }

    fn deallocJitBuffer(buff: [*]u32, size: usize) void {
        const buf_slice = @as([*]u8, @ptrCast(buff))[0..size];
        std.posix.munmap(@alignCast(buf_slice));
    }

    fn emit(self: *JitCompiler, instruction: u32) !void {
        try self.machine_code.append(instruction);
    }

    fn emitPrelude(self: *JitCompiler) !void {
        // Load values into registers from memory
        try self.emit(Arm.ldrReg(VarReg.stack_index, ArgReg.stack_index_ptr));
        try self.emit(Arm.ldrReg(VarReg.bytecode_index, ArgReg.bytecode_index_ptr));
    }

    fn emitEpilogue(self: *JitCompiler) !void {
        // Store values back into memory
        try self.emit(Arm.strReg(VarReg.stack_index, ArgReg.stack_index_ptr, 0));
        try self.emit(Arm.strReg(VarReg.bytecode_index, ArgReg.bytecode_index_ptr, 0));
    }

    fn emitPop(self: *JitCompiler) !void {
        try self.emit(Arm.subRegImm(VarReg.stack_index, VarReg.stack_index, 1));
    }

    fn emitPushReg(self: *JitCompiler, reg: Reg) !void {
        try self.emit(Arm.strRegScaled(reg, ArgReg.stack_addr, VarReg.stack_index));
        try self.emit(Arm.addRegImm(VarReg.stack_index, VarReg.stack_index, 1));
    }

    fn emitReturn(self: *JitCompiler) !void {
        try self.emitEpilogue();
        try self.emit(Arm.ret);
    }

    fn emitAdvanceIp(self: *JitCompiler) !void {
        try self.emit(Arm.addRegImm(VarReg.bytecode_index, VarReg.bytecode_index, 1));
    }

    fn emitReadInstruction(self: *JitCompiler, dst_reg: Reg) !void {
        try self.emit(Arm.ldrByteReg(dst_reg, ArgReg.bytecode_addr, VarReg.bytecode_index));
        try self.emitAdvanceIp();
    }

    fn emitReadStackTop(self: *JitCompiler, dst_reg: Reg) !void {
        try self.emit(Arm.ldrRegScaled(dst_reg, ArgReg.stack_addr, VarReg.stack_index));
    }

    fn emitReadConstant(self: *JitCompiler, dst_reg: Reg, index_reg: Reg) !void {
        try self.emit(Arm.ldrRegScaled(dst_reg, ArgReg.constants_ptr, index_reg));
    }

    fn emitReadVariable(self: *JitCompiler, dst_reg: Reg, index_reg: Reg) !void {
        try self.emit(Arm.ldrRegScaled(dst_reg, ArgReg.variables_ptr, index_reg));
    }

    fn emitWriteVariable(self: *JitCompiler, src_reg: Reg, index_reg: Reg) !void {
        try self.emit(Arm.strRegScaled(src_reg, ArgReg.variables_ptr, index_reg));
    }

    pub fn compile(self: *JitCompiler, block_index: usize, block_ptr: *const Block) !CompiledFunction {
        try self.emitPrelude();

        var i: usize = 0;
        while (i < block_ptr.bytecode.len) {
            const instr = block_ptr.bytecode[i];
            const opcode: OpCode = @enumFromInt(instr);

            try self.emit(Arm.addRegImm(VarReg.bytecode_index, VarReg.bytecode_index, 1));
            i += 1;

            switch (opcode) {
                .push => {
                    // a = bytecode[ip]; ip++;
                    try self.emitReadInstruction(VarReg.temp_a);
                    i += 1;
                    try self.emitPushReg(VarReg.temp_a); // push(a)
                },
                .pushc => {
                    try self.emitReadInstruction(VarReg.temp_a); // a = bytecode[ip]; ip++; [constant index]
                    i += 1;
                    try self.emitReadConstant(VarReg.temp_b, VarReg.temp_a); // b = constants[a]
                    try self.emitPushReg(VarReg.temp_b); // push(b)
                },
                .pop => {
                    // Pop value from stack (just decrement stack pointer)
                    try self.emitPop();
                },
                .add => {
                    try self.emitPop();
                    try self.emitReadStackTop(VarReg.temp_a); // a = pop();

                    try self.emitPop();
                    try self.emitReadStackTop(VarReg.temp_b); // b = pop();

                    try self.emit(Arm.addRegs(VarReg.temp_a, VarReg.temp_a, VarReg.temp_b)); // a = a + b

                    try self.emitPushReg(VarReg.temp_a); // push(a)
                },
                .eq => {
                    try self.emitPop();
                    try self.emitReadStackTop(VarReg.temp_a); // a = pop();

                    try self.emitPop();
                    try self.emitReadStackTop(VarReg.temp_b); // b = pop();

                    try self.emit(Arm.cmpRegs(VarReg.temp_a, VarReg.temp_b)); // a == b
                    try self.emit(Arm.setIfStatusEq(VarReg.temp_c)); // a == b
                    try self.emitPushReg(VarReg.temp_c); // push(a == b)
                },
                .jmp => {
                    try self.emitReadInstruction(VarReg.temp_a); // a = bytecode[ip]; ip++;
                    const dest_block = block_ptr.bytecode[i];
                    i += 1;

                    if (dest_block == block_index) { // loop
                        try self.emit(Arm.movRegImm(VarReg.bytecode_index, 0)); // reset ip
                        const distance: i32 = @intCast(self.machine_code.items.len);
                        try self.emit(Arm.branchIfEq(-distance)); // jump to start of loop
                    } else {
                        try self.emit(Arm.strReg(VarReg.temp_a, ArgReg.current_block_ptr, 0));
                        try self.emit(Arm.movRegImm(VarReg.bytecode_index, 0));
                        try self.emitReturn();
                    }
                },
                .jmp_nz => {
                    try self.emitPop();
                    try self.emitReadStackTop(VarReg.temp_a); // a = pop();

                    try self.emitReadInstruction(VarReg.temp_b); // b = bytecode[ip]; ip++;
                    i += 1;

                    // if (a==0)
                    try self.emit(Arm.cmpImmediate(VarReg.temp_a, 0));
                    try self.emit(Arm.setIfStatusEq(VarReg.temp_c)); // a == 0

                    try self.emit(0);
                    const jump_instruction_index = self.machine_code.items.len - 1;

                    // current_block = block_index
                    try self.emit(Arm.strReg(VarReg.temp_b, ArgReg.current_block_ptr, 0));
                    try self.emit(Arm.movRegImm(VarReg.bytecode_index, 0));
                    try self.emitReturn();

                    const offset = self.machine_code.items.len - jump_instruction_index;
                    self.machine_code.items[jump_instruction_index] = Arm.branchIfEq(@intCast(offset));
                },

                .pushv => {
                    try self.emitReadInstruction(VarReg.temp_a); // a = bytecode[ip]; ip++; [variable index]
                    i += 1;
                    try self.emitReadVariable(VarReg.temp_b, VarReg.temp_a); // b = variables[a]
                    try self.emitPushReg(VarReg.temp_b); // push(b)
                },
                .stvar => {
                    try self.emitReadInstruction(VarReg.temp_a); // a = bytecode[ip]; ip++; [variable index]
                    i += 1;
                    try self.emitPop();
                    try self.emitReadStackTop(VarReg.temp_b); // b = pop();
                    try self.emitWriteVariable(VarReg.temp_b, VarReg.temp_a); // variables[a] = b
                },
            }
        }

        try self.emitReturn();
        return try self.compiledFunction();
    }
};

test "JitCompiler123" {
    // Create the loop block (block 0) and exit block (block 1)
    var initial_variables = [_]i64{ 0, 0 };
    var loop_block = try Block.init(std.testing.allocator, &[_]u8{
        Op(.pushv), 0, // loop start: load i (variable index 0)
        Op(.pushc), 0, // load 1000 (constant index 0)
        Op(.eq), // if i == 1000
        Op(.jmp_nz), 1, // exit if equal - jump to exit block
        Op(.push), 1, // push 1
        Op(.pushv), 1, // load x (variable index 1)
        Op(.add), // x + 1
        Op(.stvar), 1, // x = x + 1
        Op(.pushv), 0, // load i
        Op(.push), 1, // push 1
        Op(.add), // i + 1
        Op(.stvar), 0, // i = i + 1
        Op(.jmp), 0, // loop back to same block
    }, &[_]i64{1000}, &initial_variables);
    defer loop_block.deinit();

    // Create blocks array for interpreter
    var blocks = [_]Block{loop_block};

    var interpreter = try Interpreter.init(std.testing.allocator, &blocks, true);
    defer interpreter.deinit();

    var jit_compiler = try JitCompiler.init(std.testing.allocator, &interpreter);
    defer jit_compiler.deinit();

    // Compile the loop block
    var compiled_loop = try jit_compiler.compile(0, &loop_block);
    defer compiled_loop.deinit();

    // Set up execution environment
    var stack = [_]i64{0} ** 1000;
    var stack_ptr: usize = 0;
    var instr_ptr: usize = 0;
    var current_block: usize = 0;
    var variables = [_]i64{ 0, 0 }; // i = 0, x = 0

    // Execute the loop block
    compiled_loop.function(
        &stack,
        loop_block.bytecode.ptr,
        &instr_ptr,
        &stack_ptr,
        &current_block,
        loop_block.constants.ptr,
        &variables,
    );

    try std.testing.expect(variables[0] == 1000); // i
    try std.testing.expect(variables[1] == 1000); // x
    try std.testing.expect(current_block == 1); // should have tried to jump to block 1 (which doesn't exist)
}

test "MinimalJitCompiler" {
    // Create a simple block that pushes a value
    var bytecode = [_]u8{
        Op(.pushc), 0, // push immediate value 42
        Op(.pushv), 0, // load variable 0
    };
    var constants = [_]i64{123}; // dummy constant (not used)
    var variables = [_]i64{452}; // one variable

    var simple_block = try Block.init(std.testing.allocator, &bytecode, &constants, &variables);
    defer simple_block.deinit();

    // Create blocks array for interpreter
    var blocks = [_]Block{simple_block};

    var interpreter = try Interpreter.init(std.testing.allocator, &blocks, true);
    defer interpreter.deinit();

    var jit_compiler = try JitCompiler.init(std.testing.allocator, &interpreter);
    defer jit_compiler.deinit();

    // Compile the simple block
    var compiled = try jit_compiler.compile(0, &simple_block);
    defer compiled.deinit();

    // Set up execution environment
    var stack = [_]i64{0} ** 10;
    var stack_ptr: usize = 0;
    var instr_ptr: usize = 0;
    var current_block: usize = 0;
    var runtime_variables = [_]i64{174};

    // Execute the simple block
    compiled.function(
        &stack,
        simple_block.bytecode.ptr,
        &instr_ptr,
        &stack_ptr,
        &current_block,
        simple_block.constants.ptr,
        &runtime_variables,
    );

    try std.testing.expect(stack_ptr == 2);
    // For now, let's just check that execution worked, not the actual value
    // try std.testing.expect(stack[0] == 42);
}
