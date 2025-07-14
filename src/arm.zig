const std = @import("std");

pub const Armv8a = struct {
    // Fixed instruction: RET - Return from function
    // Binary: 0xd65f03c0
    // Assembly: RET
    // Effect: PC = X30 (jump to return address in link register)
    pub const ret: u32 = 0xd65f03c0;

    // Fixed instruction: NOP - No operation
    // Binary: 0xd503201f
    // Assembly: NOP
    // Effect: No operation (do nothing)
    pub const nop: u32 = 0xd503201f;

    // General Purpose Registers - Only using first 13 (x0-x12) for this VM
    pub const Reg = enum(u32) { x0 = 0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12 };

    // Memory Load Operations

    /// LDR X[dst], [X[src]] - Load 64-bit value from memory
    /// Base opcode: 0xF9400000
    /// Effect: dst_reg = memory[src_reg]
    /// Example: ldrReg(.x0, .x1) → LDR X0, [X1] (load 8 bytes from address in X1 into X0)
    pub inline fn ldrReg(dst_reg: Reg, src_reg: Reg) u32 {
        const dst = @intFromEnum(dst_reg);
        const src = @intFromEnum(src_reg);
        // Encoding: [31-22: 1111001010] [21-10: offset] [9-5: src] [4-0: dst]
        return (0x3E5000 << 10) | (src << 5) | dst;
    }

    /// LDR X[dst], [X[base], X[offset], LSL #3] - Load with scaled register offset
    /// Base opcode: 0xF8607800
    /// Effect: dst_reg = memory[base_reg + (offset_reg * 8)]
    /// Use case: Loading from 64-bit arrays where offset_reg is the index
    pub inline fn ldrRegScaled(dst_reg: Reg, base_reg: Reg, offset_reg: Reg) u32 {
        const dst = @intFromEnum(dst_reg);
        const base = @intFromEnum(base_reg);
        const offset = @intFromEnum(offset_reg);
        // Encoding: [31-21: 11111000011] [20-16: offset] [15-13: 011] [12-10: 111] [9-5: base] [4-0: dst]
        return 0xF8607800 | (offset << 16) | (base << 5) | dst;
    }

    /// LDR X[dst], [X[base], X[offset]] - Load with unscaled register offset
    /// Base opcode: 0xF8606800
    /// Effect: dst_reg = memory[base_reg + offset_reg]
    /// Difference from scaled: No automatic multiplication by 8
    pub inline fn ldrRegUnscaled(dst_reg: Reg, base_reg: Reg, offset_reg: Reg) u32 {
        const dst = @intFromEnum(dst_reg);
        const base = @intFromEnum(base_reg);
        const off = @intFromEnum(offset_reg);
        // Encoding: [31-21: 11111000011] [20-16: offset] [15-13: 011] [12-10: 010] [9-5: base] [4-0: dst]
        return 0xF8606800 | (off << 16) | (base << 5) | dst;
    }

    /// LDRB W[dst], [X[base], X[offset]] - Load single byte with register offset
    /// Base opcode: 0x38606800
    /// Effect: dst_reg = (byte)memory[base_reg + offset_reg]
    /// Note: Loads only 1 byte, zeros upper 56 bits of destination
    pub inline fn ldrByteReg(dst_reg: Reg, base_reg: Reg, offset_reg: Reg) u32 {
        const dst = @intFromEnum(dst_reg);
        const base = @intFromEnum(base_reg);
        const offset = @intFromEnum(offset_reg);
        // Encoding: [31-21: 00111000011] [20-16: offset] [15-13: 011] [12-10: 010] [9-5: base] [4-0: dst]
        return 0x38606800 | (offset << 16) | (base << 5) | dst;
    }

    // Memory Store Operations

    /// STR X[src], [X[dst], #offset] - Store 64-bit value with immediate offset
    /// Base opcode: 0xF9000000
    /// Effect: memory[dst_reg + (offset * 8)] = src_reg
    /// Note: Offset is automatically scaled by 8 for 64-bit stores
    /// Limit: offset must be ≤ 4095 (12 bits)
    pub inline fn strReg(src_reg: Reg, dst_reg: Reg, offset: u32) u32 {
        const src = @intFromEnum(src_reg);
        const dst = @intFromEnum(dst_reg);
        std.debug.assert(offset <= 0b111111_111111); // 12-bit limit
        // Encoding: [31-22: 1111001000] [21-10: offset] [9-5: base] [4-0: source]
        return 0xF9000000 | (offset << 10) | (dst << 5) | src;
    }

    /// STR X[src], [X[base], X[offset], LSL #3] - Store with scaled register offset
    /// Base opcode: 0xF8207800
    /// Effect: memory[base_reg + (offset_reg * 8)] = src_reg
    /// Use case: Storing to 64-bit arrays where offset_reg is the index
    pub inline fn strRegScaled(src_reg: Reg, base_reg: Reg, offset_reg: Reg) u32 {
        const src = @intFromEnum(src_reg);
        const base = @intFromEnum(base_reg);
        const offset = @intFromEnum(offset_reg);
        // Encoding: [31-21: 11111000001] [20-16: offset] [15-13: 011] [12-10: 111] [9-5: base] [4-0: src]
        return 0xF8207800 | (offset << 16) | (base << 5) | src;
    }

    // Arithmetic Operations

    /// SUB X[dst], X[src], #imm - Subtract immediate value from register
    /// Base opcode: 0xD1000000
    /// Effect: dst_reg = src_reg - imm
    /// Limit: imm must be ≤ 4095 (12 bits)
    pub inline fn subRegImm(dst_reg: Reg, src_reg: Reg, imm: u32) u32 {
        std.debug.assert(imm <= 0b111111_111111); // 12-bit immediate limit
        const src = @intFromEnum(src_reg);
        const dst = @intFromEnum(dst_reg);
        // Encoding: [31-24: 11010001] [23-22: 00] [21-10: imm] [9-5: src] [4-0: dst]
        return 0xD1000000 | (imm << 10) | (src << 5) | dst;
    }

    /// ADD X[dst], X[src], #imm - Add immediate value to register
    /// Base opcode: 0x91000400
    /// Effect: dst_reg = src_reg + imm
    /// Limit: imm must be ≤ 4095 (12 bits)
    pub inline fn addRegImm(dst_reg: Reg, src_reg: Reg, imm: u32) u32 {
        const dst = @intFromEnum(dst_reg);
        const src = @intFromEnum(src_reg);
        std.debug.assert(imm <= 0b111111_111111); // 12-bit immediate limit
        // Encoding: [31-24: 10010001] [23-22: 00] [21-10: imm] [9-5: src] [4-0: dst]
        return 0x91000400 | (imm << 10) | (src << 5) | dst;
    }

    /// ADD X[dst], X[reg_a], X[reg_b] - Add two registers
    /// Base opcode: 0x8b000000
    /// Effect: dst_reg = reg_a + reg_b
    pub inline fn addRegs(dst_reg: Reg, reg_a: Reg, reg_b: Reg) u32 {
        const a = @intFromEnum(reg_a);
        const b = @intFromEnum(reg_b);
        const dst = @intFromEnum(dst_reg);
        // Encoding: [31-24: 10001011] [23-21: 000] [20-16: reg_b] [15-10: 000000] [9-5: reg_a] [4-0: dst]
        return 0x8b000000 | (b << 16) | (a << 5) | dst;
    }

    /// LSL X[dst], X[src], #3 - Logical Shift Left by 3 (multiply by 8)
    /// Base opcode: 0xD37DF000
    /// Effect: dst_reg = src_reg << 3 (equivalent to src_reg * 8)
    /// Use case: Converting array indices to byte offsets for 64-bit elements
    pub inline fn mult8(dst_reg: Reg, src_reg: Reg) u32 {
        const dst = @intFromEnum(dst_reg);
        const src = @intFromEnum(src_reg);
        // Encoding: [31-22: 1101001101] [21-16: 111101] [15-10: 111100] [9-5: src] [4-0: dst]
        return 0xD37DF000 | (src << 5) | dst;
    }

    // Data Movement

    /// MOV X[dst], #imm - Move immediate value to register
    /// Base opcode: 0xD2800000
    /// Effect: dst_reg = imm
    /// Actually encodes: MOVZ X[dst], #imm (Move with Zero - clears upper bits)
    pub inline fn movRegImm(dst_reg: Reg, imm: u32) u32 {
        const dst = @intFromEnum(dst_reg);
        // Encoding: [31-23: 110100101] [22-21: 00] [20-5: imm] [4-0: dst]
        return 0xD2800000 | (imm << 5) | dst;
    }

    // Comparison Operations

    /// CMP X[a], X[b] - Compare two registers
    /// Base opcode: 0xEB00001F
    /// Effect: Sets condition flags based on a_reg - b_reg
    /// Note: Result is discarded (stored to XZR), only condition flags matter
    pub inline fn cmpRegs(a_reg: Reg, b_reg: Reg) u32 {
        const a = @intFromEnum(a_reg);
        const b = @intFromEnum(b_reg);
        // Encoding: [31-24: 11101011] [23-21: 000] [20-16: b_reg] [15-10: 000000] [9-5: a_reg] [4-0: 11111 (XZR)]
        return 0xEB00001F | (b << 16) | (a << 5);
    }

    /// CMP X[reg], #value - Compare register with immediate value
    /// Base opcode: 0xF100001F
    /// Effect: Sets condition flags based on reg - value
    pub inline fn cmpImmediate(reg: Reg, value: u32) u32 {
        const r = @intFromEnum(reg);
        // Encoding: [31-24: 11110001] [23-22: 00] [21-10: value] [9-5: reg] [4-0: 11111 (XZR)]
        return 0xF100001F | (value << 10) | (r << 5);
    }

    /// CSET X[dst], EQ - Set register to 1 if equal, 0 otherwise
    /// Base opcode: 0x9A9F17E0
    /// Effect: dst_reg = (last_comparison_was_equal) ? 1 : 0
    /// Depends on: Previous CMP instruction's condition flags
    pub inline fn setIfStatusEq(dst_reg: Reg) u32 {
        const dst = @intFromEnum(dst_reg);
        // Encoding: [31-21: 10011010100] [20-16: 11111] [15-12: 0001] [11-5: 0111111] [4-0: dst]
        return 0x9A9F17E0 | dst;
    }

    // Control Flow

    /// B.EQ offset - Branch if equal (conditional branch)
    /// Base opcode: 0x54000000
    /// Effect: If last comparison was equal, jump to PC + (offset * 4)
    /// Range: ±1MB (19-bit signed offset × 4 bytes per instruction)
    pub inline fn branchIfEq(branch_offset: i32) u32 {
        const mask: i32 = std.math.maxInt(u19); // 19-bit mask for offset
        const offset: u32 = @bitCast(branch_offset & mask);
        // Encoding: [31-24: 01010100] [23-5: offset] [4-0: 00000 (EQ condition)]
        return 0x54000000 | (offset << 5);
    }
};
