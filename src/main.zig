const std = @import("std");
const interpreter = @import("interpreter.zig");
const Block = interpreter.Block;
const Interpreter = interpreter.Interpreter;
const Op = interpreter.Op;

const BenchmarkResult = struct {
    array_size: i64,
    jit_geomean: f64,
    no_jit_geomean: f64,
    speedup: f64,
};

fn runBenchmarkForArraySize(allocator: std.mem.Allocator, array_size: i64, num_runs: usize) !BenchmarkResult {
    var jit_times = try allocator.alloc(f64, num_runs);
    defer allocator.free(jit_times);
    var no_jit_times = try allocator.alloc(f64, num_runs);
    defer allocator.free(no_jit_times);

    // JIT enabled benchmark - run multiple times
    var run: usize = 0;
    while (run < num_runs) : (run += 1) {
        var gpa = std.heap.GeneralPurposeAllocator(.{}){};
        defer _ = gpa.deinit();
        const bench_allocator = gpa.allocator();

        var initial_variables_jit = [_]i64{ 0, 0 };
        var blocks_jit = [_]Block{
            try Block.init(bench_allocator, &[_]u8{
                Op(.pushv), 0, // loop start: load i
                Op(.pushc), 0, // load array_size
                Op(.eq), // if i == array_size
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
            }, &[_]i64{array_size}, &initial_variables_jit),
        };

        defer for (&blocks_jit) |*block| {
            block.deinit();
        };

        var interpreter_jit = try Interpreter.init(bench_allocator, &blocks_jit, true);
        defer interpreter_jit.deinit();
        const start_jit = std.time.nanoTimestamp();
        try interpreter_jit.run();
        const end_jit = std.time.nanoTimestamp();
        jit_times[run] = @as(f64, @floatFromInt(@as(u64, @intCast(end_jit - start_jit))));
    }

    // JIT disabled benchmark - run multiple times
    run = 0;
    while (run < num_runs) : (run += 1) {
        var gpa = std.heap.GeneralPurposeAllocator(.{}){};
        defer _ = gpa.deinit();
        const bench_allocator = gpa.allocator();

        var initial_variables_no_jit = [_]i64{ 0, 0 };
        var blocks_no_jit = [_]Block{
            try Block.init(bench_allocator, &[_]u8{
                Op(.pushv), 0, // loop start: load i
                Op(.pushc), 0, // load array_size
                Op(.eq), // if i == array_size
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
            }, &[_]i64{array_size}, &initial_variables_no_jit),
        };

        defer for (&blocks_no_jit) |*block| {
            block.deinit();
        };

        var interpreter_no_jit = try Interpreter.init(bench_allocator, &blocks_no_jit, false);
        defer interpreter_no_jit.deinit();
        const start_no_jit = std.time.nanoTimestamp();
        try interpreter_no_jit.run();
        const end_no_jit = std.time.nanoTimestamp();
        no_jit_times[run] = @as(f64, @floatFromInt(@as(u64, @intCast(end_no_jit - start_no_jit))));
    }

    // Calculate geometric means
    var log_sum_jit: f64 = 0.0;
    var log_sum_no_jit: f64 = 0.0;

    for (jit_times) |time| {
        log_sum_jit += @log(time);
    }

    for (no_jit_times) |time| {
        log_sum_no_jit += @log(time);
    }

    const geomean_jit = @exp(log_sum_jit / @as(f64, @floatFromInt(num_runs)));
    const geomean_no_jit = @exp(log_sum_no_jit / @as(f64, @floatFromInt(num_runs)));
    const speedup = geomean_no_jit / geomean_jit;

    return BenchmarkResult{
        .array_size = array_size,
        .jit_geomean = geomean_jit,
        .no_jit_geomean = geomean_no_jit,
        .speedup = speedup,
    };
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const num_runs = 10;
    const array_sizes = [_]i64{ 10, 100, 1000, 2000, 5000, 10000, 50000, 100000 };

    std.debug.print("Running benchmarks for different array sizes ({} iterations each)...\n\n", .{num_runs});

    var results = try allocator.alloc(BenchmarkResult, array_sizes.len);
    defer allocator.free(results);

    // Run benchmarks for each array size
    for (array_sizes, 0..) |array_size, i| {
        std.debug.print("Running benchmark for array size {}...\n", .{array_size});
        results[i] = try runBenchmarkForArraySize(allocator, array_size, num_runs);
    }

    // Display comprehensive results
    std.debug.print("\n" ++ "─" ** 80 ++ "\n", .{});
    std.debug.print(" ✔ PERFORMANCE BENCHMARK RESULTS ({} runs each)\n", .{num_runs});
    std.debug.print("─" ** 80 ++ "\n", .{});
    std.debug.print("  Array Size │ JIT Enabled (ns) │ JIT Disabled (ns) │ Speedup\n", .{});
    std.debug.print("  ────────── │ ──────────────── │ ───────────────── │ ───────\n", .{});

    for (results) |result| {
        const color_start = if (result.speedup > 1.0) "\x1b[32m" else "\x1b[31m";
        const color_end = "\x1b[0m";
        std.debug.print("  {d:>9}  │ {d:>14.2}   │ {d:>15.2}   │ {s}{d:>6.2}x{s}\n", .{
            result.array_size,
            result.jit_geomean,
            result.no_jit_geomean,
            color_start,
            result.speedup,
            color_end,
        });
    }

    std.debug.print("─" ** 80 ++ "\n", .{});
}
