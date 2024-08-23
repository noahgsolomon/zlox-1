const std = @import("std");
const print = std.debug.print;
const Scanner = @import("scanner.zig").Scanner;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{ .thread_safe = true }){};
    const alloc = gpa.allocator();

    defer {
        if (gpa.deinit() == .leak) {
            std.log.err("Memory leak", .{});
        }
    }
    const args = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args);

    if (args.len == 1) {
        try runPrompt(alloc);
    } else if (args.len == 2) {
        try runFile(args[1], alloc);
    } else {
        std.log.err("Cannot input more than 1 arg. Use examples: zlox, zlox hello.zlox", .{});
        return;
    }
}

pub fn runFile(path: []const u8, alloc: std.mem.Allocator) !void {
    const file = std.fs.cwd().openFile(path, .{}) catch |err| {
        std.log.err("Failed to open file: {s}\n{s}", .{ path, @errorName(err) });
        return;
    };
    defer file.close();

    const source = file.reader().readAllAlloc(alloc, std.math.maxInt(usize)) catch |err| {
        std.log.err("Failed to read file: {s}", .{@errorName(err)});
        return;
    };
    defer alloc.free(source);

    try run(source, alloc);
}

pub fn runPrompt(alloc: std.mem.Allocator) !void {
    const in = std.io.getStdIn().reader();
    const out = std.io.getStdOut().writer();
    while (true) {
        try out.print("> ", .{});
        const result = try in.readUntilDelimiterAlloc(alloc, '\n', std.math.maxInt(usize));

        if (result.len > 0 and !std.ascii.eqlIgnoreCase(result, "\n")) {
            try run(result, alloc);
            try out.print("\n", .{});
        }
    }
}

pub fn run(source: []const u8, alloc: std.mem.Allocator) !void {
    var scanner = try Scanner.init(source, alloc);
    defer scanner.deinit();
    const tokens = try scanner.scanTokens();
    for (tokens.items) |token| {
        print("{s} ", .{@tagName(token.type)});
    }
}
