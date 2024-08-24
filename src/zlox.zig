const std = @import("std");
const Scanner = @import("scanner.zig").Scanner;
const print = std.debug.print;

pub const ZLox = struct {
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
            switch (token.literal) {
                .void => print("{s} ", .{@tagName(token.type)}),
                .int => |value| print("({s}: {d}) ", .{ @tagName(token.type), value }),
                .float => |value| print("({s}: {d:.2}) ", .{ @tagName(token.type), value }),
                .str => |value| print("({s}: {s}) ", .{ @tagName(token.type), value }),
            }
        }
    }

    pub fn scanError(line: usize, message: []const u8) void {
        report(line, "", message);
    }

    pub fn report(line: usize, where: []const u8, message: []const u8) void {
        std.log.err("[line {d}] Error {s}: {s}", .{ line, where, message });
    }
};
