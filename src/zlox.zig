const std = @import("std");
const Scanner = @import("scanner.zig").Scanner;
const print = std.debug.print;
const Token = @import("token.zig").Token;
const Parser = @import("parser.zig").Parser;

pub const ZLox = struct {
    pub fn scanFile(path: []const u8, alloc: std.mem.Allocator) !void {
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

        var scanner = try Scanner.init(alloc);
        defer scanner.deinit();

        const tokens = scan(source, &scanner);
        var parser = Parser.init(tokens.items, alloc);
        defer parser.deinit();
        _ = parser.parse();
    }

    pub fn scanPrompt(
        alloc: std.mem.Allocator,
    ) !void {
        const in = std.io.getStdIn().reader();
        const out = std.io.getStdOut().writer();

        while (true) {
            var scanner = try Scanner.init(alloc);
            defer scanner.deinit();
            try out.print("> ", .{});
            const source = try in.readUntilDelimiterAlloc(alloc, '\n', std.math.maxInt(usize));

            if (source.len > 0 and !std.ascii.eqlIgnoreCase(source, "\n")) {
                const tokens = scan(source, &scanner);
                var parser = Parser.init(tokens.items, alloc);
                defer parser.deinit();
                _ = parser.parse();
                try out.print("\n", .{});
            }
        }
    }

    pub fn scan(source: []const u8, scanner: *Scanner) std.ArrayList(Token) {
        scanner.source = source;
        const tokens = scanner.scanTokens() catch unreachable;
        var line: usize = 1;
        for (tokens.items) |token| {
            while (line < token.line) {
                line += 1;
                print("\n", .{});
            }
            token.print();
        }
        return tokens;
    }

    pub fn scanError(line: usize, message: []const u8) void {
        report(line, "", message);
    }

    pub fn report(line: usize, where: []const u8, message: []const u8) void {
        std.log.err("[line {d}] Error {s}: {s}", .{ line, where, message });
    }
};
