const std = @import("std");
const Scanner = @import("scanner.zig").Scanner;
const print = std.debug.print;
const Token = @import("token.zig").Token;
const Parser = @import("parser.zig").Parser;
const Interpreter = @import("interpreter.zig").Interpreter;
const Environment = @import("interpreter.zig").Environment;
const Stmt = @import("interpreter.zig").Stmt;

pub const ZLox = struct {
    debug: bool,

    pub fn init(debug: bool) ZLox {
        return ZLox{ .debug = debug };
    }

    pub fn scanFile(self: *ZLox, path: []const u8, alloc: std.mem.Allocator) !void {
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
        var parser = Parser.init(tokens.items, alloc, self.debug);
        defer parser.deinit();
        // _ = parser.parse();
    }

    pub fn scanPrompt(self: *ZLox, alloc: std.mem.Allocator) !void {
        const in = std.io.getStdIn().reader();
        const out = std.io.getStdOut().writer();
        var interpreter = Interpreter.init(alloc);
        defer interpreter.deinit();

        while (true) {
            var scanner = try Scanner.init(alloc);
            defer scanner.deinit();
            try out.print("> ", .{});
            const source = try in.readUntilDelimiterAlloc(alloc, '\n', std.math.maxInt(usize));
            defer alloc.free(source);

            if (source.len > 0 and !std.ascii.eqlIgnoreCase(source, "\n")) {
                const tokens = scan(source, &scanner);
                var parser = Parser.init(tokens.items, alloc, self.debug);
                defer parser.deinit();

                const stmt = parser.parse() catch |err| {
                    try out.print("Error: {}\n", .{err});
                    continue;
                };

                switch (stmt) {
                    .print => |print_stmt| {
                        const value = try interpreter.evaluate(print_stmt.*);
                        try out.print("{}\n", .{value});
                    },
                    .var_decl => |var_decl| {
                        try interpreter.executeVarDecl(var_decl);
                        try out.print("Variable declared: {s}\n", .{var_decl.name.lexeme});
                    },
                    .expression => |expr_stmt| {
                        const value = try interpreter.evaluate(expr_stmt.*);
                        try out.print("= {}\n", .{value});
                    },
                }
            }
        }
    }

    pub fn scan(source: []const u8, scanner: *Scanner) std.ArrayList(Token) {
        scanner.source = source;
        const tokens = scanner.scanTokens() catch unreachable;
        var line: usize = 1;
        print("-----TOKENS------\n", .{});
        for (tokens.items) |token| {
            while (line < token.line) {
                line += 1;
                print("\n", .{});
            }
            token.print();
        }
        print("\n-----------------\n", .{});
        return tokens;
    }

    pub fn scanError(line: usize, message: []const u8) void {
        report(line, "", message);
    }

    pub fn report(line: usize, where: []const u8, message: []const u8) void {
        std.log.err("[line {d}] Error {s}: {s}", .{ line, where, message });
    }
};
