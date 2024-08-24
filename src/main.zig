const std = @import("std");
const print = std.debug.print;
const ZLox = @import("zlox.zig").ZLox;

var hadError = false;

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
        try ZLox.runPrompt(alloc);
    } else if (args.len == 2) {
        try ZLox.runFile(args[1], alloc);
    } else {
        std.log.err("Cannot input more than 1 arg. Use examples: zlox, zlox hello.zlox", .{});
        return;
    }
}
