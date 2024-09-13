const std = @import("std");
const print = std.debug.print;
const ZLox = @import("zlox.zig").ZLox;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var debug = false;
    if (args.len > 1 and std.mem.eql(u8, args[1], "--DEBUG")) {
        debug = true;
        args = args[1..];
    }

    var zlox = ZLox.init(debug);

    if (args.len > 1) {
        try zlox.scanFile(args[1], allocator);
    } else {
        try zlox.scanPrompt(allocator);
    }
}
