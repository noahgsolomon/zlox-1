// const std = @import("std");
// const print = std.debug.print;
// const ZLox = @import("zlox.zig").ZLox;
//
// pub fn main() !void {
//     var gpa = std.heap.GeneralPurposeAllocator(.{ .thread_safe = true }){};
//     const alloc = gpa.allocator();
//
//     defer {
//         if (gpa.deinit() == .leak) {
//             std.log.err("Memory leak", .{});
//         }
//     }
//     const args = try std.process.argsAlloc(alloc);
//     defer std.process.argsFree(alloc, args);
//
//     if (args.len == 1) {
//         try ZLox.scanPrompt(alloc);
//     } else if (args.len == 2) {
//         try ZLox.scanFile(args[1], alloc);
//     } else {
//         std.log.err("Cannot input more than 1 arg. Use examples: zlox, zlox hello.zlox", .{});
//         return;
//     }
// }

const std = @import("std");
const print = std.debug.print;

var idx: usize = 0;

pub fn parseTerm(expr: []const u8) usize {
    var n1 = parseFactor(expr);

    while (idx < expr.len and (expr[idx] == '+' or expr[idx] == '-')) {
        const op = expr[idx];
        idx += 1;
        const n2 = parseFactor(expr);
        n1 = if (op == '+') n1 + n2 else n1 - n2;
    }

    return n1;
}

pub fn parseFactor(expr: []const u8) usize {
    var n1 = parseExponential(expr);

    while (idx < expr.len and (expr[idx] == '*' or expr[idx] == '/')) {
        const op = expr[idx];
        idx += 1;
        const n2 = parseExponential(expr);
        n1 = if (op == '*') n1 * n2 else n1 / n2;
    }

    return n1;
}

pub fn parseExponential(expr: []const u8) usize {
    var n1 = parsePrimary(expr);
    if (idx < expr.len and expr[idx] == '^') {
        idx += 1;
        const n2 = parsePrimary(expr);
        n1 = std.math.pow(usize, n1, n2);
    }
    return n1;
}

pub fn parsePrimary(expr: []const u8) usize {
    if (expr[idx] >= '0' and expr[idx] <= '9') {
        var end = idx;
        while (end < expr.len and expr[end] >= '0' and expr[end] <= '9') {
            end += 1;
        }
        const n1 = std.fmt.parseInt(usize, expr[idx..end], 10) catch unreachable;
        idx = end;
        return n1;
    } else if (expr[idx] == '(') {
        idx += 1;
        const n1 = parseTerm(expr);
        if (expr[idx] == ')') {
            idx += 1;
            return n1;
        }
    }

    return 0;
}

pub fn main() void {
    const expr: []const u8 = "(((100*((((20+4)*(2*2*9+3-3)))))/100)^2)^2";
    const n1 = parseTerm(expr);
    print("{d}\n", .{n1});
}
