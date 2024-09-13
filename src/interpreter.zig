const std = @import("std");
const Expr = @import("parser.zig").Expr;
const Binary = @import("parser.zig").Binary;
const Unary = @import("parser.zig").Unary;
const Literal = @import("token.zig").Literal;
const Token = @import("token.zig").Token;

pub const Value = union(enum) {
    int: i64,
    float: f64,
    boolean: bool,
    str: []const u8,
    nil: void,

    pub fn format(
        self: Value,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .int => |n| try writer.print("{d}", .{n}),
            .float => |n| try writer.print("{d}", .{n}),
            .boolean => |b| try writer.print("{}", .{b}),
            .str => |s| try writer.print("\"{s}\"", .{s}),
            .nil => try writer.writeAll("nil"),
        }
    }
};

pub const RuntimeError = error{ TypeError, DivisionByZero, OutOfMemory };

pub const RuntimeErrorEnum = enum {
    TypeError,
    DivisionByZero,
    OutOfMemory,
};

fn runtimeError(err: RuntimeErrorEnum, token: Token) RuntimeError!Value {
    switch (err) {
        .TypeError => {
            std.debug.print("TypeError: operands must be numbers [line {d}], ^{s}", .{ token.line, token.lexeme });
            return RuntimeError.TypeError;
        },
        .DivisionByZero => {
            std.debug.print("DivisionByZero: division by zero [line {d}], ^{s}", .{ token.line, token.lexeme });
            return RuntimeError.DivisionByZero;
        },
        .OutOfMemory => {
            std.debug.print("OutOfMemory: no more memory [line {d}], ^{s}", .{ token.line, token.lexeme });
            return RuntimeError.OutOfMemory;
        },
    }
}

pub fn interpret(expr: Expr, alloc: std.mem.Allocator) void {
    const value = evaluate(expr, alloc) catch return;
    std.debug.print("{}\n", .{value});
}

fn evaluate(expr: Expr, alloc: std.mem.Allocator) RuntimeError!Value {
    switch (expr) {
        .literal => |l| return evaluateLiteral(l),
        .unary => |u| return evaluateUnary(u, alloc),
        .binary => |b| return evaluateBinary(b, alloc),
        .grouping => |g| return evaluate(g.expression.*, alloc),
    }
}

fn evaluateLiteral(literal: Literal) Value {
    switch (literal) {
        .int => |n| return Value{ .int = n },
        .float => |n| return Value{ .float = n },
        .str => |s| {
            if (std.ascii.eqlIgnoreCase(s, "true")) {
                return Value{ .boolean = true };
            } else if (std.ascii.eqlIgnoreCase(s, "false")) {
                return Value{ .boolean = false };
            } else {
                return Value{ .str = s };
            }
        },
        .void => return Value{ .nil = {} },
    }
}

fn evaluateUnary(unary: Unary, alloc: std.mem.Allocator) RuntimeError!Value {
    const right = try evaluate(unary.right.*, alloc);

    switch (unary.operator.type) {
        .MINUS => {
            if (right == .int) return Value{ .int = -right.int };
            if (right == .float) return Value{ .float = -right.float };
            return RuntimeError.TypeError;
        },
        .BANG => return Value{ .boolean = !isTruthy(right) },
        else => unreachable,
    }
}

fn evaluateBinary(binary: Binary, alloc: std.mem.Allocator) RuntimeError!Value {
    const left = try evaluate(binary.left.*, alloc);
    const right = try evaluate(binary.right.*, alloc);

    switch (binary.operator.type) {
        .PLUS => {
            switch (left) {
                .int => |l| switch (right) {
                    .int => |r| return Value{ .int = l + r },
                    .float => |r| return Value{ .float = @as(f64, @floatFromInt(l)) + r },
                    else => return try runtimeError(
                        RuntimeErrorEnum.TypeError,
                        binary.operator,
                    ),
                },
                .float => |l| switch (right) {
                    .int => |r| return Value{ .float = l + @as(f64, @floatFromInt(r)) },
                    .float => |r| return Value{ .float = l + r },
                    else => return try runtimeError(RuntimeErrorEnum.TypeError, binary.operator),
                },
                .str => |l| if (right == .str) {
                    const result = std.fmt.allocPrint(alloc, "{s}{s}", .{ l, right.str }) catch |err| switch (err) {
                        error.OutOfMemory => return try runtimeError(RuntimeErrorEnum.TypeError, binary.operator),
                    };
                    return Value{ .str = result };
                } else return try runtimeError(RuntimeErrorEnum.TypeError, binary.operator),
                else => return try runtimeError(RuntimeErrorEnum.TypeError, binary.operator),
            }
        },
        .MINUS => {
            switch (left) {
                .int => |l| switch (right) {
                    .int => |r| return Value{ .int = l - r },
                    .float => |r| return Value{ .float = @as(f64, @floatFromInt(l)) - r },
                    else => return try runtimeError(RuntimeErrorEnum.TypeError, binary.operator),
                },
                .float => |l| switch (right) {
                    .int => |r| return Value{ .float = l - @as(f64, @floatFromInt(r)) },
                    .float => |r| return Value{ .float = l - r },
                    else => return try runtimeError(RuntimeErrorEnum.TypeError, binary.operator),
                },
                else => return try runtimeError(RuntimeErrorEnum.TypeError, binary.operator),
            }
        },
        .STAR => {
            switch (left) {
                .int => |l| switch (right) {
                    .int => |r| return Value{ .int = l * r },
                    .float => |r| return Value{ .float = @as(f64, @floatFromInt(l)) * r },
                    else => return try runtimeError(RuntimeErrorEnum.TypeError, binary.operator),
                },
                .float => |l| switch (right) {
                    .int => |r| return Value{ .float = l * @as(f64, @floatFromInt(r)) },
                    .float => |r| return Value{ .float = l * r },
                    else => return try runtimeError(RuntimeErrorEnum.TypeError, binary.operator),
                },
                else => return try runtimeError(RuntimeErrorEnum.TypeError, binary.operator),
            }
        },
        .SLASH => {
            switch (left) {
                .int => |l| switch (right) {
                    .int => |r| {
                        if (r == 0) return try runtimeError(RuntimeErrorEnum.DivisionByZero, binary.operator);
                        return Value{ .float = @as(f64, @floatFromInt(l)) / @as(f64, @floatFromInt(r)) };
                    },
                    .float => |r| {
                        if (r == 0.0) return try runtimeError(RuntimeErrorEnum.DivisionByZero, binary.operator);
                        return Value{ .float = @as(f64, @floatFromInt(l)) / r };
                    },
                    else => return try runtimeError(RuntimeErrorEnum.TypeError, binary.operator),
                },
                .float => |l| switch (right) {
                    .int => |r| {
                        if (r == 0) return try runtimeError(RuntimeErrorEnum.DivisionByZero, binary.operator);
                        return Value{ .float = l / @as(f64, @floatFromInt(r)) };
                    },
                    .float => |r| {
                        if (r == 0.0) return try runtimeError(RuntimeErrorEnum.DivisionByZero, binary.operator);
                        return Value{ .float = l / r };
                    },
                    else => return try runtimeError(RuntimeErrorEnum.TypeError, binary.operator),
                },
                else => return try runtimeError(RuntimeErrorEnum.TypeError, binary.operator),
            }
        },
        else => return try runtimeError(RuntimeErrorEnum.TypeError, binary.operator),
    }
}

fn isTruthy(value: Value) bool {
    switch (value) {
        .boolean => |b| return b,
        .nil => return false,
        else => return true,
    }
}
