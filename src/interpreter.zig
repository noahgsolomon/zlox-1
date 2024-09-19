const std = @import("std");
const Expr = @import("parser.zig").Expr;
const Binary = @import("parser.zig").Binary;
const Unary = @import("parser.zig").Unary;
const Literal = @import("token.zig").Literal;
const Token = @import("token.zig").Token;

pub const VarDecl = struct {
    name: Token,
    initializer: ?*Expr,
};

pub const Stmt = union(enum) {
    expression: *Expr,
    print: *Expr,
    var_decl: VarDecl,
};

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

pub const RuntimeError = error{ TypeError, DivisionByZero, OutOfMemory, UndefinedVariable };

pub const RuntimeErrorEnum = enum {
    TypeError,
    DivisionByZero,
    OutOfMemory,
    UndefinedVariable,
};

pub const Environment = struct {
    values: std.StringHashMap(Value),

    pub fn init(allocator: std.mem.Allocator) Environment {
        return .{
            .values = std.StringHashMap(Value).init(allocator),
        };
    }

    pub fn deinit(self: *Environment) void {
        self.values.deinit();
    }

    pub fn define(self: *Environment, name: []const u8, value: Value) !void {
        try self.values.put(name, value);
    }

    pub fn get(self: *Environment, name: Token) RuntimeError!Value {
        if (self.values.get(name.lexeme)) |value| {
            return value;
        }
        return RuntimeError.UndefinedVariable;
    }
};

pub const Interpreter = struct {
    environment: Environment,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Interpreter {
        return .{
            .environment = Environment.init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Interpreter) void {
        self.environment.deinit();
    }

    pub fn interpret(self: *Interpreter, statements: []const Stmt) void {
        for (statements) |stmt| {
            self.execute(stmt) catch |err| {
                runtimeError(err, stmt.expression.operator);
            };
        }
    }

    pub fn execute(self: *Interpreter, stmt: Stmt) RuntimeError!void {
        switch (stmt) {
            .expression => |expr| _ = try self.evaluate(expr),
            .print => |print_stmt| {
                const value = try self.evaluate(print_stmt);
                std.debug.print("{}\n", .{value});
            },
            .var_decl => |var_decl| try self.executeVarDecl(var_decl),
        }
    }

    pub fn executeVarDecl(self: *Interpreter, var_decl: VarDecl) RuntimeError!void {
        var value: Value = .{ .nil = {} };
        if (var_decl.initializer) |initializer| {
            value = try self.evaluate(initializer.*);
        }
        try self.environment.define(var_decl.name.lexeme, value);
    }

    pub fn evaluate(self: *Interpreter, expr: Expr) RuntimeError!Value {
        switch (expr) {
            .literal => |l| return evaluateLiteral(l),
            .unary => |u| return self.evaluateUnary(u),
            .binary => |b| return self.evaluateBinary(b),
            .grouping => |g| return self.evaluate(g.expression.*),
            .variable => |v| return self.environment.get(v.name),
        }
    }

    pub fn evaluateLiteral(literal: Literal) Value {
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

    pub fn evaluateUnary(self: *Interpreter, unary: Unary) RuntimeError!Value {
        const right = try self.evaluate(unary.right.*);

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

    pub fn evaluateBinary(self: *Interpreter, binary: Binary) RuntimeError!Value {
        const left = try self.evaluate(binary.left.*);
        const right = try self.evaluate(binary.right.*);

        switch (binary.operator.type) {
            .PLUS => {
                switch (left) {
                    .int => |l| switch (right) {
                        .int => |r| return Value{ .int = l + r },
                        .float => |r| return Value{ .float = @as(f64, @floatFromInt(l)) + r },
                        else => return RuntimeError.TypeError,
                    },
                    .float => |l| switch (right) {
                        .int => |r| return Value{ .float = l + @as(f64, @floatFromInt(r)) },
                        .float => |r| return Value{ .float = l + r },
                        else => return RuntimeError.TypeError,
                    },
                    .str => |l| if (right == .str) {
                        const result = try std.fmt.allocPrint(self.allocator, "{s}{s}", .{ l, right.str });
                        return Value{ .str = result };
                    } else return RuntimeError.TypeError,
                    else => return RuntimeError.TypeError,
                }
            },
            .MINUS => {
                switch (left) {
                    .int => |l| switch (right) {
                        .int => |r| return Value{ .int = l - r },
                        .float => |r| return Value{ .float = @as(f64, @floatFromInt(l)) - r },
                        else => return RuntimeError.TypeError,
                    },
                    .float => |l| switch (right) {
                        .int => |r| return Value{ .float = l - @as(f64, @floatFromInt(r)) },
                        .float => |r| return Value{ .float = l - r },
                        else => return RuntimeError.TypeError,
                    },
                    else => return RuntimeError.TypeError,
                }
            },
            .STAR => {
                switch (left) {
                    .int => |l| switch (right) {
                        .int => |r| return Value{ .int = l * r },
                        .float => |r| return Value{ .float = @as(f64, @floatFromInt(l)) * r },
                        else => return RuntimeError.TypeError,
                    },
                    .float => |l| switch (right) {
                        .int => |r| return Value{ .float = l * @as(f64, @floatFromInt(r)) },
                        .float => |r| return Value{ .float = l * r },
                        else => return RuntimeError.TypeError,
                    },
                    else => return RuntimeError.TypeError,
                }
            },
            .SLASH => {
                switch (left) {
                    .int => |l| switch (right) {
                        .int => |r| {
                            if (r == 0) return RuntimeError.DivisionByZero;
                            return Value{ .float = @as(f64, @floatFromInt(l)) / @as(f64, @floatFromInt(r)) };
                        },
                        .float => |r| {
                            if (r == 0.0) return RuntimeError.DivisionByZero;
                            return Value{ .float = @as(f64, @floatFromInt(l)) / r };
                        },
                        else => return RuntimeError.TypeError,
                    },
                    .float => |l| switch (right) {
                        .int => |r| {
                            if (r == 0) return RuntimeError.DivisionByZero;
                            return Value{ .float = l / @as(f64, @floatFromInt(r)) };
                        },
                        .float => |r| {
                            if (r == 0.0) return RuntimeError.DivisionByZero;
                            return Value{ .float = l / r };
                        },
                        else => return RuntimeError.TypeError,
                    },
                    else => return RuntimeError.TypeError,
                }
            },
            else => return RuntimeError.TypeError,
        }
    }
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
        .UndefinedVariable => {
            std.debug.print("UndefinedVariable: undefined variable '{s}' [line {d}], ^{s}", .{ token.lexeme, token.line, token.lexeme });
            return RuntimeError.UndefinedVariable;
        },
    }
}

fn isTruthy(value: Value) bool {
    switch (value) {
        .boolean => |b| return b,
        .nil => return false,
        else => return true,
    }
}
