const std = @import("std");
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const Literal = @import("token.zig").Literal;

pub const Binary = struct { left: *Expr, right: *Expr, operator: Token };

pub const Unary = struct { right: *Expr, operator: Token };

pub const Grouping = struct { expression: *Expr };

pub const Expr = union(enum) { binary: Binary, unary: Unary, literal: Literal, grouping: Grouping };

pub const Parser = struct {
    debug: bool,
    tokens: []const Token,
    current: usize,
    alloc: std.mem.Allocator,
    expr: ?Expr = null,

    pub fn init(tokens: []const Token, alloc: std.mem.Allocator, debug: bool) Parser {
        return Parser{ .tokens = tokens, .current = 0, .alloc = alloc, .debug = debug };
    }

    pub fn deinit(self: *Parser) void {
        if (self.expr) |expr| {
            self.printExpr(expr, 0);
            // self.freeExpr(expr);
        }
    }

    fn printExpr(self: *Parser, expr: Expr, indent: usize) void {
        const indentStr = "    ";
        var i: usize = 0;
        while (i < indent) : (i += 1) {
            std.debug.print("{s}", .{indentStr});
        }
        switch (expr) {
            .binary => |val| {
                std.debug.print("Binary\n", .{});
                i = 0;
                while (i < indent + 1) : (i += 1) {
                    std.debug.print("{s}", .{indentStr});
                }
                std.debug.print("Operator: {s}\n", .{@tagName(val.operator.type)});
                i = 0;
                while (i < indent + 1) : (i += 1) {
                    std.debug.print("{s}", .{indentStr});
                }
                std.debug.print("Left:\n", .{});
                self.printExpr(val.left.*, indent + 2);
                i = 0;
                while (i < indent + 1) : (i += 1) {
                    std.debug.print("{s}", .{indentStr});
                }
                std.debug.print("Right:\n", .{});
                self.printExpr(val.right.*, indent + 2);
            },
            .unary => |val| {
                std.debug.print("Unary\n", .{});
                i = 0;
                while (i < indent + 1) : (i += 1) {
                    std.debug.print("{s}", .{indentStr});
                }
                std.debug.print("Operator: {s}\n", .{@tagName(val.operator.type)});
                i = 0;
                while (i < indent + 1) : (i += 1) {
                    std.debug.print("{s}", .{indentStr});
                }
                std.debug.print("Right:\n", .{});
                self.printExpr(val.right.*, indent + 2);
            },
            .literal => |literalVal| {
                std.debug.print("Literal: ", .{});
                switch (literalVal) {
                    .int => |val| std.debug.print("{d}\n", .{val}),
                    .float => |val| std.debug.print("{d}\n", .{val}),
                    .str => |val| std.debug.print("\"{s}\"\n", .{val}),
                    .void => std.debug.print("void\n", .{}),
                }
            },
            .grouping => |val| {
                std.debug.print("Grouping\n", .{});
                self.printExpr(val.expression.*, indent + 1);
            },
        }
    }

    fn freeExpr(self: *Parser, expr: Expr) void {
        std.debug.print("Freeing expression: {s}\n", .{@tagName(expr)});
        switch (expr) {
            .binary => |val| {
                std.debug.print("  Freeing binary expression\n", .{});
                self.freeExpr(val.left.*);
                self.freeExpr(val.right.*);
                std.debug.print("  Destroying left node\n", .{});
                self.alloc.destroy(val.left);
                std.debug.print("  Destroying right node\n", .{});
                self.alloc.destroy(val.right);
            },
            .unary => |val| {
                std.debug.print("  Freeing unary expression\n", .{});
                self.freeExpr(val.right.*);
                std.debug.print("  Destroying right node\n", .{});
                self.alloc.destroy(val.right);
            },
            .literal => {
                std.debug.print("  Freeing literal\n", .{});
            },
            .grouping => |val| {
                std.debug.print("  Freeing grouping expression\n", .{});
                self.freeExpr(val.expression.*);
                std.debug.print("  Destroying expression node\n", .{});
                self.alloc.destroy(val.expression);
            },
        }
        std.debug.print("Finished freeing expression\n", .{});
    }

    pub fn parse(self: *Parser) Expr {
        if (self.debug) std.debug.print("Starting parsing\n", .{});
        const expr = self.expression();
        self.expr = expr;
        if (self.debug) std.debug.print("Finished parsing\n", .{});
        return expr;
    }

    fn isAtEnd(self: *Parser) bool {
        return self.peek().type == TokenType.EOF;
    }

    fn peek(self: *Parser) Token {
        return self.tokens[self.current];
    }

    fn previous(self: *Parser) Token {
        return self.tokens[self.current - 1];
    }

    fn advance(self: *Parser) Token {
        if (!self.isAtEnd()) {
            self.current += 1;
        }
        return self.previous();
    }

    fn parseBinaryExpr(self: *Parser, comptime nextMethod: fn (*Parser) Expr, comptime tokenTypes: []const TokenType) Expr {
        var expr = nextMethod(self);

        while (self.match(tokenTypes)) {
            const operator = self.previous();
            const left = self.alloc.create(Expr) catch unreachable;
            left.* = expr;
            const right = self.alloc.create(Expr) catch unreachable;
            right.* = nextMethod(self);
            expr = Expr{ .binary = .{ .left = left, .operator = operator, .right = right } };
        }

        return expr;
    }

    fn match(self: *Parser, types: []const TokenType) bool {
        for (types) |tokenType| {
            if (self.check(tokenType)) {
                _ = self.advance();
                return true;
            }
        }
        return false;
    }

    fn check(self: *Parser, tokenType: TokenType) bool {
        if (self.isAtEnd()) return false;
        return self.peek().type == tokenType;
    }

    fn expression(self: *Parser) Expr {
        if (self.debug) std.debug.print("Parsing expression\n", .{});
        return self.equality();
    }

    fn equality(self: *Parser) Expr {
        if (self.debug) std.debug.print("Parsing equality\n", .{});
        const token_types = comptime [_]TokenType{ TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL };
        return self.parseBinaryExpr(comparison, &token_types);
    }

    fn comparison(self: *Parser) Expr {
        if (self.debug) std.debug.print("Parsing comparison\n", .{});
        const token_types = comptime [_]TokenType{ TokenType.LESS, TokenType.LESS_EQUAL, TokenType.GREATER, TokenType.GREATER_EQUAL };
        return self.parseBinaryExpr(term, &token_types);
    }

    fn term(self: *Parser) Expr {
        if (self.debug) std.debug.print("Parsing term\n", .{});
        const token_types = comptime [_]TokenType{ TokenType.MINUS, TokenType.PLUS };
        return self.parseBinaryExpr(factor, &token_types);
    }

    fn factor(self: *Parser) Expr {
        if (self.debug) std.debug.print("Parsing factor\n", .{});
        const token_types = comptime [_]TokenType{ TokenType.SLASH, TokenType.STAR };
        return self.parseBinaryExpr(unary, &token_types);
    }

    fn unary(self: *Parser) Expr {
        if (self.debug) std.debug.print("Parsing unary\n", .{});
        const token_types = comptime [_]TokenType{ TokenType.MINUS, TokenType.BANG };

        if (self.match(&token_types)) {
            const operator = self.previous();
            var right = self.unary();
            const expr = self.alloc.create(Expr) catch unreachable;
            expr.* = Expr{ .unary = .{ .operator = operator, .right = &right } };
            if (self.debug) std.debug.print("Created unary expression\n", .{});
            return expr.*;
        }

        return self.primary() catch unreachable;
    }

    fn primary(self: *Parser) !Expr {
        if (self.debug) std.debug.print("Parsing primary\n", .{});
        if (self.match(&[_]TokenType{TokenType.TRUE})) {
            if (self.debug) std.debug.print("Parsed TRUE literal\n", .{});
            return Expr{ .literal = Literal{ .str = "true" } };
        }
        if (self.match(&[_]TokenType{TokenType.FALSE})) {
            if (self.debug) std.debug.print("Parsed FALSE literal\n", .{});
            return Expr{ .literal = Literal{ .str = "false" } };
        }
        if (self.match(&[_]TokenType{TokenType.NIL})) {
            if (self.debug) std.debug.print("Parsed NIL literal\n", .{});
            return Expr{ .literal = Literal{ .str = "nil" } };
        }

        if (self.match(&[_]TokenType{ TokenType.STRING, TokenType.INT, TokenType.FLOAT })) {
            if (self.debug) std.debug.print("Parsed literal: {any}\n", .{self.previous().literal});
            return Expr{ .literal = self.previous().literal };
        }

        if (self.match(&[_]TokenType{TokenType.LEFT_PAREN})) {
            if (self.debug) std.debug.print("Parsing grouping\n", .{});
            var expr = self.expression();
            const grouping = self.alloc.create(Expr) catch unreachable;
            grouping.* = Expr{ .grouping = .{ .expression = &expr } };
            if (self.debug) std.debug.print("Finished parsing grouping\n", .{});
            return grouping.*;
        }

        if (self.debug) {
            std.debug.print("Unexpected token: {any} at line {d}\n", .{ self.peek().type, self.peek().line });
            std.debug.print("Current parser state:\n", .{});
            std.debug.print("  Current index: {d}\n", .{self.current});
            std.debug.print("  Total tokens: {d}\n", .{self.tokens.len});
        }

        return error.UnexpectedToken;
    }
};
