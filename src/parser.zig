const std = @import("std");
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const Literal = @import("token.zig").Literal;

pub const Binary = struct { left: *Expr, right: *Expr, operator: Token };

pub const Unary = struct { right: *Expr, operator: Token };

pub const Grouping = struct { expression: *Expr };

pub const Expr = union(enum) { binary: Binary, unary: Unary, literal: Literal, grouping: Grouping };

pub const Parser = struct {
    tokens: []const Token,
    current: usize,
    alloc: std.mem.Allocator,
    pub fn init(tokens: []const Token, alloc: std.mem.Allocator) Parser {
        return Parser{ .tokens = tokens, .current = 0, .alloc = alloc };
    }

    pub fn deinit(self: *Parser) void {
        var expr = self.parse();
        self.freeExpr(&expr);
    }

    pub fn parse(self: *Parser) Expr {
        return self.expression();
    }

    fn freeExpr(self: *Parser, expr: *Expr) void {
        switch (expr.*) {
            .binary => |val| {
                self.freeExpr(val.left);
                self.freeExpr(val.right);
                self.alloc.destroy(val.left);
                self.alloc.destroy(val.right);
            },
            .unary => |val| {
                self.freeExpr(val.right);
                self.alloc.destroy(val.right);
            },
            .literal => {},
            .grouping => |val| {
                self.freeExpr(val.expression);
                self.alloc.destroy(val.expression);
            },
        }
    }

    fn parseBinaryExpr(self: *Parser, comptime nextMethod: fn (*Parser) Expr, comptime tokenTypes: []const TokenType) Expr {
        var expr = nextMethod(self);

        while (self.match(tokenTypes)) {
            const operator = self.tokens[self.current];
            const right = self.alloc.create(Expr) catch unreachable;
            right.* = nextMethod(self);
            expr = Expr{ .binary = .{ .left = &expr, .operator = operator, .right = right } };
        }

        return expr;
    }

    fn match(self: *Parser, types: []const TokenType) bool {
        for (types) |tokenType| {
            if (self.tokens[self.current].type == tokenType) {
                self.current += 1;
                return true;
            }
        }
        return false;
    }

    fn expression(self: *Parser) Expr {
        return self.equality();
    }

    fn equality(self: *Parser) Expr {
        const token_types = comptime [_]TokenType{ TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL };
        return self.parseBinaryExpr(comparison, &token_types);
    }

    fn comparison(self: *Parser) Expr {
        const token_types = comptime [_]TokenType{ TokenType.LESS, TokenType.LESS_EQUAL, TokenType.GREATER, TokenType.GREATER_EQUAL };
        return self.parseBinaryExpr(term, &token_types);
    }

    fn term(self: *Parser) Expr {
        const token_types = comptime [_]TokenType{ TokenType.MINUS, TokenType.PLUS };
        return self.parseBinaryExpr(factor, &token_types);
    }

    fn factor(self: *Parser) Expr {
        const token_types = comptime [_]TokenType{ TokenType.SLASH, TokenType.STAR };
        return self.parseBinaryExpr(unary, &token_types);
    }

    fn unary(self: *Parser) Expr {
        const token_types = comptime [_]TokenType{ TokenType.MINUS, TokenType.BANG };

        if (self.match(&token_types)) {
            const operator = self.tokens[self.current - 1];
            const right = self.alloc.create(Expr) catch unreachable;
            right.* = self.unary();
            return Expr{ .unary = .{ .operator = operator, .right = right } };
        }

        return self.primary();
    }

    fn primary(self: *Parser) Expr {
        if (self.match(&[_]TokenType{TokenType.TRUE})) {
            return Expr{ .literal = Literal{ .str = "true" } };
        }
        if (self.match(&[_]TokenType{TokenType.FALSE})) {
            return Expr{ .literal = Literal{ .str = "false" } };
        }
        if (self.match(&[_]TokenType{TokenType.NIL})) {
            return Expr{ .literal = Literal{ .str = "nil" } };
        }

        if (self.match(&[_]TokenType{ TokenType.STRING, TokenType.INT, TokenType.FLOAT })) {
            return Expr{ .literal = self.tokens[self.current - 1].literal };
        }

        if (self.match(&[_]TokenType{TokenType.LEFT_PAREN})) {
            const expr = self.alloc.create(Expr) catch unreachable;
            expr.* = self.expression();
            if (!self.match(&[_]TokenType{TokenType.RIGHT_PAREN})) {
                std.debug.print("bro u forgot a ')' dumby. line: {d}", .{self.tokens[self.current].line});
            }
            return Expr{ .grouping = .{ .expression = expr } };
        }
        std.debug.print("Unexpected token: {any} at line {d}\n", .{ self.tokens[self.current].type, self.tokens[self.current].line });

        return Expr{ .literal = Literal{ .void = {} } };

        // return error.UnexpectedToken;
    }
};
