const std = @import("std");
const token = @import("token.zig");
const Token = token.Token;
const TokenType = token.TokenType;
const Literal = token.Literal;

pub const Scanner = struct {
    start: usize,
    current: usize,
    line: usize,

    tokens: std.ArrayList(Token),
    alloc: std.mem.Allocator,

    source: []const u8,

    pub fn init(source: []const u8, alloc: std.mem.Allocator) !Scanner {
        return Scanner{ .source = source, .start = 0, .current = 0, .line = 1, .alloc = alloc, .tokens = std.ArrayList(Token).init(alloc) };
    }

    pub fn deinit(self: *Scanner) void {
        self.tokens.deinit();
    }

    pub fn scanTokens(self: *Scanner) !std.ArrayList(Token) {
        while (!self.isAtEnd()) {
            self.start = self.current;
            try self.scanToken();
        }
        return self.tokens;
    }

    pub fn scanToken(self: *Scanner) !void {
        const c = self.advance();
        switch (c) {
            '(' => try self.addToken(TokenType.LEFT_PAREN, Literal{ .void = {} }),
            ')' => try self.addToken(TokenType.RIGHT_PAREN, Literal{ .void = {} }),
            '{' => try self.addToken(TokenType.LEFT_BRACE, Literal{ .void = {} }),
            '}' => try self.addToken(TokenType.RIGHT_BRACE, Literal{ .void = {} }),
            ',' => try self.addToken(TokenType.COMMA, Literal{ .void = {} }),
            '.' => try self.addToken(TokenType.DOT, Literal{ .void = {} }),
            '-' => try self.addToken(TokenType.MINUS, Literal{ .void = {} }),
            '+' => try self.addToken(TokenType.PLUS, Literal{ .void = {} }),
            ';' => try self.addToken(TokenType.SEMICOLON, Literal{ .void = {} }),
            '*' => try self.addToken(TokenType.STAR, Literal{ .void = {} }),
            else => {},
        }
    }

    pub fn advance(self: *Scanner) u8 {
        const c = self.source[self.current];
        self.current += 1;
        return c;
    }

    pub fn addToken(self: *Scanner, token_type: TokenType, literal: Literal) !void {
        try self.tokens.append(Token{ .type = token_type, .literal = literal, .line = self.line, .lexeme = self.source[self.start..self.current] });
    }

    pub fn isAtEnd(self: Scanner) bool {
        return self.current >= self.source.len;
    }
};
