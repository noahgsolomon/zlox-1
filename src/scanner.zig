const std = @import("std");
const token = @import("token.zig");
const ZLox = @import("zlox.zig").ZLox;
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

    keyword_map: std.StringHashMap(TokenType),

    pub fn init(source: []const u8, alloc: std.mem.Allocator) !Scanner {
        return Scanner{ .source = source, .start = 0, .current = 0, .line = 1, .alloc = alloc, .tokens = std.ArrayList(Token).init(alloc), .keyword_map = token.initKeywords(alloc) };
    }

    pub fn deinit(self: *Scanner) void {
        self.tokens.deinit();
        self.keyword_map.deinit();
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
            '!' => try self.addToken(if (self.match('=')) TokenType.BANG_EQUAL else TokenType.BANG, Literal{ .void = {} }),
            '=' => try self.addToken(if (self.match('=')) TokenType.EQUAL_EQUAL else TokenType.EQUAL, Literal{ .void = {} }),
            '<' => try self.addToken(if (self.match('=')) TokenType.LESS_EQUAL else TokenType.LESS, Literal{ .void = {} }),
            '>' => try self.addToken(if (self.match('=')) TokenType.GREATER_EQUAL else TokenType.GREATER, Literal{ .void = {} }),
            '/' => {
                if (self.match('/')) {
                    while (self.peek() != '\n' and !self.isAtEnd()) _ = self.advance();
                } else {
                    try self.addToken(TokenType.SLASH, Literal{ .void = {} });
                }
            },
            ' ' => {},
            '\r' => {},
            '\t' => {},
            '\n' => self.line += 1,
            '"' => try self.string(),
            '0'...'9' => try self.number(),
            'A'...'Z', 'a'...'z', '_' => try self.identifier(),
            else => {
                ZLox.scanError(self.line, "Unexpected character.");
            },
        }
    }

    pub fn identifier(self: *Scanner) !void {
        while (std.ascii.isAlphanumeric(self.peek())) {
            _ = self.advance();
        }

        var token_type = TokenType.IDENTIFIER;
        const keyword = self.keyword_map.get(self.source[self.start..self.current]);

        if (keyword) |value| {
            token_type = value;
        }

        try self.addToken(token_type, Literal{ .void = {} });
    }

    pub fn string(self: *Scanner) !void {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') {
                ZLox.scanError(self.line, "Invalid, cannot have multi-line string literals");
                self.line += 1;
            }
            _ = self.advance();

            if (self.isAtEnd()) {
                ZLox.scanError(self.line, "Unterminated string.");
            }
        }

        _ = self.advance();

        const value = self.source[self.start + 1 .. self.current - 1];
        try self.addToken(TokenType.STRING, Literal{ .str = value });
    }

    pub fn number(self: *Scanner) !void {
        var is_int = true;
        while (std.ascii.isDigit(self.peek())) {
            _ = self.advance();
        }

        if (self.peek() == '.' and std.ascii.isDigit(self.peekNext())) {
            is_int = false;
            _ = self.advance();
            while (std.ascii.isDigit(self.peek())) {
                _ = self.advance();
            }
        }

        if (is_int) {
            const int = try std.fmt.parseInt(i64, self.source[self.start..self.current], 10);
            try self.addToken(TokenType.INT, Literal{ .int = int });
        } else {
            const float = try std.fmt.parseFloat(f64, self.source[self.start..self.current]);
            try self.addToken(TokenType.FLOAT, Literal{ .float = float });
        }
    }

    pub fn peek(self: *Scanner) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.current];
    }

    pub fn peekNext(self: *Scanner) u8 {
        if (self.current + 1 >= self.source.len) return 0;
        return self.source[self.current + 1];
    }

    pub fn match(self: *Scanner, expected: u8) bool {
        if (self.isAtEnd()) return false;

        if (self.source[self.current] != expected) {
            return false;
        }

        self.current += 1;
        return true;
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
