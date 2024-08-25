const std = @import("std");

pub const TokenType = enum {
    // Single-character tokens.
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,

    // One or two character tokens.
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,

    // Literals.
    IDENTIFIER,
    STRING,
    INT,
    FLOAT,

    // Keywords.
    AND,
    CLASS,
    ELSE,
    FALSE,
    FUN,
    FOR,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,

    EOF,
};

pub const Literal = union(enum) { void, int: i64, float: f64, str: []const u8 };

pub const Token = struct {
    type: TokenType,
    lexeme: []const u8,
    literal: Literal,
    line: usize,

    pub fn print(self: Token) void {
        switch (self.literal) {
            .void => std.debug.print("{s} ", .{@tagName(self.type)}),
            .int => |value| std.debug.print("({s}: {d}) ", .{ @tagName(self.type), value }),
            .float => |value| std.debug.print("({s}: {d:.2}) ", .{ @tagName(self.type), value }),
            .str => |value| std.debug.print("({s}: {s}) ", .{ @tagName(self.type), value }),
        }
    }
};

pub fn initKeywords(allocator: std.mem.Allocator) std.StringHashMap(TokenType) {
    var keywords = std.StringHashMap(TokenType).init(allocator);
    keywords.put("and", TokenType.AND) catch unreachable;
    keywords.put("class", TokenType.CLASS) catch unreachable;
    keywords.put("else", TokenType.ELSE) catch unreachable;
    keywords.put("false", TokenType.FALSE) catch unreachable;
    keywords.put("for", TokenType.FOR) catch unreachable;
    keywords.put("fun", TokenType.FUN) catch unreachable;
    keywords.put("if", TokenType.IF) catch unreachable;
    keywords.put("nil", TokenType.NIL) catch unreachable;
    keywords.put("or", TokenType.OR) catch unreachable;
    keywords.put("print", TokenType.PRINT) catch unreachable;
    keywords.put("return", TokenType.RETURN) catch unreachable;
    keywords.put("super", TokenType.SUPER) catch unreachable;
    keywords.put("this", TokenType.THIS) catch unreachable;
    keywords.put("true", TokenType.TRUE) catch unreachable;
    keywords.put("var", TokenType.VAR) catch unreachable;
    keywords.put("while", TokenType.WHILE) catch unreachable;
    return keywords;
}
