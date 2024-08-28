const Token = @import("token.zig").Token;
const Literal = @import("token.zig").Literal;

pub const Binary = struct { left: Expr, right: Expr, operator: Token };

pub const Unary = struct { right: Expr, operator: Token };

pub const Grouping = struct { expression: Expr };

pub const Expr = union(enum) { binary: Binary, unary: Unary, literal: Literal, grouping: Grouping };
