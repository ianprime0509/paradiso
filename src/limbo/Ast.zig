const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const assert = std.debug.assert;
const Tokenizer = @import("Tokenizer.zig");
const ByteOffset = Tokenizer.ByteOffset;
const Token = Tokenizer.Token;
const Parser = @import("Parser.zig");

source: []const u8,
tokens: Token.List.Slice,
nodes: Node.List.Slice,
extra_data: []Node.Index,
errors: []Error,

const Ast = @This();

pub const max_source_len = std.math.maxInt(u32);

pub fn deinit(ast: *Ast, allocator: Allocator) void {
    ast.tokens.deinit(allocator);
    ast.nodes.deinit(allocator);
    allocator.free(ast.extra_data);
    allocator.free(ast.errors);
    ast.* = undefined;
}

pub fn parse(allocator: Allocator, source: []const u8) Allocator.Error!Ast {
    assert(source.len <= max_source_len);

    var tokens: Token.List = .{};
    var tokenizer = Tokenizer.init(source);
    while (true) {
        const token = tokenizer.next();
        try tokens.append(allocator, .{
            .tag = token.tag,
            .start = @intCast(token.span.start),
        });
        if (token.tag == .eof) break;
    }

    var parser: Parser = .{
        .allocator = allocator,
        .source = source,
        .token_tags = tokens.items(.tag),
    };
    defer parser.deinit();

    parser.parseRoot() catch |e| switch (e) {
        error.OutOfMemory => return error.OutOfMemory,
        error.ParseError => assert(parser.errors.items.len > 0),
    };

    return .{
        .source = source,
        .tokens = tokens.toOwnedSlice(),
        .nodes = parser.nodes.toOwnedSlice(),
        .extra_data = try parser.extra_data.toOwnedSlice(allocator),
        .errors = try parser.errors.toOwnedSlice(allocator),
    };
}

pub const Node = struct {
    tag: Tag,
    main_token: Token.Index,
    data: Data,

    pub const Index = enum(u32) { none = 0, _ };
    pub const ExtraDataIndex = enum(u32) { _ };
    pub const List = std.MultiArrayList(Node);
    pub const Range = struct { start: Index, len: u32 };

    pub const Tag = enum(u8) {
        /// `main_token` is the `implement` token, if any.
        /// `data` is `range` for the top-level decls.
        root,
        /// `main_token` is the first identifier.
        /// `data` is `range` for the module fields.
        module_decl,
        module_field,
    };

    pub const Data = union {
        range: struct {
            start: ExtraDataIndex,
            len: u32,
        },
    };
};

pub const Error = struct {
    tag: Tag,
    token: Token.Index,
    extra: union {
        none: void,
        expected_tag: Token.Tag,
    } = .{ .none = {} },

    pub const Tag = enum(u8) {
        expected_comma_or_colon,
        expected_comma_or_semicolon,
        expected_decl,
        expected_module_member,
        todo,

        /// `expected_tag` is populated.
        expected_token,
    };
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    if (args.len != 2) {
        return error.InvalidArguments; // Usage: zig run Ast.zig -- input
    }

    const input = try std.fs.cwd().readFileAlloc(allocator, args[1], max_source_len);
    defer allocator.free(input);

    var ast = try Ast.parse(allocator, input);
    defer ast.deinit(allocator);

    std.debug.print("{}\n", .{ast});
}
