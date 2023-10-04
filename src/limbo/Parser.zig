const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const assert = std.debug.assert;
const Tokenizer = @import("Tokenizer.zig");
const Token = Tokenizer.Token;
const Ast = @import("Ast.zig");
const Node = Ast.Node;

allocator: Allocator,
source: []const u8,
token_tags: []const Token.Tag,
tok_i: Token.Index = @enumFromInt(0),
errors: std.ArrayListUnmanaged(Ast.Error) = .{},
nodes: Node.List = .{},
extra_data: std.ArrayListUnmanaged(Node.Index) = .{},
scratch: std.ArrayListUnmanaged(Node.Index) = .{},

const Parser = @This();

pub fn deinit(p: *Parser) void {
    p.errors.deinit(p.allocator);
    p.nodes.deinit(p.allocator);
    p.extra_data.deinit(p.allocator);
    p.scratch.deinit(p.allocator);
    p.* = undefined;
}

pub fn parseRoot(p: *Parser) error{ OutOfMemory, ParseError }!void {
    assert(p.nodes.len == 0);
    try p.nodes.append(p.allocator, .{
        .tag = .root,
        .main_token = undefined,
        .data = undefined,
    });
    if (p.currentToken() == .keyword_implement) {
        p.nodes.items(.main_token)[0] = p.tok_i;
        _ = p.nextToken();
        while (true) {
            _ = try p.expectToken(.identifier);
            switch (p.currentToken()) {
                .comma => {},
                .semicolon => {
                    _ = p.nextToken();
                    break;
                },
                else => return p.fail(.expected_comma_or_semicolon),
            }
        }
    } else {
        p.nodes.items(.main_token)[0] = .none;
    }

    assert(p.scratch.items.len == 0);
    while (p.currentToken() != .eof) {
        try p.scratch.append(p.allocator, try p.parseTopLevelDecl());
    }
    p.nodes.items(.data)[0] = .{ .range = try p.scratchToRange(0) };
}

fn parseTopLevelDecl(p: *Parser) !Node.Index {
    if (p.currentToken() != .identifier) {
        return p.fail(.expected_decl);
    }
    const first_identifier = p.tok_i;
    _ = first_identifier;
    _ = p.nextToken();
    while (true) {
        switch (p.currentToken()) {
            .comma => {},
            .colon => {
                _ = p.nextToken();
                break;
            },
            else => return p.fail(.expected_comma_or_colon),
        }
        _ = try p.expectToken(.identifier);
    }
    switch (p.currentToken()) {
        .keyword_module => {
            _ = p.nextToken();
            _ = p.expectToken(.l_brace);
        },
        else => return p.fail(.todo),
    }
}

fn parseModuleMember(p: *Parser) !Node.Index {
    if (p.currentToken() != .identifier) {
        return p.fail(.expected_module_member);
    }
}

fn expectToken(p: *Parser, tag: Token.Tag) !Token.Index {
    if (p.currentToken() != tag) {
        return p.failMsg(.{
            .tag = .expected_token,
            .token = p.tok_i,
            .extra = .{ .expected_tag = tag },
        });
    }
    return p.nextToken();
}

fn currentToken(p: *Parser) Token.Tag {
    return p.token_tags[@intFromEnum(p.tok_i)];
}

fn nextToken(p: *Parser) Token.Index {
    const result = p.tok_i;
    p.tok_i = @enumFromInt(@intFromEnum(p.tok_i) + 1);
    return result;
}

fn scratchToRange(p: *Parser, start: usize) !Node.Data.Range {
    const extra_data_top = p.extra_data.items.len;
    try p.extra_data.appendSlice(p.allocator, p.scratch.items[start..]);
    return .{
        .start = @enumFromInt(@as(u32, @intCast(extra_data_top))),
        .len = @intCast(p.extra_data.items.len - extra_data_top),
    };
}

fn warn(p: *Parser, tag: Ast.Error.Tag) !void {
    @setCold(true);
    try p.warnMsg(.{ .tag = tag, .token = p.tok_i });
}

fn warnMsg(p: *Parser, msg: Ast.Error) !void {
    @setCold(true);
    try p.errors.append(p.allocator, msg);
}

fn fail(p: *Parser, tag: Ast.Error.Tag) error{ OutOfMemory, ParseError } {
    @setCold(true);
    return p.failMsg(.{ .tag = tag, .token = p.tok_i });
}

fn failMsg(p: *Parser, msg: Ast.Error) error{ OutOfMemory, ParseError } {
    @setCold(true);
    try p.warnMsg(msg);
    return error.ParseError;
}
