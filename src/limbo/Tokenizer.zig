const std = @import("std");

source: []const u8,
index: usize,

const Tokenizer = @This();

pub const Token = struct {
    tag: Tag,
    span: Span,

    pub const Span = struct {
        start: usize,
        len: usize,
    };

    pub const Tag = enum {
        invalid,
        eof,
        plus,
        minus,
        asterisk,
        slash,
        percent,
        ampersand,
        pipe,
        caret,
        equal_equal,
        l_angle,
        r_angle,
        l_angle_equal,
        r_angle_equal,
        bang_equal,
        l_angle_l_angle,
        r_angle_r_angle,
        ampersand_ampersand,
        pipe_pipe,
        l_angle_minus,
        colon_colon,
        equal,
        plus_equal,
        minus_equal,
        asterisk_equal,
        slash_equal,
        percent_equal,
        ampersand_equal,
        pipe_equal,
        caret_equal,
        l_angle_l_angle_equal,
        r_angle_r_angle_equal,
        colon_equal,
        tilde,
        plus_plus,
        minus_minus,
        bang,
        asterisk_asterisk,
        colon,
        semicolon,
        l_paren,
        r_paren,
        l_brace,
        r_brace,
        l_bracket,
        r_bracket,
        comma,
        period,
        minus_r_angle,
        equal_r_angle,
        constant_character,
        constant_number,
        constant_string,
        constant_raw_string,
        identifier,
        keyword_adt,
        keyword_alt,
        keyword_array,
        keyword_big,
        keyword_break,
        keyword_byte,
        keyword_case,
        keyword_chan,
        keyword_con,
        keyword_continue,
        keyword_cyclic,
        keyword_do,
        keyword_dynamic,
        keyword_else,
        keyword_exception,
        keyword_exit,
        keyword_fixed,
        keyword_fn,
        keyword_for,
        keyword_hd,
        keyword_if,
        keyword_implement,
        keyword_import,
        keyword_include,
        keyword_int,
        keyword_len,
        keyword_list,
        keyword_load,
        keyword_module,
        keyword_nil,
        keyword_of,
        keyword_or,
        keyword_pick,
        keyword_raise,
        keyword_raises,
        keyword_real,
        keyword_ref,
        keyword_return,
        keyword_self,
        keyword_spawn,
        keyword_string,
        keyword_tagof,
        keyword_tl,
        keyword_to,
        keyword_type,
        keyword_while,

        pub const keywords = std.ComptimeStringMap(Tag, .{
            .{ "adt", .keyword_adt },
            .{ "alt", .keyword_alt },
            .{ "array", .keyword_array },
            .{ "big", .keyword_big },
            .{ "break", .keyword_break },
            .{ "byte", .keyword_byte },
            .{ "case", .keyword_case },
            .{ "chan", .keyword_chan },
            .{ "con", .keyword_con },
            .{ "continue", .keyword_continue },
            .{ "cyclic", .keyword_cyclic },
            .{ "do", .keyword_do },
            .{ "dynamic", .keyword_dynamic },
            .{ "else", .keyword_else },
            .{ "exception", .keyword_exception },
            .{ "exit", .keyword_exit },
            .{ "fixed", .keyword_fixed },
            .{ "fn", .keyword_fn },
            .{ "for", .keyword_for },
            .{ "hd", .keyword_hd },
            .{ "if", .keyword_if },
            .{ "implement", .keyword_implement },
            .{ "import", .keyword_import },
            .{ "include", .keyword_include },
            .{ "int", .keyword_int },
            .{ "len", .keyword_len },
            .{ "list", .keyword_list },
            .{ "load", .keyword_load },
            .{ "module", .keyword_module },
            .{ "nil", .keyword_nil },
            .{ "of", .keyword_of },
            .{ "or", .keyword_or },
            .{ "pick", .keyword_pick },
            .{ "raise", .keyword_raise },
            .{ "raises", .keyword_raises },
            .{ "real", .keyword_real },
            .{ "ref", .keyword_ref },
            .{ "return", .keyword_return },
            .{ "self", .keyword_self },
            .{ "spawn", .keyword_spawn },
            .{ "string", .keyword_string },
            .{ "tagof", .keyword_tagof },
            .{ "tl", .keyword_tl },
            .{ "to", .keyword_to },
            .{ "type", .keyword_type },
            .{ "while", .keyword_while },
        });

        pub fn lexeme(tag: Tag) ?[]const u8 {
            return switch (tag) {
                .invalid,
                .eof,
                .constant_character,
                .constant_number,
                .constant_string,
                .constant_raw_string,
                .identifier,
                => null,

                .plus => "+",
                .minus => "-",
                .asterisk => "*",
                .slash => "/",
                .percent => "%",
                .ampersand => "&",
                .pipe => "|",
                .caret => "^",
                .equal_equal => "==",
                .l_angle => "<",
                .r_angle => ">",
                .l_angle_equal => "<=",
                .r_angle_equal => ">=",
                .bang_equal => "!=",
                .l_angle_l_angle => "<<",
                .r_angle_r_angle => ">>",
                .ampersand_ampersand => "&&",
                .pipe_pipe => "||",
                .l_angle_minus => "<-",
                .colon_colon => "::",
                .equal => "=",
                .plus_equal => "+=",
                .minus_equal => "-=",
                .asterisk_equal => "*=",
                .slash_equal => "/=",
                .percent_equal => "%=",
                .ampersand_equal => "&=",
                .pipe_equal => "|=",
                .caret_equal => "^=",
                .l_angle_l_angle_equal => "<<=",
                .r_angle_r_angle_equal => ">>=",
                .colon_equal => ":=",
                .tilde => "~",
                .plus_plus => "++",
                .minus_minus => "--",
                .bang => "!",
                .asterisk_asterisk => "**",
                .colon => ":",
                .semicolon => ";",
                .l_paren => "(",
                .r_paren => ")",
                .l_brace => "{",
                .r_brace => "}",
                .l_bracket => "[",
                .r_bracket => "]",
                .comma => ",",
                .period => ".",
                .minus_r_angle => "->",
                .equal_r_angle => "=>",

                .keyword_adt => "adt",
                .keyword_alt => "alt",
                .keyword_array => "array",
                .keyword_big => "big",
                .keyword_break => "break",
                .keyword_byte => "byte",
                .keyword_case => "case",
                .keyword_chan => "chan",
                .keyword_con => "con",
                .keyword_continue => "continue",
                .keyword_cyclic => "cyclic",
                .keyword_do => "do",
                .keyword_dynamic => "dynamic",
                .keyword_else => "else",
                .keyword_exception => "exception",
                .keyword_exit => "exit",
                .keyword_fixed => "fixed",
                .keyword_fn => "fn",
                .keyword_for => "for",
                .keyword_hd => "hd",
                .keyword_if => "if",
                .keyword_implement => "implement",
                .keyword_import => "import",
                .keyword_include => "include",
                .keyword_int => "int",
                .keyword_len => "len",
                .keyword_list => "list",
                .keyword_load => "load",
                .keyword_module => "module",
                .keyword_nil => "nil",
                .keyword_of => "of",
                .keyword_or => "or",
                .keyword_pick => "pick",
                .keyword_raise => "raise",
                .keyword_raises => "raises",
                .keyword_real => "real",
                .keyword_ref => "ref",
                .keyword_return => "return",
                .keyword_self => "self",
                .keyword_spawn => "spawn",
                .keyword_string => "string",
                .keyword_tagof => "tagof",
                .keyword_tl => "tl",
                .keyword_to => "to",
                .keyword_type => "type",
                .keyword_while => "while",
            };
        }

        pub fn symbol(tag: Tag) []const u8 {
            return tag.lexeme() orelse switch (tag) {
                .invalid => "invalid bytes",
                .eof => "EOF",
                .constant_character => "a character constant",
                .constant_number => "a number constant",
                .constant_string, .constant_raw_string => "a string constant",
                .identifier => "an identifier",
                else => unreachable,
            };
        }
    };
};

pub fn init(source: []const u8) Tokenizer {
    return .{ .source = source, .index = 0 };
}

pub fn next(tokenizer: *Tokenizer) Token {
    var token: Token = .{
        .tag = .eof,
        .span = .{ .start = tokenizer.index, .len = undefined },
    };

    var state: union(enum) {
        start,
        comment,
        identifier,
        number,
        character,
        character_backslash,
        string,
        string_backslash,
        raw_string,
        plus,
        minus,
        asterisk,
        slash,
        percent,
        ampersand,
        pipe,
        caret,
        equal,
        l_angle,
        l_angle_l_angle,
        r_angle,
        r_angle_r_angle,
        bang,
        colon,
    } = .start;

    var c_len: u3 = undefined;
    while (tokenizer.index < tokenizer.source.len) : (tokenizer.index += c_len) {
        c_len = std.unicode.utf8ByteSequenceLength(tokenizer.source[tokenizer.index]) catch {
            token.tag = .invalid;
            tokenizer.index += 1;
            break;
        };
        if (tokenizer.index + c_len > tokenizer.source.len) {
            token.tag = .invalid;
            tokenizer.index = tokenizer.source.len;
            break;
        }
        const c = switch (c_len) {
            1 => tokenizer.source[tokenizer.index],
            2 => std.unicode.utf8Decode2(tokenizer.source[tokenizer.index..][0..c_len]),
            3 => std.unicode.utf8Decode3(tokenizer.source[tokenizer.index..][0..c_len]),
            4 => std.unicode.utf8Decode4(tokenizer.source[tokenizer.index..][0..c_len]),
            else => unreachable,
        } catch {
            token.tag = .invalid;
            tokenizer.index += c_len;
            break;
        };

        switch (state) {
            .start => switch (c) {
                ' ', '\t', '\r', '\n' => token.span.start += 1,
                '#' => state = .comment,
                'a'...'z', 'A'...'Z', 160...std.math.maxInt(u21) => {
                    token.tag = .identifier;
                    state = .identifier;
                },
                '0'...'9' => {
                    token.tag = .constant_number;
                    state = .number;
                },
                '\'' => {
                    token.tag = .constant_character;
                    state = .character;
                },
                '"' => {
                    token.tag = .constant_string;
                    state = .string;
                },
                '`' => {
                    token.tag = .constant_raw_string;
                    state = .raw_string;
                },
                '+' => state = .plus,
                '-' => state = .minus,
                '*' => state = .asterisk,
                '/' => state = .slash,
                '%' => state = .percent,
                '&' => state = .ampersand,
                '|' => state = .pipe,
                '^' => state = .caret,
                '=' => state = .equal,
                '<' => state = .l_angle,
                '>' => state = .r_angle,
                '~' => {
                    token.tag = .tilde;
                    tokenizer.index += 1;
                    break;
                },
                '!' => state = .bang,
                ':' => state = .colon,
                '.' => {
                    token.tag = .period;
                    tokenizer.index += 1;
                    break;
                },
                ';' => {
                    token.tag = .semicolon;
                    tokenizer.index += 1;
                    break;
                },
                '(' => {
                    token.tag = .l_paren;
                    tokenizer.index += 1;
                    break;
                },
                ')' => {
                    token.tag = .r_paren;
                    tokenizer.index += 1;
                    break;
                },
                '{' => {
                    token.tag = .l_brace;
                    tokenizer.index += 1;
                    break;
                },
                '}' => {
                    token.tag = .r_brace;
                    tokenizer.index += 1;
                    break;
                },
                '[' => {
                    token.tag = .l_bracket;
                    tokenizer.index += 1;
                    break;
                },
                ']' => {
                    token.tag = .r_bracket;
                    tokenizer.index += 1;
                    break;
                },
                ',' => {
                    token.tag = .comma;
                    tokenizer.index += 1;
                    break;
                },
                else => {
                    token.tag = .invalid;
                    tokenizer.index += c_len;
                    break;
                },
            },
            .comment => switch (c) {
                '\n' => {
                    token.span.start = tokenizer.index + 1;
                    state = .start;
                },
                else => {},
            },
            .identifier => switch (c) {
                'a'...'z', 'A'...'Z', '0'...'9', '_', 160...std.math.maxInt(u21) => {},
                else => break,
            },
            .number => switch (c) {
                '0'...'9', 'a'...'z', 'A'...'Z', '_', '.', 160...std.math.maxInt(u21) => {},
                else => break,
            },
            .character => switch (c) {
                '\\' => state = .character_backslash,
                '\'' => {
                    tokenizer.index += 1;
                    break;
                },
                else => {},
            },
            .character_backslash => state = .character,
            .string => switch (c) {
                '\\' => state = .string_backslash,
                '"' => {
                    tokenizer.index += 1;
                    break;
                },
                else => {},
            },
            .string_backslash => state = .string,
            .raw_string => switch (c) {
                '`' => {
                    tokenizer.index += 1;
                    break;
                },
                else => {},
            },
            .plus => switch (c) {
                '=' => {
                    token.tag = .plus_equal;
                    tokenizer.index += 1;
                    break;
                },
                '+' => {
                    token.tag = .plus_plus;
                    tokenizer.index += 1;
                    break;
                },
                else => {
                    token.tag = .plus;
                    break;
                },
            },
            .minus => switch (c) {
                '=' => {
                    token.tag = .minus_equal;
                    tokenizer.index += 1;
                    break;
                },
                '-' => {
                    token.tag = .minus_minus;
                    tokenizer.index += 1;
                    break;
                },
                '>' => {
                    token.tag = .minus_r_angle;
                    tokenizer.index += 1;
                    break;
                },
                else => {
                    token.tag = .minus;
                    break;
                },
            },
            .asterisk => switch (c) {
                '=' => {
                    token.tag = .asterisk_equal;
                    tokenizer.index += 1;
                    break;
                },
                '*' => {
                    token.tag = .asterisk_asterisk;
                    tokenizer.index += 1;
                    break;
                },
                else => {
                    token.tag = .asterisk;
                    break;
                },
            },
            .slash => switch (c) {
                '=' => {
                    token.tag = .slash_equal;
                    tokenizer.index += 1;
                    break;
                },
                else => {
                    token.tag = .slash;
                    break;
                },
            },
            .percent => switch (c) {
                '=' => {
                    token.tag = .percent_equal;
                    tokenizer.index += 1;
                    break;
                },
                else => {
                    token.tag = .percent;
                    break;
                },
            },
            .ampersand => switch (c) {
                '&' => {
                    token.tag = .ampersand_ampersand;
                    tokenizer.index += 1;
                    break;
                },
                '=' => {
                    token.tag = .ampersand_equal;
                    tokenizer.index += 1;
                    break;
                },
                else => {
                    token.tag = .ampersand;
                    break;
                },
            },
            .pipe => switch (c) {
                '|' => {
                    token.tag = .pipe_pipe;
                    tokenizer.index += 1;
                    break;
                },
                '=' => {
                    token.tag = .pipe_equal;
                    tokenizer.index += 1;
                    break;
                },
                else => {
                    token.tag = .pipe;
                    break;
                },
            },
            .caret => switch (c) {
                '=' => {
                    token.tag = .caret_equal;
                    tokenizer.index += 1;
                    break;
                },
                else => {
                    token.tag = .caret;
                    break;
                },
            },
            .equal => switch (c) {
                '=' => {
                    token.tag = .equal_equal;
                    tokenizer.index += 1;
                    break;
                },
                '>' => {
                    token.tag = .equal_r_angle;
                    tokenizer.index += 1;
                    break;
                },
                else => {
                    token.tag = .equal;
                    break;
                },
            },
            .l_angle => switch (c) {
                '=' => {
                    token.tag = .l_angle_equal;
                    tokenizer.index += 1;
                    break;
                },
                '-' => {
                    token.tag = .l_angle_minus;
                    tokenizer.index += 1;
                    break;
                },
                '<' => state = .l_angle_l_angle,
                else => {
                    token.tag = .l_angle;
                    break;
                },
            },
            .l_angle_l_angle => switch (c) {
                '=' => {
                    token.tag = .l_angle_l_angle_equal;
                    tokenizer.index += 1;
                    break;
                },
                else => {
                    token.tag = .l_angle_l_angle;
                    break;
                },
            },
            .r_angle => switch (c) {
                '=' => {
                    token.tag = .r_angle_equal;
                    tokenizer.index += 1;
                    break;
                },
                '>' => state = .r_angle_r_angle,
                else => {
                    token.tag = .r_angle;
                    break;
                },
            },
            .r_angle_r_angle => switch (c) {
                '=' => {
                    token.tag = .r_angle_r_angle_equal;
                    tokenizer.index += 1;
                    break;
                },
                else => {
                    token.tag = .r_angle_r_angle;
                    break;
                },
            },
            .bang => switch (c) {
                '=' => {
                    token.tag = .bang_equal;
                    tokenizer.index += 1;
                    break;
                },
                else => {
                    token.tag = .bang;
                    break;
                },
            },
            .colon => switch (c) {
                ':' => {
                    token.tag = .colon_colon;
                    tokenizer.index += 1;
                    break;
                },
                '=' => {
                    token.tag = .colon_equal;
                    tokenizer.index += 1;
                    break;
                },
                else => {
                    token.tag = .colon;
                    break;
                },
            },
        }
    } else switch (state) {
        .start,
        .comment,
        .identifier,
        .number,
        => {},
        .character,
        .character_backslash,
        .string,
        .string_backslash,
        .raw_string,
        => token.tag = .invalid,
        .plus => token.tag = .plus,
        .minus => token.tag = .minus,
        .asterisk => token.tag = .asterisk,
        .slash => token.tag = .slash,
        .percent => token.tag = .percent,
        .ampersand => token.tag = .ampersand,
        .pipe => token.tag = .pipe,
        .caret => token.tag = .caret,
        .equal => token.tag = .equal,
        .l_angle => token.tag = .l_angle,
        .l_angle_l_angle => token.tag = .l_angle_l_angle,
        .r_angle => token.tag = .r_angle,
        .r_angle_r_angle => token.tag = .r_angle_r_angle,
        .bang => token.tag = .bang,
        .colon => token.tag = .colon,
    }

    token.span.len = tokenizer.index - token.span.start;
    if (token.tag == .identifier) {
        if (Token.Tag.keywords.get(tokenizer.source[token.span.start..][0..token.span.len])) |keyword| {
            token.tag = keyword;
        }
    }
    return token;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    if (args.len != 2) {
        return error.InvalidArguments; // Usage: zig run Tokenizer.zig -- input
    }

    const input = try std.fs.cwd().readFileAlloc(allocator, args[1], std.math.maxInt(usize));
    defer allocator.free(input);

    var tokenizer = Tokenizer.init(input);
    while (true) {
        const token = tokenizer.next();
        std.debug.print("{s: <24} {}\n", .{ token.tag.symbol(), token });
        switch (token.tag) {
            .invalid => {
                std.process.exit(1);
                break;
            },
            .eof => break,
            else => {},
        }
    }
}

test "keywords" {
    try testTokenize(
        \\adt alt array
        \\big break byte
        \\case chan con continue cyclic
        \\do dynamic
        \\else exception exit
        \\fixed fn for
        \\hd
        \\if implement import include int
        \\len list load
        \\module
        \\nil
        \\of or
        \\pick
        \\raise raises real ref return
        \\self spawn string
        \\tagof tl to type
        \\while
    ,
        &.{
            .keyword_adt,
            .keyword_alt,
            .keyword_array,
            .keyword_big,
            .keyword_break,
            .keyword_byte,
            .keyword_case,
            .keyword_chan,
            .keyword_con,
            .keyword_continue,
            .keyword_cyclic,
            .keyword_do,
            .keyword_dynamic,
            .keyword_else,
            .keyword_exception,
            .keyword_exit,
            .keyword_fixed,
            .keyword_fn,
            .keyword_for,
            .keyword_hd,
            .keyword_if,
            .keyword_implement,
            .keyword_import,
            .keyword_include,
            .keyword_int,
            .keyword_len,
            .keyword_list,
            .keyword_load,
            .keyword_module,
            .keyword_nil,
            .keyword_of,
            .keyword_or,
            .keyword_pick,
            .keyword_raise,
            .keyword_raises,
            .keyword_real,
            .keyword_ref,
            .keyword_return,
            .keyword_self,
            .keyword_spawn,
            .keyword_string,
            .keyword_tagof,
            .keyword_tl,
            .keyword_to,
            .keyword_type,
            .keyword_while,
        },
    );
}

test "operators" {
    try testTokenize(
        \\+ - * / % & | ^ == < > <= >= != << >> && || <- ::
        \\= += -= *= /= %= &= |= ^= <<= >>= :=
        \\~ ++ -- ! **
    ,
        &.{
            .plus,
            .minus,
            .asterisk,
            .slash,
            .percent,
            .ampersand,
            .pipe,
            .caret,
            .equal_equal,
            .l_angle,
            .r_angle,
            .l_angle_equal,
            .r_angle_equal,
            .bang_equal,
            .l_angle_l_angle,
            .r_angle_r_angle,
            .ampersand_ampersand,
            .pipe_pipe,
            .l_angle_minus,
            .colon_colon,
            .equal,
            .plus_equal,
            .minus_equal,
            .asterisk_equal,
            .slash_equal,
            .percent_equal,
            .ampersand_equal,
            .pipe_equal,
            .caret_equal,
            .l_angle_l_angle_equal,
            .r_angle_r_angle_equal,
            .colon_equal,
            .tilde,
            .plus_plus,
            .minus_minus,
            .bang,
            .asterisk_asterisk,
        },
    );
}

test "separators" {
    try testTokenize(": ; ( ) { } [ ] , . -> =>", &.{
        .colon,
        .semicolon,
        .l_paren,
        .r_paren,
        .l_brace,
        .r_brace,
        .l_bracket,
        .r_bracket,
        .comma,
        .period,
        .minus_r_angle,
        .equal_r_angle,
    });
}

// TODO: more tests

fn testTokenize(source: []const u8, expected_token_tags: []const Token.Tag) !void {
    var tokenizer = Tokenizer.init(source);
    for (expected_token_tags) |expected_token_tag| {
        const token = tokenizer.next();
        try std.testing.expectEqual(expected_token_tag, token.tag);
    }
    const last_token = tokenizer.next();
    try std.testing.expectEqual(Token.Tag.eof, last_token.tag);
    try std.testing.expectEqual(source.len, last_token.span.start);
    try std.testing.expectEqual(@as(usize, 0), last_token.span.len);
}
