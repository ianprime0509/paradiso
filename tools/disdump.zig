const std = @import("std");
const paradiso = @import("paradiso");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    var stdout_buf = std.io.bufferedWriter(std.io.getStdOut().writer());
    const stdout = stdout_buf.writer();
    for (args[1..]) |arg| {
        try stdout.print("{s}:\n\n", .{arg});

        var file = try std.fs.cwd().openFile(arg, .{});
        defer file.close();
        var br = std.io.bufferedReader(file.reader());
        var object = try paradiso.dis.Object.read(allocator, br.reader());
        defer object.deinit(allocator);

        try stdout.print("flags: {}\n", .{object.flags});
        try stdout.print("stack extent: {}\n", .{object.stack_extent});
        try stdout.print("entry pc: {}\n", .{object.entry_pc});
        try stdout.print("entry type: {}\n", .{object.entry_type});

        try stdout.print("instructions:\n", .{});
        for (object.instructions, 0..) |instruction, i| {
            try stdout.print("  {: >8}: {}\n", .{ i, instruction });
        }

        try stdout.print("type descriptors:\n", .{});
        for (object.type_descriptors, 0..) |type_descriptor, i| {
            try stdout.print("  {: >8}: {}\n", .{ i, type_descriptor });
        }

        try stdout.print("data:\n", .{});
        var data_rows = std.mem.window(u8, object.data, 16, 16);
        while (data_rows.next()) |row| {
            const data_row_base = @intFromPtr(row.ptr) - @intFromPtr(object.data.ptr);
            try stdout.print("  {X:0>8}: ", .{data_row_base});
            var data_words = std.mem.window(u8, row, 2, 2);
            while (data_words.next()) |word| {
                if (word.len > 0) {
                    try stdout.print("{X:0>2}", .{word[0]});
                } else {
                    try stdout.print("  ", .{});
                }
                if (word.len > 1) {
                    try stdout.print("{X:0>2} ", .{word[1]});
                } else {
                    try stdout.print("   ", .{});
                }
            }
            for (0..((16 - row.len) / 2)) |_| {
                try stdout.print("     ", .{});
            }

            for (row) |b| {
                try stdout.print("{c}", .{if (std.ascii.isPrint(b)) b else '.'});
            }
            try stdout.print("\n", .{});
        }

        try stdout.print("module name: {s}\n", .{object.module_name});

        try stdout.print("exports:\n", .{});
        for (object.exports, 0..) |@"export", i| {
            try stdout.print("  {: >8}: {}\n", .{ i, @"export" });
        }

        try stdout.print("imports:\n", .{});
        for (object.imports, 0..) |mod_imports, i| {
            try stdout.print("  {: >8}:\n", .{i});
            for (mod_imports, 0..) |import, j| {
                try stdout.print("    {: >8}: {}\n", .{ j, import });
            }
        }

        try stdout.print("handlers:\n", .{});
        for (object.handlers, 0..) |handler, i| {
            try stdout.print("  {: >8}: {}:\n", .{ i, handler });
        }
    }
    try stdout_buf.flush();
}
