const std = @import("std");
const paradiso = @import("paradiso");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    for (args[1..]) |arg| {
        std.debug.print("{s}:\n\n", .{arg});

        var file = try std.fs.cwd().openFile(arg, .{});
        defer file.close();
        var br = std.io.bufferedReader(file.reader());
        var object = try paradiso.dis.Object.read(allocator, br.reader());
        defer object.deinit(allocator);

        std.debug.print("flags: {}\n", .{object.flags});
        std.debug.print("stack extent: {}\n", .{object.stack_extent});
        std.debug.print("entry pc: {}\n", .{object.entry_pc});
        std.debug.print("entry type: {}\n", .{object.entry_type});

        std.debug.print("instructions:\n", .{});
        for (object.instructions, 0..) |instruction, i| {
            std.debug.print("  {: >8}: {}\n", .{ i, instruction });
        }

        std.debug.print("type descriptors:\n", .{});
        for (object.type_descriptors, 0..) |type_descriptor, i| {
            std.debug.print("  {: >8}: {}\n", .{ i, type_descriptor });
        }

        std.debug.print("data: {}\n", .{std.fmt.fmtSliceHexUpper(object.data)});

        std.debug.print("module name: {s}\n", .{object.module_name});

        std.debug.print("exports:\n", .{});
        for (object.exports, 0..) |@"export", i| {
            std.debug.print("  {: >8}: {}\n", .{ i, @"export" });
        }

        std.debug.print("imports:\n", .{});
        for (object.imports, 0..) |mod_imports, i| {
            std.debug.print("  {: >8}:\n", .{i});
            for (mod_imports, 0..) |import, j| {
                std.debug.print("    {: >8}: {}\n", .{ j, import });
            }
        }

        std.debug.print("handlers:\n", .{});
        for (object.handlers, 0..) |handler, i| {
            std.debug.print("  {: >8}: {}:\n", .{ i, handler });
        }
    }
}
