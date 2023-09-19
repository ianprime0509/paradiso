const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const paradiso = b.addModule("paradiso", .{
        .source_file = .{ .path = "src/paradiso.zig" },
    });

    const disdump = b.addExecutable(.{
        .name = "disdump",
        .target = target,
        .optimize = optimize,
        .root_source_file = .{ .path = "tools/disdump.zig" },
    });
    disdump.addModule("paradiso", paradiso);
    b.installArtifact(disdump);

    const run_disdump = b.addRunArtifact(disdump);
    if (b.args) |args| {
        run_disdump.addArgs(args);
    }
    b.step("disdump", "Run disdump").dependOn(&run_disdump.step);
}
