const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const paradiso = b.addModule("paradiso", .{
        .source_file = .{ .path = "src/paradiso.zig" },
    });

    const tests = b.addTest(.{
        .target = target,
        .optimize = optimize,
        .root_source_file = .{ .path = "src/paradiso.zig" },
    });
    const run_tests = b.addRunArtifact(tests);
    b.step("test", "Run tests").dependOn(&run_tests.step);

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

    const binaryen = b.dependency("binaryen", .{});

    const limbo = b.addExecutable(.{
        .name = "limbo",
        .target = target,
        .optimize = optimize,
        .root_source_file = .{ .path = "tools/limbo.zig" },
    });
    limbo.linkLibC();
    limbo.linkLibrary(binaryen.artifact("binaryen"));
    limbo.addIncludePath(binaryen.path("src"));
    b.installArtifact(limbo);

    const run_limbo = b.addRunArtifact(limbo);
    if (b.args) |args| {
        run_limbo.addArgs(args);
    }
    b.step("limbo", "Run limbo").dependOn(&run_limbo.step);
}
