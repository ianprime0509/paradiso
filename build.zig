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

    const binaryen = b.dependency("binaryen", .{}).artifact("binaryen");

    const wasmtest = b.addExecutable(.{
        .name = "wasmtest",
        .target = target,
        .optimize = optimize,
        .root_source_file = .{ .path = "tools/wasmtest.zig" },
    });
    wasmtest.linkLibC();
    wasmtest.linkLibrary(binaryen);
    // wasmtest.addLibraryPath(.{ .cwd_relative = "../binaryen/build/lib/" });
    // wasmtest.linkSystemLibrary("binaryen");
    // TODO: improve with https://github.com/ziglang/zig/pull/16667
    wasmtest.addIncludePath(.{ .path = "../binaryen/src/" });
    b.installArtifact(wasmtest);

    const run_wasmtest = b.addRunArtifact(wasmtest);
    if (b.args) |args| {
        run_wasmtest.addArgs(args);
    }
    b.step("wasmtest", "Run wasmtest").dependOn(&run_wasmtest.step);
}
