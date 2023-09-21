const std = @import("std");
const c = @cImport(@cInclude("binaryen-c.h"));

pub fn main() !void {
    const module = c.BinaryenModuleCreate();
    defer c.BinaryenModuleDispose(module);

    var ii = [_]c.BinaryenType{ c.BinaryenTypeInt32(), c.BinaryenTypeInt32() };
    const params = c.BinaryenTypeCreate(&ii, 2);
    const results = c.BinaryenTypeInt32();

    const x = c.BinaryenLocalGet(module, 0, c.BinaryenTypeInt32());
    const y = c.BinaryenLocalGet(module, 1, c.BinaryenTypeInt32());
    const add = c.BinaryenBinary(module, c.BinaryenAddInt32(), x, y);

    _ = c.BinaryenAddFunction(module, "adder", params, results, null, 0, add);

    c.BinaryenModulePrint(module);
}
