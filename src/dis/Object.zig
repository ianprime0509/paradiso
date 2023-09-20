const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const cast = std.math.cast;

flags: Flags,
stack_extent: u32,
entry_pc: u32,
entry_type: u32,
instructions: []const Instruction,
type_descriptors: []const TypeDescriptor,
data: []const u8,
module_name: []const u8,
exports: []const Export,
imports: []const []const Import,
handlers: []const Handler,

const Object = @This();

pub const xmagic = 819248;
pub const smagic = 923426;

/// Arbitrary limit on the length of a name.
const max_name_len = 1024;

pub const Flags = packed struct {
    mustcompile: bool,
    dontcompile: bool,
    sharemp: bool,
    dynmod: bool,
    hasldt0: bool,
    hasexcept: bool,
    hasldt: bool,
};

pub const Instruction = struct {
    opcode: Opcode,
    source: Operand,
    middle: MiddleOperand,
    dest: Operand,

    pub const Opcode = enum(u8) {
        nop = 0x00,
        alt = 0x01,
        nbalt = 0x02,
        goto = 0x03,
        call = 0x04,
        frame = 0x05,
        spawn = 0x06,
        runt = 0x07,
        load = 0x08,
        mcall = 0x09,
        mspawn = 0x0A,
        mframe = 0x0B,
        ret = 0x0C,
        jmp = 0x0D,
        case = 0x0E,
        exit = 0x0F,
        new = 0x10,
        newa = 0x11,
        newcb = 0x12,
        newcw = 0x13,
        newcf = 0x14,
        newcp = 0x15,
        newcm = 0x16,
        newcmp = 0x17,
        send = 0x18,
        recv = 0x19,
        consb = 0x1A,
        consw = 0x1B,
        consp = 0x1C,
        consf = 0x1D,
        consm = 0x1E,
        consmp = 0x1F,
        headb = 0x20,
        headw = 0x21,
        headp = 0x22,
        headf = 0x23,
        headm = 0x24,
        headmp = 0x25,
        tail = 0x26,
        lea = 0x27,
        indx = 0x28,
        movp = 0x29,
        movm = 0x2A,
        movmp = 0x2B,
        movb = 0x2C,
        movw = 0x2D,
        movf = 0x2E,
        cvtbw = 0x2F,
        cvtwb = 0x30,
        cvtfw = 0x31,
        cvtwf = 0x32,
        cvtca = 0x33,
        cvtac = 0x34,
        cvtwc = 0x35,
        cvtcw = 0x36,
        cvtfc = 0x37,
        cvtcf = 0x38,
        addb = 0x39,
        addw = 0x3A,
        addf = 0x3B,
        subb = 0x3C,
        subw = 0x3D,
        subf = 0x3E,
        mulb = 0x3F,
        mulw = 0x40,
        mulf = 0x41,
        divb = 0x42,
        divw = 0x43,
        divf = 0x44,
        modw = 0x45,
        modb = 0x46,
        andb = 0x47,
        andw = 0x48,
        orb = 0x49,
        orw = 0x4A,
        xorb = 0x4B,
        xorw = 0x4C,
        shlb = 0x4D,
        shlw = 0x4E,
        shrb = 0x4F,
        shrw = 0x50,
        insc = 0x51,
        indc = 0x52,
        addc = 0x53,
        lenc = 0x54,
        lena = 0x55,
        lenl = 0x56,
        beqb = 0x57,
        bneb = 0x58,
        bltb = 0x59,
        bleb = 0x5A,
        bgtb = 0x5B,
        bgeb = 0x5C,
        beqw = 0x5D,
        bnew = 0x5E,
        bltw = 0x5F,
        blew = 0x60,
        bgtw = 0x61,
        bgew = 0x62,
        beqf = 0x63,
        bnef = 0x64,
        bltf = 0x65,
        blef = 0x66,
        bgtf = 0x67,
        bgef = 0x68,
        beqc = 0x69,
        bnec = 0x6A,
        bltc = 0x6B,
        blec = 0x6C,
        bgtc = 0x6D,
        bgec = 0x6E,
        slicea = 0x6F,
        slicela = 0x70,
        slicec = 0x71,
        indw = 0x72,
        indf = 0x73,
        indb = 0x74,
        negf = 0x75,
        movl = 0x76,
        addl = 0x77,
        subl = 0x78,
        divl = 0x79,
        modl = 0x7A,
        mull = 0x7B,
        andl = 0x7C,
        orl = 0x7D,
        xorl = 0x7E,
        shll = 0x7F,
        shrl = 0x80,
        bnel = 0x81,
        bltl = 0x82,
        blel = 0x83,
        bgtl = 0x84,
        bgel = 0x85,
        beql = 0x86,
        cvtlf = 0x87,
        cvtfl = 0x88,
        cvtlw = 0x89,
        cvtwl = 0x8A,
        cvtlc = 0x8B,
        cvtcl = 0x8C,
        headl = 0x8D,
        consl = 0x8E,
        newcl = 0x8F,
        casec = 0x90,
        indl = 0x91,
        movpc = 0x92,
        tcmp = 0x93,
        mnewz = 0x94,
        cvtrf = 0x95,
        cvtfr = 0x96,
        cvtws = 0x97,
        cvtsw = 0x98,
        lsrw = 0x99,
        lsrl = 0x9A,
        eclr = 0x9B,
        newz = 0x9C,
        newaz = 0x9D,
        // Not documented in the 4th edition Dis VM specification after this point
        raise = 0x9E,
        casel = 0x9F,
        mulx = 0xA0,
        divx = 0xA1,
        cvtxx = 0xA2,
        mulx0 = 0xA3,
        divx0 = 0xA4,
        cvtxx0 = 0xA5,
        mulx1 = 0xA6,
        divx1 = 0xA7,
        cvtxx1 = 0xA8,
        cvtfx = 0xA9,
        cvtxf = 0xAA,
        expw = 0xAB,
        expl = 0xAC,
        expf = 0xAD,
        self = 0xAE,
    };

    const AddressMode = packed struct {
        dest: Operand.Type,
        source: Operand.Type,
        middle: MiddleOperand.Type,
    };

    pub const Operand = union(enum) {
        none,
        immediate: i30,
        mp_indirect: i30,
        fp_indirect: i30,
        mp_double_indirect: DoubleIndirect,
        fp_double_indirect: DoubleIndirect,

        pub const Type = enum(u3) {
            mp_indirect = 0b000,
            fp_indirect = 0b001,
            immediate = 0b010,
            none = 0b011,
            mp_double_indirect = 0b100,
            fp_double_indirect = 0b101,
            reserved1 = 0b110,
            reserved2 = 0b111,
        };

        pub const DoubleIndirect = struct {
            register_offset: u16,
            final_offset: u16,
        };

        pub fn read(reader: anytype, @"type": Type) !Operand {
            return switch (@"type") {
                inline .mp_indirect, .fp_indirect, .immediate => |tag| @unionInit(Operand, @tagName(tag), try readOp(reader)),
                .none => .none,
                inline .mp_double_indirect, .fp_double_indirect => |tag| @unionInit(Operand, @tagName(tag), .{
                    .register_offset = cast(u16, try readOp(reader)) orelse return error.InvalidInstruction,
                    .final_offset = cast(u16, try readOp(reader)) orelse return error.InvalidInstruction,
                }),
                .reserved1, .reserved2 => error.InvalidInstruction,
            };
        }

        pub fn format(operand: Operand, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            switch (operand) {
                .none => try writer.writeAll("<none>"),
                .immediate => |immediate| try writer.print("${}", .{immediate}),
                .mp_indirect => |mp_indirect| try writer.print("{}(mp)", .{mp_indirect}),
                .fp_indirect => |fp_indirect| try writer.print("{}(fp)", .{fp_indirect}),
                .mp_double_indirect => |mp_double_indirect| try writer.print("{}({}(mp))", .{ mp_double_indirect.final_offset, mp_double_indirect.register_offset }),
                .fp_double_indirect => |fp_double_indirect| try writer.print("{}({}(fp))", .{ fp_double_indirect.final_offset, fp_double_indirect.register_offset }),
            }
        }
    };

    pub const MiddleOperand = union(enum) {
        none,
        immediate: i16,
        fp_indirect: u16,
        mp_indirect: u16,

        pub const Type = enum(u2) {
            none = 0b00,
            immediate = 0b01,
            fp_indirect = 0b10,
            mp_indirect = 0b11,
        };

        pub fn read(reader: anytype, @"type": Type) !MiddleOperand {
            return switch (@"type") {
                .none => .none,
                .immediate => .{ .immediate = cast(i16, try readOp(reader)) orelse return error.InvalidInstruction },
                inline .fp_indirect, .mp_indirect => |tag| @unionInit(MiddleOperand, @tagName(tag), cast(u16, try readOp(reader)) orelse return error.InvalidInstruction),
            };
        }

        pub fn format(operand: MiddleOperand, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            switch (operand) {
                .none => try writer.writeAll("<none>"),
                .immediate => |immediate| try writer.print("${}", .{immediate}),
                .mp_indirect => |mp_indirect| try writer.print("{}(mp)", .{mp_indirect}),
                .fp_indirect => |fp_indirect| try writer.print("{}(fp)", .{fp_indirect}),
            }
        }
    };

    pub fn read(reader: anytype) !Instruction {
        const opcode: Opcode = std.meta.intToEnum(Opcode, try reader.readByte()) catch return error.InvalidInstruction;
        const address_mode: AddressMode = @bitCast(try reader.readByte());
        return .{
            .opcode = opcode,
            .middle = try MiddleOperand.read(reader, address_mode.middle),
            .source = try Operand.read(reader, address_mode.source),
            .dest = try Operand.read(reader, address_mode.dest),
        };
    }

    pub fn format(instruction: Instruction, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.writeAll(@tagName(instruction.opcode));
        var needs_comma = false;
        if (instruction.source != .none) {
            try writer.print(" {}", .{instruction.source});
            needs_comma = true;
        }
        if (instruction.middle != .none) {
            if (needs_comma) try writer.writeByte(',');
            try writer.print(" {}", .{instruction.middle});
            needs_comma = true;
        }
        if (instruction.dest != .none) {
            if (needs_comma) try writer.writeByte(',');
            try writer.print(" {}", .{instruction.dest});
        }
    }
};

pub const TypeDescriptor = struct {
    size: u32,
    pointer_map: std.DynamicBitSetUnmanaged,

    pub fn deinit(type_descriptor: *TypeDescriptor, allocator: Allocator) void {
        type_descriptor.pointer_map.deinit(allocator);
        type_descriptor.* = undefined;
    }

    pub fn read(allocator: Allocator, reader: anytype) !TypeDescriptor {
        const size = cast(u32, try readOp(reader)) orelse return error.InvalidTypeDescriptor;
        const map_size = cast(u32, try readOp(reader)) orelse return error.InvalidTypeDescriptor;
        var pointer_map = try std.DynamicBitSetUnmanaged.initEmpty(allocator, map_size * 8);
        errdefer pointer_map.deinit(allocator);
        var i: usize = 0;
        while (i < pointer_map.bit_length) : (i += 8) {
            const b = try reader.readByte();
            comptime var j = 7;
            inline while (j >= 0) : (j -= 1) {
                if (b & (1 << j) != 0) pointer_map.set(i);
            }
        }
        return .{ .size = size, .pointer_map = pointer_map };
    }

    pub fn isPointer(type_descriptor: TypeDescriptor, word: u32) bool {
        return if (word >= type_descriptor.pointer_map.bit_length) false else type_descriptor.pointer_map.isSet(word);
    }

    pub fn format(type_descriptor: TypeDescriptor, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("size = {}, pointers = ", .{type_descriptor.size});
        for (0..(type_descriptor.size / 4)) |i| {
            try writer.print("{s}", .{if (type_descriptor.isPointer(@intCast(i))) "1" else "0"});
        }
    }
};

pub const Export = struct {
    pc: u32,
    stack_frame_type: u32,
    signature: u32,
    name: []const u8,

    pub fn deinit(@"export": *Export, allocator: Allocator) void {
        allocator.free(@"export".name);
        @"export".* = undefined;
    }

    pub fn read(allocator: Allocator, reader: anytype) !Export {
        return .{
            .pc = cast(u32, try readOp(reader)) orelse return error.InvalidExport,
            .stack_frame_type = cast(u32, try readOp(reader)) orelse return error.InvalidExport,
            .signature = try reader.readIntBig(u32),
            .name = try reader.readUntilDelimiterAlloc(allocator, 0, max_name_len),
        };
    }

    pub fn format(@"export": Export, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("{s} (pc = {}, stack_frame_type = {}, signature = {X:8>0})", .{
            @"export".name,
            @"export".pc,
            @"export".stack_frame_type,
            @"export".signature,
        });
    }
};

pub const Import = struct {
    signature: u32,
    name: []const u8,

    pub fn deinit(import: *Import, allocator: Allocator) void {
        allocator.free(import.name);
        import.* = undefined;
    }

    pub fn read(allocator: Allocator, reader: anytype) !Import {
        return .{
            .signature = try reader.readIntBig(u32),
            .name = try reader.readUntilDelimiterAlloc(allocator, 0, max_name_len),
        };
    }

    pub fn format(import: Import, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("{s} (signature = {X:8>0})", .{ import.name, import.signature });
    }
};

pub const Handler = struct {
    offset: u32,
    start_pc: u32,
    end_pc: u32,
    data_type: ?u32,
    exceptions: []const Exception,
    else_pc: ?u32,

    pub fn deinit(handler: *Handler, allocator: Allocator) void {
        for (@constCast(handler.exceptions)) |*exception| exception.deinit(allocator);
        allocator.free(handler.exceptions);
        handler.* = undefined;
    }

    pub fn read(allocator: Allocator, reader: anytype) !Handler {
        const offset = cast(u32, try readOp(reader)) orelse return error.InvalidHandler;
        const start_pc = cast(u32, try readOp(reader)) orelse return error.InvalidHandler;
        const end_pc = cast(u32, try readOp(reader)) orelse return error.InvalidHandler;
        const data_type = data_type: {
            const data_type = try readOp(reader);
            break :data_type if (data_type == -1) null else cast(u32, data_type) orelse return error.InvalidHandler;
        };
        var exceptions = std.ArrayListUnmanaged(Exception){};
        errdefer exceptions.deinit(allocator);
        errdefer for (exceptions.items) |*exception| exception.deinit(allocator);
        const n_exceptions = cast(u32, try readOp(reader)) orelse return error.InvalidHandler;
        try exceptions.ensureTotalCapacityPrecise(allocator, n_exceptions);
        for (0..n_exceptions) |_| {
            exceptions.appendAssumeCapacity(try Exception.read(allocator, reader));
        }
        const else_pc = else_pc: {
            const else_pc = try readOp(reader);
            break :else_pc if (else_pc == -1) null else cast(u32, else_pc) orelse return error.InvalidHandler;
        };
        return .{
            .offset = offset,
            .start_pc = start_pc,
            .end_pc = end_pc,
            .data_type = data_type,
            .exceptions = try exceptions.toOwnedSlice(allocator),
            .else_pc = else_pc,
        };
    }

    pub fn format(handler: Handler, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("{: >8} .. {: >8} (offset = {}, data_type = {?}):", .{
            handler.start_pc,
            handler.end_pc,
            handler.offset,
            handler.data_type,
        });
        var needs_comma = false;
        for (handler.exceptions) |exception| {
            if (needs_comma) try writer.writeByte(',');
            try writer.print(" {}", .{exception});
            needs_comma = true;
        }
        if (handler.else_pc) |else_pc| {
            if (needs_comma) try writer.writeByte(',');
            try writer.print(" * => {}", .{else_pc});
        }
    }
};

pub const Exception = struct {
    name: []const u8,
    pc: u32,

    pub fn deinit(exception: *Exception, allocator: Allocator) void {
        allocator.free(exception.name);
        exception.* = undefined;
    }

    pub fn read(allocator: Allocator, reader: anytype) !Exception {
        return .{
            .name = try reader.readUntilDelimiterAlloc(allocator, 0, max_name_len),
            .pc = cast(u32, try readOp(reader)) orelse return error.InvalidHandler,
        };
    }

    pub fn format(exception: Exception, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("{s} => {}", .{ exception.name, exception.pc });
    }
};

pub fn deinit(object: *Object, allocator: Allocator) void {
    allocator.free(object.instructions);
    for (@constCast(object.type_descriptors)) |*type_descriptor| type_descriptor.deinit(allocator);
    allocator.free(object.type_descriptors);
    allocator.free(object.data);
    allocator.free(object.module_name);
    for (@constCast(object.exports)) |*@"export"| @"export".deinit(allocator);
    allocator.free(object.exports);
    for (@constCast(object.imports)) |*mod_imports| {
        for (@constCast(mod_imports.*)) |*import| import.deinit(allocator);
        allocator.free(mod_imports.*);
    }
    allocator.free(object.imports);
    for (@constCast(object.handlers)) |*handler| handler.deinit(allocator);
    allocator.free(object.handlers);
}

pub fn read(allocator: Allocator, reader: anytype) !Object {
    const magic = try readOp(reader);

    switch (magic) {
        xmagic => {},
        smagic => {
            const signature_length = try readOp(reader);
            if (signature_length < 0) return error.InvalidObject;
            try reader.skipBytes(@intCast(signature_length), .{});
        },
        else => return error.InvalidObject,
    }

    const flags: Flags = @bitCast(@as(u7, @truncate(@as(u30, @bitCast(try readOp(reader))))));

    const stack_extent = cast(u32, try readOp(reader)) orelse return error.InvalidObject;

    const code_size = cast(u32, try readOp(reader)) orelse return error.InvalidObject;
    const data_size = cast(u32, try readOp(reader)) orelse return error.InvalidObject;
    const type_size = cast(u32, try readOp(reader)) orelse return error.InvalidObject;
    const link_size = cast(u32, try readOp(reader)) orelse return error.InvalidObject;

    const entry_pc = cast(u32, try readOp(reader)) orelse return error.InvalidObject;
    const entry_type = cast(u32, try readOp(reader)) orelse return error.InvalidObject;

    var instructions = try allocator.alloc(Instruction, code_size);
    errdefer allocator.free(instructions);
    for (instructions) |*instruction| {
        instruction.* = try Instruction.read(reader);
    }

    var type_descriptors = try allocator.alloc(TypeDescriptor, type_size);
    errdefer allocator.free(type_descriptors);
    // The documentation doesn't mention this anywhere as far as I can tell, but
    // libinterp's load.c assumes that the type descriptor number is actually
    // meant to be interpreted as an index into the type descriptor array.
    const invalid_type_descriptor_size: u32 = 0xFFFF_FFFF;
    @memset(type_descriptors, .{ .size = invalid_type_descriptor_size, .pointer_map = .{} });
    errdefer for (type_descriptors) |*desc| desc.deinit(allocator);
    for (0..type_size) |_| {
        const idx = cast(u32, try readOp(reader)) orelse return error.InvalidTypeDescriptor;
        if (idx >= type_size or type_descriptors[idx].size != invalid_type_descriptor_size) return error.InvalidTypeDescriptor;
        type_descriptors[idx] = try TypeDescriptor.read(allocator, reader);
    }

    var data = std.ArrayListUnmanaged(u8){};
    errdefer data.deinit(allocator);
    try data.appendNTimes(allocator, 0, data_size);
    try readDataSection(allocator, reader, &data, type_descriptors);

    const module_name = try reader.readUntilDelimiterAlloc(allocator, 0, max_name_len);
    errdefer allocator.free(module_name);

    var exports = std.ArrayListUnmanaged(Export){};
    errdefer exports.deinit(allocator);
    errdefer for (exports.items) |*@"export"| @"export".deinit(allocator);
    try exports.ensureTotalCapacityPrecise(allocator, link_size);
    for (0..link_size) |_| {
        exports.appendAssumeCapacity(try Export.read(allocator, reader));
    }

    if (flags.hasldt0) return error.UnsupportedObject;
    var imports = std.ArrayListUnmanaged([]Import){};
    errdefer imports.deinit(allocator);
    errdefer for (imports.items) |*mod_imports| {
        for (mod_imports.*) |*import| import.deinit(allocator);
        allocator.free(mod_imports.*);
    };
    if (flags.hasldt) {
        const n_mods = cast(u32, try readOp(reader)) orelse return error.InvalidImport;
        try imports.ensureTotalCapacityPrecise(allocator, n_mods);
        for (0..n_mods) |_| {
            const n_imports = cast(u32, try readOp(reader)) orelse return error.InvalidImport;
            var mod_imports = std.ArrayListUnmanaged(Import){};
            errdefer mod_imports.deinit(allocator);
            errdefer for (mod_imports.items) |*import| import.deinit(allocator);
            try mod_imports.ensureTotalCapacityPrecise(allocator, n_imports);
            for (0..n_imports) |_| {
                mod_imports.appendAssumeCapacity(try Import.read(allocator, reader));
            }
            imports.appendAssumeCapacity(try mod_imports.toOwnedSlice(allocator));
        }
    }

    var handlers = std.ArrayListUnmanaged(Handler){};
    errdefer handlers.deinit(allocator);
    errdefer for (handlers.items) |*handler| handler.deinit(allocator);
    if (flags.hasexcept) {
        const n_handlers = cast(u32, try readOp(reader)) orelse return error.InvalidHandler;
        try handlers.ensureTotalCapacityPrecise(allocator, n_handlers);
        for (0..n_handlers) |_| {
            handlers.appendAssumeCapacity(try Handler.read(allocator, reader));
        }
    }

    return .{
        .flags = flags,
        .stack_extent = stack_extent,
        .entry_pc = entry_pc,
        .entry_type = entry_type,
        .instructions = instructions,
        .type_descriptors = type_descriptors,
        .data = try data.toOwnedSlice(allocator),
        .module_name = module_name,
        .exports = try exports.toOwnedSlice(allocator),
        .imports = try imports.toOwnedSlice(allocator),
        .handlers = try handlers.toOwnedSlice(allocator),
    };
}

fn readOp(reader: anytype) !i30 {
    const b: packed struct { value: u6, det: u2 } = @bitCast(try reader.readByte());
    switch (b.det) {
        0b00 => return b.value,
        0b01 => return -@as(i7, b.value),
        0b10 => {
            const rest = try reader.readByte();
            return (@as(i14, b.value) << 8) | rest;
        },
        0b11 => {
            const rest = try reader.readIntBig(u24);
            return (@as(i30, b.value) << 24) | rest;
        },
    }
}

fn readDataSection(
    allocator: Allocator,
    reader: anytype,
    data: *std.ArrayListUnmanaged(u8),
    type_descriptors: []const TypeDescriptor,
) !void {
    const ItemType = enum(u4) {
        bytes = 0b0001,
        words = 0b0010,
        string = 0b0011,
        real = 0b0100,
        array = 0b0101,
        set_address = 0b0110,
        restore_address = 0b0111,
        big = 0b1000,
    };
    const ArrayDesc = struct {
        addr: u32,
        item_size: u32,
    };

    var idata = std.ArrayListUnmanaged(u32){};
    defer idata.deinit(allocator);
    try idata.append(allocator, 0);
    var array_descs = std.ArrayListUnmanaged(ArrayDesc){};
    defer array_descs.deinit(allocator);
    while (true) {
        var b = try reader.readByte();
        if (b == 0) return;
        const code: packed struct { short_count: u4, type: u4 } = @bitCast(b);
        const @"type" = std.meta.intToEnum(ItemType, code.type) catch return error.InvalidDataItem;
        const count = if (code.short_count == 0)
            cast(u32, try readOp(reader)) orelse return error.InvalidDataItem
        else
            code.short_count;
        const offset = try readOp(reader);
        const addr = idata.getLast() +% @as(u32, @bitCast(@as(i32, offset)));
        if (addr >= data.items.len) return error.InvalidDataItem;
        const offset_data = data.items[addr..];
        switch (@"type") {
            .bytes => {
                if (count > offset_data.len) return error.InvalidDataItem;
                try reader.readNoEof(offset_data[0..count]);
            },
            .words => {
                if (count * 4 > offset_data.len) return error.InvalidDataItem;
                for (0..count) |i| {
                    std.mem.writeIntLittle(u32, offset_data[i * 4 ..][0..4], try reader.readIntBig(u32));
                }
            },
            .string => {
                if (offset_data.len < 4) return error.InvalidDataItem;
                const sdata_start = std.mem.alignForward(usize, data.items.len, 4);
                const sdata_start_padding = sdata_start - data.items.len;
                std.mem.writeIntLittle(u32, offset_data[0..4], @intCast(sdata_start));
                // offset_data cannot be used after this point, as data.items
                // may be moved during resize

                const utf8 = try allocator.alloc(u8, count);
                defer allocator.free(utf8);
                try reader.readNoEof(utf8);
                const utf16_len = try std.unicode.calcUtf16LeLen(utf8);
                try data.ensureUnusedCapacity(allocator, sdata_start_padding + 4 + 2 * utf16_len);
                data.items.len += sdata_start_padding + 4 + 2 * utf16_len;
                const sdata = data.items[sdata_start..];
                mem.writeIntBig(u32, sdata[0..4], @intCast(utf16_len));
                const str_contents: []u16 = @alignCast(mem.bytesAsSlice(u16, sdata[4..]));
                _ = std.unicode.utf8ToUtf16Le(str_contents, utf8) catch unreachable;
            },
            .real => {
                if (count * 8 > offset_data.len) return error.InvalidDataItem;
                for (0..count) |i| {
                    // TODO: confirm endianness of f64s
                    std.mem.writeIntLittle(u64, offset_data[i * 8 ..][0..8], try reader.readIntBig(u64));
                }
            },
            .array => {
                if (offset_data.len < 4) return error.InvalidDataItem;
                const adata_start = std.mem.alignForward(usize, data.items.len, 4);
                const adata_start_padding = adata_start - data.items.len;
                std.mem.writeIntLittle(u32, offset_data[0..4], @intCast(adata_start));
                // offset_data cannot be used after this point, as data.items
                // may be moved during resize

                const atype = try reader.readIntBig(u32);
                if (atype >= type_descriptors.len) return error.InvalidDataItem;
                const adesc = type_descriptors[atype];
                const alen = try reader.readIntBig(u32);
                try data.ensureUnusedCapacity(allocator, adata_start_padding + 4 + adesc.size * alen);
                data.items.len += adata_start_padding + 4 + adesc.size * alen;
                const adata = data.items[adata_start..];
                mem.writeIntBig(u32, adata[0..4], alen);
                @memset(adata[4..], 0);
                try array_descs.append(allocator, .{ .addr = addr, .item_size = adesc.size });
            },
            .set_address => {
                const idx = try reader.readIntBig(u32);
                const adesc = array_descs.getLastOrNull() orelse return error.InvalidDataItem;
                try idata.append(allocator, adesc.addr + idx * adesc.item_size);
            },
            .restore_address => {
                if (idata.items.len == 1 or array_descs.items.len == 0) return error.InvalidDataItem;
                idata.items.len -= 1;
                array_descs.items.len -= 1;
            },
            .big => {
                if (count * 8 > offset_data.len) return error.InvalidDataItem;
                for (0..count) |i| {
                    std.mem.writeIntLittle(u64, offset_data[i * 8 ..][0..8], try reader.readIntBig(u64));
                }
            },
        }
    }
}
