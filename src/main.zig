const std = @import("std");

const ParsedJson = std.json.Parsed;
const JsonParseError = std.json.ParseError;
const JsonParseFromValueError = std.json.ParseFromValueError;
const DynamicJsonValue = std.json.Value;

// TODO: Implementar funções e chamadas.
// Para implementar funções precisaremos de duas coisas:
// Contextos terão que poder ser pilhas de contextos.
// Além disso, funções podem capturar variáveis locais referenciadas nelas.
// Para isso, precisaremos que funções possam carregar um contexto próprio incluindo o contexto delas.
// Isso será representado pela struct RinhaClosure. A parte importante disso é que, na hora em que uma closure
// sair de escopo (como determinar isso?) o contexto local dela deverá ser desalocado.
// Talvez inicialmente seja melhor deixar closures "vazarem" e serem desalocadas com a arena inteira.
// Depois posso determinar uma estratégia mais viável pra lidar com elas.

const LocationMetadata = struct {
    start: u64,
    end: u64,
    filename: []const u8,
};

const ParameterMetadata = struct {
    text: []const u8,
    location: LocationMetadata,
};

const FileNode = struct {
    name: []const u8,
    expression: *TermNode,
    location: LocationMetadata,
};

const IntLiteralNode = struct {
    value: i64,
    location: LocationMetadata,
};

const StringLiteralNode = struct {
    value: []const u8,
    location: LocationMetadata,
};

const BooleanLiteralNode = struct {
    value: bool,
    location: LocationMetadata,
};

const TupleNode = struct {
    first: *TermNode,
    second: *TermNode,
    location: LocationMetadata,
};

const VarNode = struct {
    text: []const u8,
    location: LocationMetadata,
};

const LetNode = struct {
    name: ParameterMetadata,
    value: *TermNode,
    next: *TermNode,
    location: LocationMetadata,
};

const IfNode = struct {
    condition: *TermNode,
    then: *TermNode,
    otherwise: *TermNode,
    location: LocationMetadata,
};

const FunctionNode = struct {
    parameters: []ParameterMetadata,
    value: *TermNode,
    location: LocationMetadata,
};

const CallNode = struct {
    callee: *TermNode,
    arguments: []TermNode,
    location: LocationMetadata,
};

const BinaryOpTag = enum {
    add_binop,
    sub_binop,
    mul_binop,
    div_binop,
    rem_binop,
    eq_binop,
    neq_binop,
    lt_binop,
    gt_binop,
    lte_binop,
    gte_binop,
    and_binop,
    or_binop,

    pub fn jsonParse(
        allocator: std.mem.Allocator,
        source: anytype,
        options: std.json.ParseOptions,
    ) JsonParseError(@TypeOf(source.*))!@This() {
        var parsed = try std.json.innerParse(DynamicJsonValue, allocator, source, options);
        return jsonParseFromValue(allocator, parsed, options);
    }

    pub fn jsonParseFromValue(
        _: std.mem.Allocator,
        source: DynamicJsonValue,
        _: std.json.ParseOptions,
    ) JsonParseFromValueError!@This() {
        switch (source) {
            DynamicJsonValue.string => |op_string| {
                return parseOpString(op_string) orelse JsonParseFromValueError.InvalidEnumTag;
            },
            else => return JsonParseFromValueError.InvalidEnumTag,
        }
    }

    fn parseOpString(op_string: []const u8) ?@This() {
        if (std.mem.eql(u8, op_string, "Add")) return @This().add_binop;
        if (std.mem.eql(u8, op_string, "Sub")) return @This().sub_binop;
        if (std.mem.eql(u8, op_string, "Mul")) return @This().mul_binop;
        if (std.mem.eql(u8, op_string, "Div")) return @This().div_binop;
        if (std.mem.eql(u8, op_string, "Rem")) return @This().rem_binop;
        if (std.mem.eql(u8, op_string, "Eq")) return @This().eq_binop;
        if (std.mem.eql(u8, op_string, "Neq")) return @This().neq_binop;
        if (std.mem.eql(u8, op_string, "Lt")) return @This().lt_binop;
        if (std.mem.eql(u8, op_string, "Gt")) return @This().gt_binop;
        if (std.mem.eql(u8, op_string, "Lte")) return @This().lte_binop;
        if (std.mem.eql(u8, op_string, "Gte")) return @This().gte_binop;
        if (std.mem.eql(u8, op_string, "And")) return @This().and_binop;
        if (std.mem.eql(u8, op_string, "Or")) return @This().or_binop;
        return null;
    }
};

const BinaryOpNode = struct {
    op: BinaryOpTag,
    lhs: *TermNode,
    rhs: *TermNode,
    location: LocationMetadata,
};

const FirstProjNode = struct {
    value: *TermNode,
    location: LocationMetadata,
};

const SecondProjNode = struct {
    value: *TermNode,
    location: LocationMetadata,
};

const PrintNode = struct {
    value: *TermNode,
    location: LocationMetadata,
};

const TermNodeTag = enum { int_literal_node, string_literal_node, boolean_literal_node, tuple_node, var_node, let_node, if_node, function_node, call_node, binary_op_node, first_proj_node, second_proj_node, print_node };

const TermNode = union(TermNodeTag) {
    int_literal_node: *IntLiteralNode,
    string_literal_node: *StringLiteralNode,
    boolean_literal_node: *BooleanLiteralNode,
    tuple_node: *TupleNode,
    var_node: *VarNode,
    let_node: *LetNode,
    if_node: *IfNode,
    function_node: *FunctionNode,
    call_node: *CallNode,
    binary_op_node: *BinaryOpNode,
    first_proj_node: *FirstProjNode,
    second_proj_node: *SecondProjNode,
    print_node: *PrintNode,

    pub fn jsonParse(
        allocator: std.mem.Allocator,
        source: anytype,
        options: std.json.ParseOptions,
    ) JsonParseError(@TypeOf(source.*))!@This() {
        var parsed = try std.json.innerParse(DynamicJsonValue, allocator, source, options);
        return jsonParseFromValue(allocator, parsed, options);
    }

    pub fn jsonParseFromValue(
        allocator: std.mem.Allocator,
        source: DynamicJsonValue,
        options: std.json.ParseOptions,
    ) JsonParseFromValueError!@This() {
        switch (source) {
            DynamicJsonValue.object => |object_map| {
                const maybe_kind_string = object_map.get("kind");
                if (maybe_kind_string) |kind_string| {
                    switch (kind_string) {
                        DynamicJsonValue.string => |valid_string| {
                            return parseByKindString(allocator, source, options, valid_string);
                        },
                        else => return JsonParseFromValueError.MissingField,
                    }
                } else {
                    return JsonParseFromValueError.MissingField;
                }
            },
            else => return JsonParseFromValueError.MissingField,
        }
    }

    fn parseByKindString(allocator: std.mem.Allocator, source: DynamicJsonValue, options: std.json.ParseOptions, kind_string: []const u8) JsonParseFromValueError!@This() {
        if (std.mem.eql(u8, kind_string, "Var")) {
            return parseInnerNode(VarNode, "var_node", allocator, source, options);
        } else if (std.mem.eql(u8, kind_string, "Int")) {
            return parseInnerNode(IntLiteralNode, "int_literal_node", allocator, source, options);
        } else if (std.mem.eql(u8, kind_string, "Str")) {
            return parseInnerNode(StringLiteralNode, "string_literal_node", allocator, source, options);            
        } else if (std.mem.eql(u8, kind_string, "Bool")) {
            return parseInnerNode(BooleanLiteralNode, "boolean_literal_node", allocator, source, options);
        } else if (std.mem.eql(u8, kind_string, "Tuple")) {
            return parseInnerNode(TupleNode, "tuple_node", allocator, source, options);
        } else if (std.mem.eql(u8, kind_string, "Let")) {
            return parseInnerNode(LetNode, "let_node", allocator, source, options);
        } else if (std.mem.eql(u8, kind_string, "If")) {
            return parseInnerNode(IfNode, "if_node", allocator, source, options);
        } else if (std.mem.eql(u8, kind_string, "Function")) {
            return parseInnerNode(FunctionNode, "function_node", allocator, source, options);
        } else if (std.mem.eql(u8, kind_string, "Call")) {
            return parseInnerNode(CallNode, "call_node", allocator, source, options);
        } else if (std.mem.eql(u8, kind_string, "Binary")) {
            return parseInnerNode(BinaryOpNode, "binary_op_node", allocator, source, options);
        } else if (std.mem.eql(u8, kind_string, "First")) {
            return parseInnerNode(FirstProjNode, "first_proj_node", allocator, source, options);
        } else if (std.mem.eql(u8, kind_string, "Second")) {
            return parseInnerNode(SecondProjNode, "second_proj_node", allocator, source, options);
        } else if (std.mem.eql(u8, kind_string, "Print")) {
            return parseInnerNode(PrintNode, "print_node", allocator, source, options);
        } else {
            unreachable;
        }
    }

    fn parseInnerNode(comptime T: type, comptime field_name: []const u8, allocator: std.mem.Allocator, source: DynamicJsonValue, options: std.json.ParseOptions) JsonParseFromValueError!@This() {        
        var inner_node = try allocator.create(T);
        inner_node.* = try std.json.innerParseFromValue(T, allocator, source, options);

        return @unionInit(TermNode, field_name, inner_node);
    }
};

const EvalError = std.fs.File.WriteError || std.mem.Allocator.Error || error {
    ReferenceError,
    TypeError,
    ArgumentError,
    NotImplemented
};

const RinhaValueTag = enum {
    none,
    rec,
    int,
    boolean,
    string,
    tuple,
    closure,
};

const RinhaTuple = struct {
    first: RinhaValue,
    second: RinhaValue,
};

const RinhaClosure = struct {
    context: *ContextMap,
    args: []ParameterMetadata,
    value: *TermNode,
};

const RinhaValue = union(RinhaValueTag) {
    none: void,
    rec: []const u8,
    int: i64,
    boolean: bool,
    string: []const u8,
    tuple: *RinhaTuple,
    closure: RinhaClosure,

    pub fn testEquality(self: @This(), other: @This()) bool {
        return switch (self) {
            RinhaValueTag.none => false,
            RinhaValueTag.rec => |lhs| switch (other) {
                RinhaValueTag.rec => |rhs| std.mem.eql(u8, lhs, rhs),
                else => false,
            },
            RinhaValueTag.int => |lhs| switch (other) {
                RinhaValueTag.int => |rhs| lhs == rhs,
                else => false,
            },
            RinhaValueTag.boolean => |lhs| switch (other) {
                RinhaValueTag.boolean => |rhs| lhs == rhs,
                else => false,                
            },
            RinhaValueTag.string => |lhs| switch (other) {
                RinhaValueTag.string => |rhs| std.mem.eql(u8, lhs, rhs),
                else => false,                
            },
            RinhaValueTag.tuple => |lhs| switch (other) {
                RinhaValueTag.tuple => |rhs| testEquality(lhs.first, rhs.first) and testEquality(lhs.second, rhs.second),
                else => false,
            },
            RinhaValueTag.closure => false,
        };
    }

    pub fn extractInt(self: @This()) ?i64 {
        return switch (self) {
            RinhaValueTag.int => |value| value,
            else => null,
        };
    }

    pub fn performAddition(self: @This(), allocator: std.mem.Allocator, other: @This()) std.mem.Allocator.Error!?@This() {
        return switch (self) {
            RinhaValueTag.int => |lhs| switch (other) {
                RinhaValueTag.int => |rhs| @This(){ .int = lhs + rhs },
                RinhaValueTag.string => |rhs| @This(){ .string = try std.fmt.allocPrint(allocator, "{}{s}", .{lhs, rhs}) },
                else => null,
            },
            RinhaValueTag.string => |lhs| switch (other) {
                RinhaValueTag.int => |rhs| @This(){ .string = try std.fmt.allocPrint(allocator, "{s}{}", .{lhs, rhs}) },
                RinhaValueTag.string => |rhs| @This(){ .string = try std.fmt.allocPrint(allocator, "{s}{s}", .{lhs, rhs}) },
                else => null,
            },
            else => null,
        };
    }

    pub fn print(self: @This()) !void {
        const stdout = std.io.getStdOut().writer();
        switch (self) {
            RinhaValueTag.none => {},
            RinhaValueTag.rec => {},
            RinhaValueTag.int => |int_value| {
                try stdout.print("{}", .{int_value});
            },
            RinhaValueTag.string => |string_value| {
                try stdout.print("{s}", .{string_value});
            },
            RinhaValueTag.boolean => |bool_value| {
                try stdout.print("{}", .{bool_value});
            },
            RinhaValueTag.tuple => |tuple_value| {
                try stdout.print("( ", .{});
                try tuple_value.first.print();
                try stdout.print(", ", .{});
                try tuple_value.second.print();
                try stdout.print(" )", .{});
            },
            RinhaValueTag.closure => {
                try stdout.print("<#closure>", .{});
            },
        }
    }

    pub fn printNewline(self: @This()) !void {
        try self.print();
        const stdout = std.io.getStdOut().writer();
        try stdout.print("\n", .{});
    }
};

const EvalResult = struct {
    arena: *std.heap.ArenaAllocator,
    value: RinhaValue,

    pub fn deinit(self: @This()) void {
        const allocator = self.arena.child_allocator;
        self.arena.deinit();
        allocator.destroy(self.arena);
    }
};

const NameSet = std.StringHashMap(void);
const ContextMap = std.StringHashMap(RinhaValue);

const EvalContext = struct {
    current: ContextMap,
    parent: ?*EvalContext,

    pub fn init(allocator: std.mem.Allocator) @This() {
        return @This() {
            .current = ContextMap.init(allocator),
            .parent = null,
        };
    }

    pub fn new_frame(self: *@This()) @This() {
        return @This() {
            .current = ContextMap.init(self.current.allocator),
            .parent = self,
        };
    }

    pub fn deinit_frame(self: *@This()) void {
        self.current.deinit();
    }

    pub fn push_frame(self: *@This(), frame_map: ContextMap) @This() {
        return @This() {
            .current = frame_map,
            .parent = self,
        };
    }

    pub fn deinit(self: *@This()) void {
        if (self.parent) |parent| parent.deinit();
        self.current.deinit();
    }

    pub fn put(self: *@This(), name: []const u8, value: RinhaValue) std.mem.Allocator.Error!void {
        try self.current.put(name, value);
    }

    pub fn get(self: *@This(), name: []const u8) ?RinhaValue {
        const current_result = self.current.get(name);
        if (current_result == null) {
            if (self.parent) |parent| {
                return parent.get(name);
            } else {
                return null;
            }
        } else {
            return current_result.?;
        }
    }

    pub fn remove(self: *@This(), name: []const u8) bool {
        return self.current.remove(name);
    }
};

fn evalFile(allocator: std.mem.Allocator, term: *FileNode) EvalError!EvalResult {
    return eval(allocator, term.expression);
}

fn eval(allocator: std.mem.Allocator, term: *TermNode) EvalError!EvalResult {
    var context = EvalContext.init(allocator);
    defer context.deinit();
        
    var result = EvalResult{
        .arena = try allocator.create(std.heap.ArenaAllocator),
        .value = undefined,
    };

    errdefer allocator.destroy(result.arena);
    result.arena.* = std.heap.ArenaAllocator.init(allocator);
    errdefer result.arena.deinit();

    result.value = try evalWithContext(result.arena.allocator(), &context, term);
    return result;
}

fn evalWithContext(allocator: std.mem.Allocator, context: *EvalContext, term: *TermNode) EvalError!RinhaValue {
    return switch (term.*) {
        TermNodeTag.int_literal_node => |int_literal| RinhaValue{ .int = int_literal.value },
        TermNodeTag.string_literal_node => |string_literal| RinhaValue{ .string = string_literal.value },
        TermNodeTag.boolean_literal_node => |boolean_literal| RinhaValue{ .boolean = boolean_literal.value },
        TermNodeTag.tuple_node => |tuple_node| {
            const first = try evalWithContext(allocator, context, tuple_node.first);
            const second = try evalWithContext(allocator, context, tuple_node.second);
            const tuple = try allocator.create(RinhaTuple);
            
            tuple.* = RinhaTuple{ .first = first, .second = second };
            return RinhaValue{ .tuple = tuple };
        },
        TermNodeTag.var_node => |var_struct| context.get(var_struct.text) orelse EvalError.ReferenceError,
        TermNodeTag.first_proj_node => |first_node| {
            const operand = try evalWithContext(allocator, context, first_node.value);
            const result = try switch (operand) {
                RinhaValueTag.tuple => |tuple| tuple.first,
                else => EvalError.TypeError,
            };

            return result;
        },
        TermNodeTag.second_proj_node => |second_node| {
            const operand = try evalWithContext(allocator, context, second_node.value);
            const result = try switch (operand) {
                RinhaValueTag.tuple => |tuple| tuple.second,
                else => EvalError.TypeError,
            };

            return result;
        },
        TermNodeTag.let_node => |let_node| {
            const name = let_node.name.text;
            try context.put(name, RinhaValue{ .rec = name });
            const value = try evalWithContext(allocator, context, let_node.value);
            try context.put(name, value);
            switch (value) {
                RinhaValueTag.closure => |closure| {
                    try closure.context.put(name, value);
                },
                else => {}
            }
            
            const next = try evalWithContext(allocator, context, let_node.next);
            _ = context.remove(name);

            return next;
        },
        TermNodeTag.if_node => |if_node| {
            const condition = try evalWithContext(allocator, context, if_node.condition);
            return switch (condition) {
                RinhaValueTag.boolean => |boolean_value| {
                    if (boolean_value) {
                        return evalWithContext(allocator, context, if_node.then);
                    } else {
                        return evalWithContext(allocator, context, if_node.otherwise);
                    }
                },
                else => EvalError.TypeError,
            };
        },
        TermNodeTag.binary_op_node => |binop| evalBinopWithContext(allocator, context, binop),
        TermNodeTag.function_node => |func| RinhaValue{ .closure = try createClosure(allocator, context, func) },
        TermNodeTag.call_node => |call| {
            const callee = try evalWithContext(allocator, context, call.callee);
            return evalCall(allocator, context, call, callee);
        },
        TermNodeTag.print_node => |print_node| {
            const value = try evalWithContext(allocator, context, print_node.value);
            try value.printNewline();
            return RinhaValueTag.none;
        },
    };
}

fn evalCall(allocator: std.mem.Allocator, context: *EvalContext, call: *CallNode, callee: RinhaValue) EvalError!RinhaValue {
    return switch (callee) {
        RinhaValueTag.rec => |recursive_ref| {
            if (context.get(recursive_ref)) |recursive_callee| {
                return evalCall(allocator, context, call, recursive_callee);
            }
            return EvalError.ReferenceError;
        },
        RinhaValueTag.closure => |closure| {
            const parameter_count = closure.args.len;
            const argument_count = call.arguments.len;
            if (parameter_count != argument_count) return EvalError.ArgumentError;

            var base_context = context.push_frame(closure.context.*);
            var closure_context = base_context.new_frame();
            defer closure_context.deinit_frame();
            
            for (closure.args, call.arguments) |param, arg| {
                const argument_value = try evalWithContext(allocator, context, @constCast(&arg));
                try closure_context.put(param.text, argument_value);
            }

            const return_value = try evalWithContext(allocator, &closure_context, closure.value);

            return return_value;
        },
        else => EvalError.TypeError,
    };
}

fn evalBinopWithContext(allocator: std.mem.Allocator, context: *EvalContext, binop: *BinaryOpNode) EvalError!RinhaValue {
    return switch (binop.op) {
        BinaryOpTag.and_binop => {
            const lhs = try evalWithContext(allocator, context, binop.lhs);
            return switch (lhs) {
                RinhaValueTag.boolean => |boolean_value| {
                    if (!boolean_value) {
                        return RinhaValue{ .boolean = false };
                    } else {
                        return evalWithContext(allocator, context, binop.rhs);
                    }
                },
                else => EvalError.TypeError,
            };
        },
        BinaryOpTag.or_binop => {
            const lhs = try evalWithContext(allocator, context, binop.lhs);
            return switch (lhs) {
                RinhaValueTag.boolean => |boolean_value| {
                    if (boolean_value) {
                        return RinhaValue{ .boolean = true };
                    } else {
                        return evalWithContext(allocator, context, binop.rhs);
                    }
                },
                else => EvalError.TypeError,
            };
        },
        else => evalEagerBinopWithContext(allocator, context, binop),
    };
}

fn evalEagerBinopWithContext(allocator: std.mem.Allocator, context: *EvalContext, binop: *BinaryOpNode) EvalError!RinhaValue {
    const lhs = try evalWithContext(allocator, context, binop.lhs);
    const rhs = try evalWithContext(allocator, context, binop.rhs);
    return switch (binop.op) {
        BinaryOpTag.eq_binop => RinhaValue{ .boolean = lhs.testEquality(rhs) },
        BinaryOpTag.neq_binop => RinhaValue{ .boolean = !lhs.testEquality(rhs) },
        BinaryOpTag.add_binop => try lhs.performAddition(allocator, rhs) orelse EvalError.TypeError,
        else => {
            const int_lhs = try (lhs.extractInt() orelse EvalError.TypeError);
            const int_rhs = try (rhs.extractInt() orelse EvalError.TypeError);
            return performArithmeticBinop(binop.op, int_lhs, int_rhs);
        }
    };
}

fn performArithmeticBinop(binop_tag: BinaryOpTag, lhs: i64, rhs: i64) RinhaValue {
    return switch (binop_tag) {
        BinaryOpTag.lt_binop => RinhaValue{ .boolean = lhs < rhs },
        BinaryOpTag.gt_binop => RinhaValue{ .boolean = lhs > rhs },
        BinaryOpTag.lte_binop => RinhaValue{ .boolean = lhs <= rhs },
        BinaryOpTag.gte_binop => RinhaValue{ .boolean = lhs >= rhs },
        BinaryOpTag.sub_binop => RinhaValue{ .int = lhs - rhs },
        BinaryOpTag.mul_binop => RinhaValue{ .int = lhs * rhs },
        BinaryOpTag.div_binop => RinhaValue{ .int = @divTrunc(lhs, rhs) },
        BinaryOpTag.rem_binop => RinhaValue{ .int = @rem(lhs, rhs) },
        else => unreachable,
    };
}

fn createClosure(allocator: std.mem.Allocator, context: *EvalContext, func: *FunctionNode) EvalError!RinhaClosure {
    var parameter_set = NameSet.init(allocator);
    defer parameter_set.deinit();
    
    var local_context_map = try allocator.create(ContextMap);
    local_context_map.* = ContextMap.init(allocator);
    for (func.parameters) |parameter| {
        try parameter_set.put(parameter.text, {});
    }

    try collectClosureContext(func.value, context, &parameter_set, local_context_map);
    return RinhaClosure{
        .context = local_context_map,
        .args = func.parameters,
        .value = func.value,
    };
}

fn collectClosureContext(term: *TermNode, parent_context: *EvalContext, parameter_set: *NameSet, context_map: *ContextMap) EvalError!void {
    switch (term.*) {
        TermNodeTag.int_literal_node => {},
        TermNodeTag.string_literal_node => {},
        TermNodeTag.boolean_literal_node => {},
        TermNodeTag.if_node => |if_node| {
            try collectClosureContext(if_node.then, parent_context, parameter_set, context_map);
            try collectClosureContext(if_node.otherwise, parent_context, parameter_set, context_map);
        },
        TermNodeTag.tuple_node => |tuple_node| {
            try collectClosureContext(tuple_node.first, parent_context, parameter_set, context_map);
            try collectClosureContext(tuple_node.second, parent_context, parameter_set, context_map);
        },
        TermNodeTag.call_node => |call_node| {
            try collectClosureContext(call_node.callee, parent_context, parameter_set, context_map);
        },
        TermNodeTag.binary_op_node => |binop_node| {
            try collectClosureContext(binop_node.lhs, parent_context, parameter_set, context_map);
            try collectClosureContext(binop_node.rhs, parent_context, parameter_set, context_map);
        },
        TermNodeTag.first_proj_node => |first_proj_node| {
            try collectClosureContext(first_proj_node.value, parent_context, parameter_set, context_map);
        },
        TermNodeTag.second_proj_node => |second_proj_node| {
            try collectClosureContext(second_proj_node.value, parent_context, parameter_set, context_map);
        },
        TermNodeTag.print_node => |print_node| {
            try collectClosureContext(print_node.value, parent_context, parameter_set, context_map);
        },
        TermNodeTag.let_node => |let_node| {
            try parameter_set.put(let_node.name.text, {});
            try collectClosureContext(let_node.value, parent_context, parameter_set, context_map);
            _ = parameter_set.remove(let_node.name.text);
        },
        TermNodeTag.function_node => |func_node| {
            for (func_node.parameters) |parameter| {
                try parameter_set.put(parameter.text, {});
            }
            try collectClosureContext(func_node.value, parent_context, parameter_set, context_map);
            for (func_node.parameters) |parameter| {
                _ = parameter_set.remove(parameter.text);
            }
        },
        TermNodeTag.var_node => |var_node| {
            const var_name = var_node.text;
            if (parameter_set.get(var_name)) |_| return;
            if (parent_context.get(var_name)) |value| {
                try context_map.put(var_name, value);
                return;
            }
            return EvalError.ReferenceError;
        },
    }
}

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(gpa.deinit() == .ok);

    const allocator = gpa.allocator();
    
    var f = try std.fs.openFileAbsolute("/var/rinha/source.rinha.json", .{});
    var s = try f.readToEndAlloc(allocator, 1_000_000_000);
    defer allocator.free(s);

    const parsedData = try std.json.parseFromSlice(*FileNode, allocator, s, .{.ignore_unknown_fields = true});
    defer parsedData.deinit();

    const result = try evalFile(allocator, parsedData.value);
    defer result.deinit();

    try result.value.printNewline();
}
