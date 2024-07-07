from llvmlite import ir
import llvmlite.binding as llvm

def is_float(opcode):
    return opcode in [ir.FAdd, ir.FSub, ir.FMul, ir.FDiv, ir.FRem]

class BinaryIROperation:
    def __init__(self, opcode, bitwidth):
        self.opcode = opcode
        self.bitwidth = bitwidth

    def match(self, v, matches):
        if isinstance(v, ir.Instruction) and v.opcode == self.opcode and has_bit_width(v, self.bitwidth):
            matches.append(Match(False, [v.operands[0], v.operands[1]], v))
            return True
        return False

    def get_maximum_vf(self, tti):
        return tti.get_load_store_vec_reg_bit_width(0) // self.bitwidth

    def get_name(self):
        return f"{self.opcode.name}-i{self.bitwidth}"

class UnaryIROperation:
    def __init__(self, opcode, bitwidth):
        self.opcode = opcode
        self.bitwidth = bitwidth

    def match(self, v, matches):
        if isinstance(v, ir.Instruction) and v.opcode == self.opcode and has_bit_width(v, self.bitwidth):
            matches.append(Match(False, [v.operands[0]], v))
            return True
        return False

    def get_maximum_vf(self, tti):
        return tti.get_load_store_vec_reg_bit_width(0) // self.bitwidth

    def get_name(self):
        return f"{self.opcode.name}-{self.bitwidth}"

class IRVectorBinding:
    def __init__(self, op, name, sig, lane_ops):
        self.op = op
        self.name = name
        self.sig = sig
        self.lane_ops = lane_ops

    def get_cost(self, tti, ctx):
        if is_float(self.op.opcode):
            scalar_ty = ir.FloatType() if self.op.bitwidth == 32 else ir.DoubleType()
        else:
            scalar_ty = ir.IntType(self.op.bitwidth)
        num_elems = len(self.lane_ops)
        vec_ty = ir.VectorType(scalar_ty, num_elems)
        return tti.get_arithmetic_instr_cost(self.op.opcode, vec_ty)

    @classmethod
    def create(cls, op, vector_width):
        sig = InstSignature([vector_width, vector_width], [vector_width], False)
        elem_width = op.bitwidth
        num_lanes = vector_width // elem_width
        lane_ops = [
            BoundOperation(op, [(0, lo, hi), (1, lo, hi)])
            for i in range(num_lanes)
            for lo, hi in [(i * elem_width, (i + 1) * elem_width)]
        ]
        return cls(op, op.get_name(), sig, lane_ops)

    def emit(self, operands, builder):
        assert len(operands) == 2
        return builder.binop(self.op.opcode, operands[0], operands[1], name="")

    def is_supported(self, tti):
        return len(self.lane_ops) <= self.op.get_maximum_vf(tti)

class UnaryIRVectorBinding:
    def __init__(self, op, name, sig, lane_ops):
        self.op = op
        self.name = name
        self.sig = sig
        self.lane_ops = lane_ops

    def get_cost(self, tti, ctx):
        elem_width = self.op.bitwidth
        num_elems = len(self.lane_ops)
        if self.op.opcode == ir.FNeg:
            ty = ir.FloatType() if elem_width == 32 else ir.DoubleType()
            vec_ty = ir.VectorType(ty, num_elems)
            return tti.get_arithmetic_instr_cost(ir.FNeg, vec_ty)
        return 1.0

    @classmethod
    def create(cls, op, vector_width):
        sig = InstSignature([vector_width], [vector_width], False)
        elem_width = op.bitwidth
        num_lanes = vector_width // elem_width
        lane_ops = [
            BoundOperation(op, [(0, lo, hi)])
            for i in range(num_lanes)
            for lo, hi in [(i * elem_width, (i + 1) * elem_width)]
        ]
        return cls(op, op.get_name(), sig, lane_ops)

    def emit(self, operands, builder):
        assert len(operands) == 1
        ctx = builder.module.context
        elem_width = self.op.bitwidth
        num_elems = len(self.lane_ops)
        float_ty = ir.FloatType() if elem_width == 32 else ir.DoubleType()
        int_ty = ir.IntType(elem_width)
        vec_float_ty = ir.VectorType(float_ty, num_elems)
        vec_int_ty = ir.VectorType(int_ty, num_elems)
        v = operands[0]
        if self.op.opcode == ir.SIToFP:
            return builder.sitofp(try_cast(builder, v, vec_int_ty), vec_float_ty)
        elif self.op.opcode == ir.FPToSI:
            return builder.fptosi(try_cast(builder, v, vec_float_ty), vec_int_ty)
        elif self.op.opcode == ir.FNeg:
            return builder.fneg(try_cast(builder, v, vec_float_ty))
        else:
            raise ValueError("Unsupported unary opcode")

    def is_supported(self, tti):
        return len(self.lane_ops) <= self.op.get_maximum_vf(tti)

def try_cast(builder, v, ty):
    if v.type == ty:
        return v
    return builder.bitcast(v, ty)

class IRInstTable:
    def __init__(self):
        self.vectorizable_ops = []
        self.unary_ops = []
        self.float_to_int_ops = []
        self.int_to_float_ops = []
        self.vector_float_to_ints = []
        self.vector_int_to_floats = []
        self.vector_insts = []
        self.unary_vector_insts = []
        self.trunc_ops = []
        self.ext_ops = []
        self.select_ops = []
        self.unary_math_ops = []
        self.binary_math_ops = []
        self.vector_truncs = []
        self.vector_extensions = []
        self.vector_selects = []
        self.vector_unary_math_funcs = []
        self.vector_binary_math_funcs = []

        self._init_vectorizable_ops()
        self._init_unary_ops()
        self._init_float_int_ops()
        self._init_vector_ops()
        self._init_boolean_ops()
        self._init_cast_ops()
        self._init_select_ops()
        self._init_math_ops()
        self._init_vector_math_ops()

    def _init_vectorizable_ops(self):
        vectorizable_opcodes = [
            ir.Add, ir.FAdd, ir.Sub, ir.FSub, ir.Mul, ir.FMul,
            ir.UDiv, ir.SDiv, ir.FDiv, ir.URem, ir.SRem, ir.FRem,
            ir.Shl, ir.LShr, ir.AShr, ir.And, ir.Or, ir.Xor
        ]
        scalar_bitwidths = [1, 8, 16, 32, 64]
        for opcode in vectorizable_opcodes:
            for sb in scalar_bitwidths:
                if is_float(opcode) and sb not in [32, 64]:
                    continue
                self.vectorizable_ops.append(BinaryIROperation(opcode, sb))

    def _init_unary_ops(self):
        unary_opcodes = [ir.FNeg]
        for opcode in unary_opcodes:
            for sb in [32, 64]:
                self.unary_ops.append(UnaryIROperation(opcode, sb))

    def _init_float_int_ops(self):
        scalar_bitwidths = [1, 8, 16, 32, 64]
        for sb in scalar_bitwidths:
            self.float_to_int_ops.append(FloatToInt(sb, True))
            self.float_to_int_ops.append(FloatToInt(sb, False))
            self.int_to_float_ops.append(IntToFloat(sb, True))
            self.int_to_float_ops.append(IntToFloat(sb, False))

    def _init_vector_ops(self):
        vector_bitwidths = [64, 128, 256, 512]
        for op in self.vectorizable_ops:
            if op.bitwidth == 1:
                continue
            for vb in vector_bitwidths:
                if vb // op.bitwidth == 1:
                    continue
                self.vector_insts.append(IRVectorBinding.create(op, vb))

        for op in self.unary_ops:
            for vb in vector_bitwidths:
                if vb // op.bitwidth == 1:
                    continue
                self.unary_vector_insts.append(UnaryIRVectorBinding.create(op, vb))

    def _init_boolean_ops(self):
        for op in self.vectorizable_ops:
            if op.bitwidth != 1:
                continue
            for vl in [2, 4, 8, 16]:
                self.vector_insts.append(IRVectorBinding.create(op, vl))

    def _init_cast_ops(self):
        scalar_bitwidths = [1, 8, 16, 32, 64]
        for in_width in scalar_bitwidths:
            for out_width in scalar_bitwidths:
                if in_width > out_width:
                    self.trunc_ops.append(Truncate(in_width, out_width))
                elif in_width < out_width:
                    self.ext_ops.append(Extension(in_width, out_width, True))
                    self.ext_ops.append(Extension(in_width, out_width, False))

    def _init_select_ops(self):
        for bitwidth in [1, 8, 16, 32, 64]:
            self.select_ops.append(Select(bitwidth))

    def _init_math_ops(self):
        fp_unary_intrinsics = [
            ir.Intrinsic.sin, ir.Intrinsic.cos, ir.Intrinsic.exp,
            ir.Intrinsic.exp2, ir.Intrinsic.log, ir.Intrinsic.log10,
            ir.Intrinsic.log2, ir.Intrinsic.fabs, ir.Intrinsic.sqrt,
        ]
        for intrinsic in fp_unary_intrinsics:
            self.unary_math_ops.append(UnaryMath(intrinsic, 32, True))
            self.unary_math_ops.append(UnaryMath(intrinsic, 64, True))

        int_unary_intrinsics = [ir.Intrinsic.abs]
        for bitwidth in [1, 8, 16, 32, 64]:
            for intrinsic in int_unary_intrinsics:
                self.unary_math_ops.append(UnaryMath(intrinsic, bitwidth, False))

        self.binary_math_ops.append(BinaryMath(ir.Intrinsic.pow, True))
        self.binary_math_ops.append(BinaryMath(ir.Intrinsic.pow, False))

    def _init_vector_math_ops(self):
        for vl in [2, 4, 8, 16]:
            for trunc_op in self.trunc_ops:
                self.vector_truncs.append(VectorTruncate.create(trunc_op, vl))
            for ext_op in self.ext_ops:
                self.vector_extensions.append(VectorExtension.create(ext_op, vl))
            for sel_op in self.select_ops:
                self.vector_selects.append(VectorSelect.create(sel_op, vl))
            for unary_op in self.unary_math_ops:
                self.vector_unary_math_funcs.append(VectorUnaryMath.create(unary_op, vl))
            for binary_op in self.binary_math_ops:
                self.vector_binary_math_funcs.append(VectorBinaryMath.create(binary_op, vl))