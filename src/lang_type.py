from ctypes import *
from llvmlite import ir
from typing import List
import copy

class PtrType:
    def __init__(self, val):
        self.val = val
    def to_llvm_type(self):
        return self.val.to_llvm_type().as_pointer()

    def eq(self, other):
        if not isinstance(other, PtrType): return False
        return self.val.eq(other.val)

class Type:
    ## @param num_ptr - Levels of indirection
    def __init__(self, name):
        self.name = name

    ## Returns a copy of this type, but with an additional level of indirection
    def ptr(self): return PtrType(self)

class KindType(Type):
    ## @param val - A reference to the type this references
    def __init__(self, val):
        super().__init__("$type")
        self.val = val

    def to_llvm_type(self):
        raise NotImplementedError("Can't convert Kind to LLVM type")

class VoidType(Type):
    def __init__(self):
        super().__init__("void")
    def to_llvm_type(self): return ir.VoidType()
    def eq(self, other):
        return isinstance(other, VoidType)

class StructField:
    def __init__(self, field_name, field_type: Type):
        self.field_name = field_name
        self.field_type = field_type

class StructData:
    def __init__(self, fields: List[StructField]):
        self.fields = fields

    def human_readable_name(self):
        ret = "{ "
        for i, f in enumerate(self.fields):
            ret += f.field_name + ": " + f.field_type.name
            if i < len(self.fields)-1:
                ret += ", "
        return ret + " }"

class StructType(Type):
    def __init__(self, data: StructData):
        super().__init__(data.human_readable_name())
        self.data = data

    def eq(self, other):
        if not isinstance(other, StructType): return False
        if len(self.data.fields) != len(other.data.fields): return False
        # Compare by field
        for ii in range(len(self.data.fields)):
            if not self.data.fields[ii].field_type.eq(other.data.fields[ii].field_type): return False
        return True

    def to_llvm_type(self):
        return ir.LiteralStructType(list(map(lambda x : x.field_type.to_llvm_type(), self.data.fields)))

class IntType(Type):
    def __init__(self, num_bits: int, is_signed: bool):
        super().__init__(("i" if is_signed else "u") + str(num_bits))
        self.num_bits = num_bits
        self.is_signed = is_signed

    def to_c_type(self):
        if self.is_signed:
            if self.num_bits == 32: return c_int
            elif self.num_bits == 64: return c_long
        else:
            if self.num_bits == 32: return c_uint
            elif self.num_bits == 64: return c_ulong

    def to_llvm_type(self):
        return ir.IntType(self.num_bits)

    def eq(self, other):
        return isinstance(other, IntType) and other.num_bits == self.num_bits and other.is_signed == self.is_signed

class FloatType(Type):
    def __init__(self, num_bits: int):
        super().__init__("f" + str(num_bits))
        self.num_bits = num_bits
    def to_llvm_type(self):
        if self.num_bits == 32: return ir.FloatType
        elif self.num_bits == 64: return it.DoubleType
        else: raise NotImplementedError(str(self.num_bits) + " bit float not implemented")

    def eq(self, other):
        return isinstance(other, FloatType) and other.num_bits == self.num_bits

class FunctionType(Type):
    def __init__(self, return_type: Type, args: List[Type], is_extern: bool):
        self.return_type = return_type
        self.args = args
        self.is_extern = is_extern

    def to_llvm_type(self):
        ret = self.return_type.to_llvm_type()
        args = [arg.to_llvm_type() for arg in self.args]
        return ir.FunctionType(ret, tuple(args))

class UninstantiatedFunction(Type):
    ## @param fn_declaration - an AstFnDeclaration for instantiating this at a later date
    def __init__(self, fn_declaration):
        self.fn_declaration = fn_declaration

class BoolType(Type):
    def __init__(self):
        super().__init__("bool")
