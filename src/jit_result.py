## Module for converting from python constants (result of JIT calls) back to LLVM values

from llvmlite import ir
from lang_type import *

## @param exp_ty the lang_type that is expected of the expression
def to_llvm_constant(val, exp_ty):
    if not val: return None
    return ir.Constant(exp_ty.to_llvm_type(), val)

