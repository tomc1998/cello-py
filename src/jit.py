from ctypes import CFUNCTYPE
import llvmlite.binding as llvm
from llvmlite import ir
import jit_result

## LLVM JIT engine
jit = None

## Call this at the start of compilation
def init_jit():
    global jit
    """
    Create an ExecutionEngine suitable for JIT code generation on
    the host CPU.  The engine is reusable for an arbitrary number of
    modules.
    """
    # Create a target machine representing the host
    target = llvm.Target.from_default_triple()
    target_machine = target.create_target_machine()
    # And an execution engine with an empty backing module
    backing_mod = llvm.parse_assembly("")
    jit = llvm.create_mcjit_compiler(backing_mod, target_machine)

## Given a node 'n', jit it and return the result of the expression as an LLVM
## value (probably a constant, need to think about this though).
def jit_node(n, ty, scope):
    global jit
    assert jit, "JIT not initialized"
    ## Create the IR module, gen the module
    m = ir.Module(name="jit")
    internal_ret_ty = n.get_type(scope)
    ret_ty = internal_ret_ty.to_llvm_type()
    fnty = ir.FunctionType(ret_ty, ())
    fn = ir.Function(m, fnty, name="__jit_entry")
    entry_block = fn.append_basic_block(name="entry")
    b = ir.IRBuilder(entry_block)
    ret_val = n.codegen(m, scope, b)
    b.ret(ret_val)

    ## Create binding module
    binding_module = llvm.parse_assembly(str(m))
    binding_module.verify()

    ## Add & compile
    jit.add_module(binding_module)
    jit.finalize_object()
    jit.run_static_constructors()

    ## Get the main function
    entry_fn_ptr = jit.get_function_address("__jit_entry")
    entry_fn = CFUNCTYPE(restype=internal_ret_ty.to_c_type())(entry_fn_ptr)
    res = entry_fn()

    print("JIT RESULT = " + str(res))

    jit.remove_module(binding_module)

    return jit_result.to_llvm_constant(res, ty)
