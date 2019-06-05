from ctypes import CFUNCTYPE
import llvmlite.binding as llvm
import glob
from llvmlite import ir
from lang_type import *
import jit_result

## LLVM JIT engine
jit = None
target_machine = None
env_stack = [[]]

## Call this at the start of compilation
def init_jit(libc_path):
    global jit, target_machine
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

    # Add libc
    llvm.load_library_permanently(libc_path)

def curr_jit_env():
    global jit
    return env_stack[len(env_stack)-1]

def push_jit_env():
    global jit
    env_stack.append([])

def pop_jit_env():
    global jit
    for m in curr_jit_env(): jit.remove_module(m)
    env_stack.pop()

def add_module(m):
    global jit
    binding_module = llvm.parse_assembly(str(m))
    binding_module.verify()
    jit.add_module(binding_module)
    env_stack[len(env_stack)-1].append(binding_module)

## Given a node 'n', jit it and return the result of the expression as an LLVM
## value (probably a constant, need to think about this though).
## @param gen_preamble_fn - This is called with the jit LLVM module right
## before creating the entry function for the module. Use this to insert any
## declarations that the node needs to function.
## @param post_jit_fn - called after jitting. Passed the ExecutionEngine. This
## is useful for examining global state post-jit.
def jit_node(n, scope, gen_preamble_fn=None, post_jit_fn=None):
    global jit
    assert jit, "JIT not initialized"
    ## Create the IR module, gen the module
    m = ir.Module(name="jit")
    if gen_preamble_fn: gen_preamble_fn(m, scope)
    internal_ret_ty = n.get_type(scope)
    ret_ty = internal_ret_ty.to_llvm_type()
    fnty = ir.FunctionType(ret_ty, ())
    fn = ir.Function(m, fnty, name="__jit_entry")
    entry_block = fn.append_basic_block(name="entry")
    b = ir.IRBuilder(entry_block)
    ret_val = n.codegen(m, scope, b)
    if isinstance(internal_ret_ty, VoidType): b.ret_void()
    else: b.ret(ret_val)

    ## Create binding module
    binding_module = llvm.parse_assembly(str(m))
    binding_module.verify()

    ## Add & compile
    jit.add_module(binding_module)
    jit.finalize_object()
    jit.run_static_constructors()

    ## Get the main function
    entry_fn_ptr = jit.get_function_address("__jit_entry")
    if isinstance(internal_ret_ty, VoidType):
        entry_fn = CFUNCTYPE(None)(entry_fn_ptr)
    else:
        entry_fn = CFUNCTYPE(restype=internal_ret_ty.to_c_type())(entry_fn_ptr)
    res = entry_fn()

    if post_jit_fn: post_jit_fn(jit)

    jit.remove_module(binding_module)

    if isinstance(internal_ret_ty, VoidType): return None
    else: return res
