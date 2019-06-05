from overloads import Overload
from llvmlite import ir
from typing import List
from parser import *
from lang_type import *
from mangle import mangle_function, mangle_global
from scope import Var
import ctypes
import jit
import jit_result

class AstNode:
    def walk_dfs(self, fn): fn(self)
    def __init__(self, decoration):
        self.decoration = decoration

def can_coerce(from_ty, to_ty):
    if isinstance(from_ty, StructType) and isinstance(to_ty, PtrType) and to_ty.val.eq(from_ty):
        return True
    if isinstance(from_ty, IntType) and isinstance(to_ty, IntType):
        return True
    else:
        return False

# Asserts if not possible
# If to_ty == None, return val
def gen_coercion(b, val, from_ty, to_ty):
    if not to_ty: return val
    if from_ty == to_ty or from_ty.eq(to_ty):
        return val;
    assert can_coerce(from_ty, to_ty)
    if isinstance(from_ty, IntType) and isinstance(to_ty, IntType):
        # Just coerce with no typechecking
        if from_ty.num_bits == to_ty.num_bits: return val
        if from_ty.num_bits < to_ty.num_bits:
            if from_ty.is_signed: return b.sext(val, to_ty.to_llvm_type())
            else: return b.zext(val, to_ty.to_llvm_type())
        else:
            return b.trunc(val, to_ty.to_llvm_type())
    else:
        assert False, "Can't coerce from " + str(from_ty) + " to " + str(to_ty)

class AstProgram(AstNode):
    def __init__(self, children, decoration):
        super().__init__(decoration)
        self.children = children

    def codegen(self, m, s, b, exp_ty=None):
        ret = None
        for i, c in enumerate(self.children):
            ret = c.codegen(m, s, b)
        if isinstance(self.get_type(s), VoidType): return None
        return gen_coercion(b, ret, self.get_type(s), exp_ty)

    def run_all_comptime(self, m, s):
        for c in self.children: c.run_all_comptime(m, s)

    def walk_dfs(self, fn):
        for c in self.children: c.walk_dfs(fn)
        fn(self)

    def get_type(self, s):
        return self.children[len(self.children)-1].get_type(s)

class AstComptimeIf(AstNode):
    def __init__(self, if_expr, decoration):
        super().__init__(decoration)
        self.if_expr = if_expr
        ## Set to the given if branch body when comptime is run
        self.chosen_body = None

    def walk_dfs(self, fn):
        self.if_expr.walk_dfs(fn)
        fn(self)

    def get_type(self, s):
        assert self.chosen_body
        return self.chosen_body.get_type(s)

    def run_all_comptime(self, m, s):
        ## Run all cond comptime blocks (don't run bodies yet)
        for cond in self.if_expr.conditions:
            cond.cond.run_all_comptime(m, s)
        ## Eval all conditions
        for cond in self.if_expr.conditions:
            ## Just create an AstComptime
            comptime = AstComptime(cond.cond, cond.decoration)
            comptime.run_all_comptime(m, s)
            val = comptime.computed
            if bool(val):
                ## This is true, so just choose this condition body
                self.chosen_body = cond.body
        ## If one of the branches was true, run comptime on that now
        if self.chosen_body: self.chosen_body.run_all_comptime(m, s)

    def codegen(self, m, s, b, exp_ty=None):
        assert self.chosen_body
        return self.chosen_body.codegen(m, s, b, exp_ty)

## Basically just a list of AstConditionals with an optional fallback (else clause)
class AstIf(AstNode):
    def __init__(self, conditions, decoration, fallback=None):
        self.conditions = conditions
        self.decoration = decoration
        self.fallback = fallback

    def walk_dfs(self, fn):
        for c in self.conditions: c.walk_dfs(fn)
        self.fallback.walk_dfs(fn)
        fn(self)

    def get_type(self, s):
        return self.conditions[0].body.get_type(s)

    def run_all_comptime(self, m, s):
        for c in self.conditions: c.run_all_comptime(m, s)
        self.fallback.run_all_comptime(m, s)

    def codegen(self, m, s, b, exp_ty=None):
        internal_type = self.get_type(s)
        ty = internal_type.to_llvm_type()
        after_block = b.append_basic_block("after")
        incoming = []
        for cond in self.conditions:
            incoming.append(cond.codegen(m, s, b, after_block, ty, exp_ty=internal_type))
        if self.fallback:
            incoming.append((self.fallback.codegen(m, s, b, exp_ty=internal_type), b.block))
        b.branch(after_block)
        b.position_at_start(after_block)
        if self.fallback:
            phi = b.phi(ty, name="if-chain-result")
            for (val, block) in incoming:
                phi.add_incoming(val, block)
            return gen_coercion(b, phi, self.get_type(s), exp_ty)
        else:
            return None

class AstAssignment(AstNode):
    def __init__(self, var, op, val, decoration):
        super().__init__(decoration)
        self.var = var
        self.op = op
        self.val = val
    def get_type(self, s): return VoidType()
    def walk_dfs(self, fn):
        self.var.walk_dfs(fn)
        self.val.walk_dfs(fn)
        fn(self)

    def codegen(self, m, s, b, exp_ty=None):
        ## Check for overload
        ty = self.var.get_type(s)
        rhs_ty = self.val.get_type(s)
        if self.op in ty.operator_overloads:
            overloads = ty.operator_overloads[self.op]
            fn_declaration = None
            ## Select overload based on arg type
            for overload in overloads:
                if len(overload.arg_types) == 1 and can_coerce(rhs_ty, overload.arg_types[0]):
                    fn_declaration = overload.fn_declaration
            if fn_declaration:
                ## Call the function, and return
                fn = fn_declaration.instantiate(m, s, [])
                internal_fnty = fn_declaration.fn_signature.codegen(s)
                args = []
                lhs_arg = self.var.codegen(m, s, b, exp_ty=internal_fnty.args[0])
                rhs_arg = self.val.codegen(m, s, b, exp_ty=internal_fnty.args[1])
                return gen_coercion(b, b.call(fn, [lhs_arg, rhs_arg]), internal_fnty.return_type, exp_ty)
        ## No overload, do default
        if self.op == "=":
            return b.store(self.val.codegen(m, s, b), self.var.codegen(m, s, b, lval=True))
        else:
            ## Assume this is an op like +=
            assert len(self.op) == 2 and self.op[1] == "="
            ## basically just expand to var = var op[0] val
            binop = BinaryExpression(self.var, self.op[0], self.val, self.decoration)
            return b.store(binop.codegen(m, s, b, exp_ty=ty), self.var.codegen(m, s, b, lval=True))
    def run_all_comptime(self, m, s):
        self.var.run_all_comptime(m, s)
        self.val.run_all_comptime(m, s)
        ## Instantiate overload if this is an overload
        ty = self.var.get_type(s)
        rhs_ty = self.val.get_type(s)
        if self.op in ty.operator_overloads:
            ## Select overload based on arg type
            overloads = ty.operator_overloads[self.op]
            for overload in overloads:
                if len(overload.arg_types) == 1 and can_coerce(rhs_ty, overload.arg_types[0]):
                    overload.fn_declaration.instantiate(m, s, [])


class AstRange(AstNode):
    def __init__(self, start, end):
        self.start = start
        self.end = end
    def walk_dfs(self, fn):
        self.start.walk_dfs(fn)
        self.end.walk_dfs(fn)
        fn(self)
    def run_all_comptime(self, m, s):
        self.start.run_all_comptime(m, s)
        self.end.run_all_comptime(m, s)
    def codegen_start(self, m, s, b, exp_ty=None):
        return self.start.codegen(m, s, b, exp_ty)
    def codegen_end(self, m, s, b, exp_ty=None):
        return self.end.codegen(m, s, b, exp_ty)

class AstForLoop(AstNode):
    def __init__(self, iter_var_name, iter_expr, body, index_var_name = None):
        self.iter_var_name = iter_var_name
        self.iter_expr = iter_expr
        self.body = body
        self.index_var_name = index_var_name
    def get_type(self, s): return VoidType()
    def walk_dfs(self, fn):
        self.iter_expr.walk_dfs(fn)
        self.body.walk_dfs(fn)
        fn(self)
    def run_all_comptime(self, m, s):
        self.iter_expr.run_all_comptime(m, s)
        self.body.run_all_comptime(m, s)
    def codegen(self, m, s, b, exp_ty=None):
        if isinstance(self.iter_expr, AstRange):
            iter_var_type = IntType(64, True)
            ## Alloc iter var
            iter_var = b.alloca(iter_var_type.to_llvm_type());
            # Init iter var
            b.store(self.iter_expr.codegen_start(m, s, b, iter_var_type), iter_var)
            # Get end point
            iter_end = self.iter_expr.codegen_end(m, s, b, iter_var_type)

            body_block = b.append_basic_block()
            after_block = b.append_basic_block()
            cond_block = b.append_basic_block()

            ## Create body subscope
            body_subscope = s.subscope()
            body_subscope.set(self.iter_var_name, Var(iter_var_type, val=iter_var))

            ## Generate precondition
            cond = b.icmp_signed("<", b.load(iter_var), iter_end)
            b.cbranch(cond, body_block, after_block)

            b.position_at_start(body_block)
            ## Gen body with body subscope
            self.body.codegen(m, body_subscope, b)
            ## Increment iter var, assume end > start
            curr_iter_var = b.load(iter_var)
            new_iter_var = b.add(curr_iter_var, ir.Constant(iter_var_type.to_llvm_type(), 1))
            b.store(new_iter_var, iter_var)
            ## Cond block
            b.branch(cond_block)
            b.position_at_start(cond_block)
            cond = b.icmp_signed("<", new_iter_var, iter_end)
            b.cbranch(cond, body_block, after_block)
            b.position_at_end(after_block)



        else: assert False, "Unimpl"

class AstStructFnDeclaration(AstNode):
    def __init__(self, fn_declaration, decoration):
        super().__init__(decoration)
        self.fn_declaration = fn_declaration

    def walk_dfs(self, fn):
        self.fn_declaration.walk_dfs(fn)
        fn(self)

    def get_type(self, s):
        return StructField(self.fn_declaration.name, UninstantiatedFunction(self.fn_declaration))

class AstStructMemberVar(AstNode):
    def __init__(self, name, type_expr):
        self.name = name
        self.type_expr = type_expr

    def walk_dfs(self, fn):
        self.name.walk_dfs(fn)
        self.type_expr.walk_dfs(fn)
        fn(self)

    def get_type(self, s):
        return StructField(self.name, self.type_expr.get_type(s).val)

# A struct declaration, returns the struct when evaluated with get_type
class AstStructDefinition(AstNode):
    def __init__(self, fields):
        self.fields = fields

    def walk_dfs(self, fn):
        for f in self.fields: f.walk_dfs(fn)
        fn(self)

    ## @param name - Forward declare the given name with the incomplete struct
    ## type as the struct type is formed. If NOne, doesn't add anything to the
    ## scope.
    def get_type(self, s, name=None):
        resolved_fields = list(map(lambda x: x.get_type(s), self.fields))
        struct_type = StructType(StructData(resolved_fields))
        subscope = s.subscope()
        if name != None:
            subscope.set(name, Var(KindType(struct_type)))
        struct_type.connect_member_functions(subscope)
        return KindType(struct_type)

class AstVarDeclaration(AstNode):
    def __init__(self, name, declared_type, val, is_export=False, is_comptime=False, is_mut=False):
        self.name = name
        self.declared_type = declared_type
        self.val = val
        self.is_export = is_export
        self.is_comptime = is_comptime
        self.is_mut = is_mut
    def walk_dfs(self, fn):
        if self.declared_type: self.declared_type.walk_dfs(fn)
        self.val.walk_dfs(fn)
        fn(self)
    def run_all_comptime(self, m, s):
        if self.declared_type: self.declared_type.run_all_comptime(m, s)
        self.val.run_all_comptime(m, s)
        ## Add this to scope if we're running comptime, no need to compute val
        s.set(self.name, Var(self.get_var_type(s), is_mutable=self.is_mut, is_comptime=self.is_comptime, name=self.name))
    def get_type(self, s): return VoidType()
    def get_var_type(self, s):
        # Figure out the type
        var_type = None
        if self.declared_type: var_type = self.declared_type.resolve(s)
        else: var_type = self.val.get_type(s)
        return var_type
    def codegen(self, m, s, b, exp_ty=None):
        var_type = self.get_var_type(s)
        val = None
        ## If comptime, just jit the value and get as a python value, for
        ## better comptime inspection
        if self.is_comptime:
            val = jit.jit_node(self.val, s.subscope())
        else:
            # Coerce val?
            actual_val = None
            if self.declared_type:
                actual_val = self.val.codegen(m, s, b, exp_ty=var_type)
            else:
                actual_val = self.val.codegen(m, s, b)
            val = None
            ## If we're in global scope, then b == None. If so, create a global var.
            if not b:
                val = ir.GlobalVariable(m, var_type.to_llvm_type(), self.name)
                val.initializer = actual_val
            else:
                ## Otherwise, alloc var on stack
                val = b.alloca(var_type.to_llvm_type())
                b.store(actual_val, val)
        s.set(self.name, Var(var_type, val=val, is_mutable=self.is_mut, is_comptime=self.is_comptime, name=self.name))

class AstTypeDeclaration(AstNode):
    def __init__(self, name, definition, is_export=False):
        self.name = name
        self.definition = definition
        self.is_export = is_export
    def walk_dfs(self, fn):
        self.definition.walk_dfs(fn)
        fn(self)
    def run_all_comptime(self, m, s):
        print ("Unimpl run all comptime on ast type decl")
    def codegen(self, m, s, b, exp_ty=None):
        s.set(self.name, Var(self.definition.get_type(s, name=self.name)))

## Execute some code if the given condition is true
class AstConditional(AstNode):
    def __init__(self, cond, body, decoration):
        super().__init__(decoration)
        self.cond = cond
        self.body = body
    def walk_dfs(self, fn):
        self.cond.walk_dfs(fn)
        self.body.walk_dfs(fn)
        fn(self)
    def run_all_comptime(self, m, s):
        self.cond.run_all_comptime(m, s)
        self.body.run_all_comptime(m, s)
    def codegen(self, m, s, b, after_block, ty, exp_ty=None):
        curr_block = b.block
        true_block = b.append_basic_block()
        false_block = b.append_basic_block()
        b.position_at_end(curr_block)
        b.cbranch(self.cond.codegen(m, s, b), true_block, false_block)
        b.position_at_start(true_block)
        true_val = self.body.codegen(m, s, b)
        b.branch(after_block)
        true_block = b.block
        b.position_at_start(false_block)
        return (gen_coercion(b, true_val, self.body.get_type(s), exp_ty), true_block)

class AstComptime(AstNode):
    def __init__(self, body, decoration):
        super().__init__(decoration)
        self.body = body
        self.computed = None
    def get_type(self, s): return self.body.get_type(s)
    def walk_dfs(self, fn):
        self.body.walk_dfs(fn)
        fn(self)
    def run_all_comptime(self, m, s):
        # First run all nested comptime
        self.body.run_all_comptime(m, s)

        body = self.body
        def preamble(m, s):
            ## Walk the body & find all fncalls so we can fwd decl them all
            def fwd_decl_walk_fn(x):
                if isinstance(x, AstFnCall):
                    ## Fwd decl this function
                    type_parameters = []
                    if x.template_params:
                        type_parameters = list(map(lambda x: x.get_type(s), x.template_params))
                    internal_fnty = x.get_internal_fnty(s)
                    name = x.name.parse_node.tok_val[0].tok_val[0].tok_val[1]
                    if not internal_fnty.is_extern:
                        name = mangle_function(name, type_parameters)
                    ir.Function(m, internal_fnty.to_llvm_type(), name=name)
            body.walk_dfs(fwd_decl_walk_fn)

            ## Now walk the body & find all references to comptime variables,
            ## if they're comptime re-declare them in s as non-comptime (so
            ## they can be used)
            def comptime_var_walk_fn(x):
                if isinstance(x, VarIdent):
                    name = x.parse_node.tok_val[0].tok_val[1]
                    var = s.lookup(name)
                    if var and var.is_comptime:
                        ## It's fine to .set here, because `s` is a subscope
                        ## passed into jit_node(). It'll only shadow the
                        ## variable.
                        cloned = var.clone()
                        ty = cloned.var_type.to_llvm_type()
                        ## Create global variable
                        global_var = ir.GlobalVariable(m, ty, name)
                        global_var.initializer = cloned.val
                        cloned.val = global_var
                        cloned.is_comptime = False
                        s.set(name, cloned)
            body.walk_dfs(comptime_var_walk_fn)

        ## Post jit FN for after the jit.
        def post_jit_fn(jit):
            ## Find the global vars
            def comptime_var_walk_fn(x):
                if isinstance(x, VarIdent):
                    name = x.parse_node.tok_val[0].tok_val[1]
                    var = s.lookup_exact(name)
                    if var and var.is_comptime:
                        ## Find this var in the jit & assign it to the var
                        var_c_type = var.var_type.to_c_type()
                        addr = jit.get_global_value_address(var.name)
                        ptr = ctypes.cast(addr, ctypes.POINTER(var_c_type))
                        var.val = ptr.contents.value
            body.walk_dfs(comptime_var_walk_fn)

        subscope = s.subscope();
        ty = self.body.get_type(s)
        self.computed = jit.jit_node(self.body, subscope, preamble, post_jit_fn)
    def codegen(self, m, s, b, exp_ty=None):
        if not self.computed: return None
        ty = self.body.get_type(s)
        val = jit_result.to_llvm_constant(self.computed, ty)
        ty = self.body.get_type(s)
        if isinstance(ty, VoidType): return
        return gen_coercion(b, val, ty, exp_ty)

class AstFnDeclaration(AstNode):
    def __init__(self, name, template_parameter_decl_list, fn_signature, body, is_operator_overload, decoration):
        super().__init__(decoration)
        self.name = name
        self.template_parameter_decl_list = template_parameter_decl_list
        self.fn_signature = fn_signature
        self.body = body
        self.is_operator_overload = is_operator_overload
        ## A map of name mangles to instantiated functions (unused if template_parameter_decl_list == None or len 0)
        self.instantiated = {}

    def walk_dfs(self, fn):
        self.template_parameter_decl_list.walk_dfs(fn)
        self.fn_signature.walk_dfs(fn)
        self.body.walk_dfs(fn)
        fn(self)

    def get_type(self, s): return VoidType()

    def run_all_comptime(self, m, s): pass

    ## Add receiver fields to the given scope 's'
    def add_receiver_to_scope(self, s, b, receiver_val):
        ## If we have a receiver, insert the receiver's fields into the scope
        if self.fn_signature.receiver:
            for ii, f in enumerate(self.fn_signature.receiver.data.fields):
                if f.is_function():
                    s.set(f.field_name, Var(f.field_type))
                elif s.is_comptime:
                    s.set(f.field_name, Var(f.field_type, name="self." + f.field_name))
                else:
                    ## Create GEP
                    gep = b.gep(receiver_val, [ir.Constant(ir.IntType(32), 0), ir.Constant(ir.IntType(32), ii)])
                    s.set(f.field_name, Var(f.field_type, val=gep, name="self." + f.field_name))

    def instantiate(self, m, s, type_parameters: List[Type]):
        assert (not self.template_parameter_decl_list and len(type_parameters) == 0) \
            or len(type_parameters) == len(self.template_parameter_decl_list)
        parent_scope = s
        s = s.subscope()

        ## First figure out type params and add them all to the scope before
        ## trying to gen the fn signature (since returntype / args might depend
        ## on the template params)
        if self.template_parameter_decl_list:
            for i, f in enumerate(self.template_parameter_decl_list):
                s.set(f.name, Var(type_parameters[i], name=f.name))

        ## Create the function type
        internal_fnty = self.fn_signature.codegen(s)

        ## Create a list of tuples of names to types of args
        all_args = copy.deepcopy(self.fn_signature.parameter_decl_list)
        if self.fn_signature.receiver: all_args.insert(0, self.fn_signature.receiver.ptr())
        all_args = zip(map(lambda x: x.name if hasattr(x, 'name') else 'self', all_args), internal_fnty.args)
        all_args = list(map(lambda x: (x[0], Var(x[1], name=x[0])), all_args))


        ## Mangle the func name
        mangled = None
        if self.is_operator_overload:
            mangled = self.name
            for a in internal_fnty.args[1:]: mangled += a.mangle()
        else:
            mangled = mangle_function(self.name, list(map(lambda x: x.val, type_parameters)))

        ## Check if we have an instantiation cached
        if mangled in self.instantiated: return self.instantiated[mangled]

        ## Instantiate, so run the comptime stuff for the body in a subscope
        comptime_subscope = s.comptime_subscope()
        self.add_receiver_to_scope(comptime_subscope, None, None)
        for name,var in all_args: comptime_subscope.set(name,var)
        jit.push_jit_env()
        jit.add_module(m)
        self.body.run_all_comptime(m, comptime_subscope)
        jit.pop_jit_env()

        fnty = internal_fnty.to_llvm_type()
        ## Create the function
        fn = ir.Function(m, fnty, name = mangled)
        ## Add to (parent) scope (if this isn't an op overload)
        if not self.is_operator_overload: parent_scope.set(mangled, Var(internal_fnty, fn))

        if self.fn_signature.receiver:
            ## Set implicit receiver on any function calls to 'self'
            def set_implicit_receiver(x):
                if isinstance(x, AstFnCall):
                    x.implicit_receiver = fn.args[0]
            self.body.walk_dfs(set_implicit_receiver)

        entry_block = fn.append_basic_block(name="entry")
        b = ir.IRBuilder(entry_block)

        ## Add receiver to scope
        if len(fn.args) > 0: self.add_receiver_to_scope(s, b, fn.args[0])

        ## Insert args into the scope, storing as allocas
        ## Map fn arg LLVM values to previous arg list
        all_args = map(lambda x: (x[1][0], Var(x[1][1].var_type, val=x[0], name=x[1][1].name)), zip(fn.args, all_args))
        for name, var in all_args:
            llvm_ty = var.var_type.to_llvm_type()
            alloca = b.alloca(llvm_ty, name=name)
            b.store(var.val, alloca)
            var.val = alloca
            s.set(name, var)
            # Set the receiver val if this is the first arg
            if name == 'self': receiver_val = var.val

        if internal_fnty.return_type.eq(VoidType()):
            self.body.codegen(m, s, b, exp_ty=internal_fnty.return_type)
            b.ret_void()
        else:
            b.ret(self.body.codegen(m, s, b, exp_ty=internal_fnty.return_type))

        self.instantiated[mangled] = fn
        return fn

    def codegen(self, m, s, b):
        if not self.template_parameter_decl_list or len(self.template_parameter_decl_list) == 0:
            ## Instantiate immediately
            self.instantiate(m, s, [])
        else:
            ## Add as uninstantiated
            s.set(self.name, Var(UninstantiatedFunction(self)))

class AstFnSignature(AstNode):
    # @param receiver - The type of the receiver, None if this isn't a member fn
    def __init__(self, parameter_decl_list, is_mut, is_extern, return_type, decoration, receiver=None):
        super().__init__(decoration)
        self.parameter_decl_list = parameter_decl_list
        self.is_mut = is_mut
        self.is_extern = is_extern
        self.return_type = return_type
        self.receiver = receiver

    def walk_dfs(self, fn):
        self.parameter_decl_list.walk_dfs(fn)
        self.return_type.walk_dfs(fn)
        fn(self)

    def codegen(self, s):
        ret = None
        if self.return_type:
            ret = self.return_type.resolve(s)
        else: ret = VoidType()
        args = [pdecl.type_ident.resolve(s) for pdecl in self.parameter_decl_list]
        if self.receiver:
            args.insert(0, self.receiver.ptr())
        return FunctionType(ret, args, self.is_extern)

class AstFnInstantiation(AstNode):
    def __init__(self, name, template_params):
        self.name = name
        self.template_params = template_params

    def get_type(self, s): return VoidType()

    def walk_dfs(self, fn):
        self.template_params.walk_dfs(fn)
        fn(self)

    def run_all_comptime(self, m, s): pass

    def codegen(self, m, s, b):
        resolved = s.lookup(self.name)
        template_parameters = []
        if self.template_params:
            template_parameters = list(map(lambda x: x.get_type(s), self.template_params))
        resolved.var_type.fn_declaration.instantiate(m, s, template_parameters)

class AstFnCall(AstNode):
    def __init__(self, name, template_params, param_list, decoration):
        super().__init__(decoration)
        self.name = name
        self.template_params = template_params
        if self.template_params == None: self.template_params = []
        self.param_list = param_list
        self.implicit_receiver = None

    def walk_dfs(self, fn):
        self.name.walk_dfs(fn)
        for p in self.template_params: p.walk_dfs(fn)
        for p in self.param_list: p.walk_dfs(fn)
        fn(self)

    def run_all_comptime(self, m, s):
        ## Instantiate function now (assuming this isn't a recursive call),
        ## since we need to run the comptime stuff on the called function now -
        ## otherwise the module will be half way through a function when we
        ## JIT, and we'll get an error parsing the module
        fn_type = self.find_function_type(s)
        ## If we can't find the function, chances are this is recursive, & we
        ## don't (currently) support comptime eval for recursive calls. This
        ## would only make sense if we're instantiating functions
        ## recursively... not sure if we wantt his / need this, either way
        ## sounds like a faff. Just ignore for now.
        ## TODO if recursive comptime evaluation is needed, needs to be put in here
        if not fn_type: return
        if isinstance(fn_type, UninstantiatedFunction):
            internal_fnty = self.get_internal_fnty(s)
            concrete_template_params = list(map(lambda x: x.get_type(s), self.template_params))
            fn_type.fn_declaration.instantiate(m, s, concrete_template_params)

    def find_function_type(self, s):
        if isinstance(self.name, AstQualifiedName):
            return self.name.get_type(s)
        resolved = self.name.resolve(s)
        if not resolved:
            ## Try the mangled name
            type_parameters = []
            if self.template_params:
                type_parameters = list(map(lambda x: x.get_type(s), self.template_params))
            mangled = mangle_function(self.name.parse_node.tok_val[0].tok_val[0].tok_val[1], type_parameters)
            return s.lookup(mangled).var_type;
        else: return resolved.var_type

    ## Find the associated function Var
    def find_function(self, m, s, b):
        if isinstance(self.name, AstQualifiedName):
            resolved = self.name.resolve(m, s, b)
        else:
            resolved = self.name.resolve(s)
        if not resolved:
            ## Try the mangled name
            type_parameters = []
            if self.template_params:
                type_parameters = list(map(lambda x: x.get_type(s), self.template_params))
            mangled = mangle_function(self.name.parse_node.tok_val[0].tok_val[0].tok_val[1], type_parameters)
            return s.lookup(mangled);
        else: return resolved

    def get_internal_fnty(self, s):
        resolved_type = self.find_function_type(s)
        if isinstance(resolved_type, UninstantiatedFunction):
            ## Find the type by applying the template parameters
            subscope = s.subscope()

            type_parameters = list(map(lambda x: x.get_type(s), self.template_params))

            ## First figure out type params and add them all to the scope before
            ## trying to gen the fn signature (since returntype / args might depend
            ## on the template params)
            if resolved_type.fn_declaration.template_parameter_decl_list:
                for i, f in enumerate(resolved_type.fn_declaration.template_parameter_decl_list):
                    subscope.set(f.name, Var(type_parameters[i], name=f.name))

            ## Create the function type
            internal_fnty = resolved_type.fn_declaration.fn_signature.codegen(subscope)

            return internal_fnty
        else: return resolved_type

    def get_type(self, s):
        return self.get_internal_fnty(s).return_type

    ## Gets the receiver for this fncall if there is one - otherwise, None
    def get_receiver(self, m, s, b):
        if not isinstance(self.name, AstQualifiedName):
            return self.implicit_receiver
        resolved = self.name.resolve_minus(m, s, b, 1)
        if not resolved: return None
        else:
            val = resolved.val
            for ii in range(resolved.var_type.num_ptr()):
                val = b.load(val)
            return val

    def codegen(self, m, s, b, exp_ty=None):
        print(self.name.parse_node.to_string())
        # Find function
        fn = self.find_function(m, s, b)
        internal_fnty = self.get_internal_fnty(s)
        if isinstance(fn.var_type, UninstantiatedFunction):
            ## Instantiate this type first. Figure out type params.
            ## No type inference for now - just codegen it all
            concrete_template_params = list(map(lambda x: x.get_type(s), self.template_params))
            fn_instantiation = fn.var_type.fn_declaration.instantiate(m, s, concrete_template_params)
            # Create args
            args = []
            receiver = self.get_receiver(m, s, b)
            if receiver: args.append(receiver)
            for ii, a in enumerate(self.param_list):
                args.append(a.codegen(m, s, b, exp_ty=internal_fnty.args[ii]))
            # Call the function
            return gen_coercion(b, b.call(fn_instantiation, args), self.get_type(s), exp_ty)
        else: # Just call, no need to instantiate
            # Create args
            args = []
            for ii, a in enumerate(self.param_list):
                args.append(a.codegen(m, s, b, exp_ty=internal_fnty.args[ii]))
            # Call the function
            return gen_coercion(b, b.call(fn.val, args), self.get_type(s), exp_ty)

class AstMake(AstNode):
    def __init__(self, typename, field_vals, decoration):
        super().__init__(decoration)
        self.typename = typename
        self.field_vals = field_vals
    def get_type(self, s): return self.typename.resolve(s)
    def run_all_comptime(self, m, s):
        self.typename.run_all_comptime(m, s)
        for name in self.field_vals: self.field_vals[name].run_all_comptime(m, s)
    def walk_dfs(self, fn):
        self.typename.walk_dfs(fn)
        for name in self.field_vals: self.field_vals[name].walk_dfs(fn)
        fn(self)
    def codegen(self, m, s, b, exp_ty=None):
        ty = self.get_type(s)
        vals = list(map(lambda x: ir.Constant(ty.get_field_type(x).to_llvm_type(), None), self.field_vals))
        struct_val = ir.Constant.literal_struct(vals)
        for name in self.field_vals:
            ix = ty.get_field_ix(name)
            val = self.field_vals[name].codegen(m, s, b, exp_ty=ty.data.fields[ix].field_type)
            struct_val = b.insert_value(struct_val, val, ix)
        return gen_coercion(b, struct_val, ty, exp_ty)

class AstCStringLit(AstNode):
    def __init__(self, val, decoration):
        super().__init__(decoration)
        self.val = val
        self.internal_ty = PtrType(IntType(8, False))
    def get_type(self, s): return self.internal_ty
    def run_all_comptime(self, m, s): pass
    def codegen(self, m, s, b, exp_ty=None):
        global_name = mangle_global()
        ty = ir.ArrayType(IntType(8, False).to_llvm_type(), len(self.val))
        global_var = ir.GlobalVariable(m, ty, global_name)
        const_str = ir.Constant(ty, bytearray(self.val, encoding="utf8"))
        global_var.initializer = const_str
        return b.gep(global_var, [ir.Constant(ir.IntType(32), 0), ir.Constant(ir.IntType(32), 0)])

class AstExternFnDeclaration(AstNode):
    def __init__(self, name, fn_signature, decoration):
        super().__init__(decoration)
        self.name = name
        self.fn_signature = fn_signature
    def walk_dfs(self, fn):
        self.fn_signature.walk_dfs(fn)
        fn(self)
    def get_type(self, s): return VoidType()
    def run_all_comptime(self, m, s): pass
    def codegen(self, m, s, b, exp_ty=None):
        internal_fnty = self.fn_signature.codegen(s)
        fn = ir.Function(m, internal_fnty.to_llvm_type(), name=self.name)
        s.set(self.name, Var(internal_fnty, val=fn))

class AstIntLit(AstNode):
    def __init__(self, val, decoration):
        super().__init__(decoration)
        self.val = val
        # Find the smallest int type this'll fit into
        num_bits = None
        signed = None
        if self.val < 0:
            signed = False
            if self.val < 2**8: num_bits = 8
            elif self.val < 2**16: num_bits = 16
            elif self.val < 2**32: num_bits = 32
            elif self.val < 2**64: num_bits = 64
            else: assert False, "Int overflow"
        else:
            signed = True
            if self.val >= -2**7: num_bits = 8
            if self.val >= -2**15: num_bits = 16
            if self.val >= -2**31: num_bits = 32
            if self.val >= -2**63: num_bits = 64
            else: assert False, "Int overflow"
        ty = IntType(num_bits, signed)
        self.internal_ty = ty

    def get_type(self, s):
        return self.internal_ty

    def run_all_comptime(self, m, s): pass

    def codegen(self, m, s, b, exp_ty=None):
        # Gen the int type and return
        return gen_coercion(b, ir.Constant(self.internal_ty.to_llvm_type(), self.val), self.get_type(s), exp_ty)

class ParameterDecl(AstNode):
    def walk_dfs(self, fn):
        self.type_ident.walk_dfs(fn)
        fn(self)

    def __init__(self, name, type_ident, decoration):
        super().__init__(decoration)
        self.name = name
        self.type_ident = type_ident

## Resolveable expr, ident or qualified ident
class Resolveable(AstNode):
    def __init__(self, p, decoration):
        super().__init__(decoration)
        assert p.is_nterm(NTERM_EXPRESSION) or p.is_nterm(NTERM_IDENTIFIER)
        # The parse node for this type ident.
        self.parse_node = p
    def run_all_comptime(self, m, s): pass
    # @param s - scope
    def resolve(self, s):
        p = self.parse_node
        while p.is_nterm(NTERM_EXPRESSION): p = p.tok_val[0]
        if p.is_nterm(NTERM_IDENTIFIER):
            return s.lookup(p.tok_val[0].tok_val[1])
        else:
            assert False, "Can't resolve type " + self.parse_node.to_string()

## Pair of (bool, str). the bool indicates whether this is a static access
## ('::') or dotted access ('.'). The str indicates the field name. For
## example, 'a.*' would have 1 addition of (False, "*")
class AstQualifiedNameAddition:
    def __init__(self, is_static, name):
        self.is_static = is_static
        self.name = name

    ## Given a type, get the type resulting from the application of this addition
    def get_type_after_apply(self, s, var_type):
        if self.is_static:
            raise NotImplementedError
        else:
            if isinstance(var_type, PtrType):
                if self.name == "*": return var_type.val
                else: return self.get_type_after_apply(s, var_type.val)
            elif isinstance(var_type, StructType):
                ## Find the type of this field
                ret = None
                for ii, f in enumerate(var_type.data.fields):
                    if f.field_name == self.name:
                        ret = f.field_type
                        break
                if not ret:
                    # Try find a matching method (UFCS)
                    ret = s.lookup(self.name)
                    if ret: ret = ret.var_type
                assert ret
                return ret

    ## APply this name addition to the given Var, returning another var
    def apply(self, m, s, b, curr: Var) -> Var:
        if self.is_static:
            raise NotImplementedError
        else:
            if isinstance(curr.var_type, PtrType):
                ## Load from the pointer (this is prbably an alloca, so we're
                ## actually just loading a pointer, but we're treating
                ## everything as an alloca anyway so this'll (probably) get
                ## dereferenced in a later step)
                val = b.load(curr.val)
                ## Figure out the new type
                new_type = curr.var_type.val
                ## Create a new var & ret
                clone = curr.clone()
                clone.var_type = new_type
                clone.val = val
                ## If we're just the '*' field, return now, since we're only
                ## dereferencing - otherwise, gep into the struct too (since we
                ## auto deref pointers when accessing struct fields).
                if self.name == "*": return clone
                return self.apply(m, s, b, clone)
            elif isinstance(curr.var_type, StructType):
                ## Create a GEP - find the offset of the field
                ix = None
                new_type = None
                for ii, f in enumerate(curr.var_type.data.fields):
                    if f.is_function():
                        # Ah, this is a function - in which case, just return a
                        # var with the type being an uninstantiated function
                        clone = curr.clone()
                        clone.var_type = f.field_type
                        return clone
                    if f.field_name == self.name:
                        new_type = f.field_type
                        ix = ii
                        break
                assert ix != None
                assert new_type
                ## Now create the gep , the var, & return
                clone = curr.clone()
                clone.var_type = new_type
                clone.val = b.gep(curr.val, [ir.Constant(ir.IntType(32), 0), ir.Constant(ir.IntType(32), ix)])
                return clone

class AstQualifiedName(AstNode):
    def __init__(self, base_name: Resolveable, additions: List[AstQualifiedNameAddition], decoration):
        super().__init__(decoration)
        self.base_name = base_name
        self.additions = additions

    def run_all_comptime(self, m, s): pass

    def get_type(self, s):
        ret = self.base_name.resolve(s).var_type
        ## Loop and apply additions - each addition returns another Var
        for i, a in enumerate(self.additions):
            ret = a.get_type_after_apply(s, ret)
        return ret

    ## Resolve this as a Var
    def resolve(self, m, s, b):
        return self.resolve_minus(m, s, b, 0)

    ## Like resolve, but resolves all but n steps, where n is an integer
    def resolve_minus(self, m, s, b, n):
        ret = self.base_name.resolve(s)
        ## Loop and apply additions - each addition returns another Var
        for i, a in enumerate(self.additions):
            if i >= len(self.additions) - n: break
            ret = a.apply(m, s, b, ret)
        return ret

    ## Codegen as a variable
    def codegen(self, m, s, b, exp_ty=None, lval=False):
        ret = self.resolve(m, s, b)
        ## Get the LLVM value + type for this var
        ret_type = ret.var_type
        ret = ret.val
        if not lval: ret = b.load(ret)
        return gen_coercion(b, ret, ret_type, exp_ty)

class TypeIdent(Resolveable):
    def run_all_comptime(self, m, s): pass
    ## Return the lang type for this type
    def resolve(self, s) -> Type:
        assert self.parse_node.is_nterm(NTERM_EXPRESSION) or self.parse_node.is_nterm(NTERM_IDENTIFIER)
        if self.parse_node.is_nterm(NTERM_IDENTIFIER):
            return s.lookup(self.parse_node.tok_val[0].tok_val[1]).var_type.val
        if self.parse_node.tok_val[0].is_nterm(NTERM_IDENTIFIER):
            return s.lookup(self.parse_node.tok_val[0].tok_val[0].tok_val[1]).var_type.val
        elif self.parse_node.tok_val[0].is_nterm(NTERM_META_TYPE_IDENT):
            return s.lookup("$" + self.parse_node.tok_val[0].tok_val[1].tok_val[1]).var_type.val
        elif self.parse_node.tok_val[0].is_nterm(NTERM_OP) and \
             self.parse_node.tok_val[0].tok_val[0].is_term("&"):
            ## Pointer type
            assert self.parse_node.tok_val[1].tok_val[0].is_nterm(NTERM_IDENTIFIER)
            resolved = s.lookup(self.parse_node.tok_val[1].tok_val[0].tok_val[0].tok_val[1]).var_type.val
            return resolved.ptr()
        else: assert False, "Unimpl type ident resolve for " + self.parse_node.to_string()

class VarIdent(Resolveable):
    def run_all_comptime(self, m, s): pass
    def get_type(self, s):
        return self.resolve(s).var_type
    def codegen(self, m, s, b, exp_ty=None, lval=False):
        resolved = self.resolve(s)
        if resolved.is_comptime:
            # If it's a comptime var, then .val will just be a constant, in which case don't bother load.
            return gen_coercion(b, resolved.val, self.get_type(s), exp_ty)
        else:
            if lval:
                return gen_coercion(b, resolved.val, self.get_type(s), exp_ty)
            else:
                ## If this is an rvalue, but we're coercing to a pointer, don't bother load
                if isinstance(exp_ty, PtrType) and exp_ty.val.eq(self.get_type(s)):
                    return resolved.val
                else:
                    return gen_coercion(b, b.load(resolved.val, name=self.resolve(s).name), self.get_type(s), exp_ty)

class BinaryExpression(AstNode):
    def run_all_comptime(self, m, s):
        ty = self.lhs.get_type(s)
        rhs_ty = self.rhs.get_type(s)
        self.lhs.run_all_comptime(m, s)
        self.rhs.run_all_comptime(m, s)
        if self.op in ty.operator_overloads:
            ## Select overload based on arg type
            overloads = ty.operator_overloads[self.op]
            for overload in overloads:
                if len(overload.arg_types) == 1 and can_coerce(rhs_ty, overload.arg_types[0]):
                    overload.fn_declaration.instantiate(m, s, [])

    def __init__(self, lhs, op, rhs, decoration):
        super().__init__(decoration)
        self.lhs = lhs
        self.op = op
        self.rhs = rhs

    def walk_dfs(self, fn):
        self.lhs.walk_dfs(fn)
        self.rhs.walk_dfs(fn)
        fn(self)

    def get_type(self, s):
        if self.op == "+" or self.op == "-" or self.op == "/" or self.op == "*":
            return self.lhs.get_type(s)
        else: return BoolType()

    def codegen(self, m, s, b, exp_ty=None):
        ## Switch type, then switch on op
        ty = self.lhs.get_type(s)
        rhs_ty = self.rhs.get_type(s)
        if self.op in ty.operator_overloads:
            overloads = ty.operator_overloads[self.op]
            fn_declaration = None
            ## Select overload based on arg type
            for overload in overloads:
                if len(overload.arg_types) == 1 and can_coerce(rhs_ty, overload.arg_types[0]):
                    fn_declaration = overload.fn_declaration
            if fn_declaration:
                ## TODO Figure out what to do with template params here - we need
                ## type inference
                fn = fn_declaration.instantiate(m, s, [])
                internal_fnty = fn_declaration.fn_signature.codegen(s)
                args = []
                lhs_arg = self.lhs.codegen(m, s, b, exp_ty=internal_fnty.args[0])
                rhs_arg = self.rhs.codegen(m, s, b, exp_ty=internal_fnty.args[1])
                return gen_coercion(b, b.call(fn, [lhs_arg, rhs_arg]), internal_fnty.return_type, exp_ty)
        if isinstance(ty, IntType):
            if self.op == "+":
                return gen_coercion(b, b.add(self.lhs.codegen(m, s, b), self.rhs.codegen(m, s, b, exp_ty=self.lhs.get_type(s)), "binop(" + ty.name + ")"), self.get_type(s), exp_ty)
            elif self.op == "-":
                return gen_coercion(b, b.sub(self.lhs.codegen(m, s, b), self.rhs.codegen(m, s, b, exp_ty=self.lhs.get_type(s)), "binop(" + ty.name + ")"), self.get_type(s), exp_ty)
            elif self.op == "/":
                if ty.is_signed: return gen_coercion(b, b.div(self.lhs.codegen(m, s, b), self.rhs.codegen(m, s, b, exp_ty=self.lhs.get_type(s)), "binop(" + ty.name + ")"), self.get_type(s), exp_ty)
                else: return gen_coercion(b, b.udiv(self.lhs.codegen(m, s, b), self.rhs.codegen(m, s, b, exp_ty=self.lhs.get_type(s)), "binop(" + ty.name + ")"), self.get_type(s), exp_ty)
            elif self.op == "*":
                return gen_coercion(b, b.mul(self.lhs.codegen(m, s, b), self.rhs.codegen(m, s, b, exp_ty=self.lhs.get_type(s)), "binop(" + ty.name + ")"), self.get_type(s), exp_ty)
            elif self.op == ">" or self.op == "<" or self.op == ">=" or self.op == "<=" or self.op == "==" or self.op == "!=":
                if ty.is_signed: return gen_coercion(b, b.icmp_signed(self.op, self.lhs.codegen(m, s, b), self.rhs.codegen(m, s, b, exp_ty=self.lhs.get_type(s)), "cmp(" + ty.name + ")"), self.get_type(s), exp_ty)
                else: return gen_coercion(b, b.icmp_unsigned(self.op, self.lhs.codegen(m, s, b), self.rhs.codegen(m, s, b, exp_ty=self.lhs.get_type(s)), "cmp(" + ty.name + ")"), self.get_type(s), exp_ty)
            else: raise NotImplementedError("Unimpl op " + self.op + " for type " + ty.name)
        elif isinstance(ty, FloatType):
            if self.op == "+":
                return gen_coercion(b, b.fadd(self.lhs.codegen(m, s, b), self.rhs.codegen(m, s, b, exp_ty=self.lhs.get_type(s)), "binop(" + ty.name + ")"), self.get_type(s), exp_ty)
            elif self.op == "-":
                return gen_coercion(b, b.fsub(self.lhs.codegen(m, s, b), self.rhs.codegen(m, s, b, exp_ty=self.lhs.get_type(s)), "binop(" + ty.name + ")"), self.get_type(s), exp_ty)
            elif self.op == "/":
                return gen_coercion(b, b.fdiv(self.lhs.codegen(m, s, b), self.rhs.codegen(m, s, b, exp_ty=self.lhs.get_type(s)), "binop(" + ty.name + ")"), self.get_type(s), exp_ty)
            elif self.op == "*":
                return gen_coercion(b, b.fmul(self.lhs.codegen(m, s, b), self.rhs.codegen(m, s, b, exp_ty=self.lhs.get_type(s)), "binop(" + ty.name + ")"), self.get_type(s), exp_ty)
        elif isinstance(ty, KindType):
            if self.op == "==":
                lhs_type = self.lhs.get_type(s).val
                rhs_type = self.rhs.get_type(s).val
                eq = lhs_type.eq(rhs_type)
                return ir.Constant(BoolType().to_llvm_type(), 1 if eq else None)
        else: raise NotImplementedError("Unimpl op " + self.op + " for type " + ty.name)



