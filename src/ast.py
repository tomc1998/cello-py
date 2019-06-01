from llvmlite import ir
from typing import List
from parser import *
from lang_type import *
from mangle import mangle_function, mangle_global
from scope import Var
import jit

class AstNode:
    def walk_dfs(self, fn): fn(self)
    def __init__(self, decoration):
        self.decoration = decoration

# Asserts if not possible
# If to_ty == None, return val
def gen_coercion(b, val, from_ty, to_ty):
    if not to_ty: return val
    if from_ty == to_ty or from_ty.eq(to_ty):
        return val;
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

## Basically just a list of AstConditionals with an optional fallback (else clause)
class AstIf(AstNode):
    def __init__(self, conditions, decoration, fallback=None):
        super().__init__(decoration)
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
    def __init__(self, var, val, decoration):
        super().__init__(decoration)
        self.var = var
        self.val = val
    def get_type(self, s): return None
    def walk_dfs(self, fn):
        self.var.walk_dfs(fn)
        self.val.walk_dfs(fn)
        fn(self)
    def codegen(self, m, s, b, exp_ty=None):
        return b.store(self.val.codegen(m, s, b), self.var.codegen(m, s, b, lval=True))
    def run_all_comptime(self, m, s):
        self.var.run_all_comptime(m, s)
        self.val.run_all_comptime(m, s)

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
    def get_type(self, s): return None
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

    def get_type(self, s):
        resolved_fields = list(map(lambda x: x.get_type(s), self.fields))
        return KindType(StructType(StructData(resolved_fields)))

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
    def get_type(self, s):
        return None
    def codegen(self, m, s, b, exp_ty=None):
        # Figure out the type
        var_type = None
        if self.declared_type: var_type = self.declared_type.resolve(s)
        else: var_type = self.val.get_type(s)
        # Coerce val?
        actual_val = None
        if self.declared_type:
            actual_val = self.val.codegen(m, s, b, exp_ty=var_type)
        else:
            actual_val = self.val.codegen(m, s, b)
        ## Alloc var on stack
        alloca = b.alloca(var_type.to_llvm_type())
        ## Store val
        b.store(actual_val, alloca)
        s.set(self.name, Var(var_type, val=alloca, is_mutable=self.is_mut, is_comptime=self.is_comptime, name=self.name))

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
        s.set(self.name, Var(self.definition.get_type(s)))

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
        def preamble(m):
            ## Walk the body & find all fncalls so we can fwd decl them all
            def walk_fn(x):
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
            body.walk_dfs(walk_fn)

        subscope = s.subscope();
        ty = self.body.get_type(s)
        jit_val = jit.jit_node(self.body, ty, subscope, preamble)
        self.computed = jit_val
    def codegen(self, m, s, b, exp_ty=None):
        assert self.computed != None
        ty = self.body.get_type(s)
        if isinstance(ty, VoidType): return
        return gen_coercion(b, self.computed, ty, exp_ty)

class AstFnDeclaration(AstNode):
    def __init__(self, fn_name, template_parameter_decl_list, fn_signature, body, decoration):
        super().__init__(decoration)
        self.fn_name = fn_name
        self.template_parameter_decl_list = template_parameter_decl_list
        self.fn_signature = fn_signature
        self.body = body
        ## A map of name mangles to instantiated functions (unused if template_parameter_decl_list == None or len 0)
        self.instantiated = {}

    def walk_dfs(self, fn):
        self.template_parameter_decl_list.walk_dfs(fn)
        self.fn_signature.walk_dfs(fn)
        self.body.walk_dfs(fn)
        fn(self)

    def get_type(self, s): return None

    def run_all_comptime(self, m, s): pass

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
        ## Mangle the func name
        mangled = mangle_function(self.fn_name, list(map(lambda x: x.val, type_parameters)))

        ## Check if we have an instantiation cached
        if mangled in self.instantiated: return self.instantiated[mangled]

        ## Instantiate, so run the comptime stuff for the body in a subscope
        comptime_subscope = s.subscope()
        jit.push_jit_env()
        jit.add_module(m)
        self.body.run_all_comptime(m, comptime_subscope)
        jit.pop_jit_env()

        fnty = internal_fnty.to_llvm_type()
        ## Create the function
        fn = ir.Function(m, fnty, name=mangled)
        ## Add to (parent) scope
        parent_scope.set(mangled, Var(internal_fnty, fn))
        entry_block = fn.append_basic_block(name="entry")
        b = ir.IRBuilder(entry_block)

        ## Insert args into the scope, storing as allocas
        args = zip(fn.args, self.fn_signature.parameter_decl_list)
        for_scope = map(lambda x : (x[1].name, Var(x[1].type_ident.resolve(s), name=x[1].name, val=x[0])), args)
        for name, var in for_scope:
            llvm_ty = var.var_type.to_llvm_type()
            alloca = b.alloca(llvm_ty, name=name)
            b.store(var.val, alloca)
            var.val = alloca
            s.set(name, var)

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
            s.set(self.fn_name, Var(UninstantiatedFunction(self)))

class AstFnSignature(AstNode):
    def __init__(self, parameter_decl_list, is_mut, is_extern, return_type, decoration):
        super().__init__(decoration)
        self.parameter_decl_list = parameter_decl_list
        self.is_mut = is_mut
        self.is_extern = is_extern
        self.return_type = return_type

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
        return FunctionType(ret, args, self.is_extern)

class AstFnInstantiation(AstNode):
    def __init__(self, name, template_params):
        self.name = name
        self.template_params = template_params

    def get_type(self, s): return None

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
        fn = self.find_function(s)
        ## If we can't find the function, chances are this is recursive, & we
        ## don't (currently) support comptime eval for recursive calls. This
        ## would only make sense if we're instantiating functions
        ## recursively... not sure if we wantt his / need this, either way
        ## sounds like a faff. Just ignore for now.
        ## TODO if recursive comptime evaluation is needed, needs to be put in here
        if not fn: return
        if isinstance(fn.var_type, UninstantiatedFunction):
            internal_fnty = self.get_internal_fnty(s)
            concrete_template_params = list(map(lambda x: x.get_type(s), self.template_params))
            fn.var_type.fn_declaration.instantiate(m, s, concrete_template_params)

    ## Find the associated function Var
    def find_function(self, s):
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
        resolved = self.find_function(s)
        if isinstance(resolved.var_type, UninstantiatedFunction):
            ## Find the type by applying the template parameters
            subscope = s.subscope()

            type_parameters = list(map(lambda x: x.get_type(s), self.template_params))

            ## First figure out type params and add them all to the scope before
            ## trying to gen the fn signature (since returntype / args might depend
            ## on the template params)
            if resolved.var_type.fn_declaration.template_parameter_decl_list:
                for i, f in enumerate(resolved.var_type.fn_declaration.template_parameter_decl_list):
                    subscope.set(f.name, Var(type_parameters[i], name=f.name))

            ## Create the function type
            internal_fnty = resolved.var_type.fn_declaration.fn_signature.codegen(subscope)

            return internal_fnty
        else: return resolved.var_type

    def get_type(self, s):
        return self.get_internal_fnty(s).return_type

    def codegen(self, m, s, b, exp_ty=None):
        # Find function
        fn = self.find_function(s)
        internal_fnty = self.get_internal_fnty(s)
        if isinstance(fn.var_type, UninstantiatedFunction):
            ## Instantiate this type first. Figure out type params.
            ## No type inference for now - just codegen it all
            concrete_template_params = list(map(lambda x: x.get_type(s), self.template_params))
            fn_instantiation = fn.var_type.fn_declaration.instantiate(m, s, concrete_template_params)
            # Create args
            args = []
            for ii, a in enumerate(self.param_list):
                args.append(gen_coercion(b, a.codegen(m, s, b), a.get_type(s), internal_fnty.args[ii]))
            # Call the function
            return gen_coercion(b, b.call(fn_instantiation, args), self.get_type(s), exp_ty)
        else: # Just call, no need to instantiate
            # Create args
            args = []
            for ii, a in enumerate(self.param_list):
                args.append(gen_coercion(b, a.codegen(m, s, b), a.get_type(s), internal_fnty.args[ii]))
            # Call the function
            return gen_coercion(b, b.call(fn.val, args), self.get_type(s), exp_ty)

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
    def __init__(self, fn_name, fn_signature, decoration):
        super().__init__(decoration)
        self.fn_name = fn_name
        self.fn_signature = fn_signature
    def walk_dfs(self, fn):
        self.fn_signature.walk_dfs(fn)
        fn(self)
    def get_type(self, s): return None
    def run_all_comptime(self, m, s): pass
    def codegen(self, m, s, b, exp_ty=None):
        internal_fnty = self.fn_signature.codegen(s)
        fn = ir.Function(m, internal_fnty.to_llvm_type(), name=self.fn_name)
        s.set(self.fn_name, Var(internal_fnty, val=fn))

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
                ## Create a GEP - find the offset of the field
                ret = None
                for ii, f in enumerate(var_type.data.fields):
                    if f.field_name == self.name:
                        ret = f.field_type
                        break
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

    ## Codegen as a variable
    def get_type(self, s):
        ret = self.base_name.resolve(s).var_type
        ## Loop and apply additions - each addition returns another Var
        for i, a in enumerate(self.additions):
            ret = a.get_type_after_apply(s, ret)
        return ret

    ## Codegen as a variable
    def codegen(self, m, s, b, exp_ty=None, lval=False):
        ret = self.base_name.resolve(s)
        ## Loop and apply additions - each addition returns another Var
        for i, a in enumerate(self.additions):
            ret = a.apply(m, s, b, ret)
        ## Get the LLVM value + type for this var
        ret_type = ret.var_type
        ret = ret.val
        if not lval:
            ret = b.load(ret)
        return gen_coercion(b, ret, ret_type, exp_ty)

class TypeIdent(Resolveable):
    def run_all_comptime(self, m, s): pass
    ## Return the lang type for this type
    def resolve(self, s) -> Type:
        assert self.parse_node.is_nterm(NTERM_EXPRESSION) or self.pares_node.is_nterm(NTERM_IDENTIFIER)
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
        else: assert False, "Unimpl"

class VarIdent(Resolveable):
    def run_all_comptime(self, m, s): pass
    def get_type(self, s):
        return self.resolve(s).var_type
    def codegen(self, m, s, b, exp_ty=None, lval=False):
        if lval:
            return gen_coercion(b, self.resolve(s).val, self.get_type(s), exp_ty)
        else:
            return gen_coercion(b, b.load(self.resolve(s).val, name=self.resolve(s).name), self.get_type(s), exp_ty)

class BinaryExpression(AstNode):
    def run_all_comptime(self, m, s):
        self.lhs.run_all_comptime(m, s)
        self.rhs.run_all_comptime(m, s)
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
        else: raise NotImplementedError("Unimpl op " + self.op + " for type " + ty.name)



