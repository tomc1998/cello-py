from llvmlite import ir
from parser import *
from lang_type import *
from scope import Var
import jit

class AstNode:
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
        return gen_coercion(b, ret, self.get_type(s), exp_ty)

    def get_type(self, s):
        return self.children[len(self.children)-1].get_type(s)

## Basically just a list of AstConditionals with an optional fallback (else clause)
class AstIf(AstNode):
    def __init__(self, conditions, decoration, fallback=None):
        super().__init__(decoration)
        self.conditions = conditions
        self.decoration = decoration
        self.fallback = fallback

    def get_type(self, s):
        return self.conditions[0].body.get_type(s)

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
                print(block.name)
                phi.add_incoming(val, block)
            return gen_coercion(b, phi, self.get_type(s), exp_ty)
        else:
            return None

class AstStructMemberVar(AstNode):
    def __init__(self, name, type_expr):
        self.name = name
        self.type_expr = type_expr

    def get_type(self, s):
        return self.type_expr.get_type(s)

# A struct declaration, returns the struct when evaluated with get_type
class AstStructDeclaration(AstNode):
    def __init__(self, name, fields):
        self.name = name
        self.fields = fields

    def get_type(self, s):
        resolved_fields = map(fields, lambda x: x.get_type(s))
        return StructType(self.name, StructData(resolved_fields))

class AstTypeDeclaration(AstNode):
    def __init__(self, name, definition, is_export=False):
        self.name = name
        self.definition = definition
        self.is_export = is_export
    def codegen(self, m, s, b, exp_ty=None):
        s.set(self.name, Var(self.definition.get_type(s)))

## Execute some code if the given condition is true
class AstConditional(AstNode):
    def __init__(self, cond, body, decoration):
        super().__init__(decoration)
        self.cond = cond
        self.body = body
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

class AstFnDeclaration(AstNode):
    def __init__(self, fn_name, template_parameter_decl_list, fn_signature, body, decoration):
        super().__init__(decoration)
        self.fn_name = fn_name
        self.template_parameter_decl_list = template_parameter_decl_list
        self.fn_signature = fn_signature
        self.body = body

    def get_type(self, s): return None

    def codegen(self, m, s, b):
        ## Create the function type
        internal_fnty = self.fn_signature.codegen(m, s)
        fnty = internal_fnty.to_llvm_type()
        ## Create the function
        fn = ir.Function(m, fnty, name=self.fn_name)
        ## Add to scope
        s.set(self.fn_name, Var(internal_fnty, fn))
        entry_block = fn.append_basic_block(name="entry")
        b = ir.IRBuilder(entry_block)

        ## Insert args into the scope, storing as allocas
        subscope = s.subscope()
        args = zip(fn.args, self.fn_signature.parameter_decl_list)
        for_scope = map(lambda x : (x[1].name, Var(x[1].type_ident.resolve(s), name=x[1].name, val=x[0])), args)
        for name, var in for_scope:
            llvm_ty = var.var_type.to_llvm_type()
            alloca = b.alloca(llvm_ty, name=name)
            b.store(var.val, alloca)
            var.val = alloca
            subscope.set(name, var)

        b.ret(self.body.codegen(m, subscope, b, exp_ty=internal_fnty.return_type))

        return fn

class AstFnSignature(AstNode):
    def __init__(self, parameter_decl_list, is_mut, return_type, decoration):
        super().__init__(decoration)
        self.parameter_decl_list = parameter_decl_list
        self.is_mut = is_mut
        self.return_type = return_type

    def codegen(self, m, s):
        ret = self.return_type.resolve(s)
        args = [pdecl.type_ident.resolve(s) for pdecl in self.parameter_decl_list]
        return FunctionType(ret, args)

class AstFunctionCall(AstNode):
    def __init__(self, name, template_params, param_list, decoration):
        super().__init__(decoration)
        assert not template_params, "Template params not implemented"
        self.name = name
        self.template_params = template_params
        self.param_list = param_list

    def get_type(self, s):
        return self.name.resolve(s).var_type.return_type

    def codegen(self, m, s, b, exp_ty=None):
        # Find function
        fn = self.name.resolve(s).val
        # Create args
        args = []
        for a in self.param_list: args.append(a.codegen(m, s, b))
        # Call the function
        return gen_coercion(b, b.call(fn, args), self.get_type(s), exp_ty)

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

    def codegen(self, m, s, b, exp_ty=None):
        # Gen the int type and return
        return gen_coercion(b, ir.Constant(self.internal_ty.to_llvm_type(), self.val), self.get_type(s), exp_ty)

class ParameterDecl(AstNode):
    def __init__(self, name, type_ident, decoration):
        super().__init__(decoration)
        self.name = name
        self.type_ident = type_ident

class Resolveable(AstNode):
    def __init__(self, p, decoration):
        super().__init__(decoration)
        assert p.is_nterm(NTERM_EXPRESSION)
        # If not none, this type has already been resolved (looked up)
        self.resolved = None
        # The parse node for this type ident.
        self.parse_node = p
    # Resolve this type, and return the resolved type, or none if not found.
    # If this has already resolved, just return the old result.
    # @param s - scope
    def resolve(self, s):
        if self.resolved: return self.resolved
        n = self.parse_node.tok_val[0]
        if n.is_nterm(NTERM_IDENTIFIER):
            self.resolved = s.lookup(n.tok_val[0].tok_val[1])
        else:
            assertFalse, "Can't resolve type " + self.parse_node.to_string()
        return self.resolved

class TypeIdent(Resolveable):
    ## Return the LLVM value for this type.
    def resolve(self, s):
        ## First, resolve us
        resolved = super().resolve(s)
        ## Now convert that to an LLVM type, assuming resolved is actually a type (?)
        assert isinstance(resolved.var_type, KindType)
        return self.resolved.var_type.val

class FuncIdent(Resolveable): pass

class VarIdent(Resolveable):
    def get_type(self, s):
        return self.resolve(s).var_type
    def codegen(self, m, s, b, exp_ty=None, lval=False):
        if lval:
            return gen_coercion(b, self.resolve(s).val, self.get_type(s), exp_ty)
        else:
            return gen_coercion(b, b.load(self.resolve(s).val, name=self.resolve(s).name), self.get_type(s), exp_ty)

class BinaryExpression(AstNode):
    def __init__(self, lhs, op, rhs, decoration):
        super().__init__(decoration)
        self.lhs = lhs
        self.op = op
        self.rhs = rhs

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



