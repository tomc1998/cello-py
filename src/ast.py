from llvmlite import ir
from parser import *
from lang_type import *
from scope import Var

class AstNode: pass

class AstProgram(AstNode):
    def __init__(self, children):
        self.children = children

    def codegen(self, m, s):
        ret = []
        for c in self.children:
            ret.append(c.codegen(m, s))
        return ret

class AstFnDeclaration(AstNode):
    def __init__(self, fn_name, template_parameter_decl_list, fn_signature, body):
        self.fn_name = fn_name
        self.template_parameter_decl_list = template_parameter_decl_list
        self.fn_signature = fn_signature
        self.body = body

    def codegen(self, m, s):
        ## Create the function type
        internal_fnty = self.fn_signature.codegen(m, s)
        fnty = internal_fnty.to_llvm_type()
        ## Create the function
        fn = ir.Function(m, fnty, name=self.fn_name)
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

        b.ret(self.body.codegen(m, subscope, b))

        return fn

class AstFnSignature(AstNode):
    def __init__(self, parameter_decl_list, is_mut, return_type):
        self.parameter_decl_list = parameter_decl_list
        self.is_mut = is_mut
        self.return_type = return_type

    def codegen(self, m, s):
        ret = self.return_type.resolve(s)
        args = [pdecl.type_ident.resolve(s) for pdecl in self.parameter_decl_list]
        return FunctionType(ret, args)

class ParameterDecl(AstNode):
    def __init__(self, name, type_ident):
        self.name = name
        self.type_ident = type_ident

class Resolveable(AstNode):
    def __init__(self, p):
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
            ast_assert(False, "Can't resolve type " + self.parse_node.to_string())
        return self.resolved

class TypeIdent(Resolveable):
    ## Return the LLVM value for this type.
    def resolve(self, s):
        ## First, resolve us
        resolved = super().resolve(s)
        ## Now convert that to an LLVM type, assuming resolved is actually a type (?)
        assert isinstance(resolved.var_type, KindType)
        return self.resolved.var_type.val

class VarIdent(Resolveable):
    def get_type(self, s):
        return self.resolve(s).var_type
    def codegen(self, m, s, b, lval=False):
        if lval:
            return self.resolve(s).val
        else:
            return b.load(self.resolve(s).val, name=self.resolve(s).name)

class BinaryExpression(AstNode):
    def __init__(self, lhs, op, rhs):
        self.lhs = lhs
        self.op = op
        self.rhs = rhs

    def get_type(self, s):
        return self.lhs.get_type(s)

    def codegen(self, m, s, b):
        ## Switch type, then switch on op
        ty = self.get_type(s)
        if isinstance(ty, IntType):
            if self.op == "+":
                return b.add(self.lhs.codegen(m, s, b), self.rhs.codegen(m, s, b), "binop(" + ty.name + ")")
            elif self.op == "-":
                return b.sub(self.lhs.codegen(m, s, b), self.rhs.codegen(m, s, b), "binop(" + ty.name + ")")
            elif self.op == "/":
                if ty.is_signed: return b.div(self.lhs.codegen(m, s, b), self.rhs.codegen(m, s, b), "binop(" + ty.name + ")")
                else: return b.udiv(self.lhs.codegen(m, s, b), self.rhs.codegen(m, s, b), "binop(" + ty.name + ")")
            elif self.op == "*":
                return b.mul(self.lhs.codegen(m, s, b), self.rhs.codegen(m, s, b), "binop(" + ty.name + ")")
            else: raise NotImplementedError("Unimpl op " + self.op + " for type " + ty.name)
        elif isinstance(ty, FloatType):
            if self.op == "+":
                return b.fadd(self.lhs.codegen(m, s, b), self.rhs.codegen(m, s, b), "binop(" + ty.name + ")")
            elif self.op == "-":
                return b.fsub(self.lhs.codegen(m, s, b), self.rhs.codegen(m, s, b), "binop(" + ty.name + ")")
            elif self.op == "/":
                return b.fdiv(self.lhs.codegen(m, s, b), self.rhs.codegen(m, s, b), "binop(" + ty.name + ")")
            elif self.op == "*":
                return b.fmul(self.lhs.codegen(m, s, b), self.rhs.codegen(m, s, b), "binop(" + ty.name + ")")
        else: raise NotImplementedError("Unimpl op " + self.op + " for type " + ty.name)



