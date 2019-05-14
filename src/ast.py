from llvmlite import ir
from parser import *
from lang_type import *
from scope import Var

class AstNode: pass

class AstProgram(AstNode):
    def __init__(self, children):
        self.children = children

    def codegen(self, m, s, b):
        ret = None
        for i, c in enumerate(self.children):
            ret = c.codegen(m, s, b)
        return ret

    def get_type(self, s):
        return self.children[len(self.children)-1].get_type(s)

## Basically just a list of AstConditionals with an optional fallback (else clause)
class AstIf(AstNode):
    def __init__(self, conditions, fallback=None):
        self.conditions = conditions
        self.fallback = fallback

    def get_type(self, s):
        return self.conditions[0].body.get_type(s)

    def codegen(self, m, s, b):
        ty = self.get_type(s).to_llvm_type()
        after_block = b.append_basic_block("after")
        incoming = []
        for cond in self.conditions:
            incoming.append(cond.codegen(m, s, b, after_block, ty))
        if self.fallback:
            incoming.append((self.fallback.codegen(m, s, b), b.block))
        b.branch(after_block)
        b.position_at_start(after_block)
        if self.fallback:
            phi = b.phi(ty, name="if-chain-result")
            for (val, block) in incoming:
                print(block.name)
                phi.add_incoming(val, block)
            return phi
        else:
            return None

## Execute some code if the given condition is true
class AstConditional(AstNode):
    def __init__(self, cond, body):
        self.cond = cond
        self.body = body
    def codegen(self, m, s, b, after_block, ty):
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
        return (true_val, true_block)

class AstFnDeclaration(AstNode):
    def __init__(self, fn_name, template_parameter_decl_list, fn_signature, body):
        self.fn_name = fn_name
        self.template_parameter_decl_list = template_parameter_decl_list
        self.fn_signature = fn_signature
        self.body = body

    def codegen(self, m, s, b=None):
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

class AstFunctionCall(AstNode):
    def __init__(self, name, template_params, param_list):
        assert not template_params, "Template params not implemented"
        self.name = name
        self.template_params = template_params
        self.param_list = param_list

    def codegen(self, m, s, b):
        # Find function
        fn = self.name.resolve(s).val
        # Create args
        args = []
        for a in self.param_list: args.append(a.codegen(m, s, b))
        # Call the function
        return b.call(fn, args)

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

class FuncIdent(Resolveable): pass

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
            elif self.op == ">" or self.op == "<" or self.op == ">=" or self.op == "<=" or self.op == "==" or self.op == "!=":
                if ty.is_signed: return b.icmp_signed(self.op, self.lhs.codegen(m, s, b), self.rhs.codegen(m, s, b), "cmp(" + ty.name + ")")
                else: return b.icmp_unsigned(self.op, self.lhs.codegen(m, s, b), self.rhs.codegen(m, s, b), "cmp(" + ty.name + ")")
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


