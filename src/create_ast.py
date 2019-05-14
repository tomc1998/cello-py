from ast import *
from parser import *

def ast_assert(val, msg=""):
    assert val, msg ;

## Given an expression parse node, try to extract a type identifier (i.e.
## 'i32', or 'my_type_fn()' where my_type_fn returns a type).
def create_type_ident(p):
    return TypeIdent(p);

def create_fn_signature(p):
    assert p.is_nterm(NTERM_FN_SIGNATURE)
    parameter_decl_list = create_parameter_decl_list(p.tok_val[0]);
    ii = 1
    is_mut = False
    if p.tok_val[ii].is_term("mut"):
        is_mut = True
        ii += 1
    return_val = None
    if p.tok_val[ii].is_term("->"):
        ii += 1
        return_val = create_type_ident(p.tok_val[ii])
    return AstFnSignature(parameter_decl_list, is_mut, return_val);

def create_parameter_decl_list(p):
    assert p.is_nterm(NTERM_PARAMETER_DECL_LIST)
    ii = 1
    ret = []
    while True:
        pdecl = p.tok_val[ii]
        jj = 0
        assert not pdecl.tok_val[jj].is_term("comptime"), "Unimpl"
        pname = None
        ptype = None
        if pdecl.tok_val[jj].is_nterm(NTERM_IDENTIFIER):
            ## [comptime] Name: Type
            pname = pdecl.tok_val[jj].tok_val[0].tok_val[1]
            jj += 2
            ptype = create_type_ident(pdecl.tok_val[jj])
            jj += 1
        else:
            ## [comptime] Type
            ptype = create_type_ident(pdecl.tok_val[jj])
            jj += 1
        ret.append(ParameterDecl(pname, ptype))
        if p.tok_val[ii+1].is_term(")"): break
        ast_assert(p.tok_val[ii+1].is_term(","))
        ii += 2
    return ret

def create_op(p):
    assert(p.is_nterm(NTERM_OP));
    return p.tok_val[0].tok_val[1]

def create_binary_expression(p):
    assert p.is_nterm(NTERM_BINARY_EXPRESSION)
    lhs = create_expression(p.tok_val[0])
    op = create_op(p.tok_val[1])
    rhs = create_expression(p.tok_val[2])
    return BinaryExpression(lhs, op, rhs)

## This actually creates an AstProgram, since they're identical, but expects surrounding {}
def create_statement_list(p):
    assert p.is_nterm(NTERM_STATEMENT_LIST)
    children = []
    ii = 1
    while ii < len(p.tok_val) and not p.tok_val[ii].is_term("}"):
        children.append(create_statement(p.tok_val[ii]))
        ii += 1
        while ii < len(p.tok_val) and p.tok_val[ii].is_term(";"): ii += 1
    return AstProgram(children)

def create_if(p):
    if p.is_nterm(NTERM_IF):
        ii = 1
        cond = create_expression(p.tok_val[ii])
        ii += 1
        body = create_statement_list(p.tok_val[ii])
        ii += 1
        conditionals = [AstConditional(cond, body)]
        elifs = []
        while ii < len(p.tok_val) and p.tok_val[ii].tok_type == NTERM_ELIF:
            conditionals.append(create_if(p.tok_val[ii]))
            ii += 1
        else_ = None
        if ii < len(p.tok_val) and p.tok_val[ii].tok_type == NTERM_ELSE:
            else_ = create_if(p.tok_val[ii])
        return AstIf(conditionals, else_)
    elif p.is_nterm(NTERM_ELIF):
        cond = create_expression(p.tok_val[1])
        body = create_statement_list(p.tok_val[2])
        return AstConditional(cond, body)
    elif p.is_nterm(NTERM_ELSE):
        return create_statement_list(p.tok_val[1])

def create_parameter_list(p):
    ii = 1
    params = []
    while ii < len(p.tok_val) and not p.tok_val[ii].is_term(")"):
        if p.tok_val[ii].is_nterm(NTERM_EXPRESSION):
            params.append(create_expression(p.tok_val[ii]))
        ii += 1
    return params

def create_function_call(p):
    assert p.is_nterm(NTERM_FUNCTION_CALL)
    assert p.tok_val[0].tok_val[0].is_nterm(NTERM_IDENTIFIER), "Function call where function name isn't ident unimpl"
    function_name = FuncIdent(p.tok_val[0])
    template_params = None
    ii = 1
    if p.tok_val[ii].is_term("::"):
        ii += 1
        template_params = create_template_parameter_list(p.tok_val[ii])
        ii += 1

    assert not template_params, "Template params not implemented"
    param_list = create_parameter_list(p.tok_val[ii])
    return AstFunctionCall(function_name, template_params, param_list)

def create_literal(p):
    assert p.is_nterm(NTERM_LITERAL)
    tok = p.tok_val[0].tok_val
    if tok[0] == "int_lit":
        return AstIntLit(int(tok[1]))
    else:
        assert False, "Unimpl lit type: '" + tok[0] + "'"

def create_expression(p):
    assert p.is_nterm(NTERM_EXPRESSION)
    if p.tok_val[0].is_nterm(NTERM_IDENTIFIER):
        return VarIdent(p)
    elif p.tok_val[0].is_nterm(NTERM_BINARY_EXPRESSION):
        return create_binary_expression(p.tok_val[0])
    elif p.tok_val[0].is_nterm(NTERM_IF):
        return create_if(p.tok_val[0])
    elif p.tok_val[0].is_nterm(NTERM_FUNCTION_CALL):
        return create_function_call(p.tok_val[0])
    elif p.tok_val[0].is_nterm(NTERM_STATEMENT_LIST):
        return create_statement_list(p.tok_val[0])
    elif p.tok_val[0].is_nterm(NTERM_LITERAL):
        return create_literal(p.tok_val[0])
    else:
        assert False, "Unimpl creating expr from " + p.tok_val[0].tok_type


def create_fn_declaration(p):
    ii = 0
    ast_assert(p.tok_val[ii] != "export", "Unimpl");
    assert p.tok_val[ii].is_term("fn")
    ii += 1
    fn_name = p.tok_val[ii].tok_val[0].to_string()
    ii += 1
    template_parameter_decl_list = None
    if p.tok_val[ii].is_nterm(NTERM_TEMPLATE_PARAMETER_DECL_LIST):
        template_parameter_decl_list = create_template_parameter_decl_list(p.tok_val[ii])
        ii += 1
    assert p.tok_val[ii].is_term("=")
    ii += 1
    fn_signature = create_fn_signature(p.tok_val[ii])
    ii += 1
    body = create_expression(p.tok_val[ii]);
    return AstFnDeclaration(fn_name, template_parameter_decl_list, fn_signature, body)

def create_statement(p):
    assert p.is_nterm(NTERM_STATEMENT)
    if p.tok_val[0].is_nterm(NTERM_FN_DECLARATION):
        return create_fn_declaration(p.tok_val[0])
    elif p.tok_val[0].is_nterm(NTERM_EXPRESSION):
        return create_expression(p.tok_val[0])
    assert False

def create_statement_or_processor(p):
    assert p.is_nterm(NTERM_STATEMENT_OR_PREPROCESSOR)
    if p.tok_val[0].is_nterm(NTERM_STATEMENT):
        return create_statement(p.tok_val[0])
    else:
        return create_preprocessor(p.tok_val[0])

def create_program(p):
    children = []
    ii = 0
    while ii < len(p.tok_val):
        children.append(create_statement_or_processor(p.tok_val[ii]))
        ii += 1
        while ii < len(p.tok_val) and p.tok_val[ii].is_term(";"): ii += 1
    return AstProgram(children)

def create_ast(p):
    return create_program(p)

