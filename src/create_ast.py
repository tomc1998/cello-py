from ast import *
from parser import *

def ast_assert(val, msg=""):
    assert val, msg;

## Given an expression parse node, try to extract a type identifier (i.e.
## 'i32', or 'my_type_fn()' where my_type_fn returns a type).
def create_type_ident(p):
    return TypeIdent(p, p.sl);

def create_fn_signature(p, is_extern):
    assert p.is_nterm(NTERM_FN_SIGNATURE)
    parameter_decl_list = create_parameter_decl_list(p.tok_val[0]);
    ii = 1
    is_mut = False
    if ii < len(p.tok_val) and p.tok_val[ii].is_term("mut"):
        is_mut = True
        ii += 1
    return_val = None
    if ii < len(p.tok_val) and p.tok_val[ii].is_term("->"):
        ii += 1
        return_val = create_type_ident(p.tok_val[ii])
    return AstFnSignature(parameter_decl_list, is_mut, is_extern, return_val, p.sl);

def create_parameter_decl_list(p):
    assert p.is_nterm(NTERM_PARAMETER_DECL_LIST)
    ii = 1
    ret = []
    while p.tok_val[ii] and not p.tok_val[ii].is_term(")"):
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
        ret.append(ParameterDecl(pname, ptype, p.sl))
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
    return BinaryExpression(lhs, op, rhs, p.sl)

## This actually creates an AstProgram, since they're identical, but expects surrounding {}
def create_statement_list(p):
    assert p.is_nterm(NTERM_STATEMENT_LIST)
    children = []
    ii = 1
    while ii < len(p.tok_val) and not p.tok_val[ii].is_term("}"):
        children.append(create_statement(p.tok_val[ii]))
        ii += 1
        while ii < len(p.tok_val) and p.tok_val[ii].is_term(";"): ii += 1
    return AstProgram(children, p.sl)

def create_if(p):
    if p.is_nterm(NTERM_IF):
        ii = 1
        cond = create_expression(p.tok_val[ii])
        ii += 1
        body = create_statement_list(p.tok_val[ii])
        ii += 1
        conditionals = [AstConditional(cond, body, p.sl)]
        elifs = []
        while ii < len(p.tok_val) and p.tok_val[ii].tok_type == NTERM_ELIF:
            conditionals.append(create_if(p.tok_val[ii]))
            ii += 1
        else_ = None
        if ii < len(p.tok_val) and p.tok_val[ii].tok_type == NTERM_ELSE:
            else_ = create_if(p.tok_val[ii])
        return AstIf(conditionals, p.sl, else_)
    elif p.is_nterm(NTERM_ELIF):
        cond = create_expression(p.tok_val[1])
        body = create_statement_list(p.tok_val[2])
        return AstConditional(cond, body, p.sl)
    elif p.is_nterm(NTERM_ELSE):
        return create_statement_list(p.tok_val[1])

def create_template_parameter_list(p):
    assert p.is_nterm(NTERM_TEMPLATE_PARAMETER_LIST)
    ii = 1
    params = []
    while ii < len(p.tok_val) and not p.tok_val[ii].is_term(">"):
        if p.tok_val[ii].is_nterm(NTERM_EXPRESSION):
            params.append(create_expression(p.tok_val[ii]))
        ii += 1
    return params

def create_parameter_list(p):
    ii = 1
    params = []
    while ii < len(p.tok_val) and not p.tok_val[ii].is_term(")"):
        if p.tok_val[ii].is_nterm(NTERM_EXPRESSION):
            params.append(create_expression(p.tok_val[ii]))
        ii += 1
    return params

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
    ## Find the expression for this function
    if p.tok_val[0].tok_val[0].is_nterm(NTERM_IDENTIFIER):
        function_name = Resolveable(p.tok_val[0], p.sl)
        template_params = None
    else:
        assert p.tok_val[0].is_nterm(NTERM_QUALIFIED_TYPE)
        function_name = Resolveable(p.tok_val[0].tok_val[0], p.sl)
        template_params = create_template_parameter_list(p.tok_val[0].tok_val[2])

    param_list = create_parameter_list(p.tok_val[1])
    return AstFnCall(function_name, template_params, param_list, p.sl)

def create_literal(p):
    assert p.is_nterm(NTERM_LITERAL)
    tok = p.tok_val[0].tok_val
    if tok[0] == "int_lit":
        return AstIntLit(int(tok[1]), p.sl)
    if tok[0] == "c_string_lit":
        return AstCStringLit(tok[1][2:len(tok[1])-1], p.sl)
    else:
        assert False, "Unimpl lit type: '" + tok[0] + "'"

def create_qualified_name(p):
    assert p.is_nterm(NTERM_QUALIFIED_NAME)
    assert (len(p.tok_val) % 2) == 1
    additions = []
    base_name = create_expression(p.tok_val[0])
    ii = 1
    while ii < len(p.tok_val):
        is_static = False
        if p.tok_val[ii].tok_val[1] == "::":
            is_static == True
        else: assert p.tok_val[ii].tok_val[1] == "."
        name = p.tok_val[ii+1].tok_val[0].tok_val[1]
        additions.append(AstQualifiedNameAddition(is_static, name))
        ii += 2
    return AstQualifiedName(base_name, additions, p.sl)

def create_assignment(p):
    assert p.is_nterm(NTERM_ASSIGNMENT)
    var = create_expression(p.tok_val[0])
    val = create_expression(p.tok_val[2])
    return AstAssignment(var, val, p.sl)

def create_comptime(p):
    assert p.is_nterm(NTERM_COMPTIME)
    return AstComptime(create_statement_list(p.tok_val[1]), p.sl)

def create_for_loop(p):
    assert p.is_nterm(NTERM_FOR_LOOP)
    iter_var_name_0 = p.tok_val[1].tok_val[0].tok_val[1]
    iter_var_name_1 = None
    ii = 2
    if p.tok_val[ii].is_term(","):
        ii += 1
        iter_var_name_1 = p.tok_val[ii].tok_val[0].tok_val[1]
        ii += 1
    ii += 1
    iter_expr = create_expression(p.tok_val[ii])
    ii += 1
    body = create_expression(p.tok_val[ii])
    if not iter_var_name_1:
        return AstForLoop(iter_var_name_0, iter_expr, body)
    else:
        return AstForLoop(iter_var_name_1, iter_expr, body, index_var_name=iter_var_name_0)

def create_range(p):
    assert p.is_nterm(NTERM_RANGE)
    start = create_expression(p.tok_val[0])
    end = create_expression(p.tok_val[2])
    return AstRange(start, end)

def create_expression(p):
    while p.is_nterm(NTERM_EXPRESSION): p = p.tok_val[0]
    if p.is_nterm(NTERM_IDENTIFIER):
        return VarIdent(p, p.sl)
    elif p.is_nterm(NTERM_BINARY_EXPRESSION):
        return create_binary_expression(p)
    elif p.is_nterm(NTERM_IF):
        return create_if(p)
    elif p.is_nterm(NTERM_FUNCTION_CALL):
        return create_function_call(p)
    elif p.is_nterm(NTERM_STATEMENT_LIST):
        return create_statement_list(p)
    elif p.is_nterm(NTERM_LITERAL):
        return create_literal(p)
    elif p.is_nterm(NTERM_QUALIFIED_NAME):
        return create_qualified_name(p)
    elif p.is_nterm(NTERM_COMPTIME):
        return create_comptime(p)
    elif p.is_nterm(NTERM_RANGE):
        return create_range(p)
    else:
        assert False, "Unimpl creating expr from " + p.tok_type


def create_template_parameter_decl_list(p):
    assert p.is_nterm(NTERM_TEMPLATE_PARAMETER_DECL_LIST)
    ii = 1
    ret = []
    while p.tok_val[ii] and not p.tok_val[ii].is_term(">"):
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
        ret.append(ParameterDecl(pname, ptype, p.sl))
        if p.tok_val[ii+1].is_term(">"): break
        ast_assert(p.tok_val[ii+1].is_term(","))
        ii += 2
    return ret


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
    fn_signature = create_fn_signature(p.tok_val[ii], False)
    ii += 1
    body = create_expression(p.tok_val[ii]);
    return AstFnDeclaration(fn_name, template_parameter_decl_list, fn_signature, body, p.sl)

def create_extern_fn_declaration(p):
    assert p.is_nterm(NTERM_EXTERN_FN_DECLARATION)
    ii = 0
    assert p.tok_val[ii].is_term("extern")
    ii += 1
    assert p.tok_val[ii].is_term("fn")
    ii += 1
    fn_name = p.tok_val[ii].tok_val[0].to_string()
    ii += 1
    assert p.tok_val[ii].is_term("=")
    ii += 1
    fn_signature = create_fn_signature(p.tok_val[ii], True)
    ii += 1
    return AstExternFnDeclaration(fn_name, fn_signature, p.sl)

# Converts to lang_type.StructField (or a memmber function, but that's unimpl)
def create_struct_field(p):
    assert p.is_nterm(NTERM_STRUCT_FIELD)
    ii = 0
    is_static = False
    if p.tok_val[ii].is_term("static"):
        is_static = True
        ii += 1
    if p.tok_val[ii].is_nterm(NTERM_MEMBER_VAR_DECL):
        var = p.tok_val[ii]
        jj = 0
        field_name = var.tok_val[jj].tok_val[0].tok_val[1]
        jj += 1
        assert var.tok_val[jj].is_term(":")
        jj += 1
        field_type = create_expression(var.tok_val[jj])
        jj += 1
        if jj < len(var.tok_val) and var.tok_val[jj] == "{":
            assert False, "Unimpl bitfield"
        if jj < len(var.tok_val) and var.tok_val[jj] == "=":
            assert False, "Unimpl default value"
        return AstStructMemberVar(field_name, field_type)
    elif p.tok_val[ii].is_nterm(NTERM_FN_DECLARATION):
        print ("Unimpl member fn")
        return None
    else:
        assert False

def create_struct_definition(p):
    assert p.is_nterm(NTERM_STRUCT_DEFINITION)
    ii = 0
    assert p.tok_val[ii].is_term("struct")
    ii += 1
    assert p.tok_val[ii].is_term("{")
    ii += 1
    fields = []
    while not p.tok_val[ii].is_term("}"):
        assert p.tok_val[ii].is_nterm(NTERM_STRUCT_FIELD)
        f = create_struct_field(p.tok_val[ii])
        if f: fields.append(f)
        ii += 1
        while p.tok_val[ii].is_term(","): ii += 1

    return AstStructDefinition(fields)

def create_type_definition(p):
    assert p.is_nterm(NTERM_TYPE_DEFINITION)
    if p.tok_val[0].is_nterm(NTERM_EXPRESSION):
        return create_expression(p.tok_val[0])
    elif p.tok_val[0].is_nterm(NTERM_COMPTIME):
        assert False, "Unimpl"
    elif p.tok_val[0].is_nterm(NTERM_STRUCT_DEFINITION):
        return create_struct_definition(p.tok_val[0])
    elif p.tok_val[0].is_nterm(ENUM_DEFINITION):
        assert False, "Unimpl"

def create_type_declaration(p):
    assert p.is_nterm(NTERM_TYPE_DECLARATION)
    ii = 0
    is_export = False
    if p.tok_val[ii].is_term("export"):
        is_export = True
        ii += 1
    assert p.tok_val[ii].is_term("type")
    ii += 1
    typename = p.tok_val[ii].tok_val[1]
    ii += 1
    assert p.tok_val[ii].is_term("=")
    ii += 1
    definition = create_type_definition(p.tok_val[ii])
    return AstTypeDeclaration(typename, definition, is_export=is_export)

def create_var_declaration(p):
    assert p.is_nterm(NTERM_VAR_DECLARATION)
    ii = 0
    is_export = False
    if p.tok_val[ii].is_term("export"):
        is_export = True
        ii += 1
    is_comptime = False
    if p.tok_val[ii].is_term("comptime"):
        is_comptime = True
        ii += 1
    is_mut = False
    if p.tok_val[ii].is_term("mut"):
        is_mut = True
    else:
        assert p.tok_val[ii].is_term("var")
    ii += 1
    name = p.tok_val[ii].tok_val[0].tok_val[1]
    ii += 1
    declared_type = None
    if p.tok_val[ii].is_term(":"):
        ii += 1
        declared_type = create_type_ident(p.tok_val[ii])
        ii += 1
    assert p.tok_val[ii].is_term("=")
    ii += 1
    val = create_expression(p.tok_val[ii])
    ii += 1
    return AstVarDeclaration(name, declared_type, val, is_export=is_export, is_comptime=is_comptime, is_mut=is_mut)

def create_fn_instantiation(p):
    assert p.is_nterm(NTERM_FN_INSTANTIATION)
    return AstFnInstantiation(p.tok_val[1].term(), create_template_parameter_list(p.tok_val[2]))

def create_statement(p):
    assert p.is_nterm(NTERM_STATEMENT)
    if p.tok_val[0].is_nterm(NTERM_FN_DECLARATION):
        return create_fn_declaration(p.tok_val[0])
    elif p.tok_val[0].is_nterm(NTERM_FOR_LOOP):
        return create_for_loop(p.tok_val[0])
    elif p.tok_val[0].is_nterm(NTERM_EXTERN_FN_DECLARATION):
        return create_extern_fn_declaration(p.tok_val[0])
    elif p.tok_val[0].is_nterm(NTERM_FN_INSTANTIATION):
        return create_fn_instantiation(p.tok_val[0])
    elif p.tok_val[0].is_nterm(NTERM_TYPE_DECLARATION):
        return create_type_declaration(p.tok_val[0])
    elif p.tok_val[0].is_nterm(NTERM_VAR_DECLARATION):
        return create_var_declaration(p.tok_val[0])
    elif p.tok_val[0].is_nterm(NTERM_EXPRESSION):
        return create_expression(p.tok_val[0])
    elif p.tok_val[0].is_nterm(NTERM_ASSIGNMENT):
        return create_assignment(p.tok_val[0])
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
    return AstProgram(children, p.sl)

def create_ast(p):
    return create_program(p)

