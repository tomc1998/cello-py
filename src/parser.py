TERM = 'TERM'
NTERM_PROGRAM = 'PROGRAM'
NTERM_STATEMENT_OR_PREPROCESSOR = 'STATEMENT_OR_PREPROCESSOR'
NTERM_PREPROCESSOR = 'PREPROCESSOR'
NTERM_STATEMENT_LIST = 'STATEMENT_LIST'
NTERM_STATEMENT = 'STATEMENT'
NTERM_EXPRESSION = 'EXPRESSION'
NTERM_VAR_DECLARATION = 'VAR_DECLARATION'
NTERM_FN_DECLARATION = 'FN_DECLARATION'
NTERM_EXTERN_FN_DECLARATION = 'EXTERN_FN_DECLARATION'
NTERM_FN_SIGNATURE = 'FN_SIGNATURE'
NTERM_FN_TYPE = 'FN_TYPE'
NTERM_TYPE_DECLARATION = 'TYPE_DECLARATION'
NTERM_TYPE_DEFINITION = 'TYPE_DEFINITION'
NTERM_STRUCT_DEFINITION = 'STRUCT_DEFINITION'
NTERM_STRUCT_FIELD = 'STRUCT_FIELD'
NTERM_ENUM_DEFINITION = 'ENUM_DEFINITION'
NTERM_ENUM_FIELD = 'ENUM_FIELD'
NTERM_MEMBER_VAR_DECL = 'MEMBER_VAR_DECL'
NTERM_COMPTIME = 'COMPTIME'
NTERM_PARAMETER_DECL_LIST = 'PARAMETER_DECL_LIST'
NTERM_TEMPLATE_PARAMETER_DECL_LIST = 'TEMPLATE_PARAMETER_DECL_LIST'
NTERM_PARAMETER_DECL = 'PARAMETER_DECL'
NTERM_PARAMETER_LIST = 'PARAMETER_LIST'
NTERM_TEMPLATE_PARAMETER_LIST = 'TEMPLATE_PARAMETER_LIST'
NTERM_META_TYPE_IDENT = 'META_TYPE_IDENT'
NTERM_IDENTIFIER = 'IDENTIFIER'
NTERM_LITERAL = 'LITERAL'
NTERM_MAKE_EXPRESSION = 'MAKE_EXPRESSION'
NTERM_OP = 'OP'
NTERM_IF = 'IF'
NTERM_ELIF = 'ELIF'
NTERM_ELSE = 'ELSE'
NTERM_LAMBDA = 'LAMBDA'
NTERM_FOR_LOOP = 'FOR_LOOP'
NTERM_COMPTIME_FOR_LOOP = 'COMPTIME_FOR_LOOP'
NTERM_COMPTIME_IF = 'COMPTIME_IF'
NTERM_QUALIFIED_NAME = 'QUALIFIED_NAME'
NTERM_QUALIFIED_TYPE = 'QUALIFIED_TYPE'
NTERM_BINARY_EXPRESSION = 'BINARY_EXPRESSION'
NTERM_FUNCTION_CALL = 'FUNCTION_CALL'
NTERM_ARRAY_ACCESS = 'ARRAY_ACCESS'
NTERM_EMPTY_ARRAY_ACCESS = 'EMPTY_ARRAY_ACCESS'
NTERM_RANGE = 'RANGE'
NTERM_ASSIGNMENT = 'ASSIGNMENT'

## Parse node.
## tok_type = parser.NTERM_* value or parser.TERM
## tok_val =
##   | token (kind, val) pair IF tok_type == parser.TERM
##   | list of child nodes IF tok_type == parser.NTERM
## sl = source label
class ParseNode:
    def __init__(self, tok_type, tok_val, sl):
        self.tok_type = tok_type
        self.tok_val = tok_val

    def to_string(self):
        if self.tok_type == TERM: return self.tok_val[1]
        else:
            ret = self.tok_type + "(";
            for c in self.tok_val: ret += c.to_string()
            ret += ")"
            return ret

    def print(self):
        print(self.to_string())

    def is_nterm(self, nterm): return self.tok_type == nterm
    def is_term(self, val): return self.tok_type == TERM and self.tok_val[1] == val

class ParseError(Exception):
    def __init__(self, l, err):
        super(Exception, self).__init__(err)
        self.sl = l.sl()

def is_assignment_op(t):
  return t[1] == "=" or t[1] == "+=" or t[1] == "-=" or t[1] == "/=" or t[1] == "*="

def assert_not_empty(l, msg):
    if not l.peek(): raise ParseError(l, msg)

def assert_val(l, val):
    assert_not_empty(l, "Expected '" + val + "', got EOF")
    if l.peek()[1] != val:
        raise ParseError(l, "Expected '" + val + "', got '" + l.peek()[1] + "'")

def assert_type(l, val):
    assert_not_empty(l, "Expected '" + val + "', got EOF")
    if l.peek()[0] != val:
        raise ParseError(l, "Expected token of type '" + val + "', got '" + l.peek()[1] + "'")

def parse_preprocessor(l): raise NotImplementedError

def parse_identifier(l):
    assert_type(l, "ident")
    return ParseNode(NTERM_IDENTIFIER, [ParseNode(TERM, l.next(), l.sl())], l.sl())

def parse_fn_declaration(l):
    children = []
    if l.peek()[1] == "export":
        children.append({ *l.next(), {}, l.get_curr_source_label() })
    assert_val(l, "fn")
    children.append(ParseNode(TERM, l.next(), l.sl()))
    children.append(parse_identifier(l))
    assert_not_empty(l, "Expected rest of function declaration, got EOF")
    if l.peek()[1] == "<": children.append(parse_template_parameter_decl_list(l))
    assert_val(l, "=")
    children.append(ParseNode(TERM, l.next(), l.sl()))
    children.append(parse_fn_signature(l))
    children.append(parse_expression(l))
    return ParseNode(NTERM_FN_DECLARATION, children, l.sl())

def parse_op(l):
  assert_type(l, "op");
  return ParseNode(NTERM_OP, [ ParseNode(TERM, l.next(), l.sl()) ], l.sl());

def parse_binary_expression(l, lrec=False):
  return ParseNode(NTERM_BINARY_EXPRESSION, [lrec, parse_op(l), parse_expression(l)], l.sl());

def parse_statement_list(l):
    children = []
    assert_val(l, "{")
    children.append(ParseNode(TERM, l.next(), l.sl()))
    while l.peek() and l.peek()[1] != "}":
        children.append(parse_statement(l))
        if l.peek() and l.peek()[1] == ";":
            children.append(ParseNode(TERM, l.next(), l.sl()))
    assert_val(l, "}")
    children.append(ParseNode(TERM, l.next(), l.sl()))
    return ParseNode(NTERM_STATEMENT_LIST, children, l.sl())

def parse_elif(l):
    children = []
    assert_val(l, "elif")
    children.append(ParseNode(TERM, l.next(), l.sl()))
    children.append(parse_expression(l))
    children.append(parse_statement_list(l))
    return ParseNode(NTERM_ELIF, children, l.sl())

def parse_else(l):
    children = []
    assert_val(l, "else")
    children.append(ParseNode(TERM, l.next(), l.sl()))
    children.append(parse_statement_list(l))
    return ParseNode(NTERM_ELSE, children, l.sl())

def parse_if(l):
    children = []
    assert_val(l, "if")
    children.append(ParseNode(TERM, l.next(), l.sl()))
    children.append(parse_expression(l))
    children.append(parse_statement_list(l))
    while l.peek() and l.peek()[1] == "elif":
        children.append(parse_elif(l))
    if l.peek() and l.peek()[1] == "else":
        children.append(parse_else(l))
    return ParseNode(NTERM_IF, children, l.sl())

def parse_parameter_list(l):
    children = []
    assert_val(l, "(")
    children.append(ParseNode(TERM, l.next(), l.sl()))
    while l.peek() and l.peek()[1] != ")":
        children.append(parse_expression(l, True))
        if l.peek() and l.peek()[1] == ",":
            children.append(ParseNode(TERM, l.next(), l.sl()))
    assert_val(l, ")")
    children.append(ParseNode(TERM, l.next(), l.sl()))
    return ParseNode(NTERM_PARAMETER_LIST, children, l.sl())

def parse_template_parameter_list(l):
    children = []
    assert_val(l, "<")
    children.append(ParseNode(TERM, l.next(), l.sl()))
    while l.peek() and l.peek()[1] != ">":
        children.append(parse_expression(l, true))
        if l.peek() and l.peek()[1] == ",":
            children.append(ParseNode(TERM, l.next(), l.sl()))
    assert_val(l, ">")
    children.append(ParseNode(TERM, l.next(), l.sl()))
    return ParseNode(NTERM_TEMPLATE_PARAMETER_LIST, children, l.sl())

def parse_literal(l):
    assert_not_empty(l, "Expected literal, got EOF")
    if l.peek()[0] != "int_lit" and l.peek()[0] != "float_lit" and l.peek()[0] != "string_lit" and l.peek()[0] != "c_string_lit":
        raise ParseError(l.sl(), "Expected literal, got " + l.peek()[1].to_string())
    return ParseNode(NTERM_LITERAL, [ParseNode(TERM, l.next(), l.sl())], l.sl())


def parse_function_call(l, lrec=None):
    children = [lrec]
    if l.peek() and l.peek()[1] == "::":
        children.append(ParseNode(TERM, l.next(), l.sl()))
        children.append(parse_template_parameter_list(l))
    children.append(parse_parameter_list(l));
    return ParseNode(NTERM_FUNCTION_CALL, children, l.sl())

def parse_expression(l, no_right_angle=False):
    """
    @param no_right_angle - When true, this won't parse right angle braces (">")
    as a greater than symbol. This is to prevent abiguity when declaring function
    template parameters, i.e. `foo<a: $type> = ...` won't parse the last bit as
    `type > =`.

    Bit of a hack, but the alternative is a totally separate production, called
    'expression_no_ra' or something.

    This won't propogate to future expressions, i.e. any expressions contained in
    this one will consume as greater than (this is generally what you want,
    though).

    The default value for this is False.
    """
    assert_not_empty(l, "Expected expression, found EOF")
    lrec = None
    if l.peek()[1] == "{": ## StatementList
        lrec = ParseNode(NTERM_EXPRESSION, [ parse_statement_list(l) ], l.sl())
    elif l.peek()[1] == "(":
        children = [ParseNode(TERM, *l.next(), l.sl()), parse_expression(l)]
        assert_val(l, ")")
        children.append(ParseNode(TERM, l.next(), l.sl()))
        lrec = ParseNode(NTERM_EXPRESSION, children, l.sl())
    elif l.peek()[1] == "comptime":
        if l.peek(1) and l.peek(1)[1] == "if":
            lrec = ParseNode(NTERM_EXPRESSION, [ parse_comptime_if(l) ], l.sl())
        elif l.peek(1) and (l.peek(1)[1] == "var" or l.peek(1)[1] == "mut"):
            lrec = ParseNode(NTERM_EXPRESSION, [ parse_var_declaration(l) ], l.sl())
        else:
            lrec = ParseNode(NTERM_EXPRESSION, [ parse_comptime(l) ], l.sl())
    elif l.peek()[1] == "$":
        lrec = ParseNode(NTERM_EXPRESSION, [ parse_meta_type_ident(l) ], l.sl())
    elif l.peek()[0] == "op":
        lrec = ParseNode(NTERM_EXPRESSION, [ parse_op(l), parse_expression(l) ], l.sl())
    elif l.peek()[1] == "fn":
        lrec = ParseNode(NTERM_EXPRESSION, [ parse_fn_type(l) ], l.sl())
    elif l.peek()[1] == "lambda":
        lrec = ParseNode(NTERM_EXPRESSION, [ parse_lambda(l) ], l.sl())
    elif l.peek()[1] == "if":
        lrec = ParseNode(NTERM_EXPRESSION, [ parse_if(l) ], l.sl())
    elif l.peek()[1] == "make":
        lrec = ParseNode(NTERM_EXPRESSION, [ parse_make_expression(l) ], l.sl())
    elif l.peek()[0] == "int_lit" or l.peek()[0] == "float_lit" or l.peek()[0] == "string_lit" or l.peek()[0] == "c_string_lit":
        ## Literal
        lrec = ParseNode(NTERM_EXPRESSION, [ parse_literal(l) ], l.sl())
    elif l.peek()[0] == "ident":
        lrec = ParseNode(NTERM_EXPRESSION, [ parse_identifier(l) ], l.sl())
    elif l.peek()[1] == "undefined":
        lrec = ParseNode(NTERM_EXPRESSION, [ ParseNode(TERM, *l.next(), l.sl()) ], l.sl())
    else:
        raise ParseError(l, "Unable to parse expression starting with '" + l.peek()[1] + "'")


    ## Continually look for stuff on the end to add until we've run out of tokens
    ## - this is to prevent left recursion.
    added_lrec = False
    while True:
        if not l.peek():
            break
        elif ((l.peek()[1] == "." or l.peek()[1] == "::") and l.peek(1)
                and (l.peek(1)[0] == "ident"
                    or l.peek(1)[0] == "int_lit")):
            lrec = parse_qualified_name(l, lrec)
        elif l.peek()[1] == "(":
            lrec = parse_function_call(l, lrec)
        elif l.peek()[1] == "::" and l.peek(1) and l.peek(1)[1] == "<":
            lrec = parse_qualified_type(l, lrec)
        elif l.peek()[1] == "[":
            if l.peek(1) and l.peek(1)[1] == "]":
                lrec = parse_empty_array_access(l, lrec)
            else:
                lrec = parse_array_access(l, lrec)
        elif l.peek()[1] == "..":
            lrec = parse_range(l, lrec)
        ## Check if this is a binary op, but also if no_right_angle is set, check it's not a ">".
        ## As a bonus, don't parse assignment ops here
        elif l.peek()[0] == "op" and not is_assignment_op(l.peek()) and (not no_right_angle or l.peek()[1] != ">"):
            lrec = parse_binary_expression(l, lrec)
        else: break
        added_lrec = True

    ## If we added some more stuff on the end in the previous stage, re-wrap this
    ## in an 'expression' nterm. Otherwise, just return as-is.
    return ParseNode(NTERM_EXPRESSION, [lrec], l.sl()) if added_lrec else lrec


def parse_parameter_decl(l, no_right_angle=False):
  children = []
  if l.peek() and l.peek()[1] == "comptime":
    children.append(ParseNode(TERM, l.next(), l.sl()))
  if l.peek(0) and l.peek(1) and l.peek(0)[0] == "ident" and l.peek(1)[1] == ":":
    children.append(parse_identifier(l))
    assert_val(l, ":")
    children.append(ParseNode(TERM, l.next(), l.sl()))
    children.append(parse_expression(l, no_right_angle))
  else:
    children.append(parse_expression(l, no_right_angle))
  return ParseNode(NTERM_PARAMETER_DECL, children, l.sl())

def parse_parameter_decl_list(l):
    children = []
    assert_val(l, "(")
    children.append(ParseNode(TERM, l.next(), l.sl()))
    while l.peek() and l.peek()[1] != ")":
        children.append(parse_parameter_decl(l))
        if l.peek() and l.peek()[1] == ",":
            children.append(ParseNode(TERM, l.next(), l.sl()))
    assert_val(l, ")")
    children.append(ParseNode(TERM, l.next(), l.sl()))
    return ParseNode(NTERM_PARAMETER_DECL_LIST, children, l.sl())

def parse_fn_signature(l):
    children = []
    children.append(parse_parameter_decl_list(l))
    if l.peek() and l.peek()[1] == "mut": children.append(ParseNode(TERM, l.next(), l.sl()))
    if l.peek() and l.peek()[1] == "->":
        children.append(ParseNode(TERM, l.next(), l.sl()))
        children.append(parse_expression(l))
    return ParseNode(NTERM_FN_SIGNATURE, children, l.sl())

def parse_statement(l):
    assert_not_empty(l, "Expected statement, got EOF")
    children = []
    if l.peek()[1] == "export":
        if l.peek(1) and (l.peek(1)[1] == "var" or l.peek(1)[1] == "mut"):
            children.append(parse_var_declaration(l))
        elif l.peek(1) and l.peek(1)[1] == "type":
            children.append(parse_type_declaration(l))
        elif l.peek(1) and l.peek(1)[1] == "fn":
            children.append(parse_fn_declaration(l))
        else:
            raise ParseError(l, "Expected 'var', 'type', or 'fn' keywords after 'export', found " + l.peek()[1])
    else:
        if l.peek()[1] == "var" or l.peek()[1] == "mut":
            children.append(parse_var_declaration(l))
        elif l.peek()[1] == "type":
            children.append(parse_type_declaration(l))
        elif l.peek()[1] == "fn":
            children.append(parse_fn_declaration(l))
        elif l.peek()[1] == "extern":
            children.append(parse_extern_fn_declaration(l))
        elif l.peek()[1] == "for":
            children.append(parse_for_loop(l))
        elif l.peek()[1] == "comptime" and l.peek(1) and l.peek(1)[1] == "for":
            children.append(parse_comptime_for_loop(l))
        else:
            children.append(parse_expression(l))
    return ParseNode(NTERM_STATEMENT, children, l.sl())

def parse_statement_or_preprocessor(l):
  if l.peek() and l.peek()[1] == "#": return ParseNode(NTERM_STATEMENT_OR_PREPROCESSOR, [parse_preprocessor(l)], l.sl())
  else: return ParseNode(NTERM_STATEMENT_OR_PREPROCESSOR, [parse_statement(l)], l.sl())

def parse_program(l):
    children = []
    while(l.peek()):
        # Consume all ";"
        while l.peek() and l.peek()[1] == ";": children.append(ParseNode(TERM, l.next(), l.sl()))
        if not l.peek(): break
        children.append(parse_statement_or_preprocessor(l))
    return ParseNode(NTERM_PROGRAM, children, l.sl())


def parse(tokens):
    return parse_program(tokens)
