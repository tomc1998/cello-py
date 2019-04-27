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
NTERM_IF_ = 'IF_'
NTERM_ELIF = 'ELIF'
NTERM_ELSE_ = 'ELSE_'
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

class ParseError(Exception):
    def __init__(self, l, err):
        super(Exception, self).__init__(err)
        self.sl = l.sl()

def assert_not_empty(l, msg):
    if not l.peek(): raise ParseError(l, msg)


def parse_preprocessor(l): raise NotImplementedError


def parse_statement(l):
  assert_not_empty(l, "Expected statement, got EOF");
  children = []
  if l.peek()[1] == "export":
    if l.peek(1) and (l.peek(1)[1] == "var" or l.peek(1)[1] == "mut"):
      children.append(parse_var_declaration(l));
    elif l.peek(1) and l.peek(1)[1] == "type":
      children.append(parse_type_declaration(l));
    elif l.peek(1) and l.peek(1)[1] == "fn":
      children.append(parse_fn_declaration(l));
    else:
      raise ParseError(l, "Expected 'var', 'type', or 'fn' keywords after 'export', found " + l.peek()[1])
  else:
    if l.peek()[1] == "var" or l.peek()[1] == "mut":
      children.append(parse_var_declaration(l));
    elif l.peek()[1] == "type":
      children.append(parse_type_declaration(l));
    elif l.peek()[1] == "fn":
      children.append(parse_fn_declaration(l));
    elif l.peek()[1] == "extern":
      children.append(parse_extern_fn_declaration(l));
    elif l.peek()[1] == "for":
      children.append(parse_for_loop(l));
    elif l.peek()[1] == "comptime" and l.peek(1) and l.peek(1)[1] == "for":
      children.append(parse_comptime_for_loop(l));
    else:
      children.append(parse_expression(l));

  return ParseNode(NTERM_STATEMENT, children, l.sl())

def parse_statement_or_preprocessor(l):
  if l.peek() and l.peek()[1] == "#": return ParseNode(NTERM_STATEMENT_OR_PREPROCESSOR, [parse_preprocessor(l)], l.sl());
  else: return ParseNode(NTERM_STATEMENT_OR_PREPROCESSOR, [parse_statement(l)], l.sl());

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
