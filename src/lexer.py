import re

def get_tokens(buf):
    """
        Get all the tokens in a buf. Coroutine. Each token is a tuple of (kind, value), where 'kind' is one of the following strings:
        [[ "comment", "op", "punc", "c_string_lit", "string_lit", "float_lit", "int_lit", "ident", "mismatch" ]] 
    """
    token_specification = [
        ("comment", "(\\/\\/.*\n|\\/\\*.*?\\*\\/)"),
        ("op", "(->|!=|%=|\\+=|-=|\\*=|\\/=|&=|\\|=|&&|\\|\\||\\||&|>=|<=|==|>|<|=|\\*|\\+|-|\\/|%|!|&)"),
        ("punc", "(\\{|\\}|\\(|\\)|\\[|\\]|,|\\.\\.|\\.|`|@|\\$|#|;|::|:)"),
        ("c_string_lit", "c\"(\\\\.|[^\"\\\\])*\""),
        ("string_lit", "\"(\\\\.|[^\"\\\\])*\""),
        ("float_lit", "[-+]?[0-9]\\.[0-9]"),
        ("int_lit", "(0b|0x)?[0-9]+"),
        ("ident", "[A-Za-z_][A-Za-z0-9_]*"),
        ("ws","[ \n]"),
        ("mismatch", "."),
    ]

    tok_regex = '|'.join('(?P<%s>%s)' % pair for pair in token_specification)
    return re.finditer(tok_regex, buf)

class SourceLabel:
    def __init__(self, lineno, charno):
        self.lineno = lineno
        self.charno = charno
    def to_str(self):
        return str(self.lineno) + ":" + str(self.charno)

## Peekable
class TokenStream:
    def __init__(self, code):
        self.line_pos = 1
        self.peek_queue = []
        self.raw_stream = get_tokens(code)
        assert(self.raw_stream)

    # Get source label
    def sl(self):
        return SourceLabel(self.line_pos, 0)

    def __consume(self):
        while(True):
            mo = next(self.raw_stream, None)
            if mo == None: return None
            ## Find out how many newlines there are, advance
            kind = mo.lastgroup
            value = mo.group()
            num_newlines = 0
            for c in value:
                if c == '\n': num_newlines += 1
            self.line_pos += num_newlines
            if len(value.rstrip()) == 0: continue
            if kind == "comment" or kind == "ws": continue
            return (kind,value)

    def next(self):
        if len(self.peek_queue) > 0: return self.peek_queue.pop(0)
        else: return self.__consume()

    def peek(self, i=0):
        while i >= len(self.peek_queue):
            cons = self.__consume()
            if cons == None: return None
            self.peek_queue.append(cons)
        return self.peek_queue[i]

