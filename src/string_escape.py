## Escape a string literal, return a string with the chars properly escaped
def escape(val):
    final_str = ""
    is_escape = False
    for ii, c in enumerate(val):
        if c == "\\":
            if is_escape:
                final_str += "\\"
                is_escape = False
            else: is_escape = True
        elif is_escape:
            is_escape = False
            if c == '"':
                final_str += '"'
            elif c == "n":
                final_str += "\n"
            else: assert False, "Unrecognised escape '\\" + c + "'"
        else: final_str += c
    return final_str




