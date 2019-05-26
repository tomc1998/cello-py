from typing import List
from lang_type import *

## PRetty sure this isn't unambiguous
def mangle_type(t: Type):
    if isinstance(t, StructType):
        base = ""
        for f in t.data.fields:
            base += mangle_type(f.field_type)
        return base
    elif isinstance(t, PtrType):
        return "p" + mangle_type(t.val)
    else: return t.name

# Given a base name, list of type param concrete types, list of param types, mangle the function accordingly
def mangle_function(base_name: str, type_params: List[Type], param_types: List[Type]):
    base_name = "__mangle_" + base_name
    for t in type_params: base_name += mangle_type(t)
    for t in param_types: base_name += mangle_type(t)
    return base_name
