from lang_type import *
from scope import Var

## Given a scope, initialize the builtin types into that scope.
def init_builtin_types(scope):
    scope.set("i32", Var(KindType(IntType(32, True))))

