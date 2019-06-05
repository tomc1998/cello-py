from lang_type import *
from scope import Var

## Given a scope, initialize the builtin types into that scope.
def init_builtin_types(scope):
    scope.set("i32", Var(KindType(IntType(32, True))))
    scope.set("u8", Var(KindType(IntType(8, False))))
    scope.set("bool", Var(KindType(BoolType())))
    scope.set("usize", Var(KindType(IntType(64, False))))
    scope.set("$type", Var(KindType(KindType(None))))

