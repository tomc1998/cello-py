from lang_type import *
from scope import Var

## Given a scope, initialize the builtin types into that scope.
def init_builtin_types(scope):
    scope.set( "u8", Var(KindType(IntType( 8, False))))
    scope.set("u16", Var(KindType(IntType(16, False))))
    scope.set("u32", Var(KindType(IntType(32, False))))
    scope.set("u64", Var(KindType(IntType(64, False))))
    scope.set( "i8", Var(KindType(IntType( 8, True))))
    scope.set("i16", Var(KindType(IntType(16, True))))
    scope.set("i32", Var(KindType(IntType(32, True))))
    scope.set("i64", Var(KindType(IntType(64, True))))
    scope.set("bool", Var(KindType(BoolType())))
    scope.set("usize", Var(KindType(IntType(64, False))))
    scope.set("$type", Var(KindType(KindType(None))))

