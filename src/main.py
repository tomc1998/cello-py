#!/usr/bin/python3

from time import time
import sys
import llvmlite
import llvmlite.binding
from llvmlite import ir
from builtin_types import init_builtin_types
import lexer
import parser
import create_ast
import jit
from scope import Scope

## Init everything
llvmlite.binding.initialize()
llvmlite.binding.initialize_native_target()
llvmlite.binding.initialize_native_asmprinter()
jit.init_jit()

## Open the file
buf = ""
f = open("test/extern_fn.cel", "r")
for line in f: buf += line

## Pass the tokens into the parser, get a parse tree
try:
    start = time()

    parse_tree = parser.parse(lexer.TokenStream(buf))

    ast = create_ast.create_ast(parse_tree)

    ## Codegen
    scope = Scope()
    init_builtin_types(scope)
    module = ir.Module(name="main")
    codegen = ast.codegen(module, scope, None)

    ##print(codegen)
    ##print(module)

    # Create final obj file with the module

    # Get the target
    target = llvmlite.binding.Target.from_default_triple()
    target_machine = target.create_target_machine()

    # Create the module
    binding_module = llvmlite.binding.parse_assembly(str(module))
    binding_module.verify()

    # Emit the obj to some buffer
    obj = target_machine.emit_object(binding_module)

    end = time()

    print("---")
    print("Time taken: ", end - start)
    print("---\n")

    llvmlite.binding.shutdown()

    # Write the obj file to out.o
    f = open("out.o", "wb")
    f.write(obj)
    f.close()
except parser.ParseError as err:
    print("ERROR: " + err.sl.to_str() + " - " + str(err))
    sys.exit(1)
