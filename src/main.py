#!/usr/bin/python3

import llvmlite
import llvmlite.binding
from llvmlite import ir
from builtin_types import init_builtin_types
import lexer
import parser
import create_ast
from scope import Scope

## Open the file
buf = ""
f = open("test/e2e-00.cel", "r")
for line in f: buf += line

## Pass the tokens into the parser, get a parse tree
parse_tree = parser.parse(lexer.TokenStream(buf))

ast = create_ast.create_ast(parse_tree)

## Codegen
scope = Scope()
init_builtin_types(scope)
module = ir.Module(name="main")
codegen = ast.codegen(module, scope)

print(codegen)
print(module)

# Create final obj file with the module
## Init everything
llvmlite.binding.initialize()
llvmlite.binding.initialize_native_target()
llvmlite.binding.initialize_native_asmprinter()

# # Get the target
target = llvmlite.binding.Target.from_default_triple()
target_machine = target.create_target_machine()

# Emit the obj to some buffer
binding_module = llvmlite.binding.parse_assembly(str(module))
obj = target_machine.emit_object(binding_module)

llvmlite.binding.shutdown()

# Write the obj file to out.o
f = open("out.o", "wb")
f.write(obj)
f.close()
