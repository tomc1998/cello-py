#!/usr/bin/python3

import globals
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
import argparse
from scope import Scope

arg_parser = argparse.ArgumentParser(description='Process some integers.')
arg_parser.add_argument('filename', type=str, help='the .cel file to compile.')
arg_parser.add_argument('--libc', required=False, default="/lib/x86_64-linux-gnu/libc-2.27.so", type=str, help='location of shared libc for the JIT.')
args = arg_parser.parse_args()

## Init everything
llvmlite.binding.initialize()
llvmlite.binding.initialize_native_target()
llvmlite.binding.initialize_native_asmprinter()
target = llvmlite.binding.Target.from_default_triple()
globals.target_machine = target.create_target_machine()
jit.init_jit(args.libc)

## Open the file
buf = ""
f = open(args.filename, "r")
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
    print(module)

    # Create final obj file with the module

    # Create the module
    binding_module = llvmlite.binding.parse_assembly(str(module))
    binding_module.verify()

    # Emit the obj to some buffer
    obj = globals.target_machine.emit_object(binding_module)

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
