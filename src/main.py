#!/usr/bin/python3

import lexer
import parser

## Open the file
buf = ""
f = open("test/e2e-00.cel", "r")
for line in f: buf += line

## Pass the tokens into the parser, get a parse tree
parse_tree = parser.parse(lexer.TokenStream(buf))
