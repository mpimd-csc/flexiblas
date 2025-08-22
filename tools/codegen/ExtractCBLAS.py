# ---
# jupyter:
#   jupytext:
#     text_representation:
#       extension: .py
#       format_name: percent
#       format_version: '1.3'
#       jupytext_version: 1.17.0
#   kernelspec:
#     display_name: Python 3 (ipykernel)
#     language: python
#     name: python3
# ---

# %%
# #!/usr/bin/env python
# coding: utf-8

from pycparser import c_ast, parse_file
from pycparser.c_ast import PtrDecl, EllipsisParam

import yaml
import sys
import glob
import os
from tqdm import tqdm
from tqdm.contrib.concurrent import process_map


# %%
class FuncDeclVisitor(c_ast.NodeVisitor):
    def __init__(self):
        self.funcs = list()
        super().__init__()

    def visit_FuncDecl(self, node):
        function = dict()
        # print(node)
        function['name'] = node.type.declname
        if function['name'] == 'cblas_xerbla':
            return 
        function['load_name'] = list()
        function['load_name'].append(node.type.declname)
        function['alt_names'] = list()
        function['return_type'] = " ".join(node.type.type.names)
        args = list()
        try:
            if hasattr(node, 'args') and hasattr(node.args,'params'):
                for a in node.args.params:
                    arg = dict()
                    if isinstance(a,  EllipsisParam):
                        arg['name'] = "VAARG"
                        args.append(arg)
                        continue
                    arg['name'] = a.name
                    arg['quals'] = a.quals
                    if isinstance(a.type,PtrDecl):
                        arg['type'] = " ".join(a.type.type.type.names)
                        arg['pointer'] = True
                    else:
                        type_name = " ".join(a.type.type.names)
                        if type_name == "int32_t":
                            type_name = "CBLAS_INT"
                        arg['type'] = type_name 
                        arg['pointer'] = False
                    args.append(arg)
        except Exception as e:
            print(e)
            print(node)
        function['args'] = args
                
        self.funcs.append(function)


# %%
def gen_item(f):
    try:
        #print("Parsing %s." % (f))
        ast = parse_file(f, use_cpp = True, cpp_args=r'-Ifake_libc_include')
        v = FuncDeclVisitor()
        v.visit(ast)

        for f in v.funcs:
            fname = f['name']
            fp = open('./cblas/yaml/' + fname + '.yaml', 'w')
            yaml.dump([f], fp, sort_keys=False, indent=2)
            fp.close()
    except:
        print("Error while parsing {:s}".format(f))
try:
    os.makedirs('./cblas/yaml/')
except:
    pass 
gen_item("./cblas/inputs/cblas.h")
