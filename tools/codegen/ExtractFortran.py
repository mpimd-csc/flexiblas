# ---
# jupyter:
#   jupytext:
#     text_representation:
#       extension: .py
#       format_name: percent
#       format_version: '1.3'
#       jupytext_version: 1.17.1
#   kernelspec:
#     display_name: Python 3 (ipykernel)
#     language: python
#     name: python3
# ---

# %%
# #!/usr/bin/env python
# coding: utf-8

from pycparser import c_ast, parse_file
from pycparser.c_ast import PtrDecl
import yaml
import sys
import glob
import os
from tqdm import tqdm
from tqdm.contrib.concurrent import process_map

class FuncDeclVisitor(c_ast.NodeVisitor):
    def __init__(self):
        self.funcs = list()
        super().__init__()

    def visit_FuncDecl(self, node):
        function = dict()
        # print('Found function/subroutine %s' % (node.type.declname[:-1]))
        if node.type.declname.endswith("_"):
            function['name'] = node.type.declname[:-1]
        else:
            function['name'] = node.type.declname
        if function['name'] == 'dcabs1':
            return dict()
        if function['name'] == 'scabs1':
            return dict()
        function['load_name'] = list()
        function['load_name'].append(node.type.declname[:-1])
        function['alt_names'] = list()        
        # adjust gemmtr
        if function['name'] == 'cgemmtr':
            function['load_name'].append('cgemmt')
            function['alt_names'].append('cgemmt')
        if function['name'] == 'dgemmtr':
            function['load_name'].append('dgemmt')
            function['alt_names'].append('dgemmt')
        if function['name'] == 'sgemmtr':
            function['load_name'].append('sgemmt')
            function['alt_names'].append('sgemmt')
        if function['name'] == 'zgemmtr':
            function['load_name'].append('zgemmt')
            function['alt_names'].append('zgemmt')
            

        function['return_type'] = " ".join(node.type.type.names)
        if len(node.type.type.names) > 1 and node.type.type.names[1] == "_Complex":
            function['return_type_complex'] = True
            function['return_base_type'] = node.type.type.names[0]
        args = list()
        if hasattr(node, 'args') and hasattr(node.args,'params'):
            for a in node.args.params:
                arg = dict()
                arg['name'] = a.name
                arg['complex'] = False
                if isinstance(a.type,PtrDecl):
                    arg['type'] = " ".join(a.type.type.type.names)
                    if len(a.type.type.type.names) > 1  and a.type.type.type.names[1] == "_Complex":
                        arg['complex'] = True
                        arg['base_type'] = a.type.type.type.names[0]
                    elif arg['type'] == "int_least32_t":
                        arg['type'] = 'blaslogical'
                    arg['pointer'] = True
                    arg["hidden_str_len"] = False
                else:
                    arg['type'] = " ".join(a.type.type.names)
                    arg['pointer'] = False
                    if a.name.endswith("_len") and a.type.type.names[0] == "size_t":
                        arg["hidden_str_len"] = True
                        arg["type"] = "flexiblas_fortran_charlen_t"
                    else:
                        arg["hidden_str_len"] = False
                args.append(arg)
        function['args'] = args
        if len(function)  > 0:
            self.funcs.append(function)


gen_list = dict()
gen_list['blas/yaml'] = './blas/inputs/*.h'
gen_list['lapack/yaml/3.3.0'] = './lapack/inputs/3.3.0/*.h'
gen_list['lapack/yaml/3.3.1'] = './lapack/inputs/3.3.1/*.h'
gen_list['lapack/yaml/3.4.0'] = './lapack/inputs/3.4.0/*.h'
gen_list['lapack/yaml/3.4.1'] = './lapack/inputs/3.4.1/*.h'
gen_list['lapack/yaml/3.4.2'] = './lapack/inputs/3.4.2/*.h'
gen_list['lapack/yaml/3.5.0'] = './lapack/inputs/3.5.0/*.h'
gen_list['lapack/yaml/3.6.0'] = './lapack/inputs/3.6.0/*.h'
gen_list['lapack/yaml/3.6.0-wodprc'] = './lapack/inputs/3.6.0-wodprc/*.h'
gen_list['lapack/yaml/3.6.1'] = './lapack/inputs/3.6.1/*.h'
gen_list['lapack/yaml/3.6.1-wodprc'] = './lapack/inputs/3.6.1-wodprc/*.h'
gen_list['lapack/yaml/3.7.0'] = './lapack/inputs/3.7.0/*.h'
gen_list['lapack/yaml/3.7.0-wodprc'] = './lapack/inputs/3.7.0-wodprc/*.h'
gen_list['lapack/yaml/3.7.1'] = './lapack/inputs/3.7.1/*.h'
gen_list['lapack/yaml/3.7.1-wodprc'] = './lapack/inputs/3.7.1-wodprc/*.h'
gen_list['lapack/yaml/3.8.0'] = './lapack/inputs/3.8.0/*.h'
gen_list['lapack/yaml/3.8.0-wodprc'] = './lapack/inputs/3.8.0-wodprc/*.h'
gen_list['lapack/yaml/3.9.0'] = './lapack/inputs/3.9.0/*.h'
gen_list['lapack/yaml/3.9.0-wodprc'] = './lapack/inputs/3.9.0-wodprc/*.h'
gen_list['lapack/yaml/3.9.1'] = './lapack/inputs/3.9.1/*.h'
gen_list['lapack/yaml/3.9.1-wodprc'] = './lapack/inputs/3.9.1-wodprc/*.h'
gen_list['lapack/yaml/3.10.0'] = './lapack/inputs/3.10.0/*.h'
gen_list['lapack/yaml/3.10.0-wodprc'] = './lapack/inputs/3.10.0-wodprc/*.h'
gen_list['lapack/yaml/3.10.1'] = './lapack/inputs/3.10.1/*.h'
gen_list['lapack/yaml/3.10.1-wodprc'] = './lapack/inputs/3.10.1-wodprc/*.h'
gen_list['lapack/yaml/3.11.0'] = './lapack/inputs/3.11.0/*.h'
gen_list['lapack/yaml/3.11.0-wodprc'] = './lapack/inputs/3.11.0-wodprc/*.h'
gen_list['lapack/yaml/3.12.0'] = './lapack/inputs/3.12.0/*.h'
gen_list['lapack/yaml/3.12.0-wodprc'] = './lapack/inputs/3.12.0-wodprc/*.h'
gen_list['lapack/yaml/3.12.1'] = './lapack/inputs/3.12.1/*.h'
gen_list['lapack/yaml/3.12.1-wodprc'] = './lapack/inputs/3.12.1-wodprc/*.h'



def gen_item(f, write_local = True):
    # try:
        #print("Parsing %s." % (f))
        ast = parse_file(f, use_cpp = True, cpp_args=r'-Ifake_libc_include')
        v = FuncDeclVisitor()
        v.visit(ast)
        try:
            if write_local:
                os.makedirs('./'+name)
        except:
            pass
        if write_local:
            for fun in v.funcs:
                fname = fun['name']
                fp = open('./' + name + '/' + fname + '.yaml', 'w')
                dp = list()
                dp.append(fun)
                yaml.dump(dp, fp, sort_keys=False, indent=2)
                fp.close()
        return v.funcs
def gen_item_nowrite(f):
    return gen_item(f, False)
    
    #except:
    #    print("Error while parsing {:s}".format(f))

n_cores = os.cpu_count()

for name, d in gen_list.items():
    g =glob.glob(d)

    print("Generate YAML inputs for {:s}".format(name))
    # for f in tqdm(g):
    #    gen_item(f)
    r2 = list();
    r = process_map(gen_item_nowrite, g, max_workers = n_cores, chunksize=1)
    for i in r:
       r2.extend(i)
    # print(r2[0])
    r2.sort(key=lambda x: x['name'], reverse=False)
    fp = open('./' + name + '.yaml', 'w')
    yaml.dump(r2, fp, sort_keys=False, indent=2)
    fp.close()

# %%
