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
        if function['return_type'] == 'int32_t':
            function['return_type'] = 'lapack_int'
        args = list()
        try:
            if hasattr(node, 'args') and hasattr(node.args,'params'):
                for a in node.args.params:
                    arg = dict()
                    if isinstance(a,  EllipsisParam):
                        arg['name'] = "VAARG"
                        args.append(arg)
                        continue
                    if not a.name: 
                        continue
                    arg['name'] = a.name
                    arg['quals'] = a.quals
                                        
                    if isinstance(a.type,PtrDecl):
                        type_name = " ".join(a.type.type.type.names)
                        arg['pointer'] = True
                    else:
                        type_name = " ".join(a.type.type.names)
                        arg['pointer'] = False
                    if type_name == "int32_t":
                        type_name = "lapack_int"
                    arg['type'] = type_name 
                    args.append(arg)
        except Exception as e:
            print(e)
            print(node)
        function['args'] = args
                
        self.funcs.append(function)


# %%
def gen_item(f,version):
    try:
        os.makedirs('./lapacke/yaml/{}'.format(version))
    except:
        pass 
    r = list();
    try:
        #print("Parsing %s." % (f))

        
        ast = parse_file(f, use_cpp = True, cpp_args=[r"-Dlapack_int=int32_t",r"-Ifake_libc_include", r"-Ilapacke"])
        v = FuncDeclVisitor()
        v.visit(ast)
        for f in tqdm(v.funcs, desc= 'LAPACKE '+ version):
        #for f in v.funcs:
            fname = f['name']
            if not fname.startswith('LAPACKE_'):
                continue
            # fp = open('./lapacke/yaml/'+version+"/"+ fname + '.yaml', 'w')
            # yaml.dump([f], fp, sort_keys=False, indent=2)
            # fp.close()
            r.extend([f])
        
    except Exception as e:
        print("Error while parsing {:s}".format(f))
        print('Failed %s', e)
    return r

def gen_lapacke(version):
    r = gen_item("./lapacke/"+version+"/lapacke.h", version)
    # print(r)
    r.sort(key=lambda x: x['name'])
    fp = open('./lapacke/yaml/'+version+'.yaml','w');
    yaml.dump(r, fp, sort_keys=False, indent=2)
    fp.close()
    r = gen_item("./lapacke/"+version+"-wodprc/lapacke.h", version+"-wodprc") 
    r.sort(key=lambda x: x['name'])
    fp = open('./lapacke/yaml/'+version+'-wodprc.yaml','w');
    yaml.dump(r, fp, sort_keys=False, indent=2)
    fp.close()
    
try:
    os.makedirs('./lapacke/yaml/')
except:
    pass 

gen_lapacke("3.6.0")
gen_lapacke("3.6.1")
gen_lapacke("3.7.0")
gen_lapacke("3.7.1")
gen_lapacke("3.8.0")
gen_lapacke("3.9.0")
gen_lapacke("3.9.1")
gen_lapacke("3.10.0")
gen_lapacke("3.10.1")
gen_lapacke("3.11.0")
gen_lapacke("3.12.0")
gen_lapacke("3.12.1")

# %%
