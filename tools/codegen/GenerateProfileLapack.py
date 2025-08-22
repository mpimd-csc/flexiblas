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
import yaml
import sys
import os
import datetime
try:
    from yaml import CLoader as Loader
except:
    from yaml import Loader
import glob
from tqdm import tqdm
from tqdm.contrib.concurrent import process_map

base = '../../'
n_cores = os.cpu_count()

def read_file(file_path): 
    with open(file_path, 'r') as file:
        file_content = file.read()
    return file_content 

def load_yaml(inp):
    with open(inp,'r') as istream:
        y =  yaml.load(istream, Loader=Loader)
    return y

    
code_file_tmpl = read_file('templates/code_file.tmpl')
hook_subroutine_body = read_file('templates/hook_subroutine_body.tmpl') 
hook_function_body = read_file('templates/hook_function_body.tmpl')
hook_function_cpx_intel_body = read_file('templates/hook_function_cpx_intel_body.tmpl')
hook_function_cpx_gnu_body = read_file('templates/hook_function_cpx_gnu_body.tmpl')


# %%
def is_complex_function(y):
    cpx_function = ("return_type_complex" in y and y["return_type_complex"])
    return cpx_function
        
def generate_subroutine(y, intel_interface = False):
    function_name = y['name'];
    arg_list = ""
    arg_list_void = ""
    call_list_void = ""
    cpx_function = False
    function = False
    return_type="void"
    body = hook_subroutine_body
    
    if y["return_type"] != "void":
        body = hook_function_body
        function = True
        cpx_function = ("return_type_complex" in y and y["return_type_complex"])
        return_type = y["return_type"];
        if return_type == "int": 
            return_type = "blasint"; 
        if cpx_function and not intel_interface:
            body = hook_function_cpx_gnu_body
        if cpx_function and intel_interface:
            body = hook_function_cpx_intel_body
                     
    first = True;
    for a in y['args']:
        if not first:
            arg_list += ", "
            arg_list_void += ", "
            call_list_void += ", "
        first = False
        type_ = a['type']
        if type_ == 'int':
            type_ = 'blasint' 
        if a['pointer']:
            arg_list += type_ + " *" + a['name']
            arg_list_void += "void *" + a['name']
            call_list_void += "(void *) " + a['name']
        else:
            arg_list += type_ + " " + a['name']
            arg_list_void += a['type'] + " " + a['name']
            call_list_void += a['name']
    if len(arg_list) == 0:
        arg_list = "void"
    if len(arg_list) == 0:
        arg_list = "void"
    if len(arg_list_void) == 0:
        arg_list_void = "void"
        
    return body.format(
                      return_type = return_type,
                      function_name = function_name, 
                      arg_list = arg_list,
                      arg_list_void = arg_list_void, 
                      call_list_void = call_list_void)


def generate_lapack(version):
    print("Generate Profile Hook for LAPACK {:s}".format(version))

    year = datetime.date.today().year
    versionx = version.replace('.','_')
    versionx = versionx.replace('-','_')
    varr = version.split(".")
    major = int(varr[0])
    minor = int(varr[1])
    varr2 = varr[2].split("-")
    patch = int(varr2[0])
    if ( len(varr2) > 1):
        extra = "without deprecated" 
    else:
        extra = "with deprecated"

    inputs = glob.glob('./lapack/yaml/'+version+'/*.yaml')
    inputs.sort()

    try:
        ignore_file = load_yaml('./lapack/yaml/ignore-'+version+'.yaml')
        ignore = ignore_file["ignore"]
    except:
        ignore = list()
    
    
    body =  '#include "profile_hook.h"\n'
    body += '#include "cscutils/table.h"\n'
    body += '#include "flexiblas_backend.h"\n'
    body += '#include "flexiblas_real_lapack_'+versionx+'.h"\n'
    body += '\n\n'   

    r = process_map(load_yaml, inputs, max_workers = n_cores, chunksize=1)
    function_list = list();
    for y in tqdm(r):
        name = y[0]["name"]
        if name in ignore: 
            continue
        function_list.append(name)
        if is_complex_function(y[0]):
            body += "#ifdef FLEXIBLAS_ABI_GNU\n"
            body += generate_subroutine(y[0], intel_interface = False) + '\n'
            body += "#else\n"
            body += generate_subroutine(y[0], intel_interface = True) + '\n'
            body += "#endif\n"
            
        else:
            body += generate_subroutine(y[0]) + '\n'
            
    body += "void profile_lapack_add(csc_table_t *tab, int col_name, int col_calls, int col_time) {\n"
    for name in function_list:
        body += "    ADD_BLAS_ENTRY({:s});\n".format(name)
    body += "    return;\n"
    body += "}\n"

    profile_loader = open (base+'/src/hooks/profile/profile_lapack_'+versionx+'.c','w')
    profile_loader.write(code_file_tmpl.format(
                        year = datetime.date.today().year, 
                        body = body))
    profile_loader.close()

# %%
generate_lapack("3.12.1")
generate_lapack("3.12.1-wodprc")
generate_lapack("3.12.0")
generate_lapack("3.12.0-wodprc")
generate_lapack("3.11.0")
generate_lapack("3.11.0-wodprc")
generate_lapack("3.10.1")
generate_lapack("3.10.1-wodprc")
generate_lapack("3.10.0")
generate_lapack("3.10.0-wodprc")
generate_lapack("3.9.1")
generate_lapack("3.9.1-wodprc")
generate_lapack("3.9.0")
generate_lapack("3.9.0-wodprc")
generate_lapack("3.8.0")
generate_lapack("3.8.0-wodprc")
generate_lapack("3.7.1")
generate_lapack("3.7.1-wodprc")
generate_lapack("3.7.0")
generate_lapack("3.7.0-wodprc")
generate_lapack("3.6.1")
generate_lapack("3.6.1-wodprc")
generate_lapack("3.6.0")
generate_lapack("3.6.0-wodprc")
generate_lapack("3.5.0")
generate_lapack("3.4.2")
generate_lapack("3.4.1")
generate_lapack("3.4.0")
generate_lapack("3.3.1")
generate_lapack("3.3.0")


# %%
def load_name_from_yaml(inp):
        with open(inp,'r') as istream:
            y =  yaml.load(istream, Loader=Loader)
        name = y[0]['name'];
        return name
    
def generate_profile_structure():
    lapack_yaml = glob.glob("lapack/yaml/*/*.yaml")
    r2 = process_map(load_name_from_yaml, lapack_yaml, max_workers = n_cores, chunksize=1)
    lapack_list = list(dict.fromkeys(r2))
    print(lapack_l

    fp = open(base+'/src/hooks/profile/profile_data_lapack.h', 'w')
    for name in tqdm(lapack_list):
        fp.write('profile_data_t {:s};\n'.format(name))
    fp.close()

generate_profile_structure()

# %%
