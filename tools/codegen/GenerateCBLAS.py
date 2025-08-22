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



# %%
file_header = read_file('templates/implementation_header.tmpl')
cblas_void_body = read_file('templates/cblas_void_body.tmpl')
cblas_altnames_void_body = read_file('templates/cblas_altnames_void_body.tmpl')
# cblas_function_body = read_file('templates/cblas_void.tmpl')
cblas_function_body = read_file('templates/cblas_function_body.tmpl')
cblas_altnames_function_body = read_file('templates/cblas_function_body.tmpl')
void_hook_header = read_file('templates/cblas_subroutine_hook_header.tmpl')
function_hook_header = read_file('templates/cblas_function_hook_header.tmpl')
header_code_tmpl = read_file('templates/blas_header.tmpl')
cblas_structure_tmpl = read_file('templates/cblas/cblas_structure_template.tmpl')

loader_function_body = """
HIDDEN int __flexiblas_load_cblas( flexiblas_backend_t *backend )  {{
    int only_fallback = 0;
    int failed = 0;
    void * cblas_in_blis = dlsym(backend->library_handle, "bli_info_get_enable_cblas");
    if ( cblas_in_blis ) {{
        DPRINTF_WARN(1, "The desired BLAS library is BLIS. We do not load their CBLAS wrapper since it might alter the behavior of your programs.\\n");
        only_fallback = 1;
    }}

    {body:s}
    return failed;
}}
"""

hook_loader_function_body = """
#ifdef FLEXIBLAS_HOOK_API
HIDDEN int __flexiblas_load_cblas_hooks ( flexiblas_hook_t *hooks, void *hook_handle)  {{

{body:s}

    return 0; 
}}
#endif 
"""
hook_loader_snippet = "    LOAD_CHOOK(hooks,hook_handle,{function_blas_name:s},{name:s});"

loader_snippet = """
    do {{
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, {symbol_names:s}, NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, {symbol_names:s}, NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {{
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load {name:s}.\\n");
            failed++;
            break;
        }} 
        if ( only_fallback || ptr_library == NULL) {{
            DPRINTF(2, "Load {name:s} from internal fallback BLAS.\\n");
            backend->cblas.{name:s} = ptr_fallback;
        }} else {{
            backend->cblas.{name:s} = ptr_library;
        }}
    }} while(0);
"""

def generate_loader_snippet(y, fallback = False):
    name = y["name"]
    load_name = y["load_name"]
    function_blas_name = "_".join(name.split("_")[1:])
    symbol_names = "\"{}\"".format(load_name[0])
    symbol_names_api = "\"{}_64\"".format(load_name[0])
    if len(load_name) > 1:
        for k in range(1, len(load_name)):
            symbol_names += ", \"{}\"".format(load_name[k])
            symbol_names_api += ", \"{}_64\"".format(load_name[k])
    if not fallback:
        return loader_snippet.format(
                                name = name, 
                                function_blas_name = function_blas_name,
                                symbol_names = symbol_names, 
                                symbol_names_api = symbol_names_api) 
    else:
         return loader_fallback_snippet.format(
                                component = component,
                                name = name, 
                                symbol_names = symbol_names, 
                                symbol_names_api = symbol_names_api) 



# %%
def generate_file_header():
    year = datetime.date.today().year
    return file_header.format(year = year)


# %%
def generate_subroutine(y, intel_interface = False, component = "cblas"):
    function_name = y['name'];
    function_blas_name = "_".join(function_name.split("_")[1:])
    arg_list = ""
    arg_list_void = ""
    call_list = ""
    cpx_function = False
    function = False
    return_type="void"
    body = cblas_void_body
    alt_body = cblas_altnames_void_body
    # header = subroutine_header
    hook_header = void_hook_header
    
    if y["return_type"] != "void":
        body = cblas_function_body
        alt_body = cblas_altnames_function_body
        #header = function_header
        hook_header = function_hook_header
        function = True
        return_type = y["return_type"];
        if return_type == "int": 
            return_type = "blasint";            
    h = ""       
    
    first = True;
    for a in y['args']:
        if not first:
            arg_list += ", "
            arg_list_void += ", "
            call_list += ", "
        first = False
        if len(a['quals']) > 0:
            quals = " ".join(a['quals']) + " "
        else:
            quals = ""
        type_ = a['type']
        if type_ == 'int':
            type_ = 'blasint' 
        if type_ == "float _Complex":
            type_ = 'lapack_complex_float'
        if type_ == "double _Complex":
            type_ = 'lapack_complex_double'            
        if a['pointer']:
            arg_list += quals+ type_ + " *" + a['name']
            arg_list_void += quals + "void *" + a['name']
            call_list += a['name']
        else:
            arg_list += quals + type_ + " " + a['name']
            arg_list_void += quals + a['type'] + " " + a['name']
            call_list += a['name']
    if len(arg_list) == 0:
        arg_list = "void"
    if len(arg_list_void) == 0:
        arg_list_void = "void"
 
#    h += "    "+ header.format(
#                      return_type = return_type,
#                      function_name = function_name, 
#                      arg_list = arg_list, 
#                      component = component) + "\n"
    alt_names = ""
    for alt in y["alt_names"]:
        alt_name = alt
        ALT_NAME = alt.upper()
        
        alt_names += alt_body.format(
                                    return_type = return_type,
                                    function_name = alt_name, 
                                    function_blas_name = function_blas_name, 
                                    function_name_orig = function_name,
                                    arg_list = arg_list,
                                    arg_list_void = arg_list_void, 
                                    call_list = call_list, 
                                    component = component) + "\n"
#        h += "    " + header.format(
#                      return_type = return_type,
#                      function_name = alt_name, 
#                      FUNCTION_NAME = ALT_NAME, 
#                      arg_list = arg_list, 
#                      component = component) + "\n"  
    hook_header = hook_header.format(
                      return_type = return_type,
                      function_name = function_name, 
                      function_blas_name = function_blas_name, 
                      arg_list = arg_list,
                      arg_list_void = arg_list_void, 
                      component = component)
 
    return body.format(
                      return_type = return_type,
                      function_name = function_name,
                      function_blas_name = function_blas_name, 
                      arg_list = arg_list,
                      arg_list_void = arg_list_void, 
                      call_list = call_list,
                      component = component) + alt_names, h, hook_header

def generate_hook_loader_snippet(y):
    name = y["name"]
    function_blas_name = "_".join(name.split("_")[1:])
    return hook_loader_snippet.format(name = name, function_blas_name = function_blas_name)



# %%
def generate_all(inputs, outputs, ignore = list()):
    year = datetime.date.today().year
    fp = open(outputs["wrapper"],'w')
    fp.write(generate_file_header() + "\n")
    fp.write("#include \"cblas.h\""+"\n")
 
    struct_code = ""
    struct_template = "    flexiblas_blasfn {function_name:s};\n"
 
    loader_code = ""
    hook_loader_code = ""
    loader_fallback_code = ""
    header_body = "" 
    hook_header_body =""
    
    r = process_map(load_yaml, inputs, max_workers = n_cores, chunksize=1)
    for y in tqdm(r):
        # print("Generate Code for {}.".format(y[0]['name']))
        name = y[0]["name"]
        if name in ignore: 
            continue
            
        code, header, hook_header = generate_subroutine(y[0], intel_interface = False)
        fp.write(code)
      
        loader_code += generate_loader_snippet(y[0]) + "\n\n"
        hook_loader_code += generate_hook_loader_snippet(y[0]) + "\n" ; 
        # if component == "lapack":
        #    loader_fallback_code += generate_loader_snippet(y[0], component = component, fallback=True) + "\n\n"
        header_body += header
        hook_header_body += hook_header + '\n'
        struct_code += struct_template.format(function_name = name)

    fp.write(loader_function_body.format(body = loader_code)+"\n\n")
    # if len(loader_fallback_code) > 1:
    #    fp_gnu.write(loader_function_body.format(body = loader_fallback_code, component = component+"_fallback")+"\n\n")
    #    fp_intel.write(loader_function_body.format(body = loader_fallback_code,component = component+"_fallback")+"\n\n")
    fp.write(hook_loader_function_body.format(component = "blas", body = hook_loader_code)+"\n\n")
    fp.close()
    
    # fp_header = open(outputs["header"],'w')
    # 
    # fp_header.write(header_code_tmpl.format(
    #                    year = datetime.date.today().year, 
    #                    guard_name = outputs["header_guard"], 
    #                    body = header_body))
    # fp_header.close()

    fp_hook_header = open (outputs["real_call_header" ],'w')
    fp_hook_header.write(header_code_tmpl.format(
                        year = datetime.date.today().year, 
                        guard_name = outputs["real_call_header_guard" ], 
                        body = hook_header_body))
    fp_hook_header.close()
    if "structure_file" in outputs:
        fp = open(outputs["structure_file"], "w")
        fp.write(cblas_structure_tmpl.format(body = struct_code,
                           year = year,
                           guard_name = "CBLAS_STRUCTURE_H"))
        fp.close()



# %%
# Generate BLAS 
inputs = glob.glob('./cblas/yaml/*.yaml')
inputs.sort()

ignore = ['cblas_dcabs1','cblas_scabs1' ]

outputs = dict()
outputs["wrapper"] = base + '/src/wrapper_cblas.c'
outputs["header"] = base + '/include/cblas.h'
outputs["structure_file"] = base + "/src/flexiblas_structure_cblas.h"
outputs["header_guard"] = "CBLAS_H"
outputs["real_call_header" ] = base + '/src/flexiblas_real_cblas_calls.h'
outputs["real_call_header_guard" ] = "FLEXIBLAS_REAL_BLAS_CALLS_CBLAS_H"
print("Generate BLAS Wrappers")
generate_all(inputs, outputs, ignore)

# %%
