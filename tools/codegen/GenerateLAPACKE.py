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
import shutil

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
cblas_void_body = read_file('templates/lapacke_void_body.tmpl')
cblas_altnames_void_body = read_file('templates/lapacke_altnames_void_body.tmpl')
# cblas_function_body = read_file('templates/cblas_void.tmpl')
cblas_function_body = read_file('templates/lapacke_function_body.tmpl')
cblas_altnames_function_body = read_file('templates/lapacke_altnames_function_body.tmpl')
void_hook_header = read_file('templates/cblas_subroutine_hook_header.tmpl')
function_hook_header = read_file('templates/cblas_function_hook_header.tmpl')
header_code_tmpl = read_file('templates/blas_header.tmpl')
structure_tmpl = read_file('templates/lapacke_structure_template.tmpl')


loader_function_body = """
HIDDEN int __flexiblas_load_lapacke( flexiblas_backend_t *backend )  {{
    int failed = 0;

    {body:s}
    return failed;
}}
"""

hook_loader_function_body = """
#ifdef FLEXIBLAS_HOOK_API
HIDDEN int __flexiblas_load_lapacke_hooks ( flexiblas_hook_t *hooks, void *hook_handle)  {{

{body:s}

    return 0; 
}}
#endif 
"""
hook_loader_snippet = "    LOAD_CHOOK(hooks,hook_handle,{name:s},{name:s});"

loader_snippet = """
    do {{
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, {symbol_names:s}, NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_lapack_fallback, {symbol_names:s}, NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {{
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load {name:s}.\\n");
            failed++;
            break;
        }} 
        if ( ptr_library == NULL) {{
            DPRINTF(2, "Load {name:s} from internal fallback BLAS.\\n");
            backend->lapacke.{name:s} = ptr_fallback;
        }} else {{
            backend->lapacke.{name:s} = ptr_library;
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
    return file_header.format(year = year) + "\n#include \"lapacke.h\"\n"


# %%
def generate_subroutine(y, intel_interface = False, component = "blas"):
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
        if return_type == "int32_t": 
            return_type = "lapack_int";            
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
            type_ = 'int' 
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
    return hook_loader_snippet.format(name = name)



# %%
def generate_all(inputs, outputs, ignore = list()):
    fp = open(outputs["wrapper"],'w')
    fp.write(generate_file_header() + "\n")
    
    loader_code = ""
    hook_loader_code = ""
    loader_fallback_code = ""
    header_body = "" 
    hook_header_body =""
    
    r = process_map(load_yaml, inputs, max_workers = n_cores, chunksize=1)
    for y in tqdm(r):
    #for r in inputs: 
        # y = load_yaml(r)
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



# %%
def generate_lapacke(inputdir, version, ignore = list()):
    year = datetime.date.today().year
    versionx = version.replace('.','_')
    versionx = versionx.replace('-','_')
    varr = version.split(".")
    major = int(varr[0])
    minor = int(varr[1])
    varr2 = varr[2].split("-")
    patch = int(varr2[0])
    extra = ""
    # Generate BLAS 
    inputs = glob.glob(inputdir+'/'+version+'/*.yaml')
    inputs.sort()

    try:
        ignore_file = load_yaml(inputdir+'/ignore-'+version+'.yaml')
        ignore = ignore_file["ignore"]
    except:
        print("Failed to load ignore")
        ignore = list()
    
    
    outputs = dict()
    outputs["wrapper"] = base + '/src/lapacke/wrapper_lapacke_'+versionx+'.c'
    outputs["header"] = base + '/include/lapacke_'+versionx+'.h'
    outputs["header_guard"] = "LAPACKE_H"
    outputs["real_call_header" ] = base + '/src/lapacke/flexiblas_real_lapacke_'+versionx+'.h'
    outputs["real_call_header_guard" ] = "FLEXIBLAS_REAL_LAPACKE_CALLS_H"
    print("Generate LAPACKE Wrappers")
    generate_all(inputs, outputs, ignore)

    struct_code = ""
    struct_template = "    void *{function_name:s};\n"
    print("Generate Structure File for LAPACK {:s}".format(version))
    r = process_map(load_yaml, inputs, max_workers = n_cores, chunksize=1)
    dummy_loader_code = ""; 
    for y in tqdm(r):
        name = y[0]["name"]
        if name in ignore:
            continue
        struct_code += struct_template.format(function_name = y[0]['name'])
        
    fp = open(base + 'src/lapacke/structure_lapacke_'+versionx +'.h', 'w')
    fp.write(structure_tmpl.format(lapack_major = major, 
                                   lapack_minor = minor, 
                                   lapack_patch = patch, 
                                   lapack_extra = extra, 
                                   body = struct_code,
                                   year = year,
                                   guard_name = "LAPACKE_STRUCTURE_H"))
    fp.close()
    if os.path.exists('./lapacke/'+version+'/lapack.h'):
        shutil.copyfile('./lapacke/'+version+'/lapack.h', base + 'src/lapacke/include/lapack_'+versionx+'.h')
    if os.path.exists('./lapacke/'+version+'/lapacke.h'):
        shutil.copyfile('./lapacke/'+version+'/lapacke.h', base + 'src/lapacke/include/lapacke_'+versionx+'.h')
    if os.path.exists('./lapacke/'+version+'/lapacke_64.h'):
        shutil.copyfile('./lapacke/'+version+'/lapacke_64.h', base + 'src/lapacke/include/lapacke_64_'+versionx+'.h')
    if os.path.exists('./lapacke/'+version+'/lapacke_config.h'):
        shutil.copyfile('./lapacke/'+version+'/lapacke_config.h', base + 'src/lapacke/include/lapacke_config_'+versionx+'.h')        
    if os.path.exists('./lapacke/'+version+'/lapacke_utils.h'):
        shutil.copyfile('./lapacke/'+version+'/lapacke_config.h', base + 'src/lapacke/include/lapacke_utils_'+versionx+'.h')        
        
generate_lapacke('./lapacke/yaml/','3.6.0')
generate_lapacke('./lapacke/yaml/','3.6.0-wodprc')
generate_lapacke('./lapacke/yaml/','3.6.1')
generate_lapacke('./lapacke/yaml/','3.6.1-wodprc')
generate_lapacke('./lapacke/yaml/','3.7.0')
generate_lapacke('./lapacke/yaml/','3.7.0-wodprc')
generate_lapacke('./lapacke/yaml/','3.7.1')
generate_lapacke('./lapacke/yaml/','3.7.1-wodprc')
generate_lapacke('./lapacke/yaml/','3.8.0')
generate_lapacke('./lapacke/yaml/','3.8.0-wodprc')
generate_lapacke('./lapacke/yaml/','3.9.0')
generate_lapacke('./lapacke/yaml/','3.9.0-wodprc')
generate_lapacke('./lapacke/yaml/','3.9.1')
generate_lapacke('./lapacke/yaml/','3.9.1-wodprc')
generate_lapacke('./lapacke/yaml/','3.10.0')
generate_lapacke('./lapacke/yaml/','3.10.0-wodprc')
generate_lapacke('./lapacke/yaml/','3.10.1')
generate_lapacke('./lapacke/yaml/','3.10.1-wodprc')
generate_lapacke('./lapacke/yaml/','3.11.0')
generate_lapacke('./lapacke/yaml/','3.11.0-wodprc')
generate_lapacke('./lapacke/yaml/','3.12.0')
generate_lapacke('./lapacke/yaml/','3.12.0-wodprc')
generate_lapacke('./lapacke/yaml/','3.12.1')
generate_lapacke('./lapacke/yaml/','3.12.1-wodprc')

# %%
