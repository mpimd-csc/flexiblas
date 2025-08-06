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

n_cores = os.cpu_count()
base = '../../'


def read_file(file_path): 
    with open(file_path, 'r') as file:
        file_content = file.read()
    return file_content 


# %% editable=true slideshow={"slide_type": ""}
file_header = read_file('templates/implementation_header.tmpl')
subroutine_body = read_file('templates/subroutine_body.tmpl') 
altnames_subroutine_body = read_file('templates/altnames_subroutine_body.tmpl')
subroutine_header = read_file('templates/subroutine_header.tmpl')
function_body = read_file('templates/function_body.tmpl')
function_float_body = read_file('templates/function_float_body.tmpl')
altnames_function_body = read_file('templates/altnames_function_body.tmpl')
function_header = read_file('templates/function_header.tmpl')
function_cpx_gnu_body = read_file('templates/function_cpx_gnu_body.tmpl')
altnames_function_cpx_gnu_body = read_file('templates/altnames_function_cpx_gnu_body.tmpl')
function_cpx_gnu_header = read_file('templates/function_cpx_gnu_header.tmpl')
function_cpx_intel_body = read_file('templates/function_cpx_intel_body.tmpl')
altnames_function_cpx_intel_body = read_file('templates/altnames_function_cpx_intel_body.tmpl')
function_cpx_intel_header = read_file('templates/function_cpx_intel_header.tmpl')
subroutine_hook_header = read_file('templates/subroutine_hook_header.tmpl')
function_hook_header = read_file('templates/function_hook_header.tmpl')
function_hook_cpx_header = read_file('templates/function_hook_cpx_header.tmpl')
structure_tmpl = read_file('templates/lapack_structure_template.tmpl')
header_code_tmpl = read_file('templates/blas_header.tmpl')
code_file_tmpl = read_file('templates/code_file.tmpl')
blas_structure_tmpl = read_file('templates/blas/blas_structure_template.tmpl')


# %%
def is_complex_function(y):
    cpx_function = ("return_type_complex" in y and y["return_type_complex"])
    return cpx_function
        


# %% editable=true slideshow={"slide_type": ""}
loader_function_body = """
HIDDEN int __flexiblas_load_f{component:s} ( flexiblas_backend_t *handle, int *loaded, int *failed )  {{
	int _ifailed = *failed;

    #ifndef FLEXIBLAS_INTEGER8
    if (handle->integer_interface == FLEXIBLAS_INTERFACE_ILP64 ) {{
        DPRINTF(0, "The selected BLAS library contains an ILP64 version of BLAS. Loading fallback instead."); 
    }}
    #endif
  
    {body:s}

	if (_ifailed != (*failed))
		return 1;
	else
    	return 0;
}}
"""

loader_function_suffix_body = """
HIDDEN int __flexiblas_load_f{component:s}_64 ( flexiblas_backend_t *handle, int *loaded, int *failed )  {{
	int _ifailed = *failed;

    #ifndef FLEXIBLAS_INTEGER8
  
    {body:s}

	if (_ifailed != (*failed))
		return 1;
	else
    	return 0;
    #else 
    return 1; 
    #endif
    
}}
"""

hook_loader_function_body = """
#ifdef FLEXIBLAS_HOOK_API
HIDDEN int __flexiblas_load_{component:s}_hooks ( flexiblas_hook_t *hooks, void *hook_handle)  {{

{body:s}

    return 0; 
}}
#endif 
"""
hook_loader_snippet = "    LOAD_HOOK(hooks,hook_handle,{name:s},{name:s});"

loader_snippet = """
    // LOAD_FBLAS(handle,{component:s}.{name:s},{name:s});
    do {{
        void *ptr_library = __flexiblas_lookup_fortran_function(handle->library_handle, {symbol_names:s}, NULL);
        void *ptr_fallback = __flexiblas_lookup_fortran_function(__flexiblas_{component:s}_fallback, {symbol_names:s}, NULL);

        #ifndef FLEXIBLAS_INTEGER8
        if ( ptr_library != NULL && handle->integer_interface == FLEXIBLAS_INTERFACE_LP64) {{
            handle->{component:s}.{name:s} = ptr_library;
            *loaded = *loaded +1; 
        }} else if (ptr_fallback != NULL ) {{
            DPRINTF(2, "Load {name:s} from internal fallback BLAS.\\n");
            handle->{component:s}.{name:s} = ptr_fallback;
            *loaded = *loaded +1; 
        }} else {{
            *failed = *failed + 1; 
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load {name:s}.\\n");
        }}
        #else 
        if ( ptr_library != NULL && handle->integer_interface == FLEXIBLAS_INTERFACE_ILP64) {{
            handle->{component:s}.{name:s} = ptr_library;
            *loaded = *loaded +1; 
        }} else if (ptr_fallback != NULL ) {{
            DPRINTF(2, "Load {name:s} from internal fallback BLAS.\\n");
            handle->{component:s}.{name:s} = ptr_fallback;
            *loaded = *loaded +1; 
        }} else {{
            *failed = *failed + 1; 
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load {name:s}.\\n");
        }}
        #endif
    }} while(0);
"""

loader_fallback_snippet = """
    // LOAD_FBLAS(handle,{component:s}.{name:s},{name:s});
    do {{
        void *ptr_fallback = __flexiblas_lookup_fortran_function(__flexiblas_{component:s}_fallback, {symbol_names:s}, NULL);

        if (ptr_fallback != NULL ) {{
            DPRINTF(2, "Load {name:s} from internal fallback BLAS.\\nn");
            handle->{component:s}.{name:s} = ptr_fallback;
            *loaded = *loaded +1; 
        }} else {{
            *failed = *failed + 1; 
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load {name:s}.\\n");
        }}
    }} while (0);
"""


loader_suffix_snippet = """
    // LOAD_FBLAS(handle,blas.{name:s},{name:s});
    do {{
        void *ptr_library  = __flexiblas_lookup_fortran_function(handle->library_handle, {symbol_names:s}, NULL);
        void *ptr_library_64  = __flexiblas_lookup_fortran_function(handle->library_handle, {symbol_names_api:s}, NULL);
        void *ptr_api_suffix_library  = __flexiblas_lookup_fortran_function(handle->library_handle_64, {symbol_names:s}, NULL);
        void *ptr_api_suffix_library_64  = __flexiblas_lookup_fortran_function(handle->library_handle_64, {symbol_names_api:s}, NULL);
        
        void *ptr_fallback = __flexiblas_lookup_fortran_function(__flexiblas_blas_fallback, {symbol_names_api:s}, NULL);

        if ( handle->integer_interface == FLEXIBLAS_INTERFACE_ILP64 && 
             ptr_library != NULL ) {{
            /* The normal symbol is already ILP64 */ 
            DPRINTF(2, "Load {name:s}_64 from {name:s}.\\n"); 
            handle->blas.{name:s} = ptr_library;
            *loaded = *loaded +1; 
        }} else if ( ptr_library_64 != NULL ) {{
            /* The library ships ILP64 suffixed symbols */ 
            DPRINTF(2, "Load {name:s}_64 from {name:s}_64.\\n"); 
            handle->blas.{name:s} = ptr_library_64;
            *loaded = *loaded +1; 
        }} else if ( ptr_api_suffix_library_64 != NULL ) {{
            /* Get the symbol from the second library */ 
            DPRINTF(2, "Load {name:s}_64 from {name:s}_64 in the ILP64 library.\\n"); 
            handle->blas.{name:s} = ptr_api_suffix_library_64;
            *loaded = *loaded +1; 
        }} else if ( ptr_api_suffix_library != NULL ) {{
            /* Get the symbol from the second library */ 
            DPRINTF(2, "Load {name:s}_64 from {name:s} in the ILP64 library.\\n"); 
            handle->blas.{name:s} = ptr_api_suffix_library;
            *loaded = *loaded +1; 
        }} else if ( ptr_fallback != NULL ) {{ 
            DPRINTF(2, "Load {name:s} from internal fallback BLAS.\\n");
            handle->blas.{name:s} = ptr_fallback;
            *loaded = *loaded +1; 
        }} else {{
            *failed = *failed + 1; 
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load {name:s}_64.\\n");
        }}
    }} while(0);
"""


def generate_loader_snippet(y, component = "blas", fallback = False):
    name = y["name"]
    load_name = y["load_name"]
    symbol_names = "\"{}\"".format(load_name[0])
    symbol_names_api = "\"{}_64\"".format(load_name[0])
    if len(load_name) > 1:
        for k in range(1, len(load_name)):
            symbol_names += ", \"{}\"".format(load_name[k])
            symbol_names_api += ", \"{}_64\"".format(load_name[k])
    if not fallback:
        return loader_snippet.format(
                                component = component,
                                name = name, 
                                symbol_names = symbol_names, 
                                symbol_names_api = symbol_names_api) 
    else:
         return loader_fallback_snippet.format(
                                component = component,
                                name = name, 
                                symbol_names = symbol_names, 
                                symbol_names_api = symbol_names_api) 


def generate_loader_suffix_snippet(y, component = "blas"):
    name = y["name"]
    load_name = y["load_name"]
    symbol_names = "\"{}\"".format(load_name[0])
    symbol_names_api = "\"{}_64\"".format(load_name[0])
    if len(load_name) > 1:
        for k in range(1, len(load_name)):
            symbol_names += ", \"{}\"".format(load_name[k])
            symbol_names_api += ", \"{}_64\"".format(load_name[k])
    return loader_suffix_snippet.format(
                                component = component,
                                name = name, 
                                symbol_names = symbol_names, 
                                symbol_names_api = symbol_names_api) 



# %%
def generate_file_header():
    year = datetime.date.today().year
    return file_header.format(year = year)


# %% editable=true slideshow={"slide_type": ""}
def generate_hook_loader_snippet(y):
    name = y["name"]
    return hook_loader_snippet.format(name = name)


# %% editable=true slideshow={"slide_type": ""}
def generate_subroutine(y, intel_interface = False, component = "blas"):
    function_name = y['name'];
    FUNCTION_NAME = y['name'].upper()
    arg_list = ""
    arg_list_void = ""
    call_list_void = ""
    cpx_function = False
    function = False
    return_type="void"
    body = subroutine_body
    alt_body = altnames_subroutine_body
    header = subroutine_header
    hook_header = subroutine_hook_header
    
    if y["return_type"] != "void":
        body = function_body
        alt_body = altnames_function_body
        header = function_header
        hook_header = function_hook_header
        function = True
        cpx_function = ("return_type_complex" in y and y["return_type_complex"])
        return_type = y["return_type"];
        if return_type == "int": 
            return_type = "blasint"; 
        if return_type == "float _Complex":
            return_type = '{:s}_complex_float'.format(component)
        if return_type == "double _Complex":
            return_type = '{:s}_complex_double'.format(component)            
        if cpx_function and not intel_interface:
            body = function_cpx_gnu_body
            alt_body = altnames_function_cpx_gnu_body
            header = function_cpx_gnu_header
            hook_header = function_hook_cpx_header
        if cpx_function and intel_interface:
            body = function_cpx_intel_body
            alt_body = altnames_function_cpx_intel_body
            header = function_cpx_intel_header
            hook_header = function_hook_cpx_header
        if return_type == "float":
            body = function_float_body
                  
    h = ""       
    
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
        if type_ == "float _Complex":
            type_ = '{:s}_complex_float'.format(component)
        if type_ == "double _Complex":
            type_ = '{:s}_complex_double'.format(component)            
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
    h += "    "+ header.format(
                      return_type = return_type,
                      function_name = function_name, 
                      FUNCTION_NAME = FUNCTION_NAME, 
                      arg_list = arg_list, 
                      component = component) + "\n"
    alt_names = ""
    for alt in y["alt_names"]:
        alt_name = alt;
        ALT_NAME = alt.upper();
        
        alt_names += alt_body.format(
                                    return_type = return_type,
                                    function_name = alt_name, 
                                    FUNCTION_NAME = ALT_NAME,
                                    function_name_orig = function_name,
                                    FUNCTION_NAME_ORIG = function_name.upper(),
                                    arg_list = arg_list,
                                    arg_list_void = arg_list_void, 
                                    call_list_void = call_list_void, 
                                    component = component) + "\n"
        h += "    " + header.format(
                      return_type = return_type,
                      function_name = alt_name, 
                      FUNCTION_NAME = ALT_NAME, 
                      arg_list = arg_list, 
                      component = component) + "\n"
        
    hook_header = hook_header.format(
                      return_type = return_type,
                      function_name = function_name, 
                      FUNCTION_NAME = FUNCTION_NAME, 
                      arg_list = arg_list,
                      arg_list_void = arg_list_void, 
                      call_list_void = call_list_void,
                      component = component)
    return body.format(
                      return_type = return_type,
                      function_name = function_name, 
                      FUNCTION_NAME = FUNCTION_NAME, 
                      arg_list = arg_list,
                      arg_list_void = arg_list_void, 
                      call_list_void = call_list_void,
                      component = component) + alt_names, h, hook_header



# %% editable=true slideshow={"slide_type": ""}

def load_yaml(inp):
    with open(inp,'r') as istream:
        y =  yaml.load(istream, Loader=Loader)
    return y

def generate_all(inputs, outputs, ignore = list(), component = "blas"):
    year = datetime.date.today().year
    fp_gnu   = open(outputs["gnu_wrapper"],'w')
    fp_intel = open(outputs["intel_wrapper"], 'w')
    fp_gnu.write(generate_file_header() + "\n")
    fp_intel.write(generate_file_header() + "\n")

    loader_code = ""
    hook_loader_code = ""
    loader_fallback_code = ""
    gnu_header_body = "" 
    intel_header_body = ""
    hook_header_body =""

    struct_code = ""
    struct_template = "    flexiblas_blasfn {function_name:s};\n"
  
    # r = process_map(load_yaml, inputs, max_workers = n_cores, chunksize=1)
    r = load_yaml(inputs)
    for y in tqdm(r):
        # print("Generate Code for {}.".format(y[0]['name']))
        name = y["name"]
        if name in ignore: 
            continue
            
        gnu_str, gnu_header, hook_header = generate_subroutine(y, intel_interface = False, component = component)
        intel_str, intel_header, _ = generate_subroutine(y, intel_interface = True, component = component)
        fp_gnu.write(gnu_str)
        fp_intel.write(intel_str)
        loader_code += generate_loader_snippet(y, component = component) + "\n\n"
        hook_loader_code += generate_hook_loader_snippet(y) + "\n" ; 
        if component == "lapack":
            loader_fallback_code += generate_loader_snippet(y, component = component, fallback=True) + "\n\n"
        gnu_header_body += gnu_header
        intel_header_body += intel_header
        hook_header_body += hook_header + '\n'
        struct_code += struct_template.format(function_name = name)


    fp_gnu.write(loader_function_body.format(body = loader_code, component = component)+"\n\n")
    fp_intel.write(loader_function_body.format(body = loader_code,component = component)+"\n\n")
    if len(loader_fallback_code) > 1:
        fp_gnu.write(loader_function_body.format(body = loader_fallback_code, component = component+"_fallback")+"\n\n")
        fp_intel.write(loader_function_body.format(body = loader_fallback_code,component = component+"_fallback")+"\n\n")
    fp_gnu.write(hook_loader_function_body.format(component = component, body = hook_loader_code)+"\n\n")
    fp_intel.write(hook_loader_function_body.format(component = component, body = hook_loader_code)+"\n\n")
    fp_gnu.close()
    fp_intel.close()

    fp_gnu_header = open(outputs["gnu_header"],'w')
    fp_intel_header = open(outputs["intel_header"],'w')

    fp_gnu_header.write(header_code_tmpl.format(
                        year = datetime.date.today().year, 
                        guard_name = outputs["header_guard"], 
                        body = gnu_header_body))
    fp_intel_header.write(header_code_tmpl.format(
                        year = datetime.date.today().year, 
                        guard_name = outputs["header_guard"], 
                        body = intel_header_body))
    fp_gnu_header.close()
    fp_intel_header.close()

    fp_hook_header = open (outputs["real_call_header" ],'w')
    fp_hook_header.write(header_code_tmpl.format(
                        year = datetime.date.today().year, 
                        guard_name = outputs["real_call_header_guard" ], 
                        body = hook_header_body))
    fp_hook_header.close()

    if "structure_file" in outputs:
        fp = open(outputs["structure_file"], "w")
        fp.write(blas_structure_tmpl.format(body = struct_code,
                           year = year,
                           guard_name = "BLAS_STRUCTURE_H"))
        fp.close()


# %%
# Generate BLAS 
inputs = glob.glob('./blas/yaml/*.yaml')
inputs.sort()

outputs = dict()
outputs["gnu_wrapper"] = base + 'src/wrapper_blas_gnu.c'
outputs["intel_wrapper"] = base + 'src/wrapper_blas_intel.c'
outputs["gnu_header"] = base + '/include/blas_gnu.h'
outputs["intel_header"] = base + '/include/blas_intel.h'
outputs["structure_file"] = base + "/src/flexiblas_structure_blas.h"
outputs["header_guard"] = "BLAS_H"
outputs["real_call_header" ] = base + '/src/flexiblas_real_calls.h'
outputs["real_call_header_guard" ] = "FLEXIBLAS_REAL_BLAS_CALLS_H"
print("Generate BLAS Wrappers")
generate_all('./blas/yaml.yaml', outputs)

# %%
# Generate LAPACK 

    
def generate_lapack(version):
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

    struct_code = ""
    struct_template = "    flexiblas_blasfn {function_name:s};\n"
    
    inputs = './lapack/yaml/'+version+'.yaml'
    
    try:
        ignore_file = load_yaml('./lapack/yaml/ignore-'+version+'.yaml')
        ignore = ignore_file["ignore"]
    except:
        ignore = list()
    
    try:
        os.makedirs(base+"/src/lapack")
    except:
        pass 
        
    print("Generate Structure File for LAPACK {:s}".format(version))
    # r = process_map(load_yaml, inputs, max_workers = n_cores, chunksize=1)
    r = load_yaml(inputs)
    dummy_loader_code = ""; 
    for y in tqdm(r):
        name = y["name"]
        if name in ignore:
            continue
        struct_code += struct_template.format(function_name = y['name'])
        dummy_loader_code += "    __flexiblas_lapack_addr[k++] = (void *)((size_t) &(FC_GLOBAL({:s},{:s})));\n".format(name, name.upper())
        
    fp = open(base + 'src/lapack/structure_lapack_'+versionx +'.h', 'w')
    fp.write(structure_tmpl.format(lapack_major = major, 
                                   lapack_minor = minor, 
                                   lapack_patch = patch, 
                                   lapack_extra = extra, 
                                   body = struct_code,
                                   year = year,
                                   guard_name = "LAPACK_STRUCTURE_H"))
    fp.close()
    
    outputs = dict()
    outputs["gnu_wrapper"] = base + 'src/lapack/wrapper_lapack_'+versionx+'_gnu.c'
    outputs["intel_wrapper"] = base + 'src/lapack/wrapper_lapack_'+versionx+'_intel.c'
    outputs["gnu_header"] = base + 'src/lapack/lapack_'+versionx+'_gnu.h'
    outputs["intel_header"] = base + 'src/lapack/lapack_'+versionx+'_intel.h'
    outputs["header_guard"] = "LAPACK_H"
    outputs["real_call_header" ] = base + 'src/lapack/flexiblas_real_lapack_'+versionx+'.h'
    outputs["real_call_header_guard" ] = "FLEXIBLAS_REAL_LAPACK_CALLS_H"

    print("Generate Wrapper for LAPACK {:s}".format(version))
    generate_all(inputs, outputs, ignore = ignore , component = "lapack")

    dummy_loader = '#ifdef FLEXIBLAS_ABI_GNU\n'
    dummy_loader += '#include "lapack_'+versionx+'_gnu.h"\n'
    dummy_loader += '#else\n' 
    dummy_loader += '#include "lapack_'+versionx+'_intel.h"\n'
    dummy_loader += '#endif\n' 
    dummy_loader += 'HIDDEN void *__flexiblas_lapack_addr[10240];\n'
    dummy_loader += 'HIDDEN void flexiblas_lapack_dummy_function_not_called(void) {\n'
    dummy_loader += '    size_t k = 0;\n'
    dummy_loader += dummy_loader_code
    dummy_loader += '}\n'; 
        
    fp_dummy_loader = open (base+'/src/fallback_lapack/dummy_'+versionx+'.c','w')
    fp_dummy_loader.write(code_file_tmpl.format(
                        year = datetime.date.today().year, 
                        guard_name = 'DUMMY_LOADER', 
                        body = dummy_loader))
    fp_dummy_loader.close()

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
    
def generate_hook_structure():
    blas_yaml = glob.glob("blas/yaml.yaml")
    cblas_yaml = glob.glob("cblas/yaml.yaml")
    lapack_yaml = glob.glob("lapack/yaml/3*.yaml")
    lapacke_yaml = glob.glob("lapacke/yaml/3*.yaml")

    r = list()
    for f in blas_yaml:
        print("Load {:s}".format(f));
        r.extend(load_yaml(f))
    # r = process_map(load_yaml, blas_yaml, max_workers = n_cores, chunksize=1)
    # print(r)
    blas_list = list(dict.fromkeys([ x['name'] for x in r]))
    blas_list.extend(["cdotc_sub", "cdotu_sub", "zdotc_sub", "zdotu_sub"])

    r2 = list()
    for f in lapack_yaml:
        print("Load {:s}".format(f));
        r2.extend(load_yaml(f))
    # r2 = process_map(load_yaml, lapack_yaml, max_workers = n_cores, chunksize=1)
    
    lapack_list = list(dict.fromkeys([ x['name'] for x in r2]))

    r3 = list()
    for f in lapacke_yaml:
        print("Load {:s}".format(f));
        r3.extend(load_yaml(f))
    # r3 = process_map(load_yaml, lapacke_yaml, max_workers = n_cores, chunksize=1)
    
    lapacke_list = list(dict.fromkeys([ x['name'] for x in r3]))

    r4 = list()
    for f in cblas_yaml:
        print("Load {:s}".format(f));
        r4.extend(load_yaml(f))
    # r4 = process_map(load_yaml, cblas_yaml, max_workers = n_cores, chunksize=1)
    
    cblas_list = list(dict.fromkeys([ x['name'] for x in r4]))
   
    
    hooks_header = read_file('templates/blas_header.tmpl')
    year = datetime.date.today().year
    
    body = """
    #define FLEXIBLAS_MAX_HOOKS 256
    struct flexiblas_hook_fn {
        void *hook_function[FLEXIBLAS_MAX_HOOKS];
        uint16_t nhook;
    };
    
    typedef struct _flexiblas_hook_backend {
        void *handles[FLEXIBLAS_MAX_HOOKS];
        int  hooks_loaded;
        int  initialized;
        flexiblas_init_function_t hook_init[FLEXIBLAS_MAX_HOOKS];
        flexiblas_exit_function_t hook_exit[FLEXIBLAS_MAX_HOOKS];
    """
    
    blas_list.sort()
    
    for k in blas_list:
        body += "    struct flexiblas_hook_fn {:s};\n".format(k)
    body += "    /* CBLAS */\n"; 

    cblas_list.sort()
    
    for k in cblas_list:
        body += "    struct flexiblas_hook_fn {:s};\n".format(k)
    body += "    /* LAPACK */\n"; 

    
    lapack_list.sort()
    for k in lapack_list:
        body += "    struct flexiblas_hook_fn {:s};\n".format(k)
    body += "    /* LAPACKE */\n";         
    
    lapacke_list.sort()
    for k in lapacke_list:
        body += "    struct flexiblas_hook_fn {:s};\n".format(k)
    body += "} flexiblas_hook_t;"
    
    fp = open(base + 'src/flexiblas_hook_structure.h', 'w')
    fp.write(hooks_header.format(      body = body,
                                       year = year,
                                       guard_name = "FLEXIBLAS_HOOKS_STRUCTURE_H"))
    fp.close()

print("Generate Hook Structure")
generate_hook_structure()

# %%

# %%

# %%
