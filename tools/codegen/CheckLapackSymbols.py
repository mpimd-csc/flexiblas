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
import ctypes
import yaml
import sys
import os
import os.path
from yaml import Loader
import glob
from tqdm import tqdm
from tqdm.contrib.concurrent import process_map
from elftools.elf.elffile import ELFFile
from elftools.elf.sections import SymbolTableSection

base='lapack-build'
n_cores = os.cpu_count()


# %%
def list_symbols_from_so(file_path):
    with open(file_path, 'rb') as f:
        elf = ELFFile(f)

        symbols = []
        for section in elf.iter_sections():
            if isinstance(section, SymbolTableSection):
                for symbol in section.iter_symbols():
                    name = symbol.name
                    sym_type = symbol['st_info']['type']
                    bind = symbol['st_info']['bind']
                    visibility = symbol['st_other']['visibility']
                    addr = symbol['st_value']
                    if addr != 0 and sym_type == "STT_FUNC" and bind == "STB_GLOBAL":
                        symbols.append(name)
        return set(symbols)



 

# %%
def load_yaml(inp):
    with open(inp,'r') as istream:
        y =  yaml.load(istream, Loader=Loader)
    return y

def check_symbols_lapack(version):
    print("Process LAPACK {:s}".format(version))
    #inputs = glob.glob('./lapack/yaml/'+version+'/*.yaml')
    #inputs.sort()
    output = './lapack/yaml/ignore-'+version+'.yaml'

    # r = process_map(load_yaml, inputs, max_workers = n_cores, chunksize=1)
    r = load_yaml('./lapack/yaml/'+version+'.yaml')
    lapack_file = base + "/"+version+"/usr/local/lib/liblapack.so"

    if not os.path.exists(lapack_file):
        print("Lapack SO {:s} does not exist.".format(lapack_file))
        return 
    
    # shared_object = ctypes.CDLL(lapack_file)
    shared_object = list_symbols_from_so(lapack_file)
    ignore = list()
    
    for y in r:
        name = y["name"]
        if not name+"_" in shared_object:
            #print ("Symbol %s not found." % (str(name)))
            ignore.append(name)
            continue
    output_content = dict()
    output_content["ignore"] = ignore 
    fp = open(output, 'w')
    yaml.dump(output_content, fp, sort_keys=False, indent=2)
    fp.close()
    
def check_symbols_lapacke(version):
    try:
        inputs = glob.glob('./lapacke/yaml/'+version+'.yaml')
    except:
        return 
    print("Process LAPACKE {:s}".format(version))
    inputs.sort()
    output = './lapacke/yaml/ignore-'+version+'.yaml'

    # r = process_map(load_yaml, inputs, max_workers = n_cores, chunksize=1)
    r = load_yaml('./lapacke/yaml/'+version+'.yaml')
  
    lapack_file = base + "/"+version+"/usr/local/lib/liblapacke.so"

    if not os.path.exists(lapack_file):
        print("Lapack SO {:s} does not exist.".format(lapack_file))
        return 
    
    shared_object = list_symbols_from_so(lapack_file)
    ignore = list()
    
    for y in r:
        name = y["name"]
        if not name in shared_object:
            # print ("Symbol %s not found." % (str(name)))
            ignore.append(name)
            continue
    output_content = dict()
    output_content["ignore"] = ignore 
    fp = open(output, 'w')
    yaml.dump(output_content, fp, sort_keys=False, indent=2)
    fp.close()
    
def check_symbols(version, lapacke=False):
    check_symbols_lapack(version)
    if lapacke:
        check_symbols_lapacke(version)

check_symbols("3.3.0")
check_symbols("3.3.1")
check_symbols("3.4.0")
check_symbols("3.4.1")
check_symbols("3.4.2")
check_symbols("3.5.0")
check_symbols("3.6.0", True)
check_symbols("3.6.0-wodprc", True)
check_symbols("3.6.1", True)
check_symbols("3.6.1-wodprc", True)
check_symbols("3.7.0", True)
check_symbols("3.7.0-wodprc", True)
check_symbols("3.7.1", True)
check_symbols("3.7.1-wodprc", True)
check_symbols("3.8.0", True)
check_symbols("3.8.0-wodprc", True)
check_symbols("3.9.0", True)
check_symbols("3.9.0-wodprc", True)
check_symbols("3.9.1", True)
check_symbols("3.9.1-wodprc", True)
check_symbols("3.10.0", True)
check_symbols("3.10.0-wodprc", True)
check_symbols("3.10.1", True)
check_symbols("3.10.1-wodprc", True)
check_symbols("3.11.0", True)
check_symbols("3.11.0-wodprc", True)
check_symbols("3.12.0", True)
check_symbols("3.12.0-wodprc", True)
check_symbols("3.12.1", True)
check_symbols("3.12.1-wodprc", True)

# %%
