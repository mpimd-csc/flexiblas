#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

#
# The scripts constructs the lapack wrappers in src/lapack_interface
#

from  wrapper_gen_lapack import *
import os
import os.path

def lapack_generate(version):
    version2 = version.replace(".","_")
    print ("Generating: " + version2)
    symbolfile = "../lapack_api/symbols-lapack-" + version + ".lst"
    filename = "../lapack_api/lapack-"+version+".json"
    fp = open(filename, "r")
    functions = json.load(fp)
    fp.close()
    functions.sort(key = lambda x: x["name"])

    fp = open(symbolfile,'r')
    data = fp.read().splitlines()
    fp.close()

    cmake_file = open("../../src/lapack_interface/lapack_"+version2+".cmake","w")

    wrap_gnu   = Wrapper(WrapperConfig(int32 = 0, int64 = 0, intel_interface= False))
    cmake_file.write("SET(LAPACK_SRC \n")

    try:
        os.mkdir("../../src/lapack_interface/wrapper/")
    except OSError:
        pass

    for fn in functions:
        wrap_gnu_s   = Wrapper(WrapperConfig(int32 = 0, int64 = 0, intel_interface= False))

        if not fn["name"] in data:
            print ("Name: %s skipped." %(fn["name"]))
            continue

        # print ("Name: %s" %(fn["name"]))
        try:
            func = FortranFunction(fn)
        except Exception as e:
            print ("Skip element %s" %(fn["name"]), e)
            continue
        wrap_gnu.add_functions(fn["name"], func)
        wrap_gnu_s.add_functions(fn["name"], func)
        if not os.path.exists("../../src/lapack_interface/wrapper/"+fn["name"]+".c"):
            wrap_gnu_s.write_wrapper_file("../../src/lapack_interface/wrapper/"+fn["name"]+".c", what="flapack", pt = "lapack", loader="LOAD_FLAPACK", skip_loader=True)
        cmake_file.write("     lapack_interface/wrapper/" + fn["name"]+".c\n")

    cmake_file.write(")\n")
    cmake_file.close()
    wrap_gnu.write_header_file("../../src/lapack_interface/lapack_"+version2+".h", "LAPACK_H")
    wrap_gnu.write_header_file_real("../../src/lapack_interface/flexiblas_real_lapack_"+version2+".h", "FLEXIBLAS_REAL_CALLS_LAPACK_H")
    wrap_gnu.write_wrapper_file("../../src/lapack_interface/load_lapack_"+version2+".c", what="flapack", pt = "lapack", loader="LOAD_FLAPACK", skip_wrapper=True)
    wrap_gnu.write_wrapper_file("../../src/lapack_interface/load_lapack_"+version2+"_fallback.c", what="flapack_fallback", pt = "lapack", loader="LOAD_FLAPACK_NOFALLBACK", skip_wrapper=True)

if __name__ == "__main__":
    lapack_generate("3.9.0")
    lapack_generate("3.9.0-wodprc")


    lapack_generate("3.8.0")
    lapack_generate("3.8.0-wodprc")

    lapack_generate("3.7.0")
    lapack_generate("3.7.0-wodprc")

    lapack_generate("3.6.1")
    lapack_generate("3.6.1-wodprc")

    lapack_generate("3.6.0")
    lapack_generate("3.6.0-wodprc")
    lapack_generate("3.5.0")

    lapack_generate("3.4.2")
    lapack_generate("3.4.1")
    lapack_generate("3.4.0")

    lapack_generate("3.3.1")
    lapack_generate("3.3.0")
