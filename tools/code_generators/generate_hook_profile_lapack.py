#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# SPDX-License-Identifier: LGPL-3.0-or-later
#
# This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
# Copyright (C) 2013-2024 Martin Koehler
#
# This program is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the Free
# Software Foundation, either version 3 of the License, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
# more details.
#
# You should have received a copy of the GNU General Public License along
# with this program. If not, see <https://www.gnu.org/licenses/>.


import json

from wrapper_gen_hook import *
from datetime import datetime


#
# Header of the file containing the wrappers.
#
file_header = """
//    SPDX-License-Identifier: LGPL-3.0-or-later
/*
    This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
    Copyright (C) 2013-2024 Martin Koehler

    This program is free software: you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by the Free
    Software Foundation, either version 3 of the License, or (at your option)
    any later version.

    This program is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
    FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
    more details.

    You should have received a copy of the GNU General Public License along
    with this program. If not, see <https://www.gnu.org/licenses/>.
 */

#include <stdio.h>
#include <stdlib.h>
#include <complex.h>
#include <dlfcn.h>
#include "flexiblas_backend.h"

#include "profile_hook.h"
#include "cscutils/table.h"
"""

#
# Template for a wrapper function.
#
function_str="""
extern {return_type:s} flexiblas_chain_{function_name:s} ({chain_complex:s}{arg_list:s});
{return_type:s} hook_{function_name:s}({arg_list:s})
{{
    {declare_return_type:s}
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    {function_call:s};

    helpTimeStop = flexiblas_wtime();

    data->{function_name:s}.timings[0] += (helpTimeStop - helpTime);
    data->{function_name:s}.calls[0]++;

    return {return_value:s};
}}
"""





def main(version):
    filename = "../lapack_api/lapack-{file:s}.json".format(file = version)
    fp = open(filename, "r")
    functions = (json.load(fp))
    fp.close()

    symbolfile = "../lapack_api/symbols-lapack-{file:s}.lst".format(file = version)
    fp = open(symbolfile,'r')
    data = fp.read().splitlines()
    data.append("xerbla_array")
    fp.close()
    version2 = version.replace(".","_")

    fp = open("../../src/hooks/profile/profile_lapack_{v2:s}.c".format(v2 = version2), "w")
    fp.write(file_header.format(time =  datetime.now().ctime()))
    for func in functions:
        if not func["name"] in data:
            print ("Name: %s skipped." %(func["name"]))
            continue

        fn = FortranFunction(func)
        wrapper = function_str.format(
            return_type = fn.return_type(),
            function_name = fn.function_name(),
            arg_list = fn.arg_list(),
            declare_return_type = fn.declare_return_type(),
            function_call = fn.function_call(),
            return_value = fn.return_value(),
            chain_complex = fn.chain_complex()
            )
        fp.write(wrapper+"\n\n")

    fp.write ("void profile_lapack_add(csc_table_t *tab, int col_name, int col_calls, int col_time) {\n");
    for func in functions:
        if not func["name"] in data:
            print ("Name: %s skipped." %(func["name"]))
            continue
        fp.write("    ADD_BLAS_ENTRY({funcname:s});\n".format(funcname = func["name"]));

    fp.write("    return;\n}\n");
    fp.close()


    for func in functions:
        print("profile_data_t "+func["name"] + ";");




if __name__ == "__main__":
    main("3.12.0")
    main("3.12.0-wodprc")


    main("3.11.0")
    main("3.11.0-wodprc")

    main("3.10.1")
    main("3.10.1-wodprc")


    main("3.10.0")
    main("3.10.0-wodprc")


    main("3.9.1")
    main("3.9.1-wodprc")

    main("3.9.0")
    main("3.9.0-wodprc")


    main("3.8.0")
    main("3.8.0-wodprc")

    main("3.7.0")
    main("3.7.0-wodprc")
    main("3.6.1")
    main("3.6.1-wodprc")
    main("3.6.0")
    main("3.6.0-wodprc")
    main("3.5.0")
    main("3.4.2")
    main("3.4.1")
    main("3.4.0")
    main("3.3.1")
    main("3.3.0")




