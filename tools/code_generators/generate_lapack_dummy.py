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
#
#
#
#  This script generates the dummy files for loading all LAPACK symbols.

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

#include "flexiblas_config.h"
#include "flexiblas_fortran_mangle.h"

"""
fn_head = """
HIDDEN void flexiblas_lapack_dummy_function_not_called(void)
{
"""

fn_foot = """
}
"""


def lapack_generate(version):
    version2 = version.replace(".","_")
    print ("Generating: " + version2)
    symbolfile = "../lapack_api/symbols-lapack-" + version + ".lst"
    output = "../../src/fallback_lapack/dummy_" + version2 + ".c"
    fp = open(symbolfile,'r')
    data = fp.read().splitlines()
    fp.close()

    fo = open(output, 'w');
    fo.write(file_header);

    for fn in data:
        fo.write('void FC_GLOBAL(' + fn + ',' + fn.upper()  + ')(void);\n');

    fo.write(fn_head)
    for fn in data:
        fo.write('    FC_GLOBAL(' + fn + ',' + fn.upper()  + ')();\n');
    fo.write(fn_foot);

    fo.close()

if __name__ == "__main__":

    lapack_generate("3.12.0")
    lapack_generate("3.12.0-wodprc")


    lapack_generate("3.11.0")
    lapack_generate("3.11.0-wodprc")


    lapack_generate("3.10.1")
    lapack_generate("3.10.1-wodprc")


    lapack_generate("3.10.0")
    lapack_generate("3.10.0-wodprc")


    lapack_generate("3.9.1")
    lapack_generate("3.9.1-wodprc")

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


