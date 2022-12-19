#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#  This script generates the dummy files for loading all LAPACK symbols.

file_header = """
/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 * Linking FlexiBLAS statically or dynamically with other modules is making a combined
 * work based on FlexiBLAS. Thus, the terms and conditions of the GNU General
 * Public License cover the whole combination.
 *
 * As a special exception, the copyright holders of FlexiBLAS give you permission
 * to combine FlexiBLAS program with free software programs or libraries that are
 * released under the GNU LGPL and with independent modules that communicate with
 * FlexiBLAS solely through the BLAS/LAPACK interface as provided by the
 * BLAS/LAPACK reference implementation. You may copy and distribute such a system
 * following the terms of the GNU GPL for FlexiBLAS and the licenses of the other
 * code concerned, provided that you include the source code of that other code
 * when and as the GNU GPL requires distribution of source code and provided that
 * you do not modify the BLAS/LAPACK interface.
 *
 * Note that people who make modified versions of FlexiBLAS are not obligated to
 * grant this special exception for their modified versions; it is their choice
 * whether to do so. The GNU General Public License gives permission to release a
 * modified version without this exception; this exception also makes it possible
 * to release a modified version which carries forward this exception. If you
 * modify the BLAS/LAPACK interface, this exception does not apply to your
 * modified version of FlexiBLAS, and you must remove this exception when you
 * distribute your modified version.
 *
 * This exception is an additional permission under section 7 of the GNU General
 * Public License, version 3 (“GPLv3”)
 *
 *
 * Copyright (C) Martin Koehler, 2013-2022
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


