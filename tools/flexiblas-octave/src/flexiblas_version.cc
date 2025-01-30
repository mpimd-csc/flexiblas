//    SPDX-License-Identifier: LGPL-3.0-or-later
/*
    This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
    Copyright (C) 2013-2025 Martin Koehler

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this program; if not, write to the Free Software Foundation,
    Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/





#include <math.h>
#include <octave/oct.h>
#include <flexiblas/flexiblas_api.h>

DEFUN_DLD (flexiblas_version, args, nargout,
"-*- texinfo -*-\n\
@deftypefn {Loadable Function} [major, minor, patch] = flexiblas_version()\n\n\
The flexiblas_version function returns the version of the used FlexiBLAS library.\n\
If Octave is not linked against FlexiBLAS [0, 0, 0] is returned.\n\
@end deftypefn")
{
    int nargin = args.length ();
    octave_value_list ret;
    int major, minor, patch;

    flexiblas_set_color_output(0);
    if (nargin != 0) {
        error("The flexiblas_version function takes no input arguments.");
        print_usage();
        return octave_value_list();
    }

    if (!flexiblas_avail()) {
        warning("FlexiBLAS is not available.");
        ret(0) = octave_int32(0);
        ret(1) = octave_int32(0);
        ret(2) = octave_int32(0);
        return ret;

    }

    flexiblas_get_version(&major, &minor, &patch);

    if ( nargout <= 1) {
        dim_vector dv(3,1);
        int32NDArray val(dv);
        val(0) = major;
        val(1) = minor;
        val(2) = patch;
        ret(0) = val;
    } else if (nargout == 3 ){
        ret (0) = octave_int32(major);
        ret (1) = octave_int32(minor);
        ret (2) = octave_int32(patch);
    } else {
        error("The function either needs 1 or 3 output arguments.");
    }
    return ret;
}


