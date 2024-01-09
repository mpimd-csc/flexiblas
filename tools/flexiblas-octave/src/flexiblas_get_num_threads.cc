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




#include <math.h>
#include <octave/oct.h>
#include <flexiblas_api.h>

DEFUN_DLD (flexiblas_get_num_threads, args, nargout,
"-*- texinfo -*-\n\
@deftypefn {Loadable Function} flexiblas_get_num_threads(@var{n})\n\n\
The flexiblas_get_num_threads function checks if Octave is running using\n\
using FlexiBLAS and returns the number of threads used for the computations\n\
in the backend. If the backend does not support it or FlexiBLAS is not available\n\
is returns 1.\n\
@end deftypefn")
{
    int nargin = args.length ();
    int nt;
    octave_value_list ret;

    flexiblas_set_color_output(0);
    if ( nargin != 0 ) {
        print_usage();
        return octave_value_list();
    }

    if ( !flexiblas_avail()) {
        warning("FlexiBLAS is not available.");
        ret(0) = octave_int32(1);
        return ret;
    }

    nt = flexiblas_get_num_threads();
    ret(0) = octave_int32(nt);

    return ret;
}


