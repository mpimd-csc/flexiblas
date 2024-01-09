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

DEFUN_DLD (flexiblas_set_num_threads, args, nargout,
"-*- texinfo -*-\n\
@deftypefn {Loadable Function} flexiblas_set_num_threads(@var{n})\n\n\
The flexiblas_set_num_threads function checks if Octave is running using\n\
using FlexiBLAS and sets the number of threads to @var{n} if the backend BLAS library\n\
provides a set_num_threads function. If either FlexiBLAS or the set_num_threads\n\
is found nothing happens.\n\
@end deftypefn")
{
    int nargin = args.length ();
    double ntd;
    int nt;

    flexiblas_set_color_output(0);
    if ( nargin != 1 ) {
        print_usage();
        return octave_value_list();
    }
    if (!args(0).is_real_scalar()) {
        error("Input must be a scalar value.");
        print_usage();
        return octave_value_list();
    }

    if ( !flexiblas_avail()) {
        error("FlexiBLAS is not available.");
        return octave_value_list();
    }



    ntd =  args(0).double_value();
    nt  = (int) ( nearbyint(ntd) );

    flexiblas_set_num_threads(nt);
    return octave_value_list ();
}


