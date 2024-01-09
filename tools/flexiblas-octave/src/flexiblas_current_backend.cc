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



#include <string>
#include <math.h>
#include <octave/oct.h>
#include <flexiblas_api.h>

DEFUN_DLD (flexiblas_current_backend, args, nargout,
"-*- texinfo -*-\n\
@deftypefn {Loadable Function} current_backend = flexiblas_current_backend()\n\n\
The flexiblas_current_backend function returns the currently used backend. \n\
@seealso{flexiblas_switch,flexiblas_list_loaded,flexiblas_list} \n\
@end deftypefn")
{
    octave_idx_type nargin = args.length ();
    octave_value_list ret;
    char backend[1024];

    flexiblas_set_color_output(0);
    if (nargin != 0) {
        print_usage();
        return octave_value_list();
    }
    if ( !flexiblas_avail()) {
        warning("FlexiBLAS not available.");
        return octave_value_list();
    }


    flexiblas_current_backend(backend, 1024);
    ret.append(octave_value(std::string(backend)));

    return ret;
}

