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






#include <string>
#include <math.h>
#include <octave/oct.h>
#include <flexiblas_api.h>

DEFUN_DLD (flexiblas_list, args, nargout,
"-*- texinfo -*-\n\
@deftypefn {Loadable Function} available_backends = flexiblas_list()\n\n\
The flexiblas_list function returns an array of string containing the \n\
available backends in FlexiBLAS. If FlexiBLAS is not used a warning and\n\
an empty array are returned.\n\
@end deftypefn")
{
    octave_idx_type nargin = args.length ();
    octave_value_list ret;
    octave_idx_type nbackends, i;
    char backend[1024];
    string_vector backends_list;

    flexiblas_set_color_output(0);
    if (nargin != 0) {
        print_usage();
        return octave_value_list();
    }
    if ( !flexiblas_avail()) {
        warning("FlexiBLAS not available.");
        return octave_value_list();
    }

    nbackends = flexiblas_list(NULL, 0, 0);
    for (i = 0; i < nbackends; i++) {
        flexiblas_list(backend, 1024, (int) i);
        backends_list.append(std::string(backend));
    }

    charMatrix ch(backends_list);
    ret (0) = ch;

    return ret;
}

