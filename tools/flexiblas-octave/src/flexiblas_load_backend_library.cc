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

DEFUN_DLD (flexiblas_load_backend_library, args, nargout,
"-*- texinfo -*-\n\
@deftypefn {Loadable Function} backend_id = flexiblas_load_backend_library(@var{filename})\n\n\
The flexiblas_load_backend_library function loads a backend from a shared object\n\
into the memory and returns its ID for later usage with flexiblas_switch.\n\
If the returned ID is less than zero, an error occured.\n\
@seealso{flexiblas_switch,flexiblas_list_loaded,flexiblas_list,flexiblas_load_backend} \n\
@end deftypefn")
{
    octave_idx_type nargin = args.length ();
    octave_value_list ret;
    int ret_load = -1;
    std::string backend;

    flexiblas_set_color_output(0);
    if (nargin != 1) {
        print_usage();
        return octave_value_list();
    }
    if (!args(0).is_string()){
        print_usage();
        return octave_value_list();
    }

    backend = args(0).string_value();

    if ( !flexiblas_avail()) {
        warning("FlexiBLAS not available.");
        ret(0) = octave_int32(-1);
        return ret;
    }
    ret_load = flexiblas_load_backend_library(backend.c_str());
    ret(0) = octave_int32(ret_load);
    return ret;
}

