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
 * Copyright (C) Martin Koehler, 2013-2022
 */


#include <string>
#include <math.h>
#include <octave/oct.h>
#include <flexiblas_api.h>

DEFUN_DLD (flexiblas_load_backend, args, nargout,
"-*- texinfo -*-\n\
@deftypefn {Loadable Function} backend_id = flexiblas_load_backend(@var{name})\n\n\
The flexiblas_load_backend function loads a backend into the memory and returns\n\
its ID for later usage with flexiblas_switch. If the returned ID is less than\n\
zero, an error occured.\n\
@seealso{flexiblas_switch,flexiblas_list_loaded,flexiblas_list} \n\
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

    ret_load = flexiblas_load_backend(backend.c_str());

    ret(0) = octave_int32(ret_load);
    return ret;
}

