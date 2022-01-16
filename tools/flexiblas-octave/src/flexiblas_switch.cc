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

#include <math.h>
#include <octave/oct.h>
#include <flexiblas_api.h>

DEFUN_DLD (flexiblas_switch, args, nargout,
"-*- texinfo -*-\n\
@deftypefn {Loadable Function} flexiblas_switch(@var{ID})\n\n\
The flexiblas_switch fucntion switches the currently used BLAS backend\n\
to the one addressed by ID. The ID is the return value of flexiblas_load_backend\n\
or flexiblas_load_backend_library. If the switch was not successful an error is\n\
displayed. This can be caught by a try-catch construct.\n\
@seealso{flexiblas_load_backend, flexiblas_load_backend_library}\n\
@end deftypefn")
{
    int nargin = args.length ();
    int ID;
    int ret;

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
        warning("FlexiBLAS not available.");
        return octave_value_list();
    }

    ID =  args(0).int_value();

    ret = flexiblas_switch(ID);
    if ( ret != 0 ) {
        error("Switching BLAS backends failed. The ID maybe out of range.");
        return octave_value_list();
    }
    return octave_value_list ();
}


