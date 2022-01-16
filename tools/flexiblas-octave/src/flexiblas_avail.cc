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

DEFUN_DLD (flexiblas_avail, args, nargout,
"-*- texinfo -*-\n\
@deftypefn {Loadable Function} avail = flexiblas_avail()\n\n\
The flexiblas_avail function returns true (!=0) if FlexiBLAS\n\
is available or false(==0) otherwise. It should be called before\n\
one of the other FlexiBLAS functions is used.\n\
@end deftypefn")
{
    int nargin = args.length ();
    octave_value_list ret;
    int avail;

    flexiblas_set_color_output(0);
    if (nargin != 0) {
        print_usage();
        return octave_value_list();
    }

    avail = flexiblas_avail();

    if ( !avail ){
        ret(0) = octave_int32(0);
        return ret;
    }
    ret(0) = octave_int32(1);
    return ret;
}


