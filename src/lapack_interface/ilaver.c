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
 * Copyright (C) Martin Koehler, 2016
 */

#include <ctype.h>
#include <stdint.h>
#include "flexiblas.h"
#include "fortran_mangle.h"

void FC_GLOBAL(ilaver,ILAVER) ( Int *Major, Int * Minor, Int * Patch)
{
    *Major = (Int) FLEXIBLAS_LAPACK_MAJOR;
    *Minor = (Int) FLEXIBLAS_LAPACK_MINOR;
    *Patch = (Int) FLEXIBLAS_LAPACK_PATCH;
    return;
}

