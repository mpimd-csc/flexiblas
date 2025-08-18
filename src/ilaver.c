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




#include <ctype.h>
#include <stdint.h>
#include "flexiblas.h"
#include "flexiblas_fortran_mangle.h"

void FC_GLOBAL(ilaver,ILAVER) ( Int *Major, Int * Minor, Int * Patch)
{
    *Major = (Int) FLEXIBLAS_LAPACK_MAJOR;
    *Minor = (Int) FLEXIBLAS_LAPACK_MINOR;
    *Patch = (Int) FLEXIBLAS_LAPACK_PATCH;
    return;
}

