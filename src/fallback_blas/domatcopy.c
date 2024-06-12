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






#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <complex.h>
#include <stdint.h>
#include <string.h>
#include "flexiblas_fortran_mangle.h"
#include "flexiblas_fortran_char_len.h"

#ifndef Int
#ifndef INTEGER8
#define Int 	int
#else
#define Int 	int64_t
#endif
#endif

#define FNAME FC_GLOBAL(domatcopy,DOMATCOPY)
#define ENAME "DOMATCOPY"
#define FLOAT double
#define _DOUBLE_PRECISION
#include "omatcopy_kernel.c"


