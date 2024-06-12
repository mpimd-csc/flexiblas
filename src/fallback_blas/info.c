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






#include "flexiblas_backend.h"

FLEXIBLAS_INFO_FUNCTION(info)
{
#ifdef  INTEGER8
    info -> backend_integer_size = 8;
#else
    info -> backend_integer_size = sizeof(int);
#endif
#if defined(__ICC) || defined(__INTEL_LLVM_COMPILER) || (defined(__PGI) && !defined(__PPC__) ) || defined(__aocc__)
    info -> intel_interface = -1;
#else
    info -> intel_interface = 0;
#endif
}



