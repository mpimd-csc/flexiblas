//    SPDX-License-Identifier: LGPL-3.0-or-later
/*
    This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
    Copyright (C) 2013-2026 Martin Koehler

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

#include "backend.c"
#include "flexiblas_structures.h"

typedef void (*BLASParamErrorProc)(const char *funcName, const char *paramName,
                                   const int *paramPos,  const int *paramValue);
void SetBLASParamErrorProc(BLASParamErrorProc __ErrorProc);
flexiblas_fortran_xerbla_t active_callback = NULL;

static void apple_xerbla_translate(const char *funcName, const char *paramName,
                                   const int *paramPos,  const int *paramValue)
{
  if (!active_callback) return;
  Int info = *paramPos-1; /* it is called with CBLAS-style values */
  active_callback(funcName+6, &info, strlen(funcName)-6);
}

/*--------------------------------------------------------------------------
 * Registers the given XERBLA-like with Accelerate
 *--------------------------------------------------------------------------*/
FLEXIBLAS_SET_XERBLA_FUNCTION(fn) {
  active_callback = fn;
  SetBLASParamErrorProc(apple_xerbla_translate);
}
