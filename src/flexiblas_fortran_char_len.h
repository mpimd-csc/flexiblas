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




#ifndef FLEIXBLAS_FORTRAN_CHAR_LEN_H_
#define FLEIXBLAS_FORTRAN_CHAR_LEN_H_
#include <stdint.h>

#ifndef FLEXIBLAS_CHARLEN_T
#define FLEXIBLAS_CHARLEN_T

#if defined(__INTEL_LLVM_COMPILER) || defined(__ICC)
/* Intel Compiler (oneAPI and classic) */
typedef size_t flexiblas_fortran_charlen_t;
#elif defined (__PGI) || defined(__NVCOMPILER)
typedef int flexiblas_fortran_charlen_t;
#elif defined(__aocc__)
/* CLANG/FLANG as AMD AOCC */
typedef size_t flexiblas_fortran_charlen_t;
#elif defined(__clang__)
/* CLANG/FLANG */
typedef size_t flexiblas_fortran_charlen_t;
#elif __GNUC__ > 7
/* GNU 8.x and newer */
typedef size_t flexiblas_fortran_charlen_t;
#else
/* GNU 4.x - 7.x */
typedef int flexiblas_fortran_charlen_t;
#endif
#endif

#if __GNUC__ >= 5 && !defined (__clang__)
#include <stdint.h>

#ifdef FLEXIBLAS_INTEGER8
#define blaslogical int_fast64_t
#else
#define blaslogical int_least32_t
#endif
#else
#ifdef FLEXIBLAS_INTEGER8
#include <stdint.h>
#define blaslogical int64_t
#else
#define blaslogical int
#endif
#endif

#ifndef blasint
#ifdef FLEXIBLAS_INTEGER8
#include <stdint.h>
#define blasint int64_t
#else
#define blasint int
#endif
#endif


#endif
