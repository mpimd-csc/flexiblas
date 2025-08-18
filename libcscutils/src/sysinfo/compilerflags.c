/*
 * LIBCSCUTILS -- Helper routines of the CSC group
 * Copyright (C) Martin Koehler, 2020
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, see <http://www.gnu.org/licenses/>.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>

#include "cscutils/sysinfo.h"


const char *csc_sysinfo_c_flags(void)
{
#ifdef LANGUAGE_C
    const char *s = CMAKE_C_FLAGS;
#else
    const char *s = NULL;
#endif
    return s;
}

const char *csc_sysinfo_fortran_flags(void)
{
#ifdef LANGUAGE_Fortran
    const char *s = CMAKE_Fortran_FLAGS;
#else
    const char *s = NULL;
#endif
    return s;
}

const char *csc_sysinfo_cxx_flags(void)
{
#ifdef LANGUAGE_CXX
    const char *s = CMAKE_CXX_FLAGS;
#else
    const char *s = NULL;
#endif
    return s;
}

const char *csc_sysinfo_cuda_flags(void)
{
#ifdef LANGUAGE_CUDA
    const char *s = CMAKE_CUDA_FLAGS;
#else
    const char *s = NULL;
#endif
    return s;
}

const char *csc_sysinfo_hip_flags(void)
{
#ifdef LANGUAGE_HIP
    const char *s = CMAKE_HIP_FLAGS;
#else
    const char *s = NULL;
#endif
    return s;
}


