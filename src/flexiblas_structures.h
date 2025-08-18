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



#ifndef FLEXIBLAS_STRUCTURES_H
#define FLEXIBLAS_STRUCTURES_H

#include <stdint.h>
#include <complex.h>
#ifndef __WIN32__
#include <pthread.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif
#include "flexiblas_backend.h"

    typedef void (*flexiblas_info_function_t)(flexiblas_info_t *);
    typedef int (*flexiblas_init_function_t)(void);
    typedef void (*flexiblas_exit_function_t)(void);
    typedef void (*flexiblas_set_num_threads_function_t)(int threads);
    typedef int (*flexiblas_get_num_threads_function_t)(void);
    typedef int (*flexiblas_get_num_procs_function_t)(void);			// TODO is this procedure implemented yet, if so where and what EXACTLY does it return?


    typedef enum {
        FLEXIBLAS_COMPLEX_NONE_INTERFACE = -1,
        FLEXIBLAS_COMPLEX_GNU_INTERFACE = 0,
        FLEXIBLAS_COMPLEX_INTEL_INTERFACE = 1
    } flexiblas_complex_interface_t;

    typedef enum {
        FLEXIBLAS_INTERFACE_NONE = -1,
        FLEXIBLAS_INTERFACE_LP64 = 0,
        FLEXIBLAS_INTERFACE_ILP64 = 1
    } flexiblas_interface_t;

#if 0
    struct flexiblas_blasfn {
        void *f77_blas_function;
        void *cblas_function;
    };
#else
    typedef void* flexiblas_blasfn;
#endif
    typedef void * flexiblas_cblas_function_t;

#include "flexiblas_hook_structure.h"

#ifdef FLEXIBLAS_LAPACK

#ifdef FLEXIBLAS_LAPACK_3_12_1
#include "lapack/structure_lapack_3_12_1.h"
#endif
#ifdef FLEXIBLAS_LAPACK_3_12_1_WODPRC
#include "lapack/structure_lapack_3_12_1_wodprc.h"
#endif

#ifdef FLEXIBLAS_LAPACK_3_12_0
#include "lapack/structure_lapack_3_12_0.h"
#endif
#ifdef FLEXIBLAS_LAPACK_3_12_0_WODPRC
#include "lapack/structure_lapack_3_12_0_wodprc.h"
#endif


#ifdef FLEXIBLAS_LAPACK_3_11_0
#include "lapack/structure_lapack_3_11_0.h"
#endif
#ifdef FLEXIBLAS_LAPACK_3_11_0_WODPRC
#include "lapack/structure_lapack_3_11_0_wodprc.h"
#endif


#ifdef FLEXIBLAS_LAPACK_3_10_1
#include "lapack/structure_lapack_3_10_1.h"
#endif
#ifdef FLEXIBLAS_LAPACK_3_10_1_WODPRC
#include "lapack/structure_lapack_3_10_1_wodprc.h"
#endif


#ifdef FLEXIBLAS_LAPACK_3_10_0
#include "lapack/structure_lapack_3_10_0.h"
#endif
#ifdef FLEXIBLAS_LAPACK_3_10_0_WODPRC
#include "lapack/structure_lapack_3_10_0_wodprc.h"
#endif

#ifdef FLEXIBLAS_LAPACK_3_9_1
#include "lapack/structure_lapack_3_9_1.h"
#endif
#ifdef FLEXIBLAS_LAPACK_3_9_1_WODPRC
#include "lapack/structure_lapack_3_9_1_wodprc.h"
#endif

#ifdef FLEXIBLAS_LAPACK_3_9_0
#include "lapack/structure_lapack_3_9_0.h"
#endif
#ifdef FLEXIBLAS_LAPACK_3_9_0_WODPRC
#include "lapack/structure_lapack_3_9_0_wodprc.h"
#endif

#ifdef FLEXIBLAS_LAPACK_3_8_0
#include "lapack/structure_lapack_3_8_0.h"
#endif
#ifdef FLEXIBLAS_LAPACK_3_8_0_WODPRC
#include "lapack/structure_lapack_3_8_0_wodprc.h"
#endif

#ifdef FLEXIBLAS_LAPACK_3_7_0
#include "lapack/structure_lapack_3_7_0.h"
#endif
#ifdef FLEXIBLAS_LAPACK_3_7_0_WODPRC
#include "lapack/structure_lapack_3_7_0_wodprc.h"
#endif

#ifdef FLEXIBLAS_LAPACK_3_6_1
#include "lapack/structure_lapack_3_6_1.h"
#endif
#ifdef FLEXIBLAS_LAPACK_3_6_1_WODPRC
#include "lapack/structure_lapack_3_6_1_wodprc.h"
#endif
#ifdef FLEXIBLAS_LAPACK_3_6_0
#include "lapack/structure_lapack_3_6_0.h"
#endif
#ifdef FLEXIBLAS_LAPACK_3_6_0_WODPRC
#include "lapack/structure_lapack_3_6_0_wodprc.h"
#endif
#ifdef FLEXIBLAS_LAPACK_3_5_0
#include "lapack/structure_lapack_3_5_0.h"
#endif
#ifdef FLEXIBLAS_LAPACK_3_4_2
#include "lapack/structure_lapack_3_4_2.h"
#endif
#ifdef FLEXIBLAS_LAPACK_3_4_1
#include "lapack/structure_lapack_3_4_1.h"
#endif
#ifdef FLEXIBLAS_LAPACK_3_4_0
#include "lapack/structure_lapack_3_4_0.h"
#endif
#ifdef FLEXIBLAS_LAPACK_3_3_1
#include "lapack/structure_lapack_3_3_1.h"
#endif
#ifdef FLEXIBLAS_LAPACK_3_3_0
#include "lapack/structure_lapack_3_3_0.h"
#endif


#ifdef FLEXIBLAS_LAPACKE
#if defined(FLEXIBLAS_LAPACK_3_6_0) || defined(FLEXIBLAS_LAPACK_3_6_0_WODPRC)
#include "lapacke/structure_lapacke_3_6_0.h"
#endif
#if defined(FLEXIBLAS_LAPACK_3_6_1) || defined(FLEXIBLAS_LAPACK_3_6_1_WODPRC)
#include "lapacke/structure_lapacke_3_6_1.h"
#endif
#if defined(FLEXIBLAS_LAPACK_3_7_0) || defined(FLEXIBLAS_LAPACK_3_7_0_WODPRC)
#include "lapacke/structure_lapacke_3_7_0.h"
#endif
#if defined(FLEXIBLAS_LAPACK_3_7_1) || defined(FLEXIBLAS_LAPACK_3_7_1_WODPRC)
#include "lapacke/structure_lapacke_3_7_1.h"
#endif
#if defined(FLEXIBLAS_LAPACK_3_8_0) || defined(FLEXIBLAS_LAPACK_3_8_0_WODPRC)
#include "lapacke/structure_lapacke_3_8_0.h"
#endif
#if defined(FLEXIBLAS_LAPACK_3_9_0) || defined(FLEXIBLAS_LAPACK_3_9_0_WODPRC)
#include "lapacke/structure_lapacke_3_9_0.h"
#endif
#if defined(FLEXIBLAS_LAPACK_3_9_1) || defined(FLEXIBLAS_LAPACK_3_9_1_WODPRC)
#include "lapacke/structure_lapacke_3_9_1.h"
#endif
#if defined(FLEXIBLAS_LAPACK_3_10_0) || defined(FLEXIBLAS_LAPACK_3_10_0_WODPRC)
#include "lapacke/structure_lapacke_3_10_0.h"
#endif
#if defined(FLEXIBLAS_LAPACK_3_10_1) || defined(FLEXIBLAS_LAPACK_3_10_1_WODPRC)
#include "lapacke/structure_lapacke_3_10_1.h"
#endif
#if defined(FLEXIBLAS_LAPACK_3_11_0) || defined(FLEXIBLAS_LAPACK_3_11_0_WODPRC)
#include "lapacke/structure_lapacke_3_11_0.h"
#endif
#if defined(FLEXIBLAS_LAPACK_3_12_0) || defined(FLEXIBLAS_LAPACK_3_12_0_WODPRC)
#include "lapacke/structure_lapacke_3_12_0.h"
#endif
#if defined(FLEXIBLAS_LAPACK_3_12_1) || defined(FLEXIBLAS_LAPACK_3_12_1_WODPRC)
#include "lapacke/structure_lapacke_3_12_1.h"
#endif
#endif


#endif

#include "flexiblas_structure_blas.h"
#ifdef FLEXIBLAS_CBLAS
#include "flexiblas_structure_cblas.h"
#endif

    typedef struct _flexiblas_backend_t {
        char* name;
        void* library_handle;
        int post_init;
        int hook_init;
        pthread_mutex_t post_init_mutex;
        flexiblas_interface_t integer_interface;
        flexiblas_complex_interface_t complex_interface;
        flexiblas_info_t info;
        flexiblas_init_function_t init_function;
        flexiblas_exit_function_t exit_function;
        flexiblas_info_function_t info_function;
        flexiblas_set_num_threads_function_t set_num_threads_function[2];
        flexiblas_get_num_threads_function_t get_num_threads_function[2];
        flexiblas_get_num_procs_function_t   get_num_procs_function[2];
        flexiblas_blasfn xerbla;
        flexiblas_blasfn cblas_xerbla;

        flexiblas_blas_backend_t    blas;
#ifdef FLEXIBLAS_CBLAS
        flexiblas_cblas_backend_t   cblas;
#endif
#ifdef FLEXIBLAS_LAPACK
        flexiblas_lapack_backend_t  lapack;
#endif
#ifdef FLEXIBLAS_LAPACKE
        flexiblas_lapacke_backend_t lapacke;
#endif

    } flexiblas_backend_t;

    // extern flexiblas_backend_t *current_backend;
#ifdef __cplusplus
}
#endif
#endif
