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



#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <complex.h>
#include <dlfcn.h>
#include "flexiblas_backend.h"
#include "flexiblas_real_calls.h"

/* NVPL Specific stuff */
#include "nvpl_blas_service.h"
#include "nvpl_lapack_service.h"



/* Generic Backends can be loaded without RTLD_GLOBAL  */
int32_t flexiblas_ld_global = 0;
/* int32_t flexiblas_ld_lazy = 1; */
int32_t flexiblas_ld_deep = 1;

/*-----------------------------------------------------------------------------
 * Info function, called once before  FlexiBLAS initializes the back end
 *-----------------------------------------------------------------------------*/
FLEXIBLAS_INFO_FUNCTION(info) {
    /* The back end should use the post init mode. Important for CUDA */
    info->post_init = 0;
    /* Specify the integer width  */
#ifdef  BACKEND_INTEGER8
    info -> backend_integer_size = 8;
#else
    info -> backend_integer_size = sizeof(int);
#endif

    /* Specify that the interface is intel compatible */
#ifdef ZDOTC_MKL
    info -> intel_interface = -1;
#else
    info -> intel_interface = 0;
#endif
}


static void (*set_blas_thr)(int);
static void (*set_lapack_thr)(int);
static int (*get_blas_thr)(void);
static int (*get_lapack_thr)(void);


/*-----------------------------------------------------------------------------
 *  Init function, called once when FlexiBLAS initializes the backend.
 *-----------------------------------------------------------------------------*/
FLEXIBLAS_INIT_FUNCTION(void) {

    /* Return 0 on success, != 0 otherwise   */

    void *pset_thr_blas;
    void *pget_thr_blas;
    void *pset_thr_lapack;
    void *pget_thr_lapack;

    pset_thr_blas = dlsym(RTLD_NEXT, "nvpl_blas_set_num_threads");
    pset_thr_lapack = dlsym(RTLD_NEXT, "nvpl_lapack_set_num_threads");
    pget_thr_blas = dlsym(RTLD_NEXT, "nvpl_blas_get_max_threads");
    pget_thr_lapack = dlsym(RTLD_NEXT, "nvpl_lapack_get_max_threads");

    *((void **) &set_blas_thr) = * (void **) & pset_thr_blas;
    *((void **) &set_lapack_thr) = * (void **) & pset_thr_lapack;
    *((void **) &get_blas_thr) = * (void **) & pget_thr_blas;
    *((void **) &get_lapack_thr) = * (void **) & pget_thr_lapack;

    if ( pset_thr_blas == NULL
            || pset_thr_lapack == NULL
            || pget_thr_lapack == NULL
            || pget_thr_blas == NULL)
        return -1;

    return 0 ;
}



/*-----------------------------------------------------------------------------
 *  Exit function, called once when the program finishes.
 *-----------------------------------------------------------------------------*/
FLEXIBLAS_EXIT_FUNCTION(void) {
    return;
}


/*-----------------------------------------------------------------------------
 *  Include the remaining dumming functions to cheat LD
 *-----------------------------------------------------------------------------*/
#include "flexiblas_dummy_fortran.h"
#ifdef CBLAS_INTERFACE
#include "flexiblas_dummy_cblas.h"
#endif

/* Generate a standardized set/get num thread interface */
void flexiblas_backend_set_num_threads(int num)
{
    /** nvpl_blas_set_num_threads(num); */
    /** nvpl_lapack_set_num_threads(num); */
    set_blas_thr(num);
    set_lapack_thr(num);

}


int flexiblas_backend_get_num_threads(void)
{
    /** int n1 = nvpl_blas_get_max_threads(); */
    /** int n2 = nvpl_lapack_get_max_threads(); */
    int n1 = get_blas_thr();
    int n2 = get_lapack_thr();
    if( n1 < n2 )
        return n2;
    return n1;
}



