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
 * Copyright (C) Martin Koehler, 2015
 */
#ifndef FLEXIBLAS_STRUCTURES_H
#define FLEXIBLAS_STRUCTURES_H

#include <stdint.h>
#include <complex.h>
#include <pthread.h>

#ifdef __cplusplus
extern "C" {
#endif

#include "flexiblas_backend.h"

#define POS_FBLAS 0
#define POS_CBLAS 1

struct flexiblas_blasfn {
    void *fblas_real;
    void *cblas_real;
	void *call_fblas;
	void *call_cblas; 
	double timings[2]; 
	unsigned long calls[2]; 
};


typedef void (*flexiblas_info_function_t)(flexiblas_info_t *); 
typedef int (*flexiblas_init_function_t)(); 
typedef void (*flexiblas_exit_function_t)(); 
typedef void (*flexiblas_set_num_threads_function_t)(int threads); 
typedef int (*flexiblas_get_num_threads_function_t)();
typedef int (*flexiblas_get_num_procs_function_t)();


typedef struct _flexiblas_blas_backend {
    struct flexiblas_blasfn caxpy;
    struct flexiblas_blasfn ccopy;
    struct flexiblas_blasfn cdotc;
    struct flexiblas_blasfn cdotu;
    struct flexiblas_blasfn cgbmv;
    struct flexiblas_blasfn cgemm;
    struct flexiblas_blasfn cgemv;
    struct flexiblas_blasfn cgerc;
    struct flexiblas_blasfn cgeru;
    struct flexiblas_blasfn chbmv;
    struct flexiblas_blasfn chemm;
    struct flexiblas_blasfn chemv;
    struct flexiblas_blasfn cher;
    struct flexiblas_blasfn cher2;
    struct flexiblas_blasfn cher2k;
    struct flexiblas_blasfn cherk;
    struct flexiblas_blasfn chpmv;
    struct flexiblas_blasfn chpr;
    struct flexiblas_blasfn chpr2;
    struct flexiblas_blasfn crotg;
    struct flexiblas_blasfn cscal;
    struct flexiblas_blasfn csrot;
    struct flexiblas_blasfn csscal;
    struct flexiblas_blasfn cswap;
    struct flexiblas_blasfn csymm;
    struct flexiblas_blasfn csyr2k;
    struct flexiblas_blasfn csyrk;
    struct flexiblas_blasfn ctbmv;
    struct flexiblas_blasfn ctbsv;
    struct flexiblas_blasfn ctpmv;
    struct flexiblas_blasfn ctpsv;
    struct flexiblas_blasfn ctrmm;
    struct flexiblas_blasfn ctrmv;
    struct flexiblas_blasfn ctrsm;
    struct flexiblas_blasfn ctrsv;
    struct flexiblas_blasfn dasum;
    struct flexiblas_blasfn daxpy;
    struct flexiblas_blasfn dcopy;
    struct flexiblas_blasfn ddot;
    struct flexiblas_blasfn dgbmv;
    struct flexiblas_blasfn dgemm;
    struct flexiblas_blasfn dgemv;
    struct flexiblas_blasfn dger;
    struct flexiblas_blasfn dnrm2;
    struct flexiblas_blasfn drot;
    struct flexiblas_blasfn drotg;
    struct flexiblas_blasfn drotm;
    struct flexiblas_blasfn drotmg;
    struct flexiblas_blasfn dsbmv;
    struct flexiblas_blasfn dscal;
    struct flexiblas_blasfn dsdot;
    struct flexiblas_blasfn dspmv;
    struct flexiblas_blasfn dspr;
    struct flexiblas_blasfn dspr2;
    struct flexiblas_blasfn dswap;
    struct flexiblas_blasfn dsymm;
    struct flexiblas_blasfn dsymv;
    struct flexiblas_blasfn dsyr;
    struct flexiblas_blasfn dsyr2;
    struct flexiblas_blasfn dsyr2k;
    struct flexiblas_blasfn dsyrk;
    struct flexiblas_blasfn dtbmv;
    struct flexiblas_blasfn dtbsv;
    struct flexiblas_blasfn dtpmv;
    struct flexiblas_blasfn dtpsv;
    struct flexiblas_blasfn dtrmm;
    struct flexiblas_blasfn dtrmv;
    struct flexiblas_blasfn dtrsm;
    struct flexiblas_blasfn dtrsv;
    struct flexiblas_blasfn dzasum;
    struct flexiblas_blasfn dznrm2;
    struct flexiblas_blasfn icamax;
    struct flexiblas_blasfn idamax;
    struct flexiblas_blasfn isamax;
    struct flexiblas_blasfn izamax;
    struct flexiblas_blasfn sasum;
    struct flexiblas_blasfn saxpy;
    struct flexiblas_blasfn scasum;
    struct flexiblas_blasfn scnrm2;
    struct flexiblas_blasfn scopy;
    struct flexiblas_blasfn sdot;
    struct flexiblas_blasfn sdsdot;
    struct flexiblas_blasfn sgbmv;
    struct flexiblas_blasfn sgemm;
    struct flexiblas_blasfn sgemv;
    struct flexiblas_blasfn sger;
    struct flexiblas_blasfn snrm2;
    struct flexiblas_blasfn srot;
    struct flexiblas_blasfn srotg;
    struct flexiblas_blasfn srotm;
    struct flexiblas_blasfn srotmg;
    struct flexiblas_blasfn ssbmv;
    struct flexiblas_blasfn sscal;
    struct flexiblas_blasfn sspmv;
    struct flexiblas_blasfn sspr;
    struct flexiblas_blasfn sspr2;
    struct flexiblas_blasfn sswap;
    struct flexiblas_blasfn ssymm;
    struct flexiblas_blasfn ssymv;
    struct flexiblas_blasfn ssyr;
    struct flexiblas_blasfn ssyr2;
    struct flexiblas_blasfn ssyr2k;
    struct flexiblas_blasfn ssyrk;
    struct flexiblas_blasfn stbmv;
    struct flexiblas_blasfn stbsv;
    struct flexiblas_blasfn stpmv;
    struct flexiblas_blasfn stpsv;
    struct flexiblas_blasfn strmm;
    struct flexiblas_blasfn strmv;
    struct flexiblas_blasfn strsm;
    struct flexiblas_blasfn strsv;
    struct flexiblas_blasfn zaxpy;
    struct flexiblas_blasfn zcopy;
    struct flexiblas_blasfn zdotc;
    struct flexiblas_blasfn zdotu;
    struct flexiblas_blasfn zdrot;
    struct flexiblas_blasfn zdscal;
    struct flexiblas_blasfn zgbmv;
    struct flexiblas_blasfn zgemm;
    struct flexiblas_blasfn zgemv;
    struct flexiblas_blasfn zgerc;
    struct flexiblas_blasfn zgeru;
    struct flexiblas_blasfn zhbmv;
    struct flexiblas_blasfn zhemm;
    struct flexiblas_blasfn zhemv;
    struct flexiblas_blasfn zher;
    struct flexiblas_blasfn zher2;
    struct flexiblas_blasfn zher2k;
    struct flexiblas_blasfn zherk;
    struct flexiblas_blasfn zhpmv;
    struct flexiblas_blasfn zhpr;
    struct flexiblas_blasfn zhpr2;
    struct flexiblas_blasfn zrotg;
    struct flexiblas_blasfn zscal;
    struct flexiblas_blasfn zswap;
    struct flexiblas_blasfn zsymm;
    struct flexiblas_blasfn zsyr2k;
    struct flexiblas_blasfn zsyrk;
    struct flexiblas_blasfn ztbmv;
    struct flexiblas_blasfn ztbsv;
    struct flexiblas_blasfn ztpmv;
    struct flexiblas_blasfn ztpsv;
    struct flexiblas_blasfn ztrmm;
    struct flexiblas_blasfn ztrmv;
    struct flexiblas_blasfn ztrsm;
    struct flexiblas_blasfn ztrsv;
    struct flexiblas_blasfn dcabs1; 
    struct flexiblas_blasfn scabs1;
    struct flexiblas_blasfn cdotc_sub;
    struct flexiblas_blasfn cdotu_sub;
    struct flexiblas_blasfn zdotc_sub;
    struct flexiblas_blasfn zdotu_sub;
} flexiblas_blas_backend_t;

typedef struct _flexiblas_extblas_backend_t {
    struct flexiblas_blasfn caxpby;
    struct flexiblas_blasfn daxpby;
    struct flexiblas_blasfn zaxpby;
    struct flexiblas_blasfn saxpby;
    struct flexiblas_blasfn comatcopy;
    struct flexiblas_blasfn zomatcopy;
    struct flexiblas_blasfn domatcopy;
    struct flexiblas_blasfn somatcopy;
    struct flexiblas_blasfn cimatcopy;
    struct flexiblas_blasfn zimatcopy;
    struct flexiblas_blasfn dimatcopy;
    struct flexiblas_blasfn simatcopy;
    struct flexiblas_blasfn cgeadd; 
    struct flexiblas_blasfn dgeadd; 
    struct flexiblas_blasfn sgeadd; 
    struct flexiblas_blasfn zgeadd; 
} flexiblas_extblas_backend_t;


typedef struct _flexiblas_backend_t {
    char* name;
    void* library_handle;
    int post_init;
    pthread_mutex_t post_init_mutex;
    flexiblas_info_t info;
    flexiblas_init_function_t init_function; 
    flexiblas_exit_function_t exit_function; 
    flexiblas_info_function_t info_function;
    flexiblas_set_num_threads_function_t set_num_threads_function[2]; 
    flexiblas_get_num_threads_function_t get_num_threads_function[2];
    flexiblas_get_num_procs_function_t   get_num_procs_function[2];
    flexiblas_blas_backend_t    blas;
    flexiblas_extblas_backend_t extblas;
    struct flexiblas_blasfn xerbla; 
} flexiblas_backend_t;

// extern flexiblas_backend_t *current_backend;
#ifdef __cplusplus
}
#endif
#endif
