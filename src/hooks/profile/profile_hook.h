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


#ifndef PROFILE_HOOK_H

#define PROFILE_HOOK_H

#include "cscutils/table.h"
#include "flexiblas_config.h"

#ifdef FLEXIBLAS_INTEGER8
#define     ENV_FLEXIBLAS_PROFILE_FILE "FLEXIBLAS64_PROFILE_FILE"
#else
#define     ENV_FLEXIBLAS_PROFILE_FILE "FLEXIBLAS_PROFILE_FILE"
#endif


#ifdef __cplusplus
extern "C" {
#endif

    extern int __profile_verbose;

    // Helper for debug printing
#define	DPRINTF( level, ... )	do { if ( __profile_verbose >= (level)) {flexiblas_print_info( "flexiblas", __VA_ARGS__); } } while(0) // ... represents the "text" and optionally the "args"
#define	DPRINTFP( level, prefix, ... )	do { if ( __profile_verbose >= (level)) {flexiblas_print_info( prefix, __VA_ARGS__); } } while(0) // ... represents the "text" and optionally the "args"

#define	DPRINTF_WARN( level, ... )	do { if ( __profile_verbose >= (level)) {flexiblas_print_warning( "flexiblas", __VA_ARGS__); } } while(0) // ... represents the "text" and optionally the "args"
#define	DPRINTFP_WARN( level, prefix, ... )	do { if ( __profile_verbose >= (level)) {flexiblas_print_warning( prefix, __VA_ARGS__); } } while(0) // ... represents the "text" and optionally the "args"

#ifdef DEBUG
#define	DPRINTF_ERROR( level, ... )	    do { if ( __profile_verbose >= (level)) {flexiblas_print_error("flexiblas", __FILE__,__LINE__, __VA_ARGS__); } } while(0) // ... represents the "text" and optionally the "args"
#define	DPRINTFP_ERROR( level, prefix, ... )	do { if ( __profile_verbose >= (level)) {flexiblas_print_error(prefix, __FILE__,__LINE__, __VA_ARGS__); } } while(0) // ... represents the "text" and optionally the "args"
#else
#define	DPRINTF_ERROR( level, ... )	    do { if ( __profile_verbose >= (level)) {flexiblas_print_error("flexiblas", NULL,0, __VA_ARGS__); } } while(0) // ... represents the "text" and optionally the "args"
#define	DPRINTFP_ERROR( level, prefix, ... )	do { if ( __profile_verbose >= (level)) {flexiblas_print_error(prefix, NULL,0, __VA_ARGS__); } } while(0) // ... represents the "text" and optionally the "args"

#endif


#define ADD_BLAS_ENTRY(name) do { csc_table_new_row(tab); \
    csc_table_set_entry(tab, col_name,  #name); \
    csc_table_set_entry(tab, col_calls, data->name.calls[0]); \
    csc_table_set_entry(tab, col_time,  data->name.timings[0]); } while( 0 );

    void profile_lapack_add(csc_table_t *tab, int col_name, int col_calls, int col_time);

    typedef struct _profile_data_t {
        double timings[2];
        unsigned long calls[2];
    } profile_data_t;

    typedef struct _blas_calls_t {
        /*  BLAS  */
        profile_data_t caxpy;
        profile_data_t ccopy;
        profile_data_t cdotc;
        profile_data_t cdotu;
        profile_data_t cgbmv;
        profile_data_t cgemm;
        profile_data_t cgemv;
        profile_data_t cgerc;
        profile_data_t cgeru;
        profile_data_t chbmv;
        profile_data_t chemm;
        profile_data_t chemv;
        profile_data_t cher;
        profile_data_t cher2;
        profile_data_t cher2k;
        profile_data_t cherk;
        profile_data_t chpmv;
        profile_data_t chpr;
        profile_data_t chpr2;
        profile_data_t crotg;
        profile_data_t cscal;
        profile_data_t csrot;
        profile_data_t csscal;
        profile_data_t cswap;
        profile_data_t csymm;
        profile_data_t csyr2k;
        profile_data_t csyrk;
        profile_data_t ctbmv;
        profile_data_t ctbsv;
        profile_data_t ctpmv;
        profile_data_t ctpsv;
        profile_data_t ctrmm;
        profile_data_t ctrmv;
        profile_data_t ctrsm;
        profile_data_t ctrsv;
        profile_data_t dasum;
        profile_data_t daxpy;
        profile_data_t dcopy;
        profile_data_t ddot;
        profile_data_t dgbmv;
        profile_data_t dgemm;
        profile_data_t dgemv;
        profile_data_t dger;
        profile_data_t dnrm2;
        profile_data_t drot;
        profile_data_t drotg;
        profile_data_t drotm;
        profile_data_t drotmg;
        profile_data_t dsbmv;
        profile_data_t dscal;
        profile_data_t dsdot;
        profile_data_t dspmv;
        profile_data_t dspr;
        profile_data_t dspr2;
        profile_data_t dswap;
        profile_data_t dsymm;
        profile_data_t dsymv;
        profile_data_t dsyr;
        profile_data_t dsyr2;
        profile_data_t dsyr2k;
        profile_data_t dsyrk;
        profile_data_t dtbmv;
        profile_data_t dtbsv;
        profile_data_t dtpmv;
        profile_data_t dtpsv;
        profile_data_t dtrmm;
        profile_data_t dtrmv;
        profile_data_t dtrsm;
        profile_data_t dtrsv;
        profile_data_t dzasum;
        profile_data_t dznrm2;
        profile_data_t icamax;
        profile_data_t idamax;
        profile_data_t isamax;
        profile_data_t izamax;
        profile_data_t sasum;
        profile_data_t saxpy;
        profile_data_t scasum;
        profile_data_t scnrm2;
        profile_data_t scopy;
        profile_data_t sdot;
        profile_data_t sdsdot;
        profile_data_t sgbmv;
        profile_data_t sgemm;
        profile_data_t sgemv;
        profile_data_t sger;
        profile_data_t snrm2;
        profile_data_t srot;
        profile_data_t srotg;
        profile_data_t srotm;
        profile_data_t srotmg;
        profile_data_t ssbmv;
        profile_data_t sscal;
        profile_data_t sspmv;
        profile_data_t sspr;
        profile_data_t sspr2;
        profile_data_t sswap;
        profile_data_t ssymm;
        profile_data_t ssymv;
        profile_data_t ssyr;
        profile_data_t ssyr2;
        profile_data_t ssyr2k;
        profile_data_t ssyrk;
        profile_data_t stbmv;
        profile_data_t stbsv;
        profile_data_t stpmv;
        profile_data_t stpsv;
        profile_data_t strmm;
        profile_data_t strmv;
        profile_data_t strsm;
        profile_data_t strsv;
        profile_data_t zaxpy;
        profile_data_t zcopy;
        profile_data_t zdotc;
        profile_data_t zdotu;
        profile_data_t zdrot;
        profile_data_t zdscal;
        profile_data_t zgbmv;
        profile_data_t zgemm;
        profile_data_t zgemv;
        profile_data_t zgerc;
        profile_data_t zgeru;
        profile_data_t zhbmv;
        profile_data_t zhemm;
        profile_data_t zhemv;
        profile_data_t zher;
        profile_data_t zher2;
        profile_data_t zher2k;
        profile_data_t zherk;
        profile_data_t zhpmv;
        profile_data_t zhpr;
        profile_data_t zhpr2;
        profile_data_t zrotg;
        profile_data_t zscal;
        profile_data_t zswap;
        profile_data_t zsymm;
        profile_data_t zsyr2k;
        profile_data_t zsyrk;
        profile_data_t ztbmv;
        profile_data_t ztbsv;
        profile_data_t ztpmv;
        profile_data_t ztpsv;
        profile_data_t ztrmm;
        profile_data_t ztrmv;
        profile_data_t ztrsm;
        profile_data_t ztrsv;
        profile_data_t dcabs1;
        profile_data_t scabs1;
        profile_data_t cdotc_sub;
        profile_data_t cdotu_sub;
        profile_data_t zdotc_sub;
        profile_data_t zdotu_sub;
        /* EXT BLAS  */
        profile_data_t caxpby;
        profile_data_t daxpby;
        profile_data_t zaxpby;
        profile_data_t saxpby;
        profile_data_t comatcopy;
        profile_data_t zomatcopy;
        profile_data_t domatcopy;
        profile_data_t somatcopy;
        profile_data_t cimatcopy;
        profile_data_t zimatcopy;
        profile_data_t dimatcopy;
        profile_data_t simatcopy;
        profile_data_t cgeadd;
        profile_data_t dgeadd;
        profile_data_t sgeadd;
        profile_data_t zgeadd;

        profile_data_t cgemmt;
        profile_data_t dgemmt;
        profile_data_t sgemmt;
        profile_data_t zgemmt;
        profile_data_t cgemmtr;
        profile_data_t dgemmtr;
        profile_data_t sgemmtr;
        profile_data_t zgemmtr;

        /* Include LAPACK */
#include "profile_data_lapack.h"
    } blas_calls_t;

    extern blas_calls_t *data;


#ifdef __cplusplus
};
#endif

#endif /* end of include guard: PROFILE_HOOK_H */
