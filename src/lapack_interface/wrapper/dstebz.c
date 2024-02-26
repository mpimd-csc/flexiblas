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
#include <stdint.h>
#include <complex.h>

#include "flexiblas_fortran_mangle.h"

#include "flexiblas.h"


#ifndef FLEXIBLAS_CHARLEN_T
#define FLEXIBLAS_CHARLEN_T
#if __GNUC__ > 7
typedef size_t flexiblas_fortran_charlen_t;
#else
typedef int flexiblas_fortran_charlen_t;
#endif
#endif

#ifndef blasint
#ifdef FLEXIBLAS_INTEGER8
#define blasint int64_t
#else
#define blasint int
#endif
#endif



static TLS_STORE uint8_t hook_pos_dstebz = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dstebz,DSTEBZ)(char* range, char* order, blasint* n, double* vl, double* vu, blasint* il, blasint* iu, double* abstol, double* d, double* e, blasint* m, blasint* nsplit, double* w, blasint* iblock, blasint* isplit, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order)
#else
void FC_GLOBAL(dstebz,DSTEBZ)(char* range, char* order, blasint* n, double* vl, double* vu, blasint* il, blasint* iu, double* abstol, double* d, double* e, blasint* m, blasint* nsplit, double* w, blasint* iblock, blasint* isplit, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order)
#endif
{
	void (*fn) (void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* abstol, void* d, void* e, void* m, void* nsplit, void* w, void* iblock, void* isplit, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order);
	void (*fn_hook) (void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* abstol, void* d, void* e, void* m, void* nsplit, void* w, void* iblock, void* isplit, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.dstebz.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->dstebz.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) range, (void*) order, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) abstol, (void*) d, (void*) e, (void*) m, (void*) nsplit, (void*) w, (void*) iblock, (void*) isplit, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_range, ( flexiblas_fortran_charlen_t ) len_order); 
		return;
	} else {
		hook_pos_dstebz = 0;
		fn_hook((void*) range, (void*) order, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) abstol, (void*) d, (void*) e, (void*) m, (void*) nsplit, (void*) w, (void*) iblock, (void*) isplit, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_range, ( flexiblas_fortran_charlen_t ) len_order);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dstebz_(char* range, char* order, blasint* n, double* vl, double* vu, blasint* il, blasint* iu, double* abstol, double* d, double* e, blasint* m, blasint* nsplit, double* w, blasint* iblock, blasint* isplit, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order) __attribute__((alias(MTS(FC_GLOBAL(dstebz,DSTEBZ)))));
#else
#ifndef __APPLE__
void dstebz(char* range, char* order, blasint* n, double* vl, double* vu, blasint* il, blasint* iu, double* abstol, double* d, double* e, blasint* m, blasint* nsplit, double* w, blasint* iblock, blasint* isplit, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order) __attribute__((alias(MTS(FC_GLOBAL(dstebz,DSTEBZ)))));
#else
void dstebz(char* range, char* order, blasint* n, double* vl, double* vu, blasint* il, blasint* iu, double* abstol, double* d, double* e, blasint* m, blasint* nsplit, double* w, blasint* iblock, blasint* isplit, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order){ FC_GLOBAL(dstebz,DSTEBZ)((void*) range, (void*) order, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) abstol, (void*) d, (void*) e, (void*) m, (void*) nsplit, (void*) w, (void*) iblock, (void*) isplit, (void*) work, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_range, (flexiblas_fortran_charlen_t) len_order); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dstebz_(void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* abstol, void* d, void* e, void* m, void* nsplit, void* w, void* iblock, void* isplit, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order)
{
	void (*fn) (void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* abstol, void* d, void* e, void* m, void* nsplit, void* w, void* iblock, void* isplit, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order);

	*(void **) & fn = current_backend->lapack.dstebz.f77_blas_function; 

		fn((void*) range, (void*) order, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) abstol, (void*) d, (void*) e, (void*) m, (void*) nsplit, (void*) w, (void*) iblock, (void*) isplit, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_range, ( flexiblas_fortran_charlen_t ) len_order); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_dstebz(void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* abstol, void* d, void* e, void* m, void* nsplit, void* w, void* iblock, void* isplit, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order) __attribute__((alias("flexiblas_real_dstebz_")));
#else
void flexiblas_real_dstebz(void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* abstol, void* d, void* e, void* m, void* nsplit, void* w, void* iblock, void* isplit, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order){flexiblas_real_dstebz_((void*) range, (void*) order, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) abstol, (void*) d, (void*) e, (void*) m, (void*) nsplit, (void*) w, (void*) iblock, (void*) isplit, (void*) work, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_range, (flexiblas_fortran_charlen_t) len_order);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dstebz_(void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* abstol, void* d, void* e, void* m, void* nsplit, void* w, void* iblock, void* isplit, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order)
{
	void (*fn) (void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* abstol, void* d, void* e, void* m, void* nsplit, void* w, void* iblock, void* isplit, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order);
	void (*fn_hook) (void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* abstol, void* d, void* e, void* m, void* nsplit, void* w, void* iblock, void* isplit, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order);

	*(void **) &fn      = current_backend->lapack.dstebz.f77_blas_function; 

    hook_pos_dstebz ++;
    if( hook_pos_dstebz < __flexiblas_hooks->dstebz.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dstebz.f77_hook_function[hook_pos_dstebz];
        fn_hook((void*) range, (void*) order, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) abstol, (void*) d, (void*) e, (void*) m, (void*) nsplit, (void*) w, (void*) iblock, (void*) isplit, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_range, ( flexiblas_fortran_charlen_t ) len_order);
    } else {
        hook_pos_dstebz = 0;
		fn((void*) range, (void*) order, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) abstol, (void*) d, (void*) e, (void*) m, (void*) nsplit, (void*) w, (void*) iblock, (void*) isplit, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_range, ( flexiblas_fortran_charlen_t ) len_order); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_dstebz(void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* abstol, void* d, void* e, void* m, void* nsplit, void* w, void* iblock, void* isplit, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order) __attribute__((alias("flexiblas_chain_dstebz_")));
#else
void flexiblas_chain_dstebz(void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* abstol, void* d, void* e, void* m, void* nsplit, void* w, void* iblock, void* isplit, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order){flexiblas_chain_dstebz_((void*) range, (void*) order, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) abstol, (void*) d, (void*) e, (void*) m, (void*) nsplit, (void*) w, (void*) iblock, (void*) isplit, (void*) work, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_range, (flexiblas_fortran_charlen_t) len_order);}
#endif



