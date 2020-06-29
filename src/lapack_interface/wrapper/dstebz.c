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
 * Copyright (C) Martin Koehler, 2013-2020
 */
 /* This file it automatically generated. Please do not edit. */
 /* Generated: Wed Mar 28 11:20:04 2018 */
        
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <complex.h>

#include "fortran_mangle.h"

#include "flexiblas.h"


#ifdef INTEGER8
#define blasint int64_t
#else
#define blasint int
#endif



static TLS_STORE uint8_t hook_pos_dstebz = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dstebz,DSTEBZ)(char* range, char* order, blasint* n, double* vl, double* vu, blasint* il, blasint* iu, double* abstol, double* d, double* e, blasint* m, blasint* nsplit, double* w, blasint* iblock, blasint* isplit, double* work, blasint* iwork, blasint* info)
#else
void FC_GLOBAL(dstebz,DSTEBZ)(char* range, char* order, blasint* n, double* vl, double* vu, blasint* il, blasint* iu, double* abstol, double* d, double* e, blasint* m, blasint* nsplit, double* w, blasint* iblock, blasint* isplit, double* work, blasint* iwork, blasint* info)
#endif
{
	void (*fn) (void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* abstol, void* d, void* e, void* m, void* nsplit, void* w, void* iblock, void* isplit, void* work, void* iwork, void* info);
	void (*fn_hook) (void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* abstol, void* d, void* e, void* m, void* nsplit, void* w, void* iblock, void* isplit, void* work, void* iwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.dstebz.f77_blas_function; 
	fn_hook = __flexiblas_hooks->dstebz.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) range, (void*) order, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) abstol, (void*) d, (void*) e, (void*) m, (void*) nsplit, (void*) w, (void*) iblock, (void*) isplit, (void*) work, (void*) iwork, (void*) info); 
		return;
	} else {
		hook_pos_dstebz = 0;
		fn_hook((void*) range, (void*) order, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) abstol, (void*) d, (void*) e, (void*) m, (void*) nsplit, (void*) w, (void*) iblock, (void*) isplit, (void*) work, (void*) iwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dstebz_(char* range, char* order, blasint* n, double* vl, double* vu, blasint* il, blasint* iu, double* abstol, double* d, double* e, blasint* m, blasint* nsplit, double* w, blasint* iblock, blasint* isplit, double* work, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dstebz,DSTEBZ)))));
#else
void dstebz(char* range, char* order, blasint* n, double* vl, double* vu, blasint* il, blasint* iu, double* abstol, double* d, double* e, blasint* m, blasint* nsplit, double* w, blasint* iblock, blasint* isplit, double* work, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dstebz,DSTEBZ)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dstebz_(void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* abstol, void* d, void* e, void* m, void* nsplit, void* w, void* iblock, void* isplit, void* work, void* iwork, void* info)
{
	void (*fn) (void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* abstol, void* d, void* e, void* m, void* nsplit, void* w, void* iblock, void* isplit, void* work, void* iwork, void* info);

	fn = current_backend->lapack.dstebz.f77_blas_function; 

		fn((void*) range, (void*) order, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) abstol, (void*) d, (void*) e, (void*) m, (void*) nsplit, (void*) w, (void*) iblock, (void*) isplit, (void*) work, (void*) iwork, (void*) info); 

	return;
}

void flexiblas_real_dstebz(void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* abstol, void* d, void* e, void* m, void* nsplit, void* w, void* iblock, void* isplit, void* work, void* iwork, void* info)  __attribute__((alias("flexiblas_real_dstebz_")));





/* Chainloader for Hooks */


void flexiblas_chain_dstebz_(void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* abstol, void* d, void* e, void* m, void* nsplit, void* w, void* iblock, void* isplit, void* work, void* iwork, void* info)
{
	void (*fn) (void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* abstol, void* d, void* e, void* m, void* nsplit, void* w, void* iblock, void* isplit, void* work, void* iwork, void* info);
	void (*fn_hook) (void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* abstol, void* d, void* e, void* m, void* nsplit, void* w, void* iblock, void* isplit, void* work, void* iwork, void* info);

	fn      = current_backend->lapack.dstebz.f77_blas_function; 

    hook_pos_dstebz ++;
    if( hook_pos_dstebz < __flexiblas_hooks->dstebz.nhook) {
        fn_hook = __flexiblas_hooks->dstebz.f77_hook_function[hook_pos_dstebz];
        fn_hook((void*) range, (void*) order, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) abstol, (void*) d, (void*) e, (void*) m, (void*) nsplit, (void*) w, (void*) iblock, (void*) isplit, (void*) work, (void*) iwork, (void*) info);
    } else {
        hook_pos_dstebz = 0;
		fn((void*) range, (void*) order, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) abstol, (void*) d, (void*) e, (void*) m, (void*) nsplit, (void*) w, (void*) iblock, (void*) isplit, (void*) work, (void*) iwork, (void*) info); 
	}
	return;
}

void flexiblas_chain_dstebz(void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* abstol, void* d, void* e, void* m, void* nsplit, void* w, void* iblock, void* isplit, void* work, void* iwork, void* info)  __attribute__((alias("flexiblas_chain_dstebz_")));




