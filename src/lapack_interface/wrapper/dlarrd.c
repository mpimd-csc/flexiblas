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



static TLS_STORE uint8_t hook_pos_dlarrd = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlarrd,DLARRD)(char* range, char* order, blasint* n, double* vl, double* vu, blasint* il, blasint* iu, double* gers, double* reltol, double* d, double* e, double* e2, double* pivmin, blasint* nsplit, blasint* isplit, blasint* m, double* w, double* werr, double* wl, double* wu, blasint* iblock, blasint* indexw, double* work, blasint* iwork, blasint* info)
#else
void FC_GLOBAL(dlarrd,DLARRD)(char* range, char* order, blasint* n, double* vl, double* vu, blasint* il, blasint* iu, double* gers, double* reltol, double* d, double* e, double* e2, double* pivmin, blasint* nsplit, blasint* isplit, blasint* m, double* w, double* werr, double* wl, double* wu, blasint* iblock, blasint* indexw, double* work, blasint* iwork, blasint* info)
#endif
{
	void (*fn) (void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* gers, void* reltol, void* d, void* e, void* e2, void* pivmin, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wl, void* wu, void* iblock, void* indexw, void* work, void* iwork, void* info);
	void (*fn_hook) (void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* gers, void* reltol, void* d, void* e, void* e2, void* pivmin, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wl, void* wu, void* iblock, void* indexw, void* work, void* iwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.dlarrd.f77_blas_function; 
	fn_hook = __flexiblas_hooks->dlarrd.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) range, (void*) order, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) gers, (void*) reltol, (void*) d, (void*) e, (void*) e2, (void*) pivmin, (void*) nsplit, (void*) isplit, (void*) m, (void*) w, (void*) werr, (void*) wl, (void*) wu, (void*) iblock, (void*) indexw, (void*) work, (void*) iwork, (void*) info); 
		return;
	} else {
		hook_pos_dlarrd = 0;
		fn_hook((void*) range, (void*) order, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) gers, (void*) reltol, (void*) d, (void*) e, (void*) e2, (void*) pivmin, (void*) nsplit, (void*) isplit, (void*) m, (void*) w, (void*) werr, (void*) wl, (void*) wu, (void*) iblock, (void*) indexw, (void*) work, (void*) iwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dlarrd_(char* range, char* order, blasint* n, double* vl, double* vu, blasint* il, blasint* iu, double* gers, double* reltol, double* d, double* e, double* e2, double* pivmin, blasint* nsplit, blasint* isplit, blasint* m, double* w, double* werr, double* wl, double* wu, blasint* iblock, blasint* indexw, double* work, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dlarrd,DLARRD)))));
#else
void dlarrd(char* range, char* order, blasint* n, double* vl, double* vu, blasint* il, blasint* iu, double* gers, double* reltol, double* d, double* e, double* e2, double* pivmin, blasint* nsplit, blasint* isplit, blasint* m, double* w, double* werr, double* wl, double* wu, blasint* iblock, blasint* indexw, double* work, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dlarrd,DLARRD)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlarrd_(void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* gers, void* reltol, void* d, void* e, void* e2, void* pivmin, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wl, void* wu, void* iblock, void* indexw, void* work, void* iwork, void* info)
{
	void (*fn) (void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* gers, void* reltol, void* d, void* e, void* e2, void* pivmin, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wl, void* wu, void* iblock, void* indexw, void* work, void* iwork, void* info);

	fn = current_backend->lapack.dlarrd.f77_blas_function; 

		fn((void*) range, (void*) order, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) gers, (void*) reltol, (void*) d, (void*) e, (void*) e2, (void*) pivmin, (void*) nsplit, (void*) isplit, (void*) m, (void*) w, (void*) werr, (void*) wl, (void*) wu, (void*) iblock, (void*) indexw, (void*) work, (void*) iwork, (void*) info); 

	return;
}

void flexiblas_real_dlarrd(void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* gers, void* reltol, void* d, void* e, void* e2, void* pivmin, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wl, void* wu, void* iblock, void* indexw, void* work, void* iwork, void* info)  __attribute__((alias("flexiblas_real_dlarrd_")));





/* Chainloader for Hooks */


void flexiblas_chain_dlarrd_(void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* gers, void* reltol, void* d, void* e, void* e2, void* pivmin, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wl, void* wu, void* iblock, void* indexw, void* work, void* iwork, void* info)
{
	void (*fn) (void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* gers, void* reltol, void* d, void* e, void* e2, void* pivmin, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wl, void* wu, void* iblock, void* indexw, void* work, void* iwork, void* info);
	void (*fn_hook) (void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* gers, void* reltol, void* d, void* e, void* e2, void* pivmin, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wl, void* wu, void* iblock, void* indexw, void* work, void* iwork, void* info);

	fn      = current_backend->lapack.dlarrd.f77_blas_function; 

    hook_pos_dlarrd ++;
    if( hook_pos_dlarrd < __flexiblas_hooks->dlarrd.nhook) {
        fn_hook = __flexiblas_hooks->dlarrd.f77_hook_function[hook_pos_dlarrd];
        fn_hook((void*) range, (void*) order, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) gers, (void*) reltol, (void*) d, (void*) e, (void*) e2, (void*) pivmin, (void*) nsplit, (void*) isplit, (void*) m, (void*) w, (void*) werr, (void*) wl, (void*) wu, (void*) iblock, (void*) indexw, (void*) work, (void*) iwork, (void*) info);
    } else {
        hook_pos_dlarrd = 0;
		fn((void*) range, (void*) order, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) gers, (void*) reltol, (void*) d, (void*) e, (void*) e2, (void*) pivmin, (void*) nsplit, (void*) isplit, (void*) m, (void*) w, (void*) werr, (void*) wl, (void*) wu, (void*) iblock, (void*) indexw, (void*) work, (void*) iwork, (void*) info); 
	}
	return;
}

void flexiblas_chain_dlarrd(void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* gers, void* reltol, void* d, void* e, void* e2, void* pivmin, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wl, void* wu, void* iblock, void* indexw, void* work, void* iwork, void* info)  __attribute__((alias("flexiblas_chain_dlarrd_")));




