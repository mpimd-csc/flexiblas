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

#include "flexiblas_fortran_char_len.h"


static TLS_STORE uint8_t hook_pos_dlarrd = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlarrd,DLARRD)(char* range, char* order, blasint* n, double* vl, double* vu, blasint* il, blasint* iu, double* gers, double* reltol, double* d, double* e, double* e2, double* pivmin, blasint* nsplit, blasint* isplit, blasint* m, double* w, double* werr, double* wl, double* wu, blasint* iblock, blasint* indexw, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order)
#else
void FC_GLOBAL(dlarrd,DLARRD)(char* range, char* order, blasint* n, double* vl, double* vu, blasint* il, blasint* iu, double* gers, double* reltol, double* d, double* e, double* e2, double* pivmin, blasint* nsplit, blasint* isplit, blasint* m, double* w, double* werr, double* wl, double* wu, blasint* iblock, blasint* indexw, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order)
#endif
{
	void (*fn) (void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* gers, void* reltol, void* d, void* e, void* e2, void* pivmin, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wl, void* wu, void* iblock, void* indexw, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order);
	void (*fn_hook) (void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* gers, void* reltol, void* d, void* e, void* e2, void* pivmin, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wl, void* wu, void* iblock, void* indexw, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.dlarrd.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->dlarrd.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) range, (void*) order, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) gers, (void*) reltol, (void*) d, (void*) e, (void*) e2, (void*) pivmin, (void*) nsplit, (void*) isplit, (void*) m, (void*) w, (void*) werr, (void*) wl, (void*) wu, (void*) iblock, (void*) indexw, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_range, ( flexiblas_fortran_charlen_t ) len_order); 
		return;
	} else {
		hook_pos_dlarrd = 0;
		fn_hook((void*) range, (void*) order, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) gers, (void*) reltol, (void*) d, (void*) e, (void*) e2, (void*) pivmin, (void*) nsplit, (void*) isplit, (void*) m, (void*) w, (void*) werr, (void*) wl, (void*) wu, (void*) iblock, (void*) indexw, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_range, ( flexiblas_fortran_charlen_t ) len_order);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dlarrd_(char* range, char* order, blasint* n, double* vl, double* vu, blasint* il, blasint* iu, double* gers, double* reltol, double* d, double* e, double* e2, double* pivmin, blasint* nsplit, blasint* isplit, blasint* m, double* w, double* werr, double* wl, double* wu, blasint* iblock, blasint* indexw, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order) __attribute__((alias(MTS(FC_GLOBAL(dlarrd,DLARRD)))));
#else
#ifndef __APPLE__
void dlarrd(char* range, char* order, blasint* n, double* vl, double* vu, blasint* il, blasint* iu, double* gers, double* reltol, double* d, double* e, double* e2, double* pivmin, blasint* nsplit, blasint* isplit, blasint* m, double* w, double* werr, double* wl, double* wu, blasint* iblock, blasint* indexw, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order) __attribute__((alias(MTS(FC_GLOBAL(dlarrd,DLARRD)))));
#else
void dlarrd(char* range, char* order, blasint* n, double* vl, double* vu, blasint* il, blasint* iu, double* gers, double* reltol, double* d, double* e, double* e2, double* pivmin, blasint* nsplit, blasint* isplit, blasint* m, double* w, double* werr, double* wl, double* wu, blasint* iblock, blasint* indexw, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order){ FC_GLOBAL(dlarrd,DLARRD)((void*) range, (void*) order, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) gers, (void*) reltol, (void*) d, (void*) e, (void*) e2, (void*) pivmin, (void*) nsplit, (void*) isplit, (void*) m, (void*) w, (void*) werr, (void*) wl, (void*) wu, (void*) iblock, (void*) indexw, (void*) work, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_range, (flexiblas_fortran_charlen_t) len_order); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlarrd_(void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* gers, void* reltol, void* d, void* e, void* e2, void* pivmin, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wl, void* wu, void* iblock, void* indexw, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order)
{
	void (*fn) (void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* gers, void* reltol, void* d, void* e, void* e2, void* pivmin, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wl, void* wu, void* iblock, void* indexw, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order);

	*(void **) & fn = current_backend->lapack.dlarrd.f77_blas_function; 

		fn((void*) range, (void*) order, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) gers, (void*) reltol, (void*) d, (void*) e, (void*) e2, (void*) pivmin, (void*) nsplit, (void*) isplit, (void*) m, (void*) w, (void*) werr, (void*) wl, (void*) wu, (void*) iblock, (void*) indexw, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_range, ( flexiblas_fortran_charlen_t ) len_order); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_dlarrd(void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* gers, void* reltol, void* d, void* e, void* e2, void* pivmin, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wl, void* wu, void* iblock, void* indexw, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order) __attribute__((alias("flexiblas_real_dlarrd_")));
#else
void flexiblas_real_dlarrd(void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* gers, void* reltol, void* d, void* e, void* e2, void* pivmin, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wl, void* wu, void* iblock, void* indexw, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order){flexiblas_real_dlarrd_((void*) range, (void*) order, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) gers, (void*) reltol, (void*) d, (void*) e, (void*) e2, (void*) pivmin, (void*) nsplit, (void*) isplit, (void*) m, (void*) w, (void*) werr, (void*) wl, (void*) wu, (void*) iblock, (void*) indexw, (void*) work, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_range, (flexiblas_fortran_charlen_t) len_order);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dlarrd_(void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* gers, void* reltol, void* d, void* e, void* e2, void* pivmin, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wl, void* wu, void* iblock, void* indexw, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order)
{
	void (*fn) (void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* gers, void* reltol, void* d, void* e, void* e2, void* pivmin, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wl, void* wu, void* iblock, void* indexw, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order);
	void (*fn_hook) (void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* gers, void* reltol, void* d, void* e, void* e2, void* pivmin, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wl, void* wu, void* iblock, void* indexw, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order);

	*(void **) &fn      = current_backend->lapack.dlarrd.f77_blas_function; 

    hook_pos_dlarrd ++;
    if( hook_pos_dlarrd < __flexiblas_hooks->dlarrd.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlarrd.f77_hook_function[hook_pos_dlarrd];
        fn_hook((void*) range, (void*) order, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) gers, (void*) reltol, (void*) d, (void*) e, (void*) e2, (void*) pivmin, (void*) nsplit, (void*) isplit, (void*) m, (void*) w, (void*) werr, (void*) wl, (void*) wu, (void*) iblock, (void*) indexw, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_range, ( flexiblas_fortran_charlen_t ) len_order);
    } else {
        hook_pos_dlarrd = 0;
		fn((void*) range, (void*) order, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) gers, (void*) reltol, (void*) d, (void*) e, (void*) e2, (void*) pivmin, (void*) nsplit, (void*) isplit, (void*) m, (void*) w, (void*) werr, (void*) wl, (void*) wu, (void*) iblock, (void*) indexw, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_range, ( flexiblas_fortran_charlen_t ) len_order); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_dlarrd(void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* gers, void* reltol, void* d, void* e, void* e2, void* pivmin, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wl, void* wu, void* iblock, void* indexw, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order) __attribute__((alias("flexiblas_chain_dlarrd_")));
#else
void flexiblas_chain_dlarrd(void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* gers, void* reltol, void* d, void* e, void* e2, void* pivmin, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wl, void* wu, void* iblock, void* indexw, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order){flexiblas_chain_dlarrd_((void*) range, (void*) order, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) gers, (void*) reltol, (void*) d, (void*) e, (void*) e2, (void*) pivmin, (void*) nsplit, (void*) isplit, (void*) m, (void*) w, (void*) werr, (void*) wl, (void*) wu, (void*) iblock, (void*) indexw, (void*) work, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_range, (flexiblas_fortran_charlen_t) len_order);}
#endif



