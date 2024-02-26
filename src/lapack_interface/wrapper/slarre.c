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



static TLS_STORE uint8_t hook_pos_slarre = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slarre,SLARRE)(char* range, blasint* n, float* vl, float* vu, blasint* il, blasint* iu, float* d, float* e, float* e2, float* rtol1, float* rtol2, float* spltol, blasint* nsplit, blasint* isplit, blasint* m, float* w, float* werr, float* wgap, blasint* iblock, blasint* indexw, float* gers, float* pivmin, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_range)
#else
void FC_GLOBAL(slarre,SLARRE)(char* range, blasint* n, float* vl, float* vu, blasint* il, blasint* iu, float* d, float* e, float* e2, float* rtol1, float* rtol2, float* spltol, blasint* nsplit, blasint* isplit, blasint* m, float* w, float* werr, float* wgap, blasint* iblock, blasint* indexw, float* gers, float* pivmin, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_range)
#endif
{
	void (*fn) (void* range, void* n, void* vl, void* vu, void* il, void* iu, void* d, void* e, void* e2, void* rtol1, void* rtol2, void* spltol, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* pivmin, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range);
	void (*fn_hook) (void* range, void* n, void* vl, void* vu, void* il, void* iu, void* d, void* e, void* e2, void* rtol1, void* rtol2, void* spltol, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* pivmin, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.slarre.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->slarre.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) range, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) d, (void*) e, (void*) e2, (void*) rtol1, (void*) rtol2, (void*) spltol, (void*) nsplit, (void*) isplit, (void*) m, (void*) w, (void*) werr, (void*) wgap, (void*) iblock, (void*) indexw, (void*) gers, (void*) pivmin, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_range); 
		return;
	} else {
		hook_pos_slarre = 0;
		fn_hook((void*) range, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) d, (void*) e, (void*) e2, (void*) rtol1, (void*) rtol2, (void*) spltol, (void*) nsplit, (void*) isplit, (void*) m, (void*) w, (void*) werr, (void*) wgap, (void*) iblock, (void*) indexw, (void*) gers, (void*) pivmin, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_range);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void slarre_(char* range, blasint* n, float* vl, float* vu, blasint* il, blasint* iu, float* d, float* e, float* e2, float* rtol1, float* rtol2, float* spltol, blasint* nsplit, blasint* isplit, blasint* m, float* w, float* werr, float* wgap, blasint* iblock, blasint* indexw, float* gers, float* pivmin, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_range) __attribute__((alias(MTS(FC_GLOBAL(slarre,SLARRE)))));
#else
#ifndef __APPLE__
void slarre(char* range, blasint* n, float* vl, float* vu, blasint* il, blasint* iu, float* d, float* e, float* e2, float* rtol1, float* rtol2, float* spltol, blasint* nsplit, blasint* isplit, blasint* m, float* w, float* werr, float* wgap, blasint* iblock, blasint* indexw, float* gers, float* pivmin, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_range) __attribute__((alias(MTS(FC_GLOBAL(slarre,SLARRE)))));
#else
void slarre(char* range, blasint* n, float* vl, float* vu, blasint* il, blasint* iu, float* d, float* e, float* e2, float* rtol1, float* rtol2, float* spltol, blasint* nsplit, blasint* isplit, blasint* m, float* w, float* werr, float* wgap, blasint* iblock, blasint* indexw, float* gers, float* pivmin, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_range){ FC_GLOBAL(slarre,SLARRE)((void*) range, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) d, (void*) e, (void*) e2, (void*) rtol1, (void*) rtol2, (void*) spltol, (void*) nsplit, (void*) isplit, (void*) m, (void*) w, (void*) werr, (void*) wgap, (void*) iblock, (void*) indexw, (void*) gers, (void*) pivmin, (void*) work, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_range); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slarre_(void* range, void* n, void* vl, void* vu, void* il, void* iu, void* d, void* e, void* e2, void* rtol1, void* rtol2, void* spltol, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* pivmin, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range)
{
	void (*fn) (void* range, void* n, void* vl, void* vu, void* il, void* iu, void* d, void* e, void* e2, void* rtol1, void* rtol2, void* spltol, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* pivmin, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range);

	*(void **) & fn = current_backend->lapack.slarre.f77_blas_function; 

		fn((void*) range, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) d, (void*) e, (void*) e2, (void*) rtol1, (void*) rtol2, (void*) spltol, (void*) nsplit, (void*) isplit, (void*) m, (void*) w, (void*) werr, (void*) wgap, (void*) iblock, (void*) indexw, (void*) gers, (void*) pivmin, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_range); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_slarre(void* range, void* n, void* vl, void* vu, void* il, void* iu, void* d, void* e, void* e2, void* rtol1, void* rtol2, void* spltol, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* pivmin, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range) __attribute__((alias("flexiblas_real_slarre_")));
#else
void flexiblas_real_slarre(void* range, void* n, void* vl, void* vu, void* il, void* iu, void* d, void* e, void* e2, void* rtol1, void* rtol2, void* spltol, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* pivmin, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range){flexiblas_real_slarre_((void*) range, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) d, (void*) e, (void*) e2, (void*) rtol1, (void*) rtol2, (void*) spltol, (void*) nsplit, (void*) isplit, (void*) m, (void*) w, (void*) werr, (void*) wgap, (void*) iblock, (void*) indexw, (void*) gers, (void*) pivmin, (void*) work, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_range);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_slarre_(void* range, void* n, void* vl, void* vu, void* il, void* iu, void* d, void* e, void* e2, void* rtol1, void* rtol2, void* spltol, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* pivmin, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range)
{
	void (*fn) (void* range, void* n, void* vl, void* vu, void* il, void* iu, void* d, void* e, void* e2, void* rtol1, void* rtol2, void* spltol, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* pivmin, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range);
	void (*fn_hook) (void* range, void* n, void* vl, void* vu, void* il, void* iu, void* d, void* e, void* e2, void* rtol1, void* rtol2, void* spltol, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* pivmin, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range);

	*(void **) &fn      = current_backend->lapack.slarre.f77_blas_function; 

    hook_pos_slarre ++;
    if( hook_pos_slarre < __flexiblas_hooks->slarre.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->slarre.f77_hook_function[hook_pos_slarre];
        fn_hook((void*) range, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) d, (void*) e, (void*) e2, (void*) rtol1, (void*) rtol2, (void*) spltol, (void*) nsplit, (void*) isplit, (void*) m, (void*) w, (void*) werr, (void*) wgap, (void*) iblock, (void*) indexw, (void*) gers, (void*) pivmin, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_range);
    } else {
        hook_pos_slarre = 0;
		fn((void*) range, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) d, (void*) e, (void*) e2, (void*) rtol1, (void*) rtol2, (void*) spltol, (void*) nsplit, (void*) isplit, (void*) m, (void*) w, (void*) werr, (void*) wgap, (void*) iblock, (void*) indexw, (void*) gers, (void*) pivmin, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_range); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_slarre(void* range, void* n, void* vl, void* vu, void* il, void* iu, void* d, void* e, void* e2, void* rtol1, void* rtol2, void* spltol, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* pivmin, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range) __attribute__((alias("flexiblas_chain_slarre_")));
#else
void flexiblas_chain_slarre(void* range, void* n, void* vl, void* vu, void* il, void* iu, void* d, void* e, void* e2, void* rtol1, void* rtol2, void* spltol, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* pivmin, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range){flexiblas_chain_slarre_((void*) range, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) d, (void*) e, (void*) e2, (void*) rtol1, (void*) rtol2, (void*) spltol, (void*) nsplit, (void*) isplit, (void*) m, (void*) w, (void*) werr, (void*) wgap, (void*) iblock, (void*) indexw, (void*) gers, (void*) pivmin, (void*) work, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_range);}
#endif



