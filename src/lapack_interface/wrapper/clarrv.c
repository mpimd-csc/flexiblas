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


#if __GNUC__ > 7
typedef size_t fortran_charlen_t;
#else
typedef int fortran_charlen_t;
#endif

#ifdef INTEGER8
#define blasint int64_t
#else
#define blasint int
#endif



static TLS_STORE uint8_t hook_pos_clarrv = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(clarrv,CLARRV)(blasint* n, float* vl, float* vu, float* d, float* l, float* pivmin, blasint* isplit, blasint* m, blasint* dol, blasint* dou, float* minrgp, float* rtol1, float* rtol2, float* w, float* werr, float* wgap, blasint* iblock, blasint* indexw, float* gers, float complex* z, blasint* ldz, blasint* isuppz, float* work, blasint* iwork, blasint* info)
#else
void FC_GLOBAL(clarrv,CLARRV)(blasint* n, float* vl, float* vu, float* d, float* l, float* pivmin, blasint* isplit, blasint* m, blasint* dol, blasint* dou, float* minrgp, float* rtol1, float* rtol2, float* w, float* werr, float* wgap, blasint* iblock, blasint* indexw, float* gers, float complex* z, blasint* ldz, blasint* isuppz, float* work, blasint* iwork, blasint* info)
#endif
{
	void (*fn) (void* n, void* vl, void* vu, void* d, void* l, void* pivmin, void* isplit, void* m, void* dol, void* dou, void* minrgp, void* rtol1, void* rtol2, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* z, void* ldz, void* isuppz, void* work, void* iwork, void* info);
	void (*fn_hook) (void* n, void* vl, void* vu, void* d, void* l, void* pivmin, void* isplit, void* m, void* dol, void* dou, void* minrgp, void* rtol1, void* rtol2, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* z, void* ldz, void* isuppz, void* work, void* iwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.clarrv.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->clarrv.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) n, (void*) vl, (void*) vu, (void*) d, (void*) l, (void*) pivmin, (void*) isplit, (void*) m, (void*) dol, (void*) dou, (void*) minrgp, (void*) rtol1, (void*) rtol2, (void*) w, (void*) werr, (void*) wgap, (void*) iblock, (void*) indexw, (void*) gers, (void*) z, (void*) ldz, (void*) isuppz, (void*) work, (void*) iwork, (void*) info); 
		return;
	} else {
		hook_pos_clarrv = 0;
		fn_hook((void*) n, (void*) vl, (void*) vu, (void*) d, (void*) l, (void*) pivmin, (void*) isplit, (void*) m, (void*) dol, (void*) dou, (void*) minrgp, (void*) rtol1, (void*) rtol2, (void*) w, (void*) werr, (void*) wgap, (void*) iblock, (void*) indexw, (void*) gers, (void*) z, (void*) ldz, (void*) isuppz, (void*) work, (void*) iwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void clarrv_(blasint* n, float* vl, float* vu, float* d, float* l, float* pivmin, blasint* isplit, blasint* m, blasint* dol, blasint* dou, float* minrgp, float* rtol1, float* rtol2, float* w, float* werr, float* wgap, blasint* iblock, blasint* indexw, float* gers, float complex* z, blasint* ldz, blasint* isuppz, float* work, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(clarrv,CLARRV)))));
#else
#ifndef __APPLE__
void clarrv(blasint* n, float* vl, float* vu, float* d, float* l, float* pivmin, blasint* isplit, blasint* m, blasint* dol, blasint* dou, float* minrgp, float* rtol1, float* rtol2, float* w, float* werr, float* wgap, blasint* iblock, blasint* indexw, float* gers, float complex* z, blasint* ldz, blasint* isuppz, float* work, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(clarrv,CLARRV)))));
#else
void clarrv(blasint* n, float* vl, float* vu, float* d, float* l, float* pivmin, blasint* isplit, blasint* m, blasint* dol, blasint* dou, float* minrgp, float* rtol1, float* rtol2, float* w, float* werr, float* wgap, blasint* iblock, blasint* indexw, float* gers, float complex* z, blasint* ldz, blasint* isuppz, float* work, blasint* iwork, blasint* info){ FC_GLOBAL(clarrv,CLARRV)((void*) n, (void*) vl, (void*) vu, (void*) d, (void*) l, (void*) pivmin, (void*) isplit, (void*) m, (void*) dol, (void*) dou, (void*) minrgp, (void*) rtol1, (void*) rtol2, (void*) w, (void*) werr, (void*) wgap, (void*) iblock, (void*) indexw, (void*) gers, (void*) z, (void*) ldz, (void*) isuppz, (void*) work, (void*) iwork, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_clarrv_(void* n, void* vl, void* vu, void* d, void* l, void* pivmin, void* isplit, void* m, void* dol, void* dou, void* minrgp, void* rtol1, void* rtol2, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* z, void* ldz, void* isuppz, void* work, void* iwork, void* info)
{
	void (*fn) (void* n, void* vl, void* vu, void* d, void* l, void* pivmin, void* isplit, void* m, void* dol, void* dou, void* minrgp, void* rtol1, void* rtol2, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* z, void* ldz, void* isuppz, void* work, void* iwork, void* info);

	*(void **) & fn = current_backend->lapack.clarrv.f77_blas_function; 

		fn((void*) n, (void*) vl, (void*) vu, (void*) d, (void*) l, (void*) pivmin, (void*) isplit, (void*) m, (void*) dol, (void*) dou, (void*) minrgp, (void*) rtol1, (void*) rtol2, (void*) w, (void*) werr, (void*) wgap, (void*) iblock, (void*) indexw, (void*) gers, (void*) z, (void*) ldz, (void*) isuppz, (void*) work, (void*) iwork, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_clarrv(void* n, void* vl, void* vu, void* d, void* l, void* pivmin, void* isplit, void* m, void* dol, void* dou, void* minrgp, void* rtol1, void* rtol2, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* z, void* ldz, void* isuppz, void* work, void* iwork, void* info) __attribute__((alias("flexiblas_real_clarrv_")));
#else
void flexiblas_real_clarrv(void* n, void* vl, void* vu, void* d, void* l, void* pivmin, void* isplit, void* m, void* dol, void* dou, void* minrgp, void* rtol1, void* rtol2, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* z, void* ldz, void* isuppz, void* work, void* iwork, void* info){flexiblas_real_clarrv_((void*) n, (void*) vl, (void*) vu, (void*) d, (void*) l, (void*) pivmin, (void*) isplit, (void*) m, (void*) dol, (void*) dou, (void*) minrgp, (void*) rtol1, (void*) rtol2, (void*) w, (void*) werr, (void*) wgap, (void*) iblock, (void*) indexw, (void*) gers, (void*) z, (void*) ldz, (void*) isuppz, (void*) work, (void*) iwork, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_clarrv_(void* n, void* vl, void* vu, void* d, void* l, void* pivmin, void* isplit, void* m, void* dol, void* dou, void* minrgp, void* rtol1, void* rtol2, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* z, void* ldz, void* isuppz, void* work, void* iwork, void* info)
{
	void (*fn) (void* n, void* vl, void* vu, void* d, void* l, void* pivmin, void* isplit, void* m, void* dol, void* dou, void* minrgp, void* rtol1, void* rtol2, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* z, void* ldz, void* isuppz, void* work, void* iwork, void* info);
	void (*fn_hook) (void* n, void* vl, void* vu, void* d, void* l, void* pivmin, void* isplit, void* m, void* dol, void* dou, void* minrgp, void* rtol1, void* rtol2, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* z, void* ldz, void* isuppz, void* work, void* iwork, void* info);

	*(void **) &fn      = current_backend->lapack.clarrv.f77_blas_function; 

    hook_pos_clarrv ++;
    if( hook_pos_clarrv < __flexiblas_hooks->clarrv.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->clarrv.f77_hook_function[hook_pos_clarrv];
        fn_hook((void*) n, (void*) vl, (void*) vu, (void*) d, (void*) l, (void*) pivmin, (void*) isplit, (void*) m, (void*) dol, (void*) dou, (void*) minrgp, (void*) rtol1, (void*) rtol2, (void*) w, (void*) werr, (void*) wgap, (void*) iblock, (void*) indexw, (void*) gers, (void*) z, (void*) ldz, (void*) isuppz, (void*) work, (void*) iwork, (void*) info);
    } else {
        hook_pos_clarrv = 0;
		fn((void*) n, (void*) vl, (void*) vu, (void*) d, (void*) l, (void*) pivmin, (void*) isplit, (void*) m, (void*) dol, (void*) dou, (void*) minrgp, (void*) rtol1, (void*) rtol2, (void*) w, (void*) werr, (void*) wgap, (void*) iblock, (void*) indexw, (void*) gers, (void*) z, (void*) ldz, (void*) isuppz, (void*) work, (void*) iwork, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_clarrv(void* n, void* vl, void* vu, void* d, void* l, void* pivmin, void* isplit, void* m, void* dol, void* dou, void* minrgp, void* rtol1, void* rtol2, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* z, void* ldz, void* isuppz, void* work, void* iwork, void* info) __attribute__((alias("flexiblas_chain_clarrv_")));
#else
void flexiblas_chain_clarrv(void* n, void* vl, void* vu, void* d, void* l, void* pivmin, void* isplit, void* m, void* dol, void* dou, void* minrgp, void* rtol1, void* rtol2, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* z, void* ldz, void* isuppz, void* work, void* iwork, void* info){flexiblas_chain_clarrv_((void*) n, (void*) vl, (void*) vu, (void*) d, (void*) l, (void*) pivmin, (void*) isplit, (void*) m, (void*) dol, (void*) dou, (void*) minrgp, (void*) rtol1, (void*) rtol2, (void*) w, (void*) werr, (void*) wgap, (void*) iblock, (void*) indexw, (void*) gers, (void*) z, (void*) ldz, (void*) isuppz, (void*) work, (void*) iwork, (void*) info);}
#endif



