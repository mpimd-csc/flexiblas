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


static TLS_STORE uint8_t hook_pos_dlarrv = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlarrv,DLARRV)(blasint* n, double* vl, double* vu, double* d, double* l, double* pivmin, blasint* isplit, blasint* m, blasint* dol, blasint* dou, double* minrgp, double* rtol1, double* rtol2, double* w, double* werr, double* wgap, blasint* iblock, blasint* indexw, double* gers, double* z, blasint* ldz, blasint* isuppz, double* work, blasint* iwork, blasint* info)
#else
void FC_GLOBAL(dlarrv,DLARRV)(blasint* n, double* vl, double* vu, double* d, double* l, double* pivmin, blasint* isplit, blasint* m, blasint* dol, blasint* dou, double* minrgp, double* rtol1, double* rtol2, double* w, double* werr, double* wgap, blasint* iblock, blasint* indexw, double* gers, double* z, blasint* ldz, blasint* isuppz, double* work, blasint* iwork, blasint* info)
#endif
{
	void (*fn) (void* n, void* vl, void* vu, void* d, void* l, void* pivmin, void* isplit, void* m, void* dol, void* dou, void* minrgp, void* rtol1, void* rtol2, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* z, void* ldz, void* isuppz, void* work, void* iwork, void* info);
	void (*fn_hook) (void* n, void* vl, void* vu, void* d, void* l, void* pivmin, void* isplit, void* m, void* dol, void* dou, void* minrgp, void* rtol1, void* rtol2, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* z, void* ldz, void* isuppz, void* work, void* iwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.dlarrv.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->dlarrv.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) n, (void*) vl, (void*) vu, (void*) d, (void*) l, (void*) pivmin, (void*) isplit, (void*) m, (void*) dol, (void*) dou, (void*) minrgp, (void*) rtol1, (void*) rtol2, (void*) w, (void*) werr, (void*) wgap, (void*) iblock, (void*) indexw, (void*) gers, (void*) z, (void*) ldz, (void*) isuppz, (void*) work, (void*) iwork, (void*) info); 
		return;
	} else {
		hook_pos_dlarrv = 0;
		fn_hook((void*) n, (void*) vl, (void*) vu, (void*) d, (void*) l, (void*) pivmin, (void*) isplit, (void*) m, (void*) dol, (void*) dou, (void*) minrgp, (void*) rtol1, (void*) rtol2, (void*) w, (void*) werr, (void*) wgap, (void*) iblock, (void*) indexw, (void*) gers, (void*) z, (void*) ldz, (void*) isuppz, (void*) work, (void*) iwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dlarrv_(blasint* n, double* vl, double* vu, double* d, double* l, double* pivmin, blasint* isplit, blasint* m, blasint* dol, blasint* dou, double* minrgp, double* rtol1, double* rtol2, double* w, double* werr, double* wgap, blasint* iblock, blasint* indexw, double* gers, double* z, blasint* ldz, blasint* isuppz, double* work, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dlarrv,DLARRV)))));
#else
#ifndef __APPLE__
void dlarrv(blasint* n, double* vl, double* vu, double* d, double* l, double* pivmin, blasint* isplit, blasint* m, blasint* dol, blasint* dou, double* minrgp, double* rtol1, double* rtol2, double* w, double* werr, double* wgap, blasint* iblock, blasint* indexw, double* gers, double* z, blasint* ldz, blasint* isuppz, double* work, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dlarrv,DLARRV)))));
#else
void dlarrv(blasint* n, double* vl, double* vu, double* d, double* l, double* pivmin, blasint* isplit, blasint* m, blasint* dol, blasint* dou, double* minrgp, double* rtol1, double* rtol2, double* w, double* werr, double* wgap, blasint* iblock, blasint* indexw, double* gers, double* z, blasint* ldz, blasint* isuppz, double* work, blasint* iwork, blasint* info){ FC_GLOBAL(dlarrv,DLARRV)((void*) n, (void*) vl, (void*) vu, (void*) d, (void*) l, (void*) pivmin, (void*) isplit, (void*) m, (void*) dol, (void*) dou, (void*) minrgp, (void*) rtol1, (void*) rtol2, (void*) w, (void*) werr, (void*) wgap, (void*) iblock, (void*) indexw, (void*) gers, (void*) z, (void*) ldz, (void*) isuppz, (void*) work, (void*) iwork, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlarrv_(void* n, void* vl, void* vu, void* d, void* l, void* pivmin, void* isplit, void* m, void* dol, void* dou, void* minrgp, void* rtol1, void* rtol2, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* z, void* ldz, void* isuppz, void* work, void* iwork, void* info)
{
	void (*fn) (void* n, void* vl, void* vu, void* d, void* l, void* pivmin, void* isplit, void* m, void* dol, void* dou, void* minrgp, void* rtol1, void* rtol2, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* z, void* ldz, void* isuppz, void* work, void* iwork, void* info);

	*(void **) & fn = current_backend->lapack.dlarrv.f77_blas_function; 

		fn((void*) n, (void*) vl, (void*) vu, (void*) d, (void*) l, (void*) pivmin, (void*) isplit, (void*) m, (void*) dol, (void*) dou, (void*) minrgp, (void*) rtol1, (void*) rtol2, (void*) w, (void*) werr, (void*) wgap, (void*) iblock, (void*) indexw, (void*) gers, (void*) z, (void*) ldz, (void*) isuppz, (void*) work, (void*) iwork, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_dlarrv(void* n, void* vl, void* vu, void* d, void* l, void* pivmin, void* isplit, void* m, void* dol, void* dou, void* minrgp, void* rtol1, void* rtol2, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* z, void* ldz, void* isuppz, void* work, void* iwork, void* info) __attribute__((alias("flexiblas_real_dlarrv_")));
#else
void flexiblas_real_dlarrv(void* n, void* vl, void* vu, void* d, void* l, void* pivmin, void* isplit, void* m, void* dol, void* dou, void* minrgp, void* rtol1, void* rtol2, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* z, void* ldz, void* isuppz, void* work, void* iwork, void* info){flexiblas_real_dlarrv_((void*) n, (void*) vl, (void*) vu, (void*) d, (void*) l, (void*) pivmin, (void*) isplit, (void*) m, (void*) dol, (void*) dou, (void*) minrgp, (void*) rtol1, (void*) rtol2, (void*) w, (void*) werr, (void*) wgap, (void*) iblock, (void*) indexw, (void*) gers, (void*) z, (void*) ldz, (void*) isuppz, (void*) work, (void*) iwork, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dlarrv_(void* n, void* vl, void* vu, void* d, void* l, void* pivmin, void* isplit, void* m, void* dol, void* dou, void* minrgp, void* rtol1, void* rtol2, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* z, void* ldz, void* isuppz, void* work, void* iwork, void* info)
{
	void (*fn) (void* n, void* vl, void* vu, void* d, void* l, void* pivmin, void* isplit, void* m, void* dol, void* dou, void* minrgp, void* rtol1, void* rtol2, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* z, void* ldz, void* isuppz, void* work, void* iwork, void* info);
	void (*fn_hook) (void* n, void* vl, void* vu, void* d, void* l, void* pivmin, void* isplit, void* m, void* dol, void* dou, void* minrgp, void* rtol1, void* rtol2, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* z, void* ldz, void* isuppz, void* work, void* iwork, void* info);

	*(void **) &fn      = current_backend->lapack.dlarrv.f77_blas_function; 

    hook_pos_dlarrv ++;
    if( hook_pos_dlarrv < __flexiblas_hooks->dlarrv.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlarrv.f77_hook_function[hook_pos_dlarrv];
        fn_hook((void*) n, (void*) vl, (void*) vu, (void*) d, (void*) l, (void*) pivmin, (void*) isplit, (void*) m, (void*) dol, (void*) dou, (void*) minrgp, (void*) rtol1, (void*) rtol2, (void*) w, (void*) werr, (void*) wgap, (void*) iblock, (void*) indexw, (void*) gers, (void*) z, (void*) ldz, (void*) isuppz, (void*) work, (void*) iwork, (void*) info);
    } else {
        hook_pos_dlarrv = 0;
		fn((void*) n, (void*) vl, (void*) vu, (void*) d, (void*) l, (void*) pivmin, (void*) isplit, (void*) m, (void*) dol, (void*) dou, (void*) minrgp, (void*) rtol1, (void*) rtol2, (void*) w, (void*) werr, (void*) wgap, (void*) iblock, (void*) indexw, (void*) gers, (void*) z, (void*) ldz, (void*) isuppz, (void*) work, (void*) iwork, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_dlarrv(void* n, void* vl, void* vu, void* d, void* l, void* pivmin, void* isplit, void* m, void* dol, void* dou, void* minrgp, void* rtol1, void* rtol2, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* z, void* ldz, void* isuppz, void* work, void* iwork, void* info) __attribute__((alias("flexiblas_chain_dlarrv_")));
#else
void flexiblas_chain_dlarrv(void* n, void* vl, void* vu, void* d, void* l, void* pivmin, void* isplit, void* m, void* dol, void* dou, void* minrgp, void* rtol1, void* rtol2, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* z, void* ldz, void* isuppz, void* work, void* iwork, void* info){flexiblas_chain_dlarrv_((void*) n, (void*) vl, (void*) vu, (void*) d, (void*) l, (void*) pivmin, (void*) isplit, (void*) m, (void*) dol, (void*) dou, (void*) minrgp, (void*) rtol1, (void*) rtol2, (void*) w, (void*) werr, (void*) wgap, (void*) iblock, (void*) indexw, (void*) gers, (void*) z, (void*) ldz, (void*) isuppz, (void*) work, (void*) iwork, (void*) info);}
#endif



