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
 * Copyright (C) Martin Koehler, 2015-2017
 */
 /* This file it automatically generated. Please do not edit. */
 /* Generated: Tue Mar 28 16:07:36 2017 */ 
        
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



#ifdef FLEXIBLAS_ABI_INTEL 
void FC_GLOBAL(slarrv,SLARRV)(blasint* n, float* vl, float* vu, float* d, float* l, float* pivmin, blasint* isplit, blasint* m, blasint* dol, blasint* dou, float* minrgp, float* rtol1, float* rtol2, float* w, float* werr, float* wgap, blasint* iblock, blasint* indexw, float* gers, float* z, blasint* ldz, blasint* isuppz, float* work, blasint* iwork, blasint* info)
#else
void FC_GLOBAL(slarrv,SLARRV)(blasint* n, float* vl, float* vu, float* d, float* l, float* pivmin, blasint* isplit, blasint* m, blasint* dol, blasint* dou, float* minrgp, float* rtol1, float* rtol2, float* w, float* werr, float* wgap, blasint* iblock, blasint* indexw, float* gers, float* z, blasint* ldz, blasint* isuppz, float* work, blasint* iwork, blasint* info)
#endif 
{
    double ts;
	void (*fn) (void* n, void* vl, void* vu, void* d, void* l, void* pivmin, void* isplit, void* m, void* dol, void* dou, void* minrgp, void* rtol1, void* rtol2, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* z, void* ldz, void* isuppz, void* work, void* iwork, void* info);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->lapack.slarrv.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) n, (void*) vl, (void*) vu, (void*) d, (void*) l, (void*) pivmin, (void*) isplit, (void*) m, (void*) dol, (void*) dou, (void*) minrgp, (void*) rtol1, (void*) rtol2, (void*) w, (void*) werr, (void*) wgap, (void*) iblock, (void*) indexw, (void*) gers, (void*) z, (void*) ldz, (void*) isuppz, (void*) work, (void*) iwork, (void*) info); 
		current_backend->lapack.slarrv.timings[0] += (flexiblas_wtime() -ts);
		current_backend->lapack.slarrv.calls[0]++;
	} else { 
		fn((void*) n, (void*) vl, (void*) vu, (void*) d, (void*) l, (void*) pivmin, (void*) isplit, (void*) m, (void*) dol, (void*) dou, (void*) minrgp, (void*) rtol1, (void*) rtol2, (void*) w, (void*) werr, (void*) wgap, (void*) iblock, (void*) indexw, (void*) gers, (void*) z, (void*) ldz, (void*) isuppz, (void*) work, (void*) iwork, (void*) info); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void slarrv_(blasint* n, float* vl, float* vu, float* d, float* l, float* pivmin, blasint* isplit, blasint* m, blasint* dol, blasint* dou, float* minrgp, float* rtol1, float* rtol2, float* w, float* werr, float* wgap, blasint* iblock, blasint* indexw, float* gers, float* z, blasint* ldz, blasint* isuppz, float* work, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(slarrv,SLARRV)))));
#else
void slarrv(blasint* n, float* vl, float* vu, float* d, float* l, float* pivmin, blasint* isplit, blasint* m, blasint* dol, blasint* dou, float* minrgp, float* rtol1, float* rtol2, float* w, float* werr, float* wgap, blasint* iblock, blasint* indexw, float* gers, float* z, blasint* ldz, blasint* isuppz, float* work, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(slarrv,SLARRV)))));
#endif



