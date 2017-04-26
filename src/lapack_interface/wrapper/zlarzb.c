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
 /* Generated: Tue Mar 28 16:07:38 2017 */ 
        
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
void FC_GLOBAL(zlarzb,ZLARZB)(char* side, char* trans, char* direct, char* storev, blasint* m, blasint* n, blasint* k, blasint* l, double complex* v, blasint* ldv, double complex* t, blasint* ldt, double complex* c, blasint* ldc, double complex* work, blasint* ldwork)
#else
void FC_GLOBAL(zlarzb,ZLARZB)(char* side, char* trans, char* direct, char* storev, blasint* m, blasint* n, blasint* k, blasint* l, double complex* v, blasint* ldv, double complex* t, blasint* ldt, double complex* c, blasint* ldc, double complex* work, blasint* ldwork)
#endif 
{
    double ts;
	void (*fn) (void* side, void* trans, void* direct, void* storev, void* m, void* n, void* k, void* l, void* v, void* ldv, void* t, void* ldt, void* c, void* ldc, void* work, void* ldwork);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->lapack.zlarzb.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) side, (void*) trans, (void*) direct, (void*) storev, (void*) m, (void*) n, (void*) k, (void*) l, (void*) v, (void*) ldv, (void*) t, (void*) ldt, (void*) c, (void*) ldc, (void*) work, (void*) ldwork); 
		current_backend->lapack.zlarzb.timings[0] += (flexiblas_wtime() -ts);
		current_backend->lapack.zlarzb.calls[0]++;
	} else { 
		fn((void*) side, (void*) trans, (void*) direct, (void*) storev, (void*) m, (void*) n, (void*) k, (void*) l, (void*) v, (void*) ldv, (void*) t, (void*) ldt, (void*) c, (void*) ldc, (void*) work, (void*) ldwork); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void zlarzb_(char* side, char* trans, char* direct, char* storev, blasint* m, blasint* n, blasint* k, blasint* l, double complex* v, blasint* ldv, double complex* t, blasint* ldt, double complex* c, blasint* ldc, double complex* work, blasint* ldwork) __attribute__((alias(MTS(FC_GLOBAL(zlarzb,ZLARZB)))));
#else
void zlarzb(char* side, char* trans, char* direct, char* storev, blasint* m, blasint* n, blasint* k, blasint* l, double complex* v, blasint* ldv, double complex* t, blasint* ldt, double complex* c, blasint* ldc, double complex* work, blasint* ldwork) __attribute__((alias(MTS(FC_GLOBAL(zlarzb,ZLARZB)))));
#endif



