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
void FC_GLOBAL(slasdt,SLASDT)(blasint* n, blasint* lvl, blasint* nd, blasint* inode, blasint* ndiml, blasint* ndimr, blasint* msub)
#else
void FC_GLOBAL(slasdt,SLASDT)(blasint* n, blasint* lvl, blasint* nd, blasint* inode, blasint* ndiml, blasint* ndimr, blasint* msub)
#endif 
{
    double ts;
	void (*fn) (void* n, void* lvl, void* nd, void* inode, void* ndiml, void* ndimr, void* msub);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->lapack.slasdt.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) n, (void*) lvl, (void*) nd, (void*) inode, (void*) ndiml, (void*) ndimr, (void*) msub); 
		current_backend->lapack.slasdt.timings[0] += (flexiblas_wtime() -ts);
		current_backend->lapack.slasdt.calls[0]++;
	} else { 
		fn((void*) n, (void*) lvl, (void*) nd, (void*) inode, (void*) ndiml, (void*) ndimr, (void*) msub); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void slasdt_(blasint* n, blasint* lvl, blasint* nd, blasint* inode, blasint* ndiml, blasint* ndimr, blasint* msub) __attribute__((alias(MTS(FC_GLOBAL(slasdt,SLASDT)))));
#else
void slasdt(blasint* n, blasint* lvl, blasint* nd, blasint* inode, blasint* ndiml, blasint* ndimr, blasint* msub) __attribute__((alias(MTS(FC_GLOBAL(slasdt,SLASDT)))));
#endif



