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
void flexiblas_real_zlags2_(void* upper, void* a1, void* a2, void* a3, void* b1, void* b2, void* b3, void* csu, void* snu, void* csv, void* snv, void* csq, void* snq)
#else
void flexiblas_real_zlags2_(void* upper, void* a1, void* a2, void* a3, void* b1, void* b2, void* b3, void* csu, void* snu, void* csv, void* snv, void* csq, void* snq)
#endif 
{
	void (*fn) (void* upper, void* a1, void* a2, void* a3, void* b1, void* b2, void* b3, void* csu, void* snu, void* csv, void* snv, void* csq, void* snq);

	fn = current_backend->lapack.zlags2.fblas_real; 

		fn((void*) upper, (void*) a1, (void*) a2, (void*) a3, (void*) b1, (void*) b2, (void*) b3, (void*) csu, (void*) snu, (void*) csv, (void*) snv, (void*) csq, (void*) snq); 

	return;
}

#ifdef FLEXIBLAS_ABI_INTEL 
void flexiblas_real_zlags2(void* upper, void* a1, void* a2, void* a3, void* b1, void* b2, void* b3, void* csu, void* snu, void* csv, void* snv, void* csq, void* snq)  __attribute__((alias("flexiblas_real_zlags2_")));

#else 
void flexiblas_real_zlags2(void* upper, void* a1, void* a2, void* a3, void* b1, void* b2, void* b3, void* csu, void* snu, void* csv, void* snv, void* csq, void* snq)  __attribute__((alias("flexiblas_real_zlags2_")));

#endif



