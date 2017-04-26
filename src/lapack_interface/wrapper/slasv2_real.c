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
void flexiblas_real_slasv2_(void* f, void* g, void* h, void* ssmin, void* ssmax, void* snr, void* csr, void* snl, void* csl)
#else
void flexiblas_real_slasv2_(void* f, void* g, void* h, void* ssmin, void* ssmax, void* snr, void* csr, void* snl, void* csl)
#endif 
{
	void (*fn) (void* f, void* g, void* h, void* ssmin, void* ssmax, void* snr, void* csr, void* snl, void* csl);

	fn = current_backend->lapack.slasv2.fblas_real; 

		fn((void*) f, (void*) g, (void*) h, (void*) ssmin, (void*) ssmax, (void*) snr, (void*) csr, (void*) snl, (void*) csl); 

	return;
}

#ifdef FLEXIBLAS_ABI_INTEL 
void flexiblas_real_slasv2(void* f, void* g, void* h, void* ssmin, void* ssmax, void* snr, void* csr, void* snl, void* csl)  __attribute__((alias("flexiblas_real_slasv2_")));

#else 
void flexiblas_real_slasv2(void* f, void* g, void* h, void* ssmin, void* ssmax, void* snr, void* csr, void* snl, void* csl)  __attribute__((alias("flexiblas_real_slasv2_")));

#endif



