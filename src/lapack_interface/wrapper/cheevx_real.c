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
 /* Generated: Tue Mar 28 16:07:32 2017 */ 
        
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
void flexiblas_real_cheevx_(void* jobz, void* range, void* uplo, void* n, void* a, void* lda, void* vl, void* vu, void* il, void* iu, void* abstol, void* m, void* w, void* z, void* ldz, void* work, void* lwork, void* rwork, void* iwork, void* ifail, void* info)
#else
void flexiblas_real_cheevx_(void* jobz, void* range, void* uplo, void* n, void* a, void* lda, void* vl, void* vu, void* il, void* iu, void* abstol, void* m, void* w, void* z, void* ldz, void* work, void* lwork, void* rwork, void* iwork, void* ifail, void* info)
#endif 
{
	void (*fn) (void* jobz, void* range, void* uplo, void* n, void* a, void* lda, void* vl, void* vu, void* il, void* iu, void* abstol, void* m, void* w, void* z, void* ldz, void* work, void* lwork, void* rwork, void* iwork, void* ifail, void* info);

	fn = current_backend->lapack.cheevx.fblas_real; 

		fn((void*) jobz, (void*) range, (void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) abstol, (void*) m, (void*) w, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) rwork, (void*) iwork, (void*) ifail, (void*) info); 

	return;
}

#ifdef FLEXIBLAS_ABI_INTEL 
void flexiblas_real_cheevx(void* jobz, void* range, void* uplo, void* n, void* a, void* lda, void* vl, void* vu, void* il, void* iu, void* abstol, void* m, void* w, void* z, void* ldz, void* work, void* lwork, void* rwork, void* iwork, void* ifail, void* info)  __attribute__((alias("flexiblas_real_cheevx_")));

#else 
void flexiblas_real_cheevx(void* jobz, void* range, void* uplo, void* n, void* a, void* lda, void* vl, void* vu, void* il, void* iu, void* abstol, void* m, void* w, void* z, void* ldz, void* work, void* lwork, void* rwork, void* iwork, void* ifail, void* info)  __attribute__((alias("flexiblas_real_cheevx_")));

#endif



