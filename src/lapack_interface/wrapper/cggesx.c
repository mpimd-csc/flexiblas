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
 * Copyright (C) Martin Koehler, 2013-2020
 */
 /* This file it automatically generated. Please do not edit. */
 /* Generated: Wed Mar 28 11:20:03 2018 */
        
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



static TLS_STORE uint8_t hook_pos_cggesx = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(cggesx,CGGESX)(char* jobvsl, char* jobvsr, char* sort, blasint* selctg, char* sense, blasint* n, float complex* a, blasint* lda, float complex* b, blasint* ldb, blasint* sdim, float complex* alpha, float complex* beta, float complex* vsl, blasint* ldvsl, float complex* vsr, blasint* ldvsr, float* rconde, float* rcondv, float complex* work, blasint* lwork, float* rwork, blasint* iwork, blasint* liwork, blasint* bwork, blasint* info)
#else
void FC_GLOBAL(cggesx,CGGESX)(char* jobvsl, char* jobvsr, char* sort, blasint* selctg, char* sense, blasint* n, float complex* a, blasint* lda, float complex* b, blasint* ldb, blasint* sdim, float complex* alpha, float complex* beta, float complex* vsl, blasint* ldvsl, float complex* vsr, blasint* ldvsr, float* rconde, float* rcondv, float complex* work, blasint* lwork, float* rwork, blasint* iwork, blasint* liwork, blasint* bwork, blasint* info)
#endif
{
	void (*fn) (void* jobvsl, void* jobvsr, void* sort, void* selctg, void* sense, void* n, void* a, void* lda, void* b, void* ldb, void* sdim, void* alpha, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* rconde, void* rcondv, void* work, void* lwork, void* rwork, void* iwork, void* liwork, void* bwork, void* info);
	void (*fn_hook) (void* jobvsl, void* jobvsr, void* sort, void* selctg, void* sense, void* n, void* a, void* lda, void* b, void* ldb, void* sdim, void* alpha, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* rconde, void* rcondv, void* work, void* lwork, void* rwork, void* iwork, void* liwork, void* bwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.cggesx.f77_blas_function; 
	fn_hook = __flexiblas_hooks->cggesx.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) jobvsl, (void*) jobvsr, (void*) sort, (void*) selctg, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) sdim, (void*) alpha, (void*) beta, (void*) vsl, (void*) ldvsl, (void*) vsr, (void*) ldvsr, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) rwork, (void*) iwork, (void*) liwork, (void*) bwork, (void*) info); 
		return;
	} else {
		hook_pos_cggesx = 0;
		fn_hook((void*) jobvsl, (void*) jobvsr, (void*) sort, (void*) selctg, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) sdim, (void*) alpha, (void*) beta, (void*) vsl, (void*) ldvsl, (void*) vsr, (void*) ldvsr, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) rwork, (void*) iwork, (void*) liwork, (void*) bwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void cggesx_(char* jobvsl, char* jobvsr, char* sort, blasint* selctg, char* sense, blasint* n, float complex* a, blasint* lda, float complex* b, blasint* ldb, blasint* sdim, float complex* alpha, float complex* beta, float complex* vsl, blasint* ldvsl, float complex* vsr, blasint* ldvsr, float* rconde, float* rcondv, float complex* work, blasint* lwork, float* rwork, blasint* iwork, blasint* liwork, blasint* bwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(cggesx,CGGESX)))));
#else
void cggesx(char* jobvsl, char* jobvsr, char* sort, blasint* selctg, char* sense, blasint* n, float complex* a, blasint* lda, float complex* b, blasint* ldb, blasint* sdim, float complex* alpha, float complex* beta, float complex* vsl, blasint* ldvsl, float complex* vsr, blasint* ldvsr, float* rconde, float* rcondv, float complex* work, blasint* lwork, float* rwork, blasint* iwork, blasint* liwork, blasint* bwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(cggesx,CGGESX)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_cggesx_(void* jobvsl, void* jobvsr, void* sort, void* selctg, void* sense, void* n, void* a, void* lda, void* b, void* ldb, void* sdim, void* alpha, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* rconde, void* rcondv, void* work, void* lwork, void* rwork, void* iwork, void* liwork, void* bwork, void* info)
{
	void (*fn) (void* jobvsl, void* jobvsr, void* sort, void* selctg, void* sense, void* n, void* a, void* lda, void* b, void* ldb, void* sdim, void* alpha, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* rconde, void* rcondv, void* work, void* lwork, void* rwork, void* iwork, void* liwork, void* bwork, void* info);

	fn = current_backend->lapack.cggesx.f77_blas_function; 

		fn((void*) jobvsl, (void*) jobvsr, (void*) sort, (void*) selctg, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) sdim, (void*) alpha, (void*) beta, (void*) vsl, (void*) ldvsl, (void*) vsr, (void*) ldvsr, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) rwork, (void*) iwork, (void*) liwork, (void*) bwork, (void*) info); 

	return;
}

void flexiblas_real_cggesx(void* jobvsl, void* jobvsr, void* sort, void* selctg, void* sense, void* n, void* a, void* lda, void* b, void* ldb, void* sdim, void* alpha, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* rconde, void* rcondv, void* work, void* lwork, void* rwork, void* iwork, void* liwork, void* bwork, void* info)  __attribute__((alias("flexiblas_real_cggesx_")));





/* Chainloader for Hooks */


void flexiblas_chain_cggesx_(void* jobvsl, void* jobvsr, void* sort, void* selctg, void* sense, void* n, void* a, void* lda, void* b, void* ldb, void* sdim, void* alpha, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* rconde, void* rcondv, void* work, void* lwork, void* rwork, void* iwork, void* liwork, void* bwork, void* info)
{
	void (*fn) (void* jobvsl, void* jobvsr, void* sort, void* selctg, void* sense, void* n, void* a, void* lda, void* b, void* ldb, void* sdim, void* alpha, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* rconde, void* rcondv, void* work, void* lwork, void* rwork, void* iwork, void* liwork, void* bwork, void* info);
	void (*fn_hook) (void* jobvsl, void* jobvsr, void* sort, void* selctg, void* sense, void* n, void* a, void* lda, void* b, void* ldb, void* sdim, void* alpha, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* rconde, void* rcondv, void* work, void* lwork, void* rwork, void* iwork, void* liwork, void* bwork, void* info);

	fn      = current_backend->lapack.cggesx.f77_blas_function; 

    hook_pos_cggesx ++;
    if( hook_pos_cggesx < __flexiblas_hooks->cggesx.nhook) {
        fn_hook = __flexiblas_hooks->cggesx.f77_hook_function[hook_pos_cggesx];
        fn_hook((void*) jobvsl, (void*) jobvsr, (void*) sort, (void*) selctg, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) sdim, (void*) alpha, (void*) beta, (void*) vsl, (void*) ldvsl, (void*) vsr, (void*) ldvsr, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) rwork, (void*) iwork, (void*) liwork, (void*) bwork, (void*) info);
    } else {
        hook_pos_cggesx = 0;
		fn((void*) jobvsl, (void*) jobvsr, (void*) sort, (void*) selctg, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) sdim, (void*) alpha, (void*) beta, (void*) vsl, (void*) ldvsl, (void*) vsr, (void*) ldvsr, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) rwork, (void*) iwork, (void*) liwork, (void*) bwork, (void*) info); 
	}
	return;
}

void flexiblas_chain_cggesx(void* jobvsl, void* jobvsr, void* sort, void* selctg, void* sense, void* n, void* a, void* lda, void* b, void* ldb, void* sdim, void* alpha, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* rconde, void* rcondv, void* work, void* lwork, void* rwork, void* iwork, void* liwork, void* bwork, void* info)  __attribute__((alias("flexiblas_chain_cggesx_")));




