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



static TLS_STORE uint8_t hook_pos_dggevx = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dggevx,DGGEVX)(char* balanc, char* jobvl, char* jobvr, char* sense, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* alphar, double* alphai, double* beta, double* vl, blasint* ldvl, double* vr, blasint* ldvr, blasint* ilo, blasint* ihi, double* lscale, double* rscale, double* abnrm, double* bbnrm, double* rconde, double* rcondv, double* work, blasint* lwork, blasint* iwork, blasint* bwork, blasint* info)
#else
void FC_GLOBAL(dggevx,DGGEVX)(char* balanc, char* jobvl, char* jobvr, char* sense, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* alphar, double* alphai, double* beta, double* vl, blasint* ldvl, double* vr, blasint* ldvr, blasint* ilo, blasint* ihi, double* lscale, double* rscale, double* abnrm, double* bbnrm, double* rconde, double* rcondv, double* work, blasint* lwork, blasint* iwork, blasint* bwork, blasint* info)
#endif
{
	void (*fn) (void* balanc, void* jobvl, void* jobvr, void* sense, void* n, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* vl, void* ldvl, void* vr, void* ldvr, void* ilo, void* ihi, void* lscale, void* rscale, void* abnrm, void* bbnrm, void* rconde, void* rcondv, void* work, void* lwork, void* iwork, void* bwork, void* info);
	void (*fn_hook) (void* balanc, void* jobvl, void* jobvr, void* sense, void* n, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* vl, void* ldvl, void* vr, void* ldvr, void* ilo, void* ihi, void* lscale, void* rscale, void* abnrm, void* bbnrm, void* rconde, void* rcondv, void* work, void* lwork, void* iwork, void* bwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.dggevx.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->dggevx.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) balanc, (void*) jobvl, (void*) jobvr, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) ilo, (void*) ihi, (void*) lscale, (void*) rscale, (void*) abnrm, (void*) bbnrm, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) iwork, (void*) bwork, (void*) info); 
		return;
	} else {
		hook_pos_dggevx = 0;
		fn_hook((void*) balanc, (void*) jobvl, (void*) jobvr, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) ilo, (void*) ihi, (void*) lscale, (void*) rscale, (void*) abnrm, (void*) bbnrm, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) iwork, (void*) bwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dggevx_(char* balanc, char* jobvl, char* jobvr, char* sense, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* alphar, double* alphai, double* beta, double* vl, blasint* ldvl, double* vr, blasint* ldvr, blasint* ilo, blasint* ihi, double* lscale, double* rscale, double* abnrm, double* bbnrm, double* rconde, double* rcondv, double* work, blasint* lwork, blasint* iwork, blasint* bwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dggevx,DGGEVX)))));
#else
#ifndef __APPLE__
void dggevx(char* balanc, char* jobvl, char* jobvr, char* sense, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* alphar, double* alphai, double* beta, double* vl, blasint* ldvl, double* vr, blasint* ldvr, blasint* ilo, blasint* ihi, double* lscale, double* rscale, double* abnrm, double* bbnrm, double* rconde, double* rcondv, double* work, blasint* lwork, blasint* iwork, blasint* bwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dggevx,DGGEVX)))));
#else
void dggevx(char* balanc, char* jobvl, char* jobvr, char* sense, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* alphar, double* alphai, double* beta, double* vl, blasint* ldvl, double* vr, blasint* ldvr, blasint* ilo, blasint* ihi, double* lscale, double* rscale, double* abnrm, double* bbnrm, double* rconde, double* rcondv, double* work, blasint* lwork, blasint* iwork, blasint* bwork, blasint* info){ FC_GLOBAL(dggevx,DGGEVX)((void*) balanc, (void*) jobvl, (void*) jobvr, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) ilo, (void*) ihi, (void*) lscale, (void*) rscale, (void*) abnrm, (void*) bbnrm, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) iwork, (void*) bwork, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dggevx_(void* balanc, void* jobvl, void* jobvr, void* sense, void* n, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* vl, void* ldvl, void* vr, void* ldvr, void* ilo, void* ihi, void* lscale, void* rscale, void* abnrm, void* bbnrm, void* rconde, void* rcondv, void* work, void* lwork, void* iwork, void* bwork, void* info)
{
	void (*fn) (void* balanc, void* jobvl, void* jobvr, void* sense, void* n, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* vl, void* ldvl, void* vr, void* ldvr, void* ilo, void* ihi, void* lscale, void* rscale, void* abnrm, void* bbnrm, void* rconde, void* rcondv, void* work, void* lwork, void* iwork, void* bwork, void* info);

	*(void **) & fn = current_backend->lapack.dggevx.f77_blas_function; 

		fn((void*) balanc, (void*) jobvl, (void*) jobvr, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) ilo, (void*) ihi, (void*) lscale, (void*) rscale, (void*) abnrm, (void*) bbnrm, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) iwork, (void*) bwork, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_dggevx(void* balanc, void* jobvl, void* jobvr, void* sense, void* n, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* vl, void* ldvl, void* vr, void* ldvr, void* ilo, void* ihi, void* lscale, void* rscale, void* abnrm, void* bbnrm, void* rconde, void* rcondv, void* work, void* lwork, void* iwork, void* bwork, void* info) __attribute__((alias("flexiblas_real_dggevx_")));
#else
void flexiblas_real_dggevx(void* balanc, void* jobvl, void* jobvr, void* sense, void* n, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* vl, void* ldvl, void* vr, void* ldvr, void* ilo, void* ihi, void* lscale, void* rscale, void* abnrm, void* bbnrm, void* rconde, void* rcondv, void* work, void* lwork, void* iwork, void* bwork, void* info){flexiblas_real_dggevx_((void*) balanc, (void*) jobvl, (void*) jobvr, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) ilo, (void*) ihi, (void*) lscale, (void*) rscale, (void*) abnrm, (void*) bbnrm, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) iwork, (void*) bwork, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dggevx_(void* balanc, void* jobvl, void* jobvr, void* sense, void* n, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* vl, void* ldvl, void* vr, void* ldvr, void* ilo, void* ihi, void* lscale, void* rscale, void* abnrm, void* bbnrm, void* rconde, void* rcondv, void* work, void* lwork, void* iwork, void* bwork, void* info)
{
	void (*fn) (void* balanc, void* jobvl, void* jobvr, void* sense, void* n, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* vl, void* ldvl, void* vr, void* ldvr, void* ilo, void* ihi, void* lscale, void* rscale, void* abnrm, void* bbnrm, void* rconde, void* rcondv, void* work, void* lwork, void* iwork, void* bwork, void* info);
	void (*fn_hook) (void* balanc, void* jobvl, void* jobvr, void* sense, void* n, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* vl, void* ldvl, void* vr, void* ldvr, void* ilo, void* ihi, void* lscale, void* rscale, void* abnrm, void* bbnrm, void* rconde, void* rcondv, void* work, void* lwork, void* iwork, void* bwork, void* info);

	*(void **) &fn      = current_backend->lapack.dggevx.f77_blas_function; 

    hook_pos_dggevx ++;
    if( hook_pos_dggevx < __flexiblas_hooks->dggevx.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dggevx.f77_hook_function[hook_pos_dggevx];
        fn_hook((void*) balanc, (void*) jobvl, (void*) jobvr, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) ilo, (void*) ihi, (void*) lscale, (void*) rscale, (void*) abnrm, (void*) bbnrm, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) iwork, (void*) bwork, (void*) info);
    } else {
        hook_pos_dggevx = 0;
		fn((void*) balanc, (void*) jobvl, (void*) jobvr, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) ilo, (void*) ihi, (void*) lscale, (void*) rscale, (void*) abnrm, (void*) bbnrm, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) iwork, (void*) bwork, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_dggevx(void* balanc, void* jobvl, void* jobvr, void* sense, void* n, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* vl, void* ldvl, void* vr, void* ldvr, void* ilo, void* ihi, void* lscale, void* rscale, void* abnrm, void* bbnrm, void* rconde, void* rcondv, void* work, void* lwork, void* iwork, void* bwork, void* info) __attribute__((alias("flexiblas_chain_dggevx_")));
#else
void flexiblas_chain_dggevx(void* balanc, void* jobvl, void* jobvr, void* sense, void* n, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* vl, void* ldvl, void* vr, void* ldvr, void* ilo, void* ihi, void* lscale, void* rscale, void* abnrm, void* bbnrm, void* rconde, void* rcondv, void* work, void* lwork, void* iwork, void* bwork, void* info){flexiblas_chain_dggevx_((void*) balanc, (void*) jobvl, (void*) jobvr, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) ilo, (void*) ihi, (void*) lscale, (void*) rscale, (void*) abnrm, (void*) bbnrm, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) iwork, (void*) bwork, (void*) info);}
#endif



