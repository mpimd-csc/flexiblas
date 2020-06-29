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
 /* Generated: Wed Mar 28 11:20:04 2018 */
        
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



static TLS_STORE uint8_t hook_pos_sgeevx = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(sgeevx,SGEEVX)(char* balanc, char* jobvl, char* jobvr, char* sense, blasint* n, float* a, blasint* lda, float* wr, float* wi, float* vl, blasint* ldvl, float* vr, blasint* ldvr, blasint* ilo, blasint* ihi, float* scale, float* abnrm, float* rconde, float* rcondv, float* work, blasint* lwork, blasint* iwork, blasint* info)
#else
void FC_GLOBAL(sgeevx,SGEEVX)(char* balanc, char* jobvl, char* jobvr, char* sense, blasint* n, float* a, blasint* lda, float* wr, float* wi, float* vl, blasint* ldvl, float* vr, blasint* ldvr, blasint* ilo, blasint* ihi, float* scale, float* abnrm, float* rconde, float* rcondv, float* work, blasint* lwork, blasint* iwork, blasint* info)
#endif
{
	void (*fn) (void* balanc, void* jobvl, void* jobvr, void* sense, void* n, void* a, void* lda, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* ilo, void* ihi, void* scale, void* abnrm, void* rconde, void* rcondv, void* work, void* lwork, void* iwork, void* info);
	void (*fn_hook) (void* balanc, void* jobvl, void* jobvr, void* sense, void* n, void* a, void* lda, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* ilo, void* ihi, void* scale, void* abnrm, void* rconde, void* rcondv, void* work, void* lwork, void* iwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.sgeevx.f77_blas_function; 
	fn_hook = __flexiblas_hooks->sgeevx.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) balanc, (void*) jobvl, (void*) jobvr, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) wr, (void*) wi, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) ilo, (void*) ihi, (void*) scale, (void*) abnrm, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) iwork, (void*) info); 
		return;
	} else {
		hook_pos_sgeevx = 0;
		fn_hook((void*) balanc, (void*) jobvl, (void*) jobvr, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) wr, (void*) wi, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) ilo, (void*) ihi, (void*) scale, (void*) abnrm, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) iwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void sgeevx_(char* balanc, char* jobvl, char* jobvr, char* sense, blasint* n, float* a, blasint* lda, float* wr, float* wi, float* vl, blasint* ldvl, float* vr, blasint* ldvr, blasint* ilo, blasint* ihi, float* scale, float* abnrm, float* rconde, float* rcondv, float* work, blasint* lwork, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(sgeevx,SGEEVX)))));
#else
void sgeevx(char* balanc, char* jobvl, char* jobvr, char* sense, blasint* n, float* a, blasint* lda, float* wr, float* wi, float* vl, blasint* ldvl, float* vr, blasint* ldvr, blasint* ilo, blasint* ihi, float* scale, float* abnrm, float* rconde, float* rcondv, float* work, blasint* lwork, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(sgeevx,SGEEVX)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_sgeevx_(void* balanc, void* jobvl, void* jobvr, void* sense, void* n, void* a, void* lda, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* ilo, void* ihi, void* scale, void* abnrm, void* rconde, void* rcondv, void* work, void* lwork, void* iwork, void* info)
{
	void (*fn) (void* balanc, void* jobvl, void* jobvr, void* sense, void* n, void* a, void* lda, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* ilo, void* ihi, void* scale, void* abnrm, void* rconde, void* rcondv, void* work, void* lwork, void* iwork, void* info);

	fn = current_backend->lapack.sgeevx.f77_blas_function; 

		fn((void*) balanc, (void*) jobvl, (void*) jobvr, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) wr, (void*) wi, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) ilo, (void*) ihi, (void*) scale, (void*) abnrm, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) iwork, (void*) info); 

	return;
}

void flexiblas_real_sgeevx(void* balanc, void* jobvl, void* jobvr, void* sense, void* n, void* a, void* lda, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* ilo, void* ihi, void* scale, void* abnrm, void* rconde, void* rcondv, void* work, void* lwork, void* iwork, void* info)  __attribute__((alias("flexiblas_real_sgeevx_")));





/* Chainloader for Hooks */


void flexiblas_chain_sgeevx_(void* balanc, void* jobvl, void* jobvr, void* sense, void* n, void* a, void* lda, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* ilo, void* ihi, void* scale, void* abnrm, void* rconde, void* rcondv, void* work, void* lwork, void* iwork, void* info)
{
	void (*fn) (void* balanc, void* jobvl, void* jobvr, void* sense, void* n, void* a, void* lda, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* ilo, void* ihi, void* scale, void* abnrm, void* rconde, void* rcondv, void* work, void* lwork, void* iwork, void* info);
	void (*fn_hook) (void* balanc, void* jobvl, void* jobvr, void* sense, void* n, void* a, void* lda, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* ilo, void* ihi, void* scale, void* abnrm, void* rconde, void* rcondv, void* work, void* lwork, void* iwork, void* info);

	fn      = current_backend->lapack.sgeevx.f77_blas_function; 

    hook_pos_sgeevx ++;
    if( hook_pos_sgeevx < __flexiblas_hooks->sgeevx.nhook) {
        fn_hook = __flexiblas_hooks->sgeevx.f77_hook_function[hook_pos_sgeevx];
        fn_hook((void*) balanc, (void*) jobvl, (void*) jobvr, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) wr, (void*) wi, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) ilo, (void*) ihi, (void*) scale, (void*) abnrm, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) iwork, (void*) info);
    } else {
        hook_pos_sgeevx = 0;
		fn((void*) balanc, (void*) jobvl, (void*) jobvr, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) wr, (void*) wi, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) ilo, (void*) ihi, (void*) scale, (void*) abnrm, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) iwork, (void*) info); 
	}
	return;
}

void flexiblas_chain_sgeevx(void* balanc, void* jobvl, void* jobvr, void* sense, void* n, void* a, void* lda, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* ilo, void* ihi, void* scale, void* abnrm, void* rconde, void* rcondv, void* work, void* lwork, void* iwork, void* info)  __attribute__((alias("flexiblas_chain_sgeevx_")));




