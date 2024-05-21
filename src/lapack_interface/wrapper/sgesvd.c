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


static TLS_STORE uint8_t hook_pos_sgesvd = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(sgesvd,SGESVD)(char* jobu, char* jobvt, blasint* m, blasint* n, float* a, blasint* lda, float* s, float* u, blasint* ldu, float* vt, blasint* ldvt, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobvt)
#else
void FC_GLOBAL(sgesvd,SGESVD)(char* jobu, char* jobvt, blasint* m, blasint* n, float* a, blasint* lda, float* s, float* u, blasint* ldu, float* vt, blasint* ldvt, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobvt)
#endif
{
	void (*fn) (void* jobu, void* jobvt, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobvt);
	void (*fn_hook) (void* jobu, void* jobvt, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobvt);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.sgesvd.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->sgesvd.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) jobu, (void*) jobvt, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobu, ( flexiblas_fortran_charlen_t ) len_jobvt); 
		return;
	} else {
		hook_pos_sgesvd = 0;
		fn_hook((void*) jobu, (void*) jobvt, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobu, ( flexiblas_fortran_charlen_t ) len_jobvt);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void sgesvd_(char* jobu, char* jobvt, blasint* m, blasint* n, float* a, blasint* lda, float* s, float* u, blasint* ldu, float* vt, blasint* ldvt, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobvt) __attribute__((alias(MTS(FC_GLOBAL(sgesvd,SGESVD)))));
#else
#ifndef __APPLE__
void sgesvd(char* jobu, char* jobvt, blasint* m, blasint* n, float* a, blasint* lda, float* s, float* u, blasint* ldu, float* vt, blasint* ldvt, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobvt) __attribute__((alias(MTS(FC_GLOBAL(sgesvd,SGESVD)))));
#else
void sgesvd(char* jobu, char* jobvt, blasint* m, blasint* n, float* a, blasint* lda, float* s, float* u, blasint* ldu, float* vt, blasint* ldvt, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobvt){ FC_GLOBAL(sgesvd,SGESVD)((void*) jobu, (void*) jobvt, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobu, (flexiblas_fortran_charlen_t) len_jobvt); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_sgesvd_(void* jobu, void* jobvt, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobvt)
{
	void (*fn) (void* jobu, void* jobvt, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobvt);

	*(void **) & fn = current_backend->lapack.sgesvd.f77_blas_function; 

		fn((void*) jobu, (void*) jobvt, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobu, ( flexiblas_fortran_charlen_t ) len_jobvt); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_sgesvd(void* jobu, void* jobvt, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobvt) __attribute__((alias("flexiblas_real_sgesvd_")));
#else
void flexiblas_real_sgesvd(void* jobu, void* jobvt, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobvt){flexiblas_real_sgesvd_((void*) jobu, (void*) jobvt, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobu, (flexiblas_fortran_charlen_t) len_jobvt);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_sgesvd_(void* jobu, void* jobvt, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobvt)
{
	void (*fn) (void* jobu, void* jobvt, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobvt);
	void (*fn_hook) (void* jobu, void* jobvt, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobvt);

	*(void **) &fn      = current_backend->lapack.sgesvd.f77_blas_function; 

    hook_pos_sgesvd ++;
    if( hook_pos_sgesvd < __flexiblas_hooks->sgesvd.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->sgesvd.f77_hook_function[hook_pos_sgesvd];
        fn_hook((void*) jobu, (void*) jobvt, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobu, ( flexiblas_fortran_charlen_t ) len_jobvt);
    } else {
        hook_pos_sgesvd = 0;
		fn((void*) jobu, (void*) jobvt, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobu, ( flexiblas_fortran_charlen_t ) len_jobvt); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_sgesvd(void* jobu, void* jobvt, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobvt) __attribute__((alias("flexiblas_chain_sgesvd_")));
#else
void flexiblas_chain_sgesvd(void* jobu, void* jobvt, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobvt){flexiblas_chain_sgesvd_((void*) jobu, (void*) jobvt, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobu, (flexiblas_fortran_charlen_t) len_jobvt);}
#endif



