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


static TLS_STORE uint8_t hook_pos_ctgsja = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(ctgsja,CTGSJA)(char* jobu, char* jobv, char* jobq, blasint* m, blasint* p, blasint* n, blasint* k, blasint* l, float complex* a, blasint* lda, float complex* b, blasint* ldb, float* tola, float* tolb, float* alpha, float* beta, float complex* u, blasint* ldu, float complex* v, blasint* ldv, float complex* q, blasint* ldq, float complex* work, blasint* ncycle, blasint* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv, flexiblas_fortran_charlen_t len_jobq)
#else
void FC_GLOBAL(ctgsja,CTGSJA)(char* jobu, char* jobv, char* jobq, blasint* m, blasint* p, blasint* n, blasint* k, blasint* l, float complex* a, blasint* lda, float complex* b, blasint* ldb, float* tola, float* tolb, float* alpha, float* beta, float complex* u, blasint* ldu, float complex* v, blasint* ldv, float complex* q, blasint* ldq, float complex* work, blasint* ncycle, blasint* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv, flexiblas_fortran_charlen_t len_jobq)
#endif
{
	void (*fn) (void* jobu, void* jobv, void* jobq, void* m, void* p, void* n, void* k, void* l, void* a, void* lda, void* b, void* ldb, void* tola, void* tolb, void* alpha, void* beta, void* u, void* ldu, void* v, void* ldv, void* q, void* ldq, void* work, void* ncycle, void* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv, flexiblas_fortran_charlen_t len_jobq);
	void (*fn_hook) (void* jobu, void* jobv, void* jobq, void* m, void* p, void* n, void* k, void* l, void* a, void* lda, void* b, void* ldb, void* tola, void* tolb, void* alpha, void* beta, void* u, void* ldu, void* v, void* ldv, void* q, void* ldq, void* work, void* ncycle, void* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv, flexiblas_fortran_charlen_t len_jobq);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.ctgsja.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->ctgsja.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) jobu, (void*) jobv, (void*) jobq, (void*) m, (void*) p, (void*) n, (void*) k, (void*) l, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) tola, (void*) tolb, (void*) alpha, (void*) beta, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) q, (void*) ldq, (void*) work, (void*) ncycle, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobu, ( flexiblas_fortran_charlen_t ) len_jobv, ( flexiblas_fortran_charlen_t ) len_jobq); 
		return;
	} else {
		hook_pos_ctgsja = 0;
		fn_hook((void*) jobu, (void*) jobv, (void*) jobq, (void*) m, (void*) p, (void*) n, (void*) k, (void*) l, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) tola, (void*) tolb, (void*) alpha, (void*) beta, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) q, (void*) ldq, (void*) work, (void*) ncycle, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobu, ( flexiblas_fortran_charlen_t ) len_jobv, ( flexiblas_fortran_charlen_t ) len_jobq);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void ctgsja_(char* jobu, char* jobv, char* jobq, blasint* m, blasint* p, blasint* n, blasint* k, blasint* l, float complex* a, blasint* lda, float complex* b, blasint* ldb, float* tola, float* tolb, float* alpha, float* beta, float complex* u, blasint* ldu, float complex* v, blasint* ldv, float complex* q, blasint* ldq, float complex* work, blasint* ncycle, blasint* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv, flexiblas_fortran_charlen_t len_jobq) __attribute__((alias(MTS(FC_GLOBAL(ctgsja,CTGSJA)))));
#else
#ifndef __APPLE__
void ctgsja(char* jobu, char* jobv, char* jobq, blasint* m, blasint* p, blasint* n, blasint* k, blasint* l, float complex* a, blasint* lda, float complex* b, blasint* ldb, float* tola, float* tolb, float* alpha, float* beta, float complex* u, blasint* ldu, float complex* v, blasint* ldv, float complex* q, blasint* ldq, float complex* work, blasint* ncycle, blasint* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv, flexiblas_fortran_charlen_t len_jobq) __attribute__((alias(MTS(FC_GLOBAL(ctgsja,CTGSJA)))));
#else
void ctgsja(char* jobu, char* jobv, char* jobq, blasint* m, blasint* p, blasint* n, blasint* k, blasint* l, float complex* a, blasint* lda, float complex* b, blasint* ldb, float* tola, float* tolb, float* alpha, float* beta, float complex* u, blasint* ldu, float complex* v, blasint* ldv, float complex* q, blasint* ldq, float complex* work, blasint* ncycle, blasint* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv, flexiblas_fortran_charlen_t len_jobq){ FC_GLOBAL(ctgsja,CTGSJA)((void*) jobu, (void*) jobv, (void*) jobq, (void*) m, (void*) p, (void*) n, (void*) k, (void*) l, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) tola, (void*) tolb, (void*) alpha, (void*) beta, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) q, (void*) ldq, (void*) work, (void*) ncycle, (void*) info, (flexiblas_fortran_charlen_t) len_jobu, (flexiblas_fortran_charlen_t) len_jobv, (flexiblas_fortran_charlen_t) len_jobq); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_ctgsja_(void* jobu, void* jobv, void* jobq, void* m, void* p, void* n, void* k, void* l, void* a, void* lda, void* b, void* ldb, void* tola, void* tolb, void* alpha, void* beta, void* u, void* ldu, void* v, void* ldv, void* q, void* ldq, void* work, void* ncycle, void* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv, flexiblas_fortran_charlen_t len_jobq)
{
	void (*fn) (void* jobu, void* jobv, void* jobq, void* m, void* p, void* n, void* k, void* l, void* a, void* lda, void* b, void* ldb, void* tola, void* tolb, void* alpha, void* beta, void* u, void* ldu, void* v, void* ldv, void* q, void* ldq, void* work, void* ncycle, void* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv, flexiblas_fortran_charlen_t len_jobq);

	*(void **) & fn = current_backend->lapack.ctgsja.f77_blas_function; 

		fn((void*) jobu, (void*) jobv, (void*) jobq, (void*) m, (void*) p, (void*) n, (void*) k, (void*) l, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) tola, (void*) tolb, (void*) alpha, (void*) beta, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) q, (void*) ldq, (void*) work, (void*) ncycle, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobu, ( flexiblas_fortran_charlen_t ) len_jobv, ( flexiblas_fortran_charlen_t ) len_jobq); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_ctgsja(void* jobu, void* jobv, void* jobq, void* m, void* p, void* n, void* k, void* l, void* a, void* lda, void* b, void* ldb, void* tola, void* tolb, void* alpha, void* beta, void* u, void* ldu, void* v, void* ldv, void* q, void* ldq, void* work, void* ncycle, void* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv, flexiblas_fortran_charlen_t len_jobq) __attribute__((alias("flexiblas_real_ctgsja_")));
#else
void flexiblas_real_ctgsja(void* jobu, void* jobv, void* jobq, void* m, void* p, void* n, void* k, void* l, void* a, void* lda, void* b, void* ldb, void* tola, void* tolb, void* alpha, void* beta, void* u, void* ldu, void* v, void* ldv, void* q, void* ldq, void* work, void* ncycle, void* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv, flexiblas_fortran_charlen_t len_jobq){flexiblas_real_ctgsja_((void*) jobu, (void*) jobv, (void*) jobq, (void*) m, (void*) p, (void*) n, (void*) k, (void*) l, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) tola, (void*) tolb, (void*) alpha, (void*) beta, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) q, (void*) ldq, (void*) work, (void*) ncycle, (void*) info, (flexiblas_fortran_charlen_t) len_jobu, (flexiblas_fortran_charlen_t) len_jobv, (flexiblas_fortran_charlen_t) len_jobq);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_ctgsja_(void* jobu, void* jobv, void* jobq, void* m, void* p, void* n, void* k, void* l, void* a, void* lda, void* b, void* ldb, void* tola, void* tolb, void* alpha, void* beta, void* u, void* ldu, void* v, void* ldv, void* q, void* ldq, void* work, void* ncycle, void* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv, flexiblas_fortran_charlen_t len_jobq)
{
	void (*fn) (void* jobu, void* jobv, void* jobq, void* m, void* p, void* n, void* k, void* l, void* a, void* lda, void* b, void* ldb, void* tola, void* tolb, void* alpha, void* beta, void* u, void* ldu, void* v, void* ldv, void* q, void* ldq, void* work, void* ncycle, void* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv, flexiblas_fortran_charlen_t len_jobq);
	void (*fn_hook) (void* jobu, void* jobv, void* jobq, void* m, void* p, void* n, void* k, void* l, void* a, void* lda, void* b, void* ldb, void* tola, void* tolb, void* alpha, void* beta, void* u, void* ldu, void* v, void* ldv, void* q, void* ldq, void* work, void* ncycle, void* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv, flexiblas_fortran_charlen_t len_jobq);

	*(void **) &fn      = current_backend->lapack.ctgsja.f77_blas_function; 

    hook_pos_ctgsja ++;
    if( hook_pos_ctgsja < __flexiblas_hooks->ctgsja.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->ctgsja.f77_hook_function[hook_pos_ctgsja];
        fn_hook((void*) jobu, (void*) jobv, (void*) jobq, (void*) m, (void*) p, (void*) n, (void*) k, (void*) l, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) tola, (void*) tolb, (void*) alpha, (void*) beta, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) q, (void*) ldq, (void*) work, (void*) ncycle, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobu, ( flexiblas_fortran_charlen_t ) len_jobv, ( flexiblas_fortran_charlen_t ) len_jobq);
    } else {
        hook_pos_ctgsja = 0;
		fn((void*) jobu, (void*) jobv, (void*) jobq, (void*) m, (void*) p, (void*) n, (void*) k, (void*) l, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) tola, (void*) tolb, (void*) alpha, (void*) beta, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) q, (void*) ldq, (void*) work, (void*) ncycle, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobu, ( flexiblas_fortran_charlen_t ) len_jobv, ( flexiblas_fortran_charlen_t ) len_jobq); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_ctgsja(void* jobu, void* jobv, void* jobq, void* m, void* p, void* n, void* k, void* l, void* a, void* lda, void* b, void* ldb, void* tola, void* tolb, void* alpha, void* beta, void* u, void* ldu, void* v, void* ldv, void* q, void* ldq, void* work, void* ncycle, void* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv, flexiblas_fortran_charlen_t len_jobq) __attribute__((alias("flexiblas_chain_ctgsja_")));
#else
void flexiblas_chain_ctgsja(void* jobu, void* jobv, void* jobq, void* m, void* p, void* n, void* k, void* l, void* a, void* lda, void* b, void* ldb, void* tola, void* tolb, void* alpha, void* beta, void* u, void* ldu, void* v, void* ldv, void* q, void* ldq, void* work, void* ncycle, void* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv, flexiblas_fortran_charlen_t len_jobq){flexiblas_chain_ctgsja_((void*) jobu, (void*) jobv, (void*) jobq, (void*) m, (void*) p, (void*) n, (void*) k, (void*) l, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) tola, (void*) tolb, (void*) alpha, (void*) beta, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) q, (void*) ldq, (void*) work, (void*) ncycle, (void*) info, (flexiblas_fortran_charlen_t) len_jobu, (flexiblas_fortran_charlen_t) len_jobv, (flexiblas_fortran_charlen_t) len_jobq);}
#endif



