//  SPDX-License-Identifier: LGPL-3.0-or-later
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


static TLS_STORE uint8_t hook_pos_dgedmdq = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dgedmdq,DGEDMDQ)(char* jobs, char* jobz, char* jobr, char* jobq, char* jobt, char* jobf, blasint* whtsvd, blasint* m, blasint* n, float* f, blasint* ldf, float* x, blasint* ldx, float* y, blasint* ldy, blasint* nrnk, float* tol, blasint* k, float* reig, float* imeig, float* z, blasint* ldz, float* res, float* b, blasint* ldb, float* v, blasint* ldv, float* s, blasint* lds, float* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobs, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_jobr, flexiblas_fortran_charlen_t len_jobq, flexiblas_fortran_charlen_t len_jobt, flexiblas_fortran_charlen_t len_jobf)
#else
void FC_GLOBAL(dgedmdq,DGEDMDQ)(char* jobs, char* jobz, char* jobr, char* jobq, char* jobt, char* jobf, blasint* whtsvd, blasint* m, blasint* n, float* f, blasint* ldf, float* x, blasint* ldx, float* y, blasint* ldy, blasint* nrnk, float* tol, blasint* k, float* reig, float* imeig, float* z, blasint* ldz, float* res, float* b, blasint* ldb, float* v, blasint* ldv, float* s, blasint* lds, float* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobs, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_jobr, flexiblas_fortran_charlen_t len_jobq, flexiblas_fortran_charlen_t len_jobt, flexiblas_fortran_charlen_t len_jobf)
#endif
{
    void (*fn) (void* jobs, void* jobz, void* jobr, void* jobq, void* jobt, void* jobf, void* whtsvd, void* m, void* n, void* f, void* ldf, void* x, void* ldx, void* y, void* ldy, void* nrnk, void* tol, void* k, void* reig, void* imeig, void* z, void* ldz, void* res, void* b, void* ldb, void* v, void* ldv, void* s, void* lds, void* work, void* lwork, void* iwork, void* liwork, void* info, flexiblas_fortran_charlen_t len_jobs, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_jobr, flexiblas_fortran_charlen_t len_jobq, flexiblas_fortran_charlen_t len_jobt, flexiblas_fortran_charlen_t len_jobf);
    void (*fn_hook) (void* jobs, void* jobz, void* jobr, void* jobq, void* jobt, void* jobf, void* whtsvd, void* m, void* n, void* f, void* ldf, void* x, void* ldx, void* y, void* ldy, void* nrnk, void* tol, void* k, void* reig, void* imeig, void* z, void* ldz, void* res, void* b, void* ldb, void* v, void* ldv, void* s, void* lds, void* work, void* lwork, void* iwork, void* liwork, void* info, flexiblas_fortran_charlen_t len_jobs, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_jobr, flexiblas_fortran_charlen_t len_jobq, flexiblas_fortran_charlen_t len_jobt, flexiblas_fortran_charlen_t len_jobf);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dgedmdq.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dgedmdq.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) jobs, (void*) jobz, (void*) jobr, (void*) jobq, (void*) jobt, (void*) jobf, (void*) whtsvd, (void*) m, (void*) n, (void*) f, (void*) ldf, (void*) x, (void*) ldx, (void*) y, (void*) ldy, (void*) nrnk, (void*) tol, (void*) k, (void*) reig, (void*) imeig, (void*) z, (void*) ldz, (void*) res, (void*) b, (void*) ldb, (void*) v, (void*) ldv, (void*) s, (void*) lds, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobs, ( flexiblas_fortran_charlen_t ) len_jobz, ( flexiblas_fortran_charlen_t ) len_jobr, ( flexiblas_fortran_charlen_t ) len_jobq, ( flexiblas_fortran_charlen_t ) len_jobt, ( flexiblas_fortran_charlen_t ) len_jobf);
        return;
    } else {
        hook_pos_dgedmdq = 0;
        fn_hook((void*) jobs, (void*) jobz, (void*) jobr, (void*) jobq, (void*) jobt, (void*) jobf, (void*) whtsvd, (void*) m, (void*) n, (void*) f, (void*) ldf, (void*) x, (void*) ldx, (void*) y, (void*) ldy, (void*) nrnk, (void*) tol, (void*) k, (void*) reig, (void*) imeig, (void*) z, (void*) ldz, (void*) res, (void*) b, (void*) ldb, (void*) v, (void*) ldv, (void*) s, (void*) lds, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobs, ( flexiblas_fortran_charlen_t ) len_jobz, ( flexiblas_fortran_charlen_t ) len_jobr, ( flexiblas_fortran_charlen_t ) len_jobq, ( flexiblas_fortran_charlen_t ) len_jobt, ( flexiblas_fortran_charlen_t ) len_jobf);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void dgedmdq_(char* jobs, char* jobz, char* jobr, char* jobq, char* jobt, char* jobf, blasint* whtsvd, blasint* m, blasint* n, float* f, blasint* ldf, float* x, blasint* ldx, float* y, blasint* ldy, blasint* nrnk, float* tol, blasint* k, float* reig, float* imeig, float* z, blasint* ldz, float* res, float* b, blasint* ldb, float* v, blasint* ldv, float* s, blasint* lds, float* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobs, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_jobr, flexiblas_fortran_charlen_t len_jobq, flexiblas_fortran_charlen_t len_jobt, flexiblas_fortran_charlen_t len_jobf) __attribute__((alias(MTS(FC_GLOBAL(dgedmdq,DGEDMDQ)))));
#else
#ifndef __APPLE__
void dgedmdq(char* jobs, char* jobz, char* jobr, char* jobq, char* jobt, char* jobf, blasint* whtsvd, blasint* m, blasint* n, float* f, blasint* ldf, float* x, blasint* ldx, float* y, blasint* ldy, blasint* nrnk, float* tol, blasint* k, float* reig, float* imeig, float* z, blasint* ldz, float* res, float* b, blasint* ldb, float* v, blasint* ldv, float* s, blasint* lds, float* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobs, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_jobr, flexiblas_fortran_charlen_t len_jobq, flexiblas_fortran_charlen_t len_jobt, flexiblas_fortran_charlen_t len_jobf) __attribute__((alias(MTS(FC_GLOBAL(dgedmdq,DGEDMDQ)))));
#else
void dgedmdq(char* jobs, char* jobz, char* jobr, char* jobq, char* jobt, char* jobf, blasint* whtsvd, blasint* m, blasint* n, float* f, blasint* ldf, float* x, blasint* ldx, float* y, blasint* ldy, blasint* nrnk, float* tol, blasint* k, float* reig, float* imeig, float* z, blasint* ldz, float* res, float* b, blasint* ldb, float* v, blasint* ldv, float* s, blasint* lds, float* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobs, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_jobr, flexiblas_fortran_charlen_t len_jobq, flexiblas_fortran_charlen_t len_jobt, flexiblas_fortran_charlen_t len_jobf){ FC_GLOBAL(dgedmdq,DGEDMDQ)((void*) jobs, (void*) jobz, (void*) jobr, (void*) jobq, (void*) jobt, (void*) jobf, (void*) whtsvd, (void*) m, (void*) n, (void*) f, (void*) ldf, (void*) x, (void*) ldx, (void*) y, (void*) ldy, (void*) nrnk, (void*) tol, (void*) k, (void*) reig, (void*) imeig, (void*) z, (void*) ldz, (void*) res, (void*) b, (void*) ldb, (void*) v, (void*) ldv, (void*) s, (void*) lds, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobs, (flexiblas_fortran_charlen_t) len_jobz, (flexiblas_fortran_charlen_t) len_jobr, (flexiblas_fortran_charlen_t) len_jobq, (flexiblas_fortran_charlen_t) len_jobt, (flexiblas_fortran_charlen_t) len_jobf); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dgedmdq_(void* jobs, void* jobz, void* jobr, void* jobq, void* jobt, void* jobf, void* whtsvd, void* m, void* n, void* f, void* ldf, void* x, void* ldx, void* y, void* ldy, void* nrnk, void* tol, void* k, void* reig, void* imeig, void* z, void* ldz, void* res, void* b, void* ldb, void* v, void* ldv, void* s, void* lds, void* work, void* lwork, void* iwork, void* liwork, void* info, flexiblas_fortran_charlen_t len_jobs, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_jobr, flexiblas_fortran_charlen_t len_jobq, flexiblas_fortran_charlen_t len_jobt, flexiblas_fortran_charlen_t len_jobf)
{
    void (*fn) (void* jobs, void* jobz, void* jobr, void* jobq, void* jobt, void* jobf, void* whtsvd, void* m, void* n, void* f, void* ldf, void* x, void* ldx, void* y, void* ldy, void* nrnk, void* tol, void* k, void* reig, void* imeig, void* z, void* ldz, void* res, void* b, void* ldb, void* v, void* ldv, void* s, void* lds, void* work, void* lwork, void* iwork, void* liwork, void* info, flexiblas_fortran_charlen_t len_jobs, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_jobr, flexiblas_fortran_charlen_t len_jobq, flexiblas_fortran_charlen_t len_jobt, flexiblas_fortran_charlen_t len_jobf);

    *(void **) & fn = current_backend->lapack.dgedmdq.f77_blas_function;

    fn((void*) jobs, (void*) jobz, (void*) jobr, (void*) jobq, (void*) jobt, (void*) jobf, (void*) whtsvd, (void*) m, (void*) n, (void*) f, (void*) ldf, (void*) x, (void*) ldx, (void*) y, (void*) ldy, (void*) nrnk, (void*) tol, (void*) k, (void*) reig, (void*) imeig, (void*) z, (void*) ldz, (void*) res, (void*) b, (void*) ldb, (void*) v, (void*) ldv, (void*) s, (void*) lds, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobs, ( flexiblas_fortran_charlen_t ) len_jobz, ( flexiblas_fortran_charlen_t ) len_jobr, ( flexiblas_fortran_charlen_t ) len_jobq, ( flexiblas_fortran_charlen_t ) len_jobt, ( flexiblas_fortran_charlen_t ) len_jobf);

    return;
}
#ifndef __APPLE__
void flexiblas_real_dgedmdq(void* jobs, void* jobz, void* jobr, void* jobq, void* jobt, void* jobf, void* whtsvd, void* m, void* n, void* f, void* ldf, void* x, void* ldx, void* y, void* ldy, void* nrnk, void* tol, void* k, void* reig, void* imeig, void* z, void* ldz, void* res, void* b, void* ldb, void* v, void* ldv, void* s, void* lds, void* work, void* lwork, void* iwork, void* liwork, void* info, flexiblas_fortran_charlen_t len_jobs, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_jobr, flexiblas_fortran_charlen_t len_jobq, flexiblas_fortran_charlen_t len_jobt, flexiblas_fortran_charlen_t len_jobf) __attribute__((alias("flexiblas_real_dgedmdq_")));
#else
void flexiblas_real_dgedmdq(void* jobs, void* jobz, void* jobr, void* jobq, void* jobt, void* jobf, void* whtsvd, void* m, void* n, void* f, void* ldf, void* x, void* ldx, void* y, void* ldy, void* nrnk, void* tol, void* k, void* reig, void* imeig, void* z, void* ldz, void* res, void* b, void* ldb, void* v, void* ldv, void* s, void* lds, void* work, void* lwork, void* iwork, void* liwork, void* info, flexiblas_fortran_charlen_t len_jobs, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_jobr, flexiblas_fortran_charlen_t len_jobq, flexiblas_fortran_charlen_t len_jobt, flexiblas_fortran_charlen_t len_jobf){flexiblas_real_dgedmdq_((void*) jobs, (void*) jobz, (void*) jobr, (void*) jobq, (void*) jobt, (void*) jobf, (void*) whtsvd, (void*) m, (void*) n, (void*) f, (void*) ldf, (void*) x, (void*) ldx, (void*) y, (void*) ldy, (void*) nrnk, (void*) tol, (void*) k, (void*) reig, (void*) imeig, (void*) z, (void*) ldz, (void*) res, (void*) b, (void*) ldb, (void*) v, (void*) ldv, (void*) s, (void*) lds, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobs, (flexiblas_fortran_charlen_t) len_jobz, (flexiblas_fortran_charlen_t) len_jobr, (flexiblas_fortran_charlen_t) len_jobq, (flexiblas_fortran_charlen_t) len_jobt, (flexiblas_fortran_charlen_t) len_jobf);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dgedmdq_(void* jobs, void* jobz, void* jobr, void* jobq, void* jobt, void* jobf, void* whtsvd, void* m, void* n, void* f, void* ldf, void* x, void* ldx, void* y, void* ldy, void* nrnk, void* tol, void* k, void* reig, void* imeig, void* z, void* ldz, void* res, void* b, void* ldb, void* v, void* ldv, void* s, void* lds, void* work, void* lwork, void* iwork, void* liwork, void* info, flexiblas_fortran_charlen_t len_jobs, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_jobr, flexiblas_fortran_charlen_t len_jobq, flexiblas_fortran_charlen_t len_jobt, flexiblas_fortran_charlen_t len_jobf)
{
    void (*fn) (void* jobs, void* jobz, void* jobr, void* jobq, void* jobt, void* jobf, void* whtsvd, void* m, void* n, void* f, void* ldf, void* x, void* ldx, void* y, void* ldy, void* nrnk, void* tol, void* k, void* reig, void* imeig, void* z, void* ldz, void* res, void* b, void* ldb, void* v, void* ldv, void* s, void* lds, void* work, void* lwork, void* iwork, void* liwork, void* info, flexiblas_fortran_charlen_t len_jobs, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_jobr, flexiblas_fortran_charlen_t len_jobq, flexiblas_fortran_charlen_t len_jobt, flexiblas_fortran_charlen_t len_jobf);
    void (*fn_hook) (void* jobs, void* jobz, void* jobr, void* jobq, void* jobt, void* jobf, void* whtsvd, void* m, void* n, void* f, void* ldf, void* x, void* ldx, void* y, void* ldy, void* nrnk, void* tol, void* k, void* reig, void* imeig, void* z, void* ldz, void* res, void* b, void* ldb, void* v, void* ldv, void* s, void* lds, void* work, void* lwork, void* iwork, void* liwork, void* info, flexiblas_fortran_charlen_t len_jobs, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_jobr, flexiblas_fortran_charlen_t len_jobq, flexiblas_fortran_charlen_t len_jobt, flexiblas_fortran_charlen_t len_jobf);

    *(void **) &fn      = current_backend->lapack.dgedmdq.f77_blas_function;

    hook_pos_dgedmdq ++;
    if( hook_pos_dgedmdq < __flexiblas_hooks->dgedmdq.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dgedmdq.f77_hook_function[hook_pos_dgedmdq];
        fn_hook((void*) jobs, (void*) jobz, (void*) jobr, (void*) jobq, (void*) jobt, (void*) jobf, (void*) whtsvd, (void*) m, (void*) n, (void*) f, (void*) ldf, (void*) x, (void*) ldx, (void*) y, (void*) ldy, (void*) nrnk, (void*) tol, (void*) k, (void*) reig, (void*) imeig, (void*) z, (void*) ldz, (void*) res, (void*) b, (void*) ldb, (void*) v, (void*) ldv, (void*) s, (void*) lds, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobs, ( flexiblas_fortran_charlen_t ) len_jobz, ( flexiblas_fortran_charlen_t ) len_jobr, ( flexiblas_fortran_charlen_t ) len_jobq, ( flexiblas_fortran_charlen_t ) len_jobt, ( flexiblas_fortran_charlen_t ) len_jobf);
    } else {
        hook_pos_dgedmdq = 0;
        fn((void*) jobs, (void*) jobz, (void*) jobr, (void*) jobq, (void*) jobt, (void*) jobf, (void*) whtsvd, (void*) m, (void*) n, (void*) f, (void*) ldf, (void*) x, (void*) ldx, (void*) y, (void*) ldy, (void*) nrnk, (void*) tol, (void*) k, (void*) reig, (void*) imeig, (void*) z, (void*) ldz, (void*) res, (void*) b, (void*) ldb, (void*) v, (void*) ldv, (void*) s, (void*) lds, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobs, ( flexiblas_fortran_charlen_t ) len_jobz, ( flexiblas_fortran_charlen_t ) len_jobr, ( flexiblas_fortran_charlen_t ) len_jobq, ( flexiblas_fortran_charlen_t ) len_jobt, ( flexiblas_fortran_charlen_t ) len_jobf);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_dgedmdq(void* jobs, void* jobz, void* jobr, void* jobq, void* jobt, void* jobf, void* whtsvd, void* m, void* n, void* f, void* ldf, void* x, void* ldx, void* y, void* ldy, void* nrnk, void* tol, void* k, void* reig, void* imeig, void* z, void* ldz, void* res, void* b, void* ldb, void* v, void* ldv, void* s, void* lds, void* work, void* lwork, void* iwork, void* liwork, void* info, flexiblas_fortran_charlen_t len_jobs, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_jobr, flexiblas_fortran_charlen_t len_jobq, flexiblas_fortran_charlen_t len_jobt, flexiblas_fortran_charlen_t len_jobf) __attribute__((alias("flexiblas_chain_dgedmdq_")));
#else
void flexiblas_chain_dgedmdq(void* jobs, void* jobz, void* jobr, void* jobq, void* jobt, void* jobf, void* whtsvd, void* m, void* n, void* f, void* ldf, void* x, void* ldx, void* y, void* ldy, void* nrnk, void* tol, void* k, void* reig, void* imeig, void* z, void* ldz, void* res, void* b, void* ldb, void* v, void* ldv, void* s, void* lds, void* work, void* lwork, void* iwork, void* liwork, void* info, flexiblas_fortran_charlen_t len_jobs, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_jobr, flexiblas_fortran_charlen_t len_jobq, flexiblas_fortran_charlen_t len_jobt, flexiblas_fortran_charlen_t len_jobf){flexiblas_chain_dgedmdq_((void*) jobs, (void*) jobz, (void*) jobr, (void*) jobq, (void*) jobt, (void*) jobf, (void*) whtsvd, (void*) m, (void*) n, (void*) f, (void*) ldf, (void*) x, (void*) ldx, (void*) y, (void*) ldy, (void*) nrnk, (void*) tol, (void*) k, (void*) reig, (void*) imeig, (void*) z, (void*) ldz, (void*) res, (void*) b, (void*) ldb, (void*) v, (void*) ldv, (void*) s, (void*) lds, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobs, (flexiblas_fortran_charlen_t) len_jobz, (flexiblas_fortran_charlen_t) len_jobr, (flexiblas_fortran_charlen_t) len_jobq, (flexiblas_fortran_charlen_t) len_jobt, (flexiblas_fortran_charlen_t) len_jobf);}
#endif



