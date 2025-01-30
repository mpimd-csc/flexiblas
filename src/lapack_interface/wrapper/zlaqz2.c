//    SPDX-License-Identifier: LGPL-3.0-or-later
/*
    This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
    Copyright (C) 2013-2025 Martin Koehler

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this program; if not, write to the Free Software Foundation,
    Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <complex.h>

#include "flexiblas_config.h"

#include "flexiblas_fortran_mangle.h"

#include "flexiblas.h"

#include "flexiblas_fortran_char_len.h"


static TLS_STORE uint8_t hook_pos_zlaqz2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zlaqz2,ZLAQZ2)(blaslogical* ilschur, blaslogical* ilq, blaslogical* ilz, blasint* n, blasint* ilo, blasint* ihi, blasint* nw, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* q, blasint* ldq, double complex* z, blasint* ldz, blasint* ns, blasint* nd, double complex* alpha, double complex* beta, double complex* qc, blasint* ldqc, double complex* zc, blasint* ldzc, double complex* work, blasint* lwork, double* rwork, blasint* rec, blasint* info)
#else
void FC_GLOBAL(zlaqz2,ZLAQZ2)(blaslogical* ilschur, blaslogical* ilq, blaslogical* ilz, blasint* n, blasint* ilo, blasint* ihi, blasint* nw, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* q, blasint* ldq, double complex* z, blasint* ldz, blasint* ns, blasint* nd, double complex* alpha, double complex* beta, double complex* qc, blasint* ldqc, double complex* zc, blasint* ldzc, double complex* work, blasint* lwork, double* rwork, blasint* rec, blasint* info)
#endif
{
    void (*fn) (void* ilschur, void* ilq, void* ilz, void* n, void* ilo, void* ihi, void* nw, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* ns, void* nd, void* alpha, void* beta, void* qc, void* ldqc, void* zc, void* ldzc, void* work, void* lwork, void* rwork, void* rec, void* info);
    void (*fn_hook) (void* ilschur, void* ilq, void* ilz, void* n, void* ilo, void* ihi, void* nw, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* ns, void* nd, void* alpha, void* beta, void* qc, void* ldqc, void* zc, void* ldzc, void* work, void* lwork, void* rwork, void* rec, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.zlaqz2.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->zlaqz2.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) ilschur, (void*) ilq, (void*) ilz, (void*) n, (void*) ilo, (void*) ihi, (void*) nw, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) ns, (void*) nd, (void*) alpha, (void*) beta, (void*) qc, (void*) ldqc, (void*) zc, (void*) ldzc, (void*) work, (void*) lwork, (void*) rwork, (void*) rec, (void*) info);
        return;
    } else {
        hook_pos_zlaqz2 = 0;
        fn_hook((void*) ilschur, (void*) ilq, (void*) ilz, (void*) n, (void*) ilo, (void*) ihi, (void*) nw, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) ns, (void*) nd, (void*) alpha, (void*) beta, (void*) qc, (void*) ldqc, (void*) zc, (void*) ldzc, (void*) work, (void*) lwork, (void*) rwork, (void*) rec, (void*) info);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(zlaqz2,ZLAQZ2)(blaslogical* ilschur, blaslogical* ilq, blaslogical* ilz, blasint* n, blasint* ilo, blasint* ihi, blasint* nw, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* q, blasint* ldq, double complex* z, blasint* ldz, blasint* ns, blasint* nd, double complex* alpha, double complex* beta, double complex* qc, blasint* ldqc, double complex* zc, blasint* ldzc, double complex* work, blasint* lwork, double* rwork, blasint* rec, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(zlaqz2,ZLAQZ2)))));
void FC_GLOBAL3(zlaqz2,ZLAQZ2)(blaslogical* ilschur, blaslogical* ilq, blaslogical* ilz, blasint* n, blasint* ilo, blasint* ihi, blasint* nw, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* q, blasint* ldq, double complex* z, blasint* ldz, blasint* ns, blasint* nd, double complex* alpha, double complex* beta, double complex* qc, blasint* ldqc, double complex* zc, blasint* ldzc, double complex* work, blasint* lwork, double* rwork, blasint* rec, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(zlaqz2,ZLAQZ2)))));
#else
void FC_GLOBAL2(zlaqz2,ZLAQZ2)(blaslogical* ilschur, blaslogical* ilq, blaslogical* ilz, blasint* n, blasint* ilo, blasint* ihi, blasint* nw, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* q, blasint* ldq, double complex* z, blasint* ldz, blasint* ns, blasint* nd, double complex* alpha, double complex* beta, double complex* qc, blasint* ldqc, double complex* zc, blasint* ldzc, double complex* work, blasint* lwork, double* rwork, blasint* rec, blasint* info){ FC_GLOBAL(zlaqz2,ZLAQZ2)((void*) ilschur, (void*) ilq, (void*) ilz, (void*) n, (void*) ilo, (void*) ihi, (void*) nw, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) ns, (void*) nd, (void*) alpha, (void*) beta, (void*) qc, (void*) ldqc, (void*) zc, (void*) ldzc, (void*) work, (void*) lwork, (void*) rwork, (void*) rec, (void*) info); }
void FC_GLOBAL3(zlaqz2,ZLAQZ2)(blaslogical* ilschur, blaslogical* ilq, blaslogical* ilz, blasint* n, blasint* ilo, blasint* ihi, blasint* nw, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* q, blasint* ldq, double complex* z, blasint* ldz, blasint* ns, blasint* nd, double complex* alpha, double complex* beta, double complex* qc, blasint* ldqc, double complex* zc, blasint* ldzc, double complex* work, blasint* lwork, double* rwork, blasint* rec, blasint* info){ FC_GLOBAL(zlaqz2,ZLAQZ2)((void*) ilschur, (void*) ilq, (void*) ilz, (void*) n, (void*) ilo, (void*) ihi, (void*) nw, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) ns, (void*) nd, (void*) alpha, (void*) beta, (void*) qc, (void*) ldqc, (void*) zc, (void*) ldzc, (void*) work, (void*) lwork, (void*) rwork, (void*) rec, (void*) info); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zlaqz2_(void* ilschur, void* ilq, void* ilz, void* n, void* ilo, void* ihi, void* nw, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* ns, void* nd, void* alpha, void* beta, void* qc, void* ldqc, void* zc, void* ldzc, void* work, void* lwork, void* rwork, void* rec, void* info)
{
    void (*fn) (void* ilschur, void* ilq, void* ilz, void* n, void* ilo, void* ihi, void* nw, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* ns, void* nd, void* alpha, void* beta, void* qc, void* ldqc, void* zc, void* ldzc, void* work, void* lwork, void* rwork, void* rec, void* info);

    *(void **) & fn = current_backend->lapack.zlaqz2.f77_blas_function;

    fn((void*) ilschur, (void*) ilq, (void*) ilz, (void*) n, (void*) ilo, (void*) ihi, (void*) nw, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) ns, (void*) nd, (void*) alpha, (void*) beta, (void*) qc, (void*) ldqc, (void*) zc, (void*) ldzc, (void*) work, (void*) lwork, (void*) rwork, (void*) rec, (void*) info);

    return;
}
#ifndef __APPLE__
void flexiblas_real_zlaqz2(void* ilschur, void* ilq, void* ilz, void* n, void* ilo, void* ihi, void* nw, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* ns, void* nd, void* alpha, void* beta, void* qc, void* ldqc, void* zc, void* ldzc, void* work, void* lwork, void* rwork, void* rec, void* info) __attribute__((alias("flexiblas_real_zlaqz2_")));
#else
void flexiblas_real_zlaqz2(void* ilschur, void* ilq, void* ilz, void* n, void* ilo, void* ihi, void* nw, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* ns, void* nd, void* alpha, void* beta, void* qc, void* ldqc, void* zc, void* ldzc, void* work, void* lwork, void* rwork, void* rec, void* info){flexiblas_real_zlaqz2_((void*) ilschur, (void*) ilq, (void*) ilz, (void*) n, (void*) ilo, (void*) ihi, (void*) nw, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) ns, (void*) nd, (void*) alpha, (void*) beta, (void*) qc, (void*) ldqc, (void*) zc, (void*) ldzc, (void*) work, (void*) lwork, (void*) rwork, (void*) rec, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zlaqz2_(void* ilschur, void* ilq, void* ilz, void* n, void* ilo, void* ihi, void* nw, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* ns, void* nd, void* alpha, void* beta, void* qc, void* ldqc, void* zc, void* ldzc, void* work, void* lwork, void* rwork, void* rec, void* info)
{
    void (*fn) (void* ilschur, void* ilq, void* ilz, void* n, void* ilo, void* ihi, void* nw, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* ns, void* nd, void* alpha, void* beta, void* qc, void* ldqc, void* zc, void* ldzc, void* work, void* lwork, void* rwork, void* rec, void* info);
    void (*fn_hook) (void* ilschur, void* ilq, void* ilz, void* n, void* ilo, void* ihi, void* nw, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* ns, void* nd, void* alpha, void* beta, void* qc, void* ldqc, void* zc, void* ldzc, void* work, void* lwork, void* rwork, void* rec, void* info);

    *(void **) &fn      = current_backend->lapack.zlaqz2.f77_blas_function;

    hook_pos_zlaqz2 ++;
    if( hook_pos_zlaqz2 < __flexiblas_hooks->zlaqz2.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zlaqz2.f77_hook_function[hook_pos_zlaqz2];
        fn_hook((void*) ilschur, (void*) ilq, (void*) ilz, (void*) n, (void*) ilo, (void*) ihi, (void*) nw, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) ns, (void*) nd, (void*) alpha, (void*) beta, (void*) qc, (void*) ldqc, (void*) zc, (void*) ldzc, (void*) work, (void*) lwork, (void*) rwork, (void*) rec, (void*) info);
    } else {
        hook_pos_zlaqz2 = 0;
        fn((void*) ilschur, (void*) ilq, (void*) ilz, (void*) n, (void*) ilo, (void*) ihi, (void*) nw, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) ns, (void*) nd, (void*) alpha, (void*) beta, (void*) qc, (void*) ldqc, (void*) zc, (void*) ldzc, (void*) work, (void*) lwork, (void*) rwork, (void*) rec, (void*) info);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_zlaqz2(void* ilschur, void* ilq, void* ilz, void* n, void* ilo, void* ihi, void* nw, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* ns, void* nd, void* alpha, void* beta, void* qc, void* ldqc, void* zc, void* ldzc, void* work, void* lwork, void* rwork, void* rec, void* info) __attribute__((alias("flexiblas_chain_zlaqz2_")));
#else
void flexiblas_chain_zlaqz2(void* ilschur, void* ilq, void* ilz, void* n, void* ilo, void* ihi, void* nw, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* ns, void* nd, void* alpha, void* beta, void* qc, void* ldqc, void* zc, void* ldzc, void* work, void* lwork, void* rwork, void* rec, void* info){flexiblas_chain_zlaqz2_((void*) ilschur, (void*) ilq, (void*) ilz, (void*) n, (void*) ilo, (void*) ihi, (void*) nw, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) ns, (void*) nd, (void*) alpha, (void*) beta, (void*) qc, (void*) ldqc, (void*) zc, (void*) ldzc, (void*) work, (void*) lwork, (void*) rwork, (void*) rec, (void*) info);}
#endif



