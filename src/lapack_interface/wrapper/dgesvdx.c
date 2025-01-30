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


static TLS_STORE uint8_t hook_pos_dgesvdx = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dgesvdx,DGESVDX)(char* jobu, char* jobvt, char* range, blasint* m, blasint* n, double* a, blasint* lda, double* vl, double* vu, blasint* il, blasint* iu, blasint* ns, double* s, double* u, blasint* ldu, double* vt, blasint* ldvt, double* work, blasint* lwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobvt, flexiblas_fortran_charlen_t len_range)
#else
void FC_GLOBAL(dgesvdx,DGESVDX)(char* jobu, char* jobvt, char* range, blasint* m, blasint* n, double* a, blasint* lda, double* vl, double* vu, blasint* il, blasint* iu, blasint* ns, double* s, double* u, blasint* ldu, double* vt, blasint* ldvt, double* work, blasint* lwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobvt, flexiblas_fortran_charlen_t len_range)
#endif
{
    void (*fn) (void* jobu, void* jobvt, void* range, void* m, void* n, void* a, void* lda, void* vl, void* vu, void* il, void* iu, void* ns, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobvt, flexiblas_fortran_charlen_t len_range);
    void (*fn_hook) (void* jobu, void* jobvt, void* range, void* m, void* n, void* a, void* lda, void* vl, void* vu, void* il, void* iu, void* ns, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobvt, flexiblas_fortran_charlen_t len_range);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dgesvdx.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dgesvdx.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) jobu, (void*) jobvt, (void*) range, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) ns, (void*) s, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) work, (void*) lwork, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobu, ( flexiblas_fortran_charlen_t ) len_jobvt, ( flexiblas_fortran_charlen_t ) len_range);
        return;
    } else {
        hook_pos_dgesvdx = 0;
        fn_hook((void*) jobu, (void*) jobvt, (void*) range, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) ns, (void*) s, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) work, (void*) lwork, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobu, ( flexiblas_fortran_charlen_t ) len_jobvt, ( flexiblas_fortran_charlen_t ) len_range);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(dgesvdx,DGESVDX)(char* jobu, char* jobvt, char* range, blasint* m, blasint* n, double* a, blasint* lda, double* vl, double* vu, blasint* il, blasint* iu, blasint* ns, double* s, double* u, blasint* ldu, double* vt, blasint* ldvt, double* work, blasint* lwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobvt, flexiblas_fortran_charlen_t len_range) __attribute__((alias(MTS(FC_GLOBAL(dgesvdx,DGESVDX)))));
void FC_GLOBAL3(dgesvdx,DGESVDX)(char* jobu, char* jobvt, char* range, blasint* m, blasint* n, double* a, blasint* lda, double* vl, double* vu, blasint* il, blasint* iu, blasint* ns, double* s, double* u, blasint* ldu, double* vt, blasint* ldvt, double* work, blasint* lwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobvt, flexiblas_fortran_charlen_t len_range) __attribute__((alias(MTS(FC_GLOBAL(dgesvdx,DGESVDX)))));
#else
void FC_GLOBAL2(dgesvdx,DGESVDX)(char* jobu, char* jobvt, char* range, blasint* m, blasint* n, double* a, blasint* lda, double* vl, double* vu, blasint* il, blasint* iu, blasint* ns, double* s, double* u, blasint* ldu, double* vt, blasint* ldvt, double* work, blasint* lwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobvt, flexiblas_fortran_charlen_t len_range){ FC_GLOBAL(dgesvdx,DGESVDX)((void*) jobu, (void*) jobvt, (void*) range, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) ns, (void*) s, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) work, (void*) lwork, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobu, (flexiblas_fortran_charlen_t) len_jobvt, (flexiblas_fortran_charlen_t) len_range); }
void FC_GLOBAL3(dgesvdx,DGESVDX)(char* jobu, char* jobvt, char* range, blasint* m, blasint* n, double* a, blasint* lda, double* vl, double* vu, blasint* il, blasint* iu, blasint* ns, double* s, double* u, blasint* ldu, double* vt, blasint* ldvt, double* work, blasint* lwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobvt, flexiblas_fortran_charlen_t len_range){ FC_GLOBAL(dgesvdx,DGESVDX)((void*) jobu, (void*) jobvt, (void*) range, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) ns, (void*) s, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) work, (void*) lwork, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobu, (flexiblas_fortran_charlen_t) len_jobvt, (flexiblas_fortran_charlen_t) len_range); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dgesvdx_(void* jobu, void* jobvt, void* range, void* m, void* n, void* a, void* lda, void* vl, void* vu, void* il, void* iu, void* ns, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobvt, flexiblas_fortran_charlen_t len_range)
{
    void (*fn) (void* jobu, void* jobvt, void* range, void* m, void* n, void* a, void* lda, void* vl, void* vu, void* il, void* iu, void* ns, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobvt, flexiblas_fortran_charlen_t len_range);

    *(void **) & fn = current_backend->lapack.dgesvdx.f77_blas_function;

    fn((void*) jobu, (void*) jobvt, (void*) range, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) ns, (void*) s, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) work, (void*) lwork, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobu, ( flexiblas_fortran_charlen_t ) len_jobvt, ( flexiblas_fortran_charlen_t ) len_range);

    return;
}
#ifndef __APPLE__
void flexiblas_real_dgesvdx(void* jobu, void* jobvt, void* range, void* m, void* n, void* a, void* lda, void* vl, void* vu, void* il, void* iu, void* ns, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobvt, flexiblas_fortran_charlen_t len_range) __attribute__((alias("flexiblas_real_dgesvdx_")));
#else
void flexiblas_real_dgesvdx(void* jobu, void* jobvt, void* range, void* m, void* n, void* a, void* lda, void* vl, void* vu, void* il, void* iu, void* ns, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobvt, flexiblas_fortran_charlen_t len_range){flexiblas_real_dgesvdx_((void*) jobu, (void*) jobvt, (void*) range, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) ns, (void*) s, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) work, (void*) lwork, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobu, (flexiblas_fortran_charlen_t) len_jobvt, (flexiblas_fortran_charlen_t) len_range);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dgesvdx_(void* jobu, void* jobvt, void* range, void* m, void* n, void* a, void* lda, void* vl, void* vu, void* il, void* iu, void* ns, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobvt, flexiblas_fortran_charlen_t len_range)
{
    void (*fn) (void* jobu, void* jobvt, void* range, void* m, void* n, void* a, void* lda, void* vl, void* vu, void* il, void* iu, void* ns, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobvt, flexiblas_fortran_charlen_t len_range);
    void (*fn_hook) (void* jobu, void* jobvt, void* range, void* m, void* n, void* a, void* lda, void* vl, void* vu, void* il, void* iu, void* ns, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobvt, flexiblas_fortran_charlen_t len_range);

    *(void **) &fn      = current_backend->lapack.dgesvdx.f77_blas_function;

    hook_pos_dgesvdx ++;
    if( hook_pos_dgesvdx < __flexiblas_hooks->dgesvdx.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dgesvdx.f77_hook_function[hook_pos_dgesvdx];
        fn_hook((void*) jobu, (void*) jobvt, (void*) range, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) ns, (void*) s, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) work, (void*) lwork, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobu, ( flexiblas_fortran_charlen_t ) len_jobvt, ( flexiblas_fortran_charlen_t ) len_range);
    } else {
        hook_pos_dgesvdx = 0;
        fn((void*) jobu, (void*) jobvt, (void*) range, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) ns, (void*) s, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) work, (void*) lwork, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobu, ( flexiblas_fortran_charlen_t ) len_jobvt, ( flexiblas_fortran_charlen_t ) len_range);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_dgesvdx(void* jobu, void* jobvt, void* range, void* m, void* n, void* a, void* lda, void* vl, void* vu, void* il, void* iu, void* ns, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobvt, flexiblas_fortran_charlen_t len_range) __attribute__((alias("flexiblas_chain_dgesvdx_")));
#else
void flexiblas_chain_dgesvdx(void* jobu, void* jobvt, void* range, void* m, void* n, void* a, void* lda, void* vl, void* vu, void* il, void* iu, void* ns, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobvt, flexiblas_fortran_charlen_t len_range){flexiblas_chain_dgesvdx_((void*) jobu, (void*) jobvt, (void*) range, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) ns, (void*) s, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) work, (void*) lwork, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobu, (flexiblas_fortran_charlen_t) len_jobvt, (flexiblas_fortran_charlen_t) len_range);}
#endif



