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


static TLS_STORE uint8_t hook_pos_dbbcsd = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dbbcsd,DBBCSD)(char* jobu1, char* jobu2, char* jobv1t, char* jobv2t, char* trans, blasint* m, blasint* p, blasint* q, double* theta, double* phi, double* u1, blasint* ldu1, double* u2, blasint* ldu2, double* v1t, blasint* ldv1t, double* v2t, blasint* ldv2t, double* b11d, double* b11e, double* b12d, double* b12e, double* b21d, double* b21e, double* b22d, double* b22e, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans)
#else
void FC_GLOBAL(dbbcsd,DBBCSD)(char* jobu1, char* jobu2, char* jobv1t, char* jobv2t, char* trans, blasint* m, blasint* p, blasint* q, double* theta, double* phi, double* u1, blasint* ldu1, double* u2, blasint* ldu2, double* v1t, blasint* ldv1t, double* v2t, blasint* ldv2t, double* b11d, double* b11e, double* b12d, double* b12e, double* b21d, double* b21e, double* b22d, double* b22e, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans)
#endif
{
    void (*fn) (void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* m, void* p, void* q, void* theta, void* phi, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* b11d, void* b11e, void* b12d, void* b12e, void* b21d, void* b21e, void* b22d, void* b22e, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans);
    void (*fn_hook) (void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* m, void* p, void* q, void* theta, void* phi, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* b11d, void* b11e, void* b12d, void* b12e, void* b21d, void* b21e, void* b22d, void* b22e, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dbbcsd.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dbbcsd.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) jobv2t, (void*) trans, (void*) m, (void*) p, (void*) q, (void*) theta, (void*) phi, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) v2t, (void*) ldv2t, (void*) b11d, (void*) b11e, (void*) b12d, (void*) b12e, (void*) b21d, (void*) b21e, (void*) b22d, (void*) b22e, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobu1, ( flexiblas_fortran_charlen_t ) len_jobu2, ( flexiblas_fortran_charlen_t ) len_jobv1t, ( flexiblas_fortran_charlen_t ) len_jobv2t, ( flexiblas_fortran_charlen_t ) len_trans);
        return;
    } else {
        hook_pos_dbbcsd = 0;
        fn_hook((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) jobv2t, (void*) trans, (void*) m, (void*) p, (void*) q, (void*) theta, (void*) phi, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) v2t, (void*) ldv2t, (void*) b11d, (void*) b11e, (void*) b12d, (void*) b12e, (void*) b21d, (void*) b21e, (void*) b22d, (void*) b22e, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobu1, ( flexiblas_fortran_charlen_t ) len_jobu2, ( flexiblas_fortran_charlen_t ) len_jobv1t, ( flexiblas_fortran_charlen_t ) len_jobv2t, ( flexiblas_fortran_charlen_t ) len_trans);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(dbbcsd,DBBCSD)(char* jobu1, char* jobu2, char* jobv1t, char* jobv2t, char* trans, blasint* m, blasint* p, blasint* q, double* theta, double* phi, double* u1, blasint* ldu1, double* u2, blasint* ldu2, double* v1t, blasint* ldv1t, double* v2t, blasint* ldv2t, double* b11d, double* b11e, double* b12d, double* b12e, double* b21d, double* b21e, double* b22d, double* b22e, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans) __attribute__((alias(MTS(FC_GLOBAL(dbbcsd,DBBCSD)))));
void FC_GLOBAL3(dbbcsd,DBBCSD)(char* jobu1, char* jobu2, char* jobv1t, char* jobv2t, char* trans, blasint* m, blasint* p, blasint* q, double* theta, double* phi, double* u1, blasint* ldu1, double* u2, blasint* ldu2, double* v1t, blasint* ldv1t, double* v2t, blasint* ldv2t, double* b11d, double* b11e, double* b12d, double* b12e, double* b21d, double* b21e, double* b22d, double* b22e, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans) __attribute__((alias(MTS(FC_GLOBAL(dbbcsd,DBBCSD)))));
#else
void FC_GLOBAL2(dbbcsd,DBBCSD)(char* jobu1, char* jobu2, char* jobv1t, char* jobv2t, char* trans, blasint* m, blasint* p, blasint* q, double* theta, double* phi, double* u1, blasint* ldu1, double* u2, blasint* ldu2, double* v1t, blasint* ldv1t, double* v2t, blasint* ldv2t, double* b11d, double* b11e, double* b12d, double* b12e, double* b21d, double* b21e, double* b22d, double* b22e, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans){ FC_GLOBAL(dbbcsd,DBBCSD)((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) jobv2t, (void*) trans, (void*) m, (void*) p, (void*) q, (void*) theta, (void*) phi, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) v2t, (void*) ldv2t, (void*) b11d, (void*) b11e, (void*) b12d, (void*) b12e, (void*) b21d, (void*) b21e, (void*) b22d, (void*) b22e, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobu1, (flexiblas_fortran_charlen_t) len_jobu2, (flexiblas_fortran_charlen_t) len_jobv1t, (flexiblas_fortran_charlen_t) len_jobv2t, (flexiblas_fortran_charlen_t) len_trans); }
void FC_GLOBAL3(dbbcsd,DBBCSD)(char* jobu1, char* jobu2, char* jobv1t, char* jobv2t, char* trans, blasint* m, blasint* p, blasint* q, double* theta, double* phi, double* u1, blasint* ldu1, double* u2, blasint* ldu2, double* v1t, blasint* ldv1t, double* v2t, blasint* ldv2t, double* b11d, double* b11e, double* b12d, double* b12e, double* b21d, double* b21e, double* b22d, double* b22e, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans){ FC_GLOBAL(dbbcsd,DBBCSD)((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) jobv2t, (void*) trans, (void*) m, (void*) p, (void*) q, (void*) theta, (void*) phi, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) v2t, (void*) ldv2t, (void*) b11d, (void*) b11e, (void*) b12d, (void*) b12e, (void*) b21d, (void*) b21e, (void*) b22d, (void*) b22e, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobu1, (flexiblas_fortran_charlen_t) len_jobu2, (flexiblas_fortran_charlen_t) len_jobv1t, (flexiblas_fortran_charlen_t) len_jobv2t, (flexiblas_fortran_charlen_t) len_trans); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dbbcsd_(void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* m, void* p, void* q, void* theta, void* phi, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* b11d, void* b11e, void* b12d, void* b12e, void* b21d, void* b21e, void* b22d, void* b22e, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans)
{
    void (*fn) (void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* m, void* p, void* q, void* theta, void* phi, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* b11d, void* b11e, void* b12d, void* b12e, void* b21d, void* b21e, void* b22d, void* b22e, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans);

    *(void **) & fn = current_backend->lapack.dbbcsd.f77_blas_function;

    fn((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) jobv2t, (void*) trans, (void*) m, (void*) p, (void*) q, (void*) theta, (void*) phi, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) v2t, (void*) ldv2t, (void*) b11d, (void*) b11e, (void*) b12d, (void*) b12e, (void*) b21d, (void*) b21e, (void*) b22d, (void*) b22e, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobu1, ( flexiblas_fortran_charlen_t ) len_jobu2, ( flexiblas_fortran_charlen_t ) len_jobv1t, ( flexiblas_fortran_charlen_t ) len_jobv2t, ( flexiblas_fortran_charlen_t ) len_trans);

    return;
}
#ifndef __APPLE__
void flexiblas_real_dbbcsd(void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* m, void* p, void* q, void* theta, void* phi, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* b11d, void* b11e, void* b12d, void* b12e, void* b21d, void* b21e, void* b22d, void* b22e, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans) __attribute__((alias("flexiblas_real_dbbcsd_")));
#else
void flexiblas_real_dbbcsd(void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* m, void* p, void* q, void* theta, void* phi, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* b11d, void* b11e, void* b12d, void* b12e, void* b21d, void* b21e, void* b22d, void* b22e, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans){flexiblas_real_dbbcsd_((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) jobv2t, (void*) trans, (void*) m, (void*) p, (void*) q, (void*) theta, (void*) phi, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) v2t, (void*) ldv2t, (void*) b11d, (void*) b11e, (void*) b12d, (void*) b12e, (void*) b21d, (void*) b21e, (void*) b22d, (void*) b22e, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobu1, (flexiblas_fortran_charlen_t) len_jobu2, (flexiblas_fortran_charlen_t) len_jobv1t, (flexiblas_fortran_charlen_t) len_jobv2t, (flexiblas_fortran_charlen_t) len_trans);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dbbcsd_(void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* m, void* p, void* q, void* theta, void* phi, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* b11d, void* b11e, void* b12d, void* b12e, void* b21d, void* b21e, void* b22d, void* b22e, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans)
{
    void (*fn) (void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* m, void* p, void* q, void* theta, void* phi, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* b11d, void* b11e, void* b12d, void* b12e, void* b21d, void* b21e, void* b22d, void* b22e, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans);
    void (*fn_hook) (void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* m, void* p, void* q, void* theta, void* phi, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* b11d, void* b11e, void* b12d, void* b12e, void* b21d, void* b21e, void* b22d, void* b22e, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans);

    *(void **) &fn      = current_backend->lapack.dbbcsd.f77_blas_function;

    hook_pos_dbbcsd ++;
    if( hook_pos_dbbcsd < __flexiblas_hooks->dbbcsd.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dbbcsd.f77_hook_function[hook_pos_dbbcsd];
        fn_hook((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) jobv2t, (void*) trans, (void*) m, (void*) p, (void*) q, (void*) theta, (void*) phi, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) v2t, (void*) ldv2t, (void*) b11d, (void*) b11e, (void*) b12d, (void*) b12e, (void*) b21d, (void*) b21e, (void*) b22d, (void*) b22e, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobu1, ( flexiblas_fortran_charlen_t ) len_jobu2, ( flexiblas_fortran_charlen_t ) len_jobv1t, ( flexiblas_fortran_charlen_t ) len_jobv2t, ( flexiblas_fortran_charlen_t ) len_trans);
    } else {
        hook_pos_dbbcsd = 0;
        fn((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) jobv2t, (void*) trans, (void*) m, (void*) p, (void*) q, (void*) theta, (void*) phi, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) v2t, (void*) ldv2t, (void*) b11d, (void*) b11e, (void*) b12d, (void*) b12e, (void*) b21d, (void*) b21e, (void*) b22d, (void*) b22e, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobu1, ( flexiblas_fortran_charlen_t ) len_jobu2, ( flexiblas_fortran_charlen_t ) len_jobv1t, ( flexiblas_fortran_charlen_t ) len_jobv2t, ( flexiblas_fortran_charlen_t ) len_trans);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_dbbcsd(void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* m, void* p, void* q, void* theta, void* phi, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* b11d, void* b11e, void* b12d, void* b12e, void* b21d, void* b21e, void* b22d, void* b22e, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans) __attribute__((alias("flexiblas_chain_dbbcsd_")));
#else
void flexiblas_chain_dbbcsd(void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* m, void* p, void* q, void* theta, void* phi, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* b11d, void* b11e, void* b12d, void* b12e, void* b21d, void* b21e, void* b22d, void* b22e, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans){flexiblas_chain_dbbcsd_((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) jobv2t, (void*) trans, (void*) m, (void*) p, (void*) q, (void*) theta, (void*) phi, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) v2t, (void*) ldv2t, (void*) b11d, (void*) b11e, (void*) b12d, (void*) b12e, (void*) b21d, (void*) b21e, (void*) b22d, (void*) b22e, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobu1, (flexiblas_fortran_charlen_t) len_jobu2, (flexiblas_fortran_charlen_t) len_jobv1t, (flexiblas_fortran_charlen_t) len_jobv2t, (flexiblas_fortran_charlen_t) len_trans);}
#endif



