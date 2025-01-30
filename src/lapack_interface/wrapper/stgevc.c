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


static TLS_STORE uint8_t hook_pos_stgevc = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(stgevc,STGEVC)(char* side, char* howmny, blaslogical* select, blasint* n, float* s, blasint* lds, float* p, blasint* ldp, float* vl, blasint* ldvl, float* vr, blasint* ldvr, blasint* mm, blasint* m, float* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_howmny)
#else
void FC_GLOBAL(stgevc,STGEVC)(char* side, char* howmny, blaslogical* select, blasint* n, float* s, blasint* lds, float* p, blasint* ldp, float* vl, blasint* ldvl, float* vr, blasint* ldvr, blasint* mm, blasint* m, float* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_howmny)
#endif
{
    void (*fn) (void* side, void* howmny, void* select, void* n, void* s, void* lds, void* p, void* ldp, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_howmny);
    void (*fn_hook) (void* side, void* howmny, void* select, void* n, void* s, void* lds, void* p, void* ldp, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_howmny);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.stgevc.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->stgevc.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) side, (void*) howmny, (void*) select, (void*) n, (void*) s, (void*) lds, (void*) p, (void*) ldp, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_howmny);
        return;
    } else {
        hook_pos_stgevc = 0;
        fn_hook((void*) side, (void*) howmny, (void*) select, (void*) n, (void*) s, (void*) lds, (void*) p, (void*) ldp, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_howmny);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(stgevc,STGEVC)(char* side, char* howmny, blaslogical* select, blasint* n, float* s, blasint* lds, float* p, blasint* ldp, float* vl, blasint* ldvl, float* vr, blasint* ldvr, blasint* mm, blasint* m, float* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_howmny) __attribute__((alias(MTS(FC_GLOBAL(stgevc,STGEVC)))));
void FC_GLOBAL3(stgevc,STGEVC)(char* side, char* howmny, blaslogical* select, blasint* n, float* s, blasint* lds, float* p, blasint* ldp, float* vl, blasint* ldvl, float* vr, blasint* ldvr, blasint* mm, blasint* m, float* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_howmny) __attribute__((alias(MTS(FC_GLOBAL(stgevc,STGEVC)))));
#else
void FC_GLOBAL2(stgevc,STGEVC)(char* side, char* howmny, blaslogical* select, blasint* n, float* s, blasint* lds, float* p, blasint* ldp, float* vl, blasint* ldvl, float* vr, blasint* ldvr, blasint* mm, blasint* m, float* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_howmny){ FC_GLOBAL(stgevc,STGEVC)((void*) side, (void*) howmny, (void*) select, (void*) n, (void*) s, (void*) lds, (void*) p, (void*) ldp, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_side, (flexiblas_fortran_charlen_t) len_howmny); }
void FC_GLOBAL3(stgevc,STGEVC)(char* side, char* howmny, blaslogical* select, blasint* n, float* s, blasint* lds, float* p, blasint* ldp, float* vl, blasint* ldvl, float* vr, blasint* ldvr, blasint* mm, blasint* m, float* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_howmny){ FC_GLOBAL(stgevc,STGEVC)((void*) side, (void*) howmny, (void*) select, (void*) n, (void*) s, (void*) lds, (void*) p, (void*) ldp, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_side, (flexiblas_fortran_charlen_t) len_howmny); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_stgevc_(void* side, void* howmny, void* select, void* n, void* s, void* lds, void* p, void* ldp, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_howmny)
{
    void (*fn) (void* side, void* howmny, void* select, void* n, void* s, void* lds, void* p, void* ldp, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_howmny);

    *(void **) & fn = current_backend->lapack.stgevc.f77_blas_function;

    fn((void*) side, (void*) howmny, (void*) select, (void*) n, (void*) s, (void*) lds, (void*) p, (void*) ldp, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_howmny);

    return;
}
#ifndef __APPLE__
void flexiblas_real_stgevc(void* side, void* howmny, void* select, void* n, void* s, void* lds, void* p, void* ldp, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_howmny) __attribute__((alias("flexiblas_real_stgevc_")));
#else
void flexiblas_real_stgevc(void* side, void* howmny, void* select, void* n, void* s, void* lds, void* p, void* ldp, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_howmny){flexiblas_real_stgevc_((void*) side, (void*) howmny, (void*) select, (void*) n, (void*) s, (void*) lds, (void*) p, (void*) ldp, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_side, (flexiblas_fortran_charlen_t) len_howmny);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_stgevc_(void* side, void* howmny, void* select, void* n, void* s, void* lds, void* p, void* ldp, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_howmny)
{
    void (*fn) (void* side, void* howmny, void* select, void* n, void* s, void* lds, void* p, void* ldp, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_howmny);
    void (*fn_hook) (void* side, void* howmny, void* select, void* n, void* s, void* lds, void* p, void* ldp, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_howmny);

    *(void **) &fn      = current_backend->lapack.stgevc.f77_blas_function;

    hook_pos_stgevc ++;
    if( hook_pos_stgevc < __flexiblas_hooks->stgevc.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->stgevc.f77_hook_function[hook_pos_stgevc];
        fn_hook((void*) side, (void*) howmny, (void*) select, (void*) n, (void*) s, (void*) lds, (void*) p, (void*) ldp, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_howmny);
    } else {
        hook_pos_stgevc = 0;
        fn((void*) side, (void*) howmny, (void*) select, (void*) n, (void*) s, (void*) lds, (void*) p, (void*) ldp, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_howmny);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_stgevc(void* side, void* howmny, void* select, void* n, void* s, void* lds, void* p, void* ldp, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_howmny) __attribute__((alias("flexiblas_chain_stgevc_")));
#else
void flexiblas_chain_stgevc(void* side, void* howmny, void* select, void* n, void* s, void* lds, void* p, void* ldp, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_howmny){flexiblas_chain_stgevc_((void*) side, (void*) howmny, (void*) select, (void*) n, (void*) s, (void*) lds, (void*) p, (void*) ldp, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_side, (flexiblas_fortran_charlen_t) len_howmny);}
#endif



