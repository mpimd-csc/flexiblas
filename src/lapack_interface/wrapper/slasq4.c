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


static TLS_STORE uint8_t hook_pos_slasq4 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slasq4,SLASQ4)(blasint* i0, blasint* n0, float* z, blasint* pp, blasint* n0in, float* dmin, float* dmin1, float* dmin2, float* dn, float* dn1, float* dn2, float* tau, blasint* ttype, float* g)
#else
void FC_GLOBAL(slasq4,SLASQ4)(blasint* i0, blasint* n0, float* z, blasint* pp, blasint* n0in, float* dmin, float* dmin1, float* dmin2, float* dn, float* dn1, float* dn2, float* tau, blasint* ttype, float* g)
#endif
{
    void (*fn) (void* i0, void* n0, void* z, void* pp, void* n0in, void* dmin, void* dmin1, void* dmin2, void* dn, void* dn1, void* dn2, void* tau, void* ttype, void* g);
    void (*fn_hook) (void* i0, void* n0, void* z, void* pp, void* n0in, void* dmin, void* dmin1, void* dmin2, void* dn, void* dn1, void* dn2, void* tau, void* ttype, void* g);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.slasq4.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->slasq4.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) n0in, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dn1, (void*) dn2, (void*) tau, (void*) ttype, (void*) g);
        return;
    } else {
        hook_pos_slasq4 = 0;
        fn_hook((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) n0in, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dn1, (void*) dn2, (void*) tau, (void*) ttype, (void*) g);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(slasq4,SLASQ4)(blasint* i0, blasint* n0, float* z, blasint* pp, blasint* n0in, float* dmin, float* dmin1, float* dmin2, float* dn, float* dn1, float* dn2, float* tau, blasint* ttype, float* g) __attribute__((alias(MTS(FC_GLOBAL(slasq4,SLASQ4)))));
void FC_GLOBAL3(slasq4,SLASQ4)(blasint* i0, blasint* n0, float* z, blasint* pp, blasint* n0in, float* dmin, float* dmin1, float* dmin2, float* dn, float* dn1, float* dn2, float* tau, blasint* ttype, float* g) __attribute__((alias(MTS(FC_GLOBAL(slasq4,SLASQ4)))));
#else
void FC_GLOBAL2(slasq4,SLASQ4)(blasint* i0, blasint* n0, float* z, blasint* pp, blasint* n0in, float* dmin, float* dmin1, float* dmin2, float* dn, float* dn1, float* dn2, float* tau, blasint* ttype, float* g){ FC_GLOBAL(slasq4,SLASQ4)((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) n0in, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dn1, (void*) dn2, (void*) tau, (void*) ttype, (void*) g); }
void FC_GLOBAL3(slasq4,SLASQ4)(blasint* i0, blasint* n0, float* z, blasint* pp, blasint* n0in, float* dmin, float* dmin1, float* dmin2, float* dn, float* dn1, float* dn2, float* tau, blasint* ttype, float* g){ FC_GLOBAL(slasq4,SLASQ4)((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) n0in, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dn1, (void*) dn2, (void*) tau, (void*) ttype, (void*) g); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slasq4_(void* i0, void* n0, void* z, void* pp, void* n0in, void* dmin, void* dmin1, void* dmin2, void* dn, void* dn1, void* dn2, void* tau, void* ttype, void* g)
{
    void (*fn) (void* i0, void* n0, void* z, void* pp, void* n0in, void* dmin, void* dmin1, void* dmin2, void* dn, void* dn1, void* dn2, void* tau, void* ttype, void* g);

    *(void **) & fn = current_backend->lapack.slasq4.f77_blas_function;

    fn((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) n0in, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dn1, (void*) dn2, (void*) tau, (void*) ttype, (void*) g);

    return;
}
#ifndef __APPLE__
void flexiblas_real_slasq4(void* i0, void* n0, void* z, void* pp, void* n0in, void* dmin, void* dmin1, void* dmin2, void* dn, void* dn1, void* dn2, void* tau, void* ttype, void* g) __attribute__((alias("flexiblas_real_slasq4_")));
#else
void flexiblas_real_slasq4(void* i0, void* n0, void* z, void* pp, void* n0in, void* dmin, void* dmin1, void* dmin2, void* dn, void* dn1, void* dn2, void* tau, void* ttype, void* g){flexiblas_real_slasq4_((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) n0in, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dn1, (void*) dn2, (void*) tau, (void*) ttype, (void*) g);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_slasq4_(void* i0, void* n0, void* z, void* pp, void* n0in, void* dmin, void* dmin1, void* dmin2, void* dn, void* dn1, void* dn2, void* tau, void* ttype, void* g)
{
    void (*fn) (void* i0, void* n0, void* z, void* pp, void* n0in, void* dmin, void* dmin1, void* dmin2, void* dn, void* dn1, void* dn2, void* tau, void* ttype, void* g);
    void (*fn_hook) (void* i0, void* n0, void* z, void* pp, void* n0in, void* dmin, void* dmin1, void* dmin2, void* dn, void* dn1, void* dn2, void* tau, void* ttype, void* g);

    *(void **) &fn      = current_backend->lapack.slasq4.f77_blas_function;

    hook_pos_slasq4 ++;
    if( hook_pos_slasq4 < __flexiblas_hooks->slasq4.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->slasq4.f77_hook_function[hook_pos_slasq4];
        fn_hook((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) n0in, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dn1, (void*) dn2, (void*) tau, (void*) ttype, (void*) g);
    } else {
        hook_pos_slasq4 = 0;
        fn((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) n0in, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dn1, (void*) dn2, (void*) tau, (void*) ttype, (void*) g);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_slasq4(void* i0, void* n0, void* z, void* pp, void* n0in, void* dmin, void* dmin1, void* dmin2, void* dn, void* dn1, void* dn2, void* tau, void* ttype, void* g) __attribute__((alias("flexiblas_chain_slasq4_")));
#else
void flexiblas_chain_slasq4(void* i0, void* n0, void* z, void* pp, void* n0in, void* dmin, void* dmin1, void* dmin2, void* dn, void* dn1, void* dn2, void* tau, void* ttype, void* g){flexiblas_chain_slasq4_((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) n0in, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dn1, (void*) dn2, (void*) tau, (void*) ttype, (void*) g);}
#endif



