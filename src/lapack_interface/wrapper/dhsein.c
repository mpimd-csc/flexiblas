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


static TLS_STORE uint8_t hook_pos_dhsein = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dhsein,DHSEIN)(char* side, char* eigsrc, char* initv, blaslogical* select, blasint* n, double* h, blasint* ldh, double* wr, double* wi, double* vl, blasint* ldvl, double* vr, blasint* ldvr, blasint* mm, blasint* m, double* work, blasint* ifaill, blasint* ifailr, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_eigsrc, flexiblas_fortran_charlen_t len_initv)
#else
void FC_GLOBAL(dhsein,DHSEIN)(char* side, char* eigsrc, char* initv, blaslogical* select, blasint* n, double* h, blasint* ldh, double* wr, double* wi, double* vl, blasint* ldvl, double* vr, blasint* ldvr, blasint* mm, blasint* m, double* work, blasint* ifaill, blasint* ifailr, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_eigsrc, flexiblas_fortran_charlen_t len_initv)
#endif
{
    void (*fn) (void* side, void* eigsrc, void* initv, void* select, void* n, void* h, void* ldh, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* ifaill, void* ifailr, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_eigsrc, flexiblas_fortran_charlen_t len_initv);
    void (*fn_hook) (void* side, void* eigsrc, void* initv, void* select, void* n, void* h, void* ldh, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* ifaill, void* ifailr, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_eigsrc, flexiblas_fortran_charlen_t len_initv);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dhsein.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dhsein.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) side, (void*) eigsrc, (void*) initv, (void*) select, (void*) n, (void*) h, (void*) ldh, (void*) wr, (void*) wi, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) ifaill, (void*) ifailr, (void*) info, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_eigsrc, ( flexiblas_fortran_charlen_t ) len_initv);
        return;
    } else {
        hook_pos_dhsein = 0;
        fn_hook((void*) side, (void*) eigsrc, (void*) initv, (void*) select, (void*) n, (void*) h, (void*) ldh, (void*) wr, (void*) wi, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) ifaill, (void*) ifailr, (void*) info, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_eigsrc, ( flexiblas_fortran_charlen_t ) len_initv);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(dhsein,DHSEIN)(char* side, char* eigsrc, char* initv, blaslogical* select, blasint* n, double* h, blasint* ldh, double* wr, double* wi, double* vl, blasint* ldvl, double* vr, blasint* ldvr, blasint* mm, blasint* m, double* work, blasint* ifaill, blasint* ifailr, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_eigsrc, flexiblas_fortran_charlen_t len_initv) __attribute__((alias(MTS(FC_GLOBAL(dhsein,DHSEIN)))));
void FC_GLOBAL3(dhsein,DHSEIN)(char* side, char* eigsrc, char* initv, blaslogical* select, blasint* n, double* h, blasint* ldh, double* wr, double* wi, double* vl, blasint* ldvl, double* vr, blasint* ldvr, blasint* mm, blasint* m, double* work, blasint* ifaill, blasint* ifailr, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_eigsrc, flexiblas_fortran_charlen_t len_initv) __attribute__((alias(MTS(FC_GLOBAL(dhsein,DHSEIN)))));
#else
void FC_GLOBAL2(dhsein,DHSEIN)(char* side, char* eigsrc, char* initv, blaslogical* select, blasint* n, double* h, blasint* ldh, double* wr, double* wi, double* vl, blasint* ldvl, double* vr, blasint* ldvr, blasint* mm, blasint* m, double* work, blasint* ifaill, blasint* ifailr, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_eigsrc, flexiblas_fortran_charlen_t len_initv){ FC_GLOBAL(dhsein,DHSEIN)((void*) side, (void*) eigsrc, (void*) initv, (void*) select, (void*) n, (void*) h, (void*) ldh, (void*) wr, (void*) wi, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) ifaill, (void*) ifailr, (void*) info, (flexiblas_fortran_charlen_t) len_side, (flexiblas_fortran_charlen_t) len_eigsrc, (flexiblas_fortran_charlen_t) len_initv); }
void FC_GLOBAL3(dhsein,DHSEIN)(char* side, char* eigsrc, char* initv, blaslogical* select, blasint* n, double* h, blasint* ldh, double* wr, double* wi, double* vl, blasint* ldvl, double* vr, blasint* ldvr, blasint* mm, blasint* m, double* work, blasint* ifaill, blasint* ifailr, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_eigsrc, flexiblas_fortran_charlen_t len_initv){ FC_GLOBAL(dhsein,DHSEIN)((void*) side, (void*) eigsrc, (void*) initv, (void*) select, (void*) n, (void*) h, (void*) ldh, (void*) wr, (void*) wi, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) ifaill, (void*) ifailr, (void*) info, (flexiblas_fortran_charlen_t) len_side, (flexiblas_fortran_charlen_t) len_eigsrc, (flexiblas_fortran_charlen_t) len_initv); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dhsein_(void* side, void* eigsrc, void* initv, void* select, void* n, void* h, void* ldh, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* ifaill, void* ifailr, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_eigsrc, flexiblas_fortran_charlen_t len_initv)
{
    void (*fn) (void* side, void* eigsrc, void* initv, void* select, void* n, void* h, void* ldh, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* ifaill, void* ifailr, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_eigsrc, flexiblas_fortran_charlen_t len_initv);

    *(void **) & fn = current_backend->lapack.dhsein.f77_blas_function;

    fn((void*) side, (void*) eigsrc, (void*) initv, (void*) select, (void*) n, (void*) h, (void*) ldh, (void*) wr, (void*) wi, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) ifaill, (void*) ifailr, (void*) info, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_eigsrc, ( flexiblas_fortran_charlen_t ) len_initv);

    return;
}
#ifndef __APPLE__
void flexiblas_real_dhsein(void* side, void* eigsrc, void* initv, void* select, void* n, void* h, void* ldh, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* ifaill, void* ifailr, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_eigsrc, flexiblas_fortran_charlen_t len_initv) __attribute__((alias("flexiblas_real_dhsein_")));
#else
void flexiblas_real_dhsein(void* side, void* eigsrc, void* initv, void* select, void* n, void* h, void* ldh, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* ifaill, void* ifailr, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_eigsrc, flexiblas_fortran_charlen_t len_initv){flexiblas_real_dhsein_((void*) side, (void*) eigsrc, (void*) initv, (void*) select, (void*) n, (void*) h, (void*) ldh, (void*) wr, (void*) wi, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) ifaill, (void*) ifailr, (void*) info, (flexiblas_fortran_charlen_t) len_side, (flexiblas_fortran_charlen_t) len_eigsrc, (flexiblas_fortran_charlen_t) len_initv);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dhsein_(void* side, void* eigsrc, void* initv, void* select, void* n, void* h, void* ldh, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* ifaill, void* ifailr, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_eigsrc, flexiblas_fortran_charlen_t len_initv)
{
    void (*fn) (void* side, void* eigsrc, void* initv, void* select, void* n, void* h, void* ldh, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* ifaill, void* ifailr, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_eigsrc, flexiblas_fortran_charlen_t len_initv);
    void (*fn_hook) (void* side, void* eigsrc, void* initv, void* select, void* n, void* h, void* ldh, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* ifaill, void* ifailr, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_eigsrc, flexiblas_fortran_charlen_t len_initv);

    *(void **) &fn      = current_backend->lapack.dhsein.f77_blas_function;

    hook_pos_dhsein ++;
    if( hook_pos_dhsein < __flexiblas_hooks->dhsein.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dhsein.f77_hook_function[hook_pos_dhsein];
        fn_hook((void*) side, (void*) eigsrc, (void*) initv, (void*) select, (void*) n, (void*) h, (void*) ldh, (void*) wr, (void*) wi, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) ifaill, (void*) ifailr, (void*) info, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_eigsrc, ( flexiblas_fortran_charlen_t ) len_initv);
    } else {
        hook_pos_dhsein = 0;
        fn((void*) side, (void*) eigsrc, (void*) initv, (void*) select, (void*) n, (void*) h, (void*) ldh, (void*) wr, (void*) wi, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) ifaill, (void*) ifailr, (void*) info, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_eigsrc, ( flexiblas_fortran_charlen_t ) len_initv);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_dhsein(void* side, void* eigsrc, void* initv, void* select, void* n, void* h, void* ldh, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* ifaill, void* ifailr, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_eigsrc, flexiblas_fortran_charlen_t len_initv) __attribute__((alias("flexiblas_chain_dhsein_")));
#else
void flexiblas_chain_dhsein(void* side, void* eigsrc, void* initv, void* select, void* n, void* h, void* ldh, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* ifaill, void* ifailr, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_eigsrc, flexiblas_fortran_charlen_t len_initv){flexiblas_chain_dhsein_((void*) side, (void*) eigsrc, (void*) initv, (void*) select, (void*) n, (void*) h, (void*) ldh, (void*) wr, (void*) wi, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) ifaill, (void*) ifailr, (void*) info, (flexiblas_fortran_charlen_t) len_side, (flexiblas_fortran_charlen_t) len_eigsrc, (flexiblas_fortran_charlen_t) len_initv);}
#endif



