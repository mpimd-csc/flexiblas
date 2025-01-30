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


static TLS_STORE uint8_t hook_pos_slarrd = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slarrd,SLARRD)(char* range, char* order, blasint* n, float* vl, float* vu, blasint* il, blasint* iu, float* gers, float* reltol, float* d, float* e, float* e2, float* pivmin, blasint* nsplit, blasint* isplit, blasint* m, float* w, float* werr, float* wl, float* wu, blasint* iblock, blasint* indexw, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order)
#else
void FC_GLOBAL(slarrd,SLARRD)(char* range, char* order, blasint* n, float* vl, float* vu, blasint* il, blasint* iu, float* gers, float* reltol, float* d, float* e, float* e2, float* pivmin, blasint* nsplit, blasint* isplit, blasint* m, float* w, float* werr, float* wl, float* wu, blasint* iblock, blasint* indexw, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order)
#endif
{
    void (*fn) (void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* gers, void* reltol, void* d, void* e, void* e2, void* pivmin, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wl, void* wu, void* iblock, void* indexw, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order);
    void (*fn_hook) (void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* gers, void* reltol, void* d, void* e, void* e2, void* pivmin, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wl, void* wu, void* iblock, void* indexw, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.slarrd.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->slarrd.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) range, (void*) order, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) gers, (void*) reltol, (void*) d, (void*) e, (void*) e2, (void*) pivmin, (void*) nsplit, (void*) isplit, (void*) m, (void*) w, (void*) werr, (void*) wl, (void*) wu, (void*) iblock, (void*) indexw, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_range, ( flexiblas_fortran_charlen_t ) len_order);
        return;
    } else {
        hook_pos_slarrd = 0;
        fn_hook((void*) range, (void*) order, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) gers, (void*) reltol, (void*) d, (void*) e, (void*) e2, (void*) pivmin, (void*) nsplit, (void*) isplit, (void*) m, (void*) w, (void*) werr, (void*) wl, (void*) wu, (void*) iblock, (void*) indexw, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_range, ( flexiblas_fortran_charlen_t ) len_order);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(slarrd,SLARRD)(char* range, char* order, blasint* n, float* vl, float* vu, blasint* il, blasint* iu, float* gers, float* reltol, float* d, float* e, float* e2, float* pivmin, blasint* nsplit, blasint* isplit, blasint* m, float* w, float* werr, float* wl, float* wu, blasint* iblock, blasint* indexw, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order) __attribute__((alias(MTS(FC_GLOBAL(slarrd,SLARRD)))));
void FC_GLOBAL3(slarrd,SLARRD)(char* range, char* order, blasint* n, float* vl, float* vu, blasint* il, blasint* iu, float* gers, float* reltol, float* d, float* e, float* e2, float* pivmin, blasint* nsplit, blasint* isplit, blasint* m, float* w, float* werr, float* wl, float* wu, blasint* iblock, blasint* indexw, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order) __attribute__((alias(MTS(FC_GLOBAL(slarrd,SLARRD)))));
#else
void FC_GLOBAL2(slarrd,SLARRD)(char* range, char* order, blasint* n, float* vl, float* vu, blasint* il, blasint* iu, float* gers, float* reltol, float* d, float* e, float* e2, float* pivmin, blasint* nsplit, blasint* isplit, blasint* m, float* w, float* werr, float* wl, float* wu, blasint* iblock, blasint* indexw, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order){ FC_GLOBAL(slarrd,SLARRD)((void*) range, (void*) order, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) gers, (void*) reltol, (void*) d, (void*) e, (void*) e2, (void*) pivmin, (void*) nsplit, (void*) isplit, (void*) m, (void*) w, (void*) werr, (void*) wl, (void*) wu, (void*) iblock, (void*) indexw, (void*) work, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_range, (flexiblas_fortran_charlen_t) len_order); }
void FC_GLOBAL3(slarrd,SLARRD)(char* range, char* order, blasint* n, float* vl, float* vu, blasint* il, blasint* iu, float* gers, float* reltol, float* d, float* e, float* e2, float* pivmin, blasint* nsplit, blasint* isplit, blasint* m, float* w, float* werr, float* wl, float* wu, blasint* iblock, blasint* indexw, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order){ FC_GLOBAL(slarrd,SLARRD)((void*) range, (void*) order, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) gers, (void*) reltol, (void*) d, (void*) e, (void*) e2, (void*) pivmin, (void*) nsplit, (void*) isplit, (void*) m, (void*) w, (void*) werr, (void*) wl, (void*) wu, (void*) iblock, (void*) indexw, (void*) work, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_range, (flexiblas_fortran_charlen_t) len_order); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slarrd_(void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* gers, void* reltol, void* d, void* e, void* e2, void* pivmin, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wl, void* wu, void* iblock, void* indexw, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order)
{
    void (*fn) (void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* gers, void* reltol, void* d, void* e, void* e2, void* pivmin, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wl, void* wu, void* iblock, void* indexw, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order);

    *(void **) & fn = current_backend->lapack.slarrd.f77_blas_function;

    fn((void*) range, (void*) order, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) gers, (void*) reltol, (void*) d, (void*) e, (void*) e2, (void*) pivmin, (void*) nsplit, (void*) isplit, (void*) m, (void*) w, (void*) werr, (void*) wl, (void*) wu, (void*) iblock, (void*) indexw, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_range, ( flexiblas_fortran_charlen_t ) len_order);

    return;
}
#ifndef __APPLE__
void flexiblas_real_slarrd(void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* gers, void* reltol, void* d, void* e, void* e2, void* pivmin, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wl, void* wu, void* iblock, void* indexw, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order) __attribute__((alias("flexiblas_real_slarrd_")));
#else
void flexiblas_real_slarrd(void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* gers, void* reltol, void* d, void* e, void* e2, void* pivmin, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wl, void* wu, void* iblock, void* indexw, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order){flexiblas_real_slarrd_((void*) range, (void*) order, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) gers, (void*) reltol, (void*) d, (void*) e, (void*) e2, (void*) pivmin, (void*) nsplit, (void*) isplit, (void*) m, (void*) w, (void*) werr, (void*) wl, (void*) wu, (void*) iblock, (void*) indexw, (void*) work, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_range, (flexiblas_fortran_charlen_t) len_order);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_slarrd_(void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* gers, void* reltol, void* d, void* e, void* e2, void* pivmin, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wl, void* wu, void* iblock, void* indexw, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order)
{
    void (*fn) (void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* gers, void* reltol, void* d, void* e, void* e2, void* pivmin, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wl, void* wu, void* iblock, void* indexw, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order);
    void (*fn_hook) (void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* gers, void* reltol, void* d, void* e, void* e2, void* pivmin, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wl, void* wu, void* iblock, void* indexw, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order);

    *(void **) &fn      = current_backend->lapack.slarrd.f77_blas_function;

    hook_pos_slarrd ++;
    if( hook_pos_slarrd < __flexiblas_hooks->slarrd.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->slarrd.f77_hook_function[hook_pos_slarrd];
        fn_hook((void*) range, (void*) order, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) gers, (void*) reltol, (void*) d, (void*) e, (void*) e2, (void*) pivmin, (void*) nsplit, (void*) isplit, (void*) m, (void*) w, (void*) werr, (void*) wl, (void*) wu, (void*) iblock, (void*) indexw, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_range, ( flexiblas_fortran_charlen_t ) len_order);
    } else {
        hook_pos_slarrd = 0;
        fn((void*) range, (void*) order, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) gers, (void*) reltol, (void*) d, (void*) e, (void*) e2, (void*) pivmin, (void*) nsplit, (void*) isplit, (void*) m, (void*) w, (void*) werr, (void*) wl, (void*) wu, (void*) iblock, (void*) indexw, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_range, ( flexiblas_fortran_charlen_t ) len_order);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_slarrd(void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* gers, void* reltol, void* d, void* e, void* e2, void* pivmin, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wl, void* wu, void* iblock, void* indexw, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order) __attribute__((alias("flexiblas_chain_slarrd_")));
#else
void flexiblas_chain_slarrd(void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* gers, void* reltol, void* d, void* e, void* e2, void* pivmin, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wl, void* wu, void* iblock, void* indexw, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order){flexiblas_chain_slarrd_((void*) range, (void*) order, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) gers, (void*) reltol, (void*) d, (void*) e, (void*) e2, (void*) pivmin, (void*) nsplit, (void*) isplit, (void*) m, (void*) w, (void*) werr, (void*) wl, (void*) wu, (void*) iblock, (void*) indexw, (void*) work, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_range, (flexiblas_fortran_charlen_t) len_order);}
#endif



