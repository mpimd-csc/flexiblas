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


static TLS_STORE uint8_t hook_pos_dlarre = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlarre,DLARRE)(char* range, blasint* n, double* vl, double* vu, blasint* il, blasint* iu, double* d, double* e, double* e2, double* rtol1, double* rtol2, double* spltol, blasint* nsplit, blasint* isplit, blasint* m, double* w, double* werr, double* wgap, blasint* iblock, blasint* indexw, double* gers, double* pivmin, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_range)
#else
void FC_GLOBAL(dlarre,DLARRE)(char* range, blasint* n, double* vl, double* vu, blasint* il, blasint* iu, double* d, double* e, double* e2, double* rtol1, double* rtol2, double* spltol, blasint* nsplit, blasint* isplit, blasint* m, double* w, double* werr, double* wgap, blasint* iblock, blasint* indexw, double* gers, double* pivmin, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_range)
#endif
{
    void (*fn) (void* range, void* n, void* vl, void* vu, void* il, void* iu, void* d, void* e, void* e2, void* rtol1, void* rtol2, void* spltol, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* pivmin, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range);
    void (*fn_hook) (void* range, void* n, void* vl, void* vu, void* il, void* iu, void* d, void* e, void* e2, void* rtol1, void* rtol2, void* spltol, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* pivmin, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dlarre.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dlarre.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) range, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) d, (void*) e, (void*) e2, (void*) rtol1, (void*) rtol2, (void*) spltol, (void*) nsplit, (void*) isplit, (void*) m, (void*) w, (void*) werr, (void*) wgap, (void*) iblock, (void*) indexw, (void*) gers, (void*) pivmin, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_range);
        return;
    } else {
        hook_pos_dlarre = 0;
        fn_hook((void*) range, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) d, (void*) e, (void*) e2, (void*) rtol1, (void*) rtol2, (void*) spltol, (void*) nsplit, (void*) isplit, (void*) m, (void*) w, (void*) werr, (void*) wgap, (void*) iblock, (void*) indexw, (void*) gers, (void*) pivmin, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_range);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(dlarre,DLARRE)(char* range, blasint* n, double* vl, double* vu, blasint* il, blasint* iu, double* d, double* e, double* e2, double* rtol1, double* rtol2, double* spltol, blasint* nsplit, blasint* isplit, blasint* m, double* w, double* werr, double* wgap, blasint* iblock, blasint* indexw, double* gers, double* pivmin, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_range) __attribute__((alias(MTS(FC_GLOBAL(dlarre,DLARRE)))));
void FC_GLOBAL3(dlarre,DLARRE)(char* range, blasint* n, double* vl, double* vu, blasint* il, blasint* iu, double* d, double* e, double* e2, double* rtol1, double* rtol2, double* spltol, blasint* nsplit, blasint* isplit, blasint* m, double* w, double* werr, double* wgap, blasint* iblock, blasint* indexw, double* gers, double* pivmin, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_range) __attribute__((alias(MTS(FC_GLOBAL(dlarre,DLARRE)))));
#else
void FC_GLOBAL2(dlarre,DLARRE)(char* range, blasint* n, double* vl, double* vu, blasint* il, blasint* iu, double* d, double* e, double* e2, double* rtol1, double* rtol2, double* spltol, blasint* nsplit, blasint* isplit, blasint* m, double* w, double* werr, double* wgap, blasint* iblock, blasint* indexw, double* gers, double* pivmin, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_range){ FC_GLOBAL(dlarre,DLARRE)((void*) range, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) d, (void*) e, (void*) e2, (void*) rtol1, (void*) rtol2, (void*) spltol, (void*) nsplit, (void*) isplit, (void*) m, (void*) w, (void*) werr, (void*) wgap, (void*) iblock, (void*) indexw, (void*) gers, (void*) pivmin, (void*) work, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_range); }
void FC_GLOBAL3(dlarre,DLARRE)(char* range, blasint* n, double* vl, double* vu, blasint* il, blasint* iu, double* d, double* e, double* e2, double* rtol1, double* rtol2, double* spltol, blasint* nsplit, blasint* isplit, blasint* m, double* w, double* werr, double* wgap, blasint* iblock, blasint* indexw, double* gers, double* pivmin, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_range){ FC_GLOBAL(dlarre,DLARRE)((void*) range, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) d, (void*) e, (void*) e2, (void*) rtol1, (void*) rtol2, (void*) spltol, (void*) nsplit, (void*) isplit, (void*) m, (void*) w, (void*) werr, (void*) wgap, (void*) iblock, (void*) indexw, (void*) gers, (void*) pivmin, (void*) work, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_range); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlarre_(void* range, void* n, void* vl, void* vu, void* il, void* iu, void* d, void* e, void* e2, void* rtol1, void* rtol2, void* spltol, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* pivmin, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range)
{
    void (*fn) (void* range, void* n, void* vl, void* vu, void* il, void* iu, void* d, void* e, void* e2, void* rtol1, void* rtol2, void* spltol, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* pivmin, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range);

    *(void **) & fn = current_backend->lapack.dlarre.f77_blas_function;

    fn((void*) range, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) d, (void*) e, (void*) e2, (void*) rtol1, (void*) rtol2, (void*) spltol, (void*) nsplit, (void*) isplit, (void*) m, (void*) w, (void*) werr, (void*) wgap, (void*) iblock, (void*) indexw, (void*) gers, (void*) pivmin, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_range);

    return;
}
#ifndef __APPLE__
void flexiblas_real_dlarre(void* range, void* n, void* vl, void* vu, void* il, void* iu, void* d, void* e, void* e2, void* rtol1, void* rtol2, void* spltol, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* pivmin, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range) __attribute__((alias("flexiblas_real_dlarre_")));
#else
void flexiblas_real_dlarre(void* range, void* n, void* vl, void* vu, void* il, void* iu, void* d, void* e, void* e2, void* rtol1, void* rtol2, void* spltol, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* pivmin, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range){flexiblas_real_dlarre_((void*) range, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) d, (void*) e, (void*) e2, (void*) rtol1, (void*) rtol2, (void*) spltol, (void*) nsplit, (void*) isplit, (void*) m, (void*) w, (void*) werr, (void*) wgap, (void*) iblock, (void*) indexw, (void*) gers, (void*) pivmin, (void*) work, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_range);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dlarre_(void* range, void* n, void* vl, void* vu, void* il, void* iu, void* d, void* e, void* e2, void* rtol1, void* rtol2, void* spltol, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* pivmin, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range)
{
    void (*fn) (void* range, void* n, void* vl, void* vu, void* il, void* iu, void* d, void* e, void* e2, void* rtol1, void* rtol2, void* spltol, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* pivmin, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range);
    void (*fn_hook) (void* range, void* n, void* vl, void* vu, void* il, void* iu, void* d, void* e, void* e2, void* rtol1, void* rtol2, void* spltol, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* pivmin, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range);

    *(void **) &fn      = current_backend->lapack.dlarre.f77_blas_function;

    hook_pos_dlarre ++;
    if( hook_pos_dlarre < __flexiblas_hooks->dlarre.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlarre.f77_hook_function[hook_pos_dlarre];
        fn_hook((void*) range, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) d, (void*) e, (void*) e2, (void*) rtol1, (void*) rtol2, (void*) spltol, (void*) nsplit, (void*) isplit, (void*) m, (void*) w, (void*) werr, (void*) wgap, (void*) iblock, (void*) indexw, (void*) gers, (void*) pivmin, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_range);
    } else {
        hook_pos_dlarre = 0;
        fn((void*) range, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) d, (void*) e, (void*) e2, (void*) rtol1, (void*) rtol2, (void*) spltol, (void*) nsplit, (void*) isplit, (void*) m, (void*) w, (void*) werr, (void*) wgap, (void*) iblock, (void*) indexw, (void*) gers, (void*) pivmin, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_range);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_dlarre(void* range, void* n, void* vl, void* vu, void* il, void* iu, void* d, void* e, void* e2, void* rtol1, void* rtol2, void* spltol, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* pivmin, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range) __attribute__((alias("flexiblas_chain_dlarre_")));
#else
void flexiblas_chain_dlarre(void* range, void* n, void* vl, void* vu, void* il, void* iu, void* d, void* e, void* e2, void* rtol1, void* rtol2, void* spltol, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wgap, void* iblock, void* indexw, void* gers, void* pivmin, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_range){flexiblas_chain_dlarre_((void*) range, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) d, (void*) e, (void*) e2, (void*) rtol1, (void*) rtol2, (void*) spltol, (void*) nsplit, (void*) isplit, (void*) m, (void*) w, (void*) werr, (void*) wgap, (void*) iblock, (void*) indexw, (void*) gers, (void*) pivmin, (void*) work, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_range);}
#endif



