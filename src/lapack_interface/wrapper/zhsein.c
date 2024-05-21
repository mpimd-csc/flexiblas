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


static TLS_STORE uint8_t hook_pos_zhsein = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zhsein,ZHSEIN)(char* side, char* eigsrc, char* initv, blasint* select, blasint* n, double complex* h, blasint* ldh, double complex* w, double complex* vl, blasint* ldvl, double complex* vr, blasint* ldvr, blasint* mm, blasint* m, double complex* work, double* rwork, blasint* ifaill, blasint* ifailr, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_eigsrc, flexiblas_fortran_charlen_t len_initv)
#else
void FC_GLOBAL(zhsein,ZHSEIN)(char* side, char* eigsrc, char* initv, blasint* select, blasint* n, double complex* h, blasint* ldh, double complex* w, double complex* vl, blasint* ldvl, double complex* vr, blasint* ldvr, blasint* mm, blasint* m, double complex* work, double* rwork, blasint* ifaill, blasint* ifailr, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_eigsrc, flexiblas_fortran_charlen_t len_initv)
#endif
{
	void (*fn) (void* side, void* eigsrc, void* initv, void* select, void* n, void* h, void* ldh, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* rwork, void* ifaill, void* ifailr, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_eigsrc, flexiblas_fortran_charlen_t len_initv);
	void (*fn_hook) (void* side, void* eigsrc, void* initv, void* select, void* n, void* h, void* ldh, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* rwork, void* ifaill, void* ifailr, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_eigsrc, flexiblas_fortran_charlen_t len_initv);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.zhsein.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->zhsein.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) side, (void*) eigsrc, (void*) initv, (void*) select, (void*) n, (void*) h, (void*) ldh, (void*) w, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) rwork, (void*) ifaill, (void*) ifailr, (void*) info, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_eigsrc, ( flexiblas_fortran_charlen_t ) len_initv); 
		return;
	} else {
		hook_pos_zhsein = 0;
		fn_hook((void*) side, (void*) eigsrc, (void*) initv, (void*) select, (void*) n, (void*) h, (void*) ldh, (void*) w, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) rwork, (void*) ifaill, (void*) ifailr, (void*) info, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_eigsrc, ( flexiblas_fortran_charlen_t ) len_initv);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void zhsein_(char* side, char* eigsrc, char* initv, blasint* select, blasint* n, double complex* h, blasint* ldh, double complex* w, double complex* vl, blasint* ldvl, double complex* vr, blasint* ldvr, blasint* mm, blasint* m, double complex* work, double* rwork, blasint* ifaill, blasint* ifailr, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_eigsrc, flexiblas_fortran_charlen_t len_initv) __attribute__((alias(MTS(FC_GLOBAL(zhsein,ZHSEIN)))));
#else
#ifndef __APPLE__
void zhsein(char* side, char* eigsrc, char* initv, blasint* select, blasint* n, double complex* h, blasint* ldh, double complex* w, double complex* vl, blasint* ldvl, double complex* vr, blasint* ldvr, blasint* mm, blasint* m, double complex* work, double* rwork, blasint* ifaill, blasint* ifailr, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_eigsrc, flexiblas_fortran_charlen_t len_initv) __attribute__((alias(MTS(FC_GLOBAL(zhsein,ZHSEIN)))));
#else
void zhsein(char* side, char* eigsrc, char* initv, blasint* select, blasint* n, double complex* h, blasint* ldh, double complex* w, double complex* vl, blasint* ldvl, double complex* vr, blasint* ldvr, blasint* mm, blasint* m, double complex* work, double* rwork, blasint* ifaill, blasint* ifailr, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_eigsrc, flexiblas_fortran_charlen_t len_initv){ FC_GLOBAL(zhsein,ZHSEIN)((void*) side, (void*) eigsrc, (void*) initv, (void*) select, (void*) n, (void*) h, (void*) ldh, (void*) w, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) rwork, (void*) ifaill, (void*) ifailr, (void*) info, (flexiblas_fortran_charlen_t) len_side, (flexiblas_fortran_charlen_t) len_eigsrc, (flexiblas_fortran_charlen_t) len_initv); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zhsein_(void* side, void* eigsrc, void* initv, void* select, void* n, void* h, void* ldh, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* rwork, void* ifaill, void* ifailr, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_eigsrc, flexiblas_fortran_charlen_t len_initv)
{
	void (*fn) (void* side, void* eigsrc, void* initv, void* select, void* n, void* h, void* ldh, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* rwork, void* ifaill, void* ifailr, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_eigsrc, flexiblas_fortran_charlen_t len_initv);

	*(void **) & fn = current_backend->lapack.zhsein.f77_blas_function; 

		fn((void*) side, (void*) eigsrc, (void*) initv, (void*) select, (void*) n, (void*) h, (void*) ldh, (void*) w, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) rwork, (void*) ifaill, (void*) ifailr, (void*) info, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_eigsrc, ( flexiblas_fortran_charlen_t ) len_initv); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_zhsein(void* side, void* eigsrc, void* initv, void* select, void* n, void* h, void* ldh, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* rwork, void* ifaill, void* ifailr, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_eigsrc, flexiblas_fortran_charlen_t len_initv) __attribute__((alias("flexiblas_real_zhsein_")));
#else
void flexiblas_real_zhsein(void* side, void* eigsrc, void* initv, void* select, void* n, void* h, void* ldh, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* rwork, void* ifaill, void* ifailr, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_eigsrc, flexiblas_fortran_charlen_t len_initv){flexiblas_real_zhsein_((void*) side, (void*) eigsrc, (void*) initv, (void*) select, (void*) n, (void*) h, (void*) ldh, (void*) w, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) rwork, (void*) ifaill, (void*) ifailr, (void*) info, (flexiblas_fortran_charlen_t) len_side, (flexiblas_fortran_charlen_t) len_eigsrc, (flexiblas_fortran_charlen_t) len_initv);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zhsein_(void* side, void* eigsrc, void* initv, void* select, void* n, void* h, void* ldh, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* rwork, void* ifaill, void* ifailr, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_eigsrc, flexiblas_fortran_charlen_t len_initv)
{
	void (*fn) (void* side, void* eigsrc, void* initv, void* select, void* n, void* h, void* ldh, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* rwork, void* ifaill, void* ifailr, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_eigsrc, flexiblas_fortran_charlen_t len_initv);
	void (*fn_hook) (void* side, void* eigsrc, void* initv, void* select, void* n, void* h, void* ldh, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* rwork, void* ifaill, void* ifailr, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_eigsrc, flexiblas_fortran_charlen_t len_initv);

	*(void **) &fn      = current_backend->lapack.zhsein.f77_blas_function; 

    hook_pos_zhsein ++;
    if( hook_pos_zhsein < __flexiblas_hooks->zhsein.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zhsein.f77_hook_function[hook_pos_zhsein];
        fn_hook((void*) side, (void*) eigsrc, (void*) initv, (void*) select, (void*) n, (void*) h, (void*) ldh, (void*) w, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) rwork, (void*) ifaill, (void*) ifailr, (void*) info, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_eigsrc, ( flexiblas_fortran_charlen_t ) len_initv);
    } else {
        hook_pos_zhsein = 0;
		fn((void*) side, (void*) eigsrc, (void*) initv, (void*) select, (void*) n, (void*) h, (void*) ldh, (void*) w, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) rwork, (void*) ifaill, (void*) ifailr, (void*) info, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_eigsrc, ( flexiblas_fortran_charlen_t ) len_initv); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_zhsein(void* side, void* eigsrc, void* initv, void* select, void* n, void* h, void* ldh, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* rwork, void* ifaill, void* ifailr, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_eigsrc, flexiblas_fortran_charlen_t len_initv) __attribute__((alias("flexiblas_chain_zhsein_")));
#else
void flexiblas_chain_zhsein(void* side, void* eigsrc, void* initv, void* select, void* n, void* h, void* ldh, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* rwork, void* ifaill, void* ifailr, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_eigsrc, flexiblas_fortran_charlen_t len_initv){flexiblas_chain_zhsein_((void*) side, (void*) eigsrc, (void*) initv, (void*) select, (void*) n, (void*) h, (void*) ldh, (void*) w, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) rwork, (void*) ifaill, (void*) ifailr, (void*) info, (flexiblas_fortran_charlen_t) len_side, (flexiblas_fortran_charlen_t) len_eigsrc, (flexiblas_fortran_charlen_t) len_initv);}
#endif



