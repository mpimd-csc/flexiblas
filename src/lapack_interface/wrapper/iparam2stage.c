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


static TLS_STORE uint8_t hook_pos_iparam2stage = 0;
#ifdef FLEXIBLAS_ABI_INTEL
blasint FC_GLOBAL(iparam2stage,IPARAM2STAGE)(blasint* ispec, char* name, char* opts, blasint* ni, blasint* nbi, blasint* ibi, blasint* nxi, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts)
#else
blasint FC_GLOBAL(iparam2stage,IPARAM2STAGE)(blasint* ispec, char* name, char* opts, blasint* ni, blasint* nbi, blasint* ibi, blasint* nxi, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts)
#endif
{
    blasint (*fn) (void* ispec, void* name, void* opts, void* ni, void* nbi, void* ibi, void* nxi, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts);
    blasint (*fn_hook) (void* ispec, void* name, void* opts, void* ni, void* nbi, void* ibi, void* nxi, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts);
    blasint ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.iparam2stage.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->iparam2stage.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        ret = fn((void*) ispec, (void*) name, (void*) opts, (void*) ni, (void*) nbi, (void*) ibi, (void*) nxi, ( flexiblas_fortran_charlen_t ) len_name, ( flexiblas_fortran_charlen_t ) len_opts);
        return ret;
    } else {
        hook_pos_iparam2stage = 0;
        ret = fn_hook((void*) ispec, (void*) name, (void*) opts, (void*) ni, (void*) nbi, (void*) ibi, (void*) nxi, ( flexiblas_fortran_charlen_t ) len_name, ( flexiblas_fortran_charlen_t ) len_opts);
        return ret;
    }
}
#ifndef __APPLE__
blasint FC_GLOBAL2(iparam2stage,IPARAM2STAGE)(blasint* ispec, char* name, char* opts, blasint* ni, blasint* nbi, blasint* ibi, blasint* nxi, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts) __attribute__((alias(MTS(FC_GLOBAL(iparam2stage,IPARAM2STAGE)))));
blasint FC_GLOBAL3(iparam2stage,IPARAM2STAGE)(blasint* ispec, char* name, char* opts, blasint* ni, blasint* nbi, blasint* ibi, blasint* nxi, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts) __attribute__((alias(MTS(FC_GLOBAL(iparam2stage,IPARAM2STAGE)))));
#else
blasint FC_GLOBAL2(iparam2stage,IPARAM2STAGE)(blasint* ispec, char* name, char* opts, blasint* ni, blasint* nbi, blasint* ibi, blasint* nxi, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts){ return FC_GLOBAL(iparam2stage,IPARAM2STAGE)((void*) ispec, (void*) name, (void*) opts, (void*) ni, (void*) nbi, (void*) ibi, (void*) nxi, (flexiblas_fortran_charlen_t) len_name, (flexiblas_fortran_charlen_t) len_opts); }
blasint FC_GLOBAL3(iparam2stage,IPARAM2STAGE)(blasint* ispec, char* name, char* opts, blasint* ni, blasint* nbi, blasint* ibi, blasint* nxi, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts){ return FC_GLOBAL(iparam2stage,IPARAM2STAGE)((void*) ispec, (void*) name, (void*) opts, (void*) ni, (void*) nbi, (void*) ibi, (void*) nxi, (flexiblas_fortran_charlen_t) len_name, (flexiblas_fortran_charlen_t) len_opts); }
#endif




/* Real Implementation for Hooks */


blasint flexiblas_real_iparam2stage_(void* ispec, void* name, void* opts, void* ni, void* nbi, void* ibi, void* nxi, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts)
{
    blasint (*fn) (void* ispec, void* name, void* opts, void* ni, void* nbi, void* ibi, void* nxi, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts);
    blasint ret;

    *(void **) & fn = current_backend->lapack.iparam2stage.f77_blas_function;

    ret = fn((void*) ispec, (void*) name, (void*) opts, (void*) ni, (void*) nbi, (void*) ibi, (void*) nxi, ( flexiblas_fortran_charlen_t ) len_name, ( flexiblas_fortran_charlen_t ) len_opts);

    return ret;
}
#ifndef __APPLE__
blasint flexiblas_real_iparam2stage(void* ispec, void* name, void* opts, void* ni, void* nbi, void* ibi, void* nxi, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts) __attribute__((alias("flexiblas_real_iparam2stage_")));
#else
blasint flexiblas_real_iparam2stage(void* ispec, void* name, void* opts, void* ni, void* nbi, void* ibi, void* nxi, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts){return flexiblas_real_iparam2stage_((void*) ispec, (void*) name, (void*) opts, (void*) ni, (void*) nbi, (void*) ibi, (void*) nxi, (flexiblas_fortran_charlen_t) len_name, (flexiblas_fortran_charlen_t) len_opts);}
#endif




/* Chainloader for Hooks */


blasint flexiblas_chain_iparam2stage_(void* ispec, void* name, void* opts, void* ni, void* nbi, void* ibi, void* nxi, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts)
{
    blasint (*fn) (void* ispec, void* name, void* opts, void* ni, void* nbi, void* ibi, void* nxi, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts);
    blasint (*fn_hook) (void* ispec, void* name, void* opts, void* ni, void* nbi, void* ibi, void* nxi, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts);
    blasint ret;

    *(void **) &fn      = current_backend->lapack.iparam2stage.f77_blas_function;

    hook_pos_iparam2stage ++;
    if( hook_pos_iparam2stage < __flexiblas_hooks->iparam2stage.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->iparam2stage.f77_hook_function[hook_pos_iparam2stage];
        ret = fn_hook((void*) ispec, (void*) name, (void*) opts, (void*) ni, (void*) nbi, (void*) ibi, (void*) nxi, ( flexiblas_fortran_charlen_t )len_name, ( flexiblas_fortran_charlen_t )len_opts);
    } else {
        hook_pos_iparam2stage = 0;
        ret = fn((void*) ispec, (void*) name, (void*) opts, (void*) ni, (void*) nbi, (void*) ibi, (void*) nxi, ( flexiblas_fortran_charlen_t ) len_name, ( flexiblas_fortran_charlen_t ) len_opts);
    }
    return ret;
}
#ifndef __APPLE__
blasint flexiblas_chain_iparam2stage(void* ispec, void* name, void* opts, void* ni, void* nbi, void* ibi, void* nxi, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts) __attribute__((alias("flexiblas_chain_iparam2stage_")));
#else
blasint flexiblas_chain_iparam2stage(void* ispec, void* name, void* opts, void* ni, void* nbi, void* ibi, void* nxi, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts){return flexiblas_chain_iparam2stage_((void*) ispec, (void*) name, (void*) opts, (void*) ni, (void*) nbi, (void*) ibi, (void*) nxi, (flexiblas_fortran_charlen_t) len_name, (flexiblas_fortran_charlen_t) len_opts);}
#endif



