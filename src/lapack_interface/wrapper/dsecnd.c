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


static TLS_STORE uint8_t hook_pos_dsecnd = 0;
#ifdef FLEXIBLAS_ABI_INTEL
double FC_GLOBAL(dsecnd,DSECND)(void)
#else
double FC_GLOBAL(dsecnd,DSECND)(void)
#endif
{
    double (*fn) (void);
    double (*fn_hook) (void);
    double ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dsecnd.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dsecnd.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        ret = fn();
        return ret;
    } else {
        hook_pos_dsecnd = 0;
        ret = fn_hook();
        return ret;
    }
}
#ifndef __APPLE__
double FC_GLOBAL2(dsecnd,DSECND)(void) __attribute__((alias(MTS(FC_GLOBAL(dsecnd,DSECND)))));
double FC_GLOBAL3(dsecnd,DSECND)(void) __attribute__((alias(MTS(FC_GLOBAL(dsecnd,DSECND)))));
#else
double FC_GLOBAL2(dsecnd,DSECND)(void){ return FC_GLOBAL(dsecnd,DSECND)(void); }
double FC_GLOBAL3(dsecnd,DSECND)(void){ return FC_GLOBAL(dsecnd,DSECND)(void); }
#endif




/* Real Implementation for Hooks */


double flexiblas_real_dsecnd_(void)
{
    double (*fn) (void);
    double ret;

    *(void **) & fn = current_backend->lapack.dsecnd.f77_blas_function;

    ret = fn();

    return ret;
}
#ifndef __APPLE__
double flexiblas_real_dsecnd(void) __attribute__((alias("flexiblas_real_dsecnd_")));
#else
double flexiblas_real_dsecnd(void){return flexiblas_real_dsecnd_();}
#endif




/* Chainloader for Hooks */


double flexiblas_chain_dsecnd_(void)
{
    double (*fn) (void);
    double (*fn_hook) (void);
    double ret;

    *(void **) &fn      = current_backend->lapack.dsecnd.f77_blas_function;

    hook_pos_dsecnd ++;
    if( hook_pos_dsecnd < __flexiblas_hooks->dsecnd.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dsecnd.f77_hook_function[hook_pos_dsecnd];
        ret = fn_hook();
    } else {
        hook_pos_dsecnd = 0;
        ret = fn();
    }
    return ret;
}
#ifndef __APPLE__
double flexiblas_chain_dsecnd(void) __attribute__((alias("flexiblas_chain_dsecnd_")));
#else
double flexiblas_chain_dsecnd(void){return flexiblas_chain_dsecnd_();}
#endif



