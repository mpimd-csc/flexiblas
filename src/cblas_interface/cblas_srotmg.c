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







#include "cblas.h"
#include "cblas_f77.h"
#include "../flexiblas.h"
#include "cblas_flexiblas.h"

static TLS_STORE uint8_t hook_cblas_srotmg_pos = 0;

void cblas_srotmg( float *d1, float *d2, float *b1,
        const float b2, float *p)
{
    void (*fn)
        ( float *d1, float *d2, float *b1,
          const float b2, float *p);
    CBLAS_BACKEND_INIT();
    CBLAS_HOOK_SELECT(srotmg);
    fn(d1,d2,b1,b2,p);


}

void flexiblas_chain_cblas_srotmg( float *d1, float *d2, float *b1,
        const float b2, float *p)
{
    void (*fn)
        ( float *d1, float *d2, float *b1,
          const float b2, float *p);
    CBLAS_HOOK_ADVANCE(srotmg);
    fn(d1,d2,b1,b2,p);


}

void flexiblas_real_cblas_srotmg( float *d1, float *d2, float *b1,
        float b2, float *p)
{
    if ( current_backend->blas.srotmg.cblas_function != NULL ) {
        void (*fn)
            ( float *d1, float *d2, float *b1,
              const float b2, float *p);
        *(void **) &fn = current_backend->blas.srotmg.cblas_function;
        fn(d1,d2,b1,b2,p);
    } else {
        FC_GLOBAL(srotmg,SROTMG)(d1,d2,b1,&b2,p);
    }
    return;
}
