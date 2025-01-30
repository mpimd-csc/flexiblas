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


static TLS_STORE uint8_t hook_pos_dlasda = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlasda,DLASDA)(blasint* icompq, blasint* smlsiz, blasint* n, blasint* sqre, double* d, double* e, double* u, blasint* ldu, double* vt, blasint* k, double* difl, double* difr, double* z, double* poles, blasint* givptr, blasint* givcol, blasint* ldgcol, blasint* perm, double* givnum, double* c, double* s, double* work, blasint* iwork, blasint* info)
#else
void FC_GLOBAL(dlasda,DLASDA)(blasint* icompq, blasint* smlsiz, blasint* n, blasint* sqre, double* d, double* e, double* u, blasint* ldu, double* vt, blasint* k, double* difl, double* difr, double* z, double* poles, blasint* givptr, blasint* givcol, blasint* ldgcol, blasint* perm, double* givnum, double* c, double* s, double* work, blasint* iwork, blasint* info)
#endif
{
    void (*fn) (void* icompq, void* smlsiz, void* n, void* sqre, void* d, void* e, void* u, void* ldu, void* vt, void* k, void* difl, void* difr, void* z, void* poles, void* givptr, void* givcol, void* ldgcol, void* perm, void* givnum, void* c, void* s, void* work, void* iwork, void* info);
    void (*fn_hook) (void* icompq, void* smlsiz, void* n, void* sqre, void* d, void* e, void* u, void* ldu, void* vt, void* k, void* difl, void* difr, void* z, void* poles, void* givptr, void* givcol, void* ldgcol, void* perm, void* givnum, void* c, void* s, void* work, void* iwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dlasda.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dlasda.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) icompq, (void*) smlsiz, (void*) n, (void*) sqre, (void*) d, (void*) e, (void*) u, (void*) ldu, (void*) vt, (void*) k, (void*) difl, (void*) difr, (void*) z, (void*) poles, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) perm, (void*) givnum, (void*) c, (void*) s, (void*) work, (void*) iwork, (void*) info);
        return;
    } else {
        hook_pos_dlasda = 0;
        fn_hook((void*) icompq, (void*) smlsiz, (void*) n, (void*) sqre, (void*) d, (void*) e, (void*) u, (void*) ldu, (void*) vt, (void*) k, (void*) difl, (void*) difr, (void*) z, (void*) poles, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) perm, (void*) givnum, (void*) c, (void*) s, (void*) work, (void*) iwork, (void*) info);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(dlasda,DLASDA)(blasint* icompq, blasint* smlsiz, blasint* n, blasint* sqre, double* d, double* e, double* u, blasint* ldu, double* vt, blasint* k, double* difl, double* difr, double* z, double* poles, blasint* givptr, blasint* givcol, blasint* ldgcol, blasint* perm, double* givnum, double* c, double* s, double* work, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dlasda,DLASDA)))));
void FC_GLOBAL3(dlasda,DLASDA)(blasint* icompq, blasint* smlsiz, blasint* n, blasint* sqre, double* d, double* e, double* u, blasint* ldu, double* vt, blasint* k, double* difl, double* difr, double* z, double* poles, blasint* givptr, blasint* givcol, blasint* ldgcol, blasint* perm, double* givnum, double* c, double* s, double* work, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dlasda,DLASDA)))));
#else
void FC_GLOBAL2(dlasda,DLASDA)(blasint* icompq, blasint* smlsiz, blasint* n, blasint* sqre, double* d, double* e, double* u, blasint* ldu, double* vt, blasint* k, double* difl, double* difr, double* z, double* poles, blasint* givptr, blasint* givcol, blasint* ldgcol, blasint* perm, double* givnum, double* c, double* s, double* work, blasint* iwork, blasint* info){ FC_GLOBAL(dlasda,DLASDA)((void*) icompq, (void*) smlsiz, (void*) n, (void*) sqre, (void*) d, (void*) e, (void*) u, (void*) ldu, (void*) vt, (void*) k, (void*) difl, (void*) difr, (void*) z, (void*) poles, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) perm, (void*) givnum, (void*) c, (void*) s, (void*) work, (void*) iwork, (void*) info); }
void FC_GLOBAL3(dlasda,DLASDA)(blasint* icompq, blasint* smlsiz, blasint* n, blasint* sqre, double* d, double* e, double* u, blasint* ldu, double* vt, blasint* k, double* difl, double* difr, double* z, double* poles, blasint* givptr, blasint* givcol, blasint* ldgcol, blasint* perm, double* givnum, double* c, double* s, double* work, blasint* iwork, blasint* info){ FC_GLOBAL(dlasda,DLASDA)((void*) icompq, (void*) smlsiz, (void*) n, (void*) sqre, (void*) d, (void*) e, (void*) u, (void*) ldu, (void*) vt, (void*) k, (void*) difl, (void*) difr, (void*) z, (void*) poles, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) perm, (void*) givnum, (void*) c, (void*) s, (void*) work, (void*) iwork, (void*) info); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlasda_(void* icompq, void* smlsiz, void* n, void* sqre, void* d, void* e, void* u, void* ldu, void* vt, void* k, void* difl, void* difr, void* z, void* poles, void* givptr, void* givcol, void* ldgcol, void* perm, void* givnum, void* c, void* s, void* work, void* iwork, void* info)
{
    void (*fn) (void* icompq, void* smlsiz, void* n, void* sqre, void* d, void* e, void* u, void* ldu, void* vt, void* k, void* difl, void* difr, void* z, void* poles, void* givptr, void* givcol, void* ldgcol, void* perm, void* givnum, void* c, void* s, void* work, void* iwork, void* info);

    *(void **) & fn = current_backend->lapack.dlasda.f77_blas_function;

    fn((void*) icompq, (void*) smlsiz, (void*) n, (void*) sqre, (void*) d, (void*) e, (void*) u, (void*) ldu, (void*) vt, (void*) k, (void*) difl, (void*) difr, (void*) z, (void*) poles, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) perm, (void*) givnum, (void*) c, (void*) s, (void*) work, (void*) iwork, (void*) info);

    return;
}
#ifndef __APPLE__
void flexiblas_real_dlasda(void* icompq, void* smlsiz, void* n, void* sqre, void* d, void* e, void* u, void* ldu, void* vt, void* k, void* difl, void* difr, void* z, void* poles, void* givptr, void* givcol, void* ldgcol, void* perm, void* givnum, void* c, void* s, void* work, void* iwork, void* info) __attribute__((alias("flexiblas_real_dlasda_")));
#else
void flexiblas_real_dlasda(void* icompq, void* smlsiz, void* n, void* sqre, void* d, void* e, void* u, void* ldu, void* vt, void* k, void* difl, void* difr, void* z, void* poles, void* givptr, void* givcol, void* ldgcol, void* perm, void* givnum, void* c, void* s, void* work, void* iwork, void* info){flexiblas_real_dlasda_((void*) icompq, (void*) smlsiz, (void*) n, (void*) sqre, (void*) d, (void*) e, (void*) u, (void*) ldu, (void*) vt, (void*) k, (void*) difl, (void*) difr, (void*) z, (void*) poles, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) perm, (void*) givnum, (void*) c, (void*) s, (void*) work, (void*) iwork, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dlasda_(void* icompq, void* smlsiz, void* n, void* sqre, void* d, void* e, void* u, void* ldu, void* vt, void* k, void* difl, void* difr, void* z, void* poles, void* givptr, void* givcol, void* ldgcol, void* perm, void* givnum, void* c, void* s, void* work, void* iwork, void* info)
{
    void (*fn) (void* icompq, void* smlsiz, void* n, void* sqre, void* d, void* e, void* u, void* ldu, void* vt, void* k, void* difl, void* difr, void* z, void* poles, void* givptr, void* givcol, void* ldgcol, void* perm, void* givnum, void* c, void* s, void* work, void* iwork, void* info);
    void (*fn_hook) (void* icompq, void* smlsiz, void* n, void* sqre, void* d, void* e, void* u, void* ldu, void* vt, void* k, void* difl, void* difr, void* z, void* poles, void* givptr, void* givcol, void* ldgcol, void* perm, void* givnum, void* c, void* s, void* work, void* iwork, void* info);

    *(void **) &fn      = current_backend->lapack.dlasda.f77_blas_function;

    hook_pos_dlasda ++;
    if( hook_pos_dlasda < __flexiblas_hooks->dlasda.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlasda.f77_hook_function[hook_pos_dlasda];
        fn_hook((void*) icompq, (void*) smlsiz, (void*) n, (void*) sqre, (void*) d, (void*) e, (void*) u, (void*) ldu, (void*) vt, (void*) k, (void*) difl, (void*) difr, (void*) z, (void*) poles, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) perm, (void*) givnum, (void*) c, (void*) s, (void*) work, (void*) iwork, (void*) info);
    } else {
        hook_pos_dlasda = 0;
        fn((void*) icompq, (void*) smlsiz, (void*) n, (void*) sqre, (void*) d, (void*) e, (void*) u, (void*) ldu, (void*) vt, (void*) k, (void*) difl, (void*) difr, (void*) z, (void*) poles, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) perm, (void*) givnum, (void*) c, (void*) s, (void*) work, (void*) iwork, (void*) info);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_dlasda(void* icompq, void* smlsiz, void* n, void* sqre, void* d, void* e, void* u, void* ldu, void* vt, void* k, void* difl, void* difr, void* z, void* poles, void* givptr, void* givcol, void* ldgcol, void* perm, void* givnum, void* c, void* s, void* work, void* iwork, void* info) __attribute__((alias("flexiblas_chain_dlasda_")));
#else
void flexiblas_chain_dlasda(void* icompq, void* smlsiz, void* n, void* sqre, void* d, void* e, void* u, void* ldu, void* vt, void* k, void* difl, void* difr, void* z, void* poles, void* givptr, void* givcol, void* ldgcol, void* perm, void* givnum, void* c, void* s, void* work, void* iwork, void* info){flexiblas_chain_dlasda_((void*) icompq, (void*) smlsiz, (void*) n, (void*) sqre, (void*) d, (void*) e, (void*) u, (void*) ldu, (void*) vt, (void*) k, (void*) difl, (void*) difr, (void*) z, (void*) poles, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) perm, (void*) givnum, (void*) c, (void*) s, (void*) work, (void*) iwork, (void*) info);}
#endif



