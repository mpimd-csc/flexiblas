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




#ifndef CBLAS_FLEXIBLAS_H

#define CBLAS_FLEXIBLAS_H

#ifdef __cplusplus
extern "C" {
#endif

#include "flexiblas_real_cblas_calls.h"

#define CBLAS_BACKEND_INIT()  do { if ( current_backend->post_init != 0 ) {\
    __flexiblas_backend_init(current_backend);\
    current_backend->post_init = 0;\
}\
} while(0)

#define CBLAS_HOOK_SELECT(FN)  do { if ( __flexiblas_hooks-> FN .cblas_hook_function[0] != NULL) { \
    *(void **) &fn = __flexiblas_hooks-> FN .cblas_hook_function[0]; \
    hook_cblas_ ## FN ## _pos = 0; \
} else { \
    fn = flexiblas_real_cblas_ ## FN; \
} } while(0)

#define CBLAS_HOOK_ADVANCE(FN) do { \
    hook_cblas_ ## FN ## _pos ++; \
    if ( hook_cblas_## FN ## _pos < __flexiblas_hooks-> FN .cblas_nhook) { \
        *(void **) &fn = __flexiblas_hooks-> FN .cblas_hook_function[hook_cblas_## FN ## _pos]; \
    } else { \
        hook_cblas_## FN ## _pos = 0; \
        fn = flexiblas_real_cblas_ ## FN; \
    } } while (0)



#ifdef __cplusplus
};
#endif

#endif /* end of include guard: CBLAS_FLEXIBLAS_H */
