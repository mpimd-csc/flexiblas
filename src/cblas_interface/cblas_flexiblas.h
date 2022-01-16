/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * Linking FlexiBLAS statically or dynamically with other modules is making a
 * combined work based on FlexiBLAS. Thus, the terms and conditions of the GNU
 * General Public License cover the whole combination.
 *
 * As a special exception, the copyright holders of FlexiBLAS give you permission
 * to combine FlexiBLAS program with free software programs or libraries that are
 * released under the GNU LGPL and with independent modules that communicate with
 * FlexiBLAS solely through the BLAS/LAPACK interface as provided by the
 * BLAS/LAPACK reference implementation. You may copy and distribute such a system
 * following the terms of the GNU GPL for FlexiBLAS and the licenses of the other
 * code concerned, provided that you include the source code of that other code
 * when and as the GNU GPL requires distribution of source code and provided that
 * you do not modify the BLAS/LAPACK interface.
 *
 * Note that people who make modified versions of FlexiBLAS are not obligated to
 * grant this special exception for their modified versions; it is their choice
 * whether to do so. The GNU General Public License gives permission to release a
 * modified version without this exception; this exception also makes it possible
 * to release a modified version which carries forward this exception. If you
 * modify the BLAS/LAPACK interface, this exception does not apply to your
 * modified version of FlexiBLAS, and you must remove this exception when you
 * distribute your modified version.
 *
 * This exception is an additional permission under section 7 of the GNU General
 * Public License, version 3 (“GPLv3”)
 *
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) Martin Koehler, 2013-2022
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
            fn  = __flexiblas_hooks-> FN .cblas_hook_function[0]; \
            hook_cblas_ ## FN ## _pos = 0; \
        } else { \
            fn = flexiblas_real_cblas_ ## FN; \
        } } while(0)

#define CBLAS_HOOK_ADVANCE(FN) do { \
    hook_cblas_ ## FN ## _pos ++; \
    if ( hook_cblas_## FN ## _pos < __flexiblas_hooks-> FN .cblas_nhook) { \
        fn  = __flexiblas_hooks-> FN .cblas_hook_function[hook_cblas_## FN ## _pos]; \
    } else { \
        hook_cblas_## FN ## _pos = 0; \
        fn = flexiblas_real_cblas_ ## FN; \
    } } while (0)



#ifdef __cplusplus
};
#endif

#endif /* end of include guard: CBLAS_FLEXIBLAS_H */
