
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
#include <complex.h>
#include <dlfcn.h>
#include "flexiblas_backend.h"
#include "flexiblas_real_calls.h"

/* #ifdef BACKEND_INTEGER8
#include <stdint.h>
#define Int int64_t
#else
#define Int int
#endif  */

/*-----------------------------------------------------------------------------
 * Info function, called once before  FlexiBLAS initializes the back end
 *-----------------------------------------------------------------------------*/
FLEXIBLAS_INFO_FUNCTION(info) {
/* The back end should use the post init mode. Important for CUDA */
	info->post_init = 0;
/* Specify the integer width  */
#ifdef  BACKEND_INTEGER8
	info -> backend_integer_size = 8;
#else
	info -> backend_integer_size = sizeof(int);
#endif

/* Specify that the interface is intel compatible */
#ifdef ZDOTC_MKL
	info -> intel_interface = 1;
#else
	info -> intel_interface = 0;
#endif
}



/*-----------------------------------------------------------------------------
 *  Init function, called once when FlexiBLAS initializes the backend.
 *-----------------------------------------------------------------------------*/
FLEXIBLAS_INIT_FUNCTION() {
	fprintf(stderr, "Library Init\n");
	/* Return 0 on success, != 0 otherwise   */
	return 0 ;
}



/*-----------------------------------------------------------------------------
 *  Exit function, called once when the program finishes.
 *-----------------------------------------------------------------------------*/
FLEXIBLAS_EXIT_FUNCTION() {
	fprintf(stderr, "Library Exit\n");
	return;
}



/*-----------------------------------------------------------------------------
 *  Include the remaining dumming functions to cheat LD
 *-----------------------------------------------------------------------------*/
#include "flexiblas_dummy_fortran.h"
#ifdef CBLAS_INTERFACE
#include "flexiblas_dummy_cblas.h"
#endif
