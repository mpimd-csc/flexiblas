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

#include <stdio.h>
#include <stdlib.h>
#include <complex.h>
#include <dlfcn.h>
#include "flexiblas_backend.h"
#include "flexiblas_real_calls.h"

#include <omp.h>

/* Generic Backends can be loaded without RTLD_GLOBAL  */
int32_t flexiblas_ld_global = 0;

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
	/* Return 0 on success, != 0 otherwise   */
	return 0 ;
}



/*-----------------------------------------------------------------------------
 *  Exit function, called once when the program finishes.
 *-----------------------------------------------------------------------------*/
FLEXIBLAS_EXIT_FUNCTION() {
	return;
}

/* Cheat the linker to include OpenMP  */
void __dummy_function_to_include_openmp() {
    omp_get_num_threads();
    acmlsetnumthreads_();
    acmlsetnumthreads();
}
/*-----------------------------------------------------------------------------
 *  Include the remaining dumming functions to cheat LD
 *-----------------------------------------------------------------------------*/
#include "flexiblas_dummy_fortran.h"
#ifdef CBLAS_INTERFACE
#include "flexiblas_dummy_cblas.h"
#endif
