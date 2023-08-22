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
 * Copyright (C) Martin Koehler, 2013-2023
 */



#include "flexiblas.h"
#include <errno.h>

/*-----------------------------------------------------------------------------
 *  Load CBLAS
 *-----------------------------------------------------------------------------*/
HIDDEN int __flexiblas_load_cblas_function( void * handle , struct flexiblas_blasfn * fn, const char *name)
{
#ifdef FLEXIBLAS_CBLAS
	void *ptr_csymbol = NULL;
	char cname[40];

	/* Quick return  */
	if ( handle == NULL ) {
		return 1;
	}


	snprintf(cname, 39, "cblas_%s", name);
	DPRINTF(3, "Look up: %18s", cname);
    ptr_csymbol = dlsym(handle, cname);

	fn -> cblas_real = ptr_csymbol;
    fn -> cblas_function = ptr_csymbol;

	if ( __flexiblas_verbose > 2) {
		fprintf(stderr, " %s.\n",(fn->cblas_function == NULL)?"failed":"success");
	}

	if (fn->cblas_function == NULL) {
		return 1;
	} else {
		return 0;
	}
#else
	fn->cblas_real = NULL;
    fn->cblas_function = NULL;
    return 0;
#endif
}

/*-----------------------------------------------------------------------------
 *  Fortran Loader
 *-----------------------------------------------------------------------------*/
HIDDEN int __flexiblas_load_fortran_function( void * handle , struct flexiblas_blasfn * fn, const char *name)
{
	void *ptr_fsymbol = NULL;
	char fname[40];
	int run = 0;

	/* Quick return  */
	if ( handle == NULL ) {
		fn ->f77_blas_function = NULL;
		return 1;
	}

	DPRINTF(3, "Look up: ");
	for (run = 0; run < 2 ; run++) {
		if (run == 0) {
			snprintf(fname, 39, "%s_", name);
		} else if ( run == 1 ){
			snprintf(fname, 39, "%s", name);
		} else {
			break;
		}
		if ( __flexiblas_verbose > 2) {
			fprintf(stderr, "%10s ", fname);
		}

#ifdef __WIN32__
		ptr_fsymbol = GetProcAddress(handle, fname);
#else
		ptr_fsymbol = dlsym(handle, fname);
#endif
		if (ptr_fsymbol!=NULL) {
			break;
		}
	}

	fn -> f77_blas_function = ptr_fsymbol;

    if ( fn->f77_blas_function) {
        if ( __flexiblas_verbose > 2) {
            fprintf(stderr, " at = 0x%lx  in = 0x%lx\n", (unsigned long)  ptr_fsymbol, (unsigned long) handle );
        }
    } else {
        if ( __flexiblas_verbose > 2 ) {
            fprintf(stderr, " %s.\n",(fn->f77_blas_function == NULL)?"failed":"success");
        }
    }
    if (fn->f77_blas_function == NULL) {
		return 1;
	} else {
		return 0;
	}
}





