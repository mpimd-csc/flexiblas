/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) Martin Koehler, 2013-2020
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
		fn ->call_cblas = NULL;
		return 1;
	}


	snprintf(cname, 39, "cblas_%s", name);
	DPRINTF(3, "Look up: %18s", cname);
    ptr_csymbol = dlsym(handle, cname);

    fn -> call_cblas = ptr_csymbol;
	fn -> cblas_real = ptr_csymbol;
    fn -> cblas_function = ptr_csymbol;

	if ( __flexiblas_verbose > 2) {
		fprintf(stderr, " %s.\n",(fn->cblas_function == NULL)?"failed":"sucess");
	}

	if (fn->cblas_function == NULL) {
		return 1;
	} else {
		return 0;
	}
#else
	fn->cblas_real = NULL;
	fn->call_cblas = NULL;
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

	if ( __flexiblas_verbose > 2 ) {
		fprintf(stderr, " %s.\n",(fn->f77_blas_function == NULL)?"failed":"sucess");
	}
	if (fn->f77_blas_function == NULL) {
		return 1;
	} else {
		return 0;
	}
}





