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
 * Copyright (C) Martin Koehler, 2015
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
	void *ptr_hsymbol = NULL;
	char cname[40]; 
	char hookname[40];

	/* Quick return  */
	if ( handle == NULL ) {
		fn ->call_cblas = NULL; 
		return 1; 
	}


	/*  Search for the hook function  */
	DPRINTF(3, "Look up hook: hook_cblas_%s\n", name);
	snprintf(hookname, 40, "hook_cblas_%s",name);
#ifdef __WIN32__
	ptr_hsymbol = GetProcAddress(handle, hookname); 
#else 
	ptr_hsymbol = dlsym(handle, hookname); 
#endif

	DPRINTF(2, "Look up: cblas_%s", name); 
	snprintf(cname, 39, "cblas_%s", name); 
#ifdef __WIN32__
	ptr_csymbol = GetProcAddress(handle, cname); 
#else 
	ptr_csymbol = dlsym(handle, cname); 
#endif

	if (ptr_hsymbol) {
		if ( !ptr_csymbol ) {
			DPRINTF(0,"Found hook for cblas_%s but not the real implementation.\n", name);
			abort();
		}
		fn->call_cblas = ptr_hsymbol;
		fn->cblas_real = ptr_csymbol;
	} else {
		fn -> call_cblas = ptr_csymbol; 
		fn -> cblas_real = ptr_csymbol;
	}

	if ( __flexiblas_verbose > 2) {
		if ( ptr_hsymbol ){ 
			fprintf(stderr, " %s.\n",(fn->call_cblas == NULL)?"failed":"sucess as hook."); 
		} else {
			fprintf(stderr, " %s.\n",(fn->call_cblas == NULL)?"failed":"sucess"); 
		}
	}

	if (fn->call_cblas == NULL) {
		return 1; 
	} else {
		return 0; 
	}
#else 
	fn->cblas_real = NULL;
	fn->call_cblas = NULL; 
	return 0; 
#endif
}

/*-----------------------------------------------------------------------------
 *  Fortran Loader 
 *-----------------------------------------------------------------------------*/
HIDDEN int __flexiblas_load_fortran_function( void * handle , struct flexiblas_blasfn * fn, const char *name) 
{
	void *ptr_fsymbol = NULL;
	void *ptr_hsymbol = NULL;
	char fname[40]; 
	int run = 0;

	/* Quick return  */
	if ( handle == NULL ) {
		fn ->fblas_real = NULL;
		fn ->call_fblas = NULL; 
		return 1; 
	}

	/* Load Hook if available */
	DPRINTF(3, "Look up hook: "); 
	for (run = 0; run < 3 ; run++) {
		if (run == 0) {
			snprintf(fname, 39, "hook_%s", name);
		} else if ( run == 1 ){
			snprintf(fname, 39, "hook_%s_", name);
		} else if ( run == 2 ){
			snprintf(fname, 39, "hook_%s__", name);
		} else {
			break;
		}
		if ( __flexiblas_verbose > 2) {
			fprintf(stderr, "%s ", fname); 
		}

#ifdef __WIN32__
		ptr_hsymbol = GetProcAddress(handle, fname); 
#else 
		ptr_hsymbol = dlsym(handle, fname); 
#endif
		if (ptr_hsymbol!=NULL) {
			break;
		}
	}
	if ( __flexiblas_verbose > 2) {
		fprintf(stderr, "%s\n", (ptr_hsymbol==NULL)?("not found."):("found.")); 
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
			fprintf(stderr, "%s ", fname); 
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

	/* Found Hook  */
	if (ptr_hsymbol) {
		if ( !ptr_fsymbol ) {
			DPRINTF(0,"Found hook for %s but not the real implementation.\n", name);
			abort();
		}
		fn->call_fblas = ptr_hsymbol;
		fn->fblas_real = ptr_fsymbol;
	} else {
		fn -> call_fblas = ptr_fsymbol; 
		fn -> fblas_real = ptr_fsymbol;
	}

	if ( __flexiblas_verbose > 2 ) {
		if ( ptr_hsymbol ){ 
			fprintf(stderr, " %s.\n",(fn->call_fblas == NULL)?"failed":"sucess as hook."); 
		} else {
			fprintf(stderr, " %s.\n",(fn->call_fblas == NULL)?"failed":"sucess"); 
		}
	}
	if (fn->call_fblas == NULL) {
		return 1; 
	} else {
		return 0; 
	}
}





