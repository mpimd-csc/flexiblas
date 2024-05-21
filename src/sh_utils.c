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



#if defined(__linux__)
#ifndef _DEFAULT_SOURCE
#define _DEFAULT_SOURCE
#endif
#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE 500
#endif
#elif defined(__FreeBSD__)
#ifndef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 200809L
#endif
#endif

#include "flexiblas.h"
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <limits.h>
#include "cscutils/strutils.h"
#ifndef __WIN32__
#define DLOPEN_FLAGS (RTLD_NOW|RTLD_LOCAL)
#else
#define DLOPEN_FLAGS (0)
#include <windows.h>
#endif


/*-----------------------------------------------------------------------------
 *  Return true if it is an absolute path.
 *-----------------------------------------------------------------------------*/
static int is_absolute(const char * path )
{
	if ( path == NULL) return 0;
#ifdef __WIN32__
	if (strlen(path) < 2 ) return 0;
	if (( tolower(path[0]) >= 'a' && tolower(path[0]) <= 'z' && path[1]==':')
			|| ((path[0] == '\\' ) && (path[1] == '\\'))) {
		return 1;
	} else {
		return 0;
	}
#else
	if ( ((char)path[0]) == '/' ) {
		return 1;
	} else {
		return 0;
	}
#endif
}


/*-----------------------------------------------------------------------------
 *  Return true if it is at least an relative path.
 *-----------------------------------------------------------------------------*/
static int is_relative(const char * path)
{
	if ( path == NULL) return 0;
    if ( strstr(path,"/") != NULL) {
		return 1;
	} else {
		return 0;
	}
}

static int file_exists(const char * path )
{
	struct stat st_buf;
	memset (&st_buf, 0, sizeof(struct stat));
	if ( stat( path, &st_buf) == 0){
		if ( (S_ISREG(st_buf.st_mode) || S_ISLNK (st_buf.st_mode) )){
			return 1;
		} else {
			return 0;
		}
	} else {
		return 0;
	}
}

HIDDEN void * __flexiblas_dlopen( const char *libname, int flags, char ** sofile ){
	char *path = NULL;
	char *filepath = NULL;
	size_t len;
	void *handle;
	int found = 0 ;
	int path_count = 0;

    if (libname == NULL) return NULL;


	if ( strstr(libname,"/") == NULL ) {
        for ( path_count = -1 ; path_count < __flexiblas_count_additional_paths; path_count++){
            if ( path_count == -1 ) {
                if ( access(libname, R_OK) == 0 ) {
                    filepath = strdup(libname);
                } else {
                    continue;
                }
            } else {
    			path =  __flexiblas_additional_paths[path_count];
	    		len = strlen(path) + strlen(libname) + 5;
		    	filepath = malloc(len * sizeof ( char ));
			    snprintf(filepath, len -1, "%s/%s", path, libname);
            }
			DPRINTF(1, "Check if shared library exist: %s\n", filepath);
            if ( file_exists(filepath) ){
				found = 1;
				break;
			} else {
				free(filepath);
				filepath = NULL;
			}
		}
	} else if (is_absolute(libname)) {
		if ( file_exists(libname) ) {
			found = 1;
			filepath = strdup( libname);
		}
	} else if ( is_relative(libname)) {
		long path_max;
		char * resolvepath;
#ifdef PATH_MAX
		path_max = PATH_MAX;
#else
		path_max = pathconf(path, _PC_PATH_MAX);
		if (path_max <= 0)
			path_max = 32768;
#endif
		resolvepath = calloc(path_max, sizeof(char));

		if ( realpath(libname, resolvepath) == NULL ) {
			DPRINTF_ERROR(0,"Cannot determine type to the path: %s or file does not exists.\n",libname );
			found = 0;
			if (filepath) free(filepath);
			filepath = NULL;
			free(resolvepath);
			return NULL;
		} else {
			filepath = strdup(resolvepath);
			free(resolvepath);
			/* DPRINTF_ERROR(2, "File path: %s\n",filepath ); */
		}
		if (file_exists(filepath) ) {
			found = 1;
		}
	} else {
		DPRINTF_ERROR(0,"Cannot determine type to the path: %s\n",libname );

		found = 0;
		if (filepath) free(filepath);
		filepath = NULL;
	}


	if( found ) {
#ifdef __WIN32__
		handle = LoadLibrary(filepath);
#else
		void * ld_flags_sym_global;
		void * ld_flags_sym_lazy;
		int32_t ld_flags_global;
		int32_t ld_flags_lazy;
#ifdef __linux__
		void * ld_flags_sym_deep = NULL;
		int32_t ld_flags_deep = 0 ;
#endif
        /* flags = RTLD_GLOBAL | RTLD_NOW; */

		if ( flags < 0 ) {
			dlerror();
            csc_strremovedup(filepath, '/');
#ifdef HAVE_RTLD_NODELETE
			handle = dlopen(filepath, RTLD_LAZY | RTLD_LOCAL | RTLD_NODELETE);
#else
    		handle = dlopen(filepath, RTLD_LAZY | RTLD_LOCAL );
#endif
			if (!handle) {
				DPRINTF_ERROR(0, "Failed to load %s - error: %s \n", filepath, dlerror());
				if ( filepath) free(filepath);
				return NULL;
			}

			ld_flags_sym_global = dlsym(handle, "flexiblas_ld_global");
			if ( ld_flags_sym_global == NULL) {
				ld_flags_global = 0;
			} else {
				ld_flags_global = *((int32_t*) ld_flags_sym_global);
			}

			ld_flags_sym_lazy = dlsym(handle, "flexiblas_ld_lazy");
			if ( ld_flags_sym_lazy == NULL) {
				ld_flags_lazy = 0;
			} else {
				ld_flags_lazy = *((int32_t*) ld_flags_sym_lazy);
			}
#ifdef __linux__
			ld_flags_sym_deep = dlsym(handle, "flexiblas_ld_deep");
			if ( ld_flags_sym_deep == NULL) {
				ld_flags_deep = 0;
			} else {
				ld_flags_deep = *((int32_t*) ld_flags_sym_deep);
			}
#endif
			dlclose(handle);

			if ( ld_flags_global != 0 ) {
				flags = RTLD_GLOBAL;
				DPRINTF(1, "Load backend with RTLD_GLOBAL\n");
			} else {
				flags = RTLD_LOCAL;
			}

			if ( ld_flags_lazy != 0 ) {
				flags |= RTLD_LAZY;
				DPRINTF(1, "Load backend with RTLD_LAZY\n");
			} else {
				flags |= RTLD_NOW;
			}

#ifdef __linux__
			if ( ld_flags_deep != 0 ) {
				flags |= RTLD_DEEPBIND;
				DPRINTF(1, "Load backend with RTLD_DEEPBIND\n");
			}
#endif
/* #if ( defined(__powerpc__) || defined(__powerpc64__)) && !defined(__IBMC__)
			flags |= RTLD_DEEPBIND;
#endif  */
		}

		handle = dlopen(filepath, flags);
#endif



		if (handle == NULL){
#ifdef __WIN32__
			DPRINTF_ERROR(0, "Unable to load library: %s\r\n", filepath);
#else
			DPRINTF_ERROR(0, "dlopen: %s\n", dlerror());
#endif
		}
		if (sofile != NULL) {
			*sofile = strdup(filepath);
		}
		free(filepath);
		return handle;
	} else {
		if ( filepath) free(filepath);
		return NULL;
	}
}

HIDDEN int __flexiblas_dl_symbol_exist( const char *libname, const char *symbol_name )
{
	char *path = NULL;
	char *filepath = NULL;
	size_t len;
	void *handle;
	int found = 0 ;
	int path_count = 0;

    if (libname == NULL) return 0;


	if ( strstr(libname,"/") == NULL ) {
        for ( path_count = -1 ; path_count < __flexiblas_count_additional_paths; path_count++){
            if ( path_count == -1 ) {
                if ( access(libname, R_OK) == 0 ) {
                    filepath = strdup(libname);
                } else {
                    continue;
                }
            } else {
    			path =  __flexiblas_additional_paths[path_count];
	    		len = strlen(path) + strlen(libname) + 5;
		    	filepath = malloc(len * sizeof ( char ));
			    snprintf(filepath, len -1, "%s/%s", path, libname);
            }
			DPRINTF(1, "Check if shared library exist: %s\n", filepath);
            if ( file_exists(filepath) ){
				found = 1;
				break;
			} else {
				free(filepath);
				filepath = NULL;
			}
		}
	} else if (is_absolute(libname)) {
		if ( file_exists(libname) ) {
			found = 1;
			filepath = strdup( libname);
		}
	} else if ( is_relative(libname)) {
		long path_max;
		char * resolvepath;
#ifdef PATH_MAX
		path_max = PATH_MAX;
#else
		path_max = pathconf(path, _PC_PATH_MAX);
		if (path_max <= 0)
			path_max = 32768;
#endif
		resolvepath = calloc(path_max, sizeof(char));

		if ( realpath(libname, resolvepath) == NULL ) {
			DPRINTF_ERROR(0,"Cannot determine type to the path: %s or file does not exists.\n",libname );
			found = 0;
			if (filepath) free(filepath);
			filepath = NULL;
			free(resolvepath);
			return 0;
		} else {
			filepath = strdup(resolvepath);
			free(resolvepath);
			/* DPRINTF_ERROR(2, "Filepath: %s\n",filepath ); */
		}
		if (file_exists(filepath) ) {
			found = 1;
		}
	} else {
		DPRINTF_ERROR(0,"Cannot determine type to the path: %s\n",libname );

		found = 0;
		if (filepath) free(filepath);
		filepath = NULL;
	}


	if( found ) {
        void *sym = NULL;
        csc_strremovedup(filepath,'/');
        dlerror();
        handle = dlopen(filepath, RTLD_LAZY | RTLD_LOCAL);
        if (!handle) {
            DPRINTF_ERROR(0, "Failed to load %s - error: %s \n", filepath, dlerror());
            if ( filepath) free(filepath);
            return 0;
        }
        sym = dlsym(handle, symbol_name);
        dlclose(handle);
       	if ( filepath) free(filepath);
        return (sym == NULL) ? 0 : 1;
    }
	if ( filepath) free(filepath);
    return 0;
}


