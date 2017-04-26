/* $Id$ */ 
/* 
 Copyright (C) 2013, 2014  Martin Köhler, koehlerm@mpi-magdeburg.mpg.de

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License.
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "flexiblas.h"
#include <ctype.h>
#include <string.h>

#ifndef __WIN32__
#define DLOPEN_FLAGS (RTLD_NOW|RTLD_GLOBAL)
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
#ifdef __WIN32__
	if (strstr(path,"\\")  != NULL ){
		return 1;
	} else {
		return 0;
	}
#else 
	if ( strstr(path,"/") != NULL) {
		return 1;
	} else {
		return 0;
	}
#endif
}

static int file_exists(const char * path )
{
#ifdef __WIN32__
	struct __stat64 st_buf;
	memset(&st_buf, 0, sizeof(struct __stat64));
	if ( _stat64(path, &st_buf) == 0) {
		if ( (S_ISREG(st_buf.st_mode) )){
			return 1;
		} else {
			return 0;
		}		
	} else {
		return 0;
	}

#else 
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
#endif
}

void * __flexiblas_dlopen( const char *libname, int flags ){
	char *path = NULL;
	char *filepath = NULL;
	size_t len;
	void *handle; 
	int found = 0 ; 
	int path_count = 0;



 	if ( ! is_relative(libname) ) {
		for ( path_count = 0 ; path_count < __flexiblas_count_additional_paths; path_count++){
			path =  __flexiblas_additional_paths[path_count];
			if (__flexiblas_verbose) fprintf(stderr,PRINT_PREFIX "path: %s\n",path);
			len = strlen(path) + strlen(libname) + 5;
			filepath = malloc(len * sizeof ( char ));
		#ifdef __WIN32__
			snprintf(filepath, len -1, "%s\\%s", path, libname);
		#else
			snprintf(filepath, len -1, "%s/%s", path, libname);
		#endif 
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
	} else {
		fprintf(stderr, COLOR_RED PRINT_PREFIX "Cannot determine type to the path: %s\n" COLOR_RESET,libname );
		found = 0;
		if (filepath) free(filepath); 
		filepath = NULL;
	}


	if( found ) {
#ifdef __WIN32__
		handle = LoadLibrary(filepath); 
#else

		dlerror(); 
		handle = dlopen(filepath, flags); 
#endif
		if (handle == NULL){
#ifdef __WIN32__
			fprintf(stderr, "Unable to load library: %s\r\n", filepath);
#else
			fprintf(stderr, COLOR_RED PRINT_PREFIX "dlopen: %s\n" COLOR_RESET, dlerror());
#endif
		}
		free(filepath);
		return handle;
	} else {
		if ( filepath) free(filepath);
		return NULL; 
	}
}

