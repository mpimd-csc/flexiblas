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
 * Copyright (C) Martin Köhler, 2015
 */


#ifndef FLEXIBLAS_H
#define FLEXIBLAS_H

#define FLEXIBLAS_VERSION "1.3.0" 
#define FLEXIBLAS_VERSION_MAJOR 1 
#define FLEXIBLAS_VERSION_MINOR 3 
#define FLEXIBLAS_VERSION_PATCH 0 

#define FLEXIBLAS_YEARS "2014, 2015" 

#define COLOR_RED "\033[1;2;31m"
#define COLOR_RESET "\033[0m"
#define PRINT_PREFIX "<flexiblas> "


#include "flexiblas_config.h"

#include <stdlib.h>
#include <stdio.h> 
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdint.h>

#ifndef __WIN32__ 
#include <dlfcn.h>
#include <pthread.h> 
#endif 

#include <errno.h>
#include <ctype.h>
#include <assert.h>
#include <sys/time.h>

/* BLAS Backend Int Size  */
#ifndef Int
#ifndef INTEGER8 
#define Int 	int
#define FLEXIBLAS_RC "flexiblasrc" 
#else 
#define Int 	int64_t
#define FLEXIBLAS_RC "flexiblasrc64" 
#endif
#endif 

#include "cscutils/inifile.h"
#include "flexiblas_structures.h"
#include "flexiblas_backend.h"
#include "flexiblas_api.h"


#ifndef CMAKE_INSTALL_FULL_SYSCONFDIR 
#define CMAKE_INSTALL_FULL_SYSCONFDIR "/usr/local/etc" 
#endif 



extern void flexiblas_print_profile(); 
extern csc_ini_file_t __flexiblas_config; 

// extern int __flexiblas_initialized; 
int __flexiblas_verbose; 
int __flexiblas_profile; 

#define	DPRINTF( level, text, args... )	do { if ( __flexiblas_verbose >= (level)) {fprintf(stderr, PRINT_PREFIX "" text, ## args); } } while(0) 


#ifndef RTLD_DEEPBIND 
#define RTLD_DEEPBIND 0 
#endif 

#define FLEXIBLAS_ENV_SO_EXTENSION 0x01
#define FLEXIBLAS_ENV_HOMEDIR      0x02
#define FLEXIBLAS_ENV_GLOBAL_RC    0x03
#define FLEXIBLAS_ENV_USER_RC 	   0x04

/*  Global Vars */
extern int __flexiblas_count_additional_paths;
extern char **  __flexiblas_additional_paths;
extern int __flexiblas_verbose; 
extern int __flexiblas_profile;

extern flexiblas_backend_t *current_backend ;
extern flexiblas_backend_t **loaded_backends;
extern int                  nloaded_backends;


extern void __flexiblas_print_copyright(int prefix);
extern char * __flexiblas_getenv(int what);
extern int __flexiblas_file_exist(const char *path);  
extern void __flexiblas_insert_fallback_blas(csc_ini_file_t *conig);
extern void __flexiblas_read_config_file(const char *path, csc_ini_file_t *config, char **default_map );
extern void __flexiblas_add_path(const char * path );
extern void __flexiblas_free_paths();
extern void * __flexiblas_dlopen( const char *libname, int flags, char **soname );

int __flexiblas_load_cblas(flexiblas_backend_t *backend);  
int __flexiblas_load_fblas(flexiblas_backend_t *backend, int *loaded, int* failed);  
int __flexiblas_load_extblas(flexiblas_backend_t * backend, int *loaded, int *failed); 
int __flexiblas_setup_xerbla(flexiblas_backend_t * backend); 
void __flexiblas_load_set_num_threads(flexiblas_backend_t * backend); 
void __flexiblas_load_get_num_threads(flexiblas_backend_t * backend); 


int __flexiblas_load_cblas_function( void * handle , struct flexiblas_blasfn * fn, const char *name);  
int __flexiblas_load_fortran_function( void * handle , struct flexiblas_blasfn * fn, const char *name); 

void __flexiblas_backend_init( flexiblas_backend_t * backend); 



double flexiblas_wtime();


#define LOAD_CBLAS(handle,part,name) do { \
    __flexiblas_load_cblas_function((handle)->library_handle, &(handle ->part),  #name); \
 } while(0); 

#define LOAD_FBLAS(handle,part,name) do { \
    if ( __flexiblas_load_fortran_function((handle)->library_handle, &(handle -> part),  #name ) > 0 ) { \
        *failed = *failed + 1; \
        fprintf(stderr, COLOR_RED PRINT_PREFIX "Can not load " #name "\n" COLOR_RESET);\
    } else { *loaded = *loaded +1; }\
} while(0);



#endif /* end of include guard: FLEXIBLAS_H */
