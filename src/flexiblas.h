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



#ifndef FLEXIBLAS_H
#define FLEXIBLAS_H

#define PRINT_PREFIX "flexiblas"

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
#ifndef FLEXIBLAS_INTEGER8
#define Int 	int
#else
#define Int 	int64_t
#endif
#endif

#ifndef blasint
#ifndef FLEXIBLAS_INTEGER8
#define blasint 	int
#else
#define blasint 	int64_t
#endif
#endif


#include "flexiblas_structures.h"
#include "flexiblas_backend.h"
#include "flexiblas_api.h"
#include "flexiblas_mgmt.h"
#include "paths.h"
#include "helper.h"

#ifndef CMAKE_INSTALL_FULL_SYSCONFDIR
#define CMAKE_INSTALL_FULL_SYSCONFDIR "/usr/local/etc"
#endif


#ifndef RTLD_DEEPBIND
#define RTLD_DEEPBIND 0
#endif

#define FLEXIBLAS_ENV_SO_EXTENSION 0x01
#define FLEXIBLAS_ENV_HOMEDIR      0x02
#define FLEXIBLAS_ENV_GLOBAL_RC    0x03
#define FLEXIBLAS_ENV_USER_RC 	   0x04
#define FLEXIBLAS_ENV_HOST_RC      0x05
#define FLEXIBLAS_ENV_ENV_RC  0x06
#define FLEXIBLAS_ENV_GLOBAL_RC_DIR 0x07

/*  Global Vars */
extern flexiblas_backend_t *current_backend ;
extern flexiblas_backend_t **loaded_backends;
extern size_t               nloaded_backends;
HIDDEN extern int __flexiblas_mgmt_init;
HIDDEN extern flexiblas_mgmt_t *__flexiblas_mgmt;


extern void __flexiblas_print_copyright(int prefix);
extern char * __flexiblas_getenv(int what);
extern int __flexiblas_file_exist(const char *path);
extern int __flexiblas_directory_exists(const char * path);
extern int __flexiblas_insert_fallback_blas(flexiblas_mgmt_t *conig);
extern int __flexiblas_str_endwith(const char * haystack, const char *needle );


extern void *__flexiblas_blas_fallback;
#ifdef FLEXIBLAS_LAPACK
extern void *__flexiblas_lapack_fallback;
#endif

int __flexiblas_load_cblas(flexiblas_backend_t *backend);
int __flexiblas_load_fblas(flexiblas_backend_t *backend, int *loaded, int* failed);
#ifdef FLEXIBLAS_LAPACK
HIDDEN int __flexiblas_load_flapack(flexiblas_backend_t *backend, int *loaded, int* failed);
HIDDEN int __flexiblas_load_flapack_fallback ( flexiblas_backend_t *handle, int *loaded, int *failed );
#endif

int __flexiblas_setup_xerbla(flexiblas_backend_t * backend);
#ifdef FLEXIBLAS_CBLAS
int __flexiblas_setup_cblas_xerbla(flexiblas_backend_t *backend);
#endif
#ifdef FLEXIBLAS_LAPACKE
int __flexiblas_load_lapacke( flexiblas_backend_t *backend );
#endif

void __flexiblas_load_set_num_threads(flexiblas_backend_t * backend);
void __flexiblas_load_get_num_threads(flexiblas_backend_t * backend);
void * __flexiblas_lookup_cblas_function(void * handle, ...);
void * __flexiblas_lookup_fortran_function(void * handle, ...);
flexiblas_complex_interface_t __flexiblas_get_complex_interface(void *handle);
flexiblas_interface_t __flexiblas_get_interface(void *handle);
int __flexiblas_get_f2c_float_return(void *handle);

void __flexiblas_backend_init( flexiblas_backend_t * backend);


flexiblas_mgmt_t * flexiblas_mgmt(void);

double flexiblas_wtime(void);


HIDDEN int __flexiblas_file_exist(const char *path);

#define LOAD_CBLAS(handle,part,name) do { \
    __flexiblas_load_cblas_function((handle)->library_handle, &(handle ->part),  #name); \
} while(0);

#define LOAD_FBLAS(handle,part,name) do { \
    if ( __flexiblas_load_fortran_function((handle)->library_handle, &(handle -> part),  #name ) > 0 ) { \
        if ( __flexiblas_load_fortran_function(__flexiblas_blas_fallback, &(handle -> part),  #name ) > 0 ) { \
            *failed = *failed + 1; \
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load " #name "\n");\
        } else { \
            if ( __flexiblas_verbose > 1 ) flexiblas_print_info("flexiblas", "Load %s from internal fallback BLAS.\n", #name); \
            *loaded = *loaded +1;  \
        } \
    } else { *loaded = *loaded +1; }\
} while(0);

#define LOAD_FBLAS2(handle,part1,part2,name1,name2) do { \
    int _h1 = 0, _h2 = 0;\
    if ( __flexiblas_load_fortran_function((handle)->library_handle, &(handle -> part1),  #name1 ) > 0 ) { \
        if (__flexiblas_load_fortran_function(__flexiblas_blas_fallback, &(handle -> part1),  #name1 ) > 0 ) _h1 = 1; \
    } \
    if ( __flexiblas_load_fortran_function((handle)->library_handle, &(handle -> part2),  #name2 ) > 0 ) { \
        if (__flexiblas_load_fortran_function(__flexiblas_blas_fallback, &(handle -> part2),  #name2 ) > 0 ) _h2 = 1; \
    } \
    if ( _h1 == 0  && _h2 == 0 ) {\
        *loaded = *loaded +1;  \
        if ( __flexiblas_verbose > 1 ) flexiblas_print_info("flexiblas", "Load %s and %s as separate functions.\n", #name1, #name2); \
    } else if (_h1 == 0 && _h2 == 1 ) {\
        *loaded = *loaded +1;  \
        memcpy(&(handle -> part2), &(handle -> part1), sizeof(struct flexiblas_blasfn)); \
        if ( __flexiblas_verbose > 1 ) flexiblas_print_info("flexiblas", "Load %s for both (%s, %s).\n", #name1, #name1, #name2); \
    } else if ( _h1 == 1 && _h2 == 0) {\
        memcpy(&(handle -> part1), &(handle -> part2), sizeof(struct flexiblas_blasfn)); \
        *loaded = *loaded +1;  \
        if ( __flexiblas_verbose > 1 ) flexiblas_print_info("flexiblas", "Load %s for both (%s, %s).\n", #name2, #name1, #name2); \
    } else {\
        *failed = *failed + 1; \
        flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load " #name1 " or " #name2 "\n");\
    } \
} while(0);

#define LOAD_FLAPACK(handle,part,name) do { \
    if ( __flexiblas_load_fortran_function((handle)->library_handle, &(handle -> part),  #name ) > 0 ) { \
        if ( __flexiblas_load_fortran_function(__flexiblas_lapack_fallback, &(handle -> part),  #name ) > 0 ) { \
            *failed = *failed + 1; \
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load " #name "\n");\
        } else { \
            if ( __flexiblas_verbose > 1 ) flexiblas_print_info("flexiblas","Load %s from internal fallback LAPACK.\n", #name); \
            *loaded = *loaded +1;  \
        } \
    } else { *loaded = *loaded +1; }\
} while(0);

#define LOAD_FLAPACK_NOFALLBACK(handle,part,name) do { \
    if ( __flexiblas_load_fortran_function(__flexiblas_lapack_fallback, &(handle -> part),  #name ) > 0 ) { \
        *failed = *failed + 1; \
        flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load " #name " from fallback.\n");\
    } else { *loaded = *loaded +1; }\
} while(0);


/*
 * Hook Releated Functions
 */
#ifdef FLEXIBLAS_HOOK_API

extern flexiblas_hook_t *__flexiblas_hooks;
int __flexiblas_load_fortran_hook_function( void * handle , struct flexiblas_hook_fn *ptr, const char *name);
int __flexiblas_load_cblas_hook_function( void * handle , struct flexiblas_hook_fn *ptr, const char *name);
#define LOAD_HOOK(backend, handle,part,name) do { \
    if ( __flexiblas_load_fortran_hook_function(handle, &(backend ->part),  #name ) > 0 ) { \
        DPRINTF_WARN(3,"Cannot load hook for" #name "\n");\
    } \
} while(0);

#define LOAD_CHOOK(backend, handle,part,name) do { \
    if ( __flexiblas_load_cblas_hook_function(handle, &(backend ->part),  #name ) > 0 ) { \
        DPRINTF_WARN(3,"Cannot load hook for" #name "\n");\
    } \
} while(0);


int __flexiblas_load_blas_hooks(flexiblas_hook_t *backend, void *hook_handle);
int __flexiblas_load_cblas_hooks(flexiblas_hook_t *backend, void *hook_handle);
int __flexiblas_load_lapack_hooks(flexiblas_hook_t *backend, void *hook_handle);

#endif

// Macro to String
#define MTSA(a) #a
#define MTS(a) MTSA(a)


#endif /* end of include guard: FLEXIBLAS_H */

