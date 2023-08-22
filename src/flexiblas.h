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
#ifndef INTEGER8
#define Int 	int
#else
#define Int 	int64_t
#endif
#endif

#include "cscutils/inifile.h"
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
extern flexiblas_hook_t *__flexiblas_hooks;
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
void __flexiblas_load_set_num_threads(flexiblas_backend_t * backend);
void __flexiblas_load_get_num_threads(flexiblas_backend_t * backend);
int __flexiblas_load_cblas_function( void * handle , struct flexiblas_blasfn * fn, const char *name);
int __flexiblas_load_fortran_function( void * handle , struct flexiblas_blasfn * fn, const char *name);
int __flexiblas_load_blas_hooks(flexiblas_hook_t *backend, void *hook_handle);

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


// Macro to String
#define MTSA(a) #a
#define MTS(a) MTSA(a)


#endif /* end of include guard: FLEXIBLAS_H */

