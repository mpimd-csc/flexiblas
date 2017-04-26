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
 * Copyright (C) Martin Koehler, 2013-2016
 */

#ifdef __linux__ 
	#define __GNU_SOURCE
	#define _GNU_SOURCE
#endif
#include "flexiblas.h"
#include <errno.h>
#include <stddef.h>
#include <string.h>
#include <strings.h>
#include "cscutils/strutils.h"

#define DLOPEN_FLAGS_FROM_FILE -1 
#ifndef __WIN32__
#include <dlfcn.h>
#ifdef __linux__ 
// Linux 
// #define DLOPEN_FLAGS (RTLD_LAZY)
#define DLOPEN_FLAGS RTLD_LOCAL|RTLD_NOW
#else 
// BSD 
#define DLOPEN_FLAGS (RTLD_LOCAL|RTLD_NOW)
#endif
#else 
// Windows 
#define DLOPEN_FLAGS (0) 
#define strtok_r strtok_s 
#include <windows.h> 
#endif 

#ifdef INTEGER8
    #define     ENV_FLEXIBLAS "FLEXIBLAS64"
    #define     ENV_FLEXIBLAS_PROFILE "FLEXIBLAS64_PROFILE"
    #define     ENV_FLEXIBLAS_VERBOSE "FLEXIBLAS64_VERBOSE" 
    #define     ENV_FLEXIBLAS_PROFILE_FILE "FLEXIBLAS64_PROFILE_FILE"
#else 
    #define     ENV_FLEXIBLAS "FLEXIBLAS"
    #define     ENV_FLEXIBLAS_PROFILE "FLEXIBLAS_PROFILE"
    #define     ENV_FLEXIBLAS_VERBOSE "FLEXIBLAS_VERBOSE" 
    #define     ENV_FLEXIBLAS_PROFILE_FILE "FLEXIBLAS_PROFILE_FILE"
#endif

/*  Initialize global variables. */
HIDDEN int __flexiblas_initialized = 0; 
HIDDEN int __flexiblas_profile = 0;
HIDDEN char **  __flexiblas_additional_paths = NULL;
HIDDEN int __flexiblas_count_additional_paths = 0; 
HIDDEN char *__flexiblas_profile_file = NULL;
HIDDEN flexiblas_backend_t *current_backend = NULL;
HIDDEN flexiblas_backend_t **loaded_backends = NULL;
HIDDEN size_t                  nloaded_backends = 0;
HIDDEN flexiblas_mgmt_t *__flexiblas_mgmt = NULL;

#ifdef FLEXIBLAS_LAPACK 
HIDDEN void *__flexiblas_lapack_fallback = NULL; 
#endif 

// #include "lapack_interface/syms-3.6.1.c" 
/*-----------------------------------------------------------------------------
 *  Convert Chars to Upper Case
 *-----------------------------------------------------------------------------*/
static char *uppercase(char *str) {
	char *ret = str; 
	if ( str == NULL ) return NULL; 
	while (*str != '\0') {
		*str = (char)toupper(*str); 
		str++; 		
	}
	return ret; 
}



/*-----------------------------------------------------------------------------
 *  Default Info Structure if none is given 
 *-----------------------------------------------------------------------------*/
static void h_info_default(flexiblas_info_t *info) {
	info->flexiblas_integer_size = sizeof(Int); 
	info->backend_integer_size = 0; 
	info->intel_interface = 0 ; 
	info->post_init = 0; 
}



/*-----------------------------------------------------------------------------
 *  Init default search path 
 *-----------------------------------------------------------------------------*/
static void init_default_search_path() {
	char *searchpath = strdup(FLEXIBLAS_DEFAULT_LIB_PATH);
	char *path;
	char *r = NULL;

	path = strtok_r(searchpath,":", &r);
	while ( path != NULL ) {
		__flexiblas_add_path(path);	
		path = strtok_r(NULL, ":",&r);
	}
    free(searchpath); 
}


/*-----------------------------------------------------------------------------
 *  Init the backend. 
 *-----------------------------------------------------------------------------*/
HIDDEN void __flexiblas_backend_init( flexiblas_backend_t * backend) {
	int load = 0; 
	int failed = 0; 

	if (backend == NULL) {
		DPRINTF(0, PRINT_PREFIX " No current BLAS is set.\n");
		abort();
	}
#ifndef __WIN32__ 
	pthread_mutex_lock(&(backend->post_init_mutex)); 
	if ( backend->post_init != 0 ) {
#endif
		if (backend->init_function != NULL) {
			if ( backend->init_function() != 0 ) {
				DPRINTF(0, PRINT_PREFIX " Initialization of the backend library \"%s\" failed. \n", backend->name);
				abort();
			}
		}
	

		/*-----------------------------------------------------------------------------
		 *  Load FBLAS
		 *-----------------------------------------------------------------------------*/
		__flexiblas_load_fblas(backend, &load, &failed); 

		/*-----------------------------------------------------------------------------
		 *  Load CBLAS 
		 *-----------------------------------------------------------------------------*/
		__flexiblas_load_cblas(backend);
#ifdef FLEXIBLAS_LAPACK

		/*-----------------------------------------------------------------------------
		 *  Load LAPACK 
		 *-----------------------------------------------------------------------------*/
        	__flexiblas_load_flapack(backend, &load, &failed); 
        /* {
            int i = 0; 
            while (symbols_to_look_lapack[i] != NULL) {
                dlsym(RTLD_DEFAULT, symbols_to_look_lapack[i]); 

                i++; 
            }

        } */

#endif 
        /* Load extblas. the function does nothing if extblas is not enabled. */ 
		__flexiblas_load_extblas(backend, &failed); 

		/* Setup XERBLA */
		__flexiblas_setup_xerbla(backend); 
        backend->post_init = 0;
#ifndef __WIN32__ 
	} 
	pthread_mutex_unlock(&(backend->post_init_mutex)); 
#endif 
	if ( failed > 0) {
		fprintf(stderr, COLOR_RED PRINT_PREFIX " Failed to load the backend completely, some BLAS functions are missing. Abort!\n" COLOR_RESET);
		abort(); 
	}
}



/*-----------------------------------------------------------------------------
 *  Load the Info section from the Backend 
 *-----------------------------------------------------------------------------*/
static void flexiblas_load_info(void *library, flexiblas_backend_t *backend) 
{
	memset(&(backend->info),0,sizeof(flexiblas_info_t));
	backend->info.flexiblas_integer_size = sizeof(Int); 
#ifdef __WIN32__ 
	backend->info_function = (flexiblas_info_function_t) GetProcAddress(library, FLEXIBLAS_INFO_FUNCTION_NAME ); 
	backend->init_function = (flexiblas_init_function_t) GetProcAddress(library, FLEXIBLAS_INIT_FUNCTION_NAME); 
	backend->exit_function = (flexiblas_exit_function_t) GetProcAddress(library, FLEXIBLAS_EXIT_FUNCTION_NAME); 
#else 
	backend->info_function = (flexiblas_info_function_t) dlsym(library, FLEXIBLAS_INFO_FUNCTION_NAME); 
	backend->init_function = (flexiblas_init_function_t) dlsym(library, FLEXIBLAS_INIT_FUNCTION_NAME); 
	backend->exit_function = (flexiblas_exit_function_t) dlsym(library, FLEXIBLAS_EXIT_FUNCTION_NAME); 
#endif 

	backend->library_handle = library; 

	/* Load the Environment information function   */
	__flexiblas_load_set_num_threads(backend);
	__flexiblas_load_get_num_threads(backend);
	if ( backend->info_function ) {
		backend->info_function(&(backend->info)); 
	} else {
		DPRINTF(1,"No BLAS Info found in given backend. Using default.\n");
		h_info_default(&(backend->info));
	}
}

/*-----------------------------------------------------------------------------
 *  Print the Basic BLAS info
 *-----------------------------------------------------------------------------*/
static void flexiblas_print_info(flexiblas_backend_t *backend) 
{
	DPRINTF(1,"BLAS info:\n"); 
	DPRINTF(1," - intel_interface        = %d\n",backend->info.intel_interface); 
	DPRINTF(1," - flexiblas_integer_size = %d\n",backend->info.flexiblas_integer_size); 
	DPRINTF(1," - backend_integer_size   = %d\n",backend->info.backend_integer_size); 
	DPRINTF(1," - post_init              = %d\n",backend->info.post_init); 
}

static flexiblas_backend_t * flexiblas_load_library_from_init (flexiblas_mgmt_t *config, char *blas_default_map ) {
	char *env_FLEXIBLAS = NULL; 
	flexiblas_backend_t *backend = NULL;
	void *library = NULL;
	char name[FLEXIBLAS_MGMT_MAX_BUFFER_LEN];
	char blas_name[FLEXIBLAS_MGMT_MAX_BUFFER_LEN];

	if ( getenv(ENV_FLEXIBLAS) == NULL) {
		env_FLEXIBLAS = NULL;
	} else {
		env_FLEXIBLAS = strdup(getenv(ENV_FLEXIBLAS));
	}
	/*-----------------------------------------------------------------------------
	 *  Analyze the FLEXIBLAS environment variable 
	 *-----------------------------------------------------------------------------*/
	if (env_FLEXIBLAS== NULL) {
            char clibrary[FLEXIBLAS_MGMT_MAX_BUFFER_LEN];
        	flexiblas_mgmt_location_t loc;

		if ( flexiblas_mgmt_get_active_default(config, &loc, blas_name)) {
			DPRINTF(0,PRINT_PREFIX COLOR_RED "Failed to get the default backend. Reset to FALLBACK.\n" COLOR_RESET);
			strncpy(blas_name, "__FALLBACK__", FLEXIBLAS_MGMT_MAX_BUFFER_LEN);
        	}
        
		if ( flexiblas_mgmt_blas_get2(config, &loc, blas_name, clibrary, NULL)) {
			DPRINTF(0, PRINT_PREFIX COLOR_RED "Failed to get the BLAS backend (%s) from the configuration.\n" COLOR_RESET, blas_name);
			abort();
        	}

		DPRINTF(1,"Use default BLAS: %s - %s from %s\n", blas_name, clibrary, flexiblas_mgmt_location_to_string(loc) );
		library = __flexiblas_dlopen(clibrary, DLOPEN_FLAGS_FROM_FILE , NULL);
        	strncpy(name, blas_name, FLEXIBLAS_MGMT_MAX_BUFFER_LEN);

	} else {
		/*-----------------------------------------------------------------------------
		 *  Try to open env_FLEXIBLAS directly and the get the value from the Hashtable 
		 *-----------------------------------------------------------------------------*/
		DPRINTF(1,"Trying to use the content of " ENV_FLEXIBLAS ": \"%s\" as shared library.\n", env_FLEXIBLAS);
		library = __flexiblas_dlopen(env_FLEXIBLAS, DLOPEN_FLAGS_FROM_FILE, NULL);  
        	strncpy(name, env_FLEXIBLAS, FLEXIBLAS_MGMT_MAX_BUFFER_LEN);

        	/*  if env_FLEXIBLAS does not contain an .so file we look into the configuration  */
		if ( library == NULL) {
			char *clibrary = NULL;
			flexiblas_mgmt_location_t loc;
			char *tmp = strdup(env_FLEXIBLAS); 
			tmp = uppercase(tmp); 
			DPRINTF(1,"\"%s\" does not seem to a shared library. Search inside the FlexiBLAS configuration..\n", tmp);

            clibrary = (char*) malloc(sizeof(char)*32768); 
			if ( flexiblas_mgmt_blas_get2(config, &loc, tmp, clibrary, NULL)) {
                free(clibrary); 
				clibrary = NULL; 
			}

			/* Load the default BLAS if the env_FLEXIBLAS implementation was not found in the configuration */
			if (clibrary == NULL ) {
				if ( flexiblas_mgmt_get_active_default(config, &loc, blas_name)) {
					DPRINTF(0,PRINT_PREFIX COLOR_RED "Failed to get the default backend. Reset to FALLBACK.\n" COLOR_RESET);
					strncpy(blas_name, "__FALLBACK__", FLEXIBLAS_MGMT_MAX_BUFFER_LEN);
				}

				fprintf(stderr, COLOR_RED PRINT_PREFIX "BLAS backend  \"%s\" not found. Loading default (%s) instead.\n" COLOR_RESET, tmp, blas_name);

                clibrary = (char*) malloc(sizeof(char)*32768); 
				if ( flexiblas_mgmt_blas_get2(config, &loc, blas_name, clibrary, NULL)) {
					DPRINTF(0, PRINT_PREFIX COLOR_RED "Failed to get the BLAS backend (%s) from the configuration.\n" COLOR_RESET, blas_name);
					abort();
				}

				strncpy(name, blas_name, FLEXIBLAS_MGMT_MAX_BUFFER_LEN);
				free(tmp);
				tmp = strdup(blas_name);
			}

			DPRINTF(1,"Trying to load  %s\n", clibrary );
			library  = __flexiblas_dlopen(clibrary, DLOPEN_FLAGS_FROM_FILE , NULL);  
            if ( clibrary != NULL) free(clibrary); 
			free(tmp); 
		}
	}

	/* Load FallBack if non of the previously opened libraries worked. */
	if ( library == NULL ) {
		char *clibrary = NULL; 
		flexiblas_mgmt_location_t loc;
		DPRINTF(0, "No suitable BLAS backend could be loaded. Trying Fallback instead.\n");
        clibrary = (char* ) malloc(sizeof(char) * 32768); 
		if ( flexiblas_mgmt_blas_get2(config, &loc, "__FALLBACK__", clibrary, NULL)) {
			DPRINTF(0, PRINT_PREFIX COLOR_RED "Failed to get the BLAS backend (__FALLBACK__) from the configuration.\n" COLOR_RESET);
			library = NULL;
		} else {
			library = __flexiblas_dlopen(clibrary,DLOPEN_FLAGS_FROM_FILE , NULL);  
		}
        free(clibrary); 
		strncpy(name, "__FALLBACK__", FLEXIBLAS_MGMT_MAX_BUFFER_LEN);
	}

	if ( library == NULL ) {
		fprintf(stderr, PRINT_PREFIX "Unable to open any BLAS library (choosen: %s). Abort!\n",
		(env_FLEXIBLAS == NULL)?blas_default_map:env_FLEXIBLAS); 
		abort();
		return NULL;
	}

	backend = (flexiblas_backend_t*) malloc(sizeof(flexiblas_backend_t));
	if ( backend == NULL ) {
		DPRINTF(0, " Failed to allocate space for backend structure.\n");
		return NULL;
	}

	memset((void*) backend, 0, sizeof(flexiblas_backend_t));
	pthread_mutex_init(&(backend->post_init_mutex),NULL);

	backend->library_handle = library;
	backend->name = strdup(name);
    uppercase(backend->name); 

	if ( env_FLEXIBLAS != NULL) {
		free(env_FLEXIBLAS);
	}

	/* load info */
	flexiblas_load_info(library, backend); 
	
	/*-----------------------------------------------------------------------------
	 *  Get the integer size of the backend if not already set 
	 *-----------------------------------------------------------------------------*/
	if ( backend->info.backend_integer_size == 0 ) {
#ifdef FLEXIBLAS_INTEGER8
		backend->info.backend_integer_size = sizeof(int64_t);
#else
		backend->info.backend_integer_size = sizeof(int32_t);
#endif
	}

	backend->post_init =  backend->info.post_init; 

	if ( backend->post_init == 0 ) {
		backend->post_init = 1; 
		__flexiblas_backend_init(backend); 
	} else {
		DPRINTF(0, "BLAS backend uses post initialization.\n"); 
	}

	flexiblas_print_info(backend); 
	return backend; 
}


/*-----------------------------------------------------------------------------
 *  Load additional BLAS
 *-----------------------------------------------------------------------------*/
static flexiblas_backend_t * __flexiblas_load_backend_from_config(const char *blas_name) 
{
    flexiblas_backend_t *backend = NULL;
    void *library = NULL;
	char clibrary[FLEXIBLAS_MGMT_MAX_BUFFER_LEN]; 
    flexiblas_mgmt_location_t loc;

	if ( flexiblas_mgmt_blas_get2(__flexiblas_mgmt, &loc, blas_name, clibrary, NULL) != 0) {
        DPRINTF(0, COLOR_RED "BLAS %s not found in config.\n" COLOR_RESET, blas_name);
        return NULL;
	} 

    DPRINTF(2, " Try to load %s - %s\n", blas_name, clibrary);
    library = __flexiblas_dlopen(clibrary, DLOPEN_FLAGS_FROM_FILE, (char **) NULL);

    if ( library == NULL ) {
        DPRINTF(2, " failed.\n");
        return NULL;
    }

    backend = (flexiblas_backend_t*) malloc(sizeof(flexiblas_backend_t));
    if ( backend == NULL ){
        DPRINTF(0, " Failed to allocate space for backend structure.\n");
        return NULL;
    }
    memset((void*) backend, 0, sizeof(flexiblas_backend_t));
    pthread_mutex_init(&(backend->post_init_mutex),NULL);

    backend->library_handle = library;
    backend->name = strdup(blas_name);

	/* load info */
    flexiblas_load_info(library, backend); 

	
	/*-----------------------------------------------------------------------------
	 *  Get the integer size of the backend if not already set 
	 *-----------------------------------------------------------------------------*/
	if ( backend->info.backend_integer_size == 0 ) {
#ifdef FLEXIBLAS_INTEGER8
			backend->info.backend_integer_size = sizeof(int64_t);
#else 
			backend->info.backend_integer_size = sizeof(int32_t);
#endif
	}
    flexiblas_print_info(backend); 

    backend->post_init =  backend->info.post_init; 

	if ( backend->post_init == 0 ) {
		backend->post_init = 1; 
		__flexiblas_backend_init(backend); 
	} else {
		DPRINTF(0, "BLAS backend %s uses post initialization.\n", blas_name); 
	}
	return backend; 
}

/*-----------------------------------------------------------------------------
 *  Load BLAS by name from config (API VERSION) 
 *-----------------------------------------------------------------------------*/
int flexiblas_load_backend(const char *name ) 
{
    flexiblas_backend_t * new_backend = NULL; 
    size_t i; 

    for (i = 0; i < nloaded_backends; i++) {
        if ( csc_strcasecmp(name , loaded_backends[i]->name) == 0) {
            /* Already loaded */
            return (int)i;
        }
    }

    DPRINTF(1,"Backend %s not loaded until now. - %d \n", name, -1);
    /* Not loaded */ 
    new_backend = __flexiblas_load_backend_from_config(name);			/* FIXME does accept const char* (yet) */
    if ( new_backend != NULL ) {
        size_t new_backend_pos = nloaded_backends;
        nloaded_backends++;
        loaded_backends = realloc(loaded_backends, sizeof(flexiblas_backend_t) * nloaded_backends);
        loaded_backends[new_backend_pos] = new_backend;
        // csc_ini_setinteger((csc_ini_file_t*) __flexiblas_mgmt->host_config, name, "__loaded__", new_backend_pos);
        return (int)new_backend_pos;
    }

    /* Fails to load */
    return -1;
}



static flexiblas_backend_t * __flexiblas_load_backend_from_file(const char *blas_sofile) 
{
    flexiblas_backend_t *backend = NULL;
    void *library = NULL;

    DPRINTF(2, PRINT_PREFIX " Try to load %s \n", blas_sofile);
    library = __flexiblas_dlopen(blas_sofile, DLOPEN_FLAGS_FROM_FILE, (char **) NULL);

    if ( library == NULL ) {
        DPRINTF(2, PRINT_PREFIX " failed.\n");
        return NULL;
    }

    backend = (flexiblas_backend_t*) malloc(sizeof(flexiblas_backend_t));
    if ( backend == NULL ){
        DPRINTF(0, " Failed to allocate space for backend structure.\n");
        return NULL;
    }
    memset((void*) backend, 0, sizeof(flexiblas_backend_t));
    pthread_mutex_init(&(backend->post_init_mutex),NULL);

    backend->library_handle = library;
    backend->name = strdup(blas_sofile);

	/* load info */
    flexiblas_load_info(library, backend); 

	/*-----------------------------------------------------------------------------
	 *  Get the integer size of the backend if not already set 
	 *-----------------------------------------------------------------------------*/
	if ( backend->info.backend_integer_size == 0 ) {
        DPRINTF(0, "BLAS %s does not provide an integer size hint. Assuming 4 Byte.\n", blas_sofile);
		backend->info.backend_integer_size = sizeof(int32_t);
	}
    backend->post_init =  backend->info.post_init; 

	if ( backend->post_init == 0 ) {
		backend->post_init = 1; 
		__flexiblas_backend_init(backend); 
	} else {
		DPRINTF(0, "BLAS backend %s uses post initialization.\n", blas_sofile); 
	}

    flexiblas_print_info(backend); 
	return backend; 
}

/*-----------------------------------------------------------------------------
 * Load backend from FILE (API VERSION) 
 *-----------------------------------------------------------------------------*/
int flexiblas_load_backend_library(const char *libname)
{
	flexiblas_backend_t * new_backend = NULL; 
    size_t i; 

	for (i = 0; i < nloaded_backends; i++) {
		if ( csc_strcasecmp(libname , loaded_backends[i]->name) == 0) {
			/* Already loaded */
			return i;
		}
	}

	DPRINTF(1,"Backend %s not loaded until now. - %d \n", libname, -1);
	/* Not loaded  */ 
	new_backend = __flexiblas_load_backend_from_file(libname);
	if ( new_backend != NULL ) {  
		size_t new_backend_pos = nloaded_backends;
		nloaded_backends++; 
		loaded_backends = realloc(loaded_backends, sizeof(flexiblas_backend_t) * nloaded_backends);
		loaded_backends[new_backend_pos] = new_backend; 
		// csc_ini_setinteger((csc_ini_file_t*) __flexiblas_mgmt->host_config, libname, "__loaded__", new_backend_pos); 
		return new_backend_pos; 
	} 

	/* Fails to load */
	return -1;
}


/*-----------------------------------------------------------------------------
 *  Init Routine 
 *-----------------------------------------------------------------------------*/
#ifndef __WIN32__ 
__attribute__((constructor))
#endif 
void flexiblas_init() {
	char blas_default_map[FLEXIBLAS_MGMT_MAX_BUFFER_LEN] ; 
    char path[FLEXIBLAS_MGMT_MAX_BUFFER_LEN];
    flexiblas_backend_t  *backend = NULL;
    flexiblas_mgmt_location_t loc;
    void *iter_helper = NULL;
	/*-----------------------------------------------------------------------------
	 *  Read Environment Variables 
	 *-----------------------------------------------------------------------------*/
	char *env_FLEXIBLAS_VERBOSE=getenv(ENV_FLEXIBLAS_VERBOSE); 
	char *env_FLEXIBLAS_PROFILE=getenv(ENV_FLEXIBLAS_PROFILE);

	if ( __flexiblas_initialized != 0) return; 
	__flexiblas_initialized = 1; 

     memset(path, '\0', FLEXIBLAS_MGMT_MAX_BUFFER_LEN); 
	/*-----------------------------------------------------------------------------
	 *  Read mapping file
	 *  1. /etc/flexiblasrc  or its counterpart in the build directory
	 *  3. $HOME/.flexiblasrc 
	 *-----------------------------------------------------------------------------*/
    __flexiblas_mgmt = flexiblas_mgmt_load_config();
    if ( __flexiblas_mgmt == NULL) {
        DPRINTF(0, PRINT_PREFIX COLOR_RED "Cannot initialize/load the configuration.\n" COLOR_RESET);
        abort();
    }
       
    if (__flexiblas_insert_fallback_blas(__flexiblas_mgmt)) {
        DPRINTF(0, PRINT_PREFIX COLOR_RED "Failed to initialize the default and the fallback BLAS backend.\n" COLOR_RESET);
        abort();
    }

    /*  System Paths */
    iter_helper = NULL;
    while ( flexiblas_mgmt_list_paths(__flexiblas_mgmt, FLEXIBLAS_GLOBAL, path, &iter_helper) > 0)
    {
	    if ( strlen(path) > 0 ) __flexiblas_add_path(path);
    }
    /*  User Paths */
    iter_helper = NULL;
    while ( flexiblas_mgmt_list_paths(__flexiblas_mgmt, FLEXIBLAS_USER, path, &iter_helper) > 0)
    {
	    if ( strlen(path) > 0 ) __flexiblas_add_path(path);
    }
    /*  Host Paths */
    iter_helper = NULL;
    while ( flexiblas_mgmt_list_paths(__flexiblas_mgmt, FLEXIBLAS_HOST, path, &iter_helper) > 0)
    {
	    if ( strlen(path) > 0 ) __flexiblas_add_path(path);
    }

    /* Load Properties */
    flexiblas_mgmt_get_active_property(__flexiblas_mgmt, &loc, FLEXIBLAS_PROP_VERBOSE, &__flexiblas_verbose);
    flexiblas_mgmt_get_active_property(__flexiblas_mgmt, &loc, FLEXIBLAS_PROP_PROFILE, &__flexiblas_profile);

	/* Load environemt variables   */
	if ( env_FLEXIBLAS_VERBOSE != NULL ) {
		__flexiblas_verbose = atoi(env_FLEXIBLAS_VERBOSE); 
	} 
	if (env_FLEXIBLAS_PROFILE != NULL) {
		if (atoi(env_FLEXIBLAS_PROFILE) > 0 ) {
			__flexiblas_profile = 1;
		}		
	} 

	init_default_search_path();
    
    /*  Load the active default BLAS */
    if ( flexiblas_mgmt_get_active_default(__flexiblas_mgmt, &loc, blas_default_map) < 0 ) {
        DPRINTF(0, PRINT_PREFIX COLOR_RED "Failed to select the default BLAS backend.\n" COLOR_RESET);
        abort();
    }
	
	/*-----------------------------------------------------------------------------
	 *  Display Copyright 
	 *-----------------------------------------------------------------------------*/
	if ( __flexiblas_verbose) {
		__flexiblas_print_copyright(1); 
	}

	/*-----------------------------------------------------------------------------
	 *  Load Library 
	 *-----------------------------------------------------------------------------*/
	uppercase(blas_default_map); 
#ifdef FLEXIBLAS_LAPACK
	/*-----------------------------------------------------------------------------
	 *  Load LAPACK
	 *-----------------------------------------------------------------------------*/
	{
		char *SO_EXTENSION = __flexiblas_getenv(FLEXIBLAS_ENV_SO_EXTENSION);
		size_t len=strlen(LAPACK_FALLBACK_NAME)+strlen(SO_EXTENSION)+2;
		char *lapack_name = (char *) calloc(len,sizeof(char));
		snprintf(lapack_name,len, "%s%s", LAPACK_FALLBACK_NAME,SO_EXTENSION);

/* #if ( defined(__powerpc__) || defined(__powerpc64__)) && !defined(__IBMC__) 
		__flexiblas_lapack_fallback = __flexiblas_dlopen(lapack_name, RTLD_LAZY | RTLD_LOCAL | RTLD_DEEPBIND , NULL); 
#else  */
		__flexiblas_lapack_fallback = __flexiblas_dlopen(lapack_name, RTLD_LAZY | RTLD_GLOBAL , NULL); 
// #endif 
		if ( __flexiblas_lapack_fallback == NULL ) {
			fprintf(stderr, COLOR_RED PRINT_PREFIX " Failed to load the LAPACK fallback library.  Abort!\n" COLOR_RESET);
			abort(); 
		}
		free(lapack_name); 
	} 
#endif 

    backend = flexiblas_load_library_from_init(__flexiblas_mgmt, blas_default_map); 
    if ( backend == NULL ){
        DPRINTF(0,PRINT_PREFIX COLOR_RED "Loading Backend Failed.\n" COLOR_RESET);
        abort();
    }
    loaded_backends = (flexiblas_backend_t **) malloc(sizeof(flexiblas_backend_t*) * 1);
    /* Set the loaded backend as default one.  */
    nloaded_backends = 1;
    loaded_backends[0] = backend;
    current_backend  = backend;

	if ( __flexiblas_profile ) {
		if (getenv(ENV_FLEXIBLAS_PROFILE_FILE) != NULL) {
			__flexiblas_profile_file = strdup(getenv(ENV_FLEXIBLAS_PROFILE_FILE));
		} else {
			char tmp[FLEXIBLAS_MGMT_MAX_BUFFER_LEN];
            flexiblas_mgmt_get_active_property(__flexiblas_mgmt, &loc, FLEXIBLAS_PROP_PROFILE_FILE, tmp);
            __flexiblas_profile_file = strdup(tmp);
		}
	}
}



/*-----------------------------------------------------------------------------
 *  Cleanup 
 *-----------------------------------------------------------------------------*/
#ifndef __WIN32__ 
__attribute__((destructor)) 
#endif
void flexiblas_exit() {
    size_t i;
    if ( __flexiblas_profile > 0 ) {
        flexiblas_print_profile(); 
    } 
	if (__flexiblas_verbose ) DPRINTF(1,"cleanup\n"); 
    for (i = 0; i < nloaded_backends ; i++) {
        if ( loaded_backends[i]->exit_function != NULL) {
            loaded_backends[i]->exit_function();
        }
        free(loaded_backends[i]->name);
        if ( loaded_backends[i]->library_handle != NULL){
#ifdef __WIN32__ 
    		FreeLibrary(loaded_backends[i]->library_handle); 
#else
	    	dlclose(loaded_backends[i]->library_handle ); 
#endif
        }
        free(loaded_backends[i]);
    }
	free(loaded_backends);
#ifdef FLEXIBLAS_LAPACK
    dlclose(__flexiblas_lapack_fallback); 
#endif 
    nloaded_backends = 0;
	__flexiblas_free_paths();
    flexiblas_mgmt_free_config(__flexiblas_mgmt);
}





double flexiblas_wtime()
{
	struct timeval tv;
	gettimeofday (&tv, NULL);
	return (double)tv.tv_sec + (double)tv.tv_usec / 1e6;
}

#ifdef __WIN32__ 
#include <windows.h>
BOOL WINAPI DllMain(HINSTANCE hinstDLL, DWORD fdwReason, LPVOID lpvReserved)
{
  switch (fdwReason)
  {
    case DLL_PROCESS_ATTACH:
      /* Code path executed when DLL is loaded into a process's address space. */
      flexiblas_init(); 
      break;

    case DLL_THREAD_ATTACH:
      /* Code path executed when a new thread is created within the process. */
      break;

    case DLL_THREAD_DETACH:
      /* Code path executed when a thread within the process has exited *cleanly*. */
      break;

    case DLL_PROCESS_DETACH:
      /* Code path executed when DLL is unloaded from a process's address space. */
      flexiblas_exit(); 
      break;
  }

  return TRUE;
}
#endif
