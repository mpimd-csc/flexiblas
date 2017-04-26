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
 * Copyright (C) Martin Koehler, 2013-2015
 */

#include "flexiblas.h"
#include <errno.h>

#ifndef __WIN32__
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
csc_ini_file_t __flexiblas_config; 
int __flexiblas_initialized = 0; 
int __flexiblas_profile = 0;
char **  __flexiblas_additional_paths = NULL;
int __flexiblas_count_additional_paths = 0; 
char *__flexiblas_profile_file = NULL;

flexiblas_backend_t *current_backend = NULL;
flexiblas_backend_t **loaded_backends = NULL;
int                  nloaded_backends = 0;


/*-----------------------------------------------------------------------------
 *  Convert Chars to Upper Case
 *-----------------------------------------------------------------------------*/
static char *uppercase(char *str) {
	char *ret = str; 
	if ( str == NULL ) return NULL; 
	while (*str != '\0') {
		*str = toupper(*str); 
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
	char *r;

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
void __flexiblas_backend_init( flexiblas_backend_t * backend) {
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

        /* Load extblas. the function does nothing if extblas is not enabled. */ 
		__flexiblas_load_extblas(backend, &load, &failed); 

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

static flexiblas_backend_t * flexiblas_load_library_from_init (csc_ini_file_t *config, char *blas_default_map ) {
	char *env_FLEXIBLAS = NULL; 
	int is_64_bit = 0;
	int get_64 = 0;
    flexiblas_backend_t *backend = NULL;
    void *library = NULL;
    char *name = NULL;

	if ( getenv(ENV_FLEXIBLAS) == NULL){
		env_FLEXIBLAS = NULL;
	}  else {
		env_FLEXIBLAS = strdup(getenv(ENV_FLEXIBLAS));
	}
	/*-----------------------------------------------------------------------------
	 *  Analyze the FLEXIBLAS environment variable 
	 *-----------------------------------------------------------------------------*/
	if (env_FLEXIBLAS== NULL) {
		char *clibrary = NULL; 
		if ( csc_ini_getstring(config, blas_default_map, "library", &clibrary) != CSC_INI_SUCCESS) {
			DPRINTF(0, COLOR_RED "Default BLAS not found.\n" COLOR_RESET);
			abort(); 
		} 
		if ( csc_ini_getinteger(config, blas_default_map, "ilp64", &get_64) != CSC_INI_SUCCESS ){
			is_64_bit =  get_64;
		} else {
			is_64_bit =  0;
		}
		DPRINTF(1,"Use default BLAS: %s - %s\n", blas_default_map, clibrary );
		library = __flexiblas_dlopen(clibrary, DLOPEN_FLAGS, NULL);
        name = blas_default_map;
	} else {
		char *ilp64part;
		/*-----------------------------------------------------------------------------
		 *  Try to open env_FLEXIBLAS directly and the get the value from the Hashtable 
		 *-----------------------------------------------------------------------------*/
		/* Extract 64 if in */
		if ((ilp64part =strstr(env_FLEXIBLAS, ":")) != NULL) {
			*ilp64part = '\0';
			ilp64part ++;
			DPRINTF(1,ENV_FLEXIBLAS " provide 64 hint: %s\n", ilp64part);
			if (strlen(ilp64part) > 0 && (strcasecmp(ilp64part,"ilp64") == 0
			    || strcasecmp(ilp64part, "64") == 0)) {
				is_64_bit = 1;
			} else {
				is_64_bit = 0;
			}
		}  else {
			is_64_bit = 0;
		}
		DPRINTF(1,"Trying to use the content of " ENV_FLEXIBLAS ": \"%s\" as shared library.\n", env_FLEXIBLAS);
		library = __flexiblas_dlopen(env_FLEXIBLAS, DLOPEN_FLAGS, NULL);  
        name = env_FLEXIBLAS;

        /*  if env_FLEXIBLAS does not contain an .so file we look into the configuration  */
		if ( library == NULL) {
			char *clibrary = NULL;
			char *tmp = strdup(env_FLEXIBLAS); 
			tmp = uppercase(tmp); 
			DPRINTF(1,"\"%s\" does not seem to a shared library. Search inside the FlexiBLAS configuration..\n", tmp);
			if ( csc_ini_getstring(config, tmp, "library", &clibrary) != CSC_INI_SUCCESS) {
				clibrary = NULL; 
			}

            /* Load the default BLAS if the env_FLEXIBLAS implementation was not found in the configuration */
			if (clibrary == NULL ) {
				fprintf(stderr, COLOR_RED PRINT_PREFIX "BLAS backend  \"%s\" not found. Loading default (%s) instead.\n" COLOR_RESET, tmp, blas_default_map);
				if ( csc_ini_getstring(config, blas_default_map, "library", &clibrary) != CSC_INI_SUCCESS) {
					fprintf(stderr, COLOR_RED PRINT_PREFIX "Default BLAS not found.\n" COLOR_RESET);
					abort(); 
				}				
                name = blas_default_map;
				free(tmp);
				tmp = strdup(blas_default_map);

			}
			if ( csc_ini_getinteger(config, tmp, "ilp64", &get_64) != CSC_INI_SUCCESS ){
				is_64_bit =  get_64;
			} else {
				is_64_bit =  0;
			}

			DPRINTF(1,"Trying to load  %s\n", clibrary );
			library  = __flexiblas_dlopen(clibrary, DLOPEN_FLAGS, NULL);  
			free(tmp); 
		}
	}

	/* Load FallBack if non of the previously opened libraries worked. */
	if ( library == NULL ) {
		char *clibrary = NULL; 
		DPRINTF(0, "No suitable BLAS backend could be loaded. Tring Fallback instead.\n");
		if ( csc_ini_getstring(config, "__FALLBACK__", "library", &clibrary) != CSC_INI_SUCCESS){
			library = NULL; 
		} else {  
			library = __flexiblas_dlopen(clibrary,DLOPEN_FLAGS, NULL);  
		}
		if ( csc_ini_getinteger(config, "__FALLBACK__", "ilp64", &get_64) != CSC_INI_SUCCESS ){
			is_64_bit =  get_64;
		} else {
			is_64_bit =  0;
		}
        name = "__FALLBACK__";
	}

    if ( library == NULL ) {
        fprintf(stderr, PRINT_PREFIX "Unable to open any BLAS library (choosen: %s). Abort!\n",
                (env_FLEXIBLAS == NULL)?blas_default_map:env_FLEXIBLAS); 
        abort();
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
    backend->name = strdup(name);

    if ( env_FLEXIBLAS != NULL) {
		free(env_FLEXIBLAS);
	}


	/* load info */
    flexiblas_load_info(library, backend); 
	
	/*-----------------------------------------------------------------------------
	 *  Get the integer size of the backend if not already set 
	 *-----------------------------------------------------------------------------*/
	if ( backend->info.backend_integer_size == 0 ) {
		if ( is_64_bit ) {
			backend->info.backend_integer_size = sizeof(int64_t);
		} else {
			backend->info.backend_integer_size = sizeof(int32_t);
		}
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
	char *clibrary = NULL; 
    int is_64_bit = 0, get_64 = 0;

	if ( csc_ini_getstring(&__flexiblas_config, blas_name, "library", &clibrary) != CSC_INI_SUCCESS) {
        DPRINTF(0, COLOR_RED "BLAS %s not found in config.\n" COLOR_RESET, blas_name);
        return NULL;
	} 

    if ( csc_ini_getinteger(&__flexiblas_config, blas_name, "ilp64", &get_64) != CSC_INI_SUCCESS ){
		is_64_bit =  get_64;
	} else {
		is_64_bit =  0;
	}
	
    DPRINTF(2, " Try to load %s - %s\n", blas_name, clibrary);
    library = __flexiblas_dlopen(clibrary, DLOPEN_FLAGS, (char **) NULL);

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
		if ( is_64_bit ) {
			backend->info.backend_integer_size = sizeof(int64_t);
		} else {
			backend->info.backend_integer_size = sizeof(int32_t);
		}
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
int flexiblas_load_backend(const char * name ) 
{
    flexiblas_backend_t * new_backend = NULL; 
    int new_num = 0; 
    char *n = strdup(name); 
    int loaded = -1; 

    n = uppercase(n); 
    if ( csc_ini_getinteger(&__flexiblas_config, n, "__loaded__", &loaded) != CSC_INI_SUCCESS) {
        DPRINTF(1,"Backend %s not loaded until now. - %d \n", n, loaded);
        /* Not loaded  */ 
        new_backend = __flexiblas_load_backend_from_config(n); 
        if ( new_backend == NULL ) { free(n); return -1; } 

        new_num = nloaded_backends; 
        nloaded_backends++; 
        loaded_backends = realloc(loaded_backends, sizeof(flexiblas_backend_t) * nloaded_backends); 
        loaded_backends[new_num] = new_backend; 
        csc_ini_setinteger(&__flexiblas_config, n, "__loaded__", new_num); 
        free(n); 
        return new_num; 
    } else {
        /* Already loaded   */
        free(n); 
        return loaded; 
    }
}



static flexiblas_backend_t * __flexiblas_load_backend_from_file(const char *blas_sofile) 
{
    flexiblas_backend_t *backend = NULL;
    void *library = NULL;

    DPRINTF(2, PRINT_PREFIX " Try to load %s \n", blas_sofile);
    library = __flexiblas_dlopen(blas_sofile, DLOPEN_FLAGS, (char **) NULL);

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
    int new_num = 0; 
    char *n = strdup(libname); 
    int loaded = -1; 

    n = uppercase(n); 
    if ( csc_ini_getinteger(&__flexiblas_config, n, "__loaded__", &loaded) != CSC_INI_SUCCESS) {
        DPRINTF(1,"Backend %s not loaded until now. - %d \n", n, loaded);
        /* Not loaded  */ 
        new_backend = __flexiblas_load_backend_from_file(libname); 
        if ( new_backend == NULL ) { free(n); return -1; } 

        new_num = nloaded_backends; 
        nloaded_backends++; 
        loaded_backends = realloc(loaded_backends, sizeof(flexiblas_backend_t) * nloaded_backends); 
        loaded_backends[new_num] = new_backend; 
        csc_ini_setinteger(&__flexiblas_config, n, "__loaded__", new_num); 
        free(n); 
        return new_num; 
    } else {
        /* Already loaded   */
        free(n); 
        return loaded; 
    }

}




/*-----------------------------------------------------------------------------
 *  Init Routine 
 *-----------------------------------------------------------------------------*/
#ifndef __WIN32__ 
__attribute__((constructor))
#endif 
void flexiblas_init() {
	char *env_FLEXIBLAS_PROFILE=getenv(ENV_FLEXIBLAS_PROFILE);
	char * blas_default_map= NULL ; 
    flexiblas_backend_t  *backend = NULL;
	/*-----------------------------------------------------------------------------
	 *  Read Environment Variables 
	 *-----------------------------------------------------------------------------*/
	char *env_FLEXIBLAS_VERBOSE=getenv(ENV_FLEXIBLAS_VERBOSE); 

	if ( __flexiblas_initialized != 0) return; 
	__flexiblas_initialized = 1; 

	/*-----------------------------------------------------------------------------
	 *  Read mapping file
	 *  1. /etc/flexiblasrc  or its counterpart in the build directory
	 *  3. $HOME/.flexiblasrc 
	 *-----------------------------------------------------------------------------*/
	if ( csc_ini_empty(&__flexiblas_config) != CSC_INI_SUCCESS) {
		DPRINTF(0, "Cannot initialize the configuration\n"); 
		abort(); 
	}
	__flexiblas_insert_fallback_blas(&__flexiblas_config);

	/* Load System config */
	{	
		char * system_config_file  = __flexiblas_getenv(FLEXIBLAS_ENV_GLOBAL_RC);
		__flexiblas_read_config_file(system_config_file,&__flexiblas_config, &blas_default_map); 
		free(system_config_file);
	}
	 /* Load User Config */
	{
		char * user_config_file = __flexiblas_getenv(FLEXIBLAS_ENV_USER_RC);
		__flexiblas_read_config_file(user_config_file, &__flexiblas_config, &blas_default_map);
		free(user_config_file);
	}

	/* Load environemt variables   */
	if ( env_FLEXIBLAS_VERBOSE != NULL ) {
		__flexiblas_verbose = atoi(env_FLEXIBLAS_VERBOSE); 
	} 
	if (env_FLEXIBLAS_PROFILE != NULL) {
		if (atoi(env_FLEXIBLAS_PROFILE) > 0 ) {
			__flexiblas_profile = 1;
		}		
	} 

	// dump_ini_data(&__flexiblas_config); 
	init_default_search_path();

	if (!blas_default_map) {
		blas_default_map = strdup("NETLIB"); 
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
	blas_default_map = uppercase(blas_default_map); 
	backend = flexiblas_load_library_from_init(&__flexiblas_config, blas_default_map); 
    if ( backend == NULL ){
        DPRINTF(0,PRINT_PREFIX "Loading Backend Failed.\n");
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
			char * tmp = NULL;
			if ( csc_ini_getstring(&__flexiblas_config, CSC_INI_DEFAULT_SECTION, "profile_file", &tmp) != CSC_INI_SUCCESS) {
			__flexiblas_profile_file = NULL;
			} else {
				__flexiblas_profile_file = strdup(tmp);
			}
		}
        /* if ( atexit ( flexiblas_print_profile ) != 0 ) {
            fprintf(stderr, "Cannot setup Profiling Output \n");
        } */
	}
	if ( blas_default_map ) free (blas_default_map); 
}



/*-----------------------------------------------------------------------------
 *  Cleanup 
 *-----------------------------------------------------------------------------*/
#ifndef __WIN32__ 
__attribute__((destructor)) 
#endif
void flexiblas_exit() {
    int i;
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
    nloaded_backends = 0;
	__flexiblas_free_paths();
	csc_ini_free(&__flexiblas_config); 
}


__attribute__((unused)) 
static void print_profile(FILE * output, const char*name, struct flexiblas_blasfn *fn)
{
#ifdef FLEXIBLAS_CBLAS 
	char cblas_name[64];
#endif
	fprintf(output,"%16s \t %11.7e \t %8lu\n",name,fn->timings[0],(unsigned long) fn->calls[0]);
#ifdef FLEXIBLAS_CBLAS
	snprintf(cblas_name, 63, "cblas_%s", name);
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n",cblas_name, fn->timings[1], (unsigned long) fn->calls[1], (fn->call_cblas == NULL && fn->calls[1]>0)?"redirected to BLAS":"");
#endif

}
static void print_profile_offset(FILE * output, const char*name, ssize_t offset)
{
    double timings = 0; 
    size_t calls = 0; 
    size_t i; 
    struct flexiblas_blasfn * fn; 
#ifdef FLEXIBLAS_CBLAS 
	char cblas_name[64];
    double ctimings = 0; 
    size_t ccalls = 0; 
#endif
    for (i = 0; i < nloaded_backends; i++) {
        fn = (struct flexiblas_blasfn *) ( ((void*)loaded_backends[i]) + offset); 
        timings += fn->timings[0]; 
        calls   += fn->calls[0]; 
#ifdef FLEXIBLAS_CBLAS
        ctimings += fn->timings[1]; 
        ccalls   += fn->calls[1]; 
#endif

    }
	fprintf(output,"%16s \t %11.7e \t %8lu\n",name,timings,(unsigned long) calls);
#ifdef FLEXIBLAS_CBLAS
	snprintf(cblas_name, 63, "cblas_%s", name);
	fprintf(output,"%16s \t %11.7e \t %8lu \n",cblas_name, ctimings, (unsigned long) ccalls); 
#endif

}


#define GET_BLAS_OFFSET(BLAS_NAME) (((ssize_t) &(current_backend->blas. BLAS_NAME)) - (ssize_t) current_backend) 
#define GET_EXTBLAS_OFFSET(BLAS_NAME) (((ssize_t) &(current_backend->extblas. BLAS_NAME)) - (ssize_t) current_backend) 

void  flexiblas_print_profile() {
	FILE *output = NULL; 
	int  on_screen = 0;

	if (__flexiblas_profile_file == NULL) {
		output = stderr; 
		on_screen =1;
	} else {
		if ( strcmp(__flexiblas_profile_file, "stdout") == 0){
			output = stdout;
			on_screen = 1;
		} else if ( strcmp(__flexiblas_profile_file, "stderr") == 0 ){
			output = stderr;
			on_screen = 1;
		} else {
			output = fopen(__flexiblas_profile_file,"w"); 
			if (!output){
				int err = errno; 
				fprintf(stderr, "Opening %s for profile output failed. Use stderr instead. (Reason: %s)\n",__flexiblas_profile_file, strerror(err));
				output = stderr; 
				on_screen = 1;
			} 
		}
	}
	if (__flexiblas_profile_file != NULL) 
		free(__flexiblas_profile_file);

    /*-----------------------------------------------------------------------------
     *  Print the output 
     *-----------------------------------------------------------------------------*/
    if (on_screen) {
		fprintf(output, "\n");
		fprintf(output, "*******************************************************************************\n");
		fprintf(output, "* FlexiBLAS Profiling                                                         *\n");
		fprintf(output, "*******************************************************************************\n");
		fprintf(output, "\n");
		fprintf(output, "*-----------------------------------------------------------------------------*\n"); 
		fprintf(output, "* Single Precission BLAS calls.                                               *\n"); 
		fprintf(output, "*-----------------------------------------------------------------------------*\n"); 
	}
	fprintf(output,"#Function\t\t Runtime in s\t     Calls \n"); 

	print_profile_offset(output,"sasum", GET_BLAS_OFFSET( sasum ) );
	print_profile_offset(output,"saxpy", GET_BLAS_OFFSET( saxpy ) );
	print_profile_offset(output,"scabs1",GET_BLAS_OFFSET( scabs1 ) );
	print_profile_offset(output,"scopy", GET_BLAS_OFFSET( scopy ) );
	print_profile_offset(output,"sdot",  GET_BLAS_OFFSET( sdot ) );
	print_profile_offset(output,"sdsdot",GET_BLAS_OFFSET( sdsdot ) );
	print_profile_offset(output,"sgbmv", GET_BLAS_OFFSET( sgbmv ) );
	print_profile_offset(output,"sgemm", GET_BLAS_OFFSET( sgemm ) );
	print_profile_offset(output,"sgemv", GET_BLAS_OFFSET( sgemv ) );
	print_profile_offset(output,"sger",  GET_BLAS_OFFSET( sger ) );
	print_profile_offset(output,"snrm2", GET_BLAS_OFFSET( snrm2 ) );
	print_profile_offset(output,"srot",  GET_BLAS_OFFSET( srot ) );
	print_profile_offset(output,"srotg", GET_BLAS_OFFSET( srotg ) );
	print_profile_offset(output,"srotm", GET_BLAS_OFFSET( srotm ) );
	print_profile_offset(output,"srotmg",GET_BLAS_OFFSET( srotmg ) );
	print_profile_offset(output,"ssbmv", GET_BLAS_OFFSET( ssbmv ) );
	print_profile_offset(output,"sscal", GET_BLAS_OFFSET( sscal ) );
	print_profile_offset(output,"sspmv", GET_BLAS_OFFSET( sspmv ) );
	print_profile_offset(output,"sspr2", GET_BLAS_OFFSET( sspr2 ) );
	print_profile_offset(output,"sspr",  GET_BLAS_OFFSET( sspr ) );
	print_profile_offset(output,"sswap", GET_BLAS_OFFSET( sswap ) );
	print_profile_offset(output,"ssymm", GET_BLAS_OFFSET( ssymm ) );
	print_profile_offset(output,"ssymv", GET_BLAS_OFFSET( ssymv ) );
	print_profile_offset(output,"ssyr2", GET_BLAS_OFFSET( ssyr2 ) );
	print_profile_offset(output,"ssyr2k",GET_BLAS_OFFSET( ssyr2k ) );
	print_profile_offset(output,"ssyr",  GET_BLAS_OFFSET( ssyr ) );
	print_profile_offset(output,"ssyrk", GET_BLAS_OFFSET( ssyrk ) );
	print_profile_offset(output,"stbmv", GET_BLAS_OFFSET( stbmv ) );
	print_profile_offset(output,"stbsv", GET_BLAS_OFFSET( stbsv ) );
	print_profile_offset(output,"stpmv", GET_BLAS_OFFSET( stpmv ) );
	print_profile_offset(output,"stpsv", GET_BLAS_OFFSET( stpsv ) );
	print_profile_offset(output,"strmm", GET_BLAS_OFFSET( strmm ) );
	print_profile_offset(output,"strmv", GET_BLAS_OFFSET( strmv ) );
	print_profile_offset(output,"strsm", GET_BLAS_OFFSET( strsm ) );
	print_profile_offset(output,"strsv", GET_BLAS_OFFSET( strsv ) );



	if ( on_screen ) {
		fprintf(output, "\n");
		fprintf(output, "*-----------------------------------------------------------------------------*\n"); 
		fprintf(output, "* Double Precission BLAS calls.                                               *\n"); 
		fprintf(output, "*-----------------------------------------------------------------------------*\n"); 
		fprintf(output,"Function \t\t Runtime in s\t     Calls \n");
	}
	print_profile_offset(output,"dasum", GET_BLAS_OFFSET( dasum ) );
	print_profile_offset(output,"daxpy", GET_BLAS_OFFSET( daxpy ) );
	print_profile_offset(output,"dcabs1",GET_BLAS_OFFSET( dcabs1 ) );
	print_profile_offset(output,"dcopy", GET_BLAS_OFFSET( dcopy ) );
	print_profile_offset(output,"ddot",  GET_BLAS_OFFSET( ddot ) );
	print_profile_offset(output,"dgbmv", GET_BLAS_OFFSET( dgbmv ) );
	print_profile_offset(output,"dgemm", GET_BLAS_OFFSET( dgemm ) );
	print_profile_offset(output,"dgemv", GET_BLAS_OFFSET( dgemv ) );
	print_profile_offset(output,"dger",  GET_BLAS_OFFSET( dger ) );
	print_profile_offset(output,"dnrm2", GET_BLAS_OFFSET( dnrm2 ) );
	print_profile_offset(output,"drot",  GET_BLAS_OFFSET( drot ) );
	print_profile_offset(output,"drotg", GET_BLAS_OFFSET( drotg ) );
	print_profile_offset(output,"drotm", GET_BLAS_OFFSET( drotm ) );
	print_profile_offset(output,"drotmg",GET_BLAS_OFFSET( drotmg ) );
	print_profile_offset(output,"dsbmv", GET_BLAS_OFFSET( dsbmv ) );
	print_profile_offset(output,"dscal", GET_BLAS_OFFSET( dscal ) );
	print_profile_offset(output,"dsdot", GET_BLAS_OFFSET( dsdot ) );
	print_profile_offset(output,"dspmv", GET_BLAS_OFFSET( dspmv ) );
	print_profile_offset(output,"dspr2", GET_BLAS_OFFSET( dspr2 ) );
	print_profile_offset(output,"dspr",  GET_BLAS_OFFSET( dspr ) );
	print_profile_offset(output,"dswap", GET_BLAS_OFFSET( dswap ) );
	print_profile_offset(output,"dsymm", GET_BLAS_OFFSET( dsymm ) );
	print_profile_offset(output,"dsymv", GET_BLAS_OFFSET( dsymv ) );
	print_profile_offset(output,"dsyr2", GET_BLAS_OFFSET( dsyr2 ) );
	print_profile_offset(output,"dsyr2k",GET_BLAS_OFFSET( dsyr2k ) );
	print_profile_offset(output,"dsyr",  GET_BLAS_OFFSET( dsyr ) );
	print_profile_offset(output,"dsyrk", GET_BLAS_OFFSET( dsyrk ) );
	print_profile_offset(output,"dtbmv", GET_BLAS_OFFSET( dtbmv ) );
	print_profile_offset(output,"dtbsv", GET_BLAS_OFFSET( dtbsv ) );
	print_profile_offset(output,"dtpmv", GET_BLAS_OFFSET( dtpmv ) );
	print_profile_offset(output,"dtpsv", GET_BLAS_OFFSET( dtpsv ) );
	print_profile_offset(output,"dtrmm", GET_BLAS_OFFSET( dtrmm ) );
	print_profile_offset(output,"dtrmv", GET_BLAS_OFFSET( dtrmv ) );
	print_profile_offset(output,"dtrsm", GET_BLAS_OFFSET( dtrsm ) );
	print_profile_offset(output,"dtrsv", GET_BLAS_OFFSET( dtrsv ) );

	if ( on_screen ) {	
		fprintf(output, "\n");
		fprintf(output, "*-----------------------------------------------------------------------------*\n"); 
		fprintf(output, "* Complex Single Precission BLAS calls.                                       *\n"); 
		fprintf(output, "*-----------------------------------------------------------------------------*\n"); 
		fprintf(output,"Function \t\t Runtime in s\t     Calls \n"); 
	}

	print_profile_offset(output, "scasum",  GET_BLAS_OFFSET( scasum ));
	print_profile_offset(output, "scnrm2",  GET_BLAS_OFFSET( scnrm2 ));


	print_profile_offset(output, "caxpy",  GET_BLAS_OFFSET( caxpy ));
	print_profile_offset(output, "ccopy",  GET_BLAS_OFFSET( ccopy ));
	print_profile_offset(output, "cdotc",  GET_BLAS_OFFSET( cdotc ));
	print_profile_offset(output, "cdotu",  GET_BLAS_OFFSET( cdotu ));
	print_profile_offset(output, "cgbmv",  GET_BLAS_OFFSET( cgbmv ));
	print_profile_offset(output, "cgemm",  GET_BLAS_OFFSET( cgemm ));
	print_profile_offset(output, "cgemv",  GET_BLAS_OFFSET( cgemv ));
	print_profile_offset(output, "cgerc",  GET_BLAS_OFFSET( cgerc ));
	print_profile_offset(output, "cgeru",  GET_BLAS_OFFSET( cgeru ));
	print_profile_offset(output, "chbmv",  GET_BLAS_OFFSET( chbmv ));
	print_profile_offset(output, "chemm",  GET_BLAS_OFFSET( chemm ));
	print_profile_offset(output, "chemv",  GET_BLAS_OFFSET( chemv ));
	print_profile_offset(output, "cher",   GET_BLAS_OFFSET( cher ));
	print_profile_offset(output, "cher2",  GET_BLAS_OFFSET( cher2 ));
	print_profile_offset(output, "cher2k", GET_BLAS_OFFSET( cher2k ));
	print_profile_offset(output, "cherk",  GET_BLAS_OFFSET( cherk ));
	print_profile_offset(output, "chpmv",  GET_BLAS_OFFSET( chpmv ));
	print_profile_offset(output, "chpr",   GET_BLAS_OFFSET( chpr ));
	print_profile_offset(output, "chpr2",  GET_BLAS_OFFSET( chpr2 ));
	print_profile_offset(output, "crotg",  GET_BLAS_OFFSET( crotg ));
	print_profile_offset(output, "csrot",  GET_BLAS_OFFSET( csrot ));
	print_profile_offset(output, "cscal",  GET_BLAS_OFFSET( cscal ));
	print_profile_offset(output, "csscal", GET_BLAS_OFFSET( csscal ));
	print_profile_offset(output, "cswap",  GET_BLAS_OFFSET( cswap ));
	print_profile_offset(output, "csymm",  GET_BLAS_OFFSET( csymm ));
	print_profile_offset(output, "csyr2k", GET_BLAS_OFFSET( csyr2k ));
	print_profile_offset(output, "csyrk",  GET_BLAS_OFFSET( csyrk ));
	print_profile_offset(output, "ctbmv",  GET_BLAS_OFFSET( ctbmv ));
	print_profile_offset(output, "ctbsv",  GET_BLAS_OFFSET( ctbsv ));
	print_profile_offset(output, "ctpmv",  GET_BLAS_OFFSET( ctpmv ));
	print_profile_offset(output, "ctpsv",  GET_BLAS_OFFSET( ctpsv ));
	print_profile_offset(output, "ctrmm",  GET_BLAS_OFFSET( ctrmm ));
	print_profile_offset(output, "ctrmv",  GET_BLAS_OFFSET( ctrmv ));
	print_profile_offset(output, "ctrsm",  GET_BLAS_OFFSET( ctrsm ));
	print_profile_offset(output, "ctrsv",  GET_BLAS_OFFSET( ctrsv ));



	if ( on_screen ) {
		fprintf(output, "\n");
		fprintf(output, "*-----------------------------------------------------------------------------*\n"); 
		fprintf(output, "* Complex Double Precission BLAS calls.                                       *\n"); 
		fprintf(output, "*-----------------------------------------------------------------------------*\n"); 
		fprintf(output,"Function \t\t Runtime in s\t     Calls \n");
	}
	print_profile_offset(output, "dzasum",  GET_BLAS_OFFSET( dzasum ));
	print_profile_offset(output, "dznrm2",  GET_BLAS_OFFSET( dznrm2 ));

	print_profile_offset(output,"zaxpy", GET_BLAS_OFFSET( zaxpy ) );
	print_profile_offset(output,"zcopy", GET_BLAS_OFFSET( zcopy ) );
	print_profile_offset(output,"zdotc", GET_BLAS_OFFSET( zdotc ) );
	print_profile_offset(output,"zdotu", GET_BLAS_OFFSET( zdotu ) );
	print_profile_offset(output,"zdrot", GET_BLAS_OFFSET( zdrot ) );
	print_profile_offset(output,"zdscal", GET_BLAS_OFFSET( zdscal ) );
	print_profile_offset(output,"zgbmv", GET_BLAS_OFFSET( zgbmv ) );
	print_profile_offset(output,"zgemm", GET_BLAS_OFFSET( zgemm ) );
	print_profile_offset(output,"zgemv", GET_BLAS_OFFSET( zgemv ) );
	print_profile_offset(output,"zgerc", GET_BLAS_OFFSET( zgerc ) );
	print_profile_offset(output,"zgeru", GET_BLAS_OFFSET( zgeru ) );
	print_profile_offset(output,"zhbmv", GET_BLAS_OFFSET( zhbmv ) );
	print_profile_offset(output,"zhemm", GET_BLAS_OFFSET( zhemm ) );
	print_profile_offset(output,"zhemv", GET_BLAS_OFFSET( zhemv ) );
	print_profile_offset(output,"zher2", GET_BLAS_OFFSET( zher2 ) );
	print_profile_offset(output,"zher2k", GET_BLAS_OFFSET( zher2k ) );
	print_profile_offset(output,"zher", GET_BLAS_OFFSET( zher ) );
	print_profile_offset(output,"zherk", GET_BLAS_OFFSET( zherk ) );
	print_profile_offset(output,"zhpmv", GET_BLAS_OFFSET( zhpmv ) );
	print_profile_offset(output,"zhpr2", GET_BLAS_OFFSET( zhpr2 ) );
	print_profile_offset(output,"zhpr", GET_BLAS_OFFSET( zhpr ) );
	print_profile_offset(output,"zrotg", GET_BLAS_OFFSET( zrotg ) );
	print_profile_offset(output,"zscal", GET_BLAS_OFFSET( zscal ) );
	print_profile_offset(output,"zswap", GET_BLAS_OFFSET( zswap ) );
	print_profile_offset(output,"zsymm", GET_BLAS_OFFSET( zsymm ) );
	print_profile_offset(output,"zsyr2k", GET_BLAS_OFFSET( zsyr2k ) );
	print_profile_offset(output,"zsyrk", GET_BLAS_OFFSET( zsyrk ) );
	print_profile_offset(output,"ztbmv", GET_BLAS_OFFSET( ztbmv ) );
	print_profile_offset(output,"ztbsv", GET_BLAS_OFFSET( ztbsv ) );
	print_profile_offset(output,"ztpmv", GET_BLAS_OFFSET( ztpmv ) );
	print_profile_offset(output,"ztpsv", GET_BLAS_OFFSET( ztpsv ) );
	print_profile_offset(output,"ztrmm", GET_BLAS_OFFSET( ztrmm ) );
	print_profile_offset(output,"ztrmv", GET_BLAS_OFFSET( ztrmv ) );
	print_profile_offset(output,"ztrsm", GET_BLAS_OFFSET( ztrsm ) );
	print_profile_offset(output,"ztrsv", GET_BLAS_OFFSET( ztrsv ) );

	/* BLAS Extension  */
#ifdef EXTBLAS_ENABLED
	if ( on_screen ) {
		fprintf(output, "\n");
		fprintf(output, "*-----------------------------------------------------------------------------*\n"); 
		fprintf(output, "* BLAS Extension calls.                                                       *\n"); 
		fprintf(output, "*-----------------------------------------------------------------------------*\n"); 
		fprintf(output,"Function \t\t Runtime in s\t     Calls \n"); 
	}
    print_profile_offset(output, "saxpby",       GET_EXTBLAS_OFFSET( saxpby ) );
    print_profile_offset(output, "daxpby",       GET_EXTBLAS_OFFSET( daxpby ) );
    print_profile_offset(output, "caxpby",       GET_EXTBLAS_OFFSET( caxpby ) );
    print_profile_offset(output, "zaxpby",       GET_EXTBLAS_OFFSET( zaxpby ) );
    print_profile_offset(output, "somatcopy",       GET_EXTBLAS_OFFSET( somatcopy ) );
    print_profile_offset(output, "domatcopy",       GET_EXTBLAS_OFFSET( domatcopy ) );
    print_profile_offset(output, "comatcopy",       GET_EXTBLAS_OFFSET( comatcopy ) );
    print_profile_offset(output, "zomatcopy",       GET_EXTBLAS_OFFSET( zomatcopy ) );

    print_profile_offset(output, "simatcopy",       GET_EXTBLAS_OFFSET( simatcopy ) );
    print_profile_offset(output, "dimatcopy",       GET_EXTBLAS_OFFSET( dimatcopy ) );
    print_profile_offset(output, "cimatcopy",       GET_EXTBLAS_OFFSET( cimatcopy ) );
    print_profile_offset(output, "zimatcopy",       GET_EXTBLAS_OFFSET( zimatcopy ) );
#endif 
	if ( on_screen ){
		fprintf(output, "*******************************************************************************\n"); 
	}
    print_profile_offset(output, "xerbla", (((ssize_t) &(current_backend->xerbla)) - (ssize_t) current_backend));

	if ( on_screen ) {
		fprintf(output, "*******************************************************************************\n"); 
	}


	if ( output != stderr && output != stdout) fclose(output); 
	return; 
}

double flexiblas_wtime ()
{
	struct timeval tv;
	gettimeofday (&tv, NULL);
	return tv.tv_sec + tv.tv_usec / 1e6;
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
