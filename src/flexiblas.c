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
#include "paths.h"
#include "hooks.h"

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

/*  Initialize global variables. */
HIDDEN int __flexiblas_initialized = 0;
HIDDEN int __flexiblas_profile = 0;
HIDDEN flexiblas_backend_t *current_backend = NULL;
HIDDEN flexiblas_backend_t **loaded_backends = NULL;
HIDDEN size_t                  nloaded_backends = 0;
HIDDEN flexiblas_mgmt_t *__flexiblas_mgmt = NULL;
HIDDEN flexiblas_hook_t *__flexiblas_hooks = NULL;

HIDDEN flexiblas_exit_function_t hook_exit = NULL;

HIDDEN void *__flexiblas_blas_fallback = NULL;
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
 *  Init the backend.
 *-----------------------------------------------------------------------------*/
HIDDEN void __flexiblas_backend_init( flexiblas_backend_t * backend) {
    int load = 0;
    int failed = 0;


    if (backend == NULL) {
        DPRINTF(0, PRINT_PREFIX " No current BLAS is set.\n");
        abort();
    }
    pthread_mutex_lock(&(backend->post_init_mutex));
    if ( backend->post_init != 0 ) {
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
        int nolapack = 0;
        flexiblas_mgmt_location_t loc;
        if ( getenv(ENV_FLEXIBLAS_NOLAPACK)) {
            nolapack = atoi(getenv(ENV_FLEXIBLAS_NOLAPACK));
        } else {
            flexiblas_mgmt_get_active_property(__flexiblas_mgmt, &loc, FLEXIBLAS_PROP_NOLAPACK, &nolapack);
        }
        if ( nolapack ) {
            DPRINTF(1,"LAPACK is load from the fallback only. No LAPACK routines from the backend will be used.\n");
            __flexiblas_load_flapack_fallback(backend, &load, &failed);
        } else {
            __flexiblas_load_flapack(backend, &load, &failed);
        }
#endif

        /* Setup XERBLA */
        __flexiblas_setup_xerbla(backend);
#ifdef FLEXIBLAS_CBLAS
        __flexiblas_setup_cblas_xerbla(backend);
#endif
        backend->post_init = 0;
    }


    pthread_mutex_unlock(&(backend->post_init_mutex));

    if ( failed > 0) {
        DPRINTF_ERROR(0," Failed to load the backend completely, some BLAS functions are missing. Abort!\n");
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
static void print_info(flexiblas_backend_t *backend)
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

            DPRINTF_ERROR(0,"Failed to get the default backend. Reset to FALLBACK.\n");
            strncpy(blas_name, "__FALLBACK__", FLEXIBLAS_MGMT_MAX_BUFFER_LEN);
        }

        if ( flexiblas_mgmt_blas_get2(config, &loc, blas_name, clibrary, NULL)) {
            DPRINTF_ERROR(0, "Failed to get the BLAS backend (%s) from the configuration.\n", blas_name);
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
            DPRINTF_WARN(1,"\"%s\" does not seem to a shared library. Search inside the FlexiBLAS configuration..\n", tmp);

            clibrary = (char*) malloc(sizeof(char)*32768);
            if ( flexiblas_mgmt_blas_get2(config, &loc, tmp, clibrary, NULL)) {
                free(clibrary);
                clibrary = NULL;
            }

            /* Load the default BLAS if the env_FLEXIBLAS implementation was not found in the configuration */
            if (clibrary == NULL ) {
                if ( flexiblas_mgmt_get_active_default(config, &loc, blas_name)) {
                    DPRINTF_ERROR(0,"Failed to get the default backend. Reset to FALLBACK.\n");
                    strncpy(blas_name, "__FALLBACK__", FLEXIBLAS_MGMT_MAX_BUFFER_LEN);
                }
                DPRINTF_ERROR(0, "BLAS backend  \"%s\" not found. Loading default (%s) instead.\n", tmp, blas_name);

                clibrary = (char*) malloc(sizeof(char)*32768);
                if ( flexiblas_mgmt_blas_get2(config, &loc, blas_name, clibrary, NULL)) {
                    DPRINTF_ERROR(0, "Failed to get the BLAS backend (%s) from the configuration.\n", blas_name);
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
            DPRINTF_ERROR(0, "Failed to get the BLAS backend (__FALLBACK__) from the configuration.\n");
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


    backend->hook_init = 0;

    if ( backend->post_init == 0 ) {
        backend->post_init = 1;
        __flexiblas_backend_init(backend);
    } else {
        DPRINTF(0, "BLAS backend uses post initialization.\n");
    }

    print_info(backend);
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
        DPRINTF_ERROR(0, "BLAS %s not found in config.\n", blas_name);
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
    print_info(backend);

    backend->post_init =  backend->info.post_init;
    backend->hook_init =  0;


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
        flexiblas_backend_t ** new_loaded;
        nloaded_backends++;
        new_loaded  = realloc(loaded_backends, sizeof(flexiblas_backend_t*) * nloaded_backends);
        if (new_loaded == NULL) {
            DPRINTF(0, "Failed to allocate memory to load the new backend. Abort\n");
            abort();
        } else {
            loaded_backends = new_loaded;
        }
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
    backend->hook_init =  0;


    if ( backend->post_init == 0 ) {
        backend->post_init = 1;
        __flexiblas_backend_init(backend);
    } else {
        DPRINTF(0, "BLAS backend %s uses post initialization.\n", blas_sofile);
    }

    print_info(backend);
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
        flexiblas_backend_t **new_loaded;
        nloaded_backends++;
        new_loaded = realloc(loaded_backends, sizeof(flexiblas_backend_t*) * nloaded_backends);
        if (new_loaded == NULL) {
            DPRINTF(0, "Failed to allocate memory to load the new backend. Abort\n");
            abort();
        } else {
            loaded_backends = new_loaded;
        }

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

        /*-----------------------------------------------------------------------------
         *  Read Environment Variables
         *-----------------------------------------------------------------------------*/
        char *env_FLEXIBLAS_VERBOSE=getenv(ENV_FLEXIBLAS_VERBOSE);
        char *env_FLEXIBLAS_COLOR_OUTPUT=getenv(ENV_FLEXIBLAS_COLOR_OUTPUT);
        char *env_FLEXIBLAS_HOOK=getenv(ENV_FLEXIBLAS_HOOK);

        if ( __flexiblas_initialized != 0) return;
        __flexiblas_initialized = 1;
        __flexiblas_mgmt_init = 1;

        /* Color Output */
        if (env_FLEXIBLAS_COLOR_OUTPUT != NULL) {
            int s = atoi(env_FLEXIBLAS_COLOR_OUTPUT);
            flexiblas_set_color_output(s);
        }
        /* Load environemt variables   */
        if ( env_FLEXIBLAS_VERBOSE != NULL ) {
            __flexiblas_verbose = atoi(env_FLEXIBLAS_VERBOSE);
        }

        memset(path, '\0', FLEXIBLAS_MGMT_MAX_BUFFER_LEN);
        /*-----------------------------------------------------------------------------
         *  Read mapping file
         *  1. /etc/flexiblasrc  or its counterpart in the build directory
         *  3. $HOME/.flexiblasrc
         *-----------------------------------------------------------------------------*/
        __flexiblas_mgmt = flexiblas_mgmt_load_config();
        if ( __flexiblas_mgmt == NULL) {
            DPRINTF_ERROR(0, "Cannot initialize/load the configuration.\n");
            abort();
        }

        if ( env_FLEXIBLAS_VERBOSE == NULL) {
            /* Load Properties */
            flexiblas_mgmt_get_active_property(__flexiblas_mgmt, &loc, FLEXIBLAS_PROP_VERBOSE, &__flexiblas_verbose);
        }



        /* Add additional search paths */
        __flexiblas_add_path_from_environment();
        __flexiblas_add_path_from_config(__flexiblas_mgmt, FLEXIBLAS_ENV);
        __flexiblas_add_path_from_config(__flexiblas_mgmt, FLEXIBLAS_HOST);
        __flexiblas_add_path_from_config(__flexiblas_mgmt, FLEXIBLAS_USER);
        __flexiblas_add_path_from_config(__flexiblas_mgmt, FLEXIBLAS_GLOBAL);
        __flexiblas_init_default_paths();


        /* Search all available hooks */
        __flexiblas_add_hooks();


        if (__flexiblas_insert_fallback_blas(__flexiblas_mgmt)) {
            DPRINTF_ERROR(0, "Failed to initialize the default and the fallback BLAS backend.\n");
            abort();
        }
        /*  Load the active default BLAS */
        if ( flexiblas_mgmt_get_active_default(__flexiblas_mgmt, &loc, blas_default_map) < 0 ) {
            DPRINTF_ERROR(0, "Failed to select the default BLAS backend.\n");
            abort();
        }

        /*-----------------------------------------------------------------------------
         *  Display Copyright
         *-----------------------------------------------------------------------------*/
        if ( __flexiblas_verbose) {
            __flexiblas_print_copyright(1);
        }

        /*
         * Load Hooks
         */
        __flexiblas_hooks = (flexiblas_hook_t *) malloc(sizeof(flexiblas_hook_t) * (1));
        if (!__flexiblas_hooks) {
            DPRINTF_ERROR(0, "Failed to allocate memory for hook management. Abort.\n");
            abort();
        }
        memset(__flexiblas_hooks, 0, sizeof(flexiblas_hook_t));
        __flexiblas_hooks->hooks_loaded = 0;
        __flexiblas_hooks->initialized  = 0;


        int hooks_to_load = 0;
        char ** hook_load_list = NULL;
        int ret = 0;
        int i ;
        if ( env_FLEXIBLAS_HOOK == NULL ){
            // Load from Config
            flexiblas_mgmt_location_t locx;
            ret = flexiblas_mgmt_hook_get_active(__flexiblas_mgmt, &locx, &hooks_to_load, &hook_load_list);
            if ( ret != 0 ) {
                DPRINTF_ERROR(0, "Failed to obtain list of enabled hooks from the configuration. Continue without hooks.\n");
                goto continue_load;
            }
        } else {
            // load from environment
            char *nexttoken = NULL;
            char *saveptr = NULL;
            i = 0;
            nexttoken = strtok_r(env_FLEXIBLAS_HOOK,":,", &saveptr);
            while (nexttoken != NULL ){
                i++;
                if (!( __flexiblas_hook_exists(nexttoken))) {
                    char *hook_name = __flexiblas_hook_add_from_file(nexttoken);
                    if ( !hook_name) {
                        DPRINTF_ERROR(0, "Hook %s not found. Abort.\n", nexttoken);
                        abort();
                    }
                    hook_load_list = realloc (hook_load_list, i * sizeof(char*));
                    hook_load_list[i-1] = strdup(hook_name);

                } else {
                    hook_load_list = realloc (hook_load_list, i * sizeof(char*));
                    hook_load_list[i-1] = strdup(nexttoken);
                }
                nexttoken = strtok_r(NULL,":,", &saveptr);
            }
            hooks_to_load = i;
        }

        int k = 0;
        for (i = 0; i < hooks_to_load; i++) {
            char * sofile;
            void * handle = NULL;

            sofile = __flexiblas_hook_sofile(hook_load_list[i]);
            handle  = __flexiblas_dlopen(sofile, RTLD_NOW | RTLD_LOCAL , NULL);
            DPRINTF(1,"Load hook: %s - %s\n", hook_load_list[i], sofile);
            if ( ! handle ) {
                DPRINTF_ERROR(0, "Failed to load hook %s. Either it does not exists in the configuration or it is not a shared object.\n", hook_load_list[i]);
                free(hook_load_list[i]);
                continue;
            }


            __flexiblas_hooks->handles[k] = handle;
            __flexiblas_hooks->hook_init[k] = (flexiblas_init_function_t) dlsym(handle, FLEXIBLAS_HOOK_INIT_FUNCTION_NAME);
            __flexiblas_hooks->hook_exit[k] = (flexiblas_exit_function_t) dlsym(handle, FLEXIBLAS_HOOK_EXIT_FUNCTION_NAME);

            __flexiblas_load_blas_hooks(__flexiblas_hooks, handle);

            __flexiblas_hooks->hooks_loaded ++;
            if ( __flexiblas_hooks->hooks_loaded >= FLEXIBLAS_MAX_HOOKS ) {
                DPRINTF_ERROR(0, "More than %d hook libraries loaded. Please reduce the number of hook libraries. Abort!\n", FLEXIBLAS_MAX_HOOKS);
                abort();
            }
            k++;
            free(hook_load_list[i]);
        }
        free(hook_load_list);

        /* Init The Hook Library  */
        if (__flexiblas_hooks->initialized == 0) {
            __flexiblas_hooks->initialized = 1;
            for (k = 0; k < __flexiblas_hooks->hooks_loaded; k++) {
                __flexiblas_hooks->hook_init[k]();
            }
        }


continue_load:
        /*-----------------------------------------------------------------------------
         *  Load Library
         *-----------------------------------------------------------------------------*/
        uppercase(blas_default_map);

        /*-----------------------------------------------------------------------------
         *  Load NETLIB Fallback
         *-----------------------------------------------------------------------------*/
        {
            char *SO_EXTENSION = __flexiblas_getenv(FLEXIBLAS_ENV_SO_EXTENSION);
            size_t len=strlen(FALLBACK_NAME)+strlen(SO_EXTENSION)+2;
            char *blas_name = (char *) calloc(len,sizeof(char));
            snprintf(blas_name,len, "%s%s", FALLBACK_NAME,SO_EXTENSION);
            free(SO_EXTENSION);

            __flexiblas_blas_fallback = __flexiblas_dlopen(blas_name, RTLD_LAZY | RTLD_GLOBAL , NULL);
            if ( __flexiblas_blas_fallback == NULL ) {
                DPRINTF_ERROR(0," Failed to load the BLAS fallback library.  Abort!\n");
                abort();
            }
            free(blas_name);
        }

#ifdef FLEXIBLAS_LAPACK
        /*-----------------------------------------------------------------------------
         *  Load LAPACK Fallback
         *-----------------------------------------------------------------------------*/
        {
            char *SO_EXTENSION = __flexiblas_getenv(FLEXIBLAS_ENV_SO_EXTENSION);
            size_t len=strlen(LAPACK_FALLBACK_NAME)+strlen(SO_EXTENSION)+2;
            char *lapack_name = (char *) calloc(len,sizeof(char));
            snprintf(lapack_name,len, "%s%s", LAPACK_FALLBACK_NAME,SO_EXTENSION);
            free(SO_EXTENSION);
#ifdef LAPACK_DEEPBIND
            __flexiblas_lapack_fallback = __flexiblas_dlopen(lapack_name, RTLD_LAZY | RTLD_DEEPBIND |  RTLD_GLOBAL , NULL);
#else
            __flexiblas_lapack_fallback = __flexiblas_dlopen(lapack_name, RTLD_LAZY | RTLD_GLOBAL , NULL);
#endif
            if ( __flexiblas_lapack_fallback == NULL ) {
                DPRINTF_ERROR(0," Failed to load the LAPACK fallback library.  Abort!\n");
                abort();
            }
            free(lapack_name);
        }
#endif


        /*
         * LOAD BLAS Backend.
         */
        backend = flexiblas_load_library_from_init(__flexiblas_mgmt, blas_default_map);
        if ( backend == NULL ){
            DPRINTF_ERROR(0, "Loading Backend Failed.\n");
            abort();
        }
        loaded_backends = (flexiblas_backend_t **) malloc(sizeof(flexiblas_backend_t*) * 1);
        /* Set the loaded backend as default one.  */
        nloaded_backends = 1;
        loaded_backends[0] = backend;
        current_backend  = backend;


    }



/*-----------------------------------------------------------------------------
 *  Cleanup
 *-----------------------------------------------------------------------------*/
#ifndef __WIN32__
__attribute__((destructor))
#endif
    void flexiblas_exit() {
        size_t i;
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

        dlclose(__flexiblas_blas_fallback);
#ifdef FLEXIBLAS_LAPACK
        dlclose(__flexiblas_lapack_fallback);
#endif
        int k;
        for ( k = __flexiblas_hooks->hooks_loaded-1; k>=0; k--){
            __flexiblas_hooks->hook_exit[k]();
            dlclose(__flexiblas_hooks->handles[k]);
        }
        free(__flexiblas_hooks);
        nloaded_backends = 0;
        __flexiblas_free_paths();
        __flexiblas_exit_hook();
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


flexiblas_mgmt_t * flexiblas_mgmt()
{
    return __flexiblas_mgmt;
}
