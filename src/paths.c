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

#ifndef __GNU_SOURCE
#define __GNU_SOURCE
#endif

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "paths.h"
#include "helper.h"
#include "cscutils/strutils.h"

#ifdef _WIN32
#include "windows_fixes.h"
#include <windows.h>
#define FUNC_RETURN_ADDRESS() _ReturnAddress()
#else
#define FUNC_RETURN_ADDRESS() __builtin_extract_return_addr(__builtin_return_address(0))
#include <linux/limits.h>
#include <libgen.h>
#endif

HIDDEN char **  __flexiblas_additional_paths = NULL;
HIDDEN int __flexiblas_count_additional_paths = 0;

#include <dlfcn.h>


__attribute__((noinline)) char *
__flexiblas_get_library_location_impl(void)
{
#ifdef __WIN32__
    char * buffer = malloc(sizeof(char) * MAX_PATH);

    HMODULE module;
    if (GetModuleHandleEx(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS | GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT,
        (LPCTSTR)FUNC_RETURN_ADDRESS(), &module))
    {
        GetModuleFileNameA(module, buffer, MAX_PATH);
        char * base_path = dirname(buffer);
        free(buffer);
        buffer = base_path;
    }
#else
    char * buffer = malloc(sizeof(char) * PATH_MAX);

    for (;;)
    {
        Dl_info info;

        if (dladdr(FUNC_RETURN_ADDRESS(), &info))
        {
            if (!realpath(info.dli_fname, buffer)) {break;}
        }

        break;
    }
#endif

    return buffer;
}

HIDDEN __attribute__((noinline)) char *
__flexiblas_get_library_location(void)
{
  return __flexiblas_get_library_location_impl();
}

#define MAX_BUFFER_SIZE 32*1024

HIDDEN void __flexiblas_get_global_rc_path(char * container, int max_buffer_size,  char const * suffix)
{
    char sysconfdir[MAX_BUFFER_SIZE];
    char sysconfdir_clean[MAX_BUFFER_SIZE];
    char *libpath = __flexiblas_get_library_location();

    /* On Linux the rc path is in the libpath/../etc/suffix subdirectory */
    if (libpath != NULL)
    {
        char *folder = dirname(libpath);

        strcpy(sysconfdir, folder);
        strcat(sysconfdir, "/../etc");
#if defined(__WIN32__)
        _fullpath(sysconfdir_clean, sysconfdir, MAX_BUFFER_SIZE);
#else
        if (!realpath(sysconfdir, sysconfdir_clean)) {
            free(libpath);
            return;
        }
#endif

        snprintf(container, max_buffer_size, "%s/%s", sysconfdir_clean, suffix);

        free(libpath);
    }
}

/*-----------------------------------------------------------------------------
 *  Init default search path
 *-----------------------------------------------------------------------------*/
HIDDEN void __flexiblas_init_default_paths(void) {
    char *libpath = __flexiblas_get_library_location();
    char *searchpath = NULL;

#if !defined(__WIN32__)
    /* On Linux the default library path is in the FLEXIBLAS_LIBRARY_DIR subdirectory */
    if (libpath != NULL)
    {
        char *folder = dirname(libpath);

        searchpath = malloc(strlen(folder) + 1 + strlen(FLEXIBLAS_LIBRARY_DIR) + 1);
        if (searchpath != NULL)
        {
            strcpy(searchpath, folder);
            strcat(searchpath, "/");
            strcat(searchpath, FLEXIBLAS_LIBRARY_DIR);
        }
        free(libpath);
    }
#else
    searchpath = libpath;
#endif
    char *path;
    char *r = NULL;

    searchpath = csc_str_remove_char(searchpath,'"');
    searchpath = csc_str_remove_char(searchpath,'\'');

#if defined(__WIN32__)
    const char * delim = ";";
#else
    const char * delim = ":";
#endif

    path = strtok_r(searchpath, delim, &r);
    while ( path != NULL ) {
        __flexiblas_add_path(path);
        path = strtok_r(NULL, delim, &r);
    }
    free(searchpath);
}


/*
 * Add Search paths from FLEXIBLAS_LIBRARY_PATH
 */
HIDDEN void __flexiblas_add_path_from_environment(void)
{
    char * v;
    char * p;

#if defined(__WIN32__)
    const char * delim = ";";
#else
    const char * delim = ":";
#endif

    if ( getenv(ENV_FLEXIBLAS_LIBRARY_PATH)){
        v = strdup(getenv(ENV_FLEXIBLAS_LIBRARY_PATH));
        v = csc_str_remove_char(v, '"');
        v = csc_str_remove_char(v,'\'');
        p = strtok(v, delim);
        while ( p != NULL ) {
            if ( strlen(p) > 0 ) __flexiblas_add_path(p);
            p = strtok( NULL, delim);
        }
        free(v);
    }
}

/*-----------------------------------------------------------------------------
 *  Path management
 *-----------------------------------------------------------------------------*/
HIDDEN void __flexiblas_add_path(const char * path ) {
    char **new_additionalpath;
    __flexiblas_count_additional_paths++;
    new_additionalpath = (char **) realloc( __flexiblas_additional_paths,
            sizeof(char *) * __flexiblas_count_additional_paths);
    if ( new_additionalpath == NULL ) {
        DPRINTF(0,"Adding additional path \"%s\" failed. Skipping it.\n", path);
    } else {
        __flexiblas_additional_paths = new_additionalpath;
    }
    DPRINTF(2,"Add additional search path %s\n", path);
    __flexiblas_additional_paths[__flexiblas_count_additional_paths-1] = csc_str_remove_char(strdup(path),'"');
    __flexiblas_additional_paths[__flexiblas_count_additional_paths-1] = csc_str_remove_char(__flexiblas_additional_paths[__flexiblas_count_additional_paths-1], '\'');
}

HIDDEN void __flexiblas_free_paths(void) {
    int i = 0;
    for ( i = 0; i < __flexiblas_count_additional_paths; i++) {
        if ( __flexiblas_additional_paths[i] != NULL )
        {
            free(__flexiblas_additional_paths[i]);
            __flexiblas_additional_paths[i] = NULL;
        }
    }

    __flexiblas_count_additional_paths = 0;

    if ( __flexiblas_additional_paths != NULL )
    {
        free(__flexiblas_additional_paths);
        __flexiblas_additional_paths = NULL;
    }
}




HIDDEN void __flexiblas_add_path_from_config( flexiblas_mgmt_t * config, flexiblas_mgmt_location_t loc)
{
    char path[FLEXIBLAS_MGMT_MAX_BUFFER_LEN];
    void * iter_helper = NULL;
    iter_helper = NULL;
    while ( flexiblas_mgmt_list_paths(config, loc, path, &iter_helper) > 0)
    {
        if ( strlen(path) > 0 ) __flexiblas_add_path(path);
    }

}

