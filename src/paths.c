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



#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "paths.h"
#include "helper.h"
#include "cscutils/strutils.h"

HIDDEN char **  __flexiblas_additional_paths = NULL;
HIDDEN int __flexiblas_count_additional_paths = 0;


/*-----------------------------------------------------------------------------
 *  Init default search path
 *-----------------------------------------------------------------------------*/
HIDDEN void __flexiblas_init_default_paths(void) {
    char *searchpath = strdup(FLEXIBLAS_DEFAULT_LIB_PATH);
    char *path;
    char *r = NULL;

    searchpath = csc_str_remove_char(searchpath,'"');
    searchpath = csc_str_remove_char(searchpath,'\'');


    path = strtok_r(searchpath,":", &r);
    while ( path != NULL ) {
        __flexiblas_add_path(path);
        path = strtok_r(NULL, ":",&r);
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
    if ( getenv(ENV_FLEXIBLAS_LIBRARY_PATH)){
        v = strdup(getenv(ENV_FLEXIBLAS_LIBRARY_PATH));
        v = csc_str_remove_char(v, '"');
        v = csc_str_remove_char(v,'\'');
        p = strtok(v, ":");
        while ( p != NULL ) {
            if ( strlen(p) > 0 ) __flexiblas_add_path(p);
            p = strtok( NULL, ":");
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
        free(__flexiblas_additional_paths[i]);
    }
    if ( __flexiblas_additional_paths != NULL) free(__flexiblas_additional_paths);
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

