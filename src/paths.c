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

