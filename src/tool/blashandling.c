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
 * Copyright (C) Martin Koehler, 2013-2022
 */



#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <stdlib.h>
#include <limits.h>
#include <dlfcn.h>
#ifdef __linux__
#define _GNU_SOURCE
#endif

#include "flexiblas.h"
#include "flexiblas_config.h"
#include "paths.h"
#include "helper.h"
#include "hooks.h"
#include "cscutils/strutils.h"
#include "tool.h"

int remove_blas (flexiblas_mgmt_location_t loc, char *name)
{
    flexiblas_mgmt_t *config;
    int     ret = 0;

    config = flexiblas_mgmt_load_config();
    if (config == NULL) {
        printf("Failed to open configuration files. Abort.\n");
        return -1;
    }

    if ( flexiblas_mgmt_blas_remove(config, loc, name) ) {
        flexiblas_mgmt_free_config(config);
        printf("Failed to remove BLAS %s from %s.\n", name, flexiblas_mgmt_location_to_string(loc));
        return -1;
    }

    ret = flexiblas_mgmt_write_config(config);
    flexiblas_mgmt_free_config(config);
    if ( ret ) {
        printf("Failed to write the configuration to %s.\n", flexiblas_mgmt_location_to_string(loc));
        return -1;
    }
    return 0;
}

int add_blas (flexiblas_mgmt_location_t loc, char *name, char *blas, char *comment)
{
    flexiblas_mgmt_t *config;
    int     ret = 0;

    config = flexiblas_mgmt_load_config();
    if (config == NULL) {
        printf("Failed to open configuration files. Abort.\n");
        return -1;
    }

    if ( flexiblas_mgmt_blas_add(config, loc, name, blas, comment) ) {
        flexiblas_mgmt_free_config(config);
        printf("Failed to add BLAS (%s , %s).\n", name, blas);
        return -1;
    }

    ret = flexiblas_mgmt_write_config2(config,loc);
    flexiblas_mgmt_free_config(config);
    if ( ret ) {
        printf("Failed to write the configuration to %s.\n", flexiblas_mgmt_location_to_string(loc));
        return -1;
    }
    return 0;
}

int set_blas(flexiblas_mgmt_location_t loc, char* name)
{
    flexiblas_mgmt_t *config;
    int     ret = 0;

    config = flexiblas_mgmt_load_config();
    if (config == NULL) {
        printf("Failed to open configuration files. Abort.\n");
        return -1 ;
    }

    if ( flexiblas_mgmt_set_default(config, loc, name)) {
        printf("Failed to set default BLAS in %s to %s.\n", flexiblas_mgmt_location_to_string(loc), name);
        flexiblas_mgmt_free_config(config);
        return -1;
    }
    ret = flexiblas_mgmt_write_config2(config,loc);
    flexiblas_mgmt_free_config(config);
    if ( ret ) {
        printf("Failed to write the configuration to %s.\n", flexiblas_mgmt_location_to_string(loc));
        return -1;
    }
    return 0;
}


int list_all_blas()
{
    int ecode = 0;
    flexiblas_mgmt_t *config;
    int i;

    config = flexiblas_mgmt_load_config();

    if (config == NULL) {
        printf("Failed to open configuration files. Abort.\n");
        return -1 ;
    }

    for ( i = 0; i < FLEXIBLAS_MGMT_LOCATION_COUNT; i++){
        if ( i == 0) {
            if (!pipe_output) printf("System-wide:\n");
            ecode += print_blas(config, FLEXIBLAS_GLOBAL, "system");
        } else if ( i == 1) {
            if (!pipe_output) printf("System-wide (config directory):\n");
            ecode += print_blas(config, FLEXIBLAS_GLOBAL_DIR, "system-dir");
        } else if ( i == 2){
            if (!pipe_output) printf("User config:\n");
            ecode += print_blas(config, FLEXIBLAS_USER, "user");
        } else if ( i == 3){
            if (!pipe_output) printf("Host config:\n");
            ecode += print_blas(config, FLEXIBLAS_HOST, "host");
        } else if ( i == 4){
            if (!pipe_output) printf("Enviroment config:\n");
            ecode += print_blas(config, FLEXIBLAS_ENV, "enviroment");
        }
    }
    flexiblas_mgmt_free_config(config);
    return ecode;
}

int print_blas(flexiblas_mgmt_t *config, flexiblas_mgmt_location_t loc, char *where )
{
    void *iter_helper = NULL;
    char name[FLEXIBLAS_MGMT_MAX_BUFFER_LEN];
    char library_name[FLEXIBLAS_MGMT_MAX_BUFFER_LEN];
    char comment[FLEXIBLAS_MGMT_MAX_BUFFER_LEN];

    iter_helper = NULL;
    while ( flexiblas_mgmt_list_blas(config, loc,  name, library_name, comment, &iter_helper) > 0)
    {
        if ( pipe_output ) {
            printf("%s|%s|%s|%s\n", where, name, library_name, comment);
        } else {
            printf(" %s\n", name);
            if (strlen(library_name) == 0) {
                printf("   not usable, library not set.\n");
            }  else {
                printf("   library = %s\n", library_name);
            }
            printf("   comment = %s\n", comment);
        }
    }

    return 0;
}


