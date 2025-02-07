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
#include <math.h>
#include <string.h>

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <stdlib.h>
#include <limits.h>
#ifndef __WIN32__
#include <dlfcn.h>
#endif
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


int list_all_blas(void)
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


