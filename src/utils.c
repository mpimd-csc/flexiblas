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
#include "flexiblas.h"
#include "helper.h"
#define MAX_BUFFER_SIZE (4096 * 8)

#include <sys/types.h>
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#include <unistd.h>
#ifdef HAVE_SYSMACRO_H
#include <sys/sysmacros.h>
#endif

#include "cscutils/strutils.h"

HIDDEN int __flexiblas_file_exist(const char *path) {
    if ( path == NULL ) return 0;
    if( access( path, F_OK ) == 0  ) {
        return -1;
    } else {
        return 0;
    }
}

HIDDEN int __flexiblas_directory_exists(const char * path)
{
    struct stat statbuffer;
    if ( path == NULL) return 0;
    if ( stat(path, &statbuffer) == -1) return 0;
    if ( S_ISDIR(statbuffer.st_mode)) return 1;
    return 0;
}

HIDDEN int __flexiblas_str_endwith(const char * haystack, const char *needle )
{
    if (!haystack || !needle)
        return 0;
    size_t lenhaystack = strlen(haystack);
    size_t lenneedle = strlen(needle);
    if (lenneedle >  lenhaystack)
        return 0;
    return strncmp(haystack + lenhaystack - lenneedle, needle, lenneedle) == 0;
}


HIDDEN void __flexiblas_print_copyright (int prefix) {
    if (prefix){
        fprintf(stderr, "<%s>\n", PRINT_PREFIX );
        fprintf(stderr, "<" PRINT_PREFIX "> FlexiBLAS, version " FLEXIBLAS_VERSION "\n");
        fprintf(stderr, "<" PRINT_PREFIX "> Copyright (C) " FLEXIBLAS_YEARS " Martin Koehler and others.\n");
        fprintf(stderr, "<" PRINT_PREFIX "> This is free software; see the source code for copying conditions.\n");
        fprintf(stderr, "<" PRINT_PREFIX "> There is ABSOLUTELY NO WARRANTY; not even for MERCHANTABILITY or\n");
        fprintf(stderr, "<" PRINT_PREFIX "> FITNESS FOR A PARTICULAR PURPOSE.\n");
        fprintf(stderr, "<" PRINT_PREFIX "> \n");
    } else {
        printf("FlexiBLAS, version " FLEXIBLAS_VERSION "\n");
        printf("Copyright (C) " FLEXIBLAS_YEARS " Martin Koehler and others.\n");
        printf("This is free software; see the source code for copying conditions.\n");
        printf("There is ABSOLUTELY NO WARRANTY; not even for MERCHANTABILITY or\n");
        printf("FITNESS FOR A PARTICULAR PURPOSE.\n");
        printf("\n");
    }
    return;
}



static int mgmt_add_blas(flexiblas_mgmt_t *config, 
		char *name, 
		char *libname)
{
	int ret = 0;
	size_t len = strlen(libname);
	char *LIB_PREFIX = FLEXIBLAS_LIBRARY_PREFIX;
	char *SO_EXTENSION = __flexiblas_mgmt_getenv(FLEXIBLAS_ENV_SO_EXTENSION); 
	size_t ext_len = strlen(SO_EXTENSION);
	size_t prefix_len = strlen(LIB_PREFIX);
	
    	char *tmp = (char *) calloc(len+prefix_len+ext_len+10,sizeof(char));
    	snprintf(tmp,len+prefix_len+ext_len+10, "%s%s%s", LIB_PREFIX, libname, SO_EXTENSION);
	
	ret = flexiblas_mgmt_blas_add(config, FLEXIBLAS_GLOBAL, name, tmp, NULL);
	free(tmp);
	free(SO_EXTENSION);
	return ret;
}


/*-----------------------------------------------------------------------------
 *  Other Stuff
 *-----------------------------------------------------------------------------*/
HIDDEN int __flexiblas_insert_fallback_blas(flexiblas_mgmt_t *config)
{
    int ret = 0;
    int overall_ret = 0;

    ret = mgmt_add_blas(config, "NETLIB", "flexiblas_netlib");
    if ( ret ) {
         DPRINTF(0,"Can not insert Netlib BLAS library.\n");
	 overall_ret ++;
    }

    ret = mgmt_add_blas(config, "__FALLBACK__", FALLBACK_NAME);
    if ( ret ) {
         DPRINTF(0,"Can not insert the fall BLAS library (%s).\n", FALLBACK_NAME);
	 overall_ret ++;
    }
    return ret;
}

