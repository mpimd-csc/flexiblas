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

HIDDEN char *__flexiblas_getenv(int what) {
    static char container[MAX_BUFFER_SIZE];
    container[0] = '\0';
    switch (what) {
        case FLEXIBLAS_ENV_SO_EXTENSION:
#ifdef __APPLE__
            snprintf(container, MAX_BUFFER_SIZE, ".dylib");
#else
#ifdef __WIN32__
            snprintf(container, MAX_BUFFER_SIZE, ".dll");
#else
            snprintf(container, MAX_BUFFER_SIZE, ".so");
#endif
#endif
            break;
        case FLEXIBLAS_ENV_HOMEDIR:
#ifdef __WIN32__
            snprintf(container,MAX_BUFFER_SIZE,"%s\\%s\\",getenv("HOMEDRIVE"),getenv("HOMEPATH"));
#else
            snprintf(container,MAX_BUFFER_SIZE,"%s",getenv("HOME"));
#endif
            break;
        case FLEXIBLAS_ENV_GLOBAL_RC:
            __flexiblas_get_global_rc_path(container, MAX_BUFFER_SIZE, FLEXIBLAS_RC);
            break;
        case FLEXIBLAS_ENV_GLOBAL_RC_DIR:
            __flexiblas_get_global_rc_path(container, MAX_BUFFER_SIZE, FLEXIBLAS_RC_DIR);
                break;
        case FLEXIBLAS_ENV_USER_RC:
#ifdef __WIN32__
            snprintf(container,MAX_BUFFER_SIZE,"%s\\%s",getenv("APPDATA"), FLEXIBLAS_RC);
#else
            snprintf(container,MAX_BUFFER_SIZE,"%s/.%s", getenv("HOME"), FLEXIBLAS_RC);
#endif
            break;
        case FLEXIBLAS_ENV_HOST_RC:
#ifdef __WIN32__
            snprintf(container, MAX_BUFFER_SIZE, "None");
#else
            {
                char hostname[MAX_BUFFER_SIZE-20];
                gethostname(hostname, MAX_BUFFER_SIZE-20);
                snprintf(container,MAX_BUFFER_SIZE,"%s/.%s.%s", getenv("HOME"), FLEXIBLAS_RC, hostname);
                csc_str_remove_char(container, '"');
                csc_str_remove_char(container, '\"');

            }
#endif
            break;


        default:
            return NULL;
    }
    return strdup(container);
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




/*-----------------------------------------------------------------------------
 *  Other Stuff
 *-----------------------------------------------------------------------------*/
HIDDEN int __flexiblas_insert_fallback_blas(flexiblas_mgmt_t *config)
{
    int ret = 0;
    char *SO_EXTENSION = __flexiblas_getenv(FLEXIBLAS_ENV_SO_EXTENSION);
    size_t len=strlen(FALLBACK_NAME)+strlen(SO_EXTENSION)+2;
    char *tmp = (char *) calloc(len,sizeof(char));
    char *tmp2;

    snprintf(tmp,len, "%s%s", FALLBACK_NAME,SO_EXTENSION);
#if defined(__WIN32__)
    len = strlen("flexiblas_netlib")+strlen(SO_EXTENSION)+2;
#else
    len = strlen("libflexiblas_netlib")+strlen(SO_EXTENSION)+2;
#endif
    tmp2 = (char*) calloc(len,sizeof(char));
#if defined(__WIN32__)
    snprintf(tmp2,len, "flexiblas_netlib%s", SO_EXTENSION);
#else
    snprintf(tmp2,len, "libflexiblas_netlib%s", SO_EXTENSION);
#endif

    if ( flexiblas_mgmt_blas_add(config, FLEXIBLAS_GLOBAL, "NETLIB", tmp2, NULL)){
        DPRINTF(0,"Can not insert Netlib BLAS library.\n");
        ret ++;
    }
    if (  flexiblas_mgmt_blas_add(config, FLEXIBLAS_GLOBAL, "__FALLBACK__", tmp2, NULL)) {
        DPRINTF(0,"Can not insert Netlib BLAS library as fallback.\n");
        ret++;
    }

    free(tmp);
    free(tmp2);
    free(SO_EXTENSION);
    return ret;
}

