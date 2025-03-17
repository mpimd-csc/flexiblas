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
#include "flexiblas_config.h"

#ifdef _WIN32
#include "windows_fixes.h"
#else
#include <libgen.h>
#endif

static void print_usage(const char * prgm)
{
    printf( "FlexiBLAS, version %s\n"
            "Copyright (C) %s Martin Koehler and others.\n"
            "This is free software; see the source code for copying conditions.\n"
            "There is ABSOLUTELY NO WARRANTY; not even for MERCHANTABILITY or\n"
            "FITNESS FOR A PARTICULAR PURPOSE.\n"
            "\n\n", FLEXIBLAS_VERSION, FLEXIBLAS_YEARS);

    printf( "The flexiblas_config tool provides information about the compile-time settings\n"
            "of flexiblas.\n\n");
    printf( "Usage: %s <option>\n"
            "\n"
            "Options:\n"
            "   --ldflags       - Shows the LDFLAGS to link against FlexiBLAS.\n"
            "   --libdir        - Shows the library directory where FlexiBLAS was installed.\n"
            "   --incdir        - Shows the include directory where FlexiBLAS' header files reside.\n"
            "   --cflags        - Shows the CFLAGS which are required to use FlexiBLAS' additional API.\n"
            "   --fcflags       - Shows additional flags when compiling and linking with Fortran code.\n"
            "   --i8cflag       - Shows the eventually required flag for compiling C code in 64 bit integer mode.\n"
            "   --i8fcflag      - Shows the eventually required flag for compiling Fortran code in 64 bit integer mode.\n"
            "   --i8            - Displays 1 if FlexiBLAS was compiled with 64 bit integer support.\n"
            "   --backenddir    - Shows the directory where the backends are installed.\n"
            "   --sysconfdir    - Shows the SYSCONFDIR used when compiling FlexiBLAS,\n"
            "   --rcdir         - Shows FlexiBLAS' sub directory in the SYSCONFDIR.\n"
            "\n", prgm);
    return;
}

int main(int argc, char *argv[])
{
    if ( argc != 2) {
        print_usage(argv[0]);
        exit(1);
    }

    char * folder = __flexiblas_get_library_location();
    char * libdir = dirname(folder);
    /* LD related */
    if (strcmp(argv[1], "--ldflags") == 0) {
            printf("-L%s -l%s", libdir, FLEXIBLAS_LIBRARY_NAME);
    }
    if (strcmp(argv[1], "--libdir") == 0) {
        printf("%s", libdir);
    }

    char * basedir = dirname(libdir);
    char * includedir = malloc(strlen(basedir) + 1 + strlen("include") + 1);
    strcpy(includedir, basedir);

    /* Compile time related */
    if (strcmp(argv[1], "--incdir") == 0 ) {
#if defined(__WIN32__)
        strcat(includedir, "\\include");
        printf("%s\\%s", includedir, FLEXIBLAS_LIBRARY_NAME);
#else
        strcat(includedir, "/include");
        printf("%s/%s", includedir, FLEXIBLAS_LIBRARY_NAME);
#endif
    }

    if (strcmp(argv[1], "--cflags") == 0 ) {
        printf("-I%s/%s",includedir, FLEXIBLAS_LIBRARY_NAME);
#ifdef FLEXIBLAS_INTEGER8
        printf(" -DFLEXIBLAS_INTEGER8");
#else
        printf(" -UFLEXIBLAS_INTEGER8");
#endif

    }

    if (strcmp(argv[1], "--fcflags") == 0 ) {
        printf("-I%s/%s",includedir, FLEXIBLAS_LIBRARY_NAME);
#ifdef FLEXIBLAS_INTEGER8
        printf(" -DFLEXIBLAS_INTEGER8");
#else
        printf(" -UFLEXIBLAS_INTEGER8");
#endif
#ifdef FLEXIBLAS_FC_I8FLAG
        printf(" %s", FLEXIBLAS_FC_I8FLAG);
#else
        printf(" ");
#endif
    }

    /* 64 Bit integer related */
    if (strcmp(argv[1], "--i8cflag") == 0) {
#ifdef FLEXIBLAS_INTEGER8
        printf(" -DFLEXIBLAS_INTEGER8");
#else
        printf(" -UFLEXIBLAS_INTEGER8");
#endif
    }

    if (strcmp(argv[1],"--i8fcflag") == 0) {
#ifdef FLEXIBLAS_FC_I8FLAG
        printf("%s", FLEXIBLAS_FC_I8FLAG);
#else
        printf(" ");
#endif
    }

    if (strcmp(argv[1],"--i8") == 0 ) {
#ifdef FLEXIBLAS_INTEGER8
        printf("1");
#else
        printf("0");
#endif
    }

    /* Directory related */

    char * backenddir = malloc(strlen(libdir) + 1 + strlen(FLEXIBLAS_LIBRARY_DIR) + 1);
    strcpy(backenddir, libdir);

    if (strcmp(argv[1],"--backenddir") == 0 ) {
#if defined(__WIN32__)
        strcat(backenddir, "\\");
        strcat(backenddir, FLEXIBLAS_LIBRARY_DIR);
        printf("%s\\%s", backenddir, FLEXIBLAS_LIBRARY_DIR);
#else
        strcat(backenddir, "/");
        strcat(backenddir, FLEXIBLAS_LIBRARY_DIR);
        printf("%s/%s", backenddir, FLEXIBLAS_LIBRARY_DIR);
#endif
    }

    char * sysconfdir = malloc(strlen(basedir) + 1 + strlen("etc") + 1);
    strcpy(sysconfdir, basedir);

    if (strcmp(argv[1],"--sysconfdir") == 0 ) {
#if defined(__WIN32__)
        strcat(sysconfdir, "\\etc");
#else
        strcat(sysconfdir, "/etc");
#endif
        printf("%s", sysconfdir);
    }

    char * rcdir = malloc(strlen(basedir) + 1 + strlen(FLEXIBLAS_RC_DIR) + 1);
    strcpy(rcdir, basedir);

    if (strcmp(argv[1],"--rcdir") == 0 ) {
#if defined(__WIN32__)
        strcat(rcdir, "\\");
        strcat(rcdir, FLEXIBLAS_RC_DIR);
        printf("%s", rcdir);
#else
        strcat(rcdir, "/");
        strcat(rcdir, FLEXIBLAS_RC_DIR);
        printf("%s", rcdir);
#endif
    }

    free(folder);
    free(includedir);
    free(backenddir);
    free(sysconfdir);
    free(rcdir);

    /* Misc  */
    if ( strcmp(argv[1],"--version") == 0) {
        printf("%d.%d.%d", FLEXIBLAS_VERSION_MAJOR, FLEXIBLAS_VERSION_MINOR, FLEXIBLAS_VERSION_PATCH);
    }

    return 0;
}
