//    SPDX-License-Identifier: LGPL-3.0-or-later
/*
    This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
    Copyright (C) 2013-2024 Martin Koehler

    This program is free software: you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by the Free
    Software Foundation, either version 3 of the License, or (at your option)
    any later version.

    This program is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
    FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
    more details.

    You should have received a copy of the GNU General Public License along
    with this program. If not, see <https://www.gnu.org/licenses/>.
 */



#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "flexiblas_config.h"

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

    /* LD related */
    if (strcmp(argv[1], "--ldflags") == 0) {
        printf("-L%s -l%s", CMAKE_INSTALL_FULL_LIBDIR, FLEXIBLAS_LIBRARY_NAME);
    }
    if (strcmp(argv[1], "--libdir") == 0) {
        printf("%s", CMAKE_INSTALL_FULL_LIBDIR);
    }


    /* Compile time related */
    if (strcmp(argv[1], "--incdir") == 0 ) {
        printf("%s/%s", CMAKE_INSTALL_FULL_INCLUDEDIR, FLEXIBLAS_LIBRARY_NAME);
    }

    if (strcmp(argv[1], "--cflags") == 0 ) {
        printf("-I%s/%s",CMAKE_INSTALL_FULL_INCLUDEDIR, FLEXIBLAS_LIBRARY_NAME);
        #ifdef FLEXIBLAS_INTEGER8
        printf(" -DFLEXIBLAS_INTEGER8");
        #else
        printf(" -UFLEXIBLAS_INTEGER8");
        #endif

    }

    if (strcmp(argv[1], "--fcflags") == 0 ) {
        printf("-I%s/%s",CMAKE_INSTALL_FULL_INCLUDEDIR, FLEXIBLAS_LIBRARY_NAME);
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

    if (strcmp(argv[1],"--backenddir") == 0 ) {
        printf("%s/%s", CMAKE_INSTALL_FULL_LIBDIR, FLEXIBLAS_LIBRARY_DIR);
    }

    if (strcmp(argv[1],"--sysconfdir") == 0 ) {
        printf("%s", CMAKE_INSTALL_FULL_SYSCONFDIR);
    }
    if (strcmp(argv[1],"--rcdir") == 0 ) {
        printf("%s/%s", CMAKE_INSTALL_FULL_SYSCONFDIR, FLEXIBLAS_RC_DIR);
    }



    /* Misc  */
    if ( strcmp(argv[1],"--version") == 0) {
        printf("%d.%d.%d", FLEXIBLAS_VERSION_MAJOR, FLEXIBLAS_VERSION_MINOR, FLEXIBLAS_VERSION_PATCH);
    }




    return 0;
}
