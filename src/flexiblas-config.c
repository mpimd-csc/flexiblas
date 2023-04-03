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
