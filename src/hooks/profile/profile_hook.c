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

#include "cscutils/table.h"

#include "flexiblas_backend.h"
#include "flexiblas_real_calls.h"

#include "profile_hook.h"

int __profile_verbose = 0;

FLEXIBLAS_HOOK_OPTIONS(
    FLEXIBLAS_HOOK_OPTION("output", "Output file for profiling.", FLEXIBLAS_OPTIONS_STRING, "flexiblas_profile.txt"),
    FLEXIBLAS_HOOK_OPTIONS_END
);


FLEXIBLAS_HOOK_REGISTER(
        "Profile",  // Name of the Hook
        "PROFILE",  // Name if the section in the config
        profile,
        "This hook counts all function calls.", // Description
        "Martin Koehler");



blas_calls_t *data = NULL;
static char *profile_file;

FLEXIBLAS_HOOK_INIT_FUNCTION () {
    flexiblas_mgmt_t *mgmt;
    __profile_verbose = flexiblas_verbosity();

    data = (blas_calls_t *) malloc(sizeof(blas_calls_t) * (1));
    memset(data, '\0', sizeof(blas_calls_t));
    if (!data) {
        /* DPRINTFP_ERROR(0,"flexiblas-profile", "Failed to allocate memory for profiling data. Abort\n"); */
        abort();
    }

    mgmt = flexiblas_mgmt();
    if ( mgmt == NULL ) {
        /* DPRINTFP_ERROR(0, "flexiblas-profile", "Failed to load FlexiBLAS runtime configuration. Abort.\n"); */
        abort();
    }

    if (getenv(ENV_FLEXIBLAS_PROFILE_FILE) != NULL) {
        profile_file = strdup(getenv(ENV_FLEXIBLAS_PROFILE_FILE));
    } else {
        char *v = FLEXIBLAS_HOOK_GET_OPTION_STRING(profile, "output");
        profile_file= strdup(v);
    }
    /* DPRINTFP(1, "flexiblas-profile", "Use %s for output.\n", profile_file); */
}


FLEXIBLAS_HOOK_EXIT_FUNCTION () {
    csc_table_t *tab = csc_table_new(0);
    int col_name, col_calls, col_time;
    if ( ! tab) goto end;

    col_name = csc_table_add_column(tab, "Function", CSC_TABLE_STRING, CSC_TABLE_LEFT);
    col_calls= csc_table_add_column(tab, "Number of Calls", CSC_TABLE_INTEGER, CSC_TABLE_RIGHT);
    col_time = csc_table_add_column(tab, "Acc. Time", CSC_TABLE_FLOAT, CSC_TABLE_RIGHT);

    /* Single Precision Calls */
    ADD_BLAS_ENTRY( sasum ) ;
    ADD_BLAS_ENTRY( saxpy ) ;
    ADD_BLAS_ENTRY( scabs1 ) ;
    ADD_BLAS_ENTRY( scopy ) ;
    ADD_BLAS_ENTRY( sdot ) ;
    ADD_BLAS_ENTRY( sdsdot ) ;
    ADD_BLAS_ENTRY( sgbmv ) ;
    ADD_BLAS_ENTRY( sgemm ) ;
    ADD_BLAS_ENTRY( sgemv ) ;
    ADD_BLAS_ENTRY( sger ) ;
    ADD_BLAS_ENTRY( snrm2 ) ;
    ADD_BLAS_ENTRY( srot ) ;
    ADD_BLAS_ENTRY( srotg ) ;
    ADD_BLAS_ENTRY( srotm ) ;
    ADD_BLAS_ENTRY( srotmg ) ;
    ADD_BLAS_ENTRY( ssbmv ) ;
    ADD_BLAS_ENTRY( sscal ) ;
    ADD_BLAS_ENTRY( sspmv ) ;
    ADD_BLAS_ENTRY( sspr2 ) ;
    ADD_BLAS_ENTRY( sspr ) ;
    ADD_BLAS_ENTRY( sswap ) ;
    ADD_BLAS_ENTRY( ssymm ) ;
    ADD_BLAS_ENTRY( ssymv ) ;
    ADD_BLAS_ENTRY( ssyr2 ) ;
    ADD_BLAS_ENTRY( ssyr2k ) ;
    ADD_BLAS_ENTRY( ssyr ) ;
    ADD_BLAS_ENTRY( ssyrk ) ;
    ADD_BLAS_ENTRY( stbmv ) ;
    ADD_BLAS_ENTRY( stbsv ) ;
    ADD_BLAS_ENTRY( stpmv ) ;
    ADD_BLAS_ENTRY( stpsv ) ;
    ADD_BLAS_ENTRY( strmm ) ;
    ADD_BLAS_ENTRY( strmv ) ;
    ADD_BLAS_ENTRY( strsm ) ;
    ADD_BLAS_ENTRY( strsv ) ;
    ADD_BLAS_ENTRY( saxpby ) ;
    ADD_BLAS_ENTRY( sgeadd ) ;
    ADD_BLAS_ENTRY( simatcopy );
    ADD_BLAS_ENTRY( somatcopy );


    /* Complex Single Precision Calls */
    ADD_BLAS_ENTRY( scasum );
    ADD_BLAS_ENTRY( scnrm2 );


    ADD_BLAS_ENTRY( caxpy );
    ADD_BLAS_ENTRY( ccopy );
    ADD_BLAS_ENTRY( cdotc );
    ADD_BLAS_ENTRY( cdotu );
    ADD_BLAS_ENTRY( cgbmv );
    ADD_BLAS_ENTRY( cgemm );
    ADD_BLAS_ENTRY( cgemv );
    ADD_BLAS_ENTRY( cgerc );
    ADD_BLAS_ENTRY( cgeru );
    ADD_BLAS_ENTRY( chbmv );
    ADD_BLAS_ENTRY( chemm );
    ADD_BLAS_ENTRY( chemv );
    ADD_BLAS_ENTRY( cher );
    ADD_BLAS_ENTRY( cher2 );
    ADD_BLAS_ENTRY( cher2k );
    ADD_BLAS_ENTRY( cherk );
    ADD_BLAS_ENTRY( chpmv );
    ADD_BLAS_ENTRY( chpr );
    ADD_BLAS_ENTRY( chpr2 );
    ADD_BLAS_ENTRY( crotg );
    ADD_BLAS_ENTRY( csrot );
    ADD_BLAS_ENTRY( cscal );
    ADD_BLAS_ENTRY( csscal );
    ADD_BLAS_ENTRY( cswap );
    ADD_BLAS_ENTRY( csymm );
    ADD_BLAS_ENTRY( csyr2k );
    ADD_BLAS_ENTRY( csyrk );
    ADD_BLAS_ENTRY( ctbmv );
    ADD_BLAS_ENTRY( ctbsv );
    ADD_BLAS_ENTRY( ctpmv );
    ADD_BLAS_ENTRY( ctpsv );
    ADD_BLAS_ENTRY( ctrmm );
    ADD_BLAS_ENTRY( ctrmv );
    ADD_BLAS_ENTRY( ctrsm );
    ADD_BLAS_ENTRY( ctrsv );

    ADD_BLAS_ENTRY( caxpby ) ;
    ADD_BLAS_ENTRY( cgeadd ) ;
    ADD_BLAS_ENTRY( cimatcopy );
    ADD_BLAS_ENTRY( comatcopy );



    /* Double Precision Calls */
    ADD_BLAS_ENTRY( dasum ) ;
    ADD_BLAS_ENTRY( daxpy ) ;
    ADD_BLAS_ENTRY( dcabs1 ) ;
    ADD_BLAS_ENTRY( dcopy ) ;
    ADD_BLAS_ENTRY( ddot ) ;
    ADD_BLAS_ENTRY( dgbmv ) ;
    ADD_BLAS_ENTRY( dgemm ) ;
    ADD_BLAS_ENTRY( dgemv ) ;
    ADD_BLAS_ENTRY( dger ) ;
    ADD_BLAS_ENTRY( dnrm2 ) ;
    ADD_BLAS_ENTRY( drot ) ;
    ADD_BLAS_ENTRY( drotg ) ;
    ADD_BLAS_ENTRY( drotm ) ;
    ADD_BLAS_ENTRY( drotmg ) ;
    ADD_BLAS_ENTRY( dsbmv ) ;
    ADD_BLAS_ENTRY( dscal ) ;
    ADD_BLAS_ENTRY( dsdot ) ;
    ADD_BLAS_ENTRY( dspmv ) ;
    ADD_BLAS_ENTRY( dspr2 ) ;
    ADD_BLAS_ENTRY( dspr ) ;
    ADD_BLAS_ENTRY( dswap ) ;
    ADD_BLAS_ENTRY( dsymm ) ;
    ADD_BLAS_ENTRY( dsymv ) ;
    ADD_BLAS_ENTRY( dsyr2 ) ;
    ADD_BLAS_ENTRY( dsyr2k ) ;
    ADD_BLAS_ENTRY( dsyr ) ;
    ADD_BLAS_ENTRY( dsyrk ) ;
    ADD_BLAS_ENTRY( dtbmv ) ;
    ADD_BLAS_ENTRY( dtbsv ) ;
    ADD_BLAS_ENTRY( dtpmv ) ;
    ADD_BLAS_ENTRY( dtpsv ) ;
    ADD_BLAS_ENTRY( dtrmm ) ;
    ADD_BLAS_ENTRY( dtrmv ) ;
    ADD_BLAS_ENTRY( dtrsm ) ;
    ADD_BLAS_ENTRY( dtrsv ) ;
    ADD_BLAS_ENTRY( daxpby ) ;
    ADD_BLAS_ENTRY( dgeadd ) ;
    ADD_BLAS_ENTRY( dimatcopy );
    ADD_BLAS_ENTRY( domatcopy );

    /* Complex Double Precision Calls  */
    ADD_BLAS_ENTRY( dzasum );
    ADD_BLAS_ENTRY( dznrm2 );

    ADD_BLAS_ENTRY( zaxpy ) ;
    ADD_BLAS_ENTRY( zcopy ) ;
    ADD_BLAS_ENTRY( zdotc ) ;
    ADD_BLAS_ENTRY( zdotu ) ;
    ADD_BLAS_ENTRY( zdrot ) ;
    ADD_BLAS_ENTRY( zdscal ) ;
    ADD_BLAS_ENTRY( zgbmv ) ;
    ADD_BLAS_ENTRY( zgemm ) ;
    ADD_BLAS_ENTRY( zgemv ) ;
    ADD_BLAS_ENTRY( zgerc ) ;
    ADD_BLAS_ENTRY( zgeru ) ;
    ADD_BLAS_ENTRY( zhbmv ) ;
    ADD_BLAS_ENTRY( zhemm ) ;
    ADD_BLAS_ENTRY( zhemv ) ;
    ADD_BLAS_ENTRY( zher2 ) ;
    ADD_BLAS_ENTRY( zher2k ) ;
    ADD_BLAS_ENTRY( zher ) ;
    ADD_BLAS_ENTRY( zherk ) ;
    ADD_BLAS_ENTRY( zhpmv ) ;
    ADD_BLAS_ENTRY( zhpr2 ) ;
    ADD_BLAS_ENTRY( zhpr ) ;
    ADD_BLAS_ENTRY( zrotg ) ;
    ADD_BLAS_ENTRY( zscal ) ;
    ADD_BLAS_ENTRY( zswap ) ;
    ADD_BLAS_ENTRY( zsymm ) ;
    ADD_BLAS_ENTRY( zsyr2k ) ;
    ADD_BLAS_ENTRY( zsyrk ) ;
    ADD_BLAS_ENTRY( ztbmv ) ;
    ADD_BLAS_ENTRY( ztbsv ) ;
    ADD_BLAS_ENTRY( ztpmv ) ;
    ADD_BLAS_ENTRY( ztpsv ) ;
    ADD_BLAS_ENTRY( ztrmm ) ;
    ADD_BLAS_ENTRY( ztrmv ) ;
    ADD_BLAS_ENTRY( ztrsm ) ;
    ADD_BLAS_ENTRY( ztrsv ) ;


    ADD_BLAS_ENTRY( zaxpby ) ;
    ADD_BLAS_ENTRY( zgeadd ) ;
    ADD_BLAS_ENTRY( zimatcopy );
    ADD_BLAS_ENTRY( zomatcopy );

    /* Add LAPACK  */
#ifdef FLEXIBLAS_LAPACK
    profile_lapack_add(tab, col_name, col_calls, col_time);
#endif

    csc_table_comment_date(tab);

    DPRINTFP(0,"flexiblas-profile", "Write profile to %s\n", profile_file);
    if ( strcmp(profile_file, "stdout") == 0 ) {
        csc_table_print_ascii(stdout, tab, "   ");
    } else if (strcmp(profile_file, "stderr" ) == 0 ) {
        csc_table_print_ascii(stderr, tab, "   ");
    } else {
        FILE * fp = fopen(profile_file, "w");
        if ( !fp ) {
            /* DPRINTFP_ERROR(0, "flexiblas-profile", "failed to open %s\n", profile_file); */
            goto end;
        }
        csc_table_print_ascii(fp, tab, "    ");
        fclose(fp);
    }

end:
    if (tab) csc_table_destroy(tab);
    if (profile_file) free(profile_file);
    free(data);
}

