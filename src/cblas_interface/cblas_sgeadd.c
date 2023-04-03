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




#include "cblas.h"
#include "cblas_f77.h"
#include "../flexiblas.h"
#include "cblas_flexiblas.h"


static TLS_STORE uint8_t hook_cblas_sgeadd_pos = 0;

void cblas_sgeadd(const CBLAS_ORDER CORDER,
        const CBLAS_INT crows, const CBLAS_INT ccols, const float calpha, float *a, const CBLAS_INT clda,
        const float cbeta, float *b, const CBLAS_INT cldb)
{
    void (*fn)(const CBLAS_ORDER, const CBLAS_INT, const CBLAS_INT, const float, float *, const CBLAS_INT, const float, float *, const CBLAS_INT);
    CBLAS_BACKEND_INIT();
    CBLAS_HOOK_SELECT(sgeadd);
    fn(CORDER, crows, ccols, calpha, a, clda, cbeta, b, cldb);

}

void flexiblas_chain_cblas_sgeadd(const CBLAS_ORDER CORDER,
        const CBLAS_INT crows, const CBLAS_INT ccols, const float calpha, float *a, const CBLAS_INT clda,
        const float cbeta, float *b, const CBLAS_INT cldb)
{
    void (*fn)(const CBLAS_ORDER, const CBLAS_INT, const CBLAS_INT, const float, float *, const CBLAS_INT, const float, float *, const CBLAS_INT);
    CBLAS_HOOK_ADVANCE(sgeadd);
    fn(CORDER, crows, ccols, calpha, a, clda, cbeta, b, cldb);

}

void flexiblas_real_cblas_sgeadd(const CBLAS_ORDER CORDER,
        const CBLAS_INT crows, const CBLAS_INT ccols, const float calpha, float *a, const CBLAS_INT clda,
        const float cbeta, float *b, const CBLAS_INT cldb)
{
#ifdef F77_INT
    F77_INT F77_LDA =clda;
    F77_INT F77_LDB =cldb;
#else
#define F77_LDA  clda
#define F77_LDB  cldb
#endif

    if ( current_backend->blas.sgeadd.cblas_function != NULL ) {
        void (*fn)(const CBLAS_ORDER, const CBLAS_INT, const CBLAS_INT, const float, float *, const CBLAS_INT, const float, float *, const CBLAS_INT);
        *(void **)  &fn = current_backend->blas.sgeadd.cblas_function;
        fn(CORDER, crows, ccols, calpha, a, clda, cbeta, b, cldb);
    } else {
#ifdef F77_INT
        F77_INT t = 0;
        F77_INT rows = crows;
        F77_INT cols = ccols;
#else
        CBLAS_INT t = 0;
        CBLAS_INT rows = crows;
        CBLAS_INT cols = ccols;
#endif

        if ( CORDER == CblasRowMajor ) {
            t = rows;
            rows = cols;
            cols = t;
        }
        FC_GLOBAL(sgeadd,SGEADD)( &rows, &cols, &calpha, a, &F77_LDA, &cbeta, b, &F77_LDB);
    }
}

