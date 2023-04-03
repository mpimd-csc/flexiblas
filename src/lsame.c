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




#include <ctype.h>
#include <stdint.h>
#include "flexiblas.h"
#include "flexiblas_fortran_mangle.h"

/* #if defined(__alpha__) || defined(__sparc64__) || defined(__x86_64__) || defined(__ia64__)
typedef int ftnlen;
typedef int logical;
#else
typedef int logical;
typedef int ftnlen;
#endif */

#ifdef INTEGER8
typedef int64_t logical;
typedef int ftnlen;
typedef int64_t pint;
#else
typedef int logical;
typedef int ftnlen;
typedef int pint;
#endif


#ifdef FLEXIBLAS_ABI_IBM
logical lsame_(char *ca, char *cb, ftnlen ca_len, ftnlen cb_len)
    __attribute__((alias(MTS(FC_GLOBAL(lsame,LSAME)))));
logical lsamen_(pint * n, char *ca, char *cb, ftnlen ca_len, ftnlen cb_len)
    __attribute__((alias(MTS(FC_GLOBAL(lsamen,LSAMEN)))));
#else
#ifndef __APPLE__
logical lsame(char *ca, char *cb, ftnlen ca_len, ftnlen cb_len)
    __attribute__((alias(MTS(FC_GLOBAL(lsame,LSAME)))));
logical lsamen(pint * n, char *ca, char *cb, ftnlen ca_len, ftnlen cb_len)
    __attribute__((alias(MTS(FC_GLOBAL(lsamen,LSAMEN)))));
#endif 
#endif

logical FC_GLOBAL(lsame,LSAME)(char *ca, char *cb, ftnlen ca_len, ftnlen cb_len)
{
	char a, b;
    ca_len = ca_len;
    cb_len = cb_len;
	a = (char)tolower(ca[0]);
	b = (char)tolower(cb[0]);
	return (a==b);
}

logical FC_GLOBAL(lsamen,LSAMEN)(pint *len, char *ca, char *cb, ftnlen ca_len, ftnlen cb_len) {
    ftnlen i  = 0;

    ca_len = ca_len;
    cb_len = cb_len;

    for (i = 0; i < *len; i++) {
        if (!lsame_(&ca[i],&cb[i],1,1))
            return 0;
    }
    return 1;
}

