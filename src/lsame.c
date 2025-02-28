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



#include <ctype.h>
#include <stdint.h>
#include "flexiblas.h"
#include "flexiblas_config.h"
#include "flexiblas_fortran_mangle.h"

/* #if defined(__alpha__) || defined(__sparc64__) || defined(__x86_64__) || defined(__ia64__)
   typedef int ftnlen;
   typedef int logical;
#else
typedef int logical;
typedef int ftnlen;
#endif */

#ifdef FLEXIBLAS_INTEGER8
typedef int64_t logical;
typedef int ftnlen;
typedef int64_t pint;
#else
typedef int logical;
typedef int ftnlen;
typedef int pint;
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
        if (!FC_GLOBAL_(lsame,LSAME)(&ca[i],&cb[i],1,1))
            return 0;
    }
    return 1;
}

logical FC_GLOBAL2(lsame,LSAME)(char *ca, char *cb, ftnlen ca_len, ftnlen cb_len)
{
    char a, b;
    ca_len = ca_len;
    cb_len = cb_len;
    a = (char)tolower(ca[0]);
    b = (char)tolower(cb[0]);
    return (a==b);
}

logical FC_GLOBAL2(lsamen,LSAMEN)(pint *len, char *ca, char *cb, ftnlen ca_len, ftnlen cb_len) {
    ftnlen i  = 0;

    ca_len = ca_len;
    cb_len = cb_len;

    for (i = 0; i < *len; i++) {
        if (!FC_GLOBAL_(lsame,LSAME)(&ca[i],&cb[i],1,1))
            return 0;
    }
    return 1;
}

logical FC_GLOBAL3(lsame,LSAME)(char *ca, char *cb, ftnlen ca_len, ftnlen cb_len)
{
    char a, b;
    ca_len = ca_len;
    cb_len = cb_len;
    a = (char)tolower(ca[0]);
    b = (char)tolower(cb[0]);
    return (a==b);
}

logical FC_GLOBAL3(lsamen,LSAMEN)(pint *len, char *ca, char *cb, ftnlen ca_len, ftnlen cb_len) {
    ftnlen i  = 0;

    ca_len = ca_len;
    cb_len = cb_len;

    for (i = 0; i < *len; i++) {
        if (!FC_GLOBAL_(lsame,LSAME)(&ca[i],&cb[i],1,1))
            return 0;
    }
    return 1;
}

