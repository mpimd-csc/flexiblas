/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) Martin Koehler, 2013-2020
 */


#include <ctype.h>
#include <stdint.h>
#include "flexiblas.h"
#include "fortran_mangle.h"

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
#else 
typedef int logical;
typedef int ftnlen;
#endif


#ifdef FLEXIBLAS_ABI_IBM 
logical lsame_(char *ca, char *cb, ftnlen ca_len, ftnlen cb_len) 
    __attribute__((alias(MTS(FC_GLOBAL(lsame,LSAME))))); 
logical lsamen_(ftnlen * n, char *ca, char *cb, ftnlen ca_len, ftnlen cb_len)  
    __attribute__((alias(MTS(FC_GLOBAL(lsamen,LSAMEN))))); 
#else 
logical lsame(char *ca, char *cb, ftnlen ca_len, ftnlen cb_len) 
    __attribute__((alias(MTS(FC_GLOBAL(lsame,LSAME))))); 
logical lsamen(ftnlen * n, char *ca, char *cb, ftnlen ca_len, ftnlen cb_len)  
    __attribute__((alias(MTS(FC_GLOBAL(lsamen,LSAMEN))))); 

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

logical FC_GLOBAL(lsamen,LSAMEN)(ftnlen *len, char *ca, char *cb, ftnlen ca_len, ftnlen cb_len) {
    ftnlen i  = 0; 

    ca_len = ca_len;
    cb_len = cb_len;

    for (i = 0; i < *len; i++) {
        if (!lsame_(&ca[i],&cb[i],1,1))
            return 0; 
    }
    return 1; 
}

