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
 * Copyright (C) Martin Köhler, 2015
 */




#include <ctype.h>
#include <stdint.h>
#include "flexiblas.h"

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

#ifndef INTEGER8
int64_t lsame_(char *ca, char *cb, ftnlen ca_len, ftnlen cb_len)
{
	/* Local variables */
	char a, b; 
	int64_t ret = 0; 
	int32_t *ret2 = (int32_t *) &ret; 

	a = tolower(ca[0]); 
	b = tolower(cb[0]); 
	ret2[0] = (a==b); 
	ret2[1] = (a==b); 

	return ret; 
} 
#else 
logical lsame_(char *ca, char *cb, ftnlen ca_len, ftnlen cb_len)
{
	/* Local variables */
	char a, b; 

	a = tolower(ca[0]); 
	b = tolower(cb[0]); 
	return (a==b); 
} 

#endif 

int64_t lsame64_(char *ca, char *cb, ftnlen ca_len, ftnlen cb_len)
{
	/* Local variables */
	char a, b; 

	a = tolower(ca[0]); 
	b = tolower(cb[0]); 
	return (int64_t) (a==b); 
} 

int32_t lsame32_(char *ca, char *cb, ftnlen ca_len, ftnlen cb_len)
{
	/* Local variables */
	char a, b; 

	a = tolower(ca[0]); 
	b = tolower(cb[0]); 
	return (int32_t) (a==b); 
} 



logical lsame(char *ca, char *cb, ftnlen ca_len, ftnlen cb_len) __attribute__((alias("lsame_"))); 
int32_t lsame32(char *ca, char *cb, ftnlen ca_len, ftnlen cb_len) __attribute__((alias("lsame32_"))); 
int64_t lsame64(char *ca, char *cb, ftnlen ca_len, ftnlen cb_len) __attribute__((alias("lsame64_"))); 


