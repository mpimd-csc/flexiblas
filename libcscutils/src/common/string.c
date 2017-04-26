/* 
* CSCUTILS - A collection of various software routines uses in CSC projects
* Copyright (C) 2015 Martin Koehler
* 
* This library is free software; you can redistribute it and/or modify
* it under the terms of the GNU Lesser General Public License as published
* by the Free Software Foundation; either version 2.1 of the License, or
* (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
* GNU Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public License
* along with this library; if not, see <http://www.gnu.org/licenses/>.
* 
*/ 

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include "cscutils/strutils.h"

#define MIN(A,B) ((A)>(B))?(B):(A)

int csc_strcasecmp (const char *s1, const char *s2)
{
    while (*s1 && (tolower(*s1) == tolower(*s2)))
            s1++, s2++;
    return (tolower(*s1) - tolower(*s2));
}

int csc_strncmp(const char *s1, const char *s2, size_t n) 
{
    size_t len_a, pos_a = 0;
    size_t len_b, pos_b = 0;
    
    len_a = strnlen(s1,n);
    len_b = strnlen(s2,n);
    
    while ( pos_a < len_a && pos_b < len_b) {
        int res=tolower(s1[pos_a])-tolower(s2[pos_b]);
        if (res) return res;
        ++pos_a; ++pos_b;
    }
    if ( len_a > len_b) return -1;
    return 1;
}

/* centering function */
void csc_strcenter(const char *str, int width, char *output)
{
    int len, ws, i;
    int lws; 
    len = strlen(str);
    if (len > width ) len = width;
    ws = width-len;
    lws = ws/2 + ws%2; 
    /* printf("width: %d, len %d ws %d str %s", width, len, ws, str); */
    for (i = 0; i < lws; i++) {
        output[i] = ' ';
    }
    snprintf(output + lws, len+1, "%s", str);
    for (i = 0; i < ws-lws; i++) {
        output[lws+len+i] = ' ';
    }
    output[ws+len] = '\0';
    /* printf("out: '%s'\n", output); */

    return;
}

void csc_strleftalign(const char *str, int width, char *output)
{
    int len, ws, i;
    len = strlen(str);
    if (len > width ) len = width ;
    ws = width-len;
    /* printf("width: %d, len %d ws %d str %s", width, len, ws, str); */
    snprintf(output, len+1, "%s", str);
    for (i = 0; i <ws; i++) {
        output[len+i] = ' ';
    }
    output[len+ws] = '\0';
    /* printf("out: '%s'\n", output); */

    return;
}

void csc_strrightalign(const char *str, int width, char *output)
{
    int len, ws, i;
    int lws; 
    len = strlen(str);
    if (len > width ) len = width;
    ws = width-len;
    lws = ws; 
    /* printf("width: %d, len %d ws %d str %s", width, len, ws, str); */
    for (i = 0; i < lws; i++) {
        output[i] = ' ';
    }
    snprintf(output + lws, len+1, "%s", str);
    output[ws+len] = '\0';
    /* printf("out: '%s'\n", output); */

    return;
}


