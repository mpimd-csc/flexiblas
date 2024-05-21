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


void csc_strremovedup(char * str, char dup)
{
    size_t len = strlen(str);
    size_t k = 0, l;
    while ( k < len -1 ) {
        if ( str[k] == str[k+1] && str[k] == dup) {
            for (l = k; l < len-1; l++) {
                str[l] = str[l+1];
            }
            str[len-1] = 0;
            len--;
        }
        k++;
    }
    return;
}

char *csc_strndup(const char *str, size_t size)
{
#ifdef CSC_HAVE_STRNDUP
    return strndup(str, size);
#else
    char* buff = malloc(size+1);
    if (buff) {
        strncpy(buff, str, size);
        buff[size] = '\0';
    }
    return buff;
#endif
}

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
        int res=s1[pos_a]-s2[pos_b];
        if (res) return res;
        ++pos_a; ++pos_b;
    }
    if ( len_a > len_b) return -1;
    return 1;
}

int csc_strncasecmp(const char *s1, const char *s2, size_t n)
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

int csc_strbegin(const char * haystack, const char *needle) {
    int hlen = strlen(haystack);
    int nlen = strlen(needle);
    int p;

    if ( nlen > hlen)
        return 0;

    for (p = 0; p < nlen; p++) {
        if (haystack[p] != needle[p])
            return 0;
    }
    return 1;
}


int csc_strcasebegin(const char * haystack, const char *needle) {
    int hlen = strlen(haystack);
    int nlen = strlen(needle);
    int p;

    if ( nlen > hlen)
        return 0;

    for (p = 0; p < nlen; p++) {
        if (tolower(haystack[p]) != tolower(needle[p]))
            return 0;
    }
    return 1;
}


char *csc_struppercase(char *str) {
	char *ret = str;
	if ( str == NULL ) return NULL;
	while (*str != '\0') {
		*str = toupper(*str);
		str++;
	}
	return ret;
}

char *csc_strlowercase(char *str) {
	char *ret = str;
	if ( str == NULL ) return NULL;
	while (*str != '\0') {
		*str = tolower(*str);
		str++;
	}
	return ret;
}


int csc_str_is_valid_int(const char *str)
{
	if ( str == NULL )
		return  0;

	if (*str == '-')
		++str;
	if (!*str)
		return 0;
	while (*str)
	{
		if (!isdigit(*str))
			return 0;
		else
			++str;
	}

	return 1;
}


char *csc_str_remove_char(char *str, char c)
{
    size_t k = 0, l = 0, len = 0 ;
    if (!str) return NULL;
    len = strlen(str);
    for ( k = 0;  k < len; k++)
    {
        if (str[k] == c ) {
            for (l = k+1; l < len+1; l++) {
                str[l-1] = str[l];
            }
        }
    }
    return str;
}

char *csc_str_ltrim(char *str)
{
    size_t k, len = 0 ;
    if (!str) return NULL;
    len = strlen(str);
    while (str[0] == ' ' || str[0] == '\t' || str[0] == '\n' || str[0] == '\r')
    {
        for(k=1; k < len+1; k++) str[k-1] = str[k];
        if (len > 0 ) len--;
    }
    return str;
}


char *csc_str_rtrim(char *str)
{
    size_t len = 0;
    if ( !str ) return NULL;
    len = strlen(str);
    while(len>0 && (str[len-1] == ' ' || str[len-1] == '\t' || str[len-1] == '\n' || str[len-1] == '\r')) len--;
    str[len] = 0;
    return str;
}

