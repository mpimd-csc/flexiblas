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




#include "flexiblas.h"
#include <errno.h>
#include <stddef.h>
#include <string.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include "cscutils/strutils.h"

static int color_output = 1;

/*
 * Color Definitions
 */
#define COLOR_RED "\033[1;2;31m"
#define COLOR_CYAN "\033[0;2;36m"
#define COLOR_GREEN "\033[0;2;32m"

#define COLOR_RESET "\033[0m"

static char * make_message(const char *fmt, va_list ap)
{
    int size = 0;
    char *p = NULL;
    va_list backup;

    /* Determine required size */

    va_copy(backup, ap);
    size = vsnprintf(p, size, fmt, backup);
    va_end(backup);

    if (size < 0)
        return NULL;

    size++;             /* For '\0' */
    p = malloc(size);
    if (p == NULL)
        return NULL;

    size = vsnprintf(p, size, fmt, ap);
    if (size < 0) {
        free(p);
        return NULL;
    }

    return p;
}

int flexiblas_get_color_output(void) {
    return color_output;
}

void flexiblas_set_color_output(int s) {
    color_output = s;
}

void flexiblas_print_error(const char *prefix, const char *path, const int line, const char *fmt, ... )
{
    va_list ap;
    char *localbuffer = NULL;
    char *message = NULL;
    size_t len;


    va_start(ap, fmt);
    message = make_message(fmt, ap);
    va_end(ap);
    if ( ! message ) {
        return;
    }

    if ( path == NULL) {
        len = 3 * (strlen(prefix) + strlen(PRINT_PREFIX COLOR_RESET COLOR_RED)) + strlen(message) + 2 + 12;
    }else {
         len = 3 * (strlen(prefix) + strlen(PRINT_PREFIX COLOR_RESET COLOR_RED)) + strlen(message) + 2 + strlen(path) + 12;
    }

    localbuffer = (char *) malloc(sizeof(char) * (len));
    if (!localbuffer) {
        free(message);
        return;
    }


    if ( color_output ) {
#ifdef DEBUG
        snprintf(localbuffer, len, "%s<%s:%s:%05d> %s%s", COLOR_RED, prefix, path, line, message, COLOR_RESET);
#else
        snprintf(localbuffer, len, "%s%s %s%s", COLOR_RED, prefix , message, COLOR_RESET);
#endif
    } else {
#ifdef DEBUG
        snprintf(localbuffer, len, "<%s:%s:%05d> %s", prefix, path, line, message);
#else
        snprintf(localbuffer, len, "%s %s", prefix , message);
#endif
    }
    fprintf(stderr, "%s", localbuffer);
    free(localbuffer);
    free(message);
    return;
}

void flexiblas_print_warning(const char *prefix, const char *fmt, ... )
{
    va_list ap;
    char *localbuffer = NULL;
    char *message = NULL;
    size_t len;


    va_start(ap, fmt);
    message = make_message(fmt, ap);
    va_end(ap);
    if ( ! message ) {
        return;
    }

    len = 3 * (strlen(prefix) + strlen(PRINT_PREFIX COLOR_RESET COLOR_RED)) + strlen(message) + 2 + 12;
    localbuffer = (char *) malloc(sizeof(char) * (len));
    if (!localbuffer) {
        free(message);
        return;
    }

    if ( color_output ) {
        snprintf(localbuffer, len, "%s<%s> %s%s", COLOR_CYAN, prefix, message, COLOR_RESET);
    } else {
        snprintf(localbuffer, len, "<%s> %s", prefix , message);
    }
    fprintf(stderr, "%s", localbuffer);
    free(localbuffer);
    free(message);
    return;
}

void flexiblas_print_info(const char *prefix, const char *fmt, ... )
{
    va_list ap;
    char *localbuffer = NULL;
    char *message = NULL;
    size_t len;


    va_start(ap, fmt);
    message = make_message(fmt, ap);
    va_end(ap);
    if ( ! message ) {
        return;
    }

    len = 3 * (strlen(prefix) + strlen(PRINT_PREFIX COLOR_RESET COLOR_RED)) + strlen(message) + 2 + 12;
    localbuffer = (char *) malloc(sizeof(char) * (len));
    if (!localbuffer) {
        free(message);
        return;
    }

    if ( color_output ) {
        snprintf(localbuffer, len, "<%s> %s", prefix, message);
    } else {
        snprintf(localbuffer, len, "<%s> %s", prefix, message);
    }
    fprintf(stderr, "%s", localbuffer);
    fflush(stderr);
    free(localbuffer);
    free(message);
    return;
}


