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


