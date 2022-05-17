/*
 * LIBCSCUTILS -- Helper routines of the CSC group
 * Copyright (C) Martin Koehler, 2022
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

#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <errno.h>

#include "cscutils/cscutils_config.h"
#include "cscutils/posix_impl.h"

ssize_t csc_getline(char** restrict lineptr, size_t* restrict n, FILE* restrict stream) {
    #if defined(CSC_HAVE_GETLINE)
        return getline(lineptr, n, stream);
    #else
        return csc_getdelim(lineptr, n, '\n', stream);
    #endif
}

ssize_t csc_getdelim(char** restrict lineptr, size_t* restrict n, int delim, FILE* restrict stream) {
    #if defined(CSC_HAVE_GETDELIM)
        return getdelim(lineptr, n, delim, stream);
    #else
        if ((lineptr == NULL) || (n == NULL) || (stream == NULL)) {
// EINVAL is POSIX.1-2001 and not part of the C standard
#if defined(EINVAL)
            errno = EINVAL;
#endif
            return -1;
        }

        size_t line_len = *n;
        char* line = *lineptr;

        if (line == NULL) {
            line_len = 1024;
            line = malloc(sizeof(char) * line_len);

            if (line == NULL) {
                *lineptr = line;
// ENOMEM is POSIX.1-2001 and not part of the C standard
#if defined(ENOMEM)
                errno = ENOMEM;
#endif
                return -1;
            }
        }

        int end = 0;
        size_t offset = 0;

        if (feof(stream)) {
            end = 1;
        }

        while (!end) {
            char c = fgetc(stream);

            if (c == EOF) {
                if (feof(stream)) {
                    break;
                } else {
                    *lineptr = line;
                    *n = line_len;
                    return -1;
                }
            }

            if (c == delim) {
                end = 1;
            }

            if (offset + 1 >= line_len) {
                if ((((ssize_t)line_len) << 1) < 0) {
                    *lineptr = line;
                    *n = line_len;
// EOVERFLOW is POSIX.1-2001 and not part of the C standard
#if defined(EOVERFLOW)
                    errno = EOVERFLOW;
#endif
                    return -1;
                }

                line_len *= 2;
                line = realloc(line, sizeof(char) * line_len);
                if (line == NULL) {
                    *lineptr = line;
                    *n = line_len;
// ENOMEM is POSIX.1-2001 and not part of the C standard
#if defined(ENOMEM)
                    errno = ENOMEM;
#endif
                    return -1;
                }
            }

            line[offset++] = c;
        }

        line[offset] = '\0';

        *lineptr = line;
        *n = line_len;

        return (offset == 0) ? -1 : (ssize_t)offset;
    #endif
}
