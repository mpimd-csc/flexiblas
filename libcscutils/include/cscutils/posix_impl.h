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

#ifndef POSIX_IMPL_H
#define POSIX_IMPL_H

#ifdef  __cplusplus
extern "C" {

#if !defined(restrict)

#if defined(__GNUC__) || defined(__clang__)
#define restrict __restrict
#elif defined(_MSC_VER)
#define restrict __declspec(restrict)
#else
#define restrict
#endif

#endif /* !defined(restrict) */

#endif /* __cplusplus */

#if !defined(restrict)
#if !defined(__STDC_VERSION__) || !(__STDC_VERSION__ >= 199901L)
#define restrict
#endif /* __STDC_VERSION__ */
#endif /* !defined(restrict) */

#include <stdint.h>
#include <stdio.h>

#ifdef _WIN64
typedef long long int ssize_t;
#elif _WIN32
typedef long ssize_t;
#endif

/**
 * @brief Reads a line from a stream.
 * @param[in, out]  lineptr  Pointer to the buffer containing the line afterwards.
 * @param[in, out]  n        Pointer to the number containing the size of the buffer pointed to by lineptr.
 * @param[in]       stream   Pointer to a FILE object containing the stream.
 * @return The number of characters read including the newline character on success, -1 else.
 *
 * The \ref csc_getline function implements the getline function as defined in POSIX.1-2008.
 * Errno is only set on failure if POSIX.1-2001 error names are available.
 * The error indicator for the stream may not be set.
 * See the documentation of getline for more information.
 *
 */
ssize_t csc_getline(char** restrict lineptr, size_t* restrict n, FILE* restrict stream);

/**
 * @brief Reads a line delimited by a specific character from a stream.
 * @param[in, out]  lineptr  Pointer to the buffer containing the line afterwards.
 * @param[in, out]  n        Pointer to the number containing the size of the buffer pointed to by lineptr.
 * @param[in]       delim    Line delimiter.
 * @param[in]       stream   Pointer to a FILE object containing the stream.
 * @return The number of characters read including the newline character on success, -1 else.
 *
 * The \ref csc_getdelim function implements the getdelim function as defined in POSIX.1-2008.
 * Errno is only set on failure if POSIX.1-2001 error names are available.
 * Note that the error indicator for the stream may not be set.
 * See the documentation of getdelim for more information.
 *
 */
ssize_t csc_getdelim(char** restrict lineptr, size_t* restrict n, int delim, FILE* restrict stream);

#ifdef  __cplusplus
}
#endif

#endif /* POSIX_IMPL_H */
