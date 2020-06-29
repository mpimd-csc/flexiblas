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
#include "cscutils/error_message.h"

#ifdef CSC_HAVE_BACKTRACE
#include <execinfo.h>
#include <unistd.h>

int csc_show_backtrace() {
    int j, nptrs;
#define SIZE 200
    void *buffer[SIZE];
    char **strings;

    nptrs = backtrace(buffer, SIZE);
    strings = backtrace_symbols(buffer, nptrs);

    if (strings == NULL) {
        perror("backtrace_symbols");
        return 1;
    }

    for (j = 1; j < nptrs; j++)
        csc_info_message("%s", strings[j]);
    free(strings);
    return 0;
}

#else

int csc_show_backtrace() {
    csc_info_message("Backtrace is not available.\n");
    return 1;
}
#endif
