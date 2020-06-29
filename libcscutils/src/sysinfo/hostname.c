/*
 * LIBCSCUTILS -- Helper routines of the CSC group
 * Copyright (C) Martin Koehler, 2017
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
#include <math.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>

#include "cscutils/sysinfo.h"

char *csc_sysinfo_hostname()
{
    char * ret = NULL;
    int err = 0;
    size_t len = 128;
    do {
        if ( ret ) free(ret);
        ret = calloc(len, sizeof(char));
        if (!ret) return NULL;
        if ( gethostname(ret, len) ) {
            err = errno;
            if ( errno == ENAMETOOLONG ) len+= 128;
            else return NULL;
        } else {
            err = 0;
        }
    } while (err);
    return ret;
}
