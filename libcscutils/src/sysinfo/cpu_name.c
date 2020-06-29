/*
 * LIBCSCUTILS -- Helper routines of the CSC group
 * Copyright (C) Martin Koehler, 2020
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
#include "cscutils/strutils.h"

char *csc_sysinfo_cpuname()
{
#if defined(__linux__)
    FILE *fp = NULL;
    char *line = NULL;
    char *cpuname;
    char *pos;
    size_t len = 0;
    fp = fopen("/proc/cpuinfo", "r");

    while ( getline(&line, &len, fp) >=0 ){
        // x86
        if ( strstr(line, "model name") == line) {
            pos = strstr(line, ":");
            if (!pos) continue;
            pos++;
            cpuname = strdup(pos);
            cpuname = csc_str_rtrim(csc_str_ltrim(cpuname));
            free(line);
            return cpuname;
        }
        // POWER
#if defined(__powerpc__) || defined(__powerpc64__)
        if ( strstr(line, "cpu") == line) {
            pos = strstr(line, ":");
            if (!pos) continue;
            pos++;
            cpuname = strdup(pos);
            cpuname = csc_str_rtrim(csc_str_ltrim(cpuname));
            free(line);
            return cpuname;
        }
#endif

    }
    free(line);

    fclose(fp);
    return NULL;
#else
    return strup("unknown");
#endif
}

