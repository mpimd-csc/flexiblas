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

unsigned int csc_sysinfo_cpu_count()
{
#if defined(__linux__)
#define LINE_LEN 10

    FILE *fp;
    char value[LINE_LEN];
    unsigned int ret = 0;
    unsigned int cpunr = 0;

    fp = fopen("/proc/stat", "r");
    if (!fp) {
        printf("Couldn't count the number of CPUs (%s: %s), assuming 1\n", "/proc/stat", strerror(errno));
        return 1;
    }

    while (!feof(fp)) {
        if (!fgets(value, LINE_LEN, fp))
            continue;
        value[LINE_LEN - 1] = '\0';
        if (strlen(value) < (LINE_LEN - 2))
            continue;
        if (strstr(value, "cpu "))
            continue;
        if (sscanf(value, "cpu%u ", &cpunr) != 1)
            continue;
        if (cpunr > ret)
            ret = cpunr;
    }
    fclose(fp);

    /* cpu count starts from 0, on error return 1 (UP) */
    return ret + 1;
#elif defined(__FreeBSD__)
    return sysconf(_SC_NPROCESSORS_CONF);
#else
#warning Not implemented yet
    return 1;
#endif
}
