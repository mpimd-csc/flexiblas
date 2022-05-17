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

#if defined(_WIN32) || defined(_WIN64)
    #include <intrin.h>
#endif

#include "cscutils/sysinfo.h"
#include "cscutils/posix_impl.h"
#include "cscutils/strutils.h"

#if defined(__linux__)

char *csc_sysinfo_cpuname()
{
    FILE *fp = NULL;
    char *line = NULL;
    char *cpuname;
    char *pos;
    size_t len = 0;
    fp = fopen("/proc/cpuinfo", "r");

    while ( csc_getline(&line, &len, fp) >=0 ){
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
}

#elif defined(__amd64__) || defined(__amd64) || defined(__x86_64__) || defined(__x86_64) || defined(_M_AMD64)

static inline void __cpuid_intrinsic(unsigned int* reg, unsigned int input) {
#if defined(_WIN32) || defined(_WIN64)
    __cpuid((int*)reg,(int)input);
#elif defined(__GNUC__) || defined(__clang__) || defined(__INTEL_COMPILER)
    unsigned int* eax = reg + 0;
    unsigned int* ebx = reg + 1;
    unsigned int* ecx = reg + 2;
    unsigned int* edx = reg + 3;
    __asm__ volatile("cpuid" : "=a"(*eax),"=b"(*ebx),"=c"(*ecx),"=d"(*edx) : "0"(input), "2"(0));
#else
    reg[0] = 0;
    (void)input;
#endif
}

char *csc_sysinfo_cpuname()
{
    unsigned int brand[12];

    __cpuid_intrinsic(brand, 0x80000000);
    if (brand[0] < 0x80000004) {
        return strdup("Unknown");
    }

    for (int i = 0; i < 3; i++) {
        __cpuid_intrinsic(brand + 4 * i, 0x80000002 + i);
    }

    return strdup((char*) &brand);
}

#else

char *csc_sysinfo_cpuname()
{
    return strdup("Unknown");
}

#endif
