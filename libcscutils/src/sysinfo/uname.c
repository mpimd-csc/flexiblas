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
#include <windows.h>
#include <winnt.h>
#else
#include <sys/utsname.h>
#endif

#include "cscutils/sysinfo.h"

char * csc_sysinfo_sysname(void)
{
#if defined(_WIN32) || defined(_WIN64)
    return strdup("Windows_NT");
#else
    struct utsname buf;
    if ( uname(&buf))
        return NULL;
    return strdup(buf.sysname);
#endif
}

char * csc_sysinfo_nodename(void)
{
#if defined(_WIN32) || defined(_WIN64)
    return strdup("Unknown");
#else
    struct utsname buf;
    if ( uname(&buf))
        return NULL;
    return strdup(buf.nodename);
#endif
}

char * csc_sysinfo_release(void)
{
#if defined(_WIN32) || defined(_WIN64)
    DWORD dwVersion = 0;
    DWORD dwMajorVersion = 0;
    DWORD dwMinorVersion = 0;
    DWORD dwBuild = 0;

    dwVersion = GetVersion();

    dwMajorVersion = (DWORD)(LOBYTE(LOWORD(dwVersion)));
    dwMinorVersion = (DWORD)(HIBYTE(LOWORD(dwVersion)));

    if (dwVersion < 0x80000000) {
        dwBuild = (DWORD)(HIWORD(dwVersion));
    }

    int len = 128;
    char* ret = malloc(len * sizeof(char));
    int req = snprintf(ret, len * sizeof(char), "%lu.%lu %lu",dwMajorVersion,dwMinorVersion,dwBuild);
    if (req >= len) {
        ret = realloc(ret, (req + 1) * sizeof(char));
        sprintf(ret, "%lu.%lu %lu",dwMajorVersion,dwMinorVersion,dwBuild);
    }

    return ret;
#else
    struct utsname buf;
    if ( uname(&buf))
        return NULL;
    return strdup(buf.release);
#endif
}

char * csc_sysinfo_version(void)
{
#if defined(_WIN32) || defined(_WIN64)
    return strdup("Unknown");
#else
    struct utsname buf;
    if ( uname(&buf))
        return NULL;
    return strdup(buf.version);
#endif
}

char * csc_sysinfo_machine(void)
{
#if defined(_WIN32) || defined(_WIN64)
    SYSTEM_INFO sysinfo;
    GetSystemInfo(&sysinfo);
    WORD arch = sysinfo.wProcessorArchitecture;

    char* ret = NULL;

    switch(arch) {
        case PROCESSOR_ARCHITECTURE_INTEL:
            ret = strdup("i686");
            break;
        case PROCESSOR_ARCHITECTURE_ARM:
            ret = strdup("ARM");
            break;
        case PROCESSOR_ARCHITECTURE_IA64:
            ret = strdup("IA64");
            break;
        case PROCESSOR_ARCHITECTURE_AMD64:
            ret = strdup("x86_64");
            break;
        case PROCESSOR_ARCHITECTURE_ARM64:
            ret = strdup("ARM64");
            break;
        case PROCESSOR_ARCHITECTURE_UNKNOWN:
        default:
            ret = strdup("Unknown");
            break;
    }

    return ret;
#else
    struct utsname buf;
    if ( uname(&buf))
        return NULL;
    return strdup(buf.machine);
#endif
}
