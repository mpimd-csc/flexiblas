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

#include <sys/utsname.h>
#include "cscutils/sysinfo.h"

char * csc_sysinfo_sysname()
{
    struct utsname buf;
    if ( uname(&buf))
        return NULL;
    return strdup(buf.sysname);
}

char * csc_sysinfo_nodename()
{
    struct utsname buf;
    if ( uname(&buf))
        return NULL;
    return strdup(buf.nodename);
}

char * csc_sysinfo_release()
{
    struct utsname buf;
    if ( uname(&buf))
        return NULL;
    return strdup(buf.release);
}

char * csc_sysinfo_version()
{
    struct utsname buf;
    if ( uname(&buf))
        return NULL;
    return strdup(buf.version);
}

char * csc_sysinfo_machine()
{
    struct utsname buf;
    if ( uname(&buf))
        return NULL;
    return strdup(buf.machine);
}


