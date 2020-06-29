/*
 * LIBCSCUTILS -- Helper for CSC developed software
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
#include <stdarg.h>
#include <math.h>
#include <string.h>

#include "cscutils/sysinfo.h"
#include "cscutils/strutils.h"
#include "cscutils/error_message.h"

static char * make_message(const char *fmt, ...)
{
    int size = 0;
    char *p = NULL;
    va_list ap;

    /* Determine required size */

    va_start(ap, fmt);
    size = vsnprintf(p, size, fmt, ap);
    va_end(ap);

    if (size < 0)
        return NULL;

    size++;             /* For '\0' */
    p = malloc(size);
    if (p == NULL)
        return NULL;

    va_start(ap, fmt);
    size = vsnprintf(p, size, fmt, ap);
    va_end(ap);

    if (size < 0) {
        free(p);
        return NULL;
    }

    return p;
}


char *csc_sysinfo_ccompiler()
{
#if defined(__GNUC__) && !( defined(__clang__) || defined(__ICC))
    // GCC
    return make_message("GCC %d.%d.%d", __GNUC__, __GNUC_MINOR__, __GNUC_PATCHLEVEL__);
#elif defined(__clang__) && !defined(__ibmxl__)
    // CLang
    return make_message("Clang %d.%d.%d", __clang_major__, __clang_minor__, __clang_patchlevel__);
#elif defined(__clang__) && defined(__ibmxl__)
    // IBM XLC
    return make_message("IBM XLC %d.%d.%d.%d", __ibmxl_version__, __ibmxl_release__, __ibmxl_modification__, __ibmxl_ptf_fix_level__);
#elif defined(__xlc__)
    // IBM XLC old
    return make_message("IBM XLC (Old, non Clang version) -- Please look yourself");
#elif defined(__ICC)
    // Intel
    return make_message("Intel C/C++ %d.%d.%d", __INTEL_COMPILER/100, 0, __INTEL_COMPILER_UPDATE);
#elif defined(__PGI)
    // PGI
    return make_message("PGI %d.%d.%d", __PGIC__, __PGIC_MINOR__, __PGIC_PATCHLEVEL__);
#elif defined(__TINYC__)
    int major, minor, patch;
    patch = __TINYC__ % 100;
    minor = (__TINYC__ / 100) % 100;
    major = (__TINYC__ / 10000) % 100;
    return make_message("TinyC %d.%d.%d", major, minor, patch);
#else
    return strdup("unknown");
#endif
}
