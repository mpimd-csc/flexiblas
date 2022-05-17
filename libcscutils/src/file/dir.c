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
#include <string.h>

#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "cscutils/io.h"


#if defined(_WIN32) || defined(_WIN64)
    #define CSC_MKDIR(TMP,M) mkdir((TMP))
#else
    #define CSC_MKDIR(TMP,M) mkdir((TMP),(M))
#endif

int csc_file_mkdir(const char *dir, mode_t m)
{
#if defined(_WIN32) || defined(_WIN64)
    (void)m;
#endif

    char tmp[16*2048];
    char *p = NULL;
    size_t len;
    int ret;
    struct stat buf;

    snprintf(tmp, sizeof(tmp),"%s",dir);
    len = strlen(tmp);
    if(tmp[len - 1] == '/')
        tmp[len - 1] = 0;
    for(p = tmp + 1; *p; p++){
        if(*p == '/') {
            *p = 0;

            ret = stat (tmp, &buf);
            if ( ret == 0 ) {
                if (S_ISDIR(buf.st_mode)) {
                    *p = '/';
                    continue;
                }
            }
            ret = CSC_MKDIR(tmp, m);
            if ( ret != 0 ) return ret;
            *p = '/';
        }
    }
    ret = stat (tmp, &buf);
    if ( ret == 0 ) {
        if (!S_ISDIR(buf.st_mode)) {
            return CSC_MKDIR(tmp, m);
        }
    } else {
        return CSC_MKDIR(tmp, m);
    }
    return 0;
}

int csc_file_exist(const char *path) {
    if( access( path, F_OK ) != -1 ) {
        return -1;
    } else {
        return 0;
    }
}
