/*
 * libcscutils
 * Copyright (C) Martin Köhler, 2018
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
#include <math.h>
#include <ctype.h>
#include <errno.h>
#include <pthread.h>
#include <limits.h>
#include <stdlib.h>
#include <unistd.h>

#include "cscutils/inifile.h"
#include "cscutils/error_message.h"

int csc_ini_easy_get_int(const char *filename, const char * section, const char *key, int *val)
{

    const char * s;
    csc_ini_file_t inifile;

    /*  empty file */
    csc_ini_empty(&inifile);

    if ( csc_ini_load(filename, &inifile,0) != CSC_INI_SUCCESS) {
        csc_error_message("Failed to open %s. \n", filename);
        csc_ini_free(&inifile);
        return CSC_INI_FILEOPEN;
    }
    if ( section == NULL || strlen(section) == 0 ) {
        s = NULL;
    } else {
        s = section;
    }

    if ( csc_ini_getinteger( &inifile, s, key, val) != CSC_INI_SUCCESS) {
        csc_ini_free(&inifile);
        return CSC_INI_NOKEY;
    }
    csc_ini_free(&inifile);
    return CSC_INI_SUCCESS;
}

int csc_ini_easy_get_double(const char *filename, const char * section, const char *key, double *val)
{

    const char *s;
    csc_ini_file_t inifile;

    /*  empty file */
    csc_ini_empty(&inifile);

    if ( csc_ini_load(filename, &inifile,0) != CSC_INI_SUCCESS) {
        csc_error_message("Failed to open %s. \n", filename);
        csc_ini_free(&inifile);
        return CSC_INI_FILEOPEN;
    }

    if ( section==NULL ||  strlen(section) == 0 ) {
        s = NULL;
    } else {
        s = section;
    }


    if ( csc_ini_getfloat( &inifile, s, key, val) != CSC_INI_SUCCESS) {
        csc_ini_free(&inifile);
        return CSC_INI_NOKEY;
    }
    csc_ini_free(&inifile);
    return CSC_INI_SUCCESS;
}

int csc_ini_easy_get_string(const char *filename, const char * section, const char *key, char *val, int maxlen)
{

    const char *s;
    char *inval;
    csc_ini_file_t inifile;

    /*  empty file */
    csc_ini_empty(&inifile);

    if ( csc_ini_load(filename, &inifile,0) != CSC_INI_SUCCESS) {
        csc_error_message("Failed to open %s. \n", filename);
        csc_ini_free(&inifile);
        return CSC_INI_FILEOPEN;
    }

    if ( section==NULL ||  strlen(section) == 0 ) {
        s = NULL;
    } else {
        s = section;
    }


    if ( csc_ini_getstring( &inifile, s, key, &inval) != CSC_INI_SUCCESS) {
        csc_ini_free(&inifile);
        return CSC_INI_NOKEY;
    }
    strncpy(val, inval, maxlen);

    csc_ini_free(&inifile);
    return CSC_INI_SUCCESS;
}




