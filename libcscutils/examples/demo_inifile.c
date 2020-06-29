/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) Martin Koehler, 2015
 */


#include <stdio.h>
#include <stdlib.h>

#include "cscutils/inifile.h"


void dump_ini_data(csc_ini_file_t * ini) {
    csc_ini_iterator_t iter = NULL;
    csc_ini_iterator_t kv_iter = NULL;
    csc_ini_section_t  *sec;
    csc_ini_kvstore_t  *kv;

    while ((sec = csc_ini_section_iterator(ini, &iter)) != NULL ){
        printf("-> Section \"%s\"\n", csc_ini_getsectionname(sec));
        kv_iter = NULL;
        while ( (kv = csc_ini_kvstore_iterator(sec, &kv_iter)) != NULL) {
            printf("--> %20s = %s\n", csc_ini_getkey(kv) , csc_ini_getvalue(kv));
        }
    }
}


int main(int argc, char *argv[])
{
    csc_ini_file_t inifile;

    /*  empty file */
    csc_ini_empty(&inifile);


    if ( argc > 1) {
        csc_ini_load(argv[1], &inifile,0);
    }
    /* Fill some data */
    csc_ini_setstring(&inifile, "section1", "key1", "value1");
    csc_ini_setstring(&inifile, "section1",  "key2", "bla");
    csc_ini_setinteger(&inifile, "section2", "key1", 1);
    csc_ini_setfloat  (&inifile, "section3", "keybla", 1.2234);
    csc_ini_setfloat  (&inifile, NULL, "keyxxx", -9.2345);
    csc_ini_setstring(&inifile, "section1", "key1", "value1 mod");

    /* Get some data */
    {
        char *cval;

        if ( csc_ini_getstring(&inifile, "section1", "key1", &cval) == CSC_INI_SUCCESS) {
            printf("Got value for section1/key1: %s\n", cval);
        } else {
            printf("Get key failed. (section1/key1) \n");
        }
        if ( csc_ini_getstring(&inifile, "section1", "key1x", &cval) == CSC_INI_SUCCESS) {
            printf("Got value for section1/key1x: %s\n", cval);
        } else {
            printf("Get key failed. (section1/key1x) \n");
        }

    }
    /* Dump File */
    dump_ini_data(&inifile);

    printf("remove section1\n");

    csc_ini_section_remove(&inifile, "section1");

    dump_ini_data(&inifile);

    /* Write File  */
    csc_ini_write("test.ini",  &inifile);
    csc_ini_free(&inifile);

    return 0;
}




