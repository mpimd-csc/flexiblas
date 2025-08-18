//    SPDX-License-Identifier: LGPL-3.0-or-later
/*
    This file is part of libcscutils, a set of helper function.
    Copyright (C) 2013-2024 Martin Koehler

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this program; if not, write to the Free Software Foundation,
    Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/




#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "cscutils/error_message.h"
#include "cscutils/strutils.h"
#include "cscutils/table.h"


int main(int argc, char **argv)
{
    int cm, cn, cmb, cnb, ctime;
    csc_table_t *t;
    (void) argc;
    (void) argv;

    /* Create table */
    t = csc_table_new(0);
    /* Create columns */
    cm = csc_table_add_column(t, "M", CSC_TABLE_INTEGER, CSC_TABLE_RIGHT);
    cn = csc_table_add_column(t, "N", CSC_TABLE_INTEGER, CSC_TABLE_CENTER);
    cmb= csc_table_add_column(t, "MB", CSC_TABLE_INTEGER, CSC_TABLE_RIGHT);
    cnb= csc_table_add_column(t, "NB", CSC_TABLE_INTEGER, CSC_TABLE_RIGHT);
    ctime =csc_table_add_column(t, "Time", CSC_TABLE_FLOAT, CSC_TABLE_LEFT);

    /* Fill data  */
    csc_table_new_row(t);
    csc_table_set_entry(t, cm, (long) 1);
    csc_table_set_entry(t, cn, (long) 13424);
    csc_table_set_entry(t, cmb, (long) 64);
    csc_table_set_entry(t, cnb, (long) 128);
    csc_table_set_entry(t, ctime, (double) 2325.234);

    csc_table_new_row(t);
    csc_table_set_entry(t, cm, (long) 1234);
    csc_table_set_entry(t, cn, (long) 124);
    csc_table_set_entry(t, cmb, (long) 62344);
    csc_table_set_entry(t, cnb, (long) 128);
    csc_table_set_entry(t, ctime, (double) 2.234);

    csc_table_comment_allinfo(t);

    csc_table_print_ascii(stdout, t,"  ");

    /* Change format  */
    csc_table_column_set_format(t, ctime, "%10.2e");
    csc_table_column_set_formater(t, cn, csc_table_formater_integer);
    csc_table_print_ascii(stdout, t,"  ");

    csc_table_destroy(t);




    return 0;
}
