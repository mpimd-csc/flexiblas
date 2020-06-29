/*
 * libcscutils - Helper Routines of the CSC group
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
#include <string.h>
#include <stdarg.h>
#include <math.h>
#include "cscutils/error_message.h"
#include "cscutils/strutils.h"
#include "cscutils/table.h"

/* Select the row with the maximum value in column    */
int csc_table_max_row(csc_table_t * t, int column)
{
    int i;
    int maxpos = 0;

    if (!t) return -1;
    if ( t->number_of_rows < 1 ) return -1;
    if ( t->number_of_columns <= column) return -1;

    i = 0;
    while ( i < t->number_of_rows && t->columns[column].set[i] == 0 )
        i++;
    if ( i == t->number_of_rows) {
        return -1;
    }
    maxpos = i;

    for (i = 0; i < t->number_of_rows ; i++) {
        if ( t->columns[column].type == CSC_TABLE_INTEGER && t->columns[column].set[i] ) {
            if ( t->columns[column].v.integer_values[i] >= t->columns[column].v.integer_values[maxpos] )  {
                maxpos = i;
            }
        } else if ( t->columns[column].type == CSC_TABLE_FLOAT && t->columns[column].set[i] ) {
            if ( t->columns[column].v.float_values[i] >= t->columns[column].v.float_values[maxpos] )  {
                maxpos = i;
            }

        } else if ( t->columns[column].type == CSC_TABLE_STRING && t->columns[column].set[i] ) {
            if ( strncmp(t->columns[column].v.string_values[i], t->columns[column].v.string_values[maxpos], CSC_TABLE_MAXLEN) >= 0  )  {
                maxpos = i;
            }
        }
    }
    return maxpos;
}

int csc_table_amax_row(csc_table_t * t, int column)
{
    int i;
    int maxpos = 0;

    if (!t) return -1;
    if ( t->number_of_rows < 1 ) return -1;
    if ( t->number_of_columns <= column) return -1;

    i = 0;
    while ( i < t->number_of_rows && t->columns[column].set[i] == 0 )
        i++;
    if ( i == t->number_of_rows) {
        return -1;
    }
    maxpos = i;

    for (i = 0; i < t->number_of_rows ; i++) {
        if ( t->columns[column].type == CSC_TABLE_INTEGER && t->columns[column].set[i] ) {
            if ( labs(t->columns[column].v.integer_values[i]) >= labs(t->columns[column].v.integer_values[maxpos]) )  {
                maxpos = i;
            }
        } else if ( t->columns[column].type == CSC_TABLE_FLOAT && t->columns[column].set[i] ) {
            if ( fabs(t->columns[column].v.float_values[i]) >= fabs(t->columns[column].v.float_values[maxpos] ))  {
                maxpos = i;
            }

        } else if ( t->columns[column].type == CSC_TABLE_STRING && t->columns[column].set[i] ) {
            if ( strncmp(t->columns[column].v.string_values[i], t->columns[column].v.string_values[maxpos], CSC_TABLE_MAXLEN) >= 0  )  {
                maxpos = i;
            }
        }
    }
    return maxpos;
}

int csc_table_min_row(csc_table_t * t, int column)
{
    int i;
    int minpos = 0;

    if (!t) return -1;
    if ( t->number_of_rows < 1 ) return -1;
    if ( t->number_of_columns <= column) return -1;

    i = 0;
    while ( i < t->number_of_rows && t->columns[column].set[i] == 0 )
        i++;
    if ( i == t->number_of_rows) {
        return -1;
    }
    minpos = i;

    for (i = 0; i < t->number_of_rows ; i++) {
        if ( t->columns[column].type == CSC_TABLE_INTEGER && t->columns[column].set[i] ) {
            if ( t->columns[column].v.integer_values[i] <= t->columns[column].v.integer_values[minpos] )  {
                minpos = i;
            }
        } else if ( t->columns[column].type == CSC_TABLE_FLOAT && t->columns[column].set[i] ) {
            if ( t->columns[column].v.float_values[i] <= t->columns[column].v.float_values[minpos] )  {
                minpos = i;
            }

        } else if ( t->columns[column].type == CSC_TABLE_STRING && t->columns[column].set[i] ) {
            if ( strncmp(t->columns[column].v.string_values[i], t->columns[column].v.string_values[minpos], CSC_TABLE_MAXLEN) <= 0  )  {
                minpos = i;
            }
        }
    }
    return minpos;

}


int csc_table_amin_row(csc_table_t * t, int column)
{
    int i;
    int minpos = 0;

    if (!t) return -1;
    if ( t->number_of_rows < 1 ) return -1;
    if ( t->number_of_columns <= column) return -1;

    i = 0;
    while ( i < t->number_of_rows && t->columns[column].set[i] == 0 )
        i++;
    if ( i == t->number_of_rows) {
        return -1;
    }
    minpos = i;

    for (i = 0; i < t->number_of_rows ; i++) {
        if ( t->columns[column].type == CSC_TABLE_INTEGER && t->columns[column].set[i] ) {
            if ( labs(t->columns[column].v.integer_values[i]) <= labs(t->columns[column].v.integer_values[minpos]) )  {
                minpos = i;
            }
        } else if ( t->columns[column].type == CSC_TABLE_FLOAT && t->columns[column].set[i] ) {
            if ( fabs(t->columns[column].v.float_values[i]) <= fabs(t->columns[column].v.float_values[minpos]) )  {
                minpos = i;
            }

        } else if ( t->columns[column].type == CSC_TABLE_STRING && t->columns[column].set[i] ) {
            if ( strncmp(t->columns[column].v.string_values[i], t->columns[column].v.string_values[minpos], CSC_TABLE_MAXLEN) <= 0  )  {
                minpos = i;
            }
        }
    }
    return minpos;

}


