/* 
 * libcscutils - Helper Routines of the CSC group  
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
#include <string.h>
#include <stdarg.h>
#include "cscutils/error_message.h"
#include "cscutils/strutils.h"
#include "cscutils/table.h"


#define MAX(A,B) ((A)>(B)?(A):(B))

static void print_row_ascii(FILE *stream, csc_table_t *t, const char *colsep, int r ); 
static void print_header_ascii(FILE *stream, csc_table_t *t, const char *colsep); 

csc_table_t * csc_table_new(int continous_print ) {
    csc_table_t * t;

    t = (csc_table_t  *) malloc(sizeof(csc_table_t) * (1));
    if ( ! t ) return NULL;
    t->number_of_rows = 0;
    t->number_of_columns = 0;
    t->current_row = -1;
    t->columns =  NULL;
    t->cp = continous_print; 
    t->comment = csc_table_new_comment(); 
    if (!t->comment) {
        free(t); 
        return NULL; 
    }
    return t;
}

csc_table_t * csc_table_new_from_table(csc_table_t *table) {
    csc_table_t * t;
    int i = 0; 
    if (!table) {
        csc_error_message("Source table points to NULL.\n"); 
        return NULL; 
    }
    t = csc_table_new(table->cp); 
    if (!t) {
        csc_error_message("Failed to allocate empty table.\n"); 
        return NULL; 
    }

    for (i = 0; i < table->number_of_columns; i++) {
       csc_table_add_column(t, table->columns[i].name, table->columns[i].type, table->columns[i].align); 
    }
    return t; 
}

void csc_table_destroy(csc_table_t * t) {
    int i;
    if (!t) return;
    for (i = 0; i < t->number_of_columns; i++) {
        csc_table_column_destroy(t->columns[i]);
    }
    free(t->columns);
    csc_table_destroy_comment(t->comment); 
    free(t);
    return;
}

void csc_table_column_destroy(csc_table_column_t col){
    if ( col.type == CSC_TABLE_INTEGER ){
        if (col.v.integer_values) free(col.v.integer_values);
    } else if ( col.type == CSC_TABLE_FLOAT ) {
        if (col.v.float_values) free(col.v.float_values);
    } else if ( col.type == CSC_TABLE_STRING ) {
        int i;
        for (i = 0; i < col.len; i++) {
            if (col.v.string_values[i]) free(col.v.string_values[i]);
        }
        free(col.v.string_values);
    }
    if (col.set) free(col.set);
    return;
}

int csc_table_add_column(csc_table_t *t, const char *name, csc_table_value_t type, csc_table_align_t align) {
    int last;
    if (!t) return -1;
    if (t->current_row >= 0) {
        csc_error_message("Cannot add a column to a filled table.\n");
        return -1;
    }
    last = t->number_of_columns++;
    t->columns = realloc(t->columns, sizeof(csc_table_column_t) * t->number_of_columns);
    if ( !t->columns) {
        csc_error_message("Failed to allocate memory for the new column.\n");
        return -1;
    }
    t->columns[last].type = type;
    t->columns[last].v.ptr = NULL;
    strncpy(t->columns[last].name, name, CSC_TABLE_MAXLEN);
    t->columns[last].set = NULL;
    t->columns[last].len = 0;
    t->columns[last].width = strnlen(name, CSC_TABLE_MAXLEN);
    switch (type) {
        case CSC_TABLE_INTEGER:
            strncpy(t->columns[last].format_str,"%ld", CSC_TABLE_MAXLEN);
            break;
        case CSC_TABLE_FLOAT:
            strncpy(t->columns[last].format_str,"%lg", CSC_TABLE_MAXLEN);
            break;
        case CSC_TABLE_STRING:
            strncpy(t->columns[last].format_str,"%s", CSC_TABLE_MAXLEN);
            break;
    }
    t->columns[last].formater = NULL; 
    t->columns[last].align = align; 
    return last;
}

static void update_width(csc_table_t *t, int column) 
{
    char tmp[CSC_TABLE_MAXLEN+1]; 
    int i, len; 

    t->columns[column].width = strnlen(t->columns[column].name,CSC_TABLE_MAXLEN); 
    if (t->columns[column].formater) {
        for (i = 0; i < t->columns[column].len; i++) {
            switch (t->columns[column].type) { 
                case CSC_TABLE_INTEGER:
                    t->columns[column].formater(tmp, CSC_TABLE_MAXLEN, CSC_TABLE_INTEGER, t->columns[column].v.integer_values[i]); 
                    break;
                case CSC_TABLE_FLOAT:
                    t->columns[column].formater(tmp, CSC_TABLE_MAXLEN, CSC_TABLE_FLOAT, t->columns[column].v.float_values[i]); 
                    break;
                case CSC_TABLE_STRING:
                    t->columns[column].formater(tmp, CSC_TABLE_MAXLEN, CSC_TABLE_STRING, t->columns[column].v.string_values[i]); 
                    break;
            }
            len = strlen(tmp); 
            t->columns[column].width = MAX(t->columns[column].width, len); 

        }
    } else {
        for (i = 0; i < t->columns[column].len; i++) {
            switch (t->columns[column].type) {
                case CSC_TABLE_INTEGER:
                    snprintf(tmp, CSC_TABLE_MAXLEN+1, t->columns[column].format_str, t->columns[column].v.integer_values[i]); 
                    break;
                case CSC_TABLE_FLOAT:
                    snprintf(tmp, CSC_TABLE_MAXLEN+1, t->columns[column].format_str, t->columns[column].v.float_values[i]); 
                    break;
                case CSC_TABLE_STRING:
                    snprintf(tmp, CSC_TABLE_MAXLEN+1, t->columns[column].format_str, t->columns[column].v.string_values[i]); 
                    break;
            }
            len = strlen(tmp); 
            t->columns[column].width = MAX(t->columns[column].width, len); 
        }
    }
}

int  csc_table_column_set_format(csc_table_t *t, int column, const char *fmt)
{
    if (!t) {
        return -1; 
    }
    if (column >= t->number_of_columns) {
        csc_error_message("Column number %d is out of range (%d)\n", column, t->number_of_columns); 
        return -1;    
    }

    t->columns[column].formater = NULL; 
    strncpy(t->columns[column].format_str, fmt, CSC_TABLE_MAXLEN); 
    update_width(t, column); 

    return 0; 
}

int  csc_table_column_set_formater(csc_table_t *t, int column, csc_table_formater_t fmt)
{
    if (!t) {
        return -1; 
    }
    if (column >= t->number_of_columns) {
        csc_error_message("Column number %d is out of range (%d)\n", column, t->number_of_columns); 
        return -1;    
    }

    if ( !fmt ) {
        t->columns[column].formater = NULL; 
        switch (t->columns[column].type) {
            case CSC_TABLE_INTEGER:
                strncpy(t->columns[column].format_str,"%ld", CSC_TABLE_MAXLEN);
                break;
            case CSC_TABLE_FLOAT:
                strncpy(t->columns[column].format_str,"%lg", CSC_TABLE_MAXLEN);
                break;
            case CSC_TABLE_STRING:
                strncpy(t->columns[column].format_str,"%s", CSC_TABLE_MAXLEN);
                break;
        }
    } else {
        t->columns[column].formater = fmt; 
    }
    update_width(t, column); 
    return 0; 
}


static int new_row_internal(csc_table_t * t) {
    int last = t->number_of_rows;
    int i;
    t->number_of_rows++;
    for (i = 0; i < t->number_of_columns; i++) {
        int last_len = t->columns[i].len++;
        switch (t->columns[i].type) {
            case CSC_TABLE_INTEGER:
                t->columns[i].v.integer_values = realloc ( t->columns[i].v.integer_values, sizeof(long)*t->columns[i].len);
                break;
            case CSC_TABLE_FLOAT:
                t->columns[i].v.float_values = realloc ( t->columns[i].v.float_values, sizeof(double)*t->columns[i].len);
                break;
            case CSC_TABLE_STRING:
                t->columns[i].v.string_values = realloc ( t->columns[i].v.string_values, sizeof(char *)*t->columns[i].len);
                t->columns[i].v.string_values[last_len] = NULL;
                break;
        }
        if ( ! t->columns[i].v.ptr ) {
            csc_error_message("Failed to extend column %d.\n",i);
            return -1;
        }
        t->columns[i].set = realloc(t->columns[i].set, sizeof(int) * t->columns[i].len);
        if (!t->columns[i].set ) {
            csc_error_message("Failed to extend column %d.\n", i);
            return -1;
        }
        t->columns[i].set[last] = 0; 
    }
    t->current_row = last;
    return 0;

}

int  csc_table_new_row(csc_table_t * t) {
    int last = t->number_of_rows;

    if ( t-> cp ) {
        if ( last <= 0 ) {
            print_header_ascii(stdout, t, " "); 
        } else {
            print_row_ascii(stdout, t, " ", last-1); 
        }
    }
    return new_row_internal(t); 
}

int  csc_table_set_entry(csc_table_t *t, int column, ...)
{
    csc_table_value_t type;
    va_list ap;

    if ( !t ) return -1;
    if ( t->current_row < 0 ) {
        csc_error_message("No row started until now.\n");
        return -1;
    }
    if ( column >= t->number_of_columns ){
        csc_error_message("Column index %d is out of range (max = %d)\n", column, t->number_of_columns);
        return -1;
    }
    type = t->columns[column].type;
    va_start(ap, column);
    switch (type) {
        case CSC_TABLE_INTEGER:
            {
                long v = va_arg(ap, long);
                char tmp[CSC_TABLE_MAXLEN];
                int len;
                t->columns[column].v.integer_values[t->current_row] = v;
                if ( t->columns[column].formater ) {
                     t->columns[column].formater(tmp, CSC_TABLE_MAXLEN,  t->columns[column].type, v);  
                } else {
                    snprintf(tmp, CSC_TABLE_MAXLEN,t->columns[column].format_str, v);
                }
                len = strnlen(tmp, CSC_TABLE_MAXLEN);
                t->columns[column].width =  MAX(t->columns[column].width, len);
                /* printf("Width: %d\n", t->columns[column].width );  */
            }
            break;
        case CSC_TABLE_FLOAT:
            {
                double v = va_arg(ap, double);
                char tmp[CSC_TABLE_MAXLEN];
                int len;
                t->columns[column].v.float_values[t->current_row] = v;
                if ( t->columns[column].formater ) {
                     t->columns[column].formater(tmp, CSC_TABLE_MAXLEN,  t->columns[column].type, v);  
                } else {
                    snprintf(tmp, CSC_TABLE_MAXLEN,t->columns[column].format_str, v);
                }
                len = strnlen(tmp, CSC_TABLE_MAXLEN);
                t->columns[column].width =  MAX(t->columns[column].width, len);
                /* printf("Width: %d\n", t->columns[column].width );  */

            }
            break; 
        case CSC_TABLE_STRING:
            {
                char * v = va_arg(ap, char *);
                int len;
                if ( t->columns[column].v.string_values[t->current_row] )
                    free(t->columns[column].v.string_values[t->current_row]); 
                t->columns[column].v.string_values[t->current_row] = strndup(v,CSC_TABLE_MAXLEN-1);
                if ( t->columns[column].formater ) {
                     char tmp[CSC_TABLE_MAXLEN];
                     t->columns[column].formater(tmp, CSC_TABLE_MAXLEN,  t->columns[column].type, v);  
                     len =strlen(tmp); 
                } else {
                    len = strlen(v);
                }
                t->columns[column].width = MAX(t->columns[column].width, len);
            }
            break;
    }
    t->columns[column].set[t->current_row] = 1; 
    return 0;
}

int csc_table_append_row(csc_table_t *t, csc_table_t *tab, int row) {
    int i; 
    if (!t) return -1; 
    if (!tab) return -1; 
    if ( row >= tab->number_of_rows) {
        csc_error_message("Row index %d is out of range (%d)\n", row, tab->number_of_rows); 
        return -1; 
    }
    if ( tab->number_of_columns != t->number_of_columns ) {
        csc_error_message("Number of columns of both tables does not fit ( t = %d , tab = %d) .\n", t->number_of_columns, tab->number_of_columns); 
        return -1; 
    }
    for (i = 0; i < t->number_of_columns; i++) {
        if (t->columns[i].type != tab->columns[i].type) {
            csc_error_message("Type of column %d differs.\n", i); 
            return -1; 
        }
    }
    if ( new_row_internal(t)) return -1; 

    for (i = 0; i < tab->number_of_columns; i++) {
        switch (tab->columns[i].type) {
            case CSC_TABLE_INTEGER:
                if ( tab->columns[i].set[row]) {
                    csc_table_set_entry(t, i, tab->columns[i].v.integer_values[row]); 
                }
                break;
            case CSC_TABLE_FLOAT:
                if ( tab->columns[i].set[row]) {
                    csc_table_set_entry(t, i, tab->columns[i].v.float_values[row]); 
                }
                break;
            case CSC_TABLE_STRING:
                if ( tab->columns[i].set[row]) {
                    csc_table_set_entry(t, i, tab->columns[i].v.string_values[row]); 
                }

                break;
        }
    }
    return 0; 
}

static void print_header_ascii(FILE *stream, csc_table_t *t, const char *colsep) 
{
    int i; 
    char tmp[CSC_TABLE_MAXLEN+1];

    fprintf(stream,"%s", t->comment->start);
    for (i = 0; i < t->number_of_columns; i++) {
        csc_strcenter(t->columns[i].name, t->columns[i].width, tmp);
        fprintf(stream,"%s",tmp);
        if ( i < t->number_of_columns-1) {
            if (!colsep) fprintf(stream," "); 
            else fprintf(stream,"%s",colsep);
        }
    }
    fprintf(stream,"\n");

}

static void print_row_ascii(FILE *stream, csc_table_t *t, const char *colsep, int r ) 
{
    int i; 
    char tmp[CSC_TABLE_MAXLEN+1];
    char output[CSC_TABLE_MAXLEN+1]; 
    int l = strlen(t->comment->start);
    for (i = 0; i < l ; i++) {
        fprintf(stream," ");
    }

    for (i = 0; i < t->number_of_columns; i++) {
        if ( t->columns[i].formater) {
            switch (t->columns[i].type) {
                case CSC_TABLE_INTEGER:
                    t->columns[i].formater(tmp, CSC_TABLE_MAXLEN, CSC_TABLE_INTEGER, t->columns[i].v.integer_values[r]);                  
                    break;
                case CSC_TABLE_FLOAT:
                    t->columns[i].formater(tmp, CSC_TABLE_MAXLEN, CSC_TABLE_FLOAT, t->columns[i].v.float_values[r]);                  
                    break;
                case CSC_TABLE_STRING:
                    t->columns[i].formater(tmp, CSC_TABLE_MAXLEN, CSC_TABLE_STRING, t->columns[i].v.string_values[r]);                  
                    break;
            }
        } else {
            switch (t->columns[i].type) {
                case CSC_TABLE_INTEGER:
                    snprintf(tmp, CSC_TABLE_MAXLEN, t->columns[i].format_str, t->columns[i].v.integer_values[r]);                  
                    break;
                case CSC_TABLE_FLOAT:
                    snprintf(tmp, CSC_TABLE_MAXLEN, t->columns[i].format_str, t->columns[i].v.float_values[r]);                  
                    break;
                case CSC_TABLE_STRING:
                    snprintf(tmp, CSC_TABLE_MAXLEN, t->columns[i].format_str, t->columns[i].v.string_values[r]);                  
                    break;

            }
        } 
        switch (t->columns[i].align ) {
            case CSC_TABLE_LEFT:
                csc_strleftalign(tmp, t->columns[i].width, output); 
                break;
            case CSC_TABLE_RIGHT:
                csc_strrightalign(tmp, t->columns[i].width, output); 

                break;
            case CSC_TABLE_CENTER:
                csc_strcenter(tmp, t->columns[i].width, output); 
                break;
        }
        fprintf(stream,"%s", output);
        if ( i < t->number_of_columns-1) {
            if (!colsep) fprintf(stream," "); 
            else fprintf(stream,"%s",colsep);
        }
    }
    fprintf(stream,"\n");


}


void csc_table_print_ascii(FILE *stream, csc_table_t *t, const char *colsep) 
{
    int r;
    if (!t) return;

    if ( t->comment) csc_table_comment_print(stream, t->comment);
    print_header_ascii(stream, t, colsep); 
    for (r = 0; r < t->number_of_rows; r++) {
        print_row_ascii(stream, t, colsep, r); 
    }
    return ; 
}

int csc_table_save_ascii(const char * filename, csc_table_t *t, const char *colsep)
{
    FILE *fp; 
    if (!t) return -1 ;

    fp = fopen(filename, "w"); 
    if (!fp ) {
        csc_error_message("Failed to open %s for writing.\n", filename); 
        return -1; 
    }
    csc_table_print_ascii(fp, t, colsep); 
    fclose(fp); 
    return 0; 
}

void csc_table_formater_integer(char *out, int len, csc_table_value_t type, ...)
{
    long v, vtmp, p ; 
    int c; 
    char *ptr; 
    va_list ap; 

    if ( type != CSC_TABLE_INTEGER ) {
        strncpy(out, "", len); 
        return; 
    }
    va_start(ap, type); 
    v = va_arg(ap, long); 

    vtmp = v; 
    p = 1; 
    while ( v != 0 ) {
        v = v /1000; 
        p *= 1000; 
    }
    v = vtmp; 
    if ( p >= 1000 ) p = p/1000; 
    /* printf("p %d v %d  \n", p, v); */
    ptr = out;   
    c = 0; 
    while ( p != 0 ) {
        int add = snprintf(ptr, 4, "%03ld", (v / p) % 1000); 
        c = c + add; 
        p /= 1000; 
        if ( p > 0 ) {
            ptr[add] = ','; 
            ptr += add+1; 
            c++; 
        } else {
            ptr += add; 
        }
        if ( len - c < 4) {
            csc_error_message("Buffer too short to format value.\n"); 
            strncpy(out, "", CSC_TABLE_MAXLEN); 
            return; 
        }
    }
    *ptr ='\0'; 
    return; 

}



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
            if ( t->columns[column].v.integer_values[i] > t->columns[column].v.integer_values[maxpos] )  {
                maxpos = i; 
            }
        } else if ( t->columns[column].type == CSC_TABLE_FLOAT && t->columns[column].set[i] ) {
            if ( t->columns[column].v.float_values[i] > t->columns[column].v.float_values[maxpos] )  {
                maxpos = i; 
            }

        } else if ( t->columns[column].type == CSC_TABLE_STRING && t->columns[column].set[i] ) {
            if ( strncmp(t->columns[column].v.string_values[i], t->columns[column].v.string_values[maxpos], CSC_TABLE_MAXLEN) > 0  )  {
                maxpos = i; 
            }
        }
    }
    return maxpos; 
}
/* Select the row with the maximum value in column    */

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
            if ( t->columns[column].v.integer_values[i] < t->columns[column].v.integer_values[minpos] )  {
                minpos = i; 
            }
        } else if ( t->columns[column].type == CSC_TABLE_FLOAT && t->columns[column].set[i] ) {
            if ( t->columns[column].v.float_values[i] < t->columns[column].v.float_values[minpos] )  {
                minpos = i; 
            }

        } else if ( t->columns[column].type == CSC_TABLE_STRING && t->columns[column].set[i] ) {
            if ( strncmp(t->columns[column].v.string_values[i], t->columns[column].v.string_values[minpos], CSC_TABLE_MAXLEN) < 0  )  {
                minpos = i; 
            }
        }
    }
    return minpos; 

}

