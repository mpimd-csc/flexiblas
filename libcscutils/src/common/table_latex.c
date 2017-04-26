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


static void print_row_latex(FILE *stream, csc_table_t *t, int r ) 
{
    int i; 
    char tmp[CSC_TABLE_MAXLEN+1];
    char output[CSC_TABLE_MAXLEN+1]; 

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
            fprintf(stream," & ");
        }
    }
    fprintf(stream,"\\\\\n");


}

static void print_header_latex(FILE *stream, csc_table_t *t) 
{
    int i; 
    char tmp[CSC_TABLE_MAXLEN+1];

    for (i = 0; i < t->number_of_columns; i++) {
        csc_strcenter(t->columns[i].name, t->columns[i].width, tmp);
        fprintf(stream,"%s",tmp);
        if ( i < t->number_of_columns-1) {
            fprintf(stream," & ");
        }
    }
    fprintf(stream,"\\\\\\hline\n");

}



int csc_table_save_latex(const char *filename, csc_table_t *t, int standalone)
{
    FILE *fp; 
    int c; 
    int r;
    char SAVE_COMMENT[CSC_TABLE_MAXLEN]; 
    
    if (!filename) return -1; 
    if (!t) return -1; 

    fp = fopen (filename, "w"); 
    if (!fp ) {
         csc_error_message("Failed to open %s for writing.\n", filename); 
         return -1; 
    }

    strncpy(SAVE_COMMENT, t->comment->start, CSC_TABLE_MAXLEN); 
    strncpy(t->comment->start, "% ", CSC_TABLE_MAXLEN); 

    /* Standalone    */
    if ( standalone ) {
        fprintf(fp, "\\documentclass[a4paper,10pt]{article}\n");
        fprintf(fp, "\\usepackage[utf8]{inputenc}\n");
        fprintf(fp, "\\usepackage[english]{babel}\n");
        fprintf(fp, "\\usepackage{amsmath}\n"); 
        fprintf(fp, "\\usepackage{amsfonts}\n"); 
        fprintf(fp, "\\usepackage{amssymb}\n"); 
        fprintf(fp, "\n");
        fprintf(fp, "\\begin{document}\n");
    }

    /* Comments   */
    csc_table_comment_print(fp, t->comment);

    /* Header  */
    fprintf(fp, "\\begin{tabular}{");
    for (c = 0; c < t->number_of_columns; c++) {
        if ( c == 0 ) fprintf(fp, "|");
        if ( t->columns[c].align == CSC_TABLE_LEFT )   fprintf(fp, "l|");
        if ( t->columns[c].align == CSC_TABLE_CENTER ) fprintf(fp, "c|");
        if ( t->columns[c].align == CSC_TABLE_RIGHT ) fprintf(fp, "r|");                
    }
    fprintf(fp, "}\\hline\n");

    print_header_latex(fp, t);  

    for (r = 0; r < t->number_of_rows; r++) {
        print_row_latex(fp, t, r); 
    }

    fprintf(fp, "\\hline\n");



    fprintf(fp, "\\end{tabular}\n");
    if ( standalone ) {
        fprintf(fp, "\\end{document}\n");
    }

    strncpy(t->comment->start, SAVE_COMMENT, CSC_TABLE_MAXLEN); 
    fclose(fp); 
    return 0; 
}

void csc_table_formater_integer_latex(char *out, int len, csc_table_value_t type, ...)
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
        int add; 
        if (  c == 0 ) {
            add = snprintf(ptr, 4, "%3ld", (v / p) % 1000); 
        } else {
            add = snprintf(ptr, 4, "%03ld", (v / p) % 1000); 
        }
        c = c + add; 
        p /= 1000; 
        if ( p > 0 ) {
            ptr[add] = '\\'; 
            ptr[add+1] = ','; 
            ptr += add+2; 
            c+=2; 
        } else {
            ptr += add; 
        }
        if ( len - c < 5) {
            csc_error_message("Buffer too short to format value.\n"); 
            strncpy(out, "", CSC_TABLE_MAXLEN); 
            return; 
        }
    }
    *ptr ='\0'; 
    return; 

}

