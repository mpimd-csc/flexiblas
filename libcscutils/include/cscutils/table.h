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

#ifndef CSC_TABLE_H
#define CSC_TABLE_H

#ifdef  __cplusplus
extern "C" {
#endif 
	/**
	 @file libcscutils/include/cscutils/table.h
	  @defgroup table  Table: ASCII and Tex Tables 
	
	  This part of the library contains routines to deal with ASCII printable tables. The tables
      can be print as classical ASCII tables in output files as well a been formated to Latex code. 
      @todo Latex support. 

	  @addtogroup table
	  @{ 
	*/
    
    /** Maximum length of an entry in a table cell. */
    #define CSC_TABLE_MAXLEN 256


    typedef enum {
        CSC_TABLE_INTEGER = 0, 
        CSC_TABLE_FLOAT, 
        CSC_TABLE_STRING
    } csc_table_value_t;

    typedef enum {
        CSC_TABLE_CENTER = 0, 
        CSC_TABLE_LEFT, 
        CSC_TABLE_RIGHT 
    } csc_table_align_t;

    typedef struct _csc_table_comment_t {
        char start[CSC_TABLE_MAXLEN]; 
        char **lines; 
        int len; 
    } csc_table_comment_t;

    typedef void (*csc_table_formater_t)(char *, int, csc_table_value_t, ...); 

    typedef struct _csc_table_column_t {
        csc_table_value_t type;
        char name[CSC_TABLE_MAXLEN];
        union {
            long   *integer_values;
            double *float_values;
            char  **string_values;
            void *ptr;
        } v;
        char format_str[CSC_TABLE_MAXLEN];
        csc_table_formater_t formater; 
        int *set;
        int len;
        int width;
        csc_table_align_t align; 
    } csc_table_column_t;

    typedef struct _csc_table_t {
        int number_of_columns;
        int number_of_rows;
        int current_row;
        csc_table_column_t *columns;
        int cp; 
        csc_table_comment_t * comment; 
    } csc_table_t;


    csc_table_t * csc_table_new(int continous_print);
    void csc_table_destroy(csc_table_t * t); 
    void csc_table_print_ascii(FILE *stream, csc_table_t *t, const char *colsep);
    int csc_table_save_ascii(const char *filename, csc_table_t *t, const char *colsep);
    int csc_table_save_latex(const char *filename, csc_table_t *t, int standalone);

    csc_table_t * csc_table_new_from_table(csc_table_t *table); 

    int  csc_table_add_column(csc_table_t *t, const char *name, csc_table_value_t type, csc_table_align_t align);
    void csc_table_column_destroy(csc_table_column_t col);
    int  csc_table_new_row(csc_table_t * t);
    int  csc_table_set_entry(csc_table_t *t, int column, ...);
    int  csc_table_column_set_format(csc_table_t *t, int column, const char *fmt);
    int  csc_table_column_set_formater(csc_table_t *t, int column, csc_table_formater_t fmt); 
    int csc_table_append_row(csc_table_t *t, csc_table_t *tab, int row); 


    int csc_table_comment_printf(csc_table_t *t, const char * comment, ...); 
    int csc_table_comment_sign(csc_table_t *t, const char *sign); 
    void csc_table_comment_date(csc_table_t *t ); 
    void csc_table_comment_cmd(csc_table_t *t, int argc, char ** argv); 

    /* Table Operations   */
    int csc_table_max_row(csc_table_t * t, int column); 
    int csc_table_min_row(csc_table_t * t, int column); 


    /* Table comments internal management */
    csc_table_comment_t * csc_table_new_comment(void); 
    void csc_table_destroy_comment(csc_table_comment_t * c); 
    void csc_table_comment_start(csc_table_comment_t *c, const char *start); 
    int  csc_table_comment_add(csc_table_comment_t *c, const char * fmt, ...); 
    int  csc_table_comment_add_va(csc_table_comment_t *c, const char * fmt, va_list ap); 
    void csc_table_comment_print(FILE *stream, csc_table_comment_t *c); 
    void csc_table_comment_clear(csc_table_comment_t *c); 
    


    /** @} 
     *
     * @defgroup table_formaters  Different Formaters for Numeric Values 
     * @ingroup table
     *
     * This group contains different formaters for the values inside the columns. 
     *
     * @addtogroup table_formaters
     * @{ 
     **/

     void csc_table_formater_integer(char *out, int len, csc_table_value_t type, ...); 
     void csc_table_formater_integer_latex(char *out, int len, csc_table_value_t type, ...); 
 
 /** @} */
#ifdef  __cplusplus
}
#endif 
#endif 
