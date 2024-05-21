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
#include "cscutils/cscutils_config.h"
#include <stdio.h>

#ifdef  __cplusplus
extern "C" {
#endif
    /**
     @file libcscutils/include/cscutils/table.h
      @defgroup table  Table: ASCII and Tex Tables

      This part of the library contains routines to deal with ASCII printable tables. The tables
      can be print as classical ASCII tables in output files as well a been formated to Latex code.

      @addtogroup table
      @{
    */

    /**
     * @brief The maximum length of a table cell entry.
     * Maximum character length of an entry in a table cell.
     */
    #define CSC_TABLE_MAXLEN 256


    /**
     * @brief Enumeration for the columns data type.
     *
     * The csc_table_value_t enumeration represents the possible values in a column.
     */
    typedef enum {
        CSC_TABLE_INTEGER = 0,  /**< The column contains integer values. */
        CSC_TABLE_FLOAT = 1,    /**< The column contains double precision values. */
        CSC_TABLE_STRING = 2    /**< The column contains strings. */
    } csc_table_value_t;

    /**
     * @brief Enumeration for the alignment in a column.
     *
     * The csc_table_align_t enumeration represents the possible alignments inside a column.
     */
    typedef enum {
        CSC_TABLE_CENTER = 0,  /**< The entry gets centered. */
        CSC_TABLE_LEFT = 1 ,   /**< The entry gets aligned to the left. */
        CSC_TABLE_RIGHT = 2    /**< The enrty gets aligned to the right. */
    } csc_table_align_t;

     /**
      * @internal
      * @brief The csc_table_comment_t structure represents a comment for a table.
      *
      * The csc_table_comment_t structure represents a comment for the table. Additionally,
      * it stores the comment sign which is printed in front of a comment. By default this is
      * initialized with "#" to obtain an output which is easily parsed by \b sed or \b awk.
      */
    typedef struct _csc_table_comment_t {
        char start[CSC_TABLE_MAXLEN+1];       /**< The comment start string. Printed in front of every comment line. */
        char **lines;                       /**< Array of strings containing the comments. */
        int len;                            /**< Number of elements in lines. */
    } csc_table_comment_t;


    /**
     * @brief The csc_table_formater_t typedef declares a function pointer to format cell entries.
     *
     * The csc_table_formater_t typedef declares a function pointer to declare cell entries. It takes
     * an output buffer and its length as as input as well a the data type of the column as arguments. The
     * last argument contains the actual value which needs to be formated. The function does not compute
     * the alignment inside the table cell.
     */
    typedef void (*csc_table_formater_t)(char *, int, csc_table_value_t, ...);

    /**
     * @internal
     * @brief The csc_table_comment_t structure represents a column inside a csc_table_t object.
     *
     * The csc_table_comment_t structure represents a column inside a csc_table_t structure. Each column
     * can only contain data of a single data type.
     */
    typedef struct _csc_table_column_t {
        csc_table_value_t type;         /**< Data type of the cell entries. */
        char name[CSC_TABLE_MAXLEN+1];    /**< Name of the column. Used as headline while printing. */
        union {
            long   *integer_values;     /**< Array containing the integer values of the cell entries. */
            double *float_values;       /**< Array containing the double precision values of the cell entries. */
            char  **string_values;      /**< Array containing the strings in the cell entries. */
            void *ptr;                  /**< Array for future extension and generic access to the other entries. */
        } v;                            /**< Union representing the column entries. */
        char format_str[CSC_TABLE_MAXLEN+1]; /**< Printf compatible format string for the table cells. */
        csc_table_formater_t formater; /**< Format function for the table cells. If it is set than the format_str is ignored. */
        int *set;                      /**< Array which indicates which rows in the column are set.  */
        int len;                       /**< Number of elments in the column. */
        int width;                     /**< Width of the column. Recomputed everytime an entry is added. */
        int minwidth;                  /**< Minimum width of the column  */
        csc_table_align_t align;       /**< Alignment of the column entries.  */
    } csc_table_column_t;

    /**
     * @brief The csc_table_t structure represents a table.
     *
     * The csc_table_t structure represents a table which can be printed on the screen, to an ascii file,
     * or exported as a Latex document. It is the main structure used by the csc_table module.
     */
    typedef struct _csc_table_t {
        int number_of_columns;             /**< Number of column in the table. */
        int number_of_rows;                /**< Number of rows in the table. */
        int current_row;                   /**< Current row. This is the index of the row all set entry function will use. */
        csc_table_column_t *columns;       /**< Array of the columns. */
        int cp;                            /**< Indicator if continous print is used. If true that last row is printed when a new row is started. */
        csc_table_comment_t * comment;     /**< Comments for the table. These are printed before the table starts.  */
    } csc_table_t;


    /**
     * \brief Create a new Table
     * \param[in] continous_print Indicates if the table should be printed while filling up the data.
     * \return A pointer to the newly created Table, NULL if the operation failed.
     *
     * The function csc_table_new creates a new table. If the continous_print argument is not zero, the
     * table will be printed to stdout while the data is filled up. Everytime a new row is create
     * the last row will be printed.
     *
     */
    csc_table_t * csc_table_new(int continous_print);

    /**
     * \brief Create a new table from an existing table's definition
     * \param[in] table  The table to copy the columns from
     * \return A pointer to the newly created table, NULL if the operation failed.
     *
     * The function csc_table_new_from_table creates a table from the definition of an other
     * table. That means we create a new table with the same columns as the input.
     */
    csc_table_t * csc_table_new_from_table(csc_table_t *table);

    /**
     * \brief Removes a Table from Memory
     * \param[in] table  Table to remove from memory
     *
     * The function csc_table_destroy removes a table from memory and frees
     * all allocated memory.
     *
     */
    void csc_table_destroy(csc_table_t * table);

    /**
     * \brief Clear all data in a table.
     * \param[in,out] table   Table to clear.
     *
     * The function csc_table_clear clears all data entries in a table.
     *
     */
    void csc_table_clear(csc_table_t * table);

    /**
     * \brief Print a table to a file stream
     * \param[in] stream  File stream to print the table on.
     * \param[in] table   Table to print.
     * \param[in] colsep  Separator for the columns.
     *
     * The function csc_table_print_ascii prints a table to a file stream, this
     * can be stdout, stderr, as well as a classical file. The colsep specifies
     * a string which is used to separate two columns.
     *
     */
    void csc_table_print_ascii(FILE *stream, csc_table_t *table, const char *colsep);

    /**
     * \brief Print the current row.
     * \param[in] t Table to print
     *
     * The csc_table_print_current_row function prints the current
     * active row to the stdout.
     */
    void csc_table_print_current_row(csc_table_t *t);

    /**
     * \internal
     * \brief Print a Table to stdout, helper function for Fortran
     * \param[in] table Table to print
     * \param[in] colsep Separator between the columns.
     *
     * The function csc_table_print_fortran prints a table to standard output. It is
     * equivalent to
     * \code
     *  csc_table_print_ascii(stdout, table,colsep);
     * \endcode
     * It is used in the Fortran interface to avoid trouble with the file descriptors.
     *
     */
    void csc_table_print_fortran(csc_table_t *table, const char *colsep);

    /**
     * \brief Write a ASCII style table to a file
     * \param[in] filename Filename for the output
     * \param[in] table    Table to write
     * \param[in] colsep   Separator between the columns
     * \return zero on sucess, non zero otherwise.
     *
     * The function csc_table_save_ascii write a ascii table to a file.
     *
     * \see csc_table_print_ascii
     */
    int csc_table_save_ascii(const char *filename, csc_table_t *table, const char *colsep);

    /**
     * \brief Write a Table as LaTex code to a file
     * \param[in] filename Filename for the output
     * \param[in] table    Table to write
     * \param[in] standalone Indicator if the output file can compiled alone.
     * \return zero on sucess, non zero otherwise.
     *
     * The function csc_table_save_latex write a table to a LaTex file. If standalone
     * is non zero, i.e. true, the output file can be compiled directly using latex
     * as standalone document.
     *
     * \see csc_table_save_ascii
     * \see csc_table_formater_integer_latex
     *
     */
    int csc_table_save_latex(const char *filename, csc_table_t *table, int standalone);


    /**
     * @brief Append a column to a table.
     * @param[in]   t       Table to operate on.
     * @param[in]   name    Name of the column.
     * @param[in]   type    Value type.
     * @param[in]   align   Alignment of the values.
     * @return A non-negative id of the column or a negative value in case of an error.
     *
     * The csc_table_add_column function appends a column to a table. The column is identified
     * by the returned id. The entires in the column, except of the header, are aligned, as desired
     * by the align argument.
     *
     * \see csc_table_align_t
     * \see csc_table_value_t
     */
    int  csc_table_add_column(csc_table_t *t, const char *name, csc_table_value_t type, csc_table_align_t align);

    /**
     * @brief Set the minimum width of a column.
     * @param[in]   t           Table to operate on.
     * @param[in]   column      The id of the column.
     * @param[in]   minwidth    The minimum width of the column.
     * @return  zero on sucess, non zero otherwise.
     *
     * The csc_table_column_minwidth function sets the minimum width of a column.
     *
     */
    int  csc_table_column_minwidth(csc_table_t *t, int column, int minwidth);

    /**
     * @brief Add a new row to the table.
     * @param[in]   t       Table to operate on.
     * @return  zero on sucess, non zero otherwise.
     *
     * The csc_table_new_row function appends a row to a table. The following csc_table_set_entry* functions
     * will insert the values in this row.
     *
     * @see csc_table_set_entry
     * @see csc_table_set_entry_integer
     * @see csc_table_set_entry_float
     * @see csc_table_set_entry_string
     */
    int  csc_table_new_row(csc_table_t * t);

    /**
     * @brief Append a row from another table.
     * @param[in]   t       Table, where to append the row.
     * @param[in]   tab     Table, where to get the row from.
     * @param[in]   row     Number of the row from table tab to append.
     * @return zero on sucess, non-zero otherwise
     *
     * The csc_table_append_row function appends a row from another table. The second
     * table must have the same column structure as the table, where the row
     * should be added.
     *
     * @see csc_table_new_row
     * @see csc_table_new_from_table
     */
    int  csc_table_append_row(csc_table_t *t, csc_table_t *tab, int row);

    /**
     * @brief Set an entry in the current row.
     * @param[in]   t       Table to operate on.
     * @þaram[in]   column  Column of the entry.
     * @param[in]   ...     Value to set.
     * @return  zero on sucess, non zero otherwise.
     *
     * The csc_table_set_entry function sets an entry in a column of the current row.
     * Depending on the value type of the column the variadic argument ... is casted to
     * int, double, or char*, respectively. Alternatively the type safe functions can be used.
     *
     * @see csc_table_set_entry_integer
     * @see csc_table_set_entry_float
     * @see csc_table_set_entry_string
     */
    int  csc_table_set_entry(csc_table_t *t, int column, ...);

    /**
     * @brief Set an integer entry in the current row.
     * @param[in]   t       Table to operate on.
     * @þaram[in]   column  Column of the entry.
     * @param[in]   val     Value to set.
     * @return  zero on sucess, non zero otherwise.
     *
     * The csc_table_set_entry_integer function sets an integer entry in a column of the current row.
     * If the value type of the column is not integer, a conversion is tried.
     *
     * @see csc_table_set_entry
     * @see csc_table_set_entry_float
     * @see csc_table_set_entry_string
     */
    void  csc_table_set_entry_integer(csc_table_t *t, int column, int val);

    /**
     * @brief Set a double precision entry in the current row.
     * @param[in]   t       Table to operate on.
     * @þaram[in]   column  Column of the entry.
     * @param[in]   val     Value to set.
     * @return  zero on sucess, non zero otherwise.
     *
     * The csc_table_set_entry_float function sets a double precision entry in a column of the current row.
     * If the value type of the column is not double precision, a conversion is tried.
     *
     * @see csc_table_set_entry
     * @see csc_table_set_entry_integer
     * @see csc_table_set_entry_string
     */
    void  csc_table_set_entry_float(csc_table_t *t, int column, double val);

    /**
     * @brief Set a string entry in the current row.
     * @param[in]   t       Table to operate on.
     * @þaram[in]   column  Column of the entry.
     * @param[in]   val     Value to set.
     * @return  zero on sucess, non zero otherwise.
     *
     * The csc_table_set_entry_string function sets a double precision entry in a column of the current row.
     * If the value type of the column is not string, a conversion is done.
     *
     * @see csc_table_set_entry
     * @see csc_table_set_entry_integer
     * @see csc_table_set_entry_float
     */
    void  csc_table_set_entry_string(csc_table_t *t, int column, char *val);

    /**
     * @brief Set the C-format string for the column entries.
     * @param[in]   t       Table to operate on.
     * @param[in]   column  The identifier of the affected column.
     * @þaram[in]   fmt     C-like format string.
     * @return zero on sucess, non-zero otherwise
     *
     * The csc_table_column_set_format function sets the format string for the values
     * of a column. The format string needs to be printf compatible, The default strings are
     * "%d", "%lg", or "%s" for integer, double precision, or string columns, respectively.
     *
     * More advanced formats can be set using the \ref csc_table_column_set_formater function.
     */
    int  csc_table_column_set_format(csc_table_t *t, int column, const char *fmt);

     /**
     * @brief Set the format function for the column entries.
     * @param[in]   t       Table to operate on.
     * @param[in]   column  The identifier of the affected column.
     * @þaram[in]   fmt     Function to format the column entries.
     * @return zero on sucess, non-zero otherwise
     *
     * The csc_table_column_set_formater function sets the format function for the values
     * of a column. The function needs to fulfill the interface \ref csc_table_formater_t.
     * The function is called on each column entry once it is printed. The function can be
     * used to set up proper latex formating in the table cells.
     *
     * @see csc_table_column_set_format
     * @see csc_table_formater_integer
     * @see csc_table_formater_integer_latex
     */
    int  csc_table_column_set_formater(csc_table_t *t, int column, csc_table_formater_t fmt);

    /* Table Operations   */
    /**
     * @brief Obtain the row number of a maximum entry in a column.
     * @param[in]   t       Table to operate on.
     * @param[in]   column  Column, where to search for a maximum.
     * @return the row number containing the maximum or a negative value on error.
     *
     * The csc_table_max_row function returns the number of the row, where the maximum in the
     * given column is found. If the maximum is not unique, the last found one is returned.
     *
     * @see csc_table_min_row
     * @see csc_table_amax_row
     * @see csc_table_amin_row
     */
    int csc_table_max_row(csc_table_t * t, int column);

    /**
     * @brief Obtain the row number of a minimum entry in a column.
     * @param[in]   t       Table to operate on.
     * @param[in]   column  Column, where to search for a minimum.
     * @return the row number containing the minimum or a negative value on error.
     *
     * The csc_table_min_row function returns the number of the row, where the minimum in the
     * given column is found. If the minimum is not unique the last found one is returned.
     *
     * @see csc_table_max_row
     * @see csc_table_amax_row
     * @see csc_table_amin_row
     */
    int csc_table_min_row(csc_table_t * t, int column);

    /**
     * @brief Obtain the row number of a absolute value maximum entry in a column.
     * @param[in]   t       Table to operate on.
     * @param[in]   column  Column, where to search for a absolute value maximum.
     * @return the row number containing the maximum or a negative value on error.
     *
     * The csc_table_amax_row function returns the number of the row, where the absolute value maximum in the
     * given column is found. If the maximum is not unique, the last found one is returned.
     * In the case of string valued columns the result is the same as \ref csc_table_max produces.
     *
     * @see csc_table_min_row
     * @see csc_table_max_row
     * @see csc_table_amin_row
     */
    int csc_table_amax_row(csc_table_t * t, int column);

    /**
     * @brief Obtain the row number of a absolute value minimum entry in a column.
     * @param[in]   t       Table to operate on.
     * @param[in]   column  Column, where to search for a absolute value minimum.
     * @return the row number containing the minimum or a negative value on error.
     *
     * The csc_table_amin_row function returns the number of the row, where the absolute value minimum in the
     * given column is found. If the minimum is not unique the last found one is returned.
     * In the case of string valued columns the result is the same as \ref csc_table_min produces.
     *
     * @see csc_table_max_row
     * @see csc_table_amax_row
     * @see csc_table_min_row
     */
    int csc_table_amin_row(csc_table_t * t, int column);

    /**
     * @brief Append a comment to the table, using printf
     * @param[in]   t       Table to operate on
     * @param[in]   comment Comment including printf placeholders.
     * @param[in]   ...     Variadic argument for the printf placeholders.
     * @return zero on sucess, non-zero otherwise.
     *
     * The csc_table_comment_printf appends a comment to the table using the
     * printf functionallity of C. The comment can be formated as known from
     * the printf function.
     *
     */
    int csc_table_comment_printf(csc_table_t *t, const char * comment, ...);

    /**
     * @brief Append a text comment to the table.
     * @param[in]   t       Table to operate on
     * @param[in]   text    Comment text to add,
     * @return zero on sucess, non-zero otherwise.
     *
     * The csc_table_comment_text function appends a text to the comments of
     * the table. The text is transfered as it is. Printf placeholders are
     * ignored.
     *
     */
    int csc_table_comment_text(csc_table_t *t, const char * text);

    /**
     * @brief Append the current date to the table.
     * @param[in]   t       Table to operate on
     * @return zero on sucess, non-zero otherwise.
     *
     * The csc_table_comment_date function appends the current date/time stamp to the comments of
     * the table.
     *
     */
    void csc_table_comment_date(csc_table_t *t );

    /**
     * @brief Append the command line to the comments of the table.
     * @param[in]   t       Table to operate on
     * @param[in]   argc    The number of command line arguments.
     * @param[in]   aegv    The vector containing the command line arguments.
     * @return zero on sucess, non-zero otherwise.
     *
     * The csc_table_comment_cmd function appends the current command line arguments
     * as comment to the table.
     *
     */
    void csc_table_comment_cmd(csc_table_t *t, int argc, char ** argv);

     /**
     * @brief Append a brief system information to the comments of a table.
     * @param[in]   t       Table to operate on
     *
     * The csc_table_comment_sysinfo function appends a brief system information to
     * the comments of the table. This looks like:
     * \code
     * # === System Information ===
     * # Hostname: munin
     * # Opering System: Linux (5.3.0-42-generic)
     * # Architecture: x86_64
     * # Compiler: GCC 9.2.1
     * # CPU: Intel(R) Celeron(R) CPU N3450 @ 1.10GHz
     * # CPU Cores: 4
     * # Memory: 4 GiB
     * # ===========================
     * \endcode
     *
     */
    void csc_table_comment_sysinfo(csc_table_t * t);

    /**
     * @brief Append a brief OpenMP information to the comments of a table.
     * @param[in]   t       Table to operate on
     *
     * The csc_table_comment_openmp_info function appends a brief OpenMP information to
     * the comments of the table. This looks like:
     * \code
     * # === OpenMP Environment  ===
     * # omp_get_num_threads: 1
     * # omp_get_num_procs: 6
     * # omp_get_dynamic: 0
     * # omp_get_nested: 0
     * # omp_get_schedule: dynamic (1)
     * # omp_get_thread_limit: 2147483647
     * # omp_get_max_active_levels: 2147483647
     * # omp_get_proc_bind: false
     * # ===========================
     * \endcode
     *
     */
    void csc_table_comment_openmp_info(csc_table_t *t);

    /**
     * @brief Append the CMake command line to the comments of a table.
     * @param[in]   t       Table to operate on
     *
     * The csc_table_comment_cmake adds the CMake command line to
     * the comments of the table.
     */
    void csc_table_comment_cmake(csc_table_t *t);

    /**
     * @brief Append the used compiler flags to he comments of a table.
     * @param[in]   t       Table to operate on
     *
     * The csc_table_comment_compilerflags adds the used compiler flags
     * the comments of the table. This depends on the set of enabled
     * languages.
     */
    void csc_table_comment_compilerflags(csc_table_t *t);

    /**
     * @brief Append all additional informations to the comments of a table.
     * @param[in]   t       Table to operate on
     *
     * The csc_table_comment_allinfo adds the information from
     *
     * \li csc_table_comment_sysinfo
     * \li csc_table_comment_openmp_info
     * \li csc_table_comment_cmake
     * \li csc_table_comment_compilerflags
     *
     * to the comments of the given table.
     */
    void csc_table_comment_allinfo(csc_table_t * t);

    /**
     * @brief Change the sign which introduces the comments.
     * @param[in]   t       Table to operate on
     * @param[in]   sign    New begin string of the comments.
     * @return zero on sucess, non-zero otherwise.
     *
     * The csc_table_comment_sign function changes the string sequence the
     * comment row begin. The default is \b #.
     *
     */
    int csc_table_comment_sign(csc_table_t *t, const char *sign);

    /* Table comments internal management */

    /**
     * @internal
     * @brief Create a new comment block in a table.
     * @return Pointer to the new table, or NULL
     *
     * The csc_table_new_comment function creates a new comment block.
     *
     */
    csc_table_comment_t * csc_table_new_comment(void);

    /**
     * @internal
     * @brief Destroy a comment block.
     * @param[in] c     The comment to destroy.
     *
     * The csc_table_destroy_comment functions destroys a comment and clears the memory.
     */
    void csc_table_destroy_comment(csc_table_comment_t * c);

    /**
     * @internal
     * @brief Update the start string of  a comment block.
     * @param[in]  c        Comment.
     * @param[in]  start    Start string.
     *
     * The csc_table_comment_start functions updates the start string of a comment block.
     */
    void csc_table_comment_start(csc_table_comment_t *c, const char *start);

    /**
     * @internal
     * @brief Append a comment. (printf)
     * @param[in] c         Comment
     * @param[in] fmt       Format string
     * @param[in] ...       Variadic arguments for the format.
     * @return zero on sucess, non-zero otherwise.
     *
     * The csc_table_comment_add functions appends a comment using printf-stlye format string.
     *
     */
    int  csc_table_comment_add(csc_table_comment_t *c, const char * fmt, ...);

    /**
     * @internal
     * @brief Append a comment. (vaargs)
     * @param[in] c         Comment
     * @param[in] fmt       Format string
     * @param[in] ap        Variadic arguments for the format.
     * @return zero on sucess, non-zero otherwise.
     *
     * The csc_table_comment_add functions appends a comment using printf-stlye format string and a va_list argument list.
     *
     */
    int  csc_table_comment_add_va(csc_table_comment_t *c, const char * fmt, va_list ap);

    /**
     * @internal
     * @brief Print the comments to a file stream.
     * @param[in] stream            File stream to print the comments.
     * @param[in] c                 Comments to print.
     *
     * The csc_table_comment_print function prints the comments to a file stream.
     *
     */
    void csc_table_comment_print(FILE *stream, csc_table_comment_t *c);

    /**
     * @internal
     * @brief Remove the content of the comment.
     * @param[in] c         Comment.
     *
     * The csc_table_comment_clear function removes all the content of the given
     * comment.
     *
     */
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

    /**
     * @brief Format an integer value.
     * @param[out]  out         Outputbuffer of length CSC_TABLE_MAXLEN
     * @param[in]   len         Maximum length of the desired formated output.
     * @param[in]   type        Type of the value.
     * @param[in]   ...         The value to format as variadic argument.
     *
     * The csc_table_formater_integer function formats an integer value such that at
     * the tousand, million, billion position a comma is included.
     *
     * Example:
     * \code
     *  1234567 -> 1,245,567
     * \endcode
     */
    void csc_table_formater_integer(char *out, int len, csc_table_value_t type, ...);

    /**
     * @brief Format an integer value for nice latex printing.
     * @param[out]  out         Outputbuffer of length CSC_TABLE_MAXLEN
     * @param[in]   len         Maximum length of the desired formated output.
     * @param[in]   type        Type of the value.
     * @param[in]   ...         The value to format as variadic argument.
     *
     * The csc_table_formater_integer_latex function formats an integer value such that at
     * the tousand, million, billion position a small space is included.
     */
    void csc_table_formater_integer_latex(char *out, int len, csc_table_value_t type, ...);

    /** @} */
#ifdef  __cplusplus
}
#endif
#endif
