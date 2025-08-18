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

#ifndef CSC_STRUTILS_H
#define CSC_STRUTILS_H

#include "cscutils/cscutils_config.h"

#ifdef  __cplusplus
extern "C" {
#endif
    /**
     * @file libcscutils/include/cscutils/strutils.h
     *
     * @defgroup strutils  String Utilities: Handle and manipulate strings
     * @brief This part of the library contains routines to deal with strings. Like routines that are not in the standard C library.
     *
     * The strutils are a collection of additional string handling function that are not available in the standard C library or
     * they are replacesment for non-POSIX function that are not available on all platforms.
     *
     * @addtogroup strutils
     * @{
    */

    /**
      * \brief Return a pointer to a null-terminated byte string.
      * \param[in] str      pointer to the null-terminated byte string to duplicate
      * \param[in] size     max number of bytes to copy from @p str
      * \return A pointer to the newly allocated string, or a null pointer if an error occurred.
      *
      * The function csc_strndup returns a null-terminated byte string, which contains copies
      * of at most @p size bytes from the string pointed to by @p str.
      * If the function @c strndup is available, it gets called.
      *
      */
    char *csc_strndup(const char *str, size_t size);

     /**
      * \brief Compare two strings ignoring the case.
      * \param[in] s1   Frist String
      * \param[in] s2   Second String
      * \return A value <0, ==0, or >0 depending if s1 < s2, s1 == s2, or s1>s2.
      *
      * The function csc_strcasecmp  function compares to strings ignoring the case.
      *
      */
    int csc_strcasecmp (const char *s1, const char *s2);

    /**
     * \brief Compare two strings with a given maximum length.
     * \param[in] s1   Frist String
     * \param[in] s2   Second String
     * \param[in] n    Maximum number of characters to compare
     * \return A value <0, ==0, or >0 depending if s1 < s2, s1 == s2, or s1>s2.
     *
     * The function csc_strncmp compares to strings of maximum length n. If the
     * strings s1 or s2 are longer only the first n characters will be checked,
     *
     */
    int csc_strncmp(const char *s1, const char *s2, size_t n);

    /**
     * \brief Compare two strings with a given maximum length ignoring the case.
     * \param[in] s1   Frist String
     * \param[in] s2   Second String
     * \param[in] n    Maximum number of characters to compare
     * \return A value <0, ==0, or >0 depending if s1 < s2, s1 == s2, or s1>s2.
     *
     * The function csc_strncasecmp compares to strings of maximum length n. If the
     * strings s1 or s2 are longer only the first n characters will be checked,
     * In contrast to \ref csc_strncmp it ignores the case.
     *
     */
    int csc_strncasecmp(const char *s1, const char *s2, size_t n);

    /**
     * \brief Center a strings inside a given space.
     * \param[in]  str      String to center.
     * \param[in]  width    Width of the space in which the string is placed.
     * \param[out] output   Output buffer of length > width+1
     *
     * The function csc_strcenter centers a string inside an array of given length.
     * Thereby the width parameter gives the maximum width of the string. If the
     * given string is shorter it will be centered inside this space. That means output
     * must be at least width+1 element long. If the string is longer than the width allows
     * it will be cut off.
     *
     */
    void csc_strcenter(const char *str, int width, char *output);

    /**
     * \brief Left align a string inside a given space.
     * \param[in]  str      String to align.
     * \param[in]  width    Width of the space in which the string is placed.
     * \param[out] output   Output buffer of length > width+1
     *
     *
     * The function  csc_strleftalign left aligns a string inside an array of
     * given length. Thereby, the width parameter gives the maximum available spaces.
     * If the string is shorter than width elements it will be left aligned inside the array.
     * That means the output buffer needs to take at least width+1 elements to store the final
     * "\0" as well. If the given string is longer it will be truncated to width characters.
     *
     */
    void csc_strleftalign(const char *str, int width, char *output);

    /**
     * \brief Left align a string inside a given space.
     * \param[in]  str      String to align.
     * \param[in]  width    Width of the space in which the string is placed.
     * \param[out] output   Output buffer of length > width+1
     *
     *
     * The function  csc_strrightalign right aligns a string inside an array of
     * given length. Thereby, the width parameter gives the maximum available spaces.
     * If the string is shorter than width elements it will be right aligned inside the array.
     * That means the output buffer needs to take at least width+1 elements to store the final
     * "\0" as well. If the given string is longer it will be truncated to width characters.
     *
     */
    void csc_strrightalign(const char *str, int width, char *output);

    /**
     * \brief Check if a string begins with a given sequence,
     * \param[in] haystack String to search in
     * \param[in] needle   String to search for
     * \return  0 if haystack does not begin with needle, 1 otherwise.
     *
     * The function csc_strbegin checks if a string "haystack" begins with the string
     * "needle" and returns a non zero value in this case.
     *
     * \sa csc_strcasebegin
     */
    int csc_strbegin(const char * haystack, const char *needle);

    /**
     * \brief Check if a string begins with a given sequence ignoring the case,
     * \param[in] haystack String to search in
     * \param[in] needle   String to search for
     * \return  0 if haystack does not begin with needle, 1 otherwise.
     *
     * The function csc_strcasebegin checks if a string "haystack" begins with the string
     * "needle" and returns a non zero value in this case. In contrast to \ref csc_strbegin this
     * function ignores the case.
     *
     * \sa csc_strcasebegin
     */

    int csc_strcasebegin(const char * haystack, const char *needle);

    /**
     * \brief Convert a string to upper case
     * \param[inout] str    String to convert to upper case
     * \return The pointer to the string.
     *
     * The csc_struppercase functions converts a string in-place to upper case and returns
     * the pointer to its start for easy usage.
     * */
    char *csc_struppercase(char *str);

    /**
     * \brief Convert a string to lower case
     * \param[inout] str    String to convert to lower case
     * \return The pointer to the string.
     *
     * The csc_strlowercase functions converts a string in-place to lower case and returns
     * the pointer to its start for easy usage.
     * */
    char *csc_strlowercase(char *str);


    /**
     * \brief Check if a string is a valid integer value.
     * \param[in] str   Input string
     * \return 1 if the string contains a valid integer, 0 otherwise
     *
     * The csc_str_is_valid_int function checks if a string contains a valid integer value.
     * */
    int csc_str_is_valid_int(const char *str);

    /**
     * \brief Removing subsequent doublings of a character
     * \param[inout]   str      String to operate on.
     * \param[in]      dup      Character to de-double.
     *
     * The csc_strremovedup function remove subsequent doublings of a character
     * in a string.
     *
     * \b Example:
     * \code
     *  char *str = "//home/user//test";
     *  csc_strremovedup(str, '/');
     *  printf("%s\n", str);   // will print "/home/user/test"
     * \endcode
     */
    void csc_strremovedup(char * str, char dup);

    /**
     * \brief Remove all beginning whitespace from the left of a string.
     * \param[inout] str        String to operate on.
     * \return A null-terminated string in the same memory location as str.
     *
     * The csc_str_ltrim function removes all beginning whitespace characters in a string.
     * It returns the pointer str again for easier usage.
     *
     * \see csc_str_rtrim
     */
    char *csc_str_ltrim(char *str);

    /**
     * \brief Remove all trailing whitespace from the right of a string
     * \param[inout] str        String to operate on.
     * \return A null-terminated string in the same memory location as str.
     *
     * The csc_str_rtrim function removes all trailing whitespace characters in a string.
     * It returns the pointer str again for easier usage.
     *
     * \see csc_str_ltrim
     */
    char *csc_str_rtrim(char *str);

    /**
     * \brief Remove all occurrences of the a character from a string.
     * \param[inout]  str   String to operate on
     * \param[in]     c     Character to remove.
     * \return A null-terminated string in the same memory location as str.
     *
     * The csc_str_remove_char function removes all occurrences of the character c
     * from the string str.
     *
     */
    char *csc_str_remove_char(char *str, char c);
    /** @} */
#ifdef  __cplusplus
}
#endif
#endif
