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

#ifdef  __cplusplus
extern "C" {
#endif 
	/**
	 @file libcscutils/include/cscutils/strutils.h
	  @defgroup strutils  String Utilities: Handle and manipulate strings
	
	  This part of the library contains routines to deal with strings. Like 
      routines that are not in the standard C library.

	  @addtogroup strutils 
	  @{ 
	*/

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

       
    /** @} */
#ifdef  __cplusplus
}
#endif 
#endif 
