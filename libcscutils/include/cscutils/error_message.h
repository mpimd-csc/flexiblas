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

#ifndef ERROR_MESSAGE_H
#define ERROR_MESSAGE_H

#ifdef  __cplusplus
extern "C" {
#endif 

#include <stdarg.h>
	/**
	 @file libcscutils/include/cscutils/error_message.h
	  @defgroup error_message  Error Message: Global functions for display error messages. 
	
	  This part of the library contains routines to display error messages. This can be used 
	  to integrate trhe MATLAB error and warning functions mexfile-wide in all linked own 
	  written code without rewriting or relinking. 

	  The corresponding implementation is in src/common/error_message.c 

	  @addtogroup error_message
	  @{ 
	*/

	/**
	 * @brief Prototype for the print functions. 
	 *
	 * The csc_error_print_t typedef defines the prototype of for the error/waring/info
	 * print functions. 
	 * */
	typedef void (* csc_error_print_t) (const char *str); 


	/** 
	 * @brief Print an error message.
	 * @param[in] fmt  Error message to print. 
	 * @param[in] ...  Additional arguments for the message string.
	 *
	 * The csc_error_message function prints an error message. The fmt parameter 
	 * contains a string which may contain printf-like placeholders. If any placeholder
	 * is given, one has to specify the additional argument linke in printf. 
	 *
	 * \attention 
	 * The function is defined as weak symbol. That means it can be redefined and completely 
	 * replaced. 
	 */
	void csc_error_message(const char *fmt, ...);

	/** 
	 * @brief Print a warning  message.
	 * @param[in] fmt  Warning message to print. 
	 * @param[in] ...  Additional arguments for the message string.
	 *
	 * The csc_warn_message function prints a warning message. The fmt parameter 
	 * contains a string which may contain printf-like placeholders. If any placeholder
	 * is given, one has to specify the additional argument linke in printf. 
	 *
	 * \attention 
	 * The function is defined as weak symbol. That means it can be redefined and completely 
	 * replaced. 
	 */

	void csc_warn_message(const char *fmt, ...); 

	/** 
	 * @brief Print an information message.
	 * @param[in] fmt  Information message to print. 
	 * @param[in] ...  Additional arguments for the message string.
	 *
	 * The csc_info_message function prints an information message. The fmt parameter 
	 * contains a string which may contain printf-like placeholders. If any placeholder
	 * is given, one has to specify the additional argument linke in printf. 
	 *
	 * \attention 
	 * The function is defined as weak symbol. That means it can be redefined and completely 
	 * replaced. 
	 */
	void csc_info_message(const char *fmt, ...); 


	/** 
	 * @brief Set a new callback function for displaying error messages.  
	 * @param [in] fn   Function pointer to the callback function which should be used to display errors.  
     * @return Returns the old callback function as set before. 
	 *
	 * The csc_error_message_handle function sets the callback function to display errors. If the function 
	 * is invoked with a NULL argument the default is restored. The default uses fprintf to write to stderr  
	 * and is defined as
	 * \code{.c}
	 * static void error_print(const char * str){
	 *   fprintf(stderr, "ERROR:   %s\n", str);
	 * }
	 * \endcode 
	 */
	void * csc_error_message_handle( csc_error_print_t  fn); 

	/** 
	 * @brief Set a new callback function for displaying warning messages.  
	 * @param [in] fn   Function pointer to the callback function which should be used to display warnings.  
	 * @return Returns the old callback function as set before. 
     *
	 * The csc_warn_message_handle function sets the callback function to display warnings. If the function 
	 * is invoked with a NULL argument the default is restored. The default uses fprintf to write to stderr  
	 * and is defined as
	 * \code{.c}
	 * static void warn_print(const char * str){
	 *   fprintf(stderr, "WARNING:   %s\n", str);
	 * }
	 * \endcode 
	 */
	void *csc_warn_message_handle(  csc_error_print_t  fn); 

	/** 
	 * @brief Set a new callback function for displaying information messages.  
	 * @param [in] fn   Function pointer to the callback function which should be used to display information messages.  
     * @return Returns the old callback function as set before. 
	 *
	 * The csc_info_message_handle function sets the callback function to display information. If the function 
	 * is invoked with a NULL argument the default is restored. The default uses fprintf to write to stdout  
	 * and is defined as
	 * \code{.c}
	 * static void info_print(const char * str){
	 *   fprintf(stderr, "INFO:   %s\n", str);
	 * }
	 * \endcode 
	 */
	void *csc_info_message_handle(  csc_error_print_t  fn); 


	/** 
	 * @brief Change the memory management system used by the error message functions . 
	 * @param[in]  m  Pointer to the malloc replacement. It must have the same calling sequence as malloc. 
	 * @param[in]  r  Pointer to the realloc replacement. It must have the same calling sequence as realloc. 
	 * @param[in]  f  Pointer to the free replacement. It must have the same calling sequence as free. 
	 *
	 * The csc_error_message_memory function is use to replace the memory management functions used by the 
	 * printing functions \ref csc_error_message, \ref csc_warn_message and \ref csc_info_message. This can 
	 * be necessary if one uses the functions inside a MATLAB mexfile such that all dynamic memory allocations 
	 * are under control by the MATLAB, In this case one have to call the function like
	 * \code{.c}
	 *   csc_error_message_memory((void *) mxMalloc, (void*) mxRealloc, (void *) mxFree);	 *
	 * \endcode
	 */
	void csc_error_message_memory(void *m, void *r, void *f ); 

    
    /** 
     * @brief Show the current backtrace of a function. 
     * @return zero on success or a non zero error code. 
     *
     * The csc_show_backtrace function show the backtrace a the 
     * current execution point. This function works only on Linux, FreeBSD(>=10.0) 
     * and NetBSD(>=7.0). If no properly working backtrace funtion is found 
     * and error code is returned. 
     */
    int csc_show_backtrace(void); 


    /** Definition to identify a successful return from a function */ 
    #define CSC_SUCCESS 0
    /** Definition to identify a bad return value.  */
    #define CSC_ERROR   -1 
	/** @} */
#ifdef  __cplusplus
}
#endif 
#endif /* end of include guard: ERROR_MESSAGE_H */
