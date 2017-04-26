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

#ifndef CSC_IO_H

#define CSC_IO_H

#ifdef __cplusplus
extern "C" {
#endif
#if __GNUC__ >= 4 
#define CSC_ATTR_SCANF(pos1, pos2)   __attribute__ ((format (scanf, pos1, pos2)));
#else 
#define CSC_ATTR_SCANF(pos1, pos2)  
#endif 


	#include <stdio.h>
	#include <stdlib.h>
	#include <stdarg.h>
	#include <sys/stat.h>
	/**
	 @file libcscutils/include/cscutils/io.h
	  @defgroup io IO: File io with compression support. 

	   This part of the library contains routines to read and write compressed and uncompressed files. 
	   Depending on the available libraries this includes gzip, bzip2 and xz support. The file extension 
	   sets the compression when writing a file. The read function detect the compression using the magic 
	   number mechanism. 

	   If the project is build with CMake one can set the following variables to \b ON or \b OFF to 
	   influence the functionality of the IO module:
	   \li CSC_IO_ZLIB  Enable/Disable gzip/zlib compressed I/O, 
	   \li CSC_IO_BZIP2 Enable/Disable bzip2  compressed I/O, 
	   \li CSC_IO_LIBLZMA Enable/Disable lzma  compressed I/O.


	   \attention This part of the library depends on the \ref error_message module.

	  @addtogroup io
	  @{ 
	*/
	
	/**
	 * @brief Enumerator to determine the compression type. 
	 *
	 * The csc_io_compress_type_t enumerator defines an enum to represent the 
	 * use compression of a file. 
	 * */
	typedef enum {
		CSC_IO_FILE_ERROR = -1,       /**< Indicates that an error occurred during the compresseion detection. */
		CSC_IO_FILE_UNCOMPRESSED = 0, /**< Indicates that the file is uncompressed. */
		CSC_IO_FILE_GZIP = 1,         /**< Indicates that the file is compressed using gzip. */
		CSC_IO_FILE_BZIP2 = 2,        /**< Indicates that the file is compressed using bzip2. */      
		CSC_IO_FILE_XZ = 3            /**< Indicates that the file is compressed using xz(lzma). */
	} csc_io_compress_type_t;

	/** 
	 * @brief Enumerator to determine the file access mode. 
	 *
	 * The csc_io_mode_t enumerator specifies the access mode to a file. 
	 */
	typedef enum {
		CSC_IO_FILE_READ = 0,   /**< Specifies that the file is opened to read.  */
		CSC_IO_FILE_WRITE = 1,  /**< Specifies that the file is opened to write. */
	} csc_io_mode_t;

	/**
	 * @brief Size of the static buffer case for the file IO. 
	 *
	 * The CSC_IO_FILE_BUFFER_SIZE macro defines the size of the buffer cache for 
	 * the file IO. All write operations need to fill this buffer before they are written 
	 * to disk. Alternatively one has to use \ref csc_io_flush to write the buffer. 
	 */
	#define CSC_IO_FILE_BUFFER_SIZE 4096

	/**
	 * @brief Representation of a file including compression support. 
	 *
	 * The csc_io_file_t represents a file with compression support. This includes a data buffer and 
	 * a handler for the IO operations. It does not have the same functionality as the libc fopen system 
	 * but it is enough for most of out tasks. 
	 * */
	typedef struct __csc_io_file_t {
		void *handler;                        /**< Handle of the underlying io handler. */
		void *data;                           /**< Auxiliary data for the io handler. */
		char buffer[CSC_IO_FILE_BUFFER_SIZE]; /**< Buffer cache of size \ref CSC_IO_FILE_BUFFER_SIZE. */
		size_t pos;                           /**< Current position in the buffer cache. */
		size_t avail;                         /**< Available bytes in the buffer cache (Reading) */
		csc_io_mode_t mode;                     /**< Access mode.  */
		int  eof; 	                      /**< Indicate the end of the file */
	} csc_io_file_t;

	
	/** 
	 * @brief Return which compression is used for the given file. 
	 * @param[in] path 	Path of the file which should be checked.  
	 * @return Return an element of the \ref csc_io_compress_type_t enumerator identifying 
	 * the used compression type.
	 *
	 * The csc_io_is_compressed function checks a given file if it is compressed or not. If the file does 
	 * not exists the filename is checked for its extension otherwise the magic key of the file is used. 
	 */
	csc_io_compress_type_t  csc_io_is_compressed (const char *path);


	/** 
	 * @brief Open an eventually  compressed file. 
	 * @param[in]  path   Path of the file. 
	 * @param[in]  mode   Access mode to open the file. See \ref csc_io_mode_t for details. 
	 * @return A pointer to the opened file as an instance of \ref csc_io_file_t or NULL in case of an error. 
	 *
	 * The csc_io_open function opens an eventually compressed file. If the file is opened for writing the 
	 * compression is detected out of the file's extension. If the file is opened for reading this is done
	 * using the magic number of the file header. At the moment the following compressions formats are 
	 * supported:
	 * \li Zlib (gzip), extension: .gz, 
	 * \li BZip2,  extension: .bz2, 
	 * \li LZMA , extension: .xz. 
	 *
	 * \attention If a compression is not supported, e.g. the support is not compiled in, the file can not be 
	 * read but in the case of writing, the extension is truncated and the file is written as normal 
	 * uncompressed file. 
	 *
	 * \see csc_io_close. 
	 */
	csc_io_file_t* csc_io_open(const char * path, csc_io_mode_t mode);


	/** 
	 * @brief Flush the buffer cache during writing.  
	 * @param[in,out] f  File pointer.
	 * @return zero on success or a non zero error code otherwise 
	 *
	 * The csc_io_flush function flushes the buffer cache. That means all data stored in the cache is written 
	 * to disk now. Calling this function ensures that all data is stored on the disk now. 
	 */
	int csc_io_flush (csc_io_file_t  *f );


	/** 
	 * @brief Closes a given file.  
	 * @param[in] f File pointer 
	 * @return zero on success or a non zero error code otherwise 
	 *
	 * The csc_io_close function closes a given file. If the file was opened for writing the buffer cache is 
	 * flushed using \ref csc_io_flush before. The file pointer can be used afterwards again for a new file. 
	 */
	int csc_io_close (csc_io_file_t  *f );


	/** 
	 * @brief Test if the end of the file is reached.  
	 * @param[in] f File pointer 
	 * @return zero if the end of the file is not reached or a non-zero error value otherwise. 
	 *
	 * The csc_io_eof function checks if the end of a file is reached and return a non zero value 
	 * if this is the case. 
	 */
	int csc_io_eof(csc_io_file_t *f); 

	/*  Read  */

	/** 
	 * @brief Read a character from a file.  
	 * @param[in] f File pointer
	 * @return the character, EOF or -1 in case of an error. 
	 *
	 * The csc_io_getc function reads one character from the given file, If no more characters are left, it returns EOF. 
	 * 
	 */
	int     csc_io_getc (csc_io_file_t  * f );

	/**
	 * @brief Read a string from a file. 
	 * @param[in] f  File pointer. 
	 * @param[out] buf buffer where the read characters have been written to.
	 * @param[in] len maximum number of character to read
	 * @return The string respresented by buf or NULL in case of an error. 
	 *
	 * The csc_io_gets functions reads the next characters from f.  Reading stops after a
	 * newline, EOF or len characters.
	 */
	char*   csc_io_gets( char *buf, int len, csc_io_file_t * file );


	/** 
	 * @brief Get the next line from a file.  
	 * @param[in,out] buf  Pointer to a malloc'ed buffer to place the read string. 
	 * @param[in,out] len  Pointer to the length of the buffer. 
	 * @param[in]     f    File pointer. 
	 * @return zero on success or a non zero error code otherwise 
	 *
	 * The csc_io_getline function gets the next line from the file and stores it to the buffer. If the buffer is 
	 * too short it is reallocated using realloc. The number of read characters is stored in len. The newline character is copied 
	 * to the buffer as well. This behaviour is a contrast to \ref csc_io_gets. If buf == NULL and len == 0 the buffer will be allocated 
	 * using malloc. 
	 */
	ssize_t csc_io_getline( char **buf, size_t *len, csc_io_file_t *file);

	
	/** 
	 * @brief Read a line from a file and parse it.  
	 * @param[in] f  File pointer. 
	 * @param[in] fmt Format string for the parser. 
	 * @return The number of read elements, or -1 in case of an error. 
	 *
	 * The csc_io_scanf function combines the functionality of \ref csc_io_gets and sscanf to read 
	 * a line from a file and split it into different values. The format argument is compatible to 
	 * sscanf. 
	 *
	 * \attention The function is only available if the standard C library provides a vsscanf function. 
	 */
	int     csc_io_scanf(csc_io_file_t * f, const char *fmt, ...) CSC_ATTR_SCANF(2,3);


	/** 
	 * @brief Read binary data from a file.  
	 * @param[in,out] ptr  Buffer for the output. 
	 * @param[in]  selem   Size of one element 
	 * @param[in]  nelem   Number of elements to read. 
	 * @param[in]  f       File pointer 
	 * @return the number of read elements. 
	 *
	 * The csc_io_read function reads nelem elements of size selem from a given file. If the return value is not 
	 * equal to nelem, an error occured.  The buffer represented by ptr must be preallocated. 
	 */
	size_t	csc_io_read(void *ptr, size_t selem, size_t nelem, csc_io_file_t *f);

	/* Write */

	/** 
	 * @brief Write one character to a file 
	 * @param[in] c  Character to write. 
	 * @param[in] f  File pointer 
	 * @return The character casted to an int or EOF in case of an error. 
	 *
	 * The csc_io_putc function write on character to the file addressed by f. 
	 */
	int csc_io_putc ( int c, csc_io_file_t *f  );

	/** 
	 * @brief Write a string to a file 
	 * @param[in] buf  String buffer to write. 
	 * @param[in] f  File pointer 
	 * @return A non negative number on success or a negative value  in case of an error. 
	 *
	 * The csc_io_puts function writes a null-terminated string to a file. The 
	 * trailing '\0' is not written. 
	 */
	int csc_io_puts ( const char *buf, csc_io_file_t * f);

	/** 
	 * @brief Write a formated string to a file 
	 * @param[in] f  File pointer 
	 * @param[in] fmt  String containing the format specifiers. 
	 * @return The number of written characters or a negative value in case of an error. 
	 *
	 * The csc_io_printf function writes a formated string to a file. In this way it works 
	 * like fprintf and accepts the same format specifiers. 
	 *
	 * \attention This function is only available if the standard C library supports vsnprintf. 
	 */
	int csc_io_printf( csc_io_file_t * f, const char * fmt, ...);


	/** 
	 * @brief Write binary data to a file.  
	 * @param[in] ptr Buffer pointing to the input. 
	 * @param[in]  selem   Size of one element 
	 * @param[in]  nelem   Number of elements to write. 
	 * @param[in]  f       File pointer 
	 * @return The number of written elements. 
	 *
	 * The csc_io_write function writes nelems of size selem from the given buffer to a file. If the return value is not 
	 * equal to nelem, an error occured.
	 */
	size_t csc_io_write( void * ptr, size_t selem, size_t nelem, csc_io_file_t *f);




	/** 
	 * @brief Wrapper around mkdir(3) for recusive creation of directories 
	 * @param[in]  dir 	Path of the directory to create. 
	 * @param[in]  m 	Creation mode of the directory. 
	 * @return zero on success or a non zero error code otherwise 
	 *
	 * The csc_file_mkdir function creates a directory. If the directory is a is path, than 
	 * the directories will be created recusively. The mode parameter is described in the man pages
	 * of mkdir(3). A good choice is S_IRUSR|S_IWUSR|S_IXUSR.  
	 */
	int csc_file_mkdir(const char *dir, mode_t m); 

	
	/** 
	 * @brief Check if a file exists.  
	 * @param[in]  path Filename to check. 
	 * @return true (==-1) if the file exists or false(==0) otherwise. 
	 *
	 * The csc_file_exist function checks if a file exists and can be accessed. 
	 */
	int csc_file_exist(const char *path);  
	/**
	 * @} 
	 */


#ifdef __cplusplus
};
#endif



#endif /* end of include guard: CSC_IO_H */




