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

#ifndef _CSC_COMPRESSED_IO_H
#define _CSC_COMPRESSED_IO_H

#ifdef  __cplusplus
extern "C" {
#endif
#include "cscutils/io.h"


#ifdef _WIN32
#define strdup _strdup
#endif

	typedef struct _compressed_io_handler_t {
		char extension[10];
		csc_io_compress_type_t type;
		char magic[10];
		int (*open)(void **data, const char * path, const csc_io_mode_t mode, struct _compressed_io_handler_t *handler);
		int (*close)(void **data);
		size_t (*read)(void *data, void *buf, size_t len);
		size_t (*write)(void *data, void *buf, size_t len);
	} _compressed_io_handler;


	extern  _compressed_io_handler compress_methods[];
	extern int _compressed_io_handler_len;
	extern _compressed_io_handler compress_fallback; 

	void csc_io_init ();

#ifdef  __cplusplus
}
#endif


#endif
