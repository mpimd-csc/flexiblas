/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) Martin Koehler, 2015
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cscutils/io.h"

int main(int argc, char **argv)
{
	csc_io_file_t *fp;
	char sbuf[128]; 


	if (argc != 2) {
		fprintf(stderr, "Need 1 argument\n");
		return -1; 
	}

	fp = csc_io_open (argv [1], CSC_IO_FILE_READ); 
	if ( !fp ) {
		fprintf(stderr, "Open File Failed.\n");
		return -1;
	}

	printf("Get char: %c\n", csc_io_getc(fp));
	printf("Get String: %s\n", csc_io_gets(sbuf, 127, fp));
	{
		size_t len = 0; 
		char *buf = NULL; 
		ssize_t ret = 0; 

		ret = csc_io_getline(&buf, &len, fp); 
		printf("Read %d bytes: %s\n",(int) ret, buf);
		free(buf); 
	}
	{
		int v1, v2; 
		double v3; 
		int matched; 
		matched = csc_io_scanf(fp, "%d %d %lg", &v1, &v2, &v3);
		printf("Next test matched (%d) : %d %d %lg\n", matched, v1, v2, v3);
	}
	csc_io_close(fp); 

	/* Write   */
	fp = csc_io_open("test.txt", CSC_IO_FILE_WRITE); 
	if ( !fp ) {
		fprintf(stderr, "Open File Failed.\n");
		return -1;
	}
	csc_io_putc('A', fp); 
	csc_io_puts("BCDEF\n",fp); 
	csc_io_printf(fp, "Hallo werlt %d %lg %x\n", 23,1304.34, 16); 
	csc_io_close(fp); 


	/* Binary test*/ 
	{
		char *buf, *buf2; 
		int i = CSC_IO_FILE_BUFFER_SIZE * 15 -47; 
		int j; 
		fp = csc_io_open("test.bin.bz2", CSC_IO_FILE_WRITE); 
		if ( !fp ) {
			printf("error\n");
			return -1; 
		}
		buf = malloc(CSC_IO_FILE_BUFFER_SIZE * 15 -  47); 
		buf2 = malloc(CSC_IO_FILE_BUFFER_SIZE * 15 -  47); 

		for ( j = 0 ; j < i ; j++) {
			buf[j] = j % 256; 
		}
		csc_io_write(buf, sizeof(char), i, fp); 
		csc_io_close(fp); 

		fp = csc_io_open("test.bin.bz2", CSC_IO_FILE_READ); 
		if ( !fp ) {
			printf("error\n");
			return -1; 
		}
		csc_io_read(buf2, sizeof(char), i, fp); 

		csc_io_close(fp); 

		if ( memcmp ( buf, buf2, i) == 0) {
			printf("Correct \n");
		} else {
			printf("wrong\n");
		}
		
		free(buf); 
		free(buf2); 
	}

	return 0;
}

