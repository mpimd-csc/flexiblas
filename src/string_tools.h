/* $Id$ */
/* 
 Copyright (C) 2013, 2014  Martin Köhler, koehlerm@mpi-magdeburg.mpg.de

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License.
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/
#ifndef STRING_TOOLS_H
#define STRING_TOOLS_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#define MIN_CHUNK 256
static int getstr ( char **lineptr, size_t *n, FILE *stream, int terminator, int offset, int limit) 
{
	int nchars_avail;		/*  Allocated but unused chars in *LINEPTR.  */
	char *read_pos;		/*  Where we're reading into *LINEPTR. */
	int ret;
	size_t ni =0; 

	if (!lineptr || !n || !stream)   {
		return -1; 
	}
	
	if (!*lineptr)  {
		ni = MIN_CHUNK;
		*lineptr = calloc(sizeof(char), ni); 
		*n = ni; 
	}

	ni = *n ; 
	nchars_avail = ni - offset;
	read_pos = *lineptr + offset;
	for (;;) {
		int save_errno;
		int c;
		if (limit == 0) break;
		else {
			c = getc (stream);
			/*  If limit is negative, then we shouldn't pay attention to it, so decrement only if positive. */
			if (limit > 0) limit--;
		}
		save_errno = errno;
									  
		/*  We always want at least one char left in the buffer, since we
		 *  always (unless we get an error while reading the first char)
		 *  NULL-terminate the line buffer.  */
		assert((*lineptr + *n) == (read_pos + nchars_avail));
		if (nchars_avail < 2) {
			if (ni > MIN_CHUNK)    ni *= 2;
			else ni += MIN_CHUNK;
			*n = ni; 
			nchars_avail = ni + *lineptr - read_pos;
			*lineptr = (char *) realloc (*lineptr, sizeof(char) * (ni) );
			if (!*lineptr) {
				return -1; 
			}
			read_pos = ni - nchars_avail + *lineptr;
			assert((*lineptr + ni) == (read_pos + nchars_avail));
		}
		if (ferror (stream)) {
			/* Might like to return partial line, but there is no
			 * place for us to store errno.  And we don't want to just
			 * lose errno.  */
			errno = save_errno;
			return -1; 
		}
		if (c == EOF) { 
			/*  Return partial line, if any.  */
			if (read_pos == *lineptr)
				return -1;
			else
				break;
		}
		*read_pos++ = c;
		nchars_avail--;
		if (c == terminator)
			/*  Return the line.  */
			break;
	}
	/*  Done - NUL terminate and return the number of chars read.  */
	*read_pos = '\0';
	ret = read_pos - (*lineptr + offset);
	return ret;
}

static int _getline (char **line, size_t *nbytes, FILE *stream) {
	return getstr(line, nbytes, stream, '\n',0, 4096); 
}

static char *trim ( char * in) {
	char * out = in; 
	int i; 
	while ( (*out !='\0') && ( *out == ' ' || *out == '\t')) out++; 
	for (i = strlen(out); i > 0; i--) {
		if ( out[i-1] == ' ' || out[i-1]=='\t' || out[i-1]=='\n') {
			out[i-1]='\0'; 
		} else {
			break; 
		}
	}
	return out; 
}

#endif /* end of include guard: STRING_TOOLS_H */

