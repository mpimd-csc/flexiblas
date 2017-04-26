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
 * Copyright (C) Martin Koehler, 2013-2015
 */

#include <stdio.h>
#include <stdlib.h> 
#include <stdint.h>
#include <complex.h>
#include <dlfcn.h>
#include "flexiblas_backend.h" 

/* Necessary to tell FlexiBLAS that this backend needs to be loaded using RTLD_GLOBAL|RTLD_LOCAL */
FLEXIBLAS_GLOBAL_BINDING
FLEXIBLAS_DEEP_BINDING
FLEXIBLAS_LAZY_BINDING



/*-----------------------------------------------------------------------------
 *  Init function, called once when FlexiBLAS initializes the backend. 
 *-----------------------------------------------------------------------------*/
FLEXIBLAS_INIT_FUNCTION() {
	/* Return 0 on success, != 0 otherwise   */
	return 0 ; 
}



/*-----------------------------------------------------------------------------
 *  Exit function, called once when the program finishes. 
 *-----------------------------------------------------------------------------*/
FLEXIBLAS_EXIT_FUNCTION() {
	return; 
}



