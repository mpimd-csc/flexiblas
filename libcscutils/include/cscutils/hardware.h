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

#ifndef CSC_HARDWARE_H
#define CSC_HARDWARE_H

#ifdef  __cplusplus
extern "C" {
#endif 

	/**
	 @file libcscutils/include/cscutils/hardware.h
	  @defgroup hardware  Hardware: Information about hardware and the operating system 
	
	  This part of the library contains routines to retrieve information about the hardware
      and the used operating system like the size of the memory and similar stuff.

      @remark The module depends on the \ref error_message module. 

	  @addtogroup hardware 
	  @{ 
	*/


    /** 
     * @brief Retrieve information about the installed memory.  
     * @param[out]  total_ram   Returns the total (physical) size of the memory. 
     * @param[out]  free_ram    Returns the size of the free memory. 
     * @param[out]  total_swap  Returns the size of the swap space. 
     * @param[out]  free_swap   Returns the size of the free swap space. 
     * @return zero on success or a non zero error code otherwise 
     *
     * The csc_hardware_memory function retrieve information about the installed 
     * memory and the swap space. The sizes are returned in bytes. If a value 
     * is not wanted, set it to zero during the function call. 
     */
    int csc_hardware_memory(size_t *total_ram, size_t *free_ram, size_t *total_swap, size_t *free_swap); 
        
    /** @} */
#ifdef  __cplusplus
}
#endif 
#endif /* end of include guard: ERROR_MESSAGE_H */
