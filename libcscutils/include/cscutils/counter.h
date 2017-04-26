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

#ifndef CSC_COUNTER_H
#define CSC_COUNTER_H

#ifdef  __cplusplus
extern "C" {
#endif 
    #include <stdint.h>
	/**
	 @file libcscutils/include/cscutils/counter.h
	  @defgroup counter  Counter: Timers and Hardware counters
	
	  This part of the library contains routines to access timers and hardware counters
      like the wall time, the cpu time or the used instruction. The functions are also exported 
      as Fortran compatible symbols. 

	  @addtogroup counter 
	  @{ 
	*/


    double csc_wtime(void); 
    double csc_wtime_(void); 
    double csc_ctime(void); 
    double csc_ctime_(void); 
    int64_t csc_cycles(void); 
    int64_t csc_cycles_(void); 

       
    /** @} */
#ifdef  __cplusplus
}
#endif 
#endif /* end of include guard: ERROR_MESSAGE_H */
