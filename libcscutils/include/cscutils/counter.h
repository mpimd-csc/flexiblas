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
#include "cscutils/cscutils_config.h"

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

    /**
     * @brief Tic-Toc counter for the Wall-Time
     * @return The time since EPOCH as double precision value in microsecond resolution.
     *
     * The csc_wtime function can be used to realize tic-toc time measurement for the runtime/wall-time.
     * It returns the number of seconds since EPOCH (1970-01-01 00:00:00 +0000 (UTC)) with a mircoseconds resolution.
     *
     * The runtime of a piece of code can be measured like:
     * \code
     *  double tic = csc_wtime();
     *  someaction();
     *  double toc = csc_wtime() - tic;
     * \endcode
     *
     * @see csc_ctime
     */
    double csc_wtime(void);

    /**
     * @brief Fortran Wrapper around csc_wtime
     * @return The time since EPOCH as double precision value in microsecond resolution.
     *
     * The csc_wtime_ function is a Fortran wrapper for csc_wtime.
     *
     * @see csc_wtime
     */
    double csc_wtime_(void);


    /**
     * @brief Tic-Toc counter for the CPU-time.
     * @return A timestamp for the CPU-time.
     *
     * The csc_ctime function is used to implement tic-toc measurements of the CPU-time, i.e. the accumulated time the CPU cores are working.
     *
     * The runtime of a piece of code can be measured like:
     * \code
     *  double tic = csc_ctime();
     *  someaction();
     *  double toc = csc_ctime() - tic;
     * \endcode
     *
     * @see csc_wtime
     */
    double csc_ctime(void);

    /**
     * @brief Fortran Wrapper around csc_ctime
     * @return A timestamp for the CPU-time.
     *
     * The csc_ctime_ function is a Fortran wrapper for csc_ctime.
     *
     * @see csc_ctime
     */
    double csc_ctime_(void);

    /**
     * @brief Obtain the number of CPU cylces
     * @return The number of already performed CPU cycles.
     *
     * The csc_cycles function returns the number of already performed CPU cycles. The function
     * only works on x86-64 and ppc64 architectures.
     *
     * The number of CPU cycles taken by a piece of code can be measured like:
     * \code
     *  int64_t tic = csc_cycles();
     *  someaction();
     *  int64_t toc = csc_cycles() - tic;
     * \endcode
     *
     * @see csc_ctime
     * @see csc_wtime
     */
    int64_t csc_cycles(void);

    /**
     * @brief Fortran Wrapper around csc_cycles
     * @return The number of already performed CPU cycles.
     *
     * The csc_cycles_ function is a Fortran wrapper for csc_cycles.
     *
     * @see csc_cycles
     */
    int64_t csc_cycles_(void);


    /** @} */
#ifdef  __cplusplus
}
#endif
#endif /* end of include guard: ERROR_MESSAGE_H */
