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

#ifndef CSC_THREAD_POOL_H

#define CSC_THREAD_POOL_H

#include "cscutils/cscutils_config.h"

#ifdef __cplusplus
extern "C" {
#endif
    #include <pthread.h>
    #include "cscutils/worker_queue.h"

    /**
     @file libcscutils/include/cscutils/thread_pool.h
      @defgroup thread_pool Thread-Pool: A consise thread pool implementation.
      @ingroup  threading


      @addtogroup thread_pool
      @{
    */
    #define CSC_THREADPOOL_QUEUE_LENGTH 5120


    typedef struct _csc_background_worker_t {
        pthread_t tid;
        csc_worker_queue_t *work;
        int stop;
    } csc_background_worker_t;

    typedef struct _csc_background_job_t {
        void (*func)(void * arg);
        void *arg;
    } csc_background_job_t;

    csc_background_worker_t * csc_background_worker_create(size_t queue_length);
    int csc_background_worker_wait(csc_background_worker_t * worker);
    int csc_background_worker_destroy(csc_background_worker_t * worker );
    int csc_background_worker_queued(csc_background_worker_t * worker);
    int csc_background_worker_insert(csc_background_worker_t *worker, void (*func) (void *arg), void *a);



    /** @}  */
#ifdef __cplusplus
};
#endif
#endif /* end of include guard: CSC_THREAD_POOL_H */
