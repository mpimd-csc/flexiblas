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
extern "C"
{
#endif
#include "cscutils/worker_queue.h"
#include <pthread.h>
#include <stddef.h>

/**
 @file libcscutils/include/cscutils/thread_pool.h
   @defgroup thread_pool Thread-Pool: A concise thread pool implementation.
   @ingroup  threading

   This part of the library contains a concise implementation of a background worker
   with a thread pool pattern. It provides a simple interface to execute background
   tasks in a separate thread with a queue-based job management system.

   The background worker uses a producer-consumer model where tasks are enqueued
   by the main thread and consumed by a dedicated worker thread. This pattern is
   useful for asynchronous task execution without blocking the main application flow.

   @addtogroup thread_pool
   @{
*/

/**
 * @brief Maximum number of tasks that can be queued.
 *
 * This constant defines the maximum length of the internal task queue.
 * Tasks submitted beyond this limit will block until space becomes available.
 */
#define CSC_THREADPOOL_QUEUE_LENGTH 5120

    /**
     * @brief Structure representing a background worker.
     *
     * The csc_background_worker_t structure encapsulates a worker thread and its
     * associated task queue. It manages the execution of background jobs in a
     * separate thread of execution.
     *
     * @var csc_background_worker_t::tid
     *   The pthread identifier for the worker thread.
     * @var csc_background_worker_t::work
     *   Pointer to the worker queue containing pending tasks.
     * @var csc_background_worker_t::stop
     *   Flag indicating whether the worker should stop processing tasks.
     */
    typedef struct _csc_background_worker_t
    {
        pthread_t tid;
        csc_worker_queue_t* work;
        int stop;
    } csc_background_worker_t;

    /**
     * @brief Structure representing a background job.
     *
     * The csc_background_job_t structure encapsulates a function pointer and its
     * argument, forming a callable task that can be executed by the background worker.
     *
     * @var csc_background_job_t::func
     *   Function pointer to the task function to execute. The function should have
     *   the signature void func(void* arg).
     * @var csc_background_job_t::arg
     *   Pointer to the argument that will be passed to the function when executed.
     */
    typedef struct _csc_background_job_t
    {
        void (*func)(void* arg);
        void* arg;
    } csc_background_job_t;

    /**
     * @brief Create a new background worker.
     * @param[in] queue_length Maximum number of tasks that can be queued.
     * @return Pointer to the created worker, or NULL on failure.
     *
     * The csc_background_worker_create function creates a new background worker
     * with an internal task queue of the specified length. A worker thread is
     * started immediately and will process tasks as they are added to the queue.
     *
     * @note The caller is responsible for destroying the worker using
     *       csc_background_worker_destroy when it is no longer needed.
     *
     * @see csc_background_worker_destroy
     */
    csc_background_worker_t* csc_background_worker_create(size_t queue_length);

    /**
     * @brief Wait for all queued tasks to complete.
     * @param[in] worker Pointer to the background worker.
     * @return Zero on success, non-zero on error.
     *
     * The csc_background_worker_wait function blocks until all tasks currently
     * in the queue have been executed. It does not stop the worker thread,
     * allowing additional tasks to be queued after waiting completes.
     *
     * @return CSC_SUCCESS on success
     * @return CSC_ERROR if worker is NULL or invalid
     *
     * @see csc_background_worker_insert
     * @see csc_background_worker_destroy
     */
    int csc_background_worker_wait(csc_background_worker_t* worker);

    /**
     * @brief Destroy a background worker and free resources.
     * @param[in] worker Pointer to the background worker.
     * @return Zero on success, non-zero on error.
     *
     * The csc_background_worker_destroy function stops the worker thread,
     * waits for any remaining tasks to complete, and frees all associated
     * resources including the task queue and worker thread.
     *
     * @return CSC_SUCCESS on success
     * @return CSC_ERROR if worker is NULL or invalid
     *
     * @note This function will block until all queued tasks have completed.
     *
     * @see csc_background_worker_create
     */
    int csc_background_worker_destroy(csc_background_worker_t* worker);

    /**
     * @brief Get the number of tasks currently queued.
     * @param[in] worker Pointer to the background worker.
     * @return Number of tasks in the queue, or -1 on error.
     *
     * The csc_background_worker_queued function returns the current number of
     * tasks waiting in the queue to be executed by the worker thread.
     *
     * @return Number of queued tasks
     * @return -1 if worker is NULL or invalid
     *
     * @see csc_background_worker_insert
     */
    int csc_background_worker_queued(csc_background_worker_t* worker);

    /**
     * @brief Add a task to the worker's queue.
     * @param[in] worker Pointer to the background worker.
     * @param[in] func Pointer to the function to execute.
     * @param[in] arg Argument to pass to the function.
     * @return Zero on success, non-zero on failure.
     *
     * The csc_background_worker_insert function adds a new task to the worker's
     * queue. The task will be executed asynchronously by the worker thread.
     * If the queue is full, this function will block until space becomes available.
     *
     * @return CSC_SUCCESS on success
     * @return CSC_ERROR if worker is NULL or invalid
     * @return -1 if memory allocation for the job fails
     *
     * @note The function pointer should point to a function that takes a void* argument.
     * @note The arg parameter should point to dynamically allocated memory if the
     *       data needs to persist after this function returns.
     *
     * @see csc_background_worker_wait
     * @see csc_background_worker_queued
     */
    int csc_background_worker_insert(csc_background_worker_t* worker, void (*func)(void* arg), void* a);

/** @}  */
#ifdef __cplusplus
};
#endif
#endif /* end of include guard: CSC_THREAD_POOL_H */
