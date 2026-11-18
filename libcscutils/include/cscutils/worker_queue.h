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

#ifndef CSC_QUEUE_H
#define CSC_QUEUE_H

#include "cscutils/cscutils_config.h"
#include <stddef.h>

#ifdef __cplusplus
extern "C"
{
#endif
#include <pthread.h>
    /**
      @file libcscutils/include/cscutils/worker_queue.h
        @defgroup worker_queue Worker-Queue: Implementation of a queue using PThread locking.
        @ingroup threading

        This part of the library contains a concise implementation of a queue with PThread locking in order to use
        with worker queues in threadpools, pipelines, and producer-consumer models.

        The csc_worker_queue_t structure implements a bounded blocking queue using a ring buffer with pthread mutex
        and condition variables for thread-safe access. It provides blocking enqueue and dequeue operations that
        wait when the queue is full or empty, respectively.

        @addtogroup worker_queue
        @{
      */

    /**
     * @brief Structure containing a Worker-Queue.
     *
     * The csc_worker_queue_t structure represents a queue implemented on a ring buffer with PThread-based
     * synchronization. It provides thread-safe operations for enqueuing and dequeuing elements using
     * pthread mutex and condition variables.
     *
     * @var csc_worker_queue_t::buffer
     *   Pointer to the ring buffer containing queue entries as void pointers.
     * @var csc_worker_queue_t::capacity
     *   Maximum number of elements that can be stored in the queue.
     * @var csc_worker_queue_t::size
     *   Current number of elements in the queue.
     * @var csc_worker_queue_t::in
     *   Index of the tail position where next element will be enqueued.
     * @var csc_worker_queue_t::out
     *   Index of the head position from where next element will be dequeued.
     * @var csc_worker_queue_t::mutex
     *   Pthread mutex for protecting queue access.
     * @var csc_worker_queue_t::cond_full
     *   Pthread condition variable signaled when the queue becomes non-full.
     * @var csc_worker_queue_t::cond_empty
     *   Pthread condition variable signaled when the queue becomes non-empty.
     */
    typedef struct _csc_worker_queue_t
    {
        void** buffer;             /**< Buffer containing the queue entries. */
        int capacity;              /**< Capacity of the ring buffer. */
        int size;                  /**< Number of elements in the ring buffer. */
        int in;                    /**< Current position of the tail of the queue. */
        int out;                   /**< Current position of the head of the queue. */
        pthread_mutex_t mutex;     /**< Pthread mutex to lock the queue. */
        pthread_cond_t cond_full;  /**< Pthread condition to signalize a full queue. */
        pthread_cond_t cond_empty; /**< Pthread condition to signalize an empty queue. */
    } csc_worker_queue_t;

    /**
     * @brief Initialize a queue.
     * @param[out]   queue  Pointer to the queue structure which should be initialized.
     * @param[in]    size     Capacity of the queue.
     * @return zero on success or a non-zero value in case of an error.
     *
     * The csc_worker_queue_init function creates a new queue with a capacity of size elements.
     * It initializes the internal mutex and condition variables.
     *
     * @see csc_worker_queue_destroy
     */
    int csc_worker_queue_init(csc_worker_queue_t* queue, int size);

    /**
     * @brief Destroys a queue structure.
     * @param[in]   queue Pointer to the queue structure.
     *
     * The csc_worker_queue_destroy function destroys a queue and deinitializes the internal mutex
     * and condition variables. All resources associated with the queue are freed.
     *
     * @see csc_worker_queue_init
     */
    void csc_worker_queue_destroy(csc_worker_queue_t* queue);

    /**
     * @brief Enqueues a new element in the queue.
     * @param[in,out] queue   Pointer to the queue structure.
     * @param[in]     value   Pointer to the element to add to queue.
     *
     * The csc_worker_queue_enqueue function adds a new element to the queue. If the queue is full (size == capacity),
     * the calling thread blocks until space becomes available. This blocking behavior is essential for implementing
     * producer-consumer models where producers must wait when the queue is at capacity.
     *
     * @attention The value argument must not refer to a static memory buffer. The queue stores pointers directly,
     *            so the pointed-to data must remain valid until the element is dequeued.
     *
     * @see csc_worker_queue_dequeue
     */
    void csc_worker_queue_enqueue(csc_worker_queue_t* queue, void* value);

    /**
     * @brief Dequeue an element from a queue.
     * @param[in,out] queue   Pointer to the queue structure.
     * @return Pointer to the dequeued element, or NULL if the queue is invalid.
     *
     * The csc_worker_queue_dequeue function removes and returns the next element from the queue. If the queue is empty,
     * the calling thread blocks until a new element is enqueued. This blocking behavior is essential for implementing
     * consumer patterns where consumers wait for work.
     *
     * @see csc_worker_queue_enqueue
     */
    void* csc_worker_queue_dequeue(csc_worker_queue_t* queue);

    /**
     * @brief Get the current number of elements in the queue.
     * @param[in,out] queue  Pointer to the queue structure.
     * @return The number of elements currently in the queue.
     *
     * The csc_worker_queue_size function returns the current number of elements in the queue without modifying it.
     * This function can be used to monitor queue utilization.
     */
    int csc_worker_queue_size(csc_worker_queue_t* queue);

    /**
     * @brief Peek at the next element without removing it.
     * @param[in,out] queue  Pointer to the queue structure.
     * @return Pointer to the next element in the queue, or NULL if the queue is empty.
     *
     * The csc_worker_queue_top function returns a pointer to the next element in the queue without removing it.
     * This function does not block and returns immediately with the current head element.
     *
     * @see csc_worker_queue_dequeue
     */
    void* csc_worker_queue_top(csc_worker_queue_t* queue);

    /**
     * @brief Wait for the queue to become non-empty.
     * @param[in,out] queue  Pointer to the queue structure.
     * @return Zero on success or a non-zero error code.
     *
     * The csc_worker_queue_wait function blocks until the queue contains at least one element.
     * This can be used in scenarios where a consumer needs to wait for work without using
     * the automatic blocking behavior of csc_worker_queue_dequeue.
     *
     * @see csc_worker_queue_dequeue
     * @see csc_worker_queue_enqueue
     */
    int csc_worker_queue_wait(csc_worker_queue_t* queue);

    /** @}  */

#ifdef __cplusplus
};
#endif
#endif
