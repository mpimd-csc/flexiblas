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

#ifdef __cplusplus
extern "C" {
#endif
	#include <pthread.h>
	/**
	 @file libcscutils/include/cscutils/worker_queue.h
	  @defgroup worker_queue Worker-Queue: Implementation of a queue using PThread locking. 
	  @ingroup threading 
	  
	  This part of the library contains a consise implementation of a queue with PThread locking in order to use
	  with worker queues in threadpool, pipelines, producer-consumer models. 

	  @addtogroup worker_queue
	  @{ 
	*/


	/** 
	 * @brief Structure containg a Worker-Queue. 
	 *
	 * The csc_worker_queue_t structure represents a queue implemented on a ring buffer. The locking for 
	 * parallel access is done either using PThreads or OpenMP. The used lock mechanism is selected 
	 * during initialization. 
	 * */
	typedef struct _csc_worker_queue_t {
		void **buffer;              /**< Buffer containg the queue entries. */
		int capacity;               /**< Capacity of the ring buffer.  */
		int size;                   /**< Numer of elements in the ring buffer.   */
		int in;                     /**< Current postion of the tail of the queue.  */
		int out;                    /**< Current postion of the head of the queue.  */
		pthread_mutex_t mutex;      /**< Pthread mutex to lock the queue.  */
		pthread_cond_t cond_full;   /**< Pthread condition to signalize a full queue. */
		pthread_cond_t cond_empty;  /**< Pthread condition to signalize an empty queue. */
	} csc_worker_queue_t;


	/** 
	 * @brief Initialize a queue. 
	 * @param[out]   queue 	Pointer to the queue structure which should be initialized. 
	 * @param[in]    size     Capacity of the queue. 
	 * @return zero on success or a non zero in case of an error.
	 *
	 * The csc_worker_queue_init function creates a new queue with a capacity of size elements. 
	 */
	int csc_worker_queue_init    (csc_worker_queue_t *queue, int size ); 

	
	/** 
	 * @brief Destorys a queue structure.  
	 * @param[in] 	queue Pointer to the queue structure.  
	 *
	 * The csc_worker_queue_destroy function destroys a queue and deinitialize the lock. 
	 * @see csc_worker_queue_init
	 */
	void csc_worker_queue_destroy( csc_worker_queue_t *queue );  

	/** 
	 * @brief Enqueues a new element in the queue.  
	 * @param[in,out] queue   Pointer to the queue structure. 
	 * @param[in]     value   Pointer to the element to add to queue. 
	 *
	 * The csc_worker_queue_enqueue function adds a new element to the queue. If the queue is full, i.e. size == capacity in the
	 * underlying \ref csc_worker_queue_t structure, the threads waits until a postion is free in the queue. This behavior is necessary 
	 * to implement producer-consumer models. 
	 *
	 * \attention The value argument must not refer to a static memory buffer.    
	 */
	void csc_worker_queue_enqueue (csc_worker_queue_t *queue, void *value); 

	
	/** 
	 * @brief Dequeue an element from a queue.  
	 * @param[in,out] queue   Pointer to the queue structure. 
	 * @return The next value on the queue as a pointer. 
	 *
	 * The csc_worker_queue_dequeue function gets the next element from the queue. If the queue is empty it waits until 
	 * the next element is enqueued. 
	 */
	void *csc_worker_queue_dequeue(csc_worker_queue_t *queue); 


	/** 
	 * @brief Get the current number of elements in the queue.  
	 * @param[in,out] queue  Pointer to the queue structure. 
	 * @return the number of elements in the queue. 
	 *
	 * The csc_worker_queue_size function returns the number of elements in the queue. 
	 */
	int csc_worker_queue_size     (csc_worker_queue_t *queue); 


    void *csc_worker_queue_top(csc_worker_queue_t *queue);

    int csc_worker_queue_wait (csc_worker_queue_t *queue);

	/** @}  */

#ifdef __cplusplus
};
#endif
#endif

