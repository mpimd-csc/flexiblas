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


#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

#ifdef HAVE_PTHREAD_YIELD
#define YIELD pthread_yield()
#else 
#define YIELD
#endif 

#include "cscutils/worker_queue.h"


int csc_worker_queue_init    (csc_worker_queue_t *queue,  int size )
{
	queue->buffer = (void **) malloc ( sizeof(void *) * size); 
	if ( queue->buffer == NULL ) {
		return -1; 
	}
	queue->capacity = size; 
	queue->size = 0; 
	queue->in   = 0; 
	queue->out  = 0; 
	pthread_mutex_init(&(queue->mutex), NULL); 
	pthread_cond_init(&(queue->cond_full), NULL); 
	pthread_cond_init(&(queue->cond_empty), NULL); 
	return 0; 
}

void csc_worker_queue_destroy( csc_worker_queue_t *queue ) 
{
	free(queue->buffer); 
	pthread_cond_destroy(&(queue->cond_empty)); 
	pthread_cond_destroy(&(queue->cond_full)); 
	pthread_mutex_destroy(&(queue->mutex)); 
}

void csc_worker_queue_enqueue (csc_worker_queue_t *queue, void *value) 
{
	pthread_mutex_lock(&(queue->mutex));
	while (queue->size == queue->capacity)
		pthread_cond_wait(&(queue->cond_full), &(queue->mutex));
	// printf("enqueue %d\n", *(int *)value);
	queue->buffer[queue->in] = value;
	++ queue->size;
	++ queue->in;
	queue->in %= queue->capacity;

	pthread_cond_broadcast(&(queue->cond_empty));
	pthread_mutex_unlock(&(queue->mutex));
}

void *csc_worker_queue_dequeue(csc_worker_queue_t *queue)
{
	pthread_mutex_lock(&(queue->mutex));
	while (queue->size == 0) {
        YIELD;
		pthread_cond_wait(&(queue->cond_empty), &(queue->mutex));
    }
	void *value = queue->buffer[queue->out];
	// printf("dequeue %d\n", *(int *)value);
	-- queue->size;
	++ queue->out;
	queue->out %= queue->capacity;
	pthread_cond_broadcast(&(queue->cond_full));
	pthread_mutex_unlock(&(queue->mutex));
	return value;

}

void *csc_worker_queue_top(csc_worker_queue_t *queue)
{
	pthread_mutex_lock(&(queue->mutex));
	while (queue->size == 0) {
        YIELD;
		pthread_cond_wait(&(queue->cond_empty), &(queue->mutex));
    }
	void *value = queue->buffer[queue->out];
	// printf("dequeue %d\n", *(int *)value);
	pthread_mutex_unlock(&(queue->mutex));
	return value;
}


int csc_worker_queue_size     (csc_worker_queue_t *queue)
{
	pthread_mutex_lock(&(queue->mutex));
	int size = queue->size;
	pthread_mutex_unlock(&(queue->mutex));
	return size;
}

int csc_worker_queue_wait (csc_worker_queue_t *queue) 
{
	pthread_mutex_lock(&(queue->mutex));
	while (queue->size > 0) {
#ifdef _GNU_SOURCE
        pthread_yield();
#endif
    	pthread_cond_wait(&(queue->cond_full), &(queue->mutex));
    }
	pthread_mutex_unlock(&(queue->mutex));
    return 0;
}



