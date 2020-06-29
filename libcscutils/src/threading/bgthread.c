/*
 * Background Thread Implementation.
 * Copyright (C) Author, 2017
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

#ifdef CSC_HAVE_PTHREAD_YIELD
#define YIELD pthread_yield()
#else
#define YIELD
#endif

#include "cscutils/error_message.h"
#include "cscutils/thread_pool.h"
#include "cscutils/worker_queue.h"

static void * worker_routine (void *_arg)
{
    csc_background_worker_t *worker = (csc_background_worker_t *) _arg;
    csc_worker_queue_t *wq = worker->work;

    while(1) {
        csc_background_job_t *job = csc_worker_queue_top(wq);
        if ( job == NULL &&  worker->stop ) {
            csc_worker_queue_dequeue(wq);
            break;
        }

        if ( job != NULL ){
            job->func(job->arg);
            free(job);
        }
        csc_worker_queue_dequeue(wq);
        YIELD;
    }

    return NULL;
}

csc_background_worker_t * csc_background_worker_create(size_t queue_length)
{
    csc_background_worker_t * worker;

    worker = (csc_background_worker_t  *) malloc(sizeof(csc_background_worker_t ) * (1));
    if ( ! worker ) {
        csc_error_message("Failed to allocate memory for background worker.\n");
        return NULL;
    }

    worker->work = (csc_worker_queue_t *) malloc(sizeof(csc_worker_queue_t) * (1));
    if (!worker->work) {
        free(worker);
        csc_error_message("Could not allocate worker->work\n");
        return NULL;
    }
    if ( csc_worker_queue_init(worker->work, queue_length)) {
        free(worker->work);
        free(worker);
        csc_error_message("Failed to create worker queue.\n");
        return NULL;
    }
    worker -> stop = 0;
    pthread_create(&(worker->tid), NULL, worker_routine, worker) ;
    return worker;
}

int csc_background_worker_wait(csc_background_worker_t * worker)
{
    if (!worker) {
        return CSC_ERROR;
    }
    if (!worker->work) {
        return CSC_ERROR;
    }
    return csc_worker_queue_wait(worker->work);
}

int csc_background_worker_destroy(csc_background_worker_t * worker )
{
    if (!worker) return CSC_ERROR;
    worker->stop = 1;
    csc_worker_queue_enqueue(worker->work, NULL);
    csc_background_worker_wait(worker);
    pthread_cancel(worker->tid);
    pthread_join(worker->tid,  NULL);
    csc_worker_queue_destroy(worker->work);
    free(worker->work);
    free(worker);
    return CSC_SUCCESS;
}

int csc_background_worker_queued(csc_background_worker_t * worker)
{
    if (!worker) return -1;
    return csc_worker_queue_size(worker->work);
}

int csc_background_worker_insert(csc_background_worker_t *worker, void (*func) (void *arg), void *a)
{
    csc_background_job_t *job;

    if (!worker) {
        return CSC_ERROR;
    }
    job = (csc_background_job_t *) malloc(sizeof(csc_background_job_t) * (1));
    if ( !job ) {
        csc_error_message("Failed to allocate job.\n");
        return -1;
    }
    job->func = func;
    job->arg = a;
    csc_worker_queue_enqueue(worker->work, job);
    return CSC_SUCCESS;
}




