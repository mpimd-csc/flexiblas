/* 
 * Demo for CSCUTILS background worker 
 * Copyright (C) Martin Koehler, 2017
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

#include<stdio.h> 
#include<stdlib.h> 
#include <unistd.h>

#include "cscutils/thread_pool.h"


void test_task1(void *arg)
{
    size_t l = (size_t) arg;
    size_t i;

    for (i = 0; i < l; i++) {
        printf("TASK 1: %d\n", (int) i );
        sleep(1);
    }
}

void test_task2(void *arg)
{
    size_t l = (size_t) arg;
    size_t i;

    for (i = 0; i < l; i++) {
        printf("TASK 2: %d\n", (int) i );
        sleep(1);
    }
}

void test_task3(void *arg)
{
    size_t l = (size_t) arg;
    size_t i;

    for (i = 0; i < l; i++) {
        printf("TASK 3: %d\n", (int) i );
        sleep(1);
    }
}


int main(int argc, char **argv)
{
    csc_background_worker_t *worker;


    worker = csc_background_worker_create(CSC_THREADPOOL_QUEUE_LENGTH);
    csc_background_worker_insert(worker, test_task1, (void *) 10);
    csc_background_worker_insert(worker, test_task2, (void *) 5);
    printf("Wait..\n");
    printf("Enqued Jobs: %ld\n", (long) csc_background_worker_queued(worker));
    csc_background_worker_wait(worker);
    printf("Wait done.\n");
    printf("Enqued Jobs: %ld\n", (long) csc_background_worker_queued(worker));
    csc_background_worker_insert(worker, test_task3, (void*) 3);
    /* csc_background_worker_wait(worker); */
    csc_background_worker_destroy(worker);


    return 0;
}
