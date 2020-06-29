/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) Martin Koehler, 2016
 */
#if defined(FLEXIBLAS_CBLAS) && !defined(STANDALONE)
 #define USE_CBLAS
#endif




#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/time.h>
#include <string.h>
#include <getopt.h>
#include <strings.h>
#include <inttypes.h>

#ifdef USE_CBLAS
    #include "cblas.h"
#endif

#define RUNS 20000

#ifndef BACKEND
#define BACKEND "NONAME"
#endif


#ifdef INTEGER8
	#define Int long
#else
	#define Int int
#endif

#ifndef STANDALONE
#include "../src/flexiblas_api.h"
#endif

#include "fortran_mangle.h"
#include "cscutils/counter.h"

typedef void (*benchmark_func_t) (Int n, Int runs, double *rtime, double *gflops);

void FC_GLOBAL(daxpy,DAXPY)(Int *N, double *alpha, double *x, Int *incx, double *Y, Int *incy);
void FC_GLOBAL(dgemm,DGEMM)(const char * TRANSA, const char *TRANSB, Int *m, Int *n, Int *k, double *alpha, double *A, Int *lda, double *B, Int *ldb, double *beta, double *C, Int *ldc);
void FC_GLOBAL(dgemv,DGEMV)(const char * TRANSA, Int *m, Int *n, double *alpha, double *A, Int *lda, double *B, Int *incb, double *beta, double *C, Int *incc);


double wtime()
{
	struct timeval tv;
	gettimeofday (&tv, NULL);
	return tv.tv_sec + tv.tv_usec / 1e6;
}

/*-----------------------------------------------------------------------------
 *  DGEMV Benchmark
 *-----------------------------------------------------------------------------*/
void benchmark_dgemv(Int n, Int Runs, double *rtime, double *gflops)
{
    Int i;
	double *A, *B, *C;
	double ts,te;
	double alpha=1, beta=1;
	double flops;
	Int incb = 1, incc = 1;

    A = malloc(sizeof(double) * n *n );
	B = malloc(sizeof(double) * n );
	C = malloc(sizeof(double) * n );

	for ( i = 0; i < n * n; i++){
		A[i]=i+1;
	}
	for (i = 0; i < n; i++) {
		B[i]=i*2+1;
		C[i]=1;
	}

    /*-----------------------------------------------------------------------------
     *  Warmup
     *-----------------------------------------------------------------------------*/
    FC_GLOBAL(dgemv,DGEMV)("N", &n,&n,&alpha, A, &n, B,&incb, &beta, C, &incc);
    FC_GLOBAL(dgemv,DGEMV)("N", &n,&n,&alpha, A, &n, B,&incb, &beta, C, &incc);
    FC_GLOBAL(dgemv,DGEMV)("N", &n,&n,&alpha, A, &n, B,&incb, &beta, C, &incc);

    /*-----------------------------------------------------------------------------
     *  Benchmark
     *-----------------------------------------------------------------------------*/
    ts = wtime();
	for (i=0; i < Runs; i++){
		FC_GLOBAL(dgemv,DGEMV)("N", &n,&n,&alpha, A, &n, B,&incb, &beta, C, &incc);
	}
	te = wtime();
	flops = 2.0 * n *n;
	flops /=1000*1000*1000;
	flops /= (te-ts)/Runs;

    *gflops = flops;
    *rtime = (te-ts)/Runs;

    free(A);
	free(B);
	free(C);

}


/*-----------------------------------------------------------------------------
 *  DGEMV Latency Test
 *-----------------------------------------------------------------------------*/
void benchmark_dgemv_latency(Int n, Int Runs, double *rtime, double *gflops)
{
    Int i;
	double *A, *B, *C;
	double ts,te;
	double alpha=1, beta=1;
	double flops;
	Int incb = 1, incc = 1;
    Int ld =1;
    uint64_t cy_start, cy_end, cy_sum;
    n = 1;

    A = malloc(sizeof(double) * n *n );
	B = malloc(sizeof(double) * n );
	C = malloc(sizeof(double) * n );

	for ( i = 0; i < n * n; i++){
		A[i]=i+1;
	}
	for (i = 0; i < n; i++) {
		B[i]=i*2+1;
		C[i]=1;
	}
    n = 0;
    /*-----------------------------------------------------------------------------
     *  Warmup
     *-----------------------------------------------------------------------------*/
    FC_GLOBAL(dgemv,DGEMV)("N", &n,&n,&alpha, A, &ld, B,&incb, &beta, C, &incc);
    FC_GLOBAL(dgemv,DGEMV)("N", &n,&n,&alpha, A, &ld, B,&incb, &beta, C, &incc);
    FC_GLOBAL(dgemv,DGEMV)("N", &n,&n,&alpha, A, &ld, B,&incb, &beta, C, &incc);

    /*-----------------------------------------------------------------------------
     *  Benchmark
     *-----------------------------------------------------------------------------*/
    cy_start = 0;
    cy_end = 0;
    cy_sum = 0;
    ts = wtime();
	for (i=0; i < Runs; i++){
        cy_start = csc_cycles();
		FC_GLOBAL(dgemv,DGEMV)("N", &n,&n,&alpha, A, &ld, B,&incb, &beta, C, &incc);
        cy_end = csc_cycles();
        cy_sum += (cy_end-cy_start);
	}
	te = wtime();
    cy_sum = cy_sum/Runs;
	flops = 2.0 * n *n;
	flops /=1000*1000*1000;
	flops /= (te-ts)/Runs;

    *gflops = flops;
    *rtime = (double) cy_sum;

    free(A);
	free(B);
	free(C);

}


/*-----------------------------------------------------------------------------
 *  DGEMM Benchmark
 *-----------------------------------------------------------------------------*/
void benchmark_dgemm(Int n, Int Runs, double *rtime, double *gflops)
{
    Int i;
	double *A, *B, *C;
	double ts,te;
	double alpha=1, beta=1;
	double flops;

    A = malloc(sizeof(double) * n *n );
	B = malloc(sizeof(double) * n *n );
	C = malloc(sizeof(double) * n *n );

	for ( i = 0; i < n * n; i++){
		A[i]=i+1;
		B[i]=i+0.5;
	}

	/*-----------------------------------------------------------------------------
	 *  Warmup
	 *-----------------------------------------------------------------------------*/
	FC_GLOBAL(dgemm,DGEMM)("N","N", &n,&n,&n,&alpha, A, &n, B,&n, &beta, C, &n);
	FC_GLOBAL(dgemm,DGEMM)("N","N", &n,&n,&n,&alpha, A, &n, B,&n, &beta, C, &n);
	FC_GLOBAL(dgemm,DGEMM)("N","N", &n,&n,&n,&alpha, A, &n, B,&n, &beta, C, &n);

	ts = wtime();
	for (i=0; i < Runs; i++){
		FC_GLOBAL(dgemm,DGEMM)("N","N", &n,&n,&n,&alpha, A, &n, B,&n, &beta, C, &n);
	}
	te = wtime();
	double h = (double) n / 1000.0;
	flops = 2.0 * h *h *h;
	flops /= ((te-ts)/Runs);
    *gflops = flops;
    *rtime = ((te-ts)/Runs);

    free(A);
	free(B);
	free(C);


}

/*-----------------------------------------------------------------------------
 *  DGEMM Benchmark (Latency)
 *-----------------------------------------------------------------------------*/
void benchmark_dgemm_latency(Int n, Int Runs, double *rtime, double *gflops)
{
    Int i;
	double *A, *B, *C;
	double ts,te;
	double alpha=1, beta=1;
	double flops;
    Int ld = 1;
    uint64_t cy_start, cy_end, cy_sum;

    n = 1;
    A = malloc(sizeof(double) * n *n );
	B = malloc(sizeof(double) * n *n );
	C = malloc(sizeof(double) * n *n );

	for ( i = 0; i < n * n; i++){
		A[i]=i+1;
		B[i]=i+0.5;
	}

    n = 0;
	/*-----------------------------------------------------------------------------
	 *  Warmup
	 *-----------------------------------------------------------------------------*/
	FC_GLOBAL(dgemm,DGEMM)("N","N", &n,&n,&n,&alpha, A, &ld, B,&ld, &beta, C, &ld);
	FC_GLOBAL(dgemm,DGEMM)("N","N", &n,&n,&n,&alpha, A, &ld, B,&ld, &beta, C, &ld);
	FC_GLOBAL(dgemm,DGEMM)("N","N", &n,&n,&n,&alpha, A, &ld, B,&ld, &beta, C, &ld);

    cy_sum = 0 ;
	ts = wtime();
	for (i=0; i < Runs; i++){
        cy_start = csc_cycles();
        FC_GLOBAL(dgemm,DGEMM)("N","N", &n,&n,&n,&alpha, A, &ld, B,&ld, &beta, C, &ld);
        cy_end = csc_cycles();
        cy_sum += (cy_end-cy_start);
	}
	te = wtime();
    cy_sum /= Runs;
	double h = (double) n / 1000.0;
	flops = 2.0 * h *h *h;
	flops /= ((te-ts)/Runs);
    *gflops = flops;
    *rtime = cy_sum;

    free(A);
	free(B);
	free(C);


}



/*-----------------------------------------------------------------------------
 *  Daxpy Benchmark
 *-----------------------------------------------------------------------------*/
void benchmark_daxpy(Int n, Int Runs, double *rtime, double *gflops)
{
	double *A, *B;
	double ts,te;
	double alpha=1;
	double flops;
	Int incx = 1, incy = 1;
    Int i;

    A = malloc(sizeof(double) * n );
	B = malloc(sizeof(double) * n );

    for ( i = 0; i < n ; i++){
		A[i]=i+1;
		B[i]=i+0.5;
	}
    /* Warm up */
    FC_GLOBAL(daxpy,DAXPY)(&n,&alpha, A, &incx, B, &incy);
	FC_GLOBAL(daxpy,DAXPY)(&n,&alpha, A, &incx, B, &incy);
	FC_GLOBAL(daxpy,DAXPY)(&n,&alpha, A, &incx, B, &incy);

    /*  Benchmark */
	ts = wtime();
	for (i=0; i < Runs; i++){
		FC_GLOBAL(daxpy,DAXPY)(&n,&alpha, A, &incx, B, &incy);
	}
	te = wtime();
	flops = 2.0 * n;
	flops /=1000.0*1000.0*1000.0;
	flops /= (te-ts)/Runs;
    *rtime = (te-ts)/Runs;
    *gflops = flops;
    free(A);
    free(B);
}

/*-----------------------------------------------------------------------------
 *  Daxpy Benchmark
 *-----------------------------------------------------------------------------*/
void benchmark_daxpy_latency(Int n, Int Runs, double *rtime, double *gflops)
{
	double *A, *B;
	double ts,te;
	double alpha=1;
	double flops;
	Int incx = 1, incy = 1;
    Int i;
    uint64_t cy_start, cy_end, cy_sum;

    A = malloc(sizeof(double) * n );
	B = malloc(sizeof(double) * n );

    for ( i = 0; i < n ; i++){
		A[i]=i+1;
		B[i]=i+0.5;
	}
    n = 0;
    /* Warm up */
    FC_GLOBAL(daxpy,DAXPY)(&n,&alpha, A, &incx, B, &incy);
	FC_GLOBAL(daxpy,DAXPY)(&n,&alpha, A, &incx, B, &incy);
	FC_GLOBAL(daxpy,DAXPY)(&n,&alpha, A, &incx, B, &incy);

    /*  Benchmark */
    cy_sum = 0;
	ts = wtime();
	for (i=0; i < Runs; i++){
        cy_start = csc_cycles();
		FC_GLOBAL(daxpy,DAXPY)(&n,&alpha, A, &incx, B, &incy);
        cy_end = csc_cycles();
        cy_sum += (cy_end-cy_start);

	}
	te = wtime();
    cy_sum /= Runs;
	flops = 2.0 * n;
	flops /=1000.0*1000.0*1000.0;
	flops /= (te-ts)/Runs;
    *rtime = cy_sum;
    *gflops = flops;
    free(A);
    free(B);
}



int main (int argc, char **argv) {
	Int n=-1, runs=-1;
    double rtime = 0, flops =0;
    int choice, skip=0, only=0;
    char *skip_str = NULL;
    char *only_str = NULL;
    char bk_name[128];
    benchmark_func_t benchmark = NULL;
    int latency = 0;


    while (1)
    {
        static struct option long_options[] =
        {
            /* Use flags like so:
            {"verbose",    no_argument,    &verbose_flag, 'V'}*/
            /* Argument styles: no_argument, required_argument, optional_argument */
#ifndef STANDALONE
            {"version", no_argument,    0,    'v'},
            {"skip",    required_argument, 0, 's'},
            {"only",    required_argument, 0, 'o'},
#endif
            {"help",    no_argument,    0,    'h'},
            {"runs",    required_argument, 0, 'r'},
            {"dim",     required_argument, 0, 'd'},
            {"benchmark", required_argument, 0, 'b'},
            {0,0,0,0}
        };
        int option_index = 0;

        /* Argument parameters:
            no_argument: " "
            required_argument: ":"
            optional_argument: "::" */
#ifndef STANDALONE
        choice = getopt_long( argc, argv, "vhs:o:r:d:b:",
                    long_options, &option_index);
#else
       choice = getopt_long( argc, argv, "hr:d:b:",
                    long_options, &option_index);

#endif

        if (choice == -1)
            break;

        switch( choice )
        {
#ifndef STANDALONE
            case 'v':
                {
                    int major, minor, patch;
                    flexiblas_get_version(&major, &minor, &patch);
                    printf("FlexiBLAS Benchmarik\n");
                    printf("Version %d.%d.%d\n", major, minor, patch);
                    exit(0);
                }
                break;
            case 's':
                skip = 1;
                skip_str = strdup(optarg);
                break;
            case 'o':
                only = 1;
                only_str = strdup(optarg);
                break;
#endif
            case 'h':
                {
                    printf("FlexiBLAS Benchmark");
                    printf("\n");
                    printf("Usage: %s [--help|-h] [--dim|-d N] [--runs|-r R] [--benchmark|-b NAME]\n", argv[0]);
#ifndef STANDALONE
                    printf("           [--skip|-s BLAS1,BLAS2,...] [--only|-o BLAS1,BLAS2,...] [--version|-v]\n");
#endif
                    printf("\n");
                    printf("The options are:\n");
                    printf(" [--help|-h]        Print this help.\n");
                    printf(" [--dim|-d N]       Dimension of the example.\n");
                    printf(" [--runs|-r RUNS]   Number of runs to perform.\n");
                    printf(" [--benchmark|-b NAME] Name of the Benchmark\n");
#ifndef STANDALONE
                    printf(" [--version|-v]     Version of the FlexiBLAS library.\n");
                    printf(" [--skip|-s BLAS1,BLAS2, ...]   List of BLAS backends to skip.\n");
                    printf(" [--only|-o BLAS1,BLAS2, ...]   List of BLAS backends to run the benchmark on.\n");
#endif
                    printf("\n");
                    exit(0);

                }

                break;
            case 'd':
                n = atoi(optarg);
                break;
            case 'r':
                runs = atoi(optarg);
                break;
            case 'b':
                if (strcasecmp(optarg, "?" ) == 0) {
                    printf("Possible Benchmarks are:\n");
                    printf(" - DAXPY               Benchmark the DAXPY operation\n");
                    printf(" - DGEMM               Benchmark the DGEMM operation\n");
                    printf(" - DGEMV               Benchmark the DGEMV operation\n");
                    printf(" - DAXPY_LATENCY       Benchmark the latency of the DAXPY operation\n");
                    printf(" - DGEMM_LATENCY       Benchmark the latency of the DGEMM operation\n");
                    printf(" - DGEMV_LATENCY       Benchmark the latency of the DGEMV operation\n");

                    return EXIT_FAILURE;
                }
                else if (strcasecmp(optarg, "DAXPY") == 0 ){
                    strncpy(bk_name, "DAXPY", 128);
                    benchmark = & benchmark_daxpy;
                }
                else if (strcasecmp(optarg, "DGEMM") == 0 ){
                    strncpy(bk_name, "DGEMM", 128);
                    benchmark = & benchmark_dgemm;
                }
                else if (strcasecmp(optarg, "DGEMV") == 0 ){
                    strncpy(bk_name, "DGEMV", 128);
                    benchmark = & benchmark_dgemv;
                }
                else if (strcasecmp(optarg, "DGEMV_LATENCY") == 0 ){
                    strncpy(bk_name, "DGEMV_LATENCY", 128);
                    benchmark = & benchmark_dgemv_latency;
                    latency = 1;
                }
                else if (strcasecmp(optarg, "DGEMM_LATENCY") == 0 ){
                    strncpy(bk_name, "DGEMM_LATENCY", 128);
                    benchmark = & benchmark_dgemm_latency;
                    latency = 1;
                }
                else if (strcasecmp(optarg, "DAXPY_LATENCY") == 0 ){
                    strncpy(bk_name, "DAXPY_LATENCY", 128);
                    benchmark = & benchmark_daxpy_latency;
                    latency = 1;
                }

                break;
            default:
                /* Not sure how to get here... */
                return EXIT_FAILURE;
        }
    }

    if ( n < 0 ) {
        printf("The dimension has to be set to a positive integer.\n");
        exit(1);
    }
    if ( runs < 0 ) {
        printf("The number of runs has to be set to a postive integer.\n");
        exit(1);
    }
    if ( skip && only ){
        printf("Either --skip or --only can be defined. Not both of them.\n");
        exit(1);
    }

    if (benchmark == NULL) {
        printf("No benchmark selected.\n");
        exit(1);
    }

    printf("# Dimension: %d\n", (int) n);
    printf("# Runs: %d \n", (int) runs);
    printf("# Benchmark: %s\n", bk_name);
    if (skip) printf("# Skip: %s\n", skip_str);
    if (only) printf("# Only: %s\n", only_str);
    if ( latency ) {
        printf("#%29s \t %20s \n", "Name", "Latency (Cycles)");

    } else {
        printf("#%29s \t %10s \t %10s\n", "Name", "Runtime", "GFlops");
    }

#ifdef STANDALONE
    /*-----------------------------------------------------------------------------
     *  Standalone Benchmark
     *-----------------------------------------------------------------------------*/
    char *str = BACKEND;
    benchmark(n, runs, &rtime, &flops);
    if ( latency ) {
        uint64_t urtime = (uint64_t) rtime;
        printf("%30s \t %10" PRIu64 "\n", str, urtime);

    } else {
        printf("%30s \t %10.8e \t %10.8e\n", str, rtime, flops);
    }
#else
    /*-----------------------------------------------------------------------------
     * FlexiBLAS internal way
     *-----------------------------------------------------------------------------*/
    int n_blas,i, id;
    char name[1024];
    char *saveptr;
    if ( only ) {
        char * all = only_str;
        char * tok = NULL;

        while ((tok = strtok_r(all,",:;", &saveptr)) != NULL) {
            all = NULL;
            id = flexiblas_load_backend(tok);
            if ( id < 0 ) {
                fprintf(stderr, "Failed to load %s\n", tok);
                continue;
            }
            flexiblas_switch(id);

            benchmark(n, runs, &rtime, &flops);
            if ( latency ) {
                uint64_t urtime = (uint64_t) rtime;
                printf("%30s \t %10" PRIu64 "\n", tok, urtime);

            } else {
                printf("%30s \t %10.8e \t %10.8e\n", tok, rtime, flops);
            }
        }

    } else if ( skip )  {
        char *all, *tok;
        int found = 0;
        n_blas = flexiblas_list(NULL, 0, 0);
        for (i = 0; i < n_blas; i++) {
            flexiblas_list(name, 1023, i );
            if ( strcmp(name, "__FALLBACK__" ) == 0)
                continue;
            all = skip_str;
            tok = NULL;
            found = 0;
            while ((tok=strtok_r(all, ",:;", &saveptr))!=NULL) {
                all = NULL;
                if ( strcasecmp(name, tok)==0){
                    found = 1;
                    break;
                }
            }
            if (found) continue;

            id = flexiblas_load_backend(name);
            if ( id < 0 ) {
                fprintf(stderr, "Failed to load %s\n", name);
                continue;
            }
            flexiblas_switch(id);

            benchmark(n, runs, &rtime, &flops);
            if ( latency ) {
                uint64_t urtime = (uint64_t) rtime;
                printf("%30s \t %10" PRIu64 "\n", name, urtime);

            } else {
                printf("%30s \t %10.8e \t %10.8e\n", name, rtime, flops);
            }

        }
    } else {
        n_blas = flexiblas_list(NULL, 0, 0);
        for (i = 0; i < n_blas; i++) {
            flexiblas_list(name, 1023, i );
            if ( strcmp(name, "__FALLBACK__" ) == 0)
                continue;

            id = flexiblas_load_backend(name);
            if ( id < 0 ) {
                fprintf(stderr, "Failed to load %s :-( \n", name);
                continue;
            }
            flexiblas_switch(id);

            benchmark(n, runs, &rtime, &flops);
            if ( latency ) {
                uint64_t urtime = (uint64_t) rtime;
                printf("%30s \t %10" PRIu64 "\n", name, urtime);

            } else {
                printf("%30s \t %10.8e \t %10.8e\n", name, rtime, flops);
            }
        }

    }

#endif
    if (skip_str) free(skip_str);
    if (only_str) free(only_str);
	return 0;
}

