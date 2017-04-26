#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <slu_zdefs.h>
#include <complex.h>
#include <math.h>

int main(void){

    SuperMatrix A,L,U,Dummy,B,X;
    superlu_options_t options; 
    SuperLUStat_t stat; 
    int ret,info,*perm_r,*perm_c,*etree,rows,cols,nnz,*rowptr,*colptr; 
    double *R,*C,ferr,berr; 
    char equed; 
    mem_usage_t mem_usage; 
    double complex *val,*xval,*bval; 
    GlobalLU_t Glu;

    //create matrix A
    rows = 2; cols = 2; nnz = 3; 
    val = (double complex*) malloc(sizeof(double complex)*3); val[0]=1; val[1]=1+1*I; val[2]=1;
    rowptr = (int *) malloc(sizeof(int)*3); rowptr[0]=0; rowptr[1]=0; rowptr[2]=1; 
    colptr = (int *) malloc(sizeof(int)*3); colptr[0]=0; colptr[1]=1; colptr[2]=3; 
    zCreate_CompCol_Matrix(&A,rows,cols,nnz,(doublecomplex *) val, rowptr, colptr, SLU_NC,SLU_Z,SLU_GE);

    //create options
    set_default_options(&(options));
    options.Fact = DOFACT;
    options.Equil = YES;
    options.ColPerm = COLAMD;
    options.DiagPivotThresh = 1.0;
    options.Trans = NOTRANS;
    options.IterRefine = SLU_EXTRA;
    options.SymmetricMode = NO;
    options.PivotGrowth = NO;
    options.ConditionNumber = NO;
    options.PrintStat = NO;

    //init stat
    StatInit(&(stat));
    
    //create dummy rhs
    zCreate_Dense_Matrix(&Dummy,rows,0,NULL,rows,SLU_DN,SLU_Z,SLU_GE);
    xval = (double complex*) malloc(sizeof(double complex)*2); xval[0]=0; xval[1]=0;
    zCreate_Dense_Matrix(&X,rows,1,(doublecomplex*) xval,rows,SLU_DN,SLU_Z,SLU_GE);
    bval = (double complex*) malloc(sizeof(double complex)*2); bval[0]=1; bval[1]=0;
    zCreate_Dense_Matrix(&B,rows,1,(doublecomplex*) bval,rows,SLU_DN,SLU_Z,SLU_GE);

    //factorize
    perm_r  = (int*) malloc(sizeof(int)*nnz);
    perm_c  = (int*) malloc(sizeof(int)*nnz);
    etree   = (int*) malloc(sizeof(int)*nnz);
    R       = (double*) malloc(sizeof(double)*rows);
    C       = (double*) malloc(sizeof(double)*cols);
    zgssvx(&options,&A,perm_c,perm_r,etree,&equed,R,C,&L,&U,NULL,0,&Dummy,&X,NULL,NULL,NULL,NULL,&Glu,&mem_usage,&stat,&info);
    options.Fact = FACTORED;
    options.Trans = CONJ; 

    //solve
    zgssvx(&options,&A,perm_c,perm_r,etree,&equed,R,C,&L,&U,NULL,0,&B,&X,NULL,NULL,&ferr,&berr,&Glu,&mem_usage,&stat,&info);

    //correct solution is [1,-1+1I]
    double error = sqrt(cabs(cpow(xval[0]-1,2) + cpow(xval[1]-(-1+1*I),2)));
    printf("error=%e",error);
    ret = error > 1e-6;

    //free memory
    free(perm_r); free(perm_c); free(etree); free(R); free(C);
    StatFree(&stat);
    Destroy_CompCol_Matrix(&A);
    Destroy_SuperNode_Matrix(&L);
    Destroy_CompCol_Matrix(&U);
    //Destroy_Dense_Matrix(&B);
    //Destroy_Dense_Matrix(&X);
    //Destroy_Dense_Matrix(&Dummy);
    Destroy_SuperMatrix_Store(&B);
    Destroy_SuperMatrix_Store(&X);
    Destroy_SuperMatrix_Store(&Dummy);

    return ret; 
    //return 0;
}
