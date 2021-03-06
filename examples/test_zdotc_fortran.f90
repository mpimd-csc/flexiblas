
PROGRAM TEST
    IMPLICIT NONE 
    DOUBLE COMPLEX A(4), B(4)
    INTEGER ONE, N 
    DOUBLE COMPLEX R

    EXTERNAL ZDOTC
    INTRINSIC DCMPLX
    DOUBLE COMPLEX ZDOTC 

    A(1) = DCMPLX(1d0,1d0)
    A(2) = DCMPLX(2d0,1d0) 
    A(3) = DCMPLX(3d0,0)
    A(4) = DCMPLX(4d0,0) 
    B(1) = DCMPLX(1d0,0) 
    B(2) = DCMPLX(1d0,0) 
    B(3) = DCMPLX(1d0,0)
    B(4) = DCMPLX(1d0,0)

    N = 4
    ONE = 1 

    R = ZDOTC(N, A, ONE, B, ONE)

    WRITE(*,*) "A^HB = ",  R

END PROGRAM 
