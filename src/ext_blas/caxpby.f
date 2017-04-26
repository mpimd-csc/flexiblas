      SUBROUTINE FCAXPBY(N,CA,CX,INCX,CB,CY,INCY)
*     .. Scalar Arguments ..
      COMPLEX CA,CB
      INTEGER INCX,INCY,N
*     ..
*     .. Array Arguments ..
      COMPLEX CX(*),CY(*)
*     ..
*
*  Purpose
*  =======
*
*     CAXPBY constant times a vector plus a constant times a vector.
*
*  Further Details
*  ===============
*
*     jack dongarra, linpack, 3/11/78.
*     Martin Koehler, FlexiBLAS 9/5/2014
*     modified 12/3/93, array(1) declarations changed to array(*)
*     modified 09/05/2014, axpy -> axpby 
*
*  =====================================================================
*
*     .. Local Scalars ..
      INTEGER I,IX,IY
*     ..
*     .. External Functions ..
      REAL FSCABS1
      EXTERNAL FSCABS1
*     ..
      IF (N.LE.0) RETURN
      IF (FSCABS1(CA).EQ.0.0E+0 .AND. FSCABS1(CB).EQ.0.0E+0) RETURN
      IF (INCX.EQ.1 .AND. INCY.EQ.1) THEN
*
*        code for both increments equal to 1
*
         DO I = 1,N
            CY(I) = CB*CY(I) + CA*CX(I)
         END DO
      ELSE
*
*        code for unequal increments or equal increments
*          not equal to 1
*
         IX = 1
         IY = 1
         IF (INCX.LT.0) IX = (-N+1)*INCX + 1
         IF (INCY.LT.0) IY = (-N+1)*INCY + 1
         DO I = 1,N
            CY(IY) = CB*CY(IY) + CA*CX(IX)
            IX = IX + INCX
            IY = IY + INCY
         END DO
      END IF
*
      RETURN
      END
