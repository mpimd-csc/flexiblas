      DOUBLE PRECISION FUNCTION FDCABS1(Z)
!     .. Scalar Arguments ..
      DOUBLE COMPLEX Z
!     ..
!     ..
!  Purpose
!  =======
!
!  FDCABS1 computes absolute value of a double complex number 
!
!  =====================================================================
!
!     .. Intrinsic Functions ..
      INTRINSIC ABS,DBLE,DIMAG
!
      FDCABS1 = ABS(DBLE(Z)) + ABS(DIMAG(Z))
      RETURN
      END
