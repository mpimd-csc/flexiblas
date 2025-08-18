SUBROUTINE CSC_QUADEQR(A, B, C, X1, X2, IER)
    ! Solution of a quadratic equation
    !   A*X**2 + B*X + C = 0

    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: A, B, C
    DOUBLE PRECISION, INTENT(OUT) :: X1, X2
    INTEGER, INTENT(OUT) :: IER

    DOUBLE PRECISION ARG, SQ
    IER = 0
    IF ( A .NE. 0. ) THEN
        ARG = B**2 - 4*A*C
        IF ( ARG .GT. 0. ) THEN
            SQ = SQRT(ARG)
            X1 = .5 * ( -B + SQ ) / A
            X2 = .5 * ( -B - SQ ) / A
        ELSE IF ( ARG .EQ. 0. ) THEN
            X1 = -.5 * B / A
            X2 = X1
        ELSE
            IER = -1
        END IF
    ELSE IF ( B .NE. 0. ) THEN
        X1 = -C / B
        IER = 1
    ELSE
        IER = -2
    END IF
    RETURN
END SUBROUTINE CSC_QUADEQR

SUBROUTINE CSC_QUADEQC(A, B, C, X1, X2, IER)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: A, B, C
    COMPLEX(KIND=KIND(1.0D0)), INTENT(OUT) :: X1, X2
    INTEGER, INTENT(OUT) :: IER

    DOUBLE PRECISION ARG
    COMPLEX(KIND=KIND(1.0D0)) SQ
    IER = 0
    IF ( A .NE. 0. ) THEN
        ARG = B**2 - 4*A*C
        IF ( ARG .GT. 0. ) THEN
            SQ = DSQRT(ARG)
            X1 = .5 * ( -B + SQ ) / A
            X2 = .5 * ( -B - SQ ) / A
        ELSE IF ( ARG .EQ. 0. ) THEN
            X1 = -.5 * B / A
            X2 = X1
        ELSE
            SQ = SQRT(DCMPLX(ARG,0.))
            X1 = .5 * ( -B + SQ ) / A
            X2 = .5 * ( -B - SQ ) / A
            IER = -1
        END IF
    ELSE IF ( B .NE. 0. ) THEN
        X1 = -C / B
        IER = 1
    ELSE
        IER = -2
    END IF
    RETURN
END SUBROUTINE CSC_QUADEQC

