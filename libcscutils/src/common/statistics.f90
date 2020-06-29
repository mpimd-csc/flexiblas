MODULE CSC_SORT_STAT
    USE ISO_C_BINDING
    IMPLICIT NONE

    TYPE, BIND(C) :: CSC_STATISTIC_RESULTD
        INTEGER(KIND=C_SIZE_T) :: N
        REAL(KIND=C_DOUBLE) :: AVG
        REAL(KIND=C_DOUBLE) :: MIN
        REAL(KIND=C_DOUBLE) :: MAX
        REAL(KIND=C_DOUBLE) :: MEDIAN
        REAL(KIND=C_DOUBLE) :: LOWERQUART
        REAL(KIND=C_DOUBLE) :: UPPERQUART
        REAL(KIND=C_DOUBLE) :: LOWERWHISKER
        REAL(KIND=C_DOUBLE) :: UPPERWHISKER
        REAL(KIND=C_DOUBLE) :: IQR
        REAL(KIND=C_DOUBLE) :: CLEANAVG
    END TYPE


    PRIVATE :: COPYD
    PRIVATE :: SORTD

CONTAINS
    SUBROUTINE COPYD(N,DX,INCX,DY,INCY)
        IMPLICIT NONE
        INTEGER INCX,INCY,N
        DOUBLE PRECISION DX(:),DY(:)
        INTEGER I,IX,IY,M,MP1

        IF (N.LE.0) RETURN
        IF (INCX.EQ.1 .AND. INCY.EQ.1) THEN
            M = MOD(N,7)
            IF (M.NE.0) THEN
                DO I = 1,M
                    DY(I) = DX(I)
                END DO
                IF (N.LT.7) RETURN
            END IF
            MP1 = M + 1
            DO I = MP1,N,7
                DY(I) = DX(I)
                DY(I+1) = DX(I+1)
                DY(I+2) = DX(I+2)
                DY(I+3) = DX(I+3)
                DY(I+4) = DX(I+4)
                DY(I+5) = DX(I+5)
                DY(I+6) = DX(I+6)
            END DO
        ELSE
            IX = 1
            IY = 1
            IF (INCX.LT.0) IX = (-N+1)*INCX + 1
            IF (INCY.LT.0) IY = (-N+1)*INCY + 1
            DO I = 1,N
                DY(IY) = DX(IX)
                IX = IX + INCX
                IY = IY + INCY
            END DO
        END IF
        RETURN
    END SUBROUTINE COPYD


    SUBROUTINE SORTD(N, A)
        IMPLICIT NONE
        INTEGER :: N
        DOUBLE PRECISION :: A(N)

        INTEGER :: I, J, INCREMENT
        DOUBLE PRECISION :: TEMP

        INCREMENT = SIZE(A) / 2
        DO WHILE (INCREMENT > 0)
            DO I = INCREMENT+1, SIZE(A)
                J = I
                TEMP = A(I)
                DO WHILE (J >= INCREMENT+1 .AND. A(J-INCREMENT) > TEMP)
                    A(J) = A(J-INCREMENT)
                    J = J - INCREMENT
                END DO
                A(J) = TEMP
            END DO
            IF (INCREMENT == 2) THEN
                INCREMENT = 1
            ELSE
                INCREMENT = INCREMENT * 5 / 11
            END IF
        END DO

    END SUBROUTINE SORTD

    SUBROUTINE CSC_STATISTIC_ANALYZE(N, X, INCX, LQ, UQ, IQRFACT, OUTPUT)
        IMPLICIT NONE

        ! Inputs
        INTEGER, INTENT(IN) :: N, INCX
        DOUBLE PRECISION, INTENT(IN), DIMENSION(:) :: X
        TYPE(CSC_STATISTIC_RESULTD), INTENT(OUT) :: OUTPUT
        DOUBLE PRECISION, INTENT(IN) :: LQ, UQ, IQRFACT

        ! Locals
        DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: A
        INTEGER :: STATUS, K, N2, ILW, IUW
        DOUBLE PRECISION IQR, TMP, D, LW, UW

        IF ( N .LE. 0 ) THEN
            RETURN
        END IF

        ALLOCATE(A(N), STAT=STATUS)
        IF ( STATUS .NE. 0 ) THEN
            WRITE(*,*) "Failed to allocate helper array"
        END IF

        ! Copy data
        CALL COPYD(N, X, INCX, A, 1)

        ! Sort
        CALL SORTD(N, A)

        ! Average
        OUTPUT%N = N
        TMP = 0.0D0
        D = 1.0D0/DBLE(N)
        DO K = 1, N
            TMP = TMP + A(K) * D
        END DO
        OUTPUT%AVG = TMP
        OUTPUT%MIN = A(1)
        OUTPUT%MAX = A(N)


        ! Median
        IF ( MOD(N, 2) .EQ. 0 ) THEN
            OUTPUT%MEDIAN=A((N+1)/2)
        ELSE
            OUTPUT%MEDIAN=0.5D0*(A(N/2)+A((N/2)+1))
        END IF

        ! Lower Quartil
        D = DBLE(N) * LQ
        IF ( MOD(D,1.0D0) .EQ. 0.0D0) THEN
            K = INT(D)
            OUTPUT%LOWERQUART = 0.5D0 * ( A(K) + A(K+1))
        ELSE
            K = FLOOR(D) + 1
            OUTPUT%LOWERQUART = A(K)
        END IF

        ! Upper Quartil
        D = DBLE(N) * UQ
        IF ( MOD(D,1.0D0) .EQ. 0.0D0) THEN
            K = INT(D)
            OUTPUT%UPPERQUART = 0.5D0 * ( A(K) + A(K+1))
        ELSE
            K = FLOOR(D) + 1
            OUTPUT%UPPERQUART = A(K)
        END IF
        IQR = OUTPUT%UPPERQUART - OUTPUT%LOWERQUART
        OUTPUT%IQR = IQR

        ! Lower Whisker
        LW = OUTPUT%MEDIAN - IQRFACT*OUTPUT%IQR
        K = 1
        DO WHILE  ( K.LE.N .AND. LW .GT. A(K))
            K = K +1
        END DO
        ILW = K
        LW = A(K)
        OUTPUT%LOWERWHISKER = LW

        ! Upper WHisker
        UW = OUTPUT%MEDIAN + IQRFACT*OUTPUT%IQR
        K = 1

        DO WHILE  ( K.LE.N .AND. UW .GT. A(K))
            K = K +1
        END DO
        IF ( K.GT.1) THEN
            IUW = K-1
        ELSE
            IUW = 1
        END IF
        UW = A(IUW)
        OUTPUT%UPPERWHISKER = UW

        ! Clean AVG
        N2 = IUW - ILW +1
        D = 1.0D0 / DBLE(N2)
        TMP = 0.0D0
        DO K = ILW, IUW
            TMP = TMP + D*A(K)
        END DO
        OUTPUT%CLEANAVG = TMP


        DEALLOCATE(A)

    END SUBROUTINE CSC_STATISTIC_ANALYZE




END MODULE CSC_SORT_STAT
