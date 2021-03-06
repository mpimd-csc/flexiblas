MODULE CSC_MTX
    USE ISO_C_BINDING
    IMPLICIT NONE


    INTERFACE
        FUNCTION CSC_MTX_WRITE_DOUBLE_DENSE_C(FILENAME, M, N, A, LDA) BIND(C, name="csc_mtx_write_double_dense")
            IMPORT
            INTEGER(KIND=C_INT) :: CSC_MTX_WRITE_DOUBLE_DENSE_C
            CHARACTER(KIND=C_CHAR), INTENT(IN) :: FILENAME(*)
            INTEGER(KIND=C_INT), INTENT(IN), VALUE :: M
            INTEGER(KIND=C_INT), INTENT(IN), VALUE :: N
            INTEGER(KIND=C_INT), INTENT(IN), VALUE :: LDA
            REAL(KIND=C_DOUBLE), INTENT(IN) :: A(LDA, *)
        END FUNCTION CSC_MTX_WRITE_DOUBLE_DENSE_C


        FUNCTION CSC_MTX_READ_DENSE_SIZE_C(FILENAME, M, N) BIND(C, name="csc_mtx_read_dense_size")
            IMPORT
            INTEGER(KIND=C_INT) :: CSC_MTX_READ_DENSE_SIZE_C
            CHARACTER(KIND=C_CHAR), INTENT(IN) :: FILENAME(*)
            INTEGER(KIND=C_INT64_T), INTENT(INOUT) :: M
            INTEGER(KIND=C_INT64_T), INTENT(INOUT) :: N
        END FUNCTION CSC_MTX_READ_DENSE_SIZE_C

        FUNCTION CSC_MTX_READ_DOUBLE_DENSE_C(FILENAME, M, N, A, LDA) BIND(C, name="csc_mtx_read_double_dense")
            IMPORT
            INTEGER(KIND=C_INT) :: CSC_MTX_READ_DOUBLE_DENSE_C
            CHARACTER(KIND=C_CHAR), INTENT(IN) :: FILENAME(*)
            INTEGER(KIND=C_INT64_T), INTENT(IN), VALUE :: M
            INTEGER(KIND=C_INT64_T), INTENT(IN), VALUE :: N
            INTEGER(KIND=C_INT64_T), INTENT(IN), VALUE :: LDA
            REAL(KIND=C_DOUBLE), INTENT(INOUT) :: A(LDA,*)
        END FUNCTION CSC_MTX_READ_DOUBLE_DENSE_C



    END INTERFACE
CONTAINS
    SUBROUTINE CSC_MTX_WRITE_DOUBLE_DENSE(FILENAME, M, N, A, LDA, INFO)
        IMPLICIT NONE
        CHARACTER(LEN=*), INTENT(IN) :: FILENAME
        INTEGER, INTENT(IN) :: M, N, LDA
        INTEGER, INTENT(OUT) :: INFO
        DOUBLE PRECISION, INTENT(IN) :: A(LDA, *)

        INTEGER(KIND=C_INT) :: RET

        RET = CSC_MTX_WRITE_DOUBLE_DENSE_C(TRIM(FILENAME)//C_NULL_CHAR, INT(M,KIND=C_INT), INT(N, KIND=C_INT), &
            & A, INT(LDA, KIND=C_INT))

        IF ( RET .NE. 0 ) THEN
            INFO = 1
        ELSE
            INFO = 0
        END IF
        RETURN
    END SUBROUTINE CSC_MTX_WRITE_DOUBLE_DENSE

    SUBROUTINE CSC_MTX_READ_DENSE_SIZE(FILENAME, M, N, INFO)
        IMPLICIT NONE
        CHARACTER(LEN=*), INTENT(IN) :: FILENAME
        INTEGER, INTENT(OUT) :: M, N
        INTEGER, INTENT(OUT) :: INFO

        INTEGER(KIND=C_INT) :: RET
        INTEGER(KIND=C_INT64_T) :: LM, LN

        RET = CSC_MTX_READ_DENSE_SIZE_C(TRIM(FILENAME)//C_NULL_CHAR, LM, LN)

        IF ( RET .NE. 0 ) THEN
            INFO = 1
        ELSE
            INFO = 0
        END IF
        M = INT(LM)
        N = INT(LN)
        RETURN
    END SUBROUTINE CSC_MTX_READ_DENSE_SIZE

    SUBROUTINE CSC_MTX_READ_DOUBLE_DENSE(FILENAME, M, N, A, LDA, INFO)
        IMPLICIT NONE
        CHARACTER(LEN=*), INTENT(IN) :: FILENAME
        INTEGER, INTENT(IN) :: M, N, LDA
        INTEGER, INTENT(OUT) :: INFO
        DOUBLE PRECISION, INTENT(INOUT) :: A(LDA, *)

        INTEGER(KIND=C_INT) :: RET
        INTEGER(KIND=C_INT64_T) :: LM, LN, LLDA

       LM = INT(M, KIND = C_INT64_T)
       LN = INT(N, KIND = C_INT64_T)
       LLDA = INT(LDA, KIND = C_INT64_T)


        RET = CSC_MTX_READ_DOUBLE_DENSE_C(TRIM(FILENAME)//C_NULL_CHAR, LM, LN, A, LLDA)

        IF ( RET .NE. 0 ) THEN
            INFO = 1
        ELSE
            INFO = 0
        END IF
        RETURN
    END SUBROUTINE CSC_MTX_READ_DOUBLE_DENSE



END MODULE CSC_MTX
