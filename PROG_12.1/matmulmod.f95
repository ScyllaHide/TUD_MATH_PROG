MODULE MATMULMOD

    IMPLICIT NONE

    CONTAINS

    FUNCTION SIMPLE_MATMUL(A,B) RESULT (C)
        REAL, DIMENSION(:,:)                 :: A,B
        REAL, DIMENSION(SIZE(A,1),SIZE(A,1)) :: C
        INTEGER                              :: s, row, col, sum, i

        s = UBOUND(A,1)
                
        DO row = 1 , s
            DO col = 1 , s
                sum = 0
                DO i = 1 , s
                    sum = sum + A(row,i) * B(i,col)
                END DO
                C(row,col) = sum
            END DO
        END DO
       

    END FUNCTION SIMPLE_MATMUL


    RECURSIVE FUNCTION STRASSEN_MATMUL(A,B) RESULT (C)
        REAL, DIMENSION(:,:)                     :: A, B
        REAL, DIMENSION(SIZE(A,1),SIZE(A,1))     :: C 
        REAL, DIMENSION(SIZE(A,1)/2,SIZE(A,1)/2) :: M1, M2, M3, M4, M5, M6, M7
        REAL, DIMENSION(SIZE(A,1)/2,SIZE(A,1)/2) :: A11, A12, A21, A22
        REAL, DIMENSION(SIZE(A,1)/2,SIZE(A,1)/2) :: B11, B12, B21, B22
        REAL, DIMENSION(SIZE(A,1)/2,SIZE(A,1)/2) :: C11, C12, C21, C22
        INTEGER                                  :: s,n

        s = size(A,1)/2
        n = size(A,1)

        IF (s <= 128) THEN
            C = MATMUL(A,B)
        ELSE
            A11 = A(1  :s, 1  :s)
            A12 = A(1  :s, s+1:n)
            A21 = A(s+1:n, 1  :s)
            A22 = A(s+1:n, s+1:n)
            
            B11 = B(1  :s, 1  :s)
            B12 = B(1  :s, s+1:n)
            B21 = B(s+1:n, 1  :s)
            B22 = B(s+1:n, s+1:n)

            M1 = STRASSEN_MATMUL(A12 - A22 , B21 + B22)
            M2 = STRASSEN_MATMUL(A11 + A22 , B11 + B22)
            M3 = STRASSEN_MATMUL(A11 - A21 , B11 + B12)
            M4 = STRASSEN_MATMUL(A11 + A12 , B22      )
            M5 = STRASSEN_MATMUL(A11       , B12 - B22)
            M6 = STRASSEN_MATMUL(A22       , B21 - B11)
            M7 = STRASSEN_MATMUL(A21 + A22 , B11      )

            C11 = M1 + M2 - M4 + M6
            C12 = M4 + M5
            C21 = M6 + M7
            C22 = M2 - M3 + M5 - M7

            C(1   : s, 1   : s) = C11
            C(1   : s, s+1 : n) = C12
            C(s+1 : n, 1   : s) = C21
            C(s+1 : n, s+1 : n) = C22

        END IF
    END FUNCTION STRASSEN_MATMUL
  
END MODULE MATMULMOD
