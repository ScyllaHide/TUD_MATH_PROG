MODULE MAGICMOD

    IMPLICIT NONE
    PRIVATE
    PUBLIC TEST, CREATE, PUT

    CONTAINS

!===================================================================================
!   TEST IF INPUT ARRAY IS MAGICAL SQAURE

    SUBROUTINE TEST ( A )
        INTEGER, DIMENSION(1:,1:), INTENT(IN)  :: A
        INTEGER                                :: n, S, diag1, diag2, i
        LOGICAL                                :: PROOF

        n = SIZE(A,1)
        S = n * ( n**2 + 1 ) / 2
        WRITE(*,*) "Die Summe sollte sein: ", S

        !---SUM OF DIAGONALS---
        diag1 = 0   !main diagonal
        diag2 = 0   !antidiagonal
        DO i=1, n
            diag1 = diag1 + A(i,i)
            diag2 = diag2 + A(i, n+1-i)
        END DO

        WRITE(*,*) "Zeilensummen:     ", SUM(A,2)
        WRITE(*,*) "Spaltensummen:    ", SUM(A,1)
        WRITE(*,*) "Diagonalensummen: ", diag1, " ", diag2

        PROOF = ( ALL ( SUM(A,1) == ( SUM(A,2) ) .AND. ( SUM(A,1) == S ) ) .AND. (S == diag1) .AND. (S == diag2) )

        IF ( PROOF ) THEN
            WRITE(*,*) "Hurra, ein magisches Quadrat."
        ELSE
            WRITE(*,*) "Hoppla, kein magisches Quadrat."
        END IF

    END SUBROUTINE TEST

!===================================================================================
!   CREATES MAGICAL SQUARE

    FUNCTION CREATE(n) RESULT(A)
        INTEGER, INTENT(IN)     :: n
        INTEGER, DIMENSION(n,n) :: A
        INTEGER                 :: i,j,k!,m

        !m = 0.5 * (n + 1)

        !---INITIALISATION---
        A = 0

        !---CREATING---
        !first step (insert 1)
        i = 1
        j = (n + 1) / 2 !=m
        A(i,j) = 1

                       
        !remaining numbers
        DO k=2, n*n

            IF ( ((i /= 1) .AND. (j /= 1)) .AND. (A(i-1,j-1) == 0)) THEN
                    
                    i = i - 1
                    j = j - 1       !if next element is writable (next element is free and not outside)
                    A(i,j) = k
           
            ELSE                 !if end of matrix is reached
                                 !defines new i and j as stated in task
                DO
                    IF ( i == 1 ) THEN
                        IF ( j == 1 ) THEN
                            i = 2
                            j = 1
                        ELSE
                            i = n
                            j = j-1
                        END IF
                    ELSE
                        IF ( j == 1 ) THEN
                            i = i-1
                            j = n
                        ELSE
                            i = i+1
                           !j = j
                        END IF
                    END IF

                    IF (A(i,j) == 0) THEN
                        A(i,j) = k
                        EXIT
                    END IF
                END DO

            END IF

        END DO
        
    END FUNCTION CREATE
        
!===================================================================================
!   OUTPUT OF SQUARE
        
    SUBROUTINE PUT ( A )
        INTEGER, DIMENSION(1:,1:), INTENT(IN)  :: A
        INTEGER                                :: dig,i,j,n
        CHARACTER (LEN = 2)                    :: stri          !numbers in square have to have less then 98 digits
        
        n = SIZE(A,1)
           
        dig = CEILING(LOG10(REAL(n*n))) + 1     !number of digits and one space between every column
        WRITE(UNIT=stri,FMT=*) dig              !conversion from integer to character
        
        DO i=1,n        !rows
            DO j=1,n    !columns
                WRITE(UNIT=*,FMT='(i'//TRIM(stri)//')', ADVANCE='no') A(i,j)   !output: every column of  matrix
            END DO
            WRITE(*,*)
        END DO   
        
    END SUBROUTINE PUT
        

END MODULE MAGICMOD