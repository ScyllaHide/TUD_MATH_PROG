PROGRAM NEWMATMUL

    USE MATMULMOD
    IMPLICIT NONE

    REAL, DIMENSION(:,:), ALLOCATABLE :: A, B, SIMPLE, STRASSEN, IMPL
    REAL                              :: start, finish
    INTEGER                           :: i, j, k, n, row, col
    CHARACTER(LEN=128)                :: input, output, path1, path2


    WRITE(*,*) "Sollen die berechneten Matrizen ausgeben werden? (y/n)"
    READ (*,*) output

    WRITE(*,*) "Wie sollen die Matrizen eingeben werden? (hilbert/datei)"
    READ (*,*) input

    IF ( input == "hilbert" ) THEN
        WRITE(*,*) "k für 2^k x 2^k - Matrizen"
        READ (*,*) k
        n = 2 ** k    
        ALLOCATE ( A(n,n) , B(n,n) )
        DO row = 1, n
            DO col = 1, n
                A(row, col) = 1/(row + col - 1)
                B(row, col) = 1/(row + col - 1)
            END DO
        END DO
    ELSE
        WRITE(*,*) "Datei-Pfad für Datei mit Matrix A:"
        READ (*,*) path1
        OPEN (UNIT=100, FILE=path1, ACTION="READ")

        WRITE(*,*) "Größe der Matrix"
        READ (*,*) n
        ALLOCATE ( A(n,n) , B(n,n) )

        DO row = 1, n
          READ (100,*) A(row,1:n)
        END DO

        WRITE(*,*) "Datei-Pfad für Datei mit Matrix B:"
        READ (*,*) path2
        open(UNIT=200, FILE=path2, ACTION="READ")

        DO row = 1, n
          READ (200,*) B(row,1:n)
        END DO
    END IF
    
    ALLOCATE(SIMPLE(n,n), STRASSEN(n,n), IMPL(n,n))

! ============ STRASSEN-MULTIPLIKATION ============

    WRITE (*,*) "Strassen-Multiplikation"
    
    CALL CPU_TIME(start)
        STRASSEN = STRASSEN_MATMUL(A,B)
    CALL CPU_TIME(finish)

    IF (output == "y") THEN
        DO i=1, n
            WRITE(*,*) STRASSEN(i,:)
        END DO
    END IF
    
    WRITE(*,*) "Benoetigte Zeit: ", finish - start
    
! ============ VORIMPLEMENTIERTE MULTIPLIKATION ============

    WRITE (*,*) "Implementierte Multiplikation"
    
    CALL CPU_TIME(start)
    IMPL = MATMUL(A,B)
    CALL CPU_TIME(finish)

    IF (output == "y") THEN
        DO i=1, n
            WRITE(*,*) IMPL(i,:)
        END DO
    END IF
    
    WRITE(*,*) "Benoetigte Zeit: ", finish - start
    


! ============ STANDARD-MULTIPLIKATION ============

    WRITE (*,*) "Standard-Multiplikation"
    
    CALL CPU_TIME(start)
        SIMPLE = SIMPLE_MATMUL(A,B)
    CALL CPU_TIME(finish)

    IF (output == "y") THEN
        DO i=1, n
            WRITE(*,*) SIMPLE(i,:)
        END DO
    END IF
    
    WRITE(*,*) "Benoetigte Zeit: ", finish - start

END PROGRAM NEWMATMUL
