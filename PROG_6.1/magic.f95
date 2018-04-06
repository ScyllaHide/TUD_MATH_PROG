PROGRAM MAGIC
    USE MAGICMOD
    IMPLICIT NONE

    INTEGER                              :: n
    INTEGER, DIMENSION(:,:), ALLOCATABLE :: A

    outer: DO
       inner: DO
            WRITE(*,*) "Eingabe der Dimension"
            READ(*,*) n
            IF ( n < 1 ) EXIT outer
            IF ( MODULO( n , 2 ) == 1) EXIT inner
        END DO inner

        ALLOCATE( A(n,n) )

        A = CREATE ( n )    !creates magical square
        
        CALL PUT ( A )      !output
        
        CALL TEST ( A )     !tests if A is magical

        DEALLOCATE( A )

        WRITE(*,*) "EXIT: negative Zahl eingeben"

    END DO outer

END PROGRAM