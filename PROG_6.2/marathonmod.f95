MODULE MARATHONMOD

IMPLICIT NONE
PUBLIC

TYPE ZEIT
    !PRIVATE
    INTEGER :: H , M
    REAL    :: S
END TYPE ZEIT

TYPE ERGEBNIS
    TYPE(ZEIT)        :: M_TIME
    CHARACTER(LEN=30) :: RUNNER
END TYPE ERGEBNIS

INTERFACE OPERATOR( < )
    MODULE PROCEDURE IS_FASTER
END INTERFACE

INTERFACE OPERATOR ( < )
    MODULE PROCEDURE IS_BETTER
END INTERFACE

CONTAINS

FUNCTION IS_BETTER(RES1, RES2)
    TYPE(ERGEBNIS), INTENT(IN) :: RES1, RES2
    LOGICAL                    :: IS_BETTER    
    
    IS_BETTER = (RES1%M_TIME < RES2%M_TIME)
    
END FUNCTION

FUNCTION IS_FASTER(TIME1,TIME2)
    TYPE(ZEIT), INTENT(IN) :: TIME1, TIME2
    LOGICAL                :: IS_FASTER
    
    IS_FASTER = ((TIME1%H < TIME2%H) .OR. ( TIME1%H == TIME2%H .AND. TIME1%M < TIME2%M ) &
              & .OR.(TIME1%H == TIME2%H .AND. TIME1%M == TIME2%M .AND. TIME1%S < TIME2%S))

END FUNCTION IS_FASTER

SUBROUTINE LESELISTE(TABLE, n)
    TYPE(ERGEBNIS), DIMENSION(:), INTENT(INOUT) :: table
    INTEGER, INTENT(OUT)                        :: n             !number of read results
    TYPE(ERGEBNIS)                              :: zeile
    INTEGER                                     :: err, ios
    
    n = 1
    
    OPEN(UNIT=33, FILE="marathon.dat", IOSTAT=ios, ACTION="READ")
    
    IF (ios == 0) THEN
        DO
            READ(33, *, iostat=err) zeile
            IF (err /= 0 .OR. n > UBOUND(table, dim=1)) EXIT     !end of file is reached
           
            table(n) = zeile
            n = n + 1 
        END DO
    ELSE
        WRITE(*,*) "ERROR: Cannot open the input file."
    END IF
    
END SUBROUTINE

RECURSIVE SUBROUTINE QUICKSORT(list, first, last)
    IMPLICIT NONE
    TYPE(ERGEBNIS) :: list(*)
    INTEGER                      :: first, last
    TYPE(ERGEBNIS)               :: x, w
    INTEGER                      :: i, j
    
    i = first
    j = last
    x = list( (first + last) / 2 )
    
    DO
        DO WHILE (list(i) < x)
            i = i + 1
        END DO
        DO WHILE ( x < list(j) )
            j = j - 1
        END DO
        
        IF ( i >= j) EXIT
        w = list(i)
        list(i) = list(j)
        list(j) = w
        i = i + 1
        j = j - 1
    END DO
    
    IF ( first < i-1 ) CALL QUICKSORT(list, first, i-1)
    IF ( j+1 > last ) CALL QUICKSORT(list, j+1, last)
    
END SUBROUTINE QUICKSORT
    
END MODULE