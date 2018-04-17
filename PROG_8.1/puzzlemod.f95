! DOKUMENTATION:
! - Positionen an Puzzleteilen bekommen feste Zahlen (Konstanten)
! - überprüfe zum Schluss, ob Teileliste leer ist

MODULE PUZZLEMOD

    IMPLICIT NONE
    !PRIVATE
    PUBLIC 
    
    !============================= VARIABLES =============================!
    !type definition
    TYPE TEIL
        PRIVATE
        INTEGER, DIMENSION(4) :: SEITEN
        CHARACTER             :: ZEICHEN
    END TYPE
    
    !constant values
    TYPE(TEIL), PARAMETER     :: KEIN_TEIL = TEIL( (/ 0 , 0 , 0 , 0 /) , " " )
    
    !other variables
    INTEGER                   :: M , N
    TYPE(TEIL), DIMENSION(:),   ALLOCATABLE :: TEILELISTE
    TYPE(TEIL), DIMENSION(:,:), ALLOCATABLE :: PUZZLE
    LOGICAL                                 :: ERROR
    
    CONTAINS
    
    !======================= FUNCTION / SUBROUTINE =======================!
    
    SUBROUTINE LIES_PUZZLE (FILENAME)
        CHARACTER (LEN = *), INTENT(IN) :: filename
        INTEGER                         :: ios, err
        
        OPEN(UNIT=33, FILE="filename", IOSTAT=ios, ACTION="READ")
    
        IF (ios == 0) THEN
            
           !Reading puzzle's dimensions
           READ(33, *, iostat=err) m, n
            
            ALLOCATE ( TEILELISTE (m*n) )
            ALLOCATE ( PUZZLE (m,n) )
            
            !Reading parts
            DO i = 1 , m*n
               
                READ(33, *, iostat=err) TEILELISTE(i)
                
                IF (err /= 0) THEN
                    WRITE(*,*) "ERROR: No part is read."
                    EXIT
                END IF
                
            END DO
            
        ELSE
            WRITE(*,*) "ERROR: Cannot open the input file."
        END IF
        
        CLOSE(UNIT=33)
        
    END SUBROUTINE LIES_PUZZLE
    
    FUNCTION DREH (PART)
        TYPE (TEIL), INTENT(INOUT) :: PART
        INTEGER                    :: HELP
        
        HELP           = PART%SEITEN(1)
        PART%SEITEN(1) = PART%SEITEN(4)
        PART%SEITEN(4) = PART%SEITEN(3)
        PART%SEITEN(3) = PART%SEITEN(2)
        PART%SEITEN(2) = HELP
        
        !alternate version - change type of HELP into INTEGER, DIMENSION(3)
        !HELP             = PART%SEITEN(1:3)
        !PART%SEITEN(1)   = PART%SEITEN(4)
        !PART%SEITEN(2:4) = HELP 
          
    END FUNCTION DREH
    
    FUNCTION PASSENDES_TEIL (FORM,SEITE)
        INTEGER,    INTENT(IN)  :: FORM, SEITE
        TYPE(TEIL), INTENT(OUT) :: FITPART
        INTEGER                 :: i,j
        
        ! search for fitting part
        outer: DO i = 1 , m*n       ! traverses all parts
            inner: DO j = 1 , 4     ! traverses all sides of a part
                
                IF ( TEILELISTE(i)%SEITEN(j) == (-1) * FORM ) 
                !fitting part is found - may has to be turned
                    
                    SELECT CASE ( j )
                        CASE ( SEITE )     !EXIT outer
                        CASE ( MODULO( (SEITE + 3 ) , 4) ) DREH ( TEILELISTE(i) )
                        CASE ( MODULO( (SEITE + 2 ) , 4) ) DREH ( DREH ( TEILELISTE(i) ) )
                        CASE ( MODULO( (SEITE + 1 ) , 4) ) DREH ( DREH ( DREH ( TEILELISTE(i) ) ) )
                    END SELECT
                    
                    EXIT outer
                END IF
                
            END DO inner
        END DO outer
        
        FITPART       = TEILELISTE(i)   ! saves returning part
        TEILELISTE(i) = KEIN_TEIL       ! removes returned part
        
    END FUNCTION PASSENDES_TEIL
    
    SUBROUTINE LOESE_PUZZLE
        INTEGER :: i,j
        
        !left top corner
        DO i = 1 , m*n       ! traverses all parts   
            IF (TEILELISTE(i)%SEITEN(3) == 0 .AND. TEILELISTE(i)%SEITEN(4) == 0) THEN
                PUZZLE(1,1) = TEILELISTE(i)
            END IF
        END DO
        
        !solve first row
        DO i = 1 , n
            PUZZLE (1,i) = PASSENDES_TEIL(PUZZLE(1,i-1)%SEITEN(1), 1)
        END DO
        
        !solve every other row
        rows: DO j = 2 , m
            
            PUZZLE(j,1) = PASSENDES_TEIL(PUZZLE(j-1,1)%SEITEN(2), 2)
            
            columns: DO i = 2 , n
                
                PUZZLE(j,i) = PASSENDES_TEIL(PUZZLE(j,i-1)%SEITEN(1), 1)
                
                IF (PASSENDES_TEIL(PUZZLE(j-1,i)%SEITEN(2), 2) /= PUZZLE(j,i) THEN
                    WRITE(*,*) "ERROR: mistakes in captions of parts"
                    error = .TRUE.
                    EXIT rows
                END IF
             
            END DO columns
        END DO rows
        
    END SUBROUTINE LOESE_PUZZLE
    
    SUBROUTINE SCHREIBE_LOESCHE_PUZZLE
        CHARACTER (LEN=*) :: zeile
        INTEGER           :: i,j
        
        DO i = 1 , m
            DO j = 1 , n
                zeile = zeile // PUZZLE(i,j)%ZEICHEN
            END DO
            WRITE(*,*) zeile
        END DO
        
        DEALLOCATE(PUZZLE)
        DEALLOCATE(TEILELISTE)
     
    END SUBROUTINE SCHREIBE_LOESCHE_PUZZLE
        
END MODULE