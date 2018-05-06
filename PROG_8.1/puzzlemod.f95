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
    INTEGER                   :: m , n, i, j
    TYPE(TEIL), DIMENSION(:),   ALLOCATABLE :: TEILELISTE
    TYPE(TEIL), DIMENSION(:,:), ALLOCATABLE :: PUZZLE
    ! LOGICAL                               :: ERROR

    INTERFACE OPERATOR (==)
        MODULE PROCEDURE EQ
    END INTERFACE

    CONTAINS

    FUNCTION EQ (TEIL1 , TEIL2)
        TYPE(TEIL), INTENT(IN) :: TEIL1, TEIL2
        LOGICAL                :: EQ

        EQ = (ALL(TEIL1%SEITEN == TEIL2%SEITEN) .AND. TEIL1%ZEICHEN == TEIL2%ZEICHEN)
    END FUNCTION EQ

    !======================= FUNCTION / SUBROUTINE =======================!

    SUBROUTINE LIES_PUZZLE (filename)
        CHARACTER (LEN = *), INTENT(IN) :: filename
        TYPE(TEIL)                      :: zeile
        INTEGER                         :: ios, err

        OPEN(UNIT=33, FILE=filename, IOSTAT=ios, ACTION="READ")

        IF (ios == 0) THEN

           ! Reading puzzle's dimensions
           READ(33, *, iostat=err) m, n
           !WRITE(*,*) "m= ", m, " || n= ", n

           ALLOCATE ( TEILELISTE (m*n) )
           ALLOCATE ( PUZZLE (m,n) )

           ! Reading parts
           DO i = 1 , m*n
               READ(33, *, iostat=err) zeile

               IF (err /= 0) THEN
                   WRITE(*,*) "ERROR: No part is read."
                   EXIT
               END IF

              TEILELISTE(i) = zeile

           END DO

        ELSE
           WRITE(*,*) "ERROR: Cannot open the input file."
        END IF

        CLOSE(UNIT=33)

    END SUBROUTINE LIES_PUZZLE

    FUNCTION DREH (PART)
        TYPE (TEIL), INTENT(IN) :: PART
        TYPE (TEIL)             :: DREH

        DREH%SEITEN(2:4) = PART%SEITEN(1:3)
        DREH%SEITEN(1)   = PART%SEITEN(4)
        DREH%ZEICHEN     = PART%ZEICHEN

    END FUNCTION DREH

    FUNCTION CYCLE_ADD(a,b)     ! zyklisches Addieren u. Subtrahieren in Menge {1,...,div}
        INTEGER, INTENT(IN) :: a,b
        INTEGER             :: CYCLE_ADD
        INTEGER, PARAMETER  :: div = 4

        IF ( MODULO( a+b , div ) == 0) THEN
            CYCLE_ADD = div
        ELSE
            CYCLE_ADD = MODULO( a+b , div )
        END IF
    END FUNCTION CYCLE_ADD

    FUNCTION PASSENDES_TEIL (FORM,SEITE)
        INTEGER, INTENT(IN)  :: FORM, SEITE
        TYPE(TEIL)           :: PASSENDES_TEIL

        ! FORM is number of part at specific SEITE of part
        ! FORM is form (number) of origin (negated here)
        ! SEITE is side on which the form has to be (determined at call)

        outer: DO i = 1 , m*n       ! traverses all parts
            inner: DO j = 1 , 4     ! traverses all sides of a part

                IF ( (TEILELISTE(i)%SEITEN(j)) == ((-1) * FORM) ) THEN
                ! fitting part is found - may has to be turned

                    IF ( j == SEITE ) THEN
                            PASSENDES_TEIL = TEILELISTE(i)
                        ELSEIF ( j == CYCLE_ADD(SEITE,3) ) THEN
                            PASSENDES_TEIL = DREH ( TEILELISTE(i) )
                        ELSEIF ( j == CYCLE_ADD(SEITE,2) ) THEN
                            PASSENDES_TEIL = DREH ( DREH ( TEILELISTE(i) ) )
                        ELSEIF ( j == CYCLE_ADD(SEITE,1) ) THEN
                            PASSENDES_TEIL = DREH ( DREH ( DREH ( TEILELISTE(i) ) ) )
                    END IF

                    TEILELISTE(i) = KEIN_TEIL       ! removes returned part
                    EXIT outer

                END IF

            END DO inner
        END DO outer

    END FUNCTION PASSENDES_TEIL

    SUBROUTINE LOESE_PUZZLE
        INTEGER :: i,j      ! try on linux if skipable
        LOGICAL :: check

        !left top corner
        DO i = 1 , m*n       ! traverses all parts
            IF (TEILELISTE(i)%SEITEN(3) == 0 .AND. TEILELISTE(i)%SEITEN(4) == 0) THEN
                PUZZLE(1,1) = TEILELISTE(i)
                TEILELISTE(i) = KEIN_TEIL
            END IF
        END DO

        !solve first row
        DO i = 2 , n
            PUZZLE (1,i) = PASSENDES_TEIL(PUZZLE(1,i-1)%SEITEN(1), 3)
        END DO

        !solve every other row
        rows: DO j = 2 , m

            PUZZLE(j,1) = PASSENDES_TEIL(PUZZLE(j-1,1)%SEITEN(2), 4)

            columns: DO i = 2 , n

                PUZZLE(j,i) = PASSENDES_TEIL(PUZZLE(j,i-1)%SEITEN(1), 3)

                ! IF ( PASSENDES_TEIL(PUZZLE(j-1,i)%SEITEN(2), 4) /= PUZZLE(j,i) ) THEN
                    ! WRITE(*,*) "ERROR: mistakes in captions of parts"
                    ! error = .TRUE.
                    ! EXIT rows
                ! END IF

            END DO columns
        END DO rows

        CHECK = .TRUE.
         DO i = 1 , m*n
            CHECK = TEILELISTE(i) == KEIN_TEIL .AND. CHECK
         END DO

        IF(CHECK) THEN
                WRITE(*,*) "Das Puzzle wurde erfolgreich gelöst."
            ELSE
                WRITE(*,*) "Das Puzzle wurde nicht korrekt gelöst."
        END IF

    END SUBROUTINE LOESE_PUZZLE

    SUBROUTINE SCHREIBE_LOESCHE_PUZZLE
        CHARACTER (LEN=50) :: solution
        INTEGER            :: pos

        pos = 0

        DO i = 1 , m
            DO j = 1 , n
                pos = pos + 1
                solution(pos:pos) = PUZZLE(i,j)%ZEICHEN
            END DO
        END DO

        WRITE(*,*) "Das Lösungswort lautet ", TRIM(solution), "."

        DEALLOCATE(PUZZLE)
        DEALLOCATE(TEILELISTE)

    END SUBROUTINE SCHREIBE_LOESCHE_PUZZLE

    !======================= TESTS =======================!

    SUBROUTINE TEST_DREH
        TYPE (TEIL) :: PART, TURNED
        LOGICAL     :: TEST

        PART = TEIL( (/ 1 , 2 , 3 , 4 /) , "T" )
        WRITE(*,*) "Original: ", PART
        TURNED = DREH(PART)
        WRITE(*,*) "Turned:   ", TURNED
        TEST = (TURNED%SEITEN(1) == 4 .AND. TURNED%SEITEN(2) == 1 .AND. &
              & TURNED%SEITEN(3) == 2 .AND. TURNED%SEITEN(4) == 3 )
        WRITE(*,*) "TEST = ", TEST

        TURNED = DREH(DREH(PART))
        WRITE(*,*) "Turned:   ", TURNED
        TEST = (TURNED%SEITEN(1) == 3 .AND. TURNED%SEITEN(2) == 4 .AND. &
              & TURNED%SEITEN(3) == 1 .AND. TURNED%SEITEN(4) == 2 )
        WRITE(*,*) "TEST = ", TEST

        TURNED = DREH(DREH(DREH(PART)))
        WRITE(*,*) "Turned:   ", TURNED
        TEST = (TURNED%SEITEN(1) == 2 .AND. TURNED%SEITEN(2) == 3 .AND. &
              & TURNED%SEITEN(3) == 4 .AND. TURNED%SEITEN(4) == 1 )
        WRITE(*,*) "TEST = ", TEST

        READ(*,*)

    END SUBROUTINE

    SUBROUTINE TEST_CYCLE_ADD
        WRITE(*,*) "4+0= ", CYCLE_ADD(4,0)
        WRITE(*,*) "4+1= ", CYCLE_ADD(4,1)
        WRITE(*,*) "4+2= ", CYCLE_ADD(4,2)
        WRITE(*,*) "3+4= ", CYCLE_ADD(3,4)
        WRITE(*,*) "1-2= ", CYCLE_ADD(1,-2)
    END SUBROUTINE TEST_CYCLE_ADD

    SUBROUTINE TEST_PASSENDES_TEIL
        WRITE(*,*) "TEST FUNCTION PASSENDES_TEIL"
        WRITE(*,*) PASSENDES_TEIL (-7,4)
        WRITE(*,*) "Expected: 1 0 -2 7 -L-"
    END SUBROUTINE TEST_PASSENDES_TEIL

END MODULE PUZZLEMOD
