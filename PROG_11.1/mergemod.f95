MODULE MERGEMOD

 IMPLICIT NONE

    TYPE FILECOMP
        INTEGER :: NUM_UNIT  ! Unitnummer
        INTEGER :: CONT      ! zuletzt eingelesene Zahl
    END TYPE

    INTERFACE OPERATOR (<=)
        MODULE PROCEDURE LEQ
    END INTERFACE

    CONTAINS

    FUNCTION LEQ(FILE1, FILE2)
        TYPE(FILECOMP), INTENT(IN) :: FILE1, FILE2
        LOGICAL                    :: LEQ

        LEQ = (FILE1%CONT <= FILE2%CONT)
    END FUNCTION

    SUBROUTINE SORT(list)
        TYPE(FILECOMP), DIMENSION(:) :: list
        TYPE(FILECOMP)               :: toins
        INTEGER                      :: i, j

        DO i=2 , UBOUND(list,1)
            toins = list(i)
            j = i
            DO
                list(j) = list(j-1)
                j       = j - 1
                IF(j < 2) THEN
                    EXIT
                ELSEIF (list(j-1) <= toins) THEN
                    EXIT
                END IF
            END DO
            list(j) = toins
        END DO

    END SUBROUTINE SORT

    SUBROUTINE BUBBLESORT(list)
        TYPE(FILECOMP), DIMENSION(:), INTENT(INOUT) :: list
        INTEGER                                     :: num, i, n
        TYPE(FILECOMP)                              :: TEMP1, TEMP2

        num = UBOUND(list,1)

        DO n = num, 1, -1
          DO i = 0, n-1, 1
            IF(list(i+1) <= list(I)) THEN
              TEMP1 = list(i)
              TEMP2 = list(i+1)

              list(i)   = TEMP2
              list(i+1) = TEMP1
            END IF
          END DO
        END DO

    END SUBROUTINE SORT

    SUBROUTINE INSERT(list, toins)
        TYPE(FILECOMP), DIMENSION(:), INTENT(INOUT) :: list
        TYPE(FILECOMP)                              :: toins
        INTEGER                                     :: mid, left, right

        left  = LBOUND(list,1) + 1
        right = UBOUND(list,1)

        DO
            mid = (left+right) / 2
            IF(left >= right) THEN
                EXIT
            ELSEIF(toins <= list(mid)) THEN
                right = mid - 1
            ELSE
                left = mid + 1
            END IF
        END DO

        IF(mid == 1) THEN
            list(1) = toins
        ELSE
            IF(toins <= list(mid)) THEN
                mid = mid - 1
            END IF
            list(1:mid-1) = list(2:mid)
            list(mid)     = toins
        END IF

    END SUBROUTINE INSERT

    SUBROUTINE INSERT2(LIST, TOINS)
        TYPE(FILECOMP), DIMENSION(:), INTENT(INOUT) :: list
        TYPE(FILECOMP)                              :: toins
        INTEGER                                     :: left, right, j, mid, antiSpam, midsave

        left = 1
        right = SIZE(list,1)

        ! DO antiSpam = 1, 30
            IF (toins%CONT > list(right)%CONT) THEN
                mid = right
                EXIT
            END IF

            midsave = mid
            mid = (right+left)/2

            IF (mid == midsave) EXIT

            IF (toins <= list(mid)) THEN
                right = mid
            ELSE
                left = mid
            END IF

            IF (right == left) EXIT
        ! END DO

        list(1:mid-1) = list(2:mid)
        list(mid)     = toins

    END SUBROUTINE INSERT2

!=================================================================

    SUBROUTINE PHASE1(num_file)
        CHARACTER(LEN=13) :: filename
        INTEGER           :: i, num_file

        DO i=1, num_file
            WRITE(filename,FMT='(A I2.2 A)') "erfass", i, ".dat"
            OPEN(UNIT=i+50,FILE=TRIM(filename), ACTION = "READ")
            WRITE(*,*) filename, " Unit: ", i+50
        END DO
        OPEN(UNIT=44, FILE = "ziel.dat", ACTION = "WRITE")
    END SUBROUTINE PHASE1

    SUBROUTINE PHASE2(workspace)
        INTEGER                      :: i, num_file
        TYPE(FILECOMP), DIMENSION(:) :: workspace

        num_file = SIZE(workspace,1)

        DO i=1, num_file
            workspace(i)%NUM_UNIT = i+50
            READ(UNIT=i+50,*) workspace(i)%CONT
        END DO

        CALL SORT(workspace) !alternativ: BUBBLESORT(workspace)
    END SUBROUTINE PHASE2

    SUBROUTINE PHASE3(workspace)
        INTEGER                                   :: start, ios, curr_unit, n
        TYPE(FILECOMP)                            :: new
        TYPE(FILECOMP), DIMENSION(:), ALLOCATABLE :: workspace

        WRITE(*,*) workspace
        start = 1       ! represents minimum index of active workspace
        n = SIZE(workspace,1)

        DO
            WRITE(44,*) workspace(start)%CONT
            WRITE(*,*)  "Schreibe Zahl im Index ", start, " in Ziel: ", workspace(start)%CONT

            curr_unit = workspace(start)%NUM_UNIT
            new%NUM_UNIT = curr_unit
            READ(UNIT=curr_unit,FMT=*, iostat=ios) new%CONT

            IF(ios == 0) THEN
                CALL INSERT(workspace(start:n),new) !alternativ: INSERT2(workspace(start:),new)
            ELSE
                WRITE(*,*) "A file is completed."
                start = start + 1
                WRITE(*,*) new
            END IF
            IF(start > n) EXIT
        END DO
        CLOSE(UNIT=44)
    END SUBROUTINE PHASE3

    SUBROUTINE TEST(num_unit)
        INTEGER :: num_unit, num, newnum, err, i
        LOGICAL :: check
        i=1
        OPEN(UNIT=44, FILE = "ziel.dat", ACTION="READ")
        READ(num_unit,*) num
        check = .TRUE.
        DO
            i = i + 1
            READ(UNIT=num_unit,FMT='(I13)', iostat = err) newnum
            IF(newnum < num) THEN
                check = .FALSE.
                WRITE(*,*) i
                !EXIT
            END IF
            IF(.NOT. err == 0) EXIT
            num = newnum
        END DO

        IF(check) THEN
            WRITE(*,*) ">> Die Folge ist tatsächlich monoton wachsend."
        ELSE
            WRITE(*,*) ">> Ups - ein Fehler: die Folge ist nicht monoton:"

        END IF

        CLOSE(UNIT=num_unit)
    END SUBROUTINE TEST

END MODULE MERGEMOD
