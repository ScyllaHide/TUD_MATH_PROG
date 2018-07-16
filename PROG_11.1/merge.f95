PROGRAM MERGE_HP

    USE MERGEMOD
    IMPLICIT NONE

    TYPE(FILECOMP), DIMENSION(:), ALLOCATABLE :: workspace
    TYPE(FILECOMP)                            :: toins
    INTEGER                                   :: num_file, i

    DO
        WRITE(*,*) "Geben Sie die Anzahl der zu berücksichtigenden Dateien ein:"
        READ (*,*) num_file
        IF(1 <= num_file .AND. num_file <= 99) EXIT
    END DO
    ALLOCATE(workspace(1:num_file))

!    TEST SUBROUTINE SORT/INSERT
!    DO i=1,num_file
!        workspace(i)%CONT = 20 - 3*i
!    END DO
!    toins%NUM_UNIT = 11
!    toins%CONT     = 8
!    CALL SORT(workspace)

!    DO i=1,num_file
!        WRITE(*,*) "i.Stelle = ", workspace(i)%CONT
!    END DO

    CALL PHASE1(num_file)
    WRITE(*,*) "Phase 1 fertig."
    WRITE(*,*) "-----------------------------------"

    CALL PHASE2(workspace)
    WRITE(*,*) "Phase 2 fertig."
    DO i = 1, num_file
        WRITE(*,*) "Unit: ", workspace(i)%NUM_UNIT, " Zahl: ", workspace(i)%CONT
    END DO
    WRITE(*,*) "-----------------------------------"

    CALL PHASE3(workspace)
    WRITE(*,*) "Phase 3 fertig."

    CALL TEST(44)
END PROGRAM MERGE_HP
