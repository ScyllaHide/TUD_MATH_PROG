PROGRAM MERGE
    
    USE MERGEMOD
    IMPLICIT NONE

    TYPE(FILECOMP), DIMENSION(:), ALLOCATABLE :: workspace
    INTEGER                                   :: num_file

    DO
        WRITE(*,*) "Geben Sie die Anzahl der zu berücksichtigenden Dateien ein:"
        READ (*,*) num_file
        IF(1 <= num_file .AND. num_file <= 99) EXIT
    END DO
    ALLOCATE(workspace(num_file))

    CALL PHASE1(num_file)
    WRITE(*,*) "Phase 1 fertig."
    CALL PHASE2(workspace)
    WRITE(*,*) "Phase 2 fertig."
    CALL PHASE3(workspace)
    WRITE(*,*) "Phase 3 fertig."
    
CONTAINS
    
    SUBROUTINE PHASE1(num_file)
        CHARACTER(LEN=13) :: filename
        INTEGER          :: i, num_file, ios

        DO i=1, num_file
            WRITE(filename,FMT='(A I2.2 A)') "erfass", i, ".dat"
            OPEN(UNIT=i+50,FILE=TRIM(filename))
        END DO
        OPEN(UNIT=44, FILE = "ziel.dat", IOSTAT = ios, ACTION = 'WRITE', STATUS = 'NEW')

    END SUBROUTINE PHASE1
    
    SUBROUTINE PHASE2(workspace)
        INTEGER                      :: i
        TYPE(FILECOMP), DIMENSION(:) :: workspace
        
        DO i=1, num_file
            workspace(i)%NUM_UNIT = i+50
            READ(UNIT=i+50,FMT='(I13)') workspace(i)%CONT
        END DO

        CALL SORT(workspace)
    END SUBROUTINE PHASE2

    SUBROUTINE PHASE3(workspace)
        INTEGER                                   :: start, err, curr_unit, n
        TYPE(FILECOMP)                            :: new
        TYPE(FILECOMP), DIMENSION(:), ALLOCATABLE :: workspace
        
	    start = 1       ! represents minimum index of active workspace
        n = UBOUND(workspace,1)
        
        DO 
            WRITE(UNIT=44,FMT='(I13)') workspace(start)%CONT
            !WRITE(*,*) workspace(start)%CONT
            
            curr_unit = workspace(start)%NUM_UNIT
            !WRITE(*,*) "curr unit=", curr_unit
            READ(UNIT=curr_unit,FMT='(I13)', iostat=err) new%CONT
            !WRITE(*,*) "new%CONT=", new%CONT
            new%NUM_UNIT = curr_unit
            
            IF(err == 0) THEN
                !WRITE(*,*) "Im IF Zweig"
                CALL INSERT(workspace(start:n),new)
                !WRITE(*,*) workspace(1)%CONT
                !EXIT
            ELSE
                WRITE(*,*) "A file is completed."
                start = start + 1
            END IF
            IF(start > n) EXIT
        END DO      
      
    END SUBROUTINE PHASE3

END PROGRAM MERGE
