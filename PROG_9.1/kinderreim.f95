PROGRAM KINDERREIM
	
	USE KINDERREIMMOD
	IMPLICIT NONE
	
	TYPE(START)          :: LIST
	CHARACTER(LEN=50)    :: FILENAME
	TYPE(CHILD), POINTER :: CURR_CHILD
	INTEGER              :: i, j
	
	WRITE(*,*) "Geben Sie den Dateinamen ein: "
	READ(*,*) FILENAME
	CALL BUILD_CYCLE(TRIM(FILENAME), LIST)
	WRITE(*,*) "------------------------------" 
	
	DO j=1, LENGTH(LIST)-1
		WRITE(*,*) "AKTUELL SPIELEN MIT: "
		CALL PUT_CYCLE(LIST, LIST%TOP)
		WRITE(*,*) "----------------------- RUNDENSTART -----------------------"
		CURR_CHILD => LIST%TOP
		WRITE(*,*) "---> Ehne mehne muh und raus bist du, raus bist du noch lange nicht, sag mir erst wie alt du bist ?!: "
		
		! first 21 steps (syllables of counting rhyme)
		DO i=1, 20
			CURR_CHILD => CURR_CHILD%NEXT
		END DO
		
		CALL PRINT_CHILD(CURR_CHILD)
		WRITE(*,*) "Ich bin ", CURR_CHILD%DATA%AGE, " Jahre alt."
		
		! next steps (as much as child is old)
		DO i=1, CURR_CHILD%DATA%AGE-1
			CURR_CHILD => CURR_CHILD%NEXT
		END DO
		
		WRITE(*,*) "---> ausgeschieden ist: "
		CALL PRINT_CHILD(CURR_CHILD%NEXT)
		CALL DEL_NEXT(LIST,CURR_CHILD)
		
		WRITE(*,*) "----------------------- RUNDENENDE -----------------------"
		
		! change starting point of list
		LIST%TOP => CURR_CHILD%NEXT
	END DO

	WRITE(*,*) "----------------------- SPIELENDE -----------------------"
	WRITE(*,*) "GEWONNEN HAT:"
	CALL PUT_CYCLE(LIST, LIST%TOP)
	
	DEALLOCATE(CURR_CHILD)
	
END PROGRAM KINDERREIM