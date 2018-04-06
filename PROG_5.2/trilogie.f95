PROGRAM TRILOGIE
	USE TRILMOD !import of all public components of module TRILMOD
	IMPLICIT NONE
	
	TYPE(TRILOG), DIMENSION(3), PARAMETER :: VEC = (/ FALSE , MAYBE , TRUE /)
	LOGICAL                               :: PROOF1, PROOF2
	INTEGER                               :: i, j, k
	
	!================================!
	! TRUTH TABLE FOR TRILEAN VALUES !
	!================================!
	
	WRITE(*,*) "TRUTH TABLE FOR TRILEAN VALUES"
	WRITE(*,*) "=============================="
	WRITE(*,*) "A B | AND  OR  NOT"
	WRITE(*,*) "------------------"
	DO i = 1,3
		DO j = 1,3
			WRITE (*,*) TEXT(VEC(i))," ", TEXT(VEC(j)), " | ", TEXT(VEC(i) .AND. VEC(j)), "    ", &
			&									              TEXT(VEC(i) .OR. VEC(j)), "   ", &
			&									              TEXT(.NOT. VEC(i))
		END DO
	END DO
	
	!=======================!
	! DISTRIBUTIVE PROPERTY !
	!=======================!
	
	WRITE(*,*) " " 		!write an empty line for better style!
	WRITE(*,*) "DISTRIBUTIVE PROPERTY"
	WRITE(*,*) "====================="
	
	OUTER1: DO i=1,3
		INNER1: DO j=1,3
			INNEST1: DO k=1,3
			
				PROOF1 = ( (VEC(i) .AND. (VEC(j) .OR. VEC(k)) ) == ( ( VEC(i) .AND. VEC(j) ) .OR. ( VEC(i) .AND. VEC(k)) ) )
				PROOF2 = ( (VEC(i) .OR. (VEC(j) .AND. VEC(k)) ) == ( ( VEC(i) .OR. VEC(j) ) .AND. ( VEC(i) .OR. VEC(k)) ) )
				
				IF((.NOT. PROOF1) .OR. (.NOT. PROOF2)) THEN
					WRITE(*,*) "trilean logic is not distributive"
					EXIT OUTER1
				END IF
				
			END DO INNEST1
		END DO INNER1
	END DO OUTER1
	
	IF (PROOF1 .AND. PROOF2) WRITE(*,*) "trilean logic is distributive"
	
	!==================!
	! DE MORGAN'S LAWS !
	!==================!
	
	WRITE(*,*) " " 		!write an empty line for better style!
	WRITE(*,*) "DE MORGAN'S LAWS"
	WRITE(*,*) "================"
	
	OUTER2: DO i=1,3
		INNER2: DO j=1,3
			
			PROOF1 = ( ( .NOT. ((VEC(i) .AND. VEC(j))) ) == ( (.NOT. VEC(i)) .OR. (.NOT. VEC(j)) ) )
			PROOF2 = ( ( .NOT. ((VEC(i) .OR. VEC(j))) ) == ( (.NOT. VEC(i)) .AND. (.NOT. VEC(j)) ) )
			
			IF((.NOT. PROOF1) .AND. (.NOT. PROOF2)) THEN
				WRITE(*,*) "De Morgan's laws are not valid for trilean logic"
				EXIT OUTER2
			END IF
				
		END DO INNER2
	END DO OUTER2
	
	IF (PROOF1 .AND. PROOF2) WRITE(*,*) "De Morgan's laws are valid for trilean logic"
	
	READ(*,*)
	
END PROGRAM
		