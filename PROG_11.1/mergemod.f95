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
            DO WHILE (j>1 .AND. toins <= list(j-1))
                list(j) = list(j-1)
                j       = j - 1
            END DO
            list(j) = toins
        END DO
                       
    END SUBROUTINE SORT

    SUBROUTINE INSERT(list, toins)
        TYPE(FILECOMP), DIMENSION(:) :: list
        TYPE(FILECOMP)               :: toins
        INTEGER                      :: mid, left, right
        
        left  = LBOUND(list,1)
        right = UBOUND(list,1)
        
        DO 
            mid = (left+right) / 2
	    	IF(left > right) THEN
				EXIT
	        ELSEIF(toins <= list(mid)) THEN
                right = mid - 1
            ELSE
                left = mid + 1
            END IF
        END DO
        IF(toins <= list(mid)) THEN
			mid = mid - 1 
		END IF
        list(1:mid-1) = list(2:mid)
        list(mid)     = toins

    END SUBROUTINE INSERT

END MODULE MERGEMOD

!IF(toins <= list(mid) .AND. list(mid-1) <= toins) THEN	!element to insert between mid-1 an mid
!                mid = mid -1	!because element has to be inserted at mid
!                EXIT
!            ELSEIF(toins <= list(mid+1) .AND. list(mid) <= toins) THEN	!element to insert between mid and mid + 1
!                EXIT
