MODULE QUEUEMOD
    IMPLICIT NONE

    ! =========== TYPE DEFINITIONS ===========
    TYPE NODE
        INTEGER             :: CONT
        TYPE(NODE), POINTER :: NEXT
    END TYPE NODE

    TYPE QUEUE
        INTEGER             :: LENGTH
        TYPE(NODE), POINTER :: HEAD
        TYPE(NODE), POINTER :: TAIL
    END TYPE QUEUE

    ! ===========    INTERFACES    ===========
    INTERFACE PUT
        MODULE PROCEDURE PUT
    END INTERFACE

    CONTAINS

    ! =========== FUNCTIONS / SUBROUTINES ===========

    SUBROUTINE INIT(LIST)                   ! initalizes queue
        TYPE(QUEUE), INTENT(INOUT) :: LIST
        LIST%LENGTH = 0
        NULLIFY(LIST%HEAD, LIST%TAIL)
    END SUBROUTINE INIT

    FUNCTION EMPTY(LIST)                    ! checks if queue is empty
        TYPE(QUEUE), INTENT(IN) :: LIST
        LOGICAL                 :: EMPTY

        EMPTY = (LIST%LENGTH == 0)
    END FUNCTION EMPTY

    ! ----- INSERT / DELETE -----
    SUBROUTINE ENQUEUE(LIST, CONT)          ! insert at tail
        TYPE(QUEUE), INTENT(INOUT) :: LIST
        INTEGER,     INTENT(IN)    :: CONT

        IF(EMPTY(LIST)) THEN
            ALLOCATE(LIST%HEAD)
            LIST%HEAD%CONT =  CONT
            LIST%TAIL      => LIST%HEAD
        ELSE
            ALLOCATE(LIST%TAIL%NEXT)
            LIST%TAIL%NEXT%CONT =  CONT
            LIST%TAIL           => LIST%TAIL%NEXT
        END IF
        LIST%LENGTH = LIST%LENGTH + 1

    END SUBROUTINE ENQUEUE

    SUBROUTINE DEQUEUE(LIST)                ! delete at head
        TYPE(QUEUE), INTENT(INOUT) :: LIST
        TYPE(NODE) , POINTER       :: HELP

        IF(EMPTY(LIST)) THEN
            WRITE(*,*) "Die Warteschlange ist leer."
        ELSE
            IF(ASSOCIATED(LIST%HEAD, LIST%TAIL)) THEN ! one node rest
                DEALLOCATE(LIST%HEAD)
                NULLIFY(LIST%HEAD, LIST%TAIL)
            ELSE
                HELP      => LIST%HEAD%NEXT
                DEALLOCATE(LIST%HEAD)
                LIST%HEAD => HELP
            END IF
            LIST%LENGTH = LIST%LENGTH - 1
        END IF

    END SUBROUTINE DEQUEUE

    FUNCTION GET_CONT(ELEM)
        TYPE(NODE), INTENT(IN) :: ELEM
        INTEGER                :: GET_CONT

        GET_CONT = ELEM%CONT
    END FUNCTION GET_CONT

    FUNCTION SHORTEST_QUEUE_INDEX(SNAKES)
        TYPE(QUEUE), INTENT(IN), DIMENSION(:) :: SNAKES
        INTEGER                               :: SHORTEST_QUEUE_INDEX!, MINI, CURR_LEN, i

!        SHORTEST_QUEUE_INDEX = 1
!        MINI = SNAKES(1)%LENGTH

!        DO i=1, UBOUND(SNAKES, DIM=1)
!            CURR_LEN = SNAKES(i)%LENGTH
!            IF ( CURR_LEN < MINI ) THEN
!                MINI                 = CURR_LEN
!                SHORTEST_QUEUE_INDEX = i
!            END IF
!        END DO
    
        SHORTEST_QUEUE_INDEX = MINLOC(SNAKES(:)%LENGTH,1)

    END FUNCTION SHORTEST_QUEUE_INDEX

    SUBROUTINE PUT(LIST)
        TYPE(QUEUE), INTENT(IN) :: LIST
        TYPE(NODE),  POINTER    :: HELP
        INTEGER                 :: i

        HELP => LIST%HEAD
        IF(.NOT. EMPTY(LIST)) THEN
            DO i=1, LIST%LENGTH - 1
                WRITE(*,FMT='(A, 1I3)', ADVANCE='no') ' ', GET_CONT(HELP)
                HELP => HELP%NEXT
            END DO
            WRITE(*, '(A, 1I3)', ADVANCE='yes') ' ', GET_CONT(HELP)
        ELSE
            WRITE(*,*) "Die Warteschlange ist leer."
        END IF

    END SUBROUTINE PUT

    SUBROUTINE PRINT_CURRENT_STATE(SNAKES)
        TYPE(QUEUE), DIMENSION(:), INTENT(IN) :: SNAKES
        INTEGER                               :: i, NUM_SNAKES

        NUM_SNAKES = UBOUND(SNAKES, dim=1)

        DO i=1, NUM_SNAKES
            WRITE(*,FMT='(A, I2, A)') "KASSE ", i, ":"
            CALL PUT(SNAKES(i))
        END DO

    END SUBROUTINE PRINT_CURRENT_STATE

END MODULE QUEUEMOD

