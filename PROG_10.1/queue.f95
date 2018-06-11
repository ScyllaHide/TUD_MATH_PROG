PROGRAM QUEUEPROG

    USE QUEUEMOD
    IMPLICIT NONE

    INTEGER                                :: KASSEN, CONT, i, j, hour, minute, sec
    REAL                                   :: PROB, DECIDE
    TYPE(QUEUE), DIMENSION(:), ALLOCATABLE :: SNAKES

    DO
        WRITE(*,*) "Geben Sie die Anzahl der geöffneten Kassen ein: "
        READ (*,*) KASSEN

        WRITE(*,*) "Geben Sie die Ankunftswahrscheinlichkeit eines Kunden in einer Sekunde ein (0<p<1): "
        READ (*,*) PROB

        IF (KASSEN > 0 .AND. PROB > 0 .AND. PROB < 1) EXIT
    END DO

    ALLOCATE(SNAKES(KASSEN))    ! allocates an array of queues (as much as registers are open)
    DO i = 1, KASSEN            ! initalizes all registers of "SNAKES"
        CALL INIT(SNAKES(i))
    END DO

    CALL RANDOM_SEED()   ! ensures different random number with RANDOM_NUMBER

    DO hour = 8, 21
        DO minute = 1, 59
            DO sec = 1, 59

                ! FIRST STEP: Perhaps new element
                CALL RANDOM_NUMBER(DECIDE)

                IF(DECIDE < PROB) THEN
                    CONT = RANDOM_INT(10,300)

                    CALL ENQUEUE(SNAKES(SHORTEST_QUEUE_INDEX(SNAKES)), CONT)

                    WRITE(*,*) "-----------------------------------"
                    WRITE(*,"(A,1X,1I2,':',1I2,':',1I2)") "Ein neuer Kunde ist angekommen. Es ist", hour, minute, sec
                    CALL PRINT_CURRENT_STATE(SNAKES)
                END IF

                ! SECOND STEP: somebody leaves the queue
                DO j=1, KASSEN
                    IF(.NOT. EMPTY(SNAKES(j))) THEN
                        SNAKES(j)%HEAD%CONT = SNAKES(j)%HEAD%CONT - 1
                        IF(SNAKES(j)%HEAD%CONT == 0) THEN
                            CALL DEQUEUE(SNAKES(j))

                            WRITE(*,*) "-----------------------------------"
                            WRITE(*,"(A,1X,1I2,':',1I2,':',1I2)") "Ein Kunde wurde bedient. Es ist", hour, minute, sec
                            CALL PRINT_CURRENT_STATE(SNAKES)
                        END IF
                    END IF
                END DO


            END DO
        END DO
    END DO
    
    DO i=1, KASSEN
        DO WHILE(.NOT. EMPTY(SNAKES(i)))
            CALL DEQUEUE(SNAKES(i))
        END DO
    END DO
    DEALLOCATE(SNAKES)

    CONTAINS

    FUNCTION RANDOM_INT(BOT, TOP)
        INTEGER :: BOT, TOP, RANDOM_INT
        REAL    :: H

        CALL RANDOM_NUMBER(H)
        RANDOM_INT = BOT + INT(H * (TOP - BOT))
    END FUNCTION RANDOM_INT


END PROGRAM



