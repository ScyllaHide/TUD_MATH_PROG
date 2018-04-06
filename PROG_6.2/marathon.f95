PROGRAM MARATHON

USE MARATHONMOD
IMPLICIT NONE

TYPE(ERGEBNIS), DIMENSION(:), ALLOCATABLE :: table
INTEGER                       :: n, ios,i, x
CHARACTER(LEN=30)              :: datei


WRITE(*,*) "Wieviele Läufer sind in der Datei vorhanden?"
READ(*,*) x

WRITE(*,*) "Wie soll die Ergebnisdatei heißen?"
READ(*,*) datei

ALLOCATE(table(x))

CALL LESELISTE(table,n)

CALL QUICKSORT(table,1, UBOUND(table, dim=1))
!table = table(1:n-1)

OPEN(UNIT=44, FILE = datei, IOSTAT = ios, ACTION = 'WRITE', STATUS = 'NEW')
WRITE(44,*) "Sortierte Liste:"
WRITE(44,*) "================================================="

DO i=1, 30
    WRITE(44,FMT='(I3 A2 I3 A2 I3 A2 F0.3 3X A)') &
        i, ". ", table(i)%M_TIME%H, ":", table(i)%M_TIME%M, ":", REAL(INT(table(i)%M_TIME%S*10))/10, table(i)%RUNNER
END DO

! do i = 1, 3, 1
    ! write(*,*) i, ". Platz ", trim(table(i)%runner), " mit ",  &
    ! & table(p)%m_time%h, "Stunden, ",  &
    ! & table(p)%m_time%m, "Minuten und ", &
    ! & table(p)%m_time%s, "Sekunden."
  ! end do

read(*,*)
END PROGRAM MARATHON