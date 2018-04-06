program kredit

integer :: laufzeit, c
!! Choose REAL type with decimal precision >=p and decimal range >=r :
integer, parameter :: real_kind= selected_real_kind(p=15, r=307)
real (kind=real_kind) :: zinssatz, restschuld, zinsen, zinssumme

write(*,*) 'Geben Sie den Zinssatz in % ein: '
read(*,*) zinssatz
write(*,*) 'Geben Sie die Kredithöhe in EUR ein: '
read(*,*) restschuld

zinsen = restschuld * zinssatz / 100

do while (rate <= zinsen)
	write(*,*) 'Geben Sie die Rate in EUR ein: '
	read(*,*) rate
end do

laufzeit = 0
zinssumme = 0

do while (restschuld > 0)
		laufzeit = laufzeit + 1
		zinsen = restschuld * zinssatz / 100
		restschuld = restschuld + zinsen - rate
		zinssumme = zinssumme + zinsen
end do

if (restschuld < 0) then
	rate = rate + restschuld
	write(*,*) 'Die letzte Rate in EUR beträgt: ', rate
end if

write(*,*) 'Die Laufzeit in Jahren beträgt: ', laufzeit
write(*,*) 'Die Zinssumme in EUR beträgt: ', zinssumme
write(*,*) 'Herzlichen Glückwunsch, Ihr Kredit ist abbezahlt! :D'


end program kredit
