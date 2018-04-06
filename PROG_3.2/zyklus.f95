program zyklus

integer :: n, i, summe, x, mmax, mmin
!! Choose REAL type with decimal precision >=p and decimal range >=r :
integer, parameter :: real_kind= selected_real_kind(p=15, r=307)
real (kind=real_kind) :: mitte

n=0

! Die Abbruchbedingung sollte einmal am Ende stehen, damit nicht vor der ersten Eingabe schon geprüft wird.!

do while (n < 1)
	write(*,*) 'Eingabe der Anzahl an Zahlen: '
	read(*,*) n
end do

i = 0
summe = 0

write(*,*) 'Geben Sie eine Zahl ein: '
read(*,*) x
	

summe = summe + x
i = i + 1
mitte = (summe * 1.d0) / i
	
mmax = x
mmin = x
	
write(*,*) 'aktueller Mittelwert: ', mitte
write(*,*) 'aktuelle Summe: ', summe
write(*,*) 'aktuelle Anzahl an Zahlen: ', i
write(*,*) 'aktuelles Maximum: ', mmax
write(*,*) 'aktuelles Minimum: ', mmin

do while (i /= n)
	write(*,*) 'Geben Sie eine Zahl ein: '
	read(*,*) x
	
	summe = summe + x
	i = i + 1
	mitte = (summe*1.d0) / i
	
	if (x > mmax) then
		mmax = x
	end if 

	if (x < mmin) then
		mmin = x
	end if

	write(*,*) 'aktueller Mittelwert: ', mitte
	write(*,*) 'aktuelle Summe: ', summe
	write(*,*) 'aktuelle Anzahl an Zahlen: ', i
	write(*,*) 'aktuelles Maximum: ', mmax
	write(*,*) 'aktuelles Minimum: ', mmin
	
end do

write(*,*) '======================'
write(*,*) 'Mittelwert: ', mitte
write(*,*) 'Summe: ', summe
write(*,*) 'Anzahl an Zahlen: ', i
write(*,*) 'Maximum: ', mmax
write(*,*) 'Minimum: ', mmin


end program zyklus
