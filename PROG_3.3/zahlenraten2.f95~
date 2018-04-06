program zahlenraten

!www.github.com/LinFelix/sonderuebung
!intrinsischen Funktionen. siehe OPAL

integer :: l, r, vorschlag, i
character :: vergleich, proof, new_game

new_game = "j"

do while (new_game == "j")

	l = 1
	r = 0
	i = 0

	!Eingabe der linken und rechten Grenze!
	do while (l > r)
		write(*,*) 'Gib die linke und die rechte Intervallgrenze nacheinander ein: '
		read(*,*) l, r
	end do

	do while ((r-l) >= 1)
	
		vergleich = "none"
		i = i + 1

		vorschlag = (l + r) / 2

		write(*,*) 'Ist deine Zahl ', vorschlag
	
		do while (vergleich /= "<" .and. vergleich /= "=" .and. vergleich /= ">")
			read(*,*) vergleich
		end do

		if (vergleich == "<") then
			r = vorschlag - 1
		elseif (vergleich == "=") then
			r = vorschlag
			l = r
		elseif (vergleich == ">") then
			l = vorschlag + 1

		else
			write(*,*) 'Hoppla, ein Fehler.'
		end if

	end do

	if (r >= l) then

		write(*,*) 'Deine Zahl ist ', r
	
		write(*,*) 'Ist die Zahl richig?  (j/n)'
	
		proof = "unklar"

		do while (proof /= "j" .and. proof /= "n")
			read(*,*), proof
		
			if (proof == "j") then
				write(*,*) '================================='
				write(*,*) 'Deine Zahl ist ', r
				write(*,*) 'Anzahl der Rateversuche: ', i
			elseif (proof == "n") then
				write(*,*) 'Hoppla, ein Fehler.'
			end if

		end do
	
		else
			write(*,*) 'Hoppla, ein Fehler.'

	end if

	write(*,*) 'Soll ein neues Spiel gestartet werden?  (j/n)'
	read(*,*) new_game

end do

end program zahlenraten
