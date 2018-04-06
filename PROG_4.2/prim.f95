module primmirp
	implicit none
	
	contains
	
	logical function prim(n) result (proofprim)
		integer, intent(in) :: n
		integer :: i
		!logical :: proofprim
		
		proofprim = .true.
		
		do i=3 , floor(sqrt(real(n))) , 2
					
			if ( mod(n,i) == 0 ) then
				proofprim = .false.
				exit
			end if	
			
		end do
			
	end function
	
	integer function umkehrzahl(zahl) result (lhaz)
		integer, intent(in) :: zahl
		integer :: rest, zahlalt
		
		rest = 0
		lhaz = 0  
		zahlalt = zahl

		do while (zahlalt > 0)
	  
		  rest = MOD(zahlalt,10)
		  lhaz = lhaz * 10 + rest
		  zahlalt = zahlalt/10
	  
		end do
		
		!if (MOD(lhaz,2) == 0) then
		!	lhaz = lhaz + 1
		!endif
	
	end function
	
end module


program primpro

	use primmirp

	implicit none

	integer :: li, re, zahl, i
	
	do 
		write(*,*) 'Geben Sie die linke und die rechte Intervallgrenze nacheinander ein: '
		read(*,*) li, re
		if (li >= 2 .and. re <= 10**9 .and. li < re) exit
	end do

	zahl = li
	
	if (li <= 2 .and. re > 2) then	
		write(*,*) '2 ist Mirpzahl'
		zahl = 3;
	end if

	if (MOD(zahl, 2) == 0) then
		zahl = zahl + 1
	endif

	do i=zahl,re,2

		
		
		if ( prim(i) ) then
		
			if (prim(umkehrzahl(i)) ) then
				write(*,*) i, 'ist Mirpzahl.'
			else 
				write(*,*) i, 'ist Primzahl.'
			end if
		
		end if
		
	end do
	
	!read(*,*)
	
end program primpro
