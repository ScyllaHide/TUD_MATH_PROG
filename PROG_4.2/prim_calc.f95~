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
		
		if (MOD(lhaz,2) == 0) then
			lhaz = lhaz + 1
		endif
	
	end function
	
end module


program primpro

	use primmirp

	implicit none

	integer :: li, re, zahl, k
	logical :: proof1, proof2
	
	do k=2,9,1
		
		zahl = 10 ** k + 1
		proof1 = .false.
		
		do 	
				if ( prim(zahl) ) then
		
					if (prim(umkehrzahl(zahl)) ) then
						write(*,*) zahl, 'ist Mirpzahl.';
						proof1 = .true.
					end if
				end if
				
				if ( proof1 .eqv. .true. ) exit
				zahl = zahl +2
		end do
		
		zahl = 10 ** k + 1
		proof1 = .false.

		do 	
				if ( prim(zahl) ) then
		
					if (.not. (prim(umkehrzahl(zahl))) ) then
						write(*,*) zahl, 'ist Primzahl.';
						proof1 = .true.
					end if
				end if
				
				if ( proof1 .eqv. .true. ) exit
				zahl = zahl +2
		end do

		write(*,*) '----------------------------'

	end do
	
	!read(*,*)
	
end program primpro
