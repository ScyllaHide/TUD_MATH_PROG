module fib
	contains
	
	integer (kind = 8) function fib_it(n) result (erg)
		implicit none
		integer (kind = 8), intent (in) :: n
		integer (kind = 8) :: p, q, i
		
		p = 0
		q = 1
		
		if (n == 0) erg = 0;
		if (n == 1) erg = 1;
		
		do i = 2, n, 1
			erg = p + q
			p = q
			q = erg
		end do
	end function fib_it
    

end module fib


program fibonacci
	
	use fib
	implicit none

	integer (kind = 8) :: x, erg, k
	
	x = 47
	
	do
		k = fib_it(x)
		
		if (k < 0) then
			write (*,*) 'Überlauf bei ', x, k
			exit
		end if
	
		x = x + 1
	
	end do
	
	!read (*,*) x

end program fibonacci
