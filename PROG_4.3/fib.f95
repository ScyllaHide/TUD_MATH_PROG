module fib
!alles privat und nur bestimmte dinge public
	contains
	
	recursive function fib_rec(n) result(erg)
		implicit none
		integer (kind = 8), intent(in) :: n
		integer (kind = 8) :: erg
		
		if (n == 0) then
			erg = 0
		elseif (n == 1) then
			erg = 1
		else
			erg = fib_rec(n-1) + fib_rec(n-2)
		end if

	end function fib_rec
	
	integer (kind = 8) function fib_it(n) result (erg)
		implicit none
		integer (kind = 8), intent (in) :: n
		integer (kind = 8) :: p, q, i
		
		p = 0
		q = 1
		
		if (n == 0) then
			erg = 0
		elseif (n == 1) then
			erg = 1
		else
			do i = 2, n, 1
				erg = p + q
				p = q
				q = erg
			end do
		end if
	end function fib_it
    

end module


program fibonacci
	
	use fib
	implicit none
	integer (kind = 8) :: x, erg, k
		
	do
		
		write(*,*) 'Geben Sie eine natürliche Zahl ein: '
		read(*,*) x

		if (x < 0) exit
		
		write(*,*) 'Die Fibonacci-Zahl rekursiv ist: ', fib_rec(x)
		write(*,*) 'Die Fibonacci-Zahl iterativ ist: ', fib_it(x)
	
	end do

end program fibonacci


! int fib_rec(int n) {
    ! if (n == 0) return 0;
    ! if (n == 1) return 1;
	
    ! return fib_rec(n-1) + fib_rec(n-2);
! }

! int fib_tail_helper(int n, int p, int q) {
    ! if (n == 0) return p;
	
    ! return fib_tail_helper(n - 1, q, p + q);
! }

! int fib_tail(int n) {
    ! return fib_tail_helper(n, 0, 1);
! }

! int fib_it(int n) {
    ! int p = 0, q = 1, result;
    
    ! if (n == 0) return 0;
    ! if (n == 1) return 1;
    
    ! for (int i = 2; i <= n; ++i) {
        ! result = p + q;
        ! p = q;
        ! q = result;
    ! }
    
    ! return result;
! }
