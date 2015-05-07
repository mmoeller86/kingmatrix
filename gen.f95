program GenKingMatrix
	implicit none
	integer, parameter :: rows = 100
	integer, parameter :: cols = 100
	real, parameter :: pi = 3.1415927
	integer ierr
	real :: time
	COMPLEX*8 m (rows, cols)
	DOUBLE PRECISION ima (rows, cols)
	DOUBLE PRECISION rea (rows, cols)
	DOUBLE PRECISION ieig (cols)
	DOUBLE PRECISION reig (cols)
	DOUBLE PRECISION fv1 (cols)
	DOUBLE PRECISION fv2 (cols)
	DOUBLE PRECISION fv3 (cols)
	DOUBLE PRECISION reiv (rows, cols)
	DOUBLE PRECISION ieiv (rows, cols)
	DOUBLE COMPLEX eig (cols)
	integer i, j
	integer count, count_rate, count_max
	COMPLEX*16 :: sum
	REAL(8) :: s
	
	call SYSTEM_CLOCK (count, count_rate, count_max)
	call srand(count)

	do i=1, 1000
		call genkmatrix (m, rows, cols)
		call getreal (m, rea, rows, cols)
		call getimag (m, ima, rows, cols)
		call cg (rows, cols, rea, ima, reig, ieig, 0, reiv, ieiv, fv1, fv2, fv3, ierr)
		call getcmplx (eig, reig, ieig, cols)
		
		sum = CMPLX(0.0, 0.0)
		do j=1, cols
			sum = sum + eig (j)
		end do
		
		s = REALPART (sum) / cols
		print *, s
		if (s .lt. (pi / 2)) then
			print *, 'Generated a king matrix...'
			stop 0
		end if
	end do
	
	print *, 'No king matrix was generated...'
	stop 1
end program GenKingMatrix

! Subroutine -- Zero --
!
! Initializes the members of the Matrix
! ``m'' with zeros
subroutine zero (m, rows, cols)
	integer rows
	integer cols
	COMPLEX*16 m (rows, cols)
	integer i, j

	do i=1,rows
		do j=1,cols
			m (i, j) = CMPLX(0.0, 0.0)
		end do
	end do
end subroutine zero

! Subroutine -- random --
!
! Initializes the Matrix m with random values
subroutine random (m, rows, cols)
	integer rows
	integer cols
	COMPLEX*16 m (rows, cols)
	integer i, j

	do i=1,rows
		do j=1,cols
			m (i, j) = CMPLX(rand () * 1000.0, rand() * 1000.0)
		end do
	end do
end subroutine random

! Subroutine genkmatrix
!
! Generates a king matrix m
subroutine genkmatrix (m, rows, cols)
	real, parameter :: pi = 3.1415927
	integer rows
	integer cols
	COMPLEX*8 m(rows, cols)
	integer i, j

	do i=1,rows
		do j=1,cols
			m (i, j) = CMPLX (rand () * pi, rand () * pi)
		end do
	end do
end subroutine genkmatrix

subroutine getreal (m, rea, rows, cols)
	integer rows
	integer cols
	COMPLEX*8 m (rows, cols)
	DOUBLE PRECISION rea (rows, cols)
	integer i, j
	
	do i=1,rows
		do j=1,cols
			rea (i, j) = REALPART (m (i, j))
		end do
	end do
end subroutine getreal

subroutine getimag (m, imag, rows, cols)
	integer rows
	integer cols
	COMPLEX*8 m (rows, cols)
	DOUBLE PRECISION imag (rows, cols)
	integer i, j
	
	do i=1,rows
		do j=1,cols
			imag (i, j) = IMAGPART (m (i, j))
		end do
	end do
end subroutine getimag

subroutine getcmplx (d, r, im, n)
	integer n
	double complex d (n)
	double precision r(n)
	double precision im(n)
	integer i
	
	do i=1,n
		d (i) = CMPLX (r (i), im(i))
	end do
end subroutine getcmplx

double precision function getsum (vec, n)
	integer n
	DOUBLE PRECISION vec (n)
	integer i

	getsum = 0.0
	do i=1,n
		getsum = getsum + vec (i)
	end do
end function getsum
