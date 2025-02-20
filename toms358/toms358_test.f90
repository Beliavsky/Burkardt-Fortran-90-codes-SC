program main
! gfortran -o t -Og -g -Wall -fcheck=all -fbacktrace -fbounds-check toms358.f90 toms358_test.f90

!*****************************************************************************80
!
!! MAIN is the main program for TOMS358_TEST.
!
!  Discussion:
!
!    TOMS358_PRB tests the TOMS358 library.
!
!    In order to make TEST01 and TEST02 flexible enough to accept
!    variable M and N, we need to allocate the associated array space
!    in the calling routine.  It is important that these arrays have
!    enough entries to be used by the subroutines, but it is not
!    actually necessary that they have the exact same size, and it
!    is not even necessary that they be declared as doubly dimensioned
!    arrays.
!
!  Modified:
!
!    10 January 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer m
  integer n
  integer p

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TOMS358_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test TOMS358 library.'
!
!  N = 2
!
  m = 5
  n = 2
  p = 0

  call csvd_test ( m, n, p )
!
!  N < M
!
  m = 6
  n = 4
  p = 0

  call csvd_test ( m, n, p )
!
!  N = M
!
  m = 5
  n = 5
  p = 0

  call csvd_test ( m, n, p )
!
!  N = M, P>0
!
  m = 5
  n = 5
  p = 2

  call csvd_test ( m, n, p )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TOMS358_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine csvd_test ( m, n, p )

!*********************************************************************72
!
!! CSVD_TEST tests CSVD.
!
!  Discussion:
!
!    CSVD is the ACM TOMS Algorithm 358, for computing the singular
!    value decomposition of a complex rectangular matrix.
!
!  Modified:
!
!    01 December 2017
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Peter Businger, Gene Golub,
!    Algorithm 358:
!    Singular Value Decomposition of a Complex Matrix,
!    Communications of the ACM,
!    Volume 12, Number 10, October 1969, pages 564-565.
!
!  Parameters:
!
!    Input, integer M, N, the number of rows and columns in the
!    matrix A.
!
!    Input, integer P.
!    0, just compute the SVD.
!    >0, compute the SVD, and solve least squares systems A*X=B,
!    where B is input as augmenting columns A(1:M,N+1:N+P).
!
!    Input, complex (CP) A(M,N+P), storage for the matrix.
!
  use mconst
  use msub90

  implicit none

  integer, intent(in) :: m, n, p

  complex (CP) :: a(m,n+p), a1(m,n+p)
  integer :: i, j, k, nu, nv, seed
  real (FP), allocatable :: s(:)
  complex (CP), allocatable :: u(:,:), v(:,:), t(:,:)
  real (FP) :: ta, tu, tv, tb
  logical :: verbose = .True.

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CSVD_TEST'
  write ( *, '(a)' ) '  Call ACM TOMS Algorithm 358 for the'
  write ( *, '(a)' ) '  singular value decomposition:'
  write ( *, '(a)' ) '    A = U S V*'
  write ( *, '(a)' ) '  of an M by N complex matrix.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix row order M =    ', m
  write ( *, '(a,i8)' ) '  Matrix column order N = ', n
  write ( *, '(a,i8)' ) '  Number of RHSs =        ', p

  seed = 123456789
!
!  Set the matrix.
!
  call cmat_uniform_01 ( m, n+p, seed, a )

  if (verbose) then
     print *,NL,'  Matrix A:',NL
     do i = 1, m
        write ( *, '(10f8.4)' ) ( a(i,j), j = 1, n )
     end do
  end if
  a1 = a

  nu = m
  nv = n

  allocate ( s(1:n) )
  allocate ( u(1:m,1:nu) )
  allocate ( v(1:nv,1:nv) )

  call csvd ( a, nu, nv, p, s, u, v )

  if (verbose) then
     print *,NL,'  Singular values:',NL
     do i = 1, n
        write ( *, '(2x,g14.6)' ) s(i)
     end do

     print *,NL,'  U:',NL
     do i = 1, m
        write ( *, '(10f8.4)' ) u(i,1:m)
     end do

     print *,NL,'  V:',NL
     do i = 1, n
        write ( *, '(10f8.4)' ) v(i,1:n)
     end do
  end if
!
!  Compute U * S * V*.
!
  do i = 1, m
    do j = 1, n
      a(i,j) = cmplx ( 0.0_FP, 0.0_FP, kind = CP )
      do k = 1, min ( m, n )
        a(i,j) = a(i,j) + u(i,k) * s(k) * conjg ( v(j,k) )
      end do
    end do
  end do
  
  ta = maxval(abs(a(:,:n)-a1(:,:n)))
  allocate (t(nu,nu))
  t = matmul(transpose(conjg(u)),u)
  Forall (i = 1 : nu) t(i,i) = t(i,i) - (1._FP,0._FP)
  tu = maxval(abs(t))
  deallocate(t)
  allocate (t(nv,nv))
  t = matmul(v,transpose(conjg(v)))
  Forall (i = 1 : nv) t(i,i) = t(i,i) - (1._FP,0._FP)
  tv = maxval(abs(t))
  deallocate(t)
  tb = 0._FP
  if (p>0) then  
     if (m/=n) then
        Print *,'No valid test'
     else
        Forall (i=1:p) a(:,i+n)=a(:,i+n)/s(:)
        tb = maxval(abs(matmul(a1(:,:n),matmul(v,a(:,n+1:)))-a1(:,n+1:)))
     end if
  end if

  if (verbose) then
     print *,NL,'  Matrix U S V*',NL,'  (should equal the original A):',NL
     do i = 1, m
        write ( *, '(10f8.4)' ) ( a(i,j), j = 1, n )
     end do
  end if
  deallocate ( s )
  deallocate ( u )
  deallocate ( v )

  Print *, ''
  Print *, ' Max Deviations: A,U^2,V^2,B:'
  Print '(5(1pe16.8))',ta,tu,tv,tb

  return
end
subroutine timestamp ( )

!*****************************************************************************80
!
!! TIMESTAMP prints the current YMDHMS date as a time stamp.
!
!  Example:
!
!    31 May 2001   9:45:54.872 AM
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    None
!
  implicit none

  character ( len = 8 ) ampm
  integer ( kind = 4 ) d
  integer ( kind = 4 ) h
  integer ( kind = 4 ) m
  integer ( kind = 4 ) mm
  character ( len = 9 ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) s
  integer ( kind = 4 ) values(8)
  integer ( kind = 4 ) y

  call date_and_time ( values = values )

  y = values(1)
  m = values(2)
  d = values(3)
  h = values(5)
  n = values(6)
  s = values(7)
  mm = values(8)

  if ( h < 12 ) then
    ampm = 'AM'
  else if ( h == 12 ) then
    if ( n == 0 .and. s == 0 ) then
      ampm = 'Noon'
    else
      ampm = 'PM'
    end if
  else
    h = h - 12
    if ( h < 12 ) then
      ampm = 'PM'
    else if ( h == 12 ) then
      if ( n == 0 .and. s == 0 ) then
        ampm = 'Midnight'
      else
        ampm = 'AM'
      end if
    end if
  end if

  write ( *, '(i2.2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end
subroutine cmat_uniform_01 ( m, n, seed, c )

!*****************************************************************************80
!
!! CMAT_UNIFORM_01 returns a unit complex pseudorandom matrix.
!
!  Discussion:
!
!    The angles should be uniformly distributed between 0 and 2 * PI,
!    the square roots of the radius uniformly distributed between 0 and 1.
!
!    This results in a uniform distribution of values in the unit circle.
!
!  Modified:
!
!    02 December 2017
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer M, N, the number of rows and columns in the matrix.
!
!    Input/output, integer SEED, the "seed" value, which should NOT be 0.
!    On output, SEED has been updated.
!
!    Output, complex (CP) C(M,N), the pseudorandom complex matrix.
!
  use mconst

  implicit none

  integer m
  integer n

  complex (CP) c(m,n)
  integer i
  integer j
  real (FP) r
  real (FP) pi
  integer seed
  real (FP) theta

  pi = atan2 ( 0.0_FP, -1.0_FP )

  do j = 1, n
    do i = 1, m

      call random_number ( r )
      r = sqrt ( r )
      call random_number ( theta )
      theta = 2.0D+00 * pi * theta

      c(i,j) = r * cmplx ( cos ( theta ), sin ( theta ), kind = CP )

    end do

  end do

  return
end
