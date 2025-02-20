program main

!*****************************************************************************80
!
!! MAIN is the main program for COMBINATION_LOCK_TEST.
!
!  Discussion:
!
!    COMBINATION_LOCK_TEST tests the COMBINATION_LOCK library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    14 August 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 4

  integer ( kind = 4 ), dimension ( m ) :: c = (/ 1, 2, 3, 4 /)
  integer ( kind = 4 ), parameter :: n = 5
  integer ( kind = 4 ) step
 
  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'COMBINATION_LOCK_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  COMBINATION_LOCK seeks the combination of a lock.'

  write ( *, '(a)' ) '  A combination lock consists of M dials,'
  write ( *, '(a)' ) '  each having N symbols.'
  write ( *, '(a)' ) '  We seek to determine the combination C.'
!
!  Report on the problem data.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a,i4)' ) '  The number of dials is M = ', m
  write ( *, '(a,i4)' ) '  The number of symbols is N = ', n
  write ( *, '(a,i8)' ) &
    '  The number of possible combinations is M^N = ', n ** m

  call i4vec_print ( m, c, '  The "secret" combination:' );

  call combination_lock ( m, n, c, step )

  if ( step == -1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  The combination was not found!'
  else
    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  The combination was found on step ', step
  end if
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'COMBINATION_LOCK_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop
end

