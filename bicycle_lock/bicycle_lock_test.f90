program main

!*****************************************************************************80
!
!! MAIN is the main program for BICYCLE_LOCK_TEST.
!
!  Discussion:
!
!    BICYCLE_LOCK_TEST tests the BICYCLE_LOCK library.
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

  integer ( kind = 4 ) c
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n = 10
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) step

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BICYCLE_LOCK_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  BICYCLE_LOCK seeks the combination of a bicycle lock.'
  write ( *, '(a)' ) '  A bicycle combination lock consists of 3 dials,'
  write ( *, '(a)' ) '  each having 10 symbols, 0 through 9.'
  write ( *, '(a)' ) '  We seek to determine the combination C.'
!
!  Report on the problem data.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a,i4)' ) '  The number of dials is M = ', m
  write ( *, '(a,i4)' ) '  The number of symbols is N = ', n
  write ( *, '(a,i8)' ) &
    '  The number of possible combinations is M^N = ', n ** m

  call get_seed ( seed )
  c = i4_uniform_ab ( 0, 999, seed )

  write ( *, '(a,i3)' ) '  The "secret" combination is ', c

  call bicycle_lock ( c, step )

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
  write ( *, '(a)' ) 'BICYCLE_LOCK_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end

