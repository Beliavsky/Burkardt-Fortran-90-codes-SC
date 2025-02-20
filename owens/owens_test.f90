program main

!*****************************************************************************80
!
!! MAIN is the main program for OWENS_TEST.
!
!  Discussion:
!
!    OWENS_TEST tests the OWENS library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 July 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'OWENS_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the OWENS library.'

  call bivnor_test ( )
  call t_test ( )
  call znorm1_test ( )
  call znorm2_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'OWENS_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end




