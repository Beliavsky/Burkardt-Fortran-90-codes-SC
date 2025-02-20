program main

!*****************************************************************************80
!
!! MAIN is the main program for FELIPPA_TEST.
!
!  Discussion:
!
!    FELIPPA_TEST tests the FELIPPA library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 July 2009
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) degree_max

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'FELIPPA_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the FELIPPA library.'

  degree_max = 4

  call hexa_unit_monomial_test ( degree_max )
  call line_unit_monomial_test ( degree_max )
  call pyra_unit_monomial_test ( degree_max )
  call quad_unit_monomial_test ( degree_max )
  call tetr_unit_monomial_test ( degree_max )
  call trig_unit_monomial_test ( degree_max )
  call wedg_unit_monomial_test ( degree_max )

  degree_max = 6
  call hexa_unit_quad_test ( degree_max )

  degree_max = 10
  call line_unit_quad_test ( degree_max )

  degree_max = 5
  call pyra_unit_quad_test ( degree_max )

  degree_max = 10
  call quad_unit_quad_test ( degree_max )

  degree_max = 4
  call tetr_unit_quad_test ( degree_max )

  degree_max = 7
  call trig_unit_quad_test ( degree_max )

  degree_max = 8
  call wedg_unit_quad_test ( degree_max )

  call wedg_unit_write_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'FELIPPA_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end






