program main

!*****************************************************************************80
!
!! MAIN is the main program for LINE_MONTE_CARLO_TEST.
!
!  Discussion:
!
!    LINE_MONTE_CARLO_TEST tests the LINE_MONTE_CARLO library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 June 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LINE_MONTE_CARLO_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the LINE_MONTE_CARLO library.'

  call line01_sample_random_test ( )
  call line01_sample_ergodic_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LINE_MONTE_CARLO_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop
end
subroutine line01_sample_random_test ( )

!*****************************************************************************80
!
!! LINE01_SAMPLE_RANDOM_TEST uses Monte Carlo sampling to estimate integrals in 1D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 June 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) e
  real ( kind = 8 ) error
  real ( kind = 8 ) exact
  integer ( kind = 4 ) j
  real ( kind = 8 ) line01_length
  integer ( kind = 4 ) n
  real ( kind = 8 ) result(7)
  integer ( kind = 4 ) seed
  real ( kind = 8 ), allocatable :: value(:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LINE01_SAMPLE_RANDOM_TEST'
  write ( *, '(a)' ) '  LINE01_SAMPLE_RANDOM randomly samples the unit line segment.'
  write ( *, '(a)' ) '  Use it to estimate integrals.'

  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '         N' // & 
    '        1' // & 
    '               X' // & 
    '              X^2' // &
    '             X^3' // & 
    '             X^4' // & 
    '             X^5' // &
    '           X^6'
  write ( *, '(a)' ) ' '

  n = 1

  do while ( n <= 65536 )

    allocate ( value(1:n) )
    allocate ( x(1:n) )

    call line01_sample_random ( n, seed, x )

    do j = 1, 7

      e = j - 1

      call monomial_value_1d ( n, e, x, value )

      result(j) = line01_length ( ) * sum ( value(1:n) ) &
        / real ( n, kind = 8 )

    end do

    write ( *, '(2x,i8,7(2x,g14.6))' ) n, result(1:7)

    deallocate ( value )
    deallocate ( x )

    n = 2 * n

  end do

  write ( *, '(a)' ) ' '

  do j = 1, 7

    e = j - 1

    call line01_monomial_integral ( e, result(j) )

  end do

  write ( *, '(2x,a8,7(2x,g14.6))' ) '   Exact', result(1:7)

  return
end
subroutine line01_sample_ergodic_test ( )

!*****************************************************************************80
!
!! LINE01_SAMPLE_ERGODIC_TEST uses Monte Carlo sampling to estimate integrals in 1D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 June 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) e
  real ( kind = 8 ) error
  real ( kind = 8 ) exact
  integer ( kind = 4 ) j
  real ( kind = 8 ) line01_length
  integer ( kind = 4 ) n
  real ( kind = 8 ) result(7)
  real ( kind = 8 ) shift
  real ( kind = 8 ), allocatable :: value(:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LINE01_SAMPLE_ERGODIC_TEST'
  write ( *, '(a)' ) '  LINE01_SAMPLE_ERGODIC ergodically samples the unit line segment.'
  write ( *, '(a)' ) '  Use it to estimate integrals.'

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '         N' // & 
    '        1' // & 
    '               X' // & 
    '              X^2' // &
    '             X^3' // & 
    '             X^4' // & 
    '             X^5' // &
    '           X^6'
  write ( *, '(a)' ) ' '

  n = 1

  do while ( n <= 65536 )

    allocate ( value(1:n) )
    allocate ( x(1:n) )

    shift = 0.0D+00
    call line01_sample_ergodic ( n, shift, x )

    do j = 1, 7

      e = j - 1

      call monomial_value_1d ( n, e, x, value )

      result(j) = line01_length ( ) * sum ( value(1:n) ) &
        / real ( n, kind = 8 )

    end do

    write ( *, '(2x,i8,7(2x,g14.6))' ) n, result(1:7)

    deallocate ( value )
    deallocate ( x )

    n = 2 * n

  end do

  write ( *, '(a)' ) ' '

  do j = 1, 7

    e = j - 1

    call line01_monomial_integral ( e, result(j) )

  end do

  write ( *, '(2x,a8,7(2x,g14.6))' ) '   Exact', result(1:7)

  return
end
