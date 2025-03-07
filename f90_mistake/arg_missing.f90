program main

!*****************************************************************************80
!
!! MAIN is the main program for ARG_MISSING.
!
!  Discussion:
!
!    Real numerical constants in FORTRAN must have a precision.
!
!    A constant such as "4.0" will by default have single precision, sometimes
!    thought of as "8 digit precision".
!
!    A constant such as "1.2E+1" will explicitly request single precision, by
!    virtue of the use of the "E" exponent marker.  Note that this has the
!    often unexpected effect of TRUNCATING the numeric data to 8 digits,
!    before anything else is done with it.
!
!    A constant such as "3.4D+2" will explicitly request double precision,
!    by virtue of the occurrence of the "D" exponent marker, sometimes referred 
!    to as "16 digit precision".  Thus, as many as 16 digits may be usefully
!    specified in such a constant.
!
!    The point is that, while either format will allow you to list as many digits
!    as you like, the single precision format will essentially ignore all but
!    the first 8 significant digits.  This is true regardless of the type of
!    any arithmetic expression in which the constant occurs, or of the type of
!    any variable into which the constant is to be stored.
!
!    Thus, even if X and Y are double precision variables, the quantity
!      X = sin ( 3.14159265358979323846264338 * Y )
!    is computed as though the formula read
!      X = sin ( 3.14159265                   * Y ) ( 8 digits of PI)
!    and this is also true for the version
!      X = sin ( 3.14159265358979323846264338E+00 * Y )
!    while
!      X = sin ( 3.14159265358979323846264338D+00 * Y )
!    will be computed as though it read
!      X = sin ( 3.141592653589793 * Y ) (16 digits of PI )
!
!    This is a "feature" of FORTRAN that has existed since the beginning,
!    and one which occurs in no other language that I am aware of.
!
!    OK, now you've been warned.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    04 December 2016
!
!  Author:
!
!    John Burkardt.
!
  implicit none

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ARG_MISSING:'
  write ( *, '(a)' ) '  FORTRAN90 version.'
  write ( *, '(a)' ) '  What happens when you call a subroutine with'
  write ( *, '(a)' ) '  too few arguments?'

  call test01 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ARG_MISSING:'
  write ( *, '(a)' ) '  Normal end of execution.'

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 calls a subroutine, but omits an argument.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    09 March 2018
!
!  Author:
!
!    John Burkardt.
!
  integer ( kind = 4 ) a
  integer ( kind = 4 ) b
  integer ( kind = 4 ) c

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ARG_MISSING'
  write ( *, '(a)' ) '  "Traditionally", FORTRAN has not checked that'
  write ( *, '(a)' ) '  the number of arguments passed to a subroutine'
  write ( *, '(a)' ) '  corresponds to the number of arguments in the'
  write ( *, '(a)' ) '  declaration of the subroutine.'
  write ( *, '(a)' ) '  Especially with long argument lists, it is easy'
  write ( *, '(a)' ) '  to inadvertently omit an argument.'
  write ( *, '(a)' ) '  The resulting calculation, if it proceeds silently'
  write ( *, '(a)' ) '  is almost certainly erroneous.'

  a = 1
  b = 2
  c = 3

  call sub ( a, b )

  write ( *, * ) ''
  write ( *, * ) '  ', a, ' = ', b, ' + ', c

  a = 1
  b = 2
  c = 3

  call sub ( a, b, c )

  write ( *, * ) ''
  write ( *, * ) '  ', a, ' = ', b, ' + ', c

  return
end
subroutine sub ( a, b, c )

!*****************************************************************************80
!
!! SUB adds two integers.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    09 March 2018
!
!  Author:
!
!    John Burkardt.
!
!  Parameters:
!
!    Output, integer A, the sum of B and C.
!
!    Input, integer B, C, the values to add.
!
  integer ( kind = 4 ) a
  integer ( kind = 4 ) b
  integer ( kind = 4 ) c

  a = b + c

  return
end

