subroutine i4vec_uniform_ab ( n, a, b, seed, x )

!*****************************************************************************80
!
!! I4VEC_UNIFORM_AB returns a scaled pseudorandom I4VEC.
!
!  Discussion:
!
!    An I4VEC is a vector of I4's.
!
!    The pseudorandom numbers should be scaled to be uniformly distributed
!    between A and B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 November 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the dimension of the vector.
!
!    Input, integer ( kind = 4 ) A, B, the limits of the interval.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, integer ( kind = 4 ) X(N), a vector of numbers between A and B.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) a
  integer ( kind = 4 ) b
  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) k
  real ( kind = 4 ) r
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) value
  integer ( kind = 4 ) x(n)

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'I4VEC_UNIFORM_AB - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop
  end if

  do i = 1, n

    k = seed / 127773

    seed = 16807 * ( seed - k * 127773 ) - k * 2836

    if ( seed < 0 ) then
      seed = seed + i4_huge
    end if

    r = real ( seed, kind = 4 ) * 4.656612875E-10
!
!  Scale R to lie between A-0.5 and B+0.5.
!
    r = ( 1.0E+00 - r ) * ( real ( min ( a, b ), kind = 4 ) - 0.5E+00 ) &
      +             r   * ( real ( max ( a, b ), kind = 4 ) + 0.5E+00 )
!
!  Use rounding to convert R to an integer between A and B.
!
    value = nint ( r, kind = 4 )

    value = max ( value, min ( a, b ) )
    value = min ( value, max ( a, b ) )

    x(i) = value

  end do

  return
end
subroutine monomial_value ( m, n, e, x, value )

!*****************************************************************************80
!
!! MONOMIAL_VALUE evaluates a monomial.
!
!  Discussion:
!
!    This routine evaluates a monomial of the form
!
!      product ( 1 <= i <= m ) x(i)^e(i)
!
!    where the exponents are nonnegative integers.  Note that
!    if the combination 0^0 is encountered, it should be treated
!    as 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    07 May 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!
!    Input, integer ( kind = 4 ) N, the number of points at which the
!    monomial is to be evaluated.
!
!    Input, integer ( kind = 4 ) E(M), the exponents.
!
!    Input, real ( kind = 8 ) X(M,N), the point coordinates.
!
!    Output, real ( kind = 8 ) VALUE(N), the value of the monomial.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) e(m)
  integer ( kind = 4 ) i
  real ( kind = 8 ) value(n)
  real ( kind = 8 ) x(m,n)

  value(1:n) = 1.0D+00

  do i = 1, m
    if ( 0 /= e(i) ) then
      value(1:n) = value(1:n) * x(i,1:n) ** e(i)
    end if
  end do

  return
end
subroutine r8mat_uniform_ab ( m, n, a, b, seed, r )

!*****************************************************************************80
!
!! R8MAT_UNIFORM_AB returns a scaled pseudorandom R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    A <= R(I,J) <= B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    31 May 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Second Edition,
!    Springer, 1987,
!    ISBN: 0387964673,
!    LC: QA76.9.C65.B73.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, December 1986, pages 362-376.
!
!    Pierre L'Ecuyer,
!    Random Number Generation,
!    in Handbook of Simulation,
!    edited by Jerry Banks,
!    Wiley, 1998,
!    ISBN: 0471134031,
!    LC: T57.62.H37.
!
!    Peter Lewis, Allen Goodman, James Miller,
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, Number 2, 1969, pages 136-143.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns
!    in the array.
!
!    Input, real ( kind = 8 ) A, B, the lower and upper limits.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which 
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) R(M,N), the array of pseudorandom values.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) seed
  real ( kind = 8 ) r(m,n)

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8MAT_UNIFORM_AB - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop 1
  end if

  do j = 1, n

    do i = 1, m

      k = seed / 127773

      seed = 16807 * ( seed - k * 127773 ) - k * 2836

      if ( seed < 0 ) then
        seed = seed + i4_huge
      end if

      r(i,j) = a + ( b - a ) * real ( seed, kind = 8 ) * 4.656612875D-10

    end do
  end do

  return
end
function square01_area ( )

!*****************************************************************************80
!
!! SQUARE01_AREA: area of the unit square in 2D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    16 January 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) SQUARE01_AREA, the area.
!
  implicit none

  real ( kind = 8 ) square01_area

  square01_area = 1.0D+00

  return
end
subroutine square01_monomial_integral ( e, integral )

!*****************************************************************************80
!
!! SQUARE01_MONOMIAL_INTEGRAL: integral over interior of unit square in 2D.
!
!  Discussion:
!
!    The integration region is 
!
!      0 <= X <= 1, 
!      0 <= Y <= 1.
!
!    The monomial is F(X,Y) = X^E(1) * Y^E(2).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    16 January 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) E(2), the exponents.  
!    Each exponent must be nonnegative.
!
!    Output, real ( kind = 8 ) INTEGRAL, the integral.
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 2

  integer ( kind = 4 ) e(m)
  integer ( kind = 4 ) i
  real ( kind = 8 ) integral

  if ( any ( e(1:m) < 0 ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SQUARE01_MONOMIAL_INTEGRAL - Fatal error!'
    write ( *, '(a)' ) '  All exponents must be nonnegative.'
    stop 1
  end if

  integral = 1.0D+00
  do i = 1, m
    integral = integral / real ( e(i) + 1, kind = 8 )
  end do

  return
end
subroutine square01_sample ( n, seed, x )

!*****************************************************************************80
!
!! SQUARE01_SAMPLE samples the interior of the unit square in 2D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 January 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Russell Cheng,
!    Random Variate Generation,
!    in Handbook of Simulation,
!    edited by Jerry Banks,
!    Wiley, 1998, pages 168.
!
!    Reuven Rubinstein,
!    Monte Carlo Optimization, Simulation, and Sensitivity 
!    of Queueing Networks,
!    Krieger, 1992,
!    ISBN: 0894647644,
!    LC: QA298.R79.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random 
!    number generator.
!
!    Output, real ( kind = 8 ) X(2,N), the points.
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 2
  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x(m,n)

  a = 0.0D+00
  b = 1.0D+00
  call r8mat_uniform_ab ( m, n, a, b, seed, x )

  return
end
function squaresym_area ( )

!*****************************************************************************80
!
!! SQUARESYM_AREA: area of the symmetric unit square in 2D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 February 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) SQUARESYM_AREA, the area.
!
  implicit none

  real ( kind = 8 ) squaresym_area

  squaresym_area = 4.0D+00

  return
end
subroutine squaresym_monomial_integral ( e, integral )

!*****************************************************************************80
!
!! SQUARESYM_MONOMIAL_INTEGRAL: integral over interior of symmetric unit square.
!
!  Discussion:
!
!    The integration region is 
!
!      -1 <= X <= 1, 
!      -1 <= Y <= 1.
!
!    The monomial is F(X,Y) = X^E(1) * Y^E(2).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 February 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) E(2), the exponents.  
!    Each exponent must be nonnegative.
!
!    Output, real ( kind = 8 ) INTEGRAL, the integral.
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 2

  integer ( kind = 4 ) e(m)
  integer ( kind = 4 ) i
  real ( kind = 8 ) integral

  if ( any ( e(1:m) < 0 ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SQUARESYM_MONOMIAL_INTEGRAL - Fatal error!'
    write ( *, '(a)' ) '  All exponents must be nonnegative.'
    stop 1
  end if

  if ( mod ( e(1), 2 ) == 1 .or. mod ( e(2), 2 ) == 1 ) then
    integral = 0.0D+00
  else
    integral = 4.0D+00 &
      / real ( e(1) + 1, kind = 8 ) &
      / real ( e(2) + 1, kind = 8 )
  end if

  return
end
subroutine squaresym_sample ( n, seed, x )

!*****************************************************************************80
!
!! SQUARESYM_SAMPLE samples the interior of the symmetric unit square in 2D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 February 2018
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Russell Cheng,
!    Random Variate Generation,
!    in Handbook of Simulation,
!    edited by Jerry Banks,
!    Wiley, 1998, pages 168.
!
!    Reuven Rubinstein,
!    Monte Carlo Optimization, Simulation, and Sensitivity 
!    of Queueing Networks,
!    Krieger, 1992,
!    ISBN: 0894647644,
!    LC: QA298.R79.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random 
!    number generator.
!
!    Output, real ( kind = 8 ) X(2,N), the points.
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 2
  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x(m,n)

  a = -1.0D+00
  b =  1.0D+00
  call r8mat_uniform_ab ( m, n, a, b, seed, x )

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
