function p00_fun ( prob, x )

!*****************************************************************************80
!
!! P00_FUN evaluates the function for any problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 February 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) PROB, the number of the desired test problem.
!
!    Input, real ( kind = 8 ) X, the point at which the function
!    is to be evaluated.
!
!    Output, real ( kind = 8 ) P00_FUN, the value of the function at X.
!
  implicit none

  integer ( kind = 4 ) prob
  real ( kind = 8 ) p00_fun
  real ( kind = 8 ) p01_fun
  real ( kind = 8 ) p02_fun
  real ( kind = 8 ) p03_fun
  real ( kind = 8 ) p04_fun
  real ( kind = 8 ) p05_fun
  real ( kind = 8 ) value
  real ( kind = 8 ) x

  if ( prob == 1 ) then
    value = p01_fun ( x )
  else if ( prob == 2 ) then
    value = p02_fun ( x )
  else if ( prob == 3 ) then
    value = p03_fun ( x )
  else if ( prob == 4 ) then
    value = p04_fun ( x )
  else if ( prob == 5 ) then
    value = p05_fun ( x )
  else
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'P00_FUN - Fatal error!'
    write ( *, '(a,i6)' ) '  Illegal problem number = ', prob
    value = 0.0D+00
    stop
  end if

  p00_fun = value

  return
end
subroutine p00_lim ( prob, a, b )

!*****************************************************************************80
!
!! P00_LIM returns the limits of the approximation interval for any problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 February 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) PROB, the number of the desired test problem.
!
!    Output, real ( kind = 8 ) A, B, the lower and upper limits of
!    the approximation interval.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) prob

  if ( prob == 1 ) then
    call p01_lim ( a, b )
  else if ( prob == 2 ) then
    call p02_lim ( a, b )
  else if ( prob == 3 ) then
    call p03_lim ( a, b )
  else if ( prob == 4 ) then
    call p04_lim ( a, b )
  else if ( prob == 5 ) then
    call p05_lim ( a, b )
  else
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'P00_LIM - Fatal error!'
    write ( *, '(a,i6)' ) '  Illegal problem number = ', prob
    stop
  end if

  return
end
subroutine p00_prob_num ( prob_num )

!*****************************************************************************80
!
!! P00_PROB_NUM returns the number of problems.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 February 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) PROB_NUM, the number of problems.
!
  implicit none

  integer ( kind = 4 ) prob_num

  prob_num = 5

  return
end
subroutine p00_story ( prob )

!*****************************************************************************80
!
!! P00_STORY prints the "story" for any problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 February 2012
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

  integer ( kind = 4 ) prob

  if ( prob == 1 ) then
    call p01_story ( )
  else if ( prob == 2 ) then
    call p02_story ( )
  else if ( prob == 3 ) then
    call p03_story ( )
  else if ( prob == 4 ) then
    call p04_story ( )
  else if ( prob == 5 ) then
    call p05_story ( )
  else
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'P00_STORY - Fatal error!'
    write ( *, '(a)' ) '  Unexpected input value of PROB.'
    stop
  end if

  return
end
subroutine p00_title ( prob, title )

!*****************************************************************************80
!
!! P00_TITLE returns the title of any problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 February 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) PROB, the number of the desired test problem.
!
!    Output, character ( len = * ) TITLE, the title of the problem.
!
  implicit none

  integer ( kind = 4 ) prob
  character ( len = * ) title

  if ( prob == 1 ) then
    call p01_title ( title )
  else if ( prob == 2 ) then
    call p02_title ( title )
  else if ( prob == 3 ) then
    call p03_title ( title )
  else if ( prob == 4 ) then
    call p04_title ( title )
  else if ( prob == 5 ) then
    call p05_title ( title )
  else
    title = ' '
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'P00_TITLE - Fatal error!'
    write ( *, '(a,i6)' ) '  Illegal problem number = ', prob
    stop
  end if

  return
end
function p01_fun ( x )

!*****************************************************************************80
!
!! P01_FUN evaluates the function for problem 1.
!
!  Discussion:
!
!    This is a famous example, due to Runge.  If equally spaced
!    abscissas are used, the sequence of interpolating polynomials Pn(X)
!    diverges, in the sense that the max norm of the difference
!    between Pn(X) and F(X) becomes arbitrarily large as N increases.
!
!  Dimension:
!
!    N = 1
!
!  Interval:
!
!    -5 <= X <= 5
!
!  Function:
!
!    1 / ( X * X + 1 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 June 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the point at which the function
!    is to be evaluated.
!
!    Output, real ( kind = 8 ) P01_FUN, the value of the function at X.
!
  implicit none

  real ( kind = 8 ) p01_fun
  real ( kind = 8 ) x

  p01_fun = 1.0D+00 / ( x * x + 1.0D+00 )

  return
end
subroutine p01_lim ( a, b )

!*****************************************************************************80
!
!! P01_LIM returns the limits of the approximation interval for problem 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 June 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) A, B, the limits of the interval
!    of approximation.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b

  a = -5.0D+00
  b =  5.0D+00

  return
end
subroutine p01_story ( )

!*****************************************************************************80
!
!! P01_STORY prints the "story" for problem 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 June 2011
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

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '  This is a famous example, due to Runge.  If equally spaced'
  write ( *, '(a)' ) &
    '  abscissas are used, the sequence of interpolating polynomials Pn(X)'
  write ( *, '(a)' ) &
    '  diverges, in the sense that the max norm of the difference'
  write ( *, '(a)' ) &
    '  between Pn(X) and F(X) becomes arbitrarily large as N increases.'

  return
end
subroutine p01_title ( title )

!*****************************************************************************80
!
!! P01_TITLE returns the title of problem 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 June 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, character ( len = * ) TITLE, the title of the problem.
!
  implicit none

  character ( len = * ) title

  title = 'Runge example, f(x) = 1 / ( x * x + 1 ), [-5,5]'

  return
end
function p02_fun ( x )

!*****************************************************************************80
!
!! P02_FUN evaluates the function for problem 2.
!
!  Discussion:
!
!    This example is due to Bernstein.  If equally spaced
!    abscissas are used, the sequence of interpolating polynomials Pn(X)
!    only converges to F(X) at -1, 0, and 1.
!
!  Dimension:
!
!    N = 1
!
!  Interval:
!
!    -1 <= X <= 1
!
!  Function:
!
!    ABS ( X )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 June 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the point at which the function
!    is to be evaluated.
!
!    Output, real ( kind = 8 ) P02_FUN, the value of the function at X.
!
  implicit none

  real ( kind = 8 ) p02_fun
  real ( kind = 8 ) x

  p02_fun = abs ( x )

  return
end
subroutine p02_lim ( a, b )

!*****************************************************************************80
!
!! P02_LIM returns the limits of the approximation interval for problem 2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 June 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) A, B, the limits of the interval
!    of approximation.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b

  a = -1.0D+00
  b =  1.0D+00

  return
end
subroutine p02_story ( )

!*****************************************************************************80
!
!! P02_STORY prints the "story" for problem 2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 June 2011
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

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '  This example is due to Bernstein.'
  write ( *, '(a)' ) &
    '  If equally spaced abscissas are used, the sequence of interpolating'
  write ( *, '(a)' ) &
    '  polynomials Pn(X) only converges to F(X) at -1, 0, and 1.'

  return
end
subroutine p02_title ( title )

!*****************************************************************************80
!
!! P02_TITLE returns the title of problem 2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 June 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, character ( len = * ) TITLE, the title of the problem.
!
  implicit none

  character ( len = * ) title

  title = 'Bernstein example, f(x) = abs ( x ), [-1,1]'

  return
end
function p03_fun ( x )

!*****************************************************************************80
!
!! P03_FUN evaluates the function for problem 3.
!
!  Discussion:
!
!    This is a step function with a jump from 0 to 1 at 0.
!
!  Dimension:
!
!    N = 1
!
!  Interval:
!
!    -1 <= X <= 1
!
!  Function:
!
!    F(X) = 0 if X < 0
!           1 if 0 < X.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 June 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the point at which the function
!    is to be evaluated.
!
!    Output, real ( kind = 8 ) P03_FUN, the value of the function at X.
!
  implicit none

  real ( kind = 8 ) p03_fun
  real ( kind = 8 ) value
  real ( kind = 8 ) x

  if ( x < 0.0D+00 ) then
    value = 0.0D+00
  else
    value = 1.0D+00
  end if

  p03_fun = value

  return
end
subroutine p03_lim ( a, b )

!*****************************************************************************80
!
!! P03_LIM returns the limits of the approximation interval for problem 3.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 June 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) A, B, the limits of the interval
!    of approximation.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b

  a = -1.0D+00
  b =  1.0D+00

  return
end
subroutine p03_story ( )

!*****************************************************************************80
!
!! P03_STORY prints the "story" for problem 3.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 June 2011
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

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '  The step function is discontinuous.'
  write ( *, '(a)' ) &
    '  Attempts to approximate this function by high degree polynomials'
  write ( *, '(a)' ) &
    '  will rapidly diverge.'

  return
end
subroutine p03_title ( title )

!*****************************************************************************80
!
!! P03_TITLE returns the title of problem 3.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 June 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, character ( len = * ) TITLE, the title of the problem.
!
  implicit none

  character ( len = * ) title

  title = 'Step function, f jumps from 0 to 1 at 0.'

  return
end
function p04_fun ( x )

!*****************************************************************************80
!
!! P04_FUN evaluates the function for problem 4.
!
!  Discussion:
!
!    This function is highly oscillatory near X = 0.
!
!  Dimension:
!
!    N = 1
!
!  Interval:
!
!    0 <= X <= 1
!
!  Function:
!
!    F(X) = sqrt ( x * ( 1 - x ) ) * sin ( 2.1 * pi / ( x + 0.05 ) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 June 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the point at which the function
!    is to be evaluated.
!
!    Output, real ( kind = 8 ) P04_FUN, the value of the function at X.
!
  implicit none

  real ( kind = 8 ) p04_fun
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) value
  real ( kind = 8 ) x

  value = sqrt ( x * ( 1.0D+00 - x ) ) * sin ( 2.1D+00 * pi / ( x + 0.05D+00 ) )

  p04_fun = value

  return
end
subroutine p04_lim ( a, b )

!*****************************************************************************80
!
!! P04_LIM returns the limits of the approximation interval for problem 4.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 June 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) A, B, the limits of the interval
!    of approximation.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b

  a = 0.0D+00
  b = 1.0D+00

  return
end
subroutine p04_story ( )

!*****************************************************************************80
!
!! P04_STORY prints the "story" for problem 4.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 June 2011
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

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '  The Doppler function is continuous, but highly oscillatory'
  write ( *, '(a)' ) &
    '  near the value X = 0.'

  return
end
subroutine p04_title ( title )

!*****************************************************************************80
!
!! P04_TITLE returns the title of problem 4.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 June 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, character ( len = * ) TITLE, the title of the problem.
!
  implicit none

  character ( len = * ) title

  title = 'Doppler function, highly oscillatory near X = 0.'

  return
end
function p05_fun ( x )

!*****************************************************************************80
!
!! P05_FUN evaluates the function for problem 5.
!
!  Discussion:
!
!    This example is difficult to interpolate because it has a piecewise
!    definition, and the character of the function changes dramatically
!    from piece to piece.
!
!  Dimension:
!
!    N = 1
!
!  Interval:
!
!    0 <= X <= 10
!
!  Function:
!
!    max ( sin(x) + sin(x^2), 1 - abs(x-5)/5 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 February 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the point at which the function
!    is to be evaluated.
!
!    Output, real ( kind = 8 ) P05_FUN, the value of the function at X.
!
  implicit none

  real ( kind = 8 ) p05_fun
  real ( kind = 8 ) x

  p05_fun = max ( sin ( x ) + sin ( x * x ), &
                  1.0D+00 - abs ( x - 5.0D+00 ) / 5.0D+00 )

  return
end
subroutine p05_lim ( a, b )

!*****************************************************************************80
!
!! P05_LIM returns the limits of the approximation interval for problem 5.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 February 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) A, B, the limits of the interval
!    of approximation.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b

  a =  0.0D+00
  b = 10.0D+00

  return
end
subroutine p05_story ( )

!*****************************************************************************80
!
!! P05_STORY prints the "story" for problem 5.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 February 2012
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

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '  This example is very difficult to interpolate.'
  write ( *, '(a)' ) &
    '  It is essentially defined as a piecewise function,'
  write ( *, '(a)' ) &
    '  alternating between a straight line and a sinusoidal curve.'

  return
end
subroutine p05_title ( title )

!*****************************************************************************80
!
!! P05_TITLE returns the title of problem 5.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 February 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, character ( len = * ) TITLE, the title of the problem.
!
  implicit none

  character ( len = * ) title

  title = 'Rabbit ears, f(x) = max(sin(x)+sin(x^2),1-abs(x-5)/5), [0,10]'

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
!    06 August 2005
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
  character ( len = 9  ), parameter, dimension(12) :: month = (/ &
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

  write ( *, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end
