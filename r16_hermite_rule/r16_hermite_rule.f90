program main

!*****************************************************************************80
!
!! MAIN is the main program for R16_HERMITE_RULE.
!
!  Discussion:
!
!    This program computes a Gauss-Hermite quadrature rule
!    and writes it to a file.
!
!    The user specifies:
!    * the ORDER (number of points) in the rule;
!    * A, the center point;
!    * B, a scale factor;
!    * FILENAME, the root name of the output files.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 February 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 16 ) a
  real ( kind = 16 ) alpha
  integer ( kind = 4 ) arg_num
  real ( kind = 16 ) b
  real ( kind = 16 ) beta
  character ( len = 255 ) filename
  integer ( kind = 4 ) iarg
  integer ( kind = 4 ) iargc
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) kind
  integer ( kind = 4 ) last
  integer ( kind = 4 ) order
  real ( kind = 16 ) r(2)
  real ( kind = 16 ) r16_huge
  character ( len = 255 ) string
  real ( kind = 16 ), allocatable, dimension ( : ) :: w
  real ( kind = 16 ), allocatable, dimension ( : ) :: x
  real ( kind = 16 ) zero

  zero = real ( 0, kind = 16 )

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R16_HERMITE_RULE'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '  Compute a Gauss-Hermite quadrature rule for approximating'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '    Integral ( -oo < x < +oo ) f(x) exp ( - b * ( x - a )^2 ) dx'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  using ORDER points.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  The user specifies ORDER, A, B, and FILENAME.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  ORDER is the number of points (usually odd).'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  A is the center point (usually 0.0):'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  B is a scale factor (usually 1.0);'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  FILENAME is used to generate 3 files:'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    filename_w.txt - the weight file'
  write ( *, '(a)' ) '    filename_x.txt - the abscissa file.'
  write ( *, '(a)' ) '    filename_r.txt - the region file.'
!
!  Set some variables internally.
!
  alpha = zero
  beta = zero
!
!  Get the number of command line arguments.
!
  arg_num = iargc ( )
!
!  Get ORDER.
!
  if ( 1 <= arg_num ) then
    iarg = 1
    call getarg ( iarg, string )
    call s_to_i4 ( string, order, ierror, last )
  else
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Enter the rule order ORDER:'
    read ( *, * ) order
  end if
!
!  Get A.
!
  if ( 2 <= arg_num ) then
    iarg = 2
    call getarg ( iarg, string )
    call s_to_r16 ( string, a, ierror, last )
  else
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Enter A, the center point (usually 0.0):'
    read ( *, * ) a
  end if
!
!  Get B.
!
  if ( 3 <= arg_num ) then
    iarg = 3
    call getarg ( iarg, string )
    call s_to_r16 ( string, b, ierror, last )
  else
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Enter B, the scale (usually 1.0):'
    read ( *, * ) b
  end if
!
!  Get FILENAME.
!
  if ( 4 <= arg_num ) then
    iarg = 4
    call getarg ( iarg, filename )
  else
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) &
      '  Enter FILENAME, the "root name" of the quadrature files.'
    read ( *, '(a)' ) filename
  end if
!
!  Input summary.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)'      ) '  ORDER =  ', order
  write ( *, '(a,g14.6)'   ) '  A =  ', a
  write ( *, '(a,g14.6)'   ) '  B =  ', b
  write ( *, '(a)'         ) '  FILENAME = "' // trim ( filename ) // '".'
!
!  Construct the rule.
!
  allocate ( w(order) )
  allocate ( x(order) )

  kind = 6
  call cgqf ( order, kind, alpha, beta, a, b, x, w )
!
!  Write the rule.
!
  r(1) = - r16_huge ( )
  r(2) =   r16_huge ( )

  call rule_write ( order, x, w, r, filename )
!
!  Free memory.
!
  deallocate ( w )
  deallocate ( x )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R16_HERMITE_RULE:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop
end
subroutine cdgqf ( nt, kind, alpha, beta, t, wts )

!*****************************************************************************80
!
!! CDGQF computes a Gauss quadrature formula with default A, B and simple knots.
!
!  Discussion:
!
!    This routine computes all the knots and weights of a Gauss quadrature
!    formula with a classical weight function with default values for A and B,
!    and only simple knots.
!
!    There are no moments checks and no printing is done.
!
!    Use routine EIQFS to evaluate a quadrature computed by CGQFS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 January 2010
!
!  Author:
!
!    Original FORTRAN77 version by Sylvan Elhay, Jaroslav Kautsky.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Sylvan Elhay, Jaroslav Kautsky,
!    Algorithm 655: IQPACK, FORTRAN Subroutines for the Weights of
!    Interpolatory Quadrature,
!    ACM Transactions on Mathematical Software,
!    Volume 13, Number 4, December 1987, pages 399-415.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NT, the number of knots.
!
!    Input, integer ( kind = 4 ) KIND, the rule.
!    1, Legendre,             (a,b)       1.0
!    2, Chebyshev,            (a,b)       ((b-x)*(x-a))^(-0.5)
!    3, Gegenbauer,           (a,b)       ((b-x)*(x-a))^alpha
!    4, Jacobi,               (a,b)       (b-x)^alpha*(x-a)^beta
!    5, Generalized Laguerre, (a,inf)     (x-a)^alpha*exp(-b*(x-a))
!    6, Generalized Hermite,  (-inf,inf)  |x-a|^alpha*exp(-b*(x-a)^2)
!    7, Exponential,          (a,b)       |x-(a+b)/2.0|^alpha
!    8, Rational,             (a,inf)     (x-a)^alpha*(x+b)^beta
!
!    Input, real ( kind = 16 ) ALPHA, the value of Alpha, if needed.
!
!    Input, real ( kind = 16 ) BETA, the value of Beta, if needed.
!
!    Output, real ( kind = 16 ) T(NT), the knots.
!
!    Output, real ( kind = 16 ) WTS(NT), the weights.
!
  implicit none

  integer ( kind = 4 ) nt

  real ( kind = 16 ) aj(nt)
  real ( kind = 16 ) alpha
  real ( kind = 16 ) beta
  real ( kind = 16 ) bj(nt)
  integer ( kind = 4 ) kind
  real ( kind = 16 ) t(nt)
  real ( kind = 16 ) wts(nt)
  real ( kind = 16 ) zemu

  call parchk ( kind, 2 * nt, alpha, beta )
!
!  Get the Jacobi matrix and zero-th moment.
!
  call class_matrix ( kind, nt, alpha, beta, aj, bj, zemu )
!
!  Compute the knots and weights.
!
  call sgqf ( nt, aj, bj, zemu, t, wts )

  return
end
subroutine cgqf ( nt, kind, alpha, beta, a, b, t, wts )

!*****************************************************************************80
!
!! CGQF computes knots and weights of a Gauss quadrature formula.
!
!  Discussion:
!
!    The user may specify the interval (A,B).
!
!    Only simple knots are produced.
!
!    Use routine EIQFS to evaluate this quadrature formula.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 February 2010
!
!  Author:
!
!    Original FORTRAN77 version by Sylvan Elhay, Jaroslav Kautsky.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Sylvan Elhay, Jaroslav Kautsky,
!    Algorithm 655: IQPACK, FORTRAN Subroutines for the Weights of
!    Interpolatory Quadrature,
!    ACM Transactions on Mathematical Software,
!    Volume 13, Number 4, December 1987, pages 399-415.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NT, the number of knots.
!
!    Input, integer ( kind = 4 ) KIND, the rule.
!    1, Legendre,             (a,b)       1.0
!    2, Chebyshev Type 1,     (a,b)       ((b-x)*(x-a))^-0.5)
!    3, Gegenbauer,           (a,b)       ((b-x)*(x-a))^alpha
!    4, Jacobi,               (a,b)       (b-x)^alpha*(x-a)^beta
!    5, Generalized Laguerre, (a,+oo)     (x-a)^alpha*exp(-b*(x-a))
!    6, Generalized Hermite,  (-oo,+oo)   |x-a|^alpha*exp(-b*(x-a)^2)
!    7, Exponential,          (a,b)       |x-(a+b)/2.0|^alpha
!    8, Rational,             (a,+oo)     (x-a)^alpha*(x+b)^beta
!    9, Chebyshev Type 2,     (a,b)       ((b-x)*(x-a))^(+0.5)
!
!    Input, real ( kind = 16 ) ALPHA, the value of Alpha, if needed.
!
!    Input, real ( kind = 16 ) BETA, the value of Beta, if needed.
!
!    Input, real ( kind = 16 ) A, B, the interval endpoints, or
!    other parameters.
!
!    Output, real ( kind = 16 ) T(NT), the knots.
!
!    Output, real ( kind = 16 ) WTS(NT), the weights.
!
  implicit none

  integer ( kind = 4 ) nt

  real ( kind = 16 ) a
  real ( kind = 16 ) alpha
  real ( kind = 16 ) b
  real ( kind = 16 ) beta
  integer ( kind = 4 ) i
  integer ( kind = 4 ) kind
  integer ( kind = 4 ), allocatable :: mlt(:)
  integer ( kind = 4 ), allocatable :: ndx(:)
  real ( kind = 16 ) t(nt)
  real ( kind = 16 ) wts(nt)
!
!  Compute the Gauss quadrature formula for default values of A and B.
!
  call cdgqf ( nt, kind, alpha, beta, t, wts )
!
!  Prepare to scale the quadrature formula to other weight function with
!  valid A and B.
!
  allocate ( mlt(1:nt) )

  mlt(1:nt) = 1

  allocate ( ndx(1:nt) )

  do i = 1, nt
    ndx(i) = i
  end do

  call scqf ( nt, t, mlt, wts, nt, ndx, wts, t, kind, alpha, beta, a, b )

  deallocate ( mlt )
  deallocate ( ndx )

  return
end
subroutine ch_cap ( c )

!*****************************************************************************80
!
!! CH_CAP capitalizes a single character.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 July 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character C, the character to capitalize.
!
  implicit none

  character c
  integer ( kind = 4 ) itemp

  itemp = ichar ( c )

  if ( 97 <= itemp .and. itemp <= 122 ) then
    c = char ( itemp - 32 )
  end if

  return
end
function ch_eqi ( c1, c2 )

!*****************************************************************************80
!
!! CH_EQI is a case insensitive comparison of two characters for equality.
!
!  Example:
!
!    CH_EQI ( 'A', 'a' ) is .TRUE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character C1, C2, the characters to compare.
!
!    Output, logical CH_EQI, the result of the comparison.
!
  implicit none

  logical ch_eqi
  character c1
  character c1_cap
  character c2
  character c2_cap

  c1_cap = c1
  c2_cap = c2

  call ch_cap ( c1_cap )
  call ch_cap ( c2_cap )

  if ( c1_cap == c2_cap ) then
    ch_eqi = .true.
  else
    ch_eqi = .false.
  end if

  return
end
subroutine ch_to_digit ( c, digit )

!*****************************************************************************80
!
!! CH_TO_DIGIT returns the value of a base 10 digit.
!
!  Example:
!
!     C   DIGIT
!    ---  -----
!    '0'    0
!    '1'    1
!    ...  ...
!    '9'    9
!    ' '    0
!    'X'   -1
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character C, the decimal digit, '0' through '9' or blank
!    are legal.
!
!    Output, integer ( kind = 4 ) DIGIT, the corresponding value.
!    If C was 'illegal', then DIGIT is -1.
!
  implicit none

  character c
  integer ( kind = 4 ) digit

  if ( lge ( c, '0' ) .and. lle ( c, '9' ) ) then

    digit = ichar ( c ) - 48

  else if ( c == ' ' ) then

    digit = 0

  else

    digit = -1

  end if

  return
end
subroutine class_matrix ( kind, m, alpha, beta, aj, bj, zemu )

!*****************************************************************************80
!
!! CLASS_MATRIX computes the Jacobi matrix for a quadrature rule.
!
!  Discussion:
!
!    This routine computes the diagonal AJ and sub-diagonal BJ
!    elements of the order M tridiagonal symmetric Jacobi matrix
!    associated with the polynomials orthogonal with respect to
!    the weight function specified by KIND.
!
!    For weight functions 1-7, M elements are defined in BJ even
!    though only M-1 are needed.  For weight function 8, BJ(M) is
!    set to zero.
!
!    The zero-th moment of the weight function is returned in ZEMU.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 December 2009
!
!  Author:
!
!    Original FORTRAN77 version by Sylvan Elhay, Jaroslav Kautsky.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Sylvan Elhay, Jaroslav Kautsky,
!    Algorithm 655: IQPACK, FORTRAN Subroutines for the Weights of
!    Interpolatory Quadrature,
!    ACM Transactions on Mathematical Software,
!    Volume 13, Number 4, December 1987, pages 399-415.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) KIND, the rule.
!    1, Legendre,             (a,b)       1.0
!    2, Chebyshev,            (a,b)       ((b-x)*(x-a))^(-0.5)
!    3, Gegenbauer,           (a,b)       ((b-x)*(x-a))^alpha
!    4, Jacobi,               (a,b)       (b-x)^alpha*(x-a)^beta
!    5, Generalized Laguerre, (a,inf)     (x-a)^alpha*exp(-b*(x-a))
!    6, Generalized Hermite,  (-inf,inf)  |x-a|^alpha*exp(-b*(x-a)^2)
!    7, Exponential,          (a,b)       |x-(a+b)/2.0|^alpha
!    8, Rational,             (a,inf)     (x-a)^alpha*(x+b)^beta
!
!    Input, integer ( kind = 4 ) M, the order of the Jacobi matrix.
!
!    Input, real ( kind = 16 ) ALPHA, the value of Alpha, if needed.
!
!    Input, real ( kind = 16 ) BETA, the value of Beta, if needed.
!
!    Output, real ( kind = 16 ) AJ(M), BJ(M), the diagonal and subdiagonal
!    of the Jacobi matrix.
!
!    Output, real ( kind = 16 ) ZEMU, the zero-th moment.
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 16 ) a2b2
  real ( kind = 16 ) ab
  real ( kind = 16 ) aba
  real ( kind = 16 ) abi
  real ( kind = 16 ) abj
  real ( kind = 16 ) abti
  real ( kind = 16 ) aj(m)
  real ( kind = 16 ) alpha
  real ( kind = 16 ) apone
  real ( kind = 16 ) beta
  real ( kind = 16 ) bj(m)
  real ( kind = 16 ) five_hundred
  real ( kind = 16 ) four
  real ( kind = 16 ) half
  integer ( kind = 4 ) i
  integer ( kind = 4 ) kind
  real ( kind = 16 ) one
  real ( kind = 16 ) pi
  real ( kind = 16 ) r16_pi
  real ( kind = 16 ) temp
  real ( kind = 16 ) temp2
  real ( kind = 16 ) three
  real ( kind = 16 ) two
  real ( kind = 16 ) zemu
  real ( kind = 16 ) zero

  five_hundred = real ( 500.0, kind = 16 )
  four = real ( 4.0, kind = 16 )
  half = real ( 0.5, kind = 16 )
  one = real ( 1.0, kind = 16 )
  pi = r16_pi ( )
  three = real ( 3.0, kind = 16 )
  two = real ( 2.0, kind = 16 )
  zero = real ( 0.0, kind = 16 )

  temp = epsilon ( temp )

  call parchk ( kind, 2 * m - 1, alpha, beta )

  temp2 = half
  temp2 = gamma ( temp2 )

  if ( five_hundred * temp < abs ( temp2 * temp2 - pi ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CLASS_MATRIX - Warning!'
    write ( *, '(a)' ) '  Gamma function does not match machine parameters.'
    write ( *, * ) '  500 * eps = ', five_hundred * temp
    write ( *, * ) '  gamma ( 0.5 ) ^2 - pi = ', temp2 * temp2 - pi
  end if

  if ( kind == 1 ) then

    ab = zero

    zemu = two / ( ab + one )

    aj(1:m) = zero

    do i = 1, m
      abi = i + ab * mod ( i, 2 )
      abj = 2 * i + ab
      bj(i) = abi * abi / ( abj * abj - one )
    end do
    bj(1:m) =  sqrt ( bj(1:m) )

  else if ( kind == 2 ) then

    zemu = pi

    aj(1:m) = zero

    bj(1) =  sqrt ( half )
    bj(2:m) = half

  else if ( kind == 3 ) then

    ab = alpha * two
    zemu = two**( ab + one ) * gamma ( alpha + one )**2 &
      / gamma ( ab + two )

    aj(1:m) = zero
    bj(1) = one / ( two * alpha + three )
    do i = 2, m
      bj(i) = i * ( i + ab ) / ( four * ( i + alpha )**2 - one )
    end do
    bj(1:m) =  sqrt ( bj(1:m) )

  else if ( kind == 4 ) then

    ab = alpha + beta
    abi = two + ab
    zemu = two**( ab + one ) * gamma ( alpha + one ) &
      * gamma ( beta + one ) / gamma ( abi )
    aj(1) = ( beta - alpha ) / abi
    bj(1) = four * ( one + alpha ) * ( one + beta ) &
      / ( ( abi + one ) * abi * abi )
    a2b2 = beta * beta - alpha * alpha

    do i = 2, m
      abi = two * i + ab
      aj(i) = a2b2 / ( ( abi - two ) * abi )
      abi = abi**2
      bj(i) = four * i * ( i + alpha ) * ( i + beta ) * ( i + ab ) &
        / ( ( abi - one ) * abi )
    end do
    bj(1:m) =  sqrt ( bj(1:m) )

  else if ( kind == 5 ) then

    zemu = gamma ( alpha + one )

    do i = 1, m
      aj(i) = two * i - one + alpha
      bj(i) = i * ( i + alpha )
    end do
    bj(1:m) =  sqrt ( bj(1:m) )

  else if ( kind == 6 ) then

    zemu = gamma ( ( alpha + one ) / two )

    aj(1:m) = zero

    do i = 1, m
      bj(i) = ( i + alpha * mod ( i, 2 ) ) / two
    end do
    bj(1:m) =  sqrt ( bj(1:m) )

  else if ( kind == 7 ) then

    ab = alpha
    zemu = two / ( ab + one )

    aj(1:m) = zero

    do i = 1, m
      abi = i + ab * mod ( i, 2 )
      abj = 2 * i + ab
      bj(i) = abi * abi / ( abj * abj - one )
    end do
    bj(1:m) =  sqrt ( bj(1:m) )

  else if ( kind == 8 ) then

    ab = alpha + beta
    zemu = gamma ( alpha + one ) * gamma ( - ( ab + one ) ) &
      / gamma ( - beta )
    apone = alpha + one
    aba = ab * apone
    aj(1) = - apone / ( ab + two )
    bj(1) = - aj(1) * ( beta + one ) / ( ab + two ) / ( ab + three )
    do i = 2, m
      abti = ab + two * i
      aj(i) = aba + two * ( ab + i ) * ( i - 1 )
      aj(i) = - aj(i) / abti / ( abti - two )
    end do

    do i = 2, m - 1
      abti = ab + two * i
      bj(i) = i * ( alpha + i ) / ( abti - one ) * ( beta + i ) &
        / ( abti**2 ) * ( ab + i ) / ( abti + one )
    end do

    bj(m) = zero
    bj(1:m) =  sqrt ( bj(1:m) )

  end if

  return
end
subroutine get_unit ( iunit )

!*****************************************************************************80
!
!! GET_UNIT returns a free FORTRAN unit number.
!
!  Discussion:
!
!    A "free" FORTRAN unit number is an integer between 1 and 99 which
!    is not currently associated with an I/O device.  A free FORTRAN unit
!    number is needed in order to open a file with the OPEN command.
!
!    If IUNIT = 0, then no free FORTRAN unit could be found, although
!    all 99 units were checked (except for units 5, 6 and 9, which
!    are commonly reserved for console I/O).
!
!    Otherwise, IUNIT is an integer between 1 and 99, representing a
!    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
!    are special, and will never return those values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 September 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) IUNIT, the free unit number.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ios
  integer ( kind = 4 ) iunit
  logical lopen

  iunit = 0

  do i = 1, 99

    if ( i /= 5 .and. i /= 6 .and. i /= 9 ) then

      inquire ( unit = i, opened = lopen, iostat = ios )

      if ( ios == 0 ) then
        if ( .not. lopen ) then
          iunit = i
          return
        end if
      end if

    end if

  end do

  return
end
subroutine imtqlx ( n, d, e, z )

!*****************************************************************************80
!
!! IMTQLX diagonalizes a symmetric tridiagonal matrix.
!
!  Discussion:
!
!    This routine is a slightly modified version of the EISPACK routine to
!    perform the implicit QL algorithm on a symmetric tridiagonal matrix.
!
!    The authors thank the authors of EISPACK for permission to use this
!    routine.
!
!    It has been modified to produce the product Q' * Z, where Z is an input
!    vector and Q is the orthogonal matrix diagonalizing the input matrix.
!    The changes consist (essentially) of applying the orthogonal 
!    transformations directly to Z as they are generated.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 December 2009
!
!  Author:
!
!    Original FORTRAN77 version by Sylvan Elhay, Jaroslav Kautsky.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Sylvan Elhay, Jaroslav Kautsky,
!    Algorithm 655: IQPACK, FORTRAN Subroutines for the Weights of
!    Interpolatory Quadrature,
!    ACM Transactions on Mathematical Software,
!    Volume 13, Number 4, December 1987, pages 399-415.
!
!    Roger Martin, James Wilkinson,
!    The Implicit QL Algorithm,
!    Numerische Mathematik,
!    Volume 12, Number 5, December 1968, pages 377-383.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input/output, real ( kind = 16 ) D(N), the diagonal entries of the matrix.
!    On output, the information in D has been overwritten.
!
!    Input/output, real ( kind = 16 ) E(N), the subdiagonal entries of the
!    matrix, in entries E(1) through E(N-1).  On output, the information in
!    E has been overwritten.
!
!    Input/output, real ( kind = 16 ) Z(N).  On input, a vector.  On output,
!    the value of Q' * Z, where Q is the matrix that diagonalizes the
!    input symmetric tridiagonal matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 16 ) b
  real ( kind = 16 ) c
  real ( kind = 16 ) d(n)
  real ( kind = 16 ) e(n)
  real ( kind = 16 ) f
  real ( kind = 16 ) g
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ii
  integer ( kind = 4 ), parameter :: itn = 30
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) m
  integer ( kind = 4 ) mml
  real ( kind = 16 ) one
  real ( kind = 16 ) p
  real ( kind = 16 ) prec
  real ( kind = 16 ) r
  real ( kind = 16 ) s
  real ( kind = 16 ) two
  real ( kind = 16 ) z(n)
  real ( kind = 16 ) zero

  one = real ( 1.0, kind = 16 )
  two = real ( 2.0, kind = 16 )
  zero = real ( 0.0, kind = 16 )

  prec = epsilon ( prec )

  if ( n == 1 ) then
    return
  end if

  e(n) = zero

  do l = 1, n

    j = 0

    do

      do m = l, n

        if ( m == n ) then
          exit
        end if

        if ( abs ( e(m) ) <= prec * ( abs ( d(m) ) + abs ( d(m+1) ) ) ) then
          exit
        end if

      end do

      p = d(l)

      if ( m == l ) then
        exit
      end if

      if ( itn <= j ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'IMTQLX - Fatal error!'
        write ( *, '(a)' ) '  Iteration limit exceeded.'
        write ( *, '(a,i8)' ) '  J = ', j
        write ( *, '(a,i8)' ) '  L = ', l
        write ( *, '(a,i8)' ) '  M = ', m
        write ( *, '(a,i8)' ) '  N = ', n
        stop
      end if

      j = j + 1
      g = ( d(l+1) - p ) / ( two * e(l) )
      r =  sqrt ( g * g + one )
      g = d(m) - p + e(l) / ( g + sign ( r, g ) )
      s = one
      c = one
      p = zero
      mml = m - l

      do ii = 1, mml

        i = m - ii
        f = s * e(i)
        b = c * e(i)

        if ( abs ( g ) <= abs ( f ) ) then
          c = g / f
          r =  sqrt ( c * c + one )
          e(i+1) = f * r
          s = one / r
          c = c * s
        else
          s = f / g
          r =  sqrt ( s * s + one )
          e(i+1) = g * r
          c = one / r
          s = s * c
        end if

        g = d(i+1) - p
        r = ( d(i) - g ) * s + two * c * b
        p = s * r
        d(i+1) = g + p
        g = c * r - b
        f = z(i+1)
        z(i+1) = s * z(i) + c * f
        z(i) = c * z(i) - s * f

      end do

      d(l) = d(l) - p
      e(l) = g
      e(m) = zero

    end do

  end do
!
!  Sorting.
!
  do ii = 2, n

    i = ii - 1
    k = i
    p = d(i)

    do j = ii, n
      if ( d(j) < p ) then
        k = j
        p = d(j)
      end if
    end do

    if ( k /= i ) then
      d(k) = d(i)
      d(i) = p
      p = z(i)
      z(i) = z(k)
      z(k) = p
    end if

  end do

  return
end
subroutine parchk ( kind, m, alpha, beta )

!*****************************************************************************80
!
!! PARCHK checks parameters ALPHA and BETA for classical weight functions.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 December 2009
!
!  Author:
!
!    Original FORTRAN77 version by Sylvan Elhay, Jaroslav Kautsky.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Sylvan Elhay, Jaroslav Kautsky,
!    Algorithm 655: IQPACK, FORTRAN Subroutines for the Weights of
!    Interpolatory Quadrature,
!    ACM Transactions on Mathematical Software,
!    Volume 13, Number 4, December 1987, pages 399-415.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) KIND, the rule.
!    1, Legendre,             (a,b)       1.0
!    2, Chebyshev,            (a,b)       ((b-x)*(x-a))^(-0.5)
!    3, Gegenbauer,           (a,b)       ((b-x)*(x-a))^alpha
!    4, Jacobi,               (a,b)       (b-x)^alpha*(x-a)^beta
!    5, Generalized Laguerre, (a,inf)     (x-a)^alpha*exp(-b*(x-a))
!    6, Generalized Hermite,  (-inf,inf)  |x-a|^alpha*exp(-b*(x-a)^2)
!    7, Exponential,          (a,b)       |x-(a+b)/2.0|^alpha
!    8, Rational,             (a,inf)     (x-a)^alpha*(x+b)^beta
!
!    Input, integer ( kind = 4 ) M, the order of the highest moment to
!    be calculated.  This value is only needed when KIND = 8.
!
!    Input, real ( kind = 16 ) ALPHA, BETA, the parameters, if required
!    by the value of KIND.
!
  implicit none

  real ( kind = 16 ) alpha
  real ( kind = 16 ) beta
  integer ( kind = 4 ) kind
  integer ( kind = 4 ) m
  real ( kind = 16 ) one
  real ( kind = 16 ) tmp
  real ( kind = 16 ) zero

  one = real ( 1.0, kind = 16 )
  zero = real ( 0.0, kind = 16 )

  if ( kind <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'PARCHK - Fatal error!'
    write ( *, '(a)' ) '  KIND <= 0.'
    stop
  end if
!
!  Check ALPHA for Gegenbauer, Jacobi, Laguerre, Hermite, Exponential.
!
  if ( 3 <= kind .and. alpha <= - one ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'PARCHK - Fatal error!'
    write ( *, '(a)' ) '  3 <= KIND and ALPHA <= -1.'
    stop
  end if
!
!  Check BETA for Jacobi.
!
  if ( kind == 4 .and. beta <= - one ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'PARCHK - Fatal error!'
    write ( *, '(a)' ) '  KIND == 4 and BETA <= -1.0.'
    stop
  end if
!
!  Check ALPHA and BETA for rational.
!
  if ( kind == 8 ) then
    tmp = alpha + beta + real ( m + 1, kind = 16 )
    if ( zero <= tmp .or. tmp <= beta ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PARCHK - Fatal error!'
      write ( *, '(a)' ) '  KIND == 8 but condition on ALPHA and BETA fails.'
      stop
    end if
  end if

  return
end
function r16_huge ( )

!*****************************************************************************80
!
!! R16_HUGE returns a very large R16.
!
!  Discussion:
!
!    The value returned by this function is NOT required to be the
!    maximum representable R16.  This value varies from machine to machine,
!    from compiler to compiler, and may cause problems when being printed.
!    We simply want a "very large" but non-infinite number.
!
!    FORTRAN90 provides a built-in routine HUGE ( X ) that
!    can return the maximum representable number of the same datatype
!    as X, if that is what is really desired.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 May 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 16 ) R16_HUGE, a "huge" value.
!
  implicit none

  real ( kind = 16 ) r16_huge

  r16_huge = 1000000000000000000000000000000.0_16

  return
end
function r16_pi ( )

!*****************************************************************************80
!
!! R16_PI returns the value of pi as an R16.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 May 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 16 ) R16_PI, the value of pi.
!
  implicit none

  real ( kind = 16 ) r16_pi

  r16_pi = 3.1415926535897932384626433832795028_16

  return
end
subroutine r16mat_write ( output_filename, m, n, table )

!*****************************************************************************80
!
!! R16MAT_WRITE writes an R16MAT file.
!
!  Discussion:
!
!    An R16MAT is an array of R16 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 May 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) OUTPUT_FILENAME, the output file name.
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 16 ) TABLE(M,N), the table data.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) j
  character ( len = * ) output_filename
  integer ( kind = 4 ) output_status
  integer ( kind = 4 ) output_unit
  character ( len = 30 ) string
  real ( kind = 16 ) table(m,n)
!
!  Open the file.
!
  call get_unit ( output_unit )

  open ( unit = output_unit, file = output_filename, &
    status = 'replace', iostat = output_status )

  if ( output_status /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R16MAT_WRITE - Fatal error!'
    write ( *, '(a,i8)' ) '  Could not open the output file "' // &
      trim ( output_filename ) // '" on unit ', output_unit
    output_unit = -1
    stop
  end if
!
!  Create a format string.
!
  if ( 0 < m .and. 0 < n ) then

    write ( string, '(a1,i8,a1,i8,a1,i8,a1)' ) '(', m, 'g', 40, '.', 32, ')'
!
!  Write the data.
!
    do j = 1, n
      write ( output_unit, string ) table(1:m,j)
    end do

  end if
!
!  Close the file.
!
  close ( unit = output_unit )

  return
end
subroutine rule_write ( order, x, w, r, filename )

!*****************************************************************************80
!
!! RULE_WRITE writes a quadrature rule to a file.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 February 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) ORDER, the order of the rule.
!
!    Input, real ( kind = 16 ) X(ORDER), the abscissas.
!
!    Input, real ( kind = 16 ) W(ORDER), the weights.
!
!    Input, real ( kind = 16 ) R(2), defines the region.
!
!    Input, character ( len = * ) FILENAME, specifies the output.
!    'filename_w.txt', 'filename_x.txt', 'filename_r.txt' defining weights,
!    abscissas, and region.
!
  implicit none

  integer ( kind = 4 ) order

  character ( len = * ) filename
  character ( len = 255 ) filename_r
  character ( len = 255 ) filename_w
  character ( len = 255 ) filename_x
  integer ( kind = 4 ) i
  integer ( kind = 4 ) kind
  real ( kind = 16 ) r(2)
  real ( kind = 16 ) w(order)
  real ( kind = 16 ) x(order)

  filename_w = trim ( filename ) // '_w.txt'
  filename_x = trim ( filename ) // '_x.txt'
  filename_r = trim ( filename ) // '_r.txt'

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Creating quadrature files.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  "Root" file name is   "' // trim ( filename ) // '".'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Weight file will be   "' // trim ( filename_w ) // '".'
  write ( *, '(a)' ) '  Abscissa file will be "' // trim ( filename_x ) // '".'
  write ( *, '(a)' ) '  Region file will be   "' // trim ( filename_r ) // '".'

  call r16mat_write ( filename_w, 1, order, w )
  call r16mat_write ( filename_x, 1, order, x )
  call r16mat_write ( filename_r, 1, 2,     r )

  return
end
subroutine s_to_i4 ( s, ival, ierror, length )

!*****************************************************************************80
!
!! S_TO_I4 reads an I4 from a string.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 June 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, a string to be examined.
!
!    Output, integer ( kind = 4 ) IVAL, the integer value read from the string.
!    If the string is blank, then IVAL will be returned 0.
!
!    Output, integer ( kind = 4 ) IERROR, an error flag.
!    0, no error.
!    1, an error occurred.
!
!    Output, integer ( kind = 4 ) LENGTH, the number of characters of S
!    used to make IVAL.
!
  implicit none

  character c
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) isgn
  integer ( kind = 4 ) istate
  integer ( kind = 4 ) ival
  integer ( kind = 4 ) length
  character ( len = * ) s

  ierror = 0
  istate = 0
  isgn = 1
  ival = 0

  do i = 1, len_trim ( s )

    c = s(i:i)
!
!  Haven't read anything.
!
    if ( istate == 0 ) then

      if ( c == ' ' ) then

      else if ( c == '-' ) then
        istate = 1
        isgn = -1
      else if ( c == '+' ) then
        istate = 1
        isgn = + 1
      else if ( lle ( '0', c ) .and. lle ( c, '9' ) ) then
        istate = 2
        ival = ichar ( c ) - ichar ( '0' )
      else
        ierror = 1
        return
      end if
!
!  Have read the sign, expecting digits.
!
    else if ( istate == 1 ) then

      if ( c == ' ' ) then

      else if ( lle ( '0', c ) .and. lle ( c, '9' ) ) then
        istate = 2
        ival = ichar ( c ) - ichar ( '0' )
      else
        ierror = 1
        return
      end if
!
!  Have read at least one digit, expecting more.
!
    else if ( istate == 2 ) then

      if ( lle ( '0', c ) .and. lle ( c, '9' ) ) then
        ival = 10 * ival + ichar ( c ) - ichar ( '0' )
      else
        ival = isgn * ival
        length = i - 1
        return
      end if

    end if

  end do
!
!  If we read all the characters in the string, see if we're OK.
!
  if ( istate == 2 ) then
    ival = isgn * ival
    length = len_trim ( s )
  else
    ierror = 1
    length = 0
  end if

  return
end
subroutine s_to_r16 ( s, dval, ierror, length )

!*****************************************************************************80
!
!! S_TO_R16 reads an R16 from a string.
!
!  Discussion:
!
!    The routine will read as many characters as possible until it reaches
!    the end of the string, or encounters a character which cannot be
!    part of the number.
!
!    Legal input is:
!
!       1 blanks,
!       2 '+' or '-' sign,
!       2.5 blanks
!       3 integer part,
!       4 decimal point,
!       5 fraction part,
!       6 'E' or 'e' or 'D' or 'd', exponent marker,
!       7 exponent sign,
!       8 exponent integer part,
!       9 exponent decimal point,
!      10 exponent fraction part,
!      11 blanks,
!      12 final comma or semicolon,
!
!    with most quantities optional.
!
!  Example:
!
!    S                 DVAL
!
!    '1'               1.0
!    '     1   '       1.0
!    '1A'              1.0
!    '12,34,56'        12.0
!    '  34 7'          34.0
!    '-1E2ABCD'        -100.0
!    '-1X2ABCD'        -1.0
!    ' 2E-1'           0.2
!    '23.45'           23.45
!    '-4.2E+2'         -420.0
!    '17d2'            1700.0
!    '-14e-2'         -0.14
!    'e2'              100.0
!    '-12.73e-9.23'   -12.73 * 10.0**(-9.23)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 May 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string containing the
!    data to be read.  Reading will begin at position 1 and
!    terminate at the end of the string, or when no more
!    characters can be read to form a legal real.  Blanks,
!    commas, or other nonnumeric data will, in particular,
!    cause the conversion to halt.
!
!    Output, real ( kind = 16 ) DVAL, the value read from the string.
!
!    Output, integer ( kind = 4 ) IERROR, error flag.
!    0, no errors occurred.
!    1, 2, 6 or 7, the input number was garbled.  The
!    value of IERROR is the last type of input successfully
!    read.  For instance, 1 means initial blanks, 2 means
!    a plus or minus sign, and so on.
!
!    Output, integer ( kind = 4 ) LENGTH, the number of characters read
!    to form the number, including any terminating
!    characters such as a trailing comma or blanks.
!
  implicit none

  character c
  logical ch_eqi
  real ( kind = 16 ) dval
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) ihave
  integer ( kind = 4 ) isgn
  integer ( kind = 4 ) iterm
  integer ( kind = 4 ) jbot
  integer ( kind = 4 ) jsgn
  integer ( kind = 4 ) jtop
  integer ( kind = 4 ) length
  integer ( kind = 4 ) nchar
  integer ( kind = 4 ) ndig
  real ( kind = 16 ) one
  real ( kind = 16 ) rbot
  real ( kind = 16 ) rexp
  real ( kind = 16 ) rtop
  character ( len = * ) s
  real ( kind = 16 ) ten
  real ( kind = 16 ) zero

  one = real ( 1.0, kind = 16 )
  ten = real ( 10.0, kind = 16 )
  zero = real ( 0.0, kind = 16 )

  nchar = len_trim ( s )

  ierror = 0
  dval = zero
  length = -1
  isgn = 1
  rtop = 0
  rbot = 1
  jsgn = 1
  jtop = 0
  jbot = 1
  ihave = 1
  iterm = 0

  do

    length = length + 1

    if ( nchar < length + 1 ) then
      exit
    end if

    c = s(length+1:length+1)
!
!  Blank character.
!
    if ( c == ' ' ) then

      if ( ihave == 2 ) then

      else if ( ihave == 6 .or. ihave == 7 ) then
        iterm = 1
      else if ( 1 < ihave ) then
        ihave = 11
      end if
!
!  Comma.
!
    else if ( c == ',' .or. c == ';' ) then

      if ( ihave /= 1 ) then
        iterm = 1
        ihave = 12
        length = length + 1
      end if
!
!  Minus sign.
!
    else if ( c == '-' ) then

      if ( ihave == 1 ) then
        ihave = 2
        isgn = -1
      else if ( ihave == 6 ) then
        ihave = 7
        jsgn = -1
      else
        iterm = 1
      end if
!
!  Plus sign.
!
    else if ( c == '+' ) then

      if ( ihave == 1 ) then
        ihave = 2
      else if ( ihave == 6 ) then
        ihave = 7
      else
        iterm = 1
      end if
!
!  Decimal point.
!
    else if ( c == '.' ) then

      if ( ihave < 4 ) then
        ihave = 4
      else if ( 6 <= ihave .and. ihave <= 8 ) then
        ihave = 9
      else
        iterm = 1
      end if
!
!  Scientific notation exponent marker.
!
    else if ( ch_eqi ( c, 'E' ) .or. ch_eqi ( c, 'D' ) ) then

      if ( ihave < 6 ) then
        ihave = 6
      else
        iterm = 1
      end if
!
!  Digit.
!
    else if (  ihave < 11 .and. lle ( '0', c ) .and. lle ( c, '9' ) ) then

      if ( ihave <= 2 ) then
        ihave = 3
      else if ( ihave == 4 ) then
        ihave = 5
      else if ( ihave == 6 .or. ihave == 7 ) then
        ihave = 8
      else if ( ihave == 9 ) then
        ihave = 10
      end if

      call ch_to_digit ( c, ndig )

      if ( ihave == 3 ) then
        rtop = ten * rtop + real ( ndig, kind = 16 )
      else if ( ihave == 5 ) then
        rtop = ten * rtop + real ( ndig, kind = 16 )
        rbot = ten * rbot
      else if ( ihave == 8 ) then
        jtop = 10 * jtop + ndig
      else if ( ihave == 10 ) then
        jtop = 10 * jtop + ndig
        jbot = 10 * jbot
      end if
!
!  Anything else is regarded as a terminator.
!
    else
      iterm = 1
    end if
!
!  If we haven't seen a terminator, and we haven't examined the
!  entire string, go get the next character.
!
    if ( iterm == 1 ) then
      exit
    end if

  end do
!
!  If we haven't seen a terminator, and we have examined the
!  entire string, then we're done, and LENGTH is equal to NCHAR.
!
  if ( iterm /= 1 .and. length+1 == nchar ) then
    length = nchar
  end if
!
!  Number seems to have terminated.  Have we got a legal number?
!  Not if we terminated in states 1, 2, 6 or 7!
!
  if ( ihave == 1 .or. ihave == 2 .or. ihave == 6 .or. ihave == 7 ) then
    ierror = ihave
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'S_TO_R16 - Serious error!'
    write ( *, '(a)' ) '  Illegal or nonnumeric input:'
    write ( *, '(a)' ) '    ' // trim ( s )
    return
  end if
!
!  Number seems OK.  Form it.
!
  if ( jtop == 0 ) then
    rexp = one
  else
    if ( jbot == 1 ) then
      rexp = ten ** ( jsgn * jtop )
    else
      rexp = ten ** ( real ( jsgn * jtop, kind = 16 ) &
        / real ( jbot, kind = 16 ) )
    end if
  end if

  dval = real ( isgn, kind = 16 ) * rexp * rtop / rbot

  return
end
subroutine scqf ( nt, t, mlt, wts, nwts, ndx, swts, st, kind, alpha, beta, a, &
  b )

!*****************************************************************************80
!
!! SCQF scales a quadrature formula to a nonstandard interval.
!
!  Discussion:
!
!    The arrays WTS and SWTS may coincide.
!
!    The arrays T and ST may coincide.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 December 2009
!
!  Author:
!
!    Original FORTRAN77 version by Sylvan Elhay, Jaroslav Kautsky.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Sylvan Elhay, Jaroslav Kautsky,
!    Algorithm 655: IQPACK, FORTRAN Subroutines for the Weights of
!    Interpolatory Quadrature,
!    ACM Transactions on Mathematical Software,
!    Volume 13, Number 4, December 1987, pages 399-415.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NT, the number of knots.
!
!    Input, real ( kind = 16 ) T(NT), the original knots.
!
!    Input, integer ( kind = 4 ) MLT(NT), the multiplicity of the knots.
!
!    Input, real ( kind = 16 ) WTS(NWTS), the weights.
!
!    Input, integer ( kind = 4 ) NWTS, the number of weights.
!
!    Input, integer ( kind = 4 ) NDX(NT), used to index the array WTS.
!    For more details see the comments in CAWIQ.
!
!    Output, real ( kind = 16 ) SWTS(NWTS), the scaled weights.
!
!    Output, real ( kind = 16 ) ST(NT), the scaled knots.
!
!    Input, integer ( kind = 4 ) KIND, the rule.
!    1, Legendre,             (a,b)       1.0
!    2, Chebyshev Type 1,     (a,b)       ((b-x)*(x-a))^(-0.5)
!    3, Gegenbauer,           (a,b)       ((b-x)*(x-a))^alpha
!    4, Jacobi,               (a,b)       (b-x)^alpha*(x-a)^beta
!    5, Generalized Laguerre, (a,+oo)     (x-a)^alpha*exp(-b*(x-a))
!    6, Generalized Hermite,  (-oo,+oo)   |x-a|^alpha*exp(-b*(x-a)^2)
!    7, Exponential,          (a,b)       |x-(a+b)/2.0|^alpha
!    8, Rational,             (a,+oo)     (x-a)^alpha*(x+b)^beta
!    9, Chebyshev Type 2,     (a,b)       ((b-x)*(x-a))^(+0.5)
!
!    Input, real ( kind = 16 ) ALPHA, the value of Alpha, if needed.
!
!    Input, real ( kind = 16 ) BETA, the value of Beta, if needed.
!
!    Input, real ( kind = 16 ) A, B, the interval endpoints.
!
  implicit none

  integer ( kind = 4 ) nt
  integer ( kind = 4 ) nwts

  real ( kind = 16 ) a
  real ( kind = 16 ) al
  real ( kind = 16 ) alpha
  real ( kind = 16 ) b
  real ( kind = 16 ) be
  real ( kind = 16 ) beta
  real ( kind = 16 ) half
  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  integer ( kind = 4 ) kind
  integer ( kind = 4 ) l
  integer ( kind = 4 ) mlt(nt)
  integer ( kind = 4 ) ndx(nt)
  real ( kind = 16 ) one
  real ( kind = 16 ) p
  real ( kind = 16 ) shft
  real ( kind = 16 ) slp
  real ( kind = 16 ) st(nt)
  real ( kind = 16 ) swts(nwts)
  real ( kind = 16 ) t(nt)
  real ( kind = 16 ) temp
  real ( kind = 16 ) tmp
  real ( kind = 16 ) two
  real ( kind = 16 ) wts(nwts)
  real ( kind = 16 ) zero

  half = real ( 0.5, kind = 16 )
  one = real ( 1.0, kind = 16 )
  two = real ( 2.0, kind = 16 )
  zero = real ( 0.0, kind = 16 )

  temp = epsilon ( temp )

  call parchk ( kind, 1, alpha, beta )

  if ( kind == 1 ) then

    al = zero
    be = zero

    if ( abs ( b - a ) <= temp ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SCQF - Fatal error!'
      write ( *, '(a)' ) '  |B - A| too small.'
      stop
    end if

    shft = ( a + b ) / two
    slp = ( b - a ) / two

  else if ( kind == 2 ) then

    al = - half
    be = - half

    if ( abs ( b - a ) <= temp ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SCQF - Fatal error!'
      write ( *, '(a)' ) '  |B - A| too small.'
      stop
    end if

    shft = ( a + b ) / two
    slp = ( b - a ) / two

  else if ( kind == 3 ) then

    al = alpha
    be = alpha

    if ( abs ( b - a ) <= temp ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SCQF - Fatal error!'
      write ( *, '(a)' ) '  |B - A| too small.'
      stop
    end if

    shft = ( a + b ) / two
    slp = ( b - a ) / two

  else if ( kind == 4 ) then

    al = alpha
    be = beta

    if ( abs ( b - a ) <= temp ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SCQF - Fatal error!'
      write ( *, '(a)' ) '  |B - A| too small.'
      stop
    end if

    shft = ( a + b ) / two
    slp = ( b - a ) / two

  else if ( kind == 5 ) then

    if ( b <= zero ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SCQF - Fatal error!'
      write ( *, '(a)' ) '  B <= 0'
      stop
    end if

    shft = a
    slp = one / b
    al = alpha
    be = zero

  else if ( kind == 6 ) then

    if ( b <= zero ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SCQF - Fatal error!'
      write ( *, '(a)' ) '  B <= 0.'
      stop
    end if

    shft = a
    slp = one / sqrt ( b )
    al = alpha
    be = zero

  else if ( kind == 7 ) then

    al = alpha
    be = zero

    if ( abs ( b - a ) <= temp ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SCQF - Fatal error!'
      write ( *, '(a)' ) '  |B - A| too small.'
      stop
    end if

    shft = ( a + b ) / two
    slp = ( b - a ) / two

  else if ( kind == 8 ) then

    if ( a + b <= zero ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SCQF - Fatal error!'
      write ( *, '(a)' ) '  A + B <= 0.'
      stop
    end if

    shft = a
    slp = a + b
    al = alpha
    be = beta

  else if ( kind == 9 ) then

    al = half
    be = half

    if ( abs ( b - a ) <= temp ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SCQF - Fatal error!'
      write ( *, '(a)' ) '  |B - A| too small.'
      stop
    end if

    shft = ( a + b ) / two
    slp = ( b - a ) / two

  end if

  p = slp ** ( al + be + one )

  do k = 1, nt

    st(k) = shft + slp * t(k)
    l = abs ( ndx(k) )

    if ( l /= 0 ) then
      tmp = p
      do i = l, l + mlt(k) - 1
        swts(i) = wts(i) * tmp
        tmp = tmp * slp
      end do
    end if

  end do

  return
end
subroutine sgqf ( nt, aj, bj, zemu, t, wts )

!*****************************************************************************80
!
!! SGQF computes knots and weights of a Gauss Quadrature formula.
!
!  Discussion:
!
!    This routine computes all the knots and weights of a Gauss quadrature
!    formula with simple knots from the Jacobi matrix and the zero-th
!    moment of the weight function, using the Golub-Welsch technique.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 January 2010
!
!  Author:
!
!    Original FORTRAN77 version by Sylvan Elhay, Jaroslav Kautsky.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Sylvan Elhay, Jaroslav Kautsky,
!    Algorithm 655: IQPACK, FORTRAN Subroutines for the Weights of
!    Interpolatory Quadrature,
!    ACM Transactions on Mathematical Software,
!    Volume 13, Number 4, December 1987, pages 399-415.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NT, the number of knots.
!
!    Input, real ( kind = 16 ) AJ(NT), the diagonal of the Jacobi matrix.
!
!    Input/output, real ( kind = 16 ) BJ(NT), the subdiagonal of the Jacobi
!    matrix, in entries 1 through NT-1.  On output, BJ has been overwritten.
!
!    Input, real ( kind = 16 ) ZEMU, the zero-th moment of the weight function.
!
!    Output, real ( kind = 16 ) T(NT), the knots.
!
!    Output, real ( kind = 16 ) WTS(NT), the weights.
!
  implicit none

  integer ( kind = 4 ) nt

  real ( kind = 16 ) aj(nt)
  real ( kind = 16 ) bj(nt)
  integer ( kind = 4 ) i
  real ( kind = 16 ) t(nt)
  real ( kind = 16 ) wts(nt)
  real ( kind = 16 ) zemu
  real ( kind = 16 ) zero

  zero = real ( 0.0, kind = 16 )
!
!  Exit if the zero-th moment is not positive.
!
  if ( zemu <= zero ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SGQF - Fatal error!'
    write ( *, '(a)' ) '  ZEMU <= 0.'
    stop
  end if
!
!  Set up vectors for IMTQLX.
!
  t(1:nt) = aj(1:nt)

  wts(1) = sqrt ( zemu )
  wts(2:nt) = zero
!
!  Diagonalize the Jacobi matrix.
!
  call imtqlx ( nt, t, bj, wts )

  wts(1:nt) = wts(1:nt)**2

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
