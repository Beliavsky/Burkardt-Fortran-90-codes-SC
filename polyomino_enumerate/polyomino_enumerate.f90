subroutine polyomino_enumerate_chiral ( n_data, order, number )

!*****************************************************************************80
!
!! POLYOMINO_ENUMERATE_CHIRAL counts chiral polyominoes (allowing holes).
!
!  Discussion:
!
!    Polyominoes are connected planar shapes formed by adjoining unit squares.
!
!    The number of unit squares in a polyomino is its order.
!
!    If we do not ignore reflections, but ignore rotations when comparing,
!    then we are considering the class of "fixed" polyominoes.  In that case,
!    for instance, there are 18 chiral polyominoes of order 5.
!
!    As the order increases, the number of polyominoes grows very rapidly.
!    The list offered here goes no further than order 28, but the later
!    numbers in the list are too large to represent as 32 byte integers. 
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 May 2018
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Solomon Golomb,
!    Polyominoes: Puzzles, Patterns, Problems, and Packings,
!    Princeton University Press, 1996,
!    ISBN: 9780691024448
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0 
!    before the first call.  On each call, the routine increments N_DATA by 1, 
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer ( kind = 4 ) ORDER, the order of a polyomino.
!
!    Output, integer ( kind = 8 ) NUMBER, the number of chiral polyominos 
!    of this order.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 31

  integer ( kind = 4 ) n_data
  integer ( kind = 8 ) number
  integer ( kind = 8 ), save, dimension ( n_max ) :: number_vec = (/ & 
    1_8, & 
    1_8, &
    1_8, &
    2_8, &
    7_8, &
    18_8, & 
    60_8, &
    196_8, &
    704_8, &
    2500_8, &
    9189_8, & 
    33896_8, &
    126759_8, &
    476270_8, &
    1802312_8, &
    6849777_8, & 
    26152418_8, &
    100203194_8, &
    385221143_8, &
    1485200848_8, &
    5741256764_8, & 
    22245940545_8, &
    86383382827_8, &
    336093325058_8, &
    1309998125640_8, &
    5114451441106_8, & 
    19998172734786_8, &
    78306011677182_8, &
    307022182222506_8, &
    1205243866707468_8, &
    4736694001644862_8 /)
  integer ( kind = 4 ) order
  integer ( kind = 4 ), save, dimension ( n_max ) :: order_vec = (/ & 
    0, &
    1,  2,  3,  4,  5, &
    6,  7,  8,  9, 10, &
   11, 12, 13, 14, 15, &
   16, 17, 18, 19, 20, &
   21, 22, 23, 24, 25, &
   26, 27, 28, 29, 30 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    order = 0
    number = 0_8
  else
    order = order_vec(n_data)
    number = number_vec(n_data)
  end if

  return
end
subroutine polyomino_enumerate_fixed ( n_data, order, number )

!*****************************************************************************80
!
!! POLYOMINO_ENUMERATE_FIXED counts fixed polyominoes (allowing holes).
!
!  Discussion:
!
!    Polyominoes are connected planar shapes formed by adjoining unit squares.
!
!    The number of unit squares in a polyomino is its order.
!
!    If we do not ignore reflections and rotations when comparing polyominoes,
!    then we are considering the class of "fixed" polyominoes.  In that case,
!    for instance, there are 65 fixed polyominoes of order 5.
!
!    As the order increases, the number of polyominoes grows very rapidly.
!    The list offered here goes no further than order 28, but the later
!    numbers in the list are too large to represent as 32 byte integers. 
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 April 2018
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Solomon Golomb,
!    Polyominoes: Puzzles, Patterns, Problems, and Packings,
!    Princeton University Press, 1996,
!    ISBN: 9780691024448
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0 
!    before the first call.  On each call, the routine increments N_DATA by 1, 
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer ( kind = 4 ) ORDER, the order of a polyomino.
!
!    Output, integer ( kind = 8 ) NUMBER, the number of fixed polyominos 
!    of this order.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 29

  integer ( kind = 4 ) n_data
  integer ( kind = 8 ) number
  integer ( kind = 8 ), save, dimension ( n_max ) :: number_vec = (/ & 
    1_8, &
    1_8, &
    2_8, &
    6_8, &
    19_8, &
    63_8, &
    216_8, &
    760_8, &
    2725_8, &
    9910_8, &
    36446_8, &
    135268_8, &
    505861_8, &
    1903890_8, &
    7204874_8, &
    27394666_8, &
    104592937_8, &
    400795844_8, &
    1540820542_8, &
    5940738676_8, &
    22964779660_8, &
    88983512783_8, &
    345532572678_8, &
    1344372335524_8, &
    5239988770268_8, &
    20457802016011_8, &
    79992676367108_8, &
    313224032098244_8, &
    1228088671826973_8 /)
  integer ( kind = 4 ) order
  integer ( kind = 4 ), save, dimension ( n_max ) :: order_vec = (/ & 
    0, &
    1,  2,  3,  4,  5, &
    6,  7,  8,  9, 10, &
   11, 12, 13, 14, 15, &
   16, 17, 18, 19, 20, &
   21, 22, 23, 24, 25, &
   26, 27, 28 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    order = 0
    number = 0_8
  else
    order = order_vec(n_data)
    number = number_vec(n_data)
  end if

  return
end
subroutine polyomino_enumerate_free ( n_data, order, number )

!*****************************************************************************80
!
!! POLYOMINO_ENUMERATE_FREE counts free polyominoes (allowing holes).
!
!  Discussion:
!
!    Polyominoes are connected planar shapes formed by adjoining unit squares.
!
!    The number of unit squares in a polyomino is its order.
!
!    If we ignore reflections and rotations when comparing polyominoes,
!    then we are considering the class of "free" polyominoes.  In that case,
!    for instance, there are just 12 free polyominoes of order 5, the
!    so called "pentominoes".
!
!    As the order increases, the number of polyominoes grows very rapidly.
!    The list offered here goes no further than order 28, but the later
!    numbers in the list are too large to represent as 32 byte integers. 
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 April 2018
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Solomon Golomb,
!    Polyominoes: Puzzles, Patterns, Problems, and Packings,
!    Princeton University Press, 1996,
!    ISBN: 9780691024448
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0 
!    before the first call.  On each call, the routine increments N_DATA by 1, 
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer ORDER ( kind = 4 ), the order of a polyomino.
!
!    Output, integer NUMBER ( kind = 8 ), the number of free polyominos of 
!    this order.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 29

  integer ( kind = 4 ) n_data
  integer ( kind = 8 ) number
  integer ( kind = 8 ), save, dimension ( n_max ) :: number_vec = (/ & 
    1_8, &  
    1_8, &
    1_8, &
    2_8, &
    5_8, &
    12_8, &
    35_8, &
    108_8, &
    369_8, &
    1285_8, &
    4655_8, &
    17073_8, &
    63600_8, &
    238591_8, &
    901971_8, &
    3426576_8, &
    13079255_8, &
    50107909_8, &
    192622052_8, &
    742624232_8, &
    2870671950_8, &
    11123060678_8, &
    43191857688_8, &
    168047007728_8, &
    654999700403_8, &
    2557227044764_8, &
    9999088822075_8, &
    39153010938487_8, &
    153511100594603_8 /)
  integer ( kind = 4 ) order
  integer ( kind = 4 ), save, dimension ( n_max ) :: order_vec = (/ & 
    0, &
    1,  2,  3,  4,  5, &
    6,  7,  8,  9, 10, &
   11, 12, 13, 14, 15, &
   16, 17, 18, 19, 20, &
   21, 22, 23, 24, 25, &
   26, 27, 28 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    order = 0
    number = 0_8
  else
    order = order_vec(n_data)
    number = number_vec(n_data)
  end if

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

  write ( *, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end
