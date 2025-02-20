subroutine polyomino_index ( m, n, p, pin )

!*****************************************************************************80
!
!! POLYOMINO_INDEX assigns an index to each nonzero entry of a polyomino.
!
!  Example:
!
!    P = 
!      1 0 1 1
!      1 1 1 0
!      0 1 1 0
!
!    PIN =
!      1 0 2 3
!      4 5 6 0
!      0 7 8 0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 May 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns in the 
!    array that represents the polyomino.
!
!    Input, integer ( kind = 4 ) P(M*N), the polyomino.  It is assumed that 
!    every entry is a 0 or a 1.
!
!    Output, integer ( kind = 4 ) PIN(M*N), the index of each nonzero entry.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) i;
  integer ( kind = 4 ) j;
  integer ( kind = 4 ) k;
  integer ( kind = 4 ) p(m*n)
  integer ( kind = 4 ) pin(m*n)

  k = 0
  do i = 1, m
    do j = 1, n
      if ( p(i+(j-1)*m) /= 0 ) then
        k = k + 1
        pin(i+(j-1)*m) = k
      else
        pin(i+(j-1)*m) = 0
      end if
    end do
  end do

  return
end
subroutine polyomino_print ( m, n, p, label )

!*****************************************************************************80
!
!! POLYOMINO_PRINT prints a polyomino.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 April 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns in the 
!    representation of the polyomino P.
!
!    Input, integer ( kind = 4 ) P[M*N], a matrix of 0's and 1's representing 
!    the polyomino.  The matrix should be "tight", that is, there should be a
!    1 in row 1, and in column 1, and in row M, and in column N.
!
!    Input, character ( len = * ) LABEL, a title for the polyomino.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  character ( len = * ) label
  integer ( kind = 4 ) p(m,n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) label
  write ( *, '(a)' ) ''

  if ( m < 1 .or. n < 1 ) then

    write ( *, '(a)' ) '  [ Null matrix ]'

  else

    do i = 1, m
      do j = 1, n
        write ( *, '(1x,i1)', advance = 'no' ) p(i,j)
      end do
      write ( *, '(a)' ) ''
    end do

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

  write ( *, '(i2.2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end
