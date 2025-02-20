subroutine polyomino_condense ( mp, np, p, mq, nq, q )

!*****************************************************************************80
!
!! POLYOMINO_CONDENSE condenses a polyomino.
!
!  Discussion:
!
!    A polyomino is a shape formed by connecting unit squares edgewise.
!
!    A polyomino can be represented by an MxN matrix, whose entries are
!    1 for squares that are part of the polyomino, and 0 otherwise.
!
!    This program is given an MxN matrix that is meant to represent a 
!    polyomino.  It first replaces all nonzero entries by the value 1.
!    It then "condenses" the matrix, if possible, by removing initial and
!    final rows and columns that are entirely zero.
!
!    While this procedure might save a slight amount of space, its purpose
!    is to simplify the task of manipulating polyominos, embedding them in
!    larger shapes, and detecting whether two polyominos describe the same
!    shape.
!
!    It is entirely possible, and usual, that the output quantities are
!    simply copies of the input quantities.
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
!    Input, integer ( kind = 4 ) MP, NP, the number of rows and columns in the 
!    representation of the polyomino P.
!
!    Input, integer ( kind = 4 ) P(MP*NP), a matrix of 0's and 1's representing 
!    the polyomino.  
!
!    Output, integer ( kind = 4 ) MQ, NQ, the number of rows and columns of the
!    condensed polyomino.
!
!    Output, integer ( kind = 4 ) Q(MQ*NQ), the representation of the condensed
!    polyomino.
!
  implicit none

  integer ( kind = 4 ) mp
  integer ( kind = 4 ) mq
  integer ( kind = 4 ) np
  integer ( kind = 4 ) nq

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i_max
  integer ( kind = 4 ) i_min
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j_max
  integer ( kind = 4 ) j_min
  integer ( kind = 4 ) p(mp*np)
  integer ( kind = 4 ) q(mq*nq)
!
!  Discard nonsense.
!
  if ( mp <= 0 .or. np <= 0 ) then
    mq = 0
    nq = 0
    return
  end if
!
!  Seek first and last nonzero rows, columns.
!
  i_min = -1
  do i = 1, mp
    do j = 1, np
      if ( p(i+(j-1)*mp) /= 0 ) then
        i_min = i;
        exit
      end if
    end do
    if ( i_min /= -1 ) then
      exit
    end if
  end do
!
!  If I_MIN = -1, then we have a null matrix.
!
  if ( i_min == -1 ) then
    mq = 0
    nq = 0
    return
  end if

  i_max = mp + 1
  do i = mp, 1, -1
    do j = 1, np
      if ( p(i+(j-1)*mp) /= 0 ) then
        i_max = i
        exit
      end if
    end do
    if ( i_max /= mp + 1 ) then
      exit
    end if
  end do

  j_min = -1
  do j = 1, np
    do i = 1, mp
      if ( p(i+(j-1)*mp) /= 0 ) then
        j_min = j
        exit
      end if
    end do
    if ( j_min /= -1 ) then
      exit
    end if
  end do

  j_max = np + 1
  do j = np, 1, -1
    do i = 1, mp
      if ( p(i+(j-1)*mp) /= 0 ) then
        j_max = j
        exit
      end if
    end do
    if ( j_max /= np + 1 ) then
      exit
    end if
  end do
!
!  Measure the nonzero block.
!
  mq = i_max + 1 - i_min
  nq = j_max + 1 - j_min
!
!  Copy the nonzero block.
!
  do j = 1, nq
    do i = 1, mq
      if ( p(i+i_min-1+(j+j_min-1-1)*mp) /= 0 ) then
        q(i+(j-1)*mq) = 1
      else
        q(i+(j-1)*mq) = 0
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
!    Input, integer ( kind = 4 ) P(M*N), a matrix of 0's and 1's representing 
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
