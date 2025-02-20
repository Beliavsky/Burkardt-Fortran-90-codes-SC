subroutine i4mat_to_i4vec ( m, n, a, b )

!*****************************************************************************80
!
!! I4MAT_TO_I4VEC copies an I4MAT into an I4VEC.
!
!  Discussion:
!
!    An I4MAT is an M by N array of I4's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 April 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input, integer ( kind = 4 ) A(M,N), an M by N matrix.
!
!    Output, integer ( kind = 4 ) B(M*N), the vector.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) b(m*n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  do j = 1, n
    do i = 1, m
      b(j+(i-1)*n)= a(i,j)
    end do
  end do

  return
end
subroutine i4mat_flip_cols ( m, n, a )

!*****************************************************************************80
!
!! I4MAT_FLIP_COLS swaps the columns of an I4MAT.
!
!  Discussion:
!
!    An I4MAT is an M by N array of I4's.
!
!    To "flip" the columns of an I4MAT is to start with something like
!
!      11 12 13 14 15
!      21 22 23 24 25
!      31 32 33 34 35
!      41 42 43 44 45
!      51 52 53 54 55
!
!    and return
!
!      15 14 13 12 11
!      25 24 23 22 21
!      35 34 33 32 31
!      45 44 43 42 41
!      55 54 53 52 51
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 June 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input/output, integer ( kind = 4 ) A(M,N), the matrix whose columns
!    are to be flipped.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) b(m)
  integer ( kind = 4 ) j

  do j = 1, n / 2
    b(1:m      ) = a(1:m,    j)
    a(1:m,    j) = a(1:m,n+1-j)
    a(1:m,n+1-j) = b(1:m)
  end do

  return
end
subroutine i4mat_flip_rows ( m, n, a )

!*****************************************************************************80
!
!! I4MAT_FLIP_ROWS swaps the rows of an I4MAT.
!
!  Discussion:
!
!    An I4MAT is an M by N array of I4's.
!
!    To "flip" the rows of an I4MAT is to start with something like
!
!      11 12 13 14 15
!      21 22 23 24 25
!      31 32 33 34 35
!      41 42 43 44 45
!      51 52 53 54 55
!
!    and return
!
!      51 52 53 54 55
!      41 42 43 44 45
!      31 32 33 34 35
!      21 22 23 24 25
!      11 12 13 14 15
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 June 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input/output, integer ( kind = 4 ) A(M,N), the matrix whose rows
!    are to be flipped.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) b(n)
  integer ( kind = 4 ) i

  do i = 1, m / 2
    b(      1:n) = a(    i,1:n)
    a(    i,1:n) = a(m+1-i,1:n)
    a(m+1-i,1:n) = b(      1:n)
  end do

  return
end
subroutine i4mat_transpose ( m, n, a )

!*****************************************************************************80
!
!! I4MAT_TRANSPOSE transposes an I4MAT.
!
!  Discussion:
!
!    An I4MAT is an M by N array of I4's.
!
!    GNU Fortran's "transpose()" function doesn't do the job here.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 April 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input, integer ( kind = 4 ) A(M,N), an M by N matrix.
!
!    Output, integer ( kind = 4 ) A(N,M), the transposed matrix.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(m*n)
  integer ( kind = 4 ), allocatable :: b(:)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  allocate ( b(m*n) )

  do j = 1, n
    do i = 1, m
      b(j+(i-1)*n)= a(i+(j-1)*m)
    end do
  end do

  do j = 1, n
    do i = 1, m
      a(j+(i-1)*n) = b(j+(i-1)*n)
    end do
  end do
  
  deallocate ( b )

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
subroutine polyomino_transform ( m, n, p, rotate, reflect, mq, nq, q )

!*****************************************************************************80
!
!! POLYOMINO_TRANSFORM transforms a polyomino.
!
!  Discussion:
!
!    A polyomino can be rotated or reflected.
!
!    This program is given a polyomino and returns the resulting polyomino
!    after the specified reflection and rotation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 April 2018
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
!    Input, integer ( kind = 4 ) ROTATE, is 0, 1, 2, or 3, the number of 90 
!    degree counterclockwise rotations to be applied.
!
!    Input, integer ( kind = 4 ) REFLECT, is 0 or 1.  If it is 1, then each row 
!    of the polyomino matrix is to be reflected before any rotations are 
!    performed.
!
!    Output, integer ( kind = 4 ) *MQ, *NQ, the number of rows and columns of 
!    the representation of the transformed polyomino
!
!    Output, integer ( kind = 4 ) Q[MQ*NQ], the representation of the 
!    transformed polyomino.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) k
  integer ( kind = 4 ) mq
  integer ( kind = 4 ) nq
  integer ( kind = 4 ) p(m,n)
  integer ( kind = 4 ) q(m*n)
  integer ( kind = 4 ) reflect
  integer ( kind = 4 ) rotate
  integer ( kind = 4 ) r
  integer ( kind = 4 ) s

  mq = m
  nq = n

  reflect = mod ( reflect, 2 )

  call i4mat_to_i4vec ( m, n, p, q )

  if ( reflect == 1 ) then
    call i4mat_flip_cols ( mq, nq, q )
  end if

  rotate = mod ( rotate, 4 )

  do k = 1, rotate
    call i4mat_transpose ( mq, nq, q )
    r = mq
    s = nq
    mq = s
    nq = r
    call i4mat_flip_rows ( mq, nq, q )
  end do

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
