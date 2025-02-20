subroutine polyomino_embed_list ( mr, nr, r, mp, np, p, number, list )

!*****************************************************************************80
!
!! POLYOMINO_EMBED_LIST lists the polyomino embeddings in a region.
!
!  Discusion:
!
!    A region R is a subset of an MRxNR grid of squares.
!
!    A polyomino P is a subset of an MPxNP grid of squares.
!
!    Both objects are represented by binary matrices, with the property that
!    there are no initial or final zero rows or columns.
!
!    For this computation, we regard P as a "fixed" polyomino in other words,
!    no reflections or rotations will be allowed.
!
!    An "embedding" of P into R is an offset (MI,NJ) such that 
!      P(I,J) = R(I+MI,J+NJ) 
!      for 1 <= I <= MP, 1 <= J <= NP, and 
!      for 0 <= MI <= MR-MP, 0 <= MJ <= NR-NP.
!    We can detect an embedding simply by taking what amounts to a kind of
!    dot product of P with a corresponding subregion of R.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 May 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) MR, NR, the number of rows and columns in the 
!    representation of the region R.
!
!    Input, integer ( kind = 4 ) R(MR,NR), a matrix of 0's and 1's 
!    representing the region.
!
!    Input, integer ( kind = 4 ) MP, NP, the number of rows and columns in the 
!    representation of the polyomino P.
!
!    Input, integer ( kind = 4 ) P(MP,NP), a matrix of 0's and 1's representing
!    the polyomino.
!
!    Input, integer ( kind = 4 ) NUMBER, the number of embeddings.
!
!    Output, integer ( kind = 4 ) LIST(NUMBER,2), for each embedding, the I and 
!    J offsets applied to the polyomino P.
!
  implicit none

  integer ( kind = 4 ) mp
  integer ( kind = 4 ) mr
  integer ( kind = 4 ) np
  integer ( kind = 4 ) nr
  integer ( kind = 4 ) number

  integer ( kind = 4 ) k
  integer ( kind = 4 ) list(number,2)
  integer ( kind = 4 ) mi
  integer ( kind = 4 ) nj
  integer ( kind = 4 ) p(mp,np)
  integer ( kind = 4 ) pr
  integer ( kind = 4 ) r(mr,nr)
  integer ( kind = 4 ) srp
!
!  Count the 1's in P.
!
  pr = sum ( p(1:mp,1:np) )
!
!  For each possible (I,J) coordinate of the upper left corner of a subset of R,
!  see if it matches P.
!
  k = 0
  do mi = 0, mr - mp
    do nj = 0, nr - np
      srp = sum ( p(1:mp,1:np) * r(1+mi:mp+mi,1+nj:np+nj) )
      if ( srp == pr ) then
        k = k + 1
        list(k,1) = mi
        list(k,2) = nj
      end if
    end do
  end do

  return
end
subroutine polyomino_embed_number ( mr, nr, r, mp, np, p, number )

!*****************************************************************************80
!
!! POLYOMINO_EMBED_NUMBER counts the number of polyomino embeddings in a region.
!
!  Discusion:
!
!    A region R is a subset of an MRxNR grid of squares.
!
!    A polyomino P is a subset of an MPxNP grid of squares.
!
!    Both objects are represented by binary matrices, with the property that
!    there are no initial or final zero rows or columns.
!
!    For this computation, we regard P as a "fixed" polyomino in other words,
!    no reflections or rotations will be allowed.
!
!    An "embedding" of P into R is an offset (MI,NJ) such that 
!      P(I,J) = R(I+MI,J+NJ) 
!      for 1 <= I <= MP, 1 <= J <= NP, and 
!      for 0 <= MI <= MR-MP, 0 <= MJ <= NR-NP.
!    We can detect an embedding simply by taking what amounts to a kind of
!    dot product of P with a corresponding subregion of R.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 May 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) MR, NR, the number of rows and columns in
!    the representation of the region R.
!
!    Input, integer ( kind = 4 ) R[MR*NR], a matrix of 0's and 1's representing
!    the region.
!
!    Input, integer ( kind = 4 ) MP, NP, the number of rows and columns in the 
!    representation of the polyomino P.
!
!    Input, integer ( kind = 4 )v P[MP*NP], a matrix of 0's and 1's 
!    representing the polyomino.
!
!    Output, integer ( kind = 4 ) POLYOMINO_EMBED_NUMBER, the number of 
!    distinct embeddings of P into R.
!
  implicit none

  integer ( kind = 4 ) mp
  integer ( kind = 4 ) mr
  integer ( kind = 4 ) np
  integer ( kind = 4 ) nr

  integer ( kind = 4 ) mi
  integer ( kind = 4 ) nj
  integer ( kind = 4 ) number
  integer ( kind = 4 ) p(mp,np)
  integer ( kind = 4 ) pr
  integer ( kind = 4 ) r(mr,nr)
  integer ( kind = 4 ) srp

  number = 0
!
!  Count the 1's in P.
!
  pr = sum ( p(1:mp,1:np) )
!
!  For each possible (I,J) coordinate of the upper left corner of a subset of R,
!  see if it matches P.
!
  do mi = 0, mr - mp
    do nj = 0, nr - np
      srp = sum ( p(1:mp,1:np) * r(1+mi:mp+mi,1+nj:np+nj) )
      if ( srp == pr ) then
        number = number + 1
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

