subroutine get_unit ( iunit )

!*****************************************************************************80
!
!! GET_UNIT returns a free FORTRAN unit number.
!
!  Discussion:
!
!    A "free" FORTRAN unit number is a value between 1 and 99 which
!    is not currently associated with an I/O device.  A free FORTRAN unit
!    number is needed in order to open a file with the OPEN command.
!
!    If IUNIT = 0, then no free FORTRAN unit could be found, although
!    all 99 units were checked (except for units 5, 6 and 9, which
!    are commonly reserved for console I/O).
!
!    Otherwise, IUNIT is a value between 1 and 99, representing a
!    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
!    are special, and will never return those values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 October 2008
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
  logical ( kind = 4 ) lopen

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
subroutine polyomino_lp_write ( filename, label, m, n, a, b )

!*****************************************************************************80
!
!! POLYOMINO_LP_WRITE writes an LP file for the polyomino problem.
!
!  Discussion:
!
!    The absurd and tedious process of trying to convince FORTRAN to print
!    data without extraneous blanks is a real comparative disadvantage.
!    I have to relearn it every time I need it.
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
!  Parameters:
!
!    Input, character ( len = * ) FILENAME, the output filename.
!
!    Input, character ( len = * ) LABEL, the problem title.
!
!    Input, integer ( kind = 4 ) M, the number of equations
!
!    Input, integer ( kind = 4 ) N, the number of variables.
!
!    Input, integer ( kind = 4 ) A(M,N), the coefficients.
!
!    Input, integer ( kind = 4 ) B(M), the right hand sides.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) b(m)
  character ( len = * ) filename
  logical first
  character ( len = * ) label
  integer ( kind = 4 ) output
  integer ( kind = 4 ) output_status
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  character ( len = 200 ) s
  character ( len = 80 ) s_j
  character ( len = 80 ) s_mag
!
!  Open the file.
!
  call get_unit ( output )

  open ( unit = output, file = filename, status = 'replace', &
    iostat = output_status )

  if ( output_status /= 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'POLYOMINO_LP_WRITE - Error!'
    write ( *, '(a)' ) '  Could not open the output file.'
    stop 1
  end if

  write ( output, '(a)' ) label
  write ( output, '(a)' ) ''

  write ( output, '(a)' ) 'Maximize'
  write ( output, '(a)' ) '  Obj: 0'

  write ( output, '(a)' ) 'Subject to'

  do i = 1, m

    first = .true.

    do j = 1, n

      if ( a(i,j) /= 0 ) then

        s = ''

        if ( a(i,j) < 0 ) then
          s = trim ( s ) // ' -'
        else if ( .not. first ) then
          s = trim ( s ) // ' +'
        end if

        if ( abs ( a(i,j) ) /= 1 ) then
          write ( s_mag, '(i8)' ) abs ( a(i,j) )
          s_mag = adjustl ( s_mag )
          s = trim ( s ) // ' ' // trim ( s_mag )
        end if

        write ( s_j, '(i4)' ) j
        s_j = adjustl ( s_j )
        s = trim ( s ) // ' x' // s_j
        write ( output, '(a)', advance = 'no' ) trim ( s )

        first = .false.

      end if
    end do
    s = ' ='
    write ( s_mag, '(i8)' ) b(i)
    s_mag = adjustl ( s_mag )
    s = trim ( s ) // ' ' // trim ( s_mag )
    write ( output, '(a)', advance = 'no' ) trim ( s )
    write ( output, '(a)' ) ''
 
  end do

  write ( output, '(a)' ) 'Binary'
  s = ' '
  do j = 1, n
    write ( s_j, '(i4)' ) j
    s_j = adjustl ( s_j )
    s = trim ( s ) // ' x' // s_j
  end do
  write ( output, '(a)' ) trim ( s )

  write ( output, '(a)' ) 'End'
!
!  Close the file.
!
  close ( output )

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
