program main

!*****************************************************************************80
!
!! MAIN is the main program for MARIO.
!
!  Discussion:
!
!    MARIO creates an image of Mario using colored squares.
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
  implicit none

  integer ( kind = 4 ), parameter :: m = 16
  integer ( kind = 4 ), parameter :: n = 13

  character ( len = 16 ) color
  integer ( kind = 4 ) :: color_index(m,n) = reshape ( (/ &
     0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 5, 5, 5, 0, 0, 6, &
     0, 0, 0, 6, 6, 6, 0, 0, 2, 2, 5, 5, 5, 0, 6, 6, &
     0, 2, 6, 5, 5, 6, 0, 2, 2, 2, 2, 5, 3, 3, 6, 6, &
     2, 2, 6, 6, 6, 5, 5, 2, 2, 2, 3, 3, 3, 3, 6, 6, &
     2, 2, 6, 5, 6, 5, 5, 3, 3, 3, 4, 3, 3, 3, 0, 0, &
     2, 2, 5, 5, 5, 5, 5, 2, 2, 3, 3, 3, 3, 0, 0, 0, &
     2, 2, 5, 5, 5, 5, 5, 2, 2, 3, 3, 3, 3, 0, 0, 0, &
     2, 2, 5, 5, 5, 5, 5, 2, 3, 3, 4, 3, 3, 3, 0, 0, &
     2, 2, 1, 1, 5, 1, 5, 2, 2, 2, 3, 3, 3, 3, 6, 6, &
     0, 2, 5, 5, 1, 1, 5, 0, 2, 2, 2, 5, 3, 3, 6, 6, &
     0, 2, 0, 5, 5, 1, 5, 0, 2, 2, 5, 5, 5, 0, 6, 6, &
     0, 2, 0, 5, 5, 1, 0, 0, 0, 2, 5, 5, 5, 0, 0, 6, &
     0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /), &
    (/ m, n /) )
  character ( len = 255 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 255 ) header
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i_reverse
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  character ( len = 255 ) plot_filename

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'MARIO'
  write ( *, '(a)' ) '  Create a GNUPLOT input file that will draw a'
  write ( *, '(a)' ) '  picture of Mario, using colored squares.'

  header = 'mario'
!
!  Create the command file.
!
  call get_unit ( command_unit )
  command_filename = 'mario_commands.txt'
  open ( unit = command_unit, file = command_filename, &
    status = 'replace' )
  write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '# Usage:'
  write ( command_unit, '(a)' ) '#  gnuplot < ' // &
    trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set term png'
  plot_filename = trim ( header ) // '.png'
  write ( command_unit, '(a)' ) 'set output "' // &
    trim ( plot_filename ) // '"'
  write ( command_unit, '(a)' ) 'set title "MARIO"'
!
!  Trying to get a plot TRUE SQUARES was exasperatingly awkward.
!
  write ( command_unit, '(a)' ) 'set xrange [ -1.5 : 14.5 ]'
  write ( command_unit, '(a)' ) 'set yrange [ 0 : 16 ]'
  write ( command_unit, '(a)' ) 'set size square'
  write ( command_unit, '(a)' ) 'unset border'
  write ( command_unit, '(a)' ) 'unset tics'
  write ( command_unit, '(a)' ) 'set nokey'

  k = 0
  do i = 1, m

    do j = 1, n
      k = k + 1

      if ( color_index(i,j) == 0 ) then
        color = 'white'
      else if ( color_index(i,j) == 1 ) then
        color = 'black'
      else if ( color_index(i,j) == 2 ) then
        color = 'red'
      else if ( color_index(i,j) == 3 ) then
        color = 'blue'
      else if ( color_index(i,j) == 4 ) then
        color = 'yellow'
!
!  The next color should be 'blanchedalmond' but stoopid GNUPLOT won't take
!  it as a string.'
!
      else if ( color_index(i,j) == 5 ) then
        color = '#FFEBCD'
      else if ( color_index(i,j) == 6 ) then
        color = 'brown'
      end if

      i_reverse = m + 1 - i
      write ( command_unit, '(a,i4,a,i2,a,i2,a,i2,a,i2,a)' ) &
        'set object ', k, ' rect from ', j-1, ',', i_reverse-1, ' to ', j, ',', i_reverse, ' back'
      write ( command_unit, '(a,i4,a)' ) &
        'set object ', k, ' rect fc rgb "' // trim ( color ) // '" fillstyle solid 1.0'

    end do
  end do
!
!  If you don't have some bogus PLOT command here, all the previous work
!  results in no plot all.  Way to go, gnuplot!
!  Here, we plot the function y = -1, which is out of range and won't show up.
!
  write ( command_unit, '(a)' ) 'plot -1 with lines'
  close ( unit = command_unit )

  write ( *, '(a)' ) &
    '  Created command file "' // trim ( command_filename ) // '".'
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'MARIO:'
  write ( *, '(a)' ) '  Normal end of execution.'

  stop 0
end
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

