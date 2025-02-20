program main

!*****************************************************************************80
!
!! MAIN is the main program for SLICE.
!
!  Discussion:
!
!    SLICE is a program which reads multidimensional graphic data and 
!    makes a "slice" of it in a given plane, along which a contour plot
!    can be created.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    real CMAX(NCOST), the maximum value of each cost function.
! 
!    real CMIN(NCOST), contains the minimum value of each
!    cost function.
!
!    real COST(0:MAXSTP,NCOST), contains the value of the 
!    functionals at each computed point.  COST(0,*) is the 
!    value of the functional at the target solution, and 
!    should be zero or very close to it.
!
!    character ( len = 10 ) DEV, the graphics output device
!    to be used.  Current legal values for DEV include:
!    CGMB - CGM binary file.
!    PS   - PostScript file.
!    XWS  - X window screen (interactive).
!
!    character ( len = 40 ) FILDAT, the name of the data file to be 
!    read in, which contains the information defining the 
!    sequence of parameters and their function values.
!
!    integer ( kind = 4 ) LOGY, determines whether graphs will be X versus
!    Y, or X versus LOG(Y).
!    0, X versus Y.
!    1, X versus LOG(Y).
!
!    integer ( kind = 4 ) NCON, the number of contour levels.
!
!    integer ( kind = 4 ) NCOST, the number of different cost functions,
!    currently set to 6.
!
!    real PARA(0:MAXSTP,MAXPAR), contains the value of the
!    parameters at each computed point.  In particular,
!    the value of the J-th parameter at step I is
!    PARA(I,J).  It is assumed that the first step is
!    numbered 0.
!
!    character ( len = 40 ) TITLE, the plot title.
!
  implicit none

  integer ( kind = 4 ), parameter :: maxstp = 100
  integer ( kind = 4 ), parameter :: maxobj = 5
  integer ( kind = 4 ), parameter :: ncost = 1

  real cmax
  real cmin
  character ( len = 80 ) command
  real cost(maxstp)
  character ( len = 10 ) dev
  character ( len = 40 ) fildat
  real grace
  integer ( kind = 4 ) i
  integer ( kind = 4 ) icolor(maxobj)
  integer ( kind = 4 ) ifile
  integer ( kind = 4 ) ios
  integer ( kind = 4 ) iplot
  integer ( kind = 4 ) iwrite
  integer ( kind = 4 ) kcost(ncost)
  character ( len = 40 ) lcost(ncost)
  logical s_eqi
  logical lnei
  integer ( kind = 4 ) logy
  integer ( kind = 4 ) ncon
  integer ( kind = 4 ) nmax
  integer ( kind = 4 ) nmin
  integer ( kind = 4 ) nstep
  character ( len = 12 ) object(maxobj)
  logical show(maxobj)
  real temp1
  real temp2
  character ( len = 40 ) title
  character ( len = 40 ) title2
  real xarray(maxstp)
  real xsmax
  real xsmin
  real yarray(maxstp)
  real ysmax
  real ysmin
!
!  Greetings!
!
  call timestamp ( )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SLICE'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Last modified on 16 August 1994'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Display "slices" of a functional, or'
  write ( *, '(a)' ) '  contours for any two parameters, or'
  write ( *, '(a)' ) '  the history of an optimization iteration.'
  write ( *, '(a)' ) ' '
!
!  Set defaults and initial values.
!
  call init(cost,dev,fildat,grace,icolor,ifile,iplot,iwrite, &
    kcost,lcost,logy,maxobj,maxstp,ncon,ncost,object,show,title, &
    title2,xsmax,xsmin,ysmax,ysmin)
!
!  Get the next command from the user.
!
   30 continue
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '? (H for help)'
  read(*,'(a)')command
  if ( command == ' ')go to 30
!
!  A: show axes.
!
  if ( s_eqi(command,'a') ) then
  
    show(5) = .not.show(5)
    
    if ( show(2) ) then
      write ( *, '(a)' ) 'Axes will be shown in the next plot.'
    else
      write ( *, '(a)' ) 'Axes will NOT be shown in the next plot.'
    end if
!
!  C: choose colors.
!
  else if ( s_eqi(command,'c') ) then
  
    write ( *, '(a)' ) ' '
    write ( *, * ) 'Enter ',maxobj,' integers between 0 and 255.'
    write ( *, '(a)' ) 'These choose the colors to be used for'
    write ( *, '(a)' ) 'lines, frame, nodes, pressures, '
    write ( *, '(a)' ) 'streamlines, title, velocities, directions,'
    write ( *, '(a)' ) 'and magnitudes.'

    read(*,*)(icolor(i),i = 1,maxobj)
!
!  CMAX = : Choose maximum cost to display.
!
  else if ( s_eqi(command(1:4),'cmax') ) then
 
    if ( s_eqi(command(1:5),'cmax = ') ) then
      read(command(6:),*)cmax
    else
      write ( *, '(a)' ) 'Enter maximum value of cost.'
      read(*,*)cmax
    end if
!
!  CMIN = : Choose minimum cost to display.
!
  else if ( s_eqi(command(1:4),'cmin') ) then
 
    if ( s_eqi(command(1:5),'cmin = ') ) then
      read(command(6:),*)cmin
    else
      write ( *, '(a)' ) 'Enter minimum value of cost.'
      read(*,*)cmin
    end if
!
!  DAT = : specify the data file to be read.
!
  else if ( s_eqi(command(1:3),'dat') ) then

    if ( ifile > 0 ) then
      close(unit = 2)
      write ( *, '(a)' ) 'SLICE is now closing the file ' // fildat
    end if
 
    ifile = 0

    if ( s_eqi(command(1:4),'dat = ') ) then
      fildat = command(5:)
    else
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'Enter the name of the new input data file:'
      read(*,'(a)')fildat
    end if
 
    open ( unit = 2,file=fildat,status='old',err=70)
    ifile = 1
    write ( *, '(a)' ) 'SLICE has opened the new file.'
 
    call getdat ( cmax, cmin, cost, ifile, maxstp, nmax, nmin, nstep )

    if ( ifile < 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SLICE - Warning!'
      write ( *, '(a)' ) '  No data has been read!'
      write ( *, '(a)' ) '  The current file is being closed!'
      close(unit = 2)
      go to 30
    else
      write ( *, '(a)' ) 'SLICE has read the data.'
    end if
!
!  DEV = : set graphics output device.
!
  else if ( s_eqi(command(1:3),'dev') ) then
  
    if ( s_eqi(command(1:4),'dev = ') ) then
      dev = command(5:)
    else
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'Enter the graphics device desired.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'Options include:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CGMB  output to a CGM binary file.'
      write ( *, '(a)' ) 'PS    output to a PostScript file.'
      write ( *, '(a)' ) 'XWS   output to an X window screen.'
      read(*,'(a)') dev
    end if

    write ( *, '(a)' ) 'Output device set to ' // trim ( dev )
!
!  E: choose which costs will be plotted.
!
  else if ( s_eqi(command,'e') ) then

    call cost_show ( kcost, lcost, ncost )
!
!  F: show a frame around the plot.
!
  else if ( s_eqi(command,'f') ) then
  
    show(2) = .not. show(2)
    if ( show(2) ) then
      write ( *, '(a)' ) 'Frame will be shown in the next plot.'
    else
      write ( *, '(a)' ) 'Frame will NOT be shown in the next plot.'
    end if
!
!  H: help.
!
  else if ( s_eqi(command,'h') ) then
  
    call help
!
!  LINES: show lines.
!
  else if ( s_eqi(command,'l') ) then
  
    show(1) = .not.show(1)
    
    if ( show(1) ) then
      write ( *, '(a)' ) 'Lines will be shown in the next plot.'
    else
      write ( *, '(a)' ) 'Lines will NOT be shown in the next plot.'
    end if
!
!  M: set parameters.
!
  else if ( s_eqi(command,'m') ) then
  
    call setprm(iwrite,logy,ncon)
!
!  N: show nodes.
!
  else if ( s_eqi(command,'n') ) then
  
    show(3) = .not.show(3)
    if ( show(3) ) then
      write ( *, '(a)' ) 'Nodes will be shown in the next plot.'
    else
      write ( *, '(a)' ) 'Nodes will NOT be shown in the next plot.'
    end if
!
!  NMAX = : Choose maximum node to display.
!
  else if ( s_eqi(command(1:4),'nmax') ) then
 
    if ( s_eqi(command(1:5),'nmax = ') ) then
      read(command(6:),*)nmax
    else
      write ( *, '(a)' ) 'Enter maximum node.'
      read(*,*)nmax
    end if

    if ( nmax > nstep ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SLICE - Warning!'
      write ( *, * ) '  NMAX must be no more than ',nstep
      nmax = nstep
    end if

    cmin = minval ( cost(nmin:nmax) )
    cmax = maxval ( cost(nmin:nmax) )
!
!  NMIN = : Choose minimum node to display.
!
  else if ( s_eqi(command(1:4),'nmin') ) then
 
    if ( s_eqi(command(1:5),'nmin = ') ) then
      read(command(6:),*)nmin
    else
      write ( *, '(a)' ) 'Enter minimum node.'
      read(*,*)nmin
    end if

    if ( nmin <= 0 ) then
      write ( *, '(a)' ) 'Slice - Warning!'
      write ( *, '(a)' ) '  NMIN must be at least 1.'
      nmin = 1
    end if

    cmin = minval ( cost(nmin:nmax) )
    cmax = maxval ( cost(nmin:nmax) )
!
!  PRINT: Print out data.
!
  else if ( s_eqi(command,'p') ) then

    call printr(cost,dev,fildat,icolor,ifile,iplot,logy,maxobj,maxstp, &
      ncon,nstep,object,show,title,xsmax,xsmin,ysmax,ysmin)
!
!  Q: quit.
!
  else if ( s_eqi(command,'q') ) then
  
    write ( *, '(a)' ) 'Enter "y" to confirm you want to quit.'
    read(*,'(a)')command
    if ( lnei(command(1:1),'y'))go to 30

    write ( *, '(a)' ) 'SLICE is stopping now.'

    if ( iplot > 0 ) then
      call grfcls
    end if

    if ( ifile > 0 ) then
      close(unit = 2)
    end if

    if ( lnei(dev,'cgmb') ) then
      call delete('CGMOUT')
    end if

    stop
!
!  R: Plot the function value versus 1 parameter.
!
  else if ( s_eqi(command,'r') ) then

     write ( *, '(a)' ) ' '
     write ( *, '(a)' ) 'FGRAPH turned off right now.'
!
!  S: Plot the function values in sequence.
!
  else if ( s_eqi(command,'s') ) then

    call sgraph(cmax,cmin,cost,dev,icolor,iplot,maxobj, &
      maxstp,nmax,nmin,nstep,show,title,title2,xarray,yarray)
!
!  T: show or hide the title.
!
  else if ( s_eqi(command,'t') ) then
  
    show(4) = .not.show(4)
    
    if ( show(4) ) then
      write ( *, '(a)' ) 'The title will be shown in the next plot.'
    else
      write ( *, '(a)' ) 'The title will NOT be shown in the next plot.'
    end if
!
!  TITLE2 = : set subtitle
!
  else if ( s_eqi(command(1:6),'title2') ) then

    if ( s_eqi(command(1:7),'title2 = ') ) then
      title2 = command(8:)
    else if ( s_eqi(command,'title2') ) then
      write ( *, '(a)' ) 'Enter subtitle'
      read(*,'(a)')title2
    end if
!
!  TITLE = : set title
!
  else if ( s_eqi(command(1:5),'title') ) then

    if ( s_eqi(command(1:6),'title = ') ) then
      title = command(7:)
    else if ( s_eqi(command,'title') ) then
      write ( *, '(a)' ) 'Enter title'
      read(*,'(a)')title
    end if
!
!  X: set the data window.
!
  else if ( s_eqi(command,'x') ) then
  
    do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'Current data window coordinates are:'
      write ( *, '(a)' ) ' '
      write ( *, * ) 'X varies from ',xsmin,' to ',xsmax
      write ( *, * ) 'Y varies from ',ysmin,' to ',ysmax
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'Current picture window coordinates are:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'Enter new window coordinates for X and Y:'
      write ( *, '(a)' ) 'Use the order xmin, xmax, ymin, ymax:'

      read (*,*,iostat = ios) xsmin,xsmax,ysmin,ysmax

      if ( ios /= 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SLICE - Warning!'
        write ( *, '(a)' ) 'Your input was not acceptable!'
        write ( *, '(a)' ) 'Please try again!'
        write ( *, '(a)' ) ' '
      end if

      if ( ios == 0 ) then
        exit
      end if

    end do
!
!  Compute box containing data.
!
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'Current data window coordinates are:'
    write ( *, '(a)' ) ' '
    write ( *, * ) 'X varies from ',xsmin,' to ',xsmax
    write ( *, * ) 'Y varies from ',ysmin,' to ',ysmax
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'Current picture window coordinates are:'
    write ( *, '(a)' ) ' '
!
!  Unrecognized command.
!
  else
  
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SLICE - Warning!'
    write ( *, '(a)' ) 'Your command was not recognized.'
    
  end if
!
!  Go back, and get the next command.
!
  go to 30

   70 continue
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SLICE - Serious error!'
  write ( *, '(a)' ) 'The input file could not be opened.'
  write ( *, '(a)' ) ' '
  ifile = -1
  go to 30

end
subroutine box ( xmin, xmax, ymin, ymax )

!*****************************************************************************80
!
!! BOX draws a rectangle whose corners are specified by the user.
!
!  Discussion:
!
!    The rectangle drawn by box has the corners:
!
!      (XMIN,YMAX)   (XMAX,YMAX)
!
!      (XMIN,YMIN)   (XMAX,YMIN)
!
!    BOX can be used to place a rectangle anywhere in the picture.
!    However, BOX may also be used to place a rectangle around the
!    entire picture, producing a "frame".
!
!    The DRAWCGM routine PLYLIN is used to draw the box, and hence
!    the color of the line may be set by calling the DRAWCGM routine
!    LINCLR first.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real XMIN, XMAX, the minimum and maximum X coordinates.
!
!    Input, real YMIN, YMAX, the minimum and maximum Y coordinates.
!
  implicit none

  integer ( kind = 4 ), parameter :: npoints = 5

  real x(npoints)
  real xmax
  real xmin
  real y(npoints)
  real ymax
  real ymin

  x(1) = xmin
  y(1) = ymin

  x(2) = xmax
  y(2) = ymin

  x(3) = xmax
  y(3) = ymax

  x(4) = xmin
  y(4) = ymax

  x(5) = xmin
  y(5) = ymin

  call plylin ( npoints, x, y )

  return
end
subroutine buzz ( dev, x1min, x1max, y1min, y1max )

!*****************************************************************************80
!
!! BUZZ is just "busy work" to force XWS to draw the whole picture.
!
!  Discussion:
!
!    BUZZ seems to fix a problem that occurs when the XWS interface is
!    used.  In that case, the last bit of the graph is not drawn.  So
!    here, we just make the last bit of the graph something we don't
!    care about.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) DEV, the graphics output device:
!      cgmb - CGM binary file.
!      ps   - PostScript file.
!      XWS  - X window screen (interactive).
!
!    Input, real X1MIN, X1MAX, the minimum and maximum X
!    coordinates of the plot, which includes a small grace margin.
!
!    Input, real Y1MIN, Y1MAX, the minimum and maximum Y
!    coordinates of the plot, which includes a small grace margin.
!
  implicit none
!
  character ( len = * ) dev
  integer ( kind = 4 ) i
  integer ( kind = 4 ) icolor
  logical s_eqi
  real x1max
  real x1min
  real y1max
  real y1min

  if ( s_eqi ( dev, 'XWS' ) ) then

    icolor = 0
    icolor = 1
    call linclr ( icolor )

    do i = 1, 10000

      call box ( x1min, x1max, y1min, y1max )

    end do

  end if

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
function chrint(intval)

!*****************************************************************************80
!
!! CHRINT accepts an integer and returns in CHRINT the 6-character
!  representation of the integer, right justified, or '******' if 
!  the integer is too large or negative to fit in six positions.  
!
!  Compare CHLINT and CHR0NT.
!
!  Examples (all assuming that STRING has 6 characters):
!
!    INTVAL  STRING
!
!         1       1
!        -1      -1
!         0       0
!      1952    1952
!    123456  123456
!   1234567  ******  <-- Not enough room!  
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 July 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!  INTVAL Input, integer ( kind = 4 ) INTVAL, an integer variable to be 
!         converted.
!
!  CHRINT Output (through function value), character*6 CHRINT, a 6 
!         character representation of the integer, right justified. 
!         Thus, if INTVAL = 1, CHRINT='     1'.  CHRINT must be 
!         declared "character*6 CHRINT" in the calling program.
!
  implicit none

  character ( len = 6 ) chrint
  character ( len = 6 ) chrtmp
  integer ( kind = 4 ) intval

  if ( intval > 999999 ) then
    chrtmp = '******'
  else if ( intval < -99999 ) then
    chrtmp = '-*****'
  else
    write(chrtmp,'(i6)')intval
  end if

  chrint = chrtmp

  return
end
function chrrat(ival,jval)

!*****************************************************************************80
!
!! CHRRAT accepts a pair of integers IVAL and JVAL, and returns a 
!  right-justified representation of the ratio IVAL/JVAL.
!
!  If the ratio is negative, a minus sign precedes IVAL.
!  A slash separates IVAL and JVAL.
!
!  Compare CHLRAT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 July 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!  IVAL,
!  JVAL   Input, integer ( kind = 4 ) IVAL, JVAL, the two integers whose
!         ratio IVAL/JVAL is to be represented.
!
!         Note that if IVAL is nonzero and JVAL is 0, CHRRAT will 
!         be returned as "Inf" or "-Inf" (Infinity), and if both 
!         IVAL and JVAL are zero, CHRRAT will be returned as "NaN" 
!         (Not-a-Number).
!
!  CHRRAT Output, character*22 CHRRAT, a right-justified string
!         containing the representation of IVAL/JVAL.
!
  implicit none

  character ( len = 22 ) chrrat
  character ( len = 22 ) chrtmp
  integer ( kind = 4 ) ival
  integer ( kind = 4 ) ival2
  integer ( kind = 4 ) jval
  integer ( kind = 4 ) jval2
!
!  Take care of simple cases right away.
!
  if ( ival == 0 ) then

    if ( jval /= 0 ) then
      chrtmp = '0'
    else
      chrtmp = 'NaN'
    end if

  else if ( jval == 0 ) then

    if ( ival > 0 ) then
      chrtmp = 'Inf'
    else
      chrtmp = '-Inf'
    end if
!
!  Make copies of IVAL and JVAL.
!
  else
    ival2 = ival
    jval2 = jval

    if ( jval2 == 1 ) then
      write(chrtmp,'(i11)')ival2
    else
      write(chrtmp,'(i11,''/'',i10)')ival2,jval2
    end if
    call s_blank_delete(chrtmp)
  end if
!
!  Shift copy to flush right.
!
  chrrat = adjustr ( chrtmp )

  return
end
function chrrel(rval)

!*****************************************************************************80
!
!! CHRREL accepts a real number in RVAL and returns in CHRREL a
!  14-character right-justified representation of that number.
!
!  Compare CHLREL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 July 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!  RVAL   Input, real RVAL, a real number.
!
!  CHRREL Output (through function value), character*14 CHRREL,
!         a right-justified character variable containing the
!         representation of RVAL, using a G14.6 format.
!
  implicit none

  character ( len = 14 ) chrrel
  character ( len = 14 ) chrtmp
  real rval
!
!  We can't seem to write directly into CHRREL because of compiler
!  quibbles.
!
  if ( real(int(rval)) == rval.and. abs(rval) < 1.0E+13 ) then

    write(chrtmp,'(i14)')int(rval)

  else

    write(chrtmp,'(g14.6)')rval

  end if

  chrrel = chrtmp
  return
end
subroutine cost_show ( kcost, lcost, ncost )

!*****************************************************************************80
!
!! COST_SHOW allows the user to choose which cost functions will be visible.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 May 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) ncost

  integer ( kind = 4 ) ival
  integer ( kind = 4 ) kcost(ncost)
  character ( len = 40 ) lcost(ncost)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'Here is the current visibility status of costs:' 
  write ( *, '(a)' ) ' '
  
  call prcost ( kcost, lcost, ncost )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'Type:'

  do

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '-1 to quit'
    write ( *, '(a)' ) '0 to print'
    write ( *, '(a,i6,a)' ) 'N between 1 and ', ncost, ' to toggle a cost.'

    read ( *, * )ival
  
    if ( ival <= -1 ) then
      exit
    else if ( ival == 0 ) then
      call prcost ( kcost, lcost, ncost )
    else if ( 1 <= ival .and. ival <= ncost ) then
      kcost(ival) = 1 - kcost(ival)
    else
      write ( *, '(a)' ) 'Your input value was ignored.'
      write ( *, '(a)' ) 'It was too large.'
    end if
  
  end do

  return
end
subroutine delete ( filnam )

!*****************************************************************************80
!
!! DELETE is used to dispose of old copies of files before opening
!  a new copy.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 July 2009
!
!  Author:
!
!    John Burkardt
!
  implicit none

  character ( len = * ) filnam

  open(unit = 99,file=filnam,status='old',err=10)
  close(unit = 99,status='delete',err=10)
   10 continue

  return
end
subroutine drop2(nstep,nsave,xarray,yarray,xmin,xmax,ymin,ymax)

!*****************************************************************************80
!
!! DROP2 "crops" points that fall outside a given rectangle.
!
!  Discussion:
!
!    More specifically, DROP2 is given a pair of arrays XARRAY and
!    YARRAY, representing (X,Y) coordinates of points, and a set
!    of limits, XMIN, XMAX, YMIN, YMAX, representing a rectangle.
!
!    DROP2 removes each pair of values (XARRAY(I), YARRAY(I)) 
!    which falls outside the rectangle.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 July 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) nstep

  integer ( kind = 4 ) i
  integer ( kind = 4 ) nsave
  real xarray(nstep)
  real xmax
  real xmin
  real yarray(nstep)
  real ymax
  real ymin

  nsave = 0
  
  do i = 1,nstep
  
    if ( xmin <= xarray(i).and.xarray(i) <= xmax.and. &
       ymin <= yarray(i).and.yarray(i) <= ymax ) then
      nsave = nsave+1
      xarray(nsave) = xarray(i)
      yarray(nsave) = yarray(i)
    end if
    
  end do

  return
end
subroutine exheap(n,indx,i,j,isgn)

!*****************************************************************************80
!
!! EXHEAP externally sorts a list of items into linear order.
!  The actual list is not passed to the routine.  Hence it may
!  consist of integers, reals, numbers, names, etc.  The user,
!  after each return from EXHEAP, will be asked to compare or
!  interchange two items.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 July 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!  N      Input, integer ( kind = 4 ) N, the length of the input list.
!
!  INDX   Input/output, integer ( kind = 4 ) INDX.
!
!         The user must set INDX to 0 before the first call to
!         EXHEAP.
!
!         On return,
!
!           if INDX is greater than 0, the user must interchange
!           items I and J
!           and recall the routine.
!
!           If INDX is less than 0, the user is to compare items I
!           and J and return in ISGN a negative value if I is to
!           precede J, and a positive value otherwise.
!
!           If INDX is 0, the sorting is done.
!
!  I,
!  J      Output, integer ( kind = 4 ) I, J. On return with INDX positive,
!         elements I and J of the user's list should be
!         interchanged.  On return with INDX negative, elements I
!         and J are to be compared by the user.
!
!  ISGN   Input, integer ( kind = 4 ) ISGN. On return with INDX negative, the
!         user should compare elements I and J of the list.  If
!         item I is to precede item J, set ISGN negative,
!         otherwise set ISGN postive.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx
  integer ( kind = 4 ) isgn
  integer ( kind = 4 ) j
  integer ( kind = 4 ) l
  integer ( kind = 4 ) l1
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n1

  save l
  save l1
  save n1

  data l /0/
  data l1 /0/

  if ( indx < 0 ) then

    if ( indx /= -1 ) then
      if ( isgn < 0)i = i+1
      j = l1
      l1 = i
      indx = -1
      return
    end if

    if ( isgn <= 0)go to 20
    indx = 2
    return

  else if ( indx == 1 ) then

    l1 = l

  else if ( indx /= 2 ) then

    n1 = n
    l = n/2
    l1 = l

  end if
    
10    continue

  i = l1+l1
  
  if ( i == n1 ) then
    j = l1
    l1 = i
    indx = -1
    return
  else if ( i <= n1 ) then
    j = i+1
    indx = -2
    return
  end if

20    continue

  if ( l > 1 ) then
    l = l-1
    l1 = l
    go to 10
  end if
    
  if ( n1 == 1 ) then
    indx = 0
    return
  end if
  
  i = n1
  n1 = n1-1
  j = 1
  indx = 1
  return
end
subroutine fgraph ( cost, dev, icolor, ipar, iplot, logy, maxobj, &
  maxpar, maxstp, nstep, para, show, title, xarray, yarray )

!*****************************************************************************80
!
!! FGRAPH draws a graph of the function values versus a particular parameter.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) maxobj
  integer ( kind = 4 ) maxpar
  integer ( kind = 4 ) maxstp

  real angle
  real cost(0:maxstp)
  real csize
  real cwide
  character ( len = 10 ) dev
  logical filled
  real grace
  integer ( kind = 4 ) i
  integer ( kind = 4 ) icolor(maxobj)
  integer ( kind = 4 ) ipar
  integer ( kind = 4 ) iplot
  character isay
  integer ( kind = 4 ) lent
  logical s_eqi
  integer ( kind = 4 ) logy
  integer ( kind = 4 ) nsave
  integer ( kind = 4 ) nstep
  real para(0:maxstp,maxpar)
  real pwide
  logical show(maxobj)
  character ( len = * )  title
  real x
  real xarray(maxstp)
  real xgrace
  real xmax
  real xmin
  real xpmax
  real xpmin
  real xrange
  real xsmax
  real xsmin
  real y
  real yarray(maxstp)
  real ygrace
  real ymax
  real ymin
  real ypmax
  real ypmin
  real yrange
  real ysmax
  real ysmin
!
!  Update the count of pictures.
!
  iplot = iplot+1
!
!  If it's the first picture, then
!
!    Choose an output device,
!    Give the output file a name,
!    Initialize the graphics package.
!
!  If it's a later picture,
!
!    Issue a "new frame" command.
!
  if ( iplot == 1 ) then
    call device(dev)

    if ( s_eqi(dev,'cgmb') ) then
      call outfil('slice.cgm')
    else if ( dev == 'ps' ) then
      call outfil('slice.ps')
    end if

    call grfini
  else
    call newfrm
  end if
!
!  Copy data.
!
  do i = 1,nstep
    xarray(i) = para(i,ipar)
    yarray(i) = cost(i)
  end do
!
!  Get scales.
!
  xmin = minval ( xarray(1:nstep) )
  xmax = maxval ( xarray(1:nstep) )
  ymin = minval ( yarray(1:nstep) )
  ymax = maxval ( yarray(1:nstep) )
!
!  Sort data on the abscissa.
!
  write ( *, '(a)' ) 'Unsorted'
  do i = 1,nstep
    write(*,*)i,xarray(i),yarray(i)
  end do
  
  call rheap2(nstep,xarray,yarray)
  write ( *, '(a)' ) 'Sorted'
  do i = 1,nstep
    write(*,*)i,xarray(i),yarray(i)
  end do
!
!  Offer to restrict range.
!
  write ( *, * ) 'Current X range is ',xmin,' to ',xmax
  write ( *, * ) 'Current Y range is ',ymin,' to ',ymax
  write ( *, '(a)' ) 'Do you want to restrict the range?'
  read(*,'(a)')isay

  if ( s_eqi(isay,'y') ) then
    write ( *, '(a)' ) 'Enter new xmin, xmax, ymin, ymax'
    read(*,*)xmin,xmax,ymin,ymax
    call drop2(nstep,nsave,xarray,yarray,xmin,xmax,ymin,ymax)
    nstep = nsave
  end if
!
!  Get updated scales.
!
  xmin = minval ( xarray(1:nstep) )
  xmax = maxval ( xarray(1:nstep) )
  ymin = minval ( yarray(1:nstep) )
  ymax = maxval ( yarray(1:nstep) )
!
!  Scale damn data!!!!!!!!!!!!!!
!
  do i = 1,nstep
    xarray(i) = (xarray(i)-xmin)/(xmax-xmin)
    yarray(i) = (yarray(i)-ymin)/(ymax-ymin)
  end do
  
  xmax = 1.0E+00
  xmin = 0.0E+00
  ymax = 1.0E+00
  ymin = 0.0E+00

  xsmin = xmin
  xsmax = xmax
  ysmin = ymin
  ysmax = ymax

  if ( xsmax <= xsmin ) then
    xrange = 1
  else
    xrange = xsmax-xsmin
  end if
  
  if ( ysmax <= ysmin ) then
    yrange = 1
  else
    yrange = ysmax-ysmin
  end if
  
  grace = 0.05E+00
  xgrace = grace*xrange
  ygrace = grace*yrange
  csize = 0.01E+00*min(xrange,yrange)
  filled = .true.

  xpmin = xsmin-xgrace
  xpmax = xsmax+xgrace
  ypmin = ysmin-ygrace
  ypmax = ysmax+ygrace
!
!  Draw a box around the region.
!
  if ( show(2) ) then
    call linclr(icolor(2))
    call box(xpmin,xpmax,ypmin,ypmax)
  end if
!
!  Draw the title.
!
  if ( show(4) ) then

    lent = len_trim ( title )

    if ( lent > 0 ) then

      cwide = 0.5E+00*grace*xrange
      x = 0.5E+00*(xsmax+xsmin)-0.5E+00*lent*cwide
      y = ysmax+0.25E+00*grace*yrange
      call linclr(icolor(4))

      angle = 0.0E+00
      pwide = 1.0E+00
      call s_plot(angle,cwide,pwide,title(1:lent),x,y,'center')

    else

      if ( logy == 0 ) then
        title = 'Functional'
      else
        title = 'Logarithms of Functional'
      end if

      cwide = 0.5E+00*grace*xrange
      x = 0.5E+00*(xsmax+xsmin)-0.5E+00*lent*cwide
      y = ysmax+0.5E+00*grace*yrange
      call linclr(icolor(4))

      angle = 0.0E+00
      pwide = 1.0E+00
      call s_plot(angle,cwide,pwide,title(1:lent),x,y,'center')
      title = ' '
    end if

  end if
!
!  Draw the axes.
!
  if ( show(5) ) then
    call linclr(icolor(5))
    call movcgm(xsmin,ysmin)
    call drwcgm(xsmin,ysmax)
    call movcgm(xsmin,ysmin)
    call drwcgm(xsmax,ysmin)
  end if
!
!  Draw the line.
!
  if ( show(1) ) then
    call linclr(icolor(1))
    call plylin(nstep,xarray,yarray)
  end if
!
!  Draw the nodes.
!
  if ( show(3) ) then
    write ( *, '(a)' ) 'drawing nodes'
    call linclr(icolor(3))
    call filclr(icolor(3))
    call ncircle(nstep,xarray,yarray,csize,filled)
  end if
!
!  The following loop is just "busy work" which seems to fix a 
!  problem that occurs when the XWS interface is used.  In that 
!  case, the last bit of the graph is not drawn.  So here, we just 
!  make the last bit of the graph something we don't care about.
!
  if ( show(2) ) then
    call linclr(icolor(2))
  else
    call linclr(0)
  end if

  do i = 1,100
    call box(xpmin,xpmax,ypmin,ypmax)
  end do
 
  return
end
subroutine getdat ( cmax, cmin, cost, ifile, maxstp, nmax, nmin, nstep )

!*****************************************************************************80
!
!! GETDAT reads in data. 
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) maxstp

  real cmax
  real cmin
  real cost(maxstp)
  integer ( kind = 4 ) ifile
  integer ( kind = 4 ) ios
  integer ( kind = 4 ) nmax
  integer ( kind = 4 ) nmin
  integer ( kind = 4 ) nstep
  real temp

  nstep = 0

  do

    read ( 2, *, iostat = ios ) temp

    if ( ios /= 0 ) then
      exit
    end if

    nstep = nstep + 1

    if ( nstep > maxstp ) then
      write ( *, '(a)' ) 'GETDAT - Cannot read any more records.'
      exit
    end if

    cost(nstep) = temp

  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'GETDAT read ',nstep,' records.'
  ifile = 2

  cmin = minval ( cost(1:nstep) )
  cmax = maxval ( cost(1:nstep) )

  nmin = 1
  nmax = nstep

  return
end
subroutine help

!*****************************************************************************80
!
!! HELP prints out a list of commands.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 July 2009
!
!  Author:
!
!    John Burkardt
!
  implicit none

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'Commands:'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'A      show axes in next plot.'
  write ( *, '(a)' ) 'F      show frame in next plot.'
  write ( *, '(a)' ) 'L      show lines in next plot.'
  write ( *, '(a)' ) 'N      show nodes in next plot.'
  write ( *, '(a)' ) 'T      show title in next plot.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'M      set parameters.'
  write ( *, '(a)' ) 'P      print out data.'
  write ( *, '(a)' ) 'CMAX =   set maximum cost to show.'
  write ( *, '(a)' ) 'CMIN =   set minimum cost to show.'
  write ( *, '(a)' ) 'DAT =    specify the input data file.'
  write ( *, '(a)' ) 'DEV =    set graphics output device.'
  write ( *, '(a)' ) 'NMAX =   set maximum node to show.'
  write ( *, '(a)' ) 'NMIN =   set minimum node to show.'
  write ( *, '(a)' ) 'C      choose colors.'
  write ( *, '(a)' ) 'E      choose which costs will be plotted.'
  write ( *, '(a)' ) 'TITLE =  set the plot title.'
  write ( *, '(a)' ) 'X      choose X, Y window.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R      Plot function values versus a parameter.'
  write ( *, '(a)' ) 'S      Plot the sequence of function values.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'H      help (print this list)'
  write ( *, '(a)' ) 'Q      quit.'
  write ( *, '(a)' ) ' '

  return
end
subroutine init(cost,dev,fildat,grace,icolor,ifile,iplot,iwrite, &
  kcost,lcost,logy,maxobj,maxstp,ncon,ncost,object,show,title, &
  title2,xsmax,xsmin,ysmax,ysmin)

!*****************************************************************************80
!
!! INIT initializes the values of data.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 July 2009
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) maxobj
  integer ( kind = 4 ) maxstp
  integer ( kind = 4 ) ncost

  real cost(maxstp)
  character ( len = 10 ) dev
  character ( len = 40 ) fildat
  real grace
  integer ( kind = 4 ) i
  integer ( kind = 4 ) icolor(maxobj)
  integer ( kind = 4 ) ifile
  integer ( kind = 4 ) iplot
  integer ( kind = 4 ) iwrite
  integer ( kind = 4 ) kcost(ncost)
  character ( len = 40 ) lcost(ncost)
  integer ( kind = 4 ) logy
  integer ( kind = 4 ) ncon
  character ( len = 12 ) object(maxobj)
  logical show(maxobj)
  character ( len = 40 ) title
  character ( len = 40 ) title2
  real xsmax
  real xsmin
  real ysmax
  real ysmin

  cost(1:maxstp) = 0.0E+00
  dev = 'xws'
  fildat = 'pfg.dat'
  grace = 0.10E+00
  icolor(1:maxobj) = 1
  icolor(2) = 2
  icolor(3) = 3
  icolor(4) = 4
  ifile = 0
  iplot = 0
  iwrite = 0
  kcost(1) = 1
  kcost(2:ncost) = 0
  lcost(1) = 'Total cost'
  logy = 0
  ncon = 12
  object(1) = 'lines'
  object(2) = 'frame'
  object(3) = 'nodes'
  object(4) = 'title'
  object(5) = 'axes'
  show(1:maxobj) = .true.  
  title = ' '
  title2 = ' '
  xsmax = 0.0E+00
  xsmin = 0.0E+00
  ysmax = 0.0E+00
  ysmin = 0.0E+00

  return
end
function lnei(strng1,strng2)

!*****************************************************************************80
! 
!! LNEI is a case-insensitive comparison of two strings for non-equality.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 July 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!  STRNG1,
!  STRNG2 Input, character*(*) STRNG1, STRNG2, the strings to 
!         compare.
!
!  LNEI   Output, logical LNEI, the result of the comparison.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) len1
  integer ( kind = 4 ) len2
  integer ( kind = 4 ) lenc
  logical lnei
  character null
  character s1
  character s2
  character ( len = * )  strng1
  character ( len = * )  strng2

  len1 = len(strng1)
  len2 = len(strng2)
  lenc = min(len1,len2)

  lnei = .true.

  do i = 1,lenc
    s1 = strng1(i:i)
    s2 = strng2(i:i)
    call s_cap ( s1)
    call s_cap ( s2)
    if ( s1 /= s2)return
  end do

  null = char(0)

  do i = lenc+1,len1
    if ( strng1(i:i) /= ' '.and. strng1(i:i) /= null)return
  end do

  do i = lenc+1,len2
    if ( strng2(i:i) /= ' '.and. strng2(i:i) /= null)return
  end do

  lnei = .false.

  return
end
subroutine ncircle ( nval, xval, yval, csize, filled )

!*****************************************************************************80
!
!! NCIRCLE draws circles at a set of points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 July 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) nval

  real csize
  logical filled
  integer ( kind = 4 ) i
  real xval(nval)
  real yval(nval)

  do i = 1, nval
    call circle ( xval(i), yval(i), csize, filled )
  end do
   
  return
end
subroutine prange(cmax,cmin,maxpar,ncost,npara,xrange)

!*****************************************************************************80
!
!! PRANGE prints the ranges of the data.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 July 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) maxpar
  integer ( kind = 4 ) ncost

  real cmax(ncost)
  real cmin(ncost)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) npara
  real xrange(2,maxpar)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'PRANGE:'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'Parameter  Minimum       Maximum'
  write ( *, '(a)' ) ' '
  do i = 1, npara
    write(*,'(i9,g14.6,g14.6)')i,xrange(1,i),xrange(2,i)
  end do
  
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'Function   Minimum       Maximum'
  write ( *, '(a)' ) ' '
  do i = 1,ncost
    write(*,'(i2,7x,g14.6,g14.6)')i,cmin(i),cmax(i)
  end do

  return
end
subroutine prcost(kcost,lcost,ncost)

!*****************************************************************************80
!
!! PRCOST prints out the visibility of the various costs.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!!  Modified:
!
!    17 July 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) ncost

  integer ( kind = 4 ) i
  integer ( kind = 4 ) kcost(ncost)
  character ( len = 40 ) lcost(ncost)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'Which cost functions will be graphed?'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'Visible?   Cost Name'
  write ( *, '(a)' ) ' '
  do i = 1,ncost
    if ( kcost(i) == 0 ) then
      write(*,'(i2,2x,a3,2x,a40)')i,'No ',lcost(i)
    else
      write(*,'(i2,2x,a3,2x,a40)')i,'Yes',lcost(i)
    end if
  end do

  return
end
subroutine printr ( cost, dev, fildat, icolor, ifile, iplot, logy, &
  maxobj, maxstp, ncon, nstep, object, show, title, xsmax, xsmin, &
  ysmax, ysmin )

!*****************************************************************************80
!
!! PRINTR prints out information about the program data.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 July 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) maxobj
  integer ( kind = 4 ) maxstp

  real cost(maxstp)
  character ( len = 10 ) dev
  character ( len = 40 ) fildat
  integer ( kind = 4 ) i
  integer ( kind = 4 ) icolor(maxobj)
  integer ( kind = 4 ) ifile
  integer ( kind = 4 ) iplot
  integer ( kind = 4 ) lent
  integer ( kind = 4 ) logy
  integer ( kind = 4 ) ncon
  integer ( kind = 4 ) nstep
  character ( len = 12 ) object(maxobj)
  logical show(maxobj)
  character ( len = * )  title
  real xsmax
  real xsmin
  real ysmax
  real ysmin
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'Information about the problem:'
  write ( *, '(a)' ) ' '

  if ( ifile == 0 ) then
    write ( *, '(a)' ) 'No data file has been opened.'
  else if ( ifile == 1 ) then
    write ( *, '(a)' ) 'A data file has been opened, named'
    write(*,'(a)')fildat
  else if ( ifile == 2 ) then
    write ( *, '(a)' ) 'A data file has been read, named'
    write(*,'(a)')fildat
  end if
  write ( *, '(a)' ) ' '

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'Current data window coordinates are:'
  write ( *, '(a)' ) ' '
  write ( *, * ) 'X varies from ',xsmin,' to ',xsmax
  write ( *, * ) 'Y varies from ',ysmin,' to ',ysmax
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'Current picture window coordinates are:'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'RETURN for more:'
  read(*,*)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'Plotting information:'
  write ( *, '(a)' ) ' '
  write ( *, * ) 'A total of ',iplot,' plots have been made so far.'
  write ( *, * ) 'Graphics output is of type ',dev
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'Current choices for plotting:'
  write ( *, '(a)' ) ' '
  write ( *, * ) ncon,' contour levels will be used.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) ' Object    Visible?    Color'
  write ( *, '(a)' ) ' '
  do i = 1,maxobj
    write(*,'(a12,2x,l1,2x,i3)')object(i),show(i),icolor(i)
  end do

  if ( len_trim ( title ) > 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'title:'
    write ( *, '(a)' ) trim ( title )
  else
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'No title has been assigned.'
  end if

  write ( *, '(a)' ) ' '

  write ( *, '(a)' ) ' '
  if ( logy == 0 ) then
    write ( *, '(a)' ) 'Plots will be X versus Y.'
  else
    write ( *, '(a)' ) 'Plots will be X versus LOG(Y).'
  end if

  if ( ifile /= 2 ) then
    write ( *, '(a)' ) 'No data has been read yet!'
    return
  end if
!
!  Cost function.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'Cost function values:'
  write ( *, '(a)' ) ' '
  do i = 1,nstep
    write(*,*)i,cost(i)
  end do

  return
end
subroutine range2(maxpar,maxstp,npara,nstep,para,xrange)

!*****************************************************************************80
!
!! RANGE2 sets the ranges of an array.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 July 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) maxpar
  integer ( kind = 4 ) maxstp

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) npara
  integer ( kind = 4 ) nstep
  real para(0:maxstp,maxpar)
  real xrange(2,maxpar)

  xrange(1,1:npara) = para(0,1:npara)
  xrange(2,1:npara) = para(0,1:npara)

  do i = 0,nstep
    do j = 1,npara
      xrange(1,j) = min(xrange(1,j),para(i,j))
      xrange(2,j) = max(xrange(2,j),para(i,j))
    end do
  end do

  return
end
subroutine rheap2(n,xarray,yarray)

!*****************************************************************************80
!
!! RHEAP2 sorts an array of reals into nondecreasing order, and
!  shifts a second array to correspond to the changes made in
!  the first array.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 July 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!  N      Input, integer ( kind = 4 ) N, length of input array.
!
!  XARRAY Input/output, real XARRAY(N).  On input, an unsorted 
!         array of reals. On output, XARRAY has been sorted.
!
!  YARRAY Input/output, real YARRAY(N), an array which is to be
!         shifted corresponding to the shifts made in XARRAY.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx
  integer ( kind = 4 ) isgn
  integer ( kind = 4 ) j
  real temp
  real xarray(n)
  real yarray(n)

  i = 0
  indx = 0
  isgn = 0
  j = 0
  
10    continue

  call exheap(n,indx,i,j,isgn)

  if ( indx > 0 ) then

    temp = xarray(i)
    xarray(i) = xarray(j)
    xarray(j) = temp

    temp = yarray(i)
    yarray(i) = yarray(j)
    yarray(j) = temp

    go to 10

  else if ( indx < 0 ) then

    if ( xarray(i) <= xarray(j) ) then
      isgn = -1
    else
      isgn = +1
    end if

    go to 10
    
  end if

  return
end
subroutine s_blank_delete ( s )

!*****************************************************************************80
!
!! S_BLANK_DELETE removes blanks from a string, left justifying the remainder.
!
!  Discussion:
!
!    All TAB characters are also removed.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 July 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, the string to be transformed.
!
  implicit none

  character c
  integer ( kind = 4 ) iget
  integer ( kind = 4 ) iput
  integer ( kind = 4 ) nchar
  character ( len = * ) s
  character, parameter :: TAB = char ( 9 )

  iput = 0
  nchar = len_trim ( s )

  do iget = 1, nchar

    c = s(iget:iget)

    if ( c /= ' ' .and. c /= TAB ) then
      iput = iput + 1
      s(iput:iput) = c
    end if

  end do

  s(iput+1:nchar) = ' '

  return
end
subroutine s_cap ( s )

!*****************************************************************************80
!
!! S_CAP replaces any lowercase letters by uppercase ones in a string.
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
!    Input/output, character ( len = * ) S, the string to be transformed.
!
  implicit none

  character c
  integer ( kind = 4 ) i
  integer ( kind = 4 ) nchar
  character ( len = * ) s

  nchar = len_trim ( s )

  do i = 1, nchar

    c = s(i:i)
    call ch_cap ( c )
    s(i:i) = c

  end do

  return
end
function s_eqi ( s1, s2 )

!*****************************************************************************80
!
!! S_EQI is a case insensitive comparison of two strings for equality.
!
!  Example:
!
!    S_EQI ( 'Anjana', 'ANJANA' ) is .TRUE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S1, S2, the strings to compare.
!
!    Output, logical S_EQI, the result of the comparison.
!
  implicit none

  character c1
  character c2
  integer ( kind = 4 ) i
  integer ( kind = 4 ) len1
  integer ( kind = 4 ) len2
  integer ( kind = 4 ) lenc
  logical s_eqi
  character ( len = * ) s1
  character ( len = * ) s2

  len1 = len ( s1 )
  len2 = len ( s2 )
  lenc = min ( len1, len2 )

  s_eqi = .false.

  do i = 1, lenc

    c1 = s1(i:i)
    c2 = s2(i:i)
    call ch_cap ( c1 )
    call ch_cap ( c2 )

    if ( c1 /= c2 ) then
      return
    end if

  end do

  do i = lenc + 1, len1
    if ( s1(i:i) /= ' ' ) then
      return
    end if
  end do

  do i = lenc + 1, len2
    if ( s2(i:i) /= ' ' ) then
      return
    end if
  end do

  s_eqi = .true.

  return
end
subroutine s_plot ( angle, cwide, pwide, s, x, y, flush )

!*****************************************************************************80
!
!! S_PLOT plots a character string onto a graphics image.
!
!  Discussion:
!
!    The string can be at any angle and at any size.
!
!    The plot is assumed to be of size PWIDE by PHITE, although PHITE
!    itself is not input.
!
!    This routine must be modified to work with a particular graphics package.
!    The current code calls two routines:
!      MOVCGM ( X, Y ) moves to a point (X,Y) in the plot;
!      DRWCGM ( X, Y ) draws a line from the current point to (X,Y).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ANGLE, the angle in degrees at which the
!    string is to be drawn.  0 is typical.  90 degrees would
!    cause the string to be written from top to bottom.
!
!    Input, real CWIDE, the width of the characters.  This
!    is measured in the same units as the plot width PWIDE.
!    For PWIDE = 1, a plot size of 0.025 would be reasonable,
!    since 40 characters would fit, but 2.0 would be nonsense.
!
!    Input, real PWIDE, the width of the plot, in the same
!    units as CWIDE.
!
!    Input, character ( len = * ) S, contains the text to be plotted.
!    Only characters with ASCII codes between 32 and 126 will actually
!    be plotted.  Any other characters are "unprintable", and will be
!    plotted as blanks.
!
!    Input, real X, Y, the coordinates of a point which
!    determines where the string is drawn.  The string will
!    be drawn starting at, centered or, or ending at (X,Y),
!    depending on the value of FLUSH.
!
!    Input, character ( len = * ) FLUSH, a string which specifies how to
!    place the string.  Only the first character of FLUSH is examined, and
!    the case of the character is not important.
!    'L' - the string will be drawn flush left.
!    'C' - the string will be centered.
!    'R' - the string will be drawn flush right.
!
  implicit none

  real, parameter :: PI = 3.1415926535E+00
  real, parameter :: DEG_TO_RAD = PI / 180.0E+00

  real angle
  real ca
  character c
  real cwide
  character ( len = * ) flush
  integer ( kind = 4 ) i
  integer ( kind = 4 ) iascii
  integer ( kind = 4 ) icr
  integer ( kind = 4 ), save, dimension ( 1617 ) :: ifont
  integer ( kind = 4 ) ip
  integer ( kind = 4 ) ipen
  integer ( kind = 4 ), save, dimension ( 95 ) :: ipoint
  integer ( kind = 4 ) iv
  integer ( kind = 4 ) nchar
  integer ( kind = 4 ) nmax
  integer ( kind = 4 ) nvec
  real pwide
  logical rotate
  character ( len = * ) s
  real sa
  real scl2
  real x
  real xb
  real xc
  real xcopy
  real xnew
  real xold
  real xrot
  real xt
  real y
  real yb
  real yc
  real ycopy
  real ynew
  real yold
  real yrot
  real yt
!
!  IPOINT is a pointer array into IFONT.
!
!  IPOINT(I) records where the "strokes" for character I begin
!  in the IFONT array.
!
  data ( ipoint(i), i = 1, 95 ) / &
   1,   3,  26,  45,  66, 102, 130, 156, 166, 186, 206, 222, 233, &
     249, 255, 267, 273, 293, 306, 328, 353, 363, 383, 411, 423, 457, &
     483, 506, 533, 541, 552, 560, 587, 625, 638, 665, 683, 699, 714, &
     727, 754, 770, 786, 805, 818, 826, 838, 848, 868, 884, 909, 930, &
     956, 967, 981, 989,1001,1012,1025,1035,1045,1051,1061,1069,1075, &
    1081,1108,1131,1149,1172,1194,1214,1243,1260,1284,1307,1323,1336, &
    1364,1381,1401,1424,1447,1464,1486,1499,1516,1524,1536,1547,1560, &
    1570,1586,1592,1608 /
!
!  IFONT contains the strokes defining the various symbols.
!
  data ( ifont(i), i = 1, 396 ) / &
     1, 0, 2,10,11, 9,22,10,23,11,22,10,11, 0, 9, 7, 9, 9,11, 9,11, 7, &
     9, 7, 0, 2, 8,17, 7,23, 9,23, 8,17, 0,14,17,13,23,15,23,14,17, 0, &
     4, 9,23, 7, 7, 0,13,23,11, 7, 0, 5,17,15,17, 0, 5,13,15,13, 0, 3, &
    15,19,13,21, 9,21, 7,19, 7,17, 9,15,13,15,15,13,15,11,13, 9, 9, 9, &
     7,11, 0, 9,23, 9, 7, 0,13,23,13, 7, 0, 3, 5,23, 9,23, 9,19, 5,19, &
     5,23, 0,17,23, 5, 7, 0,13, 7,13,11,17,11,17, 7,13, 7, 0, 1,17, 7, &
     7,17, 7,19, 9,21,13,21,15,19,15,17, 5,13, 5,11, 9, 7,13, 7,17,15, &
     0, 1,10,17, 9,23,11,23,10,17, 0, 1,12,23,11,21,10,19, 9,17, 9,15, &
     9,13,10,11,11, 9,12, 7, 0, 1,12,23,13,21,14,19,15,17,15,15,15,13, &
    14,11,13, 9,12, 7, 0, 3, 7,15,15,15, 0,13,19, 9,11, 0, 9,19,13,11, &
     0, 2, 7,15,15,15, 0,11,19,11,11, 0, 1,11, 7, 9, 7, 9, 9,11, 9,11, &
     7,11, 6,10, 4, 0, 1, 7,15,15,15, 0, 1, 9, 7, 9, 9,11, 9,11, 7, 9, &
     7, 0, 1,15,23, 7, 7, 0, 1, 9,23,13,23,15,19,15,11,13, 7, 9, 7, 7, &
    11, 7,19, 9,23, 0, 2, 7,21, 9,23, 9, 7, 0, 7, 7,11, 7, 0, 1, 5,21, &
     9,23,15,23,17,21,17,19,15,17, 7,13, 5,10, 5, 7,17, 7, 0, 2, 5,23, &
    17,23,15,17,13,15, 9,15, 0,13,15,17,13,17,10,14, 7, 8, 7, 5,10, 0, &
     1,13, 7,13,23, 5,13,17,13, 0, 1,17,23, 5,23, 5,17,13,17,17,15,17, &
    11,13, 7, 9, 7, 5,11, 0, 1,17,19,13,23, 9,23, 5,19, 5,13, 9,15,13 /

  data ( ifont(i), i =  397, 792 ) / &
    15,17,13,17,11,13, 7, 9, 7, 5,11, 5,13, 0, 1, 5,19, 5,23,17,23,11, &
    15,11, 7, 0, 1, 8,15, 6,17, 6,21, 8,23,14,23,16,21,16,17,14,15, 8, &
    15, 5,13, 5, 9, 8, 7,14, 7,17, 9,17,13,14,15, 0, 1,17,17,15,15, 7, &
    15, 5,17, 5,21, 7,23,15,23,17,21,17,11,15, 7, 7, 7, 5,11, 0, 2, 9, &
    13, 9,15,11,15,11,13, 9,13, 0, 9, 7, 9, 9,11, 9,11, 7, 9, 7, 0, 2, &
     9,13, 9,15,11,15,11,13, 9,13, 0,11, 7, 9, 7, 9, 9,11, 9,11, 7,11, &
     6,10, 4, 0, 1,17,21, 5,15,17, 9, 0, 2, 7,15,15,15, 0, 7, 9,15, 9, &
     0, 1, 5,21,17,15, 5, 9, 0, 2, 7,21, 9,23,13,23,15,21,15,19,11,15, &
    11,11, 0,10, 7,10, 9,12, 9,12, 7,10, 7, 0, 1,13, 7, 9, 7, 5,11, 5, &
    19, 9,23,13,23,17,19,17,11,15, 9,13,11,12,10,10,10, 9,11, 9,15,10, &
    16,12,16,13,15,13,11, 0, 2, 5, 7,11,23,17, 7, 0, 8,15,14,15, 0, 2, &
     5, 7, 5,23,15,23,17,21,17,17,15,15, 5,15, 0,15,15,17,13,17, 9,15, &
     7, 5, 7, 0, 1,17,19,13,23, 9,23, 5,19, 5,11, 9, 7,13, 7,17,11, 0, &
     1, 5, 7, 5,23,13,23,17,19,17,11,13, 7, 5, 7, 0, 2,17,23, 5,23, 5, &
     7,17, 7, 0, 5,15,12,15, 0, 2, 5, 7, 5,23,17,23, 0, 5,15,12,15, 0, &
     2,17,19,13,23, 9,23, 5,19, 5,11, 9, 7,13, 7,17,11,17,15,13,15, 0, &
    17,11,17, 7, 0, 3, 5, 7, 5,23, 0, 5,15,17,15, 0,17,23,17, 7, 0, 3, &
     9,23,13,23, 0,11,23,11, 7, 0, 9, 7,13, 7, 0, 2,15,23,15,11,12, 7 /

  data ( ifont(i), i =  793, 1188 ) / &
     8, 7, 5,11, 5,13, 0,13,23,17,23, 0, 2, 5, 7, 5,23, 0,17,23, 5,15, &
    17, 7, 0, 1, 5,23, 5, 7,17, 7, 0, 1, 5, 7, 5,23,11,11,17,23,17, 7, &
     0, 1, 5, 7, 5,23,17, 7,17,23, 0, 1,17,19,13,23, 9,23, 5,19, 5,11, &
     9, 7,13, 7,17,11,17,19, 0, 1, 5, 7, 5,23,13,23,17,21,17,17,13,15, &
     5,15, 0, 2,17,19,13,23, 9,23, 5,19, 5,11, 9, 7,13, 7,17,11,17,19, &
     0,13,11,17, 7, 0, 2, 5, 7, 5,23,13,23,17,21,17,17,13,15, 5,15, 0, &
    13,15,17, 7, 0, 1,17,19,13,23, 9,23, 5,20, 5,18, 9,15,13,15,17,12, &
    17,10,13, 7, 9, 7, 5,10, 0, 2, 5,23,17,23, 0,11,23,11, 7, 0, 1, 5, &
    23, 5,10, 8, 7,14, 7,17,10,17,23, 0, 1, 5,23,11, 7,17,23, 0, 1, 5, &
    23, 8, 7,11,17,14, 7,17,23, 0, 2, 5,23,17, 7, 0,17,23, 5, 7, 0, 2, &
     5,23,11,13,17,23, 0,11,13,11, 7, 0, 1, 5,23,17,23, 5, 7,17, 7, 0, &
     1,11,23, 7,23, 7, 7,11, 7, 0, 1, 7,23,15, 7, 0, 1, 7,23,11,23,11, &
     7, 7, 7, 0, 1, 7,21,11,23,15,21, 0, 1, 5, 3,17, 3, 0, 1, 9,23,13, &
    19, 0, 2, 7,14, 9,15,13,15,15,14,15, 7, 0,15,12, 9,12, 7,11, 7, 8, &
     9, 7,13, 7,15, 8, 0, 2, 7,23, 7, 7, 0, 7,13, 9,15,13,15,15,13,15, &
     9,13, 7, 9, 7, 7, 9, 0, 1,15,13,13,15, 9,15, 7,13, 7, 9, 9, 7,13, &
     7,15, 9, 0, 2,15,13,13,15, 9,15, 7,13, 7, 9, 9, 7,13, 7,15, 9, 0, &
    15,23,15, 7, 0, 1, 7,11,15,11,15,13,13,15, 9,15, 7,13, 7, 9, 9, 7 /

  data ( ifont(i), i = 1189, 1584 ) / &
    13, 7,15, 9, 0, 3, 9, 7, 9,23,13,23,13,22, 0, 8,15,12,15, 0, 8, 7, &
    11, 7, 0, 2,15,13,13,15, 9,15, 7,13, 7, 9, 9, 7,13, 7,15, 9, 0,15, &
    13,15, 3,13, 1, 9, 1, 7, 3, 0, 2, 7, 7, 7,23, 0, 7,14, 9,15,13,15, &
    15,14,15, 7, 0, 3, 9,15,11,15,11, 7, 0, 9, 7,13, 7, 0, 9,17, 9,19, &
    11,19,11,17, 9,17, 0, 2, 9,15,11,15,11, 1, 7, 1, 7, 3, 0, 9,17,11, &
    17,11,19, 9,19, 9,17, 0, 3, 7, 7, 7,23, 0,15,15, 7,10, 0, 9,11,15, &
     7, 0, 2, 9,23,11,23,11, 7, 0, 9, 7,13, 7, 0, 3, 7,15, 7, 7, 0, 7, &
    14, 8,15,10,15,11,14,11, 7, 0,11,14,12,15,14,15,15,14,15, 7, 0, 2, &
     7, 7, 7,15, 0, 7,14, 9,15,13,15,15,14,15, 7, 0, 1, 7,13, 9,15,13, &
    15,15,13,15, 9,13, 7, 9, 7, 7, 9, 7,13, 0, 2, 7,13, 9,15,13,15,15, &
    13,15, 9,13, 7, 9, 7, 7, 9, 0, 7,14, 7, 1, 0, 2,15,13,13,15, 9,15, &
     7,13, 7, 9, 9, 7,13, 7,15, 9, 0,15,14,15, 1, 0, 2, 7,15, 9,15, 9, &
     7, 0, 9,13,11,15,13,15,15,13, 0, 1,15,13,13,15, 9,15, 7,13, 9,11, &
    13,11,15, 9,13, 7, 9, 7, 7, 9, 0, 2, 9,23, 9, 7,11, 7, 0, 7,17,11, &
    17, 0, 2, 7,15, 7, 9, 9, 7,13, 7,15, 9, 0,15,15,15, 7, 0, 1, 7,15, &
    11, 7,15,15, 0, 1, 7,15, 9, 7,11,11,13, 7,15,15, 0, 2, 7,15,15, 7, &
     0, 7, 7,15,15, 0, 2, 7,15,11, 7, 0,15,15,10, 5, 7, 1, 0, 1, 7,15, &
    15,15, 7, 7,15, 7, 0, 1,11,23, 7,23, 9,17, 7,15, 9,13, 7, 7,11, 7 /

  data ( ifont(i), i = 1585, 1617 ) / &
     0, 1, 9,23, 9, 7, 0, 1, 7,23,11,23, 9,17,11,15, 9,13,11, 7, 7, 7, &
     0, 1, 5,21, 7,23,15,21,17,23, 0 /

  nchar = len_trim ( s )

  if ( pwide <= 0.0E+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'S_PLOT - Serious error!'
    write ( *, '(a)' ) '  The plot width PWIDE is negative!'
    write ( *, * ) '  PWIDE = ', pwide
    return
  end if
!
!  Chop titles that are too long.  To do this, we need to know the
!  width of the plot (PWIDE) in same units as CWIDE.
!
  nmax = int ( pwide / cwide )

  if ( nchar > nmax ) then
    nchar = nmax
  end if
!
!  Shift string if centering or right flush option used.
!
  if ( flush(1:1) == 'l' .or. flush(1:1) == 'L' ) then
    xcopy = x
    ycopy = y
  else if ( flush(1:1) == 'c' .or. flush(1:1) == 'C' ) then
    xcopy = x - 0.5E+00 * nchar * cwide * cos ( angle * DEG_TO_RAD )
    ycopy = y - 0.5E+00 * nchar * cwide * sin ( angle* DEG_TO_RAD )
  else if ( flush(1:1) == 'r' .or. flush(1:1) == 'R' ) then
    xcopy = x - nchar * cwide * cos ( angle * DEG_TO_RAD )
    ycopy = y - nchar * cwide * sin ( angle * DEG_TO_RAD )
  else
    xcopy = x
    ycopy = y
  end if
!
!  Note that screen coordinates are used.
!  Thus a width of 0.1 is intended to mean 1/10 of screen size.
!
!  Set the scale factor for character height.
!
  scl2 = cwide / 16.0E+00
!
!  Set the starting point for the line of text, the lower left
!  corner of the first character.
!
!  Set the origin about which rotation is performed.
!
  xb = xcopy
  xrot = xcopy
  yb = ycopy
  yrot = ycopy
!
!  Get trig functions if rotation required, converting from
!  degrees to radians.
!
  if ( angle == 0.0E+00 ) then
    rotate = .false.
  else
    ca = cos ( angle * DEG_TO_RAD )
    sa = sin ( angle * DEG_TO_RAD )
    rotate = .true.
  end if
!
!  Loop over all characters in the string.
!
  do icr = 1, nchar

    xold = x
    yold = y
    xnew = x
    ynew = y
!
!  Get the ASCII code for the character and shift by 31 so that
!  the first printable character becomes code 1.
!
    c = s(icr:icr)
    iascii = ichar ( c ) - 31
!
!  Replace any nonprintable characters with blanks.
!
    if ( iascii < 1 .or. iascii > 95 ) then
      iascii = 1
    end if
!
!  Get the pointer to this character in font table.
!
    ip = ipoint(iascii)
!
!  Get the number of "vectors" required to draw the character.
!  Here "vectors" means the number of times the pen is lowered, not
!  the number of pen strokes.
!
!  For blanks, this number is 1, due to the way the
!  algorithm is coded.
!
    nvec = ifont(ip)
!
!  Loop over all required pen movements.
!
    do iv = 1, nvec

  ipen = 3
  ip = ip + 1

  do while ( ifont(ip) /= 0 )

    xc = xb + scl2 * ( ifont(ip) - 1 )
    yc = yb + scl2 * ( ifont(ip+1) - 7 )
!
!  Apply rotation if necessary.
!
    if ( rotate ) then
      xt = xc - xrot
      yt = yc - yrot
      xc = xrot + ca * xt - sa * yt
      yc = yrot + sa * xt + ca * yt
    end if
!
!  Plot the pen stroke.
!
    if ( ipen == 3 ) then
      xnew = xc
      ynew = yc
    else
      xold = xnew
      yold = ynew
      xnew = xc
      ynew = yc
      call movcgm ( xold, yold )
      call drwcgm ( xnew, ynew )
    end if

    ipen = 2
    ip = ip + 2

  end do

    end do
!
!  Advance the base to compensate for character just drawn.
!
    xb = xb + cwide

  end do

  return
end
subroutine scale ( xmin, xmax, ymin, ymax )

!*****************************************************************************80
!
!! SCALE resets the current coordinate system for the picture.
!
!  Discussion:
!
!    After calling SCALE, the coordinate ranges become
!
!      XMIN < =  X <= XMAX
!      YMIN < =  Y <= YMAX
!
!    You can create a new coordinate system at any time by calling
!    SCALE again, or by calling SCL01 to restore the original 0, 1
!    coordinate system, or using SETSCL or SETWCD.
!
!    Since this routine calls SETSCL, the picture will have the same
!    "aspect ratio" or proportions as the coordinate system.  In 
!    other words,
!
!      call scale(1.0, 3.0, 10.0, 11.0)
!
!    will produce a picture that shows up twice as wide (3.0 - 1.0)
!    as it is tall (11.0 - 10.0).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Parameters:
!
!    Input, real XMIN, XMAX, the minimum and maximum values of
!    X to be allowed.
!
!    Input, real YMIN, YMAX, the minimum and maximum values of
!    Y to be allowed.
!
  implicit none

  integer ( kind = 4 ), parameter :: nbox = 2

  real xbox(nbox)
  real xmax
  real xmin
  real ybox(nbox)
  real ymax
  real ymin

  xbox(1) = xmin
  ybox(1) = ymin

  xbox(2) = xmax
  ybox(2) = ymax

  call setscl(xbox,ybox,nbox)

  return
end
subroutine setprm ( iwrite, logy, ncon )

!*****************************************************************************80
!
!! SETPRM allows the user to reset various parameters.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 July 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
  implicit none

  character ( len = 20 ) choice
  integer ( kind = 4 ) itemp
  integer ( kind = 4 ) iwrite
  logical s_eqi
  integer ( kind = 4 ) logy
  integer ( kind = 4 ) ncon

10    continue

  write ( *, '(a)' ) 'Enter a parameter to change, or H for help.'
  read(*,'(a)',end = 30)choice
!
!  HELP: Help!
!
  if ( s_eqi(choice(1:1),'h') ) then
  
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'Choices:'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'IWRITE Amount of printed output.'
    write ( *, '(a)' ) 'LOGY   0 = plot X versus Y,'
    write ( *, '(a)' ) '       1 = plot X versus log(Y).'
    write ( *, '(a)' ) 'NCON   Number of contour levels.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'QUIT   Quit this menu.'
    write ( *, '(a)' ) 'HELP   Print this list.'
!
!  IWRITE: amount of debugging output.
!
  else if ( s_eqi(choice,'iwrite') ) then
  
    write ( *, '(a,i6)' ) 'Current value of IWRITE = ',iwrite
    write ( *, '(a)' ) 'Enter a new value.'
    read(*,*,end = 20)itemp
    iwrite = itemp
!
!  LOGY: Logarithmic plots?
!
  else if ( s_eqi(choice,'logy') ) then
  
    write ( *, '(a,i6)' ) 'Current value of LOGY = ', logy
    write ( *, '(a)' ) 'Enter a new value.'
    read(*,*,end = 20) itemp
    logy = itemp
!
!  NCON: number of contour levels.
!
  else if ( s_eqi(choice,'ncon') ) then
  
    write ( *, * ) 'Current value of NCON is ',ncon
    write ( *, '(a)' ) 'Enter new value for NCON'
    read(*,*,end = 20)itemp
    ncon = itemp
!
!  QUIT: Let's blow this joint.
!
  else if ( s_eqi(choice(1:1),'q') ) then
  
    write ( *, '(a)' ) 'Returning to main menu.'
    return
!
!  Otherwise: Huh?
!
  else
  
    write ( *, '(a)' ) 'Unrecognized choice.'
    
  end if

  go to 10

20    continue
  write ( *, '(a)' ) 'Input was interrupted.  No change was made.'
  go to 10

30    continue
  write ( *, '(a)' ) 'SETPRM input was lost.'
  return
end
subroutine sgraph(cmax,cmin,cost,dev,icolor,iplot,maxobj,maxstp, &
  nmax,nmin,nstep,show,title,title2,xarray,yarray)

!*****************************************************************************80
!
!! SGRAPH draws a graph of the sequence of function values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 July 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) maxobj
  integer ( kind = 4 ) maxstp

  real angle
  character ( len = 6 ) chrint
  character ( len = 14 ) chrrel
  real cmax
  real cmin
  real cost(maxstp)
  real csize
  character ( len = 14 ) ctemp
  real cwide
  character ( len = 10 ) dev
  real eps
  logical filled
  character ( len = 6 ) flush
  integer ( kind = 4 ) i
  integer ( kind = 4 ) icolor(maxobj)
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) iplot 
  character ( len = 20 ) labelx
  character ( len = 20 ) labely
  integer ( kind = 4 ) lent
  integer ( kind = 4 ) narray
  integer ( kind = 4 ) nmax
  integer ( kind = 4 ) nmin
  integer ( kind = 4 ) nstep
  real pwide
  logical show(maxobj)
  character ( len = * )  title
  character ( len = * )  title2
  real x
  real xarray(maxstp)
  real xmax
  real xmin
  real xpsmax
  real xpsmin
  real xsmax
  real xsmin
  real xtemp
  real y
  real yarray(maxstp)
  real ymax
  real ymin
  real ypsmax
  real ypsmin
  real ysmax
  real ysmin
  real ytemp
!
!  Update the count of pictures.
!
  iplot = iplot + 1
!
!  If it's the first picture, then
!
!    Choose an output device,
!    Give the output file a name,
!    Initialize the graphics package.
!
!  If it's a later picture,
!
!    Issue a "new frame" command.
!
  if ( iplot == 1 ) then
  
    call device(dev)
    
    if ( dev == 'cgmb' ) then
      call outfil('slice.cgm')
    else if ( dev == 'ps' ) then
      call outfil('slice.ps')
    end if
    
    call grfini
    
  else
  
    call newfrm
    
  end if
!
!  Set ranges.
!
  xmax = 1
  xmin = 0
  ymax = 1
  ymin = 0

  xsmin = 0.05E+00
  xsmax = 0.95E+00
  ysmin = 0.05E+00
  ysmax = 0.95E+00

  xpsmin = 0.20E+00
  xpsmax = 0.80E+00
  ypsmin = 0.20E+00
  ypsmax = 0.80E+00

  call setwcd(xmin,ymin,xmax,ymax,ierror)
!
!  Copy data.
!

  narray = 0

  do i = nmin,nmax
  
    narray = narray+1
    xarray(narray) = i
    yarray(narray) = cost(i)
    
  end do
!
!  Scale the data.
!
  do i = 1,nstep
    xarray(i) = xpsmin+(xarray(i)-nmin)*(xpsmax-xpsmin)/real(nmax-nmin)
    yarray(i) = ypsmin+(yarray(i)-cmin)*(ypsmax-ypsmin)/(cmax-cmin)
  end do
!
!  Draw a box around the region.
!
  if ( show(2) ) then
    call linclr(icolor(2))
    call box(xsmin,xsmax,ysmin,ysmax)
  end if
!
!  Draw the title.
!
  if ( show(4) ) then

    lent = len_trim ( title )
    call linclr(icolor(4))

    angle = 0.0E+00
    cwide = 0.02E+00
    pwide = 1.0E+00
    x = 0.5E+00
    y = ysmax-0.05E+00
    flush = 'center'

    if ( lent > 0 ) then

      call s_plot(angle,cwide,pwide,title(1:lent),x,y,flush)
      
    end if
    
    lent = len_trim ( title2 )

    angle = 0.0E+00
    cwide = 0.02E+00
    pwide = 1.0E+00
    x = 0.5E+00
    y = ysmax-0.10E+00
    flush = 'center'

    if ( lent > 0 ) then

      call s_plot(angle,cwide,pwide,title2(1:lent),x,y,flush)
      
    end if

  end if
!
!  Draw the X label.
!
  labelx = 'Marching direction'

  lent = len_trim ( labelx )
  if ( lent > 0 ) then
    angle = 0.0E+00
    cwide = 0.015E+00
    pwide = 1.0E+00
    xtemp = 0.5E+00
    ytemp = 0.15E+00
    flush = 'center'
    call s_plot(angle,cwide,pwide,labelx(1:lent),xtemp,ytemp,flush)
  end if
!
!  Draw the Y label.
!
  labely = 'Functional value'

  lent = len_trim ( labely )
  if ( lent > 0 ) then
    angle = 90.0E+00
    cwide = 0.015E+00
    pwide = 1.0E+00
    xtemp = 0.15E+00
    ytemp = 0.5E+00
    flush = 'center'
    call s_plot(angle,cwide,pwide,labely(1:lent),xtemp,ytemp,flush)
  end if
!
!  Draw the axes.
!
  if ( show(5) ) then
    call linclr(icolor(5))

    eps = 0.025E+00

    call movcgm(xpsmin-eps,ypsmin)
    call drwcgm(xpsmax,ypsmin)
  
    ctemp = chrint(nmin)
    call s_blank_delete ( ctemp )
    lent = len_trim ( ctemp )
    angle = 0.0E+00
    cwide = 0.010E+00
    pwide = 1.0E+00
    x = xpsmin
    y = 0.15E+00
    flush = 'left'
    call s_plot(angle,cwide,pwide,ctemp(1:lent),x,y,flush)

    ctemp = chrint(nmax)
    call s_blank_delete(ctemp)
    lent = len_trim ( ctemp )
    angle = 0.0E+00
    cwide = 0.010E+00
    pwide = 1.0E+00
    x = xpsmax
    y = 0.15E+00
    flush = 'right'
    call s_plot(angle,cwide,pwide,ctemp(1:lent),x,y,flush)

    call movcgm(xpsmin,ypsmin-eps)
    call drwcgm(xpsmin,ypsmax)

    ctemp = chrrel(cmin)
    call s_blank_delete(ctemp)
    lent = len_trim ( ctemp )
    angle = 90.0E+00
    cwide = 0.010E+00
    pwide = 1.0E+00
    x = 0.15E+00
    y = ypsmin
    flush = 'left'
    call s_plot(angle,cwide,pwide,ctemp(1:lent),x,y,flush)

    ctemp = chrrel(cmax)
    call s_blank_delete(ctemp)
    lent = len_trim ( ctemp )
    angle = 90.0E+00
    cwide = 0.010E+00
    pwide = 1.0E+00
    x = 0.15E+00
    y = ypsmax
    flush = 'right'
    call s_plot(angle,cwide,pwide,ctemp(1:lent),x,y,flush)
  end if
!
!  Draw the points.
!
  if ( show(1) ) then
    call linclr(icolor(1))
    call plylin(narray,xarray,yarray)
  end if
!
!  Draw the nodes.
!
  if ( show(3) ) then
    call linclr(icolor(3))
    call filclr(icolor(3))
    call ncircle(narray,xarray,yarray,csize,filled)
  end if
 
  call buzz ( dev, xsmin, xsmax, ysmin, ysmax )

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

  character ( len = 8  ) ampm
  integer   ( kind = 4 ) d
  integer   ( kind = 4 ) h
  integer   ( kind = 4 ) m
  integer   ( kind = 4 ) mm
  character ( len = 9  ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer   ( kind = 4 ) n
  integer   ( kind = 4 ) s
  integer   ( kind = 4 ) values(8)
  integer   ( kind = 4 ) y

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
