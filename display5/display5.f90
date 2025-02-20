program display5

!*****************************************************************************80
!
!! DISPLAY5 displays graphics for finite element data.
!
!  Changes:
!
!    02 December 2002: Wondering why the streamlines aren't coming out.
!    Ah, it's because I'm trying to read arbitrary data, so I have to
!    look at the associated labels!
!
!    23 June 2001: Successfully read ELEMENTS and NODES and wrote NODES.
!
!    22 June 2001: Jettison EQN, which was read in, and allowed you to
!    specify walls.  If boundary information is needed, then we need to
!    mark specific element sides.
!
!    22 June 2001: The WRITE_NODE and WRITE_ELEMENT routines are out of date,
!    compared to what's in CONTOUR.  But I should concentrate on READ_NODE
!    and READ_ELEMENT for now.
!
!    21 June 2001: Modified READ_ELEMENT and READ_NODE to allow for blank
!    lines and comment lines with "#" in column 1.  Trying to modify READ_NODE
!    to read an arbitrary set of node data.
!
!  Modified:
!
!    03 April 2003
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: maxbou = 600
  integer ( kind = 4 ), parameter :: maxcontour = 50
  integer ( kind = 4 ), parameter :: maxnpe = 6
  integer ( kind = 4 ), parameter :: maxnx = 151
  integer ( kind = 4 ), parameter :: maxny = 51
  integer ( kind = 4 ), parameter :: maxobj = 39
!
!  Parameters that depend on primitive parameters.
!
  integer ( kind = 4 ), parameter :: maxelm = 2 * ( maxnx - 1 ) * ( maxny - 1 )
  integer ( kind = 4 ), parameter :: maxnp = ( 2 * maxnx - 1 ) * ( 2 * maxny - 1 )

  character ( len = 10 ) arrow
  real bval
  integer ( kind = 4 ) c_contour(maxcontour)
  character ( len = 80 ) command
  character ( len = 8 ) date
  real delx
  real dely
  character ( len = 10 ) dev
  real dudxn(maxnp)
  real dudyn(maxnp)
  real dvdxn(maxnp)
  real dvdyn(maxnp)
  logical eflag(maxelm)
  logical eflagu(maxelm)
  character ( len = 80 ) :: element_file_name = 'element.txt'
  real etaref(maxnpe)
  character ( len = 80 ) filgrf
  real grace
  real gval
  integer ( kind = 4 ) i
  integer ( kind = 4 ) icmax
  integer ( kind = 4 ) icmin
  integer ( kind = 4 ) icolor(maxobj)
  integer ( kind = 4 ) idata
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) ios
  integer ( kind = 4 ) iplot
  character isay
  integer ( kind = 4 ) itable
  integer ( kind = 4 ) itemp
  integer ( kind = 4 ) iwork1(maxelm)
  integer ( kind = 4 ) iwork2(maxnp)
  integer ( kind = 4 ) iwrite
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j_p
  integer ( kind = 4 ) j_r
  integer ( kind = 4 ) j_u
  integer ( kind = 4 ) j_v
  integer ( kind = 4 ) j_x
  integer ( kind = 4 ) j_y
  integer ( kind = 4 ) jbound(5,maxbou)
  integer ( kind = 4 ) jcmax
  integer ( kind = 4 ) jcmin
  integer ( kind = 4 ) jtemp
  integer ( kind = 4 ) k
  character ( len = 30 ) labelx
  character ( len = 30 ) labely
  logical lbar
  integer ( kind = 4 ) line(maxobj)
  character ( len = 10 ) name(100)
  integer ( kind = 4 ) nbound
  integer ( kind = 4 ) ncontour
  integer ( kind = 4 ) nelem
  logical nflag(maxnp)
  logical nflag0(maxnp)
  logical nflag1(maxnp)
  integer ( kind = 4 ) node(maxnpe,maxelm)
  character ( len = 80 ) :: node_file = 'node.txt'
  integer ( kind = 4 ) node_x
  integer ( kind = 4 ) node_y
  logical none
  integer ( kind = 4 ) np
  integer ( kind = 4 ) npe
  integer ( kind = 4 ) nq
  integer ( kind = 4 ) numel(maxnp)
  character ( len = 40 ) object(maxobj)
  logical ovrlay
  real p(maxnp)
  real rho(maxnp)
  real rval
  real s(maxnp)
  real s_contour(maxcontour)
  logical s_eqi
  real s2(maxnp)
  real scalee
  real scalen
  real scale
  real scalev
  logical show(maxobj)
  real smax
  real smin
  real srange
  real t(maxnp)
  character ( len = 80 ) :: tecplot_file = 'tecplot.txt'
  real temp
  character ( len = 10 ) time
  character ( len = 40 ) title
  character ( len = 40 ) title2
  real u(maxnp)
  real vv(maxnp)
  real, allocatable, dimension ( :, : ) :: v
  real x1max
  real x1min
  real x2max
  real x2min
  real x4max
  real x4min
  real xmax
  real xmin
  real xsiref(maxnpe)
  real xsmax
  real xsmin
  real xtmax
  real xtmin
  real y1max
  real y1min
  real y2max
  real y2min
  real y4max
  real y4min
  real ymax
  real ymin
  real ysmax
  real ysmin
  real ytmax
  real ytmin
!
  call timestamp ( )
!
!  Greetings!
!
  call hello ( maxbou, maxcontour, maxelm, maxnp, maxnpe, maxnx, maxny, maxobj )
!
!  Set initial values.
!
  call init ( arrow, c_contour, delx, dely, dev, &
    eflag, eflagu, etaref, &
    filgrf, grace, icmax, icmin, icolor, &
    idata, iplot, itable, iwrite, jcmax, &
    jcmin, labelx, labely, lbar, line, maxcontour, maxelm, maxnp, &
    maxnpe, maxobj, nbound, ncontour, nelem, nflag, nflag0, &
    nflag1, node, np, npe, object, ovrlay, s_contour, &
    scalee, scalen, scalev, show, smax, smin, title, &
    title2, x1max, x1min, x2max, x2min, x4max, x4min, &
    xsiref, xsmax, xsmin, y1max, y1min, y2max, y2min, y4max, &
    y4min, ysmax, ysmin )
!
!  Get a command from the user.
!
  do

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '? ("H" for help)'
    read ( *, '(a)', iostat = ios ) command

    if ( ios /= 0 ) then
      ierror = 1
      exit
    end if

    if ( command == ' ') then
      cycle
    end if
!
!  To simplify parsing, remove all blanks if possible.
!
    if ( .not. s_eqi ( command(1:5), 'TITLE' ) ) then
      call s_blank_delete ( command )
    end if
!
!  ARROWS =: Choose solid or line.
!
    if ( s_eqi ( command(1:5), 'ARROW' ) ) then

      if ( s_eqi ( command(1:6), 'ARROW=' ) ) then
        arrow = command(7:)
      else if ( s_eqi ( command(1:7), 'ARROWS=' ) ) then
        arrow = command(8:)
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DISPLAY5 - Input request:'
        write ( *, '(a)' ) '  Choose HOLLOW, LINE, SOLID for arrows.'
        read ( *, '(a)' ) arrow
      end if

      write ( *, '(a)' ) '  The arrow option is now ARROW = ' // trim ( arrow )
!
!  B:
!  BO: switch the boundary display option.
!
    else if ( s_eqi ( command, 'B' ) .or. s_eqi ( command(1:2), 'BO' ) ) then

      show(1) = .not. show(1)
      if ( show(1) ) then
        write ( *, '(a)' ) 'The boundary will be shown.'
      else
        write ( *, '(a)' ) 'The boundary will NOT be shown.'
      end if
!
!  BACK: show the background
!
    else if ( s_eqi ( command(1:4), 'BACK' ) ) then

      show(21) = .not. show(21)
      if ( show(21) ) then
        write ( *, '(a)' ) 'The background will be shown.'
      else
        write ( *, '(a)' ) 'The background will NOT be shown.'
      end if
!
!  BAR: Switch display of color bar.
!
    else if ( s_eqi ( command, 'BAR' ) ) then

      lbar = .not. lbar
      if ( lbar ) then
        write ( *, '(a)' ) 'The color bar will be shown.'
      else
        write ( *, '(a)' ) 'The color bar will NOT be shown.'
      end if
!
!  BH: bottom half.
!
    else if ( s_eqi ( command, 'BH' ) ) then

      ysmax = ysmin + 0.5E+00 * ( ysmax - ysmin )

      call pltbox ( grace, srange, x1max, x1min, x2max, x2min, xsmax, &
        xsmin, y1max, y1min, y2max, y2min, ysmax, ysmin )
!
!  BL: bottom left quarter.
!
    else if ( s_eqi ( command, 'BL' ) ) then

      xsmax = xsmin + 0.5E+00 * ( xsmax - xsmin )
      ysmax = ysmin + 0.5E+00 * ( ysmax - ysmin )

      call pltbox ( grace, srange, x1max, x1min, x2max, x2min, xsmax, &
        xsmin, y1max, y1min, y2max, y2min, ysmax, ysmin )
!
!  BC: bottom center quarter.
!
    else if ( s_eqi ( command, 'BC' ) ) then

      temp = 0.25E+00 * ( xsmax - xsmin )
      xsmin = xsmin + temp
      xsmax = xsmax - temp
      ysmax = ysmin + 0.5E+00 * ( ysmax - ysmin )

      call pltbox ( grace, srange, x1max, x1min, x2max, x2min, xsmax, &
        xsmin, y1max, y1min, y2max, y2min, ysmax, ysmin )
!
!  BR: bottom right quarter.
!
    else if ( s_eqi ( command, 'BR' ) ) then

      xsmin = xsmin + 0.5E+00 * ( xsmax - xsmin )
      ysmax = ysmin + 0.5E+00 * ( ysmax - ysmin )

      call pltbox ( grace, srange, x1max, x1min, x2max, x2min, xsmax, &
        xsmin, y1max, y1min, y2max, y2min, ysmax, ysmin )
!
!  C: choose colors.
!
    else if ( s_eqi ( command, 'C' ) ) then

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) ' Number  Color  Name'
      write ( *, '(a)' ) ' '
      do i = 1, maxobj
        write ( *, '(i2,2x,i3,2x,a)' ) i, icolor(i), trim ( object(i) )
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'Enter an object number, and a color number.'
      read ( *, *, iostat = ios ) itemp, jtemp

      if ( ios /= 0 ) then
        ierror = 1
        exit
      end if

      if ( 1 <= itemp .and. itemp <= maxobj ) then
        icolor(itemp) = jtemp
      else
        write ( *, '(a)' ) 'Your object number was out of bounds.'
      end if
!
!  CC =: choose color contour labels
!
!  For some strange reason, in order to make the color table
!  active, we have to call NEWFRM!
!
    else if ( s_eqi ( command(1:2), 'CC' ) ) then

      if ( dev == ' ' ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'Please use the DEV command first!'
        cycle
      end if

      if ( s_eqi ( command(1:3), 'CC=' ) ) then

        read ( command(4:), *, iostat = ios ) itable

        if ( ios /= 0 ) then
          ierror = 1
          exit
        end if

      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'Built in color tables include:'
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '1  low black to high white.'
        write ( *, '(a)' ) '2  low white to high black.'
        write ( *, '(a)' ) '3  low blue to high yellow.'
        write ( *, '(a)' ) '4  low red, high blue, with bands between.'
        write ( *, '(a)' ) '5  low red, yellow, green, blue, high white.'
        write ( *, '(a)' ) '6  low white, blue, green, yellow, high red'
        write ( *, '(a)' ) '7  low blue to high red.'
        write ( *, '(a)' ) '8  linear table between 2 user colors.'
        write ( *, '(a)' ) '9  linear table between N user colors.'

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'Enter a color table index between 1 and 9,'
        write ( *, '(a)' ) 'or 0 to enter a color table from a file.'

        read ( *, *, iostat = ios ) itable

        if ( ios /= 0 ) then
          ierror = 1
          exit
        end if

      end if

      call color_table_choose ( dev, filgrf, icmax, icmin, ierror, &
        iplot, itable, ovrlay, x1max, x1min, y1max, y1min )

      if ( itable == 1 .or. itable == 2 ) then
        jcmax = 200
        jcmin = 32
      else
        jcmax = 255
        jcmin = 2
      end if

      write ( *, '(a)' ) ' '
      write ( *, '(a,i6)' ) 'Lowest color used will be JCMIN =  ', jcmin
      write ( *, '(a,i6)' ) 'Highest color used will be JCMAX = ', jcmax
!
!  CH: center half.
!
    else if ( s_eqi ( command, 'CH' ) ) then

      temp = 0.25E+00 * ( xsmax - xsmin )
      xsmin = xsmin + temp
      xsmax = xsmax - temp

      call pltbox ( grace, srange, x1max, x1min, x2max, x2min, xsmax, &
        xsmin, y1max, y1min, y2max, y2min, ysmax, ysmin )
!
!  COLOR
!
    else if ( s_eqi ( command(1:5), 'COLOR' ) ) then

      write ( *, '(a)' ) 'Enter the color index between 0 and 255'

      read ( *, *, iostat = ios ) i

      if ( ios /= 0 ) then
        ierror = 1
        exit
      end if

      write ( *, '(a)' ) 'Enter(R,G,B)'
      write ( *, '(a)' ) 'Note: (0,0,0) is black, (1,1,1) is white!'

      read ( *, *, iostat = ios ) rval, gval, bval

      if ( ios /= 0 ) then
        ierror = 1
        exit
      end if

      call setclr ( i, rval, gval, bval )
!
!  CTAB
!
    else if ( s_eqi ( command, 'CTAB' ) ) then

      call preplt ( dev, filgrf, icmax, icmin, iplot, itable, ovrlay )

      call color_box

      if ( x1min == x1max ) then
        x1min = 0.0E+00
        x1max = 1.0E+00
      end if

      if ( y1min == y1max ) then
        y1min = 0.0E+00
        y1max = 1.0E+00
      end if

      ierror = 0
      call setwcd ( x1min, y1min, x1max, y1max, ierror )

      call buzz ( dev, x1min, x1max, y1min, y1max )
!
!  'DEV = ' Choose the graphics device.
!
    else if ( s_eqi ( command(1:3), 'DEV' ) ) then

      if ( dev /= ' ' ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DISPLAY5 - Error!'
        write ( *, '(a)' ) '  You have already chosen device ' // trim ( dev )
        write ( *, '(a)' ) '  You may not change your mind!'
        cycle
      end if

      if ( s_eqi ( command(1:4), 'DEV=' ) ) then
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
        read ( *, '(a)', iostat = ios ) dev

        if ( ios /= 0 ) then
          ierror = 1
          exit
        end if

      end if

      if ( s_eqi ( dev(1:3), 'CGM' ) ) then
        dev = 'cgmb'
        write ( *, '(a)' ) 'Output will be to a CGM binary file "display.cgm".'
      else if ( s_eqi ( dev, 'PS' ) ) then
        write ( *, '(a)' ) 'Output will be to a PostScript file "display.ps".'
      else if ( s_eqi ( dev, 'XWS' ) ) then
        write ( *, '(a)' ) 'Output will be to an X window screen.'
      else
        write ( *, '(a)' ) 'Your device ' // trim ( dev ) &
          // ' was not recognized!'
        dev = ' '
      end if
!
!  DIV: show divergence contours
!
    else if ( s_eqi ( command, 'DIV' ) ) then

      show(38) = .not. show(38)
      if ( show(38) ) then
        write ( *, '(a)' ) 'Divergence contours will be shown.'
      else
        write ( *, '(a)' ) 'Divergence contours will NOT be shown.'
      end if
!
!  DIVC: show divergence color plots.
!
    else if ( s_eqi ( command, 'DIVC' ) ) then

      show(39) = .not. show(39)
      if ( show(39) ) then
        write ( *, '(a)' ) 'Divergence colors will be shown.'
      else
        write ( *, '(a)' ) 'Divergence colors will NOT be shown.'
      end if
!
!  E: show elements.
!
    else if ( s_eqi ( command, 'E' ) .or. s_eqi ( command, 'ELEMENTS' ) ) then

      show(2) = .not. show(2)
      if ( show(2) ) then
        write ( *, '(a)' ) 'Elements will be shown.'
      else
        write ( *, '(a)' ) 'Elements will NOT be shown.'
      end if
!
!  EC: show element colors.
!
    else if ( s_eqi ( command, 'EC' ) ) then

      show(20) = .not. show(20)
      if ( show(20) ) then
        write ( *, '(a)' ) 'Element colors will be shown.'
      else
        write ( *, '(a)' ) 'Element colors will NOT be shown.'
      end if
!
!  EN: Show element numbers
!
    else if ( s_eqi ( command, 'EN' ) ) then

      show(28) = .not. show(28)
      if ( show(28) ) then
        write ( *, '(a)' ) 'Element numbers will be shown.'
      else
        write ( *, '(a)' ) 'Element numbers will NOT be shown.'
      end if
!
!  EXAMPLE
!    Define example problem.
!
    else if ( s_eqi ( command(1:4), 'EXAM' ) ) then

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Enter the number of nodes per element (3, 4, 6):'
      read ( *, * ) npe

      call example_element ( maxelm, maxnp, maxnpe, nelem, node, &
        node_x, node_y, np, npe )

      nq = 6

      if ( allocated ( v ) ) then
        deallocate ( v )
      end if

      allocate ( v(1:np,1:nq) )

      call example_node ( name, node_x, node_y, np, nq, v )

      nflag(1:np) = .true.
      nflag0(1:np) = .true.
      nflag1(1:np) = .true.
      eflag(1:nelem) = .true.
      eflagu(1:nelem) = .true.

      call rsize ( delx, dely, grace, nelem, nflag, np, srange, &
        x1max, x1min, x2max, x2min, v(1,1), xmax, xmin, xsmax, xsmin, &
        xtmax, xtmin, y1max, y1min, y2max, y2min, v(1,2), ymax, ymin, &
        ysmax, ysmin, ytmax, ytmin )

      idata = 1
!
!  FILE =: set the name of the graphics output file.
!
    else if ( s_eqi ( command(1:4), 'FILE' ) ) then

      if ( iplot > 0 ) then

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DISPLAY5 - Warning:'
        write ( *, '(a)' ) '  It is too late to specify a plot file name.'

      else if ( s_eqi ( command(1:5), 'FILE=' ) ) then

        filgrf = command(6:)

      else

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'Enter the plot file name.'
        read ( *, '(a)', iostat = ios ) filgrf

        if ( ios /= 0 ) then
          ierror = 1
          exit
        end if

      end if

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DISPLAY5 - Note:'
      write ( *, '(a)' ) '  The plot file will be named ' // trim ( filgrf )
!
!  FRAME: switch the frame display option.
!
    else if ( s_eqi ( command, 'FRAME' ) ) then

      show(3) = .not. show(3)

      if ( show(3) ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DISPLAY5 - Note:'
        write ( *, '(a)' ) '  A frame will be shown.'
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DISPLAY5 - Note:'
        write ( *, '(a)' ) '  A frame will NOT be shown.'
      end if
!
!  FULL: show full picture.
!
    else if ( s_eqi ( command, 'FULL' ) ) then

      xsmax = xtmax
      xsmin = xtmin
      ysmax = ytmax
      ysmin = ytmin

      call pltbox ( grace, srange, x1max, x1min, x2max, x2min, xsmax, &
        xsmin, y1max, y1min, y2max, y2min, ysmax, ysmin )
!
!  G: create current graph.
!
    else if ( s_eqi ( command, 'G' ) .or. s_eqi ( command, 'GRAPH' ) ) then

      call graph ( arrow, c_contour, delx, dely, dev, dudxn, &
        dudyn, dvdxn, dvdyn, eflag, eflagu, etaref, filgrf, &
        icmax, icmin, icolor, iplot, itable, iwork1, &
        iwork2, jbound, jcmax, jcmin, lbar, line, maxbou, maxcontour, &
        maxelm, maxnp, maxnpe, maxobj, nbound, ncontour, nelem, &
        nflag, nflag0, nflag1, node, np, npe, nq, numel, object, ovrlay, &
        p, rho, s, s_contour, s2, scalee, scalen, scalev, show, smax, &
        smin, srange, t, &
        title, title2, u, v, x1max, x1min, x2max, x2min, v(1,1), xsiref, &
        xsmax, xsmin, y1max, y1min, y2max, y2min, v(1,2), ysmax, ysmin )
!
!  GRACE =: set the grace margin.
!
    else if ( s_eqi ( command(1:5), 'GRACE' ) ) then

      if ( s_eqi ( command(1:6), 'GRACE=' ) ) then

        read ( command(7:), *, iostat = ios ) grace

        if ( ios /= 0 ) then
          ierror = 1
          exit
        end if

      else
        write ( *, '(a)' ) 'Enter the grace margin:'
        read ( *, * ) grace
      end if

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DISPLAY5 - Note:'
      write ( *, '(a,g14.6)' ) '  The grace margin was set to GRACE = ', grace

      call pltbox ( grace, srange, x1max, x1min, x2max, x2min, xsmax, &
        xsmin, y1max, y1min, y2max, y2min, ysmax, ysmin )
!
!  HELLO: print program version, data, and maxima:
!
    else if ( s_eqi ( command, 'HELLO' ) ) then

      call hello ( maxbou, maxcontour, maxelm, maxnp, maxnpe, maxnx, maxny, &
        maxobj )
!
!  HELP: help
!
    else if ( s_eqi ( command, 'H' ) .or. s_eqi ( command, 'HELP' ) ) then

      call help
!
!  ICMAX = : set the maximum available color index.
!
    else if ( s_eqi ( command(1:5), 'ICMAX' ) ) then

      if ( s_eqi ( command(1:6), 'ICMAX=' ) ) then

        read ( command(7:), *, iostat = ios ) icmax

        if ( ios /= 0 ) then
          ierror = 1
          exit
        end if

      else
        write ( *, '(a)' ) 'Enter ICMAX, maximum color index.'
        read ( *, * ) icmax
      end if

      if ( icmax > 255 ) then
        write ( *, '(a)' ) 'ICMAX must be no more than 255'
        icmax = 255
      end if

      write ( *, '(a,i6)' ) 'Maximum available color set to ', icmax
!
!  ICMIN = : set the minimum available color index.
!
    else if ( s_eqi ( command(1:5), 'ICMIN' ) ) then

      if ( s_eqi ( command(1:6), 'ICMIN=' ) ) then

        read ( command(7:), *, iostat = ios ) icmin

        if ( ios /= 0 ) then
          ierror = 1
          exit
        end if

      else
        write ( *, '(a)' ) 'Enter ICMIN, minimum color index.'
        read ( *, * ) icmin
      end if

      if ( icmin < 2 ) then
        write ( *, '(a)' ) 'ICMIN must be at least 2.'
        icmin = 2
      end if
      write ( *, '(a,i6)' ) 'Minimum available color set to ', icmin
!
!  INIT: set variables to initial (zero!) values.
!
    else if ( s_eqi ( command(1:4), 'INIT' ) ) then

      call init ( arrow, c_contour, delx, dely, dev, &
        eflag, eflagu, etaref, &
        filgrf, grace, icmax, icmin, icolor, &
        idata, iplot, itable, iwrite, jcmax, &
        jcmin, labelx, labely, lbar, line, maxcontour, maxelm, maxnp, &
        maxnpe, maxobj, nbound, ncontour, nelem, nflag, nflag0, &
        nflag1, node, np, npe, object, ovrlay, s_contour, &
        scalee, scalen, scalev, show, smax, smin, title, &
        title2, x1max, x1min, x2max, x2min, x4max, x4min, &
        xsiref, xsmax, xsmin, y1max, y1min, y2max, y2min, y4max, &
        y4min, ysmax, ysmin )
!
!  IWRITE = : set the debugging output level.
!
    else if ( s_eqi ( command(1:6), 'IWRITE' ) ) then

      if ( s_eqi ( command(1:7), 'IWRITE=' ) ) then

        read ( command(8:), *, iostat = ios ) iwrite

        if ( ios /= 0 ) then
          ierror = 1
          exit
        end if

      else
        write ( *, '(a)' ) 'Enter IWRITE, debug output level.'
        read ( *, * ) iwrite
      end if

      write ( *, '(a,i6)' ) 'Debugging level set to ', iwrite
!
!  JCMAX = : set the maximum used color index.
!
    else if ( s_eqi ( command(1:6), 'JCMAX=' ) ) then

      read ( command(7:), *, iostat = ios ) jcmax

      if ( ios /= 0 ) then
        ierror = 1
        exit
      end if

      if ( jcmax > 255 ) then
        jcmax = 255
        write ( *, * ) 'JCMAX must be no more than 255.'
      end if
      write ( *, '(a,i6)' ) 'Maximum used color set to ', jcmax
!
!  JCMIN = : set the minimum used color index.
!
    else if ( s_eqi ( command(1:6), 'JCMIN=' ) ) then

      read ( command(7:), *, iostat = ios ) jcmin

      if ( ios /= 0 ) then
        ierror = 1
        exit
      end if

      if ( jcmin < 2 ) then
        jcmin = 2
        write ( *, '(a)' ) 'JCMIN must be no less than 2.'
      end if
      write ( *, '(a,i6)' ) 'Minimum used color set to ', jcmin
!
!  KV: show kinematic velocity vectors.
!
    else if ( s_eqi ( command, 'KV' ) ) then

      show(8) = .not. show(8)
      if ( show(8) ) then
        write ( *, '(a)' ) 'Kinematic velocity vectors will be shown.'
      else
        write ( *, '(a)' ) 'Kinematic velocity vectors will NOT be shown.'
      end if
!
!  KVMAG: show kinematic velocity magnitude contours.
!
    else if ( s_eqi ( command, 'KVMAG' ) ) then

      show(10) = .not. show(10)
      if ( show(10) ) then
        write ( *, '(a)' ) 'Kinematic velocity magnitudes will be shown.'
      else
        write ( *, '(a)' ) 'Kinematic velocity magnitudes will NOT be shown.'
      end if
!
!  KVMAGC: show velocity magnitude color plots.
!
    else if ( s_eqi ( command, 'KVMAGC' ) ) then

      show(14) = .not. show(14)
      if ( show(14) ) then
        write ( *, '(a)' ) 'Kinematic velocity magnitude colors will be shown.'
      else
        write ( *, '(a)' ) &
          'Kinematic velocity magnitude colors will NOT be shown.'
      end if
!
!  KVX: show X kinematic velocity contours.
!
    else if ( s_eqi ( command, 'KVX' ) ) then

      show(15) = .not. show(15)
      if ( show(15) ) then
        write ( *, '(a)' )'X kinematic velocity contours will be shown.'
      else
        write ( *, '(a)' ) 'X kinematic velocity contours will NOT be shown.'
      end if
!
!  KVXC: show X kinematic velocity color contours.
!
    else if ( s_eqi ( command, 'KVXC' ) ) then

      show(17) = .not. show(17)
      if ( show(17) ) then
        write ( *, '(a)' ) 'X kinematic velocity color contours will be shown.'
      else
        write ( *, '(a)' ) 'X kinematic velocity color contours will NOT be shown.'
      end if
!
!  KVY: show Y kinematic velocity contours.
!
    else if ( s_eqi ( command, 'KVY' ) ) then

      show(16) = .not. show(16)
      if ( show(16) ) then
        write ( *, * ) 'Y kinematic velocity contours will be shown.'
      else
        write ( *, * ) 'Y kinematic velocity contours will NOT be shown.'
      end if
!
!  KVYC: show Y kinematic velocity color contours.
!
    else if ( s_eqi ( command, 'KVYC' ) ) then

      show(18) = .not. show(18)
      if ( show(18) ) then
        write ( *, '(a)' ) 'Y kinematic velocity color contours will be shown.'
      else
        write ( *, '(a)' ) 'Y kinematic velocity color contours will NOT be shown.'
      end if
!
!  L: list current values
!
    else if ( s_eqi ( command(1:4), 'LIST' ) ) then

      call list ( delx, dely, dev, element_file_name, grace, icmax, icmin, &
        icolor, idata, iplot, itable, iwrite, &
        jbound, maxbou, maxnp, maxobj, nbound, ncontour, nelem, node_file, np, &
        npe, object, scalev, show, tecplot_file, &
        title, title2, x2max, x2min, &
        xmax, xmin, xsmax, xsmin, ymax, &
        ymin, y2max, y2min, ysmax, ysmin )
!
!  LH: left half.
!
    else if ( s_eqi ( command, 'LH' ) ) then

      xsmax = xsmin + 0.5E+00 * ( xsmax - xsmin )
      temp = 0.25E+00 * ( ysmax - ysmin )

      call pltbox ( grace, srange, x1max, x1min, x2max, x2min, xsmax, &
        xsmin, y1max, y1min, y2max, y2min, ysmax, ysmin )
!
!  LINE: choose line type.
!
    else if ( s_eqi ( command, 'LINE' ) ) then

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) ' Number  Linetype  Name'
      write ( *, '(a)' ) ' '
      do i = 1, maxobj
        write ( *, '(i2,2x,i3,2x,a)' ) i, line(i), trim ( object(i) )
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'Enter an object number, and a line type.'
      write ( *, '(a)' ) '0 = Solid black, 1=dashed black,'
      write ( *, '(a)' ) '2 = Solid color, 3=dashed color.'

      read ( *, *, iostat = ios ) itemp, jtemp

      if ( ios /= 0 ) then
        ierror = 1
        exit
      end if

      if ( 1 <= itemp .and. itemp <= maxobj ) then
        line(itemp) = jtemp
      else
        write ( *, '(a)' ) 'Your object number was out of bounds.'
      end if
!
!  MACH: draw Mach contour lines.
!
    else if ( s_eqi ( command, 'MACH' ) ) then

      show(30) = .not. show(30)
      if ( show(30) ) then
        write ( *, '(a)' ) 'Mach contours will be plotted.'
      else
        write ( *, '(a)' ) 'Mach contours will NOT be plotted.'
      end if
!
!  MACHC: draw density colors.
!
    else if ( s_eqi ( command, 'MACHC' ) ) then

      show(33) = .not. show(33)
      if ( show(33) ) then
        write ( *, '(a)' ) 'Mach colors will be plotted.'
      else
        write ( *, '(a)' ) 'Mach colors will NOT be plotted.'
      end if
!
!  MC: middle center quarter.
!
    else if ( s_eqi ( command, 'MC' ) ) then

      temp = 0.25E+00 * ( xsmax - xsmin )
      xsmin = xsmin + temp
      xsmax = xsmax - temp
      temp = 0.25E+00 * ( ysmax - ysmin )
      ysmax = ysmax - temp
      ysmin = ysmin + temp

      call pltbox ( grace, srange, x1max, x1min, x2max, x2min, xsmax, &
        xsmin, y1max, y1min, y2max, y2min, ysmax, ysmin )
!
!  MH: middle half.
!
    else if ( s_eqi ( command, 'MH' ) ) then

      temp = 0.25E+00 * ( ysmax - ysmin )
      ysmax = ysmax - temp
      ysmin = ysmin + temp

      call pltbox ( grace, srange, x1max, x1min, x2max, x2min, xsmax, &
        xsmin, y1max, y1min, y2max, y2min, ysmax, ysmin )
!
!  ML: middle left quarter.
!
    else if ( s_eqi ( command, 'ML' ) ) then

      xsmax = xsmin + 0.5E+00 * ( xsmax - xsmin )
      temp = 0.25E+00 * ( ysmax - ysmin )
      ysmax = ysmax - temp
      ysmin = ysmin + temp

      call pltbox ( grace, srange, x1max, x1min, x2max, x2min, xsmax, &
        xsmin, y1max, y1min, y2max, y2min, ysmax, ysmin )
!
!  MR: middle right quarter.
!
    else if ( s_eqi ( command, 'MR' ) ) then

      xsmin = xsmin + 0.5E+00 *( xsmax - xsmin )
      temp = 0.25E+00 * ( ysmax - ysmin )
      ysmax = ysmax - temp
      ysmin = ysmin + temp

      call pltbox ( grace, srange, x1max, x1min, x2max, x2min, xsmax, &
        xsmin, y1max, y1min, y2max, y2min, ysmax, ysmin )
!
!  MV: show kinematic velocity vectors.
!
    else if ( s_eqi ( command, 'MV' ) ) then

      show(35) = .not. show(35)
      if ( show(35) ) then
        write ( *, '(a)' ) 'Mass velocities will be shown.'
      else
        write ( *, '(a)' ) 'Mass velocities will NOT be shown.'
      end if
!
!  MVMAG: show mass velocity magnitude contours.
!
    else if ( s_eqi ( command, 'MVMAG' ) ) then

      show(36) = .not. show(36)
      if ( show(36) ) then
        write ( *, '(a)' ) 'Mass velocity magnitudes will be shown.'
      else
        write ( *, '(a)' ) 'Mass velocity magnitudes will NOT be shown.'
      end if
!
!  MVMAGC: show mass magnitude color plots.
!
    else if ( s_eqi ( command, 'MVMAGC' ) ) then

      show(37) = .not. show(37)
      if ( show(37) ) then
        write ( *, '(a)' ) 'Mass velocity magnitude colors will be shown.'
      else
        write ( *, '(a)' ) 'Mass velocity magnitude colors will NOT be shown.'
      end if
!
!  N: show nodes
!
    else if ( s_eqi ( command, 'N' ) .or. s_eqi ( command, 'NODES' ) ) then

      show(4) = .not. show(4)
      if ( show(4) ) then
        write ( *, '(a)' ) 'Nodes will be shown.'
      else
        write ( *, '(a)' ) 'Nodes will NOT be shown.'
      end if
!
!  NCONTOUR = : Set the number of contour lines.
!
    else if ( s_eqi ( command(1:2), 'NC' ) ) then

      if ( s_eqi ( command, 'NCONTOUR=' ) .or. s_eqi ( command, 'NCON=' ) ) then
        read ( command(6:), *, iostat = ios ) ncontour

        if ( ios /= 0 ) then
          ierror = 1
          exit
        end if

        write ( *, '(a)' ) 'Number of contour lines set to ', ncontour
      else
        write ( *, '(a)' ) 'Enter number of contour lines.'
        read ( *, * ) ncontour
      end if
!
!  NN: Show node numbers
!
    else if ( s_eqi ( command, 'NN' ) ) then

      show(27) = .not. show(27)
      if ( show(27) ) then
        write ( *, '(a)' ) 'Node numbers will be shown.'
      else
        write ( *, '(a)' ) 'Node numbers will NOT be shown.'
      end if
!
!  OVERLAY: Switch the overlay value.
!
    else if ( s_eqi ( command, 'OV' ) .or. s_eqi ( command, 'OVERLAY' ) ) then

      ovrlay = .not. ovrlay
      if ( ovrlay ) then
        write ( *, '(a)' ) 'Plots will be overlayed until next OVERLAY.'
      else
        write ( *, '(a)' ) 'This overlay plot is done.'
        call newfrm
      end if
!
!  P: show pressure contours.
!
    else if ( s_eqi ( command, 'P' ) .or. s_eqi ( command, 'PRESSURE' ) ) then

      show(5) = .not. show(5)
      if ( show(5) ) then
        write ( *, '(a)' ) 'Pressures will be shown.'
      else
        write ( *, '(a)' ) 'Pressures will NOT be shown.'
      end if
!
!  PC: show pressure color plots.
!
    else if ( s_eqi ( command, 'PC' ) ) then

      show(12) = .not. show(12)
      if ( show(12) ) then
        write ( *, '(a)' ) 'Pressure colors will be shown.'
      else
        write ( *, '(a)' ) 'Pressure colors will NOT be shown.'
      end if
!
!  Q: quit.
!  QUIT or QY: QUIT NOW!
!
    else if ( s_eqi ( command(1:1), 'Q' ) ) then

      if ( s_eqi ( command(2:2), 'Y' ) .or. s_eqi ( command, 'QUIT' ) ) then

        command = 'Y'

      else

        write ( *, '(a)' ) 'Enter "Y" to confirm you want to quit!'
        read ( *, '(a)' ) command

      end if

      if ( s_eqi ( command(1:1), 'Y' ) ) then
        call quit ( dev, iplot )
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DISPLAY5:'
        write ( *, '(a)' ) '  Normal end of execution.'
        stop
      end if
!
!  READELEMENT filename 
!    read the specified element file.
!
    else if ( s_eqi ( command(1:11), 'READELEMENT' ) ) then

      if ( command(12:12) == '=' ) then
        element_file_name = command(13:)
      else if ( len_trim ( command(12:) ) > 0 ) then
        element_file_name = command(12:)
      else
        write ( *, * ) ' '
        write ( *, '(a)' ) 'Enter the name of the element data file.'
        read ( *, '(a)' ) element_file_name
      end if
!
!  Read the number of elements.
!
      call file_line_count ( element_file_name, nelem )

      if ( nelem <= 0 ) then
        ierror = 1
        write ( *, * ) ' '
        write ( *, * ) 'DISPLAY5 - Error!'
        write ( *, * ) '  This problem has zero elements!'
        cycle
      end if
 
      if ( nelem > maxelm ) then
        ierror = 1
        write ( *, * ) ' '
        write ( *, * ) 'DISPLAY5 - Error!'
        write ( *, * ) '  This problem has too many elements!'
        write ( *, * ) '  Number of elements NELEM = ', nelem
        write ( *, * ) '  DISPLAY4 can handle up to MAXELM = ', maxelm
        cycle
      end if
!
!  Read the number of nodes per element.
!
      call file_column_count ( element_file_name, npe )

      if ( npe /= 3 .and. npe /= 4 .and. npe /= 6 ) then
        ierror = 1
        write ( *, * ) ' '
        write ( *, * ) 'DISPLAY5 - Error!'
        write ( *, * ) '  Legal values of NPE are 3, 4 and 6.'
        write ( *, * ) '  User input value is NPE = ', npe
        cycle
      end if

      call read_element ( element_file_name, ierror, maxelm, maxnp, maxnpe, &
        nelem, node, np, npe )

      if ( ierror /= 0 ) then
        ierror = 0
        cycle
      end if

      xsmin = 0.0E+00
      xsmax = 0.0E+00
      ysmin = 0.0E+00
      ysmax = 0.0E+00
!
!  READNODE filename 
!    read the specified node file.
!
!    YOW.  Let's assume for now that:
!
!    X      Y      U      V      P
!    V(*,1) V(*,2) V(*,3) V(*,4) V(*,5)
!
    else if ( s_eqi ( command(1:8), 'READNODE' ) ) then

      if ( nelem <= 0 ) then
        write ( *, * ) ' '
        write ( *, * ) 'No elements have been defined yet.'
        write ( *, * ) 'Please enter a command like READ ELEMENT first!'
        cycle
      end if

      if ( command(9:9) == '=' ) then
        node_file = command(10:)
      else if ( len_trim ( command(9:) ) > 0 ) then
        node_file = command(9:)
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'Enter the name of the node data file.'
        read ( *, '(a)' ) node_file
      end if

      call read_node_sizes ( node_file, np, nq )

      if ( allocated ( v ) ) then
        deallocate ( v )
      end if

      allocate ( v(1:np,1:nq) )

      call read_node_values ( node_file, np, nq, v, name, ierror )

      if ( ierror /= 0 ) then
        cycle
      end if

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '# Name'
      write ( *, '(a)' ) ' '
      do i = 1, nq
        write ( *, '(i2,2x,a)' ) i, trim ( name(i) )
      end do
!
!  Initially, all elements will be visible.
!
      eflag(1:nelem) = .true.
      eflagu(1:nelem) = .true.
      nflag(1:np) = .true.
      nflag0(1:np) = .true.
!
!  Check element orientation.
!
      call element_check ( maxelm, maxnpe, nelem, node, np, npe, &
        v(1,1), v(1,2) )
!
!  Get the region size.
!
      call rsize ( delx, dely, grace, nelem, nflag, np, srange, x1max, x1min, &
        x2max, x2min, v(1,1), xmax, xmin, xsmax, xsmin, xtmax, xtmin, y1max, &
        y1min, y2max, y2min, v(1,2), ymax, ymin, ysmax, ysmin, ytmax, ytmin )
!
!  READTECPLOT
!    read the specified TECPLOT data file.
!
    else if ( s_eqi ( command(1:11), 'READTECPLOT' ) ) then

      if ( command(12:12) == '=' ) then
        tecplot_file = command(13:)
      else
        write ( *, * ) 'Enter the name of the TECPLOT data file.'
        read ( *, '(a)' ) tecplot_file
      end if

      call read_tecplot ( tecplot_file, ierror, maxelm, maxnp, maxnpe, &
        name, nelem, node, np, npe, v )

      if ( ierror /= 0 ) then
        ierror = 0
        cycle
      end if

      xsmin = 0.0E+00
      xsmax = 0.0E+00
      ysmin = 0.0E+00
      ysmax = 0.0E+00
!
!  Initially, all elements will be visible.
!
      eflag(1:nelem) = .true.
      eflagu(1:nelem) = .true.
      nflag(1:np) = .true.
      nflag0(1:np) = .true.
!
!  Check element orientation.
!
      call element_check ( maxelm, maxnpe, nelem, node, np, npe, &
        v(1,1), v(1,2) )
!
!  Get the region size.
!
      call rsize ( delx, dely, grace, nelem, nflag, np, srange, &
        x1max, x1min, x2max, x2min, v(1,1), xmax, xmin, xsmax, xsmin, &
        xtmax, xtmin, y1max, y1min, y2max, y2min, v(1,2), ymax, ymin, &
        ysmax, ysmin, ytmax, ytmin )
!
!  RH: right half.
!
    else if ( s_eqi ( command, 'RH') ) then

      temp = 0.50E+00 * ( xsmax - xsmin )
      xsmin = xsmin + temp

      call pltbox ( grace, srange, x1max, x1min, x2max, x2min, xsmax, &
        xsmin, y1max, y1min, y2max, y2min, ysmax, ysmin )
!
!  RHO: draw density contour lines.
!
    else if ( s_eqi ( command, 'RHO' ) ) then

      show(31) = .not. show(31)
      if ( show(31) ) then
        write ( *, * ) 'Rho contours will be plotted.'
      else
        write ( *, * ) 'Rho contours will NOT be plotted.'
      end if
!
!  RHOC: draw density colors.
!
    else if ( s_eqi ( command, 'RHOC' ) ) then

      show(34) = .not. show(34)
      if ( show(34) ) then
        write ( *, * ) 'Rho colors will be plotted.'
      else
        write ( *, * ) 'Rho colors will NOT be plotted.'
      end if
!
!  S: show stream lines
!
    else if ( s_eqi ( command, 'S' ) .or. s_eqi ( command, 'STREAM' ) ) then

      show(6) = .not. show(6)
      if ( show(6) ) then
        write ( *, * ) 'Stream lines will be shown.'
      else
        write ( *, * ) 'Stream lines will NOT be shown.'
      end if
!
!  SC: show stream colors
!
    else if ( s_eqi ( command, 'SC' ) ) then

      show(22) = .not. show(22)
      if ( show(22) ) then
        write ( *, * ) 'Stream colors will be shown.'
      else
        write ( *, * ) 'Stream colors will NOT be shown.'
      end if
!
!  SCALEE = : set the element number scale factor.
!
    else if ( s_eqi ( command(1:7), 'SCALEE=' ) ) then

      read ( command(8:), *, iostat = ios ) scalee

      if ( ios /= 0 ) then
        ierror = 1
        exit
      end if

      write ( *, * ) 'Element number scale factor set to ', scalee
!
!  SCALEN = : set the node number scale factor.
!
    else if ( s_eqi ( command(1:7), 'SCALEN=' ) ) then

      read ( command(8:), *, iostat = ios ) scalen

      if ( ios /= 0 ) then
        ierror = 1
        exit
      end if

      write ( *, * ) 'Node number scale factor set to ', scalen
!
!  SCALEV = : set the velocity vector scale.
!
    else if ( s_eqi ( command(1:7), 'SCALEV=' ) ) then

      read ( command(8:), *, iostat = ios ) scalev

      if ( ios /= 0 ) then
        ierror = 1
        exit
      end if

      write ( *, * ) 'Velocity vector scale factor set to ', scalev
!
!  TC: top center quarter.
!
    else if ( s_eqi ( command, 'TC' ) ) then

      temp = 0.25E+00 * ( xsmax - xsmin )
      xsmin = xsmin + temp
      xsmax = xsmax - temp
      ysmin = ysmin + 0.5E+00 * ( ysmax - ysmin )

      call pltbox ( grace, srange, x1max, x1min, x2max, x2min, xsmax, &
        xsmin, y1max, y1min, y2max, y2min, ysmax, ysmin )
!
!  TH: top half
!
    else if ( s_eqi ( command, 'TH' ) ) then

      ysmin = ysmin + 0.5E+00 * ( ysmax - ysmin )

      call pltbox ( grace, srange, x1max, x1min, x2max, x2min, xsmax, &
        xsmin, y1max, y1min, y2max, y2min, ysmax, ysmin )
!
!  TITLE2 = : enter subtitle
!
    else if ( s_eqi ( command(1:7), 'TITLE2=' ) ) then

      title2 = command(8:)

    else if ( s_eqi ( command(1:6), 'TITLE2' ) ) then

      write ( *, * ) 'Enter the subtitle:'
      read ( *, '(a)', iostat = ios ) title2

      if ( ios /= 0 ) then
        ierror = 1
        exit
      end if
!
!  TITLE = : enter title
!
    else if ( s_eqi ( command(1:6), 'TITLE=' ) ) then

      title = command(7:)

    else if ( s_eqi ( command(1:5), 'TITLE' ) ) then

      write ( *, * ) 'Enter the plot title:'
      read ( *, '(a)', iostat = ios ) title

      if ( ios /= 0 ) then
        ierror = 1
        exit
      end if
!
!  TL: top left quarter.
!
    else if ( s_eqi ( command, 'TL' ) ) then

      xsmax = xsmin + 0.5E+00 * ( xsmax - xsmin )
      ysmin = ysmin + 0.5E+00 * ( ysmax - ysmin )

      call pltbox ( grace, srange, x1max, x1min, x2max, x2min, xsmax, &
        xsmin, y1max, y1min, y2max, y2min, ysmax, ysmin )
!
!  TR: top right quarter.
!
    else if ( s_eqi ( command, 'TR' ) ) then

      xsmin = xsmin + 0.5E+00 * ( xsmax - xsmin )
      ysmin = ysmin + 0.5E+00 * ( ysmax - ysmin )

      call pltbox ( grace, srange, x1max, x1min, x2max, x2min, xsmax, &
        xsmin, y1max, y1min, y2max, y2min, ysmax, ysmin )
!
!  UV: show unit velocity direction vectors.
!
    else if ( s_eqi ( command, 'UV' ) ) then

      show(9) = .not. show(9)
      if ( show(9) ) then
        write ( *, * ) 'Unit velocity vectors will be shown.'
      else
        write ( *, * ) 'Unit velocity vectors will NOT be shown.'
      end if
!
!  VE: set visible elements.
!
    else if ( s_eqi ( command, 'VE' ) ) then

      call vizelm ( eflagu, nelem )

      write ( *, * ) '  Do you want to adjust the data limits'
      write ( *, * ) '  to focus on the visible elements?'
      read ( *, '(a1)' ) isay

      if ( .not. s_eqi ( isay, 'Y' ) ) then
        cycle
      end if

      call frame_visible_elements ( eflagu, grace, maxelm, maxnpe, nelem, &
        node, np, npe, srange, x1max, x1min, x2max, x2min, v(1,1), &
        xsmax, xsmin, y1max, y1min, y2max, y2min, v(1,2), ysmax, ysmin )
!
!  VN: set visible nodes.
!
    else if ( s_eqi ( command, 'VN' ) ) then

      call viznod ( np, nflag0 )
!
!  VND: set visible nodes by distance.
!
    else if ( s_eqi ( command, 'VND' ) ) then

      call viznd ( np, nflag0, v(1,1), v(1,2) )
!
!  VORT: show vorticity contours.
!
    else if ( s_eqi ( command, 'VORT' ) ) then

      show(11) = .not. show(11)
      if ( show(11) ) then
        write ( *, * ) 'Vorticity contours will be shown.'
      else
        write ( *, * ) 'Vorticity contours will NOT be shown.'
      end if
!
!  VORTC: show vorticity color plots.
!
    else if ( s_eqi ( command, 'VORTC' ) ) then

      show(13) = .not. show(13)
      if ( show(13) ) then
        write ( *, * ) 'Vorticity colors will be shown.'
      else
        write ( *, * ) 'Vorticity colors will NOT be shown.'
      end if
!
!  WRITEELEMENT filename
!    write element data to specified file.
!
    else if ( s_eqi ( command(1:12), 'WRITEELEMENT' ) ) then

      if ( command(13:13) == '=' ) then
        element_file_name = command(14:)
      else
        write ( *, * ) 'Enter the name of the element data file.'
        read ( *, '(a)' ) element_file_name
      end if

      call write_element ( element_file_name, maxelm, maxnpe, nelem, node, npe )
!
!  WRITENODE filename
!    write node data to specified file.
!
    else if ( s_eqi ( command(1:9), 'WRITENODE' ) ) then

      if ( command(10:10) == '=' ) then
        node_file = command(11:)
      else
        write ( *, * ) 'Enter the name of the node data file.'
        read ( *, '(a)' ) node_file
      end if

      call write_node ( node_file, np, nq, v, name )
!
!  WRITETECPLOT filename
!    write data to specified TECPLOT file.
!
    else if ( s_eqi ( command(1:15), 'WRITETECPLOT' ) ) then

      if ( command(16:16) == '=' ) then
        tecplot_file = command(17:)
      else
        write ( *, * ) 'Enter the name of the TECPLOT data file.'
        read ( *, '(a)' ) tecplot_file
      end if

      call write_tecplot ( tecplot_file, maxelm, maxnpe, nelem, node, np, &
        npe, nq, v )
!
!  X: set the data window.
!
    else if ( s_eqi ( command, 'X' ) ) then

      call getwin ( grace, srange, xmax, xmin, x1max, x1min, &
        x2max, x2min, xsmax, xsmin, ymax, ymin, y1max, y1min, y2max, &
        y2min, ysmax, ysmin )
!
!  XC: show X coordinate contours.
!
    else if ( s_eqi ( command, 'XC' ) ) then

      show(25) = .not. show(25)
      if ( show(25) ) then
        write ( *, * ) 'X coordinate contours will be shown.'
      else
        write ( *, * ) 'X coordinate contours will NOT be shown.'
      end if
!
!  XCC: show X coordinate color plots.
!
    else if ( s_eqi ( command, 'XCC' ) ) then

      show(23) = .not. show(23)
      if ( show(23) ) then
        write ( *, * ) 'X coordinate colors will be shown.'
      else
        write ( *, * ) 'X coordinate colors will NOT be shown.'
      end if
!
!  YC: show Y coordinate contours.
!
    else if ( s_eqi ( command, 'YC' ) ) then

      show(26) = .not. show(26)
      if ( show(26) ) then
        write ( *, * ) 'Y coordinate contours will be shown.'
      else
        write ( *, * ) 'Y coordinate contours will NOT be shown.'
      end if
!
!  YCC: show Y coordinate color plots.
!
    else if ( s_eqi ( command, 'YCC' ) ) then

      show(24) = .not. show(24)
      if ( show(24) ) then
        write ( *, '(a)' ) 'Y coordinate colors will be shown.'
      else
        write ( *, '(a)' ) 'Y coordinate colors will NOT be shown.'
      end if
!
!  #: a comment
!
    else if ( command(1:1) == '#' ) then

    else

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DISPLAY5 - Warning!'
      write ( *, '(a)' ) '  Your command was not recognized:'
      write ( *, '(a)' ) '  "' // trim ( command ) // '"'

    end if

  end do

  if ( ierror /= 0 ) then

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'DISPLAY5 - Fatal error!'
    write ( *, '(a)' ) '  Error or end of file reading user input!'

    call quit ( dev, iplot )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'DISPLAY5:'
    write ( *, '(a)' ) '  Abnormal end of execution.'
    stop

  end if

  stop
end
subroutine arrow_line ( xstart, ystart, xtip, ytip )
!
!*******************************************************************************
!
!! ARROW_LINE makes a line drawing of an arrow at any point on a graph.
!
!
!  Discussion:
!
!    The arrow will stretch between two user specified points.
!
!    The "head" of the arrow may be fatter or thinner than expected
!    if the X and Y scales of the graph are not in the same
!    proportions.
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
!    Input, real XSTART, YSTART, the starting point for the arrow.
!
!    Input, real XTIP, YTIP, the end point for the arrow.
!
  implicit none
!
  real alpha
  real del
  real dist
  real pi
  real theta
  real xbase
  real xleft
  real xrite
  real xstart
  real xtip
  real ybase
  real yleft
  real yrite
  real ystart
  real ytip
!
!                left
!                |\
!                | \
!    start ------- base  tip
!                | /
!                |/
!                rite
!
  if ( xstart == xtip .and. ystart == ytip ) then
    return
  end if

  theta = 0.5E+00 * pi() - atan2 ( 2.0E+00, 1.0E+00 )
  dist = sqrt ( ( xtip - xstart )**2 + ( ytip - ystart )**2 )
  alpha = atan2 ( ytip - ystart, xtip - xstart )
  del = sqrt ( 5.0E+00 ) * dist / 3.0E+00

  call movcgm ( xstart, ystart )

  xbase = xstart + dist * cos ( alpha ) * 2.0E+00 / 3.0E+00
  ybase = ystart + dist * sin ( alpha ) * 2.0E+00 / 3.0E+00

  call drwcgm ( xbase, ybase )

  xleft = xstart + del * cos ( alpha - theta )
  yleft = ystart + del * sin ( alpha - theta )

  call drwcgm ( xleft, yleft )

  call drwcgm ( xtip, ytip )

  xrite = xstart + del * cos ( alpha + theta )
  yrite = ystart + del * sin ( alpha + theta )

  call drwcgm ( xrite, yrite )

  call drwcgm ( xbase, ybase )

  return
end
subroutine arrow_poly ( arrow, xstart, ystart, xtip, ytip )
!
!*******************************************************************************
!
!! ARROW_POLY makes a polygonal drawing of an arrow at any point on a graph.
!
!
!  Discussion:
!
!    The arrow will stretch between two user specified points.
!
!    The "head" of the arrow may be fatter or thinner than expected
!    if the X and Y scales of the graph are not in the same
!    proportions.
!
!    It might be nice to include an "OUTLINE" option that draws the
!    arrow outline in black, and fills it with the current color.
!
!    By the way, the current color should either be a constant,
!    or depend on the velocity magnitude, choosable by the user.
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
!    Input, character ( len = * ) ARROW, specifies how the arrow will be drawn.
!    LINE, line drawing;
!    SOLID, polygonal shape, filled in;
!    HOLLOW, polygonal shape, outlined.
!
!    Input, real XSTART, YSTART, the starting point for the arrow.
!
!    Input, real XTIP, YTIP, the end point for the arrow.
!
  implicit none
!
  integer ( kind = 4 ), parameter :: npts = 7
!
  real alpha
  character ( len = * ) arrow
  real dist
  real pi
  logical s_eqi
  real theta
  real xpts(npts+1)
  real xstart
  real xtip
  real ypts(npts+1)
  real ystart
  real ytip
!
!                     left
!                     5
!                     |\
!      7--------------6 \
!    start         base  4tip
!      1--------------2 /
!                     |/
!                     3
!                     rite
!
  if ( xstart == xtip .and. ystart == ytip ) then
    return
  end if

  dist = sqrt ( ( xtip - xstart )**2 + ( ytip - ystart )**2 )

  theta = 0.5E+00 * pi() - atan2 ( 2.0E+00, 1.0E+00 )
  alpha = atan2 ( ytip - ystart, xtip - xstart )

  xpts(1) = xstart + dist * sin ( alpha ) / 10.0E+00
  ypts(1) = ystart - dist * cos ( alpha ) / 10.0E+00

  xpts(2) = xstart + dist * sin ( alpha ) / 10.0E+00 &
          + dist * cos ( alpha ) * 2.0E+00 / 3.0E+00
  ypts(2) = ystart - dist * cos ( alpha ) / 10.0E+00 &
          + dist * sin ( alpha ) * 2.0E+00 / 3.0E+00

  xpts(3) = xstart + dist * sqrt ( 5.0E+00 ) * cos ( alpha - theta ) / 3.0E+00
  ypts(3) = ystart + dist * sqrt ( 5.0E+00 ) * sin ( alpha - theta ) / 3.0E+00

  xpts(4) = xtip
  ypts(4) = ytip

  xpts(5) = xstart + dist * sqrt ( 5.0E+00 ) * cos(alpha+theta)/3.0E+00
  ypts(5) = ystart + dist * sqrt ( 5.0E+00 ) * sin(alpha+theta)/3.0E+00

  xpts(6) = xstart - dist * sin(alpha) / 10.0E+00 + dist * cos(alpha)*2.0E+00/3.0E+00
  ypts(6) = ystart + dist * cos(alpha) / 10.0E+00 + dist * sin(alpha)*2.0E+00/3.0E+00

  xpts(7) = xstart - dist * sin(alpha) / 10.0E+00
  ypts(7) = ystart + dist * cos(alpha) / 10.0E+00

  xpts(8) = xpts(1)
  ypts(8) = ypts(1)

  if ( s_eqi ( arrow, 'HOLLOW' ) ) then

    call plylin ( npts+1, xpts, ypts )

  else if ( s_eqi ( arrow, 'SOLID' ) ) then

    call plygon ( npts, xpts, ypts )

  end if

  return
end
subroutine bound_print ( jbound, maxbou, nbound )
!
!*******************************************************************************
!
!! BOUND_PRINT prints the edges which are boundaries of the region.
!
!
!  Modified:
!
!    05 December 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) JBOUND(5,MAXBOU)
!
!    For each line segment of the boundary:
!
!    JBOUND(1,I) contains the element number;
!
!    JBOUND(2,I) contains the local node number of one corner
!      of the element, which forms the edge;
!
!    JBOUND(2,I) contains the "next" node along the edge.
!      If the element is linear, this is the other corner node.
!      If the element is quadratic, this is the midside node along
!        the edge.
!
!    JBOUND(4,I) contains the "next" node along the edge.
!      If the element is linear, this is 0.
!      If the element is quadratic, this is the other corner node
!        along the edge.
!
!    JBOUND(5,I) contains:
!      0 if the boundary is a wall (U = V=0);
!      1 if the boundary is open.
!
!    Input, integer ( kind = 4 ) MAXBOU, the amount of storage in the IBOUND array.
!
!    Input, integer ( kind = 4 ) NBOUND, the number of points defining the boundary.
!
  implicit none
!
  integer ( kind = 4 ) maxbou
!
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jbound(5,maxbou)
  integer ( kind = 4 ) nbound
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BOUND_PRINT'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  I, Element, N1, N2, N3, Wall/Open'
  write ( *, '(a)' ) ' '
  do i = 1, nbound
    write ( *, * ) i, ( jbound(j,i), j = 1, 5 )
  end do

  return
end
subroutine bound_set ( eqn, jbound, maxbou, maxelm, maxnpe, nbound, nelem, &
  node, np, npe )
!
!*******************************************************************************
!
!! BOUND_SET finds edges which are boundaries of the region.
!
!
!  Discussion:
!
!    It does this stupidly, by considering each element, and then each
!    edge of that element, and seeing if it occurs (in the opposite
!    orientation) in any other element.
!
!    For large regions, BOUND_SET is bound to be slow.  If you know
!    where your boundaries occur, then you might want to use
!    a simpler routine.  However, BOUND_SET is guaranteed
!    to work for a wide variety of regions.
!
!    If an edge only occurs in one element, and no velocities
!    are specified along it, then it is a wall, and we record it
!    as a boundary.
!
!    Only edges formed by the three corner nodes are considered.
!
!  Modified:
!
!    06 December 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = 2 ) EQN(3,NP).
!    EQN records the "type" of each equation that will be generated, and
!    which is associated with an unknown.  Note that most boundary
!    conditions do not result in an equation.  The current values are:
!
!    'UM' The horizontal momentum equation.
!    'UW' The condition U = 0 applied at a node on a fixed wall.
!    'VM' The vertical momentum equation.
!    'VW' The condition V = 0 applied at a node on a fixed wall.
!    'PC' The continuity equation.
!    'PB' The condition P = 0 applied at (XMAX,YMAX).
!
!    Output, integer ( kind = 4 ) JBOUND(5,MAXBOU)
!
!    For each line segment of the boundary:
!
!    JBOUND(1,I) contains the element number;
!
!    JBOUND(2,I) contains the local node number of one corner
!      of the element, which forms the edge;
!
!    JBOUND(2,I) contains the "next" node along the edge.
!      If the element is linear, this is the other corner node.
!      If the element is quadratic, this is the midside node along
!        the edge.
!
!    JBOUND(4,I) contains the "next" node along the edge.
!      If the element is linear, this is 0.
!      If the element is quadratic, this is the other corner node
!        along the edge.
!
!    JBOUND(5,I) contains:
!      0 if the boundary is a wall (U = V=0);
!      1 if the boundary is open.
!
!    Input, integer ( kind = 4 ) MAXBOU.
!    The amount of storage available for the IBOUND array.
!
!    Input, integer ( kind = 4 ) MAXELM, the maximum number of elements.
!
!    Input, integer ( kind = 4 ) MAXNPE, the maximum number of nodes per element.
!
!    Input, integer ( kind = 4 ) NBOUND, the number of points defining the boundary.
!
!    Input, integer ( kind = 4 ) NELEM, the number of elements.
!
!    Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), the global node numbers in 
!    each element.
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!    Input, integer ( kind = 4 ) NPE, the number of nodes per element.
!
  implicit none
!
  integer ( kind = 4 ) maxbou
  integer ( kind = 4 ) maxelm
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) np
!
  character ( len = 2 ) eqn(3,np)
  integer ( kind = 4 ) ielem
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jbound(5,maxbou)
  integer ( kind = 4 ) jp1
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) lp1
  integer ( kind = 4 ) m1
  integer ( kind = 4 ) m2
  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n2
  integer ( kind = 4 ) nbound
  integer ( kind = 4 ) nedge
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) node(maxnpe,maxelm)
  integer ( kind = 4 ) npe
!
!  NEDGE is the number of sides an element has.
!
  if ( npe == 3 ) then
    nedge = 3
  else if ( npe == 4 ) then
    nedge = 4
  else if ( npe == 6 ) then
    nedge = 3
  end if

  nbound = 0
!
!  For each element IELEM...
!
  do ielem = 1, nelem
!
!  ...with edge J...
!
    do j = 1, nedge
!
!  ...with corner nodes (N1, N2);
!
      n1 = node(j,ielem)
      if ( j < nedge ) then
        jp1 = j + 1
      else
        jp1 = 1
      end if
      n2 = node(jp1,ielem)
!
!  compare with each element K not equal to I...
!
      do k = 1, nelem

        if ( k /= ielem ) then
!
!  ...with edge L...
!
          do l = 1, nedge
!
!  ...with corner nodes (M1,M2)...
!
            m1 = node(l,k)
            if ( l < nedge ) then
              lp1 = l+1
            else
              lp1 = 1
            end if
            m2 = node(lp1,k)
!
!  ...and if (N1, N2) = (M1, M2), or (N1, N2) = (M2, M1), then skip out.
!
            if ( ( m1 == n1 .and. m2 == n2 ) .or. &
                 ( m1 == n2 .and. m2 == n1 ) ) then
              go to 10
            end if

          end do

        end if

      end do
!
!  Otherwise, the pair (N1, N2) occurs in no other element.  Therefore,
!  it is a boundary edge.
!
      if ( nbound >= maxbou ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BOUND_SET - Warning!'
        write ( *, '(a)' ) '  Ran out of space to store boundary!'
        write ( *, * ) '  Currently using ', nbound, ' boundary edges.'
        return
      end if
!
!  Increment the number of boundary edges found, and store
!  the indices of the element, the first node, the middle node
!  (if quadratic), and the second node.
!
      nbound = nbound + 1
      jbound(1,nbound) = ielem
      jbound(2,nbound) = j

      if ( npe == 3 ) then

        jbound(3,nbound) = jp1
        jbound(4,nbound) = 0

      else if ( npe == 4 ) then

        jbound(3,nbound) = jp1
        jbound(4,nbound) = 0

      else if ( npe == 6 ) then

        if ( j == 1 ) then
          jbound(3,nbound) = 5
        else if ( j == 2 ) then
          jbound(3,nbound) = 6
        else if ( j == 3 ) then
          jbound(3,nbound) = 4
        end if
        jbound(4,nbound) = jp1

      end if
!
!  Now try to figure out what kind of boundary segment it is.
!
!  It is OPEN if there is an unknown U velocity at either end node
!  which does not correspond to a wall,
!  or there is a specified U velocity at either end node.
!
      if ( eqn(1,n1) == 'UW' .and. eqn(1,n2) == 'UW' ) then
        jbound(5,nbound) = 0
      else
        jbound(5,nbound) = 1
      end if

10        continue

    end do
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BOUND_SET - Note:'
  write ( *, '(a,i6)' ) '  Number of boundary edges = ', nbound

  return
end
subroutine bound_show ( eflag, etaref, jbound, line, maxbou, maxnpe, maxobj, &
  nbound, nelem, nflag, node, np, npe, xc, xsiref, yc )
!
!*******************************************************************************
!
!! BOUND_SHOW displays the boundary.
!
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
!    Input, logical EFLAG(NELEM), element visibility flags.
!
!    Input, real ETAREF(MAXNPE), the ETA coordinates of the reference nodes.
!
!    Input, integer ( kind = 4 ) JBOUND(5,MAXBOU)
!
!    For each line segment of the boundary:
!
!    JBOUND(1,I) contains the element number;
!
!    JBOUND(2,I) contains the local node number of one corner
!      of the element, which forms the edge;
!
!    JBOUND(2,I) contains the "next" node along the edge.
!      If the element is linear, this is the other corner node.
!      If the element is quadratic, this is the midside node along
!        the edge.
!
!    JBOUND(4,I) contains the "next" node along the edge.
!      If the element is linear, this is 0.
!      If the element is quadratic, this is the other corner node
!        along the edge.
!
!    JBOUND(5,I) contains:
!      0 if the boundary is a wall (U = V=0);
!      1 if the boundary is open.
!
!    Input, integer ( kind = 4 ) MAXBOU, the storage available for the IBOUND array.
!
!    Input, integer ( kind = 4 ) MAXNPE, the maximum number of nodes per element.
!
!    Input, integer ( kind = 4 ) NBOUND, the number of points defining the boundary.
!
!    Input, integer ( kind = 4 ) NELEM, the number of elements.
!
!    Input, logical NFLAG(NP), flags nodes which are active.
!
!    Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), the global node numbers in 
!    each element.
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!    Input, integer ( kind = 4 ) NPE, the number of nodes per element.
!
!    Input, real XC(NP), the X coordinates of the nodes.
!
!    Input, real XSIREF(MAXNPE), the XSI coordinates of the reference nodes.
!
!    Input, real YC(NP), the Y coordinates of the nodes.
!
  implicit none
!
  integer ( kind = 4 ) maxbou
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) maxobj
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) np
!
  logical eflag(nelem)
  real etaref(maxnpe)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ielem
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j1
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) jbound(5,maxbou)
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) line(maxobj)
  integer ( kind = 4 ) nbound
  logical nflag(np)
  integer ( kind = 4 ) ng1
  integer ( kind = 4 ) ng2
  integer ( kind = 4 ) ng3
  integer ( kind = 4 ) node(maxnpe,nelem)
  integer ( kind = 4 ) npe
  real x1
  real x2
  real x3
  real xc(np)
  real xsiref(maxnpe)
  real xtemp
  real y1
  real y2
  real y3
  real yc(np)
  real ytemp
!
!  Draw the line segments:
!    for linears: from node N1 to node N2;
!    for quadratics: from node N1 to N2, and from N2 to N3.
!
!  Consider the I-th triangle edge that lies on the boundary.
!
  do i = 1, nbound
!
!  Retrieve the element number IELEM of the triangle that contains
!  the edge.
!
    ielem = jbound(1,i)
!
!  If the element is visible, then proceed, else skip to next
!  triangle edge.
!
    if ( eflag(ielem) ) then
!
!  Boundary stretch is "simple".
!
      if ( npe == 3 .or. npe == 4  ) then

        ng1 = node(jbound(2,i),ielem)
        ng2 = node(jbound(3,i),ielem)

        if ( npe == 3 ) then
          ng3 = 0
        else if ( npe == 4 ) then
          ng3 = 0
        else
          ng3 = node(jbound(4,i),ielem)
        end if
!
!  Draw a solid line, if requested.
!
        if ( line(1) == 0 .or. line(1)==2 ) then

          if ( nflag(ng1) .and. nflag(ng2) ) then
            call movcgm ( xc(ng1), yc(ng1))
            call drwcgm ( xc(ng2), yc(ng2))
          end if

          if ( npe == 6 ) then
            if ( nflag(ng2) .and. nflag(ng3) ) then
              call movcgm ( xc(ng2), yc(ng2))
              call drwcgm ( xc(ng3), yc(ng3))
            end if
          end if
!
!  Draw a solid wall.
!
        else

          if ( jbound(5,i) == 0 ) then

            if ( nflag(ng1) .and. nflag(ng2) ) then
              call movcgm ( xc(ng1), yc(ng1))
              call drwcgm ( xc(ng2), yc(ng2))
            end if

            if ( npe == 6 ) then
              if ( nflag(ng2) .and. nflag(ng3) ) then
                call movcgm ( xc(ng2), yc(ng2))
                call drwcgm ( xc(ng3), yc(ng3))
              end if
            end if
!
!  Draw an dashed open boundary.
!
          else

            if ( nflag(ng1) .and. nflag(ng2) ) then
              call movcgm ( xc(ng1), yc(ng1))
              xtemp = xc(ng1) + 0.25E+00 * ( xc(ng2) - xc(ng1) )
              ytemp = yc(ng1) + 0.25E+00 * ( yc(ng2) - yc(ng1) )
              call drwcgm ( xtemp, ytemp)

              xtemp = xc(ng1) + 0.75E+00 * ( xc(ng2) - xc(ng1) )
              ytemp = yc(ng1) + 0.75E+00 * ( yc(ng2) - yc(ng1) )
              call movcgm ( xtemp, ytemp)
              call drwcgm ( xc(ng2), yc(ng2))
            end if

            if ( npe == 6 ) then

              if ( nflag(ng2) .and. nflag(ng3) ) then
                call movcgm ( xc(ng2), yc(ng2))
                xtemp = xc(ng2) + 0.25E+00 * ( xc(ng3) - xc(ng2) )
                ytemp = yc(ng2) + 0.25E+00 * ( yc(ng3) - yc(ng2) )
                call drwcgm ( xtemp, ytemp)

                xtemp = xc(ng2) + 0.75E+00 * ( xc(ng3) - xc(ng2) )
                ytemp = yc(ng2) + 0.75E+00 * ( yc(ng3) - yc(ng2) )
                call movcgm ( xtemp, ytemp)
                call drwcgm ( xc(ng3), yc(ng3))
              end if

            end if

          end if

        end if
!
!  Boundary stretch that is part of isoparametric element.
!
      else

        if ( npe == 6 ) then
          jhi = 2
        end if

        do j = 1, jhi

          j1 = jbound(j+1,i)
          j2 = jbound(j+2,i)

          x1 = xsiref(j1)
          y1 = etaref(j1)

          x2 = xsiref(j2)
          y2 = etaref(j2)

          if ( line(1) == 0 .or. line(1) == 2 ) then

            call iso_line_q6  ( x1, y1, x2, y2, ielem, maxnpe, &
              nelem, node, np, npe, xc, yc )

          else

            if ( jbound(5,i) == 0 ) then

              call iso_line_q6 ( x1, y1, x2, y2, ielem, maxnpe, &
                nelem, node, np, npe, xc, yc )

            else

              x1 = xsiref(j1)
              y1 = etaref(j1)

              x3 = x1 + 0.25E+00 * ( x2 - x1 )
              y3 = y1 + 0.25E+00 * ( y2 - y1 )

              call iso_line_q6 ( x1, y1, x3, y3, ielem, maxnpe, &
                nelem, node, np, npe, xc, yc )

              x3 = x1 + 0.75E+00 * ( x2 - x1 )
              y3 = y1 + 0.75E+00 * ( y2 - y1 )

              call iso_line_q6 ( x3, y3, x2, y2, ielem, maxnpe, &
                nelem, node, np, npe, xc, yc )

            end if

          end if

        end do
      end if

    end if

  end do

  return
end
subroutine box ( xmin, xmax, ymin, ymax )
!
!*******************************************************************************
!
!! BOX draws a rectangle whose corners are specified by the user.
!
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
!
  integer ( kind = 4 ), parameter :: npoints = 5
!
  real x(npoints)
  real xmax
  real xmin
  real y(npoints)
  real ymax
  real ymin
!
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
!
!*******************************************************************************
!
!! BUZZ is just "busy work" to force XWS to draw the whole picture.
!
!
!  Discussion:
!
!    BUZZ seems to fix a problem that occurs when the XWS interface is
!    used.  In that case, the last bit of the graph is not drawn.  So
!    here, we just make the last bit of the graph something we don't
!    care about.
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
!
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
!
!*******************************************************************************
!
!! CH_CAP capitalizes a single character.
!
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
!
  character c
  integer ( kind = 4 ) itemp
!
  itemp = ichar ( c )

  if ( 97 <= itemp .and. itemp <= 122 ) then
    c = char ( itemp - 32 )
  end if

  return
end
function ch_eqi ( c1, c2 )
!
!*******************************************************************************
!
!! CH_EQI is a case insensitive comparison of two characters for equality.
!
!
!  Examples:
!
!    CH_EQI ( 'A', 'a' ) is .TRUE.
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
!
  logical ch_eqi
  character c1
  character c1_cap
  character c2
  character c2_cap
!
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
!
!*******************************************************************************
!
!! CH_TO_DIGIT returns the integer value of a base 10 digit.
!
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
!    Output, integer ( kind = 4 ) DIGIT, the corresponding integer value.  If C was
!    'illegal', then DIGIT is -1.
!
  implicit none
!
  character c
  integer ( kind = 4 ) digit
!
  if ( lge ( c, '0' ) .and. lle ( c, '9' ) ) then

    digit = ichar ( c ) - 48

  else if ( c == ' ' ) then

    digit = 0

  else

    digit = -1

  end if

  return
end
subroutine chrcti2 ( string, intval, ierror, lchar )
!
!*******************************************************************************
!
!! CHRCTI2 finds and reads an integer from a string.
!
!
!  Discussion:
!
!    CHRCTI2 is given a string which may contain one or more integers.
!    Starting at the first character position, it looks for the first
!    substring that could represent an integer.  If it finds such a string,
!    it returns the integer's value, and the position of the last character
!    read.
!
!  Examples:
!
!    STRING            INTVAL      LCHAR
!
!    'Apollo 13'       13          9
!    '     1   '       1           6
!    '1A'              1           1
!    '12,34,56'        12          2
!    'A1A2A3'          1           2
!    '-1E2ABCD'        -1          2
!    '-X20ABCD'        20          4
!    '23.45'           23          2
!    ' N = 34, $'      34          7
!    'Oops!'           0           0
!
!  Modified:
!
!    26 September 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) STRING, the string to be read.
!    Reading will begin at position 1 and terminate at the end of the
!    string, or when no more characters can be read to form a legal integer.
!    Blanks, commas, or other nonnumeric data will, in particular,
!    cause the conversion to halt.
!
!    Output, integer ( kind = 4 ) INTVAL, the integer read from the string, or 0
!    if there was an error.
!
!    Output, integer ( kind = 4 ) IERROR, 0 an integer was found, 1 if no integer found.
!
!    Output, integer ( kind = 4 ) LCHAR, the last character of STRING that is part
!    of the integer.
!
  implicit none
!
  character chrtmp
  integer ( kind = 4 ) i
  integer ( kind = 4 ) idig
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) ihave
  integer ( kind = 4 ) intval
  integer ( kind = 4 ) isgn
  integer ( kind = 4 ) iterm
  integer ( kind = 4 ) lchar
  integer ( kind = 4 ) nchar
  character ( len = * ) string
!
  nchar = len ( string )
  ierror = 0
  i = 0
  isgn = 1
  intval = 0
  ihave = 0
  iterm = 0
!
!  Examine the next character.
!
10    continue

  i = i + 1

  if ( i > nchar ) then

    iterm = 1

  else

    chrtmp = string(i:i)
!
!  Minus sign.
!
    if ( chrtmp == '-' ) then

      if ( ihave == 0 ) then
        ihave = 1
        isgn = -1
      else
        iterm = 1
      end if
!
!  Plus sign.
!
    else if ( chrtmp == '+' ) then

      if ( ihave == 0 ) then
        ihave = 1
      else
        iterm = 1
      end if
!
!  Digit.
!
    else if ( lge ( chrtmp, '0' ) .and. lle ( chrtmp, '9' ) ) then

      ihave = 2

      call digten ( chrtmp, idig )

      intval = 10 * intval + idig
!
!  Blank or TAB.
!
    else

      if ( ihave == 2 ) then
        iterm = 1
      else
        ihave = 0
      end if

    end if

  end if

  if ( iterm /= 1 ) then
    go to 10
  end if

  if ( ihave == 2 ) then
    lchar = i - 1
    intval = isgn * intval
  else
    ierror = 0
    lchar = 0
    intval = 0
  end if

  return
end
function chrrel ( rval )
!
!*******************************************************************************
!
!! CHRREL represents a real using 14 right justified characters.
!
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
!    Input, real RVAL, a real number.
!
!    Output (through function value), character ( len = 14 ) CHRREL,
!    a right-justified character variable containing the
!    representation of RVAL, using a G14.6 format.
!
  implicit none
!
  character ( len = 14 ) chrrel
  character ( len = 14 ) chrtmp
  real rval
!
!  We can't seem to write directly into CHRREL because of compiler
!  quibbles.
!
  if ( real ( int ( rval ) ) == rval .and. abs ( rval ) < 1.0e+13 ) then

    write ( chrtmp, '(i14)' ) int ( rval )

  else

    write ( chrtmp, '(g14.6)' ) rval

  end if

  chrrel = chrtmp

  return
end
subroutine color_bar ( c_contour, icolor, maxcontour, maxobj, ncontour, &
  s_contour, srange, x1, x2, y1, y2 )
!
!*******************************************************************************
!
!! COLOR_BAR draws a color bar in a given rectangle.
!
!
!  Modified:
!
!    16 July 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) C_CONTOUR(MAXCONTOUR), the index of the color associated
!    with each contour level.
!
!    Input, integer ( kind = 4 ) ICOLOR(MAXOBJ), the color index for each object.
!
!    Input, integer ( kind = 4 ) MAXCONTOUR, the maximum number of contour levels.
!
!    Input, integer ( kind = 4 ) MAXOBJ, the number of graphical "objects".
!
!    Input, integer ( kind = 4 ) NCONTOUR, the number of color contour
!    regions drawn, and hence, the number of colors
!    to be displayed in the color bar.
!
!    Input, real SMAX, SMIN, the maximum and minimum
!    values of the quantity whose color contours are
!    being drawn.  These numbers will be printed along
!    with the color bar.
!
!    Input, real SRANGE, the maximum of XSMAX-XSMIN and YSMAX-YSMIN.
!    This gives the size of a square containing the data
!    window.
!
!    Input, real X1, X2, Y1, Y2, specify the minimum and
!    maximum X and Y coordinates of the color bar.
!
  implicit none
!
  integer ( kind = 4 ) maxcontour
  integer ( kind = 4 ) maxobj
!
  real angle
  integer ( kind = 4 ) c_contour(maxcontour)
  character ( len = 14 ) chrrel
  character ( len = 14 ) chrtmp
  real cwide
  character ( len = 6 ) flush
  integer ( kind = 4 ) i
  integer ( kind = 4 ) icolor(maxobj)
  integer ( kind = 4 ) ncontour
  real pwide
  real s_contour(maxcontour)
  real srange
  real x
  real x1
  real x2
  real xcorn(5)
  real xl
  real xr
  real y
  real y1
  real y2
  real ycorn(5)
!
  ycorn(1) = y1
  ycorn(2) = y1
  ycorn(3) = y2
  ycorn(4) = y2
  ycorn(5) = y1

  call linclr ( icolor(1) )

  do i = 1, ncontour

    xl = ( real ( ncontour - i + 1 ) * x1 + real ( i - 1 ) * x2 ) / &
      real ( ncontour )
    xr = ( real ( ncontour - i ) * x1 + real ( i ) * x2 ) / &
      real ( ncontour )

    xcorn(1) = xl
    xcorn(2) = xr
    xcorn(3) = xr
    xcorn(4) = xl
    xcorn(5) = xl

    call filclr ( c_contour(i) )

    call plygon ( 4, xcorn, ycorn )

    call plylin ( 5, xcorn, ycorn )

  end do
!
!  Print labels for the lowest and highest contours.
!
  cwide = 0.9E+00 * srange / 40.0E+00

  chrtmp = chrrel ( s_contour(1) )
  call s_blank_delete ( chrtmp )

  angle = 0.0E+00
  x = x1
  y = y1 - 1.5E+00 * cwide
  pwide = srange
  flush = 'left'
  call s_plot ( angle, cwide, pwide, trim ( chrtmp ), x, y, flush )

  chrtmp = chrrel ( s_contour(ncontour) )
  call s_blank_delete ( chrtmp )

  angle = 0.0E+00
  x = x2
  y = y1 - 1.5E+00 * cwide
  pwide = srange
  flush = 'right'
  call s_plot ( angle, cwide, pwide, trim ( chrtmp ), x, y, flush )

  return
end
subroutine color_box
!
!*******************************************************************************
!
!! COLOR_BOX draws a 16 by 16 box of colors in the current color table.
!
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
!    None
!
  implicit none
!
  integer ( kind = 4 ), parameter :: npoly = 5
!
  real angle
  real cwide
  character ( len = 6 ) flush
  real grace
  integer ( kind = 4 ) i
  integer ( kind = 4 ) icolor
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) j
  real pwide
  character ( len = 3 ) string
  real x(npoly)
  real x1max
  real x1min
  real x2max
  real x2min
  real xtemp
  real y(npoly)
  real y1max
  real y1min
  real y2max
  real y2min
  real ytemp
!
!  Set the coordinate system to be 0 < =  X <= 16.0,
!  and 0.0E+00 < =  Y <= 16.0E+00
!
  x2min = 0.0E+00
  x2max = 16.0E+00
  y2min = 0.0E+00
  y2max = 16.0E+00

  grace = 0.05E+00

  x1min = x2min - grace * ( x2max - x2min )
  x1max = x2max + grace * ( x2max - x2min )
  y1min = y2min - grace * ( y2max - y2min )
  y1max = y2max + grace * ( y2max - y2min )

  ierror = 0
  call setwcd ( x1min, y1min, x1max, y1max, ierror )
!
!  Draw the color boxes.
!
  icolor = 0

  do i = 1, 16

    y(1) = 16 - i
    y(2) = 16 - i
    y(3) = 17 - i
    y(4) = 17 - i

    do j = 1, 16

      call filclr ( icolor )
      icolor = icolor + 1

      x(1) = j - 1
      x(2) = j
      x(3) = j
      x(4) = j - 1

      call plygon ( 4, x, y )

    end do
  end do
!
!  Draw black lines around the boxes.
!
  icolor = 1
  call linclr ( icolor )

  do i = 1, 16

    y(1) = 16 - i
    y(2) = 17 - i
    y(3) = 17 - i
    y(4) = 16 - i
    y(5) = y(1)

    do j = 1, 16

      x(1) = j - 1
      x(2) = j - 1
      x(3) = j
      x(4) = j
      x(5) = x(1)

      call plylin ( 5, x, y )

    end do
  end do
!
!  Print numeric indices.
!
  icolor = 1
  call linclr ( icolor )

  icolor = 0

  do i = 1, 16

    y(1) = 16 - i
    y(2) = 17 - i
    y(3) = 17 - i
    y(4) = 16 - i
    y(5) = y(1)

    do j = 1, 16

      x(1) = j - 1
      x(2) = j - 1
      x(3) = j
      x(4) = j
      x(5) = x(1)

      write ( string, '(i3)' ) icolor

      angle = 0.0E+00
      cwide = 0.015E+00 * ( x2max - x2min )
      pwide = x2max - x2min
      xtemp = real ( j ) - 0.5E+00
      ytemp = 16.5E+00 - real ( i )
      flush = 'center'

      call s_plot ( angle, cwide, pwide, string, xtemp, ytemp, flush )

      icolor = icolor + 1

      call plylin ( 5, x, y )

    end do
  end do

  return
end
subroutine color_table_choose ( dev, filgrf, icmax, icmin, ierror, iplot, &
  itable, ovrlay, x1max, x1min, y1max, y1min )
!
!*******************************************************************************
!
!! COLOR_TABLE_CHOOSE gets the color table choice from the user.
!
!
!  Modified:
!
!    20 June 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = 10 ) DEV, the graphics output device:
!      cgmb - CGM binary file.
!      ps   - PostScript file.
!      XWS  - X window screen (interactive).
!
!    Input, character ( len = 80 ) FILGRF, the name of the output
!    graphics file.
!
!    Input, integer ( kind = 4 ) ICMAX, ICMIN, the maximum and minimum color
!    indices to use in color contour graphics.
!
!    Output, integer ( kind = 4 ) IERROR, error flag.
!    0, no error occurred.
!    nonzero, an error occurred.
!
!    Input, integer ( kind = 4 ) IPLOT, the number of plots made so far.
!
!    Output, integer ( kind = 4 ) ITABLE, the desired color table.
!
!    1: low black to high white
!    2: low blue to high yellow
!    3: low red, high blue, with bands between.
!    4: low red, yellow, green, blue, high white.
!    5: low white, blue, green, yellow, high red.
!    6: low blue to high red
!    7: linear table between 2 user colors.
!    8: linear table between N user colors.
!    9: low white to high black.
!
!    Input, logical OVRLAY.
!    If OVRLAY is true, then the next time that a plot is
!    requested, a "new frame" command is suppressed, so that
!    the new plot is shown on top of the previous one.
!
!    Input, real X1MAX, X1MIN, the maximum and minimum X
!    coordinates of the plot, which includes a small grace margin.
!
!    Input, real Y1MAX, Y1MIN, the maximum and minimum Y
!    coordinates of the plot, which includes a small grace margin.
!
  implicit none
!
  character ( len = 10 ) dev
  character ( len = 80 ) filcol
  character ( len = 80 ) filgrf
  integer ( kind = 4 ) icmax
  integer ( kind = 4 ) icmin
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) ios
  integer ( kind = 4 ) iplot
  integer ( kind = 4 ) itable
  logical ovrlay
  logical s_eqi
  real x1max
  real x1min
  real y1max
  real y1min
!
  if ( itable == 0 ) then

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GETTAB - Input request:'
    write ( *, '(a)' ) '  Enter name of color table file.'

    read ( *, '(a)', iostat = ios ) filcol

    if ( ios /= 0 ) then
      ierror = 1
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GET_COLOR_TABLE - I/O error.'
      write ( *, '(a)' ) '  Operation cancelled.'
      return
    end if

    call getctb ( icmin, icmax, filcol, ierror )

  end if

  call preplt ( dev, filgrf, icmax, icmin, iplot, itable, ovrlay )

  if ( iplot > 1 ) then
    call color_table_set ( icmin, icmax, itable )
  end if

  if ( s_eqi ( dev, 'XWS' ) ) then

    call color_box
!
!  Maybe this will help?
!
    call preplt ( dev, filgrf, icmax, icmin, iplot, itable, ovrlay )

    call color_box

    ierror = 0
    call setwcd ( x1min, y1min, x1max, y1max, ierror )

    call buzz ( dev, x1min, x1max, y1min, y1max )

  end if

  return
end
subroutine color_table_set ( icmin, icmax, itable )
!
!*******************************************************************************
!
!! COLOR_TABLE_SET sets up the color table.
!
!
!  Discussion:
!
!    This routine replaces the unreliable DRAWCGM routine SETCTB.
!
!    The routine sets the colors between ICMIN and ICMAX, which
!    should typically be 2 and 255.
!
!    It will also set the values of color 0 to white, and
!    color 1 to black.
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
!    Input, integer ( kind = 4 ) ICMIN, ICMAX, the minimum and maximum color indices.
!
!    Input, integer ( kind = 4 ) ITABLE, the desired table.
!
!    1: low black to high white
!    2: low white to high black.
!    3: low blue to high yellow
!    4: low red, high blue, with bands between.
!    5: low red, yellow, green, blue, high white.
!    5: low white, blue, green, yellow, high red.
!    7: low blue to high red
!    8: linear table between 2 user colors.
!    9: linear table between n user colors.
!
  implicit none
!
  real bhi
  real blo
  real bval
  real ghi
  real glo
  real gval
  integer ( kind = 4 ) i
  integer ( kind = 4 ) icmax
  integer ( kind = 4 ) icmin
  integer ( kind = 4 ) icol1
  integer ( kind = 4 ) icol2
  integer ( kind = 4 ) ios
  integer ( kind = 4 ) itable
  integer ( kind = 4 ) ival
  real pi
  real rhi
  real rlo
  real rval
  real temp
  real theta
!
  icmin = max ( icmin, 2 )
  icmax = min ( icmax, 255 )
!
!  1: Low black to high white
!  2: Low white to high black
!  3: Low blue to high yellow.
!  4: Low red, high blue, with bands between.
!  5: Low red, yellow, green, blue, high white.
!  6: Low white, blue, green, yellow, high red.
!  7: Low blue to high red
!
  if ( itable >= 1 .and. itable <= 7 ) then

    do i = icmin, icmax

      if ( icmin == icmax ) then
        temp = 0.5E+00
      else
        temp = real ( i - icmin ) / real ( icmax - icmin )
      end if

      if ( itable == 1 ) then
        bval = temp
        gval = temp
        rval = temp
      else if ( itable == 2 ) then
        bval = 1.0E+00 - temp
        gval = 1.0E+00 - temp
        rval = 1.0E+00 - temp
      else if ( itable == 3 ) then
        rval = temp
        gval = temp
        bval = 1.0E+00 - temp
      else if ( itable == 4 ) then
        theta = 0.5E+00 * pi() * temp
        rval = cos ( theta )**2
        bval = sin ( theta )**2
        gval = 0.8E+00 * sin ( 10.0E+00 * theta )**6
      else if ( itable == 5 ) then
        theta = 4.0E+00 * temp
        rval = exp(-(theta-1.0E+00)**2) + exp(-(theta-4.0E+00)**2)
        gval = exp(-(theta-2.0E+00)**2) + exp(-(theta-4.0E+00)**2)
        bval = exp(-(theta-3.0E+00)**2) + exp(-(theta-4.0E+00)**2)
        rval = max ( rval, 0.0E+00 )
        rval = min ( rval, 1.0E+00 )
        gval = max ( gval, 0.0E+00 )
        gval = min ( gval, 1.0E+00 )
        bval = max ( bval, 0.0E+00 )
        bval = min ( bval, 1.0E+00 )
      else if ( itable == 6 ) then
        theta = 4.0E+00 * temp
        rval = exp(-(theta-1.0E+00)**2) + exp(-(theta-4.0E+00)**2)
        gval = exp(-(theta-2.0E+00)**2) + exp(-(theta-4.0E+00)**2)
        bval = exp(-(theta-3.0E+00)**2) + exp(-(theta-4.0E+00)**2)
        rval = max ( rval, 0.0E+00 )
        rval = min ( rval, 1.0E+00 )
        gval = max ( gval, 0.0E+00 )
        gval = min ( gval, 1.0E+00 )
        bval = max ( bval, 0.0E+00 )
        bval = min ( bval, 1.0E+00 )
      else if ( itable == 7 ) then
        rval = temp
        gval = 0.0E+00
        bval = 1.0E+00 - temp
      end if

      ival = i
      call setclr ( ival, rval, gval, bval )

    end do
!
!  8: Interpolate between two values.
!
  else if ( itable == 8 ) then

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'COLOR_TABLE_SET - Input request:'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Enter  Rlo, Glo, Blo,   Rhi, Ghi, Bhi'
    write ( *, '(a)' ) '  Note: 0,0,0 is black, and 1,1,1 is white!'
    write ( *, '(a)' ) ' '

    read ( *, *, end = 1952, err = 1964 ) rlo, glo, blo, rhi, ghi, bhi

    rlo = max ( rlo, 0.0E+00 )
    rhi = min ( rhi, 1.0E+00 )
    glo = max ( glo, 0.0E+00 )
    ghi = min ( ghi, 1.0E+00 )
    blo = max ( blo, 0.0E+00 )
    bhi = min ( bhi, 1.0E+00 )

    do i = icmin, icmax

      if ( icmin == icmax ) then
        temp = 0.5E+00
      else
        temp = real ( i - icmin ) / real ( icmax - icmin )
      end if

      rval = rlo * ( 1.0E+00 - temp ) + rhi * temp
      gval = glo * ( 1.0E+00 - temp ) + ghi * temp
      bval = blo * ( 1.0E+00 - temp ) + bhi * temp

      ival = i
      call setclr ( ival, rval, gval, bval )

    end do
!
!  9: Interpolate between several values.
!
  else if ( itable == 9 ) then

    icol1 = icmin
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'COLOR_TABLE_SET - Input request:'
    write ( *, '(a)' ) ' '
    write ( *, * ) '  Enter (R, G, B) for color index ', icol1
    write ( *, '(a)' ) '      (0, 0, 0) is black.'
    write ( *, '(a)' ) '      (1, 1, 1) is white.'
    read ( *, * ) rlo, glo, blo

    rlo = max ( rlo, 0.0E+00 )
    glo = max ( glo, 0.0E+00 )
    blo = max ( blo, 0.0E+00 )

    do

      do

        write ( *, '(a)' ) '  Enter index of next color to set'
        write ( *, * ) '  between ', icol1+1, ' and ', icmax
        read ( *, * ) icol2

        if ( icol2 > icol1 .and. icol2 <= icmax ) then
          exit
        end if

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SETTAB - Warning!'
        write ( *, '(a)' ) '  Your color index was not accepted!'

      end do

      write ( *, '(a)' ) ' '
      write ( *, * ) 'Enter (R, G, B) for color index ', icol2
      read ( *, * ) rhi, ghi, bhi

      rhi = min ( rhi, 1.0E+00 )
      ghi = min ( ghi, 1.0E+00 )
      bhi = min ( bhi, 1.0E+00 )

      do i = icol1, icol2

        if ( icol1 == icol2 ) then
          temp = 0.5E+00
        else
          temp = real ( i - icol1 ) / real ( icol2 - icol1 )
        end if

        rval = rlo * ( 1.0E+00 - temp ) + rhi * temp
        gval = glo * ( 1.0E+00 - temp ) + ghi * temp
        bval = blo * ( 1.0E+00 - temp ) + bhi * temp

        ival = i
        call setclr ( ival, rval, gval, bval )

      end do

      if ( icol2 >= icmax ) then
        exit
      end if

      icol1 = icol2
      rlo = rhi
      glo = ghi
      blo = bhi

    end do
!
!  Unknown table.
!
  else

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'COLOR_TABLE_SET - Fatal error!'
    write ( *, '(a)' ) '  Legal color table indices are '
    write ( *, * ) '  between 1 and 9.  Your value was ', itable

  end if
!
!  Background color 0 is to be white.
!
  ival = 0
  rval = 1.0E+00
  gval = 1.0E+00
  bval = 1.0E+00
  call setclr ( ival, rval, gval, bval )
!
!  Foreground color 1 is to be black.
!
  ival = 1
  rval = 0.0E+00
  gval = 0.0E+00
  bval = 0.0E+00
  call setclr ( ival, rval, gval, bval )

  return

1952  continue

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'COLOR_TABLE_SET - Fatal error!'
  write ( *, '(a)' ) '  Unexpected end of file!'
  stop

1964  continue

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'COLOR_TABLE_SET - Warning!'
  write ( *, '(a)' ) '  Illegal format for input data!'

  return
end
subroutine contour_color_l3 ( c_contour, eflag, maxcontour, maxelm, maxnpe, &
  ncontour, nelem, node, np, s, s_contour, xc, yc )
!
!*******************************************************************************
!
!! CONTOUR_COLOR_L3 does color contours in a 3 node linear element.
!
!
!  Modified:
!
!    16 July 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) C_CONTOUR(MAXCONTOUR), the index of the color associated
!    with each contour level.
!
!    Input, logical EFLAG(NELEM), element visibility flags.
!
!    Input, integer ( kind = 4 ) MAXCONTOUR, the maximum number of contour levels.
!
!    Input, integer ( kind = 4 ) MAXELM, the maximum number of elements.
!
!    Input, integer ( kind = 4 ) MAXNPE, the maximum number of nodes per element.
!
!    Input, integer ( kind = 4 ) NCONTOUR, the number of color contours to use.
!
!    Input, integer ( kind = 4 ) NELEM, the number of elements.
!
!    Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), the global node numbers in 
!    each element.
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!    Input, real S(NP), a scalar quantity associated with the nodes.
!    This routine only looks at the values associated with
!    corner element nodes.
!
!    Input, real XC(NP), the X coordinates of the nodes.
!
!    Input, real YC(NP), the Y coordinates of the nodes.
!
  implicit none
!
  integer ( kind = 4 ) maxcontour
  integer ( kind = 4 ) maxelm
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) np
!
  integer ( kind = 4 ) c_contour(maxcontour)
  logical eflag(maxelm)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) i3
  integer ( kind = 4 ) ncontour
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) node(maxnpe,maxelm)
  real s(np)
  real s_contour(maxcontour)
  real xc(np)
  real yc(np)
!
!  Consider element I:
!
  do i = 1, nelem
!
!  If it is visible...
!
    if ( eflag(i) ) then

      i1 = node(1,i)
      i2 = node(2,i)
      i3 = node(3,i)

      call tric_l3 ( c_contour, i1, i2, i3, maxcontour, ncontour, np, s, &
        s_contour, xc, yc )

    end if
  end do

  return
end
subroutine contour_color_l4 ( c_contour, eflag, maxcontour, maxelm, maxnpe, &
  ncontour, nelem, node, np, s, s_contour, xc, yc )
!
!*******************************************************************************
!
!! CONTOUR_COLOR_L4 does color contours for linear quadrilateral elements.
!
!
!  Modified:
!
!    16 July 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) C_CONTOUR(MAXCONTOUR), the index of the color associated
!    with each contour level.
!
!    Input, logical EFLAG(NELEM), element visibility flags.
!
!    Input, integer ( kind = 4 ) MAXCONTOUR, the maximum number of contour levels.
!
!    Input, integer ( kind = 4 ) MAXELM, the maximum number of elements.
!
!    Input, integer ( kind = 4 ) MAXNPE, the maximum number of nodes per element.
!
!    Input, integer ( kind = 4 ) NCONTOUR, the number of color contours to use.
!
!    Input, integer ( kind = 4 ) NELEM, the number of elements.
!
!    Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), the global node numbers in 
!    each element.
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!    Input, real S(NP), a scalar quantity associated with the nodes.
!    This routine only looks at the values associated with
!    corner element nodes.
!
!    Input, real XC(NP), the X coordinates of the nodes.
!
!    Input, real YC(NP), the Y coordinates of the nodes.
!
  implicit none
!
  integer ( kind = 4 ) maxcontour
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) maxelm
  integer ( kind = 4 ) np
!
  integer ( kind = 4 ) c_contour(maxcontour)
  logical eflag(maxelm)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) i3
  integer ( kind = 4 ) ncontour
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) node(maxnpe,maxelm)
  real s(np)
  real s_contour(maxcontour)
  real xc(np)
  real yc(np)
!
  do i = 1, nelem

    if ( eflag(i) ) then

      i1 = node(1,i)
      i2 = node(2,i)
      i3 = node(3,i)

      call tric_l3 ( c_contour, i1, i2, i3, maxcontour, ncontour, np, s, &
        s_contour, xc, yc )

      i1 = node(3,i)
      i2 = node(4,i)
      i3 = node(1,i)

      call tric_l3 ( c_contour, i1, i2, i3, maxcontour, ncontour, np, s, &
        s_contour, xc, yc )

    end if

  end do

  return
end
subroutine contour_color_q6 ( c_contour, eflag, etaref, jcmax, jcmin, &
  maxcontour, maxelm, maxnpe, ncontour, nelem, node, np, npe, s, s_contour, &
  smax, smin, xc, xsiref, yc )
!
!*******************************************************************************
!
!! CONTOUR_COLOR_Q6 uses color to indicate points with a value greater than a given value.
!
!
!  Discussion:
!
!    This routine is used for quantities associated with the six
!    corner nodes of a 6 node finite element, in particular,
!    vorticity or velocity magnitude.
!
!    The triangular element is broken up into four three
!    node triangles for further treatment.
!
!     2
!     |\
!     | \
!     6--5
!     |\ |\
!     | \| \
!     3--4--1
!
!    Thus, the four triangles are defined by the nodes as follows:
!
!      Triangle   Nodes
!
!      1          1, 5, 4
!      2          2, 6, 5
!      3          3, 4, 6
!      4          4, 5, 6
!
!  Modified:
!
!    06 December 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) C_CONTOUR(MAXCONTOUR), the index of the color associated
!    with each contour level.
!
!    Input, logical EFLAG(NELEM), element visibility flags.
!
!    Input, real ETAREF(MAXNPE), the ETA coordinates of the reference nodes.
!
!    Input, integer ( kind = 4 ) JCMAX, JCMIN, the maximum and minimum color
!    indices to use for contours.
!
!    Input, integer ( kind = 4 ) MAXCONTOUR, the maximum number of contour levels.
!
!    Input, integer ( kind = 4 ) MAXELM, the maximum number of elements.
!
!    Input, integer ( kind = 4 ) MAXNPE, the maximum number of nodes per element.
!
!    Input, integer ( kind = 4 ) NCONTOUR, the number of contours to draw.
!
!    Input, integer ( kind = 4 ) NELEM, the number of elements.
!
!    Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), the global node numbers in 
!    each element.
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!    Input, real S(NP), the value of S at each node.
!
!    Input, real SMAX, SMIN, the maximum and minimum values of S.
!
!    Input, real XC(NP), the X coordinates of the nodes.
!
!    Input, real XSIREF(MAXNPE), the XSI coordinates of the reference nodes.
!
!    Input, real YC(NP), the Y coordinates of the nodes.
!
  implicit none
!
  integer ( kind = 4 ) maxcontour
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) maxelm
  integer ( kind = 4 ) np
!
  integer ( kind = 4 ) c_contour(maxcontour)
  logical eflag(maxelm)
  real eta1
  real eta2
  real eta3
  real etaref(maxnpe)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) i3
  integer ( kind = 4 ) ielem
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jcmax
  integer ( kind = 4 ) jcmin
  integer ( kind = 4 ) ncontour
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) node(maxnpe,maxelm)
  integer ( kind = 4 ) npe
  integer ( kind = 4 ) nsub(4,3)
  real s(np)
  real s_contour(maxcontour)
  real s1
  real s2
  real s3
  real smax
  real smin
  real xc(np)
  real xsi1
  real xsi2
  real xsi3
  real xsiref(maxnpe)
  real yc(np)
!
  nsub(1,1) = 1
  nsub(1,2) = 5
  nsub(1,3) = 4

  nsub(2,1) = 2
  nsub(2,2) = 6
  nsub(2,3) = 5

  nsub(3,1) = 3
  nsub(3,2) = 4
  nsub(3,3) = 6

  nsub(4,1) = 4
  nsub(4,2) = 5
  nsub(4,3) = 6
!
!  Consider each element.
!
  do i = 1, nelem

    ielem = i
!
!  ...proceed if the element is visible.
!
    if ( eflag(i) ) then
!
!  Consider each of the four subtriangles.
!
      do j = 1, 4

        i1 = node(nsub(j,1),i)
        i2 = node(nsub(j,2),i)
        i3 = node(nsub(j,3),i)
        eta1 = etaref(nsub(j,1))
        eta2 = etaref(nsub(j,2))
        eta3 = etaref(nsub(j,3))
        s1 = s(i1)
        s2 = s(i2)
        s3 = s(i3)
        xsi1 = xsiref(nsub(j,1))
        xsi2 = xsiref(nsub(j,2))
        xsi3 = xsiref(nsub(j,3))

        call tric_q6 ( c_contour, eta1, eta2, eta3, ielem, jcmax, jcmin, &
          maxcontour, maxnpe, ncontour, nelem, node, np, npe, &
          s_contour, s1, s2, s3, xc, xsi1, xsi2, xsi3, yc )

      end do

    end if

  end do

  return
end
subroutine contour_levels ( c_contour, jcmax, jcmin, maxcontour, name, &
  ncontour, s_contour, stmax, stmin, svmax, svmin )
!
!*******************************************************************************
!
!! CONTOUR_LEVELS allows the user to set the contour levels.
!
!
!  Modified:
!
!    02 December 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) C_CONTOUR(MAXCONTOUR), the index of the color associated
!    with each contour level.
!
!    Input, integer ( kind = 4 ) JCMAX, JCMIN, the maximum and minimum color
!    indices to use for contours.
!
!    Input, integer ( kind = 4 ) MAXCONTOUR, the maximum number of contour levels.
!
!    Input, character ( len = * ) NAME, the name of the variable.
!
!    Input/output, integer ( kind = 4 ) NCONTOUR, the number of contour lines to use.
!
!    Input/output, real S_CONTOUR(NCONTOUR), the contour levels.
!
!    Input, real STMAX, STMIN, the maximum and minimum values
!    of the quantity at all nodes.
!
!    Input, real SVMAX, SVMIN, the maximum and minimum values of
!    the quantity at all visible nodes.
!
  implicit none
!
  integer ( kind = 4 ) maxcontour
!
  integer ( kind = 4 ) c_contour(maxcontour)
  character ( len = 80 ) contour_filename
  integer ( kind = 4 ) contour_unit
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ios
  character isay
  integer ( kind = 4 ) jcmax
  integer ( kind = 4 ) jcmin
  character ( len = * ) name
  integer ( kind = 4 ) ncontour
  logical s_eqi
  real s_contour(maxcontour)
  real stmax
  real stmin
  real sumax
  real sumin
  real svmax
  real svmin
  real x
!
10    continue

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) name
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CONTOUR_LEVELS - Input request:'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Total data range:'
  write ( *, '(a,g14.6)' ) '  Minimum = ', stmin
  write ( *, '(a,g14.6)' ) '  Maximum = ', stmax
  write ( *, '(a)' ) '  Range of data at visible nodes:'
  write ( *, '(a,g14.6)' ) '  Minimum = ', svmin
  write ( *, '(a,g14.6)' ) '  Maximum = ', svmax
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  F=File, T=total, U=user, V=visible'
  read ( *, '(a)', iostat = ios ) isay

  if ( ios /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CONTOUR_LEVELS - Warning!'
    write ( *, '(a)' ) '  There was trouble reading your input.'
    go to 10
  end if
!
!  File:
!
  if ( s_eqi ( isay, 'F' ) ) then

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CONTOUR_LEVELS:'
    write ( *, '(a)' ) '  Enter the filename containing contour values.'
    read ( *, '(a)' ) contour_filename

    call get_unit ( contour_unit )

    open ( unit = contour_unit, file = contour_filename, status = 'old', &
      iostat = ios )

    if ( ios /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CONTOUR_LEVELS - Warning!'
      write ( *, '(a)' ) '  Could not open the contour value file.'
      go to 10
    end if

    ncontour = 0

    do while ( ncontour < maxcontour )

      read ( contour_unit, *, iostat = ios ) x

      if ( ios /= 0 ) then
        exit
      end if

      ncontour = ncontour + 1
      s_contour(ncontour) = x

    end do

    close ( unit = contour_unit )
!
!  Total range limits:
!
  else if ( s_eqi ( isay, 'T' ) ) then

    do i = 1, ncontour

      s_contour(i) = ( real ( ncontour + 1 - i ) * stmin &
        + real ( i ) * stmax ) / real ( ncontour + 1 )

    end do
!
!  User input:
!
  else if ( s_eqi ( isay, 'U' ) ) then

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CONTOUR_LEVELS:'
    write ( *, '(a)' ) '  Enter new minimum and maximum values for contours.'
    read ( *, *, iostat = ios ) sumin, sumax

    if ( ios /= 0 ) then
      go to 10
    end if

    do i = 1, ncontour

      s_contour(i) = ( real ( ncontour + 1 - i ) * sumin &
        + real ( i ) * sumax ) / real ( ncontour + 1 )

    end do
!
!  Visible limits:
!
  else if ( s_eqi ( isay, 'V' ) ) then

    do i = 1, ncontour

      s_contour(i) = ( real ( ncontour + 1 - i ) * svmin &
        + real ( i ) * svmax ) / real ( ncontour + 1 )

    end do
!
!  Huh?
!
  else

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CONTOUR_LEVELS - Warning!'
    write ( *, '(a)' ) '  Your choice was not acceptable.'
    go to 10

  end if
!
!  Set the colors.
!
  do i = 1, ncontour

    c_contour(i) = ( ( ncontour + 1 - i ) * jcmin + i * jcmax ) &
      / ( ncontour + 1 )

  end do

  return
end
subroutine contour_line ( c_contour, eflag, etaref, jcmax, jcmin, &
  jcolor, maxcontour, maxelm, maxnp, maxnpe, ncontour, nelem, nflag, node, &
  np, npe, object_name, s, s_contour, xc, xsiref, yc )
!
!*******************************************************************************
!
!! CONTOUR_LINE supervises the drawing of a contour line plot.
!
!
!  Modified:
!
!    13 July 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) C_CONTOUR(MAXCONTOUR), the index of the color associated
!    with each contour level.
!
!    Input, logical EFLAG(NELEM), element visibility flags.
!
  implicit none
!
  integer ( kind = 4 ) maxcontour
  integer ( kind = 4 ) maxelm
  integer ( kind = 4 ) maxnp
  integer ( kind = 4 ) maxnpe
!
  integer ( kind = 4 ) c_contour(maxcontour)
  logical eflag(maxelm)
  real etaref(maxnpe)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) jcmax
  integer ( kind = 4 ) jcmin
  integer ( kind = 4 ) jcolor
  integer ( kind = 4 ) ncontour
  integer ( kind = 4 ) nelem
  logical nflag(maxnp)
  integer ( kind = 4 ) node(maxnpe,maxelm)
  integer ( kind = 4 ) np
  integer ( kind = 4 ) npe
  character ( len = 40 ) object_name
  real s(maxnp)
  real s_contour(maxcontour)
  real scon
  real stmax
  real stmin
  real svmax
  real svmin
  real xc(maxnp)
  real xsiref(maxnpe)
  real yc(maxnp)
!
  call linclr ( jcolor )

  call fsize ( nflag, np, s, stmax, stmin, svmax, svmin )

  if ( stmax <= stmin ) then

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GRAPH - Warning!'
    write ( *, '(a)' ) '  Could not display ' // trim ( object_name )
    write ( *, '(a,g14.6)' ) '  All data values were equal to ', stmin

    return

  end if

  call contour_levels ( c_contour, jcmax, jcmin, maxcontour, &
    object_name, ncontour, s_contour, stmax, stmin, svmax, svmin )

  do i = 1, ncontour

    scon = s_contour(i)

    if ( npe == 3 ) then

      call contour_line_l3 ( eflag, maxelm, maxnpe, nelem, node, &
        np, s, scon, xc, yc )

    else if ( npe == 4 ) then

      call contour_line_l4 ( eflag, maxelm, maxnpe, nelem, node, &
        np, s, scon, xc, yc )

    else if ( npe == 6 ) then

      call contour_line_q6 ( eflag, etaref, maxnpe, nelem, node, &
        np, npe, s, scon, xc, xsiref, yc )

    end if

  end do

  return
end
subroutine contour_line_l3 ( eflag, maxelm, maxnpe, nelem, node, np, s, scon, &
  xc, yc )
!
!*******************************************************************************
!
!! CONTOUR_LINE_L3 draws a single contour line in a single 3 node element.
!
!
!  Modified:
!
!    06 December 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, logical EFLAG(NELEM), element visibility flags.
!
!    Input, integer ( kind = 4 ) MAXELM, the maximum number of elements.
!
!    Input, integer ( kind = 4 ) MAXNPE, the maximum number of nodes per element.
!
!    Input, integer ( kind = 4 ) NELEM, the number of elements.
!
!    Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), the global node numbers in 
!    each element.
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!    Input, real S(NP), the quantity for which a contour line is desired.
!
!    Input, real SCON, the value associated with the contour line.
!
!    Input, real XC(MAXNP), the X coordinates of nodes.
!
!    Input, real YC(MAXNP), the Y coordinates of nodes.
!
  implicit none
!
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) maxelm
  integer ( kind = 4 ) np
!
  logical eflag(maxelm)
  integer ( kind = 4 ) ielem
  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n2
  integer ( kind = 4 ) n3
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) node(maxnpe,maxelm)
  real s(np)
  real s1
  real s2
  real s3
  real scon
  real smax
  real smin
  real x1
  real x2
  real x3
  real xc(np)
  real y1
  real y2
  real y3
  real yc(np)
!
  integer ( kind = 4 ) n_cross
  n_cross = 0
!
!  Draw the contour line by searching over each element.
!
  do ielem = 1, nelem

    if ( eflag(ielem) ) then

      n1 = node(1,ielem)
      x1 = xc(n1)
      y1 = yc(n1)
      s1 = s(n1)

      n2 = node(2,ielem)
      x2 = xc(n2)
      y2 = yc(n2)
      s2 = s(n2)

      n3 = node(3,ielem)
      x3 = xc(n3)
      y3 = yc(n3)
      s3 = s(n3)

      smin = min ( s1, s2, s3 )
      smax = max ( s1, s2, s3 )

      if ( smin <= scon .and. scon <= smax ) then
        n_cross = n_cross + 1
        call cross_l3 ( s1, s2, s3, scon, x1, x2, x3, y1, y2, y3 )
      end if

    end if

  end do

  write ( *, '(a,i6)' ) 'N_CROSS = ', n_cross

  return
end
subroutine contour_line_l4 ( eflag, maxelm, maxnpe, nelem, node, np, s, scon, &
  xc, yc )
!
!*******************************************************************************
!
!! CONTOUR_LINE_L4 draws a contour line in a 4 node rectangular element.
!
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
!    Input, logical EFLAG(NELEM), element visibility flags.
!
!    Input, integer ( kind = 4 ) MAXELM, the maximum number of elements.
!
!    Input, integer ( kind = 4 ) MAXNPE, the maximum number of nodes per element.
!
!    Input, integer ( kind = 4 ) NELEM, the number of elements.
!
!    Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), the global node numbers in 
!    each element.
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!    Input, real S(NP), a scalar quantity associated with the nodes.
!
!    Input, real SCON, the value associated with the contour line.
!
!    Input, real XC(NP), the X coordinates of the nodes.
!
!    Input, real YC(NP), the Y coordinates of the nodes.
!
  implicit none
!
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) maxelm
  integer ( kind = 4 ) np
!
  logical eflag(maxelm)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n2
  integer ( kind = 4 ) n3
  integer ( kind = 4 ) n4
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) node(maxnpe,nelem)
  real s(np)
  real s1
  real s2
  real s3
  real s4
  real scon
  real smax
  real smin
  real x1
  real x2
  real x3
  real x4
  real xc(np)
  real y1
  real y2
  real y3
  real y4
  real yc(np)
!
!  Look at a particular element, IELEM.
!
  do i = 1, nelem
!
!  Is the element visible?
!
    if ( eflag(i) ) then

      n1 = node(1,i)
      x1 = xc(n1)
      y1 = yc(n1)
      s1 = s(n1)

      smin = s1
      smax = s1

      n2 = node(2,i)
      x2 = xc(n2)
      y2 = yc(n2)
      s2 = s(n2)

      smin = min ( smin, s2 )
      smax = max ( smax, s2 )

      n3 = node(3,i)
      x3 = xc(n3)
      y3 = yc(n3)
      s3 = s(n3)

      smin = min ( smin, s3 )
      smax = max ( smax, s3 )

      n4 = node(4,i)
      x4 = xc(n4)
      y4 = yc(n4)
      s4 = s(n4)

      smin = min ( smin, s4 )
      smax = max ( smax, s4 )

      if ( smin <= scon .and. scon <= smax ) then
        call cross_q4 ( s1, s2, s3, s4, scon, x1, x2, x3, x4, y1, y2, y3, y4 )
      end if

    end if

  end do

  return
end
subroutine contour_line_q6 ( eflag, etaref, maxnpe, nelem, node, np, npe, s, &
  scon, xc, xsiref, yc )
!
!*******************************************************************************
!
!! CONTOUR_LINE_Q6 draws a contour line in a 6 node quadratic triangular element.
!
!
!  Note:
!
!    THIS ROUTINE SHOULD BE REWRITTEN SO THAT THE NUMBER OF SUBTRIANGLES
!    MAY BE EASILY VARIED...I JUST NEED TO DEFINE THE X, Y, S
!    values of each subtriangle.
!
!  Discussion:
!
!    The original six node quadratic element is broken up into four
!    three node elements, which are treated as though they were
!    linear:
!
!     2
!     |\
!     | \
!     6--5
!     |\ |\
!     | \| \
!     3--4--1
!
!    Thus, the four triangles are defined by the nodes as follows:
!
!      Triangle   Nodes
!
!      1          1, 5, 4
!      2          5, 2, 6
!      3          4, 6, 3
!      4          6, 4, 5
!
!  Modified:
!
!    02 October 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, logical EFLAG(NELEM), element visibility flags.
!
!    Input, real ETAREF(MAXNPE), the ETA coordinates of the reference nodes.
!
!    Input, integer ( kind = 4 ) MAXNPE, the maximum number of nodes per element.
!
!    Input, integer ( kind = 4 ) NELEM, the number of elements.
!
!    Input, logical NFLAG(NP), is TRUE for nodes which are visible.
!
!    Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), the global node numbers in 
!    each element.
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!    Input, integer ( kind = 4 ) NPE, the number of nodes per element.
!
!    Input, real S(NP), a scalar quantity associated with the nodes.
!
!    Input, real SCON, the value associated with the contour line.
!
!    Input, real XC(NP), the X coordinates of the nodes.
!
!    Input, real XSIREF(MAXNPE), the XSI coordinates of the reference nodes.
!
!    Input, real YC(NP), the Y coordinates of the nodes.
!
  implicit none
!
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) np
!
  logical eflag(nelem)
  real etaref(maxnpe)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) icross
  integer ( kind = 4 ) ielem
  logical inside
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) lines(3,4,2)
  integer ( kind = 4 ) m1
  integer ( kind = 4 ) m2
  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n2
  integer ( kind = 4 ) node(maxnpe,nelem)
  integer ( kind = 4 ) npe
  real s(np)
  real s1
  real s2
  real scon
  real x1
  real x2
  real xc(np)
  real xsiref(maxnpe)
  real xx(6)
  real xx2(2)
  real y1
  real y2
  real yc(np)
  real yy(6)
  real yy2(2)
!
!  List the local node numbers, which define the three lines,
!  which make up each of the four triangles which subdivide
!  the original 6 node triangular element 3 node triangles.
!
  lines(1,1,1) = 1
  lines(1,1,2) = 5
  lines(2,1,1) = 5
  lines(2,1,2) = 4
  lines(3,1,1) = 4
  lines(3,1,2) = 1

  lines(1,2,1) = 5
  lines(1,2,2) = 2
  lines(2,2,1) = 2
  lines(2,2,2) = 6
  lines(3,2,1) = 6
  lines(3,2,2) = 5

  lines(1,3,1) = 4
  lines(1,3,2) = 6
  lines(2,3,1) = 6
  lines(2,3,2) = 3
  lines(3,3,1) = 3
  lines(3,3,2) = 4

  lines(1,4,1) = 6
  lines(1,4,2) = 4
  lines(2,4,1) = 4
  lines(2,4,2) = 5
  lines(3,4,1) = 5
  lines(3,4,2) = 6
!
!  To draw the contour line:
!
!  Consider each element.
!
  do i = 1, nelem
!
!  ...but proceed only if the element is visible.
!
    if ( eflag(i) ) then
!
!  Consider each of the four subtriangles that compose the triangle.
!
        do k = 1, 4

          icross = 0
!
!  Consider each side of a subtriangle.
!
          do j = 1, 3

            m1 = lines(j,k,1)
            n1 = node(m1,i)

            m2 = lines(j,k,2)
            n2 = node(m2,i)

            s1 = s(n1)
            s2 = s(n2)
!
!  Does the value SCON lie between the values at the endpoints of
!  a side of a subtriangle?
!
            call line_seg_contains_point_1d ( s1, scon, s2, inside )

            if ( inside ) then

              x1 = xsiref(m1)
              y1 = etaref(m1)
              x2 = xsiref(m2)
              y2 = etaref(m2)
!
!  Consider 4 cases:
!
!  A) S1 = S2 = SCON, the whole side is an intersection.
!  B) S1 = SCON,
!  C) S2 = SCON,
!  D) SCON strictly between S1 and S2.  Find the location of the crossing.
!
              if ( s1 == scon .and. s2 == scon ) then

                icross = icross + 1
                xx(icross) = x1
                yy(icross) = y1
                icross = icross + 1
                xx(icross) = x2
                yy(icross) = y2

              else if ( s1 == scon ) then

                icross = icross + 1
                xx(icross) = x1
                yy(icross) = y1

              else if ( s2 == scon ) then

                icross = icross + 1
                xx(icross) = x2
                yy(icross) = y2

              else

                icross = icross + 1
                xx(icross) = x1 + ( scon - s1 ) * ( x2 - x1 ) / ( s2 - s1 )
                yy(icross) = y1 + ( scon - s1 ) * ( y2 - y1 ) / ( s2 - s1 )
              end if

            end if

          end do
!
!  How many times did the contour value cross the subtriangle?
!
          if ( icross > 1 ) then

            do j = 1, icross
              do l = j+1, icross

                ielem = i

                if ( npe == 3 ) then
                  xx2(1) = xx(j)
                  yy2(1) = yy(j)
                  xx2(2) = xx(l)
                  yy2(2) = yy(l)
                  call plylin ( 2, xx2, yy2 )
                else
                  x1 = xx(j)
                  y1 = yy(j)
                  x2 = xx(l)
                  y2 = yy(l)

                  call iso_line_q6 ( x1, y1, x2, y2, ielem, &
                    maxnpe, nelem, node, np, npe, xc, yc )
                end if

              end do

            end do

          end if

        end do

    end if

  end do

  return
end
subroutine cross ( px, py, qx, qy, sl, sm, sh, scon, xl, xm, xh, yl, ym, yh )
!
!*******************************************************************************
!
!! CROSS finds the two places where the value S = SCON occurs on a triangle.
!
!
!  Discussion:
!
!    The corners of the triangle are (XL,YL), (XM,YM) and
!    (XH,YH), and the associated S values are SL, SM and SH.  It
!    must be the case that SL < =  SM <= SH.
!
!    CROSS returns two points:
!
!     (PX,PY), which occurs on one of the two sides that include (XM,YM), and
!     (QX,QY), which occurs on the side between (XL,YL) and (XH,YH).
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
!    Output, real PX, PY, the X and Y coordinates of a point
!    at which S = SCON, lying on a side of the triangle which
!    ends at (XM,YM).
!
!    Output, real QX, QY, the X and Y coordinates of a point
!    at which S = SCON, lying on the side of the triangle which
!    lies between (XL,YL) and (XH,YH).
!
!    Input, real SL, SM, SH, the low, medium, and high values
!    of S, associated with the three corners.
!
!    Input, real SCON, the value of S for which a contour line is sought.
!
!    Input, real XL, XM, XH, the X coordinates of the nodes
!    at which the low, medium and high values of S occur.
!
!    Input, real YL, YM, YH, the Y coordinates of the nodes
!    at which the low, medium and high values of S occur.
!
  implicit none
!
  real px
  real py
  real qx
  real qy
  real scon
  real sl
  real sm
  real sh
  real xl
  real xm
  real xh
  real yl
  real ym
  real yh
!
  if ( scon < sl ) then

    px = xl
    py = yl
    qx = xl
    qy = yl

  else if ( scon >= sh ) then

    px = xh
    py = yh
    qx = xh
    qy = yh

  else

    if ( scon < sm ) then
      px = xl + ( scon - sl ) * ( xm - xl ) / ( sm - sl )
      py = yl + ( scon - sl ) * ( ym - yl ) / ( sm - sl )
    else
      px = xm + ( scon - sm ) * ( xh - xm ) / ( sh - sm )
      py = ym + ( scon - sm ) * ( yh - ym ) / ( sh - sm )
    end if

    qx = xl + ( scon - sl ) * ( xh - xl ) / ( sh - sl )
    qy = yl + ( scon - sl ) * ( yh - yl ) / ( sh - sl )

  end if

  return
end
subroutine cross_l2 ( icross, s1, s2, scon, x1, x2, xc, y1, y2, yc )
!
!*******************************************************************************
!
!! CROSS_L2 seeks contour crossings along a line.
!
!
!  Discussion:
!
!    CROSS_L2 is given the coordinates of two points in the plane, and
!    the values of a quantity S there.  It is assumed that S varies
!    linearly along the line through the two points.
!
!    CROSS_L2 is also given a desired value SC of S, and is supposed
!    to seek a point, lying between the two given points, at which
!    this value of S is achieved.
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
!    Output, integer ( kind = 4 ) ICROSS.
!
!    -3, X1 = X2 and Y1=Y2.
!    -2, S1 and S2 and SC are equal.
!    -1, S1 and S2 are equal, and no crossing was found.
!
!    0, S1 and S2 are distinct, and no crossing was found.
!
!    1, S1 and S2 are distinct, and a crossing was found.
!    2, S1 and S2 are distinct, and S1 = SC.
!    3, S2 and S2 are distinct, and S2 = SC.
!
!    Input, real S1, S2, the values of S at (X1,Y1) and (X2,Y2).
!
!    Input, real SCON, the S value at which a crossing is sought.
!
!    Input, real X1, X2, the X coordinates of the two endpoints.
!
!    Output, real XC.
!    If ICROSS = 1, 2 or 3, then XC is the X coordinate of the crossing.
!    Otherwise XC is 0.
!
!    Input, real Y1, Y2, the Y coordinates of the two endpoints.
!
!    Output, real YC.
!    If ICROSS = 1, 2, or 3, then YC is the Y coordinate of the crossing.
!    Otherwise YC is 0.
!
  implicit none
!
  integer ( kind = 4 ) icross
  logical inside
  real s1
  real s2
  real scon
  real x1
  real x2
  real xc
  real y1
  real y2
  real yc
!
  xc = 0.0E+00
  yc = 0.0E+00
!
!  Are the two points distinct?
!
  if ( x1 == x2 .and. y1 == y2 ) then
    icross = -3
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CROSS_L2 - Fatal error!'
    write ( *, * ) '  X1 = X2 = ', x1
    write ( *, * ) '  Y1 = Y2 = ', y1
    stop
  end if
!
!  Are the two S values distinct?
!
  if ( s1 == s2 ) then
    if ( scon == s1 ) then
      icross = -2
    else
      icross = -1
    end if

    return

  end if
!
!  Where does SCON fit in?
!
  call line_seg_contains_point_1d ( s1, scon, s2, inside )

  if ( s2 == scon ) then
    icross = 3
    xc = x2
    yc = y2
  else if ( s1 == scon ) then
    icross = 2
    xc = x1
    yc = y1
  else if ( inside ) then
    icross = 1
    xc = x1 + ( scon - s1 ) * ( x2 - x1 ) / ( s2 - s1 )
    yc = y1 + ( scon - s1 ) * ( y2 - y1 ) / ( s2 - s1 )
  else
    icross = 0
  end if

  return
end
subroutine cross_l3 ( s1, s2, s3, scon, x1, x2, x3, y1, y2, y3 )
!
!*******************************************************************************
!
!! CROSS_L3 seeks contour crossings in a 3 node linear element.
!
!
!  Discussion:
!
!    The coordinates of the corners of a triangle and the values of a
!    scalar quantity S at those corners are given, as well as SCON, a
!    special value of S.
!
!    Linear interpolation is used to extend the definition of S
!    to the entire triangle, and then a contour line is drawn
!    to represent those locations within the triangle which
!    would have the value SCON.
!
!  Diagram:
!
!    The triangular element has the logical form:
!
!      2
!      |\
!      | \
!      |  \
!      3---1
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
!    Input, real S1, S2, S3, the values of S at the corners.
!
!    Input, real SCON, the contour value of S.
!
!    Input, real X1, X2, X3, the values of X at the corners.
!
!    Input, real Y1, Y2, Y3, the values of Y at the corners.
!
  implicit none
!
  integer ( kind = 4 ) icross
  integer ( kind = 4 ) npts
  real s1
  real s2
  real s3
  real scon
  real x1
  real x2
  real x3
  real xc
  real xpts(3)
  real y1
  real y2
  real y3
  real yc
  real ypts(3)
!
  npts = 0
!
!  Does the contour value lie in (S1,S2)?
!
  call cross_l2 ( icross, s1, s2, scon, x1, x2, xc, y1, y2, yc )

  if ( icross == -2 ) then
    call movcgm ( x1, y1)
    call drwcgm ( x2, y2)
  else if ( icross == 1 ) then
    npts = npts + 1
    xpts(npts) = xc
    ypts(npts) = yc
  end if
!
!  Does the contour value lie in (S2,S3)?
!
  call cross_l2 ( icross, s2, s3, scon, x2, x3, xc, y2, y3, yc )

  if ( icross == -2 ) then
    call movcgm ( x2, y2 )
    call drwcgm ( x3, y3 )
  else if ( icross == 1 ) then
    npts = npts + 1
    xpts(npts) = xc
    ypts(npts) = yc
  end if
!
!  Does the contour value lie in (S3,S1)?
!
  call cross_l2 ( icross, s3, s1, scon, x3, x1, xc, y3, y1, yc )

  if ( icross == -2 ) then
    call movcgm ( x3, y3 )
    call drwcgm ( x1, y1 )
  else if ( icross == 1 ) then
    npts = npts + 1
    xpts(npts) = xc
    ypts(npts) = yc
  end if
!
!  Ignore a single crossing, draw the line between two, and
!  ponder 3.
!
  if ( npts == 1 ) then

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CROSS_L3:'
    write ( *, '(a)' ) '  That''s strange, just one crossing!'

  else if ( npts == 2 ) then

    call plylin ( npts, xpts, ypts )

  else if ( npts > 2 ) then

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CROSS_L3:'
    write ( *, '(a,i6)' ) '  That''s strange, NPTS = ', npts

  end if

  return
end
subroutine cross_q4 ( s1, s2, s3, s4, scon, x1, x2, x3, x4, y1, y2, y3, y4 )
!
!*******************************************************************************
!
!! CROSS_Q4 seeks contour crossings in a 4 node linear element.
!
!
!  Discussion:
!
!    CROSS_Q4 divides the element into two triangles and calls CROSS_L3:
!
!      3---2
!      |\  |
!      | \ |
!      |  \|
!      4---1
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
!    Input, real S1, S2, S3, S4, the values of S at the corners.
!
!    Input, real SCON, the contour value of S.
!
!    Input, real X1, X2, X3, X4, the values of X at the corners.
!
!    Input, real Y1, Y2, Y3, Y4, the values of Y at the corners.
!
  implicit none
!
  real s1
  real s2
  real s3
  real s4
  real scon
  real x1
  real x2
  real x3
  real x4
  real y1
  real y2
  real y3
  real y4
!
  call cross_l3 ( s1, s3, s4, scon, x1, x3, x4, y1, y3, y4 )

  call cross_l3 ( s3, s1, s2, scon, x3, x1, x2, y3, y1, y2 )

  return
end
subroutine dash_line ( n, x, y, dshsiz )
!
!*******************************************************************************
!
!! DASH_LINE draws a dashed line connecting a series of points.
!
!
!  Discussion:
!
!    If the X and Y coordinates use different scale factors, then dashes
!    at different angles will seem to have different lengths.
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
!    Input, integer ( kind = 4 ) N, the number of points to be connected.
!
!    Input, real X(N), Y(N), the X and Y coordinates of the
!    points.
!
!    Input, real DSHSIZ, the length, in the X, Y coordinate
!    system, of the dashed lines.  If it is negative or zero,
!    an error occurs.
!
  implicit none
!
  integer ( kind = 4 ) n
!
  real dist
  real dshsiz
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) ndash
  real x(n)
  real xnew
  real xold
  real xxnew
  real xxold
  real y(n)
  real ynew
  real yold
  real yynew
  real yyold
!
!  Make sure that DSIZE is positive.
!
  if ( dshsiz <= 0.0E+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'DSHLIN - Fatal error!'
    write ( *, '(a)' ) '  The parameter DSHSIZ must be positive.'
    write ( *, '(a,g14.6)' ) '  but the input value is ', dshsiz
    return
  end if

  xnew = x(1)
  ynew = y(1)

  do i = 2, n

    xold = xnew
    yold = ynew
    xnew = x(i)
    ynew = y(i)
    dist = sqrt ( ( xnew - xold )**2 + ( ynew - yold )**2 )

    if ( dist > 0.0E+00 ) then

      ndash = int ( dist / dshsiz ) + 1

      if ( mod ( ndash, 4 ) /= 0 ) then
        ndash = ndash + ( 4 - mod ( ndash, 4 ) )
      end if

      if ( ndash <= 3 ) then
        ndash = 4
      end if
!
!  Desired pattern is:
!
!  X0 - dash - blank - blank - dash - dash - blank - blank - dash - X1
!
      do j = 1, ndash

        if ( mod(j,4) == 0 .or. mod(j,4) == 1 ) then
          xxold = ( real (ndash+1-j) * xold + real (j-1) * xnew ) &
            / real ( ndash )
          yyold = ( real (ndash+1-j) * yold + real (j-1) * ynew ) &
            / real ( ndash )
          xxnew = ( real (ndash-j) * xold + real ( j ) * xnew ) / real ( ndash )
          yynew = ( real (ndash-j) * yold + real ( j ) * ynew ) / real ( ndash )
          call movcgm ( xxold, yyold )
          call drwcgm ( xxnew, yynew )
        end if

      end do

    end if

  end do

  return
end
function degrees_to_radians ( angle )
!
!*******************************************************************************
!
!! DEGREES_TO_RADIANS converts an angle from degrees to radians.
!
!
!  Modified:
!
!    10 July 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ANGLE, an angle in degrees.
!
!    Output, real DEGREES_TO_RADIANS, the equivalent angle
!    in radians.
!
  implicit none
!
  real, parameter :: pi = 3.14159265358979323846264338327950288419716939937510E+00

  real angle
  real degrees_to_radians

  degrees_to_radians = ( angle / 180.0E+00 ) * pi

  return
end
subroutine digten ( tenval, intval )
!
!*******************************************************************************
!
!! DIGTEN returns the integer value of a base 10 digit.
!
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
!    Input, character TENVAL, the decimal digit, '0' through '9'.
!
!    Output, integer ( kind = 4 ) INTVAL, the corresponding integer value.
!
  implicit none
!
  integer ( kind = 4 ) intval
  character tenval
!
  if ( lge ( tenval, '0' ) .and. lle ( tenval, '9' ) ) then

    intval = ichar ( tenval ) - 48

  else if ( tenval == ' ' ) then

    intval = 0

  else

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'DIGTEN - Serious error!'
    write ( *, '(a)' ) '  Illegal decimal digit = ' // tenval
    write ( *, '(a,i6)' ) '  ASCII number ', ichar ( tenval )
    intval = 0
    stop

  end if

  return
end
subroutine doaxes ( labelx, labely, title, title2, x4max, x4min, xdmax, &
  xdmin, y4max, y4min, ydmax, ydmin )
!
!*******************************************************************************
!
!! DOAXES places the title, the axes, and the axis labels.
!
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
!    Input, character ( len = 30 ) LABELX, LABELY, the labels for the axes.
!
!    Input, character ( len = 40 ) TITLE, a title for the plots.
!
!    Input, character ( len = 40 ) TITLE2, a subtitle used in the profile plots.
!
!    Input, real X4MAX, X4MIN, the maximum and minimum X
!    coordinates that  are used for the plot, not including axes.
!
!    Input, real XDMAX, XDMIN, the maximum and minimum values
!    to print out on the X axis.
!
!    Input, real Y4MAX, Y4MIN, the maximum and minimum Y
!    coordinates that are used for the plot, not including axes.
!
!    Input, real YDMAX, YDMIN, the maximum and minimum values
!    to print out on the Y axis.
!
  implicit none
!
  real angle
  character ( len = 14 ) chrrel
  character ( len = 14 ) ctemp
  real cwide
  real dshsiz
  real eps
  character ( len = 6 ) flush
  character ( len = * ) labelx
  character ( len = * ) labely
  integer ( kind = 4 ) nval
  real pwide
  character ( len = * ) title
  character ( len = * ) title2
  real x4max
  real x4min
  real xdmax
  real xdmin
  real xtemp
  real xval(2)
  real y4max
  real y4min
  real ydmax
  real ydmin
  real ytemp
  real yval(2)
!
!  Draw the title.
!
  if ( len_trim ( title ) > 0 ) then
    angle = 0.0E+00
    cwide = 0.02E+00
    pwide = 1.0E+00
    xtemp = 0.5E+00
    ytemp = 0.90E+00
    flush = 'center'
    call s_plot ( angle, cwide, pwide, trim ( title ), xtemp, ytemp, flush )
  end if
!
!  Draw the subtitle.
!
  if ( len_trim ( title2 ) > 0 ) then
    angle = 0.0E+00
    cwide = 0.015E+00
    pwide = 1.0E+00
    xtemp = 0.5E+00
    ytemp = 0.86E+00
    flush = 'center'
    call s_plot ( angle, cwide, pwide, trim ( title2 ), xtemp, ytemp, flush )
  end if
!
!  Draw the X label.
!
  if ( len_trim ( labelx ) > 0 ) then
    angle = 0.0E+00
    cwide = 0.015E+00
    pwide = 1.0E+00
    xtemp = 0.5E+00
    ytemp = 0.15E+00
    flush = 'center'
    call s_plot ( angle, cwide, pwide, trim ( labelx ), xtemp, ytemp, flush )
  end if
!
!  Draw the Y label.
!
  if ( len_trim ( labely) > 0 ) then
    angle = 90.0E+00
    cwide = 0.015E+00
    pwide = 1.0E+00
    xtemp = 0.15E+00
    ytemp = 0.5E+00
    flush = 'center'
    call s_plot ( angle, cwide, pwide, trim ( labely ), xtemp, ytemp, flush )
  end if
!
!  Draw the X axis, and label it.
!
  eps = 0.025E+00

  call movcgm ( x4min-eps, y4min )
  call drwcgm ( x4max, y4min )

  ctemp = chrrel ( xdmin )
  call s_blank_delete ( ctemp )
  angle = 0.0E+00
  cwide = 0.010E+00
  pwide = 1.0E+00
  xtemp = x4min
  ytemp = y4min - 2.0E+00 * cwide
  flush = 'left'
  call s_plot ( angle, cwide, pwide, trim ( ctemp ), xtemp, ytemp, flush )

  ctemp = chrrel ( xdmax )
  call s_blank_delete ( ctemp )
  angle = 0.0E+00
  cwide = 0.010E+00
  pwide = 1.0E+00
  xtemp = x4max
  ytemp = y4min - 2.0E+00 * cwide
  flush = 'right'
  call s_plot ( angle, cwide, pwide, trim ( ctemp ), xtemp, ytemp, flush )
!
!  Draw the Y axis, and label it.
!
  call movcgm ( x4min, y4min-eps )
  call drwcgm ( x4min, y4max )

  ctemp = chrrel ( ydmin )
  call s_blank_delete ( ctemp )
  angle = 90.0E+00
  cwide = 0.010E+00
  pwide = 1.0E+00
  xtemp = x4min - 2.0E+00 * cwide
  ytemp = y4min
  flush = 'left'
  call s_plot ( angle, cwide, pwide, trim ( ctemp ), xtemp, ytemp, flush )

  ctemp = chrrel ( ydmax )
  call s_blank_delete ( ctemp )
  angle = 90.0E+00
  cwide = 0.010E+00
  pwide = 1.0E+00
  xtemp = x4min - 2.0E+00 * cwide
  ytemp = y4max
  flush = 'right'
  call s_plot ( angle, cwide, pwide, trim ( ctemp ), xtemp, ytemp, flush )
!
!  If zero occurs within the Y range, draw a dashed horizontal axis line.
!
  if ( ydmin < 0.0E+00 .and. 0.0E+00 < ydmax ) then
    ytemp = y4min + ( y4max - y4min ) * ( 0.0E+00 - ydmin ) / ( ydmax - ydmin )
    dshsiz = 0.005E+00
    nval = 2
    xval(1) = x4min
    yval(1) = ytemp
    xval(2) = x4max
    yval(2) = ytemp
    call dash_line ( nval, xval, yval, dshsiz )
  end if

  return
end
subroutine element_check ( maxelm, maxnpe, nelem, node, np, npe, xc, yc )
!
!*******************************************************************************
!
!! ELEMENT_CHECK checks elements for correct orientation.
!
!
!  Modified:
!
!    06 December 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) MAXELM, the maximum number of elements.
!
!    Input, integer ( kind = 4 ) MAXNPE, the maximum number of nodes per element.
!
!    Input, integer ( kind = 4 ) NELEM, the number of elements.
!
!    Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), the global node numbers in
!    each element.
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!    Input, integer ( kind = 4 ) NPE, the number of nodes per element.
!
!    Input, real XC(NP), the X coordinates of the nodes.
!
!    Input, real YC(NP), the Y coordinates of the nodes.
!
  implicit none
!
  integer ( kind = 4 ) maxelm
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) np
!
  real area
  integer ( kind = 4 ) i
  integer ( kind = 4 ) itemp
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j1
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) nedge
  integer ( kind = 4 ) neg
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) node(maxnpe,maxelm)
  integer ( kind = 4 ) npe
  integer ( kind = 4 ) npos
  integer ( kind = 4 ) nzer
  real xc(np)
  real yc(np)
!
  neg = 0
  npos = 0
  nzer = 0

  if ( npe == 3 ) then
    nedge = 3
  else if ( npe == 4 ) then
    nedge = 4
  else if ( npe == 6 ) then
    nedge = 3
  end if

  do i = 1, nelem
!
!  Compute the area.
!
    area = 0.0E+00

    do j = 1, nedge

      j1 = node(j,i)
      if ( j < nedge ) then
        j2 = node(j+1,i)
      else
        j2 = node(1,i)
      end if

      area = area + ( yc(j1) + yc(j2) ) * ( xc(j1) - xc(j2) )

    end do

    if ( area == 0.0E+00 ) then

      nzer = nzer + 1

      if ( nzer < 10 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'ELEMENT_CHECK - Warning'
        write ( *, * ) '  Element ', I, ' has zero area.'
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'Nloc  Nglob   X(N), Y(N)'
        write ( *, '(a)' ) ' '
        do j = 1, npe
          write ( *, '(2i6,2g14.6)' ) j, node(j,i), xc(node(j,i)), yc(node(j,i))
        end do
      else if ( nzer == 10 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'ELEMENT_CHECK - Warning!'
        write ( *, '(a)' ) '  This is the TENTH element with zero area.'
        write ( *, '(a)' ) '  No more warnings will be printed.'
      end if
!
!  Reverse the node ordering for elements with negative area.
!
    else if ( area < 0.0E+00 ) then

      neg = neg + 1

      if ( npe == 3 ) then
        call i_swap ( node(2,i), node(3,i) )
      else if ( npe == 4 ) then
        call i_swap ( node(2,i), node(4,i) )
      else if ( npe == 6 ) then
        call i_swap ( node(2,i), node(3,i) )
        call i_swap ( node(4,i), node(5,i) )
      end if

    else

      npos = npos + 1

    end if

  end do

  if ( nzer > 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'ELEMENT_CHECK - Warning!'
    write ( *, '(a,i6)' ) '  Elements with zero orientation  = ', nzer
    write ( *, '(a)' ) '  Do NOT plot this data!'
  else if ( neg > 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'ELEMENT_CHECK - Note:'
    write ( *, * ) '  Reoriented ', neg, ' elements.'
  end if

  return
end
subroutine example_element ( maxelm, maxnp, maxnpe, nelem, node, node_x, &
  node_y, np, npe )
!
!*******************************************************************************
!
!! EXAMPLE_ELEMENT sets up the example element data.
!
!
!  Modified:
!
!    23 June 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) MAXELM, the maximum number of elements.
!
!    Input, integer ( kind = 4 ) MAXNP, the maximum number of nodes.
!
!    Input, integer ( kind = 4 ) MAXNPE, the maximum number of nodes per element.
!
!    Output, integer ( kind = 4 ) NELEM, the number of elements.
!
!    Output, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), the global node numbers in 
!    each element.
!
!    Output, integer ( kind = 4 ) NP, the number of nodes.
!
!    Input, integer ( kind = 4 ) NPE, the number of nodes per element.
!
  implicit none
!
  integer ( kind = 4 ) maxelm
  integer ( kind = 4 ) maxnp
  integer ( kind = 4 ) maxnpe
!
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ip
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) nelem_x
  integer ( kind = 4 ) nelem_y
  integer ( kind = 4 ) node(maxnpe,maxelm)
  integer ( kind = 4 ) node_x
  integer ( kind = 4 ) node_y
  integer ( kind = 4 ) np
  integer ( kind = 4 ) npe
!
  nelem_x = 10
  nelem_y = 3

  if ( npe == 3 ) then

    node_x = nelem_x + 1
    node_y = nelem_y + 1

    nelem = 2 * nelem_x * nelem_y

  else if ( npe == 4 ) then

    node_x = nelem_x + 1
    node_y = nelem_y + 1

    nelem = nelem_x * nelem_y

  else if ( npe == 6 ) then

    node_x = 2 * nelem_x + 1
    node_y = 2 * nelem_y + 1

    nelem = 2 * nelem_x * nelem_y

  end if

  np = node_x * node_y

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'EXAMPLE_ELEMENT:'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i6)' ) '  Number of nodes per element = ', npe
  write ( *, '(a)' ) ' '
  write ( *, '(a,i6)' ) '  Number of nodes in the X direction = ', node_x
  write ( *, '(a,i6)' ) '  Number of nodes in the Y direction = ', node_y
  write ( *, '(a,i6)' ) '  Total number of nodes, NP =    ', np
  write ( *, '(a)' ) ' '
  write ( *, '(a,i6)' ) '  Number of elements in the X direction = ', nelem_x
  write ( *, '(a,i6)' ) '  Number of elements in the Y direction = ', nelem_y
  write ( *, '(a,i6)' ) '  Total number of elements, NELEM = ', nelem
!
!  Define elements.
!
!
!  IP+1  IP+NODE_Y+1
!
!  IP    IP+NODE_Y
!
  if ( npe == 3 ) then

    k = 0

    do i = 1, node_x - 1
      do j = 1, node_y - 1

        ip = ( i - 1 ) * node_y + j

        k = k + 1
        node(1,k) = ip + node_y
        node(2,k) = ip + 1
        node(3,k) = ip

        k = k + 1
        node(1,k) = ip + 1
        node(2,k) = ip + node_y
        node(3,k) = ip + node_y + 1

      end do
    end do
!
!  IP+1  IP+NODE_Y+1
!
!  IP    IP+NODE_Y
!
  else if ( npe == 4 ) then

    k = 0

    do i = 1, node_x - 1
      do j = 1, node_y - 1

        ip = ( i - 1 ) * node_y + j

        k = k + 1
        node(1,k) = ip + node_y
        node(2,k) = ip + node_y + 1
        node(3,k) = ip + 1
        node(4,k) = ip

      end do
    end do
!
!  IP+2  IP+NODE_Y+2  IP+2*NODE_Y+2
!
!  IP+1  IP+NODE_Y+1  IP+2*NODE_Y+1
!
!  IP    IP+NODE_Y    IP+2*NODE_Y
!
  else if ( npe == 6 ) then

    k = 0

    do i = 1, node_x-2, 2
      do j = 1, node_y-2, 2

        ip = ( i - 1 ) * node_y + j

        k = k + 1
        node(1,k) = ip + 2 * node_y
        node(2,k) = ip + 2
        node(3,k) = ip
        node(4,k) = ip + node_y
        node(5,k) = ip + node_y + 1
        node(6,k) = ip + 1

        k = k + 1
        node(1,k) = ip + 2
        node(2,k) = ip + 2 * node_y
        node(3,k) = ip + 2 * node_y + 2
        node(4,k) = ip + node_y + 2
        node(5,k) = ip + node_y + 1
        node(6,k) = ip + 2 * node_y + 1

      end do
    end do

  end if

  return
end
subroutine example_node ( name, node_x, node_y, np, nq, v )
!
!*******************************************************************************
!
!! EXAMPLE_NODE sets up the example node data.
!
!
!  Modified:
!
!    23 June 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, character ( len = 10 ) NAME(NQ), the names of the quantities.
!
!    Input, integer ( kind = 4 ) NODE_X, NODE_Y, the number of nodes in the X and Y
!    grid directions.
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!    Input, integer ( kind = 4 ) NQ, the number of node quantities.
!
!    Output, real V(NP,NQ), the node quantities.
!
  implicit none
!
  integer ( kind = 4 ) np
  integer ( kind = 4 ) nq
!
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  character ( len = 10 ) name(nq)
  integer ( kind = 4 ) node_x
  integer ( kind = 4 ) node_y
  real v(np,nq)
  real xlen
  real ylen
!
!  Set the region size.
!
  xlen = 10.0E+00
  ylen = 3.0E+00

  name(1) = 'X'
  name(2) = 'Y'
  name(3) = 'R'
  name(4) = 'U'
  name(5) = 'V'
  name(6) = 'P'
!
!  Define node positions, and values at nodes.
!
  k = 0

  do i = 1, node_x

    do j = 1, node_y

      k = k + 1

      v(k,1) = real ( i - 1 ) * xlen / real ( node_x - 1 )
      v(k,2) = real ( j - 1 ) * ylen / real ( node_y - 1 )
      v(k,3) = 1.0E+00
      v(k,4) = v(k,2) * ( ylen - v(k,2) )
      v(k,5) = 0.0E+00
      v(k,6) = - 2.0E+00 * v(k,1)

    end do
  end do

  return
end
subroutine fe_gradient ( dudxn, dudyn, maxnpe, nelem, node, np, npe, numel, &
  u, xc, yc )
!
!*******************************************************************************
!
!! FE_GRADIENT approximates dUdX and dUdY at mesh nodes.
!
!
!  Discussion:
!
!    The routine seeks an approximation to the partial derivatives
!    dUdX and dUdY at each such node.  However, the finite element
!    representation of U is only continuous, not continously differentiable,
!    at the nodes, which lie on the element boundaries.  Therefore, the
!    value of dUdX and dUdY depends on which element the node is assumed
!    to lie in.
!
!    For each node, this routine evaluates dUdX and dUdY in every element
!    that contains the node, and then averages the results, to give a
!    reasonable estimate for the value of the derivatives.
!
!  Modified:
!
!    15 October 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real DUDXN(NP), an estimate for dU/dX at each node.
!
!    Output, real DUDXY(NP), an estimate for dU/dY at each node.
!
!    Input, integer ( kind = 4 ) MAXNPE, the maximum number of nodes per element.
!
!    Input, integer ( kind = 4 ) NELEM, the number of elements.
!
!    Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), the global node numbers in 
!    each element.
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!    Input, integer ( kind = 4 ) NPE, the number of nodes per element.
!
!    Input, real U(MAXNP), the value of the quantity U at node I.
!
!    Input, real XC(MAXNP), the X coordinates of the nodes.
!
!    Input, real YC(MAXNP), the Y coordinates of the nodes.
!
  implicit none
!
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) np
!
  real det
  real detadx
  real detady
  real dudx
  real dudxn(np)
  real dudy
  real dudyn(np)
  real dwdeta
  real dwdxsi
  real dwdx
  real dwdy
  real dxdeta
  real dxdxsi
  real dxsidx
  real dxsidy
  real dydeta
  real dydxsi
  real eta
  real etan(6)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ielem
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) ip
  integer ( kind = 4 ) iq
  integer ( kind = 4 ) jp
  integer ( kind = 4 ) jq
  integer ( kind = 4 ) node(maxnpe,nelem)
  integer ( kind = 4 ) npe
  integer ( kind = 4 ) numel(np)
  real u(np)
  real w
  real xc(np)
  real xsi
  real xsin(6)
  real yc(np)
!
!  Get the nodal coordinates of the reference element.
!
  call refnode ( npe, xsin, etan )
!
!  Estimate dUdX and dUdY at each node.
!
!  We have a problem, because these quantities are not continuously
!  defined in all directions at nodes, which lie on the border between
!  two or more elements.
!
!  In order to assign some reasonable value to these quantities,
!  we look at each node, count the number of elements in which it
!  lies, evaluate the quantity within each element, and average the
!  result.
!
  numel(1:np) = 0
  dudxn(1:np) = 0.0E+00
  dudyn(1:np) = 0.0E+00
!
!  Loop over all the elements.
!
  do ielem = 1, nelem
!
!  At each node in the element, estimate dUdX and dUdY.
!
    do iq = 1, npe

      ip = node(iq,ielem)

      eta = etan(iq)
      xsi = xsin(iq)

      call transform ( det, detadx, detady, dxdeta, dxdxsi, &
        dxsidx, dxsidy, dydeta, dydxsi, eta, ielem, ierror, &
        maxnpe, nelem, node, np, npe, xc, xsi, yc )
!
!  Compute dUdX and dUdY by summing over the weighted derivatives of the
!  local finite element basis functions.  Here, we use the fact that the
!  value of U at node JQ is the coefficient of the finite element basis
!  function associated with that node.
!
      dudx = 0.0E+00
      dudy = 0.0E+00

      do jq = 1, npe

        jp = node(jq,ielem)

        if ( npe == 3 ) then

          call refbf_l3 ( w, dwdeta, dwdxsi, eta, jq, xsi )

        else if ( npe == 4 ) then

          call refbf_l4 ( w, dwdeta, dwdxsi, eta, jq, xsi )

        else if ( npe == 6 ) then

          call refbf_q6 ( w, dwdeta, dwdxsi, eta, jq, xsi )

        end if

        dwdx = dwdxsi * dxsidx + dwdeta * detadx
        dwdy = dwdxsi * dxsidy + dwdeta * detady

        dudx = dudx + u(jp) * dwdx
        dudy = dudy + u(jp) * dwdy

      end do
!
!  Add the estimates for dUdX and dUdY to the running total.
!  Count how many estimates we have made at each node,
!  so that we can average them later.
!
      dudxn(ip) = dudxn(ip) + dudx
      dudyn(ip) = dudyn(ip) + dudy
      numel(ip) = numel(ip) + 1

    end do

  end do
!
!  Take the average value of the quantities over all the
!  different elements along whose boundaries they are defined.
!
  do i = 1, np
    if ( numel(i) == 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FE_GRADIENT - Warning!'
      write ( *, * ) '  Node ', i, ' occurs in no element!'
    else
      dudxn(i) = dudxn(i) / real ( numel(i) )
      dudyn(i) = dudyn(i) / real ( numel(i) )
    end if
  end do

  return
end
subroutine file_column_count ( file_name, ncolumn )
!
!*******************************************************************************
!
!! FILE_COLUMN_COUNT counts the number of columns in the first line of a file.
!
!
!  Discussion:
!
!    The file is assumed to be a simple text file.
!
!    Most lines of the file is presumed to consist of NCOLUMN words, separated
!    by spaces.  There may also be some blank lines, and some comment lines,
!    which have a "#" in column 1.
!
!    The routine tries to find the first non-comment non-blank line and
!    counts the number of words in that line.
!
!    If all lines are blanks or comments, it goes back and tries to analyze
!    a comment line.
!
!  Modified:
!
!    21 June 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) FILE_NAME, the name of the file.
!
!    Output, integer ( kind = 4 ) NCOLUMN, the number of columns assumed to be in the file.
!
  implicit none
!
  character ( len = * ) file_name
  logical got_one
  integer ( kind = 4 ) ios
  integer ( kind = 4 ) iunit
  character ( len = 256 ) line
  integer ( kind = 4 ) ncolumn
!
!  Open the file.
!
  call get_unit ( iunit )

  open ( unit = iunit, file = file_name, status = 'old', form = 'formatted', &
    access = 'sequential', iostat = ios )

  if ( ios /= 0 ) then
    ncolumn = - 1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FILE_COLUMN_COUNT - Fatal error!'
    write ( *, '(a)' ) '  Could not open the file:'
    write ( *, '(4x,a)' ) '"' // trim ( file_name ) // '".'
    return
  end if
!
!  Read one line, but skip blank lines and comment lines.
!
  got_one = .false.

  do

    read ( iunit, '(a)', iostat = ios ) line

    if ( ios /= 0 ) then
      exit
    end if

    if ( len_trim ( line ) == 0 ) then
      cycle
    end if

    if ( line(1:1) == '#' ) then
      cycle
    end if

    got_one = .true.
    exit

  end do

  if ( .not. got_one ) then

    rewind ( iunit )

    do

      read ( iunit, '(a)', iostat = ios ) line

      if ( ios /= 0 ) then
        exit
      end if

      if ( len_trim ( line ) == 0 ) then
        cycle
      end if

      got_one = .true.
      exit

    end do

  end if

  close ( unit = iunit )

  if ( .not. got_one ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FILE_COLUMN_COUNT - Warning!'
    write ( *, '(a)' ) '  The file does not seem to contain any data.'
    ncolumn = 0
    return
  end if

  call word_count ( line, ncolumn )

  return
end
subroutine file_line_count ( file_name, nline )
!
!*******************************************************************************
!
!! FILE_LINE_COUNT counts the number of lines in a file.
!
!
!  Discussion:
!
!    The file is assumed to be a simple text file.
!
!    Blank lines and comment lines, which begin with '#', are not counted.
!
!  Modified:
!
!    21 June 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) FILE_NAME, the name of the file.
!
!    Output, integer ( kind = 4 ) NLINE, the number of lines found in the file.
!    If the file could not be opened, then NLINE is returned as -1.
!
  implicit none
!
  character ( len = * ) file_name
  integer ( kind = 4 ) ios
  integer ( kind = 4 ) iunit
  character ( len = 256 ) line
  integer ( kind = 4 ) nline
  logical, parameter :: verbose = .false.
!
  nline = 0
!
!  Open the file.
!
  call get_unit ( iunit )

  open ( unit = iunit, file = file_name, status = 'old', form = 'formatted', &
    access = 'sequential', iostat = ios )

  if ( ios /= 0 ) then

    nline = - 1

    if ( verbose ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FILE_LINE_COUNT - Fatal error!'
      write ( *, '(a)' ) '  Could not open the file:'
      write ( *, '(4x,a)' ) '"' // trim ( file_name ) // '".'
    end if

    return

  end if
!
!  Count the lines.
!
  do

    read ( iunit, '(a)', iostat = ios ) line

    if ( ios /= 0 ) then
      exit
    end if

    if ( len_trim ( line ) == 0 ) then
      cycle
    end if

    if ( line(1:1) == '#' ) then
      cycle
    end if

    nline = nline + 1

  end do

  close ( unit = iunit )

  return
end
subroutine fillin ( narray, x1, x2, xarray )
!
!*******************************************************************************
!
!! FILLIN sets array entries to rise linearly from X1 to X2.
!
!
!  Discussion:
!
!    Normally, XARRAY will include the endpoints X1 and X2.  Thus, to
!    generate the points 0.0, 0.1, 0.2, ..., 0.9, 1.0, you would
!    pass
!
!      X1 = 0.0, X2=1.0, NARRAY=11.
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
!    Input, integer ( kind = 4 ) NARRAY, the number of points to generate.
!    If NARRAY = 1, then XARRAY(1)=0.5*(X1+X2).
!
!    Input, real X1, X2, the endpoints of the list of values.
!
!    Output, real XARRAY(NARRAY), evenly spaced values with
!    XARRAY(1) = X1 and XARRAY(NARRAY)=X2.
!
  implicit none
!
  integer ( kind = 4 ) narray
!
  integer ( kind = 4 ) i
  real x1
  real x2
  real xarray(narray)
!
  if ( narray <= 0 ) then

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FILLIN - Fatal error!'
    write ( *, '(a)' ) '  NARRAY is not positive!'
    stop

  else if ( narray == 1 ) then

    xarray(1) = 0.5E+00 * ( x1 + x2 )

  else

    xarray(1) = x1

    do i = 2, narray-1
      xarray(i) = ( real ( narray - i ) * x1 + real ( i - 1 ) * x2 ) &
        / real ( narray - 1 )
    end do

    xarray(narray) = x2

  end if

  return
end
subroutine fillin2 ( nfill, np1, x1, x2 )
!
!*******************************************************************************
!
!! FILLIN2 "expands" a vector of data by linear interpolation.
!
!
!  Discussion:
!
!    FILLIN2 accepts an array X1 of NP1 points, and computes a new
!    array X2, which includes all the points X1, plus NFILL new
!    points between each pair, with NFILL more points after the last
!    entry.  The inserted points are linearly interpolated.
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
!    Input, integer ( kind = 4 ) NFILL, the number of fillin points to insert between
!    every pair of points.
!
!    Input, integer ( kind = 4 ) NP1, the number of data points in the input vector.
!
!    Input, real X1(NP1), the data, between which fillin
!    points are to be inserted.
!
!    Output, real X2((NFILL+1)*NP1), the X1 data, with
!    linearly interpolated fillin points.
!
  implicit none
!
  integer ( kind = 4 ) nfill
  integer ( kind = 4 ) np1
!
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) np2
  real x1(np1)
  real x2((nfill+1)*np1)
  real xhi
  real xlo
!
  if ( np1 <= 0 ) then

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FILLIN2 - Fatal error!'
    write ( *, '(a)' ) '  NP1 is not positive!'
    stop

  else

    np2 = 0

    do i = 1, np1

      xlo = x1(i)

      if ( i < np1 ) then
        xhi = x1(i+1)
      else
        xhi = x1(1)
      end if

      do j = 0, nfill
        np2 = np2 + 1
        x2(np2) = ( real ( nfill + 1 - j ) * xlo + real ( j ) * xhi ) &
          / real ( nfill + 1 )
      end do

    end do

  end if

  return
end
subroutine flushl ( string )
!
!*******************************************************************************
!
!! FLUSHL flushes a string left.
!
!
!  Discussion:
!
!    Both blanks and tabs are treated as "white space".
!
!  Examples:
!
!    Input             Output
!
!    '     Hello'      'Hello     '
!    ' Hi there!  '    'Hi there!   '
!    'Fred  '          'Fred  '
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
!    Input/output, character ( len = * ) STRING.
!
!    On input, STRING is a string of characters.
!
!    On output, any initial blank or tab characters in STRING
!    have been cut.
!
  implicit none
!
  integer ( kind = 4 ) i
  integer ( kind = 4 ) lchar
  integer ( kind = 4 ) nonb
  character ( len = * ) string
!
!  Check the length of the string to the last nonblank.
!  If nonpositive, return.
!
  lchar = len_trim ( string )

  if ( lchar <= 0 ) then
    return
  end if
!
!  Find NONB, the location of the first nonblank, nontab.
!
  do i = 1, lchar

    nonb = i

    if ( string(i:i) /= ' ' .and. string(i:i) /= char(9) ) then
      go to 10
    end if

  end do

  string = ' '

  return

10    continue
!
!  Shift the string left.
!
  if ( nonb > 1 ) then
    do i = 1, lchar + 1 - nonb
      string(i:i) = string(i+nonb-1:i+nonb-1)
    end do
  end if
!
!  Blank out the end of the string.
!
  string(lchar+2-nonb:lchar) = ' '

  return
end
subroutine frame_visible_elements ( eflagu, grace, maxelm, maxnpe, nelem, &
  node, np, npe, srange, x1max, x1min, x2max, x2min, xc, xsmax, xsmin, &
  y1max, y1min, y2max, y2min, yc, ysmax, ysmin )
!
!*******************************************************************************
!
!! FRAME_VISIBLE_ELEMENTS adjusts the data limits to the visible elements.
!
!
!  Modified:
!
!    23 June 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
  implicit none
!
  integer ( kind = 4 ) maxelm
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) np
!
  logical eflagu(nelem)
  real grace
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) node(maxnpe,maxelm)
  logical none
  integer ( kind = 4 ) npe
  real srange
  real x1max
  real x1min
  real x2max
  real x2min
  real xc(np)
  real xsmax
  real xsmin
  real y1max
  real y1min
  real y2max
  real y2min
  real yc(np)
  real ysmax
  real ysmin
!
  none = .true.

  do i = 1, nelem
    if ( eflagu(i) ) then
      do j = 1, npe
        k = node(j,i)
        if ( none ) then
          xsmin = xc(k)
          xsmax = xc(k)
          ysmin = yc(k)
          ysmax = yc(k)
          none = .false.
        else
          xsmin = min ( xsmin, xc(k) )
          xsmax = max ( xsmax, xc(k) )
          ysmin = min ( ysmin, yc(k) )
          ysmax = max ( ysmax, yc(k) )
        end if
      end do
    end if
  end do

  if ( none ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FRAME_VISIBLE_ELEMENTS - Warning!'
    write ( *, '(a)' ) '  No visible elements were found!'
    return
  end if

  call pltbox ( grace, srange, x1max, x1min, x2max, x2min, &
    xsmax, xsmin, y1max, y1min, y2max, y2min, ysmax, ysmin )

  return
end
subroutine fsize ( nflag, np, r, rtmax, rtmin, rvmax, rvmin )
!
!*******************************************************************************
!
!! FSIZE computes the maximum and minimum entries of a node array.
!
!
!  Discussion:
!
!    Ranges are given both for the "total" array and the "visible" array.
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
!    Input, logical NFLAG(NP), flags nodes which are active.
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!    Input, real R(NP), the real array to be examined.
!
!    Output, real RTMAX, RTMIN, the maximum and minimum values in R.
!
!    Output, real RVMAX, RVMIN, the maximum and minimum values
!    of R at visible nodes.
!
  implicit none
!
  integer ( kind = 4 ) np
!
  integer ( kind = 4 ) i
  logical nflag(np)
  real r(np)
  real rtmax
  real rtmin
  real rvmax
  real rvmin
  logical startrt
  logical startrv
!
  startrt = .false.
  rtmax = 0.0E+00
  rtmin = 0.0E+00

  startrv = .false.
  rvmax = 0.0E+00
  rvmin = 0.0E+00

  do i = 1, np

    if ( startrt ) then
      rtmin = min ( rtmin, r(i) )
      rtmax = max ( rtmax, r(i) )
    else
      startrt = .true.
      rtmin = r(i)
      rtmax = r(i)
    end if

    if ( nflag(i) ) then

      if ( startrv ) then
        rvmin = min ( rvmin, r(i) )
        rvmax = max ( rvmax, r(i) )
      else
        startrv = .true.
        rvmin = r(i)
        rvmax = r(i)
      end if

    end if

  end do

  return
end
subroutine get_unit ( iunit )
!
!*******************************************************************************
!
!! GET_UNIT returns a free FORTRAN unit number.
!
!
!  Discussion:
!
!    A "free" FORTRAN unit number is an integer between 1 and 99 which
!    is not currently associated with an I/O device.  A free FORTRAN unit
!    number is needed in order to open a file with the OPEN command.
!
!  Modified:
!
!    02 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) IUNIT.
!
!    If IUNIT = 0, then no free FORTRAN unit could be found, although
!    all 99 units were checked (except for units 5 and 6).
!
!    Otherwise, IUNIT is an integer between 1 and 99, representing a
!    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
!    are special, and will never return those values.
!
  implicit none
!
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ios
  integer ( kind = 4 ) iunit
  logical lopen

  iunit = 0

  do i = 1, 99

    if ( i /= 5 .and. i /= 6 ) then

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
subroutine getwin ( grace, srange, xmax, xmin, x1max, x1min, x2max, &
  x2min, xsmax, xsmin, ymax, ymin, y1max, y1min, y2max, y2min, ysmax, ysmin )
!
!*******************************************************************************
!
!! GETWIN reports the current window and gets new values from the user.
!
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
!    Input, real GRACE, the size of the "grace" margin.
!
!    Output, real SRANGE.
!    The maximum of XSMAX-XSMIN and YSMAX-YSMIN.
!    This gives the size of a square containing the data
!    window.
!
!    Input, real XMAX.
!    The maximum X coordinate of all the nodes.
!    The maximum entry in the XC array.
!
!    Input, real XMIN.
!    The minimum X coordinate of all the nodes.
!    The minimum entry in the XC array.
!
!    Input/output, real X1MAX, X1MIN, the maximum and minimum X
!    coordinates of the plot, which includes a small grace margin.
!
!    Input/output, real X2MAX, X2MIN, the maximum and minimum X
!    coordinates that should be used for plotting.  No plotting
!    commands should exceed these values.  This is where the
!    "frame" might be drawn.
!
!    Input/output, real XSMAX.
!    The maximum X coordinate of the data to be displayed.
!    XSMAX defaults to XMAX, but can be made smaller to
!    focus on a portion of the region.
!
!    Input/output, real XSMIN.
!    The minimum X coordinate of the data to be displayed.
!    XSMIN defaults to XMIN, but can be made larger to
!    focus on a portion of the region.
!
!    Input/output, real Y1MAX, Y1MIN, the maximum and minimum Y
!    coordinates of the plot, which includes a small grace margin.
!
!    Input/output, real Y2MAX, Y2MIN, the maximum and minimum Y
!    coordinates that should be used for plotting.  No plotting commands
!    should  exceed these values.  This is where the "frame" might be
!    drawn.
!
!    Input, real YMAX, the maximum Y coordinate of all the nodes.
!
!    Input, real YMIN, the minimum Y coordinate of all the nodes.
!
!    Input/output, real YSMAX.
!    The maximum Y coordinate of the data to be displayed.
!    YSMAX defaults to YMAX, but can be made smaller to
!    focus on a portion of the region.
!
!    Input/output, real YSMIN, the minimum displayed Y coordinate.
!
  implicit none
!
  real grace
  real srange
  real x1max
  real x1min
  real x2max
  real x2min
  real xmax
  real xmin
  real xsmax
  real xsmin
  real y1max
  real y1min
  real y2max
  real y2min
  real ymax
  real ymin
  real ysmax
  real ysmin
!
10    continue

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'GETWIN:'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Total picture coordinates:'
  write ( *, '(a)' ) ' '
  write ( *, * ) x1min, ' X1MIN < =  X <= X1MAX ', x1max
  write ( *, * ) y1min, ' Y1MIN < =  Y <= Y1MAX ', y1max
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Graphing area coordinates:'
  write ( *, '(a)' ) ' '
  write ( *, * ) x2min, ' X2MIN < =  X <= X2MAX ', x2max
  write ( *, * ) y2min, ' Y2MIN < =  Y <= Y2MAX ', y2max
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Displayed data coordinates:'
  write ( *, '(a)' ) ' '
  write ( *, * ) xsmin, ' XSMIN < =  X <= XSMAX ', xsmax
  write ( *, * ) ysmin, ' YSMIN < =  Y <= YSMAX', ysmax
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Data coordinates:'
  write ( *, '(a)' ) ' '
  write ( *, * ) xmin, ' < =  X <= ', xmax
  write ( *, * ) ymin, ' < =  Y <= ', ymax
  write ( *, * ) ' '

  write ( *, '(a)' ) '  Enter new X, Y displayed data limits:'
  write ( *, '(a)' ) '  Use the order Xsmin, Xsmax, Ysmin, Ysmax:'

  read ( *, *, err = 20, end = 20 ) xsmin, xsmax, ysmin, ysmax
!
!  Compute box containing data.
!
  call pltbox ( grace, srange, x1max, x1min, x2max, x2min, xsmax, &
    xsmin, y1max, y1min, y2max, y2min, ysmax, ysmin )

  return

20    continue

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'GETWIN - Warning!'
  write ( *, '(a)' ) '  Your input was not acceptable!'
  write ( *, '(a)' ) '  Please try again!'
  write ( *, '(a)' ) ' '
  go to 10

end
subroutine gquad ( ngauss, wq, xq )
!
!*******************************************************************************
!
!! GQUAD returns the weights and abscissas for a 3 point Gauss rule.
!
!
!  Discussion:
!
!    The rule is for a 1 dimensional, 3 point Gauss quadrature rule defined
!    on the interval [-1,1].
!
!    The integral of a function F(X) over the interval [-1,1]
!
!      Integral (-1 to 1) F(X) DX
!
!    may then be approximated by
!
!      Sum (I = 1 to 3) WQ(I) * F(XQ(I))
!
!  Modified:
!
!    27 September 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NGAUSS, the order of the Gauss rule.
!
!    Output, real WQ(NGAUSS), the weight factors.
!
!    Output, real XQ(NGAUSS), the quadrature points.
!
  implicit none
!
  integer ( kind = 4 ) ngauss
!
  real wq(ngauss)
  real xq(ngauss)
!
  if ( ngauss == 3 ) then

    xq(1) = -0.77459666E+00
    xq(2) =  0.0E+00
    xq(3) =  0.77459666E+00

    wq(1) = 5.0E+00 / 9.0E+00
    wq(2) = 8.0E+00 / 9.0E+00
    wq(3) = 5.0E+00 / 9.0E+00

  else

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GQUAD - Fatal error!'
    write ( *, '(a,i6)' ) '  Unacceptable value of NGAUSS = ', ngauss
    stop

  end if

  return
end
subroutine graph ( arrow, c_contour, delx, dely, dev, dudxn, dudyn, dvdxn, &
  dvdyn, eflag, eflagu, etaref, filgrf, icmax, icmin, icolor, iplot, &
  itable, iwork1, iwork2, jbound, jcmax, jcmin, lbar, line, maxbou, &
  maxcontour, maxelm, maxnp, maxnpe, maxobj, nbound, ncontour, nelem, nflag, &
  nflag0, nflag1, node, np, npe, nq, numel, object, ovrlay, p, rho, s, &
  s_contour, &
  s2, scalee, scalen, scalev, show, smax, smin, srange, t, title, title2, u, &
  v, x1max, x1min, x2max, x2min, xc, xsiref, xsmax, xsmin, y1max, y1min, &
  y2max, y2min, yc, ysmax, ysmin )
!
!*******************************************************************************
!
!! GRAPH draws the graph.
!
!
!  Modified:
!
!    15 July 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real DELX.
!    The X spacing between nodes.  In some cases,
!    this spacing is modified to create isoparametric elements.
!
!    Input, real DELY.
!    The Y spacing between nodes.  In some cases,
!    this spacing is modified to create isoparametric elements.
!
!    Input, character ( len = 10 ) DEV, the graphics output device:
!      cgmb - CGM binary file.
!      ps   - PostScript file.
!      XWS  - X window screen (interactive).
!
!    Input, logical EFLAG(NELEM), element visibility flags.
!
!    Input, logical EFLAGU(NELEM), allows the user to mark
!    certain elements not to be displayed.
!
!    Input, real ETAREF(MAXNPE), the ETA coordinates of the reference nodes.
!
!    Input, character ( len = 80 ) FILGRF, the name of the output
!    graphics file.
!
!    Input, integer ( kind = 4 ) ICMAX, ICMIN, the maximum and minimum color
!    indices to use in color contour graphics.
!
!    Input, integer ( kind = 4 ) ICOLOR(MAXOBJ), the color index for each object.
!
!    Input, integer ( kind = 4 ) IPLOT, the number of plots made so far.
!
!    Input, integer ( kind = 4 ) ITABLE, the desired color table.
!
!    1: low black to high white
!    2: low blue to high yellow
!    3: low red, high blue, with bands between.
!    4: low red, yellow, green, blue, high white.
!    5: low white, blue, green, yellow, high red.
!    6: low blue to high red
!    7: linear table between 2 user colors.
!    8: linear table between N user colors.
!    9: low white to high black.
!
!    Workspace, integer IWORK1(NELEM).
!
!    Workspace, integer IWORK2(NP).
!
!    Input, integer ( kind = 4 ) JBOUND(5,MAXBOU)
!
!    For each line segment of the boundary:
!
!    JBOUND(1,I) contains the element number;
!
!    JBOUND(2,I) contains the local node number of one corner
!      of the element, which forms the edge;
!
!    JBOUND(2,I) contains the "next" node along the edge.
!      If the element is linear, this is the other corner node.
!      If the element is quadratic, this is the midside node along
!        the edge.
!
!    JBOUND(4,I) contains the "next" node along the edge.
!      If the element is linear, this is 0.
!      If the element is quadratic, this is the other corner node
!        along the edge.
!
!    JBOUND(5,I) contains:
!      0 if the boundary is a wall (U = V=0);
!      1 if the boundary is open.
!
!    Input, integer ( kind = 4 ) JCMAX, JCMIN, the maximum and minimum color
!    indices to use for contours.
!
!    Input, logical LBAR, .TRUE. if the color bar may be drawn,
!    .FALSE. if it should not be drawn.
!
!    Input, integer ( kind = 4 ) MAXBOU.
!    The amount of storage available for the IBOUND array.
!
!    Input, integer ( kind = 4 ) MAXELM, the maximum number of elements.
!
!    Input, integer ( kind = 4 ) MAXNP, the maximum number of nodes.
!
!    Input, integer ( kind = 4 ) MAXNPE, the maximum number of nodes per element.
!
!    Input, integer ( kind = 4 ) MAXOBJ.
!    The number of graphical "objects".
!
!    Input, integer ( kind = 4 ) NBOUND, the number of points defining the boundary.
!
!    Input, integer ( kind = 4 ) NCONTOUR, the number of contours to draw.
!
!    Input, integer ( kind = 4 ) NELEM, the number of elements.
!
!    Input, logical NFLAG(MAXNP), flags nodes which are active.
!
!    Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), the global node numbers in 
!    each element.
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!    Input, integer ( kind = 4 ) NPE, the number of nodes per element.
!
!    Input, integer ( kind = 4 ) NY.
!    Determines the number of nodes and elements in the Y
!    direction.  There will be 2*NY-1 nodes, 2*NY-2 elements.
!
!    Input, character ( len = 30 ) OBJECT(MAXOBJ), the names of the
!    graphical objects.
!
!    Input, logical OVRLAY.
!    If OVRLAY is true, then the next time that a plot is
!    requested, a "new frame" command is suppressed, so that
!    the new plot is shown on top of the previous one.
!
!    Input, real P(MAXNP), the pressure at node I.
!
!    Workspace, real S(MAXNP).
!    S is used to store the stream function, or certain other
!    scalar functions derived from the basic flow.
!
!    Input, real SCALEE.
!    The scale factor for the element numbers, with default value 1.
!
!    Input, real SCALEN.
!    The scale factor for the node numbers, with default value 1.
!
!    Input, real SCALEV.
!    A scale factor for velocity vectors.  This starts out at 1.0.
!
!    Input, logical SHOW(MAXOBJ).
!    Contains, for each object, a flag determining whether it
!    is to be shown or not.
!
!    Input, real SRANGE.
!    The maximum of XSMAX-XSMIN and YSMAX-YSMIN.
!    This gives the size of a square containing the data
!    window.
!
!    Workspace, real T(MAXNP).
!
!    Input, character ( len = 40 ) TITLE.
!    A title for the plots.
!
!    Input, character ( len = 40 ) TITLE2.
!    A subtitle used in the profile plots.
!
!    Input, real U(MAXNP), the horizontal fluid velocity at node I.
!
!    Input, real V(MAXNP), the vertical fluid velocity at node I.
!
!    Input, real X1MAX, X1MIN, the maximum and minimum X
!    coordinates of the plot, which includes a small grace margin.
!
!    Input, real X2MAX, X2MIN, the maximum and minimum X coordinates that
!    should be used for plotting.  No plotting commands should
!    exceed these values.  This is where the "frame" might be drawn.
!
!    Input, real XC(MAXNP), the X coordinates of the nodes.
!
!    Input, real XSIREF(MAXNPE), the XSI coordinates of the reference nodes.
!
!    Input, real XSMAX.
!    The maximum X coordinate of the data to be displayed.
!    XSMAX defaults to XMAX, but can be made smaller to
!    focus on a portion of the region.
!
!    Input, real XSMIN.
!    The minimum X coordinate of the data to be displayed.
!    XSMIN defaults to XMIN, but can be made larger to
!    focus on a portion of the region.
!
!    Input, real Y1MAX, Y1MIN, the maximum and minimum Y
!    coordinates of the plot, which includes a small grace margin.
!
!    Input, real Y2MAX, Y2MIN, the maximum and minimum Y coordinates that
!    should be used for plotting.  No plotting commands should
!    exceed these values.  This is where the "frame" might be drawn.
!
!    Input, real YC(MAXNP), the Y coordinates of the nodes.
!
!    Input, real YSMAX.
!    The maximum Y coordinate of the data to be displayed.
!    YSMAX defaults to YMAX, but can be made smaller to
!    focus on a portion of the region.
!
!    Input, real YSMIN, the minimum displayed Y coordinate.
!
  implicit none
!
  integer ( kind = 4 ) maxbou
  integer ( kind = 4 ) maxcontour
  integer ( kind = 4 ) maxelm
  integer ( kind = 4 ) maxnp
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) maxobj
  integer ( kind = 4 ) nq
!
  real angle
  character ( len = 10 ) arrow
  integer ( kind = 4 ) c_contour(maxcontour)
  character ( len = 6 ) chrtmp
  real csize
  real cwide
  real delx
  real dely
  character ( len = 10 ) dev
  real dudxn(maxnp)
  real dudyn(maxnp)
  real dvdxn(maxnp)
  real dvdyn(maxnp)
  logical eflag(maxelm)
  logical eflagu(maxelm)
  real etaref(maxnpe)
  character ( len = 80 ) filgrf
  logical filled
  character ( len = 6 ) flush
  integer ( kind = 4 ) i
  integer ( kind = 4 ) iblack
  integer ( kind = 4 ) icmax
  integer ( kind = 4 ) icmin
  integer ( kind = 4 ) icolor(maxobj)
  integer ( kind = 4 ) ido
  integer ( kind = 4 ) iplot
  integer ( kind = 4 ) itable
  integer ( kind = 4 ) iwork1(maxelm)
  integer ( kind = 4 ) iwork2(maxnp)
  integer ( kind = 4 ) jbound(5,maxbou)
  integer ( kind = 4 ) jcmax
  integer ( kind = 4 ) jcmin
  integer ( kind = 4 ) jcolor
  logical lbar
  logical s_eqi
  integer ( kind = 4 ) line(maxobj)
  integer ( kind = 4 ) nbound
  integer ( kind = 4 ) ncontour
  integer ( kind = 4 ) nelem
  logical nflag(maxnp)
  logical nflag0(maxnp)
  logical nflag1(maxnp)
  integer ( kind = 4 ) node(maxnpe,maxelm)
  integer ( kind = 4 ) nonzer
  integer ( kind = 4 ) np
  integer ( kind = 4 ) npe
  integer ( kind = 4 ) npts
  integer ( kind = 4 ) numel(maxnp)
  character ( len = 40 ) object(maxobj)
  character ( len = 40 ) object_name
  logical ovrlay
  real p(maxnp)
  real pwide
  real rho(maxnp)
  real s(maxnp)
  real s_contour(maxcontour)
  real s2(maxnp)
  real scalee
  real scalen
  real scalev
  logical show(maxobj)
  real smax
  real smin
  real srange
  real stmax
  real stmin
  real svmax
  real svmin
  real t(maxnp)
  character ( len = * ) title
  character ( len = * ) title2
  real u(maxnp)
  real uvmag
  real v(maxnp)
  real vecscl
  real vtmax
  real vtmin
  real vvmax
  real vvmin
  real x
  real x1
  real x1max
  real x1min
  real x2
  real x2max
  real x2min
  real xc(maxnp)
  real xsiref(maxnpe)
  real xsmax
  real xsmin
  real xval(4)
  real y
  real y1
  real y1max
  real y1min
  real y2
  real y2max
  real y2min
  real yc(maxnp)
  real ysmax
  real ysmin
  real yval(4)
!
!  Determine EFLAG and NFLAG, which record the visibility of elements
!  and nodes, based on the user's preferences.
!
  call setviz ( eflag, eflagu, maxnpe, nelem, nflag, nflag0, &
    nflag1, node, np, npe, xc, xsmax, xsmin, yc, ysmax, ysmin )
!
!  Prepare for the new plot.
!
  call preplt ( dev, filgrf, icmax, icmin, iplot, itable, ovrlay )
!
!  Set the scale of the picture.
!  We allow ourselves more or less a one percent margin.
!
  xval(1) = x1min
  yval(1) = y1min
  xval(2) = x1max
  yval(2) = y1max

  call setscl ( xval, yval, 2 )
!
!  Draw a box around the region.
!
  if ( show(3) ) then
    call linclr ( icolor(3) )
    call box ( x2min, x2max, y2min, y2max )
  end if
!
!  Draw the background.
!
  if ( show(21) ) then
    call filclr ( icolor(21) )
    xval(1) = x1min
    yval(1) = y1min
    xval(2) = x1max
    yval(2) = y1min
    xval(3) = x1max
    yval(3) = y1max
    xval(4) = x1min
    yval(4) = y1max
    npts = 4
    call plygon ( npts, xval, yval )
  end if
!
!  Draw the titles.
!
  if ( show(7) ) then

    if ( len_trim ( title ) > 0 ) then

      angle = 0.0E+00
      cwide = 0.025E+00 * srange
      x = 0.5E+00 * ( xsmax + xsmin )
      y = ysmax + 3.0E+00 * cwide
      call linclr ( icolor(7) )
      pwide = srange
      flush = 'center'
      call s_plot ( angle, cwide, pwide, trim ( title ), x, y, flush )

    end if

    if ( len_trim ( title2 ) > 0 ) then
      angle = 0.0E+00
      cwide = 0.025E+00 * srange
      x = 0.5E+00 * ( xsmax + xsmin )
      y = ysmax + 1.5E+00 * cwide
      call linclr ( icolor(7) )
      pwide = srange
      flush = 'center'
      call s_plot ( angle, cwide, pwide, trim ( title2 ), x, y, flush )
    end if

  end if
!
!  COLOR CONTOURS ARE DRAWN HERE.
!
!
!  DIVC: draw divergence color contours.
!  Divergence is defined on the kinematic velocities.
!
  if ( show(39) ) then

    call fe_gradient ( dudxn, dudyn, maxnpe, nelem, node, np, npe, numel, u, &
      xc, yc )

    call fe_gradient ( dvdxn, dvdyn, maxnpe, nelem, node, np, npe, numel, v, &
      xc, yc )

    s(1:np) = dudxn(1:np) + dvdyn(1:np)
    nflag(1:np) = nflag0(1:np) .and. nflag1(1:np)

    call fsize ( nflag, np, s, stmax, stmin, svmax, svmin )

    if ( stmax > stmin ) then

      call contour_levels ( c_contour, jcmax, jcmin, maxcontour, &
        object(39), ncontour, s_contour, stmax, stmin, svmax, svmin )

      call contour_color_q6 ( c_contour, eflag, etaref, jcmax, jcmin, &
        maxcontour, maxelm, maxnpe, ncontour, nelem, node, np, npe, s, &
        s_contour, smax, smin, xc, xsiref, yc )

      if ( lbar ) then
        x1 = x2min
        x2 = x2max
        y1 = y2max
        y2 = y1max
        call color_bar ( c_contour, icolor, maxcontour, maxobj, ncontour, &
          s_contour, srange, x1, x2, y1, y2 )
      end if

    else

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GRAPH - Warning!'
      write ( *, '(a)' ) '  Could not display ' // trim ( object(39) )
      write ( *, '(a)' ) '  All data values were equal.'

    end if

  end if
!
!  EC: draw the element colors.
!
  if ( show(20) ) then

    call filclr ( icolor(20) )

    call shoelc ( eflag, etaref, maxnpe, nelem, node, np, npe, xc, &
      xsiref, yc )

  end if
!
!  KVMAGC: draw kinematic velocity magnitude color contours.
!
  if ( show(14) ) then

    s(1:np) = sqrt ( u(1:np)**2 + v(1:np)**2 )
    nflag(1:np) = nflag0(1:np) .and. nflag1(1:np)

    call fsize ( nflag, np, s, stmax, stmin, svmax, svmin )

    if ( stmax > stmin ) then

      call contour_levels ( c_contour, jcmax, jcmin, &
        maxcontour, object(14), ncontour, s_contour, &
        stmax, stmin, svmax, svmin )

      call contour_color_q6 ( c_contour, eflag, etaref, jcmax, &
        jcmin, maxcontour, maxelm, maxnpe, ncontour, nelem, node, &
        np, npe, s, s_contour, smax, smin, xc, xsiref, yc )

      if ( lbar ) then
        x1 = x2min
        x2 = x2max
        y1 = y2max
        y2 = y1max
        call color_bar ( c_contour, icolor, maxcontour, &
          maxobj, ncontour, s_contour, srange, x1, x2, y1, y2 )
      end if

    else

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GRAPH - Warning!'
      write ( *, '(a)' ) '  Could not display ' // trim ( object(14) )
      write ( *, '(a)' ) '  All data values were equal.'

    end if

  end if
!
!  KVXC: draw X kinematic velocity color contours.
!
  if ( show(17) ) then

    nflag(1:np) = nflag0(1:np) .and. nflag1(1:np)
    s(1:np) = u(1:np)

    call fsize ( nflag, np, s, stmax, stmin, svmax, svmin )

    if ( stmax > stmin ) then

      call contour_levels ( c_contour, jcmax, jcmin, &
        maxcontour, object(17), ncontour, s_contour, &
        stmax, stmin, svmax, svmin )

      call contour_color_q6 ( c_contour, eflag, etaref, jcmax, &
        jcmin, maxcontour, maxelm, maxnpe, ncontour, nelem, node, &
        np, npe, s, s_contour, smax, smin, xc, xsiref, yc )

      if ( lbar ) then
        x1 = x2min
        x2 = x2max
        y1 = y2max
        y2 = y1max
        call color_bar ( c_contour, icolor, maxcontour, &
          maxobj, ncontour, s_contour, srange, x1, x2, y1, y2 )
      end if

    else

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GRAPH - Warning!'
      write ( *, '(a)' ) '  Could not display ' // trim ( object(17) )
      write ( *, '(a)' ) '  All data values were equal.'

    end if

  end if
!
!  KVYC: draw Y kinematic velocity color contours.
!
  if ( show(18) ) then

    nflag(1:np) = nflag0(1:np) .and. nflag1(1:np)
    s(1:np) = v(1:np)

    call fsize ( nflag, np, s, stmax, stmin, svmax, svmin )

    if ( stmax > stmin ) then

      call contour_levels ( c_contour, jcmax, jcmin, &
        maxcontour, object(18), ncontour, s_contour, &
        stmax, stmin, svmax, svmin )

      call contour_color_q6 ( c_contour, eflag, etaref, jcmax, &
        jcmin, maxcontour, maxelm, maxnpe, ncontour, nelem, node, &
        np, npe, s, s_contour, smax, smin, xc, xsiref, yc )

      if ( lbar ) then
        x1 = x2min
        x2 = x2max
        y1 = y2max
        y2 = y1max
        call color_bar ( c_contour, icolor, maxcontour, &
          maxobj, ncontour, s_contour, srange, x1, x2, y1, y2 )
      end if

    else

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GRAPH - Warning!'
      write ( *, '(a)' ) '  Could not display ' // trim ( object(18) )
      write ( *, '(a)' ) '  All data values were equal.'

    end if

  end if
!
!  MVMAGC: draw mass velocity magnitude color contours.
!
  if ( show(37) ) then

    s(1:np) = rho(1:np) * sqrt ( u(1:np)**2 + v(1:np)**2 )
    nflag(1:np) = nflag0(1:np) .and. nflag1(1:np)

    call fsize ( nflag, np, s, stmax, stmin, svmax, svmin )

    if ( stmax > stmin ) then

      call contour_levels ( c_contour, jcmax, jcmin, &
        maxcontour, object(37), ncontour, s_contour, &
        stmax, stmin, svmax, svmin )

      call contour_color_q6 ( c_contour, eflag, etaref, jcmax, &
        jcmin, maxcontour, maxelm, maxnpe, ncontour, nelem, node, &
        np, npe, s, s_contour, smax, smin, xc, xsiref, yc )

      if ( lbar ) then
        x1 = x2min
        x2 = x2max
        y1 = y2max
        y2 = y1max
        call color_bar ( c_contour, icolor, maxcontour, &
          maxobj, ncontour, s_contour, srange, x1, x2, y1, y2 )
      end if

    else

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GRAPH - Warning!'
      write ( *, '(a)' ) '  Could not display ' // trim ( object(37) )
      write ( *, '(a)' ) '  All data values were equal.'

    end if

  end if
!
!  PC: draw pressure color contours.
!
  if ( show(12) ) then

    s(1:np) = p(1:np)
    nflag(1:np) = nflag0(1:np) .and. nflag1(1:np)

    call fsize ( nflag, np, s, stmax, stmin, svmax, svmin )

    if ( stmax > stmin ) then

      call contour_levels ( c_contour, jcmax, jcmin, &
        maxcontour, object(12), ncontour, s_contour,  &
        stmax, stmin, svmax, svmin )

      if ( npe == 3 ) then

        call contour_color_l3 ( c_contour, eflag, &
          maxcontour, maxelm, maxnpe, ncontour, nelem, node, np, s, &
          s_contour, xc, yc )

      else if ( npe == 4 ) then

        call contour_color_l4 ( c_contour, eflag, &
          maxcontour, maxelm, maxnpe, ncontour, nelem, node, np, s, &
          s_contour, xc, yc )

      else if ( npe == 6 ) then

        call contour_color_q6 ( c_contour, eflag, etaref, jcmax, &
          jcmin, maxcontour, maxelm, maxnpe, ncontour, nelem, node, &
          np, npe, s, s_contour, smax, smin, xc, xsiref, yc )

      end if

      if ( lbar ) then
        x1 = x2min
        x2 = x2max
        y1 = y2max
        y2 = y1max
        call color_bar ( c_contour, icolor, maxcontour, &
          maxobj, ncontour, s_contour, srange, x1, x2, y1, y2 )
      end if

    else

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GRAPH - Warning!'
      write ( *, '(a)' ) '  Could not display ' // trim ( object(12) )
      write ( *, '(a)' ) '  All data values were equal.'

    end if

  end if
!
!  RHOC: draw Rho color contours.
!
  if ( show(34) ) then

    s(1:np) = rho(1:np)
    nflag(1:np) = nflag0(1:np) .and. nflag1(1:np)

    call fsize ( nflag, np, s, stmax, stmin, svmax, svmin )

    if ( stmax > stmin ) then

      call contour_levels ( c_contour, jcmax, jcmin, &
        maxcontour, object(34), ncontour, s_contour, &
        stmax, stmin, svmax, svmin )

      if ( npe == 3 ) then

        call contour_color_l3 ( c_contour, eflag, &
          maxcontour, maxelm, maxnpe, ncontour, nelem, node, np, s, &
          s_contour, xc, yc )

      else if ( npe == 4 ) then

        call contour_color_l4 ( c_contour, eflag, &
          maxcontour, maxelm, maxnpe, ncontour, nelem, node, np, s, &
          s_contour, xc, yc )

      else if ( npe == 6 ) then

        call contour_color_q6 ( c_contour, eflag, etaref, jcmax, &
          jcmin, maxcontour, maxelm, maxnpe, ncontour, nelem, node, &
          np, npe, s, s_contour, smax, smin, xc, xsiref, yc )

      end if

      if ( lbar ) then
        x1 = x2min
        x2 = x2max
        y1 = y2max
        y2 = y1max
        call color_bar ( c_contour, icolor, maxcontour, &
          maxobj, ncontour, s_contour, srange, x1, x2, y1, y2 )
      end if

    else

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GRAPH - Warning!'
      write ( *, '(a)' ) '  Could not display ' // trim ( object(12) )
      write ( *, '(a)' ) '  All data values were equal.'

    end if

  end if
!
!  SC: draw stream color contours.
!
  if ( show(22) ) then

    call stream ( iwork1, iwork2, maxnpe, nelem, node, np, npe, rho, &
      s, u, v, xc, yc )

    nflag(1:np) = nflag0(1:np) .and. nflag1(1:np)

    call fsize ( nflag, np, s, stmax, stmin, svmax, svmin )

    if ( stmax > stmin ) then

      call contour_levels ( c_contour, jcmax, jcmin, &
        maxcontour, object(22), ncontour, s_contour, &
        stmax, stmin, svmax, svmin )

      call contour_color_q6 ( c_contour, eflag, etaref, jcmax, &
        jcmin, maxcontour, maxelm, maxnpe, ncontour, nelem, node, &
        np, npe, s, s_contour, smax, smin, xc, xsiref, yc )

      if ( lbar ) then
        x1 = x2min
        x2 = x2max
        y1 = y2max
        y2 = y1max
        call color_bar ( c_contour, icolor, maxcontour, maxobj, ncontour, &
          s_contour, srange, x1, x2, y1, y2 )
      end if

    else

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GRAPH - Warning!'
      write ( *, '(a)' ) '  Could not display ' // trim ( object(22) )
      write ( *, '(a,g14.6)' ) '  All data values were equal to ', stmin

    end if

  end if
!
!  VORTC: draw vorticity color contours.
!  Vorticity is defined on the kinematic velocities.
!
  if ( show(13) ) then

    call fe_gradient ( dudxn, dudyn, maxnpe, nelem, node, np, &
      npe, numel, u, xc, yc )

    call fe_gradient ( dvdxn, dvdyn, maxnpe, nelem, node, np, &
      npe, numel, v, xc, yc )

    s(1:np) = dvdxn(1:np) - dudyn(1:np)
    nflag(1:np) = nflag0(1:np) .and. nflag1(1:np)

    call fsize ( nflag, np, s, stmax, stmin, svmax, svmin )

    if ( stmax > stmin ) then

      call contour_levels ( c_contour, jcmax, jcmin, &
        maxcontour, object(13), ncontour, s_contour, &
        stmax, stmin, svmax, svmin )

      call contour_color_q6 ( c_contour, eflag, etaref, jcmax, &
        jcmin, maxcontour, maxelm, maxnpe, ncontour, nelem, node, &
        np, npe, s, s_contour, smax, smin, xc, xsiref, yc )

      if ( lbar ) then
        x1 = x2min
        x2 = x2max
        y1 = y2max
        y2 = y1max
        call color_bar ( c_contour, icolor, maxcontour, &
          maxobj, ncontour, s_contour, srange, x1, x2, y1, y2 )
      end if

    else

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GRAPH - Warning!'
      write ( *, '(a)' ) '  Could not display ' // trim ( object(13) )
      write ( *, '(a)' ) '  All data values were equal.'

    end if

  end if
!
!  XCC: draw X coordinate contours.
!
  if ( show(23) ) then

    nflag(1:np) = nflag0(1:np) .and. nflag1(1:np)
    s(1:np) = xc(1:np)

    call fsize ( nflag, np, s, stmax, stmin, svmax, svmin )

    if ( stmax > stmin ) then

      call contour_levels ( c_contour, jcmax, jcmin, &
        maxcontour, object(23), ncontour, s_contour, &
        stmax, stmin, svmax, svmin )

      call contour_color_q6 ( c_contour, eflag, etaref, jcmax, &
        jcmin, maxcontour, maxelm, maxnpe, ncontour, nelem, node, &
        np, npe, s, s_contour, smax, smin, xc, xsiref, yc )

      if ( lbar ) then
        x1 = x2min
        x2 = x2max
        y1 = y2max
        y2 = y1max
        call color_bar ( c_contour, icolor, maxcontour, &
          maxobj, ncontour, s_contour, srange, x1, x2, y1, y2 )
      end if

    else

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GRAPH - Warning!'
      write ( *, '(a)' ) '  Could not display ' // trim ( object(23) )
      write ( *, '(a)' ) '  All data values were equal.'

    end if

  end if
!
!  YCC: draw Y coordinate contours.
!
  if ( show(24) ) then

    nflag(1:np) = nflag0(1:np) .and. nflag1(1:np)
    s(1:np) = yc(1:np)

    call fsize ( nflag, np, s, stmax, stmin, svmax, svmin )

    if ( stmax > stmin ) then

      call contour_levels ( c_contour, jcmax, jcmin, &
        maxcontour, object(24), ncontour, s_contour, &
        stmax, stmin, svmax, svmin )

      call contour_color_q6 ( c_contour, eflag, etaref, jcmax, &
        jcmin, maxcontour, maxelm, maxnpe, ncontour, nelem, node, &
        np, npe, s, s_contour, smax, smin, xc, xsiref, yc )

      if ( lbar ) then
        x1 = x2min
        x2 = x2max
        y1 = y2max
        y2 = y1max
        call color_bar ( c_contour, icolor, maxcontour, &
          maxobj, ncontour, s_contour, srange, x1, x2, y1, y2 )
      end if

    else

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GRAPH - Warning!'
      write ( *, '(a)' ) '  Could not display ' // trim ( object(24) )
      write ( *, '(a)' ) '  All data values were equal.'

    end if

  end if
!
!  SIMPLE LINES ARE DRAWN HERE.
!
!  Draw the boundary.
!
  if ( show(1) ) then

    if ( line(1) == 0 .or. line(1) == 1 ) then
      iblack = 1
      call linclr ( iblack )
    else
      call linclr ( icolor(1) )
    end if

    nflag(1:np) = nflag1(1:np)

    call bound_show ( eflag, etaref, jbound, line, maxbou, maxnpe, maxobj, &
      nbound, nelem, nflag, node, np, npe, xc, xsiref, yc )

  end if
!
!  N: draw the nodes.
!
  if ( show(4) ) then

    call linclr ( icolor(4) )
    filled = .false.
    csize = 0.005E+00 * srange

    do i = 1, np

      if ( nflag0(i) .and. nflag1(i) ) then
        call circle ( xc(i), yc(i), csize, filled )
      end if

    end do

  end if
!
!  NN: draw the node numbers.
!
  if ( show(27) ) then

    angle = 0.0E+00
    cwide = 0.025*srange*scalen
    pwide = srange
    flush = 'center'
    call linclr ( icolor(27) )

    do i = 1, np

      if ( nflag0(i) .and. nflag1(i) ) then

        x = xc(i)
        y = yc(i)
        write ( chrtmp, '(i6)' ) i
        call flushl ( chrtmp )

        call s_plot ( angle, cwide, pwide, trim ( chrtmp ), x, y, flush )

      end if

    end do

  end if
!
!  E: draw the element boundaries.
!
  if ( show(2) ) then

    call linclr ( icolor(2) )

    call shoelm ( eflag, etaref, maxnpe, nelem, node, np, npe, xc, xsiref, yc )

  end if
!
!  EN: draw the element numbers.
!
  if ( show(28) ) then
    angle = 0.0E+00
    cwide = 0.025*srange*scalee
    pwide = srange
    flush = 'center'
    call linclr ( icolor(28) )
    do i = 1, nelem

      if ( eflag(i) ) then

        x = ( xc(node(1,i)) + xc(node(2,i)) + xc(node(3,i)) ) / 3.0E+00
        y = ( yc(node(1,i)) + yc(node(2,i)) + yc(node(3,i)) ) / 3.0E+00

        write ( chrtmp, '(i6)' ) i
        call flushl ( chrtmp )

        call s_plot ( angle, cwide, pwide, trim ( chrtmp ), x, y, flush )

      end if

    end do

  end if
!
!  CONTOUR LINES ARE DRAWN HERE.
!
  do i = 1, np
    nflag(i) = nflag0(i) .and. nflag1(i)
  end do
!
!  DIV: vorticity contour lines.
!
  if ( show(38) ) then

    call linclr ( icolor(38) )

    call fe_gradient ( dudxn, dudyn, maxnpe, nelem, node, np, npe, &
      numel, u, xc, yc )

    call fe_gradient ( dvdxn, dvdyn, maxnpe, nelem, node, np, npe, &
      numel, v, xc, yc )

    s(1:np) = dudxn(1:np) + dvdyn(1:np)

    jcolor = icolor(38)
    object_name = object(38)

    call contour_line ( c_contour, eflag, etaref, jcmax, &
      jcmin, jcolor, maxcontour, maxelm, maxnp, maxnpe, ncontour, &
      nelem, nflag, node, np, npe, object_name, s, s_contour, xc, xsiref, yc )

  end if
!
!  KVMAG: draw kinematic velocity magnitude line contours.
!
  if ( show(10) ) then

    call linclr ( icolor(10) )

    s(1:np) = sqrt ( u(1:np)**2 + v(1:np)**2 )

    jcolor = icolor(10)
    object_name = object(10)

    call contour_line ( c_contour, eflag, etaref, jcmax, jcmin, &
      jcolor, maxcontour, maxelm, maxnp, maxnpe, ncontour, nelem, nflag, &
      node, np, npe, object_name, s, s_contour, xc, xsiref, yc )

  end if
!
!  KVX: draw X kinematic velocity line contours.
!
  if ( show(15) ) then

    call linclr ( icolor(15) )

    s(1:np) = u(1:np)

    jcolor = icolor(15)
    object_name = object(15)

    call contour_line ( c_contour, eflag, etaref, jcmax, jcmin, jcolor, &
      maxcontour, maxelm, maxnp, maxnpe, ncontour, nelem, nflag, node, np, &
      npe, object_name, s, s_contour, xc, xsiref, yc )

  end if
!
!  KVY: draw Y kinematic velocity line contours.
!
  if ( show(16) ) then

    call linclr ( icolor(16) )

    s(1:np) = v(1:np)

    jcolor = icolor(16)
    object_name = object(16)

    call contour_line ( c_contour, eflag, etaref, jcmax, jcmin, jcolor, &
      maxcontour, maxelm, maxnp, maxnpe, ncontour, nelem, nflag, node, np, &
      npe, object_name, s, s_contour, xc, xsiref, yc )

  end if
!
!  MVMAG: draw mass velocity magnitude line contours.
!
  if ( show(36) ) then

    call linclr ( icolor(36) )

    s(1:np) = rho(1:np) * sqrt ( u(1:np)**2 + v(1:np)**2 )

    jcolor = icolor(36)
    object_name = object(36)

    call contour_line ( c_contour, eflag, etaref, jcmax, jcmin, jcolor, &
      maxcontour, maxelm, maxnp, maxnpe, ncontour, nelem, nflag, node, np, &
      npe, object_name, s, s_contour, xc, xsiref, yc )

  end if
!
!  P: draw pressure contours.
!  There is a slight complication, because pressure on quadratic elements
!  is often actually linear.
!
  if ( show(5) ) then

    call linclr ( icolor(5) )

    s(1:np) = p(1:np)

    jcolor = icolor(5)
    object_name = object(5)

    call contour_line ( c_contour, eflag, etaref, jcmax, jcmin, jcolor, &
      maxcontour, maxelm, maxnp, maxnpe, ncontour, nelem, nflag, node, np, &
      npe, object_name, s, s_contour, xc, xsiref, yc )

  end if
!
!  RHO: draw RHO contours.
!
  if ( show(31) ) then

    call linclr ( icolor(31) )

    s(1:np) = rho(1:np)

    jcolor = icolor(31)
    object_name = object(31)

    call contour_line ( c_contour, eflag, etaref, jcmax, jcmin, jcolor, &
      maxcontour, maxelm, maxnp, maxnpe, ncontour, nelem, nflag, node, np, &
      npe, object_name, s, s_contour, xc, xsiref, yc )

  end if
!
!  S: draw stream lines.
!
  if ( show(6) ) then

    call linclr ( icolor(6) )

    call stream ( iwork1, iwork2, maxnpe, nelem, node, np, npe, rho, s, u, v, &
      xc, yc )

    jcolor = icolor(6)
    object_name = object(6)

    call contour_line ( c_contour, eflag, etaref, jcmax, jcmin, jcolor, &
      maxcontour, maxelm, maxnp, maxnpe, ncontour, nelem, nflag, node, np, &
      npe, object_name, s, s_contour, xc, xsiref, yc )

  end if
!
!  VORT: vorticity contour lines.
!
  if ( show(11) ) then

    call linclr ( icolor(11) )

    call fe_gradient ( dudxn, dudyn, maxnpe, nelem, node, np, npe, &
      numel, u, xc, yc )

    call fe_gradient ( dvdxn, dvdyn, maxnpe, nelem, node, np, npe, &
      numel, v, xc, yc )

    s(1:np) = dvdxn(1:np) - dudyn(1:np)

    jcolor = icolor(11)
    object_name = object(11)

    call contour_line ( c_contour, eflag, etaref, jcmax, jcmin, jcolor, &
      maxcontour, maxelm, maxnp, maxnpe, ncontour, nelem, nflag, node, np, &
      npe, object_name, s, s_contour, xc, xsiref, yc )

  end if
!
!  XC: draw X coordinate contours.
!
  if ( show(25) ) then

    call linclr ( icolor(25) )

    s(1:np) = xc(1:np)

    jcolor = icolor(25)
    object_name = object(25)

    call contour_line ( c_contour, eflag, etaref, jcmax, jcmin, jcolor, &
      maxcontour, maxelm, maxnp, maxnpe, ncontour, nelem, nflag, node, np, &
      npe, object_name, s, s_contour, xc, xsiref, yc )

  end if
!
!  YC: draw Y coordinate contours.
!
  if ( show(26) ) then

    call linclr ( icolor(26) )

    s(1:np) = yc(1:np)

    jcolor = icolor(26)
    object_name = object(26)

    call contour_line ( c_contour, eflag, etaref, jcmax, jcmin, jcolor, &
      maxcontour, maxelm, maxnp, maxnpe, ncontour, nelem, nflag, node, np, &
      npe, object_name, s, s_contour, xc, xsiref, yc )

  end if
!
!  VECTOR PLOTS BEGIN HERE
!

!
!  KV: draw kinematic velocity vectors.
!
  if ( show(8) ) then

    call linclr ( icolor(8) )

    write ( *, * ) ' '
    write ( *, * ) 'GRAPH - Note:'
    write ( *, '(a,i6)' ) '  Kinematic velocity vector color is ', icolor(8)


    nflag(1:np) = nflag0(1:np) .and. nflag1(1:np)

    s(1:np) = u(1:np)
    s2(1:np) = v(1:np)

    call velocity_max ( nflag, np, s, s2, vtmax, vtmin, vvmax, vvmin )

    if ( vtmax /= 0.0E+00 ) then

      write ( *, * ) ' '
      write ( *, * ) 'GRAPH - Note:'
      write ( *, * ) '  All kinematic velocity norms range from ', &
        vtmin, ' to ', vtmax
      write ( *, * ) '  Visible kinematic velocity norms range from ', &
        vvmin, ' to ', vvmax

      vecscl = scalev * 0.5E+00 * min ( delx, dely ) / vtmax

      if ( line(8) == 0 ) then
        ido = 0
      else
        ido = 1
      end if

      if ( s_eqi ( arrow, 'SOLID' ) ) then
        jcolor = icolor(8)
        call filclr ( jcolor )
      else if ( s_eqi ( arrow, 'HOLLOW' ) ) then
        jcolor = 0
        call filclr ( jcolor )
        jcolor = 1
        call linclr ( jcolor )
      end if

      call vec_plot ( arrow, ido, jcmax, jcmin, ncontour, nflag, &
        np, vecscl, s, s2, vtmax, vtmin, xc, yc )

      if ( lbar .and. line(8) /= 0 ) then

        x1 = x2min
        x2 = x2max
        y1 = y2max
        y2 = y1max
        call color_bar ( c_contour, icolor, maxcontour, &
          maxobj, ncontour, s_contour, srange, x1, x2, y1, y2 )
      end if

    else

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GRAPH - Warning!'
      write ( *, '(a)' ) '  Could not display ' // trim ( object(8) )
      write ( *, * ) '  All values were ', vtmax

    end if

  end if
!
!  MV: draw mass velocity vectors.
!
  if ( show(35) ) then

    call linclr ( icolor(35) )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GRAPH - Note:'
    write ( *, '(a,i6)' ) '  The line color was set to ', icolor(35)

    nflag(1:np) = nflag0(1:np) .and. nflag1(1:np)

    s(1:np) = rho(1:np) * u(1:np)
    s2(1:np) = rho(1:np) * v(1:np)

    call velocity_max ( nflag, np, s, s2, vtmax, vtmin, vvmax, vvmin )

    if ( vtmax /= 0.0E+00 ) then

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GRAPH - Note:'
      write ( *, * ) '  All mass velocity norms range from ', vtmin, &
        ' to ', vtmax
      write ( *, * ) '  Visible mass velocity norms range from ', &
        vvmin, ' to ', vvmax

      vecscl = scalev * 0.5E+00 * min ( delx, dely ) / vtmax

      if ( line(35) == 0 ) then
        ido = 0
      else
        ido = 1
      end if

      call vec_plot ( arrow, ido, jcmax, jcmin, ncontour, nflag, &
        np, vecscl, s, s2, vtmax, vtmin, xc, yc )

      if ( lbar .and. line(35) /= 0 ) then

        x1 = x2min
        x2 = x2max
        y1 = y2max
        y2 = y1max

        call color_bar ( c_contour, icolor, maxcontour, &
          maxobj, ncontour, s_contour, srange, x1, x2, y1, y2 )

      end if

    else

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GRAPH - Warning!'
      write ( *, '(a)' ) '  Could not display ' // trim ( object(35) )
      write ( *, * ) '  All values were ', vtmax

    end if

  end if
!
!  UV: draw velocity direction vectors.
!
  if ( show(9) ) then

    call linclr ( icolor(9) )

    nonzer = 0

    nflag(1:np) = nflag0(1:np) .and. nflag1(1:np)

    do i = 1, np

      uvmag = sqrt ( u(i)**2 + v(i)**2 )

      if ( uvmag /= 0.0E+00 ) then
        nonzer = nonzer + 1
        s(i) = u(i) / uvmag
        t(i) = v(i) / uvmag
      else
        s(i) = 0.0E+00
        t(i) = 0.0E+00
      end if

    end do

    call velocity_max ( nflag, np, u, v, vtmax, vtmin, vvmax, vvmin )

    if ( nonzer > 0 ) then

      vecscl = scalev * 0.5E+00 * min ( delx, dely )

      if ( line(9) == 0 ) then
        ido = 0
      else
        ido = 1
      end if

      call vec_plot ( arrow, ido, jcmax, jcmin, ncontour, nflag, &
        np, vecscl, s, t, vtmax, vtmin, xc, yc )

      if ( lbar .and. line(9) /= 0 ) then

        x1 = x2min
        x2 = x2max
        y1 = y2max
        y2 = y1max

        call color_bar ( c_contour, icolor, maxcontour, &
          maxobj, ncontour, s_contour, srange, x1, x2, y1, y2 )

      end if

    else

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GRAPH - Warning!'
      write ( *, '(a)' ) '  Could not display ' // trim ( object(9) )
      write ( *, * ) '  All data values were zero.'

    end if

  end if
!
!  Pause, if we are doing X-Windows.
!
  call buzz ( dev, x1min, x1max, y1min, y1max )

  return
end
subroutine hello ( maxbou, maxcontour, maxelm, maxnp, maxnpe, maxnx, maxny, &
  maxobj )
!
!*******************************************************************************
!
!! HELLO prints out the program name, date, and purpose.
!
!
!  Modified:
!
!    15 October 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) MAXBOU, the amount of storage available for the
!    IBOUND array.
!
!    Input, integer ( kind = 4 ) MAXCONTOUR, the maximum number of contour levels.
!
!    Input, integer ( kind = 4 ) MAXELM, the maximum number of elements.
!
!    Input, integer ( kind = 4 ) MAXNP, the maximum number of nodes.
!
!      MAXNP = (2 * MAXNX - 1) * (2 * MAXNY - 1)
!
!    Input, integer ( kind = 4 ) MAXNPE, the maximum number of nodes per element.
!
!    Input, integer ( kind = 4 ) MAXOBJ, the number of graphical "objects".
!
  implicit none
!
  integer ( kind = 4 ) maxbou
  integer ( kind = 4 ) maxcontour
  integer ( kind = 4 ) maxelm
  integer ( kind = 4 ) maxnp
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) maxnx
  integer ( kind = 4 ) maxny
  integer ( kind = 4 ) maxobj
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'HELLO!'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Last modified on 13 February 2002.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  DISPLAY5 produces X Window, PostScript or CGM'
  write ( *, '(a)' ) '  graphics output from the data of the 2D finite '
  write ( *, '(a)' ) '  element program FLOW.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Scalars are displayed by contour lines or colors.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Vectors are displayed as scaled or unit vectors.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Line or color contours of stream function,'
  write ( *, '(a)' ) '  magnitude, X and Y components, and so on, '
  write ( *, '(a)' ) '  are also available.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Built-in maxima:'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i6)' ) '    Boundary edges     MAXBOU =     ', maxbou
  write ( *, '(a,i6)' ) '    Contour levels     MAXCONTOUR = ', maxcontour
  write ( *, '(a,i6)' ) '    Nodes per element  MAXNPE =     ', maxnpe
  write ( *, '(a,i6)' ) '    X elements         MAXNX =      ', maxnx
  write ( *, '(a,i6)' ) '    Y elements         MAXNY =      ', maxny
  write ( *, '(a,i6)' ) '    Objects            MAXOBJ =     ', maxobj
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Derived maxima:'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i6)' ) '    Elements           MAXELM =     ', maxelm
  write ( *, '(a,i6)' ) '    Nodes              MAXNP =      ', maxnp
  write ( *, '(a)' ) ' '

  return
end
subroutine help
!
!*******************************************************************************
!
!! HELP prints out a list of commands.
!
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
!    None
!
  implicit none
!
  integer ( kind = 4 ) ios
  character isay
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'Commands:'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TH MH BH  top, middle, bottom halves.'
  write ( *, '(a)' ) 'LH CH RH  left, center, right halves.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TL TC TR  top left, center, right quarters.'
  write ( *, '(a)' ) 'ML MC MR  middle left, center, right quarters.'
  write ( *, '(a)' ) 'BL BC BR  bottom left, center, right quarters.'
  write ( *, '(a)' ) 'X 	choose arbitrary X, Y subwindow.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'FULL      return to full picture.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'VE	set visible elements.'
  write ( *, '(a)' ) 'VN	set visible nodes by index.'
  write ( *, '(a)' ) 'VND	set visible nodes by distance.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'RETURN for more..., Q to quit'
  read ( *, '(a1)', iostat = ios ) isay

  if ( ios /= 0 ) then
    return
  end if

  if ( isay /= ' ' ) then
    return
  end if

  write ( *, '(a)' ) 'B             show boundary.'
  write ( *, '(a)' ) 'BACK          show plot background.'
  write ( *, '(a)' ) 'BAR           show color bar.'
  write ( *, '(a)' ) 'E,    EC      show elements or element colors.'
  write ( *, '(a)' ) 'EN            show element numbers.'
  write ( *, '(a)' ) 'FRAME         switch the frame display option.'
  write ( *, '(a)' ) 'KV            kinematic velocity vectors.'
  write ( *, '(a)' ) &
    'KVMAG, KVMAGC line/color kinematic velocity magnitude contours.'
  write ( *, '(a)' ) 'KVX,    KVXC  line/color horizontal velocity contours.'
  write ( *, '(a)' ) 'KVY,    KVYC  line/color vertical velocity contours.'
  write ( *, '(a)' ) 'MV            mass velocity vectors.'
  write ( *, '(a)' ) 'MVMAG, MVMAGC line/color mass velocity magnitude contours.'
  write ( *, '(a)' ) 'N             show nodes.'
  write ( *, '(a)' ) 'NN            show node numbers.'
  write ( *, '(a)' ) 'P,     PC     line/color pressure contours.'
  write ( *, '(a)') 'S,     SC     stream line/color contours.'
  write ( *, '(a)' ) 'UV            unit velocity vectors.'

  write ( *, '(a)' ) 'DIV,   DIVC   line/color divergence contours.'
  write ( *, '(a)' ) 'VORT,  VORTC  line/color vorticity contours.'
  write ( *, '(a)' ) 'XC,    XCC    line/color X coordinate.'
  write ( *, * ) 'YC,    YCC    line/color Y coordinate.'
  write ( *, * ) ' '
  write ( *, * ) 'RETURN for more..., Q to quit'

  read ( *, '(a1)', iostat = ios ) isay

  if ( ios /= 0 ) then
    return
  end if

  if ( isay /= ' ' ) then
    return
  end if

  write ( *, * ) 'ARROW =    HOLLOW, LINE or SOLID.'
  write ( *, * ) 'CC =       Choose a color table.'
  write ( *, * ) 'GRACE =    Set grace margin.'
  write ( *, * ) 'ICMAX =    Set maximum available color.'
  write ( *, * ) 'ICMIN =    Set minimum available color.'
  write ( *, * ) 'JCMAX =    Set maximum used color.'
  write ( *, * ) 'JCMIN =    Set minimum used color.'
  write ( *, * ) 'LINE       Set line type of any object.'
  write ( *, * ) 'NCONTOUR = Set number of contour levels.'
  write ( *, * ) 'SCALEE =   Set a scale factor for element numbers.'
  write ( *, * ) 'SCALEN =   Set a scale factor for node numbers.'
  write ( *, * ) 'SCALEV =   Set velocity scale factor.'
  write ( *, * ) 'TITLE =    Set plot title.'
  write ( *, * ) 'TITLE2 =   Set plot subtitle.'

  write ( *, * ) ' '
  write ( *, * ) 'RETURN for more..., Q to quit'

  read ( *, '(a1)', iostat = ios ) isay

  if ( ios /= 0 ) then
    return
  end if

  if ( isay /= ' ' ) then
    return
  end if

  write ( *, * ) ' '

  write ( *, * ) 'C        choose colors.'
  write ( *, * ) 'COLOR    choose one color index.'
  write ( *, * ) 'CTAB     show current color table.'
  write ( *, * ) 'Help     help (print this list)'
  write ( *, * ) 'HELLO    print program data, version, maxima.'
  write ( *, * ) 'INIT     initialize all data to zero.'
  write ( *, * ) 'LIST     list current values.'
  write ( *, * ) 'OVERLAY  overlay next plots.'
  write ( *, * ) ' '
  write ( *, * ) '#        Begin a comment line.'
  write ( *, * ) 'G        go!  create current graph.'
  write ( *, * ) 'Q        quit.'
  write ( *, * ) 'QY       quit NOW.'
  write ( *, * ) ' '

  write ( *, * ) 'RETURN for more..., Q to quit'

  read ( *, '(a1)', iostat = ios ) isay

  if ( ios /= 0 ) then
    return
  end if

  if ( isay /= ' ') then
    return
  end if

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'Files:'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'READ TECPLOT filename'
  write ( *, '(a)' ) '  read the specified TECPLOT file.'
  write ( *, '(a)' ) 'READ ELEMENT filename'
  write ( *, '(a)' ) '  read the specified element file.'
  write ( *, '(a)' ) 'READ NODE filename'
  write ( *, '(a)' ) '  read the specified node file.'
  write ( *, * ) 'READ TECPLOT filename'
  write ( *, * ) '  read the specified TECPLOT file.'
  write ( *, * ) ' '
  write ( *, * ) 'WRITE TECPLOT filename'
  write ( *, * ) '  write the specified TECPLOT file.'
  write ( *, * ) 'WRITE ELEMENT filename'
  write ( *, * ) '  write the specified element file.'
  write ( *, * ) 'WRITE NODE filename'
  write ( *, * ) '  write the specified node file.'
  write ( *, * ) ' '
  write ( *, * ) 'DEV =      Choose plotting output.'
  write ( *, * ) 'FILE =     Name the graphics output file.'
  write ( *, * ) ' '
  write ( *, * ) 'Miscellaneous:'
  write ( *, * ) ' '
  write ( *, * ) 'EXAMPLE    Set up the example problem.'
  write ( *, * ) 'IWRITE =   Set debugging output level.'
  write ( *, * ) ' '

  return
end
subroutine i_next ( s, ival, done )
!
!*******************************************************************************
!
!! I_NEXT "reads" integers from a string, one at a time.
!
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
!    Input, character ( len = * ) S, a string, presumably containing
!    integer ( kind = 4 )s.  These may be separated by spaces or commas.
!
!    Output, integer ( kind = 4 ) IVAL.  If DONE is FALSE, then IVAL contains the
!    "next" integer read.  If DONE is TRUE, then IVAL is zero.
!
!    Input/output, logical DONE.
!    On input with a fresh string, the user should set DONE to TRUE.
!    On output, the routine sets DONE to FALSE if another integer
!    was read, or TRUE if no more integers could be read.
!
  implicit none
!
  logical done
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) ival
  integer ( kind = 4 ) lchar
  integer ( kind = 4 ), save :: next = 1
  character ( len = * ) s
!
  ival = 0

  if ( done ) then
    next = 1
    done = .false.
  end if

  if ( next > len ( s ) ) then
    done = .true.
    return
  end if

  call s_to_i ( s(next:), ival, ierror, lchar )

  if ( ierror /= 0 .or. lchar == 0 ) then
    done = .true.
    next = 1
  else
    done = .false.
    next = next + lchar
  end if

  return
end
subroutine i_random ( ilo, ihi, i )
!
!*******************************************************************************
!
!! I_RANDOM returns a random integer in a given range.
!
!
!  Modified:
!
!    23 September 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) ILO, IHI, the minimum and maximum acceptable values.
!
!    Output, integer ( kind = 4 ) I, the randomly chosen integer.
!
  implicit none
!
  logical, save :: seed = .false.
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  real r
  real rhi
  real rlo
!
  if ( .not. seed ) then
    call random_seed
    seed = .true.
  end if
!
!  Pick a random number in (0,1).
!
  call random_number ( harvest = r )
!
!  Set a real interval [RLO,RHI] which contains the integers [ILO,IHI],
!  each with a "neighborhood" of width 1.
!
  rlo = real ( ilo ) - 0.5E+00
  rhi = real ( ihi ) + 0.5E+00
!
!  Set I to the integer that is nearest the scaled value of R.
!
  i = nint ( ( 1.0E+00 - r ) * rlo + r * rhi )
!
!  In case of oddball events at the boundary, enforce the limits.
!
  i = max ( i, ilo )
  i = min ( i, ihi )

  return
end
subroutine i_swap ( i, j )
!
!*******************************************************************************
!
!! I_SWAP swaps two integer values.
!
!
!  Modified:
!
!    30 November 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) I, J.  On output, the values of I and
!    J have been interchanged.
!
  implicit none
!
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
!
  k = i
  i = j
  j = k

  return
end
subroutine init ( arrow, c_contour, delx, dely, dev, &
  eflag, eflagu, etaref, filgrf, &
  grace, icmax, icmin, icolor, idata, iplot, itable, &
  iwrite, jcmax, jcmin, labelx, labely, lbar, line, maxcontour, maxelm, &
  maxnp, maxnpe, maxobj, nbound, ncontour, nelem, nflag, nflag0, nflag1, &
  node, np, npe, object, ovrlay, s_contour, scalee, &
  scalen, scalev, &
  show, smax, smin, title, title2, x1max, x1min, x2max, x2min, x4max, &
  x4min, xsiref, xsmax, xsmin, y1max, y1min, y2max, y2min, y4max, &
  y4min, ysmax, ysmin )
!
!*******************************************************************************
!
!! INIT initializes the values of data.
!
!
!  Modified:
!
!    16 July 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, character ( len = 10 ) DEV, the graphics output device:
!      cgmb - CGM binary file.
!      ps   - PostScript file.
!      XWS  - X window screen (interactive).
!
!    Output, logical EFLAG(MAXELM), element visibility flags.
!
!    Output, logical EFLAGU(NELEM).
!    EFLAGU is used to "flag" which elements the user wants to see.
!    If EFLAGU(I) is TRUE, then element I should be displayed.
!
!    Output, real ETAREF(MAXNPE), the ETA coordinates of the reference nodes.
!
!    Output, character ( len = 80 ) FILGRF, the name of the output 
!    graphics file.
!
!    Output, real GRACE, the size of the "grace" margin.
!
!    Output, integer ( kind = 4 ) ICMAX, ICMIN, the maximum and
!    minimum color indices to use in the color bar.
!
!    Output, integer ( kind = 4 ) ICOLOR(MAXOBJ), the color index for each object.
!
!    Output, integer ( kind = 4 ) IDATA.
!    0, no problem has been defined.
!    nonzero, a problem has been defined.
!
!    Output, integer ( kind = 4 ) IPLOT, the number of plots made so far.
!
!    Output, integer ( kind = 4 ) ITABLE, the desired color table.
!
!    1: low black to high white
!    2: low blue to high yellow
!    3: low red, high blue, with bands between.
!    4: low red, yellow, green, blue, high white.
!    5: low white, blue, green, yellow, high red.
!    6: low blue to high red
!    7: linear table between 2 user colors.
!    8: linear table between N user colors.
!    9: low white to high black.
!
!    Output, integer ( kind = 4 ) IWRITE.
!    Controls debugging output.
!
!    0 means no such output.
!    1 means some.
!    2 means a lot.
!
!    Output, integer ( kind = 4 ) JCMAX, JCMIN, the maximum and minimum color
!    indices to use for contours.
!
!    Output, character ( len = 30 ) LABELX, LABELY, the labels for the axes.
!
!    Output, logical LBAR, .TRUE. if the color bar may be shown,
!    .FALSE. if it should not be shown.
!
!    Output, integer ( kind = 4 ) LINE(MAXOBJ).
!    LINE allows the user to specify or change the line type
!    for each object.  Only some objects actually have a line
!    type, and the choices for a line type depend on the object.
!
!    Here is the list:
!
!      0, solid black lines.
!      1, dashed black lines.
!      2, solid lines of current color.
!      3, dashed lines of current color.
!
!    Input, integer ( kind = 4 ) MAXELM, the maximum number of elements.
!
!    Input, integer ( kind = 4 ) MAXNP.
!    The maximum number of nodes which the program can handle.
!
!    Input, integer ( kind = 4 ) MAXNPE, the maximum number of nodes per element.
!
!    Input, integer ( kind = 4 ) MAXOBJ, the number of graphical "objects".
!
!    Output, integer ( kind = 4 ) NBOUND, the number of points defining the boundary.
!
!    Output, integer ( kind = 4 ) NCONTOUR.
!    The number of contour lines to be drawn.  This is
!    initialized to 12, but may be changed by the user.
!
!    Output, integer ( kind = 4 ) NELEM, the number of elements.
!
!    Output, logical NFLAG(MAXNP), flags nodes which are active.
!
!    Output, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), the global node numbers in 
!    each element.
!
!    Output, integer ( kind = 4 ) NP, the number of nodes.
!
!    Output, integer ( kind = 4 ) NPE, the number of nodes per element.
!
!    Output, character ( len = 30 ) OBJECT(MAXOBJ), the names of the
!    graphical objects.
!
!    Output, logical OVRLAY.
!    If OVRLAY is true, then the next time that a plot is
!    requested, a "new frame" command is suppressed, so that
!    the new plot is shown on top of the previous one.
!
!    Output, real SCALEE.
!    The scale factor for the element numbers, with default value 1.
!
!    Output, real SCALEN.
!    The scale factor for the node numbers, with default value 1.
!
!    Output, real SCALEV.
!    A scale factor for velocity vectors.  This starts out at 1.0.
!
!    Output, logical SHOW(MAXOBJ).
!    Contains, for each object, a flag determining whether it
!    is to be shown or not.
!
!    Output, character ( len = 40 ) TITLE.
!    A title for the plots.
!
!    Output, character ( len = 40 ) TITLE2.
!    A subtitle used in the profile plots.
!
!    Output, real X1MAX, X1MIN, the maximum and minimum X
!    coordinates of the plot, which includes a small grace margin.
!
!    Output, real X2MAX, X2MIN, the maximum and minimum X
!    coordinates that should be used for plotting.  No plotting
!    commands should  exceed these values.  This is where the
!    "frame" might be drawn.
!
!    Output, real X4MAX, X4MIN, the maximum and minimum X
!    coordinates that  are used for the plot, not including axes.
!
!    Output, real XSIREF(MAXNPE), the XSI coordinates of the reference nodes.
!
!    Output, real XSMAX.
!    The maximum X coordinate of the data to be displayed.
!    XSMAX defaults to XMAX, but can be made smaller to
!    focus on a portion of the region.
!
!    Output, real XSMIN.
!    The minimum X coordinate of the data to be displayed.
!    XSMIN defaults to XMIN, but can be made larger to
!    focus on a portion of the region.
!
!    Output, real Y1MAX, Y1MIN, the maximum and minimum Y
!    coordinates of the plot, which includes a small grace margin.
!
!    Output, real Y2MAX, Y2MIN, the maximum and minimum Y
!    coordinates that should be used for plotting.  No plotting
!    commands should  exceed these values.  This is where the
!    "frame" might be drawn.
!
!    Output, real Y4MAX, Y4MIN, the maximum and minimum Y
!    coordinates that  are used for the plot, not including axes.
!
!    Output, real YSMAX.
!    The maximum Y coordinate of the data to be displayed.
!    YSMAX defaults to YMAX, but can be made smaller to
!    focus on a portion of the region.
!
!    Output, real YSMIN, the minimum displayed Y coordinate.
!
  implicit none
!
  integer ( kind = 4 ) maxcontour
  integer ( kind = 4 ) maxelm
  integer ( kind = 4 ) maxnp
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) maxobj
!
  character ( len = 10 ) arrow
  integer ( kind = 4 ) c_contour(maxcontour)
  real delx
  real dely
  character ( len = 10 ) dev
  logical eflag(maxelm)
  logical eflagu(maxelm)
  real etaref(maxnpe)
  character ( len = 80 ) filgrf
  real grace
  integer ( kind = 4 ) i
  integer ( kind = 4 ) icmax
  integer ( kind = 4 ) icmin
  integer ( kind = 4 ) icolor(maxobj)
  integer ( kind = 4 ) idata
  integer ( kind = 4 ) iplot
  integer ( kind = 4 ) itable
  integer ( kind = 4 ) iwrite
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jcmax
  integer ( kind = 4 ) jcmin
  character ( len = * ) labelx
  character ( len = * ) labely
  logical lbar
  integer ( kind = 4 ) line(maxobj)
  integer ( kind = 4 ) nbound
  integer ( kind = 4 ) ncontour
  integer ( kind = 4 ) nelem
  logical nflag(maxnp)
  logical nflag0(maxnp)
  logical nflag1(maxnp)
  integer ( kind = 4 ) node(maxnpe,maxelm)
  integer ( kind = 4 ) np
  integer ( kind = 4 ) npe
  character ( len = 40 ) object(maxobj)
  logical ovrlay
  real s_contour(maxcontour)
  real scalee
  real scalen
  real scalev
  logical show(maxobj)
  real smax
  real smin
  character ( len = 40 ) title
  character ( len = 40 ) title2
  real x1max
  real x1min
  real x2max
  real x2min
  real x4max
  real x4min
  real xsiref(maxnpe)
  real xsmax
  real xsmin
  real y1max
  real y1min
  real y2max
  real y2min
  real y4max
  real y4min
  real ysmax
  real ysmin
!
  arrow = 'line'
  c_contour(1:maxcontour) = 0
  delx = 1.0E+00
  dely = 1.0E+00
  dev = ' '
  eflag(1:maxelm) = .true.
  eflagu(1:maxelm) = .true.
  filgrf = ' '
  grace = 0.05E+00
  icmax = 255
  icmin = 2
  icolor(1:maxobj) = 1
  icolor(8) = 0
  icolor(20) = 127
  icolor(21) = 0

  idata = 0
  iplot = 0
  itable = 2
  iwrite = 0
!
!  For gray scale color tables, avoid extremes of all white or all black.
!
  jcmax = 255
  jcmin = 2
  labelx = ' '
  labely = ' '
  lbar = .true.
!
!  Set default line type to solid lines of current color.
!  Boundary is dashed, current color.
!
  line(1:maxobj) = 2
  line(1) = 2

  nbound = 0
  ncontour = 9
  nelem = 0
  nflag(1:maxnp) = .true.
  nflag0(1:maxnp) = .true.
  nflag1(1:maxnp) = .true.
  node(1:maxnpe,1:maxelm) = 0
  np = 0
  npe = 6

  object(1) = 'boundary'
  object(2) = 'element'
  object(3) = 'frame'
  object(4) = 'nodes'
  object(5) = 'pressure'
  object(6) = 'stream lines'
  object(7) = 'title'
  object(8) = 'kinematic velocity vectors'
  object(9) = 'unit velocity vectors'
  object(10) = 'kinematic velocity magnitude contours'
  object(11) = 'vorticity contours'
  object(12) = 'pressure colors'
  object(13) = 'vorticity colors'
  object(14) = 'kinematic velocity magnitude colors'
  object(15) = 'X kinematic velocity contours'
  object(16) = 'Y kinematic velocity contours'
  object(17) = 'X kinematic velocity colors'
  object(18) = 'Y kinematic velocity colors'
  object(19) = ' '
  object(20) = 'element colors'
  object(21) = 'background'
  object(22) = 'stream colors'
  object(23) = 'X coordinate colors'
  object(24) = 'Y coordinate colors'
  object(25) = 'X coordinate contours'
  object(26) = 'Y coordinate contours'
  object(27) = 'node numbers'
  object(28) = 'element numbers'
  object(29) = 'Cp contours'
  object(30) = 'Mach contours'
  object(31) = 'Rho contours'
  object(32) = 'Cp colors'
  object(33) = 'Mach colors'
  object(34) = 'Rho colors'
  object(35) = 'Mass velocity vectors'
  object(36) = 'Mass velocity magnitude contours'
  object(37) = 'Mass velocity magnitude colors'
  object(38) = 'divergence contours'
  object(39) = 'divergence colors'
  ovrlay = .false.
  s_contour(1:maxcontour) = 0.0E+00
  scalee = 1.0E+00
  scalen = 1.0E+00
  scalev = 1.0E+00
  show(1:maxobj) = .false.
  show(1) = .true.
  show(3) = .false.
  show(7) = .true.

  smax = 0.0E+00
  smin = 0.0E+00

  title = ' '
  title2 = ' '
  x2max = 1.0E+00
  x2min = 0.0E+00

  x4max = 0.95E+00
  x4min = 0.05E+00
  xsmax = 1.0E+00
  xsmin = 0.0E+00
  y2max = 1.0E+00
  y2min = 0.0E+00
  y4max = 0.95E+00
  y4min = 0.05E+00
  ysmax = 1.0E+00
  ysmin = 0.0E+00
!
!  Set things that depend on other things.
!
  call refnode ( npe, xsiref, etaref )

  x1max = x2max + grace * ( x2max - x2min )
  x1min = x2min - grace * ( x2max - x2min )
  y1max = y2max + grace * ( y2max - y2min )
  y1min = y2min - grace * ( y2max - y2min )

  return
end
subroutine intnexrd ( string, intval, ierror )
!
!*******************************************************************************
!
!! INTNEXRD finds and reads the next integer in a string.
!
!
!  Discussion:
!
!    INTNEXRD can be used to extract, one at a time, the integers in
!    a string.
!
!  Modified:
!
!    26 September 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) STRING, the string to be examined.
!
!    Output, integer ( kind = 4 ) INTVAL, the next integer in the string, or 0
!    if no integer could be found.
!
!    Input/output, integer ( kind = 4 ) IERROR.
!
!    On the first call for a given string, set IERROR = -1.
!
!    Thereafter, the routine will return IERROR = 0 if another
!    integer ( kind = 4 ) was found, or 1 if no more integers were found.
!
  implicit none
!
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) intval
  integer ( kind = 4 ), save :: istart = 0
  integer ( kind = 4 ) lchar
  character ( len = * ) string
!
  if ( ierror == -1 ) then
    istart = 0
  end if

  ierror = 0
  intval = 0

  if ( istart >= len ( string ) ) then
    ierror = 1
    return
  end if

  call chrcti2 ( string(istart:), intval, ierror, lchar )

  if ( ierror == 0 ) then
    istart = istart + lchar
  else
    ierror = 1
  end if

  return
end
subroutine iso_line_l3 ( xa, ya, xb, yb )
!
!*******************************************************************************
!
!! ISO_LINE_L3 draws a line between two points in a linear isoparametric element.
!
!
!  Discussion:
!
!    The line is straight in the original reference element, and will be
!    straight in the image element.
!
!    The routine is given the locations of the points (XA,YA) and
!    (XB,YB), which are presumed to lie inside the same element.
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
!    Input, real XA, YA, the first point to be connected.
!
!    Input, real XB, YB, the second point to be connected.
!
  implicit none
!
  integer ( kind = 4 ), parameter :: narray = 2
!
  real xa
  real xb
  real xarray(narray)
  real ya
  real yb
  real yarray(narray)
!
  xarray(1) = xa
  yarray(1) = ya

  xarray(2) = xb
  yarray(2) = yb
!
!  Draw the image points.
!
  call plylin ( narray, xarray, yarray )

  return
end
subroutine iso_line_q6 ( xa, ya, xb, yb, ielem, maxnpe, nelem, node, np, npe, &
  xc, yc )
!
!*******************************************************************************
!
!! ISO_LINE_Q6 draws a line between two points in a quadratic isoparametric element.
!
!
!  Discussion:
!
!    The line is straight in the original, reference, element, but will
!    probably be curved in the image element.
!
!    The locations of the points (XA,YA) and (XB,YB) are given, which are
!    presumed to lie inside of element IELEM.
!
!    An interpolated array of NARRAY points is created,
!    (XDREF(I), YDREF(I)) stretching from (XA,YA) to (XB,YB).
!
!    Then the image locations of each point are computed,
!    and the line connecting these points is drawn.
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
!    Input, real XA, YA, the first point to be connected.
!
!    Input, real XB, YB, the second point to be connected.
!
!    Input, integer ( kind = 4 ) IELEM, the element in which we are working.
!
!    Input, integer ( kind = 4 ) MAXNPE, the maximum number of nodes per element.
!
!    Input, integer ( kind = 4 ) NELEM, the number of elements.
!
!    Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), the global node numbers in 
!    each element.
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!    Input, integer ( kind = 4 ) NPE, the number of nodes per element.
!
!    Input, real XC(NP), the X coordinates of the nodes.
!
!    Input, real YC(NP), the Y coordinates of the nodes.
!
  implicit none
!
  integer ( kind = 4 ), parameter :: narray = 5
!
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) np
!
  real a1
  real a2
  real b1
  real b2
  real c1
  real c2
  real d1
  real d2
  real e1
  real e2
  real f1
  real f2
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ielem
  integer ( kind = 4 ) node(maxnpe,nelem)
  integer ( kind = 4 ) npe
  real xa
  real xb
  real xc(np)
  real xdimage(narray)
  real xdref(narray)
  real xn(6)
  real ya
  real yb
  real yc(np)
  real ydimage(narray)
  real ydref(narray)
  real yn(6)
!
!  Locate the images of the nodes of the reference triangle.
!
  do i = 1, npe
    xn(i) = xc(node(i,ielem))
    yn(i) = yc(node(i,ielem))
  end do
!
!  Get the coefficients of the map.
!
  call refmap_q6 ( xn, a1, b1, c1, d1, e1, f1 )
  call refmap_q6 ( yn, a2, b2, c2, d2, e2, f2 )
!
!  Interpolate between the X and Y data reference values.
!
  call fillin ( narray, xa, xb, xdref )
  call fillin ( narray, ya, yb, ydref )
!
!  Map each point on the reference line to the image line.
!
  do i = 1, narray

    xdimage(i) = a1 * xdref(i)**2 + b1 * xdref(i) * ydref(i) &
      + c1 * ydref(i)**2 + d1 * xdref(i) + e1 * ydref(i) + f1

    ydimage(i) = a2 * xdref(i)**2 + b2 * xdref(i) * ydref(i) &
      + c2 * ydref(i)**2 + d2 * xdref(i) + e2 * ydref(i) + f2

  end do
!
!  Draw the image points.
!
  call plylin ( narray, xdimage, ydimage )

  return
end
subroutine iso_poly_q6 ( etagon, ielem, maxnpe, nelem, node, np, npe, npts, &
  xc, xsigon, yc )
!
!*******************************************************************************
!
!! ISO_POLY_Q6 draws a filled contour polygon in a quadratic triangle.
!
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
!    Input, real ETAGON(NPTS), the ETA coordinates of the vertices.
!
!    Input, integer ( kind = 4 ) IELEM, the element in which we are working.
!
!    Input, integer ( kind = 4 ) MAXNPE, the maximum number of nodes per element.
!
!    Input, integer ( kind = 4 ) NELEM, the number of elements.
!
!    Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), the global node numbers in
!    each element.
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!    Input, integer ( kind = 4 ) NPE, the number of nodes per element.
!
!    Input, integer ( kind = 4 ) NPTS, the number of vertices in the polygon.
!
!    Input, real XC(NP), the X coordinates of the nodes.
!
!    Input, real XSIGON(NPTS), the XSI coordinates of the vertices.
!
!    Input, real YC(NP), the Y coordinates of the nodes.
!
  implicit none
!
  integer ( kind = 4 ), parameter :: maxpts2 = 100
!
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) np
  integer ( kind = 4 ) npts
!
  real a1
  real a2
  real b1
  real b2
  real c1
  real c2
  real d1
  real d2
  real e1
  real e2
  real eta
  real etagon(npts)
  real etagon2(maxpts2)
  real f1
  real f2
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ielem
  integer ( kind = 4 ) nfill
  integer ( kind = 4 ) node(maxnpe,nelem)
  integer ( kind = 4 ) npe
  integer ( kind = 4 ) npts2
  real xc(np)
  real xgon(maxpts2)
  real xn(6)
  real x
  real xsi
  real xsigon(npts)
  real xsigon2(maxpts2)
  real yc(np)
  real ygon(maxpts2)
  real yn(6)
  real y
!
!  Get XN(*), YN(*), the images of the nodes of the reference triangle.
!
  do i = 1, npe
    xn(i) = xc(node(i,ielem))
    yn(i) = yc(node(i,ielem))
  end do
!
!  Get the coefficients of the map from the reference triangle
!  to the image triangle.
!
  call refmap_q6 ( xn, a1, b1, c1, d1, e1, f1 )
  call refmap_q6 ( yn, a2, b2, c2, d2, e2, f2 )
!
!  Insert two extra points between every pair of original ones.
!
!     nfill = 2
  nfill = 0
  npts2 = ( nfill + 1 ) * npts

  if ( npts2 > maxpts2 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'ISO_POLY_Q6 - Fatal error!'
    write ( *, '(a)' ) '  The input polygon has too many points!'
    stop
  end if
!
!  Insert 2 fillin points between each pair of points.
!
  call fillin2 ( nfill, npts, xsigon, xsigon2 )
  call fillin2 ( nfill, npts, etagon, etagon2 )
!
!  Compute the images of the points.
!
  do i = 1, npts2

    xsi = xsigon2(i)
    eta = etagon2(i)

    x = a1 * xsi**2 + b1 * xsi * eta + c1 * eta**2 + d1 * xsi + e1 * eta + f1

    y = a2 * xsi**2 + b2 * xsi * eta + c2 * eta**2 + d2 * xsi + e2 * eta + f2

    xgon(i) = x
    ygon(i) = y

  end do


  call plygon ( npts2, xgon, ygon )

  return
end
subroutine line_seg_contains_point_1d ( x1, xmid, x2, inside )
!
!*******************************************************************************
!
!! LINE_SEG_CONTAINS_POINT_1D reports if a line segment contains a point in 1D.
!
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
!    Input, real X1, XMID, X2, three values to be tested.
!
!    Output, logical INSIDE, will be TRUE if XMID is "inside" the interval
!    spanned by X1 and X2.  That is, if
!
!      X1 <= XMID <= X2
!    or
!      X2 <= XMID <= X1
!
!    Otherwise, INSIDE will be FALSE.
!
  implicit none
!
  logical inside
  real x1
  real x2
  real xmid
!
  if ( ( x1 <= xmid .and. xmid <= x2 ) .or. &
       ( x1 >= xmid .and. xmid >= x2 ) ) then

    inside = .true.

  else

    inside = .false.

  end if

  return
end
subroutine list ( delx, dely, dev, element_file_name, grace, icmax, &
  icmin, icolor, idata, iplot, itable, iwrite, jbound, maxbou, maxnp, &
  maxobj, nbound, &
  ncontour, nelem, node_file, np, npe, object, scalev, show, &
  tecplot_file, title, title2, &
  x2max, x2min, xmax, xmin, xsmax, xsmin, ymax, ymin, y2max, y2min, ysmax, &
  ysmin )
!
!*******************************************************************************
!
!! LIST prints out the values of the variables of interest to the user.
!
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
!    Input, real DELX, the X spacing between nodes.  In some cases,
!    this spacing is modified to create isoparametric elements.
!
!    Input, real DELY, the Y spacing between nodes.  In some cases,
!    this spacing is modified to create isoparametric elements.
!
!    Input, character ( len = 10 ) DEV, the graphics output device:
!      cgmb - CGM binary file.
!      ps   - PostScript file.
!      XWS  - X window screen (interactive).
!
!    Input, character ( len = 80 ) ELEMENT_FILE_NAME, the element file.
!
!    Input, integer ( kind = 4 ) ICMAX, ICMIN, the maximum and minimum color
!    indices to use in color contour graphics.
!
!    Input, integer ( kind = 4 ) ICOLOR(MAXOBJ), the color index for each object.
!
!    Input, integer ( kind = 4 ) IDATA.
!    0, no problem has been defined.
!    nonzero, a problem has been defined.
!
!    Input, integer ( kind = 4 ) IPLOT, the number of plots made so far.
!
!    Output, integer ( kind = 4 ) ITABLE, the desired color table.
!
!    1: low black to high white
!    2: low blue to high yellow
!    3: low red, high blue, with bands between.
!    4: low red, yellow, green, blue, high white.
!    5: low white, blue, green, yellow, high red.
!    6: low blue to high red
!    7: linear table between 2 user colors.
!    8: linear table between N user colors.
!    9: low white to high black.
!
!    Input, integer ( kind = 4 ) IWRITE.
!    Controls debugging output.
!
!    0 means no such output.
!    1 means some.
!    2 means a lot.
!
!    Input, integer ( kind = 4 ) JBOUND(5,MAXBOU)
!
!    For each line segment of the boundary:
!
!    JBOUND(1,I) contains the element number;
!
!    JBOUND(2,I) contains the local node number of one corner
!      of the element, which forms the edge;
!
!    JBOUND(2,I) contains the "next" node along the edge.
!      If the element is linear, this is the other corner node.
!      If the element is quadratic, this is the midside node along
!        the edge.
!
!    JBOUND(4,I) contains the "next" node along the edge.
!      If the element is linear, this is 0.
!      If the element is quadratic, this is the other corner node
!        along the edge.
!
!    JBOUND(5,I) contains:
!      0 if the boundary is a wall (U = V=0);
!      1 if the boundary is open.
!
!    Input, integer ( kind = 4 ) MAXBOU.
!    The amount of storage available for the IBOUND array.
!
!    Input, integer ( kind = 4 ) MAXNP, the maximum number of nodes.
!
!    Input, integer ( kind = 4 ) MAXOBJ, the number of graphical "objects".
!
!    Input, integer ( kind = 4 ) NBOUND, the number of points defining the boundary.
!
!    Input, integer ( kind = 4 ) NCONTOUR, the number of contours to draw.
!
!    Input, integer ( kind = 4 ) NELEM, the number of elements.
!
!    Input, character ( len = 80 ) NODE_FILE, the node file.
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!    Input, integer ( kind = 4 ) NPE, the number of nodes per element.
!
!    Input, character ( len = 30 ) OBJECT(MAXOBJ), the names of the
!    graphical objects.
!
!    Input, real SCALEV.
!    A scale factor for velocity vectors.  This starts out at 1.0.
!
!    Input, logical SHOW(MAXOBJ).
!    Contains, for each object, a flag determining whether it
!    is to be shown or not.
!
!    Input, character ( len = 80 ) TECPLOT_FILE, the TECPLOT file.
!
!    Input, character ( len = 40 ) TITLE.
!    A title for the plots.
!
!    Input, character ( len = 40 ) TITLE2.
!    A subtitle used in the profile plots.
!
!    Input, real X2MAX, X2MIN, the maximum and minimum X coordinates that
!    should be used for plotting.  No plotting commands should
!    exceed these values.  This is where the "frame" might be drawn.
!
!    Input, real XMAX.
!    The maximum X coordinate of all the nodes.
!    The maximum entry in the XC array.
!
!    Input, real XMIN.
!    The minimum X coordinate of all the nodes.
!    The minimum entry in the XC array.
!
!    Input, real XSMAX.
!    The maximum X coordinate of the data to be displayed.
!    XSMAX defaults to XMAX, but can be made smaller to
!    focus on a portion of the region.
!
!    Input, real XSMIN.
!    The minimum X coordinate of the data to be displayed.
!    XSMIN defaults to XMIN, but can be made larger to
!    focus on a portion of the region.
!
!    Input, real Y2MAX, Y2MIN, the maximum and minimum Y coordinates that
!    should be used for plotting.  No plotting commands should
!    exceed these values.  This is where the "frame" might be drawn.
!
!    Output, real YMAX, the maximum Y coordinate of all the nodes.
!
!    Output, real YMIN, the minimum Y coordinate of all the nodes.
!
!    Input, real YSMAX.
!    The maximum Y coordinate of the data to be displayed.
!    YSMAX defaults to YMAX, but can be made smaller to
!    focus on a portion of the region.
!
!    Input, real YSMIN, the minimum displayed Y coordinate.
!
  implicit none
!
  integer ( kind = 4 ) maxbou
  integer ( kind = 4 ) maxnp
  integer ( kind = 4 ) maxobj
!
  real delx
  real dely
  character ( len = 10 ) dev
  character ( len = 80 ) element_file_name
  real grace
  integer ( kind = 4 ) i
  integer ( kind = 4 ) icmax
  integer ( kind = 4 ) icmin
  integer ( kind = 4 ) icolor(maxobj)
  integer ( kind = 4 ) idata
  integer ( kind = 4 ) iplot
  character isay
  integer ( kind = 4 ) itable
  integer ( kind = 4 ) iwrite
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jbound(5,maxbou)
  character ( len = 10 ) name
  integer ( kind = 4 ) nbound
  integer ( kind = 4 ) ncontour
  integer ( kind = 4 ) nelem
  character ( len = 80 ) node_file
  integer ( kind = 4 ) np
  integer ( kind = 4 ) npe
  character ( len = 40 ) object(maxobj)
  real pmax
  real pmin
  logical s_eqi
  real scalev
  logical show(maxobj)
  character ( len = 80 ) tecplot_file
  character ( len = 40 ) title
  character ( len = 40 ) title2
  real umax
  real umin
  real vmax
  real vmin
  real x2max
  real x2min
  real xmax
  real xmin
  real xsmax
  real xsmin
  real ymax
  real ymin
  real y2max
  real y2min
  real ysmax
  real ysmin
!
10    continue

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'Type the name of the variable to be displayed,'
  write ( *, '(a)' ) 'BLANK to return to the main program, '
  write ( *, '(a)' ) '? for a list of variable names.'
  write ( *, '(a)' ) ' '

  read ( *, '(a)', end = 40, err = 40 ) name

  if ( name == ' ' ) then
    return
  end if

  if ( name == '?' ) then

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'LIST:'
    write ( *, '(a)' ) '  The following variables may be listed:'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  DELX,     DELY,   DEV,       ELEMENT_FILE_NAME,'
    write ( *, '(a)' ) '  GRACE,    ICMAX,    ICMIN,  ICOLOR,    IDATA, '
    write ( *, '(a)' ) '  IPLOT,'
    write ( *, '(a)' ) '  ITABLE,   JBOUND, NBOUND,'
    write ( *, '(a)' ) '  NCONTOUR, NELEM,  NODE_FILE,    NP,   NPE,   '
    write ( *, '(a)' ) '  OBJECT,   '
    write ( *, '(a)' ) '  SCALEV,   SHOW,   TECPLOT_FILE, TITLE, '
    write ( *, '(a)' ) '  TITLE2,   '
    write ( *, '(a)' ) '  X2MAX,    X2MIN,  XMAX,         XMIN,   XPROF,  '
    write ( *, '(a)' ) '  XSMAX,    XSMIN,  YMAX,         YMIN,   Y2MAX,  '
    write ( *, '(a)' ) '  Y2MIN,    YSMAX,  YSMIN'

    go to 10
  end if
!
!  Print the value of the given variable.
!
  if ( s_eqi ( name, 'delx' ) ) then
    write ( *, * ) 'The nominal X spacing is DELX = ', delx
  else if ( s_eqi ( name, 'dely' ) ) then
    write ( *, * ) 'The nominal Y spacing is DELY = ', dely
  else if ( s_eqi ( name, 'dev' ) ) then
    if ( dev == ' ' ) then
      write ( *, * ) 'No graphics output device has been chosen.'
    else
      write ( *, * ) 'Graphics output is of type DEV = ', dev
    end if
  else if ( s_eqi ( name, 'ELEMENT_FILE_NAME' ) ) then
    write ( *, '(a)' ) 'The current element file name is ' &
      // trim ( element_file_name )
  else if ( s_eqi ( name, 'GRACE' ) ) then
    write ( *, * ) 'The grace margin is GRACE = ', grace
  else if ( s_eqi ( name, 'icmax' ) ) then
    write ( *, * ) 'Maximum index from table is ICMAX = ', icmax
  else if ( s_eqi ( name, 'icmin' ) ) then
    write ( *, * ) 'Minimum index from table is ICMIN = ', icmin
  else if ( s_eqi ( name, 'IDATA' ) ) then
    if ( idata == 0 ) then
      write ( *, * ) 'No problem has been defined, IDATA = ', idata
    else
      write ( *, * ) 'A problem has been defined, IDATA = ', idata
    end if
  else if ( s_eqi ( name, 'IPLOT' ) ) then
    write ( *, * ) 'IPLOT = ', iplot, ' plots have been made.'
  else if ( s_eqi ( name, 'ITABLE' ) ) then
    write ( *, * ) 'Color table used for area fill is ITABLE = ', itable
  else if ( s_eqi ( name, 'IWRITE' ) ) then
    write ( *, * ) 'Debugging level IWRITE = ', iwrite
  else if ( s_eqi ( name, 'NBOUND' ) ) then
    write ( *, * ) 'Number of boundary segments NBOUND = ', nbound
  else if ( s_eqi ( name, 'NCONTOUR' ) ) then
    write ( *, * ) 'Number of contour levels NCONTOUR = ', ncontour
  else if ( s_eqi ( name, 'NELEM' ) ) then
    write ( *, * ) 'There are NELEM = ', nelem, ' elements.'
  else if ( s_eqi ( name, 'NODE_FILE' ) ) then
    write ( *, * ) 'The current node file is ' // trim ( node_file )
  else if ( s_eqi ( name, 'NP' ) ) then
    write ( *, * ) 'There are NP = ', np, ' nodes.'
  else if ( s_eqi ( name, 'NPE' ) ) then
    write ( *, * ) 'The number of nodes per element, NPE = ', npe
  else if ( s_eqi ( name, 'SCALEV') ) then
    write ( *, * ) 'Scale factor for velocities is SCALEV = ', scalev
  else if ( s_eqi ( name, 'TECPLOT_FILE' ) ) then
    write ( *, * ) 'The current TECPLOT file is ' // trim ( tecplot_file )
  else if ( s_eqi ( name, 'TITLE' ) ) then
    if ( len_trim ( title ) > 0 ) then
      write ( *, * ) 'Title:' // trim ( title )
    else
      write ( *, * ) 'No title has been assigned.'
    end if
  else if ( s_eqi ( name, 'title2' ) ) then
    if ( len_trim ( title2 ) > 0 ) then
      write ( *, * ) 'Title2:' // trim ( title2 )
    else
      write ( *, * ) 'No subtitle has been assigned.'
    end if

  else if ( s_eqi ( name, 'x2max' ) .or. s_eqi ( name, 'x2min' ) ) then
    write ( *, * ) 'X2MIN = ', x2min, ' X2MAX=', x2max

  else if ( s_eqi ( name, 'xmax' ) .or. s_eqi ( name, 'xmin' ) ) then
    write ( *, * ) 'XMIN = ', xmin, ' XMAX=', xmax

  else if ( s_eqi ( name, 'xsmax' ) .or. s_eqi ( name, 'xsmin' ) ) then
    write ( *, * ) 'XSMIN = ', xsmin, ' XSMAX=', xsmax

  else if ( s_eqi ( name, 'y2max' ) .or. s_eqi ( name, 'y2min' ) ) then
    write ( *, * ) 'Y2MIN = ', y2min, ' Y2MAX=', y2max

  else if ( s_eqi ( name, 'ymax' ) .or. s_eqi ( name, 'ymin' ) ) then
    write ( *, * ) 'YMIN = ', ymin, ' YMAX=', ymax

  else if ( s_eqi ( name, 'ysmax' ) .or. s_eqi ( name, 'ysmin' ) ) then
    write ( *, * ) 'YSMIN = ', ysmin, ' YSMAX=', ysmax
  end if
!
!  Color and visibility information.
!
  if ( s_eqi ( name, 'OBJECT' ) .or. s_eqi ( name, 'SHOW' ) .or. &
       s_eqi ( name, 'ICOLOR' ) .or. s_eqi ( name, '*' ) ) then

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) ' Object    Show?    Icolor'
    write ( *, '(a)' ) ' '
    do i = 1, maxobj
      write ( *, '(a40,2x,l1,2x,i3)' ) object(i), show(i), icolor(i)
    end do
  end if

  if ( s_eqi ( name, 'JBOUND' ) ) then

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '    I Element   Node 1   Node 2   Node 3 Type'
    write ( *, '(a)' ) ' '

    do i = 1, nbound

      write ( *, '(i6,2x,5i6)' ) i, jbound(1:5,i)

      if ( mod(i,22) == 0 ) then
        read ( *, '(a)' ) isay
        if ( isay /= ' ' ) then
          go to 10
        end if
      end if

    end do

  end if

40    continue

  go to 10
end
function lnei ( strng1, strng2 )
!
!*******************************************************************************
!
!! LNEI compares two strings for non-equality, ignoring case.
!
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
!    Input, character ( len = * ) STRNG1, STRNG2, the strings to compare.
!
!    Output, logical LNEI, the result of the comparison.
!
  implicit none
!
  integer ( kind = 4 ) i
  integer ( kind = 4 ) len1
  integer ( kind = 4 ) len2
  integer ( kind = 4 ) lenc
  logical lnei
  character s1
  character s2
  character ( len = * ) strng1
  character ( len = * ) strng2
!
  len1 = len ( strng1 )
  len2 = len ( strng2 )
  lenc = min ( len1, len2 )

  lnei = .true.

  do i = 1, lenc

    s1 = strng1(i:i)
    s2 = strng2(i:i)
    call ch_cap ( s1 )
    call ch_cap ( s2 )

    if ( s1 /= s2 ) then
      return
    end if

  end do

  do i = lenc+1, len1
    if ( strng1(i:i) /= ' ' ) then
      return
    end if
  end do

  do i = lenc+1, len2
    if ( strng2(i:i) /= ' ' ) then
      return
    end if
  end do

  lnei = .false.

  return
end
subroutine nodeq16 ( r, s, area )
!
!*******************************************************************************
!
!! NODEQ16 returns the basis nodes for a 16 node quadrilateral.
!
!
!  Diagram:
!
!    |
!    1 13--14--15--16
!    |  |   :   :   |
!    |  |   :   :   |
!    |  9..10..11..12
!    S  |   :   :   |
!    |  |   :   :   |
!    |  5...6...7...8
!    |  |   :   :   |
!    |  |   :   :   |
!    0  1---2---3---4
!    |
!    +--0-----R-----1-->
!
!  Modified:
!
!    14 October 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real R(16), S(16), the coordinates of the basis nodes.
!
!    Output, real AREA, the area of the element.
!
  implicit none
!
  real area
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real r(16)
  real s(16)
!
  k = 0
  do i = 0, 3
    do j = 0, 3
      k = k + 1
      r(k) = real ( j ) / 3.0E+00
      s(k) = real ( i ) / 3.0E+00
    end do
  end do

  area = 1.0E+00

  return
end
subroutine nodeq4 ( r, s, area )
!
!*******************************************************************************
!
!! NODEQ4 returns the basis nodes for a 4 node quadrilateral.
!
!
!  Diagram:
!
!    |
!    1  3-------2
!    |  |       |
!    |  |       |
!    S  |       |
!    |  |       |
!    |  |       |
!    0  4-------1
!    |
!    +--0---R---1-->
!
!  Modified:
!
!    06 October 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real R(4), S(4), the coordinates of the basis nodes.
!
!    Output, real AREA, the area of the element.
!
  implicit none
!
  real area
  real r(4)
  real s(4)
!
  r(1) = 1.0E+00
  s(1) = 0.0E+00

  r(2) = 1.0E+00
  s(2) = 1.0E+00

  r(3) = 0.0E+00
  s(3) = 1.0E+00

  r(4) = 0.0E+00
  s(4) = 0.0E+00

  area = 1.0E+00

  return
end
subroutine nodeq8 ( r, s, area )
!
!*******************************************************************************
!
!! NODEQ8 returns the basis nodes for an 8 node quadrilateral.
!
!
!  Comment:
!
!    This element is known as the "serendipity" element.
!
!  Diagram:
!
!    |
!    1  3---7---2
!    |  |       |
!    |  |       |
!    S  8       6
!    |  |       |
!    |  |       |
!    0  4---5---1
!    |
!    +--0---R---1-->
!
!  Modified:
!
!    05 October 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real R(8), S(8), the coordinates of the basis nodes.
!
!    Output, real AREA, the area of the element.
!
  implicit none
!
  real area
  real r(8)
  real s(8)
!
  r(1) = 1.0E+00
  s(1) = 0.0E+00

  r(2) = 1.0E+00
  s(2) = 1.0E+00

  r(3) = 0.0E+00
  s(3) = 1.0E+00

  r(4) = 0.0E+00
  s(4) = 0.0E+00

  r(5) = 0.5E+00
  s(5) = 0.0E+00

  r(6) = 1.0E+00
  s(6) = 0.5E+00

  r(7) = 0.5E+00
  s(7) = 1.0E+00

  r(8) = 0.0E+00
  s(8) = 0.5E+00

  area = 1.0E+00

  return
end
subroutine nodeq9 ( r, s, area )
!
!*******************************************************************************
!
!! NODEQ9 returns the basis nodes for a 9 node quadrilateral.
!
!
!  Diagram:
!
!    |
!    1  3---7---2
!    |  |   :   |
!    |  |   :   |
!    S  8...9...6
!    |  |   :   |
!    |  |   :   |
!    0  4---5---1
!    |
!    +--0--R--1-->
!
!  Modified:
!
!    06 October 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real R(9), S(9), the coordinates of the basis nodes.
!
!    Output, real AREA, the area of the element.
!
  implicit none
!
  real area
  real r(9)
  real s(9)
!
  r(1) = 1.0E+00
  s(1) = 0.0E+00

  r(2) = 1.0E+00
  s(2) = 1.0E+00

  r(3) = 0.0E+00
  s(3) = 1.0E+00

  r(4) = 0.0E+00
  s(4) = 0.0E+00

  r(5) = 0.5E+00
  s(5) = 0.0E+00

  r(6) = 1.0E+00
  s(6) = 0.5E+00

  r(7) = 0.5E+00
  s(7) = 1.0E+00

  r(8) = 0.0E+00
  s(8) = 0.5E+00

  r(9) = 0.5E+00
  s(9) = 0.5E+00

  area = 1.0E+00

  return
end
subroutine nodet10 ( r, s, area )
!
!*******************************************************************************
!
!! NODET10 returns the basis nodes for a 10 node triangle.
!
!
!  Diagram:
!
!    |
!    1  2
!    |  |\
!    |  | \
!    |  5  7
!    |  |   \
!    S  |    \
!    |  8 10  4
!    |  |      \
!    |  |       \
!    0  3--6--9--1
!    |
!    +--0----R---1-->
!
!  Modified:
!
!    14 October 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real R(10), S(10), the coordinates of the basis nodes.
!
!    Output, real AREA, the area of the element.
!
  implicit none
!
  real area
  real r(10)
  real s(10)
!
  r(1) = 1.0E+00
  s(1) = 0.0E+00

  r(2) = 0.0E+00
  s(2) = 1.0E+00

  r(3) = 0.0E+00
  s(3) = 0.0E+00

  r(4) = 2.0E+00 / 3.0E+00
  s(4) = 1.0E+00 / 3.0E+00

  r(5) = 0.0E+00
  s(5) = 2.0E+00 / 3.0E+00

  r(6) = 1.0E+00 / 3.0E+00
  s(6) = 0.0E+00

  r(7) = 1.0E+00 / 3.0E+00
  s(7) = 2.0E+00 / 3.0E+00

  r(8) = 0.0E+00
  s(8) = 1.0E+00 / 3.0E+00

  r(9) = 2.0E+00 / 3.0E+00
  s(9) = 0.0E+00

  r(10) = 1.0E+00 / 3.0E+00
  s(10) = 1.0E+00 / 3.0E+00

  area = 0.5E+00

  return
end
subroutine nodet3 ( r, s, area )
!
!*******************************************************************************
!
!! NODET3 returns the basis nodes for the 3 node triangle.
!
!
!  Diagram:
!
!    |
!    1  2
!    |  |\
!    |  | \
!    S  |  \
!    |  |   \
!    |  |    \
!    0  3-----1
!    |
!    +--0--R--1-->
!
!  Modified:
!
!    04 October 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real R(3), S(3), the coordinates of the basis nodes.
!
!    Output, real AREA, the area of the element.
!
  implicit none
!
  real area
  real r(3)
  real s(3)

  r(1) = 1.0E+00
  s(1) = 0.0E+00

  r(2) = 0.0E+00
  s(2) = 1.0E+00

  r(3) = 0.0E+00
  s(3) = 0.0E+00

  area = 0.5E+00

  return
end
subroutine nodet6 ( r, s, area )
!
!*******************************************************************************
!
!! NODET6 returns the basis nodes for a 6 node triangle.
!
!
!  Diagram:
!
!    |
!    1  2
!    |  |\
!    |  | \
!    S  6  5
!    |  |   \
!    |  |    \
!    0  3--4--1
!    |
!    +--0--R--1-->
!
!  Modified:
!
!    04 October 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real R(6), S(6), the coordinates of the basis nodes.
!
!    Output, real AREA, the area of the element.
!
  implicit none
!
  real area
  real r(6)
  real s(6)
!
  r(1) = 1.0E+00
  s(1) = 0.0E+00

  r(2) = 0.0E+00
  s(2) = 1.0E+00

  r(3) = 0.0E+00
  s(3) = 0.0E+00

  r(4) = 0.5E+00
  s(4) = 0.0E+00

  r(5) = 0.5E+00
  s(5) = 0.5E+00

  r(6) = 0.0E+00
  s(6) = 0.5E+00

  area = 0.5E+00

  return
end
function pi ( )
!
!*******************************************************************************
!
!! PI returns the value of pi.
!
!
!  Modified:
!
!    04 December 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real PI, the value of pi.
!
  implicit none
!
  real pi
!
  pi = 3.14159265358979323846264338327950288419716939937510E+00

  return
end
subroutine pltbox ( grace, srange, x1max, x1min, x2max, x2min, &
  xsmax, xsmin, y1max, y1min, y2max, y2min, ysmax, ysmin )
!
!*******************************************************************************
!
!! PLTBOX computes a square box containing the data.
!
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
!    Input, real GRACE, the size of the "grace" margin.
!
!    Output, real SRANGE, the maximum of XSMAX-XSMIN and YSMAX-YSMIN.
!    This gives the size of a square containing the data
!    window.
!
!    Output, real X1MAX, X1MIN, the maximum and minimum X
!    coordinates of the plot, which includes a small grace margin.
!
!    Output, real X2MAX, X2MIN, the maximum and minimum X coordinates that
!    should be used for plotting.  No plotting commands should
!    exceed these values.  This is where the "frame" might be drawn.
!
!    Input, real XSMAX.
!    The maximum X coordinate of the data to be displayed.
!    XSMAX defaults to XMAX, but can be made smaller to
!    focus on a portion of the region.
!
!    Input, real XSMIN.
!    The minimum X coordinate of the data to be displayed.
!    XSMIN defaults to XMIN, but can be made larger to
!    focus on a portion of the region.
!
!    Output, real Y1MAX, Y1MIN, the maximum and minimum Y
!    coordinates of the plot, which includes a small grace margin.
!
!    Output, real Y2MAX, Y2MIN, the maximum and minimum Y coordinates that
!    should be used for plotting.  No plotting commands should
!    exceed these values.  This is where the "frame" might be drawn.
!
!    Input, real YSMAX.
!    The maximum Y coordinate of the data to be displayed.
!    YSMAX defaults to YMAX, but can be made smaller to
!    focus on a portion of the region.
!
!    Input, real YSMIN, the minimum displayed Y coordinate.
!
  implicit none
!
  real grace
  real srange
  real x1max
  real x1min
  real x2max
  real x2min
  real xsmax
  real xsmin
  real y1max
  real y1min
  real y2max
  real y2min
  real ysmax
  real ysmin
!
  srange = max ( xsmax - xsmin, ysmax - ysmin )

  if ( srange <= 0.0E+00 ) then
    srange = 1.0E+00
  end if

  x2min = 0.5E+00 * ( xsmin + xsmax ) - 0.5E+00 * srange
  x2max = 0.5E+00 * ( xsmin + xsmax ) + 0.5E+00 * srange
  y2min = 0.5E+00 * ( ysmin + ysmax ) - 0.5E+00 * srange
  y2max = 0.5E+00 * ( ysmin + ysmax ) + 0.5E+00 * srange

  x1min = x2min - grace * ( x2max - x2min )
  x1max = x2max + grace * ( x2max - x2min )
  y1min = y2min - grace * ( y2max - y2min )
  y1max = y2max + grace * ( y2max - y2min )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'PLTBOX - Note:'
  write ( *, '(a)' ) '  New total picture coordinates:'
  write ( *, '(a)' ) ' '
  write ( *, * ) x1min, ' X1MIN < =  X <= X1MAX ', x1max
  write ( *, * ) y1min, ' Y1MIN < =  Y <= Y1MAX ', y1max
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  New graphing area coordinates:'
  write ( *, '(a)' ) ' '
  write ( *, * ) x2min, ' X2MIN < =  X <= X2MAX ', x2max
  write ( *, * ) y2min, ' Y2MIN < =  Y <= Y2MAX ', y2max
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  New data window coordinates:'
  write ( *, '(a)' ) ' '
  write ( *, * ) xsmin, ' XSMIN < =  X <= XSMAX ', xsmax
  write ( *, * ) ysmin, ' YSMIN < =  Y <= YSMAX ', ysmax

  return
end
subroutine preplt ( dev, filgrf, icmax, icmin, iplot, itable, ovrlay )
!
!*******************************************************************************
!
!! PREPLT should be called before doing each plot.
!
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
!    Input, character ( len = 10 ) DEV, the graphics output device:
!      cgmb - CGM binary file.
!      ps   - PostScript file.
!      XWS  - X window screen (interactive).
!
!    Input/output, character ( len = 80 ) FILGRF, the name of the output
!    graphics file.
!
!    Input, integer ( kind = 4 ) ICMAX, ICMIN.
!    The maximum and minimum color indices to be used from
!    the color table.  The color table contains 256 colors,
!    but color indices 1 and 2 are black and white, and for some
!    reason, the predefined DRAWCGM tables generally only
!    use 2-200 for sensible colors.
!
!    Of course the entries in the color table are "off by one".
!    The first entry is for color 0, and the 256-th entry for
!    color 255.
!
!    Input/output, integer ( kind = 4 ) IPLOT, the number of plots made so far.
!
!    Input, integer ( kind = 4 ) ITABLE, the desired color table.
!
!    1: low black to high white
!    2: low blue to high yellow
!    3: low red, high blue, with bands between.
!    4: low red, yellow, green, blue, high white.
!    5: low white, blue, green, yellow, high red.
!    6: low blue to high red
!    7: linear table between 2 user colors.
!    8: linear table between N user colors.
!    9: low white to high black.
!
!    Input, logical OVRLAY.
!    If OVRLAY is true, then the next time that a plot is
!    requested, a "new frame" command is suppressed, so that
!    the new plot is shown on top of the previous one.
!
  implicit none
!
  integer ( kind = 4 ), parameter :: nval = 2
!
  character ( len = * ) dev
  character ( len = 80 ) filgrf
  integer ( kind = 4 ) icmax
  integer ( kind = 4 ) icmin
  integer ( kind = 4 ) icolor
  integer ( kind = 4 ) iplot
  integer ( kind = 4 ) itable
  logical, save :: linit = .false.
  logical ovrlay
  real xval(nval)
  real yval(nval)
!
!  If it's the first picture, then
!
!    Choose an output device,
!    Give the output file a name,
!    Initialize the graphics package.
!
  if ( .not. linit ) then

    linit = .true.

    if ( dev == ' ' ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PREPLT - Warning!'
      write ( *, '(a)' ) '  No output device was specified.'
      write ( *, '(a)' ) '  PostScript output will be generated.'
      dev = 'ps'
    end if

    call device ( dev )

    if ( dev == 'cgmb' ) then

      if ( filgrf == ' ' ) then
        filgrf = 'display.cgm'
      end if

      call outfil ( filgrf )

    else if ( dev == 'ps' ) then

      if ( filgrf == ' ' ) then
        filgrf = 'display.ps'
      end if

      call outfil ( filgrf )

    end if

    if ( itable == 0 ) then
      itable = 1
    end if

    call color_table_set ( icmin, icmax, itable )

    call grfini

    xval(1) = 0.0E+00
    xval(2) = 1.0E+00
    yval(1) = 0.0E+00
    yval(2) = 1.0E+00
    call setscl ( xval, yval, nval )

    icolor = 1
    call linclr ( icolor )
    call filclr ( icolor )
!
!  Else, if it's a later picture,
!
!    Issue a "new frame" command, unless an overlay was requested.
!
  else

    if ( .not. ovrlay ) then
      call newfrm
    end if

  end if

  iplot = iplot + 1

  return
end
subroutine quit ( dev, iplot )
!
!*******************************************************************************
!
!! QUIT shuts down the program.
!
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
!    Input, character ( len = 10 ) DEV, the graphics output device:
!      cgmb - CGM binary file.
!      ps   - PostScript file.
!      XWS  - X window screen (interactive).
!
!    Input, integer ( kind = 4 ) IPLOT, the number of plots made so far.
!
  implicit none
!
  character ( len = * ) dev
  integer ( kind = 4 ) iplot
  logical lnei
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'QUIT - Note.'
  write ( *, '(a)' ) '  DISPLAY5 is stopping now.'
  write ( *, '(a)' ) '  A copy of your commands is in "display.inp".'

  if ( iplot > 0 ) then
    call grfcls
  end if

  return
end
subroutine r_next ( s, r, done )
!
!*******************************************************************************
!
!! R_NEXT "reads" real numbers from a string, one at a time.
!
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
!    Input, character ( len = * ) S, a string, presumably containing real
!    numbers.  These may be separated by spaces or commas.
!
!    Output, real R.  If DONE is FALSE, then R contains the
!    "next" real value read from the string.  If DONE is TRUE, then
!    R is zero.
!
!    Input/output, logical DONE.
!    On input with a fresh string, the user should set DONE to TRUE.
!    On output, the routine sets DONE to FALSE if another real
!    value was read, or TRUE if no more reals could be read.
!
  implicit none
!
  logical done
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) lchar
  integer ( kind = 4 ), save :: next = 1
  real r
  character ( len = * ) s
!
  r = 0.0E+00

  if ( done ) then
    next = 1
    done = .false.
  end if

  if ( next > len ( s ) ) then
    done = .true.
    return
  end if

  call s_to_r ( s(next:), r, ierror, lchar )

  if ( ierror /= 0 .or. lchar == 0 ) then
    done = .true.
    next = 1
  else
    done = .false.
    next = next + lchar
  end if

  return
end
subroutine read_element ( element_file_name, ierror, maxelm, maxnp, maxnpe, &
  nelem, node, np, npe )
!
!*******************************************************************************
!
!! READ_ELEMENT reads an element file.
!
!
!  Format:
!
!    The element file contains information about the organization of
!    the nodes into elements.
!
!    The file may contain comment lines, which must begin with a '#' sign
!    in column 1.
!
!    Otherwise, the I-th line should contain the list, in order, of
!    the nodes that belong to element I.
!
!  Modified:
!
!    03 April 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = 80 ) ELEMENT_FILE_NAME, the name of the file
!    containing the element information.
!
!    Output, integer ( kind = 4 ) IERROR, is nonzero if an error occurred.
!
!    Input, integer ( kind = 4 ) MAXELM, the maximum number of elements.
!
!    Input, integer ( kind = 4 ) MAXNP, the maximum number of nodes.
!
!    Input, integer ( kind = 4 ) MAXNPE, the maximum number of nodes per element.
!
!    Input, integer ( kind = 4 ) NELEM, the number of elements.
!
!    Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), the global node numbers in
!    each element.
!
!    Output, integer ( kind = 4 ) NP, the number of nodes.
!
!    Input, integer ( kind = 4 ) NPE, the number of nodes per element.
!
  implicit none
!
  integer ( kind = 4 ) maxelm
  integer ( kind = 4 ) maxnpe
!
  character ( len = * ) element_file_name
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ielem
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) input_unit
  integer ( kind = 4 ) ios
  integer ( kind = 4 ) maxnp
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) node(maxnpe,maxelm)
  integer ( kind = 4 ) np
  integer ( kind = 4 ) npe
  character ( len = 100 ) string
!
  ierror = 0

  call get_unit ( input_unit )
!
!  Open the data file.
!
  open ( unit = input_unit, file = element_file_name, status = 'old', &
    iostat = ios )

  if ( ios /= 0 ) then
    ierror = 1
    write ( *, * ) ' '
    write ( *, * ) 'READ_ELEMENT - Fatal error!'
    write ( *, * ) '  Could not open the element file.'
    return
  end if
!
!  Initialize number of elements, number of nodes.
!
  ielem = 0
  np = 0
!
!  Try to read the next line of the file, which should contain
!  the node numbers associated with the next element.
!
  do ielem = 1, nelem
!
!  Find a non-comment, nonblank line to read.
!
    do

      read ( input_unit, '(a)', iostat = ios ) string

      if ( ios /= 0 ) then
        write ( *, * ) ' '
        write ( *, * ) 'READ_ELEMENT - Fatal error!'
        write ( *, * ) '  End of file while seeking data for element ', ielem
        ierror = 1
        stop
      end if

      if ( string(1:1) /= '#' .and. 0 < len_trim ( string ) ) then
        exit
      end if

    end do
!
!  Read the element data from the string.
!
    read ( string, * ) node(1:npe,ielem)

    do i = 1, npe

      if ( node(i,ielem) <= 0 ) then
        ierror = 1
        write ( *, * ) ' '
        write ( *, * ) 'READ_ELEMENT - Fatal error!'
        write ( *, * ) '  Illegal node in element ', ielem
        write ( *, * ) '  Local node number       ', i
        write ( *, * ) '  has global node number  ', node(i,ielem)
        return
      end if

      np = max ( np, node(i,ielem) )

    end do

  end do

  close ( unit = input_unit )

  if ( np > maxnp ) then
    ierror = 1
    write ( *, * ) ' '
    write ( *, * ) 'READ_ELEMENT - Fatal error!'
    write ( *, * ) '  This problem has too many nodes!'
    write ( *, * ) '  Number of nodes NP = ', np
    write ( *, * ) '  DISPLAY4 can handle up to MAXNP = ', maxnp
    return
  end if

  if ( np <= 0 ) then
    ierror = 1
    write ( *, * ) ' '
    write ( *, * ) 'READ_ELEMENT - Fatal error!'
    write ( *, * ) '  Number of nodes NP = ', np
    write ( *, * ) '  but the number of nodes must be positive!'
    return
  end if
!
!  All seems well.
!
  write ( *, * ) ' '
  write ( *, * ) 'READ_ELEMENT - Note:'
  write ( *, * ) '  Number of elements, NELEM = ', nelem
  write ( *, * ) '  Number of nodes NP = ', np

  return
end
subroutine read_node_sizes ( node_file, np, nq )
!
!*******************************************************************************
!
!! READ_NODE_SIZES reads size information from the node file.
!
!
!  Discussion:
!
!    The node file contains the value of various quantities at the nodes.
!    The format is:
!
!      Line 1: NP, the number of nodes
!      Line 2: NQ, counts X, Y and the quantities.
!      Line 3: NAMES, the names of X, Y, and the quantities
!      Lines 4 through NP+3: X, Y, and the quantities for each node.
!
!    Blanks and comment lines, which have a "#" in column 1, may occur anywhere.
!
!  Example:
!
!    # Node data file.
!    #
!    5
!    6
!
!     X    Y    P    U    V    W
!
!    0.0  0.0  1.0  0.5  0.3  0.2
!    1.0  0.0  1.2  0.4  0.4  1.4
!    0.0  1.0  0.9  0.5  0.7  3.2
!    1.0  1.0  1.1  0.3  0.4  4.1
!    0.5  0.5  0.8  0.2  0.3  4.1
!
!  Modified:
!
!    22 June 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) NODE_FILE, the file containing the data.
!    This file should be a "plain text" file.
!
!    Output, integer ( kind = 4 ) NP, the number of nodes.
!
!    Output, integer ( kind = 4 ) NQ, the number of node quantities.
!
  implicit none
!
  logical done
  integer ( kind = 4 ) i
  integer ( kind = 4 ) input_unit
  integer ( kind = 4 ) ios
  character ( len = 256 ) line
  character ( len = * ) node_file
  integer ( kind = 4 ) np
  integer ( kind = 4 ) nq
!
  call get_unit ( input_unit )

  open ( unit = input_unit, file = node_file, status = 'old', iostat = ios )

  if ( ios /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'READ_NODE_SIZES - Fatal error!'
    write ( *, '(a)' ) '  Could not open the node data file!'
    return
  end if
!
!  Read the number of nodes.
!
  do

    read ( input_unit, '(a)', iostat = ios ) line

    if ( ios /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'READ_NODE_SIZES - Fatal error!'
      write ( *, '(a)' ) '  I/O error reading NP, the number of nodes.'
      stop
    end if

    if ( len_trim ( line ) /= 0 ) then
      if ( line(1:1) /= '#' ) then
        exit
      end if
    end if

  end do

  done = .true.
  call i_next ( line, np, done )
!
!  Read the number of quantities.
!
  do

    read ( input_unit, '(a)', iostat = ios ) line

    if ( ios /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'READ_NODE_SIZES - Fatal error!'
      write ( *, '(a)' ) '  I/O error reading NQ, the number of quantities.'
      stop
    end if

    if ( len_trim ( line ) /= 0 ) then
      if ( line(1:1) /= '#' ) then
        exit
      end if
    end if

  end do

  done = .true.
  call i_next ( line, nq, done )

  close ( unit = input_unit )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'READ_NODE_SIZES:'
  write ( *, '(a,i6)' ) '  Number of nodes           = ', np
  write ( *, '(a,i6)' ) '  Number of node properties = ', nq

  return
end
subroutine read_node_values ( node_file, np, nq, v, name, ierror )
!
!*******************************************************************************
!
!! READ_NODE_VALUES reads value information from a node file.
!
!
!  Discussion:
!
!    The node file contains the value of various quantities at the nodes.
!    The format is:
!
!      Line 1: NP, the number of nodes
!      Line 2: NQ, counts X, Y and the other quantities.
!      Line 3: NAMES, the names of X, Y, and the other quantities
!      Lines 4 through NP+3: X, Y, and the other quantities for each node.
!
!    Blanks and comment lines, which have a "#" in column 1, may occur anywhere.
!
!  Example:
!
!    # Node data file.
!    #
!    5
!    6
!
!     X    Y    P    U    V    W
!
!    0.0  0.0  1.0  0.5  0.3  0.2
!    1.0  0.0  1.2  0.4  0.4  1.4
!    0.0  1.0  0.9  0.5  0.7  3.2
!    1.0  1.0  1.1  0.3  0.4  4.1
!    0.5  0.5  0.8  0.2  0.3  4.1
!
!  Modified:
!
!    22 June 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) NODE_FILE, the file containing the data.
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!    Input, integer ( kind = 4 ) NQ, the number of node quantities.
!
!    Output, real V(NP,NQ), the values of the node quantities.
!
!    Output, character ( len = 10 ) NAME(NQ), the names of the quantities.
!
!    Output, integer ( kind = 4 ) IERROR, error flag.
!
  implicit none
!
  integer ( kind = 4 ) np
  integer ( kind = 4 ) nq
!
  logical done
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) input_unit
  integer ( kind = 4 ) ios
  integer ( kind = 4 ) j
  character ( len = 256 ) line
  character ( len = 10 ) name(nq)
  character ( len = * ) node_file
  real v(np,nq)
!
  ierror = 0

  call get_unit ( input_unit )

  open ( unit = input_unit, file = node_file, status = 'old', iostat = ios )

  if ( ios /= 0 ) then
    ierror = 1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'READ_NODE_VALUES - Fatal error!'
    write ( *, '(a)' ) '  Could not open the node data file!'
    return
  end if
!
!  Read the number of nodes.
!
  do

    read ( input_unit, '(a)', iostat = ios ) line

    if ( ios /= 0 ) then
      ierror = 1
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'READ_NODE_VALUES - Fatal error!'
      write ( *, '(a)' ) '  I/O error reading NP, the number of nodes.'
      stop
    end if

    if ( len_trim ( line ) /= 0 ) then
      if ( line(1:1) /= '#' ) then
        exit
      end if
    end if

  end do

  done = .true.
! call i_next ( line, np, done )
!
!  Read the number of quantities.
!
  do

    read ( input_unit, '(a)', iostat = ios ) line

    if ( ios /= 0 ) then
      ierror = 1
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'READ_NODE_VALUES - Fatal error!'
      write ( *, '(a)' ) '  I/O error reading NQ, the number of quantities.'
      stop
    end if

    if ( len_trim ( line ) /= 0 ) then
      if ( line(1:1) /= '#' ) then
        exit
      end if
    end if

  end do

  done = .true.
! call i_next ( line, nq, done )
!
!  Read the titles.
!
  do

    read ( input_unit, '(a)', iostat = ios ) line

    if ( ios /= 0 ) then
      ierror = 1
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'READ_NODE_VALUES - Fatal error!'
      write ( *, '(a)' ) '  I/O error reading the quantity titles.'
      stop
    end if

    if ( len_trim ( line ) /= 0 ) then
      if ( line(1:1) /= '#' ) then
        exit
      end if
    end if

  end do

  done = .true.

  do i = 1, nq
    call word_next_read ( line, name(i), done )
  end do
!
!  For each node, read the quantities.
!
  do i = 1, np

    do

      read ( input_unit, '(a)', iostat = ios ) line

      if ( ios /= 0 ) then
        ierror = 1
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'READ_NODE_VALUES - Fatal error!'
        write ( *, '(a)' ) '  I/O error reading the quantities.'
        stop
      end if

      if ( len_trim ( line ) /= 0 ) then
        if ( line(1:1) /= '#' ) then
          exit
        end if
      end if

    end do

    done = .true.

    do j = 1, nq
      call r_next ( line, v(i,j), done )
    end do

  end do

  close ( unit = input_unit )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'READ_NODE_VALUES - Note:'
  write ( *, '(a)' ) '  The node value data has been read.'

  return
end
subroutine read_tecplot ( tecplot_file, ierror, maxelm, maxnp, maxnpe, &
  name, nelem, node, np, npe, v )
!
!*******************************************************************************
!
!! READ_TECPLOT reads a TECPLOT data file.
!
!
!  Discussion:
!
!    The file has a format something like this:
!
!    3 title lines:
!
!      Title="Finite Element Data from WRITE_TECPLOT"'
!      Variables="X","Y","P","RHO","U","V"
!      Zone N=         441, E=         800, F=FEPOINT, ET=TRIANGLE
!
!    N node records of the form:
!
!      V(I,1), V(I,2), ... V(I,NQ)         
!
!    E element records of the form:
!
!      NODE(1,I), NODE(2,I), NODE(3,I)
!
!  Modified:
!
!    02 December 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) TECPLOT_FILE, the TECPLOT file.
!
!    Input, integer ( kind = 4 ) MAXELM, the maximum number of elements.
!
!    Input, integer ( kind = 4 ) MAXNPE, the maximum number of nodes per element.
!
!    Output, character ( len = * ) NAME(5), the names of the objects.
!
!    Output, integer ( kind = 4 ) NELEM, the number of elements.
!
!    Output, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), the global node numbers in 
!    each element.
!
!    Output, integer ( kind = 4 ) NP, the number of nodes.
!
!    Output, integer ( kind = 4 ) NPE, the number of nodes per element.
!
!    Output, real V(NP,5), X and Y coordinates, the horizontal
!    and vertical velocity, and the pressure at each node.
!
  implicit none
!
  integer ( kind = 4 ) maxelm
  integer ( kind = 4 ) maxnp
  integer ( kind = 4 ) maxnpe
!
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) input_unit
  integer ( kind = 4 ) ios
  integer ( kind = 4 ) j
  character ( len = * ) name(5)
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) node(maxnpe,maxelm)
  integer ( kind = 4 ) np
  integer ( kind = 4 ) npe
  integer ( kind = 4 ) numline
  character ( len = 80 ) tecplot_file
  character ( len = 80 ) title
  real v(maxnp,5)
!
  ierror = 0
  nelem = 0
  np = 0
  numline = 0

  name(1) = 'X'
  name(2) = 'Y'
  name(3) = 'U'
  name(4) = 'V'
  name(5) = 'P'
!
!  Open the data file.
!
  call get_unit ( input_unit )

  open ( unit = input_unit, file = tecplot_file, status = 'old', iostat = ios )

  if ( ios /= 0 ) then
    ierror = 1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'READ_TECPLOT - Fatal error!'
    write ( *, '(a)' ) '  Could not open the input file: ' &
      // trim ( tecplot_file )
    return
  end if
!
!  Read title line 1
!
  numline = numline + 1
  read ( input_unit, '(a)', iostat = ios ) title

  if ( ios /= 0 ) then
    ierror = 1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'READ_TECPLOT - Fatal error!'
    write ( *, '(a)' ) '  Error while reading TITLE 1.'
    return
  end if

  write ( *, '(a)' ) trim ( title )
!
!  Read title line 2
!  Here is where you should properly read the actual names, and 
!  the number of variables!
!
  numline = numline + 1
  read ( input_unit, '(a)', iostat = ios ) title

  if ( ios /= 0 ) then
    ierror = 1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'READ_TECPLOT - Fatal error!'
    write ( *, '(a)' ) '  Error while reading TITLE 2.'
    return
  end if

  write ( *, '(a)' ) trim ( title )
!
!  Read title line 3
!
  numline = numline + 1
  read ( input_unit, '(a)', iostat = ios ) title

  if ( ios /= 0 ) then
    ierror = 1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'READ_TECPLOT - Fatal error!'
    write ( *, '(a)' ) '  Error while reading TITLE 3.'
    return
  end if
  write ( *, '(a)' ) trim ( title )
!
!  We presume this line has the format:
!   "Zone N=         441, E=         800, F=FEPOINT, ET=TRIANGLE"
!
  ierror = 0
  call intnexrd ( title, np, ierror )

  if ( ierror /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'READ_TECPLOT - Fatal error!'
    write ( *, '(a)' ) '  Could not extract NP from data.'
    stop
  end if

  if ( np > maxnp ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'READ_TECPLOT - Fatal error!'
    write ( *, '(a)' ) '  This problem has too many nodes!'
    write ( *, '(a,i6)' ) '  Number of nodes NP = ', np
    write ( *, '(a,i6)' ) '  DISPLAY5 can handle up to MAXNP = ', maxnp
    stop
  end if

  call intnexrd ( title, nelem, ierror )

  if ( ierror /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'READ_TECPLOT - Fatal error!'
    write ( *, '(a)' ) '  Could not extract NELEM from data.'
    stop
  end if

  if ( nelem > maxelm ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'READ_TECPLOT - Fatal error!'
    write ( *, '(a)' ) '  This problem has too many elements!'
    write ( *, * ) '  Number of elements NELEM = ', nelem
    write ( *, * ) '  DISPLAY5 can handle up to MAXELM = ', maxelm
    stop
  end if

  npe = 3
!
!  Read the node data.
!
  do i = 1, np
    numline = numline + 1
    read ( input_unit, *, iostat = ios  ) v(i,1), v(i,2), v(i,3), v(i,4), v(i,5)
    if ( ios /= 0 ) then
      ierror = 1
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'READ_TECPLOT - Fatal error!'
      write ( *, '(a,i6)' ) &
        '  Error while reading physical data for node I = ', i
      return
    end if
  end do
!
!  Read the element data.
!
  do j = 1, nelem
    read ( input_unit, *, iostat = ios  ) node(i,1:npe)
    if ( ios /= 0 ) then
      ierror = 1
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'READ_TECPLOT - Fatal error!'
      write ( *, * ) '  Error while reading node data for element ', j
      return
    end if
  end do

  close ( unit = input_unit )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'READ_TECPLOT - Read the input data.'
  write ( *, * ) '  Number of input lines = ', numline
  write ( *, * ) '  Number of nodes =       ', np
  write ( *, * ) '  Nodes per element =     ', npe
  write ( *, * ) '  Number of elements =    ', nelem

  return
end
subroutine refbf_l3 ( q, dqdeta, dqdxsi, eta, iq, xsi )
!
!*******************************************************************************
!
!! REFBF_L3 evaluates a reference element linear basis function.
!
!
!  Diagram:
!
!          ^
!          |
!        1 +    2
!          |    |\
!    ETA   |    | \
!          |    |  \
!        0 +    3---1
!          |
!          +----+---+--->
!               0   1
!
!              XSI
!
!    The basis function and its X and Y derivatives are evaluated at a
!    particular point (X,Y) in a particular element, by referring to the
!    corresponding points (XSI,ETA) in the reference triangle.
!
!  Modified:
!
!    28 September 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real Q, DQDETA, DQDXSI, the basis
!    function, and its derivatives with respect to ETA and XSI, at
!    the point with reference coordinates (ETA,XSI).
!
!    Input, real ETA, XSI, the local coordinates of the
!    point at which the basis information is desired.
!
!    Input, integer ( kind = 4 ) IQ, the local node number, between 1 and
!    3, whose basis function is being evaluated.
!
  implicit none
!
  real dqdeta
  real dqdxsi
  real eta
  integer ( kind = 4 ) iq
  real q
  real xsi
!
  if ( iq == 1 ) then
    q = xsi
    dqdxsi = 1.0E+00
    dqdeta =  0.0E+00
  else if ( iq == 2 ) then
    q = eta
    dqdxsi = 0.0E+00
    dqdeta = 1.0E+00
  else if ( iq == 3 ) then
    q = 1.0E+00 - xsi - eta
    dqdxsi = - 1.0E+00
    dqdeta = - 1.0E+00
  else
    q = 0.0E+00
    dqdxsi = 0.0E+00
    dqdeta = 0.0E+00
  end if

  return
end
subroutine refbf_l4 ( w, dwdeta, dwdxsi, eta, iq, xsi )
!
!*******************************************************************************
!
!! REFBF_L4 evaluates a linear basis function in the reference quadrilateral.
!
!
!  Diagram:
!
!          ^
!          |
!        1 +    3---2
!          |    |   |
!    ETA   |    |   |
!          |    |   |
!        0 +    4---1
!          |
!          +----+---+--->
!               0   1
!
!           XSI
!
!    The basis function has the form:
!
!      W(XSI,ETA) = A * XSI * ETA + B * XSI + C * ETA + D.
!
!  Modified:
!
!    01 October 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real W, DWDETA, DWDXSI, the value of the basis
!    function, and its derivatives with respect to ETA and XSI, at
!    the point (ETA,XSI).
!
!    Input, real ETA, the local ETA coordinate
!    at which the basis information is desired.
!
!    Input, integer ( kind = 4 ) IQ, the local node number, between 1 and
!    4, whose basis function is being evaluated.
!
!    Input, real XSI, the local XSI coordinate
!    at which the basis information is desired.
!
  implicit none
!
  real dwdeta
  real dwdxsi
  real eta
  integer ( kind = 4 ) iq
  real w
  real xsi
!
  if ( iq == 1 ) then
    w = xsi * ( 1.0E+00 - eta )
    dwdxsi = ( 1.0E+00 - eta )
    dwdeta = - xsi
  else if ( iq == 2 ) then
    w = xsi * eta
    dwdxsi = eta
    dwdeta = xsi
  else if ( iq == 3 ) then
    w = ( 1.0E+00 - xsi ) * eta
    dwdxsi = - eta
    dwdeta = ( 1.0E+00 - xsi )
  else if ( iq == 4 ) then
    w = ( 1.0E+00 - xsi ) * ( 1.0E+00 - eta )
    dwdxsi = - ( 1.0E+00 - eta )
    dwdeta = - ( 1.0E+00 - xsi )
  else
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'REFBF_L4 - Fatal error!'
    write ( *, '(a,i6)' ) '  Request for basis function IQ = ', iq
    write ( *, '(a)' ) '  but IQ must be between 1 and 4.'
    stop
  end if

  return
end
subroutine refbf_q6 ( w, dwdeta, dwdxsi, eta, iq, xsi )
!
!*******************************************************************************
!
!! REFBF_Q6 evaluates shape functions for a 6 node triangle.
!
!
!  Diagram:
!
!    |
!    1  2
!    |  |\
!    |  | \
!    S  6  5
!    |  |   \
!    |  |    \
!    0  3--4--1
!    |
!    +--0--R--1-->
!
!  Modified:
!
!    04 October 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real W, DWDETA, DWDXSI, the value of the basis
!    function, and its derivatives with respect to ETA and XSI, at
!    the point (XSI,ETA).
!
!    Input, real ETA, the ETA coordinate of the point.
!
!    Input, integer ( kind = 4 ) IQ, the basis function, between 1 and 6.
!
!    Input, real XSI, the XSI coordinate of the point.
!
  implicit none
!
  real dwdeta
  real dwdxsi
  real eta
  integer ( kind = 4 ) iq
  real w
  real xsi
!
  if ( iq == 1 ) then

    w =        2.0E+00 * xsi * ( xsi - 0.5E+00 )
    dwdxsi = - 1.0E+00 + 4.0E+00 * xsi
    dwdeta =   0.0E+00

  else if ( iq == 2 ) then

    w =        2.0E+00 * eta * ( eta - 0.5E+00 )
    dwdxsi =   0.0E+00
    dwdeta = - 1.0E+00           + 4.0E+00 * eta

  else if ( iq == 3 ) then

    w =        2.0E+00 * ( 1.0E+00 - xsi - eta ) * ( 0.5E+00 - xsi - eta )
    dwdxsi = - 3.0E+00 + 4.0E+00 * xsi + 4.0E+00 * eta
    dwdeta = - 3.0E+00 + 4.0E+00 * xsi + 4.0E+00 * eta

  else if ( iq == 4 ) then

    w =        4.0E+00 * xsi * ( 1.0E+00 - xsi - eta )
    dwdxsi =   4.0E+00 - 8.0E+00 * xsi - 4.0E+00 * eta
    dwdeta =       - 4.0E+00 * xsi

  else if ( iq == 5 ) then

    w =      4.0E+00 * xsi * eta
    dwdxsi = 4.0E+00       * eta
    dwdeta = 4.0E+00 * xsi

  else if ( iq == 6 ) then

    w =      4.0E+00 * eta * ( 1.0E+00 - xsi - eta )
    dwdxsi =                 - 4.0E+00 * eta
    dwdeta = 4.0E+00 - 4.0E+00 * xsi - 8.0E+00 * eta

  else

    w = 0.0E+00
    dwdxsi = 0.0E+00
    dwdeta = 0.0E+00

  end if

  return
end
subroutine refmap_l3 ( x, a, b, c )
!
!*******************************************************************************
!
!! REFMAP_L3 sets reference-to-physical map for a linear triangle.
!
!
!  Diagram:
!
!    2
!    |\
!    | \
!    |  \
!    |   \
!    |    \
!    3-----1
!
!  Formula:
!
!    X(ETA,XSI) = A1 * XSI + B1 * ETA + C1
!    Y(ETA,XSI) = A2 * XSI + B2 * ETA + C2
!
!  Modified:
!
!    27 September 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real X(3), the X (or Y) coordinates of the element nodes.
!
!    Output, real A, B, C, D, the coefficients of
!    the reference-to-physical element mapping.
!
  implicit none
!
  real a
  real b
  real c
  real x(3)
!
  a =  + x(1)        - x(3)
  b =         + x(2) - x(3)
  c =                  x(3)

  return
end
subroutine refmap_l4 ( x, a, b, c, d )
!
!*******************************************************************************
!
!! REFMAP_L4 sets the reference-to-physical map for a linear quadrilateral.
!
!
!  Diagram:
!
!    3---------2
!    |         |
!    |         |
!    |         |
!    |         |
!    4---------1
!
!  Formula:
!
!    X(ETA,XSI) = A1 * XSI + B1 * XSI*ETA + C1 * ETA + D1
!    Y(ETA,XSI) = A2 * XSI + B2 * XSI*ETA + C2 * ETA + D2
!
!  Modified:
!
!    02 October 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real X(4), the X (or Y) coordinates of the element nodes.
!
!    Output, real A, B, C, D, the coefficients of
!    the reference-to-physical element mapping.
!
  implicit none
!
  real a
  real b
  real c
  real d
  real x(4)
!
  a =    x(1)               - x(4)
  b =  - x(1) + x(2) - x(3) + x(4)
  c =                + x(3) - x(4)
  d =                       + x(4)

  return
end
subroutine refmap_q6 ( qdata, a, b, c, d, e, f )
!
!*******************************************************************************
!
!! REFMAP_Q6 returns the interpolation map for data on a 6 node triangle.
!
!
!  Diagram:
!
!    |
!    1  2
!    |  |\
!    E  | \
!    T  6  5
!    A  |   \
!    |  |    \
!    0  3--4--1
!    |
!    +--0-XSI-1-->
!
!  Formula:
!
!    Q(R,S) = A * XSI**2 + B * XSI * ETA + C * ETA**2 + D * XSI + E * ETA + F
!
!  Note:
!
!    To find the polynomial form of, say, the third basis function,
!    set QDATA = ( 0, 0, 1, 0, 0, 0 ).
!
!  Modified:
!
!    14 October 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real QDATA(6), the values of a quantity at the basis nodes.
!
!    Output, real A, B, C, D, E, F, the polynomial coefficients of the
!    interpolation function over the element.
!
  implicit none
!
  real a
  real b
  real c
  real d
  real e
  real f
  real qdata(6)
!
  a =  2.0E+00 * qdata(1) - 4.0E+00 * qdata(4) + 2.0E+00 * qdata(3)
  b =  4.0E+00 * qdata(3) - 4.0E+00 * qdata(4) + 4.0E+00 * qdata(5) &
     - 4.0E+00 * qdata(6)
  c =  2.0E+00 * qdata(2) - 4.0E+00 * qdata(6) + 2.0E+00 * qdata(3)
  d =      - qdata(1) + 4.0E+00 * qdata(4) - 3.0E+00 * qdata(3)
  e =      - qdata(2) + 4.0E+00 * qdata(6) - 3.0E+00 * qdata(3)
  f =        qdata(3)

  return
end
subroutine refnode ( n, r, s )
!
!*******************************************************************************
!
!! REFNODE returns the basis nodes for any available element.
!
!
!  Modified:
!
!    04 October 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of nodes in the element.
!
!    Output, real R(N), S(N), the coordinates of the basis nodes.
!
  implicit none
!
  integer ( kind = 4 ) n
!
  real area
  real r(n)
  real s(n)
!
  if ( n == 3 ) then
    call nodet3 ( r, s, area )
  else if ( n == 4 ) then
    call nodeq4 ( r, s, area )
  else if ( n == 6 ) then
    call nodet6 ( r, s, area )
  else if ( n == 8 ) then
    call nodeq8 ( r, s, area )
  else if ( n == 9 ) then
    call nodeq9 ( r, s, area )
  else if ( n == 10 ) then
    call nodet10 ( r, s, area )
  else if ( n == 16 ) then
    call nodeq16 ( r, s, area )
  else
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'REFNODE - Fatal error!'
    write ( *, '(a)' ) '  The requested number of nodes per element'
    write ( *, '(a)' ) '  is not a valid choice.'
    write ( *, '(a,i6)' ) '  N = ', n
    stop
  end if

  return
end
subroutine rsize ( delx, dely, grace, nelem, nflag, np, srange, x1max, x1min, &
  x2max, x2min, xc, xmax, xmin, xsmax, xsmin, xtmax, xtmin, y1max, y1min, &
  y2max, y2min, yc, ymax, ymin, ysmax, ysmin, ytmax, ytmin )
!
!*******************************************************************************
!
!! RSIZE computes the size of the region.
!
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
!    Input, real DELX.
!    The X spacing between nodes.  In some cases,
!    this spacing is modified to create isoparametric elements.
!
!    Input, real DELY.
!    The Y spacing between nodes.  In some cases,
!    this spacing is modified to create isoparametric elements.
!
!    Input, real GRACE, the size of the "grace" margin.
!
!    Input, integer ( kind = 4 ) NELEM, the number of elements.
!
!    Output, logical NFLAG(MAXNP), flags nodes which are active.
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!    Output, real SRANGE, the maximum of the height and width
!    of the data in physical coordinates.
!
!      SRANGE = MAX( XSMAX-XSMIN, YSMAX-YSMIN )
!
!    Output, real X1MAX, X1MIN, the maximum and minimum X
!    coordinates of the plot, which includes a small grace margin.
!
!    Output, real X2MAX, X2MIN, the maximum and minimum X
!    coordinates that should be used for plotting.  No plotting
!    commands should  exceed these values.  This is where the
!    "frame" might be drawn.
!
!    Input, real XC(MAXNP), the X coordinates of the nodes.
!
!    Output, real XMAX, the maximum X coordinate of all the nodes.
!
!    Output, real XMIN, the minimum X coordinate of all the nodes.
!
!    Output, real XSMAX.
!    The maximum X coordinate of the data to be displayed.
!    XSMAX defaults to XMAX, but can be made smaller to
!    focus on a portion of the region.
!
!    Output, real XSMIN.
!    The minimum X coordinate of the data to be displayed.
!    XSMIN defaults to XMIN, but can be made larger to
!    focus on a portion of the region.
!
!    Output, real Y1MAX, Y1MIN, the maximum and minimum Y
!    coordinates of the plot, which includes a small grace margin.
!
!    Output, real Y2MAX, Y2MIN, the maximum and minimum Y
!    coordinates that should be used for plotting.  No plotting
!    commands should exceed these values.  This is where the
!    "frame" might be drawn.
!
!    Input, real YC(MAXNP), the Y coordinates of the nodes.
!
!    Output, real YMAX, the maximum Y coordinate of all the nodes.
!
!    Output, real YMIN, the minimum Y coordinate of all the nodes.
!
!    Output, real YSMAX.
!    The maximum Y coordinate of the data to be displayed.
!    YSMAX defaults to YMAX, but can be made smaller to
!    focus on a portion of the region.
!
!    Output, real YSMIN, the minimum displayed Y coordinate.
!
  implicit none
!
  integer ( kind = 4 ) np
!
  real delx
  real dely
  real grace
  integer ( kind = 4 ) nelem
  logical nflag(np)
  real srange
  real x1max
  real x1min
  real x2max
  real x2min
  real xc(np)
  real xmax
  real xmin
  real xsmax
  real xsmin
  real xtmax
  real xtmin
  real xvmax
  real xvmin
  real y1max
  real y1min
  real y2max
  real y2min
  real yc(np)
  real ymax
  real ymin
  real ysmax
  real ysmin
  real ytmax
  real ytmin
  real yvmax
  real yvmin
!
!  Find the maximum and minimum values of X and Y.
!
  call fsize ( nflag, np, xc, xtmax, xtmin, xvmax, xvmin )

  call fsize ( nflag, np, yc, ytmax, ytmin, yvmax, yvmin )

  xmax = xvmax
  xmin = xvmin
  ymax = yvmax
  ymin = yvmin
!
!  DELX and DELY are estimates for the X and Y dimensions of the
!  region.  For various reasons, we are going to behave as though
!  the region were a square.
!
  delx = sqrt ( ( xmax - xmin ) * ( ymax - ymin ) / real ( nelem ) )
  dely = delx

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'RSIZE:'
  write ( *, '(a)' ) '  Total physical data coordinates:'
  write ( *, '(g14.6,a,g14.6)' ) xtmin, ' =  XTMIN <= X <= XTMAX = ', xtmax
  write ( *, '(g14.6,a,g14.6)' ) ytmin, ' =  YTMIN <= Y <= YTMAX = ', ytmax
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Visible physical data coordinates:'
  write ( *, '(g14.6,a,g14.6)' ) xmin, ' =  XMIN <= X <= XMAX = ', xmax
  write ( *, '(g14.6,a,g14.6)' ) ymin, ' =  YMIN <= Y <= YMAX = ', ymax

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'RSIZE - Note:'
  write ( *, '(a)' ) '  Setting data window to physical window!'
!
!  Data window starts out the same as the physical window.
!
  xsmax = xmax
  xsmin = xmin
  ysmax = ymax
  ysmin = ymin
!
!  Compute box containing data.
!
  call pltbox ( grace, srange, x1max, x1min, x2max, x2min, xsmax, &
    xsmin, y1max, y1min, y2max, y2min, ysmax, ysmin )

  return
end
subroutine s_blank_delete ( s )
!
!*******************************************************************************
!
!! S_BLANK_DELETE removes blanks from a string, left justifying the remainder.
!
!
!  Comment:
!
!    All TAB characters are also removed.
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
!
  character c
  integer ( kind = 4 ) iget
  integer ( kind = 4 ) iput
  character ( len = * ) s
  character, parameter :: TAB = char ( 9 )
!
  iput = 0

  do iget = 1, len ( s )

    c = s(iget:iget)

    if ( c /= ' ' .and. c /= TAB ) then
      iput = iput + 1
      s(iput:iput) = c
    end if

  end do

  s(iput+1:) = ' '

  return
end
function s_eqi ( strng1, strng2 )
!
!*******************************************************************************
!
!! S_EQI is a case insensitive comparison of two strings for equality.
!
!
!  Examples:
!
!    S_EQI ( 'Anjana', 'ANJANA' ) is .TRUE.
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
!    Input, character ( len = * ) STRNG1, STRNG2, the strings to compare.
!
!    Output, logical S_EQI, the result of the comparison.
!
  implicit none
!
  integer ( kind = 4 ) i
  integer ( kind = 4 ) len1
  integer ( kind = 4 ) len2
  integer ( kind = 4 ) lenc
  logical s_eqi
  character s1
  character s2
  character ( len = * ) strng1
  character ( len = * ) strng2
!
  len1 = len ( strng1 )
  len2 = len ( strng2 )
  lenc = min ( len1, len2 )

  s_eqi = .false.

  do i = 1, lenc

    s1 = strng1(i:i)
    s2 = strng2(i:i)
    call ch_cap ( s1 )
    call ch_cap ( s2 )

    if ( s1 /= s2 ) then
      return
    end if

  end do

  do i = lenc + 1, len1
    if ( strng1(i:i) /= ' ' ) then
      return
    end if
  end do

  do i = lenc + 1, len2
    if ( strng2(i:i) /= ' ' ) then
      return
    end if
  end do

  s_eqi = .true.

  return
end
subroutine s_plot ( angle, cwide, pwide, s, x, y, flush )
!
!*******************************************************************************
!
!! S_PLOT plots a character string onto a graphics image.
!
!
!  Discussion:
!
!    The string can be at any angle and at any size.
!
!    The plot is assumed to be of size PWIDE by PHITE, although PHITE
!    itself is not input.
!
!  Warning:
!
!    This routine must be modified to work with a particular graphics package.
!    The current code calls two routines:
!      MOVCGM ( X, Y ) moves to a point (X,Y) in the plot;
!      DRWCGM ( X, Y ) draws a line from the current point to (X,Y).
!
!  Modified:
!
!    14 April 1999
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
!    since 40 characters would fit, but 2.0E+00 would be nonsense.
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
!
!    'L' - the string will be drawn flush left.
!    'C' - the string will be centered.
!    'R' - the string will be drawn flush right.
!
  implicit none
!
  real angle
  real ca
  character c
  real cwide
  real degrees_to_radians
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
!
  nchar = len_trim ( s )

  if ( pwide <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'S_PLOT - Serious error!'
    write ( *, '(a)' ) '  The plot width PWIDE is negative!'
    write ( *, '(a,g14.6)' ) '  PWIDE = ', pwide
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
    xcopy = x - 0.5E+00 * nchar * cwide * cos ( degrees_to_radians ( angle ) )
    ycopy = y - 0.5E+00 * nchar * cwide * sin ( degrees_to_radians ( angle ) )
  else if ( flush(1:1) == 'r' .or. flush(1:1) == 'R' ) then
    xcopy = x - nchar * cwide * cos ( degrees_to_radians ( angle ) )
    ycopy = y - nchar * cwide * sin ( degrees_to_radians ( angle ) )
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
    ca = cos ( degrees_to_radians ( angle ) )
    sa = sin ( degrees_to_radians ( angle ) )
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

      do

        if ( ifont(ip) == 0 ) then
          exit
        end if

        xc = xb + ( ifont(ip) - 1 ) * scl2
        yc = yb + ( ifont(ip+1) - 7 ) * scl2
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
!
!  Call the user supplied routine to draw a line from
!  (XOLD,YOLD) to (XNEW,YNEW).
!
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
subroutine s_to_i ( s, ival, ierror, last )
!
!*******************************************************************************
!
!! S_TO_I reads an integer value from a string.
!
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
!    Output, integer ( kind = 4 ) LAST, the last character of S used to make IVAL.
!
  implicit none
!
  character c
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) isgn
  integer ( kind = 4 ) istate
  integer ( kind = 4 ) ival
  integer ( kind = 4 ) last
  character ( len = * ) s
!
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
        last = i - 1
        return
      end if

    end if

  end do
!
!  If we read all the characters in the string, see if we're OK.
!
  if ( istate == 2 ) then
    ival = isgn * ival
    last = len_trim ( s )
  else
    ierror = 1
    last = 0
  end if

  return
end
subroutine s_to_r ( s, r, ierror, lchar )
!
!*******************************************************************************
!
!! S_TO_R reads a real number from a string.
!
!
!  Discussion:
!
!    This routine will read as many characters as possible until it reaches
!    the end of the string, or encounters a character which cannot be
!    part of the real number.
!
!    Legal input is:
!
!       1 blanks,
!       2 '+' or '-' sign,
!       2.5 spaces
!       3 integer part,
!       4 decimal point,
!       5 fraction part,
!       6 'E' or 'e' or 'D' or 'd', exponent marker,
!       7 exponent sign,
!       8 exponent integer part,
!       9 exponent decimal point,
!      10 exponent fraction part,
!      11 blanks,
!      12 final comma or semicolon.
!
!    with most quantities optional.
!
!  Examples:
!
!    S                 R
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
!  Modified:
!
!    12 February 2001
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
!    Output, real R, the real value that was read from the string.
!
!    Output, integer ( kind = 4 ) IERROR, error flag.
!
!    0, no errors occurred.
!
!    1, 2, 6 or 7, the input number was garbled.  The
!    value of IERROR is the last type of input successfully
!    read.  For instance, 1 means initial blanks, 2 means
!    a plus or minus sign, and so on.
!
!    Output, integer ( kind = 4 ) LCHAR, the number of characters read from
!    the string to form the number, including any terminating
!    characters such as a trailing comma or blanks.
!
  implicit none
!
  logical ch_eqi
  character c
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) ihave
  integer ( kind = 4 ) isgn
  integer ( kind = 4 ) iterm
  integer ( kind = 4 ) jbot
  integer ( kind = 4 ) jsgn
  integer ( kind = 4 ) jtop
  integer ( kind = 4 ) lchar
  integer ( kind = 4 ) nchar
  integer ( kind = 4 ) ndig
  real r
  real rbot
  real rexp
  real rtop
  character ( len = * ) s
  character, parameter :: TAB = char ( 9 )
!
  nchar = len_trim ( s )
  ierror = 0
  r = 0.0E+00
  lchar = - 1
  isgn = 1
  rtop = 0.0E+00
  rbot = 1.0E+00
  jsgn = 1
  jtop = 0
  jbot = 1
  ihave = 1
  iterm = 0

  do

    lchar = lchar + 1
    c = s(lchar+1:lchar+1)
!
!  Blank or TAB character.
!
    if ( c == ' ' .or. c == TAB ) then

      if ( ihave == 2 ) then

      else if ( ihave == 6 .or. ihave == 7 ) then
        iterm = 1
      else if ( ihave > 1 ) then
        ihave = 11
      end if
!
!  Comma.
!
    else if ( c == ',' .or. c == ';' ) then

      if ( ihave /= 1 ) then
        iterm = 1
        ihave = 12
        lchar = lchar + 1
      end if
!
!  Minus sign.
!
    else if ( c == '-' ) then

      if ( ihave == 1 ) then
        ihave = 2
        isgn = - 1
      else if ( ihave == 6 ) then
        ihave = 7
        jsgn = - 1
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
      else if ( ihave >= 6 .and. ihave <= 8 ) then
        ihave = 9
      else
        iterm = 1
      end if
!
!  Exponent marker.
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
    else if ( ihave < 11 .and. lge ( c, '0' ) .and. lle ( c, '9' ) ) then

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
        rtop = 10.0E+00 * rtop + real ( ndig )
      else if ( ihave == 5 ) then
        rtop = 10.0E+00 * rtop + real ( ndig )
        rbot = 10.0E+00 * rbot
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
    if ( iterm == 1 .or. lchar+1 >= nchar ) then
      exit
    end if

  end do
!
!  If we haven't seen a terminator, and we have examined the
!  entire string, then we're done, and LCHAR is equal to NCHAR.
!
  if ( iterm /= 1 .and. lchar+1 == nchar ) then
    lchar = nchar
  end if
!
!  Number seems to have terminated.  Have we got a legal number?
!  Not if we terminated in states 1, 2, 6 or 7!
!
  if ( ihave == 1 .or. ihave == 2 .or. ihave == 6 .or. ihave == 7 ) then

    ierror = ihave

    return
  end if
!
!  Number seems OK.  Form it.
!
  if ( jtop == 0 ) then
    rexp = 1.0E+00
  else

    if ( jbot == 1 ) then
      rexp = 10.0E+00**( jsgn * jtop )
    else
      rexp = jsgn * jtop
      rexp = rexp / jbot
      rexp = 10.0E+00**rexp
    end if

  end if

  r = isgn * rexp * rtop / rbot

  return
end
subroutine setviz ( eflag, eflagu, maxnpe, nelem, nflag, nflag0, nflag1, node, &
  np, npe, xc, xsmax, xsmin, yc, ysmax, ysmin )
!
!*******************************************************************************
!
!! SETVIZ determines what nodes and elements should be visible.
!
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
!    Output, logical EFLAG(MAXELM), element visibility flags.
!
!    Input, integer ( kind = 4 ) MAXNPE, the maximum number of nodes per element.
!
!    Input, integer ( kind = 4 ) NELEM, the number of elements.
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!    Input, real XC(MAXNP), the X coordinates of nodes.
!
!    Input, real YC(MAXNP), the Y coordinates of nodes.
!
  implicit none
!
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) np
!
  logical eflag(nelem)
  logical eflagu(nelem)
  integer ( kind = 4 ) i
  logical insidex
  logical insidey
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) maxe
  integer ( kind = 4 ) maxn
  integer ( kind = 4 ) mine
  integer ( kind = 4 ) minn
  logical nflag(np)
  logical nflag0(np)
  logical nflag1(np)
  integer ( kind = 4 ) node(maxnpe,nelem)
  integer ( kind = 4 ) npe
  integer ( kind = 4 ) nvize
  integer ( kind = 4 ) nvizn
  real xc(np)
  real xsmax
  real xsmin
  real yc(np)
  real ysmax
  real ysmin
!
!  Determine NFLAG1, which says which nodes are within the current window.
!
  do i = 1, np

    call line_seg_contains_point_1d ( xsmin, xc(i), xsmax, insidex )
    call line_seg_contains_point_1d ( ysmin, yc(i), ysmax, insidey )

    if ( insidex .and. insidey ) then

      nflag1(i) = .true.

    else

      nflag1(i) = .false.

    end if

  end do
!
!  Determine NVIZN, the number of nodes that are visible.
!
  nvizn = 0
  minn = np + 1
  maxn = 0

  do i = 1, np

    if ( nflag0(i) .and. nflag1(i) ) then
      nflag(i) = .true.
      nvizn = nvizn + 1
      minn = min ( minn, i )
      maxn = max ( maxn, i )
    else
      nflag(i) = .false.
    end if

  end do

  if ( nvizn /= np ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SETVIZ - Note - Some nodes are hidden:'
    write ( *, '(a,i6)' ) '  The number of nodes is NP = ', np
    write ( *, * ) '  Of these, there are ', nvizn, ' visible.'
    if ( minn <= maxn ) then
      write ( *, * ) '  They range from ', minn, ' through ', maxn
    end if
  end if
!
!  Report how many elements are visible.
!  An element is only visible if the user wants to see it (EFLAGU),
!  and all its nodes lie within the window (NFLAG1).
!
  nvize = 0
  mine = nelem + 1
  maxe = 0

  do i = 1, nelem

    eflag(i) = eflagu(i)

    do j = 1, npe

      if ( eflag(i) ) then

        k = node(j,i)

        if ( .not. nflag1(k) ) then
          eflag(i) = .false.
        end if

      end if

    end do

    if ( eflag(i) ) then
      nvize = nvize + 1
      mine = min ( mine, i )
      maxe = max ( maxe, i )
    end if

  end do

  if ( nvize /= nelem ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SETVIZ - Note - Some elements are hidden:'
    write ( *, '(a,i6)' ) '  The number of elements is NELEM = ', nelem
    write ( *, * ) '  Of these, there are ', nvize, ' visible.'
    if ( mine <= maxe ) then
      write ( *, * ) '  They range from ', mine, ' through ', maxe
    end if
  end if

  return
end
subroutine shoelc ( eflag, etaref, maxnpe, nelem, node, np, npe, xc, &
  xsiref, yc )
!
!*******************************************************************************
!
!! SHOELC colors in the elements.
!
!
!  Discussion:
!
!    For isoparametric elements of higher than linear order, we generate
!    extra points between the standard nodes, and approximate the curved
!    elements by polygons.
!
!  Modified:
!
!    05 December 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, logical EFLAG(NELEM), element visibility flags.
!
!    Input, real ETAREF(MAXNPE), the ETA coordinates of the reference nodes.
!
!    Input, integer ( kind = 4 ) MAXNPE, the maximum number of nodes per element.
!
!    Input, integer ( kind = 4 ) NELEM, the number of elements.
!
!    Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), the global node numbers in each
!    element.
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!    Input, integer ( kind = 4 ) NPE, the number of nodes per element.
!
!    Input, real XC(NP), the X coordinates of the nodes.
!
!    Input, real XSIREF(MAXNPE), the XSI coordinates of the reference nodes.
!
!    Input, real YC(NP), the Y coordinates of the nodes.
!
!  Local Parameters:
!
!    Integer NOD(6), contains a list of the local node
!    numbers, in the order that we want to visit them, while
!    drawing the outline of the element.
!
  implicit none
!
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) np
!
  logical eflag(nelem)
  real etaref(maxnpe)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) icolor
  integer ( kind = 4 ) ielem
  integer ( kind = 4 ) iglobal
  integer ( kind = 4 ) ilocal
  integer ( kind = 4 ) j
  integer ( kind = 4 ) nod(6)
  integer ( kind = 4 ) node(maxnpe,nelem)
  integer ( kind = 4 ) npe
  integer ( kind = 4 ) npts
  real xc(np)
  real xpoly(6)
  real xsiref(maxnpe)
  real yc(np)
  real ypoly(6)
!
  if ( npe == 3 ) then
    nod(1) = 1
    nod(2) = 2
    nod(3) = 3
  else if ( npe == 4 ) then
    nod(1) = 1
    nod(2) = 2
    nod(3) = 3
    nod(4) = 4
  else if ( npe == 6 ) then
    nod(1) = 1
    nod(2) = 5
    nod(3) = 2
    nod(4) = 6
    nod(5) = 3
    nod(6) = 4
  end if

  npts = 6

  do i = 1, nelem

    ielem = i
!
!  If element is "visible":
!
    if ( eflag(i) ) then
!
!  Set the "fill color" randomly.
!
      call i_random ( 0, 255, icolor )
      call filclr ( icolor )
!
!  If element is 3 or 4 node:
!
      if ( npe == 3 .or. npe == 4  ) then

        do j = 1, npe
          ilocal = nod(j)
          iglobal = node(ilocal,i)
          xpoly(j) = xc(iglobal)
          ypoly(j) = yc(iglobal)
        end do

        call plygon ( npe, xpoly, ypoly )
!
!  If element is isoparametric:
!
      else

        do j = 1, npe
          ilocal = nod(j)
          xpoly(j) = xsiref(ilocal)
          ypoly(j) = etaref(ilocal)
        end do

        if ( npe == 6 ) then

          call iso_poly_q6 ( ypoly, ielem, maxnpe, nelem, node, np, &
            npe, npts, xc, xpoly, yc )

        end if

      end if

    end if

  end do

  return
end
subroutine shoelm ( eflag, etaref, maxnpe, nelem, node, np, npe, xc, xsiref, &
  yc )
!
!*******************************************************************************
!
!! SHOELM draws the elements of the grid.
!
!
!  Discussion:
!
!    For isoparametric elements of higher than linear order, we generate
!    extra points between the standard nodes, and approximate the curved
!    boundaries by a sequence of short straight lines.
!
!  Modified:
!
!    05 December 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, logical EFLAG(NELEM), element visibility flags.
!
!    Input, real ETAREF(MAXNPE), the ETA coordinates of the reference nodes.
!
!    Input, integer ( kind = 4 ) MAXNPE, the maximum number of nodes per element.
!
!    Input, integer ( kind = 4 ) NELEM, the number of elements.
!
!    Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), the global node numbers in 
!    each element.
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!    Input, integer ( kind = 4 ) NPE, the number of nodes per element.
!
!    Input, real XC(NP), the X coordinates of the nodes.
!
!    Input, real XSIREF(MAXNPE), the XSI coordinates of the reference nodes.
!
!    Input, real YC(NP), the Y coordinates of the nodes.
!
!  Local Parameters:
!
!    Integer NOD(6), contains a list of the local node
!    numbers, in the order that we want to visit them, while
!    drawing the outline of the element.
!
  implicit none
!
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) np
!
  logical eflag(nelem)
  real etaref(maxnpe)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ielem
  integer ( kind = 4 ) iglobal
  integer ( kind = 4 ) ilocal
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jglobal
  integer ( kind = 4 ) jlocal
  integer ( kind = 4 ) jp1
  integer ( kind = 4 ) nod(6)
  integer ( kind = 4 ) node(maxnpe,nelem)
  integer ( kind = 4 ) npe
  real xa
  real xb
  real xc(np)
  real xsiref(maxnpe)
  real ya
  real yb
  real yc(np)
!
  if ( npe == 3 ) then
    nod(1) = 1
    nod(2) = 2
    nod(3) = 3
  else if ( npe == 4 ) then
    nod(1) = 1
    nod(2) = 2
    nod(3) = 3
    nod(4) = 4
  else if ( npe == 6 ) then
    nod(1) = 1
    nod(2) = 5
    nod(3) = 2
    nod(4) = 6
    nod(5) = 3
    nod(6) = 4
  end if

  do i = 1, nelem
!
!  If element is "visible":
!
    if ( eflag(i) ) then
!
!  If element is 3 or 4 node:
!
      if ( npe == 3 .or. npe == 4 ) then

        ilocal = nod(1)
        iglobal = node(ilocal,i)
        call movcgm ( xc(iglobal), yc(iglobal))

        do j = 1, npe

          if ( j < npe ) then
            jp1 = j+1
          else
            jp1 = 1
          end if

          jlocal = nod(jp1)
          jglobal = node(jlocal,i)
          call drwcgm ( xc(jglobal), yc(jglobal) )

        end do
!
!  If element is isoparametric:
!
      else

        ilocal = nod(1)
        xa = xsiref(ilocal)
        ya = etaref(ilocal)

        do j = 1, npe

          if ( j < npe ) then
            jp1 = j + 1
          else
            jp1 = 1
          end if

          jlocal = nod(jp1)
          xb = xsiref(jlocal)
          yb = etaref(jlocal)

          ielem = i

          call iso_line_q6 ( xa, ya, xb, yb, ielem, maxnpe, nelem, &
            node, np, npe, xc, yc )

          xa = xb
          ya = yb

        end do

      end if

    end if

  end do

  return
end
subroutine stream ( ie, in, maxnpe, nelem, node, np, npe, rho, s, u, v, xc, yc )
!
!*******************************************************************************
!
!! STREAM assigns the value of the stream function to each node.
!
!
!  Discussion:
!
!    STREAM is given the horizontal and vertical mass velocities at
!    each node, and tries to do line integration to compute the
!    stream function.
!
!    The stream function PSI(X,Y) is related to the mass velocity
!    ( RHO * U, RHO * V ) by
!
!      Curl PSI = ( d PSI / d Y, - d PSI / d X ) = ( RHO * U, RHO * V )
!
!    The stream function is unique up to a constant.  We fix this constant
!    by setting PSI to zero at the first node in the first element.
!
!    In any element, if we are given the value of PSI at a point
!    with local coordinates ( XSI0, ETA0 ), we can determine PSI
!    at another node with local coordinates ( XSI1, ETA1 ) using the
!    fact that PSI is determined by a line integral:
!
!      PSI(XSI1,ETA1)
!
!      = PSI(XSI0,ETA0) + Integral (S0 to S1) d PSI/d S  dS.
!
!      = PSI(XSI0,ETA0) + Integral (S0 to S1)
!        ( dPSI/dX * dX/dXSI + dPSI/dY * dY/dXSI dXSI/dS
!        + dPSI/dX * dX/dETA + dPSI/dY * dY/dETA dETA/dS ) dS
!
!      = PSI(XSI0,ETA0) + Integral (S0 to S1)
!        ( ( - RHO * V dX/dXSI + RHO * U dY/dXSI ) dXSI/dS
!        + ( - RHO * V dX/dETA + RHO * U dY/dETA ) dETA/dS ) dS.
!
!    Once all the nodes in this element are done, we find another
!    element, which has some nodes done and some not.  We compute
!    the line integrals from a known node to the unknown nodes
!    to complete this element.
!
!    This process is repeated until all elements are complete.
!
!    This routine makes no assumptions about the global numbering of
!    the nodes and elements.  Instead, once it has computed all the
!    stream function values for a given element, it searches for any
!    other element which has some nodes with defined values and some
!    without, because that is an element which can be "finished".
!    This process is repeated until all elements are done.  The only
!    way it could fail is if the elements are not simply connected.
!
!  Modified:
!
!    24 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Workspace, integer IE(NELEM).
!
!    Workspace, integer IN(NP).
!
!    Input, integer ( kind = 4 ) MAXNPE, the maximum number of nodes per element.
!
!    Input, integer ( kind = 4 ) NELEM, the actual number of elements.
!
!    Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), the global node numbers in each
!    element.
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!    Input, integer ( kind = 4 ) NPE, the number of nodes per element.
!
!    Output, real S(NP), the value of the stream function at each node.
!
!    Input, real U(NP), the horizontal velocities at each node.
!
!    Input, real V(NP), the vertical velocities at each node.
!
!    Input, real XC(NP), the X coordinates of the nodes.
!
!    Input, real YC(NP), the Y coordinates of the nodes.
!
  implicit none
!
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) np
!
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ie(nelem)
  integer ( kind = 4 ) ielem
  integer ( kind = 4 ) iglob
  integer ( kind = 4 ) iloc
  integer ( kind = 4 ) in(np)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jglob
  integer ( kind = 4 ) jloc
  integer ( kind = 4 ) kglob
  integer ( kind = 4 ) kloc
  logical l0
  logical l1
  integer ( kind = 4 ) nexnod(6,5)
  integer ( kind = 4 ) next
  integer ( kind = 4 ) node(maxnpe,nelem)
  integer ( kind = 4 ) npe
  real rho(np)
  real s(np)
  real sinc
  real u(np)
  real v(np)
  real xc(np)
  real yc(np)
!
!  Record in NEXNOD the sequence of nodes.
!
  if ( npe == 3 ) then

    nexnod(1,1) = 2
    nexnod(1,2) = 3

    nexnod(2,1) = 3
    nexnod(2,2) = 1

    nexnod(3,1) = 1
    nexnod(3,2) = 2

  else if ( npe == 4 ) then

    nexnod(1,1) = 2
    nexnod(1,2) = 3
    nexnod(1,3) = 4

    nexnod(2,1) = 3
    nexnod(2,2) = 4
    nexnod(2,3) = 1

    nexnod(3,1) = 4
    nexnod(3,2) = 1
    nexnod(3,3) = 2

    nexnod(4,1) = 1
    nexnod(4,2) = 2
    nexnod(4,3) = 3

  else if ( npe == 6 ) then

    nexnod(1,1) = 5
    nexnod(1,2) = 2
    nexnod(1,3) = 6
    nexnod(1,4) = 3
    nexnod(1,5) = 4

    nexnod(2,1) = 6
    nexnod(2,2) = 3
    nexnod(2,3) = 4
    nexnod(2,4) = 1
    nexnod(2,5) = 5

    nexnod(3,1) = 4
    nexnod(3,2) = 1
    nexnod(3,3) = 5
    nexnod(3,4) = 2
    nexnod(3,5) = 6

    nexnod(4,1) = 1
    nexnod(4,2) = 5
    nexnod(4,3) = 2
    nexnod(4,4) = 6
    nexnod(4,5) = 3

    nexnod(5,1) = 2
    nexnod(5,2) = 6
    nexnod(5,3) = 3
    nexnod(5,4) = 4
    nexnod(5,5) = 1

    nexnod(6,1) = 3
    nexnod(6,2) = 4
    nexnod(6,3) = 1
    nexnod(6,4) = 5
    nexnod(6,5) = 2

  end if
!
!  Zero out the stream function.
!
  s(1:np) = 0.0E+00
!
!  IN records that the stream function has been computed at a given node.
!
  in(1:np) = 0
!
!  IE records that the stream function has been computed at all nodes
!  of a given element.
!
  ie(1:nelem) = 0
!
!  Set S of the lower left hand node in element 1 to 0.
!
  ielem = 1
  if ( npe == 3 ) then
    iloc = 3
  else if ( npe == 4 ) then
    iloc = 4
  else if ( npe == 6 ) then
    iloc = 3
  end if

  iglob = node(iloc,ielem)
  s(iglob) = 0.0E+00
  in(iglob) = 1
!
!  Work on given element IE, for which we know that local node
!  INOD has a computed value of S.
!  Fill in the value of S at all nodes where it is still unknown.
!
10    continue

  kloc = iloc
  kglob = node(kloc,ielem)
!
!  Supposing that we know the stream function at local node JLOC
!  of element IELEM, work your way around the element from there,
!  checking successive nodes.
!
  do next = 1, npe-1

    jloc = kloc
    jglob = kglob

    kloc = nexnod(iloc,next)
    kglob = node(kloc,ielem)
!
!  If the next node KLOC in the element does not have its stream
!  function computed, then call GETINC to compute it.
!
    if ( in(kglob) == 0 ) then

      call stream_inc ( ielem, jloc, maxnpe, nelem, node, np, npe, &
        rho, sinc, u, v, xc, yc )

      s(kglob) = s(jglob) + sinc
      in(kglob) = 1

    end if

  end do
!
!  Now that all the nodes in element IELEM have been done, mark the
!  element as done.
!
  ie(ielem) = 1
!
!  Now seek an element which is not known to be done, containing
!  some elements which are known and some which are not.
!
  do i = 1, nelem

    if ( ie(i) == 0 ) then

      l1 = .false.
      l0 = .false.

      do j = 1, npe
        jglob = node(j,i)
        if ( in(jglob) == 0 ) then
          l0 = .true.
        else
          iloc = j
          l1 = .true.
        end if
      end do

      if ( l0 .and. l1 ) then
        ielem = i
        go to 10
      end if
!
!  It's possible for an element to be done without our knowing it
!  beforehand.  If so, mark it now.
!
      if ( l1 ) then
        ie(i) = 1
      end if

    end if

  end do

  return
end
subroutine stream_inc ( ielem, jloc, maxnpe, nelem, node, np, npe, rho, sinc, &
  u, v, xc, yc )
!
!*******************************************************************************
!
!! STREAM_INC computes SINC, the stream function increment.
!
!
!  Discussion:
!
!    SINC is the increment incurred in moving from local node JLOC to
!    local node JLOC+1 in element IELEM.
!
!  Modified:
!
!    15 October 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) IELEM, the element we are examining.
!
!    Input, integer ( kind = 4 ) JLOC, the local node number of interest.
!
!    Input, integer ( kind = 4 ) MAXNPE, the maximum number of nodes per element.
!
!    Input, integer ( kind = 4 ) NELEM, the number of elements.
!
!    Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), the global node numbers in each element.
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!    Input, integer ( kind = 4 ) NPE, the number of nodes per element.
!
!    Output, real SINC, the increment in the stream function.
!
!    Input, real U(NP), V(NP), the horizontal and vertical
!    velocities at each node.
!
!    Input, real XC(NP), the X coordinates of the nodes.
!
!    Input, real YC(NP), the Y coordinates of the nodes.
!
  implicit none
!
  integer ( kind = 4 ), parameter :: ngauss = 3
!
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) np
!
  real dels
  real det
  real detads
  real detadx
  real detady
  real dxdeta
  real dxdxsi
  real dxsids
  real dxsidx
  real dxsidy
  real dydeta
  real dydxsi
  real eta
  integer ( kind = 4 ) ielem
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) igauss
  integer ( kind = 4 ) jloc
  integer ( kind = 4 ) node(maxnpe,nelem)
  integer ( kind = 4 ) npe
  real rho(np)
  real rhoval
  real sinc
  real u(np)
  real uval
  real v(np)
  real vval
  real wq(ngauss)
  real xc(np)
  real xq(ngauss)
  real xsi
  real yc(np)
!
!  Get the Gauss weights and abscissas for the interval [-1,1].
!
  call gquad ( ngauss, wq, xq )
!
!  Compute the line integral along one side of the element.
!
  sinc = 0.0E+00

  do igauss = 1, ngauss

    if ( npe == 3 ) then

      if ( jloc == 1 ) then
        xsi = 0.5E+00 * ( 1.0E+00 - xq(igauss) )
        eta = 0.5E+00 * ( 1.0E+00 + xq(igauss) )
        dxsids = - 0.5E+00
        detads = 0.5E+00
        dels = 1.0E+00
      else if ( jloc == 2 ) then
        xsi = 1.0E+00
        eta = 0.5E+00 * ( 1.0E+00 - xq(igauss) )
        dxsids = 0.0E+00
        detads = - 0.5E+00
        dels = 1.0E+00
      else if ( jloc == 3 ) then
        xsi = 0.5E+00 * ( 1.0E+00 + xq(igauss) )
        eta = 0.0E+00
        dxsids = 0.5E+00
        detads = 0.0E+00
        dels = 1.0E+00
      end if

    else if ( npe == 4 ) then

      if ( jloc == 1 ) then
        xsi = 1.0E+00
        eta = 0.5E+00 * ( 1.0E+00 + xq(igauss) )
        dxsids = 0.0E+00
        detads = 0.5E+00
        dels = 1.0E+00
      else if ( jloc == 2 ) then
        xsi = 0.5E+00 * ( 1.0E+00 - xq(igauss) )
        eta = 1.0E+00
        dxsids = -0.5E+00
        detads = 0.0E+00
        dels = 1.0E+00
      else if ( jloc == 3 ) then
        xsi = 0.0E+00
        eta = 0.5E+00 * ( 1.0E+00 - xq(igauss) )
        dxsids = 0.0E+00
        detads = - 0.5E+00
        dels = 1.0E+00
      else if ( jloc == 4 ) then
        xsi = 0.5E+00 * ( 1.0E+00 + xq(igauss) )
        eta = 0.0E+00
        dxsids = 0.5E+00
        detads = 0.0E+00
        dels = 1.0E+00
      end if

    else if ( npe == 6 ) then

      if ( jloc == 1 ) then
        xsi = ( 3.0E+00 - xq(igauss) ) / 4.0E+00
        eta = ( 1.0E+00 + xq(igauss) ) / 4.0E+00
        dxsids = - sqrt ( 2.0E+00 ) / 2.0E+00
        detads =   sqrt ( 2.0E+00 ) / 2.0E+00
        dels = sqrt ( 2.0E+00 ) / 2.0E+00
      else if ( jloc == 5 ) then
        xsi = ( 1.0E+00 - xq(igauss) ) / 4.0E+00
        eta = ( 3.0E+00 + xq(igauss) ) / 4.0E+00
        dxsids = - sqrt ( 2.0E+00 ) / 2.0E+00
        detads =   sqrt ( 2.0E+00 ) / 2.0E+00
        dels = sqrt ( 2.0E+00 ) / 2.0E+00
      else if ( jloc == 2 ) then
        xsi = 0.0E+00
        eta = ( 3.0E+00 - xq(igauss) ) / 4.0E+00
        dxsids =   0.0E+00
        detads = - 1.0E+00
        dels = 0.5E+00
      else if ( jloc == 6 ) then
        xsi = 0.0E+00
        eta = ( 1.0E+00 - xq(igauss) ) / 4.0E+00
        dxsids =   0.0E+00
        detads = - 1.0E+00
        dels = 0.5E+00
      else if ( jloc == 3 ) then
        xsi = ( 1.0E+00 + xq(igauss) ) / 4.0E+00
        eta = 0.0E+00
        dxsids = 1.0E+00
        detads = 0.0E+00
        dels = 0.5E+00
      else if ( jloc == 4 ) then
        xsi = ( 3.0E+00 + xq(igauss) ) / 4.0E+00
        eta = 0.0E+00
        dxsids = 1.0E+00
        detads = 0.0E+00
        dels = 0.5E+00
      end if

    end if

    call transform ( det, detadx, detady, dxdeta, dxdxsi, dxsidx, &
      dxsidy, dydeta, dydxsi, eta, ielem, ierror, maxnpe, nelem,  &
      node, np, npe, xc, xsi, yc )
!
!  Evaluate RHO, U and V at the Gauss point.
!
    call ueval ( eta, ielem, maxnpe, nelem, node, np, npe, &
      rho, rhoval, u, uval, v, vval, xsi )
!
!  Compute the contribution to the increment.
!
    sinc = sinc + 0.5E+00 * dels * wq(igauss) * rhoval * &
      ( ( - vval * dxdxsi + uval * dydxsi ) * dxsids + &
      ( - vval * dxdeta + uval * dydeta ) * detads )

  end do

  return
end
subroutine timestamp ( )
!
!*******************************************************************************
!
!! TIMESTAMP prints the current YMDHMS date as a time stamp.
!
!
!  Example:
!
!    May 31 2001   9:45:54.872 AM
!
!  Modified:
!
!    31 May 2001
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
!
  character ( len = 8 ) ampm
  integer ( kind = 4 ) d
  character ( len = 8 ) date
  integer ( kind = 4 ) h
  integer ( kind = 4 ) m
  integer ( kind = 4 ) mm
  character ( len = 9 ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) s
  character ( len = 10 )  time
  integer ( kind = 4 ) values(8)
  integer ( kind = 4 ) y
  character ( len = 5 ) zone
!
  call date_and_time ( date, time, zone, values )

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

  write ( *, '(a,1x,i2,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    trim ( month(m) ), d, y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end
subroutine transform ( det, detadx, detady, dxdeta, dxdxsi, &
  dxsidx, dxsidy, dydeta, dydxsi, eta, ielem, ierror, maxnpe, &
  nelem, node, np, npe, xc, xsi, yc )
!
!*******************************************************************************
!
!! TRANSFORM maps reference elements to image elements.
!
!
!  Discussion:
!
!    We know everything about the (isoparametric) element once we
!    specify the location of nodes.
!
!    TRANSFORM computes the entries of the jacobian of the transformation
!    and the determinant of the jacobian.  Essentially, the jacobian
!    records the relationship between derivatives with respect to XSI
!    and ETA and a point in the reference element, and derivatives
!    with respect to X and Y of the same function as defined in the
!    isoparametric element.
!
!    The four entries of the jacobian are symbolically named DETADX,
!    DETADY, DXSIDX and DXSIDY, and we know that the jacobian gives
!    us the following relation between derivatives with respect to
!    XSI and ETA, and derivatives with respect to X and Y:
!
!      d F(X,Y)/dX     (d XSI/dX  d ETA/dX )   ( d F(XSI, ETA)/d XSI )
!      d F(X,Y)/dY  =  (d XSI/dY  d ETA/dY ) * ( d F(XSI, ETA)/d ETA )
!
!  Modified:
!
!    15 October 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real DET, the determinant of the jacobian of the
!    transformation between the reference and isoparametric
!    elements.
!
!    Output, real DETADX, DETADY, the partial derivatives
!    d ETA/d X and d ETA/d Y at (XSI,ETA).
!
!    Output, real DXDETA, DXDXSI, the partial derivatives
!    d X/d ETA, d X/d XSI evaluated at (XSI,ETA).
!
!    Output, real DXSIDX, DXSIDY, the partial derivatives
!    d XSI/d X and d XSI/d Y at (XSI,ETA).
!
!    Output, real DYDETA, DYDXSI, the partial derivatives
!    d Y/d ETA, d Y/d XSI evaluated at (XSI,ETA).
!
!    Input, real ETA, the ETA coordinate of the point.
!
!    Input, integer ( kind = 4 ) IELEM, the number of the isoparametric
!    element we are examining.
!
!    Output, integer ( kind = 4 ) IERROR, error flag.
!    0, no error occurred.
!    1, the Jacobian was singular.
!
!    Input, integer ( kind = 4 ) MAXNPE, the maximum number of nodes per element.
!
!    Input, integer ( kind = 4 ) NELEM, the number of elements.
!
!    Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), the global node numbers in each element.
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!    Input, integer ( kind = 4 ) NPE, the number of nodes per element.
!
!    Input, real XC(NP), the X coordinates of the nodes.
!
!    Input, real XSI, the XSI coordinate of the point.
!
!    Input, real YC(NP), the Y coordinates of the nodes.
!
  implicit none
!
  logical, parameter :: debug = .FALSE.

  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) np
!
  real a
  real b
  real c
  real d
  real det
  real detadx
  real detady
  real dxdeta
  real dxdxsi
  real dxsidx
  real dxsidy
  real dydeta
  real dydxsi
  real e
  real eta
  real f
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ielem
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) node(maxnpe,nelem)
  integer ( kind = 4 ) npe
  real xx(6)
  real xc(np)
  real xsi
  real xval
  real yc(np)
  real yval
!
  ierror = 0
!
!  Compute X(XSI,ETA).
!
  do i = 1, npe
    xx(i) = xc(node(i,ielem))
  end do

  if ( npe == 3 ) then

    call refmap_l3 ( xx, a, b, c )

    xval = a * xsi + b * eta + c
    dxdxsi = a
    dxdeta = b

  else if ( npe == 4 ) then

    call refmap_l4 ( xx, a, b, c, d )

    xval = a * xsi + b * xsi * eta + c * eta + d
    dxdxsi = a + b * eta
    dxdeta =     b * xsi + c

  else if ( npe == 6 ) then

    call refmap_q6 ( xx, a, b, c, d, e, f )

    xval = a * xsi**2 + b * xsi * eta + c * eta**2 + d * xsi + e * eta + f
    dxdxsi =  2.0E+00 * a * xsi +       b * eta + d
    dxdeta =        b * xsi + 2.0E+00 * c * eta + e

  else

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TRANSFORM - Fatal error!'
    write ( *, '(a,i6)' ) '  Illegal value of NPE = ', npe
    stop

  end if
!
!  Compute Y(XSI,ETA).
!
  do i = 1, npe
    xx(i) = yc(node(i,ielem))
  end do

  if ( npe == 3 ) then

    call refmap_l3 ( xx, a, b, c )

    yval = a * xsi + b * eta + c
    dydxsi = a
    dydeta = b

  else if ( npe == 4 ) then

    call refmap_l4 ( xx, a, b, c, d )

    yval = a * xsi + b * xsi * eta + c * eta + d
    dydxsi = a + b * eta
    dydeta =     b * xsi + c

  else if ( npe == 6 ) then

    call refmap_q6 ( xx, a, b, c, d, e, f )

    yval = a * xsi**2 + b * xsi * eta + c * eta**2 + d * xsi + e * eta + f
    dydxsi =  2.0E+00 * a * xsi +       b * eta + d
    dydeta =        b * xsi + 2.0E+00 * c * eta + e

  else

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TRANSFORM - Fatal error!'
    write ( *, '(a,i6)' ) '  Illegal value of NPE = ', npe
    stop

  end if
!
!  Compute the determinant of the jacobian matrix:
!
!    J: (XSI,ETA) --> (X,Y)
!
  det = dxdxsi * dydeta - dxdeta * dydxsi
!
!  Watch out for a zero determinant.
!
  if ( det == 0.0E+00 ) then

    ierror = 1
    dxsidx = 0.0E+00
    dxsidy = 0.0E+00
    detadx = 0.0E+00
    detady = 0.0E+00

    if ( debug ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRANSFORM - Warning!'
      write ( *, '(a)' ) '  The jacobian is singular.'
      write ( *, '(a)' ) '  J: (XSI,ETA) --> (X,Y)'
      write ( *, '(2g14.6)' ) dxdxsi, dxdeta
      write ( *, '(2g14.6)' ) dydxsi, dydeta
      write ( *, '(a,i6)' ) '  This occurred for element number ', ielem
      write ( *, * ) '  Local coordinates:  ', xsi, eta
      write ( *, * ) '  Global coordinates: ', xval, yval
      write ( *, '(a)' ) '  Element nodes at:'
      do i = 1, npe
        write ( *, * ) xc(node(i,ielem)), yc(node(i,ielem))
      end do
      write ( *, '(a)' ) '  Transform coefficients are:'
      write ( *, '(a,g14.6)' ) '  A = ', a
      write ( *, * ) '  B = ', b
      write ( *, * ) '  C = ', c
      write ( *, * ) '  D = ', d
      write ( *, * ) '  E = ', e
      write ( *, * ) '  F = ', f
    end if

    return

  end if
!
!  Compute
!
!    d ETA/d X, d ETA/d Y, d XSI/d X, d XSI/d Y
!
!  by inverting the jacobian matrix
!
!    J: (XSI,ETA) --> (X,Y)
!
!  to get the jacobian matrix
!
!    J: (X,Y) --> (XSI,ETA).
!
!  This uses the simple fact that the inverse of
!
!    (a b)
!    (c d)
!
!  is
!
!    1/(ad-bc) * ( d -b)
!                (-c  a)
!
  dxsidx =   dydeta / det
  dxsidy = - dxdeta / det

  detadx = - dydxsi / det
  detady =   dxdxsi / det

  return
end
subroutine tric_l3 ( c_contour, i1, i2, i3, maxcontour, ncontour, np, s, &
  s_contour, xc, yc )
!
!*******************************************************************************
!
!! TRIC_L3 colors contour regions in a linear triangle.
!
!
!  Discussion:
!
!    That portion of the triangle which is greater than contour value
!    value SCON(I) but less than SCON(I+1) is to be filled in with
!    color I.
!
!  Modified:
!
!    22 July 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I1, I2, I3, the numbers of three nodes that
!    form the triangle under consideration.
!
!    Input, integer ( kind = 4 ) NCONTOUR, the number of color contours to use.
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!    Input, real S(NP).
!    S is a scalar quantity associated with the nodes.
!    This routine only looks at the values associated with
!    corner element nodes.
!
!    Input, real XC(NP), the X coordinates of the nodes.
!
!    Input, real YC(NP), the Y coordinates of the nodes.
!
  implicit none
!
  integer ( kind = 4 ) maxcontour
  integer ( kind = 4 ) np
!
  integer ( kind = 4 ) c_contour(maxcontour)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) i3
  integer ( kind = 4 ) ih
  integer ( kind = 4 ) il
  integer ( kind = 4 ) im
  integer ( kind = 4 ) jcolor
  integer ( kind = 4 ) ncontour
  integer ( kind = 4 ) npts
  real px
  real pxold
  real py
  real pyold
  real qx
  real qxold
  real qy
  real qyold
  real s(np)
  real s_contour(maxcontour)
  real s1
  real s2
  real s3
  real sc1
  real sc2
  real sh
  real sl
  real sm
  real xc(np)
  real xh
  real xl
  real xm
  real xpoly(5)
  real yc(np)
  real yh
  real yl
  real ym
  real ypoly(5)
!
  s1 = s(i1)
  s2 = s(i2)
  s3 = s(i3)

  if ( s1 <= s2 .and. s2 <= s3 ) then
    il = i1
    im = i2
    ih = i3
  else if ( s1 <= s3 .and. s3 <= s2 ) then
    il = i1
    im = i3
    ih = i2
  else if ( s2 <= s1 .and. s1 <= s3 ) then
    il = i2
    im = i1
    ih = i3
  else if ( s2 <= s3 .and. s3 <= s1 ) then
    il = i2
    im = i3
    ih = i1
  else if ( s3 <= s1 .and. s1 <= s2 ) then
    il = i3
    im = i1
    ih = i2
  else if ( s3 <= s2 .and. s2 <= s1 ) then
    il = i3
    im = i2
    ih = i1
  end if

  sl = s(il)
  sm = s(im)
  sh = s(ih)

  xl = xc(il)
  xm = xc(im)
  xh = xc(ih)

  yl = yc(il)
  ym = yc(im)
  yh = yc(ih)

  do i = 1, ncontour - 1

    sc1 = s_contour(i)

    sc2 = s_contour(i+1)
!
!  Check that some data in the triangle lies in the range [SC1,SC2).
!
    if ( max ( sl, sc1 ) <= min ( sh, sc2 ) ) then

      jcolor = c_contour(i)

      call filclr ( jcolor )
!
!  The entire triangle might lie in the contour.
!
      if ( sc1 <= sl .and. sh < sc2 ) then

        npts = 3
        xpoly(1) = xl
        ypoly(1) = yl
        xpoly(2) = xm
        ypoly(2) = ym
        xpoly(3) = xh
        ypoly(3) = yh

        call plygon ( npts, xpoly, ypoly )
!
!  Find (PXOLD,PYOLD) and (QXOLD,QYOLD), where the line S = SC1 crosses
!  the triangle.
!
      else

        call cross ( px, py, qx, qy, sl, sm, sh, sc1, xl, xm, &
          xh, yl, ym, yh)

        pxold = px
        pyold = py
        qxold = qx
        qyold = qy
!
!  Find (PX,PY) and (QX,QY), where the line S = SC2 crosses the triangle.
!
        call cross ( px, py, qx, qy, sl, sm, sh, sc2, xl, xm, &
          xh, yl, ym, yh)
!
!  Now draw the polygon formed by these four points, plus possibly
!  the point (XM,YM).
!
        npts = 4
        xpoly(1) = pxold
        ypoly(1) = pyold
        xpoly(2) = qxold
        ypoly(2) = qyold
        xpoly(3) = qx
        ypoly(3) = qy
        xpoly(4) = px
        ypoly(4) = py

        if ( sc1 <= sm .and. sm <= sc2 ) then
          npts = 5
          xpoly(5) = xm
          ypoly(5) = ym
        end if

        call plygon ( npts, xpoly, ypoly )

      end if

    end if

  end do

  return
end
subroutine tric_q6 ( c_contour, eta1, eta2, eta3, ielem, jcmax, &
  jcmin, maxcontour, maxnpe, ncontour, nelem, node, np, npe, &
  s_contour, s1, s2, s3, xc, xsi1, xsi2, xsi3, yc )
!
!*******************************************************************************
!
!! TRIC_Q6 colors contour regions in an isoparametric quadratic triangle.
!
!
!  Modified:
!
!    02 October 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ETA1, ETA2, ETA3, the ETA coordinates of the nodes.
!
!    Input, integer ( kind = 4 ) IELEM, the element within which we are working.
!
!    Input, integer ( kind = 4 ) JCMAX, JCMIN, the maximum and minimum color
!    indices to use for contours.
!
!    Input, integer ( kind = 4 ) MAXNPE, the maximum number of nodes per element.
!
!    Input, integer ( kind = 4 ) NCONTOUR, the number of color contours to use.
!
!    Input, integer ( kind = 4 ) NELEM, the number of elements.
!
!    Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), the global node numbers in each element.
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!    Input, integer ( kind = 4 ) NPE, the number of nodes per element.
!
!    Input, real S1, S2, S3, the values of the quantity S at the
!    three nodes.
!
!    Input, real XC(NP), the X coordinates of the nodes.
!
!    Input, real XSI1, XSI2, XSI3, the XSI coordinates of the
!    three nodes.
!
!    Input, real YC(NP), the Y coordinates of the nodes.
!
  implicit none
!
  integer ( kind = 4 ) maxcontour
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) np
!
  integer ( kind = 4 ) c_contour(maxcontour)
  real eta1
  real eta2
  real eta3
  real etagon(5)
  real etah
  real etal
  real etam
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ielem
  integer ( kind = 4 ) jcmax
  integer ( kind = 4 ) jcmin
  integer ( kind = 4 ) jcolor
  integer ( kind = 4 ) ncontour
  integer ( kind = 4 ) node(maxnpe,nelem)
  integer ( kind = 4 ) npe
  integer ( kind = 4 ) npts
  real px
  real pxold
  real py
  real pyold
  real qx
  real qxold
  real qy
  real qyold
  real s_contour(maxcontour)
  real s1
  real s2
  real s3
  real sc1
  real sc2
  real sh
  real sl
  real sm
  real xc(np)
  real xsi1
  real xsi2
  real xsi3
  real xsigon(5)
  real xsih
  real xsil
  real xsim
  real yc(np)
!
!  Assign L, M, and H to the low, middle and high values of S.
!
  if ( s1 <= s2 .and. s2 <= s3 ) then
    sl = s1
    sm = s2
    sh = s3
    etal = eta1
    etam = eta2
    etah = eta3
    xsil = xsi1
    xsim = xsi2
    xsih = xsi3
  else if ( s1 <= s3 .and. s3 <= s2 ) then
    sl = s1
    sm = s3
    sh = s2
    etal = eta1
    etam = eta3
    etah = eta2
    xsil = xsi1
    xsim = xsi3
    xsih = xsi2
  else if ( s2 <= s1 .and. s1 <= s3 ) then
    sl = s2
    sm = s1
    sh = s3
    etal = eta2
    etam = eta1
    etah = eta3
    xsil = xsi2
    xsim = xsi1
    xsih = xsi3
  else if ( s2 <= s3 .and. s3 <= s1 ) then
    sl = s2
    sm = s3
    sh = s1
    etal = eta2
    etam = eta3
    etah = eta1
    xsil = xsi2
    xsim = xsi3
    xsih = xsi1
  else if ( s3 <= s1 .and. s1 <= s2 ) then
    sl = s3
    sm = s1
    sh = s2
    etal = eta3
    etam = eta1
    etah = eta2
    xsil = xsi3
    xsim = xsi1
    xsih = xsi2
  else if ( s3 <= s2 .and. s2 <= s1 ) then
    sl = s3
    sm = s2
    sh = s1
    etal = eta3
    etam = eta2
    etah = eta1
    xsil = xsi3
    xsim = xsi2
    xsih = xsi1
  end if

  do i = 1, ncontour-1

    sc1 = s_contour(i)

    sc2 = s_contour(i+1)
!
!  Check that some data in the triangle lies in the range [SC1,SC2).
!
    if ( max ( sc1, sl ) <= min ( sc2, sh ) ) then

!
!  Set the correct color.
!
      jcolor = int ( real ( ( ncontour - i ) * jcmin + i * jcmax ) &
        / real ( ncontour ) )

      call filclr ( jcolor )
!
!  Take care of possibility that entire triangle formed by the
!  three points lies within the color contour.
!
      if ( sc1 <= sl .and. sh < sc2 ) then

        npts = 3
        xsigon(1) = xsil
        etagon(1) = etal
        xsigon(2) = xsim
        etagon(2) = etam
        xsigon(3) = xsih
        etagon(3) = etah

        call iso_poly_q6 ( etagon, ielem, maxnpe, nelem, node, &
          np, npe, npts, xc, xsigon, yc )
!
!  Find (PXOLD,PYOLD) and (QXOLD,QYOLD), where the line S = SC1 crosses
!  the triangle.
!
      else

        call cross ( px, py, qx, qy, sl, sm, sh, sc1, xsil, xsim, &
          xsih, etal, etam, etah )

        pxold = px
        pyold = py
        qxold = qx
        qyold = qy
!
!  Find (PX,PY) and (QX,QY), where the line S = SC2 crosses the triangle.
!
        call cross ( px, py, qx, qy, sl, sm, sh, sc2, xsil, &
          xsim, xsih, etal, etam, etah )
!
!  Now draw the polygon formed by these four points, plus possibly
!  the point (XM,YM).
!
        npts = 4
        xsigon(1) = pxold
        etagon(1) = pyold
        xsigon(2) = qxold
        etagon(2) = qyold
        xsigon(3) = qx
        etagon(3) = qy
        xsigon(4) = px
        etagon(4) = py

        if ( sc1 <= sm .and. sm <= sc2 ) then
          npts = 5
          xsigon(5) = xsim
          etagon(5) = etam
        end if

        call iso_poly_q6 ( etagon, ielem, maxnpe, nelem, node, np, &
          npe, npts, xc, xsigon, yc )

      end if

    end if

  end do

  return
end
subroutine ueval ( eta, ielem, maxnpe, nelem, node, np, npe, rho, rhoval, u, &
  uval, v, vval, xsi )
!
!*******************************************************************************
!
!! UEVAL evaluates velocity and density in any element.
!
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
!    Input, real ETA, the second local coordinate of the point.
!
!    Input, integer ( kind = 4 ) IELEM, the element in which (XSI,ETA) lies.
!
!    Input, integer ( kind = 4 ) MAXNPE, the maximum number of nodes per element.
!
!    Input, integer ( kind = 4 ) NELEM, the number of elements.
!
!    Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), the global node numbers in each element.
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!    Input, integer ( kind = 4 ) NPE, the number of nodes per element.
!
!    Input, real U(NP), the horizontal velocity at each node.
!
!    Output, real UVAL, the horizontal velocity at (XSI,ETA).
!
!    Input, real V(NP), the vertical velocity at each node.
!
!    Output, real VVAL, the vertical velocity at (XSI,ETA).
!
!    Input, real XSI, the first local coordinate of the point.
!
  implicit none
!
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) np
!
  real dwdeta
  real dwdxsi
  real eta
  integer ( kind = 4 ) ielem
  integer ( kind = 4 ) iq
  integer ( kind = 4 ) jq
  integer ( kind = 4 ) node(maxnpe,nelem)
  integer ( kind = 4 ) npe
  real rho(np)
  real rhoval
  real u(np)
  real uval
  real v(np)
  real vval
  real w
  real xsi
!
!  Start the variables at 0.
!
  rhoval = 0.0E+00
  uval = 0.0E+00
  vval = 0.0E+00
!
!  Sum up contributions from each node.
!
  do iq = 1, npe

    if ( npe == 3 ) then
      call refbf_l3 ( w, dwdeta, dwdxsi, eta, iq, xsi )
    else if ( npe == 4 ) then
      call refbf_l4 ( w, dwdeta, dwdxsi, eta, iq, xsi )
    else if ( npe == 6 ) then
      call refbf_q6 ( w, dwdeta, dwdxsi, eta, iq, xsi )
    end if

    jq = node(iq,ielem)

    rhoval = rhoval + w * rho(jq)
    uval = uval + w * u(jq)
    vval = vval + w * v(jq)

  end do

  return
end
subroutine vec_plot ( arrow, ido, jcmax, jcmin, ncontour, nflag, np, vecscl, &
  u, v, vmax, vmin, xc, yc )
!
!*******************************************************************************
!
!! VEC_PLOT draws a vector field.
!
!
!  Discussion:
!
!    An arrow pointing in the direction (U(I), V(I)) is drawn at the
!    point (XC(I),YC(I)).  The arrow's length is scaled by VECSCL.
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
!    Input, character ( len = * ) ARROW, specifies how the arrow will be drawn.
!    LINE, line drawing;
!    SOLID, polygonal shape, filled in;
!    HOLLOW, polygonal shape, outlined.
!
!    Input, integer ( kind = 4 ) JCMAX, JCMIN, the highest and lowest color indices
!    that will be used.
!
!    Input, logical NFLAG(NP), flags nodes which are active,
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!    Input, real VECSCL.
!    A scale factor for velocity vectors.  This starts out at
!    1.0.
!
!    Input, real U(NP), the horizontal velocity at each node.
!
!    Input, real V(NP), the vertical velocity at each node I.
!
!    Input, real XC(NP), the X coordinates of the nodes.
!
!    Input, real YC(NP), the Y coordinates of the nodes.
!
  implicit none
!
  integer ( kind = 4 ) np
!
  character ( len = 10 ) arrow
  integer ( kind = 4 ) i
  integer ( kind = 4 ) icon
  integer ( kind = 4 ) ido
  integer ( kind = 4 ) jcmax
  integer ( kind = 4 ) jcmin
  integer ( kind = 4 ) jcolor
  integer ( kind = 4 ) ncontour
  logical nflag(np)
  logical s_eqi
  real u(np)
  real v(np)
  real vecscl
  real vmag
  real vmax
  real vmin
  real xc(np)
  real xtip
  real yc(np)
  real ytip
!
!  Look at each node.
!
  do i = 1, np
!
!  Don't proceed unless the node is visible.
!
    if ( nflag(i) ) then
!
!  If IDO = 1, choose a color for the vector based on the relative
!  velocity magnitude.
!
      if ( ido == 1 ) then
        vmag = sqrt ( u(i)**2 + v(i)**2 )
        icon = 1 + int ( real ( ncontour ) * ( vmag - vmin ) / ( vmax - vmin ) )
        icon = max ( icon, 1 )
        icon = min ( icon, ncontour )
        jcolor = int ( ( ( ncontour - icon ) * jcmin + icon * jcmax ) / &
          real ( ncontour ) )
        call linclr ( jcolor )
      end if
!
!  Locate the tip of the arrow.
!
      xtip = xc(i) + vecscl * u(i)
      ytip = yc(i) + vecscl * v(i)

      if ( s_eqi ( arrow, 'LINE' ) ) then

        call arrow_line ( xc(i), yc(i), xtip, ytip )

      else if ( s_eqi ( arrow, 'SOLID' ) .or. s_eqi ( arrow, 'HOLLOW' ) ) then

        call arrow_poly ( arrow, xc(i), yc(i), xtip, ytip )

      else

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'VEC_PLOT - Fatal error!'
        write ( *, '(a)' ) '  Illegal option ARROW = ' // trim ( arrow )
        stop

      end if

    end if

  end do

  return
end
subroutine velocity_max ( nflag, np, u, v, vtmax, vtmin, vvmax, vvmin )
!
!*******************************************************************************
!
!! VELOCITY_MAX computes the maximum magnitude of visible velocities.
!
!
!  Discussion:
!
!    No arrow will be drawn if the node associated with the vector
!    quantity has been rendered "invisible" via NFLAG.
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
!    Input, logical NFLAG(NP), flags nodes which are active.
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!    Input, real U(MAXNP), the horizontal velocity at each node.
!
!    Input, real V(MAXNP), the vertical velocity at each node.
!
!    Output, real VTMAX, VTMIN, the largest and smallest velocity norms.
!
!    Output, real VVMAX, VVMIN, the largest and smallest Euclidean norms of
!    the VISIBLE velocity vectors.
!
  implicit none
!
  integer ( kind = 4 ) np
!
  integer ( kind = 4 ) i
  logical lfirst
  logical nflag(np)
  real u(np)
  real v(np)
  real vmag
  real vtmax
  real vtmin
  real vvmax
  real vvmin
!
  vtmax = 0.0E+00
  vtmin = 0.0E+00
  vvmax = 0.0E+00
  vvmin = 0.0E+00
  lfirst = .true.

  do i = 1, np

    vmag = sqrt ( u(i)**2 + v(i)**2 )

    if ( i == 1 ) then
      vtmax = vmag
      vtmin = vmag
    else
      vtmax = max ( vtmax, vmag )
      vtmin = min ( vtmin, vmag )
    end if
!
!  Figure out whether the node is to be skipped because of
!  the value of NFLAG.
!
    if ( nflag(i) ) then

      if ( lfirst ) then
        vvmax = vmag
        vvmin = vmag
        lfirst = .false.
      else
        vvmax = max ( vvmax, vmag )
        vvmin = min ( vvmin, vmag )
      end if

    end if

  end do

  return
end
subroutine vizelm ( eflagu, nelem )
!
!*******************************************************************************
!
!! VIZELM allows the user to specify which elements are visible.
!
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
!    Input/output, logical EFLAGU(NELEM).
!    EFLAGU is used to "flag" which elements the user wants to see.
!    If EFLAGU(I) is TRUE, then element I should be displayed.
!
!    Input, integer ( kind = 4 ) NELEM, the number of elements.
!
  implicit none
!
  integer ( kind = 4 ) nelem
!
  logical eflagu(nelem)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) itemp
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'VIZELM - Set visibility of elements.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i6)' ) '  First, we hide elements 1 through NELEM = ',  nelem
  write ( *, '(a)' ) ' '

  eflagu(1:nelem) = .false.

10    continue

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Enter a range of elements ELO, EHI,'
  write ( *, '(a)' ) '  to be added to the visible list, '
  write ( *, '(a)' ) '  or 0, 0 to stop.'

  read ( *, *, err = 20, end = 20 ) ilo, ihi

  if ( ilo == 0 .and. ihi == 0 ) then
    go to 30
  end if

  if ( ilo > ihi ) then
    itemp = ihi
    ihi = ilo
    ilo = itemp
  end if

  if ( ilo <= 0 ) then
    go to 30
  end if

  ilo = min ( ilo, nelem )

  if ( ihi <= 0 ) then
    go to 30
  end if

  ihi = min ( ihi, nelem )

  eflagu(ilo:ihi) = .true.

  go to 10

20    continue

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'VIZELM - Serious error!'
  write ( *, '(a)' ) '  Error reading user input!'
  return

30    continue

  itemp = 0
  do i = 1, nelem
    if ( eflagu(i) ) then
      itemp = itemp + 1
    end if
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'VIZELM'
  write ( *, '(a,i6)' ) '  Number of visible elements is ', itemp

  return
end
subroutine viznd ( np, nflag0, xc, yc )
!
!*******************************************************************************
!
!! VIZND sets the visibility of nodes by minimum distance.
!
!
!  Discussion:
!
!    The user sets a minimum distance.  The routine picks the nodes
!    to display based on the requirement that they be at least this
!    distance apart.
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
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!    Input/output, logical NFLAG0(NP).
!    NFLAG0(I) is .TRUE. if and only if the user is willing to
!    have node I displayed.
!
!    Input, real XC(NP), the X coordinates of the nodes.
!
!    Input, real YC(NP), the Y coordinates of the nodes.
!
  implicit none
!
  integer ( kind = 4 ) np
!
  real dismin
  real disnod
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) noff
  real temp
  logical nflag0(np)
  real xc(np)
  real yc(np)
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'VIZND:'
  write ( *, '(a)' ) '  Give a minimum separation for visible nodes.'

  read ( *, * ) dismin

  temp = dismin**2

  noff = 0
  do i = 1, np

    nflag0(i) = .true.

    do j = 1, i-1

      if ( nflag0(j) ) then
        disnod = ( xc(i) - xc(j) )**2 + ( yc(i) - yc(j) )**2
        if ( disnod < temp ) then
          nflag0(i) = .false.
          noff = noff + 1
          exit
        end if
      end if

    end do

  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'VIZND - Note:'
  write ( *, '(a,i6)' ) '  Total number of nodes is ', np
  write ( *, '(a,i6)' ) '  you have turned off ', noff
  write ( *, '(a,i6)' ) '  leaving ', np-noff

  return
end
subroutine viznod ( np, nflag0 )
!
!*******************************************************************************
!
!! VIZNOD sets the visibility of nodes by index.
!
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
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!    Input/output, logical NFLAG0(NP).
!    NFLAG0(I) is .TRUE. if and only if the user is willing to
!    have node I displayed.
!
  implicit none
!
  integer ( kind = 4 ) np
!
  integer ( kind = 4 ) i
  integer ( kind = 4 ) itemp
  integer ( kind = 4 ) nhi
  integer ( kind = 4 ) nlo
  integer ( kind = 4 ) non
  logical nflag0(np)
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'VIZNOD - Note:'
  write ( *, '(a)' ) '  All nodes will be invisible by default.'
  write ( *, '(a)' ) '  Now specify ranges of node numbers that should'
  write ( *, '(a)' ) '  be visible.  To finish, give a range of 0,0.'
  write ( *, '(a)' ) ' '

  nflag0(1:np) = .false.

  do

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Enter low node, high node:'
    read ( *, * ) nlo, nhi

    if ( nlo > nhi ) then
      itemp = nlo
      nlo = nhi
      nhi = itemp
    end if

    if ( nlo <= 0 .and. nhi <= 0 ) then
      exit
    end if

    if ( nlo > np ) then
      write ( *, '(a,i6)' ) '  Your low node was greater than ', np
    else if ( nhi > np ) then
      write ( *, '(a,i6)' ) '  Your high node was greater than ', np
    else
      nflag0(nlo:nhi) = .true.
    end if

  end do
!
!  Count up the nodes that are on.
!
  non = 0
  do i = 1, np
    if ( nflag0(i) ) then
      non = non + 1
    end if
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a,i6)' ) '  Total number of nodes = ', np
  write ( *, '(a,i6)' ) '  you have turned on ', non
  write ( *, '(a,i6)' ) '  turning off ', np-non

  return
end
subroutine word_count ( s, nword )
!
!*******************************************************************************
!
!! WORD_COUNT counts the number of "words" in a string.
!
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
!    Input, character ( len = * ) S, the string to be examined.
!
!    Output, integer ( kind = 4 ) NWORD, the number of "words" in the string.
!    Words are presumed to be separated by one or more blanks.
!
  implicit none
!
  logical blank
  integer ( kind = 4 ) i
  integer ( kind = 4 ) lens
  integer ( kind = 4 ) nword
  character ( len = * ) s
!
  nword = 0
  lens = len ( s )

  if ( lens <= 0 ) then
    return
  end if

  blank = .true.

  do i = 1, lens

    if ( s(i:i) == ' ' ) then
      blank = .true.
    else if ( blank ) then
      nword = nword + 1
      blank = .false.
    end if

  end do

  return
end
subroutine word_next_read ( s, word, done )
!
!*******************************************************************************
!
!! WORD_NEXT_READ "reads" words from a string, one at a time.
!
!
!  Special cases:
!
!    The following characters are considered to be a single word,
!    whether surrounded by spaces or not:
!
!      " ( ) { } [ ]
!
!    Also, if there is a trailing comma on the word, it is stripped off.
!    This is to facilitate the reading of lists.
!
!  Modified:
!
!    23 May 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, a string, presumably containing words
!    separated by spaces.
!
!    Output, character ( len = * ) WORD.
!
!    If DONE is FALSE, then WORD contains the "next" word read.
!    If DONE is TRUE, then WORD is blank, because there was no more to read.
!
!    Input/output, logical DONE.
!
!    On input with a fresh string, set DONE to TRUE.
!
!    On output, the routine sets DONE:
!      FALSE if another word was read,
!      TRUE if no more words could be read.
!
  implicit none
!
  logical done
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ), save :: lenc = 0
  integer ( kind = 4 ), save :: next = 1
  character ( len = * ) s
  character, parameter :: TAB = char ( 9 )
  character ( len = * ) word
!
!  We "remember" LENC and NEXT from the previous call.
!
!  An input value of DONE = TRUE signals a new line of text to examine.
!
  if ( done ) then

    next = 1
    done = .false.
    lenc = len_trim ( s )

    if ( lenc <= 0 ) then
      done = .true.
      word = ' '
      return
    end if

  end if
!
!  Beginning at index NEXT, search the string for the next nonblank,
!  which signals the beginning of a word.
!
  ilo = next
!
!  ...S(NEXT:) is blank.  Return with WORD = ' ' and DONE = TRUE.
!
  do

    if ( ilo > lenc ) then
      word = ' '
      done = .true.
      next = lenc + 1
      return
    end if
!
!  If the current character is blank, skip to the next one.
!
    if ( s(ilo:ilo) /= ' ' .and. s(ilo:ilo) /= TAB ) then
      exit
    end if

    ilo = ilo + 1

  end do
!
!  ILO is the index of the next nonblank character in the string.
!
!  If this initial nonblank is a special character,
!  then that's the whole word as far as we're concerned,
!  so return immediately.
!
  if ( s(ilo:ilo) == '"' .or. &
       s(ilo:ilo) == '(' .or. &
       s(ilo:ilo) == ')' .or. &
       s(ilo:ilo) == '{' .or. &
       s(ilo:ilo) == '}' .or. &
       s(ilo:ilo) == '[' .or. &
       s(ilo:ilo) == ']' ) then

    word = s(ilo:ilo)
    next = ilo + 1
    return

  end if
!
!  Now search for the last contiguous character that is not a
!  blank, TAB, or special character.
!
  next = ilo + 1

  do while ( next <= lenc )

    if ( s(next:next) == ' ' ) then
      exit
    else if ( s(next:next) == TAB ) then
      exit
    else if ( s(next:next) == '"' ) then
      exit
    else if ( s(next:next) == '(' ) then
      exit
    else if ( s(next:next) == ')' ) then
      exit
    else if ( s(next:next) == '{' ) then
      exit
    else if ( s(next:next) == '}' ) then
      exit
    else if ( s(next:next) == '[' ) then
      exit
    else if ( s(next:next) == ']' ) then
      exit
    end if

    next = next + 1

  end do
!
!  Ignore a trailing comma.
!
  if ( s(next-1:next-1) == ',' ) then
    word = s(ilo:next-2)
  else
    word = s(ilo:next-1)
  end if

  return
end
subroutine write_element ( element_file_name, maxelm, maxnpe, nelem, node, npe )
!
!*******************************************************************************
!
!! WRITE_ELEMENT writes an element file.
!
!
!  Modified:
!
!    03 April 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) ELEMENT_FILE_NAME, the name of the
!    element file.
!
!    Input, integer ( kind = 4 ) MAXELM, the maximum number of elements.
!
!    Input, integer ( kind = 4 ) MAXNPE, the maximum number of nodes per element.
!
!    Input, integer ( kind = 4 ) NELEM, the number of elements.
!
!    Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), the global node numbers in 
!    each element.
!
!    Input, integer ( kind = 4 ) NPE, the number of nodes per element.
!
  implicit none
!
  integer ( kind = 4 ) maxelm
  integer ( kind = 4 ) maxnpe
!
  character ( len = * ) element_file_name
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ielem
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) ios
  integer ( kind = 4 ) j
  integer ( kind = 4 ) maxnp
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) node(maxnpe,maxelm)
  integer ( kind = 4 ) np
  integer ( kind = 4 ) npe
  integer ( kind = 4 ) output_unit
!
  ierror = 0

  call get_unit ( output_unit )
!
!  Open the data file.
!
  open ( unit = output_unit, file = element_file_name, status = 'replace', &
    iostat = ios )

  if ( ios /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'WRITE_ELEMENT - Fatal error!'
    write ( *, '(a)' ) '  Could not open the element file.'
    return
  end if
!
!  Header.
!
  write ( output_unit, '(a)'    ) '#  ' // trim ( element_file_name )
  write ( output_unit, '(a)'    ) '#  created by routine WRITE_ELEMENT,'
  write ( output_unit, '(a)'    ) '#  program DISPLAY5.'
  write ( output_unit, '(a)'    ) '#'
  write ( output_unit, '(a)'    ) '#  Each line lists the nodes for an element.'
  write ( output_unit, '(a)'    ) '#'
  write ( output_unit, '(a,i6)' ) '#  Number of nodes, NP = ', np
  write ( output_unit, '(a)'    ) '#'
  write ( output_unit, '(a,i6)' ) '#  Number of elements NELEM = ', nelem
  write ( output_unit, '(a,i6)' ) '#  Number of elements per node, NPE = ', npe
  write ( output_unit, '(a)'    ) '#'
!
!  Node numbers associated with each element.
!
  do ielem = 1, nelem
    write ( output_unit, '(6i6)' ) node(1:npe,ielem)
  end do

  close ( unit = output_unit )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'WRITE_ELEMENT:'
  write ( *, '(a)' ) '  The element data was written to a file.'

  return
end
subroutine write_node ( node_file, np, nq, v, name )
!
!*******************************************************************************
!
!! WRITE_NODE writes a node data file.
!
!
!  Discussion:
!
!    The node file contains the value of various quantities at the nodes.
!    The format is:
!
!      Line 1: NP, the number of nodes
!      Line 2: NQ, the number of values per node, (X, Y and the quantities).
!      Line 3: NAME, the names of X, Y, and the quantities
!      Lines 4 through NP+3: X, Y, and the quantities for each node.
!
!    Blanks and comment lines, which have a "#" in column 1, may occur anywhere.
!
!  Modified:
!
!    22 June 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) NODE_NAME, the node file.
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!    Input, integer ( kind = 4 ) NQ, the number of quantities (including X and Y).
!
!    Input, real V(NP,NQ), X, Y, and various quantities associated with
!    the nodes.
!
!    Input, character ( len = 10 ) NAME(NQ), names for the quantities.
!
  implicit none
!
  integer ( kind = 4 ) np
  integer ( kind = 4 ) nq
!
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ios
  character ( len = 10 ) name(nq)
  character ( len = * ) node_file
  integer ( kind = 4 ) output_unit
  real v(np,nq)
!
  call get_unit ( output_unit )

  open ( unit = output_unit, file = node_file, status = 'replace', &
    iostat = ios )

  if ( ios /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'WRITE_NODE - Fatal error!'
    write ( *, '(a)' ) '  Could not open the node data file:' &
      // trim ( node_file )
    return
  end if

  write ( output_unit, '(a)' ) '#  ' // trim ( node_file )
  write ( output_unit, '(a)' ) '#  created by routine WRITE_NODE,'
  write ( output_unit, '(a)' ) '#  program DISPLAY5.'
  write ( output_unit, '(a)' ) '#'
  write ( output_unit, '(a)' ) '#  Line 1 is number of nodes.'
  write ( output_unit, '(a)' ) '#  Line 2 is number of properties per node.'
  write ( output_unit, '(a)' ) '#  Line 3 is property names.'
  write ( output_unit, '(a)' ) '#  Subsequent lines are X, Y and properties for each node.'
  write ( output_unit, '(a)' ) '#'

  write ( output_unit, '(i6)' ) np
  write ( output_unit, '(i6)' ) nq
  write ( output_unit, '(4x,10(a10,4x))' ) name(1:nq)(1:10)

  do i = 1, np
    write ( output_unit, '(12g14.6)' ) v(i,1:nq)
  end do

  close ( unit = output_unit )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'WRITE_NODE:'
  write ( *, '(a)' ) '  The node data was written to the file: ' &
    // trim ( node_file )

  return
end
subroutine write_tecplot ( tecplot_file, maxelm, maxnpe, nelem, node, np, npe, &
  nq, v )
!
!*******************************************************************************
!
!! WRITE_TECPLOT writes a TECPLOT file.
!
!
!  Modified:
!
!    23 June 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) TECPLOT_FILE, the name of the file to write.
!
!    Input, integer ( kind = 4 ) MAXELM, the maximum number of elements.
!
!    Input, integer ( kind = 4 ) MAXNPE, the maximum number of nodes per element.
!
!    Input, integer ( kind = 4 ) NELEM, the number of elements.
!
!    Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), the global node numbers in 
!    each element.
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!    Input, integer ( kind = 4 ) NPE, the number of nodes per element.
!
!    Input, integer ( kind = 4 ) NQ, the number of node quantities.
!
!    Input, real V(NP,NQ), the node quantities.
!
  implicit none
!
  integer ( kind = 4 ) maxelm
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) np
  integer ( kind = 4 ) nq
!
  integer ( kind = 4 ) i
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) nelem2
  integer ( kind = 4 ) node(maxnpe,maxelm)
  integer ( kind = 4 ) npe
  integer ( kind = 4 ) output_unit
  character ( len = * ) tecplot_file
  real v(np,nq)
!
!  Open a fresh copy of the TECPLOT data file.
!
  call get_unit ( output_unit )

  open ( unit = output_unit, file = tecplot_file, form = 'formatted', &
    access = 'sequential', status = 'replace' )

  write ( output_unit, '(a)' ) 'Title="Finite Element Data from WRITE_TECPLOT"'
  write ( output_unit, '(a)' ) 'Variables="X","Y","P","RHO","U","V"'

  if ( npe == 3 ) then
    nelem2 = nelem
  else if ( npe == 4 ) then
    nelem2 = 2 * nelem
  else if ( npe == 6 ) then
    nelem2 = 4 * nelem
  end if

  write ( output_unit, '(a)' ) &
    'Zone N=', np, ', E=', nelem2, ', F=FEPOINT, ET=TRIANGLE'
!
!  Write out the data at each node.
!
  do i = 1, np
    write ( output_unit, '(10g15.6)' ) v(i,1:nq)
  end do
!
!  Write out the data that defines the elements.
!
  if ( npe == 3 ) then

    do i = 1, nelem
      write ( output_unit, '(3i6)' ) node(1,i), node(2,i), node(3,i)
    end do

  else if ( npe == 4 ) then

    do i = 1, nelem
      write ( output_unit, '(3i6)' ) node(1,i), node(2,i), node(3,i)
      write ( output_unit, '(3i6)' ) node(3,i), node(4,i), node(1,i)
    end do

  else if ( npe == 6 ) then

    do i = 1, nelem
      write ( output_unit, '(3i6)' ) node(1,i), node(4,i), node(6,i)
      write ( output_unit, '(3i6)' ) node(2,i), node(5,i), node(4,i)
      write ( output_unit, '(3i6)' ) node(3,i), node(6,i), node(5,i)
      write ( output_unit, '(3i6)' ) node(4,i), node(5,i), node(6,i)
    end do

  end if

  close ( unit = output_unit )

  return
end
