program main

!*****************************************************************************80
!
!! MAIN is the main program for SLAB.
!
!  Discussion:
!
!    SLAB presents a 2D contour plot of a scalar function of several variables.
!
!    SLAB was written to display 2D contours of a scalar cost functional
!    F(X), which depends on a vector argument X.  Values of F are
!    available at regularly spaced points in a plane.
!
!    You may want to be able to change the colors of various
!    graphical objects, as you can in DISPLAY.  I've started using
!    MAXOBJ, ICOLOR, OBJECT and SHOW now.
!
!    New stream function code added.  It's better than the old code,
!    but still doesn't look quite plausible.  What was I trying to do
!    exactly?
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 September 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Character ( len = 80 ) COMMAND, the command typed by the user.
!
!    CHARACTER*10 DEV, the output device chosen by the
!    user.  It may be:
!      'cgmb' for a CGM file.
!      'ps'   for a PostScript file.
!      'xws'  for an X window display.
!
!    DEV is initialized to be blank.  If, when DEV is
!    needed, it is still blank, it will be reset to 'ps'.
!
!    Real DIFX(MAXNP), DIFY(MAXNP),
!    The difference between the estimates of the gradients computed
!    in FLOW3 by sensitivities and finite differences.
!      DIFX(I)  =  DPDXSN(I) - DPDXFD(I),
!      DIFY(I)  =  DPDYSN(I) - DPDYFD(I).
!
!    Real DPDXSN(MAXNP), DPDYSN(MAXNP).
!    DPDXSN(I) is an approximation to d P/d X at node I,
!    computed in FLOW3 by discretized sensitivities.
!    Similarly, DPDYSN(I) is an approximation to d P/d Y.
!
!    Real DPDXFD(MAXNP), DQDYFD(MAXNP).
!    DPDXFD(I) is an approximation to d P/d X at node I,
!    computed in FLOW3 by finite difference sensitivities.
!    Similarly, DPDYFD(I) is an approximation to d P/d Y.
!
!    Real DPDXC(MAXNP), DQDYC(MAXNP).
!    DPDXC(I) is an approximation to d P/d X at node I,
!    computed in FLOW3 by finite difference quotients on
!    the cost function.
!    Similarly, DPDYC(I) is an approximation to d P/d Y.
!
!    Real DRDX(MAXNP), DRDY(MAXNP).
!    DRDX(I) is an approximation to d P/d X at node I,
!    computed in SLAB by finite differences.
!    Similarly, DRDY(I) is an approximation to d P/d Y.
!    These values are used partly as a check on DPDXSN,
!    DPDYSN, DPDXFD and DPDYFD, which should normally be better
!    approximations.
!
!    Integer ICMAX, ICMIN.
!    The maximum and minimum available colors.
!
!    integer ( kind = 4 ) IDATA.
!    0, the data file is not open.
!    1, the data file has been opened.
!    2, the data file has been opened, and information has been read.
!
!    Integer ISHAPE(MAXNP).
!     0, use an open circle for a marker.
!     1, use a filled circle for a marker.
!
!    Integer JCMAX, JCMIN.
!    The maximum and minimum used colors.
!
!    Logical LBAR.
!    TRUE if the color bar should be shown.
!    FALSE if the color bar should not be shown.
!
!    Logical LFRAME.
!    TRUE if a frame is to be drawn around the plot.
!
!  LGOPEN LOGICAL LGOPEN.
!         TRUE if a GRFINI command has been issued.
!         FALSE otherwise.
!
!  MARK   Integer MARK(MAXNP).
!         -1, node I is not to be marked on the graph.
!         0 or greater, the index of the color of the circle to be
!         placed at node I.
!
!  MAXCON integer MAXCON, the maximum number of contour levels
!         allowed.
!
!  MAXELM integer MAXELM, the maximum number of "elements" the
!         program can handle.  This is based on the value of
!         MAXNX.
!
!  MAXIT  integer ( kind = 4 ) MAXIT, the maximum number of iterates whose
!         coordinates can be read in by SLAB.
!
!  MAXNX  integer ( kind = 4 ) MAXNX, the maximum number of nodes along a
!         side that the program can handle.  This assumes that
!         the data is a rectangular array of values.
!
!  MAXNP  integer ( kind = 4 ) MAXNP, the maximum number of nodes that the
!         program can handle.
!
!
!  NELEM  integer ( kind = 4 ) NELEM, the number of elements associated
!         with the data.  NELEM = 2*(NX-1)*(NY-1).
!
!  NFLAG  Logical NFLAG(MAXNP), records whether each node is
!         visible or not, based on whether it lies inside of
!         XPMIN2, XPMAX2 and YPMIN2, YPMAX2.
!
!  NODE   integer ( kind = 4 ) NODE(3,NELEM), contains the three nodes that
!         make up each element.
!
!  NP     integer ( kind = 4 ) NP, the number of nodes.  NP  =  NX*NY.
!
!  NPCON  integer ( kind = 4 ) NPCON, the number of cost contour levels to be
!         drawn.
!
!  NSCON  integer ( kind = 4 ) NSCON, the number of S contour levels to be
!         drawn.
!
!  NX     integer ( kind = 4 ) NX, the number of nodes in the X direction.
!
!  NY     integer ( kind = 4 ) NY, the number of nodes in the Y direction.
!
!  P      real P(NP), the value of the scalar quantity P at each
!         node.
!
!  PCMAX  real PCMAX, the maximum value of P to use for a contour
!         level.
!
!  PCMIN  real PCMIN, the minimum value of P to use for a contour
!         level.
!
!  PCON   real PCON(NCON), the contour level values for P.
!
!  PMAX,
!  PMIN   real PMAX, PMIN, the maximum and minimum values of the
!         data P.
!
!  S      real S(NP), the value of the scalar quantity S at each
!         node.
!
!    real SCMAX, the maximum value of S to use for a contour level.
!
!    real SCMIN, the minimum value of S to use for a contour level.
!
!    real SCON(NCON), the contour level values for S.
!
!    real SMAX, SMIN, the maximum and minimum values of the data S.
!
!    CHARACTER*40 TITLE, a title to print on the plot.
!
!    CHARACTER*40 TITLE2, a second, smaller title for the plot.
!
!    Real VMIN, vectors of norm less than VMIN are treated as zero.
!
!    Real X(NP), the physical X coordinates of the nodes.
!
!    real XPMAX, XPMIN, the maximum and minimum values of the
!    X coordinates of the points at which the functional has been
!    evaluated.  XPMIN = 0, XPMAX=1.
!
!    real XPMAX2, XPMIN2, the maximum and minimum values of the
!    X coordinates of the points to be displayed.
!
!    Real XS(NP), the screen X coordinates of the nodes.
!
!    Real XSPMAX, XSPMIN, the maximum and minimum X screen
!    coordinates used to display physical points.
!
!    real Y(NP), the physical Y coordinates of the nodes.
!
!    real YPMAX, YPMIN, the maximum and minimum values of the
!    Y coordinates of the points at which the functional has been
!    evaluated.  YPMIN = 0, YPMAX=1.
!
!    real YPMAX2, YPMIN2, the maximum and minimum values of the
!    Y coordinates of the points to be displayed.
!
!    real YS(NP), the screen Y coordinates of the nodes.
!
!    real YSPMAX, YSPMIN, the maximum and minimum Y screen
!    coordinates used to display physical points.
!
  implicit none

  integer ( kind = 4 ), parameter :: maxobj = 7
  integer ( kind = 4 ), parameter :: maxit = 100
  integer ( kind = 4 ), parameter :: maxlin = 10
  integer ( kind = 4 ), parameter :: maxnx = 100
  integer ( kind = 4 ), parameter :: maxnp = maxnx*maxnx
  integer ( kind = 4 ), parameter :: maxelm = 2*(maxnx-1)*(maxnx-1)

  real bval
  character ( len = 80 ) command
  character ( len = 10 ) dev
  real difx(maxnp)
  real dify(maxnp)
  real dist(2)
  real dpdxc(maxnp)
  real dpdyc(maxnp)
  real dpdxfd(maxnp)
  real dpdyfd(maxnp)
  real dpdxsn(maxnp)
  real dpdysn(maxnp)
  real drdx(maxnp)
  real drdy(maxnp)
  character ( len = 40 ) filcol
  character ( len = 40 ) filein
  character ( len = 40 ) fileit
  character ( len = 40 ) filem
  character ( len = 40 ) filgrf
  real grace
  real gval
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) icmax
  integer ( kind = 4 ) icmin
  integer ( kind = 4 ) icolor(maxobj)
  integer ( kind = 4 ) icolr
  integer ( kind = 4 ) idata
  integer ( kind = 4 ) ie(maxelm)
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) in(maxnp)
  integer ( kind = 4 ) inode
  logical inside
  integer ( kind = 4 ) ios
  integer ( kind = 4 ) iplot
  integer ( kind = 4 ) ish
  integer ( kind = 4 ) ishape(maxnp)
  integer ( kind = 4 ) istep
  integer ( kind = 4 ) itable
  integer ( kind = 4 ) itemp
  integer ( kind = 4 ) j1
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) jcmax
  integer ( kind = 4 ) jcmin
  integer ( kind = 4 ) jtemp
  character ( len = 20 ) labelx
  character ( len = 20 ) labely
  logical laxes
  integer ( kind = 4 ) lchar
  logical lcvec
  logical ldvec
  integer ( kind = 4 ) lenc
  logical s_eqi
  logical lgopen
  integer ( kind = 4 ) line(2,maxlin)
  logical lit
  logical lnei
  logical lncvec
  logical lndvec
  logical lnpvec
  logical lnqvec
  logical lnrvec
  logical lqvec
  logical lrvec
  logical lsline
  integer ( kind = 4 ) mark(maxnp)
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) nerror
  logical nflag(maxnp)
  integer ( kind = 4 ) nit
  integer ( kind = 4 ) nline
  integer ( kind = 4 ) node(3,maxelm)
  integer ( kind = 4 ) np
  integer ( kind = 4 ) npcon
  integer ( kind = 4 ) nscon
  integer ( kind = 4 ) nval
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny
  integer ( kind = 4 ) nz
  character ( len = 25 ) object(maxobj)
  real p(maxnp)
  real pcmax
  real pcmin
  real pmax
  real pmin
  real rval
  real s(maxnp)
  real scmax
  real scmin
  logical show(maxobj)
  real smax
  real smin
  character ( len = 40 ) title
  character ( len = 40 ) title2
  real vecscl
  real vmin
  real x(maxnp)
  real x1
  real x2
  real xit(maxit)
  real xpmax
  real xpmax2
  real xpmin
  real xpmin2
  real xs(maxnp)
  real y(maxnp)
  real y1
  real y2
  real yit(maxit)
  real ypmax
  real ypmax2
  real ypmin
  real ypmin2
  real ys(maxnp)
!
  call timestamp ( )
!
!  Say hello.
!
  call hello
!
!  Initialize data.
!
  call init(dev,difx,dify,dist,dpdxc,dpdyc,dpdxfd,dpdyfd,dpdxsn,dpdysn, &
    drdx,drdy,filein,filgrf,grace,icmax,icmin,icolor,ie,in,iplot,ishape, &
    istep,itable,jcmax,jcmin,labelx,labely,laxes,lcvec,ldvec,lgopen,line, &
    lit,lncvec,lndvec,lnpvec,lnqvec,lnrvec,lsline,lqvec,lrvec,mark,maxelm, &
    maxit,maxlin,maxnp,maxobj,nelem,nerror,nflag,nit,nline,node,np,npcon, &
    nscon,nx,ny,object,p,pcmax,pcmin,pmax,pmin,s,scmax,scmin,show,smax,smin, &
    title,title2,vecscl,vmin,x,xit,xpmax,xpmax2,xpmin,xpmin2,y,yit,ypmax, &
    ypmax2,ypmin,ypmin2)
!
!  Open file to contain a record of user commands.
!
  open ( unit = 17, file = filein, status = 'replace', access = 'sequential', &
    form = 'formatted' )
!
!  Cycle, getting the next command.
!
  do
!
!  Get the next command.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'Command?  H for help'
  read ( *, '(a)' ) command
  lenc = max ( len_trim ( command ), 1 )
  write ( 17, '(a)' ) command(1:lenc)
!
!  A: advance.
!
  if ( s_eqi ( command, 'a' ) ) then

    call getdat ( dist, dpdxc, dpdyc, dpdxfd, dpdyfd, dpdxsn, dpdysn, &
      idata, istep, np, nx, ny, p )

    if ( idata == 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SLAB - Warning!'
      write ( *, '(a)' ) '  There was no more data to read!'
      cycle
    end if

    call dodat(difx,dify,dist,dpdxc,dpdyc,dpdxfd,dpdyfd,dpdxsn,dpdysn,drdx, &
      drdy,ie,in,mark,maxelm,maxnp,nelem,node,np,nx,ny,p,pcmax,pcmin,pmax, &
      pmin,s,scmax,scmin,smax,smin,x,xpmax,xpmin,y,ypmax,ypmin)
!
!  AXES: show the axes.
!
  else if ( s_eqi(command(1:2),'ax') ) then

    laxes = .not.laxes
    if ( laxes ) then
      write ( *, * ) 'The axes will be shown in the plot.'
    else
      write ( *, * ) 'The axes will NOT be shown in the plot.'
    end if
!
!  BAR: show the color bar.
!
  else if ( s_eqi(command,'bar') ) then

    show(2) = .not.show(2)
    if ( show(2) ) then
      write ( *, * ) 'The color bar will be shown.'
    else
      write ( *, * ) 'The color bar will NOT be shown.'
    end if
!
!  C: choose colors.
!
  else if ( s_eqi(command,'c') ) then

    write ( *, * ) ' '
    write ( *, * ) ' Number  Color  Name'
    write ( *, * ) ' '
    do i = 1,maxobj
      write(*,'(1x,i2,2x,i3,2x,a)')i,icolor(i),object(i)
    end do

    write ( *, * ) 'Enter an object number, and a color number.'

    read ( *, *, iostat = ios ) itemp, jtemp

    if ( ios /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SLAB - Error!'
      cycle
    end if

    write(17,*)itemp,jtemp
    if ( 1<=itemp.and.itemp<=maxobj ) then
      icolor(itemp) = jtemp
    else
      write ( *, * ) 'Your object number was out of bounds.'
    end if
!
!  CC: choose color table
!
!  For some strange reason, in order to make the color table
!  active, we have to call NEWFRM!
!
  else if ( s_eqi(command(1:3),'cc=').or.s_eqi(command(1:2),'cc') ) then

    if ( s_eqi(command(1:3),'cc=') ) then

      read ( command(4:), *, iostat = ios ) itable

      if ( ios /= 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SLAB - Error!'
        cycle
      end if

    else
      write ( *, * ) ' '
      write ( *, * ) 'Built in color tables include:'
      write ( *, * ) ' '
      write ( *, * ) '1  Gray scale, low black to high white.'
      write ( *, * ) '2  Low blue to high yellow.'
      write ( *, * ) '3  Low red, high blue, with bands between.'
      write ( *, * ) '4  Low red, orange, green, high blue.'
      write ( *, * ) '5  Low white, blue, green, yellow, high orange'
      write ( *, * ) '6  Low blue to high red.'
      write ( *, * ) '7  Linear table, 2 user colors.'
      write ( *, * ) '8  Linear table, N user colors.'
      write ( *, * ) ' '
      write ( *, * ) 'Enter a color table index between 1 and 8,'
      write ( *, * ) 'or 0 to enter a color table from a file.'

      read ( *, *, iostat = ios ) itable

      if ( ios /= 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SLAB - Error!'
        cycle
      end if

      write(17,*)itable

    end if

    if ( itable == 0 ) then

      write ( *, * ) ' '
      write ( *, * ) 'Enter name of color table file.'
      read ( *, '(a)', iostat = ios ) filcol

      if ( ios /= 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SLAB - Error!'
        cycle
      end if

      write(17,'(a)')filcol
      call getctb(icmin,icmax,filcol,ierror)
    end if

    call preplt(dev,filgrf,icmax,icmin,iplot,itable,lgopen)

    call settab(icmax,icmin,itable)

    if ( dev == 'xws' ) then
      x1 = 0.05
      x2 = 0.95
      y1 = 0.05
      y2 = 0.95
      grace = 0.05
      call color_box
      call buzz ( dev, x1, x2, y1, y2 )
    end if
!
!  COLOR
!
  else if ( s_eqi(command,'color') ) then

    write ( *, * ) 'Enter the color index between 0 and 255'
    read ( *, *, iostat = ios ) i

    if ( ios /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SLAB - Error!'
      cycle
    end if

    write(17,*)i
    write ( *, * ) 'Enter(R,G,B)'
    write ( *, * ) 'Note: (0,0,0) is black, (1,1,1) is white!'
    read(*,*, iostat = ios )rval,gval,bval

    if ( ios /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SLAB - Error!'
      cycle
    end if

    write(17,*)rval,gval,bval
    call setclr(i,bval,gval,rval)
!
!  CTAB
!
  else if ( s_eqi(command,'ctab') ) then

    call preplt(dev,filgrf,icmax,icmin,iplot,itable,lgopen)
    x1 = 0.05
    x2 = 0.95
    y1 = 0.05
    y2 = 0.95
    grace = 0.05
    call color_box
    call buzz ( dev, x1, x2, y1, y2 )
!
!  'DAT = ' Enter data file name.
!
  else if ( s_eqi(command(1:3),'dat').or.s_eqi(command(1:4),'dat=') ) then

    if ( s_eqi(command(1:4),'dat=') ) then
      filem = command(5:)
    else
      write ( *, * ) ' '
      write ( *, * ) 'Enter the input data file name.'
      read ( *, '(a)', iostat = ios ) filem
      if ( ios /= 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SLAB - Error!'
        cycle
      end if
      lenc = max ( len_trim ( filem ), 1 )
      write(17,'(a)')filem(1:lenc)
    end if

    call predat(filem,idata,istep,maxnp,np,nx,ny,nz,xpmax,xpmin,ypmax,ypmin)
    if ( idata == 0) then
      cycle
    end if

    call getdat(dist,dpdxc,dpdyc,dpdxfd,dpdyfd,dpdxsn,dpdysn,idata,istep, &
      np,nx,ny,p)

    if ( idata == 0) then
      cycle
    end if

    call dodat(difx,dify,dist,dpdxc,dpdyc,dpdxfd,dpdyfd,dpdxsn,dpdysn,drdx, &
      drdy,ie,in,mark,maxelm,maxnp,nelem,node,np,nx,ny,p,pcmax,pcmin,pmax, &
      pmin,s,scmax,scmin,smax,smin,x,xpmax,xpmin,y,ypmax,ypmin)
!
!  'DEV = ' Choose the graphics device.
!
  else if ( s_eqi(command(1:4),'dev=') .or. s_eqi(command(1:3),'dev') ) then

    if ( dev /= ' ' ) then
      write ( *, * ) ' '
      write ( *, * ) 'SLAB - Error!'
      write ( *, * ) '  You have already chosen device '// trim ( dev )
      write ( *, * ) '  You may not change your mind!'
      cycle
    end if

    if ( s_eqi(command(1:4), 'dev=') ) then
      dev = command(5:)
    else
      write ( *, * ) ' '
      write ( *, * ) 'Enter the graphics device desired.'
      write ( *, * ) ' '
      write ( *, * ) 'Options include:'
      write ( *, * ) ' '
      write ( *, * ) 'CGMB  output to a CGM binary file.'
      write ( *, * ) 'PS    output to a PostScript file.'
      write ( *, * ) 'XWS   output to an X window screen.'

      read ( *, '(a)', iostat = ios )dev

      if ( ios /= 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SLAB - Error!'
        cycle
      end if

      write(17,'(a)')dev

    end if

    if ( s_eqi(dev(1:3),'cgm') ) then
      dev = 'cgmb'
      write ( *, * ) 'Output will be to a CGM binary file "slab.cgm".'
    else if ( s_eqi(dev,'ps') ) then
      write ( *, * ) 'Output will be to a PostScript file "slab.ps".'
    else if ( s_eqi(dev,'xws') ) then
      write ( *, * ) 'Output will be to an X window screen.'
    else
      write ( *, * ) 'Your device '//dev//' was not recognized!'
      dev = ' '
    end if
!
!  ELEMENTS: show the elements
!
  else if ( s_eqi(command(1:3),'ele') ) then

    show(7) = .not.show(7)
    if ( show(7) ) then
      write ( *, * ) 'The elements WILL be shown.'
    else
      write ( *, '(a)' ) 'The elements will NOT be shown.'
    end if
!
!  F: show a frame around the plot.
!
  else if ( s_eqi(command,'frame') ) then

    show(3) = .not.show(3)
    if ( show(3) ) then
      write ( *, '(a)' ) 'A frame WILL be shown around the plot.'
    else
      write ( *, '(a)' ) 'A frame will NOT be shown around the plot.'
    end if
!
!  FILE: set the name of the graphics output file.
!
  else if ( s_eqi(command(1:4),'file') ) then

    if ( iplot > 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SLAB - Warning:'
      write ( *, '(a)' ) '  It is too late to specify a plot file name.'
    else if ( s_eqi(command(1:5),'file=') ) then
      filgrf = command(6:)
    else
      write ( *, * ) ' '
      write ( *, * ) 'Enter the plot file name.'

      read ( *, '(a)', iostat = ios ) filgrf

      if ( ios /= 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SLAB - Error!'
        cycle
      end if

      write(17,'(a)')filgrf

    end if
!
!  G: Graph
!
  else if ( s_eqi(command(1:1),'g') ) then

    call graph(dev,difx,dify,dist,dpdxc,dpdyc,dpdxfd,dpdyfd,dpdxsn,dpdysn, &
     drdx,drdy,filgrf,icmax,icmin,icolor,iplot,ishape,itable,jcmax,jcmin, &
     labelx,labely,laxes,lcvec,ldvec,lgopen,line,lit,lncvec,lndvec,lnpvec, &
     lnqvec,lnrvec,lsline,lqvec,lrvec,mark,maxit,maxlin,maxobj,nelem,nflag, &
     nit,nline,node,np,npcon,nscon,nx,ny,p,pcmax,pcmin,s,scmax,scmin,show, &
     title,title2,vecscl,x,xit,xpmax,xpmax2,xpmin,xpmin2,xs,y,yit,ypmax, &
     ypmax2,ypmin,ypmin2,ys)
!
!  H: Help
!
  else if ( s_eqi(command(1:1),'h') ) then

    call help
!
!  ICMAX = : set the maximum color index.
!
  else if ( s_eqi(command(1:6),'icmax=') ) then

    read(command(7:),*, iostat = ios ) icmax

    if ( ios /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SLAB - Error!'
      cycle
    end if

    write ( *, * ) 'Maximum color set to ',icmax

  else if ( s_eqi(command(1:5),'icmax') ) then

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'Enter maximum color index'
    read(*,*, iostat = ios ) icmax

    if ( ios /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SLAB - Error!'
      cycle
    end if

    write(17,*)icmax
    write ( *, * ) 'Maximum color set to ',icmax
!
!  ICMIN = : set the minimum color index.
!
  else if ( s_eqi(command(1:6),'icmin=') ) then

    read(command(7:),*,iostat = ios )icmin

    if ( ios /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SLAB - Error!'
      cycle
    end if

    write ( *, * ) 'Minimum color set to ',icmin

  else if ( s_eqi(command(1:5),'icmin') ) then

    write ( *, * ) ' '
    write ( *, * ) 'Enter minimum color index'
    read(*,*, iostat = ios )icmin

    if ( ios /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SLAB - Error!'
      cycle
    end if

    write(17,*)icmin
    write ( *, * ) 'Minimum color set to ',icmin
!
!  'IDAT = ' Enter iterate data file name.
!
  else if ( s_eqi(command(1:5),'idat=') ) then

    fileit = command(6:)

    call getit(fileit,maxit,nit,xit,yit)

  else if ( s_eqi(command(1:4),'idat') ) then

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'Enter the iterate data file name.'
    read(*,'(a)', iostat = ios )fileit

    if ( ios /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SLAB - Error!'
      cycle
    end if

    write(17,'(a)')fileit

    call getit(fileit,maxit,nit,xit,yit)
!
!  'IT', show the iterates.
!
  else if ( s_eqi(command(1:2),'it') ) then

    lit = .not.lit
    if ( lit ) then
      write ( *, * ) 'The iterates WILL be shown in the plot.'
    else
      write ( *, * ) 'The iterates will NOT be shown in the plot.'
    end if
!
!  JCMAX = : set the maximum color index.
!
  else if ( s_eqi(command(1:6),'jcmax=') ) then

    read(command(7:),*, iostat = ios )jcmax

    if ( ios /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SLAB - Error!'
      cycle
    end if

    write ( *, * ) 'Maximum color set to ',jcmax

  else if ( s_eqi(command(1:5),'jcmax') ) then

    write ( *, * ) ' '
    write ( *, * ) 'Enter maximum color index'
    read(*,*, iostat = ios )jcmax

    if ( ios /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SLAB - Error!'
      cycle
    end if

    write(17,*)jcmax
    write ( *, * ) 'Maximum color set to ',jcmax
!
!  JCMIN = : set the minimum color index.
!
  else if ( s_eqi(command(1:6),'jcmin=') ) then

    read(command(7:),*, iostat = ios ) jcmin

    if ( ios /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SLAB - Error!'
      cycle
    end if

    write ( *, * ) 'Minimum color set to ',jcmin

  else if ( s_eqi(command(1:5),'jcmin') ) then

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'Enter minimum color index'
    read(*,*, iostat = ios )jcmin

    if ( ios /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SLAB - Error!'
      cycle
    end if

    write(17,*)jcmin
    write ( *, * ) 'Minimum color set to ',jcmin
!
!  LABELX = : set X label
!
  else if ( s_eqi(command(1:7),'labelx=') ) then
    labelx = command(8:)
  else if ( s_eqi(command,'labelx') ) then
    write ( *, * ) ' '
    write ( *, * ) 'Enter X label'
    read(*,'(a)')labelx
    lenc = max ( len_trim ( labelx ), 1 )
    write(17,'(a)')labelx(1:lenc)
!
!  LABELY = : set Y label
!
  else if ( s_eqi(command(1:7),'labely=') ) then
    labely = command(8:)
  else if ( s_eqi(command,'labely') ) then
    write ( *, * ) ' '
    write ( *, * ) 'Enter Y label'
    read(*,'(a)')labely
    lenc = max(len_trim(labely),1)
    write(17,'(a)')labely(1:lenc)
!
!  LINE,I1,J1,I2,J2
!
  else if ( s_eqi(command(1:4),'line') ) then

    if ( s_eqi(command(1:5),'line=') ) then

      read(command(6:),*, iostat = ios ) i1,j1,i2,j2

      if ( ios /= 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SLAB - Error!'
        cycle
      end if

    else

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'Enter column and row for first node,'
      write ( *, '(a)' ) 'then second node.'
      read(*,*, iostat = ios )i1,j1,i2,j2

      if ( ios /= 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SLAB - Error!'
        cycle
      end if

      write(17,*)i1,j1,i2,j2

    end if

    if ( nline>=maxlin ) then
      write ( *, '(a)' ) 'Sorry, no more lines can be accepted.'
    else
      nline = nline+1
      nval = (i1-1)*ny+j1
      line(1,nline) = nval
      nval = (i2-1)*ny+j2
      line(2,nline) = nval
    end if
!
!  MARK = I,J,ICOLOR,ISHAPE
!  mark the node I over and J up with color ICOLOR.
!  Set up a node array, use this to switch mark option
!  to true.
!
  else if ( s_eqi(command(1:4),'mark') ) then

    if ( s_eqi(command(1:5),'mark=') ) then

      read(command(6:),*, iostat = ios )i1,j1,icolr,ish

      if ( ios /= 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SLAB - Error!'
        cycle
      end if

    else
      write ( *, * ) ' '
      write ( *, * ) 'Enter number of columns over,'
      write ( *, * ) '      number of rows up.'
      read(*,*)i1,j1,icolr,ish
      write(17,*)i1,j1,icolr,ish
    end if

    nval = (i1-1)*ny+j1
    write ( *, * ) 'The node number is ',nval
    mark(nval) = icolr
    ishape(nval) = ish
!
!  NPCON = , set number of contour levels for P.
!
  else if ( s_eqi(command(1:6),'npcon=') ) then

    call chrcti(command(7:),npcon,ierror,lchar)

  else if ( s_eqi(command(1:5),'npcon') ) then

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'How many contour levels for P should be drawn?'
    read(*,*, iostat = ios )npcon

    if ( ios /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SLAB - Error!'
      cycle
    end if

    write(17,*)npcon
!
!  NSCON = , set number of contour levels for S.
!
  else if ( s_eqi(command(1:6),'nscon=') ) then

    call chrcti(command(7:),nscon,ierror,lchar)

  else if ( s_eqi(command(1:5),'nscon') ) then

    write ( *, * ) ' '
    write ( *, * ) 'How many contour levels for S should be drawn?'
    read(*,*, iostat = ios )nscon

    if ( ios /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SLAB - Error!'
      cycle
    end if

    write(17,*)nscon
!
!  NC: Switch normalized C vector settings.
!
  else if ( s_eqi(command(1:2),'nc') ) then

    lncvec = .not.lncvec
    if ( lncvec ) then
      write ( *, * ) 'Normalized C vectors WILL be shown.'
    else
      write ( *, * ) 'Normalized C vectors will NOT be shown.'
    end if
!
!  ND: Switch normalized D vector settings.
!
  else if ( s_eqi(command(1:2),'nd') ) then

    lndvec = .not.lndvec
    if ( lndvec ) then
      write ( *, '(a)' ) 'Normalized D vectors WILL be shown.'
    else
      write ( *, '(a)' ) 'Normalized D vectors will NOT be shown.'
    end if
!
!  NP: Switch normalized discretized sensitivity vector settings.
!
  else if ( s_eqi(command(1:2),'np') ) then

    lnpvec = .not.lnpvec
    if ( lnpvec ) then
      write ( *, * ) 'Normalized discretized sensitity gradients WILL be shown.'
    else
      write ( *, * ) 'Normalized discretized sensitity gradients WILL NOT be shown.'
    end if
!
!  NQ: Switch normalized Q vector settings.
!
  else if ( s_eqi(command(1:2),'nq') ) then

    lnqvec = .not.lnqvec
    if ( lnqvec ) then
      write ( *, * ) 'Normalized Q vectors WILL be shown.'
    else
      write ( *, * ) 'Normalized Q vectors will NOT be shown.'
    end if
!
!  NR: Switch normalized R vector settings.
!
  else if ( s_eqi(command(1:2),'nr') ) then

    lnrvec = .not.lnrvec
    if ( lnrvec ) then
      write ( *, * ) 'Normalized R vectors WILL be shown.'
    else
      write ( *, * ) 'Normalized R vectors will NOT be shown.'
    end if
!
!  PC: Switch P contour color settings.
!
  else if ( s_eqi(command,'pc') ) then
    show(5) = .not.show(5)
    if ( show(5) ) then
      write ( *, '(a)' ) 'P contour colors WILL be shown.'
    else
      write ( *, '(a)' ) 'P contour colors will NOT be shown.'
    end if
!
!  PCMAX = 
!
  else if ( s_eqi(command(1:6),'pcmax=') ) then

    call chrctr(command(7:),pcmax,ierror,lchar)

  else if ( s_eqi(command(1:5),'pcmax') ) then

    write ( *, * ) 'The maximum value of P is ',pmax
    write ( *, * ) 'The maximum contour value was ',pcmax
    write ( *, * ) 'What should the maximum contour level be?'
    read(*,*, iostat = ios )pcmax

    if ( ios /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SLAB - Error!'
      cycle
    end if

    write(17,*)pcmax
!
!  PCMIN = 
!
  else if ( s_eqi(command(1:6),'pcmin=') ) then

    call chrctr(command(7:),pcmin,ierror,lchar)

  else if ( s_eqi(command(1:5),'pcmin') ) then

    write ( *, * ) 'The minimum value of P is ',pmin
    write ( *, * ) 'The minimum contour value of P was ',pcmin
    write ( *, * ) 'What should the minimum contour level be?'
    read(*,*, iostat = ios )pcmin

    if ( ios /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SLAB - Error!'
      cycle
    end if

    write(17,*)pcmin
!
!  PL: Switch P contour line settings.
!
  else if ( s_eqi(command(1:2),'pl') ) then
    show(4) = .not.show(4)
    if ( show(4) ) then
      write ( *, '(a)' ) 'P contour lines WILL be shown.'
    else
      write ( *, '(a)' ) 'P contour lines will NOT be shown.'
    end if
!
!  PRCON, print contour levels.
!
  else if ( s_eqi(command(1:5),'prcon') ) then

    call prcon(npcon,pcmax,pcmin)
!
!  PRCOST, Print X, Y and COST
!
  else if ( s_eqi(command(1:6),'prcost') ) then

    call prcost(np,nx,ny,p,x,y)
!
!  PRELEM, Print element data.
!
  else if ( s_eqi(command(1:4),'prel') ) then

    call prelm(nelem,node)
!
!  PRNODE, Print data at a node.
!
  else if ( s_eqi(command(1:6),'prnode') ) then

    write ( *, '(a)' ) ' '
    write ( *, * ) 'Enter node number between 1 and ',np
    write ( *, * ) 'or 0 for all nodes.'
    read(*,*)inode
    write(17,*)inode

    call prnode(difx,dify,dpdxc,dpdyc,dpdxfd,dpdyfd, &
      dpdxsn,dpdysn,drdx,drdy,inode,np,nx,ny,p,x,y)
!
!  PRSET, Print settings.
!
  else if ( s_eqi(command,'prset') ) then

    call prset(dev,icmax,icmin,icolor,idata,istep,lcvec,ldvec,lncvec,lndvec, &
      lnpvec,lnqvec,lnrvec,lqvec,lrvec,maxobj,npcon,object,show,vmin)
!
!  Q, quit
!
  else if ( s_eqi(command(1:1),'q') ) then

    write ( *, * ) ' '
    write ( *, * ) 'Type "y" to verify you want to quit!'

    read(*,'(a)')command

    lenc = max(len_trim ( command),1)
    write(17,'(a)')command(1:lenc)

    if ( s_eqi(command(1:1),'Y')) then
      exit
    end if
!
!  SCMAX = 
!
  else if ( s_eqi(command(1:6),'scmax=') ) then

    call chrctr(command(7:),scmax,ierror,lchar)

  else if ( s_eqi(command(1:5),'scmax') ) then

    write ( *, * ) ' '
    write ( *, * ) 'What should the maximum contour level be?'
    read(*,*, iostat = ios )scmax

    if ( ios /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SLAB - Error!'
      cycle
    end if

    write(17,*)scmax
!
!  SCMIN = 
!
  else if ( s_eqi(command(1:6),'scmin=') ) then

    call chrctr(command(7:),scmin,ierror,lchar)

  else if ( s_eqi(command(1:5),'scmin') ) then

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'What should the minimum contour level be?'
    read(*,*, iostat = ios )scmin

    if ( ios /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SLAB - Error!'
      cycle
    end if

    write(17,*)scmin
!
!  SL: Switch S contour line settings.
!
  else if ( s_eqi(command(1:2),'sl') ) then
    lsline = .not.lsline
    if ( lsline ) then
      write ( *, * ) 'S contour lines WILL be shown.'
    else
      write ( *, * ) 'S contour lines will NOT be shown.'
    end if
!
!  TITLE = : set title
!
  else if ( s_eqi(command(1:6),'title=') ) then
    title = command(7:)
  else if ( s_eqi(command,'title') ) then
    write ( *, * ) 'Enter title'
    read(*,'(a)')title
    lenc = max(len_trim(title),1)
    write(17,'(a)')title(1:lenc)
!
!  TITLE2 = : set title 2
!
  else if ( s_eqi(command(1:7),'title2=') ) then
    title2 = command(8:)
  else if ( s_eqi(command,'title2') ) then
    write ( *, * ) ' '
    write ( *, * ) 'Enter title2'
    read(*,'(a)')title2
    lenc = max(len_trim(title2),1)
    write(17,'(a)')title2(1:lenc)
!
!  VC: Switch C vector settings.
!
  else if ( s_eqi(command,'vc') ) then

    lcvec = .not.lcvec
    if ( lcvec ) then
      write ( *, '(a)' ) 'C vectors WILL be shown.'
    else
      write ( *, '(a)' ) 'C vectors will NOT be shown.'
    end if

!
!  VD: Switch D vector settings.
!
  else if ( s_eqi(command,'vd') ) then

    ldvec = .not.ldvec
    if ( ldvec ) then
      write ( *, * ) 'D vectors WILL be shown.'
    else
      write ( *, * ) 'D vectors will NOT be shown.'
    end if
!
!  VP: Switch discretized sensitivity gradient settings.
!
  else if ( s_eqi(command,'vp') ) then

    show(6) = .not.show(6)
    if ( show(6) ) then
      write ( *, * ) 'Discretized sensitivity gradients WILL be shown.'
    else
      write ( *, * ) 'Discretized sensitivity gradients will NOT be shown.'
    end if
!
!  VQ: Switch Q vector settings.
!
  else if ( s_eqi(command,'vq') ) then

    lqvec = .not.lqvec
    if ( lqvec ) then
      write ( *, '(a)' ) 'Q vectors WILL be shown.'
    else
      write ( *, '(a)' ) 'Q vectors will NOT be shown.'
    end if
!
!  VR: Switch R vector settings.
!
  else if ( s_eqi(command,'vr') ) then

    lrvec = .not.lrvec
    if ( lrvec ) then
      write ( *, * ) 'R vectors WILL be shown.'
    else
      write ( *, * ) 'R vectors will NOT be shown.'
    end if
!
!  VECSCL = , set vector scale factor.
!
  else if ( s_eqi(command(1:7),'vecscl=') ) then

    call chrctr(command(8:),vecscl,ierror,lchar)

  else if ( s_eqi(command(1:6),'vecscl') ) then

    write ( *, * ) ' '
    write ( *, * ) 'What should the vector scale be?'
    read(*,*, iostat = ios ) vecscl

    if ( ios /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SLAB - Error!'
      cycle
    end if

    write(17,*)vecscl
!
!  X: set the data window.
!
  else if ( s_eqi(command,'x') ) then

    write ( *, * ) ' '
    write ( *, * ) 'Current data ranges:'
    write ( *, * ) ' '
    write ( *, * ) 'X:',xpmin,xpmax
    write ( *, * ) 'Y:',ypmin,ypmax
    write ( *, * ) ' '
    write ( *, * ) 'Displayed data ranges:'
    write ( *, * ) ' '
    write ( *, * ) 'X:',xpmin2,xpmax2
    write ( *, * ) 'Y:',ypmin2,ypmax2
    write ( *, * ) ' '
    write ( *, * ) 'Enter min X, max X, min Y, max Y to display.'
    read(*,*, iostat = ios )xpmin2,xpmax2,ypmin2,ypmax2

    if ( ios /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SLAB - Error!'
      cycle
    end if

    write(17,*)xpmin2,xpmax2,ypmin2,ypmax2

    do i = 1,np
      if ( inside(xpmin2,x(i),xpmax2).and.inside(ypmin2,y(i),ypmax2) ) then
        nflag(i) = .true.
      else
        nflag(i) = .false.
      end if
    end do
!
!  #, a comment.
!
  else if ( command(1:1) == '#' ) then
!
!  Blank command
!
  else if ( command == ' ' ) then
    write(17,*)' '
!
!  Unrecognized command.
!
  else

    write ( *, * ) ' '
    write ( *, * ) 'SLAB - Warning.'
    write ( *, * ) '  Your command was not recognized!'
    write ( *, * ) '  You typed:' // trim ( command )
    nerror = nerror+1

  end if
!
!  If too many errors have occurred, shut down.
!
  if ( nerror > 20 ) then
    write ( *, * ) ' '
    write ( *, * ) 'SLAB - Fatal error!'
    write ( *, * ) '  Too many errors have occurred!'
    write ( *, * ) '  The number so far is ',nerror
    write ( *, * ) '  SLAB is shutting down.'
    exit
  end if

  end do

  if ( idata>0 ) then
    close(unit = 1)
  end if

  if ( lgopen ) then
    call grfcls
    lgopen = .false.
  end if

  close(unit = 17)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SLAB:'
  write ( *, '(a)' ) '  Normal end of execution.'

  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop
end
subroutine arrow ( xstart, ystart, xtip, ytip )

!*****************************************************************************80
!
!! ARROW can be used to draw an arrow at any point on a graph.
!
!  Discussion:
!
!    The arrow will stretch between two user specified points.
!
!    The "head" of the arrow may be fatter or thinner than expected
!    if the X and Y scales of the graph are not in the same
!    proportions.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Parameters:
!
!  XSTART,
!  YSTART  Input, real XSTART, YSTART, the starting point for the
!          arrow.
!
!  XTIP,
!  YTIP    Input, real XTIP, YTIP, the end point for the arrow.
!
!
!                     left
!                     |\
!                     | \
!    start ------- base  tip
!                     | /
!                     |/
!                     rite
!
  implicit none

  real alpha
  real del
  real dist
  real, parameter :: pi = 3.14159265E+00
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

  if ( xstart == xtip .and. ystart == ytip )return

  theta = 0.5E+00 *pi-atan2(2.0E+00, 1.0E+00 )
  dist = sqrt((xtip-xstart)**2+(ytip-ystart)**2)
  alpha = atan2(ytip-ystart,xtip-xstart)
  del = sqrt(5.0E+00)*dist/3.0E+00

  call movcgm(xstart,ystart)

  xbase = xstart+dist*cos(alpha)*2.0E+00/3.0E+00
  ybase = ystart+dist*sin(alpha)*2.0E+00/3.0E+00
  call drwcgm(xbase,ybase)

  xleft = xstart+del*cos(alpha-theta)
  yleft = ystart+del*sin(alpha-theta)
  call drwcgm(xleft,yleft)

  call drwcgm(xtip,ytip)

  xrite = xstart+del*cos(alpha+theta)
  yrite = ystart+del*sin(alpha+theta)
  call drwcgm(xrite,yrite)

  call drwcgm(xbase,ybase)

  return
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
!    07 March 1999
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
!    Input, real X1MIN, X1MAX, the maximum and minimum X
!    coordinates of the plot, which includes a small grace margin.
!
!    Input, real Y1MIN, Y1MAX, the maximum and minimum Y
!    coordinates of the plot, which includes a small grace margin.
!
  implicit none

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
subroutine cbar(jcmax,jcmin,npcon,smax,smin,srange,x1,x2,y1,y2)

!*****************************************************************************80
!
!! CBAR draws a color bar in the rectangle whose corners are (X1,Y1) and (X2,Y2).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Parameters:
!
!  JCMAX,
!  JCMIN  Input, integer ( kind = 4 ) JCMAX, JCMIN, the maximum and
!         minimum color indices to use in the color bar.
!
!  NCON   Input, integer ( kind = 4 ) NCON, the number of color contour
!         regions drawn, and hence, the number of colors
!         to be displayed in the color bar.
!
!  SMAX,
!  SMIN   Input, real SMAX, SMIN, the maximum and minimum
!         values of the quantity whose color contours are
!         being drawn.  These numbers will be printed along
!         with the color bar.
!
!  X1,
!  X2,
!  Y1,
!  Y2     Input, real X1, X2, Y1, Y2, specify the minimum and
!         maximum X and Y coordinates of the color bar.
!
  implicit none

  real angle
  character ( len = 14 ) chrrel
  character ( len = 14 ) chrtmp
  real cwide
  character ( len = 6 ) flush
  integer ( kind = 4 ) i
  integer ( kind = 4 ) jcmax
  integer ( kind = 4 ) jcmin
  integer ( kind = 4 ) jcolor
  integer ( kind = 4 ) lent
  integer ( kind = 4 ) npcon
  real pwide
  real smax
  real smin
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

  ycorn(1) = y1
  ycorn(2) = y1
  ycorn(3) = y2
  ycorn(4) = y2
  ycorn(5) = y1

  do i = 1,npcon-1

    xl = ((npcon-i)*x1+(i-1)*x2)/real(npcon-1)
    xr = ((npcon-1-i)*x1+i*x2)/real(npcon-1)

    xcorn(1) = xl
    xcorn(2) = xr
    xcorn(3) = xr
    xcorn(4) = xl
    xcorn(5) = xl

    jcolor = int(((npcon-i)*jcmin+i*jcmax)/real(npcon))
    call filclr(jcolor)

    call plygon(4,xcorn,ycorn)

    call plylin(5,xcorn,ycorn)

  end do
!
!  Print labels for the lowest and highest contours.
!
  cwide = 0.9E+00 * srange / 40.0E+00

  chrtmp = chrrel(smin)
  call chrdb1(chrtmp)
  lent = len_trim(chrtmp)

  angle = 0.0E+00
  x = x1
  y = y1-1.5E+00*cwide
  pwide = srange
  flush = 'left'

  call s_plot ( angle,cwide,pwide,chrtmp(1:lent),x,y,flush)

  chrtmp = chrrel(smax)
  call chrdb1(chrtmp)
  lent = len_trim(chrtmp)

  angle = 0.0E+00
  x = x2
  y = y1-1.5E+00*cwide
  pwide = srange
  flush = 'right'

  call s_plot ( angle,cwide,pwide,chrtmp(1:lent),x,y,flush)

  return
end
subroutine color_box ( )

!*****************************************************************************80
!
!! COLOR_BOX draws a 16 by 16 box of colors in the current color table.
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
!    None
!
  implicit none

  integer ( kind = 4 ), parameter :: npoly = 5

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
subroutine chrcti(string,intval,ierror,lchar)

!*****************************************************************************80
!
!! CHRCTI accepts a STRING of characters and reads an integer
!  from STRING into INTVAL.  The STRING must begin with an integer
!  but that may be followed by other information.
!  CHRCTI will read as many characters as possible until it reaches
!  the end of the STRING, or encounters a character which cannot be
!  part of the number.
!
!  Legal input is
!
!    blanks,
!    initial sign,
!    blanks,
!    integer ( kind = 4 ) part,
!    blanks,
!    final comma,
!
!  with most quantities optional.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Parameters:
!
!  STRING Input, CHARACTER*(*) STRING, the string containing the
!         data to be read.  Reading will begin at position 1 and
!         terminate at the end of the string, or when no more
!         characters can be read to form a legal integer.  Blanks,
!         commas, or other nonnumeric data will, in particular,
!         cause the conversion to halt.
!
!         Sample results:
!
!         STRING            INTVAL
!
!         '1'               1
!         '     1   '       1
!         '1A'              1
!         '12,34,56'        12
!         '  34 7'          34
!         '-1E2ABCD'        -100
!         '-1X2ABCD'        -1
!         ' 2E-1'           0
!         '23.45'           23
!
!  INTVAL Output, integer ( kind = 4 ) INTVAL, the integer read from the string.
!
!  IERROR Output, integer ( kind = 4 ) IERROR, error flag.
!         0 if no errors,
!         Value of IHAVE when error occurred otherwise.
!
!  LCHAR  Output, integer ( kind = 4 ) LCHAR, number of characters read from
!         STRING to form the number.
!
  implicit none

  character chrtmp
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) ihave
  integer ( kind = 4 ) intval
  integer ( kind = 4 ) isgn
  integer ( kind = 4 ) iterm
  integer ( kind = 4 ) itop
  integer ( kind = 4 ) lchar
  integer ( kind = 4 ) nchar
  integer ( kind = 4 ) ndig
  character null
  character ( len = * ) string

  nchar = len(string)

  ierror = 0
  intval = 0
  lchar = -1
  isgn = 1
  itop = 0
  ihave = 1
  iterm = 0
  null = char(0)

10    continue

  lchar = lchar+1
  chrtmp = string(lchar+1:lchar+1)
!
!  Blank.
!
  if ( chrtmp==' '.or. chrtmp==null ) then

    if ( ihave==2 ) then

    else if ( ihave==3 ) then
      ihave = 11
    end if
!
!  Comma.
!
  else if ( chrtmp==',' ) then

    if ( ihave/=1 ) then
      iterm = 1
      ihave = 12
      lchar = lchar+1
    end if
!
!  Minus sign.
!
  else if ( chrtmp=='-' ) then

    if ( ihave==1 ) then
      ihave = 2
      isgn = -1
    else
      iterm = 1
    end if
!
!  Plus sign.
!
  else if ( chrtmp=='+' ) then

    if ( ihave==1 ) then
      ihave = 2
    else
      iterm = 1
    end if
!
!  Digit.
!
  else if ( ihave<11.and.lge(chrtmp,'0').and.lle(chrtmp,'9') ) then

    ihave = 3
    read(chrtmp,'(i1)')ndig
    itop = 10*itop+ndig
!
!  Anything else is regarded as a terminator.
!
  else
    iterm = 1
  end if

  if ( iterm/=1.and.lchar+1<nchar)go to 10
  if ( iterm/=1.and.lchar+1==nchar)lchar = nchar
!
!  Number seems to have terminated.  Have we got a legal number?
!
  if ( ihave==1.or.ihave==2 ) then
    ierror = ihave
    write ( *, * ) 'CHRCTI - Serious error!'
    write ( *, * ) '  IERROR = ',ierror
    write ( *, * ) '  Illegal or nonnumeric input:'
    write(*,'(1x,a)')string
    return
  end if
!
!  Number seems OK.  Form it.
!
  intval = isgn*itop
  return
end
subroutine chrctr(string,rval,ierror,lchar)

!*****************************************************************************80
!
!! CHRCTR accepts a string of characters, and tries to extract a
!  real number from the initial part of the string.
!
!  CHRCTR will read as many characters as possible until it reaches
!  the end of the string, or encounters a character which cannot be
!  part of the real number.
!
!  Legal input is:
!
!     1 blanks,
!     2 '+' or '-' sign,
!     2.5 spaces
!     3 integer part,
!     4 decimal point,
!     5 fraction part,
!     6 'E' or 'e' or 'D' or 'd', exponent marker,
!     7 exponent sign,
!     8 exponent integer part,
!     9 exponent decimal point,
!    10 exponent fraction part,
!    11 blanks,
!    12 final comma,
!
!  with most quantities optional.
!
!  Example:
!
!    STRING            RVAL
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
!  Parameters:
!
!  STRING Input, CHARACTER*(*) STRING, the string containing the
!         data to be read.  Reading will begin at position 1 and
!         terminate at the end of the string, or when no more
!         characters can be read to form a legal real.  Blanks,
!         commas, or other nonnumeric data will, in particular,
!         cause the conversion to halt.
!
!  RVAL   Output, real RVAL, the real value that was read from
!         the string.
!
!  IERROR Output, integer ( kind = 4 ) IERROR, error flag.
!
!         0, no errors occurred.
!
!         1, 2, 6 or 7, the input number was garbled.  The
!         value of IERROR is the last type of input successfully
!         read.  For instance, 1 means initial blanks, 2 means
!         a plus or minus sign, and so on.
!
!  LCHAR  Output, integer ( kind = 4 ) LCHAR, the number of characters read from
!         STRING to form the number, including any terminating
!         characters such as a trailing comma or blanks.
!
  implicit none

  character chrtmp
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) ihave
  integer ( kind = 4 ) isgn
  integer ( kind = 4 ) iterm
  integer ( kind = 4 ) jbot
  integer ( kind = 4 ) jsgn
  integer ( kind = 4 ) jtop
  integer ( kind = 4 ) lchar
  logical s_eqi
  integer ( kind = 4 ) nchar
  integer ( kind = 4 ) ndig
  character null
  real rbot
  real rexp
  real rtop
  real rval
  character ( len = * ) string

  nchar = len(string)

  ierror = 0
  rval = 0.0
  lchar = -1
  isgn = 1
  rtop = 0
  rbot = 1
  jsgn = 1
  jtop = 0
  jbot = 1
  ihave = 1
  iterm = 0
  null = char(0)

10    continue
  lchar = lchar+1
  chrtmp = string(lchar+1:lchar+1)
!
!  Blank character.
!
  if ( chrtmp==' '.or.chrtmp==null ) then

!
!  20 November 1993
!
!  I would like to allow input like "+ 2", where there is a space
!  between the plus and the number.  So I am going to comment out
!  this line, because I think that's all that's keeping me from
!  doing this.
!
    if ( ihave==2 ) then

    else if ( (ihave==6).or.(ihave==7) ) then
      iterm = 1
    else if ( ihave>1 ) then
      ihave = 11
    end if
!
!  Comma.
!
  else if ( chrtmp==',' ) then

    if ( ihave/=1 ) then
      iterm = 1
      ihave = 12
      lchar = lchar+1
    end if
!
!  Minus sign.
!
  else if ( chrtmp=='-' ) then

    if ( ihave==1 ) then
      ihave = 2
      isgn = -1
    else if ( ihave==6 ) then
      ihave = 7
      jsgn = -1
    else
      iterm = 1
    end if
!
!  Plus sign.
!
  else if ( chrtmp=='+' ) then

    if ( ihave==1 ) then
      ihave = 2
    else if ( ihave==6 ) then
      ihave = 7
    else
      iterm = 1
    end if
!
!  Decimal point.
!
  else if ( chrtmp=='.' ) then

    if ( ihave<4 ) then
      ihave = 4
    else if ( ihave>=6.and.ihave<=8 ) then
      ihave = 9
    else
      iterm = 1
    end if
!
!  Exponent marker.
!
  else if ( s_eqi(chrtmp,'e').or.s_eqi(chrtmp,'d')  ) then

    if ( ihave<6 ) then
      ihave = 6
    else
      iterm = 1
    end if
!
!  Digit.
!
  else if ( ihave<11.and.lge(chrtmp,'0').and.lle(chrtmp,'9')  ) then

    if ( ihave<=2 ) then
      ihave = 3
    else if ( ihave==4 ) then
      ihave = 5
    else if ( ihave==6.or.ihave==7 ) then
      ihave = 8
    else if ( ihave==9 ) then
      ihave = 10
    end if

    read(chrtmp,'(i1)')ndig

    if ( ihave==3 ) then
      rtop = 10*rtop+ndig
    else if ( ihave==5 ) then
      rtop = 10*rtop+ndig
      rbot = 10*rbot
    else if ( ihave==8 ) then
      jtop = 10*jtop+ndig
    else if ( ihave==10 ) then
      jtop = 10*jtop+ndig
      jbot = 10*jbot
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
  if ( iterm/=1.and.lchar+1<nchar)go to 10
!
!  If we haven't seen a terminator, and we have examined the
!  entire string, then we're done, and LCHAR is equal to NCHAR.
!
  if ( iterm/=1 .and. lchar+1==nchar)lchar = nchar
!
!  Number seems to have terminated.  Have we got a legal number?
!  Not if we terminated in states 1, 2, 6 or 7!
!
  if ( ihave==1 .or. ihave==2 .or. ihave==6 .or. ihave==7 ) then

    ierror = ihave
    write ( *, * ) 'CHRCTR - Serious error!'
    write ( *, * ) '  Illegal or nonnumeric input:'
    write(*,'(3x,a)')string
    return
  end if
!
!  Number seems OK.  Form it.
!
  if ( jtop==0 ) then
    rexp = 1.0
  else

    if ( jbot==1 ) then
      rexp = 10.0E+00**(jsgn*jtop)
    else
      rexp = jsgn*jtop
      rexp = rexp/jbot
      rexp = 10.0E+00**rexp
    end if

  end if

  rval = isgn*rexp*rtop/rbot

  return
end
subroutine chrdb1(string)

!*****************************************************************************80
!
!! CHRDB1 accepts a string of characters and removes all
!  blanks and nulls, left justifying the remainder and padding with
!  blanks.
!
!  Compare CHRDB0 which pads with nulls.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Parameters:
!
!  STRING Input/output, CHARACTER*(*) STRING, the string to be
!         transformed.
!
  implicit none

  character chrtmp
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) nchar
  character null
  character ( len = * )  string

  nchar = len(string)
  null = char(0)

  j = 0
  do i = 1,nchar
    chrtmp = string(i:i)
    string(i:i) = ' '

    if ( chrtmp/=' '.and.chrtmp/=null ) then

      j = j+1
      string(j:j) = chrtmp

    end if

  end do

  return
end
subroutine chrdb2(string)

!*****************************************************************************80
!
!! CHRDB2 accepts a string of characters.  It replaces all nulls
!  by blanks.  It replaces all strings of consecutive blanks by a
!  single blank, left justifying the remainder and padding with
!  blanks.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Parameters:
!
!  STRING Input/output, CHARACTER*(*) STRING, the string to be
!         transformed.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) nchar
  character newchr
  character null
  character oldchr
  character ( len = * )  string

  nchar = len(string)
  null = char(0)

  j = 0
  newchr = ' '

  do i = 1,nchar

    oldchr = newchr
    if ( string(i:i)==null)string(i:i) = ' '
    newchr = string(i:i)
    string(i:i) = ' '

    if ( oldchr/=' '.or.newchr/=' ' ) then

      j = j+1
      string(j:j) = newchr

    end if

  end do

  return
end
function chrrel(rval)

!*****************************************************************************80
!
!! CHRREL accepts a real number in RVAL and returns in CHRREL a
!  14-character right-justified representation of that number.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Parameters:
!
!  RVAL   Input, real RVAL, a real number.
!
!  CHRREL Output (through function value), CHARACTER*14 CHRREL,
!         a right-justified character variable containing the
!         representation of RVAL, using a G14.6 format.
!
  implicit none

  character ( len = 14 )  chrrel
  character ( len = 14 )  chrtmp
  real rval
!
!  We can't seem to write directly into CHRREL because of compiler
!  quibbles.
!
  if ( real(int(rval))==rval.and.abs(rval)<1.0e+13 ) then

    write(chrtmp,'(i14)')int(rval)

  else

    write(chrtmp,'(g14.6)')rval

  end if

  chrrel = chrtmp
  return
end
subroutine dodat(difx,dify,dist,dpdxc,dpdyc,dpdxfd,dpdyfd,dpdxsn,dpdysn, &
  drdx,drdy,ie,in,mark,maxelm,maxnp,nelem,node,np,nx,ny,p,pcmax, &
  pcmin,pmax,pmin,s,scmax,scmin,smax,smin,x,xpmax,xpmin,y,ypmax,ypmin)

!*****************************************************************************80
!
!! DODAT processes data that has been read from a file.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) maxelm
  integer ( kind = 4 ) maxnp

  real difx(maxnp)
  real dify(maxnp)
  real dist(2)
  real dpdxc(maxnp)
  real dpdyc(maxnp)
  real dpdxfd(maxnp)
  real dpdyfd(maxnp)
  real dpdxsn(maxnp)
  real dpdysn(maxnp)
  real drdx(maxnp)
  real drdy(maxnp)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ie(maxelm)
  integer ( kind = 4 ) in(maxnp)
  integer ( kind = 4 ) ip
  character isay
  integer ( kind = 4 ) j
  logical s_eqi
  integer ( kind = 4 ) mark(maxnp)
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) node(3,maxelm)
  integer ( kind = 4 ) np
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny
  real p(maxnp)
  real pcmax
  real pcmin
  real pmax
  real pmin
  real s(maxnp)
  real scmax
  real scmin
  real smax
  real smin
  real x(maxnp)
  real xpmax
  real xpmin
  real y(maxnp)
  real ypmax
  real ypmin
!
!  Get the range of the data.
!
  pmax = maxval ( p(1:np) )
  pmin = minval ( p(1:np) )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DoDat:'
  write ( *, * ) '  The cost data ranges from ',pmin,' to ',pmax

  pcmax = pmax
  pcmin = pmin
!
!  By default, mark the location of the minimum cost.
!
  do i = 1,np
    if ( p(i)==pmin ) then
      mark(i) = 1
    else
      mark(i) = -1
    end if
  end do
!
!  Set the X and Y locations.
!
  call setxy(np,nx,ny,x,xpmax,xpmin,y,ypmax,ypmin)
!
!  Set the element data.
!
  nelem = 2*(nx-1)*(ny-1)
  write ( *, * ) '  The number of elements is ',nelem

  call setelm(nelem,node,nx,ny)
!
!  Set the crude finite difference estimate for derivatives,
!  based on the pointwise cost data supplied in P.
!
  ip = 0

  do i = 1,nx
    do j = 1,ny

      ip = ip+1

      if ( i==1 ) then
        drdx(ip) = (p(ip+nx)-p(ip))/dist(1)
      else if ( i==nx ) then
        drdx(ip) = (p(ip)-p(ip-nx))/dist(1)
      else
        drdx(ip) = (p(ip+nx)-p(ip-nx))/(2.0E+00*dist(1))
      end if

      if ( j==1 ) then
        drdy(ip) = (p(ip+1)-p(ip))/dist(2)
      else if ( j==ny ) then
        drdy(ip) = (p(ip)-p(ip-1))/dist(2)
      else
        drdy(ip) = (p(ip+1)-p(ip-1))/(2.0E+00*dist(2))
      end if

    end do
  end do
!
!  Set the stream function.
!
  write ( *, * ) ' '
  write ( *, * ) 'DoDat:'
  write ( *, * ) '  Specify how the stream function is to be set:'
  write ( *, * ) ' '
  write ( *, * ) '    S use the discretized sensitivities;'
  write ( *, * ) '    F use the finite difference sensitivities;'
  write ( *, * ) '    R use the crude finite difference sensitivities.'
  write ( *, * ) '    C use the finite difference cost.'

  read(*,'(a)')isay
  write(17,'(a)')isay

  if ( s_eqi(isay,'s') ) then
    call stream3(ie,in,nelem,node,np,s,dpdxsn,dpdysn,x,y)
  else if ( s_eqi(isay,'f') ) then
    call stream3(ie,in,nelem,node,np,s,dpdxfd,dpdyfd,x,y)
  else if ( s_eqi(isay,'r') ) then
    call stream3(ie,in,nelem,node,np,s,drdx,drdy,x,y)
  else if ( s_eqi(isay,'c') ) then
    call stream3(ie,in,nelem,node,np,s,dpdxc,dpdyc,x,y)
  else
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'DoDat - Warning!'
    write ( *, '(a)' ) '  Your input was illegal.'
    write ( *, '(a)' ) '  I will use discretized sensitivities.'
    call stream3(ie,in,nelem,node,np,s,dpdxsn,dpdysn,x,y)
  end if

  smax = maxval ( s(1:np) )
  smin = minval ( s(1:np) )

  scmax = smax
  scmin = smin
!
!  Prepare auxilliary information, if needed.
!
  difx(1:np) = dpdxsn(1:np) - dpdxfd(1:np)
  dify(1:np) = dpdysn(1:np) - dpdyfd(1:np)

  return
end
subroutine doofus3(ielem,jloc,nelem,node,np,sinc,u,v,xc,yc)

!*****************************************************************************80
!
!! DOOFUS3 is used by STREAM3, to compute SINC, the stream function
!  increment in moving from local node JLOC to local node KLOC in
!  element IELEM.
!
!  Here is a picture of IELEM
!
!       A
!       |        2
!       |       /|
!   E   |      / |
!   T   |     /  |
!   A   |    /   |
!       |   /    |
!       |  1-----3
!       |
!       +----------->
!           XSI
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ), parameter :: ngauss = 3

  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) np

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
  integer ( kind = 4 ) igauss
  integer ( kind = 4 ) jloc
  integer ( kind = 4 ) node(3,nelem)
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
!  Get the Gauss weights and abscissas.
!
  xq(1) = -0.77459666E+00
  xq(2) =  0.0E+00
  xq(3) =  0.77459666E+00

  wq(1) = 5.0E+00/9.0E+00
  wq(2) = 8.0E+00/9.0E+00
  wq(3) = 5.0E+00/9.0E+00
!
!  Compute the integral.
!
  sinc = 0.0E+00

  do igauss = 1,3

    if ( jloc==1 ) then
      xsi = (xq(igauss)+1.0E+00)/2.0E+00
      eta = (xq(igauss)+1.0E+00)/2.0E+00
      dxsids = sqrt(2.0E+00)
      detads = sqrt(2.0E+00)
      dels = sqrt(2.0E+00)
    else if ( jloc==2 ) then
      xsi = 1.0E+00
      eta = (xq(igauss)+1.0E+00)/2.0E+00
      dxsids = 0.0E+00
      detads = -1.0E+00
      dels = 1.0E+00
    else if ( jloc==3 ) then
      xsi = (xq(igauss)+1.0E+00)/2.0E+00
      eta = 0.0E+00
      dxsids = -1.0E+00
      detads = 0.0E+00
      dels = 1.0E+00
    end if

    call trans3(det,detadx,detady,dxdeta,dxdxsi,dxsidx,dxsidy, &
      dydeta,dydxsi,eta,ielem,nelem,node,np,xc,xsi,yc)

    call ueval3(detadx,detady,dxsidx,dxsidy,eta,ielem,nelem,node,np,u, &
      uval,v,vval,xsi)

    sinc = sinc+0.5E+00*dels*wq(igauss) &
         *((-vval*dxdxsi+uval*dydxsi)*dxsids &
          +(-vval*dxdeta+uval*dydeta)*detads )

  end do

  return
end
subroutine getdat ( dist, dpdxc, dpdyc, dpdxfd, dpdyfd, dpdxsn, dpdysn, &
  idata, istep, np, nx, ny, p )

!*****************************************************************************80
!
!! GETDAT reads the information from a file.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) np

  real dist(2)
  real dpdxc(np)
  real dpdyc(np)
  real dpdxfd(np)
  real dpdyfd(np)
  real dpdxsn(np)
  real dpdysn(np)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) idata
  integer ( kind = 4 ) istep
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny
  real p(np)
!
!  The first node read in is in the lower left corner.
!  The next node read in is in the same row, but one column to the left,
!  and so on until NX nodes are read.
!  Then a new row is read, one row higher.
!
!  Because SLAB numbers up first, and then to the right, we must be
!  careful in how we read in the values.
!
  do j = 1,ny
    do i = 1,nx
      k = (i-1)*ny+j
      read(1,*,end = 30)p(k)
      read(1,*,end = 30)dpdxsn(k),dpdysn(k)
      read(1,*,end = 30)dpdxfd(k),dpdyfd(k)
      read(1,*,end = 30)dpdxc(k),dpdyc(k)
    end do
  end do
!
!  Read in the physical length of the two marching directions.
!
  read(1,*,end = 10)dist(1),dist(2)
  go to 20

10    continue

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'GETDAT - Warning!'
  write ( *, '(a)' ) '  The values of DIST(1) and DIST(2) were missing'
  write ( *, '(a)' ) '  from the marching file.'
  write ( *, '(a)' ) '  No problem, I just set them to 1.'

  dist(1) = 1.0E+00
  dist(2) = 1.0E+00

20    continue

  write ( *, '(a)' ) ' '
  write ( *, * ) '  X marching distance is ',dist(1)
  write ( *, * ) '  Y marching distance is ',dist(2)

  istep = istep+1

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'GETDAT:'
  write ( *, '(a,i6)' ) '  The data has been read for step ', istep

  idata = 2

  return

30    continue
  idata = 0
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'GETDAT - Serious error!'
  write ( *, '(a)' ) '  End of file while reading information!'
  return

end
subroutine getit(fileit,maxit,nit,xit,yit)

!*****************************************************************************80
!
!! GETIT reads X and Y coordinates of the iterates.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) maxit

  character ( len = 40 ) fileit
  integer ( kind = 4 ) nit
  real temp1
  real temp2
  real temp3
  real xit(maxit)
  real yit(maxit)

  open(unit = 2,file=fileit,access='sequential',form='formatted', &
    status = 'old',err=30)

10    continue
  read(2,*,end = 20,err=20)temp1,temp2,temp3
  nit = nit+1
  yit(nit) = temp2
  xit(nit) = temp3
  go to 10

20    continue
  close(unit = 2)
  write ( *, * ) ' '
  write ( *, * ) 'GetIt read data for ',nit,' iterates.'
  return

30    continue
  write ( *, * ) ' '
  write ( *, * ) 'GetIt - Fatal error!'
  write ( *, * ) '  The iterate file could not be opened!'
  nit = 0
  return
end
subroutine graph(dev,difx,dify,dist,dpdxc,dpdyc,dpdxfd,dpdyfd,dpdxsn, &
  dpdysn,drdx,drdy,filgrf,icmax,icmin,icolor,iplot,ishape,itable,jcmax, &
  jcmin,labelx,labely,laxes,lcvec,ldvec,lgopen,line,lit,lncvec,lndvec, &
  lnpvec,lnqvec,lnrvec,lsline,lqvec,lrvec,mark,maxit,maxlin,maxobj, &
  nelem,nflag,nit,nline,node,np,npcon,nscon,nx,ny,p,pcmax,pcmin,s,scmax, &
  scmin,show,title,title2,vecscl,x,xit,xpmax,xpmax2,xpmin,xpmin2, &
  xs,y,yit,ypmax,ypmax2,ypmin,ypmin2,ys)

!*****************************************************************************80
!
!! GRAPH produces the graph.
!
!  WARNING:
!
!    I am using PMAX and PMIN inconsistently.
!
!    PMAX is the maximum weighted entry in the vectors (DPDXSN,DPDYSN).
!    PMIN is the minimum entry in the P array.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) maxit
  integer ( kind = 4 ) maxlin
  integer ( kind = 4 ) maxobj
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) np
  integer ( kind = 4 ) npcon
  integer ( kind = 4 ) nscon

  real angle
  real cell
  real chigh
  character ( len = 14 )  chrrel
  real chwide
  real cmax
  real csize
  character ( len = 14 )  ctemp
  real cwide
  character ( len = 10 ) dev
  real dmax
  real difx(np)
  real dify(np)
  real dist(2)
  real dpdxc(np)
  real dpdyc(np)
  real dpdxfd(np)
  real dpdyfd(np)
  real dpdxsn(np)
  real dpdysn(np)
  real drdx(np)
  real drdy(np)
  real eps
  character ( len = 40 ) filgrf
  logical filled
  character ( len = 6 ) flush
  integer ( kind = 4 ) i
  integer ( kind = 4 ) icmax
  integer ( kind = 4 ) icmin
  integer ( kind = 4 ) icol
  integer ( kind = 4 ) icolor(maxobj)
  integer ( kind = 4 ) ie
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) in
  integer ( kind = 4 ) ine
  integer ( kind = 4 ) ip
  integer ( kind = 4 ) iplot
  integer ( kind = 4 ) ishape(np)
  integer ( kind = 4 ) itable
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jcmax
  integer ( kind = 4 ) jcmin
  character ( len = 20 ) labelx
  character ( len = 20 ) labely
  logical laxes
  logical lcvec
  logical ldvec
  integer ( kind = 4 ) lent
  logical s_eqi
  logical lgopen
  integer ( kind = 4 ) line(2,maxlin)
  logical lit
  logical lncvec
  logical lndvec
  logical lnpvec
  logical lnqvec
  logical lnrvec
  logical lqvec
  logical lrvec
  logical lsline
  integer ( kind = 4 ) mark(np)
  logical nflag(np)
  integer ( kind = 4 ) nit
  integer ( kind = 4 ) nline
  integer ( kind = 4 ) node(3,nelem)
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny
  real p(np)
  real pcmax
  real pcmin
  real pmax
  real pval
  real pwide
  real qmax
  real rmax
  real s(np)
  real scmax
  real scmin
  logical show(maxobj)
  real srange
  real sval
  character ( len = 40 ) title
  character ( len = 40 ) title2
  real vdscal
  real vecscl
  real vcscal
  real vpscal
  real vqscal
  real vrscal
  real vscale
  real x(np)
  real x1
  real x2
  real xgmax
  real xgmin
  real xit(maxit)
  real xpmax
  real xpmax2
  real xpmin
  real xpmin2
  real xs(np)
  real xsmax
  real xsmin
  real xspmax
  real xspmin
  real xsval
  real xtemp
  real xwmax
  real xwmin
  real y(np)
  real y1
  real y2
  real ygmax
  real ygmin
  real yit(maxit)
  real ypmax
  real ypmax2
  real ypmin
  real ypmin2
  real ys(np)
  real ysmax
  real ysmin
  real yspmax
  real yspmin
  real ysval
  real ytemp
  real ywmax
  real ywmin

  call preplt(dev,filgrf,icmax,icmin,iplot,itable,lgopen)
!
!  Set coordinates.
!
  xgmin = 0.00E+00
  xgmax = 1.00E+00
  ygmin = 0.00E+00
  ygmax = 1.00E+00

  xsmin = 0.05E+00
  xsmax = 0.95E+00
  ysmin = 0.05E+00
  ysmax = 0.95E+00

  xwmin = 0.15E+00
  xwmax = 0.85E+00
  ywmin = 0.15E+00
  ywmax = 0.85E+00

  xspmin = 0.20E+00
  xspmax = 0.80E+00
  yspmin = 0.20E+00
  yspmax = 0.80E+00
!
!  Convert X and Y physical coordinates to screen coordinates.
!
  do i = 1,np
    xs(i) = xspmin+(x(i)-xpmin2)*(xspmax-xspmin)/(xpmax2-xpmin2)
    ys(i) = yspmin+(y(i)-ypmin2)*(yspmax-yspmin)/(ypmax2-ypmin2)
  end do
!
!  Tell the graphics package our graphical coordinate system.
!
  call setwcd(xgmin,ygmin,xgmax,ygmax,ierror)
!
!  Draw some boxes.
!
  if ( show(3) ) then
    icol = icolor(3)
    call linclr(icol)
    call box(xsmin,xsmax,ysmin,ysmax)
    call box(xwmin,xwmax,ywmin,ywmax)
    call box(xspmin,xspmax,yspmin,yspmax)
  end if
!
!  Draw the title.
!
  lent = len_trim(title)
  if ( lent>0 ) then
    icol = icolor(1)
    call linclr(icol)
    angle = 0.0E+00
    chwide = 0.02E+00
    pwide = 1.0E+00
    xtemp = 0.5E+00
    ytemp = ysmax-0.05E+00
    flush = 'center'
    call s_plot ( angle,chwide,pwide,title(1:lent),xtemp,ytemp,flush)
  end if
!
!  Draw title 2.
!
  lent = len_trim(title2)
  if ( lent>0 ) then
    icol = icolor(1)
    call linclr(icol)
    angle = 0.0E+00
    chwide = 0.015E+00
    pwide = 1.0E+00
    xtemp = 0.5E+00
    ytemp = ysmax-0.075E+00
    flush = 'center'
    call s_plot ( angle,chwide,pwide,title2(1:lent),xtemp,ytemp,flush)
  end if
!
!  Draw the X label.
!
  lent = len_trim(labelx)
  if ( lent>0 ) then
    angle = 0.0E+00
    chwide = 0.015E+00
    pwide = 1.0E+00
    xtemp = 0.5E+00
    ytemp = 0.15E+00
    flush = 'center'
    call s_plot ( angle,chwide,pwide,labelx(1:lent),xtemp,ytemp,flush)
  end if
!
!  Draw the Y label.
!
  lent = len_trim(labely)
  if ( lent>0 ) then
    angle = 90.0E+00
    chwide = 0.015E+00
    pwide = 1.0E+00
    xtemp = 0.15E+00
    ytemp = 0.5E+00
    flush = 'center'
    call s_plot ( angle,chwide,pwide,labely(1:lent),xtemp,ytemp,flush)
  end if
!
!  Draw the axes.
!
  if ( laxes ) then
    eps = 0.025E+00

    call movcgm(xspmin-eps,yspmin)
    call drwcgm(xspmax,yspmin)

    ctemp = chrrel(xpmin)
    call chrdb1(ctemp)
    lent = len_trim ( ctemp)
    angle = 0.0E+00
    chwide = 0.010E+00
    pwide = 1.0E+00
    xtemp = xspmin
    ytemp = 0.15E+00
    flush = 'left'
    call s_plot ( angle,chwide,pwide,ctemp(1:lent),xtemp,ytemp,flush)

    ctemp = chrrel(xpmax)
    call chrdb1(ctemp)
    lent = len_trim ( ctemp)
    angle = 0.0E+00
    chwide = 0.010E+00
    pwide = 1.0E+00
    xtemp = xspmax
    ytemp = 0.15E+00
    flush = 'right'
    call s_plot ( angle,chwide,pwide,ctemp(1:lent),xtemp,ytemp,flush)

    call movcgm(xspmin,yspmin-eps)
    call drwcgm(xspmin,yspmax)

    ctemp = chrrel(ypmin)
    call chrdb1(ctemp)
    lent = len_trim ( ctemp)
    angle = 90.0E+00
    chwide = 0.010E+00
    pwide = 1.0E+00
    xtemp = 0.15E+00
    ytemp = yspmin
    flush = 'left'
    call s_plot ( angle,chwide,pwide,ctemp(1:lent),xtemp,ytemp,flush)

    ctemp = chrrel(ypmax)
    call chrdb1(ctemp)
    lent = len_trim ( ctemp)
    angle = 90.0E+00
    chwide = 0.010E+00
    pwide = 1.0E+00
    xtemp = 0.15E+00
    ytemp = yspmax
    flush = 'right'
    call s_plot ( angle,chwide,pwide,ctemp(1:lent),xtemp,ytemp,flush)
  end if
!
!  Draw contour colors.
!
  if ( show(5) ) then

    write ( *, * ) 'jcmax = ',jcmax
    write ( *, * ) 'jcmin = ',jcmin

    call tricol(jcmax,jcmin,npcon,nelem,nflag,node,np,p,pcmax,pcmin,xs,ys)

    if ( show(2) ) then
      x1 = 0.15E+00
      y1 = 0.08E+00
      x2 = 0.85E+00
      y2 = 0.13E+00
      srange = x2-x1

      call cbar(jcmax,jcmin,npcon,pcmax,pcmin,srange,x1,x2,y1,y2)
    end if

  end if
!
!  Draw the elements.
!
  if ( show(7) ) then
    icol = icolor(7)
    call linclr(icol)

    do i = 1,nx-1
      do j = 1,ny-1
        ip = (i-1)*ny+j
        in = ip+1
        ie = ip+ny
        ine = ip+ny+1
        call movcgm(xs(ip),ys(ip))
        call drwcgm(xs(in),ys(in))
        call drwcgm(xs(ine),ys(ine))
        call drwcgm(xs(ip),ys(ip))
        call drwcgm(xs(ie),ys(ie))
        call drwcgm(xs(ine),ys(ine))
      end do
    end do

  end if
!
!  Draw P contour lines.
!
  if ( show(4) ) then

    icol = icolor(4)
    call linclr(icol)

    do i = 1,npcon
      pval = ((npcon-i)*pcmin+(i-1)*pcmax)/real(npcon-1)
      call lincon(nelem,nflag,node,np,p,pval,xs,ys)
    end do
  end if
!
!  Draw S contour lines.
!
  if ( lsline ) then

    do i = 1,nscon
      sval = ((nscon-i)*scmin+(i-1)*scmax)/real(nscon-1)
      call lincon(nelem,nflag,node,np,s,sval,xs,ys)
    end do

  end if
!
!  Find the maximum of the weighted absolute values of the entries
!  in the vectors.
!
  call vrange(np,cmax,dpdxc,dpdyc,dist)
  if ( cmax==0.0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'Graph - Warning!'
    write ( *, * ) '  The maximum entry in the C vector array is zero!'
  end if

  call vrange(np,dmax,difx,dify,dist)

  call vrange(np,pmax,dpdxsn,dpdysn,dist)
  if ( pmax==0.0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'Graph - Warning!'
    write ( *, * ) '  The maximum discretized sensitivity gradient is 0!'
  end if

  call vrange(np,qmax,dpdxfd,dpdyfd,dist)
  if ( qmax==0.0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'Graph - Warning!'
    write ( *, * ) '  The maximum entry in the Q vector array is zero!'
  end if

  call vrange(np,rmax,drdx,drdy,dist)
!
!  Compute the minimum of the width and height of a single cell,
!  and divide by two to get the length of a vector that would
!  never enter another cell.
!
  cwide = (xspmax-xspmin)/((xpmax2-xpmin2)*real(nx-1))
  chigh = (yspmax-yspmin)/((ypmax2-ypmin2)*real(ny-1))
  cell = 0.5E+00 *max(cwide,chigh)
!
!  Now set the scale factors for the various vectors.
!
  if ( lcvec ) then
    vcscal = vecscl*cell/cmax
  else
    vcscal = 0.0E+00
  end if

  if ( ldvec ) then
    vdscal = vecscl*cell/dmax
  else
    vdscal = 0.0E+00
  end if

  if ( show(6) ) then
    icol = icolor(6)
    call linclr(icol)
    vpscal = vecscl*cell/pmax
  else
    vpscal = 0.0E+00
  end if

  if ( lqvec ) then
    vqscal = vecscl*cell/qmax
  else
    vqscal = 0.0E+00
  end if

  if ( lrvec ) then
    vrscal = vecscl*cell/rmax
  else
    vrscal = 0.0E+00
  end if
!
!  Set the overall vector scale factor.
!
  vscale = 0.0E+00

  if ( abs(vcscal)>vscale ) then
    vscale = abs(vcscal)
  end if

  if ( abs(vdscal)>vscale ) then
    vscale = abs(vdscal)
  end if

  if ( abs(vpscal)>vscale ) then
    vscale = abs(vpscal)
  end if

  if ( abs(vqscal)>vscale ) then
    vscale = abs(vqscal)
  end if

  if ( abs(vrscal)>vscale ) then
    vscale = abs(vrscal)
  end if
!
!  Draw C vectors.
!
  if ( lcvec ) then
    vcscal = vscale*sign(1.0,vcscal)
    call vector(dist,nflag,np,dpdxc,dpdyc,vcscal,xs,ys)
  end if
!
!  Draw D vectors.
!
  if ( ldvec ) then
    vdscal = vscale*sign(1.0,vdscal)
    call vector(dist,nflag,np,difx,dify,vdscal,xs,ys)
  end if
!
!  Draw discretized sensitivity gradients.
!
  if ( show(6) ) then
    icol = icolor(6)
    call linclr(icol)
    vpscal = vscale*sign(1.0,vpscal)
    call vector(dist,nflag,np,dpdxsn,dpdysn,vpscal,xs,ys)
  end if
!
!  Draw Q vectors.
!
  if ( lqvec ) then
    vqscal = vscale*sign(1.0,vqscal)
    call vector(dist,nflag,np,dpdxfd,dpdyfd,vqscal,xs,ys)
  end if
!
!  Draw R vectors.
!
  if ( lrvec ) then
    vrscal = vscale*sign(1.0,vrscal)
    call vector(dist,nflag,np,drdx,drdy,vrscal,xs,ys)
  end if
!
!  Draw normalized C vectors.
!
  if ( lncvec ) then
    vscale = cell*vecscl
    call norvec(dist,nflag,np,dpdxc,dpdyc,vscale,xs,ys)
  end if
!
!  Draw normalized D vectors.
!
  if ( lndvec ) then
    vscale = cell*vecscl
    call norvec(dist,nflag,np,difx,dify,vscale,xs,ys)
  end if
!
!  Draw normalized discretized sensitivity gradients.
!
  if ( lnpvec ) then
    vscale = cell*vecscl
    call norvec(dist,nflag,np,dpdxsn,dpdysn,vscale,xs,ys)
  end if
!
!  Draw normalized Q vectors.
!
  if ( lnqvec ) then
    vscale = cell*vecscl
    call norvec(dist,nflag,np,dpdxfd,dpdyfd,vscale,xs,ys)
  end if
!
!  Draw normalized R vectors.
!
  if ( lnrvec ) then
    vscale = cell*vecscl
    call norvec(dist,nflag,np,drdx,drdy,vscale,xs,ys)
  end if
!
!  Mark the nodes.
!
  csize = 0.01E+00
  icol = 1
  call linclr(icol)

  do i = 1,np
    if ( nflag(i) ) then
      if ( mark(i)>0 ) then
        icol = mark(i)
        call filclr(icol)
        if ( ishape(i)==0 ) then
          filled = .false.
          call circle(xs(i),ys(i),csize,filled)
        else if ( ishape(i)==1 ) then
          filled = .true.
          call circle(xs(i),ys(i),csize,filled)
        else if ( ishape(i)==2 ) then
          filled = .false.
          call square(xs(i),ys(i),csize,filled)
        else if ( ishape(i)==3 ) then
          filled = .true.
          call square(xs(i),ys(i),csize,filled)
        end if
      end if
    end if
  end do
!
!  Draw the lines.
!
  icol = 1
  call linclr(icol)

  do i = 1,nline
    call movcgm(xs(line(1,i)),ys(line(1,i)))
    call drwcgm(xs(line(2,i)),ys(line(2,i)))
  end do
!
!  Draw the iterates.
!
  if ( lit ) then

    csize = 0.01E+00
    filled = .true.
    icol = 1
    call filclr(icol)

    do i = 1,nit

      xsval = xspmin+(xit(i)-xpmin)*(xspmax-xspmin)/(xpmax-xpmin)
      ysval = yspmin+(yit(i)-ypmin)*(yspmax-yspmin)/(ypmax-ypmin)
      call circle(xsval,ysval,csize,filled)

      if ( i==1 ) then
        call movcgm(xsval,ysval)
      else
        call drwcgm(xsval,ysval)
      end if

    end do
  end if
!
!  Pause, if we are doing X-Windows.
!
  call buzz ( dev, xsmin, xsmax, ysmin, ysmax )

  return
end
subroutine hello

!*****************************************************************************80
!
!! HELLO prints a brief welcoming message.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Parameters:
!
!    None.
!
  write ( *, * ) ' '
  write ( *, * ) 'SLAB'
  write ( *, * ) '  20 October 1995'
  write ( *, * ) '  Contour plots of cost functional values.'
  write ( *, * ) '  Vector plots of the projected cost gradient.'
  write ( *, * ) ' '

  return
end
subroutine help

!*****************************************************************************80
!
!! HELP prints the list of legal commands.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Parameters:
!
!    None.
!
  implicit none

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'AXES     Show axes.'
  write ( *, '(a)' ) 'BAR      Show the color bar.'
  write ( *, '(a)' ) 'ELEMENTS Show the "elements".'
  write ( *, '(a)' ) 'FRAME    Show a frame.'
  write ( *, '(a)' ) 'IT       Show the iterates.'
  write ( *, '(a)' ) 'NC       Show normalized C vectors (or D, Q, R).'
  write ( *, '(a)' ) 'PC       Show color P contours.'
  write ( *, '(a)' ) 'PL       Show P contour lines.'
  write ( *, '(a)' ) 'SL       Show S contour lines.'
  write ( *, * ) 'NP       Show normalized discretized sensitivity gradients.'
  write ( *, * ) 'VC       Show C vectors (or D, Q, R).'
  write ( *, * ) 'VP       Show discretized sensitivity gradients.'
  write ( *, * ) ' '
  write ( *, * ) 'A       Advance to next step.'
  write ( *, * ) 'G       Draw the graph.'
  write ( *, * ) 'H       Help (print this list)'
  write ( *, * ) 'Q       Quit'
  write ( *, * ) ' '
  write ( *, * ) 'C       Set color of some things.'
  write ( *, * ) 'CC =      Choose color table.'
  write ( *, * ) 'COLOR   Set one color index.'
  write ( *, * ) 'CTAB    Show current color table.'
  write ( *, * ) 'DAT =     Specify the input data file.'
  write ( *, * ) 'DEV =     Choose the graphics device.'
  write ( *, * ) 'FILE =    Name the graphics output file.'
  write ( *, * ) 'ICMAX =   Set maximum color.'
  write ( *, * ) 'ICMIN =   Set minimum color.'
  write ( *, * ) 'IDAT =    Specify the iterate data file.'
  write ( *, * ) 'LABELX =  Set X axis label.'
  write ( *, * ) 'LABELY =  Set Y axis label.'
  write ( *, * ) 'LINE = I1,J1,I2,J2'
  write ( *, * ) '        Connect nodes at (I1,J1) and (I2,J2).'
  write ( *, * ) 'MARK = I,J,ICOLOR,ISHAPE'
  write ( *, * ) '        Mark node I over, J up with color ICOLOR.'
  write ( *, * ) '        ISHAPE = 0, circle, 1, filled circle,'
  write ( *, * ) '               2, square, 3, filled square.'
  write ( *, * ) 'NPCON =   Set number of P contours.'
  write ( *, * ) 'NSCON =   Set number of S contours.'
  write ( *, * ) 'PCMAX =   Set maximum P contour level.'
  write ( *, * ) 'PCMIN =   Set minimum P contour level.'
  write ( *, * ) 'SCMAX =   Set maximum S contour level.'
  write ( *, * ) 'SCMIN =   Set minimum S contour level.'
  write ( *, * ) 'TITLE =   Set title.'
  write ( *, * ) 'TITLE2 =  Set title #2.'
  write ( *, * ) 'VECSCL =  Set vector scale factor.'
  write ( *, * ) 'X       Set min and max X and Y to display.'
  write ( *, * ) ' '
  write ( *, * ) 'PRCON   Print contour data.'
  write ( *, * ) 'PRCOST  Print X, Y, Cost.'
  write ( *, * ) 'PREL    Print element data.'
  write ( *, * ) 'PRNODE  Print data at a node.'
  write ( *, * ) 'PRSET   Print settings.'
  write ( *, * ) ' '
  write ( *, * ) '#       # begins a comment line.'
  write ( *, * ) ' '
  write ( *, * ) 'C vectors: The cost gradient estimated by '
  write ( *, * ) 'finite differences.'
  write ( *, * ) ' '
  write ( *, * ) 'D vectors: The difference between P and Q vectors.'
  write ( *, * ) ' '
  write ( *, * ) 'Q vectors: The cost gradients estimated by '
  write ( *, * ) 'finite difference sensitivities.'
  write ( *, * ) ' '
  write ( *, * ) 'R vectors: Finite difference approximations'
  write ( *, * ) 'to the cost gradient, generated by SLAB from'
  write ( *, * ) 'the available cost data.'

  return
end
subroutine init(dev,difx,dify,dist,dpdxc,dpdyc,dpdxfd,dpdyfd,dpdxsn,dpdysn, &
  drdx,drdy,filein,filgrf,grace,icmax,icmin,icolor,ie,in,iplot,ishape, &
  istep,itable,jcmax,jcmin,labelx,labely,laxes,lcvec,ldvec,lgopen,line,lit, &
  lncvec,lndvec,lnpvec,lnqvec,lnrvec,lsline,lqvec,lrvec,mark,maxelm, &
  maxit,maxlin,maxnp,maxobj,nelem,nerror,nflag,nit,nline,node,np,npcon, &
  nscon,nx,ny,object,p,pcmax,pcmin,pmax,pmin,s,scmax,scmin,show,smax,smin, &
  title,title2,vecscl,vmin,x,xit,xpmax,xpmax2,xpmin,xpmin2,y,yit,ypmax, &
  ypmax2,ypmin,ypmin2)

!*****************************************************************************80
!
!! INIT sets initial values for the program variables.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) maxelm
  integer ( kind = 4 ) maxit
  integer ( kind = 4 ) maxlin
  integer ( kind = 4 ) maxnp
  integer ( kind = 4 ) maxobj

  real del
  character ( len = 10 ) dev
  real dfx
  real dfy
  real difx(maxnp)
  real dify(maxnp)
  real dist(2)
  real dpdxc(maxnp)
  real dpdyc(maxnp)
  real dpdxfd(maxnp)
  real dpdyfd(maxnp)
  real dpdxsn(maxnp)
  real dpdysn(maxnp)
  real drdx(maxnp)
  real drdy(maxnp)
  real f
  character ( len = 40 ) filein
  character ( len = 40 ) filgrf
  real grace
  integer ( kind = 4 ) i
  integer ( kind = 4 ) icmax
  integer ( kind = 4 ) icmin
  integer ( kind = 4 ) icolor(maxobj)
  integer ( kind = 4 ) ie(maxelm)
  integer ( kind = 4 ) in(maxnp)
  integer ( kind = 4 ) ip
  integer ( kind = 4 ) iplot
  integer ( kind = 4 ) ishape(maxnp)
  integer ( kind = 4 ) istep
  integer ( kind = 4 ) itable
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jcmax
  integer ( kind = 4 ) jcmin
  character ( len = 20 ) labelx
  character ( len = 20 ) labely
  logical laxes
  logical lcvec
  logical ldvec
  logical lgopen
  integer ( kind = 4 ) line(2,maxlin)
  logical lit
  logical lncvec
  logical lndvec
  logical lnpvec
  logical lnqvec
  logical lnrvec
  logical lsline
  logical lqvec
  logical lrvec
  integer ( kind = 4 ) mark(maxnp)
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) nerror
  logical nflag(maxnp)
  integer ( kind = 4 ) nit
  integer ( kind = 4 ) nline
  integer ( kind = 4 ) node(3,maxelm)
  integer ( kind = 4 ) np
  integer ( kind = 4 ) npcon
  integer ( kind = 4 ) nscon
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny
  character ( len = 25 ) object(maxobj)
  real p(maxnp)
  real pcmax
  real pcmin
  real pmax
  real pmin
  real s(maxnp)
  real scmax
  real scmin
  logical show(maxobj)
  real smax
  real smin
  character ( len = 40 ) title
  character ( len = 40 ) title2
  real vecscl
  real vmin
  real x(maxnp)
  real xit(maxit)
  real xpmax
  real xpmax2
  real xpmin
  real xpmin2
  real xval
  real y(maxnp)
  real yit(maxit)
  real ypmax
  real ypmax2
  real ypmin
  real ypmin2
  real yval

  f(xval,yval) = xval*(yval-0.5E+00 )**2
  dfx(xval,yval) = (yval-0.5E+00 )**2
  dfy(xval,yval) = xval*2.0E+00 * (yval-0.5E+00)
!
!  Simple settings.
!
  dev = ' '
  dist(1) = 1.0E+00
  dist(2) = 1.0E+00
  filein = 'slab.inp'
  filgrf = ' '
  grace = 0.01E+00
  icmin = 2
  icmax = 255
  icolor(1:maxobj) = 1
  iplot = 0
  ishape(1:maxnp) = 0
  istep = 1
  itable = 0
  jcmin = 2
  jcmax = 255
  labelx = ' '
  labely = ' '
  laxes = .false.
  lcvec = .false.
  ldvec = .false.
  lgopen = .false.
  line(1:2,1:maxlin) = 0
  lit = .false.
  lncvec = .false.
  lndvec = .false.
  lnpvec = .false.
  lnqvec = .false.
  lnrvec = .false.
  lsline = .false.
  lqvec = .false.
  lrvec = .false.
  mark(1:maxnp) = -1
  nerror = 0
  nflag(1:maxnp) = .true.
  nit = 0
  nline = 0
  npcon = 20
  nscon = 20
  nx = 10
  ny = 10
  np = nx*ny
  object(1) = 'Titles'
  object(2) = 'Color bar'
  object(3) = 'Frame'
  object(4) = 'Cost contour lines'
  object(5) = 'Cost color contours'
  object(6) = 'Discretized sensitivity cost gradients'
  object(7) = 'Elements'
  show(1) = .true.
  show(2) = .true.
  show(3) = .false.
  show(4) = .false.
  show(5) = .false.
  show(6) = .false.
  show(7) = .false.

  title = ' '
  title2 = ' '
  vmin = 1.0E-08
  xpmax = 1.0E+00
  xpmax2 = 1.0E+00
  xpmin = 0.0E+00
  xpmin2 = 0.0E+00
  ypmax = 1.0E+00
  ypmax2 = 1.0E+00
  ypmin = 0.0E+00
  ypmin2 = 0.0E+00
  xit(1:maxit) = 0.0E+00
  yit(1:maxit) = 0.0E+00
!
!  Things that depend on other things.
!
  call setxy(np,nx,ny,x,xpmax,xpmin,y,ypmax,ypmin)

  nelem = 2*(nx-1)*(ny-1)
  call setelm(nelem,node,nx,ny)

  del = 0.01E+00
  do i = 1,np
    p(i) = f(x(i),y(i))
    dpdxc(i) = (f(x(i)+del,y(i)) - f(x(i)-del,y(i))) / (2.0E+00*del)
    dpdyc(i) = (f(x(i),y(i)+del) - f(x(i),y(i)-del)) / (2.0E+00*del)
    dpdxfd(i) = (f(x(i)+del,y(i)) - f(x(i)-del,y(i))) / (2.0E+00*del)
    dpdyfd(i) = (f(x(i),y(i)+del) - f(x(i),y(i)-del)) / (2.0E+00*del)
    dpdxsn(i) = dfx(x(i),y(i))
    dpdysn(i) = dfy(x(i),y(i))
    difx(i) = dpdxsn(i)-dpdxfd(i)
    dify(i) = dpdysn(i)-dpdyfd(i)
  end do

  ip = 0

  do i = 1,nx
    do j = 1,ny

      ip = ip+1

      if ( i==1 ) then
        drdx(ip) = p(ip+nx)-p(ip)
      else if ( i==nx ) then
        drdx(ip) = p(ip)-p(ip-nx)
      else
        drdx(ip) = (p(ip+nx)-p(ip-nx))/2.0
      end if

      if ( j==1 ) then
        drdy(ip) = p(ip+1)-p(ip)
      else if ( j==ny ) then
        drdy(ip) = p(ip)-p(ip-1)
      else
        drdy(ip) = (p(ip+1)-p(ip-1))/2.0
      end if

    end do
  end do

  pmax = maxval ( p(1:np) )
  pmin = minval ( p(1:np) )

  vecscl = 1.0E+00

  pcmax = pmax
  pcmin = 0.0E+00

  call stream3(ie,in,nelem,node,np,s,dpdxsn,dpdysn,x,y)

  smax = maxval ( s(1:np) )
  smin = minval ( s(1:np) )

  scmax = smax
  scmin = smin

  return
end
function inside(x1,xmid,x2)

!*****************************************************************************80
!
!! INSIDE reports whether XMID is between X1 and X2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Parameters:
!
!  X1,
!  XMID,
!  X2     Input, real X1, XMID, X2, three values to be tested.
!
!  INSIDE Output, LOGICAL INSIDE.
!
!         INSIDE will be TRUE if XMID is "inside" the interval
!         spanned by X1 and X2.  That is, if
!
!           X1 < =  XMID <= X2
!         or
!           X2 < =  XMID <= X1
!
!         Otherwise, INSIDE will be FALSE.
!
  implicit none

  logical inside
  real x1
  real x2
  real xmid

  if ( (x1<=xmid.and.xmid<=x2).or.(x1>=xmid.and.xmid>=x2) ) then
    inside = .true.
  else
    inside = .false.
  end if

  return
end
subroutine lincon(nelem,nflag,node,np,p,pval,x,y)

!*****************************************************************************80
!
!! LINCON draws a contour plot of a scalar quantity associated with the
!  the corner nodes of triangular finite element.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) np

  integer ( kind = 4 ) i
  integer ( kind = 4 ) icross
  logical inside
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jp1
  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n2
  logical nflag(np)
  integer ( kind = 4 ) node(3,nelem)
  real p(np)
  real p1
  real p2
  real pval
  real x1
  real x2
  real x(np)
  real xx(3)
  real y1
  real y2
  real y(np)
  real yy(3)
!
!  Draw contour by searching over each element.
!
  do i = 1,nelem

    icross = 0

    do j = 1,3

      n1 = node(j,i)
      x1 = x(n1)
      y1 = y(n1)
      p1 = p(n1)

      jp1 = j+1
      if ( jp1>3)jp1 = 1
      n2 = node(jp1,i)

      x2 = x(n2)
      y2 = y(n2)
      p2 = p(n2)

      if ( nflag(n1).and.nflag(n2) ) then
        if ( inside(p1,pval,p2) ) then
          if ( p1==pval.and.pval==p2 ) then
            call movcgm(x1,y1)
            call drwcgm(x2,y2)
          else
            icross = icross+1
            xx(icross) = x1+(pval-p1)*(x2-x1)/(p2-p1)
            yy(icross) = y1+(pval-p1)*(y2-y1)/(p2-p1)
          end if
        end if
      end if

    end do

    if ( icross==2 ) then
      call plylin(2,xx,yy)
    end if

  end do

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
!  Parameters:
!
!  STRNG1,
!  STRNG2 Input, CHARACTER*(*) STRNG1, STRNG2, the strings to
!         compare.
!
!  LNEI   Output, LOGICAL LNEI, the result of the comparison.
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
    call s_cap(s1)
    call s_cap(s2)
    if ( s1/=s2)return
  end do

  null = char(0)

  do i = lenc+1,len1
    if ( strng1(i:i)/=' '.and. strng1(i:i)/=null)return
  end do

  do i = lenc+1,len2
    if ( strng2(i:i)/=' '.and. strng2(i:i)/=null)return
  end do

  lnei = .false.

  return
end
subroutine norvec(dist,nflag,np,u,v,vscale,x,y)

!*****************************************************************************80
!
!! NORVEC draws a normalized vector direction field.
!
!  Discussion:
!
!    An arrow pointing in the direction (U(I), V(I)) is drawn at the
!    point (X(I), Y(I)).  The arrow's length is normalized to 1,
!    and then scaled.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Parameters:
!
!  NP     integer ( kind = 4 ) NP.
!         The number of nodes.
!
!  VSCALE real VSCALE.
!         A scale factor for vectors.
!
!  U      real U(NP).
!
!         U(I) is the horizontal vector component at node I.
!
!  V      real V(NP).
!
!         V(I) is the vertical vector component at node I.
!
!  X      real X(NP).
!         The X coordinates of the nodes.
!
!  Y      real Y(NP).
!         The Y coordinates of the nodes.
!
  implicit none

  integer ( kind = 4 ) np

  real dist(2)
  integer ( kind = 4 ) i
  logical nflag(np)
  real u(np)
  real uval
  real v(np)
  real vnorm
  real vscale
  real vval
  real x(np)
  real xtip
  real y(np)
  real ytip
!
  do i = 1,np

    if ( nflag(i) ) then

      uval = u(i)*dist(1)
      vval = v(i)*dist(2)
      vnorm = sqrt(uval**2+vval**2)

      if ( vnorm>0.00000001E+00 ) then
        xtip = x(i)+vscale*uval/vnorm
        ytip = y(i)+vscale*vval/vnorm
        call arrow(x(i),y(i),xtip,ytip)
      end if

    end if

  end do

  return
end
subroutine prcon(npcon,pcmax,pcmin)

!*****************************************************************************80
!
!! PRCON prints the value of the contour levels.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) npcon

  real pcmax
  real pcmin
  real pval
  integer ( kind = 4 ) i

  write ( *, * ) ' '
  write ( *, * ) 'PrCon: Number of contour levels is ',npcon
  write ( *, * ) ' '
  do i = 1,npcon
    pval = ((npcon-i)*pcmin+(i-1)*pcmax)/real(npcon-1)
    write(*,'(g14.4)')pval
  end do

  return
end
subroutine predat(filem,idata,istep,maxnp,np,nx,ny,nz,xpmax,xpmin,ypmax,ypmin)

!*****************************************************************************80
!
!! PREDAT opens a data file, gets the first line, containing
!  NX and NY, and positions the file at the first line
!  of cost data.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Parameters:
!
  implicit none

  character ( len = * )  filem
  integer ( kind = 4 ) idata
  integer ( kind = 4 ) istep
  integer ( kind = 4 ) maxnp
  integer ( kind = 4 ) np
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny
  integer ( kind = 4 ) nz
  real xpmax
  real xpmin
  real ypmax
  real ypmin

  if ( idata>0 ) then
    close(unit = 1)
    idata = 0
  end if

  open(unit = 1,file=filem,access='sequential',form='formatted', &
    status = 'old',err=10)

  read(1,*,end = 20)nx,ny,nz

  np = nx*ny
  write ( *, * ) '  The number of nodes is ',np
  if ( nz>1 ) then
    write ( *, * ) '  The number of "levels" is ',nz
  end if

  if ( np>maxnp ) then
    write ( *, * ) ' '
    write ( *, * ) 'PreDat - Serious error!'
    write ( *, * ) '  The number of nodes is too large.'
    write ( *, * ) '  NP = ',np
    write ( *, * ) '  Maximum allowed is ',maxnp
    idata = 0
    return
  end if

  xpmin = 0.0E+00
  ypmin = 0.0E+00
  xpmax = 1.0E+00
  ypmax = 1.0E+00

  idata = 1
  istep = 0

  return
!
!  Error: The input data file could not be opened.
!
10    continue
  write ( *, * ) ' '
  write ( *, * ) 'PreDat - Warning!'
  write ( *, * ) '  The input data file could not be opened.'
  idata = 0
  return
!
!  Error: NX, NY could not be read.
!
20    continue
  write ( *, * ) ' '
  write ( *, * ) 'PreDat - Warning!'
  write ( *, * ) '  NX, NY could not be read.'
  idata = 0
  return
end
subroutine prelm ( nelem, node )

!*****************************************************************************80
!
!! PRELM prints the element data.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) nelem

  integer ( kind = 4 ) i
  integer ( kind = 4 ) node(3,nelem)

  write ( *, * ) ' '
  write ( *, * ) 'PRELM'
  write ( *, * ) ' '
  do i = 1,nelem
    write(*,*)i,node(1,i),node(2,i),node(3,i)
  end do

  return
end
subroutine preplt(dev,filgrf,icmax,icmin,iplot,itable,lgopen)

!*****************************************************************************80
!
!! PREPLT should be called before doing each plot.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Parameters:
!
!    Input, CHARACTER*10 DEV.
!    The graphics output device to be used.  Current legal
!    values for DEV include:
!    cgmb - CGM binary file.
!    ps   - PostScript file.
!    xws  - X window screen (interactive).
!
!  ICMAX,
!  ICMIN  Input, integer ( kind = 4 ) ICMAX, ICMIN.
!         The maximum and minimum color indices to be used from
!         the color table.  The color table contains 256 colors,
!         but color indices 1 and 2 are black and white, and for some
!         reason, the predefined DRAWCGM tables generally only
!         use 2-200 for sensible colors.
!
!         Of course the entries in the color table are "off by one".
!         The first entry is for color 0, and the 256-th entry for
!         color 255.
!
!  IPLOT  Input/output, integer ( kind = 4 ) IPLOT.
!         The number of plots made so far.
!
!         PREPLT increments IPLOT by 1.
!
!  ITABLE Input, integer ( kind = 4 ) ITABLE.
!         The index of the color table to be used for color contour
!         plots.
!
!           1: Gray scale, black to white
!           2: Blue to yellow
!           3: Waves
!           4: Pseudospectral
!           5: Inverted pseudospectral
!           6: Blue to red
!
  implicit none

  integer ( kind = 4 ), parameter :: nval = 2

  character ( len = * )  dev
  character ( len = 40 ) filgrf
  integer ( kind = 4 ) icmax
  integer ( kind = 4 ) icmin
  integer ( kind = 4 ) icolor
  integer ( kind = 4 ) iplot
  integer ( kind = 4 ) itable
  logical lgopen
  logical, save :: linit = .false.
  real xval(nval)
  real yval(nval)
!
!  If it's the first picture, then
!
!    Choose an output device,
!    Give the output file a name,
!    Initialize the graphics package.
!
  if ( .not.linit ) then

    linit = .true.

    if ( dev==' ' ) then
      write ( *, * ) ' '
      write ( *, * ) 'PrePlt - Warning!'
      write ( *, * ) '  No output device was specified.'
      write ( *, * ) '  PostScript output will be generated.'
      dev = 'ps'
    end if

    call device(dev)

    if ( dev=='cgmb' ) then

      if ( filgrf==' ' ) then
        filgrf = 'slab.cgm'
      end if

      call outfil(filgrf)

    else if ( dev=='ps' ) then

      if ( filgrf==' ' ) then
        filgrf = 'slab.ps'
      end if

      call outfil(filgrf)

    end if

    if ( itable==0 ) then
      itable = 1
      call settab(icmax,icmin,itable)
    end if

    call grfini ( )
    lgopen = .true.

    xval(1) = 0.0E+00
    xval(2) = 1.0E+00
    yval(1) = 0.0E+00
    yval(2) = 1.0E+00
    call setscl(xval,yval,nval)

    icolor = 1
    call linclr(icolor)
    call filclr(icolor)

    iplot = 1
!
!  Else, issue a "new frame" command.
!
  else

    call newfrm

  end if

  iplot = iplot+1

  return
end
subroutine prcost ( np, nx, ny, p, x, y )

!*****************************************************************************80
!
!! PRCOST prints X, Y and the COST.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) np

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny
  real p(np)
  real x(np)
  real y(np)

  write ( *, * ) ' '
  write ( *, * ) 'PRCOST'
  write ( *, * ) ' '
  write ( *, * ) 'I, J, K, X(K), Y(K), P(K)'
  write ( *, * ) ' '
  k = 0
  do i = 1,nx
    write ( *, * ) ' '
    do j = 1,ny
      k = k+1
      write(*,'(3i4,4g14.6)')i,j,k,x(k),y(k),p(k)
    end do
  end do

  return
end
subroutine prnode(difx,dify,dpdxc,dpdyc,dpdxfd,dpdyfd,dpdxsn,dpdysn,drdx, &
  drdy,inode,np,nx,ny,p,x,y)

!*****************************************************************************80
!
!! PRNODE prints the values of certain quantities at a node.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) np

  real difx(np)
  real dify(np)
  real dpdxc(np)
  real dpdyc(np)
  real dpdxfd(np)
  real dpdyfd(np)
  real dpdxsn(np)
  real dpdysn(np)
  real drdx(np)
  real drdy(np)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) inode
  integer ( kind = 4 ) ival
  integer ( kind = 4 ) jval
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny
  real p(np)
  real x(np)
  real y(np)

  if ( inode>=1.and.inode<=np ) then
    ilo = inode
    ihi = inode
    write ( *, * ) ' '
    write ( *, * ) 'PRNODE: Information for node ',inode
  else
    ilo = 1
    ihi = np
    write ( *, * ) ' '
    write ( *, * ) 'PRNODE: Information for all nodes.'
  end if

  do i = ilo,ihi

    ival = (i/nx)+1
    jval = mod(i-1,ny)+1

    write ( *, * ) ' '
    write ( *, * ) 'Node:                   ',i
    write ( *, * ) 'Location:               ',x(i),y(i)
    write ( *, * ) 'Coordinates:            ',ival,jval
    write ( *, * ) 'Cost:                   ',p(i)
    write ( *, * ) 'C: FD cost gradient     ',dpdxc(i),dpdyc(i)
    write ( *, * ) 'P: Disc Sensitivities:  ',dpdxsn(i),dpdysn(i)
    write ( *, * ) 'Q: FD Sensitivities:    ',dpdxfd(i),dpdyfd(i)
    write ( *, * ) 'R: Est Derivatives:     ',drdx(i),drdy(i)
    write ( *, * ) 'D: P-Q:                 ',difx(i),dify(i)

  end do

  return
end
subroutine prset(dev,icmax,icmin,icolor,idata,istep,lcvec,ldvec,lncvec, &
  lndvec,lnpvec,lnqvec,lnrvec,lqvec,lrvec,maxobj,npcon,object,show,vmin)

!*****************************************************************************80
!
!! PRSET prints the value of some of the settings.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) maxobj

  character ( len = * )  dev
  integer ( kind = 4 ) i
  integer ( kind = 4 ) icmax
  integer ( kind = 4 ) icmin
  integer ( kind = 4 ) icolor(maxobj)
  integer ( kind = 4 ) idata
  integer ( kind = 4 ) istep
  logical lcvec
  logical ldvec
  logical s_eqi
  logical lncvec
  logical lndvec
  logical lnpvec
  logical lnqvec
  logical lnrvec
  logical lqvec
  logical lrvec
  integer ( kind = 4 ) npcon
  character ( len = 25 ) object(maxobj)
  logical show(maxobj)
  real vmin

  write ( *, * ) ' '
  if ( dev==' ' ) then
    write ( *, * ) 'No plotting device has been selected yet.'
  else if ( s_eqi(dev,'cgmb') ) then
    write ( *, * ) 'Output will be to a CGM binary file "slab.cgm".'
  else if ( s_eqi(dev,'ps') ) then
    write ( *, * ) 'Output will be to a PostScript file "slab.ps".'
  else if ( s_eqi(dev,'xws') ) then
    write ( *, * ) 'Output will be to an X window screen.'
  end if

  write ( *, * ) ' '
  if ( idata==0 ) then
    write ( *, * ) 'No data file is open.'
  else if ( idata==1 ) then
    write ( *, * ) 'The data file is open.'
  else if ( idata==2 ) then
    write ( *, * ) 'The data file is open.'
    write ( *, * ) 'We are on step number ',istep
  end if

  write ( *, * ) ' '
  write ( *, * ) 'Minimum color index:',icmin
  write ( *, * ) 'Maximum color index:',icmax

  write ( *, * ) ' '
  write ( *, * ) 'We ignore vectors of norm less than ',vmin
  write ( *, * ) ' '
  write ( *, * ) 'We are using ',npcon,' contour levels.'
  write ( *, * ) ' '
  write ( *, * ) 'Things we ARE drawing:'
  write ( *, * ) ' '

  if ( lcvec)write ( *, * ) 'C vectors'
  if ( lqvec)write ( *, * ) 'Q vectors'
  if ( lrvec)write ( *, * ) 'R vectors.'
  if ( ldvec)write ( *, * ) 'D vectors'

  if ( lncvec)write ( *, * ) 'Normalized C vectors'
  if ( lnpvec)write ( *, * ) 'Normalized discretized sensitivity gradients'
  if ( lnqvec)write ( *, * ) 'Normalized Q vectors.'
  if ( lnrvec)write ( *, * ) 'Normalized R vectors.'
  if ( lndvec)write ( *, * ) 'Normalized D vectors.'

  write ( *, * ) ' '
  write ( *, * ) 'Things we are NOT drawing:'
  write ( *, * ) ' '

  if ( .not.lcvec)write ( *, * ) 'C vectors'
  if ( .not.lqvec)write ( *, * ) 'Q vectors'
  if ( .not.lrvec)write ( *, * ) 'R vectors.'
  if ( .not.ldvec)write ( *, * ) 'D vectors'

  if ( .not.lncvec)write ( *, * ) 'Normalized C vectors'
  if ( .not.lnpvec)write ( *, * ) 'Normalized discretized sensitivity gradients.'
  if ( .not.lnqvec)write ( *, * ) 'Normalized Q vectors.'
  if ( .not.lnrvec)write ( *, * ) 'Normalized R vectors.'
  if ( .not.lndvec)write ( *, * ) 'Normalized D vectors.'

  write ( *, * ) ' '
  write ( *, * ) ' Name Color  Visible?'
  write ( *, * ) ' '
  do i = 1,maxobj
    write(*,'(1x,a25,2x,i5,2x,l1)')object(i),icolor(i),show(i)
  end do

  return
end
subroutine refbsp(q,dqdx,dqdy,detadx,detady,iq,dxsidx,dxsidy,eta,xsi)

!*****************************************************************************80
!
!! REFBSP evaluates one of the three linear basis functions,
!  and its X and Y derivatives, at a particular point (X,Y)
!  in a particular element, by referring to the corresponding
!  points (XSI,ETA) in the reference triangle.
!
!  It is assumed that we already know the value of the jacobian
!  of the isoparametric transformation between the (XSI, ETA) and
!  (X, Y) spaces.  The four entries of the jacobian are
!  symbolically named DETADX, DETADY, DXSIDX and DXSIDY, and
!  we know that the jacobian gives us the following relation
!  between derivatives with respect to XSI and ETA, and derivatives
!  with respect to X and Y:
!
!    dF/dX  =  dF/dXsi dXsi/dX + dF/dEta dEta/dX
!    dF/dY  =  dF/dXsi dXsi/dY + dF/dEta dEta/dY
!
!  Here is a graph of the (XSI, ETA) reference triangle we will
!  use.
!
!        ^
!        |
!      1 +        2
!        |       /|
!  ETA   |      / |
!        |     /  |
!      0 +    1---3
!        |
!        +----+---+--->
!             0   1
!
!              XSI
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Parameters:
!
!  Q,
!  DQDX,
!  DQDY   Output, double precision Q, DQDX, DQDY, the value of the basis
!         function, and its derivatives with respect to X and Y, at
!         the point (ETA,XSI).
!
!  DETADX,
!  DETADY Input, double precision DETADX, DETADY, the partial derivative
!         d ETA/d X and d ETA/d Y at (ETA,XSI).
!
!  IQ     Input, integer ( kind = 4 ) IQ, the local node number, between 1 and
!         3, whose basis function is being evaluated.
!
!  DXSIDX,
!  DXSIDY Input, double precision DXSIDX, DXSIDY, the partial derivative
!         d XSI/d X and d XSI/d Y at (ETA,XSI).
!
!  ETA,
!  XSI    Input, double precision ETA, XSI, the local coordinates of the
!         at which the basis information is desired.
!
  implicit none

  real detadx
  real detady
  real dqdeta
  real dqdx
  real dqdxsi
  real dqdy
  real dxsidx
  real dxsidy
  real eta
  integer ( kind = 4 ) iq
  real q
  real xsi

  if ( iq==1 ) then
    q = 1.0E+00-xsi
    dqdxsi = -1.0E+00
    dqdeta =  0.0E+00
  else if ( iq==2 ) then
    q = eta
    dqdxsi = 0.0E+00
    dqdeta = 1.0E+00
  else if ( iq==3 ) then
    q = xsi-eta
    dqdxsi = 1.0E+00
    dqdeta = -1.0E+00
  else if ( iq>=4.and.iq<=6 ) then
    q = 0.0E+00
    dqdxsi = 0.0E+00
    dqdeta = 0.0E+00
  else
    write ( *, * ) 'RefBSP - Fatal error!'
    write ( *, * ) '  Request for basis function IQ = ',iq
    write ( *, * ) '  but IQ must be between 1 and 6.'
    stop
  end if

  dqdx = dqdxsi*dxsidx+dqdeta*detadx
  dqdy = dqdxsi*dxsidy+dqdeta*detady

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
!
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
!
  nchar = len_trim ( s )

  if ( pwide <= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'S_PLOT - Serious error!'
    write ( *, * ) '  The plot width PWIDE is negative!'
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
    xcopy = x - 0.5 * nchar * cwide * cos ( angle * DEG_TO_RAD )
    ycopy = y - 0.5 * nchar * cwide * sin ( angle* DEG_TO_RAD )
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
subroutine setelm(nelem,node,nx,ny)

!*****************************************************************************80
!
!! SETELM sets up the element data.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) nelem

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ibase
  integer ( kind = 4 ) ielem
  integer ( kind = 4 ) j
  integer ( kind = 4 ) node(3,nelem)
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny

  ielem = 0
  do i = 1,nx-1
    ibase = (i-1)*ny
    do j = 1,ny-1
      ibase = ibase+1

      ielem = ielem+1
      node(1,ielem) = ibase
      node(2,ielem) = ibase+ny+1
      node(3,ielem) = ibase+ny

      ielem = ielem+1
      node(1,ielem) = ibase
      node(2,ielem) = ibase+1
      node(3,ielem) = ibase+ny+1

    end do
  end do

  return
end
subroutine settab(icmax,icmin,itable)

!*****************************************************************************80
!
!! SETTAB replaces SETCTB, the DRAWCGM routine for setting up
!  the color tables.
!
!  SETCTB does not work properly, at least on the IRIS.  Colors beyond
!  color number 201 are typically mangled.
!
!  So SETTAB sets the colors between ICMIN and ICMAX, which
!  should typically be 2 and 255.
!
!  SETTAB will also set the values of color 0 to white, and
!  color 1 to black.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Parameters:
!
!  ICMAX  Input, integer ( kind = 4 ) ICMAX, the maximum color index to be set.
!
!  ICMIN  Input, integer ( kind = 4 ) ICMIN, the minimum color index to be set.
!
!  ITABLE Input, integer ( kind = 4 ) ITABLE, the desired table.
!
!         1: low black to high white
!         2: low blue to high yellow
!         3: low red, high blue, with bands between.
!         4: low red, orange, green, high blue.
!         5: low white, blue, green, yellow, high orange.
!         6: low blue to high red
!         7: linear table between 2 user colors.
!         8: linear table between n user colors.
!
  implicit none

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
  integer ( kind = 4 ) itable
  real, parameter :: pi = 3.1415926E+00
  real rhi
  real rlo
  real rval
  real theta
!
!  1: Low black to high white
!
  if ( itable==1 ) then
    do i = icmin,icmax
      bval = real(i-icmin)/real(icmax-icmin)
      gval = real(i-icmin)/real(icmax-icmin)
      rval = real(i-icmin)/real(icmax-icmin)
      call setclr(i,bval,gval,rval)
    end do
!
!  2: Low blue to high yellow.
!
  else if ( itable==2 ) then
    do i = icmin,icmax
      rval = real(i-icmin)/real(icmax-icmin)
      gval = real(i-icmin)/real(icmax-icmin)
      bval = (icmax-i)/real(icmax-icmin)
      call setclr(i,bval,gval,rval)
    end do
!
!  3: Low red, high blue, with bands between.
!
  else if ( itable==3 ) then
    do i = icmin,icmax
      theta = 0.5E+00*pi*real(i-icmin)/real(icmax-icmin)
      rval = cos(theta)**2
      bval = sin(theta)**2
      gval = 0.8E+00*sin(10.0E+00*theta)**6
      call setclr(i,bval,gval,rval)
    end do
!
!  4: Low red, orange, green, high blue.
!
  else if ( itable==4 ) then
    do i = icmin,icmax
      theta = 4.0E+00*real(i-icmin)/real(icmax-icmin)
      rval = exp(-(theta-1.0E+00)**2)+exp(-(theta-4.0)**2)
      gval = exp(-(theta-2.0E+00)**2)+exp(-(theta-4.0)**2)
      bval = exp(-(theta-3.0E+00)**2)+exp(-(theta-4.0)**2)
      if ( rval>1.0)rval = 1.0E+00
      if ( gval>1.0)gval = 1.0E+00
      if ( bval>1.0)bval = 1.0E+00
      call setclr(i,bval,gval,rval)
    end do
!
!  5: Low white, blue, green, yellow, high orange.
!
  else if ( itable==5 ) then
    do i = icmin,icmax
      theta = 4.0E+00*real(icmax-i)/real(icmax-icmin)
      rval = exp(-(theta-1.0E+00)**2)+exp(-(theta-4.0)**2)
      gval = exp(-(theta-2.0E+00)**2)+exp(-(theta-4.0)**2)
      bval = exp(-(theta-3.0E+00)**2)+exp(-(theta-4.0)**2)
      if ( rval>1.0)rval = 1.0E+00
      if ( gval>1.0)gval = 1.0E+00
      if ( bval>1.0)bval = 1.0E+00
      call setclr(i,bval,gval,rval)
    end do
!
!  6: Low blue to high red
!
  else if ( itable==6 ) then
    do i = icmin,icmax
      rval = real(i-icmin)/real(icmax-icmin)
      gval = 0.0E+00
      bval = (icmax-i)/real(icmax-icmin)
      call setclr(i,bval,gval,rval)
    end do
!
!  7: Interpolate between two values.
!
  else if ( itable==7 ) then
    write ( *, * ) ' '
    write ( *, * ) 'Enter (Rlo, Glo, Blo), (Rhi, Ghi, Bhi)'
    write ( *, * ) 'Note: (0,0,0) is black, (1,1,1) is white!'
    write ( *, * ) ' '
    read(*,*)rlo,glo,blo,rhi,ghi,bhi
    write(17,*)rlo,glo,blo,rhi,ghi,bhi
    if ( rlo<0.0E+00)rlo = 0.0E+00
    if ( rhi>1.0E+00)rhi = 1.0E+00
    if ( glo<0.0E+00)glo = 0.0E+00
    if ( ghi>1.0E+00)ghi = 1.0E+00
    if ( blo<0.0E+00)blo = 0.0E+00
    if ( bhi>1.0E+00)bhi = 1.0E+00
    do i = icmin,icmax
      rval = (rlo*(icmax-i)+rhi*(i-icmin))/real(icmax-icmin)
      gval = (glo*(icmax-i)+ghi*(i-icmin))/real(icmax-icmin)
      bval = (blo*(icmax-i)+bhi*(i-icmin))/real(icmax-icmin)
      call setclr(i,bval,gval,rval)
    end do
!
!  8: Interpolate between several values.
!
  else if ( itable==8 ) then

    icol1 = icmin
    write ( *, * ) 'Enter (R, G, B) for color index ',icol1
    write ( *, * ) '      (0, 0, 0) is black.'
    write ( *, * ) '      (1, 1, 1) is white.'
    read(*,*)rlo,glo,blo
    write(17,*)rlo,glo,blo
    if ( rlo<0.0E+00)rlo = 0.0E+00
    if ( glo<0.0E+00)glo = 0.0E+00
    if ( blo<0.0E+00)blo = 0.0E+00

10      continue

    write ( *, * ) 'Enter index of next color to set'
    write ( *, * ) 'between ',icol1+1,' and ',icmax
    read(*,*)icol2
    write(17,*)icol2

    if ( icol2<=icol1.or.icol2>icmax ) then
      write ( *, * ) 'SetTab - Warning!'
      write ( *, * ) '  Your color index was not accepted!'
      go to 10
    end if

    write ( *, * ) ' '
    write ( *, * ) 'Enter (R, G, B) for color index ',icol2
    read(*,*)rhi,ghi,bhi
    write(17,*)rhi,ghi,bhi

    if ( rhi>1.0E+00)rhi = 1.0E+00
    if ( ghi>1.0E+00)ghi = 1.0E+00
    if ( bhi>1.0E+00)bhi = 1.0E+00

    do i = icol1,icol2
      rval = (rlo*(icol2-i)+rhi*(i-icol1))/real(icol2-icol1)
      gval = (glo*(icol2-i)+ghi*(i-icol1))/real(icol2-icol1)
      bval = (blo*(icol2-i)+bhi*(i-icol1))/real(icol2-icol1)
      call setclr(i,bval,gval,rval)
    end do

    if ( icol2<icmax ) then
      icol1 = icol2
      rlo = rhi
      glo = ghi
      blo = bhi
      go to 10
    end if
!
!  Unknown table.
!
  else
    write ( *, * ) ' '
    write ( *, * ) 'SetTab - Fatal error!'
    write ( *, * ) '  Legal color table indices are '
    write ( *, * ) '  between 1 and 8.  Your value was ',itable
  end if
!
!  Background color 0 is to be white.
!
  i = 0
  rval = 1.0E+00
  gval = 1.0E+00
  bval = 1.0E+00
  call setclr(i,rval,gval,bval)
!
!  Foreground color 1 is to be black.
!
  i = 1
  rval = 0.0E+00
  gval = 0.0E+00
  bval = 0.0E+00
  call setclr(i,rval,gval,bval)

  return
end
subroutine setxy(np,nx,ny,x,xpmax,xpmin,y,ypmax,ypmin)

!*****************************************************************************80
!
!! SETXY sets up the X and Y coordinates for the nodes.
!
!  Here is a sample ordering for the case NX = 3, NY=5:
!
!  Y = YMAX  5 10 15
!          4  9 14
!          3  8 13
!          2  7 12
!  Y = YMIN  1  6 11
!
!      X = XMIN   X=XMAX
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) np

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ip
  integer ( kind = 4 ) j
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny
  real x(np)
  real xpmax
  real xpmin
  real y(np)
  real ypmax
  real ypmin

  ip = 0
  do i = 1,nx
    do j = 1,ny
      ip = ip+1
      x(ip) = (real(nx-i)*xpmin+real(i-1)*xpmax)/real(nx-1)
      y(ip) = (real(ny-j)*ypmin+real(j-1)*ypmax)/real(ny-1)
    end do
  end do

  return
end
subroutine square(x,y,csize,filled)

!*****************************************************************************80
!
!! SQUARE draws a square.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Parameters:
!
  implicit none

  real csize
  logical filled
  real x
  real xval(5)
  real y
  real yval(5)

  xval(1) = x-csize
  yval(1) = y-csize
  xval(2) = x+csize
  yval(2) = y-csize
  xval(3) = x+csize
  yval(3) = y+csize
  xval(4) = x-csize
  yval(4) = y+csize
  xval(5) = x-csize
  yval(5) = y-csize

  call plylin(5,xval,yval)

  if ( filled ) then
    call plygon(4,xval,yval)
  end if

  return
end
subroutine stream3(ie,in,nelem,node,np,s,u,v,xc,yc)

!*****************************************************************************80
!
!! STREAM3 assigns the value of the stream function to each node
!  in a finite element grid, given the horizontal and vertical
!  velocities at those nodes.
!
!  The stream function PSI(X,Y) is related to the velocity (U,V) by
!
!    Curl PSI  =  (U,V)
!
!  That is,
!
!    U  =   d PSI/d Y
!    V  =  -d PSI/d X
!
!  To recover the stream function, we pick node ILOC = 1 in
!  element IELEM = 1, with global index IGLOB, and set PSI there to
!  zero.
!
!  Now we check all other nodes in the element, using the fact that
!  PSI is defined by a line integral:
!
!    PSI(XSI1,ETA1)
!
!     =  PSI(XSI0,ETA0) + Integral (S0 to S1) d PSI/d S  dS.
!
!     =  PSI(XSI0,ETA0) + Integral (S0 to S1)
!      ( dPSI/dX * dX/dXSI + dPSI/dY * dY/dXSI dXSI/dS
!      + dPSI/dX * dX/dETA + dPSI/dY * dY/dETA dETA/dS ) dS
!
!     =  PSI(XSI0,ETA0) + Integral (S0 to S1)
!      ( (-V dX/dXSI + U dY/dXSI) dXSI/dS
!      + (-V dX/dETA + U dY/dETA) dETA/dS ) dS.
!
!  Once all the nodes in this element are done, we find another
!  element, which has some nodes done and some not.  We compute
!  the line integrals from a known node to the unknown nodes
!  to complete this element.
!
!  This process is repeated until all elements are complete.
!
!  This routine makes no assumptions about the global numbering of
!  the nodes and elements.  Instead, once it has computed all the
!  stream function values for a given element, it searches for any
!  other element which has some nodes with defined values and some
!  without, because that is an element which can be "finished".
!  This process is repeated until all elements are done.  The only
!  way it could fail is if the elements are not simply connected.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Parameters:
!
!  IE     Workspace, integer IE(NELEM).
!
!  IS     Workspace, integer IS(NP).
!
!  NELEM  Input, integer ( kind = 4 ) NELEM, the actual number of elements.
!
!  NODE   Input, integer ( kind = 4 ) NODE(3,NELEM), contains the numbers
!         of the nodes that make up each element.  Element number
!         I is associated with nodes NODE(1,I) through NODE(3,I).
!
!  NP     Input, integer ( kind = 4 ) NP, the number of nodes.
!
!  S      Output, real S(NP), the value of the stream function at
!         each node.  Node 1 will always have a zero value of S.
!
!  U,
!  V      Input, real U(NP), V(NP), the value of the horizontal and
!         vertical velocities at each node.
!
!  XC,
!  YC     Input, real XC(NP), YC(NP), the X and Y coordinates of
!         each node.
!
  implicit none

  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) np

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
  integer ( kind = 4 ) nexnod(3,5)
  integer ( kind = 4 ) next
  integer ( kind = 4 ) node(3,nelem)
  real s(np)
  real sinc
  real u(np)
  real v(np)
  real xc(np)
  real yc(np)
!
  nexnod(1,1) = 2
  nexnod(1,2) = 3

  nexnod(2,1) = 3
  nexnod(2,2) = 1

  nexnod(3,1) = 1
  nexnod(3,2) = 2
!
!  Zero out the stream function,
!  and the vector that records that a node has been done.
!
  s(1:np) = 0.0E+00
  in(1:np) = 0
!
!  Zero out the vector that records that an element is done.
!
  do i = 1,nelem
    ie(i) = 0
  end do
!
!  Set S of local node 1 of element 1 to 0.
!
  ielem = 1
  iloc = 1
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

  do next = 1,2

    jloc = kloc
    jglob = kglob

    kloc = nexnod(iloc,next)
    kglob = node(kloc,ielem)

    if ( in(kglob)==0 ) then
      call doofus3(ielem,jloc,nelem,node,np,sinc,u,v,xc,yc)
      s(kglob) = s(jglob)+sinc
      in(kglob) = 1
    end if

  end do
!
!  Now that all the nodes in element IELEM have been done, mark the
!  element.
!
  ie(ielem) = 1
!
!  Now seek an element which is not known to be done, containing
!  some elements which are known and some which are not.
!
  do i = 1,nelem

    if ( ie(i)==0 ) then

      l1 = .false.
      l0 = .false.

      do j = 1,3
        jglob = node(j,i)
        if ( in(jglob)==0 ) then
          l0 = .true.
        else
          iloc = j
          l1 = .true.
        end if
      end do

      if ( l0.and.l1 ) then
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
  integer ( kind = 4 )   ( kind = 4 ) d
  integer ( kind = 4 )   ( kind = 4 ) h
  integer ( kind = 4 )   ( kind = 4 ) m
  integer ( kind = 4 )   ( kind = 4 ) mm
  character ( len = 9  ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer ( kind = 4 )   ( kind = 4 ) n
  integer ( kind = 4 )   ( kind = 4 ) s
  integer ( kind = 4 )   ( kind = 4 ) values(8)
  integer ( kind = 4 )   ( kind = 4 ) y

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
subroutine trans3(det,detadx,detady,dxdeta,dxdxsi,dxsidx,dxsidy, &
  dydeta,dydxsi,eta,ielem,nelem,node,np,xc,xsi,yc)

!*****************************************************************************80
!
!! TRANS3 calculates the linear transformation which maps the
!  reference element in (XSI,ETA) space into a particular
!  isoparametric element in (X,Y) space.
!
!  We know everything about the isoparametric element once we
!  specify the location of its six nodes.
!
!  TRANS computes the entries of the jacobian of the transformation
!  and the determinant of the jacobian.  Essentially, the jacobian
!  records the relationship between derivatives with respect to XSI
!  and ETA and a point in the reference element, and derivatives
!  with respect to X and Y of the same function as defined in the
!  isoparametric element.
!
!  The four entries of the jacobian are symbolically named DETADX,
!  DETADY, DXSIDX and DXSIDY, and we know that the jacobian gives
!  us the following relation between derivatives with respect to
!  XSI and ETA, and derivatives with respect to X and Y:
!
!    d F(X,Y)/dX     (d XSI/dX  d ETA/dX )   ( d F(XSI, ETA)/d XSI )
!    d F(X,Y)/dY   =   (d XSI/dY  d ETA/dY ) * ( d F(XSI, ETA)/d ETA )
!
!  Here is a graph of the (XSI, ETA) reference triangle we will
!  use.
!
!        ^
!        |
!      1 +        2
!        |       /|
!  ETA   |      / |
!        |     /  |
!      0 +    1---3
!        |
!        +----+---+--->
!             0   1
!
!            XSI
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Parameters:
!
!  DET    Output, real DET, the determinant of the jacobian of the
!         transformation between the reference and isoparametric
!         elements.
!
!  DETADX,
!  DETADY Output, real DETADX, DETADY, the partial derivatives
!         d ETA/d X and d ETA/d Y at (XSI,ETA).
!
!  DXDETA,
!  DXDXSI Output, real DXDETA, DXDXSI, the partial derivatives
!         d X/d ETA, d X/d XSI evaluated at (XSI,ETA).
!
!  DXSIDX,
!  DXSIDY Output, real DXSIDX, DXSIDY, the partial derivatives
!         d XSI/d X and d XSI/d Y at (XSI,ETA).
!
!  DYDETA,
!  DYDXSI Output, real DYDETA, DYDXSI, the partial derivatives
!         d Y/d ETA, d Y/d XSI evaluated at (XSI,ETA).
!
!  ETA    Input, real ETA, the ETA coordinate of the point.
!
!  IELEM  Input, integer ( kind = 4 ) IELEM, the number of the isoparametric
!         element we are examining.
!
!  NELEM  Input, integer ( kind = 4 ) NELEM, the number of elements.
!
!  NODE   Input, integer ( kind = 4 ) NODE(NELEM,6), contains the numbers
!         of the nodes that make up each element.  Element number
!         I is associated with nodes NODE(I,1) through NODE(I,6).
!
!  NP     Input, integer ( kind = 4 ) NP, the number of nodes.
!
!  XC     Input, real XC(NP), the X coordinates of the nodes.
!
!  XSI    Input, real XSI, the XSI coordinate of the point.
!
!  YC     Input, real YC(NP), the Y coordinates of the nodes.
!
  implicit none

  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) np

  real det
  real detadx
  real detady
  real dxdeta
  real dxdxsi
  real dxsidx
  real dxsidy
  real dydeta
  real dydxsi
  real eta
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ielem
  integer ( kind = 4 ) node(3,nelem)
  real x(3)
  real xc(np)
  real xsi
  real xval
  real y(3)
  real yc(np)
  real yval
!
!  Pick off the X, Y coordinates of the nodes and store them
!  in two short lists.
!
  do i = 1,3
    x(i) = xc(node(i,ielem))
    y(i) = yc(node(i,ielem))
  end do
!
!  Set the coefficients in the transformation
!
!    (XSI,ETA) --> (X,Y).
!
!  The mapping has the form:
!
!    X(XSI,ETA)  =  (1-XSI)*X1 + XSI * ETA * X2 + XSI * (1-ETA) * X3
!
!    Y(XSI,ETA)  =  (1-XSI)*Y1 + XSI * ETA * Y2 + XSI * (1-ETA) * Y3
!
  xval = (1.0E+00-xsi)*x(1)+xsi*eta*x(2)+xsi*(1.0E+00-eta)*x(3)
  yval = (1.0E+00-xsi)*y(1)+xsi*eta*y(2)+xsi*(1.0E+00-eta)*y(3)
!
!  Compute the partial derivatives at the point (XSI,ETA).
!  This is the jacobian matrix
!
!    J: (XSI,ETA) --> (X,Y).
!
  dxdxsi = -x(1)+eta*x(2)+(1.0E+00-eta)*x(3)
  dxdeta = xsi*x(2)-xsi*x(3)

  dydxsi = -y(1)+eta*y(2)+(1.0E+00-eta)*y(3)
  dydeta = xsi*y(2)-xsi*y(3)
!
!  Compute the determinant of the jacobian matrix:
!
!    J: (XSI,ETA) --> (X,Y)
!
  det = dxdxsi*dydeta-dxdeta*dydxsi
!
!  Watch out for a zero determinant.
!
  if ( det == 0.0E+00 ) then
    write ( *, * ) ' '
    write ( *, * ) 'Trans3 - Fatal error!'
    write ( *, * ) '  The jacobian J: (XSI,ETA) --> (X,Y) is singular!'
    write ( *, * ) '  This occurred for element number ',ielem
    write ( *, * ) '  Local coordinates:',xsi,eta
    write ( *, * ) '  Global coordinates:',xval,yval
    write ( *, * ) ' '
    write ( *, * ) '  The X, Y nodes were:'
    write ( *, * ) ' '
    do i = 1,3
      write(*,*)x(i),y(i)
    end do

    stop
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
  dxsidx =  dydeta/det
  dxsidy = -dxdeta/det

  detadx = -dydxsi/det
  detady =  dxdxsi/det

  return
end
subroutine tricol(jcmax,jcmin,ncon,nelem,nflag,node,np,s,smax,smin,x,y)

!*****************************************************************************80
!
!! TRICOL uses color to indicate all the points which have a
!  function value greater than a given value.
!
!  TRICOL is used for quantities associated with a three
!  node element.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) np

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) i3
  integer ( kind = 4 ) jcmax
  integer ( kind = 4 ) jcmin
  integer ( kind = 4 ) ncon
  logical nflag(np)
  integer ( kind = 4 ) node(3,nelem)
  real s(np)
  real smax
  real smin
  real x(np)
  real y(np)

  write ( *, * ) 'NP = ',np
  write ( *, * ) 'NELEM = ',nelem
!
!  Draw the contour line by searching over each element.
!
  do i = 1,nelem

    i1 = node(1,i)
    i2 = node(2,i)
    i3 = node(3,i)

    if ( nflag(i1).and.nflag(i2).and.nflag(i3) ) then

      call trilhk(i1,i2,i3,jcmax,jcmin,ncon,np,s,smax,smin,x,y)

    end if

  end do

  return
end
subroutine trilhk(i1,i2,i3,jcmax,jcmin,ncon,np,s,smax,smin,x,y)

!*****************************************************************************80
!
!! TRILHK is given a (non-isoperimetric) triangle, formed by nodes I1,
!  I2, and I3 and the value of the quantity S at these nodes.
!
!  That portion of the triangle which is greater than contour value
!  value SVAL(I) but less than SVAL(I+1) is to be filled in with
!  color I.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) np

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) i3
  integer ( kind = 4 ) ih
  integer ( kind = 4 ) il
  integer ( kind = 4 ) im
  integer ( kind = 4 ) jcmax
  integer ( kind = 4 ) jcmin
  integer ( kind = 4 ) jcolor
  integer ( kind = 4 ) ncon
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
  real s1
  real s2
  real s3
  real sc1
  real sc2
  real sh
  real sl
  real sm
  real smax
  real smin
  real x(np)
  real xh
  real xl
  real xm
  real xpoly(5)
  real y(np)
  real yh
  real yl
  real ym
  real ypoly(5)

  s1 = s(i1)
  s2 = s(i2)
  s3 = s(i3)

  if ( s1<=s2.and.s2<=s3 ) then
    il = i1
    im = i2
    ih = i3
  else if ( s1<=s3.and.s3<=s2 ) then
    il = i1
    im = i3
    ih = i2
  else if ( s2<=s1.and.s1<=s3 ) then
    il = i2
    im = i1
    ih = i3
  else if ( s2<=s3.and.s3<=s1 ) then
    il = i2
    im = i3
    ih = i1
  else if ( s3<=s1.and.s1<=s2 ) then
    il = i3
    im = i1
    ih = i2
  else if ( s3<=s2.and.s2<=s1 ) then
    il = i3
    im = i2
    ih = i1
  end if

  sl = s(il)
  sm = s(im)
  sh = s(ih)

  xl = x(il)
  xm = x(im)
  xh = x(ih)

  yl = y(il)
  ym = y(im)
  yh = y(ih)

!     do i = 0,ncon
!
!       sc1 = ((ncon+1-i)*smin+i*smax)/real(ncon+1)
!       sc2 = ((ncon-i)*smin+(i+1)*smax)/real(ncon+1)

  do i = 1,ncon-1
    sc1 = ((ncon-i)*smin+(i-1)*smax)/real(ncon-1)
    sc2 = ((ncon-i-1)*smin+i*smax)/real(ncon-1)
!
!  Check that some data in the triangle lies in the range
!  [SC1,SC2).
!
    if ( max(sl,sc1)<min(sh,sc2) ) then

      jcolor = int( ((ncon-i)*jcmin+i*jcmax)/real(ncon) )
!         jcolor = int(((ncon-i)*jcmin+i*jcmax)/real(ncon))
      call filclr(jcolor)
!
!  Take care of possibility that entire triangle lies in the contour.
!
      if ( sc1<=sl.and.sh<sc2 ) then

        npts = 3
        xpoly(1) = xl
        ypoly(1) = yl
        xpoly(2) = xm
        ypoly(2) = ym
        xpoly(3) = xh
        ypoly(3) = yh

        call plygon(npts,xpoly,ypoly)
!
!  Find (PXOLD,PYOLD) and (QXOLD,QYOLD), where the line S = SC1 crosses
!  the triangle.
!
      else

        call cross(px,py,qx,qy,sl,sm,sh,sc1,xl,xm,xh,yl,ym,yh)

        pxold = px
        pyold = py
        qxold = qx
        qyold = qy
!
!  Find (PX,PY) and (QX,QY), where the line S = SC2 crosses the triangle.
!
        call cross(px,py,qx,qy,sl,sm,sh,sc2,xl,xm,xh,yl,ym,yh)
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

        if ( sc1<=sm.and.sm<=sc2 ) then
          npts = 5
          xpoly(5) = xm
          ypoly(5) = ym
        end if

        call plygon(npts,xpoly,ypoly)

      end if

    end if

  end do

  return
end
subroutine cross(px,py,qx,qy,sl,sm,sh,sval,xl,xm,xh,yl,ym,yh)

!*****************************************************************************80
!
!! CROSS finds the two places where the value S = SVAL occurs on a
!  triangle.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Parameters:
!
  implicit none

  real px
  real py
  real qx
  real qy
  real sl
  real sm
  real sh
  real sval
  real xl
  real xm
  real xh
  real yl
  real ym
  real yh

  if ( sval<sl ) then

    px = xl
    py = yl
    qx = xl
    qy = yl

  else if ( sval>=sh ) then

    px = xh
    py = yh
    qx = xh
    qy = yh

  else

    if ( sval<sm ) then
      px = xl+(sval-sl)*(xm-xl)/(sm-sl)
      py = yl+(sval-sl)*(ym-yl)/(sm-sl)
    else
      px = xm+(sval-sm)*(xh-xm)/(sh-sm)
      py = ym+(sval-sm)*(yh-ym)/(sh-sm)
    end if

    qx = xl+(sval-sl)*(xh-xl)/(sh-sl)
    qy = yl+(sval-sl)*(yh-yl)/(sh-sl)

  end if

  return
end
subroutine ueval3(detadx,detady,dxsidx,dxsidy,eta,ielem,nelem,node,np, &
  u,uval,v,vval,xsi)

!*****************************************************************************80
!
!! UEVAL3 evaluates the velocities at an arbitrary point in a
!  given three node element.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Parameters:
!
!  DETADX,
!  DETADY,
!  DXSIDX,
!  DXSIDY Input, real DETADX, DETADY, DXSIDX, DXSIDY, the derivatives
!         d ETA/d X, d ETA/d Y, d XSI/d X and d XSI/d Y at (XSI,ETA).
!
!  ETA    Input, real ETA, the second local coordinate of the point
!         where the velocity is desired.
!
!  IELEM  Input, integer ( kind = 4 ) IELEM, the number of the element in which the
!         point (XSI,ETA) lies.
!
!  MAXELM Input, integer ( kind = 4 ) MAXELM, the maximum number of elements.
!
!  NODE   Input, integer ( kind = 4 ) NODE(3,MAXELM), contains, for each
!         element, the nodes that belong to that element.
!
!  NP     Input, integer ( kind = 4 ) NP, the number of nodes.
!
!  U      Input, real U(NP), the horizontal velocity at each node.
!
!  UVAL   Output, real UVAL, the horizontal velocity at the point with
!         local coordinates (XSI,ETA).
!
!  V      Input, real V(NP), the vertical velocity at each node.
!
!  VVAL   Output, real VVAL, the vertical velocity at the point
!         with local coordinates (XSI,ETA).
!
!  XSI    Input, real XSI, the first local coordinate of the point
!         where the velocity is desired.
!
  implicit none

  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) np

  real detadx
  real detady
  real dqdx
  real dqdy
  real dxsidx
  real dxsidy
  real eta
  integer ( kind = 4 ) ielem
  integer ( kind = 4 ) iq
  integer ( kind = 4 ) jq
  integer ( kind = 4 ) node(3,nelem)
  real q
  real u(np)
  real uval
  real v(np)
  real vval
  real xsi

  uval = 0.0E+00
  vval = 0.0E+00

  do iq = 1,3

    call refbsp(q,dqdx,dqdy,detadx,detady,iq,dxsidx,dxsidy,eta,xsi)

    jq = node(iq,ielem)
    uval = uval+q*u(jq)
    vval = vval+q*v(jq)

  end do

  return
end
subroutine vector(dist,nflag,np,u,v,vscale,x,y)

!*****************************************************************************80
!
!! VECTOR draws a vector field.
!
!  Discussion:
!
!    An arrow pointing in the direction (U(I), V(I)) is drawn at the
!    point (X(I), Y(I)).  The arrow's length is scaled by VSCALE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Parameter:
!
!    integer ( kind = 4 ) NP, the number of nodes.
!
!    real VSCALE, a scale factor for vectors.
!
!    real U(NP), the horizontal vector component at node I.
!
!    real V(NP), the vertical vector component at node I.
!
!    real X(NP), the X coordinates of the nodes.
!
!    real Y(NP), the Y coordinates of the nodes.
!
  implicit none

  integer ( kind = 4 ) np

  real dist(2)
  integer ( kind = 4 ) i
  logical nflag(np)
  real u(np)
  real v(np)
  real vnorm
  real vscale
  real x(np)
  real xtip
  real y(np)
  real ytip

  do i = 1,np

    if ( nflag(i) ) then

      vnorm = sqrt(u(i)**2+v(i)**2)

      if ( vnorm > 0.0000001E+00 ) then
        xtip = x(i)+vscale*u(i)*dist(1)
        ytip = y(i)+vscale*v(i)*dist(2)
        call arrow(x(i),y(i),xtip,ytip)
      end if

    end if

  end do

  return
end
subroutine vrange ( nv, vmax, vx, vy, dist )

!*****************************************************************************80
!
!! VRANGE computes the maximum absolute value in a pair of vectors.
!
!  Discussion:
!
!    The vectors are implicitly scaled.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 February 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NV, the number of entries in VX and VY.
!
!    Output, real VMAX, the maximum of ABS(VX(I)*DIST(1))
!    and ABS(VY(I)*DIST(2)) over all indices I.
!
!    Input, real VX(NV), VY(NV), the vectors to be examined.
!
!    Input, real DIST(2), scaling factors for VX and VY.
!
  implicit none

  integer ( kind = 4 ) nv

  real dist(2)
  integer ( kind = 4 ) i
  real temp
  real vmax
  real vx(nv)
  real vy(nv)

  if ( dist(1) == 0.0E+00 ) then
    write ( *, * ) ' '
    write ( *, * ) 'VRange - Fatal error!'
    write ( *, * ) '  The divisor DIST(1) = ',dist(1)
    stop
  end if

  if ( dist(2) == 0.0E+00 ) then
    write ( *, * ) ' '
    write ( *, * ) 'VRange - Fatal error!'
    write ( *, * ) '  The divisor DIST(2) = ',dist(2)
    stop
  end if

  vmax = 0.0E+00

  do i = 1, nv

    temp = abs(vx(i)*dist(1))
    vmax = max(vmax,temp)

    temp = abs(vy(i)*dist(2))
    vmax = max(vmax,temp)

  end do

  return
end

