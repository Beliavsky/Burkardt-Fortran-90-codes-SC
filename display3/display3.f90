program main

!*****************************************************************************80
!
!! MAIN is the main program for DISPLAY3.
!
!  Discussion:
!
!    DISPLAY3 reads graphics data from FLOW programs and makes plots.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
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
!  "Primitive" parameters, that depend on nothing else.
!
  integer ( kind = 4 ), parameter :: maxbou = 500
  integer ( kind = 4 ), parameter :: maxnpe = 6
  integer ( kind = 4 ), parameter :: maxnx = 81
  integer ( kind = 4 ), parameter :: maxny = 51
  integer ( kind = 4 ), parameter :: maxobj = 37
  integer ( kind = 4 ), parameter :: maxpar = 15
!
!  Parameters that depend on primitive parameters.
!
  integer ( kind = 4 ), parameter :: maxelm = 2*(maxnx-1)*(maxny-1)
  integer ( kind = 4 ), parameter :: maxnp = (2*maxnx-1)*(2*maxny-1)
  integer ( kind = 4 ), parameter :: maxsen = 2*maxpar

  character ( len = 10 ) arrow
  real bval
  character ( len = 6 ) chrint
  character ( len = 80 ) command
  real cp(maxnp)
  real delx
  real dely
  character ( len = 10 ) dev
  real dudxn(maxnp)
  real dudyn(maxnp)
  real dvdxn(maxnp)
  real dvdyn(maxnp)
  logical echo
  logical eflag(maxelm)
  logical eflagu(maxelm)
  character ( len = 2 )  eqn(3,maxnp)
  real etaref(maxnpe)
  character ( len = 80 ) fildat
  character ( len = 80 ) filgrf
  character ( len = 80 ) filelm
  character ( len = 80 ) filinp
  character ( len = 20 ) filtyp
  real grace
  real gval
  integer ( kind = 4 ) i
  integer ( kind = 4 ) icmax
  integer ( kind = 4 ) icmin
  integer ( kind = 4 ) icolor(maxobj)
  integer ( kind = 4 ) icomp
  integer ( kind = 4 ) idata
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) ifile
  logical inside
  integer ( kind = 4 ) iplot
  character isay
  integer ( kind = 4 ) iset
  integer ( kind = 4 ) isotri(maxelm)
  integer ( kind = 4 ) itable
  integer ( kind = 4 ) itemp
  integer ( kind = 4 ) iwork1(maxelm)
  integer ( kind = 4 ) iwork2(maxnp)
  integer ( kind = 4 ) iwrite
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jbound(5,maxbou)
  integer ( kind = 4 ) jcmax
  integer ( kind = 4 ) jcmin
  integer ( kind = 4 ) jfile
  integer ( kind = 4 ) jtemp
  integer ( kind = 4 ) k
  character ( len = 30 ) labelx
  character ( len = 30 ) labely
  logical lbar
  integer ( kind = 4 ) lenc
  integer ( kind = 4 ) lens
  integer ( kind = 4 ) lent
  integer ( kind = 4 ) line(maxobj)
  logical lppro
  logical lptpro
  logical lupro
  logical lutpro
  logical lvpro
  logical lvtpro
  integer ( kind = 4 ) nbound
  integer ( kind = 4 ) ncon
  integer ( kind = 4 ) nelem
  logical nflag(maxnp)
  logical nflag0(maxnp)
  integer ( kind = 4 ) node(maxnpe,maxelm)
  logical none
  integer ( kind = 4 ) np
  integer ( kind = 4 ) npar
  integer ( kind = 4 ) npe
  integer ( kind = 4 ) nprof(2*maxny-1)
  integer ( kind = 4 ) nsen
  integer ( kind = 4 ) nset
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) nxskip
  integer ( kind = 4 ) ny
  integer ( kind = 4 ) nyskip
  character ( len = 30 ) object(maxobj)
  logical ovrlay
  real p(maxnp,0:maxsen)
  real para(maxpar)
  real ptar(maxnp)
  real rho(maxnp)
  real rmach(maxnp)
  real rval
  real s(maxnp)
  logical s_eqi
  real s2(maxnp)
  real scalee
  real scalen
  real scalev
  logical show(maxobj)
  real smax
  real smin
  real srange
  character ( len = 80 ) string
  real t(maxnp)
  real temp
  character ( len = 40 ) title
  character ( len = 40 ) title2
  real u(maxnp,0:maxsen)
  real utar(maxnp)
  real v(maxnp,0:maxsen)
  real vtar(maxnp)
  real x1max
  real x1maxc
  real x1min
  real x1minc
  real x2max
  real x2min
  real x4max
  real x4min
  real xc(maxnp)
  real xmax
  real xmin
  real xprof
  real xsiref(maxnpe)
  real xsmax
  real xsmin
  real xtmax
  real xtmin
  real y1max
  real y1maxc
  real y1min
  real y1minc
  real y2max
  real y2min
  real y4max
  real y4min
  real yc(maxnp)
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
  call hello ( maxbou, maxelm, maxnp, maxnpe, maxnx, maxny, maxobj, &
    maxpar, maxsen )
!
!  Set initial values.
!
  call init ( arrow, cp, delx, dely, dev, dudxn, dudyn, dvdxn, dvdyn, echo, &
    eflag, eflagu, eqn, etaref, fildat, filgrf,filinp,filtyp,grace,icmax,icmin, &
    icolor,icomp,idata,ifile,iplot,iset,itable,iwrite,jcmax, &
    jcmin,labelx,labely,lbar,line,lppro,lptpro,lupro, &
    lutpro,lvpro,lvtpro,maxelm,maxnp,maxnpe,maxny,maxobj, &
    maxpar,maxsen,nbound,ncon,nelem,nflag,nflag0,node, &
    np,npe,nprof,nsen,nset,nxskip,ny,nyskip,object,ovrlay,p,rho,rmach,scalee, &
    scalen,scalev,show,smax,smin,title,title2,u,v,x1max,x1min, &
    x2max,x2min,x4max,x4min,xc,xprof,xsiref,xsmax,xsmin,y1max, &
    y1min,y2max,y2min,y4max,y4min,yc,ysmax,ysmin)
!
!  Open the file to contain a copy of the user input commands.
!
  open ( unit = 17, file = filinp, status = 'replace', &
    access = 'sequential', &
    form = 'formatted' )
!
!  Get command from user
!
10    continue

  write(*,*)' '
  write(*,*)'? ("H" for help)'
  read(*,'(a)',end = 80,err=80)command
  call s_blank_delete ( command )
  write ( 17, '(a)' ) trim ( command )
  if ( echo ) then
    write ( *, '(a)' ) trim ( command )
  end if

  if ( command == ' ' ) then
    go to 10
  end if
!
!  A: advance, read next set of data from file.
!
  if ( s_eqi ( command,'a') ) then

    if ( s_eqi ( filtyp, 'FLOW' ) ) then
 
      call advanc ( delx,dely,echo,eflag,eqn,grace,ifile,iset,isotri, &
        jbound,maxbou,maxelm,maxnp,maxnpe,maxny,maxpar,maxsen, &
        nbound,nelem,nflag,nflag0,node,np,npar,npe,nprof,nsen, &
        nset,nx,ny,p,para,ptar,rho,srange,u,utar,v, &
        vtar,x1max,x1min,x2max,x2min,xc,xmax, &
        xmin,xsmax,xsmin,xtmax,xtmin,y1max,y1min,y2max,y2min,yc, &
        ymax,ymin,ysmax,ysmin,ytmax,ytmin)

    end if

    idata = 1
!
!  ARROWS = : Choose solid or line.
!
  else if ( s_eqi ( command(1:5),'arrow') ) then
    if ( s_eqi ( command(1:6),'arrow=') ) then
      arrow = command(7:)
    else if ( s_eqi ( command(1:7),'arrows=') ) then
      arrow = command(8:)
    else
      write(*,*)' '
      write(*,*)'DISPLAY - Input request:'
      write(*,*)'  Choose HOLLOW, LINE, SOLID for arrows.'
      read(*,'(a)')arrow
      write(17,'(a)')arrow
      if ( echo ) then
        write(*,'(a)')arrow
      end if
    end if

    write(*,*)'  The arrow option is now ARROW = ' // trim ( arrow )
!
!  B: show boundary.
!
  else if ( s_eqi ( command, 'B' ) ) then

    show(1) = .not. show(1)
    if ( show(1) ) then
      write(*,*)'The boundary will be shown.'
    else
      write(*,*)'The boundary will NOT be shown.'
    end if
!
!  BACK: show the background
!
  else if ( s_eqi ( command(1:4), 'BACK' ) ) then

    show(21) = .not.show(21)
    if ( show(21) ) then
      write(*,*)'The background will be shown.'
    else
      write(*,*)'The background will NOT be shown.'
    end if
!
!  BAR: Switch display of color bar.
!
  else if ( s_eqi ( command, 'BAR' ) ) then
 
    lbar = .not.lbar
    if ( lbar ) then
      write(*,*)'The color bar will be shown.'
    else
      write(*,*)'The color bar will NOT be shown.'
    end if
!
!  BH: bottom half.
!
  else if ( s_eqi ( command,'BH') ) then

    ysmax = ysmin+0.5*(ysmax-ysmin)

    call pltbox ( grace,srange,x1max,x1min,x2max,x2min,xsmax,xsmin, &
      y1max,y1min,y2max,y2min,ysmax,ysmin)
!
!  BL: bottom left quarter.
!
  else if ( s_eqi ( command,'BL') ) then

    xsmax = xsmin+0.5*(xsmax-xsmin)
    ysmax = ysmin+0.5*(ysmax-ysmin)

    call pltbox ( grace,srange,x1max,x1min,x2max,x2min,xsmax,xsmin, &
      y1max,y1min,y2max,y2min,ysmax,ysmin)
!
!  BC: bottom center quarter.
!
  else if ( s_eqi ( command,'BC') ) then

    temp = 0.25*(xsmax-xsmin)
    xsmin = xsmin+temp
    xsmax = xsmax-temp
    ysmax = ysmin+0.5*(ysmax-ysmin)

    call pltbox ( grace,srange,x1max,x1min,x2max,x2min,xsmax,xsmin, &
      y1max,y1min,y2max,y2min,ysmax,ysmin)
!
!  BR: bottom right quarter.
!
  else if ( s_eqi ( command,'br') ) then

    xsmin = xsmin+0.5*(xsmax-xsmin)
    ysmax = ysmin+0.5*(ysmax-ysmin)

    call pltbox ( grace,srange,x1max,x1min,x2max,x2min,xsmax,xsmin, &
      y1max,y1min,y2max,y2min,ysmax,ysmin)
!
!  C: choose colors.
!
  else if ( s_eqi ( command,'c') ) then

    write(*,*)' '
    write(*,*)' Number  Color  Name'
    write(*,*)' '
    do i = 1,maxobj
      write(*,'(1x,i2,2x,i3,2x,a)')i,icolor(i),object(i)
    end do

    write(*,*)' '
    write(*,*)'Enter an object number, and a color number.'
    read(*,*,end = 90,err=90)itemp,jtemp
    write(17,*)itemp,jtemp
    if ( echo ) then
      write(*,*)itemp,jtemp
    end if
    if ( 1 <= itemp.and.itemp <= maxobj ) then
      icolor(itemp) = jtemp
    else
      write(*,*)'Your object number was out of bounds.'
    end if
!
!  CC: choose color contour labels
!
!  For some strange reason, in order to make the color table
!  active, we have to call NEWFRM!
!
  else if ( s_eqi ( command(1:2),'cc') ) then

    if ( dev == ' ' ) then
      write(*,*)' '
      write(*,*)'Please use the DEV command first!'
      go to 10
    end if

    if ( s_eqi ( command(1:3),'cc=') ) then
      read(command(4:),*,end = 90,err=90)itable
    else
      write(*,*)' '
      write(*,*)'Built in color tables include:'
      write(*,*)' '
      write(*,*)'1  low black to high white.'
      write(*,*)'2  low blue to high yellow.'
      write(*,*)'3  low red, high blue, with bands between.'
      write(*,*)'4  low red, yellow, green, blue, high white.'
      write(*,*)'5  low white, blue, green, yellow, high red'
      write(*,*)'6  low blue to high red.'
      write(*,*)'7  linear table between 2 user colors.'
      write(*,*)'8  linear table between N user colors.'
      write(*,*)'9  low white to high black.'
      write(*,*)' '
      write(*,*)'Enter a color table index between 1 and 9,'
      write(*,*)'or 0 to enter a color table from a file.'

      read(*,*,end = 90,err=90)itable
      write(17,*)itable
      if ( echo ) then
        write(*,*)itable
      end if
    end if

    call get_tab ( dev,echo,filgrf,grace,icmax,icmin,ierror,iplot,itable,ovrlay )
 
    if ( itable == 1.or.itable == 9 ) then
      jcmax = 200
      jcmin = 32
    else
      jcmax = 255
      jcmin = 2
    end if
 
    write(*,*)' '
    write(*,*)'Lowest color used will be JCMIN =  ',jcmin
    write(*,*)'Highest color used will be JCMAX = ',jcmax
!
!  CH: center half.
!
  else if ( s_eqi ( command,'ch') ) then

    temp = 0.25*(xsmax-xsmin)
    xsmin = xsmin+temp
    xsmax = xsmax-temp

    call pltbox ( grace,srange,x1max,x1min,x2max,x2min,xsmax,xsmin, &
      y1max,y1min,y2max,y2min,ysmax,ysmin)
!
!  COLOR
!
  else if ( s_eqi ( command,'color') ) then
    write(*,*)'Enter the color index between 0 and 255'
    read(*,*,end = 90,err=90)i
    write(17,*)i
    if ( echo ) then
      write(*,*)i
    end if
    write(*,*)'Enter(R,G,B)'
    write(*,*)'Note: (0,0,0) is black, (1,1,1) is white!'
    read(*,*,end = 90,err=90)rval,gval,bval
    write(17,*)rval,gval,bval
    if ( echo ) then
      write(*,*)rval,gval,bval
    end if
    call setclr(i,bval,gval,rval)
!
!  CP: draw CP contour lines.
!
  else if ( s_eqi ( command,'cp') ) then

    show(29) = .not.show(29)
    if ( show(29) ) then
      write(*,*)'CP contours will be plotted.'
    else
      write(*,*)'CP contours will NOT be plotted.'
    end if
!
!  CPC: draw CP colors.
!
  else if ( s_eqi ( command,'cpc') ) then

    show(32) = .not.show(32)
    if ( show(32) ) then
      write(*,*)'CP colors will be plotted.'
    else
      write(*,*)'CP colors will NOT be plotted.'
    end if
!
!  CTAB
!
  else if ( s_eqi ( command,'ctab') ) then

    call preplt ( dev,echo,filgrf,icmax,icmin,iplot,itable,ovrlay )

    call cbox ( grace )
!
!  If the user hadn't already graphed anything, X1MIN = X1MAX=0 is
!  possible.  Set to a default.
!
    if ( x1min == x1max ) then
      x1min = 0.0
      x1max = 1.0
    else if ( y1min == y1max ) then
      y1min = 0.0
      y1max = 1.0
    end if

    call setwcd ( x1min,y1min,x1max,y1max,ierror )

    call buzz ( dev, x1minc, x1maxc, y1minc, y1maxc )
!
!  DAT = : specify the data file to be read.
!
  else if ( s_eqi ( command(1:3),'DAT') ) then
 
    filtyp = 'flow'

    if ( s_eqi ( command(1:4),'dat=') ) then

      fildat = command(5:)

    else

      if ( ifile > 0 ) then
        close(unit = 2)
        write(*,*)'OpnFil is closing the old file '// trim ( fildat )
      end if
   
      write(*,*)' '
      write(*,*)'Enter the name of the new input data file:'
      read(*,'(a)',err = 90,end=90) fildat
      write(17,'(a)') trim ( fildat )
      if ( echo ) then
        write(*,'(a)') trim ( fildat )
      end if

    end if

    call opnfil ( fildat, ifile, iset )

    if ( ifile == -2)go to 110
    if ( ifile == -1)go to 100

    iset = 0

20      continue

    call plot_file_read ( eqn,ierror,ifile,iset,isotri,maxelm,maxnp, &
      maxnpe,maxny,maxpar,maxsen,nelem,node,np,npar,npe,nprof, &
      nsen,nx,ny,p,para,ptar,rho,u,utar,v,vtar,xc,xprof,yc)

    if ( ierror == 0 ) then
      iset = iset+1
      go to 20
    end if

    nset = iset
    string = 'This file contains '//chrint(nset)//' data sets.'
    call s_blanks_delete ( string)
    write ( *, '(a)') trim ( string )

    xsmin = 0.0
    xsmax = 0.0
    ysmin = 0.0
    ysmax = 0.0
!
!  DB: debug output.
!
  else if ( s_eqi ( command,'db') ) then

    call node_data_print ( maxnp,maxsen,nflag,np,p,u,v,xc,yc )
!
!  'DEV = ' Choose the graphics device.
!
  else if ( s_eqi ( command(1:3),'dev') ) then

    if ( dev /= ' ' ) then
      write(*,*)' '
      write(*,*)'Display - Error!'
      write(*,*)'  You have already chosen device '// trim ( dev )
      write(*,*)'  You may not change your mind!'
      go to 10
    end if

    if ( s_eqi ( command(1:4),'dev=') ) then
      dev = trim ( command(5:) )
    else
      write ( *, * ) ' '
      write ( *, * ) 'Enter the graphics device desired.'
      write ( *, * ) ' '
      write ( *, * ) '  CGMB  output to a CGM binary file.'
      write ( *, * ) '  PS    output to a PostScript file.'
      write ( *, * ) '  XWS   output to an X window screen.'

      read(*,'(a)',end = 90, err=90 ) dev
      write ( 17, '(a)' ) dev
      if ( echo ) then
        write ( *, '(a)' ) dev
      end if
    end if

    if ( s_eqi ( dev(1:3),'cgm') ) then
      dev = 'cgmb'
      write(*,*)'Output will be to a CGM binary file "display.cgm".'
    else if ( s_eqi ( dev,'ps') ) then
      write(*,*)'Output will be to a PostScript file "display.ps".'
    else if ( s_eqi ( dev,'xws') ) then
      write(*,*)'Output will be to an X window screen.'
    else
      write(*,*)'Your device '// trim ( dev ) //' was not recognized!'
      dev = ' '
    end if
!
!  DJE = : specify the JEFF element file to be read.
!
  else if ( s_eqi ( command(1:3),'dje') ) then
 
    filtyp = 'jeff'

    if ( s_eqi ( command(1:4),'dje=') ) then
      filelm = command(5:)
    else
      write(*,*)'Enter the name of the element data file.'
      read(*,'(a)')filelm
      write(17,'(a)')filelm
      if ( echo ) then
        write(*,'(a)')filelm
      end if
    end if

    call rdelj(filelm,ifile,maxelm,maxnp,maxnpe,nelem,node,np,npe)

    xsmin = 0.0
    xsmax = 0.0
    ysmin = 0.0
    ysmax = 0.0
!
!  DJN = : specify the JEFF node file to be read.
!
  else if ( s_eqi ( command(1:3),'djn') ) then

    if ( s_eqi ( command(1:4),'djn=' ) ) then
      fildat = command(5:)
    else
      write(*,*)'Enter the name of the node data file.'
      read(*,'(a)')fildat
      write(17,'(a)') trim ( fildat )
      if ( echo ) then
        write(*,'(a)') trim ( fildat )
      end if
    end if
 
    call adjeff(delx,dely,eflag,eqn,fildat,grace,ifile,isotri, &
      jbound,jfile,maxbou,maxelm,maxnp,maxnpe,nbound,nelem,nflag,nflag0, &
      node,np,npe,nset,nx,ny,p,rho,srange,u,v,x1max,x1min,x2max,x2min,xc, &
      xmax,xmin,xsmax,xsmin,xtmax,xtmin,y1max,y1min, &
      y2max,y2min,yc,ymax,ymin,ysmax,ysmin,ytmax,ytmin)
!
!  DTEC = : specify the TECPLOT data file to be read.
!
  else if ( s_eqi ( command(1:4),'dtec') ) then
 
    filtyp = 'tecplot'

    if ( s_eqi ( command(1:5),'dtec=') ) then
      filelm = command(6:)
    else
      write(*,*)'Enter the name of the TECPLOT data file.'
      read(*,'(a)')filelm
      write(17,'(a)')filelm
      if ( echo ) then
        write(*,'(a)')filelm
      end if
    end if

    call rdtec(cp,filelm,ifile,maxelm,maxnp,maxnpe,nelem,node, &
      np,npe,nx,ny,rho,rmach,u,v,xc,yc)

    xsmin = 0.0
    xsmax = 0.0
    ysmin = 0.0
    ysmax = 0.0

    nset = 1
    ifile = 2
!
!  We assume all triangles are nonisoperimetric.
!
    do i = 1,nelem
      isotri(i) = 0
    end do
!
!  Initially, all elements will be visible.
!
    do i = 1,nelem
      eflag(i) = .true.
    end do

    do i = 1,np
      nflag(i) = .true.
      nflag0(i) = .true.
    end do
!
!  We give default values to the equations.
!
    do i = 1,np
      eqn(1,i) = 'U'
      eqn(2,i) = 'V'
      eqn(3,i) = 'P'
    end do
!
!  Check element orientation.
!
    call element_check(maxnpe,nelem,node,np,npe,xc,yc)
!
!  Determine the region size.
!
    call region_size ( delx,dely,grace,nelem,nflag,np,srange, &
      x1max,x1min,x2max,x2min,xc,xmax,xmin,xsmax,xsmin,xtmax,xtmin,y1max, &
      y1min,y2max,y2min,yc,ymax,ymin,ysmax,ysmin,ytmax,ytmin)
!
!  Set the location of boundary edges.
!
    call boundary_set(eqn,jbound,maxbou,maxnpe,nbound,nelem,node,np,npe)
!
!  E: show elements.
!
  else if ( s_eqi ( command,'e').or.s_eqi ( command,'elements') ) then

    show(2) = .not.show(2)
    if ( show(2) ) then
      write(*,*)'Elements will be shown.'
    else
      write(*,*)'Elements will NOT be shown.'
    end if
!
!  EC: show element colors.
!
  else if ( s_eqi ( command,'ec') ) then

    show(20) = .not.show(20)
    if ( show(20) ) then
      write(*,*)'Element colors will be shown.'
    else
      write(*,*)'Element colors will NOT be shown.'
    end if
!
!  EN: Show element numbers
!
  else if ( s_eqi ( command,'en') ) then

    show(28) = .not.show(28)
    if ( show(28) ) then
      write(*,*)'Element numbers will be shown.'
    else
      write(*,*)'Element numbers will NOT be shown.'
    end if
!
!  EX: Define example problem.
!
  else if ( s_eqi ( command(1:2),'ex') ) then
 
    if ( ifile > 0 ) then
      close(unit = 2)
      write(*,*)'OpnFil is closing the old file '// trim ( fildat )
    end if
   
    fildat = ' '

    call exdat(eqn,isotri,maxelm,maxnp,maxnpe,maxny,maxpar, &
      maxsen,nelem,node,np,npar,nprof, &
      nx,ny,p,para,ptar,u,utar,v,vtar,xc,xprof,yc)
 
    do i = 1,np
      nflag(i) = .true.
      nflag0(i) = .true.
    end do

    do i = 1,nelem
      eflag(i) = .true.
    end do
!
!  Determine the region size.
!
    call region_size ( delx,dely,grace,nelem,nflag,np,srange, &
      x1max,x1min,x2max,x2min,xc,xmax,xmin,xsmax,xsmin,xtmax,xtmin,y1max, &
      y1min,y2max,y2min,yc,ymax,ymin,ysmax,ysmin,ytmax,ytmin)
 
    call boundary_set(eqn,jbound,maxbou,maxnpe,nbound,nelem,node,np,npe)
 
    idata = 1
    nset = 1
    string = 'This example includes '//chrint(nset)//' data sets.'
    call s_blanks_delete ( string)
    write(*,'(1x,a)') trim ( string )
!
!  ECHO: switch the echo option
!
  else if ( s_eqi ( command,'echo') ) then

    echo = .not.echo
    if ( echo ) then
      write(*,'(a)') trim ( command )
      write(*,*)'User input will be echoed.'
    else
      write(*,*)'User input will NOT be echoed.'
    end if
!
!  F: switch the frame option
!
  else if ( s_eqi ( command,'f') ) then

    show(3) = .not.show(3)
    if ( show(3) ) then
      write(*,*)'The frame will be shown.'
    else
      write(*,*)'The frame will NOT be shown.'
    end if
!
!  FILE = : set the name of the graphics output file.
!
  else if ( s_eqi ( command(1:4),'file') ) then

    if ( iplot > 0 ) then
      write(*,*)' '
      write(*,*)'Display - Warning:'
      write(*,*)'  It is too late to specify a plot file name.'
    else if ( s_eqi ( command(1:5),'file=') ) then
      filgrf = command(6:)
    else
      write(*,*)' '
      write(*,*)'Enter the plot file name.'
      read(*,'(a)',err = 90,end=90)filgrf
      write(17,'(a)') trim ( filgrf )
      if ( echo ) then
        write(*,'(a)') trim ( filgrf )
      end if
    end if

    write(*,*)' '
    write(*,*)'DISPLAY - Note:'
    write(*,*)'  The plot file will be named '//filgrf
!
!  FILTYP = : set the type of the input file.
!
  else if ( s_eqi ( command(1:6),'filtyp') ) then

    if ( s_eqi ( command(1:7),'filtyp=') ) then
      filtyp = command(8:)
    else
      write(*,*)' '
      write(*,*)'Enter the file type (FLOW, JEFF or TECPLOT).'
      read(*,'(a)',err = 90,end=90)filtyp
      write(17,'(a)')filtyp
      if ( echo ) then
        write(*,'(a)')filtyp
      end if
    end if

    write(*,*)' '
    write(*,*)'DISPLAY - Note:'
    write(*,*)'  The input file type is set to '//filtyp
!
!  FRAME: show the frame
!
  else if ( s_eqi ( command,'frame') ) then

    show(3) = .true.

    write(*,*)'DISPLAY - Note:'
    write(*,*)'  A frame will be shown around the picture.'
!
!  FULL: show full picture.
!
  else if ( s_eqi ( command,'full') ) then

    xsmax = xtmax
    xsmin = xtmin
    ysmax = ytmax
    ysmin = ytmin

    call pltbox(grace,srange,x1max,x1min,x2max,x2min,xsmax,xsmin, &
      y1max,y1min,y2max,y2min,ysmax,ysmin)
!
!  G: create current graph.
!
  else if ( s_eqi ( command,'g') ) then

    call graph ( arrow,cp,delx,dely,dev,dudxn,dudyn,dvdxn,dvdyn,echo,eflag, &
      eflagu,etaref,filgrf,filtyp,icmax,icmin,icolor,icomp,iplot,isotri, &
      itable,iwork1,iwork2,jbound,jcmax,jcmin,lbar,line,maxbou,maxnp,maxnpe, &
      maxobj,maxsen,nbound,ncon,nelem,nflag,nflag0,node,np, &
      npe,nxskip,ny,nyskip,object,ovrlay,p,rho,rmach,s,s2,scalee, &
      scalen,scalev,show,smax,smin,srange,t,title,title2,u,v,x1max,x1min, &
      x2max,x2min,xc,xprof,xsiref,xsmax,xsmin,y1max,y1min,y2max,y2min, &
      yc,ysmax,ysmin)
!
!  GRACE = : set the grace margin.
!
  else if ( s_eqi ( command(1:5),'grace') ) then

    if ( s_eqi ( command(1:6),'grace=') ) then
      read(command(7:),*,err = 90,end=90)grace
    else
      write(*,*)'Enter the grace margin:'
      read(*,*)grace
      write(17,*)grace
      if ( echo ) then
        write(*,*)grace
      end if
    end if

    write(*,*)' '
    write(*,*)'DISPLAY - Note:'
    write(*,*)'  The grace margin was set to GRACE  =  ',grace

    call pltbox(grace,srange,x1max,x1min,x2max,x2min,xsmax,xsmin, &
      y1max,y1min,y2max,y2min,ysmax,ysmin)
!
!  HELLO: print program version, data, and maxima:
!
  else if ( s_eqi ( command,'hello') ) then

    call hello(maxbou,maxelm,maxnp,maxnpe,maxnx,maxny,maxobj,maxpar,maxsen)
!
!  HELP: help
!
  else if ( s_eqi ( command,'h').or.s_eqi ( command,'help') ) then

    call help(echo)
!
!  ICMAX = : set the maximum available color index.
!
  else if ( s_eqi ( command(1:6),'icmax=') ) then

    read(command(7:),*,err = 90,end=90)icmax
    if ( icmax > 255 ) then
      write(*,*)'ICMAX must be no more than 255'
      icmax = 255
    end if
    write(*,*)'Maximum available color set to ',icmax
!
!  ICMIN = : set the minimum available color index.
!
  else if ( s_eqi ( command(1:6),'icmin=') ) then

    read(command(7:),*,err = 90,end=90)icmin
    if ( icmin < 2 ) then
      write(*,*)'ICMIN must be at least 2.'
      icmin = 2
    end if
    write(*,*)'Minimum available color set to ',icmin
!
!  ICOMP = : set the sensitivity component.
!
  else if ( s_eqi ( command(1:5),'icomp') ) then

    if ( s_eqi ( command(1:6),'icomp=') ) then
      read(command(7:),*,err = 90,end=90)itemp
    else
      write(*,*)'Enter the sensitivity component:'
      read(*,*)itemp
      write(17,*)itemp
      if ( echo ) then
        write(*,*)itemp
      end if
    end if

    if ( 0 == itemp ) then
      write(*,*)'Primitive variables will be displayed.'
    else if ( 1 <= itemp.and.itemp <= npar ) then
      icomp = itemp
      write(*,*)'Sensitivity component set to ',icomp
    else if ( npar+1 <= itemp.and.itemp <= 2*npar ) then
      icomp = itemp
      write(*,*)'Finite difference component set to ',icomp-npar
    else
      write(*,*)'Display - Warning!'
      write(*,*)'  Your command ICOMP = ',itemp
      write(*,*)'  was ignored, because ICOMP must be'
      write(*,*)'  0 for the primitive variables,'
      write(*,*)'  between 1 and ',npar,' for disc. sens.'
      write(*,*)'  between ',npar+1,' and ',2*npar,' fin. dif.'
    end if
!
!  INIT: set variables to initial (zero!) values.
!
  else if ( s_eqi ( command(1:4),'init') ) then

    call init ( arrow,cp,delx,dely,dev,dudxn,dudyn,dvdxn,dvdyn,echo,eflag, &
      eflagu,eqn,etaref,fildat,filgrf,filinp,filtyp,grace,icmax,icmin, &
      icolor,icomp,idata,ifile,iplot,iset,itable,iwrite,jcmax, &
      jcmin,labelx,labely,lbar,line,lppro,lptpro,lupro, &
      lutpro,lvpro,lvtpro,maxelm,maxnp,maxnpe,maxny,maxobj, &
      maxpar,maxsen,nbound,ncon,nelem,nflag,nflag0,node, &
      np,npe,nprof,nsen,nset,nxskip,ny,nyskip,object,ovrlay,p,rho, &
      rmach,scalee, &
      scalen,scalev,show,smax,smin,title,title2,u,v,x1max,x1min, &
      x2max,x2min,x4max,x4min,xc,xprof,xsiref,xsmax,xsmin,y1max, &
      y1min,y2max,y2min,y4max,y4min,yc,ysmax,ysmin)
!
!  IWRITE = : set the debugging output level.
!
  else if ( s_eqi ( command(1:7),'iwrite=') ) then

    read(command(8:),*,err = 90,end=90)iwrite
    write(*,*)'Debugging level set to ',iwrite
!
!  JCMAX = : set the maximum used color index.
!
  else if ( s_eqi ( command(1:6),'jcmax=') ) then

    read(command(7:),*,err = 90,end=90)jcmax
    if ( jcmax > 255 ) then
      jcmax = 255
      write(*,*)'JCMAX must be no more than 255.'
    end if
    write(*,*)'Maximum used color set to ',jcmax
!
!  JCMIN = : set the minimum used color index.
!
  else if ( s_eqi ( command(1:6),'jcmin=') ) then

    read(command(7:),*,err = 90,end=90)jcmin
    if ( jcmin < 2 ) then
      jcmin = 2
      write(*,*)'JCMIN must be no less than 2.'
    end if
    write(*,*)'Minimum used color set to ',jcmin
!
!  KV: show kinematic velocity vectors.
!
  else if ( s_eqi ( command,'kv') ) then

    show(8) = .not.show(8)
    if ( show(8) ) then
      write(*,*)'Kinematic velocities will be shown.'
    else
      write(*,*)'Kinematic velocities will NOT be shown.'
    end if
!
!  KVMAG: show kinematic velocity magnitude contours.
!
  else if ( s_eqi ( command,'kvmag') ) then

    show(10) = .not.show(10)
    if ( show(10) ) then
      write(*,*)'Kinematic velocity magnitudes will be shown.'
    else
      write(*,*)'Kinematic velocity magnitudes will NOT be shown.'
    end if
!
!  KVMAGC: show velocity magnitude color plots.
!
  else if ( s_eqi ( command,'kvmagc') ) then

    show(14) = .not.show(14)
    if ( show(14) ) then
      write(*,*)'Kinematic velocity magnitude colors will be shown.'
    else
      write(*,*)'Kinematic velocity magnitude colors will NOT be shown.'
    end if
!
!  KVX: show X kinematic velocity contours.
!
  else if ( s_eqi ( command,'kvx') ) then

    show(15) = .not.show(15)
    if ( show(15) ) then
      write(*,*)'X kinematic velocity contours will be shown.'
    else
      write(*,*)'X kinematic velocity contours will NOT be shown.'
    end if
!
!  KVXC: show X kinematic velocity color contours.
!
  else if ( s_eqi ( command,'kvxc') ) then

    show(17) = .not.show(17)
    if ( show(17) ) then
      write(*,*)'X kinematic velocity color contours will be shown.'
    else
      write(*,*)'X kinematic velocity color contours will NOT be shown.'
    end if
!
!  KVY: show Y kinematic velocity contours.
!
  else if ( s_eqi ( command,'kvy') ) then

    show(16) = .not.show(16)
    if ( show(16) ) then
      write(*,*)'Y kinematic velocity contours will be shown.'
    else
      write(*,*)'Y kinematic velocity contours will NOT be shown.'
    end if
!
!  KVYC: show Y kinematic velocity color contours.
!
  else if ( s_eqi ( command,'kvyc') ) then

    show(18) = .not.show(18)
    if ( show(18) ) then
      write(*,*)'Y kinematic velocity color contours will be shown.'
    else
      write(*,*)'Y kinematic velocity color contours will NOT be shown.'
    end if
!
!  L: list current values
!
  else if (  (s_eqi ( command,'l')).or. &
             ( s_eqi (command(1:4),'list'))  ) then

    call list(delx,dely,dev,echo,fildat,grace,icmax,icmin,icolor, &
      icomp,idata,ifile,iplot,iset,itable,iwrite,jbound, &
      maxbou,maxnp,maxobj,maxpar,maxsen,nbound,ncon,nelem,np,npar, &
      npe,nset,nx,nxskip,ny,nyskip,object,p,scalev,show,&
      title,title2,u,v,x2max,x2min,xmax,xmin,xprof,xsmax,xsmin,ymax, &
      ymin,y2max,y2min,ysmax,ysmin)
!
!  LH: left half.
!
  else if ( s_eqi ( command,'lh') ) then

    xsmax = xsmin+0.5*(xsmax-xsmin)
    temp = 0.25*(ysmax-ysmin)

    call pltbox(grace,srange,x1max,x1min,x2max,x2min,xsmax,xsmin, &
      y1max,y1min,y2max,y2min,ysmax,ysmin)
!
!  LINE: choose line type.
!
  else if ( s_eqi ( command,'line') ) then

    write(*,*)' '
    write(*,*)' Number  Linetype  Name'
    write(*,*)' '
    do i = 1,maxobj
      write(*,'(1x,i2,2x,i3,2x,a)')i,line(i),object(i)
    end do

    write(*,*)' '
    write(*,*)'Enter an object number, and a line type.'
    write(*,*)'0 = Solid black, 1=dashed black,'
    write(*,*)'2 = Solid color, 3=dashed color.'

    read(*,*,end = 90,err=90)itemp,jtemp
    write(17,*)itemp,jtemp
    if ( echo ) then
      write(*,*)itemp,jtemp
    end if
    if ( 1 <= itemp.and.itemp <= maxobj ) then
      line(itemp) = jtemp
    else
      write(*,*)'Your object number was out of bounds.'
    end if
!
!  MACH: draw mach contour lines.
!
  else if ( s_eqi ( command,'mach') ) then

    show(30) = .not.show(30)
    if ( show(30) ) then
      write(*,*)'Mach contours will be plotted.'
    else
      write(*,*)'Mach contours will NOT be plotted.'
    end if
!
!  MACHC: draw density colors.
!
  else if ( s_eqi ( command,'machc') ) then

    show(33) = .not.show(33)
    if ( show(33) ) then
      write(*,*)'Mach colors will be plotted.'
    else
      write(*,*)'Mach colors will NOT be plotted.'
    end if
!
!  MC: middle center quarter.
!
  else if ( s_eqi ( command,'mc') ) then

    temp = 0.25*(xsmax-xsmin)
    xsmin = xsmin+temp
    xsmax = xsmax-temp
    temp = 0.25*(ysmax-ysmin)
    ysmax = ysmax-temp
    ysmin = ysmin+temp

    call pltbox(grace,srange,x1max,x1min,x2max,x2min,xsmax,xsmin, &
      y1max,y1min,y2max,y2min,ysmax,ysmin)
!
!  MH: middle half.
!
  else if ( s_eqi ( command,'mh') ) then

    temp = 0.25*(ysmax-ysmin)
    ysmax = ysmax-temp
    ysmin = ysmin+temp

    call pltbox(grace,srange,x1max,x1min,x2max,x2min,xsmax,xsmin, &
      y1max,y1min,y2max,y2min,ysmax,ysmin)
!
!  ML: middle left quarter.
!
  else if ( s_eqi ( command,'ml') ) then

    xsmax = xsmin+0.5*(xsmax-xsmin)
    temp = 0.25*(ysmax-ysmin)
    ysmax = ysmax-temp
    ysmin = ysmin+temp

    call pltbox(grace,srange,x1max,x1min,x2max,x2min,xsmax,xsmin, &
      y1max,y1min,y2max,y2min,ysmax,ysmin)

!
!  MR: middle right quarter.
!
  else if ( s_eqi ( command,'mr') ) then

    xsmin = xsmin+0.5*(xsmax-xsmin)
    temp = 0.25*(ysmax-ysmin)
    ysmax = ysmax-temp
    ysmin = ysmin+temp

    call pltbox(grace,srange,x1max,x1min,x2max,x2min,xsmax,xsmin, &
      y1max,y1min,y2max,y2min,ysmax,ysmin)
!
!  MV: show kinematic velocity vectors.
!
  else if ( s_eqi ( command,'mv') ) then

    show(35) = .not.show(35)
    if ( show(35) ) then
      write(*,*)'Mass velocities will be shown.'
    else
      write(*,*)'Mass velocities will NOT be shown.'
    end if
!
!  MVMAG: show mass velocity magnitude contours.
!
  else if ( s_eqi ( command,'mvmag') ) then

    show(36) = .not.show(36)
    if ( show(36) ) then
      write(*,*)'Mass velocity magnitudes will be shown.'
    else
      write(*,*)'Mass velocity magnitudes will NOT be shown.'
    end if
!
!  MVMAGC: show mass magnitude color plots.
!
  else if ( s_eqi ( command,'mvmagc') ) then

    show(37) = .not.show(37)
    if ( show(37) ) then
      write(*,*)'Mass velocity magnitude colors will be shown.'
    else
      write(*,*)'Mass velocity magnitude colors will NOT be shown.'
    end if
!
!  N: show nodes
!
  else if ( s_eqi ( command,'n') ) then

    show(4) = .not.show(4)
    if ( show(4) ) then
      write(*,*)'Nodes will be shown.'
    else
      write(*,*)'Nodes will NOT be shown.'
    end if
!
!  NCON = : Set the number of contour lines.
!
  else if ( s_eqi ( command(1:4),'ncon') ) then

    if ( s_eqi ( command(1:5),'ncon=') ) then
      read(command(6:),*,err = 90,end=90)ncon
      write(*,*)'Number of contour lines set to ',ncon
    else
      write(*,*)'Enter number of contour lines.'
      read(*,*)ncon
      write(17,*)ncon
      if ( echo ) then
        write(*,*)ncon
      end if
    end if
!
!  NN: Show node numbers
!
  else if ( s_eqi ( command,'nn') ) then

    show(27) = .not.show(27)
    if ( show(27) ) then
      write(*,*)'Node numbers will be shown.'
    else
      write(*,*)'Node numbers will NOT be shown.'
    end if
!
!  NOFRAME: no frame, please
!
  else if ( s_eqi ( command,'noframe') ) then

    show(3) = .false.
    write(*,*)'No frame will be shown around the picture.'
!
!  NXSKIP = : skip value for column nodes.
!
  else if ( s_eqi ( command(1:7),'nxskip=') ) then

    read(command(8:),*,err = 90,end=90)itemp
    if ( itemp <= 0 ) then
      write(*,*)'Error!  NXSKIP must be 1 or more!'
    else
      nxskip = itemp
      write(*,*)'Skip value for column nodes is ',nxskip
    end if
!
!  NYSKIP = : skip value for row nodes.
!
  else if ( s_eqi ( command(1:7),'nyskip=') ) then

    read(command(8:),*,err = 90,end=90)itemp
    if ( itemp <= 0 ) then
      write(*,*)'Error!  NYSKIP must be 1 or more!'
    else
      nyskip = itemp
      write(*,*)'Skip value for row nodes is ',nyskip
    end if
!
!  OVERLAY: Switch the overlay value.
!
  else if ( s_eqi ( command,'overlay') ) then
    ovrlay = .not.ovrlay
    if ( ovrlay ) then
      write(*,*)'Plots will be overlayed until next OVERLAY.'
    else
      write(*,*)'This overlay plot is done.'
      call newfrm
    end if
!
!  P: show pressure contours.
!
  else if ( s_eqi ( command,'p') ) then

    show(5) = .not.show(5)
    if ( show(5) ) then
      write(*,*)'Pressures will be shown.'
    else
      write(*,*)'Pressures will NOT be shown.'
    end if
!
!  PC: show pressure color plots.
!
  else if ( s_eqi ( command,'pc') ) then

    show(12) = .not.show(12)
    if ( show(12) ) then
      write(*,*)'Pressure colors will be shown.'
    else
      write(*,*)'Pressure colors will NOT be shown.'
    end if
!
!  PPRO: draw P profile
!
  else if ( s_eqi ( command,'ppro') ) then

    lppro = .not.lppro
    if ( lppro ) then
      write(*,*)'Pressure will be shown in profile.'
    else
      write(*,*)'Pressure will NOT be shown in profile.'
    end if
!
!  PROGRAF: show a graph of the profile data.
!
  else if ( s_eqi ( command,'prograf') ) then

    call pgraf(dev,echo,filgrf,icmax,icmin,icolor,iplot,itable, &
      labelx,labely,lppro,lptpro,lupro,lutpro,lvpro,lvtpro,maxnp,maxobj, &
      maxsen,np,nprof,ny,ovrlay,p,ptar,show,title,title2,u,utar,v, &
      vtar,x1max,x1min,x2max,x2min,x4max,x4min,y1max,y1min,y2max, &
      y2min,y4max,y4min,yc)
!
!  PROF: show profile line.
!
  else if ( s_eqi ( command(1:4),'prof') ) then
    
    show(19) = .not.show(19)
    if ( show(19) ) then
      write(*,*)'The profile line will be shown.'
    else
      write(*,*)'The profile line will NOT be shown.'
    end if
!
!  PTPRO: draw P target profile
!
  else if ( s_eqi ( command,'ptpro') ) then

    lptpro = .not.lptpro
    if ( lptpro ) then
      write(*,*)'Target pressure will be shown in profile.'
    else
      write(*,*)'Target pressure will NOT be shown in profile.'
    end if
!
!  Q: quit.
!  QU: QUIT NOW!
!
  else if ( s_eqi ( command(1:1), 'Q' ) ) then

    if ( s_eqi ( command(2:2), 'U' ) ) then
      command = 'Y'
    else
      write ( *, * ) 'Enter "Y" to confirm you want to quit!'
      read ( *, '(a)' ) command
      write ( 17, '(a)' ) trim ( command ) 
      if ( echo ) then
        write(*,'(a)') trim ( command )
      end if
    end if

    if ( s_eqi ( command, 'Y' ) ) then
      call quit ( dev, ifile, iplot )
    end if
!
!  RH: right half.
!
  else if ( s_eqi ( command,'rh') ) then

    temp = 0.50*(xsmax-xsmin)
    xsmin = xsmin+temp

    call pltbox ( grace,srange,x1max,x1min,x2max,x2min,xsmax,xsmin, &
      y1max,y1min,y2max,y2min,ysmax,ysmin)
!
!  RHO: draw density contour lines.
!
  else if ( s_eqi ( command,'rho') ) then

    show(31) = .not.show(31)
    if ( show(31) ) then
      write(*,*)'Rho contours will be plotted.'
    else
      write(*,*)'Rho contours will NOT be plotted.'
    end if
!
!  RHOC: draw density colors.
!
  else if ( s_eqi ( command,'rhoc') ) then

    show(34) = .not.show(34)
    if ( show(34) ) then
      write(*,*)'Rho colors will be plotted.'
    else
      write(*,*)'Rho colors will NOT be plotted.'
    end if
!
!  S: show stream lines
!
  else if ( s_eqi ( command,'s').or. s_eqi ( command(1:6),'stream') ) then

    show(6) = .not.show(6)
    if ( show(6) ) then
      write(*,*)'Stream lines will be shown.'
    else
      write(*,*)'Stream lines will NOT be shown.'
    end if
!
!  SC: show stream colors
!
  else if ( s_eqi ( command,'sc') ) then

    show(22) = .not.show(22)
    if ( show(22) ) then
      write(*,*)'Stream colors will be shown.'
    else
      write(*,*)'Stream colors will NOT be shown.'
    end if
!
!  SCALEE = : set the element number scale factor.
!
  else if ( s_eqi ( command(1:7),'scalee=') ) then

    read ( command(8:),*,err = 90,end=90)scalee
    write(*,*)'Element number scale factor set to ',scalee
!
!  SCALEN = : set the node number scale factor.
!
  else if ( s_eqi ( command(1:7),'scalen=') ) then

    read(command(8:),*,err = 90,end=90)scalen
    write(*,*)'Node number scale factor set to ',scalen
!
!  SCALEV = : set the velocity vector scale.
!
  else if ( s_eqi ( command(1:7),'scalev=') ) then

    read(command(8:),*,err = 90,end=90)scalev
    write(*,*)'Velocity vector scale factor set to ',scalev
!
!  SMAX = : enter contour maximum value.
!
  else if ( s_eqi ( command(1:5),'smax=') ) then
    read(command(6:),*,err = 90,end=90)smax
  else if ( s_eqi ( command(1:4),'smax') ) then
    write(*,*)'Enter the contour maximum range:'
    read(*,*,end = 80)smax
    write(17,*)smax
    if ( echo ) then
      write(*,*)smax
    end if
!
!  SMIN = : enter contour minimum value.
!
  else if ( s_eqi ( command(1:5),'smin=') ) then
    read(command(6:),*,err = 90,end=90)smin
  else if ( s_eqi ( command(1:4),'smin') ) then
    write(*,*)'Enter the contour minimum range:'
    read(*,*,end = 80)smin
    write(17,*)smin
    if ( echo ) then
      write(*,*)smin
    end if
!
!  TC: top center quarter.
!
  else if ( s_eqi ( command,'tc') ) then

    temp = 0.25*(xsmax-xsmin)
    xsmin = xsmin+temp
    xsmax = xsmax-temp
    ysmin = ysmin+0.5*(ysmax-ysmin)

    call pltbox(grace,srange,x1max,x1min,x2max,x2min,xsmax,xsmin, &
      y1max,y1min,y2max,y2min,ysmax,ysmin)
!
!  TH: top half
!
  else if ( s_eqi ( command,'th') ) then

    ysmin = ysmin+0.5*(ysmax-ysmin)

    call pltbox(grace,srange,x1max,x1min,x2max,x2min,xsmax,xsmin, &
      y1max,y1min,y2max,y2min,ysmax,ysmin)

!
!  TITLE2 = : enter subtitle
!
  else if ( s_eqi ( command(1:7),'title2=') ) then
    title2 = command(8:)
  else if ( s_eqi ( command(1:6),'title2') ) then
    write(*,*)'Enter the subtitle:'
    read(*,'(a)',end = 80)title2
    write(17,'(a)') trim ( title2 )
    if ( echo ) then
      write(*,'(a)') trim ( title2 )
    end if
!
!  TITLE = : enter title
!
  else if ( s_eqi ( command(1:6),'title=') ) then
    title = command(7:)
  else if ( s_eqi ( command(1:5),'title') ) then
    write(*,*)'Enter the plot title:'
    read(*,'(a)',end = 80)title
    write(17,'(a)') trim ( title )
    if ( echo ) then
      write(*,'(a)') trim ( title )
    end if
!
!  TL: top left quarter.
!
  else if ( s_eqi ( command,'tl') ) then

    xsmax = xsmin+0.5*(xsmax-xsmin)
    ysmin = ysmin+0.5*(ysmax-ysmin)

    call pltbox(grace,srange,x1max,x1min,x2max,x2min,xsmax,xsmin, &
      y1max,y1min,y2max,y2min,ysmax,ysmin)
!
!  TR: top right quarter.
!
  else if ( s_eqi ( command,'tr') ) then

    xsmin = xsmin+0.5*(xsmax-xsmin)
    ysmin = ysmin+0.5*(ysmax-ysmin)

    call pltbox(grace,srange,x1max,x1min,x2max,x2min,xsmax,xsmin, &
      y1max,y1min,y2max,y2min,ysmax,ysmin)
!
!  UPRO: draw U profile
!
  else if ( s_eqi ( command,'upro') ) then

    lupro = .not.lupro
    if ( lupro ) then
      write(*,*)'Horizontal velocity will be shown in profile.'
    else
      write(*,*)'Horizontal velocity will NOT be shown in profile.'
    end if
!
!  UTPRO: draw U target profile
!
  else if ( s_eqi ( command,'utpro') ) then

    lutpro = .not.lutpro
    if ( lutpro ) then
      write(*,*)'Target horizontal velocity will be shown in profile.'
    else
      write(*,*)'Target horizontal velocity will NOT be shown in profile.'
    end if
!
!  UV: show unit velocity direction vectors.
!
  else if ( s_eqi ( command,'uv') ) then

    show(9) = .not.show(9)
    if ( show(9) ) then
      write(*,*)'Unit velocity vectors will be shown.'
    else
      write(*,*)'Unit velocity vectors will NOT be shown.'
    end if
!
!  VE: set visible elements.
!
  else if ( s_eqi ( command,'ve') ) then

    call element_visibility(eflagu,nelem)

    write(*,*)'Do you want to adjust the data limits'
    write(*,*)'to focus on the visible elements?'
    read(*,'(a1)')isay
    write(17,'(a1)')isay
    if ( echo ) then
      write(*,'(a1)')isay
    end if

    if ( s_eqi ( isay,'y') ) then

      none = .true.

      do i = 1,nelem
        if ( eflagu(i) ) then
          do j = 1,npe
            k = node(j,i)
            if ( none ) then
              xsmin = xc(k)
              xsmax = xc(k)
              ysmin = yc(k)
              ysmax = yc(k)
              none = .false.
            else
              xsmin = min(xsmin,xc(k))
              xsmax = max(xsmax,xc(k))
              ysmin = min(ysmin,yc(k))
              ysmax = max(ysmax,yc(k))
            end if
          end do
        end if
      end do

      if ( none ) then

        write(*,*)' '
        write(*,*)'DISPLAY - Warning!'
        write(*,*)'  No visible elements were found!'

      else

        call pltbox(grace,srange,x1max,x1min,x2max,x2min,xsmax, &
          xsmin,y1max,y1min,y2max,y2min,ysmax,ysmin)

      end if

    end if
!
!  VN: set visible nodes.
!
  else if ( s_eqi ( command,'vn') ) then
 
    call viznod ( echo, np, nflag0 )
!
!  VND: set visible nodes by distance.
!
  else if ( s_eqi ( command,'vnd') ) then
 
    call viznd ( echo, np, nflag0, xc, yc )
!
!  VORT: show vorticity
!
  else if ( s_eqi ( command,'vort') ) then

    show(11) = .not.show(11)
    if ( show(11) ) then
      write(*,*)'Vorticity contours will be shown.'
    else
      write(*,*)'Vorticity contours will NOT be shown.'
    end if
!
!  VORTC: show vorticity color plots.
!
  else if ( s_eqi ( command,'vortc') ) then

    show(13) = .not.show(13)
    if ( show(13) ) then
      write(*,*)'Vorticity colors will be shown.'
    else
      write(*,*)'Vorticity colors will NOT be shown.'
    end if
!
!  VPRO: draw V profile
!
  else if ( s_eqi ( command,'vpro') ) then

    lvpro = .not.lvpro
    if ( lvpro ) then
      write(*,*)'Vertical velocity will be shown in profile.'
    else
      write(*,*)'Vertical velocity will NOT be shown in profile.'
    end if
!
!  VTPRO: draw V target profile
!
  else if ( s_eqi ( command,'vtpro') ) then

    lvtpro = .not.lvtpro
    if ( lvtpro ) then
      write(*,*)'Target vertical velocity will be shown in profile.'
    else
      write(*,*)'Target vertical velocity will NOT be shown in profile.'
    end if
!
!  X: set the data window.
!
  else if ( s_eqi ( command,'x') ) then

    call get_win(echo,grace,srange,xmax,xmin,x1max,x1min,x2max, &
      x2min,xsmax,xsmin,ymax,ymin,y1max,y1min,y2max,y2min,ysmax,ysmin)
!
!  XC: show X coordinate contours.
!
  else if ( s_eqi ( command,'xc') ) then

    show(25) = .not.show(25)
    if ( show(25) ) then
      write(*,*)'X coordinate contours will be shown.'
    else
      write(*,*)'X coordinate contours will NOT be shown.'
    end if
!
!  XCC: show X coordinate color plots.
!
  else if ( s_eqi ( command,'xcc') ) then

    show(23) = .not.show(23)
    if ( show(23) ) then
      write(*,*)'X coordinate colors will be shown.'
    else
      write(*,*)'X coordinate colors will NOT be shown.'
    end if
!
!  XPROF = : reset location of profile line (til next step is read!).
!
  else if ( s_eqi ( command,'xprof=') ) then

    read(command(7:),*,err = 90,end=90)xprof
    write(*,*)'X coordinate of profile reset to ',xprof
!
!  YC: show Y coordinate contours.
!
  else if ( s_eqi ( command,'yc') ) then

    show(26) = .not.show(26)
    if ( show(26) ) then
      write(*,*)'Y coordinate contours will be shown.'
    else
      write(*,*)'Y coordinate contours will NOT be shown.'
    end if
!
!  YCC: show Y coordinate color plots.
!
  else if ( s_eqi ( command,'ycc') ) then

    show(24) = .not.show(24)
    if ( show(24) ) then
      write(*,*)'Y coordinate colors will be shown.'
    else
      write(*,*)'Y coordinate colors will NOT be shown.'
    end if
!
!  #: a comment
!
  else if ( command(1:1) == '#' ) then
!
!  Unrecognized command.
!
  else

    write(*,*)' '
    write(*,*)'DISPLAY - Warning!'
    write(*,*)'  DISPLAY did not recognize your command:'
    write(*,'(2x,''"'',a,''"'')') trim ( command )

  end if
!
!  Go back, and get next command.
!
  go to 10

80    continue

  write(*,*)' '
  write(*,*)'DISPLAY - Fatal error!'
  write(*,*)'  Error or end of file reading user input!'

  call quit(dev,ifile,iplot)

90    continue

  write(*,*)' '
  write(*,*)'DISPLAY - Serious error!'
  write(*,*)'  Error reading user input!'

  command = ' '

  go to 10

100   continue

  write(*,*)' '
  write(*,*)'DISPLAY - Serious error!'
  write(*,*)'  The input file you named could not be opened!'

  command = ' '

  go to 10

110   continue

  write(*,*)' '
  write(*,*)'DISPLAY - Serious error!'
  write(*,*)'  End of file, or error occurred, while trying'
  write(*,*)'  to read the input file you named.'

  command = ' '

  go to 10

end
subroutine adjeff ( delx,dely,eflag,eqn,fildat,grace,ifile,isotri,jbound, &
  jfile,maxbou,maxelm,maxnp,maxnpe,nbound,nelem,nflag,nflag0,node,np,npe, &
  nset,nx,ny,p,rho,srange,u,v,x1max,x1min,x2max,x2min,xc,xmax,xmin,xsmax, &
  xsmin,xtmax,xtmin,y1max,y1min,y2max,y2min,yc,ymax,ymin,ysmax,ysmin, &
  ytmax,ytmin)
!
!***********************************************************************
!
!! ADJEFF reads the node data for a Jeff plot.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 January 2001
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
!    Output, logical EFLAG(MAXELM).
!    EFLAG is used to "flag" which elements are to be displayed.
!    If EFLAG(I) is TRUE, then element I should be displayed.
! 
!    Output, character ( len = 2 )  EQN(3,NP).
!    EQN records the "type" of each equation that will be generated, and
!    which is associated with an unknown.  Note that most boundary 
!    conditions do not result in an equation.  The current values are:
!
!    'U'  The horizontal momentum equation.
!    'UW' The condition U = 0 applied at a node on a fixed wall.
!    'V'  The vertical momentum equation.
!    'VW' The condition V = 0 applied at a node on a fixed wall.
!    'P'  The continuity equation.
!    'PB' The condition P = 0 applied at (XMAX,YMAX).
!
!    Input, character ( len = 80 ) FILDAT.
!    The name of the data file to be read in, which contains
!    the information defining the mesh and the physical
!    parameters.
! 
!    Input, real GRACE.
!         The size of the "grace" margin on the plot.
! 
!  IFILE  Input/output, integer ( kind = 4 ) IFILE.
!         Records the status of the data file whose name is FILDAT.
! 
!  ISOTRI Output, integer ( kind = 4 ) ISOTRI(MAXELM).
! 
!         0, if element I is not isoparametric.
!         1 or 2, if element I is isoparametric.
! 
!  JBOUND Output, integer ( kind = 4 ) JBOUND(5,MAXBOU)
! 
!         For each line segment of the boundary:
! 
!         JBOUND(1,I) contains the element number;
! 
!         JBOUND(2,I) contains the local node number of one corner 
!           of the element, which forms the edge;
!
!         JBOUND(2,I) contains the "next" node along the edge.
!           If the element is linear, this is the other corner node.
!           If the element is quadratic, this is the midside node along
!             the edge.
!
!         JBOUND(4,I) contains the "next" node along the edge.
!           If the element is linear, this is 0.
!           If the element is quadratic, this is the other corner node 
!             along the edge.
!
!         JBOUND(5,I) contains:
!           0 if the boundary is a wall (U = V=0);
!           1 if the boundary is open.
!
!  JFILE  Output, integer ( kind = 4 ) JFILE, an error indicator.
!         JFILE is 2 if the file was successfully opened and read.
!         (If not, then RdNod actually halts execution!)
!
!  MAXBOU Input, integer ( kind = 4 ) MAXBOU.
!         The amount of storage available for the IBOUND array.
! 
!  MAXELM Input, integer ( kind = 4 ) MAXELM.
!         The maximum number of elements which the program can
!         handle.
! 
!  MAXNP  Input, integer ( kind = 4 ) MAXNP, the maximum number of nodes.
!
!  MAXNPE Input, integer ( kind = 4 ) MAXNPE.
!         MAXNPE is the maximum number of nodes per element.
!
!  MAXNY  Input, integer ( kind = 4 ) MAXNY, the maximum allowed value of NY.
!
!  NBOUND Output, integer ( kind = 4 ) NBOUND.
!         The number of points (XBOUND(I),YBOUND(I)) used to
!         define the boundary.
! 
!  NELEM  Output, integer ( kind = 4 ) NELEM.
!         The number of elements.
! 
!  NFLAG  Output, logical NFLAG(MAXNP).
! 
!         NFLAG is used to "flag" which nodes are active,
!         that is, to be displayed, and which not, in the graph.
! 
!  NODE   Output, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), or NODE(MAXNPE,NELEM).
!         NODE contains, for each element, the global node numbers
!         of its NPE nodes.  
!
!         For linear elements (NPE = 3), the order of the nodes probably
!         doesn't matter, but we will draw them this way:
!
!                       2
!                      /|
!                    /  |
!                  /    |
!                /      |
!               1-------3
!
!         For quadratic elements (NPE = 6), the nodes must be given in a 
!         particular order, which is as follows:
!
!                       2
!                      /|
!                    /  |
!                  4    5
!                /      |
!               1---6---3
! 
!    Output, integer ( kind = 4 ) NP, the number of nodes.
!
!  NPE    Output, integer ( kind = 4 ) NPE.
!         NPE is the number of nodes per element, which should be
!         3 for linear elements and 6 for quadratics.
!
!  NSET   Output, integer ( kind = 4 ) NSET.
!         The number of sets of data contained in the data file.
! 
!  NX     Output, integer ( kind = 4 ) NX.
!         Determines the number of nodes and elements in the X
!         direction.  There will be 2*NX-1 nodes, 2*NX-2 elements.
!  
!  NY     Output, integer ( kind = 4 ) NY.
!         Determines the number of nodes and elements in the Y
!         direction.  There will be 2*NY-1 nodes, 2*NY-2 elements.
! 
!  P      Output, real P(MAXNP,0:MAXPAR).
! 
!         P(I,0) is the pressure at node I.
! 
!         P(I,J) is the sensitivity of the pressure with respect
!         to parameter J.
! 
!  SRANGE Output, real SRANGE.
!         The maximum of XSMAX-XSMIN and YSMAX-YSMIN.
!         This gives the size of a square containing the data
!         window.
!
!  U      Output, real U(MAXNP,MAXPAR).
! 
!         U(I,0) is the horizontal fluid velocity at node I.
! 
!         U(I,J) is the sensitivity of the horizontal velocity with 
!         respect to parameter J.
! 
!  V      Output, real V(MAXNP,MAXPAR).
! 
!         V(I,0) is the vertical fluid velocity at node I.
! 
!         V(I,J) is the sensitivity of the vertical velocity with
!         respect to parameter J.
! 
!  X1MAX,
!  X1MIN  Output, real X1MAX, X1MIN, the maximum and minimum X 
!         coordinates of the plot, which includes a small grace margin.
!
!  X2MAX,
!  X2MIN  Output, real X2MAX, X2MIN, the maximum and minimum X 
!         coordinates that should be used for plotting.  No plotting 
!         commands should exceed these values.  This is where the 
!         "frame" might be drawn.
!
!  XC     Output, real XC(MAXNP).
!         XC contains the X coordinates of the nodes.
! 
!  XMAX   Output, real XMAX.
!         The maximum X coordinate of all the nodes.
!         The maximum entry in the XC array.
! 
!  XMIN   Output, real XMIN.
!         The minimum X coordinate of all the nodes.
!         The minimum entry in the XC array.
! 
!  XSMAX  Output, real XSMAX.
!         The maximum X coordinate of the data to be displayed.
!         XSMAX defaults to XMAX, but can be made smaller to
!         focus on a portion of the region.
! 
!  XSMIN  Output, real XSMIN.
!         The minimum X coordinate of the data to be displayed.
!         XSMIN defaults to XMIN, but can be made larger to
!         focus on a portion of the region.
! 
!  Y1MAX,
!  Y1MIN  Output, real Y1MAX, Y1MIN, the maximum and minimum Y 
!         coordinates of the plot, which includes a small grace margin.
!
!  Y2MAX,
!  Y2MIN  Output, real Y2MAX, Y2MIN, the maximum and minimum Y 
!         coordinates that should be used for plotting.  No plotting 
!         commands should exceed these values.  This is where the 
!         "frame" might be drawn.
!  
!  YC     Output, real YC(MAXNP).
!         The Y coordinates of the nodes.
! 
!  YMAX   Output, real YMAX.
!         The maximum Y coordinate of all the nodes.
!         The maximum value attained by the YC array.
! 
!  YMIN   Output, real YMIN.
!         The minimum Y coordinate of all the nodes.
!         The minimum value attained by the YC array.
!  
!  YSMAX  Output, real YSMAX.
!         The maximum Y coordinate of the data to be displayed.
!         YSMAX defaults to YMAX, but can be made smaller to
!         focus on a portion of the region.
! 
!  YSMIN  Output, real YSMIN.
!         The minimum Y coordinate of the data to be displayed.
!         YSMIN defaults to YMIN, but can be made larger to
!         focus on a portion of the region.
! 
  integer ( kind = 4 ) maxbou
  integer ( kind = 4 ) maxelm
  integer ( kind = 4 ) maxnp
  integer ( kind = 4 ) maxnpe
!
  real delx
  real dely
  logical eflag(maxelm)
  character ( len = 2 )  eqn(3,maxnp)
  character ( len = 80 ) fildat
  real grace
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ifile
  logical inside
  integer ( kind = 4 ) isotri(maxelm)
  integer ( kind = 4 ) jbound(5,maxbou)
  integer ( kind = 4 ) jfile
  integer ( kind = 4 ) nbound
  integer ( kind = 4 ) nelem
  logical nflag(maxnp)
  logical nflag0(maxnp)
  integer ( kind = 4 ) node(maxnpe,maxelm)
  integer ( kind = 4 ) np
  integer ( kind = 4 ) npe
  integer ( kind = 4 ) nset
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny
  real p(maxnp)
  real rho(maxnp)
  real srange
  real u(maxnp)
  real v(maxnp)
  real xc(maxnp)
  real xmax
  real xmin
  real x1max
  real x1min
  real x2max
  real x2min
  real xsmax
  real xsmin
  real xtmax
  real xtmin
  real yc(maxnp)
  real ymax
  real ymin
  real y1max
  real y1min
  real y2max
  real y2min
  real ysmax
  real ysmin
  real ytmax
  real ytmin
!
!  Read nodal data.
!
  call rdnod ( fildat, jfile, np, p, rho, u, v, xc, yc )

  nset = 1
  ifile = 2
!
!  The values for NX, and NY are just dummy values for now.
!
  nx = 1
  ny = 1
!
!  We assume all triangles are nonisoperimetric.
!
  isotri(1:nelem) = 0
!
!  Initially, all elements will be visible.
!
  do i = 1,nelem
    eflag(i) = .true.
  end do
!
  do i = 1,np
    nflag(i) = .true.
    nflag0(i) = .true.
  end do
!
!  We give default values to the equations.
!
  do i = 1,np
    eqn(1,i) = 'U'
    eqn(2,i) = 'V'
    eqn(3,i) = 'P'
  end do

  write(*,*)' '
  write(*,*)'AdJeff - Note:'
  write(*,*)'  The node data has been read.'
!
!  Check element orientation.
!
  call element_check(maxnpe,nelem,node,np,npe,xc,yc)
!
!  Determine the region size.
!
  call region_size ( delx,dely,grace,nelem,nflag,np,srange, &
    x1max,x1min,x2max,x2min,xc,xmax,xmin,xsmax,xsmin,xtmax,xtmin,y1max, &
    y1min,y2max,y2min,yc,ymax,ymin,ysmax,ysmin,ytmax,ytmin)
!
!  Set the location of boundary edges.
!
  call boundary_set(eqn,jbound,maxbou,maxnpe,nbound,nelem,node,np,npe)
 
  return
end
subroutine advanc ( delx,dely,echo,eflag,eqn,grace,ifile,iset,isotri, &
  jbound,maxbou,maxelm,maxnp,maxnpe,maxny,maxpar,maxsen,nbound, &
  nelem,nflag,nflag0,node,np,npar,npe,nprof,nsen,nset,nx,ny,p, &
  para,ptar,rho,srange,u,utar,v,vtar,x1max,x1min,x2max,x2min,xc, &
  xmax,xmin,xsmax,xsmin,xtmax,xtmin,y1max,y1min,y2max,y2min,yc, &
  ymax,ymin,ysmax,ysmin,ytmax,ytmin)
!
!***********************************************************************
!
!! ADVANC responds to a user's "A" command by asking which step we are
!  to advance to, and then reading the information for that step.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    07 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real DELX, the X spacing between nodes.  In some cases,
!    this spacing is modified to create isoparametric elements.
! 
!    Output, real DELY, the Y spacing between nodes.  In some cases,
!    this spacing is modified to create isoparametric elements.
! 
!    Output, logical EFLAG(MAXELM).
!         EFLAG is used to "flag" which elements are to be displayed.
!         If EFLAG(I) is TRUE, then element I should be displayed.
! 
!    Output, character ( len = 2 )  EQN(3,NP).
!         EQN records the "type" of each equation that will be generated, and
!         which is associated with an unknown.  Note that most boundary 
!         conditions do not result in an equation.  The current values are:
!
!         'U'  The horizontal momentum equation.
!         'UW' The condition U = 0 applied at a node on a fixed wall.
!         'V'  The vertical momentum equation.
!         'VW' The condition V = 0 applied at a node on a fixed wall.
!         'P'  The continuity equation.
!         'PB' The condition P = 0 applied at (XMAX,YMAX).
!
!    Input, real GRACE.
!         The size of the "grace" margin on the plot.
! 
!  IFILE  Input/output, integer ( kind = 4 ) IFILE.
!         Records the status of the data file whose name is FILDAT.
! 
!         -2, an error occurred while reading from the file.
!         -1, the file could not be opened.
!          0, no file is currently open.
!          1, a file has been opened, but not read from.
!          2, data has been read from a file.
!
!  ISET   Output, integer ( kind = 4 ) ISET.
!         The data set being examined from the file.  If no file
!         is open, or if no data set has been read, then ISET is
!         zero.
! 
!  ISOTRI Output, integer ( kind = 4 ) ISOTRI(MAXELM).
! 
!         0, if element I is not isoparametric.
!         1 or 2, if element I is isoparametric.
! 
!  JBOUND Output, integer ( kind = 4 ) JBOUND(5,MAXBOU)
! 
!         For each line segment of the boundary:
! 
!         JBOUND(1,I) contains the element number;
! 
!         JBOUND(2,I) contains the local node number of one corner 
!           of the element, which forms the edge;
!
!         JBOUND(2,I) contains the "next" node along the edge.
!           If the element is linear, this is the other corner node.
!           If the element is quadratic, this is the midside node along
!             the edge.
!
!         JBOUND(4,I) contains the "next" node along the edge.
!           If the element is linear, this is 0.
!           If the element is quadratic, this is the other corner node 
!             along the edge.
!
!         JBOUND(5,I) contains:
!           0 if the boundary is a wall (U = V=0);
!           1 if the boundary is open.
!
!  MAXBOU Input, integer ( kind = 4 ) MAXBOU.
!         The amount of storage available for the IBOUND array.
! 
!  MAXELM Input, integer ( kind = 4 ) MAXELM.
!         The maximum number of elements which the program can
!         handle.
! 
!  MAXNP  Input, integer ( kind = 4 ) MAXNP, the maximum number of nodes.
!
!  MAXNPE Input, integer ( kind = 4 ) MAXNPE.
!         MAXNPE is the maximum number of nodes per element.
!
!  MAXPAR Input, integer ( kind = 4 ) MAXPAR.
!         The maximum number of parameters the program can handle.
! 
!  NBOUND Output, integer ( kind = 4 ) NBOUND.
!         The number of points (XBOUND(I),YBOUND(I)) used to
!         define the boundary.
! 
!  NELEM  Output, integer ( kind = 4 ) NELEM.
!         The number of elements.
! 
!  NFLAG  Output, logical NFLAG(MAXNP).
! 
!         NFLAG is used to "flag" which nodes are active,
!         that is, to be displayed, and which not, in the graph.
! 
!  NODE   Output, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), or NODE(MAXNPE,NELEM).
!         NODE contains, for each element, the global node numbers
!         of its NPE nodes.  
!
!         For linear elements (NPE = 3), the order of the nodes probably
!         doesn't matter, but we will draw them this way:
!
!                       2
!                      /|
!                    /  |
!                  /    |
!                /      |
!               1-------3
!
!         For quadratic elements (NPE = 6), the nodes must be given in a 
!         particular order, which is as follows:
!
!                       2
!                      /|
!                    /  |
!                  4    5
!                /      |
!               1---6---3
! 
!    Output, integer ( kind = 4 ) NP, the number of nodes.
!
!  NPAR   Output, integer ( kind = 4 ) NPAR.
!         The number of parameters.
! 
!  NPE    Output, integer ( kind = 4 ) NPE.
!         NPE is the number of nodes per element, which should be
!         3 for linear elements and 6 for quadratics.
!
!  NPROF  Output, integer ( kind = 4 ) NPROF(MY).
!         Records the indices of the nodes that lie along the profile 
!         line.
!
!  NSET   Output, integer ( kind = 4 ) NSET.
!         The number of sets of data contained in the data file.
! 
!  NX     Output, integer ( kind = 4 ) NX.
!         Determines the number of nodes and elements in the X
!         direction.  There will be 2*NX-1 nodes, 2*NX-2 elements.
!  
!  NY     Output, integer ( kind = 4 ) NY.
!         Determines the number of nodes and elements in the Y
!         direction.  There will be 2*NY-1 nodes, 2*NY-2 elements.
! 
!  P      Output, real P(MAXNP,0:MAXPAR).
! 
!         P(I,0) is the pressure at node I.
! 
!         P(I,J) is the sensitivity of the pressure with respect
!         to parameter J.
! 
!  PARA   Output, real PARA(MAXPAR).
!         The value of the parameters.
! 
!  PTAR   Output, real PTAR(MAXNP)
!         The pressure field associated with the target solution, at 
!         node I.
!
!  SRANGE Output, real SRANGE.
!         The maximum of XSMAX-XSMIN and YSMAX-YSMIN.
!         This gives the size of a square containing the data
!         window.
!
!  U      Output, real U(MAXNP,MAXPAR).
! 
!         U(I,0) is the horizontal fluid velocity at node I.
! 
!         U(I,J) is the sensitivity of the horizontal velocity with 
!         respect to parameter J.
! 
!  UTAR   Output, real UTAR(MAXNP)
!         The horizontal velocity field associated with the target 
!         solution, at node I.
!
!  V      Output, real V(MAXNP,MAXPAR).
! 
!         V(I,0) is the vertical fluid velocity at node I.
! 
!         V(I,J) is the sensitivity of the vertical velocity with
!         respect to parameter J.
! 
!  VTAR   Output, real VTAR(MAXNP)
!         The vertical velocity field associated with the target solution, 
!         at node I.
!
!  X1MAX,
!  X1MIN  Output, real X1MAX, X1MIN, the maximum and minimum X 
!         coordinates of the plot, which includes a small grace margin.
!
!  X2MAX,
!  X2MIN  Output, real X2MAX, X2MIN, the maximum and minimum X 
!         coordinates that should be used for plotting.  No plotting 
!         commands should exceed these values.  This is where the 
!         "frame" might be drawn.
!
!  XC     Output, real XC(MAXNP).
!         The X coordinates of the nodes.
! 
!  XMAX   Output, real XMAX.
!         The maximum X coordinate of all the nodes.
!         The maximum entry in the XC array.
! 
!  XMIN   Output, real XMIN.
!         The minimum X coordinate of all the nodes.
!         The minimum entry in the XC array.
! 
!  XSMAX  Output, real XSMAX.
!         The maximum X coordinate of the data to be displayed.
!         XSMAX defaults to XMAX, but can be made smaller to
!         focus on a portion of the region.
! 
!  XSMIN  Output, real XSMIN.
!         The minimum X coordinate of the data to be displayed.
!         XSMIN defaults to XMIN, but can be made larger to
!         focus on a portion of the region.
! 
!  Y1MAX,
!  Y1MIN  Output, real Y1MAX, Y1MIN, the maximum and minimum Y 
!         coordinates of the plot, which includes a small grace margin.
!
!  Y2MAX,
!  Y2MIN  Output, real Y2MAX, Y2MIN, the maximum and minimum Y 
!         coordinates that should be used for plotting.  No plotting 
!         commands should exceed these values.  This is where the 
!         "frame" might be drawn.
!  
!  YC     Output, real YC(MAXNP).
!         The Y coordinates of the nodes.
! 
!  YMAX   Output, real YMAX.
!         The maximum Y coordinate of all the nodes.
!         The maximum value attained by the YC array.
! 
!  YMIN   Output, real YMIN.
!         The minimum Y coordinate of all the nodes.
!         The minimum value attained by the YC array.
!  
!  YSMAX  Output, real YSMAX.
!         The maximum Y coordinate of the data to be displayed.
!         YSMAX defaults to YMAX, but can be made smaller to
!         focus on a portion of the region.
! 
!  YSMIN  Output, real YSMIN.
!         The minimum Y coordinate of the data to be displayed.
!         YSMIN defaults to YMIN, but can be made larger to
!         focus on a portion of the region.
! 
  integer ( kind = 4 ) maxbou
  integer ( kind = 4 ) maxelm
  integer ( kind = 4 ) maxnp
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) maxny
  integer ( kind = 4 ) maxpar
  integer ( kind = 4 ) maxsen
!
  character ( len = 6 ) chrint
  real delx
  real dely
  logical echo
  logical eflag(maxelm)
  character ( len = 2 )  eqn(3,maxnp)
  real grace
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) ifile
  logical inside
  integer ( kind = 4 ) iset
  integer ( kind = 4 ) isotri(maxelm)
  integer ( kind = 4 ) iwant
  integer ( kind = 4 ) jbound(5,maxbou)
  integer ( kind = 4 ) nbound
  integer ( kind = 4 ) nelem
  logical nflag(maxnp)
  logical nflag0(maxnp)
  integer ( kind = 4 ) node(maxnpe,maxelm)
  integer ( kind = 4 ) np
  integer ( kind = 4 ) npar
  integer ( kind = 4 ) npe
  integer ( kind = 4 ) nprof(2*maxny-1)
  integer ( kind = 4 ) nsen
  integer ( kind = 4 ) nset
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny
  real p(maxnp,0:maxsen)
  real para(maxpar)
  real ptar(maxnp)
  real rho(maxnp)
  real srange
  character ( len = 80 ) string
  real u(maxnp,0:maxsen)
  real utar(maxnp)
  real v(maxnp,0:maxsen)
  real vtar(maxnp)
  real x1max
  real x1min
  real x2max
  real x2min
  real xc(maxnp)
  real xmax
  real xmin
  real xprof
  real xsmax
  real xsmin
  real xtmax
  real xtmin
  real y1max
  real y1min
  real y2max
  real y2min
  real yc(maxnp)
  real ymax
  real ymin
  real ysmax
  real ysmin
  real ytmax
  real ytmin
!
!  ADVANC expects to use 6 node quadrilaterals.
!
  npe = 6
!
  if ( nset <= 0 ) then
    write(*,*)' '
    write(*,*)'Advanc - Error!'
    write(*,*)'  Please use the "DAT" command first, to define'
    write(*,*)'  a data file to read!'
    return
  end if
!
!  Get the desired step number.
!
10    continue

  write(*,*)' '
  string = 'There are a total of '//chrint(nset)//' steps in the data file.'
  call s_blanks_delete ( string)
  write(*,'(1x,a)')

  write(*,*)'What step would you like to see?'
  read(*,*,err = 60,end=60)iwant
  write(17,*)iwant
  if ( echo ) then
    write(*,*)iwant
  end if

  if ( iwant <= 0 ) then
    write(*,*)' '
    write(*,*)'Sorry, the step must be positive!'
    go to 10
  else if ( iwant > nset ) then
    write(*,*)' '
    string = 'Sorry, the step must be no greater than '// chrint(nset)
    call s_blanks_delete ( string)
    write(*,'(1x,a)')string
    go to 10
  end if
!
!  Advance to the desired step.
!
!  If this REWIND doesn't work, you may have to close and then reopen.
!
  rewind 2

  do iset = 0, iwant-1

    call plot_file_read ( eqn,ierror,ifile,iset,isotri,maxelm,maxnp, &
      maxnpe,maxny,maxpar,maxsen,nelem,node,np,npar,npe,nprof, &
      nsen,nx,ny,p,para,ptar,rho,u,utar,v,vtar,xc,xprof,yc)

    if ( ierror /= 0 ) then
      write(*,*)' '
      write(*,*)'ADVANC - Serious error!'
      write(*,*)'  Error occurred while trying to reach a desired'
      write(*,*)'  step.  We were reading information for step'
      write(*,*)iset
      return
    end if
 
  end do

  nflag(1:np) = .true.
  nflag0(1:np) = .true.
  eflag(1:nelem) = .true.

  write(*,*)' '
  write(*,*)'ADVANC - Note:'
  write(*,*)'  Read data for step ',iwant
!
!  Check the orientation of the elements.
!
  call element_check ( maxnpe, nelem, node, np, npe, xc, yc )
!
!  Determine the region size.
!
  call region_size ( delx,dely,grace,nelem,nflag,np,srange, &
    x1max,x1min,x2max,x2min,xc,xmax,xmin,xsmax,xsmin,xtmax,xtmin,y1max, &
    y1min,y2max,y2min,yc,ymax,ymin,ysmax,ysmin,ytmax,ytmin )
!
!  Compute the location of boundary edges.
!
  call boundary_set ( eqn, jbound, maxbou, maxnpe, nbound, nelem, node, &
    np, npe )

  return

60    continue
  write(*,*)' '
  write(*,*)'ADVANC - Serious error!'
  write(*,*)'  Error while reading user input.'
  write(*,*)' '
  return
end
subroutine arrlin ( xstart, ystart, xtip, ytip )
!
!***********************************************************************
!
!! ARRLIN can be used to make a line drawing of an arrow at any point
!  on a graph.
!
!  The arrow will stretch between two user specified points.
!
!  The "head" of the arrow may be fatter or thinner than expected
!  if the X and Y scales of the graph are not in the same
!  proportions.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 January 2001
!
!  Author:
!
!    John Burkardt
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
  real pi
  parameter (pi = 3.1415926)
!
  real alpha
  real del
  real dist
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
!
  if ( xstart == xtip.and.ystart == ytip)return

  theta = 0.5*pi-atan2(2.0,1.0)
  dist = sqrt((xtip-xstart)**2+(ytip-ystart)**2)
  alpha = atan2(ytip-ystart,xtip-xstart)
  del = sqrt(5.0)*dist/3.0

  call movcgm(xstart,ystart)

  xbase = xstart+dist*cos(alpha)*2.0/3.0
  ybase = ystart+dist*sin(alpha)*2.0/3.0
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
subroutine arrgon ( arrow, xstart, ystart, xtip, ytip )
!
!***********************************************************************
!
!! ARRGON can be used to make a polygonal drawing of an arrow at any 
!  point on a graph.
!
!  The arrow will stretch between two user specified points.
!
!  The "head" of the arrow may be fatter or thinner than expected
!  if the X and Y scales of the graph are not in the same
!  proportions.
!
!  It might be nice to include an "OUTLINE" option that draws the
!  arrow outline in black, and fills it with the current color.
!
!  By the way, the current color should either be a constant,
!  or depend on the velocity magnitude, choosable by the user.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 January 2001
!
!  Author:
!
!    John Burkardt
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
!                     5
!                     |\
!      7--------------6 \
!    start         base  4tip
!      1--------------2 /
!                     |/
!                     3
!                     rite
!
  real pi
  parameter (pi = 3.1415926)
!
  integer ( kind = 4 ) npts
  parameter (npts = 7)
!
  real alpha
  character ( len = 10 ) arrow
  real dist
  logical s_eqi
  real theta
  real xpts(npts+1)
  real xstart
  real xtip
  real ypts(npts+1)
  real ystart
  real ytip
!
!
  if ( xstart == xtip.and.ystart == ytip ) then
    return
  end if

  dist = sqrt((xtip-xstart)**2+(ytip-ystart)**2)

  theta = 0.5*pi-atan2(2.0,1.0)
  alpha = atan2(ytip-ystart,xtip-xstart)

  xpts(1) = xstart + dist*sin(alpha)/10.0
  ypts(1) = ystart - dist*cos(alpha)/10.0

  xpts(2) = xstart + dist*sin(alpha)/10.0 + dist*cos(alpha)*2.0/3.0
  ypts(2) = ystart - dist*cos(alpha)/10.0 + dist*sin(alpha)*2.0/3.0

  xpts(3) = xstart + dist*sqrt(5.0)*cos(alpha-theta)/3.0
  ypts(3) = ystart + dist*sqrt(5.0)*sin(alpha-theta)/3.0

  xpts(4) = xtip
  ypts(4) = ytip

  xpts(5) = xstart + dist*sqrt(5.0)*cos(alpha+theta)/3.0
  ypts(5) = ystart + dist*sqrt(5.0)*sin(alpha+theta)/3.0

  xpts(6) = xstart - dist*sin(alpha)/10.0 + dist*cos(alpha)*2.0/3.0
  ypts(6) = ystart + dist*cos(alpha)/10.0 + dist*sin(alpha)*2.0/3.0

  xpts(7) = xstart - dist*sin(alpha)/10.0
  ypts(7) = ystart + dist*cos(alpha)/10.0

  xpts(8) = xpts(1)
  ypts(8) = ypts(1)

  if ( s_eqi ( arrow,'hollow') ) then

    call plylin(npts+1,xpts,ypts)

  end if

  if ( s_eqi ( arrow,'solid') ) then

    call plygon(npts,xpts,ypts)

  end if

  return
end
subroutine bfl ( ielem, in, w, dwdx, dwdy, maxnpe, nelem, node, np, xc, &
  xq, yc, yq )
!
!***********************************************************************
!
!! BFL evaluates a particular linear basis function at a point
!  in a nonisoparametric element.
!
!      ^
!      |        2
!      |       /|
!   Y  |      / |
!      |     /  |
!      |    1---3
!      |
!      +------------>
!             X
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!  IELEM  Input, integer ( kind = 4 ) IELEM, the number of the element we are
!         examining.  This will be a value between 1 and NELEM.
!
!  IN     Input, integer ( kind = 4 ) IN, the number of the basis function we
!         want.  This will be a value between 1 and 3.  
!
!  W,
!  DWDX,
!  DWDY   Output, real W, DWDX, DWDY, the value of the
!         IN-th basis  function and its X and Y derivatives, at the
!         given point.
!
!  MAXNPE Input, integer ( kind = 4 ) MAXNPE.
!         MAXNPE is the maximum number of nodes per element.
!
!    Input, integer ( kind = 4 ) NELEM, the number of elements.
!
!  NODE   Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), or NODE(MAXNPE,NELEM).
!         NODE contains, for each element, the global node numbers
!         of its NPE nodes.  
!
!         For linear elements (NPE = 3), the order of the nodes probably
!         doesn't matter, but we will draw them this way:
!
!                       2
!                      /|
!                    /  |
!                  /    |
!                /      |
!               1-------3
!
!         For quadratic elements (NPE = 6), the nodes must be given in a 
!         particular order, which is as follows:
!
!                       2
!                      /|
!                    /  |
!                  4    5
!                /      |
!               1---6---3
! 
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!  XC     Input, real XC(NP), the X coordinates of the
!         nodes.
!
!  XQ     Input, real XQ, the X coordinate of the point
!         where the basis function is to be evaluated.
!
!  YC     Input, real YC(NP), the Y coordinates of the nodes
!
!  YQ     Input, real YQ, the Y coordinate of the point wher
!         the basis function is to be evaluated.
!
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) np
!
  real d
  real dwdx
  real dwdy
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) i3
  integer ( kind = 4 ) ielem
  integer ( kind = 4 ) in
  integer ( kind = 4 ) in1
  integer ( kind = 4 ) in2
  integer ( kind = 4 ) in3
  integer ( kind = 4 ) node(maxnpe,nelem)
  real w
  real xc(np)
  real xq
  real yc(np)
  real yq
!
!  IN1, IN2, and IN3 are the local node numbers of the three
!  corner nodes.
!
  in1 = in
  in2 = mod(in,3)+1
  in3 = mod(in+1,3)+1
!
!  I1, I2, I3 are the global node numbers.
!
  i1 = node(in1,ielem)
  i2 = node(in2,ielem)
  i3 = node(in3,ielem)
 
  d = (xc(i2)-xc(i1))*(yc(i3)-yc(i1))-(xc(i3)-xc(i1))*(yc(i2)-yc(i1))
 
  if ( d == 0.0 ) then
    write(*,*)' '
    write(*,*)'BFL - Fatal error!'
    write(*,*)'  D = 0'
    write(*,*)'  Element IELEM = ',ielem
    write(*,*)'  I1, XC(I1), YC(I1) = ',i1,xc(i1),yc(i1)
    write(*,*)'  I2, XC(I2), YC(I2) = ',i2,xc(i2),yc(i2)
    write(*,*)'  I3, XC(I3), YC(I3) = ',i3,xc(i3),yc(i3)
    stop
  end if

  dwdx = (yc(i2)-yc(i3))/d
  dwdy = (xc(i3)-xc(i2))/d

  w = 1.0 + dwdx*(xq-xc(i1)) + dwdy*(yq-yc(i1))

  return
end
subroutine bfq ( ielem,in,w,dwdx,dwdy,maxnpe,nelem,node,np,xc,xq,yc,yq )
!
!***********************************************************************
!
!! BFQ evaluates a particular quadratic basis function at a point
!  in a nonisoparametric element.
!
!      ^
!      |        2
!      |       /|
!   Y  |      4 5
!      |     /  |
!      |    1-6-3
!      |
!      +------------>
!             X
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!  IELEM  Input, integer ( kind = 4 ) IELEM, the number of the element we are
!         examining.  This will be a value between 1 and NELEM.
!
!  IN     Input, integer ( kind = 4 ) IN, the number of the basis function we
!         want.  This will be a value between 1 and 6.  Functions
!         1 through 3 are associated with corners, 4 though 6
!         with sides.
!
!  W,
!  DWDX,
!  DWDY   Output, real W, DWDX, DWDY, the value of the
!         IN-th basis  function and its X and Y derivatives, at the
!         given point.
!
!  MAXNPE Input, integer ( kind = 4 ) MAXNPE.
!         MAXNPE is the maximum number of nodes per element.
!
!    Input, integer ( kind = 4 ) NELEM, the number of elements.
!
!  NODE   Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), or NODE(MAXNPE,NELEM).
!         NODE contains, for each element, the global node numbers
!         of its NPE nodes.  
!
!         For linear elements (NPE = 3), the order of the nodes probably
!         doesn't matter, but we will draw them this way:
!
!                       2
!                      /|
!                    /  |
!                  /    |
!                /      |
!               1-------3
!
!         For quadratic elements (NPE = 6), the nodes must be given in a 
!         particular order, which is as follows:
!
!                       2
!                      /|
!                    /  |
!                  4    5
!                /      |
!               1---6---3
! 
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!  XC     Input, real XC(NP), the X coordinates of the
!         nodes.
!
!  XQ     Input, real XQ, the X coordinate of the point
!         where the basis function is to be evaluated.
!
!  YC     Input, real YC(NP), the Y coordinates of the nodes
!
!  YQ     Input, real YQ, the Y coordinate of the point wher
!         the basis function is to be evaluated.
!
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) np
!
  real c
  real d
  real dwdx
  real dwdy
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) i3
  integer ( kind = 4 ) ielem
  integer ( kind = 4 ) in
  integer ( kind = 4 ) in1
  integer ( kind = 4 ) in2
  integer ( kind = 4 ) in3
  integer ( kind = 4 ) node(maxnpe,nelem)
  real s
  real t
  real w
  real xc(np)
  real xq
  real yc(np)
  real yq
!
!  Case 1: We are inquiring about a basis function associated
!  with a corner.
!
!  Notice that the basis function W is zero exactly if
!  T is 0 or T is 1/2.
!
!  IN1, IN2, and IN3 are the local node numbers of the three
!  corner nodes, and I1, I2 and I3 are the corresponding
!  global node numbers, which are used to look up the X and
!  Y coordinates of the nodes.
!
  if ( 1 <= in.and.in <= 3 ) then
 
    in1 = in
    in2 = mod(in,3)+1
    in3 = mod(in+1,3)+1
 
    i1 = node(in1,ielem)
    i2 = node(in2,ielem)
    i3 = node(in3,ielem)
 
    d = (xc(i2)-xc(i1))*(yc(i3)-yc(i1))-(xc(i3)-xc(i1))*(yc(i2)-yc(i1))
 
    if ( d == 0.0 ) then
      write(*,*)' '
      write(*,*)'BFQ - Fatal error!'
      write(*,*)'  D = 0'
      write(*,*)'  Element IELEM = ',ielem
      write(*,*)'  I1, XC(I1), YC(I1) = ',i1,xc(i1),yc(i1)
      write(*,*)'  I2, XC(I2), YC(I2) = ',i2,xc(i2),yc(i2)
      write(*,*)'  I3, XC(I3), YC(I3) = ',i3,xc(i3),yc(i3)
      stop
    end if

    t = 1.0+( (xq    -xc(i1))*(yc(i2)-yc(i3)) &
            +(xc(i3)-xc(i2))*(yq    -yc(i1)) )/d
 
    w = t*(2.0*t-1.0)
 
    dwdx = (yc(i2)-yc(i3))*(4.0*t-1.0)/d
    dwdy = (xc(i3)-xc(i2))*(4.0*t-1.0)/d
!
!  Case 2: We are inquiring about a basis function associated
!  with a midpoint.
!
  else if ( in >= 4.and.in <= 6 ) then
 
    in1 = in-3
    in2 = mod(in-3,3)+1
    in3 = mod(in-2,3)+1
 
    i1 = node(in1,ielem)
    i2 = node(in2,ielem)
    i3 = node(in3,ielem)
 
    d =     (xc(i2)-xc(i1))*(yc(i3)-yc(i1)) &
            -(xc(i3)-xc(i1))*(yc(i2)-yc(i1))

    if ( d == 0.0 ) then
      write(*,*)' '
      write(*,*)'BFQ - Fatal error!'
      write(*,*)'  D = 0'
      write(*,*)'  Element IELEM = ',ielem
      write(*,*)'  I1, XC(I1), YC(I1) = ',i1,xc(i1),yc(i1)
      write(*,*)'  I2, XC(I2), YC(I2) = ',i2,xc(i2),yc(i2)
      write(*,*)'  I3, XC(I3), YC(I3) = ',i3,xc(i3),yc(i3)
      stop
    end if

    c =     (xc(i3)-xc(i2))*(yc(i1)-yc(i2))-(xc(i1)-xc(i2))*(yc(i3)-yc(i2))

    if ( c == 0.0 ) then
      write(*,*)' '
      write(*,*)'BFQ - Fatal error!'
      write(*,*)'  C = 0'
      write(*,*)'  Element IELEM = ',ielem
      write(*,*)'  I1, XC(I1), YC(I1) = ',i1,xc(i1),yc(i1)
      write(*,*)'  I2, XC(I2), YC(I2) = ',i2,xc(i2),yc(i2)
      write(*,*)'  I3, XC(I3), YC(I3) = ',i3,xc(i3),yc(i3)
      stop
    end if

    t = 1.0+( (xq    -xc(i1))*(yc(i2)-yc(i3))+(xc(i3)-xc(i2))*(yq    -yc(i1)) )/d
 
    s = 1.0+( (xq    -xc(i2))*(yc(i3)-yc(i1))+(xc(i1)-xc(i3))*(yq    -yc(i2)) )/c
 
    w = 4.0 * s*t
    dwdx = 4.0 * ((yc(i3)-yc(i1))*t/c + (yc(i2)-yc(i3))*s/d)
    dwdy = 4.0 * ((xc(i1)-xc(i3))*t/c + (xc(i3)-xc(i2))*s/d)
 
  else
 
    write(*,*)' '
    write(*,*)'BFQ - Fatal error!'
    write(*,*)'  Request for basis function IN = ',in
    write(*,*)'  but IN must be between 1 and 6.'
    stop
 
  end if
 
  return
end
subroutine bfrefl ( w,dwdx,dwdy,detadx,detady,dxsidx,dxsidy,eta,iq,xsi )
!
!***********************************************************************
!
!! BFREFL evaluates one of the three linear basis functions,
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
!  Modified:
!
!    12 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!  W,
!  DWDX,
!  DWDY   Output, real W, DWDX, DWDY, the value of the basis
!         function, and its derivatives with respect to X and Y, at
!         the point (ETA,XSI).
!
!  DETADX,
!  DETADY Input, real DETADX, DETADY, the partial derivative
!         d ETA/d X and d ETA/d Y at (ETA,XSI).
!
!  DXSIDX,
!  DXSIDY Input, real DXSIDX, DXSIDY, the partial derivative
!         d XSI/d X and d XSI/d Y at (ETA,XSI).
!
!  ETA    Input, real ETA, the local ETA coordinate 
!         at which the basis information is desired.
!
!  IQ     Input, integer ( kind = 4 ) IQ, the local node number, between 1 and
!         3, whose basis function is being evaluated.
!
!  XSI    Input, real XSI, the local XSI coordinate 
!         at which the basis information is desired.
!
  real detadx
  real detady
  real dwdeta
  real dwdx
  real dwdxsi
  real dwdy
  real dxsidx
  real dxsidy
  real eta
  integer ( kind = 4 ) iq
  real w
  real xsi
!
  if ( iq == 1 ) then
    w = 1.0-xsi
    dwdxsi = -1.0
    dwdeta =  0.0
  else if ( iq == 2 ) then
    w = eta
    dwdxsi = 0.0
    dwdeta = 1.0
  else if ( iq == 3 ) then
    w = xsi-eta
    dwdxsi = 1.0
    dwdeta = -1.0
  else
    write(*,*)' '
    write(*,*)'BFRefL - Fatal error!'
    write(*,*)'  Request for basis function IQ = ',iq
    write(*,*)'  but IQ must be between 1 and 3.'
    stop
  end if
 
  dwdx = dwdxsi*dxsidx+dwdeta*detadx
  dwdy = dwdxsi*dxsidy+dwdeta*detady
 
  return
end
subroutine bfrefq ( w,dwdx,dwdy,detadx,detady,dxsidx,dxsidy,eta,iq,xsi )
!
!***********************************************************************
!
!! BFREFQ evaluates one of the six quadratic basis functions,
!  and its X and Y derivatives, at a particular point in a
!  finite element, by referring to the reference triangle.
!
!  The point we are interested in is referred to by its coordinates
!  in the reference triangle.  That is, we are given coordinates
!  (XSI, ETA), even though, physically, we are interested 
!  in points in (X, Y) space.
!
!  It is assumed that we already know the value of the jacobian
!  of the isoparametric transformation between the (XSI, ETA) and
!  (X, Y) spaces.  The four entries of the jacobian are 
!  symbolically named DETADX, DETADY, DXSIDX and DXSIDY, and
!  we know that the jacobian gives us the following relation
!  between derivatives with respect to XSI and ETA, and derivatives
!  with respect to X and Y:
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
!  ETA   |      4 5
!        |     /  |
!      0 +    1-6-3
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
!  Modified:
!
!    12 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!  W,
!  DWDX,
!  DWDY   Output, real W, DWDX, DWDY, the value of the basis 
!         function, and its derivatives with respect to X and Y, at 
!         the point (XSI,ETA).
!
!  DETADX,
!  DETADY Input, real DETADX, DETADY, the partial derivatives 
!         d ETA/d X and d ETA/d Y at (XSI,ETA).
!
!  DXSIDX,
!  DXSIDY Input, real DXSIDX, DXSIDY, the partial derivatives 
!         d XSI/d X and d XSI/d Y at (XSI,ETA).
!
!  ETA    Input, real ETA, the ETA coordinate of the point.
!
!  IQ     Input, integer ( kind = 4 ) IQ, the local node number, between 1 and 6, 
!         whose basis function is being evaluated.
!
!  XSI    Input, real XSI, the XSI coordinate of the point.
!
  real detadx
  real detady
  real dwdeta
  real dwdx
  real dwdxsi
  real dwdy
  real dxsidx
  real dxsidy
  real eta
  integer ( kind = 4 ) iq
  real w
  real xsi
!
!  Find the formula for the desired basis function, and evaluate it.
!  Also evaluate the partial derivatives d/d XSI and d/d ETA.
!
!  Basis function 1 is zero if XSI = 0.5 or XSI=1.
!
  if ( iq == 1 ) then
    w =  (2.0*xsi-1.0) * (xsi-1.0)
    dwdxsi = -3.0+4.0*xsi
    dwdeta = 0.0
!
!  Basis function 2 is zero if ETA = 0 or ETA=0.5.
!
  else if ( iq == 2 ) then
    w =  eta * (2.0*eta-1.0)
    dwdxsi = 0.0
    dwdeta = -1.0+4.0*eta
!
!  Basis function 3 is zero if XSI = ETA, or XSI=ETA+0.5
!
  else if ( iq == 3 ) then
    w =  (xsi-eta) * (2.0*xsi-2.0*eta-1.0)
    dwdxsi = -1.0+4.0*xsi-4.0*eta
    dwdeta = 1.0-4.0*xsi+4.0*eta
!
!  Basis function 4 is zero if ETA = 0 or XSI=1.
!
  else if ( iq == 4 ) then
    w =  4.0 * eta * (1.0-xsi)
    dwdxsi = -4.0*eta
    dwdeta = 4.0-4.0*xsi
!
!  Basis function 5 is zero if ETA = 0 or XSI=ETA.
!
  else if ( iq == 5 ) then
    w = 4.0 * eta * (xsi-eta)
    dwdxsi = 4.0*eta
    dwdeta = 4.0*xsi-8.0*eta
!
!  Basis function 6 is zero if XSI = ETA or XSI=1.
!
  else if ( iq == 6 ) then
    w = 4.0 * (xsi-eta) * (1.0-xsi)
    dwdxsi = 4.0-8.0*xsi+4.0*eta
    dwdeta = -4.0+4.0*xsi

  else
    write(*,*)' '
    write(*,*)'BFRefQ - Fatal error!'
    write(*,*)'  Illegal value of IQ = ',iq
    stop
  end if
!
!  Convert the d/d XSI and d/d ETA derivatives to d/d X and d/d Y.
!
  dwdx = dwdxsi*dxsidx+dwdeta*detadx
  dwdy = dwdxsi*dxsidy+dwdeta*detady

  return
end
subroutine boundary_set ( eqn,jbound,maxbou,maxnpe,nbound,nelem,node,np,npe )
!
!***********************************************************************
!
!! BOUNDARY_SET deduces which edges of the finite element triangle constitute 
!  boundaries of the region.
!
!  It does this stupidly, by considering each element, and then each
!  edge of that element, and seeing if it occurs (in the opposite 
!  orientation) in any other element.
!
!  For large regions, SETBND is bound to be slow.  If you know
!  where your boundaries occur, then you might want to replace
!  BOUND by a simpler routine.  However, SETBND is guaranteed
!  to work for a wide variety of regions.
!
!  If an edge only occurs in one element, and no velocities
!  are specified along it, then it is a wall, and we record it
!  as a boundary.
!
!  Only edges formed by the three corner nodes are considered.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!  EQN    Input, character ( len = 2 )  EQN(3,NP).
!         EQN records the "type" of each equation that will be generated, and
!         which is associated with an unknown.  Note that most boundary 
!         conditions do not result in an equation.  The current values are:
!
!         'U'  The horizontal momentum equation.
!         'UW' The condition U = 0 applied at a node on a fixed wall.
!         'V'  The vertical momentum equation.
!         'VW' The condition V = 0 applied at a node on a fixed wall.
!         'P'  The continuity equation.
!         'PB' The condition P = 0 applied at (XMAX,YMAX).
!
!  JBOUND Output, integer ( kind = 4 ) JBOUND(5,MAXBOU)
! 
!         For each line segment of the boundary:
! 
!         JBOUND(1,I) contains the element number;
! 
!         JBOUND(2,I) contains the local node number of one corner 
!           of the element, which forms the edge;
!
!         JBOUND(2,I) contains the "next" node along the edge.
!           If the element is linear, this is the other corner node.
!           If the element is quadratic, this is the midside node along
!             the edge.
!
!         JBOUND(4,I) contains the "next" node along the edge.
!           If the element is linear, this is 0.
!           If the element is quadratic, this is the other corner node 
!             along the edge.
!
!         JBOUND(5,I) contains:
!           0 if the boundary is a wall (U = V=0);
!           1 if the boundary is open.
!
!  MAXBOU Input, integer ( kind = 4 ) MAXBOU.
!         The amount of storage available for the IBOUND array.
! 
!  MAXNPE Input, integer ( kind = 4 ) MAXNPE.
!         MAXNPE is the maximum number of nodes per element.
!
!  NBOUND Input, integer ( kind = 4 ) NBOUND.
!         The number of points (XBOUND(I),YBOUND(I)) used to
!         define the boundary.
!  
!    Input, integer ( kind = 4 ) NELEM.
!         The number of elements.
! 
!  NODE   Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), or NODE(MAXNPE,NELEM).
!         NODE contains, for each element, the global node numbers
!         of its NPE nodes.  
!
!         For linear elements (NPE = 3), the order of the nodes probably
!         doesn't matter, but we will draw them this way:
!
!                       2
!                      /|
!                    /  |
!                  /    |
!                /      |
!               1-------3
!
!         For quadratic elements (NPE = 6), the nodes must be given in a 
!         particular order, which is as follows:
!
!                       2
!                      /|
!                    /  |
!                  4    5
!                /      |
!               1---6---3
! 
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!  NPE    Input, integer ( kind = 4 ) NPE.
!         NPE is the number of nodes per element, which should be
!         3 for linear elements and 6 for quadratics.

  integer ( kind = 4 ) maxbou
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) np
!
  character ( len = 2 )  eqn(3,np)
  integer ( kind = 4 ) i
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
  integer ( kind = 4 ) node(maxnpe,nelem)
  integer ( kind = 4 ) npe
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
!  For each element I...
!
  do i = 1,nelem
!
!  ...consider, for J = 1 to NEDGE, edges of the form 
!  (N1, N2)  =  (NODE(J,I), NODE(J+1,I))...
!
    do j = 1,nedge
      n1 = node(j,i)
      jp1 = j+1
      if ( j == nedge)jp1 = 1
      n2 = node(jp1,i)
!
!  ...and now, for each element K,...
!
      do k = 1,nelem
!
!  ...where K is different from I, ...
!
        if ( k /= i ) then
!
!  ...consider, for L = 1 to NEDGE, edges of the form 
!  (M1, M2)  =  (NODE(L,K), NODE(L+1,K))...
!
          do l = 1,nedge
            m1 = node(l,k)
            lp1 = l+1
            if ( l == nedge)lp1 = 1
            m2 = node(lp1,k)
!
!  And if (N1, N2)  =  (M1, M2), or (N1, N2) = (M2, M1), then skip out.
!
            if (  (m1 == n1.and.m2 == n2) .or. (m1 == n2.and.m2 == n1) ) then
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
        write(*,*)' '
        write(*,*)'SETBND - Warning!'
        write(*,*)'  Ran out of space to store boundary!'
        write(*,*)'  Currently using ',nbound,' boundary edges.'
        return
      end if
!
!  Increment the number of boundary edges found, and store
!  the indices of the element, the first node, the middle node 
!  (if quadratic), and the second node.
!
      nbound = nbound+1
      jbound(1,nbound) = i
      jbound(2,nbound) = j

      if ( npe == 3 ) then
        jbound(3,nbound) = jp1
      else if ( npe == 4 ) then
        jbound(3,nbound) = jp1         
      else if ( npe == 6 ) then
        jbound(3,nbound) = j+3
      end if

      if ( npe == 3 ) then
        jbound(4,nbound) = 0
      else if ( npe == 4 ) then
        jbound(4,nbound) = 0
      else if ( npe == 6 ) then
        jbound(4,nbound) = jp1
      end if
!
!  Now try to figure out what kind of boundary segment it is.
!
!  It is OPEN if there is an unknown U velocity at either end node
!  which does not correspond to a wall,
!  or there is a specified U velocity at either end node.
!
      if ( eqn(1,n1) == 'UW'.and.eqn(1,n2) == 'UW' ) then
        jbound(5,nbound) = 0
      else
        jbound(5,nbound) = 1
      end if

10        continue
 
    end do
  end do

  write(*,*)' '
  write(*,*)'SETBND - Note:'
  write(*,*)'  Found NBOUND = ',nbound,' boundary edges.'

  return
end
subroutine boundary_show ( eflag, etaref, isotri, jbound, line, maxbou, maxnpe, &
  maxobj, nbound, nelem, nflag, node, np, npe, xc, xsiref, yc )
!
!***********************************************************************
!
!! BOUNDARY_SHOW displays the boundary.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, logical EFLAG(NELEM), flags the elements to be displayed.
!    If EFLAG(I) is TRUE, then element I should be displayed.
! 
!  ETAREF Input, real ETAREF(MAXNPE).
!         The ETA coordinates of the nodes of the reference triangle.
! 
!  ISOTRI Input, integer ( kind = 4 ) ISOTRI(NELEM).
! 
!         0, if element I is not isoparametric.
!         1 or 2, if element I is isoparametric.
!
!  JBOUND Input, integer ( kind = 4 ) JBOUND(5,MAXBOU)
! 
!         For each line segment of the boundary:
! 
!         JBOUND(1,I) contains the element number;
! 
!         JBOUND(2,I) contains the local node number of one corner 
!           of the element, which forms the edge;
!
!         JBOUND(2,I) contains the "next" node along the edge.
!           If the element is linear, this is the other corner node.
!           If the element is quadratic, this is the midside node along
!             the edge.
!
!         JBOUND(4,I) contains the "next" node along the edge.
!           If the element is linear, this is 0.
!           If the element is quadratic, this is the other corner node 
!             along the edge.
!
!         JBOUND(5,I) contains:
!           0 if the boundary is a wall (U = V=0);
!           1 if the boundary is open.
!
!  MAXBOU Input, integer ( kind = 4 ) MAXBOU.
!         The amount of storage available for the IBOUND array.
! 
!  MAXNPE Input, integer ( kind = 4 ) MAXNPE.
!         MAXNPE is the maximum number of nodes per element.
!
!  NBOUND Input, integer ( kind = 4 ) NBOUND.
!         The number of points (XBOUND(I),YBOUND(I)) used to
!         define the boundary.
! 
!    Input, integer ( kind = 4 ) NELEM, the number of elements.
!
!  NFLAG  Input, logical NFLAG(NP).
!
!         NFLAG is used to "flag" which nodes are active,
!         that is, to be displayed, and which not, in the graph.
!
!  NODE   Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), or NODE(MAXNPE,NELEM).
!         NODE contains, for each element, the global node numbers
!         of its NPE nodes.  
!
!         For linear elements (NPE = 3), the order of the nodes probably
!         doesn't matter, but we will draw them this way:
!
!                       2
!                      /|
!                    /  |
!                  /    |
!                /      |
!               1-------3
!
!         For quadratic elements (NPE = 6), the nodes must be given in a 
!         particular order, which is as follows:
!
!                       2
!                      /|
!                    /  |
!                  4    5
!                /      |
!               1---6---3
! 
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!  NPE    Input, integer ( kind = 4 ) NPE.
!         NPE is the number of nodes per element, which should be
!         3 for linear elements and 6 for quadratics.
!
!  XC     Input, real XC(NP).
!         The X coordinates of the nodes.
! 
!  XSIREF Input, real XSIREF(MAXNPE).
!         The XSI coordinates of the nodes of the reference
!         triangle.
!
!  YC     Input, real YC(NP).
!         The Y coordinates of the nodes.
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
  integer ( kind = 4 ) isotri(nelem)
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
!
!  Consider the I-th triangle edge that lies on the boundary.
!
  do i = 1,nbound
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
!  Boundary stretch is part of nonisoparametric element.
!
      if ( npe == 3 .or. npe == 4 .or. isotri(ielem) == 0 ) then

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
        if ( line(1) == 0 .or. line(1) == 2 ) then

          if ( nflag(ng1) .and. nflag(ng2) ) then
            call movcgm(xc(ng1),yc(ng1))
            call drwcgm(xc(ng2),yc(ng2))
          end if

          if ( npe == 6 ) then
            if ( nflag(ng2).and.nflag(ng3) ) then
              call movcgm(xc(ng2),yc(ng2))
              call drwcgm(xc(ng3),yc(ng3))
            end if
          end if
!
!  Draw a solid wall.
!
        else

          if ( jbound(5,i) == 0 ) then

            if ( nflag(ng1).and.nflag(ng2) ) then
              call movcgm(xc(ng1),yc(ng1))
              call drwcgm(xc(ng2),yc(ng2))
            end if

            if ( npe == 6 ) then
              if ( nflag(ng2).and.nflag(ng3) ) then
                call movcgm(xc(ng2),yc(ng2))
                call drwcgm(xc(ng3),yc(ng3))
              end if
            end if
!
!  Draw an dashed open boundary.
!
          else

            if ( nflag(ng1).and.nflag(ng2) ) then
              call movcgm(xc(ng1),yc(ng1))
              xtemp = xc(ng1)+0.25*(xc(ng2)-xc(ng1))
              ytemp = yc(ng1)+0.25*(yc(ng2)-yc(ng1))
              call drwcgm(xtemp,ytemp)

              xtemp = xc(ng1)+0.75*(xc(ng2)-xc(ng1))
              ytemp = yc(ng1)+0.75*(yc(ng2)-yc(ng1))
              call movcgm(xtemp,ytemp)
              call drwcgm(xc(ng2),yc(ng2))
            end if

            if ( npe == 6 ) then
              if ( nflag(ng2).and.nflag(ng3) ) then
                call movcgm(xc(ng2),yc(ng2))
                xtemp = xc(ng2)+0.25*(xc(ng3)-xc(ng2))
                ytemp = yc(ng2)+0.25*(yc(ng3)-yc(ng2))
                call drwcgm(xtemp,ytemp)

                xtemp = xc(ng2)+0.75*(xc(ng3)-xc(ng2))
                ytemp = yc(ng2)+0.75*(yc(ng3)-yc(ng2))
                call movcgm(xtemp,ytemp)
                call drwcgm(xc(ng3),yc(ng3))
              end if
            end if

          end if

        end if
!
!  Boundary stretch that is part of an isoparametric element.
!
      else

        if ( npe == 6 ) then
          jhi = 2
        end if

        do j = 1,jhi

          j1 = jbound(j+1,i)
          j2 = jbound(j+2,i)

          x1 = xsiref(j1)
          y1 = etaref(j1)

          x2 = xsiref(j2)
          y2 = etaref(j2)

          if ( line(1) == 0 .or. line(1) == 2 ) then

            call isoln6(x1,y1,x2,y2,ielem,maxnpe,nelem,node,np,npe,xc,yc)

          else

            if ( jbound(5,i) == 0 ) then

              call isoln6(x1,y1,x2,y2,ielem,maxnpe,nelem,node,np,npe,xc,yc)

            else

              x1 = xsiref(j1)
              y1 = etaref(j1)

              x3 = x1+0.25*(x2-x1)
              y3 = y1+0.25*(y2-y1)
 
              call isoln6(x1,y1,x3,y3,ielem,maxnpe,nelem,node,np,npe,xc,yc)

              x3 = x1+0.75*(x2-x1)
              y3 = y1+0.75*(y2-y1)
 
              call isoln6(x3,y3,x2,y2,ielem,maxnpe,nelem,node,np,npe,xc,yc)

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
!    Input, real XMIN, XMAX, the minimum and maximum X
!    coordinates of the box.
!
!    Input, real YMIN, YMAX, the minimum and maximum Y
!    coordinates of the box.
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
  character c
  integer ( kind = 4 ) itemp
!
  itemp = ichar ( c )

  if ( 97 <= itemp .and. itemp <= 122 ) then
    c = char ( itemp - 32 )
  end if

  return
end
subroutine cbar ( icolor,jcmax,jcmin,maxobj,ncon,smax,smin,srange,x1,x2,y1,y2)
!
!***********************************************************************
!
!! CBAR draws a color bar in the rectangle whose corners are 
!  (X1,Y1) and (X2,Y2).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
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
!  ICOLOR Input, integer ( kind = 4 ) ICOLOR(MAXOBJ).
!         ICOLOR contains the color index for each object.
! 
!  JCMAX,
!  JCMIN  Input, integer ( kind = 4 ) JCMAX, JCMIN, the maximum and 
!         minimum color indices to use in the color bar.
!
!  MAXOBJ Input, integer ( kind = 4 ) MAXOBJ.
!         The number of graphical "objects".
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
!  SRANGE Input, real SRANGE.
!         The maximum of XSMAX-XSMIN and YSMAX-YSMIN.
!         This gives the size of a square containing the data
!         window.
!
!  X1,
!  X2,
!  Y1,
!  Y2     Input, real X1, X2, Y1, Y2, specify the minimum and
!         maximum X and Y coordinates of the color bar.
!
  integer ( kind = 4 ) maxobj
!
  real angle
  character ( len = 14 ) chrrel
  character ( len = 14 ) chrtmp
  real cwide
  character ( len = 6 ) flush
  integer ( kind = 4 ) i
  integer ( kind = 4 ) icolor(maxobj)
  integer ( kind = 4 ) jcmax
  integer ( kind = 4 ) jcmin
  integer ( kind = 4 ) jcolor
  integer ( kind = 4 ) lent
  integer ( kind = 4 ) ncon
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
!
!
  ycorn(1) = y1
  ycorn(2) = y1
  ycorn(3) = y2
  ycorn(4) = y2
  ycorn(5) = y1

  call linclr(icolor(1))

  do i = 0,ncon

    xl = ((ncon+1-i)*x1+i*x2)/real(ncon+1)
    xr = ((ncon-i)*x1+(i+1)*x2)/real(ncon+1)

    xcorn(1) = xl
    xcorn(2) = xr
    xcorn(3) = xr
    xcorn(4) = xl
    xcorn(5) = xl

    jcolor = int(((ncon-i)*jcmin+i*jcmax)/real(ncon))
    call filclr(jcolor)

    call plygon(4,xcorn,ycorn)

    call plylin(5,xcorn,ycorn)

  end do
!
!  Print labels for the lowest and highest contours.
!
  cwide = 0.9*srange/40.0

  chrtmp = chrrel(smin)
  call s_blank_delete ( chrtmp)
  lent = len_trim ( chrtmp )

  angle = 0.0
  x = x1
  y = y1-1.5*cwide
  pwide = srange
  flush = 'left'
  call s_plot(angle,cwide,pwide,chrtmp(1:lent),x,y,flush)

  chrtmp = chrrel(smax)
  call s_blank_delete ( chrtmp)
  lent = len_trim ( chrtmp )

  angle = 0.0
  x = x2
  y = y1-1.5*cwide
  pwide = srange
  flush = 'right'
  call s_plot(angle,cwide,pwide,chrtmp(1:lent),x,y,flush)

  return
end
subroutine cbox ( grace )
!
!***********************************************************************
!
!! CBOX draws a 16 by 16 box, filling each entry with a color
!  from the current color table.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
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
!    Input, real GRACE.
!         The size of the "grace" margin on the plot.
!
  integer ( kind = 4 ) npoly
  parameter (npoly = 5)
!
  real grace
  integer ( kind = 4 ) i
  integer ( kind = 4 ) icolor
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) j
  real x(npoly)
  real x1max
  real x1min
  real x2max
  real x2min
  real y(npoly)
  real y1max
  real y1min
  real y2max
  real y2min
!
!  Set the coordinate system to be 0 < =  X <= 16.0, 
!  and 0.0 < =  Y <= 16.0
!
  x2min = 0.0
  x2max = 16.0
  y2min = 0.0
  y2max = 16.0

  x1min = x2min-grace*(x2max-x2min)
  x1max = x2max+grace*(x2max-x2min)
  y1min = y2min-grace*(y2max-y2min)
  y1max = y2max+grace*(y2max-y2min)

  call setwcd(x1min,y1min,x1max,y1max,ierror)
! 
!  Draw the color boxes.
!
  icolor = 0

  do i = 1,16

    y(1) = 16-i
    y(2) = 16-i
    y(3) = 17-i
    y(4) = 17-i

    do j = 1,16

      call filclr(icolor)
      icolor = icolor+1

      x(1) = j-1
      x(2) = j
      x(3) = j
      x(4) = j-1

      call plygon(4,x,y)

    end do
  end do
!
!  Draw black lines around the boxes.
!
  icolor = 1
  call linclr(icolor)

  do i = 1,16

    y(1) = 16-i
    y(2) = 17-i
    y(3) = 17-i
    y(4) = 16-i
    y(5) = y(1)

    do j = 1,16

      x(1) = j-1
      x(2) = j-1
      x(3) = j
      x(4) = j
      x(5) = x(1)

      call plylin(5,x,y)

    end do
  end do

  return
end
function chrint ( intval )
!
!***********************************************************************
!
!! CHRINT accepts an integer and returns in CHRINT the 6-character
!  representation of the integer, right justified, or '******' if 
!  the integer is too large or negative to fit in six positions.  
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
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
!  INTVAL Input, integer ( kind = 4 ) INTVAL, an integer variable to be 
!         converted.
!
!  CHRINT Output (through function value), character*6 CHRINT, a 6 
!         character representation of the integer, right justified. 
!         Thus, if INTVAL = 1,CHRINT='     1'.  CHRINT must be 
!         declared "character CHRINT*6" in the calling program.
!
  character ( len = 6 ) chrint
  character ( len = 6 ) chrtmp
  integer ( kind = 4 ) intval
!
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
function chrrel ( rval )
!
!***********************************************************************
!
!! CHRREL accepts a real number in RVAL and returns in CHRREL a
!  14-character right-justified representation of that number.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
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
!  RVAL   Input, real RVAL, a real number.
!
!  CHRREL Output (through function value), character ( len = 14 ) CHRREL,
!         a right-justified character variable containing the
!         representation of RVAL, using a G14.6 format.
!
  character ( len = 14 ) chrrel
  character ( len = 14 ) chrtmp
  real rval
!
!
!  We can't seem to write directly into CHRREL because of compiler
!  quibbles.
!
  if ( real(int(rval)) == rval.and.abs(rval) < 1.0E+13 ) then

    write(chrtmp,'(i14)')int(rval)

  else

    write(chrtmp,'(g14.6)')rval

  end if

  chrrel = chrtmp

  return
end
subroutine conc3 ( eflag,jcmax,jcmin,maxnpe,ncon,nelem,node,np,s, &
  smax,smin,xc,yc)
!
!***********************************************************************
!
!! CONC3 uses color to indicate all the points which have a function 
!  value between given limits.
!
!  CONC3 is used for quantities associated with a 3 node finite element.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!  EFLAG  Input, logical EFLAG(NELEM).
!         EFLAG is used to "flag" which elements are to be displayed.
!         If EFLAG(I) is TRUE, then element I should be displayed.
! 
!  ETAREF Input, real ETAREF(MAXNPE).
!         The ETA coordinates of the nodes of the reference 
!         triangle.
!
!  ISOTRI Output, integer ( kind = 4 ) ISOTRI(NELEM).
! 
!         0, if element I is not isoparametric.
!         1 or 2, if element I is isoparametric.
!
!  JCMAX,
!  JCMIN  Input, integer ( kind = 4 ) JCMAX, JCMIN, the maximum and minimum color
!         indices to use for contours.
!
!  MAXNPE Input, integer ( kind = 4 ) MAXNPE.
!         MAXNPE is the maximum number of nodes per element.
!
!  NCON   Input, integer ( kind = 4 ) NCON, the number of color contours to use.
!
!    Input, integer ( kind = 4 ) NELEM.
!         The number of elements.
!
!  NODE   Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), or NODE(MAXNPE,NELEM).
!         NODE contains, for each element, the global node numbers
!         of its NPE nodes.  
!
!         For linear elements (NPE = 3), the order of the nodes probably
!         doesn't matter, but we will draw them this way:
!
!                       2
!                      /|
!                    /  |
!                  /    |
!                /      |
!               1-------3
!
!         For quadratic elements (NPE = 6), the nodes must be given in a 
!         particular order, which is as follows:
!
!                       2
!                      /|
!                    /  |
!                  4    5
!                /      |
!               1---6---3
! 
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!  NPE    Input, integer ( kind = 4 ) NPE.
!         NPE is the number of nodes per element, which should be
!         3 for linear elements and 6 for quadratics.
!
!  S      Input, real S(NP).
!         S is a scalar quantity associated with the nodes.
!         This routine only looks at the values associated with
!         corner element nodes.
!
!  SMAX,
!  SMIN   Input, real SMAX, SMIN, the maximum and minimum values of S.
!
!  XC     Input, real XC(NP).
!         The X coordinates of the nodes.
!
!  XSIREF Input, real XSIREF(MAXNPE).
!         The XSI coordinates of the nodes of the reference 
!         triangle.
!
!  YC     Input, real YC(NP).
!         The Y coordinates of the nodes.
!
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) np
!
  logical eflag(nelem)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) i3
  integer ( kind = 4 ) jcmax
  integer ( kind = 4 ) jcmin
  integer ( kind = 4 ) ncon
  integer ( kind = 4 ) node(maxnpe,nelem)
  real s(np)
  real smax
  real smin
  real xc(np)
  real yc(np)
!
  do i = 1,nelem

    if ( eflag(i) ) then

      i1 = node(1,i)
      i2 = node(2,i)
      i3 = node(3,i)

      call tricno(i1,i2,i3,jcmax,jcmin,ncon,np,s,smax,smin,xc,yc)

    end if
  end do

  return
end
subroutine conc4 ( eflag,jcmax,jcmin,maxnpe,ncon,nelem,node,np,s, &
  smax,smin,xc,yc)
!
!***********************************************************************
!
!! CONC4 uses color to indicate all the points which have a function 
!  value between given limits.
!
!  CONC4 is used for quantities associated with a nonisoparametric
!  rectangular 4 node finite element.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!  EFLAG  Input, logical EFLAG(NELEM).
!         EFLAG is used to "flag" which elements are to be displayed.
!         If EFLAG(I) is TRUE, then element I should be displayed.
! 
!  ETAREF Input, real ETAREF(MAXNPE).
!         The ETA coordinates of the nodes of the reference 
!         triangle.
!
!  ISOTRI Output, integer ( kind = 4 ) ISOTRI(NELEM).
! 
!         0, if element I is not isoparametric.
!         1 or 2, if element I is isoparametric.
!
!  JCMAX,
!  JCMIN  Input, integer ( kind = 4 ) JCMAX, JCMIN, the maximum and minimum color
!         indices to use for contours.
!
!  MAXNPE Input, integer ( kind = 4 ) MAXNPE.
!         MAXNPE is the maximum number of nodes per element.
!
!  NCON   Input, integer ( kind = 4 ) NCON, the number of color contours to use.
!
!    Input, integer ( kind = 4 ) NELEM.
!         The number of elements.
!
!  NODE   Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), or NODE(MAXNPE,NELEM).
!         NODE contains, for each element, the global node numbers
!         of its NPE nodes.  
!
!         For linear elements (NPE = 3), the order of the nodes probably
!         doesn't matter, but we will draw them this way:
!
!                       2
!                      /|
!                    /  |
!                  /    |
!                /      |
!               1-------3
!
!         For quadratic elements (NPE = 6), the nodes must be given in a 
!         particular order, which is as follows:
!
!                       2
!                      /|
!                    /  |
!                  4    5
!                /      |
!               1---6---3
! 
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!  NPE    Input, integer ( kind = 4 ) NPE.
!         NPE is the number of nodes per element, which should be
!         3 for linear elements and 6 for quadratics.
!
!  S      Input, real S(NP).
!         S is a scalar quantity associated with the nodes.
!         This routine only looks at the values associated with
!         corner element nodes.
!
!  SMAX,
!  SMIN   Input, real SMAX, SMIN, the maximum and minimum values of S.
!
!  XC     Input, real XC(NP).
!         The X coordinates of the nodes.
!
!  XSIREF Input, real XSIREF(MAXNPE).
!         The XSI coordinates of the nodes of the reference 
!         triangle.
!
!  YC     Input, real YC(NP).
!         The Y coordinates of the nodes.
!
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) np
!
  logical eflag(nelem)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) i3
  integer ( kind = 4 ) jcmax
  integer ( kind = 4 ) jcmin
  integer ( kind = 4 ) ncon
  integer ( kind = 4 ) node(maxnpe,nelem)
  real s(np)
  real smax
  real smin
  real xc(np)
  real yc(np)
!
  do i = 1,nelem

    if ( eflag(i) ) then

      i1 = node(1,i)
      i2 = node(2,i)
      i3 = node(3,i)

      call tricno(i1,i2,i3,jcmax,jcmin,ncon,np,s,smax,smin,xc,yc)

      i1 = node(3,i)
      i2 = node(4,i)
      i3 = node(1,i)

      call tricno(i1,i2,i3,jcmax,jcmin,ncon,np,s,smax,smin,xc,yc)

    end if
  end do

  return
end
subroutine conc6 ( eflag,etaref,isotri,jcmax,jcmin,maxnpe,ncon, &
  nelem,node,np,npe,s,smax,smin,xc,xsiref,yc )
!
!***********************************************************************
!
!! CONC6 uses color to indicate all the points which have a
!  function value greater than a given value.
!
!  CONC6 is used for quantities associated with the six
!  corner nodes of a 6 node finite element, in particular,
!  vorticity or velocity magnitude.
!
!  CONC6 breaks this triangular element up into four three
!  node triangles for further treatment.
!
!      
!              2
!             /|
!            / |
!           4--5
!          /| /|
!         / |/ |
!        1--6--3
!        
!  Thus, the four triangles are defined by the nodes as follows:
!
!    Triangle   Nodes
!
!    1          1, 4, 6
!    2          2, 5, 4
!    3          3, 6, 5
!    4          4, 5, 6
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!  EFLAG  Input, logical EFLAG(NELEM).
!         EFLAG is used to "flag" which elements are to be displayed.
!         If EFLAG(I) is TRUE, then element I should be displayed.
! 
!  ETAREF Input, real ETAREF(MAXNPE).
!         The ETA coordinates of the nodes of the reference
!         triangle.
! 
!  ISOTRI Output, integer ( kind = 4 ) ISOTRI(NELEM).
! 
!         0, if element I is not isoparametric.
!         1 or 2, if element I is isoparametric.
! 
!  JCMAX,
!  JCMIN  Input, integer ( kind = 4 ) JCMAX, JCMIN, the maximum and minimum color
!         indices to use for contours.
!
!  MAXNPE Input, integer ( kind = 4 ) MAXNPE.
!         MAXNPE is the maximum number of nodes per element.
!
!  NCON   Input, integer ( kind = 4 ) NCON, the number of contours to draw.
!
!    Input, integer ( kind = 4 ) NELEM.
!         The number of elements.
!   
!  NODE   Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), or NODE(MAXNPE,NELEM).
!         NODE contains, for each element, the global node numbers
!         of its NPE nodes.  
!
!         For linear elements (NPE = 3), the order of the nodes probably
!         doesn't matter, but we will draw them this way:
!
!                       2
!                      /|
!                    /  |
!                  /    |
!                /      |
!               1-------3
!
!         For quadratic elements (NPE = 6), the nodes must be given in a 
!         particular order, which is as follows:
!
!                       2
!                      /|
!                    /  |
!                  4    5
!                /      |
!               1---6---3
! 
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!  NPE    Input, integer ( kind = 4 ) NPE.
!         NPE is the number of nodes per element, which should be
!         3 for linear elements and 6 for quadratics.
!
!  S      Input, real S(NP), the value of S at each node.
!
!  SMAX,
!  SMIN   Input, real SMAX, SMIN, the maximum and minimum values of S.
!
!  XC     Input, real XC(NP), the X coordinates of the nodes.
!
!  XSIREF Input, real XSIREF(MAXNPE).
!         The XSI coordinates of the nodes of the reference
!         triangle.
! 
!  YC     Input, real YC(NP), the Y coordinates of the nodes.
!
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) np
!
  logical eflag(nelem)
  real eta1
  real eta2
  real eta3
  real etaref(maxnpe)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) i3
  integer ( kind = 4 ) ielem
  integer ( kind = 4 ) isotri(nelem)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jcmax
  integer ( kind = 4 ) jcmin
  integer ( kind = 4 ) ncon
  integer ( kind = 4 ) node(maxnpe,nelem)
  integer ( kind = 4 ) npe
  integer ( kind = 4 ) nsub(4,3)
  real s(np)
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
  nsub(1,2) = 4
  nsub(1,3) = 6

  nsub(2,1) = 2
  nsub(2,2) = 5
  nsub(2,3) = 4

  nsub(3,1) = 3
  nsub(3,2) = 6
  nsub(3,3) = 5

  nsub(4,1) = 4
  nsub(4,2) = 5
  nsub(4,3) = 6
!
!  Draw the contour line by searching over each element.
!
  do i = 1,nelem

    ielem = i

    if ( eflag(i) ) then

      if ( npe == 3.or.isotri(i) == 0 ) then

        do j = 1,4
          i1 = node(nsub(j,1),i)
          i2 = node(nsub(j,2),i)
          i3 = node(nsub(j,3),i)
          call tricno(i1,i2,i3,jcmax,jcmin,ncon,np,s,smax,smin,xc,yc)
        end do

      else

        do j = 1,4

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

          call tricis(eta1,eta2,eta3,ielem,jcmax,jcmin,maxnpe, &
            ncon,nelem,node,np,npe,s1,s2,s3,smax,smin,xc,xsi1, &
            xsi2,xsi3,yc)

        end do
      end if

    end if

  end do

  return
end
subroutine conc63 ( eflag,etaref,isotri,jcmax,jcmin,maxnpe,ncon, &
  nelem,node,np,npe,s,smax,smin,xc,xsiref,yc)
!
!***********************************************************************
!
!! CONC63 uses color to indicate all the points which have a function 
!  value between given limits.
!
!  CONC63 is used for quantities associated with the three corner nodes 
!  of a 6 node finite element, in particular, pressure.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!  EFLAG  Input, logical EFLAG(NELEM).
!         EFLAG is used to "flag" which elements are to be displayed.
!         If EFLAG(I) is TRUE, then element I should be displayed.
! 
!  ETAREF Input, real ETAREF(MAXNPE).
!         The ETA coordinates of the nodes of the reference 
!         triangle.
!
!  ISOTRI Output, integer ( kind = 4 ) ISOTRI(NELEM).
! 
!         0, if element I is not isoparametric.
!         1 or 2, if element I is isoparametric.
!
!  JCMAX,
!  JCMIN  Input, integer ( kind = 4 ) JCMAX, JCMIN, the maximum and minimum color
!         indices to use for contours.
!
!  MAXNPE Input, integer ( kind = 4 ) MAXNPE.
!         MAXNPE is the maximum number of nodes per element.
!
!  NCON   Input, integer ( kind = 4 ) NCON, the number of color contours to use.
!
!    Input, integer ( kind = 4 ) NELEM.
!         The number of elements.
!
!  NODE   Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), or NODE(MAXNPE,NELEM).
!         NODE contains, for each element, the global node numbers
!         of its NPE nodes.  
!
!         For linear elements (NPE = 3), the order of the nodes probably
!         doesn't matter, but we will draw them this way:
!
!                       2
!                      /|
!                    /  |
!                  /    |
!                /      |
!               1-------3
!
!         For quadratic elements (NPE = 6), the nodes must be given in a 
!         particular order, which is as follows:
!
!                       2
!                      /|
!                    /  |
!                  4    5
!                /      |
!               1---6---3
! 
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!  NPE    Input, integer ( kind = 4 ) NPE.
!         NPE is the number of nodes per element, which should be
!         3 for linear elements and 6 for quadratics.
!
!  S      Input, real S(NP).
!         S is a scalar quantity associated with the nodes.
!         This routine only looks at the values associated with
!         corner element nodes.
!
!  SMAX,
!  SMIN   Input, real SMAX, SMIN, the maximum and minimum values of S.
!
!  XC     Input, real XC(NP).
!         The X coordinates of the nodes.
!
!  XSIREF Input, real XSIREF(MAXNPE).
!         The XSI coordinates of the nodes of the reference 
!         triangle.
!
!  YC     Input, real YC(NP).
!         The Y coordinates of the nodes.
!
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) np
!
  logical eflag(nelem)
  real eta1
  real eta2
  real eta3
  real etaref(maxnpe)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) i3
  integer ( kind = 4 ) ielem
  integer ( kind = 4 ) isotri(nelem)
  integer ( kind = 4 ) jcmax
  integer ( kind = 4 ) jcmin
  integer ( kind = 4 ) ncon
  integer ( kind = 4 ) node(maxnpe,nelem)
  integer ( kind = 4 ) npe
  real s(np)
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
!  Search over each element, and if it is visible, then
!  call the appropriate nonisoperimetric or isoperimetric routine.
!
  do i = 1,nelem

    if ( eflag(i) ) then

      ielem = i
      i1 = node(1,ielem)
      i2 = node(2,ielem)
      i3 = node(3,ielem)

      if ( isotri(i) == 0 ) then

        call tricno(i1,i2,i3,jcmax,jcmin,ncon,np,s,smax,smin,xc,yc)

      else

        eta1 = etaref(1)
        eta2 = etaref(2)
        eta3 = etaref(3)
        s1 = s(i1)
        s2 = s(i2)
        s3 = s(i3)
        xsi1 = xsiref(1)
        xsi2 = xsiref(2)
        xsi3 = xsiref(3)

        call tricis(eta1,eta2,eta3,ielem,jcmax,jcmin,maxnpe,ncon, &
          nelem,node,np,npe,s1,s2,s3,smax,smin,xc,xsi1,xsi2, &
          xsi3,yc)

      end if

    end if
  end do

  return
end
subroutine conl3 ( eflag,maxnpe,nelem,node,np,s,scon,xc,yc)
!
!***********************************************************************
!
!! CONL3 draws a contour line of a scalar in a 3 node element.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) np
!
  logical eflag(nelem)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n2
  integer ( kind = 4 ) n3
  integer ( kind = 4 ) node(maxnpe,nelem)
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
!  Draw the contour line by searching over each element.
!
  do i = 1,nelem

    if ( eflag(i) ) then

      n1 = node(1,i)
      x1 = xc(n1)
      y1 = yc(n1)
      s1 = s(n1)

      n2 = node(2,i)
      x2 = xc(n2)
      y2 = yc(n2)
      s2 = s(n2)

      n3 = node(3,i)
      x3 = xc(n3)
      y3 = yc(n3)
      s3 = s(n3)

      smin = min(s1,s2,s3)
      smax = max(s1,s2,s3)

      if ( smin <= scon.and.scon <= smax ) then
        call crosst(s1,s2,s3,scon,x1,x2,x3,y1,y2,y3)
      end if

    end if

  end do

  return
end
subroutine conl4 ( eflag,maxnpe,nelem,node,np,s,scon,xc,yc)
!
!***********************************************************************
!
!! CONL4 draws a contour line of a scalar in a 4 node nonisoparametric element.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!  EFLAG  Input, logical EFLAG(NELEM).
!         EFLAG is used to "flag" which elements are to be displayed.
!         If EFLAG(I) is TRUE, then element I should be displayed.
! 
!  MAXNPE Input, integer ( kind = 4 ) MAXNPE.
!         MAXNPE is the maximum number of nodes per element.
!
!    Input, integer ( kind = 4 ) NELEM.
!         The number of elements.
!
!  NODE   Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), or NODE(MAXNPE,NELEM).
!         NODE contains, for each element, the global node numbers
!         of its NPE nodes.  
!
!         For linear elements (NPE = 3), the order of the nodes probably
!         doesn't matter, but we will draw them this way:
!
!                       2
!                      /|
!                    /  |
!                  /    |
!                /      |
!               1-------3
!
!         For quadratic elements (NPE = 6), the nodes must be given in a 
!         particular order, which is as follows:
!
!                       2
!                      /|
!                    /  |
!                  4    5
!                /      |
!               1---6---3
! 
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!  S      Input, real S(NP).
!         S is a scalar quantity associated with the nodes.
!
!  SCON   Input, real SCON.
!         SCON is the value associated with the contour line.
!
!  XC     Input, real XC(NP).
!         The X coordinates of the nodes.
!
!  YC     Input, real YC(NP).
!         The Y coordinates of the nodes.
!
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) np
!
  logical eflag(nelem)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n2
  integer ( kind = 4 ) n3
  integer ( kind = 4 ) n4
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
  do i = 1,nelem
!
!  Is the element visible?
!
    if ( eflag(i) ) then

      n1 = node(1,i)
      x1 = xc(n1)
      y1 = yc(n1)
      s1 = s(n1)

      n2 = node(2,i)
      x2 = xc(n2)
      y2 = yc(n2)
      s2 = s(n2)

      n3 = node(3,i)
      x3 = xc(n3)
      y3 = yc(n3)
      s3 = s(n3)

      n4 = node(4,i)
      x4 = xc(n4)
      y4 = yc(n4)
      s4 = s(n4)

      smin = min(s1,s2,s3,s4)
      smax = max(s1,s2,s3,s4)

      if ( smin <= scon.and.scon <= smax ) then
        call crossr(s1,s2,s3,s4,scon,x1,x2,x3,x4,y1,y2,y3,y4)
      end if

    end if

  end do

  return
end
subroutine conl6 ( eflag,etaref,isotri,maxnpe,nelem,node, &
  np,npe,s,scon,xc,xsiref,yc)
!
!***********************************************************************
!
!! CONL6 draws a contour line of a scalar in a six node triangular element.
!
!
!  CONL6 SHOULD BE REWRITTEN SO THAT THE NUMBER OF SUBTRIANGLES
!  MAY BE EASILY VARIED...I JUST NEED TO DEFINE THE X, Y, S
!  values of each subtriangle.
!
!
!  The original six node quadratic element is broken up into four
!  three node elements, which are treated as though they were
!  linear:
!
!      ^
!      |          2
!      |         /|
!      |        / |
!  ETA |       4--5
!      |      /| /|
!      |     / |/ |
!      |    1--6--3
!      |  
!      +------------>
!           XSI
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!  EFLAG  Input, logical EFLAG(NELEM).
!         EFLAG is used to "flag" which elements are to be displayed.
!         If EFLAG(I) is TRUE, then element I should be displayed.
! 
!  ETAREF Input, real ETAREF(MAXNPE).
!         The ETA coordinates of the nodes of the reference 
!         triangle.
!
!  ISOTRI Output, integer ( kind = 4 ) ISOTRI(NELEM).
! 
!         0, if element I is not isoparametric.
!         1 or 2, if element I is isoparametric.
!
!  MAXNPE Input, integer ( kind = 4 ) MAXNPE.
!         MAXNPE is the maximum number of nodes per element.
!
!    Input, integer ( kind = 4 ) NELEM.
!         The number of elements.
!
!  NFLAG  Input, logical NFLAG(NP).
! 
!         NFLAG is used to "flag" which nodes are active,
!         that is, to be displayed, and which not, in the graph.
!
!  NODE   Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), or NODE(MAXNPE,NELEM).
!         NODE contains, for each element, the global node numbers
!         of its NPE nodes.  
!
!         For linear elements (NPE = 3), the order of the nodes probably
!         doesn't matter, but we will draw them this way:
!
!                       2
!                      /|
!                    /  |
!                  /    |
!                /      |
!               1-------3
!
!         For quadratic elements (NPE = 6), the nodes must be given in a 
!         particular order, which is as follows:
!
!                       2
!                      /|
!                    /  |
!                  4    5
!                /      |
!               1---6---3
! 
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!  NPE    Input, integer ( kind = 4 ) NPE.
!         NPE is the number of nodes per element, which should be
!         3 for linear elements and 6 for quadratics.
!
!  S      Input, real S(NP).
!         S is a scalar quantity associated with the nodes.
!
!  SCON   Input, real SCON.
!         SCON is the value associated with the contour line.
!
!  XC     Input, real XC(NP).
!         The X coordinates of the nodes.
!
!  XSIREF Input, real XSIREF(MAXNPE).
!         The XSI coordinates of the nodes of the reference 
!         triangle.
!
!  YC     Input, real YC(NP).
!         The Y coordinates of the nodes.
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
  integer ( kind = 4 ) isotri(nelem)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) lines(3,4,2)
  integer ( kind = 4 ) m1
  integer ( kind = 4 ) m2
  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n2
  integer ( kind = 4 ) n3
  integer ( kind = 4 ) n4
  integer ( kind = 4 ) n5
  integer ( kind = 4 ) n6
  integer ( kind = 4 ) node(maxnpe,nelem)
  integer ( kind = 4 ) npe
  real s(np)
  real s1
  real s2
  real s3
  real s4
  real s5
  real s6
  real scon
  real smax
  real smin
  real x1
  real x2
  real x3
  real x4
  real x5
  real x6
  real xc(np)
  real xsiref(maxnpe)
  real xx(6)
  real y1
  real y2
  real y3
  real y4
  real y5
  real y6
  real yc(np)
  real yy(6)
!
!  List the local node numbers, which define the three lines,
!  which make up each of the four triangles which subdivide
!  the original 6 node triangular element 3 node triangles.
!
  lines(1,1,1) = 1
  lines(1,1,2) = 4
  lines(2,1,1) = 4
  lines(2,1,2) = 6
  lines(3,1,1) = 6
  lines(3,1,2) = 1

  lines(1,2,1) = 2
  lines(1,2,2) = 4
  lines(2,2,1) = 4
  lines(2,2,2) = 5
  lines(3,2,1) = 5
  lines(3,2,2) = 2

  lines(1,3,1) = 3
  lines(1,3,2) = 5
  lines(2,3,1) = 5
  lines(2,3,2) = 6
  lines(3,3,1) = 6
  lines(3,3,2) = 3

  lines(1,4,1) = 4
  lines(1,4,2) = 5
  lines(2,4,1) = 5
  lines(2,4,2) = 6
  lines(3,4,1) = 6
  lines(3,4,2) = 4
!
!  Draw the contour line by searching over each element.
!
  do i = 1,nelem
!
!  Is the entire element visible?
!
    if ( eflag(i) ) then
!
!  Is the element non-isoparametric?
!
      if ( isotri(i) == 0 ) then

        n1 = node(1,i)
        x1 = xc(n1)
        y1 = yc(n1)
        s1 = s(n1)

        n2 = node(2,i)
        x2 = xc(n2)
        y2 = yc(n2)
        s2 = s(n2)

        n3 = node(3,i)
        x3 = xc(n3)
        y3 = yc(n3)
        s3 = s(n3)

        n4 = node(4,i)
        x4 = xc(n4)
        y4 = yc(n4)
        s4 = s(n4)

        n5 = node(5,i)
        x5 = xc(n5)
        y5 = yc(n5)
        s5 = s(n5)

        n6 = node(6,i)
        x6 = xc(n6)
        y6 = yc(n6)
        s6 = s(n6)

        smin = min(s1,s2,s3,s4,s5,s6)
        smax = max(s1,s2,s3,s4,s5,s6)

      if ( smin <= scon.and.scon <= smax ) then
        call crosst(s1,s4,s6,scon,x1,x4,x6,y1,y4,y6)
        call crosst(s4,s2,s5,scon,x4,x2,x5,y4,y2,y5)
        call crosst(s6,s5,s3,scon,x6,x5,x3,y6,y5,y3)
        call crosst(s5,s6,s4,scon,x5,x6,x4,y5,y6,y4)
      end if
!
!  Code for isoparametric:
!
      else
!
!  Search each of the four three-node subtriangles.
!
        do k = 1,4

          icross = 0
!
!  Look on each side of the subtriangle.
!
          do j = 1,3

            m1 = lines(j,k,1)
            n1 = node(m1,i)

            m2 = lines(j,k,2)
            n2 = node(m2,i)

            s1 = s(n1)
            s2 = s(n2)

            if ( inside ( s1, scon, s2 ) ) then

              x1 = xsiref(m1)
              y1 = etaref(m1)
              x2 = xsiref(m2)
              y2 = etaref(m2)
!
!  Consider 4 cases:
!
!  A) S1 = S2=SCON, whole side is an intersection.
!  B) S1 = SCON, then only a "glancing" intersection.  Ignore.
!  C) S2 = SCON, ignore.
!  D) SCON strictly between S1 and S2.  Find single point.
!
              if ( s1 == s2 ) then

                icross = icross+1
                xx(icross) = x1
                yy(icross) = y1
                icross = icross+1
                xx(icross) = x2
                yy(icross) = y2

              else if ( s1 == scon ) then

              else if ( s2 == scon ) then

              else
                icross = icross+1
                xx(icross) = x1+(scon-s1)*(x2-x1)/(s2-s1)
                yy(icross) = y1+(scon-s1)*(y2-y1)/(s2-s1)
              end if

            end if

          end do
!
!  The only possibilities are:
!
!  2 crossings, on different sides.
!  2 crossings, on the same side, meaning the whole side has
!    the value.
!  6 crossings, meaning the entire element has the value.
!
!  For now, we will ignore the 6 crossing case.
!
          if ( icross == 1 ) then
            write(*,*)'That''s strange, ICROSS = 1'
          else if ( icross == 2 ) then

            ielem = i

            if ( npe == 3 ) then
              call plylin(2,xx,yy)
            else
              call isoln6(xx(1),yy(1),xx(2),yy(2),ielem,maxnpe, &
                nelem,node,np,npe,xc,yc)
            end if

          else if ( icross > 2 ) then

            write(*,*)'That''s strange, ICROSS = ',icross

          end if

        end do

      end if

    end if

  end do

  return
end
subroutine conl63 ( eflag,etaref,isotri,maxnpe,nelem,node,np, &
  npe,s,scon,xc,xsiref,yc)
!
!***********************************************************************
!
!! CONL63 draws a single contour line of a scalar quantity associated 
!  only with the three corner nodes of a six node element.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!  EFLAG  Input, logical EFLAG(NELEM).
!         EFLAG is used to "flag" which elements are to be displayed.
!         If EFLAG(I) is TRUE, then element I should be displayed.
! 
!  ETAREF Input, real ETAREF(MAXNPE).
!         The ETA coordinates of the nodes of the reference 
!         triangle.
!
!  ISOTRI Output, integer ( kind = 4 ) ISOTRI(NELEM).
! 
!         0, if element I is not isoparametric.
!         1 or 2, if element I is isoparametric.
!
!  MAXNPE Input, integer ( kind = 4 ) MAXNPE.
!         MAXNPE is the maximum number of nodes per element.
!
!    Input, integer ( kind = 4 ) NELEM.
!         The number of elements.
!
!  NODE   Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), or NODE(MAXNPE,NELEM).
!         NODE contains, for each element, the global node numbers
!         of its NPE nodes.  
!
!         For linear elements (NPE = 3), the order of the nodes probably
!         doesn't matter, but we will draw them this way:
!
!                       2
!                      /|
!                    /  |
!                  /    |
!                /      |
!               1-------3
!
!         For quadratic elements (NPE = 6), the nodes must be given in a 
!         particular order, which is as follows:
!
!                       2
!                      /|
!                    /  |
!                  4    5
!                /      |
!               1---6---3
! 
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!  NPE    Input, integer ( kind = 4 ) NPE.
!         NPE is the number of nodes per element, which should be
!         3 for linear elements and 6 for quadratics.
!
!  S      Input, real S(NP).
!         S is a scalar quantity associated with the nodes.
!         This routine only looks at the values associated with
!         corner element nodes.
!
!         Currently, S will only be pressure.
!
!  SCON   Input, real SCON.
!         SCON is the value associated with the contour line.
!
!  XC     Input, real XC(NP).
!         The X coordinates of the nodes.
!
!  XSIREF Input, real XSIREF(MAXNPE).
!         The XSI coordinates of the nodes of the reference 
!         triangle.
!
!  YC     Input, real YC(NP).
!         The Y coordinates of the nodes.
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
  integer ( kind = 4 ) isotri(nelem)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jp1
  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n2
  integer ( kind = 4 ) n3
  integer ( kind = 4 ) node(maxnpe,nelem)
  integer ( kind = 4 ) npe
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
  real xsiref(maxnpe)
  real xx(3)
  real y1
  real y2
  real y3
  real yc(np)
  real yy(3)
!
!  Draw the contour line by searching over each element.
!
  do i = 1,nelem
!
!  Is the element visible?
!
    if ( eflag(i) ) then
!
!  Code for a nonisoparametric element:
!
      if ( isotri(i) == 0 ) then

        n1 = node(1,i)
        x1 = xc(n1)
        y1 = yc(n1)
        s1 = s(n1)

        n2 = node(2,i)
        x2 = xc(n2)
        y2 = yc(n2)
        s2 = s(n2)

        n3 = node(3,i)
        x3 = xc(n3)
        y3 = yc(n3)
        s3 = s(n3)

        smin = min(s1,s2,s3)
        smax = max(s1,s2,s3)

        if ( smin <= scon.and.scon <= smax ) then
          call crosst(s1,s2,s3,scon,x1,x2,x3,y1,y2,y3)
        end if
 
      else

        icross = 0
!
!  Conside the side of the triangle associated with node J and JP1.
!
        do j = 1,3

          n1 = node(j,i)
          x1 = xsiref(j)
          y1 = etaref(j)
          s1 = s(n1)

          jp1 = j+1
          if ( jp1 > 3)jp1 = 1

          n2 = node(jp1,i)
          x2 = xsiref(jp1)
          y2 = etaref(jp1)
          s2 = s(n2)
!
!  Does the contour value pass between the values at the two ends?
!
          if ( inside ( s1, scon, s2 ) ) then

            if ( s1 == s2 ) then
              ielem = i
              call isoln6(x1,y1,x2,y2,ielem,maxnpe,nelem,node,np,npe,xc,yc)
            else
              icross = icross+1
              xx(icross) = x1+(scon-s1)*(x2-x1)/(s2-s1)
              yy(icross) = y1+(scon-s1)*(y2-y1)/(s2-s1)
            end if

          end if

        end do
!
!  If we found two crossings, draw the line.
!
        if ( icross == 1 ) then
          write(*,*)'That''s strange, ICROSS = ',icross
        else if ( icross == 2 ) then
          ielem = i
          call isoln6(xx(1),yy(1),xx(2),yy(2),ielem,maxnpe,nelem, &
            node,np,npe,xc,yc)
        else if ( icross > 2 ) then
          write(*,*)'That''s strange, ICROSS = ',icross
        end if

      end if

    end if

  end do

  return
end
subroutine cross ( px,py,qx,qy,sl,sm,sh,scon,xl,xm,xh,yl,ym,yh)
!
!***********************************************************************
!
!! CROSS finds the two places where the value S = SCON occurs on a 
!  triangle.  The corners of the triangle are (XL,YL), (XM,YM) and
!  (XH,YH), and the associated S values are SL, SM and SH.  It 
!  must be the case that SL < =  SM <= SH.
!
!  CROSS returns two points:
!
!   (PX,PY), which occurs on one of the two sides that include 
!           (XM,YM), and 
!   (QX,QY), which occurs on the side between (XL,YL) and (XH,YH).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!  PX,
!  PY     Output, real PX, PY, the X and Y coordinates of a point
!         at which S = SCON, lying on a side of the triangle which 
!       ends at (XM,YM).
!
!  QX,
!  QY     Output, real QX, QY, the X and Y coordinates of a point
!         at which S = SCON, lying on the side of the triangle which
!         lies between (XL,YL) and (XH,YH).
!
!  SL,
!  SM,
!  SH     Input, real SL, SM, SH, the low, medium, and high values
!         of S, associated with the three corners.
!
!  SCON   Input, real SCON, the value of S for which a contour line
!         is sought.
!
!  XL,
!  XM,
!  XH     Input, real XL, XM, XH, the X coordinates of the nodes
!         at which the low, medium and high values of S occur.
!
!  YL,
!  YM,
!  YH     Input, real YL, YM, YH, the Y coordinates of the nodes
!         at which the low, medium and high values of S occur.
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
      px = xl+(scon-sl)*(xm-xl)/(sm-sl)
      py = yl+(scon-sl)*(ym-yl)/(sm-sl)
    else
      px = xm+(scon-sm)*(xh-xm)/(sh-sm)
      py = ym+(scon-sm)*(yh-ym)/(sh-sm)
    end if

    qx = xl+(scon-sl)*(xh-xl)/(sh-sl)
    qy = yl+(scon-sl)*(yh-yl)/(sh-sl)

  end if

  return
end
subroutine crossl ( icross,s1,s2,scon,x1,x2,xc,y1,y2,yc)
!
!***********************************************************************
!
!! CROSSL is given the coordinates of two points in the plane, and
!  the values of a quantity S there.  It is assumed that S varies
!  linearly along the line through the two points.
!
!  CROSSL is also given a desired value SC of S, and is supposed
!  to seek a point, lying between the two given points, at which
!  this value of S is achieved.
!
!
!  ICROSS Output, integer ( kind = 4 ) ICROSS.
!
!         -3, X1 = X2 and Y1=Y2.
!         -2, S1 and S2 and SC are equal.
!         -1, S1 and S2 are equal, and no crossing was found.
!
!         0, S1 and S2 are distinct, and no crossing was found.
!
!         1, S1 and S2 are distinct, and a crossing was found.
!         2, S1 and S2 are distinct, and S1 = SC.
!         3, S2 and S2 are distinct, and S2 = SC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!  S1,
!  S2     Input, real S1, S2, the values of S at (X1,Y1) and (X2,Y2).
!
!  SC     Input, real SC, the S value at which a crossing is sought.
!
!  X1,
!  X2     Input, real X1, X2, the X coordinates of the two endpoints.
!
!  XC     Output, real XC.
!         If ICROSS = 1, 2 or 3, then XC is the X coordinate of the crossing.
!         Otherwise XC is 0.
!
!  Y1,
!  Y2     Input, real Y1, Y2, the Y coordinates of the two endpoints.
!
!  YC     Output, real YC.
!         If ICROSS = 1, 2, or 3, then YC is the Y coordinate of the crossing.
!         Otherwise YC is 0.
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
!
  xc = 0.0
  yc = 0.0
!
!  Are the two points distinct?
!
  if ( x1 == x2.and.y1 == y2 ) then
    icross = -3
    write(*,*)' '
    write(*,*)'CROSSL - Fatal error!'
    write(*,*)'  X1 = X2=',x1
    write(*,*)'  Y1 = Y2=',y1
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
  if ( s2 == scon ) then
    icross = 3
    xc = x2
    yc = y2
  else if ( s1 == scon ) then
    icross = 2
    xc = x1
    yc = y1
  else if ( inside ( s1, scon, s2 ) ) then
    icross = 1
    xc = x1+(scon-s1)*(x2-x1)/(s2-s1)
    yc = y1+(scon-s1)*(y2-y1)/(s2-s1)
  else
    icross = 0
  end if

  return
end
subroutine crossr ( s1,s2,s3,s4,scon,x1,x2,x3,x4,y1,y2,y3,y4)
!
!***********************************************************************
!
!! CROSSR is given the coordinates of the corners of a rectangle, 
!  and the values of a scalar quantity S at those corners.
!
!  It is also given SCON, a special value of S.
!
!  CROSSR divides the rectangle into two triangles, and then
!  calls CROSST to draw contour lines, if any, to represent those 
!  locations within the rectangle, which would have the value SCON.
!
!  Here is how the rectangle is divided into two triangles before 
!  calling CROSST:
!
!    2---3
!    |  /|
!    | / |
!    |/  |
!    1---4
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!  S1,
!  S2,
!  S3,
!  S4     Input, real S1, S2, S3, S4, the values of S at the corneers.
!
!  SCON   Input, real SCON, the contour value of S.
!
!  X1,
!  X2,
!  X3,
!  X4     Input, real X1, X2, X3, X4, the values of X at the corners.
!
!  Y1,
!  Y2,
!  Y3,
!  Y4     Input, real Y1, Y2, Y3, Y4, the values of Y at the corners.
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
  call crosst(s1,s2,s3,scon,x1,x2,x3,y1,y2,y3)

  call crosst(s3,s4,s1,scon,x3,x4,x1,y3,y4,y1)

  return
end
subroutine crosst ( s1,s2,s3,scon,x1,x2,x3,y1,y2,y3)
!
!***********************************************************************
!
!! CROSST is given the coordinates of the corners of a triangle, 
!  and the values of a scalar quantity S at those corners.
!
!  It is also given SCON, a special value of S.
!
!  CROSST uses linear interpolation to extend the definition of S
!  to the entire triangle, and then tries to draw a contour line,
!  if any, to represent those locations within the triangle, which
!  would have the value SCON.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!  S1,
!  S2,
!  S3     Input, real S1, S2, S3, the values of S at the corneers.
!
!  SCON   Input, real SCON, the contour value of S.
!
!  X1,
!  X2,
!  X3     Input, real X1, X2, X3, the values of X at the corners.
!
!  Y1,
!  Y2,
!  Y3     Input, real Y1, Y2, Y3, the values of Y at the corners.
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

  call crossl(icross,s1,s2,scon,x1,x2,xc,y1,y2,yc)

  if ( icross == -2 ) then
    call movcgm(x1,y1)
    call drwcgm(x2,y2)
  else if ( icross == 1 ) then
    npts = npts+1
    xpts(npts) = xc
    ypts(npts) = yc
  end if

  call crossl(icross,s2,s3,scon,x2,x3,xc,y2,y3,yc)

  if ( icross == -2 ) then
    call movcgm(x1,y1)
    call drwcgm(x2,y2)
  else if ( icross == 1 ) then
    npts = npts+1
    xpts(npts) = xc
    ypts(npts) = yc
  end if

  call crossl(icross,s3,s1,scon,x3,x1,xc,y3,y1,yc)

  if ( icross == -2 ) then
    call movcgm(x1,y1)
    call drwcgm(x2,y2)
  else if ( icross == 1 ) then
    npts = npts+1
    xpts(npts) = xc
    ypts(npts) = yc
  end if
!
!  If we found two crossings, draw the line.
!
  if ( npts == 1 ) then
    write(*,*)'That''s strange, just one crossing!'
  else if ( npts == 2 ) then
    call plylin(npts,xpts,ypts)
  else if ( npts > 2 ) then
    write(*,*)'That''s strange, NPTS = ',npts
  end if

  return
end
subroutine defalt ( delx,dely,eflag,eflagu,etaref,fildat,filinp, &
  grace,icmax,icmin,icolor,itable,jcmax,jcmin,lbar,line, &
  lppro,lptpro,lupro,lutpro,lvpro,lvtpro,maxelm,maxnp,maxnpe, &
  maxobj,ncon,nflag,nflag0,npe, &
  nxskip,nyskip,object,ovrlay,scalee,scalen, &
  scalev,show,x1max,x1min,x2max,x2min, &
  x4max,x4min,xsiref,y1max,y1min,y2max, &
  y2min,y4max,y4min)
!
!***********************************************************************
!
!! DEFALT sets certain data to default values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 January 2001
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
  integer ( kind = 4 ) maxelm
  integer ( kind = 4 ) maxnp
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) maxobj
!
  real delx
  real dely
  logical eflag(maxelm)
  logical eflagu(maxelm)
  real etaref(maxnpe)
  character ( len = 80 ) fildat
  character ( len = 80 ) filinp
  real grace
  integer ( kind = 4 ) i
  integer ( kind = 4 ) icmax
  integer ( kind = 4 ) icmin
  integer ( kind = 4 ) icolor(maxobj)
  integer ( kind = 4 ) itable
  integer ( kind = 4 ) jcmax
  integer ( kind = 4 ) jcmin
  logical lbar
  integer ( kind = 4 ) line(maxobj)
  logical lppro
  logical lptpro
  logical lupro
  logical lutpro
  logical lvpro
  logical lvtpro
  integer ( kind = 4 ) ncon
  logical nflag(maxnp)
  logical nflag0(maxnp)
  integer ( kind = 4 ) npe
  integer ( kind = 4 ) nxskip
  integer ( kind = 4 ) nyskip
  character ( len = 30 ) object(maxobj)
  logical ovrlay
  real scalee
  real scalen
  real scalev
  logical show(maxobj)
  real x1max
  real x1min
  real x2max
  real x2min
  real x4max
  real x4min
  real xsiref(maxnpe)
  real y1max
  real y1min
  real y2max
  real y2min
  real y4max
  real y4min
!
  delx = 1.0
  dely = 1.0

  do i = 1,maxelm
    eflag(i) = .true.
    eflagu(i) = .true.
  end do

  fildat = 'display.dat'
  filinp = 'display.inp'
  grace = 0.05
  icmax = 255
  icmin = 2

  do i = 1,maxobj
    icolor(i) = 1
  end do

  icolor(20) = 127
  icolor(21) = 0

  itable = 9

  jcmax = 255
  jcmin = 2
  lbar = .true.

  do i = 1,maxobj
    line(i) = 2
  end do
  line(1) = 2

  lppro = .false.
  lptpro = .false.
  lupro = .false.
  lutpro = .false.
  lvpro = .false.
  lvtpro = .false.

  ncon = 9

  do i = 1,maxnp
    nflag(i) = .true.
    nflag0(i) = .true.
  end do

  npe = maxnp
  nxskip = 1
  nyskip = 1

  object(1) = 'boundary'
  object(2) = 'element'
  object(3) = 'frame'
  object(4) = 'nodes'
  object(5) = 'pressure'
  object(6) = 'stream lines'
  object(7) = 'title'
  object(8) = 'velocity vectors'
  object(9) = 'unit velocity vectors'
  object(10) = 'velocity magnitude contours'
  object(11) = 'vorticity contours'
  object(12) = 'pressure colors'
  object(13) = 'vorticity colors'
  object(14) = 'velocity magnitude colors'
  object(15) = 'X velocity contours'
  object(16) = 'Y velocity contours'
  object(17) = 'X velocity colors'
  object(18) = 'Y velocity colors'
  object(19) = 'profile line'
  object(20) = 'element colors'
  object(21) = 'background'
  object(22) = 'stream colors'
  object(23) = 'X coordinate colors'
  object(24) = 'Y coordinate colors'
  object(25) = 'X coordinate contours'
  object(26) = 'Y coordinate contours'
  object(27) = 'node numbers'
  object(28) = 'element numbers'

  ovrlay = .false.

  scalee = 1.0
  scalen = 1.0
  scalev = 1.0

  do i = 1,maxobj
    show(i) = .false.
  end do

  show(1) = .true.
  show(3) = .false.
  show(7) = .true.

  x2max = 1.0
  x2min = 0.0
  x4max = 0.95
  x4min = 0.05

  y2max = 1.0
  y2min = 0.0
  y4max = 0.95
  y4min = 0.05
!
!  Set things that depend on other things.
!
  call setref(etaref,maxnpe,npe,xsiref)

  x1max = x2max+grace*(x2max-x2min)
  x1min = x2min-grace*(x2max-x2min)

  y1max = y2max+grace*(y2max-y2min)
  y1min = y2min-grace*(y2max-y2min)

  return
end
subroutine delete ( filnam )
!
!***********************************************************************
!
!! DELETE deletes a file.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!  FILNAM Input, character ( len = * )  FILNAM, the file to be deleted.
!
  character ( len = * )  filnam
!
  open(unit = 99,file=filnam,status='old',err=10)
  close(unit = 99,status='delete',err=10)
10    continue

  return
end
subroutine doaxes ( labelx,labely,title,title2,x4max,x4min, &
  xdmax,xdmin,y4max,y4min,ydmax,ydmin)
!
!***********************************************************************
!
!! DOAXES places the title, the axes, and the axis labels.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!  LABELX,
!  LABELY Input, character ( len = 30 ) LABELX, LABELY.  For profile plots, these 
!         contain labels for the X and Y axes.
!
!  TITLE  Input, character ( len = 40 ) TITLE.
!         A title for the plots.
! 
!  TITLE2 Input, character ( len = 40 ) TITLE2.
!         A subtitle used in the profile plots.
!
!  X4MAX,
!  X4MIN  Input, real X4MAX, X4MIN, the maximum and minimum X 
!         coordinates that  are used for the plot, not including axes.
!
!         For profile graphs, X2MIN = 0.20, X2MAX=0.80.
!
!  XDMAX,
!  XDMIN  Input, real XDMAX, XDMIN, the maximum and minimum values
!         to print out on the X axis.
!
!  Y4MAX,
!  Y4MIN  Input, real Y4MAX, Y4MIN, the maximum and minimum Y 
!         coordinates that are used for the plot, not including axes.
!
!         For profile graphs, Y2MIN = 0.20, Y2MAX=0.80.
!
!  YDMAX,
!  YDMIN  Input, real YDMAX, YDMIN, the maximum and minimum values
!         to print out on the Y axis.
!
  real angle
  character ( len = 14 ) chrrel
  character ( len = 14 ) ctemp
  real cwide
  real dshsiz
  real eps
  character ( len = 6 ) flush
  character ( len = * )  labelx
  character ( len = * )  labely
  integer ( kind = 4 ) lent
  integer ( kind = 4 ) nval
  real pwide
  character ( len = * )  title
  character ( len = * )  title2
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
  lent = len_trim ( title )

  if ( lent > 0 ) then
    angle = 0.0
    cwide = 0.02
    pwide = 1.0
    xtemp = 0.5
    ytemp = 0.90
    flush = 'center'
    call s_plot(angle,cwide,pwide,title(1:lent),xtemp,ytemp,flush)
  end if
!
!  Draw the subtitle.
!
  lent = len_trim ( title2 )

  if ( lent > 0 ) then
    angle = 0.0
    cwide = 0.015
    pwide = 1.0
    xtemp = 0.5
    ytemp = 0.86
    flush = 'center'
    call s_plot(angle,cwide,pwide,title2(1:lent),xtemp,ytemp,flush)
  end if
!
!  Draw the X label.
!
  lent = len_trim ( labelx )

  if ( lent > 0 ) then
    angle = 0.0
    cwide = 0.015
    pwide = 1.0
    xtemp = 0.5
    ytemp = 0.15
    flush = 'center'
    call s_plot(angle,cwide,pwide,labelx(1:lent),xtemp,ytemp,flush)
  end if
!
!  Draw the Y label.
!
  lent = len_trim ( labely )

  if ( lent > 0 ) then
    angle = 90
    cwide = 0.015
    pwide = 1.0
    xtemp = 0.15
    ytemp = 0.5
    flush = 'center'
    call s_plot(angle,cwide,pwide,labely(1:lent),xtemp,ytemp,flush)
  end if
!
!  Draw the X axis, and label it.
!
  eps = 0.025

  call movcgm(x4min-eps,y4min)
  call drwcgm(x4max,y4min)
  
  ctemp = chrrel(xdmin)
  call s_blank_delete ( ctemp)
  lent = len_trim ( ctemp )
  angle = 0.0
  cwide = 0.010
  pwide = 1.0
  xtemp = x4min
  ytemp = y4min-2*cwide
  flush = 'left'
  call s_plot(angle,cwide,pwide,ctemp(1:lent),xtemp,ytemp,flush)

  ctemp = chrrel(xdmax)
  call s_blank_delete ( ctemp)
  lent = len_trim ( ctemp )
  angle = 0.0
  cwide = 0.010
  pwide = 1.0
  xtemp = x4max
  ytemp = y4min-2*cwide
  flush = 'right'
  call s_plot(angle,cwide,pwide,ctemp(1:lent),xtemp,ytemp,flush)
!
!  Draw the Y axis, and label it.
!
  call movcgm(x4min,y4min-eps)
  call drwcgm(x4min,y4max)

  ctemp = chrrel(ydmin)
  call s_blank_delete ( ctemp)
  lent = len_trim ( ctemp )
  angle = 90
  cwide = 0.010
  pwide = 1.0
  xtemp = x4min-2*cwide
  ytemp = y4min
  flush = 'left'
  call s_plot(angle,cwide,pwide,ctemp(1:lent),xtemp,ytemp,flush)

  ctemp = chrrel(ydmax)
  call s_blank_delete ( ctemp)
  lent = len_trim ( ctemp )
  angle = 90.0
  cwide = 0.010
  pwide = 1.0
  xtemp = x4min-2*cwide
  ytemp = y4max
  flush = 'right'
  call s_plot(angle,cwide,pwide,ctemp(1:lent),xtemp,ytemp,flush)
!
!  If zero occurs within the Y range, draw a dashed horizontal axis line.
!
  if ( ydmin < 0.0.and.0.0 < ydmax ) then
    ytemp = y4min+(y4max-y4min)*(0.0-ydmin)/(ydmax-ydmin)
    dshsiz = 0.005
    nval = 2
    xval(1) = x4min
    yval(1) = ytemp
    xval(2) = x4max
    yval(2) = ytemp
    call dshlin(nval,xval,yval,dshsiz)
  end if
 
  return
end
subroutine dshlin ( n,x,y,dshsiz)
!
!***********************************************************************
!
!! DSHLIN draws a dashed line connecting a series of points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!  N      Input, integer ( kind = 4 ) N, the number of points to be connected.
!
!  X,
!  Y      Input, real X(N), Y(N), the X and Y coordinates of the
!         points.
!
!  DSHSIZ Input, real DSHSIZ, the length, in the X, Y coordinate
!         system, of the dashed lines.  If it is negative or zero,
!         an error occurs.
!
!         Warning: If the X and Y coordinates use different scale
!         factors, then dashes at different angles will seem to
!         have different lengths.
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
!
!  Make sure that DSIZE is positive.
!
  if ( dshsiz <= 0.0 ) then
    write(*,*)' '
    write(*,*)'DSHLIN - Fatal error!'
    write(*,*)'  The parameter DSHSIZ must be positive.'
    write(*,*)'  but the input value is ',dshsiz
    return
  end if
!
  xnew = x(1)
  ynew = y(1)
  
  do i = 2,n
  
    xold = xnew
    yold = ynew
    xnew = x(i)
    ynew = y(i)
    dist = sqrt( (xnew-xold)**2 + (ynew-yold)**2 )
    
    if ( dist > 0.0 ) then
    
      ndash = int((dist/dshsiz)+1)
      
      if ( mod(ndash,4) /= 0 ) then
        ndash = ndash+(4-mod(ndash,4))
      end if
      
      if ( ndash <= 3)ndash = 4
!
!  Desired pattern is:
!
!  X0 - dash - blank - blank - dash - dash - blank - blank - dash - X1
!
      do j = 1,ndash
      
        if ( mod(j,4) == 0.or.mod(j,4) == 1 ) then
          xxold = ( (ndash+1-j)*xold + (j-1)*xnew) / real(ndash)
          yyold = ( (ndash+1-j)*yold + (j-1)*ynew) / real(ndash)
          xxnew = ( (ndash-j)*xold + j*xnew) / real(ndash)
          yynew = ( (ndash-j)*yold + j*ynew) / real(ndash)
          call movcgm(xxold,yyold)
          call drwcgm(xxnew,yynew)
        end if
        
      end do
      
    end if
    
  end do

  return
end
subroutine element_check ( maxnpe, nelem, node, np, npe, xc, yc )
!
!***********************************************************************
!
!! ELEMENT_CHECK checks elements for correct orientation.  
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) MAXNPE, the maximum number of nodes per element.
!
!    Input, integer ( kind = 4 ) NELEM.
!         The number of elements.
! 
!  NODE   Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), or NODE(MAXNPE,NELEM).
!         NODE contains, for each element, the global node numbers
!         of its NPE nodes.  
!
!         For linear elements (NPE = 3), the order of the nodes probably
!         doesn't matter, but we will draw them this way:
!
!                       2
!                      /|
!                    /  |
!                  /    |
!                /      |
!               1-------3
!
!         For quadratic elements (NPE = 6), the nodes must be given in a 
!         particular order, which is as follows:
!
!                       2
!                      /|
!                    /  |
!                  4    5
!                /      |
!               1---6---3
! 
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!  XC     Input, real XC(NP).
!         The X coordinates of the nodes.
! 
!  YC     Input, real YC(NP).
!         The Y coordinates of the nodes.
! 
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) nelem
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
  integer ( kind = 4 ) node(maxnpe,nelem)
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

  do i = 1,nelem
!
!  Compute the area;
!  The area is not correct for 6 node triangles, nor for 
!  isoparametric elements.
!
    area = 0.0

    do j = 1,nedge

      j1 = node(j,i)
      if ( j < nedge ) then
        j2 = node(j+1,i)
      else
        j2 = node(1,i)
      end if

      area = area+(yc(j1)+yc(j2))*(xc(j2)-xc(j1))

    end do

    if ( area == 0.0 ) then

      nzer = nzer+1

      if ( nzer < 10 ) then
        write(*,*)' '
        write(*,*)'ELMCHK - Warning'
        write(*,*)'  Element ',I,' has zero area.'
        write(*,*)' '
        write(*,*)'Nloc  Nglob   X(N), Y(N)'
        write(*,*)' '
        do j = 1,npe
          write(*,'(2i6,2g14.6)')j,node(j,i),xc(node(j,i)),yc(node(j,i))
        end do
      else if ( nzer == 10 ) then
        write(*,*)' '
        write(*,*)'ELMCHK - Warning!'
        write(*,*)'  This is the TENTH element with zero area.'
        write(*,*)'  No more warnings will be printed.'
      end if
!
!  Reverse the node ordering for elements with negative area.
!
    else if ( area < 0.0 ) then

      neg = neg+1

      do j = 1,npe/2
        itemp = node(j,i)
        node(j,i) = node(npe+1-j,i)
        node(npe+1-j,i) = itemp
      end do

    else

      npos = npos+1

    end if

  end do

  if ( nzer > 0 ) then
    write(*,*)' '
    write(*,*)'ELEMENT_CHECK - Warning!'
    write(*,*)'  Elements with zero orientation  = ',nzer
    write(*,*)'  Do NOT plot this data!'
  else if ( neg > 0 ) then
    write(*,*)' '
    write(*,*)'ELEMENT_CHECK - Note:'
    write(*,*)'  Reoriented ',neg,' elements.'
  end if

  return
end
subroutine element_color ( eflag, etaref, isotri, maxnpe, nelem, node, np, &
  npe, xc, xsiref, yc )
!
!***********************************************************************
!
!! ELEMENT_COLOR colors in the elements.
!
!
!  Discussion:
!
!    For isoparametric elements of higher than linear order, we 
!    generate extra points between the standard nodes, and 
!    approximate the curved elements by polygons.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, logical EFLAG(NELEM), flags elements to display.
!    If EFLAG(I) is TRUE, then element I should be displayed.
! 
!  ETAREF Input, real ETAREF(MAXNPE).
!         The ETA coordinates of the nodes of the reference
!         triangle.
! 
!  ISOTRI Output, integer ( kind = 4 ) ISOTRI(NELEM).
! 
!         0, if element I is not isoparametric.
!         1 or 2, if element I is isoparametric.
! 
!    Input, integer ( kind = 4 ) NELEM.
!         The number of elements.
!   
!  NODE   Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), or NODE(MAXNPE,NELEM).
!         NODE contains, for each element, the global node numbers
!         of its NPE nodes.  
!
!         For linear elements (NPE = 3), the order of the nodes probably
!         doesn't matter, but we will draw them this way:
!
!                       2
!                      /|
!                    /  |
!                  /    |
!                /      |
!               1-------3
!
!         For quadratic elements (NPE = 6), the nodes must be given in a 
!         particular order, which is as follows:
!
!                       2
!                      /|
!                    /  |
!                  4    5
!                /      |
!               1---6---3
!
!  NPE    Input, integer ( kind = 4 ) NPE.
!         NPE is the number of nodes per element, which should be
!         3 for linear elements and 6 for quadratics.
!
! 
!  XC     Input, real XC(NP), the X coordinates of the nodes.
!
!  XSIREF Input, real XSIREF(MAXNPE).
!         The XSI coordinates of the nodes of the reference
!         triangle.
! 
!  YC     Input, real YC(NP), the Y coordinates of the nodes.
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
  integer ( kind = 4 ) isotri(nelem)
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
!  NOD    integer ( kind = 4 ) NOD(6), contains a list of the local node 
!         numbers, in the order that we want to visit them, while 
!         drawing the outline of the element.
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
    nod(2) = 4
    nod(3) = 2
    nod(4) = 5
    nod(5) = 3
    nod(6) = 6
  end if

  npts = 6

  do i = 1,nelem

    ielem = i
!
!  If element is "visible":
!
    if ( eflag(i) ) then
!
!  If element is 3 or 4 node, or non-isoparametric:
!
      if ( npe == 3.or.npe == 4.or.isotri(i) == 0 ) then

        do j = 1,npe
          ilocal = nod(j)
          iglobal = node(ilocal,i)
          xpoly(j) = xc(iglobal)
          ypoly(j) = yc(iglobal)
        end do

        call plygon(npe,xpoly,ypoly)
!
!  If element is isoparametric:
!
      else

        do j = 1,npe
          ilocal = nod(j)
          xpoly(j) = xsiref(ilocal)
          ypoly(j) = etaref(ilocal)
        end do

        if ( npe == 6 ) then

          call isogn6(ypoly,ielem,maxnpe,nelem,node,np,npe,npts,xc,xpoly,yc)

        end if

      end if

    end if

  end do

  return
end
subroutine element_line ( eflag,etaref,isotri,maxnpe,nelem,node,np,npe, &
  xc,xsiref,yc)
!
!***********************************************************************
!
!! ELEMENT_LINE draws the elements of the grid.
!
!
!  Discussion:
!
!    For isoparametric elements of higher than linear order, we 
!    generate extra points between the standard nodes, and 
!    approximate the curved boundaries by a sequence of short
!    straight lines.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, logical EFLAG(NELEM), flags elements to display.
!    If EFLAG(I) is TRUE, then element I should be displayed.
! 
!    Input, real ETAREF(MAXNPE), the ETA coordinates of the nodes of the 
!    reference triangle.
! 
!    Output, integer ( kind = 4 ) ISOTRI(NELEM).
!    0, if element I is not isoparametric.
!    1 or 2, if element I is isoparametric.
! 
!    Input, integer ( kind = 4 ) MAXNPE, the maximum number of nodes per element.
!
!    Input, integer ( kind = 4 ) NELEM, the number of elements.
!   
!    Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), or NODE(MAXNPE,NELEM).
!    NODE contains, for each element, the global node numbers of nodes.  
!    For linear elements (NPE = 3), the order of the nodes probably
!    doesn't matter, but we will draw them this way:
!
!                       2
!                      /|
!                    /  |
!                  /    |
!                /      |
!               1-------3
!
!    For quadratic elements (NPE = 6), the nodes must be given in a 
!    particular order, which is as follows:
!
!                       2
!                      /|
!                    /  |
!                  4    5
!                /      |
!               1---6---3
! 
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!  NPE    Input, integer ( kind = 4 ) NPE.
!         NPE is the number of nodes per element, which should be
!         3 for linear elements and 6 for quadratics.
!
!  XC     Input, real XC(NP), the X coordinates of the nodes.
!
!  XSIREF Input, real XSIREF(MAXNPE).
!         The XSI coordinates of the nodes of the reference
!         triangle.
! 
!  YC     Input, real YC(NP), the Y coordinates of the nodes.
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
  integer ( kind = 4 ) isotri(nelem)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jglobal
  integer ( kind = 4 ) jlocal
  integer ( kind = 4 ) jp1
  integer ( kind = 4 ) nod(6)
  integer ( kind = 4 ) node(maxnpe,nelem)
  integer ( kind = 4 ) npe
  real xc(np)
  real xi
  real xj
  real xsiref(maxnpe)
  real yc(np)
  real yi
  real yj
!
!  NOD    integer ( kind = 4 ) NOD(6), contains a list of the local node 
!         numbers, in the order that we want to visit them, while 
!         drawing the outline of the element.
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
    nod(2) = 4
    nod(3) = 2
    nod(4) = 5
    nod(5) = 3
    nod(6) = 6
  end if
 
  do i = 1, nelem
!
!  If element is "visible":
!
    if ( eflag(i) ) then
!
!  If element is non-isoparametric:
!
      if ( npe == 3 .or. npe == 4 .or. isotri(i) == 0 ) then

        ilocal = nod(1)
        iglobal = node(ilocal,i)
        call movcgm(xc(iglobal),yc(iglobal))

        do j = 1, npe

          if ( j < npe ) then
            jp1 = j+1
          else
            jp1 = 1
          end if

          jlocal = nod(jp1)
          jglobal = node(jlocal,i)
          call drwcgm(xc(jglobal),yc(jglobal))

        end do
!
!  If element is isoparametric:
!
      else

        ilocal = nod(1)
        xi = xsiref(ilocal)
        yi = etaref(ilocal)

        do j = 1,npe

          if ( j < npe ) then
            jp1 = j+1
          else
            jp1 = 1
          end if

          jlocal = nod(jp1)
          xj = xsiref(jlocal)
          yj = etaref(jlocal)

          ielem = i

          call isoln6(xi,yi,xj,yj,ielem,maxnpe,nelem,node,np,npe,xc,yc)

          xi = xj
          yi = yj

        end do

      end if

    end if

  end do

  return
end
subroutine element_visibility ( eflagu, nelem )
!
!***********************************************************************
!
!! ELEMENT_VISIBILITY allows the user to specify which elements are visible.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 January 2001
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
  integer ( kind = 4 ) nelem
!
  logical eflagu(nelem)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) itemp
!
  write ( *, * ) ' '
  write ( *, * ) 'ELEMENT_VISIBILITY - Set visibility of elements.'
  write ( *, * ) ' '
  write ( *, * ) '  First, we hide ALL elements.'
  write ( *, * ) ' '

  eflagu(1:nelem) = .false.

10    continue

  write(*,*)'  Enter a range of visible elements ELO, EHI,'
  write(*,*)'  or 0, 0 to stop.'

  read(*,*,err = 20,end=20)ilo,ihi

  if ( ilo <= 0.or.ilo > nelem)return
  if ( ihi <= 0.or.ihi > nelem)return

  if ( ilo > ihi ) then
    itemp = ilo
    ilo = ihi
    ihi = itemp
  end if

  do i = ilo,ihi
    eflagu(i) = .true.
  end do

  go to 10

20    continue
  write(*,*)' '
  write(*,*)'VIZELM - Serious error!'
  write(*,*)'  Error reading user input!'
  return
end
subroutine exdat ( eqn,isotri,maxelm,maxnp,maxnpe,maxny,maxpar, &
  maxsen,nelem,node,np,npar,nprof,nx,ny,p,para,ptar,u,utar,v, &
  vtar,xc,xprof,yc )
!
!***********************************************************************
!
!! EXDAT sets up data that defines a simple example problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, character ( len = 2 )  EQN(3,NP).
!         EQN records the "type" of each equation that will be generated, and
!         which is associated with an unknown.  Note that most boundary 
!         conditions do not result in an equation.  The current values are:
!
!         'U'  The horizontal momentum equation.
!         'UW' The condition U = 0 applied at a node on a fixed wall.
!         'V'  The vertical momentum equation.
!         'VW' The condition V = 0 applied at a node on a fixed wall.
!         'P'  The continuity equation.
!         'PB' The condition P = 0 applied at (XMAX,YMAX).
!
!  ISOTRI Output, integer ( kind = 4 ) ISOTRI(NELEM).
! 
!         0, if element I is not isoparametric.
!         1 or 2, if element I is isoparametric.
! 
!  MAXELM Input, integer ( kind = 4 ) MAXELM.
!         The maximum number of elements which the program can
!         handle.
! 
!  MAXNP  Input, integer ( kind = 4 ) MAXNP, the maximum number of nodes.
!
!  MAXNPE Input, integer ( kind = 4 ) MAXNPE.
!         MAXNPE is the maximum number of nodes per element.
!
!  MAXPAR Input, integer ( kind = 4 ) MAXPAR.
!         The maximum number of parameters the program can handle.
! 
!  NELEM  Output, integer ( kind = 4 ) NELEM.
!         The number of elements.
! 
!  NODE   Output, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), or NODE(MAXNPE,NELEM).
!         NODE contains, for each element, the global node numbers
!         of its NPE nodes.  
!
!         For linear elements (NPE = 3), the order of the nodes probably
!         doesn't matter, but we will draw them this way:
!
!                       2
!                      /|
!                    /  |
!                  /    |
!                /      |
!               1-------3
!
!         For quadratic elements (NPE = 6), the nodes must be given in a 
!         particular order, which is as follows:
!
!                       2
!                      /|
!                    /  |
!                  4    5
!                /      |
!               1---6---3
! 
!  NP     Output, integer ( kind = 4 ) NP, the number of nodes.
!
!  NPAR   Output, integer ( kind = 4 ) NPAR.
!         The number of parameters.
! 
!  NPROF  Output, integer ( kind = 4 ) NPROF(MY).
!         Records the indices of the nodes that lie along the 
!         profile line.
!
!  NX     Output, integer ( kind = 4 ) NX.
!         Determines the number of nodes and elements in the X
!         direction.  There will be 2*NX-1 nodes, 2*NX-2 elements.
! 
!  NY     Output, integer ( kind = 4 ) NY.
!         Determines the number of nodes and elements in the Y
!         direction.  There will be 2*NY-1 nodes, 2*NY-2 elements.
! 
!  P      Output, real P(MAXNP,0:MAXPAR).
! 
!         P(I,0) is the pressure at node I.
! 
!         P(I,J) is the sensitivity of the pressure with respect
!         to parameter J.
! 
!  PARA   Output, real PARA(MAXPAR).
!         The value of the parameters.
! 
!  PTAR   Output, real PTAR(MAXNP)
!         The pressure field associated with the target solution, at 
!         node I.
!
!  U      Output, real U(MAXNP,MAXPAR).
! 
!         U(I,0) is the horizontal fluid velocity at node I.
! 
!         U(I,J) is the sensitivity of the horizontal velocity with 
!         respect to parameter J.
! 
!  UTAR   Output, real UTAR(MAXNP)
!         The horizontal velocity field associated with the target 
!         solution, at node I.
!
!  V      Output, real V(MAXNP,MAXPAR).
! 
!         V(I,0) is the vertical fluid velocity at node I.
! 
!         V(I,J) is the sensitivity of the vertical velocity with
!         respect to parameter J.
! 
!  VTAR   Output, real VTAR(MAXNP)
!         The vertical velocity field associated with the target 
!         solution, at node I.
!
!  XC     Output, real XC(MAXNP).
!         The X coordinates of the nodes.
! 
!  XPROF  Output, real XPROF.
!         The X coordinate of the profile line.
!
!  YC     Output, real YC(MAXNP).
!         The Y coordinates of the nodes.
! 
  integer ( kind = 4 ) maxelm
  integer ( kind = 4 ) maxnp
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) maxny
  integer ( kind = 4 ) maxpar
  integer ( kind = 4 ) maxsen
!
  character ( len = 2 )  eqn(3,maxnp)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ip
  integer ( kind = 4 ) isotri(maxelm)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) kk
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) node(maxnpe,maxelm)
  integer ( kind = 4 ) np
  integer ( kind = 4 ) npar
  integer ( kind = 4 ) nprof(2*maxny-1)
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny
  real p(maxnp,0:maxsen)
  real para(maxpar)
  real ptar(maxnp)
  real u(maxnp,0:maxsen)
  real utar(maxnp)
  real v(maxnp,0:maxsen)
  real vtar(maxnp)
  real xc(maxnp)
  real xlen
  real xprof
  real yc(maxnp)
  real ylen
!
  nx = 5
  ny = 4

  nelem = 2*(nx-1)*(ny-1)
  np = (2*nx-1)*(2*ny-1)
 
  write(*,*)' '
  write(*,*)'ExDat - Setting up the sample problem.'
  write(*,*)'  NX = ',nx
  write(*,*)'  NY = ',ny
  write(*,*)'  NP = ',np
  write(*,*)'  NELEM = ',nelem
  xlen = 10.0
  ylen = 3.0

  k = 0
  kk = 0
  do i = 1,2*nx-1
    do j = 1,2*ny-1
      k = k+1
      xc(k) = (i-1)*xlen/real(2*nx-2)
      yc(k) = (j-1)*ylen/real(2*ny-2)
 
      u(k,0) = 4.0*yc(k)*(ylen-yc(k))/(ylen*ylen)
      v(k,0) = 64.0*yc(k)*(ylen-yc(k))*(0.5*ylen-yc(k))/(ylen**3)
      p(k,0) = ylen-yc(k)
      utar(k) = u(k,0)
      vtar(k) = v(k,0)
      ptar(k) = p(k,0)
 
      if ( j == 1.or.j == 2*ny-1 ) then
        eqn(1,k) = 'UW'
      else
        eqn(1,k) = 'U'
      end if
 
      if ( j == 1.or.j == 2*ny-1 ) then
        eqn(2,k) = 'VW'
      else
        eqn(2,k) = 'V'
      end if
 
      if ( i /= 2*nx-1.or.j /= 2*ny-1 ) then
        eqn(3,k) = 'P'
      else
        eqn(3,k) = 'PB'
      end if
 
      if ( i == 4 ) then
        kk = kk+1
        nprof(kk) = k
        xprof = xc(k)
      end if
 
    end do
  end do
 
  isotri(1:nelem) = 0
 
  k = 0
  do i = 1,2*nx-3,2
    do j = 1,2*ny-3,2
 
      ip = (i-1)*(2*ny-1)+j
      k = k+1
      node(1,k) = ip
      node(2,k) = ip+2
      node(3,k) = ip+2*(2*ny-1)+2
      node(4,k) = ip+1
      node(5,k) = ip+(2*ny-1)+2
      node(6,k) = ip+(2*ny-1)+1
   
      k = k+1
      node(1,k) = ip
      node(2,k) = ip+2*(2*ny-1)+2
      node(3,k) = ip+2*(2*ny-1)
      node(4,k) = ip+(2*ny-1)+1
      node(5,k) = ip+2*(2*ny-1)+1
      node(6,k) = ip+(2*ny-1)
   
    end do
  end do
 
  npar = 1
  para(1) = 17.0
 
  return
end
subroutine fegrad ( dudxn,dudyn,isotri,maxnpe,nelem,node,np,npe,u,xc,yc)
!
!***********************************************************************
!
!! FEGRAD is given a finite element mesh, and the value of a quantity
!  U at each finite element node.  It seeks an approximation to the
!  partial derivatives dUdX and dUdY at each such node.  However,
!  the finite element representation of U is only continuous, not
!  continously differentiable, at the nodes, which lie on the element
!  boundaries.  Therefore, the value of dUdX and dUdY depends on which
!  element the node is assumed to lie in.
!
!  For each node, FEGRAD evaluates dUdX and dUdY in every element
!  that contains the node, and then averages the results, to give a
!  reasonable estimate for the value of the derivatives.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
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
!  DUDXN  Output, DOUBLE PRECISION DUDXN(NP), an estimate for the
!         partial derivative dUdX at each node.
!
!  DUDXY  Output, DOUBLE PRECISION DUDXY(NP), an estimate for the
!         partial derivative dUdY at each node.
!
!  ISOTRI Input, integer ( kind = 4 ) ISOTRI(MAXELM).
! 
!         0, if element I is not isoparametric.
!         1 or 2, if element I is isoparametric.
! 
!  MAXNPE Input, integer ( kind = 4 ) MAXNPE.
!         MAXNPE is the maximum number of nodes per element.
!
!    Input, integer ( kind = 4 ) NELEM.
!         The number of elements.
! 
!  NODE   Input, integer ( kind = 4 ) NODE(MAXELM,MAXNPE), or NODE(MAXELM,NPE).
!         NODE contains, for each element, the global node numbers
!         of its NPE nodes.  
!
!         For linear elements (NPE = 3), the order of the nodes probably
!         doesn't matter, but we will draw them this way:
!
!                       2
!                      /|
!                    /  |
!                  /    |
!                /      |
!               1-------3
!
!         For quadratic elements (NPE = 6), the nodes must be given in a 
!         particular order, which is as follows:
!
!                       2
!                      /|
!                    /  |
!                  4    5
!                /      |
!               1---6---3
! 
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!  NPE    Input, integer ( kind = 4 ) NPE.
!         NPE is the number of nodes per element, which should be
!         3 for linear elements and 6 for quadratics.
!
!  U      Input, real U(MAXNP).
!         U(I) is the value of the quantity U at node I.
! 
!  XC     Input, real XC(MAXNP).
!         XC contains the X coordinates of the nodes.
! 
!  YC     Input, real YC(MAXNP).
!         YC contains the Y coordinates of the nodes.
! 
  integer ( kind = 4 ) maxnx
  integer ( kind = 4 ) maxny
!
  parameter (maxnx = 81)
  parameter (maxny = 25)
!
  integer ( kind = 4 ) maxnp
  parameter (maxnp = (2*maxnx-1)*(2*maxny-1))
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
  integer ( kind = 4 ) ip
  integer ( kind = 4 ) iq
  integer ( kind = 4 ) isotri(nelem)
  integer ( kind = 4 ) jp
  integer ( kind = 4 ) jq
  integer ( kind = 4 ) node(maxnpe,nelem)
  integer ( kind = 4 ) npe
  integer ( kind = 4 ) numel(maxnp)
  real u(np)
  real w
  real xc(np)
  real xq
  real xsi
  real xsin(6)
  real yc(np)
  real yq
!
  if ( np > maxnp ) then
    write(*,*)' '
    write(*,*)'FEGRAD - Fatal error!'
    write(*,*)'  NP exceeds MAXNP.'
    write(*,*)'  NP = ',np
    write(*,*)'  MAXNP = ',maxnp
    stop
  end if

  if ( npe == 3 ) then

    etan(1) = 0.0
    etan(2) = 1.0
    etan(3) = 0.0

    xsin(1) = 0.0
    xsin(2) = 1.0
    xsin(3) = 1.0

  else if ( npe == 6 ) then

    etan(1) = 0.0
    etan(2) = 1.0
    etan(3) = 0.0
    etan(4) = 0.5
    etan(5) = 0.5
    etan(6) = 0.0

    xsin(1) = 0.0
    xsin(2) = 1.0
    xsin(3) = 1.0
    xsin(4) = 0.5
    xsin(5) = 1.0
    xsin(6) = 0.5

  end if
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
  do i = 1,np
    numel(i) = 0
    dudxn(i) = 0.0
    dudyn(i) = 0.0
  end do
!
!  Loop over all the elements.  
!
  do ielem = 1,nelem
!
!  At each node in the element, estimate dUdX and dUdY.
!
    do iq = 1,npe
   
      ip = node(iq,ielem)

      xq = xc(ip)
      yq = yc(ip)
   
      eta = etan(iq)
      xsi = xsin(iq)
   
      if ( isotri(ielem) /= 0 ) then

        if ( npe == 3 ) then

          call tranl3(det,detadx,detady,dxdeta,dxdxsi,dxsidx,dxsidy, &
            dydeta,dydxsi,eta,ielem,maxnpe,nelem,node,np,npe,xc, &
            xsi,yc)

        else if ( npe == 4 ) then

          call tranl4(det,detadx,detady,dxdeta,dxdxsi,dxsidx,dxsidy, &
            dydeta,dydxsi,eta,ielem,maxnpe,nelem,node,np,npe,xc, &
            xsi,yc)

        else if ( npe == 6 ) then

          call tranq6(det,detadx,detady,dxdeta,dxdxsi,dxsidx,dxsidy, &
            dydeta,dydxsi,eta,ielem,maxnpe,nelem,node,np,npe,xc, &
            xsi,yc)

        end if

      end if
!
!  Compute dUdX and dUdY by summing over the weighted derivatives of the
!  local finite element basis functions.  Here, we use the fact that the 
!  value of U at node JQ is the coefficient of the finite element basis
!  function associated with that node.
!
      dudx = 0.0
      dudy = 0.0

      do jq = 1,npe

        jp = node(jq,ielem)

        if ( isotri(ielem) == 0 ) then

          if ( npe == 3 ) then

            call bfl ( ielem,jq,w,dwdx,dwdy,maxnpe,nelem,node,np,xc,xq,yc,yq)

          else if ( npe == 6 ) then

            call bfq ( ielem,jq,w,dwdx,dwdy,maxnpe,nelem,node,np,xc,xq,yc,yq)

          end if

        else
 
          if ( npe == 3 ) then

            call bfrefl(w,dwdx,dwdy,detadx,detady,dxsidx,dxsidy,eta,jq,xsi)

          else if ( npe == 6 ) then

            call bfrefq(w,dwdx,dwdy,detadx,detady,dxsidx,dxsidy,eta,jq,xsi)

          end if
 
        end if

        dudx = dudx+u(jp)*dwdx
        dudy = dudy+u(jp)*dwdy

      end do
!
!  Add the estimates for dUdX and dUdY to the running total.
!  Count how many estimates we have made at each node, 
!  so that we can average them later.
!
      dudxn(ip) = dudxn(ip)+dudx
      dudyn(ip) = dudyn(ip)+dudy
      numel(ip) = numel(ip)+1

    end do
   
  end do
!
!  Take the average value of the quantities over all the
!  different elements along whose boundaries they are defined.
!
  do i = 1,np
    if ( numel(i) == 0 ) then
      write(*,*)' '
      write(*,*)'FEGRAD - Warning!'
      write(*,*)'  Node ',i,' occurs in no element!'
    else
      dudxn(i) = dudxn(i)/real(numel(i))
      dudyn(i) = dudyn(i)/real(numel(i))
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
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
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
!  Discussion:
!
!    The file is assumed to be a simple text file.
!
!    Blank lines and comment lines, which begin with '#', are not counted.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
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
subroutine fillin ( narray,x1,x2,xarray)
!
!***********************************************************************
!
!! FILLIN fills XARRAY with a set of values that vary linearly 
!  between X1 and X2.
!
!  Normally, XARRAY will include the endpoints X1 and X2.  Thus, to
!  generate the points 0.0, 0.1, 0.2, ..., 0.9, 1.0, you would 
!  pass
!
!    X1 = 0.0, X2=1.0, NARRAY=11.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
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
!  NARRAY Input, integer ( kind = 4 ) NARRAY, the number of points to generate.
!         If NARRAY = 1, then XARRAY(1)=0.5*(X1+X2).
!
!  X1,
!  X2     Input, real X1, X2, the endpoints of the list of values.
!
!  XARRAY Output, real XARRAY(NARRAY), evenly spaced values with
!         XARRAY(1) = X1 and XARRAY(NARRAY)=X2.
!
  integer ( kind = 4 ) narray
!
  integer ( kind = 4 ) i
  real x1
  real x2
  real xarray(narray)
!
  if ( narray <= 0 ) then
 
    write(*,*)' '
    write(*,*)'FillIn - Fatal error!'
    write(*,*)'  NARRAY is not positive!'
    stop
 
  else if ( narray == 1 ) then
 
    xarray(1) = 0.5*(x1+x2)
 
  else
 
    xarray(1) = x1
    do i = 2,narray-1
      xarray(i) = ((narray-i)*x1+(i-1)*x2)/real(narray-1)
    end do
    xarray(narray) = x2
 
  end if

  return
end
subroutine fillin2 ( nfill,np1,x1,x2)
!
!***********************************************************************
!
!! FILLIN2 accepts an array X1 of NP1 points, and computes a new
!  array X2, which includes all the points X1, plus NFILL new
!  points between each pair, with NFILL more points after the last
!  entry.  The inserted points are linearly interpolated.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
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
!  NFILL  Input, integer ( kind = 4 ) NFILL, the number of fillin points to
!         insert between every pair of points.
!
!  NP1    Input, integer ( kind = 4 ) NP1, the number of data points in the
!         input vector X1.
!
!  X1     Input, real X1(NP1), the data, between which fillin 
!         points are to be inserted.
!
!  X2     Output, real X2((NFILL+1)*NP1), the X1 data, with
!         linearly interpolated fillin points.
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
 
    write(*,*)' '
    write(*,*)'FILLIN2 - Fatal error!'
    write(*,*)'  NP1 is not positive!'
    stop
 
  else
 
    np2 = 0

    do i = 1,np1

      xlo = x1(i)

      if ( i < np1 ) then
        xhi = x1(i+1)
      else
        xhi = x1(1)
      end if

      do j = 0,nfill
        np2 = np2+1
        x2(np2) = ((nfill+1-j)*xlo+j*xhi)/real(nfill+1)
      end do

    end do
 
  end if

  return
end
subroutine flushl ( string )
!
!***********************************************************************
!
!! FLUSHL flushes a string left.  
!
!  For instance:
!
!    Input             Output
!
!    '     Hello'      'Hello     '
!    ' Hi there!  '    'Hi there!   '
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
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
!  STRING Input/output, character ( len = * )  STRING.
!
!         On input, STRING is a string of characters.
!
!         On output, any initial blank characters in STRING
!         have been cut, and pasted back onto the end.
!
  integer ( kind = 4 ) i
  integer ( kind = 4 ) lchar
  integer ( kind = 4 ) nonb
  character null
  character ( len = * )  string
!
!
!  Check the length of the string to the last nonblank.
!  If nonpositive, return.
!
  lchar = len_trim ( string )
  if ( lchar <= 0)return
  null = char(0)
!
!  Find the occurrence of the first nonblank.
!
  do i = 1,lchar

    nonb = i
    if ( string(i:i) /= ' '.and. string(i:i) /= null)go to 10

  end do

  return

10    continue
!
!  Shift the string left.
!
  do i = 1,lchar+1-nonb
    string(i:i) = string(i+nonb-1:i+nonb-1)
  end do
!
!  Blank out the end of the string.
!
  do i = lchar-nonb+2,lchar
    string(i:i) = ' '
  end do

  return
end
subroutine fsize ( nflag,np,r,rtmax,rtmin,rvmax,rvmin)
!
!***********************************************************************
!
!! FSIZE computes the maximum and minimum entries of a real array 
!  defined at the nodes.  Only "flagged" values are considered.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
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
!  NFLAG  Input, logical NFLAG(NP).
!
!         NFLAG is used to "flag" which nodes are visible.
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!  R      Input, real R(NP), the real array to be examined.
!
!  RTMAX,
!  RTMIN  Output, real RTMAX, RTMIN, the maximum and minimum values
!         encountered in R.
!
!  RVMAX,
!  RVMIN  Output, real RVMAX, RVMIN, the maximum and minimum values
!         of R at visible nodes.
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
  rtmax = 0.0
  rtmin = 0.0

  startrv = .false.
  rvmax = 0.0
  rvmin = 0.0
 
  do i = 1,np
 
    if ( startrt ) then
      rtmin = min(rtmin,r(i))
      rtmax = max(rtmax,r(i))
    else
      startrt = .true.
      rtmin = r(i)
      rtmax = r(i)
    end if

    if ( nflag(i) ) then
 
      if ( startrv ) then
        rvmin = min(rvmin,r(i))
        rvmax = max(rvmax,r(i))
      else
        startrv = .true.
        rvmin = r(i)
        rvmax = r(i)
      end if
 
    end if
 
  end do

  return
end
subroutine get_inc ( ielem,jloc,maxnpe,nelem,node,np,npe,rho,sinc,u,v,xc,yc)
!
!***********************************************************************
!
!! GET_INC is used by STREAM, to compute SINC, the stream function 
!  increment in moving from local node JLOC to local node JLOC+1 in 
!  element IELEM.
!
!  Question: What do you mean by KLOC, since it's not an argument?
!
!  Here is a picture of a linear triangular element (NPE = 3):
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
!  and here is a picture of a linear rectangular element (NPE = 4):
!
!       A
!       | 
!   E   |   
!   T   |  2-----3
!   A   |  |     |
!       |  |     |
!       |  1-----4
!       |
!       +----------->
!           XSI
!
!  and here is a picture of a quadratic element (NPE = 6):
!
!       A
!       |        2
!       |       /|
!   E   |      / |
!   T   |     4  5
!   A   |    /   |
!       |   /    |
!       |  1--6--3
!       |
!       +----------->
!           XSI
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
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
!  IELEM  Input, integer ( kind = 4 ) IELEM, the element we are examining.
!
!  JLOC   Input, integer ( kind = 4 ) JLOC, the local node number of interest.
!
!  MAXNPE Input, integer ( kind = 4 ) MAXNPE.
!         MAXNPE is the maximum number of nodes per element.
! 
!    Input, integer ( kind = 4 ) NELEM.
!         The number of elements.
! 
!  NODE   Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), or NODE(MAXNPE,NELEM).
!         NODE contains, for each element, the global node numbers
!         of its NPE nodes.  
! 
!         For linear elements (NPE = 3), the order of the nodes probably
!         doesn't matter, but we will draw them this way:
!
!                       2
!                      /|
!                    /  |
!                  /    |
!                /      |
!               1-------3
!
!         For quadratic elements (NPE = 6), the nodes must be given in a 
!         particular order, which is as follows:
!
!                       2
!                      /|
!                    /  |
!                  4    5
!                /      |
!               1---6---3
! 
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!  NPE    Input, integer ( kind = 4 ) NPE.
!         NPE is the number of nodes per element, which should be
!         3 for linear elements and 6 for quadratics.
!
!  SINC   Output, real SINC, the increment in the stream function.
!
!  U,
!  V      Input, real U(NP), V(NP), the horizontal and vertical
!         velocities at each node.
!
!  XC     Input, real XC(NP).
!         The X coordinates of the nodes.
! 
!  YC     Input, real YC(NP).
!         The Y coordinates of the nodes.
! 
  integer ( kind = 4 ) ngauss
!
  parameter (ngauss = 3)
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
  xq(1) = -0.77459666
  xq(2) =  0.0
  xq(3) =  0.77459666

  wq(1) = 5.0/9.0
  wq(2) = 8.0/9.0
  wq(3) = 5.0/9.0
!
!  Compute the line integral along one side of the element.
!
  sinc = 0.0

  do igauss = 1,3

    if ( npe == 3 ) then

      if ( jloc == 1 ) then
        xsi = (xq(igauss)+1.0)/4.0
        eta = (xq(igauss)+1.0)/4.0
        dxsids = sqrt(2.0)/2.0
        detads = sqrt(2.0)/2.0
        dels = sqrt(2.0)/2.0
      else if ( jloc == 2 ) then
        xsi = 1.0
        eta = (xq(igauss)+3.0)/4.0
        dxsids = 0.0
        detads = -1.0
        dels = 0.5
      else if ( jloc == 3 ) then
        xsi = (xq(igauss)+3.0)/4.0
        eta = 0.0
        dxsids = -1.0
        detads = 0.0
        dels = 0.5
      end if

      call tranl3(det,detadx,detady,dxdeta,dxdxsi,dxsidx,dxsidy, &
        dydeta,dydxsi,eta,ielem,maxnpe,nelem,node,np,npe,xc,xsi,yc)

    else if ( npe == 4 ) then

      if ( jloc == 1 ) then
        xsi = 0.0
        eta = (xq(igauss)+1.0)/2.0
        dxsids = 0.0
        detads = 1.0
        dels = 1.0
      else if ( jloc == 2 ) then
        xsi = (xq(igauss)+1.0)/2.0
        eta = 1.0
        dxsids = 1.0
        detads = 0.0
        dels = 1.0
      else if ( jloc == 3 ) then
        xsi = 1.0
        eta = (1.0-xq(igauss))/2.0
        dxsids = 0.0
        detads = -1.0
        dels = 1.0
      else if ( jloc == 4 ) then
        xsi = (1.0-xq(igauss))/2.0
        eta = 0.0
        dxsids = 0.0
        detads = -1.0
        dels = 1.0
      end if

      call tranl4(det,detadx,detady,dxdeta,dxdxsi,dxsidx,dxsidy, &
        dydeta,dydxsi,eta,ielem,maxnpe,nelem,node,np,npe,xc,xsi,yc)

    else if ( npe == 6 ) then

      if ( jloc == 1 ) then
        xsi = (xq(igauss)+1.0)/4.0
        eta = (xq(igauss)+1.0)/4.0
        dxsids = sqrt(2.0)/2.0
        detads = sqrt(2.0)/2.0
        dels = sqrt(2.0)/2.0
      else if ( jloc == 4 ) then
        xsi = (xq(igauss)+3.0)/4.0
        eta = (xq(igauss)+3.0)/4.0
        dxsids = sqrt(2.0)/2.0
        detads = sqrt(2.0)/2.0
        dels = sqrt(2.0)/2.0
      else if ( jloc == 2 ) then
        xsi = 1.0
        eta = (xq(igauss)+3.0)/4.0
        dxsids = 0.0
        detads = -1.0
        dels = 0.5
      else if ( jloc == 5 ) then
        xsi = 1.0
        eta = (xq(igauss)+1.0)/4.0
        dxsids = 0.0
        detads = -1.0
        dels = 0.5
      else if ( jloc == 3 ) then
        xsi = (xq(igauss)+3.0)/4.0
        eta = 0.0
        dxsids = -1.0
        detads = 0.0
        dels = 0.5
      else if ( jloc == 6 ) then
        xsi = (xq(igauss)+1.0)/4.0
        eta = 0.0
        dxsids = -1.0
        detads = 0.0
        dels = 0.5
      end if

      call tranq6(det,detadx,detady,dxdeta,dxdxsi,dxsidx,dxsidy, &
        dydeta,dydxsi,eta,ielem,maxnpe,nelem,node,np,npe,xc,xsi,yc)

    end if
!
!  Evaluate U and V at the Gauss point.
!
    call ueval(detadx,detady,dxsidx,dxsidy,eta,ielem, &
      maxnpe,nelem,node,np,npe,rho,rhoval,u,uval,v,vval,xsi)
!
!  Compute the contribution to the increment.
!
    sinc = sinc+0.5*dels*wq(igauss)*rhoval &
      *((-vval*dxdxsi+uval*dydxsi)*dxsids &
      +(-vval*dxdeta+uval*dydeta)*detads )

  end do

  return
end
subroutine get_map3 ( a,b,c,f,x,y )
!
!***********************************************************************
!
!! GET_MAP3 computes the mapping coefficients defined by the nodes
!  of a linear isoparametric element.
!
!  The resulting interpolating mapping has the form:
!
!    F(X,Y)  =  A*X + B*Y + C
!
!  which is taken to hold only in the given element.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
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
!  A,
!  B,
!  C      Output, real A, B, C, the mapping coefficients
!         defined by the nodes.
!
!  F      Input, real F(3), the value of the function to be interpolated,
!         at each of the three nodes.
!
!  X      Input, real X(3), the X coordinates of the three nodes
!         of a linear isoparametric element, given in the standard 
!         order.
!
!  Y      Input, real Y(3), the Y coordinates of the three nodes
!         of a linear isoparametric element, given in the standard 
!         order.
!
  real a
  real b
  real c
  real denom
  real f(3)
  real x(3)
  real y(3)
!
  denom = -x(2)*x(3)+x(3)*y(2)-x(3)*y(1)+x(1)*y(3)-x(1)*y(2)+x(2)*y(1)

  a = ( f(1)*(y(3)-y(2))+f(2)*(y(1)-y(3))+f(3)*(y(2)-y(1)) )/denom
  b = ( f(1)*(x(3)-x(2))+f(2)*(x(1)-x(3))+f(3)*(x(2)-x(1)) )/denom
  c = ( f(1)*(x(2)*y(3)-x(3)*y(2))+f(2)*(x(3)*y(1)-x(1)*y(3)) &
      +f(3)*(x(1)*y(2)-x(2)*y(1)) )/denom

  return
end
subroutine get_map6 ( a,b,c,d,e,f,x )
!
!***********************************************************************
!
!! GET_MAP6 computes the mapping coefficients defined by the nodes
!  of a quadratic isoparametric element.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
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
!  A,
!  B,
!  C,
!  D,
!  E,
!  F      Output, real A, B, C, D, E, F, the mapping coefficients
!         defined by the nodes.
!
!  X      Input, real X(6), the X (or Y) coordinates of the six nodes
!         of a quadratic isoparametric element, given in the standard 
!         order.
!
  real a
  real b
  real c
  real d
  real e
  real f
  real x(6)
!
  a =  2.0*x(1)+2.0*x(3)-4.0*x(6)
  b = -4.0*x(3)-4.0*x(4)+4.0*x(5)+4.0*x(6)
  c =  2.0*x(2)+2.0*x(3)-4.0*x(5)
  d = -3.0*x(1)         -    x(3)+4.0*x(6)
  e =              -x(2)+    x(3)+4.0*x(4)-4.0*x(6)
  f =      x(1)

  return
end
subroutine get_tab ( dev,echo,filgrf,grace,icmax,icmin,ierror,iplot,itable,ovrlay)
!
!***********************************************************************
!
!! GET_TAB gets the color table choice from the user.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
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
!  DEV    Input, character*10 DEV.
!         The graphics output device to be used.  Current legal
!         values for DEV include:
! 
!         cgmb - CGM binary file.
!         ps   - PostScript file.
!         xws  - X window screen (interactive).
!
!  FILGRF Input, character ( len = 80 ) FILGRF, the name of the output
!         graphics file.
!
!    Input, real GRACE.
!         The size of the "grace" margin on the plot.
!
!  ICMAX,
!  ICMIN  Input, integer ( kind = 4 ) ICMAX, ICMIN, the maximum and minimum color 
!         indices to use in color contour graphics.
!
!  IERROR Output, integer ( kind = 4 ) IERROR, error flag.
!         0, no error occurred.
!         nonzero, an error occurred.
!
!  IPLOT  Input, integer ( kind = 4 ) IPLOT.
!         The number of plots made so far.
! 
!  ITABLE Output, integer ( kind = 4 ) ITABLE, the desired color table.
!  
!         1: low black to high white
!         2: low blue to high yellow
!         3: low red, high blue, with bands between.
!         4: low red, yellow, green, blue, high white.
!         5: low white, blue, green, yellow, high red.
!         6: low blue to high red
!         7: linear table between 2 user colors.
!         8: linear table between N user colors.
!         9: low white to high black.
! 
!  OVRLAY Input, logical OVRLAY.
!         If OVRLAY is true, then the next time that a plot is
!         requested, a "new frame" command is suppressed, so that
!         the new plot is shown on top of the previous one.
! 
  character ( len = 10 ) dev
  logical echo
  character ( len = 80 ) filcol
  character ( len = 80 ) filgrf
  real grace
  integer ( kind = 4 ) icmax
  integer ( kind = 4 ) icmin
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) iplot
  integer ( kind = 4 ) itable
  logical ovrlay
  real x1max
  real x1min
  real y1max
  real y1min
!
  if ( itable == 0 ) then

    write(*,*)' '
    write(*,*)'GETTAB - Input request:'
    write(*,*)'  Enter name of color table file.'

    read(*,'(a)',err = 90,end=90)filcol
    write(17,'(a)')filcol
    if ( echo ) then
      write(*,'(a)')filcol
    end if

    call getctb(icmin,icmax,filcol,ierror)

  end if

  call preplt(dev,echo,filgrf,icmax,icmin,iplot,itable,ovrlay)

  call settab(echo,icmax,icmin,itable)

  if ( dev == 'xws' ) then

    call cbox(grace)

    call setwcd(x1min,y1min,x1max,y1max,ierror)

    call buzz ( dev, x1min, x1max, y1min, y1max )

  end if

  return

90    continue
  write(*,*)' '
  write(*,*)'GetTab - Error'
  ierror = 1
  return
end
subroutine get_unit ( iunit )
!
!*******************************************************************************
!
!! GET_UNIT returns a free FORTRAN unit number.
!
!  Discussion:
!
!    A "free" FORTRAN unit number is an integer between 1 and 99 which
!    is not currently associated with an I/O device.  A free FORTRAN unit
!    number is needed in order to open a file with the OPEN command.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
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
subroutine get_win ( echo,grace,srange,xmax,xmin,x1max,x1min,x2max, &
  x2min,xsmax,xsmin,ymax,ymin,y1max,y1min,y2max,y2min,ysmax,ysmin)
!
!***********************************************************************
!
!! GET_WIN responds to the "W" command by telling the user
!  the current window, and getting the new value from the
!  user.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
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
!    Input, real GRACE.
!         The size of the "grace" margin on the plot.
!
!  SRANGE Output, real SRANGE.
!         The maximum of XSMAX-XSMIN and YSMAX-YSMIN.
!         This gives the size of a square containing the data
!         window.
!
!  XMAX   Input, real XMAX.
!         The maximum X coordinate of all the nodes.
!         The maximum entry in the XC array.
! 
!  XMIN   Input, real XMIN.
!         The minimum X coordinate of all the nodes.
!         The minimum entry in the XC array.
!   
!  X1MAX,
!  X1MIN  Input/output, real X1MAX, X1MIN, the maximum and minimum X 
!         coordinates of the plot, which includes a small grace margin.
!
!  X2MAX,
!  X2MIN  Input/output, real X2MAX, X2MIN, the maximum and minimum X 
!         coordinates that should be used for plotting.  No plotting 
!         commands should exceed these values.  This is where the 
!         "frame" might be drawn.
!
!  XSMAX  Input/output, real XSMAX.
!         The maximum X coordinate of the data to be displayed.
!         XSMAX defaults to XMAX, but can be made smaller to
!         focus on a portion of the region.
! 
!  XSMIN  Input/output, real XSMIN.
!         The minimum X coordinate of the data to be displayed.
!         XSMIN defaults to XMIN, but can be made larger to
!         focus on a portion of the region.
! 
!  Y1MAX,
!  Y1MIN  Input/output, real Y1MAX, Y1MIN, the maximum and minimum Y
!         coordinates of the plot, which includes a small grace margin.
!
!  Y2MAX,
!  Y2MIN  Input/output, real Y2MAX, Y2MIN, the maximum and minimum Y 
!         coordinates that should be used for plotting.  No plotting commands 
!         should  exceed these values.  This is where the "frame" might be 
!         drawn.
!
!  YMAX   Input, real YMAX.
!         The maximum Y coordinate of all the nodes.
!         The maximum value attained by the YC array.
! 
!  YMIN   Input, real YMIN.
!         The minimum Y coordinate of all the nodes.
!         The minimum value attained by the YC array.
!  
!  YSMAX  Input/output, real YSMAX.
!         The maximum Y coordinate of the data to be displayed.
!         YSMAX defaults to YMAX, but can be made smaller to
!         focus on a portion of the region.
! 
!  YSMIN  Input/output, real YSMIN.
!         The minimum Y coordinate of the data to be displayed.
!         YSMIN defaults to YMIN, but can be made larger to
!         focus on a portion of the region.
! 
  logical echo
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
  write(*,*)' '
  write(*,*)'GetWin:'
  write(*,*)' '
  write(*,*)'  Total picture coordinates:'
  write(*,*)' '
  write(*,*)x1min,' X1MIN < =  X <= X1MAX ',x1max
  write(*,*)y1min,' Y1MIN < =  Y <= Y1MAX ',y1max
  write(*,*)' '
  write(*,*)'  Graphing area coordinates:'
  write(*,*)' '
  write(*,*)x2min,' X2MIN < =  X <= X2MAX ',x2max
  write(*,*)y2min,' Y2MIN < =  Y <= Y2MAX ',y2max
  write(*,*)' '
  write(*,*)'  Displayed data coordinates:'
  write(*,*)' '
  write(*,*)xsmin,' XSMIN < =  X <= XSMAX ',xsmax
  write(*,*)ysmin,' YSMIN < =  Y <= YSMAX',ysmax
  write(*,*)' '
  write(*,*)'  Data coordinates:'
  write(*,*)' '
  write(*,*)xmin,' < =  X <= ',xmax
  write(*,*)ymin,' < =  Y <= ',ymax
  write(*,*)' '

  write(*,*)'  Enter new displayed data coordinates for X and Y:'
  write(*,*)'  Use the order Xsmin, Xsmax, Ysmin, Ysmax:'

  read(*,*,err = 20,end=20)xsmin,xsmax,ysmin,ysmax
  write(17,*)xsmin,xsmax,ysmin,ysmax
  if ( echo ) then
    write(*,*)xsmin,xsmax,ysmin,ysmax
  end if
!
!  Compute box containing data.
!
  call pltbox(grace,srange,x1max,x1min,x2max,x2min,xsmax,xsmin, &
    y1max,y1min,y2max,y2min,ysmax,ysmin)

  return

20    continue
  write(*,*)' '
  write(*,*)'GETWIN - Warning!'
  write(*,*)'  Your input was not acceptable!'
  write(*,*)'  Please try again!'
  write(*,*)' '
  go to 10

end
subroutine gquad ( wq, xq )
!
!***********************************************************************
!
!! GQUAD returns the weights and abscissas for a 1 dimensional,
!  3 point Gauss quadrature rule defined on the interval [-1,1].
! 
!  The integral of a function F(X) over the interval [-1,1] 
!
!    Integral (-1 to 1) F(X) DX
!
!  may then be approximated by
!
!    Sum (I  =  1 to 3) WQ(I) * F(XQ(I))
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
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
!  WQ     Output, real WQ(3).
!         WQ(I) is the weight factor corresponding to the
!         I-th quadrature point.
!
!  XQ     Output, real XQ(3).
!         XQ(I) is the I-th quadrature point.
!
  real wq(3)
  real xq(3)
!
  xq(1) = -0.77459666
  xq(2) =  0.0
  xq(3) =  0.77459666

  wq(1) = 5.0/9.0
  wq(2) = 8.0/9.0
  wq(3) = 5.0/9.0

  return
end
subroutine graph ( arrow, cp, delx, dely, dev, dudxn, dudyn, dvdxn, dvdyn, &
  echo,eflag, &
  eflagu,etaref,filgrf,filtyp,icmax,icmin,icolor,icomp,iplot,isotri, &
  itable,iwork1,iwork2,jbound,jcmax,jcmin,lbar,line,maxbou,maxnp,maxnpe, &
  maxobj,maxsen,nbound,ncon,nelem,nflag,nflag0,node,np,npe, &
  nxskip,ny,nyskip,object,ovrlay,p,rho,rmach,s,s2,scalee,scalen,scalev, &
  show,smax,smin,srange,t,title,title2,u,v,x1max,x1min,x2max,x2min,xc,xprof, &
  xsiref,xsmax,xsmin,y1max,y1min,y2max,y2min,yc,ysmax,ysmin)
!
!***********************************************************************
!
!! GRAPH draws the graph.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 January 2001
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
!  DEV    Input, character*10 DEV.
!         The graphics output device to be used.  Current legal
!         values for DEV include:
! 
!         cgmb - CGM binary file.
!         ps   - PostScript file.
!         xws  - X window screen (interactive).
!
!  EFLAG  Input, logical EFLAG(NELEM).
!         EFLAG is used to "flag" which elements are to be displayed.
!         If EFLAG(I) is TRUE, then element I should be displayed.
! 
!  EFLAGU Input, logical EFLAGU(NELEM), allows the user to mark
!         certain elements not to be displayed.
!
!  ETAREF Input, real ETAREF(NPE).
!         The ETA coordinates of the nodes of the reference
!         triangle.
! 
!  FILGRF Input, character ( len = 80 ) FILGRF, the name of the output
!         graphics file.
!
!  ICMAX,
!  ICMIN  Input, integer ( kind = 4 ) ICMAX, ICMIN, the maximum and minimum color 
!         indices to use in color contour graphics.
!
!  ICOLOR Input, integer ( kind = 4 ) ICOLOR(MAXOBJ).
!         ICOLOR contains the color index for each object.
! 
!  ICOMP  Input, integer ( kind = 4 ) ICOMP.
!         The component of the flow to be studied.
! 
!         ICOMP = 0 studies the basic flow.
!         ICOMP = I studies the sensitivity with respect to
!                 parameter I.
! 
!  IPLOT  Input, integer ( kind = 4 ) IPLOT.
!         The number of plots made so far.
! 
!  ISOTRI Input, integer ( kind = 4 ) ISOTRI(NELEM).
! 
!         0, if element I is not isoparametric.
!         1 or 2, if element I is isoparametric.
!
!  ITABLE Input, integer ( kind = 4 ) ITABLE, the desired color table.
!  
!         1: low black to high white
!         2: low blue to high yellow
!         3: low red, high blue, with bands between.
!         4: low red, yellow, green, blue, high white.
!         5: low white, blue, green, yellow, high red.
!         6: low blue to high red
!         7: linear table between 2 user colors.
!         8: linear table between N user colors.
!         9: low white to high black.
! 
!  IWORK1 Workspace, integer IWORK1(NELEM).
!
!  IWORK2 Workspace, integer IWORK2(NP).
!
!  JBOUND Input, integer ( kind = 4 ) JBOUND(5,MAXBOU)
! 
!         For each line segment of the boundary:
! 
!         JBOUND(1,I) contains the element number;
! 
!         JBOUND(2,I) contains the local node number of one corner 
!           of the element, which forms the edge;
!
!         JBOUND(2,I) contains the "next" node along the edge.
!           If the element is linear, this is the other corner node.
!           If the element is quadratic, this is the midside node along
!             the edge.
!
!         JBOUND(4,I) contains the "next" node along the edge.
!           If the element is linear, this is 0.
!           If the element is quadratic, this is the other corner node 
!             along the edge.
!
!         JBOUND(5,I) contains:
!           0 if the boundary is a wall (U = V=0);
!           1 if the boundary is open.
!
!  JCMAX,
!  JCMIN  Input, integer ( kind = 4 ) JCMAX, JCMIN, the maximum and minimum color
!         indices to use for contours.
!
!  LBAR   Input, logical LBAR, .TRUE. if the color bar may be drawn,
!         .FALSE. if it should not be drawn.
!
!  MAXBOU Input, integer ( kind = 4 ) MAXBOU.
!         The amount of storage available for the IBOUND array.
! 
!  MAXNP  Input, integer ( kind = 4 ) MAXNP, the maximum number of nodes.
!
!  MAXNPE Input, integer ( kind = 4 ) MAXNPE.
!         MAXNPE is the maximum number of nodes per element.
! 
!  MAXOBJ Input, integer ( kind = 4 ) MAXOBJ.
!         The number of graphical "objects" including:
! 
!  MAXPAR Input, integer ( kind = 4 ) MAXPAR, the maximum number of parameters.
!
!  NBOUND Input, integer ( kind = 4 ) NBOUND.
!         The number of points (XBOUND(I),YBOUND(I)) used to
!         define the boundary.
! 
!  NCON   Input, integer ( kind = 4 ) NCON, the number of contours to draw.
!
!    Input, integer ( kind = 4 ) NELEM, the number of elements.
!
!    Input, logical NFLAG(MAXNP), flags nodes which are active,
!    that is, to be displayed, and which not, in the graph.
!
!    Input, logical NFLAG0(MAXNP), a user-defined node visibility flag.
!
!    Local, logical NFLAG1(MAXNP), flags nodes which are within the
!    current window of [XSMIN,XSMAX] by [YSMIN,YSMAX].
!
!    Local, logical NFLAG2(MAXNP), flags nodes which are element corner nodes.
!
!  NODE   Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), or NODE(MAXNPE,NELEM).
!         NODE contains, for each element, the global node numbers
!         of its NPE nodes.  
!
!         For linear elements (NPE = 3), the order of the nodes probably
!         doesn't matter, but we will draw them this way:
!
!                       2
!                      /|
!                    /  |
!                  /    |
!                /      |
!               1-------3
!  
!         For quadratic elements (NPE = 6), the nodes must be given in a 
!         particular order, which is as follows:
!
!                       2
!                      /|
!                    /  |
!                  4    5
!                /      |
!               1---6---3
! 
!    Input, integer ( kind = 4 ) NP.
!         NP is the total number of nodes. 
!
!  NPE    Input, integer ( kind = 4 ) NPE.
!         NPE is the number of nodes per element, which should be
!         3 for linear elements and 6 for quadratics.
!
!  NXSKIP Input, integer ( kind = 4 ) NXSKIP.
!         NXSKIP is used to "thin" out a vector plot.  
!
!         If NXSKIP = 1, then a standard vector plot is made.
!
!         Otherwise, in the X direction, vectors are drawn only
!         in columns 1, 1+NXSKIP, 1+2*NXSKIP and so on.
!  
!  NY     Input, integer ( kind = 4 ) NY.
!         Determines the number of nodes and elements in the Y
!         direction.  There will be 2*NY-1 nodes, 2*NY-2 elements.
! 
!  NYSKIP Input, integer ( kind = 4 ) NYSKIP.
!         NYSKIP is used to "thin" out a vector plot.  
!
!         If NYSKIP = 1, then a standard vector plot is made.
!
!         Otherwise, in the Y direction, vectors are drawn only
!         in rows 1, 1+NYSKIP, 1+2*NYSKIP and so on.
!  
!  OBJECT Input, character ( len = 30 ) OBJECT(MAXOBJ), the names of the 
!         graphical objects.
!
!  OVRLAY Input, logical OVRLAY.
!         If OVRLAY is true, then the next time that a plot is
!         requested, a "new frame" command is suppressed, so that
!         the new plot is shown on top of the previous one.
! 
!  P      Input, real P(MAXNP,0:MAXPAR).
! 
!         P(I,0) is the pressure at node I.
! 
!         P(I,J) is the sensitivity of the pressure with respect
!         to parameter J.
! 
!  S      Workspace, real S(MAXNP).
!         S is used to store the stream function, or certain other
!         scalar functions derived from the basic flow.
! 
!  SCALEE Input, real SCALEE.
!         The scale factor for the element numbers, with default value 1.
!
!  SCALEN Input, real SCALEN.
!         The scale factor for the node numbers, with default value 1.
!
!  SCALEV Input, real SCALEV.
!         A scale factor for velocity vectors.  This starts out at 1.0.
! 
!  SHOW   Input, logical SHOW(MAXOBJ).
!         Contains, for each object, a flag determining whether it
!         is to be shown or not.
! 
!  SRANGE Input, real SRANGE.
!         The maximum of XSMAX-XSMIN and YSMAX-YSMIN.
!         This gives the size of a square containing the data
!         window.
! 
!  T      Workspace, real T(MAXNP).
! 
!  TITLE  Input, character ( len = 40 ) TITLE.
!         A title for the plots.
! 
!  TITLE2 Input, character ( len = 40 ) TITLE2.
!         A subtitle used in the profile plots.
!
!  U      Input, real U(MAXNP,MAXPAR).
! 
!         U(I,0) is the horizontal fluid velocity at node I.
! 
!         U(I,J) is the sensitivity of the horizontal velocity with 
!         respect to parameter J.
!  
!  V      Input, real V(MAXNP,MAXPAR).
! 
!         V(I,0) is the vertical fluid velocity at node I.
! 
!         V(I,J) is the sensitivity of the vertical velocity with
!         respect to parameter J.
! 
!  X1MAX,
!  X1MIN  Input, real X1MAX, X1MIN, the maximum and minimum X 
!         coordinates of the plot, which includes a small grace margin.
!
!  X2MAX,
!  X2MIN  Input, real X2MAX, X2MIN, the maximum and minimum X coordinates that
!         should be used for plotting.  No plotting commands should 
!         exceed these values.  This is where the "frame" might be drawn.
!
!  XC     Input, real XC(MAXNP).
!         The X coordinates of the nodes.
! 
!  XPROF  Input, real XPROF.
!         The X coordinate of the profile line.
! 
!  XSIREF Input, real XSIREF(NPE).
!         The XSI coordinates of the nodes of the reference
!         triangle.
! 
!  XSMAX  Input, real XSMAX.
!         The maximum X coordinate of the data to be displayed.
!         XSMAX defaults to XMAX, but can be made smaller to
!         focus on a portion of the region.
! 
!  XSMIN  Input, real XSMIN.
!         The minimum X coordinate of the data to be displayed.
!         XSMIN defaults to XMIN, but can be made larger to
!         focus on a portion of the region.
! 
!  Y1MAX,
!  Y1MIN  Input, real Y1MAX, Y1MIN, the maximum and minimum Y
!         coordinates of the plot, which includes a small grace margin.
!
!  Y2MAX,
!  Y2MIN  Input, real Y2MAX, Y2MIN, the maximum and minimum Y coordinates that
!         should be used for plotting.  No plotting commands should 
!         exceed these values.  This is where the "frame" might be drawn.
!
!  YC     Input, real YC(MAXNP).
!         The Y coordinates of the nodes.
! 
!  YSMAX  Input, real YSMAX.
!         The maximum Y coordinate of the data to be displayed.
!         YSMAX defaults to YMAX, but can be made smaller to
!         focus on a portion of the region.
! 
!  YSMIN  Input, real YSMIN.
!         The minimum Y coordinate of the data to be displayed.
!         YSMIN defaults to YMIN, but can be made larger to
!         focus on a portion of the region.
! 
  integer ( kind = 4 ) maxbou
  integer ( kind = 4 ) maxnp
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) maxobj
  integer ( kind = 4 ) maxsen
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) np
  integer ( kind = 4 ) npe
!
  real angle
  character ( len = 10 ) arrow
  character ( len = 6 ) chrtmp
  real cp(np)
  real csize
  real cwide
  real delx
  real dely
  character ( len = 10 ) dev
  real dshsiz
  real dudxn(np)
  real dudyn(np)
  real dvdxn(np)
  real dvdyn(np)
  logical echo
  logical eflag(nelem)
  logical eflagu(nelem)
  real etaref(npe)
  character ( len = 80 ) filgrf
  logical filled
  character ( len = 20 ) filtyp
  character ( len = 6 ) flush
  integer ( kind = 4 ) i
  integer ( kind = 4 ) iblack
  integer ( kind = 4 ) icmax
  integer ( kind = 4 ) icmin
  integer ( kind = 4 ) icol
  integer ( kind = 4 ) icolor(maxobj)
  integer ( kind = 4 ) icomp
  integer ( kind = 4 ) ido
  logical inside
  integer ( kind = 4 ) iplot
  integer ( kind = 4 ) irow
  character isay
  integer ( kind = 4 ) isotri(nelem)
  integer ( kind = 4 ) itable
  integer ( kind = 4 ) iwork1(nelem)
  integer ( kind = 4 ) iwork2(np)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jbound(5,maxbou)
  integer ( kind = 4 ) jcmax
  integer ( kind = 4 ) jcmin
  integer ( kind = 4 ) jcolor
  integer ( kind = 4 ) k
  logical lbar
  integer ( kind = 4 ) lent
  logical s_eqi
  integer ( kind = 4 ) line(maxobj)
  integer ( kind = 4 ) maxe
  integer ( kind = 4 ) maxn
  integer ( kind = 4 ) mine
  integer ( kind = 4 ) minn
  integer ( kind = 4 ) nbound
  integer ( kind = 4 ) ncon
  logical nflag(np)
  logical nflag0(np)
  logical nflag1(np)
  logical nflag2(np)
  integer ( kind = 4 ) node(maxnpe,nelem)
  integer ( kind = 4 ) nonzer
  integer ( kind = 4 ) np_vis
  integer ( kind = 4 ) npts
  integer ( kind = 4 ) nval
  integer ( kind = 4 ) nvize
  integer ( kind = 4 ) nxskip
  integer ( kind = 4 ) ny
  integer ( kind = 4 ) nyskip
  character ( len = 30 ) object(maxobj)
  logical ovrlay
  real p(maxnp,0:maxsen)
  real pwide
  real rho(np)
  real rmach(np)
  real s(np)
  real s2(np)
  real scalee
  real scalen
  real scalev
  real scon
  logical show(maxobj)
  real smax
  real smin
  real srange
  real stmax
  real stmin
  real svmax
  real svmin
  real t(np)
  character ( len = * )  title
  character ( len = * )  title2
  real u(maxnp,0:maxsen)
  real uvmag
  real v(maxnp,0:maxsen)
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
  real xc(np)
  real xsiref(npe)
  real xprof
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
  real yc(np)
  real ysmax
  real ysmin
  real yval(4)
!
!  Determine NFLAG1, which says which nodes are within the current window.
!
  do i = 1, np

    if ( inside ( xsmin, xc(i), xsmax ) .and. &
         inside ( ysmin, yc(i), ysmax ) ) then
      nflag1(i) = .true.
    else
      nflag1(i) = .false.
    end if

  end do
!
!  Determine NFLAG2, which says which nodes are not being skipped.
!
  do i = 1, np

    if ( s_eqi ( filtyp, 'FLOW' ) ) then
      irow = 1 + mod(i-1,(2*ny-1))
      icol = 1 + ((i-1)/(2*ny-1))
    else
      irow = 1 + mod(i-1,ny)
      icol = 1 + (i-1)/ny
    end if

    if ( mod(irow-1,nyskip) /= 0 .or. mod((icol-1),nxskip) /= 0 ) then
      nflag2(i) = .false.
    else
      nflag2(i) = .true.
    end if

  end do
!
!  Report how many nodes are visible.
!
  np_vis = 0
  minn = np+1
  maxn = 0

  do i = 1, np

    if ( nflag0(i) .and. nflag1(i) .and. nflag2(i) ) then
      nflag(i) = .true.
      np_vis = np_vis+1
      minn = min(minn,i)
      maxn = max(maxn,i)
    else
      nflag(i) = .false.
    end if

  end do
 
  if ( np_vis /= np ) then
    write ( *, * ) ' '
    write ( *, * ) 'GRAPH - Note:'
    write ( *, * ) '  NOT ALL NODES WILL BE VISIBLE!'
    write ( *, * ) ' '
    write ( *, * ) '  The number of nodes is         ', np
    write ( *, * ) '  The number of visible nodes is ', np_vis
    write ( *, * ) '  They range from ',minn,' through ',maxn
  end if
!
!  Report how many elements are visible.
!  An element is only visible if the user wants to see it (EFLAGU),
!  and all its nodes lie within the window (NFLAG1).
!
  nvize = 0
  mine = nelem+1
  maxe = 0

  do i = 1, nelem

    eflag(i) = eflagu(i)

    do j = 1,npe
      if ( eflag(i) ) then
        k = node(j,i)
        if ( .not.nflag1(k) ) then
          eflag(i) = .false.
        end if
      end if
    end do

    if ( eflag(i) ) then
      nvize = nvize+1
      mine = min(mine,i)
      maxe = max(maxe,i)
    end if

  end do

  if ( nvize /= nelem ) then
    write ( *, * ) ' '
    write ( *, * ) 'GRAPH - Note:'
    write ( *, * ) '  NOT ALL ELEMENTS WILL BE VISIBLE!'
    write ( *, * ) ' '
    write ( *, * ) '  The number of elements is         ', nelem
    write ( *, * ) '  There are ', nvize, ' elements visible.'
    write ( *, * ) '  They range from ', mine,' through ', maxe
  end if
!
!  Prepare for the new plot.
!
  call preplt ( dev, echo, filgrf, icmax, icmin, iplot, itable, ovrlay )
!
!  Set the scale of the picture.
!  We allow ourselves more or less a one percent margin.
!
  xval(1) = x1min
  yval(1) = y1min
  xval(2) = x1max
  yval(2) = y1max
 
  call setscl(xval,yval,2)
!
!  Draw a box around the region.
!
  if ( show(3) ) then
    call linclr ( icolor(3) )
    call box ( x2min, x2max, y2min, y2max )
  end if
!
!  Draw the background
!
  if ( show(21) ) then
    call filclr(icolor(21))
    xval(1) = x1min
    yval(1) = y1min
    xval(2) = x1max
    yval(2) = y1min
    xval(3) = x1max
    yval(3) = y1max
    xval(4) = x1min
    yval(4) = y1max
    npts = 4
    call plygon(npts,xval,yval)
  end if
!
!  Draw the titles.
!
  if ( show(7) ) then

    lent = len_trim ( title )

    if ( lent > 0 ) then
      angle = 0.0
      cwide = 0.9*srange/40
      x = 0.5*(xsmax+xsmin)
      y = ysmax+3.0*cwide
      call linclr(icolor(7))
      pwide = srange
      flush = 'center'
      call s_plot(angle,cwide,pwide,title(1:lent),x,y,flush)
    end if

    lent = len_trim ( title2 )

    if ( lent > 0 ) then
      angle = 0.0
      cwide = 0.9*srange/40
      x = 0.5*(xsmax+xsmin)
      y = ysmax+1.5*cwide
      call linclr(icolor(7))
      pwide = srange
      flush = 'center'
      call s_plot(angle,cwide,pwide,title2(1:lent),x,y,flush)
    end if

  end if
!
!  COLOR CONTOURS ARE DRAWN HERE.
!
!
!  CPC: draw CP color contours.
!
  if ( show(32) ) then

    do i = 1,np
      s(i) = cp(i)
      nflag(i) = nflag0(i).and.nflag1(i)
    end do

    call fsize(nflag,np,s,stmax,stmin,svmax,svmin)

    if ( stmax > stmin ) then

      call setsiz(echo,object(32),smax,smin,stmax,stmin,svmax,svmin)

      if ( npe == 3 ) then

        call conc3(eflag,jcmax,jcmin,maxnpe,ncon,nelem,node,np,s,smax,smin,xc,yc)

      else if ( npe == 4 ) then

        call conc4(eflag,jcmax,jcmin,maxnpe,ncon,nelem,node,np,s,smax,smin,xc,yc)

      else if ( npe == 6 ) then

        call conc6(eflag,etaref,isotri,jcmax,jcmin,maxnpe,ncon, &
          nelem,node,np,npe,s,smax,smin,xc,xsiref,yc)

      end if

      if ( lbar ) then
        x1 = x2min
        x2 = x2max
        y1 = y2max
        y2 = y1max
        call cbar(icolor,jcmax,jcmin,maxobj,ncon,smax,smin,srange,x1,x2,y1,y2)
      end if

    else

      write(*,*)' '
      write(*,'(a)')'GRAPH - Warning!'
      write(*,*)'  Could not display '//object(12)
      write(*,*)'  All data values were equal.'

    end if

  end if
!
!  EC: draw the element colors.
!
  if ( show(20) ) then

    call filclr(icolor(20))

    call element_color ( eflag, etaref, isotri, maxnpe, nelem, node, np, &
      npe, xc, xsiref, yc )

  end if
!
!  KVMAGC: draw kinematic velocity magnitude color contours.
!
  if ( show(14) ) then

    do i = 1,np
      s(i) = sqrt(u(i,icomp)**2+v(i,icomp)**2)
      nflag(i) = nflag0(i).and.nflag1(i)
    end do

    call fsize(nflag,np,s,stmax,stmin,svmax,svmin)

    if ( stmax > stmin ) then

      call setsiz(echo,object(14),smax,smin,stmax,stmin,svmax,svmin)

      call conc6(eflag,etaref,isotri,jcmax,jcmin,maxnpe,ncon, &
        nelem,node,np,npe,s,smax,smin,xc,xsiref,yc)

      if ( lbar ) then
        x1 = x2min
        x2 = x2max
        y1 = y2max
        y2 = y1max
        call cbar(icolor,jcmax,jcmin,maxobj,ncon,smax,smin,srange,x1,x2,y1,y2)
      end if

    else

      write(*,*)' '
      write(*,'(a)')'GRAPH - Warning!'
      write(*,*)'  Could not display '//object(14)
      write(*,*)'  All data values were equal.'

    end if
 
  end if
!
!  KVXC: draw X kinematic velocity color contours.
!
  if ( show(17) ) then

    do i = 1,np
      nflag(i) = nflag0(i).and.nflag1(i)
      s(i) = u(i,icomp)
    end do

    call fsize(nflag,np,s,stmax,stmin,svmax,svmin)

    if ( stmax > stmin ) then

      call setsiz(echo,object(17),smax,smin,stmax,stmin,svmax,svmin)

      call conc6(eflag,etaref,isotri,jcmax,jcmin,maxnpe,ncon, &
        nelem,node,np,npe,s,smax,smin,xc,xsiref,yc)

      if ( lbar ) then
        x1 = x2min
        x2 = x2max
        y1 = y2max
        y2 = y1max
        call cbar(icolor,jcmax,jcmin,maxobj,ncon,smax,smin,srange,x1,x2,y1,y2)
      end if

    else

      write(*,*)' '
      write(*,'(a)')'GRAPH - Warning!'
      write(*,*)'  Could not display '//object(17)
      write(*,*)'  All data values were equal.'

    end if

  end if
!
!  KVYC: draw Y kinematic velocity color contours.
!
  if ( show(18) ) then

    do i = 1,np
      nflag(i) = nflag0(i).and.nflag1(i)
      s(i) = v(i,icomp)
    end do

    call fsize(nflag,np,s,stmax,stmin,svmax,svmin)

    if ( stmax > stmin ) then

      call setsiz(echo,object(18),smax,smin,stmax,stmin,svmax,svmin)

      call conc6(eflag,etaref,isotri,jcmax,jcmin,maxnpe,ncon, &
        nelem,node,np,npe,s,smax,smin,xc,xsiref,yc)

      if ( lbar ) then
        x1 = x2min
        x2 = x2max
        y1 = y2max
        y2 = y1max
        call cbar(icolor,jcmax,jcmin,maxobj,ncon,smax,smin,srange,x1,x2,y1,y2)
      end if

    else

      write(*,*)' '
      write(*,'(a)')'GRAPH - Warning!'
      write(*,*)'  Could not display '//object(18)
      write(*,*)'  All data values were equal.'

    end if

  end if
!
!  MACHC: draw Mach color contours.
!
  if ( show(33) ) then

    do i = 1,np
      s(i) = rmach(i)
      nflag(i) = nflag0(i).and.nflag1(i)
    end do

    call fsize(nflag,np,s,stmax,stmin,svmax,svmin)

    if ( stmax > stmin ) then

      call setsiz(echo,object(33),smax,smin,stmax,stmin,svmax,svmin)

      if ( npe == 3 ) then

        call conc3(eflag,jcmax,jcmin,maxnpe,ncon,nelem,node,np,s,smax,smin,xc,yc)

      else if ( npe == 4 ) then

        call conc4(eflag,jcmax,jcmin,maxnpe,ncon,nelem,node,np,s,smax,smin,xc,yc)

      else if ( npe == 6 ) then
 
        call conc6(eflag,etaref,isotri,jcmax,jcmin,maxnpe,ncon, &
          nelem,node,np,npe,s,smax,smin,xc,xsiref,yc)

      end if

      if ( lbar ) then
        x1 = x2min
        x2 = x2max
        y1 = y2max
        y2 = y1max
        call cbar(icolor,jcmax,jcmin,maxobj,ncon,smax,smin,srange,x1,x2,y1,y2)
      end if

    else

      write(*,*)' '
      write(*,'(a)')'GRAPH - Warning!'
      write(*,*)'  Could not display '//object(12)
      write(*,*)'  All data values were equal.'

    end if

  end if
!
!  MVMAGC: draw mass velocity magnitude color contours.
!
  if ( show(37) ) then

    do i = 1,np
      s(i) = rho(i)*sqrt(u(i,icomp)**2+v(i,icomp)**2)
      nflag(i) = nflag0(i).and.nflag1(i)
    end do

    call fsize(nflag,np,s,stmax,stmin,svmax,svmin)

    if ( stmax > stmin ) then

      call setsiz(echo,object(37),smax,smin,stmax,stmin,svmax,svmin)

      call conc6(eflag,etaref,isotri,jcmax,jcmin,maxnpe,ncon, &
        nelem,node,np,npe,s,smax,smin,xc,xsiref,yc)

      if ( lbar ) then
        x1 = x2min
        x2 = x2max
        y1 = y2max
        y2 = y1max
        call cbar(icolor,jcmax,jcmin,maxobj,ncon,smax,smin,srange,x1,x2,y1,y2)
      end if

    else

      write(*,*)' '
      write(*,'(a)')'GRAPH - Warning!'
      write(*,*)'  Could not display '//object(37)
      write(*,*)'  All data values were equal.'

    end if
 
  end if
!
!  PC: draw pressure color contours.
!
  if ( show(12) ) then

    if ( npe == 6 ) then
      write(*,*)' '
      write(*,*)'GRAPH - Question:'
      write(*,*)'  Use linear elements to graph pressure?'
      read(*,'(a)')isay
      write(17,'(a)')isay
      if ( echo ) then
        write(*,'(a)')isay
      end if
    end if

    do i = 1,np
      s(i) = p(i,icomp)
      nflag(i) = nflag0(i).and.nflag1(i)
    end do

    call fsize(nflag,np,s,stmax,stmin,svmax,svmin)

    if ( stmax > stmin ) then

      call setsiz(echo,object(12),smax,smin,stmax,stmin,svmax,svmin)

      if ( npe == 3 ) then

        call conc3(eflag,jcmax,jcmin,maxnpe,ncon,nelem,node,np,s,smax,smin,xc,yc)

      else if ( npe == 4 ) then

        call conc4(eflag,jcmax,jcmin,maxnpe,ncon,nelem,node,np,s,smax,smin,xc,yc)

      else if ( npe == 6 ) then
 
        if ( s_eqi ( isay,'n') ) then

          call conc6(eflag,etaref,isotri,jcmax,jcmin,maxnpe,ncon, &
            nelem,node,np,npe,s,smax,smin,xc,xsiref,yc)

        else

          call conc63(eflag,etaref,isotri,jcmax,jcmin,maxnpe, &
            ncon,nelem,node,np,npe,s,smax,smin,xc,xsiref,yc)

        end if
      end if

      if ( lbar ) then
        x1 = x2min
        x2 = x2max
        y1 = y2max
        y2 = y1max
        call cbar(icolor,jcmax,jcmin,maxobj,ncon,smax,smin,srange,x1,x2,y1,y2)
      end if

    else

      write(*,*)' '
      write(*,'(a)')'GRAPH - Warning!'
      write(*,*)'  Could not display '//object(12)
      write(*,*)'  All data values were equal.'

    end if

  end if
!
!  RHOC: draw Rho color contours.
!
  if ( show(34) ) then

    do i = 1,np
      s(i) = rho(i)
      nflag(i) = nflag0(i).and.nflag1(i)
    end do

    call fsize(nflag,np,s,stmax,stmin,svmax,svmin)

    if ( stmax > stmin ) then

      call setsiz(echo,object(34),smax,smin,stmax,stmin,svmax,svmin)

      if ( npe == 3 ) then

        call conc3(eflag,jcmax,jcmin,maxnpe,ncon,nelem,node,np,s,smax,smin,xc,yc)

      else if ( npe == 4 ) then

        call conc4(eflag,jcmax,jcmin,maxnpe,ncon,nelem,node,np,s,smax,smin,xc,yc)

      else if ( npe == 6 ) then
 
        call conc6(eflag,etaref,isotri,jcmax,jcmin,maxnpe,ncon, &
          nelem,node,np,npe,s,smax,smin,xc,xsiref,yc)

      end if

      if ( lbar ) then
        x1 = x2min
        x2 = x2max
        y1 = y2max
        y2 = y1max
        call cbar(icolor,jcmax,jcmin,maxobj,ncon,smax,smin,srange,x1,x2,y1,y2)
      end if

    else

      write(*,*)' '
      write(*,'(a)')'GRAPH - Warning!'
      write(*,*)'  Could not display '//object(12)
      write(*,*)'  All data values were equal.'

    end if

  end if
!
!  SC: draw stream color contours.
!
  if ( show(22) ) then

    call stream(iwork1,iwork2,maxnpe,nelem,node,np,npe,rho,s, &
      u(1,icomp),v(1,icomp),xc,yc)

    do i = 1,np
      nflag(i) = nflag0(i).and.nflag1(i)
    end do

    call fsize(nflag,np,s,stmax,stmin,svmax,svmin)

    if ( stmax > stmin ) then

      call setsiz(echo,object(22),smax,smin,stmax,stmin,svmax,svmin)

      call conc6(eflag,etaref,isotri,jcmax,jcmin,maxnpe,ncon, &
        nelem,node,np,npe,s,smax,smin,xc,xsiref,yc)

      if ( lbar ) then
        x1 = x2min
        x2 = x2max
        y1 = y2max
        y2 = y1max
        call cbar(icolor,jcmax,jcmin,maxobj,ncon,smax,smin,srange,x1,x2,y1,y2)
      end if

    else

      write(*,*)' '
      write(*,'(a)')'GRAPH - Warning!'
      write(*,*)'  Could not display '//object(22)
      write(*,*)'  All data values were equal.'

    end if

  end if
!
!  VORTC: draw vorticity color contours.
!  Vorticity is defined on the kinematic velocities.
!
  if ( show(13) ) then

    call fegrad ( dudxn,dudyn,isotri,maxnpe,nelem,node,np,npe,u(1,icomp),xc,yc)

    call fegrad ( dvdxn,dvdyn,isotri,maxnpe,nelem,node,np,npe,v(1,icomp),xc,yc)

    do i = 1,np
      s(i) = dvdxn(i)-dudyn(i)
    end do

    do i = 1,np
      nflag(i) = nflag0(i).and.nflag1(i)
    end do

    call fsize(nflag,np,s,stmax,stmin,svmax,svmin)

    if ( stmax > stmin ) then

      call setsiz(echo,object(13),smax,smin,stmax,stmin,svmax,svmin)

      call conc6(eflag,etaref,isotri,jcmax,jcmin,maxnpe,ncon, &
        nelem,node,np,npe,s,smax,smin,xc,xsiref,yc)

      if ( lbar ) then
        x1 = x2min
        x2 = x2max
        y1 = y2max
        y2 = y1max
        call cbar(icolor,jcmax,jcmin,maxobj,ncon,smax,smin,srange,x1,x2,y1,y2)
      end if

    else

      write(*,*)' '
      write(*,'(a)')'GRAPH - Warning!'
      write(*,*)'  Could not display '//object(13)
      write(*,*)'  All data values were equal.'

    end if

  end if
!
!  XCC: draw X coordinate contours.
!
  if ( show(23) ) then

    do i = 1,np
      nflag(i) = nflag0(i).and.nflag1(i)
      s(i) = xc(i)
    end do

    call fsize(nflag,np,s,stmax,stmin,svmax,svmin)

    if ( stmax > stmin ) then

      call setsiz(echo,object(23),smax,smin,stmax,stmin,svmax,svmin)

      call conc6(eflag,etaref,isotri,jcmax,jcmin,maxnpe,ncon, &
        nelem,node,np,npe,s,smax,smin,xc,xsiref,yc)

      if ( lbar ) then
        x1 = x2min
        x2 = x2max
        y1 = y2max
        y2 = y1max
        call cbar(icolor,jcmax,jcmin,maxobj,ncon,smax,smin,srange,x1,x2,y1,y2)
      end if

    else

      write(*,*)' '
      write(*,'(a)')'GRAPH - Warning!'
      write(*,*)'  Could not display '//object(23)
      write(*,*)'  All data values were equal.'

    end if

  end if
!
!  YCC: draw Y coordinate contours.
!
  if ( show(24) ) then

    do i = 1,np
      nflag(i) = nflag0(i).and.nflag1(i)
      s(i) = yc(i)
    end do

    call fsize(nflag,np,s,stmax,stmin,svmax,svmin)

    if ( stmax > stmin ) then

      call setsiz(echo,object(24),smax,smin,stmax,stmin,svmax,svmin)

      call conc6(eflag,etaref,isotri,jcmax,jcmin,maxnpe,ncon, &
        nelem,node,np,npe,s,smax,smin,xc,xsiref,yc)

      if ( lbar ) then
        x1 = x2min
        x2 = x2max
        y1 = y2max
        y2 = y1max
        call cbar(icolor,jcmax,jcmin,maxobj,ncon,smax,smin,srange,x1,x2,y1,y2)
      end if

    else

      write(*,*)' '
      write(*,'(a)')'GRAPH - Warning!'
      write(*,*)'  Could not display '//object(24)
      write(*,*)'  All data values were equal.'

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
      call linclr(iblack)
    else
      call linclr(icolor(1))
    end if

    nflag(1:np) = nflag1(1:np)

    call boundary_show ( eflag, etaref, isotri, jbound, line, maxbou, maxnpe, &
      maxobj, nbound, nelem, nflag, node, np, npe, xc, xsiref, yc )

  end if
!
!  Draw the profile line.
!
  if ( show(19) ) then
    call linclr(icolor(19))
    dshsiz = 0.02
    nval = 2
    xval(1) = xprof
    yval(1) = 0.0
    xval(2) = xprof
    yval(2) = 3.0
    call dshlin(nval,xval,yval,dshsiz)
  end if
!
!  N: draw the nodes.
!
  if ( show(4) ) then
    call linclr(icolor(4))
    filled = .false.
    csize = 0.005*srange
    do i = 1,np
      if ( nflag0(i).and.nflag1(i).and.nflag2(i) ) then
        call circle(xc(i),yc(i),csize,filled)
      end if
    end do
  end if
!
!  NN: draw the node numbers.
!
  if ( show(27) ) then
    angle = 0.0
    cwide = 0.025*srange*scalen
    pwide = srange
    flush = 'center'
    call linclr(icolor(27))
    do i = 1,np
      if ( nflag0(i).and.nflag1(i).and.nflag2(i) ) then

        x = xc(i)
        y = yc(i)
        write(chrtmp,'(i6)')i
        call flushl(chrtmp)
        lent = len_trim ( chrtmp )

        call s_plot(angle,cwide,pwide,chrtmp(1:lent),x,y,flush)

      end if
    end do

  end if
!
!  E: draw the element boundaries.
!
  if ( show(2) ) then

    call linclr ( icolor(2) )

    call element_line ( eflag, etaref, isotri, maxnpe, nelem, node, np, npe, &
      xc, xsiref, yc )

  end if
!
!  EN: draw the element numbers.
!
  if ( show(28) ) then
    angle = 0.0
    cwide = 0.025*srange*scalee
    pwide = srange
    flush = 'center'
    call linclr(icolor(28))
    do i = 1,nelem

      if ( eflag(i) ) then

        x = (xc(node(1,i))+xc(node(2,i))+xc(node(3,i)))/3.0
        y = (yc(node(1,i))+yc(node(2,i))+yc(node(3,i)))/3.0

        write(chrtmp,'(i6)')i
        call flushl(chrtmp)
        lent = len_trim ( chrtmp )

        call s_plot(angle,cwide,pwide,chrtmp(1:lent),x,y,flush)

      end if

    end do

  end if
!
!  CONTOUR LINES ARE DRAWN HERE.
!
!  CP: draw CP contours.
!
  if ( show(29) ) then

    call linclr(icolor(29))

    do i = 1,np
      nflag(i) = nflag0(i).and.nflag1(i)
      s(i) = cp(i)
    end do

    call fsize(nflag,np,s,stmax,stmin,svmax,svmin)

    if ( stmax > stmin ) then

      call setsiz(echo,object(29),smax,smin,stmax,stmin,svmax,svmin)

      do i = 1,ncon

        scon = ((ncon+1-i)*smin+i*smax)/real(ncon+1)

        if ( show(32) ) then
          jcolor = 1
        else if ( line(5) == 0 ) then
          jcolor = 1
        else
          jcolor = int(((ncon-i)*jcmin+i*jcmax)/real(ncon))
        end if

        call linclr(jcolor)

        if ( npe == 3 ) then

          call conl3(eflag,maxnpe,nelem,node,np,s,scon,xc,yc)

        else if ( npe == 4 ) then

          call conl4(eflag,maxnpe,nelem,node,np,s,scon,xc,yc)

        else if ( npe == 6 ) then

          call conl6(eflag,etaref,isotri,maxnpe,nelem,node,np, &
            npe,s,scon,xc,xsiref,yc)

        end if

      end do

    else

      write(*,*)' '
      write(*,'(a)')'GRAPH - Warning!'
      write(*,*)'  Could not display '//object(5)
      write(*,*)'  All data values were equal to ',smin

    end if

  end if
!
!  KVMAG: draw kinematic velocity magnitude line contours.
!
  if ( show(10) ) then

    call linclr(icolor(10))

    do i = 1,np
      nflag(i) = nflag0(i).and.nflag1(i)
      s(i) = sqrt(u(i,icomp)**2+v(i,icomp)**2)
    end do

    call fsize(nflag,np,s,stmax,stmin,svmax,svmin)

    if ( stmax > stmin ) then

      call setsiz(echo,object(10),smax,smin,stmax,stmin,svmax,svmin)

      do i = 1,ncon

        scon = ((ncon+1-i)*smin+i*smax)/real(ncon+1)

        if ( show(14) ) then
          jcolor = 1
        else if ( line(10) == 0 ) then
          jcolor = 1
        else
          jcolor = int(((ncon-i)*jcmin+i*jcmax)/real(ncon))
        end if

        call linclr(jcolor)

        if ( npe == 3 ) then

          call conl3(eflag,maxnpe,nelem,node,np,s,scon,xc,yc)

        else if ( npe == 4 ) then

          call conl4(eflag,maxnpe,nelem,node,np,s,scon,xc,yc)

        else if ( npe == 6 ) then

          call conl6(eflag,etaref,isotri,maxnpe,nelem,node, &
            np,npe,s,scon,xc,xsiref,yc)

        end if

      end do

    else

      write(*,*)' '
      write(*,'(a)')'GRAPH - Warning!'
      write(*,*)'  Could not display '//object(10)
      write(*,*)'  All data values were equal to ',smin

    end if

  end if
!
!  KVX: draw X kinematic velocity line contours.
!
  if ( show(15) ) then

    call linclr(icolor(15))

    do i = 1,np
      nflag(i) = nflag0(i).and.nflag1(i)
      s(i) = u(i,icomp)
    end do

    call fsize(nflag,np,s,stmax,stmin,svmax,svmin)

    if ( stmax > stmin ) then

      call setsiz(echo,object(15),smax,smin,stmax,stmin,svmax,svmin)

      do i = 1,ncon

        scon = ((ncon+1-i)*smin+i*smax)/real(ncon+1)

        if ( show(17) ) then
          jcolor = 1
        else if ( line(15) == 0 ) then
          jcolor = 1
        else
          jcolor = int(((ncon-i)*jcmin+i*jcmax)/real(ncon))
        end if

        call linclr(jcolor)

        if ( npe == 3 ) then

          call conl3(eflag,maxnpe,nelem,node,np,s,scon,xc,yc)

        else if ( npe == 4 ) then

          call conl4(eflag,maxnpe,nelem,node,np,s,scon,xc,yc)

        else if ( npe == 6 ) then

          call conl6(eflag,etaref,isotri,maxnpe,nelem,node, &
            np,npe,s,scon,xc,xsiref,yc)

        end if

      end do

    else

      write(*,*)' '
      write(*,'(a)')'GRAPH - Warning!'
      write(*,*)'  Could not display '//object(15)
      write(*,*)'  All data values were equal to ',smin

    end if

  end if
!
!  KVY: draw Y kinematic velocity line contours.
!
  if ( show(16) ) then

    call linclr(icolor(16))

    do i = 1,np
      nflag(i) = nflag0(i).and.nflag1(i)
      s(i) = v(i,icomp)
    end do

    call fsize(nflag,np,s,stmax,stmin,svmax,svmin)

    if ( stmax > stmin ) then

      call setsiz(echo,object(16),smax,smin,stmax,stmin,svmax,svmin)

      do i = 1,ncon

        scon = ((ncon+1-i)*smin+i*smax)/real(ncon+1)

        if ( show(18) ) then
          jcolor = 1
        else if ( line(16) == 0 ) then
          jcolor = 1
        else
          jcolor = int(((ncon-i)*jcmin+i*jcmax)/real(ncon))
        end if

        call linclr(jcolor)

        if ( npe == 3 ) then

          call conl3(eflag,maxnpe,nelem,node,np,s,scon,xc,yc)

        else if ( npe == 4 ) then

          call conl4(eflag,maxnpe,nelem,node,np,s,scon,xc,yc)

        else if ( npe == 6 ) then

          call conl6(eflag,etaref,isotri,maxnpe,nelem,node, &
            np,npe,s,scon,xc,xsiref,yc)

        end if

      end do

    else

      write(*,*)' '
      write(*,'(a)')'GRAPH - Warning!'
      write(*,*)'  Could not display '//object(16)
      write(*,*)'  All data values were equal to ',smin

    end if

  end if
!
!  MACH: draw Mach contours.
!
  if ( show(30) ) then

    call linclr(icolor(30))

    do i = 1,np
      nflag(i) = nflag0(i).and.nflag1(i)
      s(i) = rmach(i)
    end do

    call fsize(nflag,np,s,stmax,stmin,svmax,svmin)

    if ( stmax > stmin ) then

      call setsiz(echo,object(30),smax,smin,stmax,stmin,svmax,svmin)

      do i = 1,ncon

        scon = ((ncon+1-i)*smin+i*smax)/real(ncon+1)

        if ( show(33) ) then
          jcolor = 1
        else if ( line(5) == 0 ) then
          jcolor = 1
        else
          jcolor = int(((ncon-i)*jcmin+i*jcmax)/real(ncon))
        end if

        call linclr(jcolor)

        if ( npe == 3 ) then

          call conl3(eflag,maxnpe,nelem,node,np,s,scon,xc,yc)

        else if ( npe == 4 ) then

          call conl4(eflag,maxnpe,nelem,node,np,s,scon,xc,yc)

        else if ( npe == 6 ) then

          call conl6(eflag,etaref,isotri,maxnpe,nelem,node,np, &
            npe,s,scon,xc,xsiref,yc)

        end if

      end do

    else

      write(*,*)' '
      write(*,'(a)')'GRAPH - Warning!'
      write(*,*)'  Could not display '//object(5)
      write(*,*)'  All data values were equal to ',smin

    end if

  end if
!
!  MVMAG: draw mass velocity magnitude line contours.
!
  if ( show(36) ) then

    call linclr(icolor(36))

    do i = 1,np
      nflag(i) = nflag0(i).and.nflag1(i)
      s(i) = rho(i)*sqrt(u(i,icomp)**2+v(i,icomp)**2)
    end do

    call fsize(nflag,np,s,stmax,stmin,svmax,svmin)

    if ( stmax > stmin ) then

      call setsiz(echo,object(36),smax,smin,stmax,stmin,svmax,svmin)

      do i = 1,ncon

        scon = ((ncon+1-i)*smin+i*smax)/real(ncon+1)

        if ( show(37) ) then
          jcolor = 1
        else if ( line(36) == 0 ) then
          jcolor = 1
        else
          jcolor = int(((ncon-i)*jcmin+i*jcmax)/real(ncon))
        end if

        call linclr(jcolor)

        if ( npe == 3 ) then

          call conl3(eflag,maxnpe,nelem,node,np,s,scon,xc,yc)

        else if ( npe == 4 ) then

          call conl4(eflag,maxnpe,nelem,node,np,s,scon,xc,yc)

        else if ( npe == 6 ) then

          call conl6(eflag,etaref,isotri,maxnpe,nelem,node, &
            np,npe,s,scon,xc,xsiref,yc)

        end if

      end do

    else

      write(*,*)' '
      write(*,'(a)')'GRAPH - Warning!'
      write(*,*)'  Could not display '//object(36)
      write(*,*)'  All data values were equal to ',smin

    end if

  end if
!
!  P: draw pressure contours.
!  There is a slight complication, because pressure on quadratic elements
!  is often actually linear.
!
  if ( show(5) ) then

    call linclr(icolor(5))

    if ( npe == 6 ) then
      write(*,*)' '
      write(*,*)'GRAPH - Question:'
      write(*,*)'  Use linear elements to graph pressure?'
      read(*,'(a)')isay
      write(17,'(a)')isay
      if ( echo ) then
        write(*,'(a)')isay
      end if
    end if

    do i = 1,np
      nflag(i) = nflag0(i).and.nflag1(i)
      s(i) = p(i,icomp)
    end do

    call fsize(nflag,np,s,stmax,stmin,svmax,svmin)

    if ( stmax > stmin ) then

      call setsiz(echo,object(5),smax,smin,stmax,stmin,svmax,svmin)

      do i = 1,ncon

        scon = ((ncon+1-i)*smin+i*smax)/real(ncon+1)

        if ( show(12) ) then
          jcolor = 1
        else if ( line(5) == 0 ) then
          jcolor = 1
        else
          jcolor = int(((ncon-i)*jcmin+i*jcmax)/real(ncon))
        end if

        call linclr(jcolor)

        if ( npe == 3 ) then

          call conl3(eflag,maxnpe,nelem,node,np,s,scon,xc,yc)

        else if ( npe == 4 ) then

          call conl4(eflag,maxnpe,nelem,node,np,s,scon,xc,yc)

        else if ( npe == 6 ) then
 
          if ( s_eqi ( isay,'n') ) then

            call conl6(eflag,etaref,isotri,maxnpe,nelem,node,np, &
              npe,s,scon,xc,xsiref,yc)

          else

            call conl63(eflag,etaref,isotri,maxnpe,nelem, &
              node,np,npe,s,scon,xc,xsiref,yc)

          end if

        end if

      end do

    else

      write(*,*)' '
      write(*,'(a)')'GRAPH - Warning!'
      write(*,*)'  Could not display '//object(5)
      write(*,*)'  All data values were equal to ',smin

    end if

  end if
!
!  RHO: draw RHO contours.
!
  if ( show(31) ) then

    call linclr(icolor(31))

    do i = 1,np
      nflag(i) = nflag0(i).and.nflag1(i)
      s(i) = rho(i)
    end do

    call fsize(nflag,np,s,stmax,stmin,svmax,svmin)

    if ( stmax > stmin ) then

      call setsiz(echo,object(31),smax,smin,stmax,stmin,svmax,svmin)

      do i = 1,ncon

        scon = ((ncon+1-i)*smin+i*smax)/real(ncon+1)

        if ( show(34) ) then
          jcolor = 1
        else if ( line(5) == 0 ) then
          jcolor = 1
        else
          jcolor = int(((ncon-i)*jcmin+i*jcmax)/real(ncon))
        end if

        call linclr(jcolor)

        if ( npe == 3 ) then

          call conl3(eflag,maxnpe,nelem,node,np,s,scon,xc,yc)

        else if ( npe == 4 ) then

          call conl4(eflag,maxnpe,nelem,node,np,s,scon,xc,yc)

        else if ( npe == 6 ) then

          call conl6(eflag,etaref,isotri,maxnpe,nelem,node,np, &
            npe,s,scon,xc,xsiref,yc)

        end if

      end do

    else

      write(*,*)' '
      write(*,'(a)')'GRAPH - Warning!'
      write(*,*)'  Could not display '//object(5)
      write(*,*)'  All data values were equal to ',smin

    end if

  end if
!
!  S: draw stream lines.
!
  if ( show(6) ) then

    call linclr(icolor(6))

    call stream(iwork1,iwork2,maxnpe,nelem,node,np,npe,rho,s, &
      u(1,icomp),v(1,icomp),xc,yc)

    do i = 1,np
      nflag(i) = nflag0(i).and.nflag1(i)
    end do

    call fsize(nflag,np,s,stmax,stmin,svmax,svmin)

    if ( stmax > stmin ) then

      call setsiz(echo,object(6),smax,smin,stmax,stmin,svmax,svmin)

      do i = 1,ncon

        scon = ((ncon+1-i)*smin+i*smax)/real(ncon+1)

        if ( show(22) ) then
          jcolor = 1
        else if ( line(6) == 0 ) then
          jcolor = 1
        else
          jcolor = int(((ncon-i)*jcmin+i*jcmax)/real(ncon))
        end if

        call linclr(jcolor)

        if ( npe == 3 ) then

          call conl3(eflag,maxnpe,nelem,node,np,s,scon,xc,yc)

        else if ( npe == 4 ) then

          call conl4(eflag,maxnpe,nelem,node,np,s,scon,xc,yc)

        else if ( npe == 6 ) then

          call conl6(eflag,etaref,isotri,maxnpe,nelem,node, &
            np,npe,s,scon,xc,xsiref,yc)

        end if

      end do

    else

      write(*,*)' '
      write(*,'(a)')'GRAPH - Warning!'
      write(*,*)'  Could not display '//object(6)
      write(*,*)'  All data values were equal to ',smin

    end if

  end if

!
!  VORT: draw vorticity contours.
!
  if ( show(11) ) then

    call linclr(icolor(11))

    call fegrad(dudxn,dudyn,isotri,maxnpe,nelem,node,np,npe,u(1,icomp),xc,yc)

    call fegrad(dvdxn,dvdyn,isotri,maxnpe,nelem,node,np,npe,v(1,icomp),xc,yc)

    do i = 1,np
      s(i) = dvdxn(i)-dudyn(i)
    end do

    do i = 1,np
      nflag(i) = nflag0(i).and.nflag1(i)
    end do

    call fsize(nflag,np,s,stmax,stmin,svmax,svmin)

    if ( stmax > stmin ) then

      call setsiz(echo,object(11),smax,smin,stmax,stmin,svmax,svmin)

      do i = 1,ncon

        scon = ((ncon+1-i)*smin+i*smax)/real(ncon+1)

        if ( show(13) ) then
          jcolor = 1
        else if ( line(11) == 0 ) then
          jcolor = 1
        else
          jcolor = int(((ncon-i)*jcmin+i*jcmax)/real(ncon))
        end if

        call linclr(jcolor)

        if ( npe == 3 ) then

          call conl3(eflag,maxnpe,nelem,node,np,s,scon,xc,yc)

        else if ( npe == 4 ) then

          call conl4(eflag,maxnpe,nelem,node,np,s,scon,xc,yc)

        else if ( npe == 6 ) then

          call conl6(eflag,etaref,isotri,maxnpe,nelem,node, &
            np,npe,s,scon,xc,xsiref,yc)

        end if

      end do

    else

      write(*,*)' '
      write(*,*)'GRAPH - Warning!'
      write(*,'(a)')'  Could not display '//object(11)
      write(*,*)'  All data values were equal to ',smin

    end if

  end if
!
!  XC: draw X coordinate contours.
!
  if ( show(25) ) then

    call linclr(icolor(25))

    do i = 1,np
      nflag(i) = nflag0(i).and.nflag1(i)
      s(i) = xc(i)
    end do

    call fsize(nflag,np,s,stmax,stmin,svmax,svmin)

    if ( stmax > stmin ) then

      call setsiz(echo,object(25),smax,smin,stmax,stmin,svmax,svmin)

      do i = 1,ncon

        scon = ((ncon+1-i)*smin+i*smax)/real(ncon+1)

        if ( show(17) ) then
          jcolor = 1
        else if ( line(25) == 0 ) then
          jcolor = 1
        else
          jcolor = int(((ncon-i)*jcmin+i*jcmax)/real(ncon))
        end if

        call linclr(jcolor)

        if ( npe == 3 ) then

          call conl3(eflag,maxnpe,nelem,node,np,s,scon,xc,yc)

        else if ( npe == 4 ) then

          call conl4(eflag,maxnpe,nelem,node,np,s,scon,xc,yc)

        else if ( npe == 6 ) then

          call conl6(eflag,etaref,isotri,maxnpe,nelem,node, &
            np,npe,s,scon,xc,xsiref,yc)

        end if

      end do

    else

      write(*,*)' '
      write(*,'(a)')'GRAPH - Warning!'
      write(*,*)'  Could not display '//object(25)
      write(*,*)'  All data values were equal to ',smin

    end if

  end if
!
!  YC: draw Y coordinate contours.
!
  if ( show(26) ) then

    call linclr(icolor(26))

    do i = 1,np
      nflag(i) = nflag0(i).and.nflag1(i)
      s(i) = yc(i)
    end do

    call fsize(nflag,np,s,stmax,stmin,svmax,svmin)

    if ( stmax > stmin ) then

      call setsiz(echo,object(26),smax,smin,stmax,stmin,svmax,svmin)

      do i = 1,ncon

        scon = ((ncon+1-i)*smin+i*smax)/real(ncon+1)

        if ( show(17) ) then
          jcolor = 1
        else if ( line(26) == 0 ) then
          jcolor = 1
        else
          jcolor = int(((ncon-i)*jcmin+i*jcmax)/real(ncon))
        end if

        call linclr(jcolor)

        if ( npe == 3 ) then

          call conl3(eflag,maxnpe,nelem,node,np,s,scon,xc,yc)

        else if ( npe == 4 ) then

          call conl4(eflag,maxnpe,nelem,node,np,s,scon,xc,yc)

        else if ( npe == 6 ) then

          call conl6(eflag,etaref,isotri,maxnpe,nelem,node, &
            np,npe,s,scon,xc,xsiref,yc)

        end if

      end do

    else

      write(*,*)' '
      write(*,'(a)')'GRAPH - Warning!'
      write(*,*)'  Could not display '//object(26)
      write(*,*)'  All data values were equal ',smin

    end if

  end if

!
!  VECTOR PLOTS BEGIN HERE
!

!
!  KV: draw kinematic velocity vectors.
!
  if ( show(8) ) then

    call linclr(icolor(8))

    write ( *, * ) ' '
    write ( *, * ) 'GRAPH - Note:'
    write ( *, '(a,i3)' ) '  The line color was set to ', icolor(8)

    do i = 1, np
      nflag(i) = nflag0(i) .and. nflag1(i) .and. nflag2(i)
    end do

    do i = 1, np
      s(i) = u(i,icomp)
      s2(i) = v(i,icomp)
    end do

    call vsize ( nflag, np, nxskip, ny, nyskip, s, s2, vtmax, vtmin, &
      vvmax, vvmin )

    if ( vtmax /= 0.0 ) then

      write(*,*)' '
      write(*,*)'GRAPH - Note:'
      write(*,*)'  Norms of all kinematic velocity range from ', &
        vtmin,' to ',vtmax
      write(*,*)'  Norms of visible kinematic velocity range from ', &
        vvmin,' to ',vvmax

      vecscl = scalev*0.5*min(nxskip*delx,nyskip*dely)/vtmax

      if ( line(8) == 0 ) then
        ido = 0
      else
        ido = 1
      end if

      ido = 0

      if ( s_eqi ( arrow,'solid') ) then
        jcolor = icolor(8)
        call filclr(jcolor)
      else if ( s_eqi ( arrow,'hollow') ) then
        jcolor = 0
        call filclr(jcolor)
        jcolor = 1
        call linclr(jcolor)
      end if

      call vector(arrow,ido,jcmax,jcmin,ncon,nflag,np,vecscl,s,s2,vtmax, &
        vtmin,xc,yc)

      if ( lbar.and.line(8) /= 0 ) then

        x1 = x2min
        x2 = x2max
        y1 = y2max
        y2 = y1max

        call cbar(icolor,jcmax,jcmin,maxobj,ncon,vtmax,vtmin,srange,x1,x2,y1,y2)
      end if

    else

      write(*,*)' '
      write(*,*)'GRAPH - Warning!'
      write(*,*)'  Could not display '//object(8)
      write(*,*)'  All values were ',vtmax

    end if

  end if
!
!  MV: draw mass velocity vectors.
!
  if ( show(35) ) then

    call linclr(icolor(35))

    write(*,*)' '
    write(*,*)'GRAPH - Note:'
    write(*,*)'  The line color was set to ',icolor(35)

    do i = 1,np
      nflag(i) = nflag0(i).and.nflag1(i).and.nflag2(i)
    end do

    do i = 1,np
      s(i) = rho(i)*u(i,icomp)
      s2(i) = rho(i)*v(i,icomp)
    end do

    call vsize(nflag,np,nxskip,ny,nyskip,s,s2,vtmax,vtmin,vvmax,vvmin)

    if ( vtmax /= 0.0 ) then

      write(*,*)' '
      write(*,*)'GRAPH - Note:'
      write(*,*)'  Norms of all mass velocity range from ',vtmin,' to ',vtmax
      write(*,*)'  Norms of visible mass velocity range from ', &
        vvmin,' to ',vvmax

      vecscl = scalev*0.5*min(nxskip*delx,nyskip*dely)/vtmax

      if ( line(35) == 0 ) then
        ido = 0
      else
        ido = 1
      end if

      ido = 0

      call vector ( arrow,ido,jcmax,jcmin,ncon,nflag,np,vecscl, &
        s,s2,vtmax,vtmin,xc,yc)

      if ( lbar.and.line(35) /= 0 ) then

        x1 = x2min
        x2 = x2max
        y1 = y2max
        y2 = y1max

        call cbar(icolor,jcmax,jcmin,maxobj,ncon,vtmax,vtmin,srange, &
          x1,x2,y1,y2)
      end if

    else

      write(*,*)' '
      write(*,*)'GRAPH - Warning!'
      write(*,*)'  Could not display '//object(35)
      write(*,*)'  All values were ',vtmax

    end if

  end if
!
!  UV: draw velocity direction vectors.
!
  if ( show(9) ) then

    call linclr(icolor(9))

    nonzer = 0

    do i = 1,np
      nflag(i) = nflag0(i).and.nflag1(i).and.nflag2(i)
      uvmag = sqrt(u(i,icomp)**2+v(i,icomp)**2)
      if ( uvmag /= 0.0 ) then
        nonzer = nonzer+1
        s(i) = u(i,icomp)/uvmag
        t(i) = v(i,icomp)/uvmag
      else
        s(i) = 0.0
        t(i) = 0.0
      end if

    end do

    call vsize(nflag,np,nxskip,ny,nyskip,u(1,icomp),v(1,icomp), &
      vtmax,vtmin,vvmax,vvmin)

    if ( nonzer > 0 ) then

      vecscl = scalev*0.5*min(nyskip*delx,nxskip*dely)

      if ( line(9) == 0 ) then
        ido = 0
      else
        ido = 1
      end if

      ido = 0

      call vector ( arrow,ido,jcmax,jcmin,ncon,nflag,np,vecscl, &
        s,t,vtmax,vtmin,xc,yc)

      if ( lbar.and.line(9) /= 0 ) then
        x1 = x2min
        x2 = x2max
        y1 = y2max
        y2 = y1max
        call cbar(icolor,jcmax,jcmin,maxobj,ncon,vtmax,vtmin,srange, &
          x1,x2,y1,y2)
      end if

    else

      write(*,*)' '
      write(*,'(a)')'GRAPH - Warning!'
      write(*,*)'  Could not display '//object(9)
      write(*,*)'  All data values were zero.'

    end if

  end if
!
!  Pause, if we are doing X-Windows.
!
  call buzz ( dev, x1min, x1max, y1min, y1max )

  return
end
subroutine hello ( maxbou, maxelm, maxnp, maxnpe, maxnx, maxny, maxobj, &
  maxpar, maxsen )
!
!***********************************************************************
!
!! HELLO prints out the program name, date, and purpose.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) MAXBOU, the amount of storage available for the IBOUND array.
! 
!    Input, integer ( kind = 4 ) MAXELM, the maximum number of elements.
!    MAXELM is derived from other maxima:
!
!      MAXELM  =  2 * (MAXNX-1) * (MAXNY-1)
! 
!    Input, integer ( kind = 4 ) MAXNP, the maximum number of nodes.
! 
!      MAXNP  =  (2 * MAXNX - 1) * (2 * MAXNY - 1)
!
!    Input, integer ( kind = 4 ) MAXNPE, the maximum number of nodes per element, that is,
!    the maximum legal value for NPE.  
!  
!    Currently, MAXNPE is set to 6, and the program expects NPE
!    to be 3 or 6.
! 
!    Input, integer ( kind = 4 ) MAXNX, the maximum value allowed for NX, the number of nodes
!    in the X direction.
!
!    Input, integer ( kind = 4 ) MAXNY, the maximum value allowed for NY, the number of nodes
!    in the Y direction.
!
!    Input, integer ( kind = 4 ) MAXOBJ, the number of graphical "objects".
! 
!    Input, integer ( kind = 4 ) MAXPAR, the maximum number of parameters.
! 
  integer ( kind = 4 ) maxbou
  integer ( kind = 4 ) maxelm
  integer ( kind = 4 ) maxnp
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) maxnx
  integer ( kind = 4 ) maxny
  integer ( kind = 4 ) maxobj
  integer ( kind = 4 ) maxpar
  integer ( kind = 4 ) maxsen
!
  write(*,*)' '
  write(*,*)'HELLO!'
  write(*,*)' '
  write(*,*)'  DISPLAY was last modified on 12 January 2001.'
  write(*,*)' '
  write(*,*)'  DISPLAY produces X Window, PostScript or CGM'
  write(*,*)'  graphics output from the data of the 2D finite '
  write(*,*)'  element programs FLOW and ARBY.'
  write(*,*)' '
  write(*,*)'  Scalars are displayed using contour lines or colors.'
  write(*,*)' '
  write(*,*)'  Vectors are displayed as scaled or unit vectors.'
  write(*,*)' '
  write(*,*)'  Line or color contours of stream function,'
  write(*,*)'  magnitude, X and Y components, and so on, '
  write(*,*)'  are also available.'
  write(*,*)' '
  write(*,*)' '
  write(*,*)'  DISPLAY''s built-in maxima:'
  write(*,*)' '
  write(*,*)'    Number of boundary edges,    MAXBOU = ',maxbou
  write(*,*)'    Number of nodes per element, MAXNPE = ',maxnpe
  write(*,*)'    Number of X nodes,           MAXNX =  ',maxnx
  write(*,*)'    Number of Y nodes,           MAXNY =  ',maxny
  write(*,*)'    Number of "objects",         MAXOBJ = ',maxobj
  write(*,*)'    Number of parameters,        MAXPAR = ',maxpar
  write(*,*)'    Number of sensitivities,     MAXSEN = ',maxsen
  write(*,*)' '
  write(*,*)'  DISPLAY''s derived maxima:'
  write(*,*)' '
  write(*,*)'    Number of elements,          MAXELM = ',maxelm
  write(*,*)'      MAXELM  =  2 * (MAXNX-1) * (MAXNY-1)'
  write(*,*)' '
  write(*,*)'    Number of nodes,             MAXNP =  ',maxnp
  write(*,*)'      MAXNP  =  (2 * MAXNX - 1) * (2 * MAXNY - 1)'
  write(*,*)' '
 
  return
end
subroutine help ( echo )
!
!***********************************************************************
!
!! HELP prints out a list of commands.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
  logical echo
  character isay
!
  write(*,*)' '
  write(*,*)'Commands:'
  write(*,*)' '
  write(*,*)'TH MH BH  top, middle, bottom halves.'
  write(*,*)'LH CH RH  left, center, right halves.'
  write(*,*)' '
  write(*,*)'TL TC TR  top left, center, right quarters.'
  write(*,*)'ML MC MR  middle left, center, right quarters.'
  write(*,*)'BL BC BR  bottom left, center, right quarters.'
  write(*,*)'X         choose arbitrary X, Y subwindow.'
  write(*,*)' '
  write(*,*)'FULL      return to full picture.'
  write(*,*)' '
  write(*,*)'Echo      echo user commands to output file.'
  write(*,*)'EX        set up example.'
  write(*,*)'PROF      show profile line.'
  write(*,*)' '
  write(*,*)'VE        set visible elements.'
  write(*,*)'VN        set visible nodes by index.'
  write(*,*)'VND       set visible nodes by distance.'
  write(*,*)' '
  write(*,*)'PROGRAF   Show a profile plot.'
  write(*,*)'PPRO      show pressure in profiles.'
  write(*,*)'PTPRO     show target pressure in profiles.'
  write(*,*)'UPRO      show horizontal velocity in profiles.'
  write(*,*)'UTPRO     show target horizontal velocity in profiles.'
  write(*,*)'VPRO      show vertical velocity in profiles.'
  write(*,*)'VTPRO     show target vertical velocity in profiles.'

  write(*,*)' '
  write(*,*)' '
  write(*,*)'RETURN for more..., Q to quit'
  read(*,'(a1)',err = 10,end=10)isay
  write(17,'(1x,a1)')isay
  if ( echo ) then
    write(*,'(a1)')isay
  end if
  if ( isay /= ' ')return

  write(*,*)'B             show boundary.'
  write(*,*)'BACK          show plot background.'
  write(*,*)'BAR           show color bar.'
  write(*,*)'E,    EC      show elements or element colors.'
  write(*,*)'EN            show element numbers.'
  write(*,*)'FRAME         show frame.'
  write(*,*)'KV            kinematic velocity vectors.'
  write(*,*)'KVMAG, KVMAGC line/color kinematic velocity magnitude contours.'
  write(*,*)'KVX,    KVXC  line/color horizontal velocity contours.'
  write(*,*)'KVY,    KVYC  line/color vertical velocity contours.'
  write(*,*)'MV            mass velocity vectors.'
  write(*,*)'MVMAG, MVMAGC line/color mass velocity magnitude contours.'
  write(*,*)'N             show nodes.'
  write(*,*)'NN            show node numbers.'
  write(*,*)'NOFRAME       Don''t show a frame.'
  write(*,*)'P,     PC     line/color pressure contours.'
  write(*,*)'S,     SC     stream line/color contours.'
  write(*,*)'UV            unit velocity vectors.'

  write(*,*)'VORT,  VORTC  line/color vorticity contours.'
  write(*,*)'XC,    XCC    line/color X coordinate.'
  write(*,*)'YC,    YCC    line/color Y coordinate.'
  write(*,*)' '
  write(*,*)'RETURN for more..., Q to quit'

  read(*,'(a1)',err = 10,end=10)isay
  write(17,'(1x,a1)')isay
  if ( echo ) then
    write(*,'(a)')isay
  end if
  if ( isay /= ' ')return

  write(*,*)'ARROW =    HOLLOW, LINE or SOLID.'
  write(*,*)'CC =       Choose a color table.'
  write(*,*)'DAT =      Specify the FLOW input data file.'
  write(*,*)'DJE =      Read JEFF element file.'
  write(*,*)'DJN =      Read JEFF node file.'
  write(*,*)'DEV =      Choose plotting output.'
  write(*,*)'DTEC =     Read TECPLOT data file.'
  write(*,*)'FILE =     Name the graphics output file.'
  write(*,*)'FILTYP =   Specify input type (FLOW/JEFF/TECPLOT).'
  write(*,*)'GRACE =    Set grace margin.'
  write(*,*)'ICMAX =    Set maximum available color.'
  write(*,*)'ICMIN =    Set minimum available color.'
  write(*,*)'JCMAX =    Set maximum used color.'
  write(*,*)'JCMIN =    Set minimum used color.'
  write(*,*)'ICOMP =    Set sensitivity component.'
  write(*,*)'IRFORM =   Set input file format.'
  write(*,*)'IWRITE =   Set debugging output level.'
  write(*,*)'LINE     Set line type of any object.'
  write(*,*)'NCON =     Set number of contour levels.'
  write(*,*)'NXSKIP =   Set skip value for column nodes.'
  write(*,*)'NYSKIP =   Set skip value for row nodes.'
  write(*,*)'SCALEE =   Set a scale factor for element numbers.'
  write(*,*)'SCALEN =   Set a scale factor for node numbers.'
  write(*,*)'SCALEV =   Set velocity scale factor.'
  write(*,*)'SMAX =     Set contour maximum value.'
  write(*,*)'SMIN =     Set contour minimum value.'
  write(*,*)'TITLE =    Set plot title.'
  write(*,*)'TITLE2 =   Set plot subtitle.'
  write(*,*)'XPROF =    Set X coordinate of profile.'

  write(*,*)' '
  write(*,*)'RETURN for more..., Q to quit'
  read(*,'(a1)',err = 10,end=10)isay
  write(17,'(1x,a1)')isay
  if ( echo ) then
    write(*,'(a)')isay
  end if
  if ( isay /= ' ')return

  write(*,*)' '
  write(*,*)'A        advance to a particular step.'
  write(*,*)'C        choose colors.'
  write(*,*)'COLOR    choose one color index.'
  write(*,*)'CTAB     show current color table.'
  write(*,*)'DB       print U,V,P at visible nodes.'
  write(*,*)'Help     help (print this list)'
  write(*,*)'HELLO    print program data, version, maxima.'
  write(*,*)'INIT     initialize all data to zero.'
  write(*,*)'LIST     list current values.'
  write(*,*)'OVERLAY  overlay next plots.'
  write(*,*)' '
  write(*,*)' '
  write(*,*)'#        Begin a comment line.'
  write(*,*)'G        go!  create current graph.'
  write(*,*)'Q        quit.'
  write(*,*)'QY       quit NOW.'
  write(*,*)' '

10    continue

  return
end
subroutine init ( arrow,cp,delx,dely,dev,dudxn,dudyn,dvdxn,dvdyn,echo,eflag, &
  eflagu,eqn,etaref,fildat,filgrf,filinp,filtyp,grace,icmax,icmin,icolor, &
  icomp,idata,ifile,iplot,iset,itable,iwrite,jcmax,jcmin,labelx,labely,lbar, &
  line,lppro,lptpro,lupro,lutpro,lvpro,lvtpro,maxelm,maxnp,maxnpe,maxny, &
  maxobj,maxpar,maxsen,nbound,ncon,nelem,nflag,nflag0,node, &
  np,npe,nprof,nsen,nset,nxskip,ny,nyskip,object,ovrlay,p,rho,rmach,scalee, &
  scalen,scalev,show,smax,smin,title,title2,u,v,x1max,x1min, &
  x2max,x2min,x4max,x4min,xc,xprof,xsiref,xsmax,xsmin,y1max, &
  y1min,y2max,y2min,y4max,y4min,yc,ysmax,ysmin)
!
!***********************************************************************
!
!! INIT initializes the values of data.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 January 2001
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
!  DEV    Output, character*10 DEV.
!         The graphics output device to be used.  Current legal
!         values for DEV include:
! 
!         cgmb - CGM binary file.
!         ps   - PostScript file.
!         xws  - X window screen (interactive).
!
!    Output, logical EFLAG(MAXELM).
!         EFLAG is used to "flag" which elements are to be displayed.
!         If EFLAG(I) is TRUE, then element I should be displayed.
! 
!  EFLAGU Output, logical EFLAGU(NELEM).
!         EFLAGU is used to "flag" which elements the user wants to see.
!         If EFLAGU(I) is TRUE, then element I should be displayed.
! 
!    Output, character ( len = 2 )  EQN(3,NP).
!         EQN records the "type" of each equation that will be generated, and
!         which is associated with an unknown.  Note that most boundary 
!         conditions do not result in an equation.  The current values are:
!
!         'U'  The horizontal momentum equation.
!         'UW' The condition U = 0 applied at a node on a fixed wall.
!         'V'  The vertical momentum equation.
!         'VW' The condition V = 0 applied at a node on a fixed wall.
!         'P'  The continuity equation.
!         'PB' The condition P = 0 applied at (XMAX,YMAX).
!
!  ETAREF Output, real ETAREF(MAXNPE).
!         The ETA coordinates of the nodes of the reference
!         triangle.
! 
!  FILDAT Output, character ( len = 80 ) FILDAT.
!         The name of the data file to be read in, which contains
!         the information defining the mesh and the physical
!         parameters.
! 
!  FILGRF Output, character ( len = 80 ) FILGRF, the name of the output
!         graphics file.
!
!  FILINP Output, character ( len = 80 ) FILINP.
!         The name of a file in which will be placed a copy of the
!         input typed by the user while running Display.
!
!  GRACE  Output, real GRACE.
!         The size of the "grace" margin on the plot.
! 
!  ICMAX,
!  ICMIN  Output, integer ( kind = 4 ) ICMAX, ICMIN, the maximum and 
!         minimum color indices to use in the color bar.
!
!  ICOLOR Output, integer ( kind = 4 ) ICOLOR(MAXOBJ).
!         Contains the color indexes for each object.
!         However, in some cases, ICOLOR is actual a color table
!         index.
! 
!  ICOMP  Output, integer ( kind = 4 ) ICOMP.
!         The component of the flow to be studied.
! 
!         ICOMP = 0 studies the basic flow.
!         ICOMP = I studies the sensitivity with respect to
!                 parameter I.
! 
!  IDATA  Output, integer ( kind = 4 ) IDATA.
!         0, no problem has been defined.
!         nonzero, a problem has been defined.
!
!  IFILE  Output, integer ( kind = 4 ) IFILE.
!         Records the status of the data file whose name is FILDAT.
!  
!         -2, an error occurred while reading from the file.
!         -1, the file could not be opened.
!          0, no file is currently open.
!          1, a file has been opened, but not read from.
!          2, data has been read from a file.
! 
!  IPLOT  Output, integer ( kind = 4 ) IPLOT.
!         The number of plots made so far.
! 
!  ISET   Output, integer ( kind = 4 ) ISET.
!         The data set being examined from the file.  If no file
!         is open, or if no data set has been read, then ISET is
!         zero.
! 
!  ITABLE Output, integer ( kind = 4 ) ITABLE, the desired color table.
!  
!         1: low black to high white
!         2: low blue to high yellow
!         3: low red, high blue, with bands between.
!         4: low red, yellow, green, blue, high white.
!         5: low white, blue, green, yellow, high red.
!         6: low blue to high red
!         7: linear table between 2 user colors.
!         8: linear table between N user colors.
!         9: low white to high black.
! 
!  IWRITE Output, integer ( kind = 4 ) IWRITE.
!         Controls debugging output.
!  
!         0 means no such output.
!         1 means some.
!         2 means a lot.
! 
!  JCMAX,
!  JCMIN  Output, integer ( kind = 4 ) JCMAX, JCMIN, the maximum and minimum color
!         indices to use for contours.
!
!  LABELX,
!  LABELY Output, character ( len = 30 ) LABELX, LABELY.  For profile plots, these 
!         contain labels for the X and Y axes.
!
!  LBAR   Output, logical LBAR, .TRUE. if the color bar may be shown,
!         .FALSE. if it should not be shown.
!
!  LINE   Output, integer ( kind = 4 ) LINE(MAXOBJ).
!         LINE allows the user to specify or change the line type
!         for each object.  Only some objects actually have a line
!         type, and the choices for a line type depend on the object.
!
!         Here is the list:
!
!           0, solid black lines.
!           1, dashed black lines.
!           2, solid lines of current color.
!           3, dashed lines of current color.
!
!  LPPRO  Output, logical LPPRO.
!         If TRUE, then the computed pressure should be displayed
!         in profile plots.
!
!  LPTPRO Output, logical LPTPRO.
!         If TRUE, then the target pressure should be displayed
!         in profile plots.
!
!  LUPRO  Output, logical LUPRO.
!         If TRUE, then the computed horizontal velocity should be displayed
!         in profile plots.
!
!  LUTPRO Output, logical LUTPRO.
!         If TRUE, then the target horizontal velocity should be displayed
!         in profile plots.
!
!  LVPRO  Output, logical LVPRO.
!         If TRUE, then the computed vertical velocity should be displayed
!         in profile plots.
!
!  LVTPRO Output, logical LVTPRO.
!         If TRUE, then the target vertical velocity should be displayed
!         in profile plots.
!
!  MAXELM Input, integer ( kind = 4 ) MAXELM.
!         The maximum number of elements which the program can
!         handle.
! 
!  MAXNP  Input, integer ( kind = 4 ) MAXNP.
!         The maximum number of nodes which the program can handle.
! 
!  MAXNY  Input, integer ( kind = 4 ) MAXNY.
!         The maximum value allowed for NY.
!
!  MAXOBJ Input, integer ( kind = 4 ) MAXOBJ.
!         The number of graphical "objects".
! 
!  MAXPAR Input, integer ( kind = 4 ) MAXPAR, the maximum number of parameters.
!
!  NBOUND Output, integer ( kind = 4 ) NBOUND.
!         The number of points (XBOUND(I),YBOUND(I)) used to
!         define the boundary.
! 
!  NCON   Output, integer ( kind = 4 ) NCON.
!         The number of contour lines to be drawn.  This is
!         initialized to 12, but may be changed by the user.
! 
!  NELEM  Output, integer ( kind = 4 ) NELEM, the number of elements.
!
!  NFLAG  Output, logical NFLAG(MAXNP).
!
!         NFLAG is used to "flag" which nodes are active,
!         that is, to be displayed, and which not, in the graph.
!
!  NODE   Output, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), or NODE(MAXNPE,NELEM).
!         NODE contains, for each element, the global node numbers
!         of its NPE nodes.  
!
!         For linear elements (NPE = 3), the order of the nodes probably
!         doesn't matter, but we will draw them this way:
!
!                       2
!                      /|
!                    /  |
!                  /    |
!                /      |
!               1-------3
!
!         For quadratic elements (NPE = 6), the nodes must be given in a 
!         particular order, which is as follows:
!
!                       2
!                      /|
!                    /  |
!                  4    5
!                /      |
!               1---6---3
! 
!  NP     Output, integer ( kind = 4 ) NP.
!         NP is the total number of nodes.
! 
!  NPE    Output, integer ( kind = 4 ) NPE.
!         NPE is the number of nodes per element, which should be
!         3 for linear elements and 6 for quadratics.
!
!  NPROF  Output, integer ( kind = 4 ) NPROF(MY).
!         Records the indices of the nodes that lie along the profile line.
!
!  NSET   Output, integer ( kind = 4 ) NSET.
!         The number of sets of data contained in the data file.
! 
!  NXSKIP Output, integer ( kind = 4 ) NXSKIP.
!         NXSKIP is used to "thin" out a vector plot.  
!
!         If NXSKIP = 1, then a standard vector plot is made.
!  
!         Otherwise, in the X direction, vectors are drawn only
!         in columns 1, 1+NXSKIP, 1+2*NXSKIP and so on.
!
!  NY     Output, integer ( kind = 4 ) NY, the number of elements in the Y direction.
!
!  NYSKIP Output, integer ( kind = 4 ) NYSKIP.
!         NYSKIP is used to "thin" out a vector plot.  
!
!         If NYSKIP = 1, then a standard vector plot is made.
!
!         Otherwise, in the Y direction, vectors are drawn only
!         in rows 1, 1+NYSKIP, 1+2*NYSKIP and so on.
!  
!  OBJECT Output, character ( len = 30 ) OBJECT(MAXOBJ), the names of the 
!         graphical objects.
!
!  OVRLAY Output, logical OVRLAY.
!         If OVRLAY is true, then the next time that a plot is
!         requested, a "new frame" command is suppressed, so that
!         the new plot is shown on top of the previous one.
! 
!  P      Input, real P(MAXNP,0:2*MAXPAR).
! 
!         P(I,0) is the pressure at node I.
! 
!         P(I,J) is the sensitivity of the pressure with respect
!         to parameter J.
! 
!  SCALEE Output, real SCALEE.
!         The scale factor for the element numbers, with default value 1.
!
!  SCALEN Output, real SCALEN.
!         The scale factor for the node numbers, with default value 1.
!
!  SCALEV Output, real SCALEV.
!         A scale factor for velocity vectors.  This starts out at 1.0.
! 
!  SHOW   Output, logical SHOW(MAXOBJ).
!         Contains, for each object, a flag determining whether it
!         is to be shown or not.
! 
!  TITLE  Output, character ( len = 40 ) TITLE.
!         A title for the plots.
! 
!  TITLE2 Output, character ( len = 40 ) TITLE2.
!         A subtitle used in the profile plots.
!
!  U      Output, real U(MAXNP,MAXPAR).
! 
!         U(I,0) is the horizontal fluid velocity at node I.
! 
!         U(I,J) is the sensitivity of the horizontal velocity with 
!         respect to parameter J.
!  
!  V      Output, real V(MAXNP,MAXPAR).
! 
!         V(I,0) is the vertical fluid velocity at node I.
! 
!         V(I,J) is the sensitivity of the vertical velocity with
!         respect to parameter J.
! 
!  X1MAX,
!  X1MIN  Output, real X1MAX, X1MIN, the maximum and minimum X 
!         coordinates of the plot, which includes a small grace margin.
!
!  X2MAX,
!  X2MIN  Output, real X2MAX, X2MIN, the maximum and minimum X 
!         coordinates that should be used for plotting.  No plotting
!         commands should  exceed these values.  This is where the 
!         "frame" might be drawn.
!
!  X4MAX,
!  X4MIN  Output, real X4MAX, X4MIN, the maximum and minimum X 
!         coordinates that  are used for the plot, not including axes.
!
!         For profile graphs, X2MIN = 0.20, X2MAX=0.80.
!
!  XC     Output, real XC(MAXNP).
!         The X coordinates of the nodes.
! 
!  XPROF  Output, real XPROF.
!         The X coordinate of the profile line.
! 
!  XSIREF Output, real XSIREF(MAXNPE).
!         The XSI coordinates of the nodes of the reference
!         triangle.
! 
!  XSMAX  Output, real XSMAX.
!         The maximum X coordinate of the data to be displayed.
!         XSMAX defaults to XMAX, but can be made smaller to
!         focus on a portion of the region.
! 
!  XSMIN  Output, real XSMIN.
!         The minimum X coordinate of the data to be displayed.
!         XSMIN defaults to XMIN, but can be made larger to
!         focus on a portion of the region.
! 
!  Y1MAX,
!  Y1MIN  Output, real Y1MAX, Y1MIN, the maximum and minimum Y 
!         coordinates of the plot, which includes a small grace margin.
!
!  Y2MAX,
!  Y2MIN  Output, real Y2MAX, Y2MIN, the maximum and minimum Y
!         coordinates that should be used for plotting.  No plotting
!         commands should  exceed these values.  This is where the 
!         "frame" might be drawn.
!
!  Y4MAX,
!  Y4MIN  Output, real Y4MAX, Y4MIN, the maximum and minimum Y 
!         coordinates that  are used for the plot, not including axes.
!
!         For profile graphs, Y4MIN = 0.20, Y4MAX=0.80.
!
!  YC     Output, real YC(MAXNP).
!         The Y coordinates of the nodes.
! 
!  YSMAX  Output, real YSMAX.
!         The maximum Y coordinate of the data to be displayed.
!         YSMAX defaults to YMAX, but can be made smaller to
!         focus on a portion of the region.
! 
!  YSMIN  Output, real YSMIN.
!         The minimum Y coordinate of the data to be displayed.
!         YSMIN defaults to YMIN, but can be made larger to
!         focus on a portion of the region.
! 
  integer ( kind = 4 ) maxelm
  integer ( kind = 4 ) maxnp
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) maxny
  integer ( kind = 4 ) maxobj
  integer ( kind = 4 ) maxpar
  integer ( kind = 4 ) maxsen
!
  character ( len = 10 ) arrow
  real cp(maxnp)
  real delx
  real dely
  character ( len = 10 ) dev
  real dudxn(maxnp)
  real dudyn(maxnp)
  real dvdxn(maxnp)
  real dvdyn(maxnp)
  logical echo
  logical eflag(maxelm)
  logical eflagu(maxelm)
  character ( len = 2 )  eqn(3,maxnp)
  real etaref(maxnpe)
  character ( len = 80 ) fildat
  character ( len = 80 ) filgrf
  character ( len = 80 ) filinp
  character ( len = 20 ) filtyp
  real grace
  integer ( kind = 4 ) i
  integer ( kind = 4 ) icmax
  integer ( kind = 4 ) icmin
  integer ( kind = 4 ) icolor(maxobj)
  integer ( kind = 4 ) icomp
  integer ( kind = 4 ) idata
  integer ( kind = 4 ) ifile
  integer ( kind = 4 ) iplot
  integer ( kind = 4 ) iset
  integer ( kind = 4 ) itable
  integer ( kind = 4 ) iwrite
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jcmax
  integer ( kind = 4 ) jcmin
  character ( len = * ) labelx
  character ( len = * ) labely
  logical lbar
  integer ( kind = 4 ) line(maxobj)
  logical lppro
  logical lptpro
  logical lupro
  logical lutpro
  logical lvpro
  logical lvtpro
  integer ( kind = 4 ) nbound
  integer ( kind = 4 ) ncon
  integer ( kind = 4 ) nelem
  logical nflag(maxnp)
  logical nflag0(maxnp)
  integer ( kind = 4 ) node(maxnpe,maxelm)
  integer ( kind = 4 ) np
  integer ( kind = 4 ) npe
  integer ( kind = 4 ) nprof(2*maxny-1)
  integer ( kind = 4 ) nxskip
  integer ( kind = 4 ) ny
  integer ( kind = 4 ) nyskip
  integer ( kind = 4 ) nsen
  integer ( kind = 4 ) nset
  character ( len = 30 ) object(maxobj)
  logical ovrlay
  real p(maxnp,0:maxsen)
  real rho(maxnp)
  real rmach(maxnp)
  real scalee
  real scalen
  real scalev
  logical show(maxobj)
  real smax
  real smin
  character ( len = 40 ) title
  character ( len = 40 ) title2
  real u(maxnp,0:maxsen)
  real v(maxnp,0:maxsen)
  real xprof
  real x1max
  real x1min
  real x2max
  real x2min
  real x4max
  real x4min
  real xc(maxnp)
  real xsiref(maxnpe)
  real xsmax
  real xsmin
  real y1max
  real y1min
  real y2max
  real y2min
  real y4max
  real y4min
  real yc(maxnp)
  real ysmax
  real ysmin
!
  arrow = 'line'
  cp(1:maxnp) = 0.0
  delx = 1.0
  dely = 1.0
  dev = ' '

  do i = 1,maxnp
    dudxn(i) = 0.0
  end do

  do i = 1,maxnp
    dudyn(i) = 0.0
  end do

  do i = 1,maxnp
    dvdxn(i) = 0.0
  end do

  do i = 1,maxnp
    dvdyn(i) = 0.0
  end do

  do i = 1,3
    do j = 1,maxnp
      eqn(i,j) = '  '
    end do
  end do

  echo = .false.
 
  do i = 1,maxelm
    eflag(i) = .true.
    eflagu(i) = .true.
  end do

  fildat = 'display.dat'
  filgrf = ' '
  filinp = 'display.inp'
  filtyp = 'flow'
  grace = 0.05
  icmax = 255
  icmin = 2
!
!  Set the default colors for the objects.
!
  do i = 1,maxobj
    icolor(i) = 1
  end do

  icolor(20) = 127
  icolor(21) = 0

  icomp = 0
  idata = 0
  ifile = 0
  iplot = 0
  iset = 0
  itable = 9
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
  do i = 1,maxobj
    line(i) = 2
  end do
  line(1) = 2

  lppro = .false.
  lptpro = .false.
  lupro = .false.
  lutpro = .false.
  lvpro = .false.
  lvtpro = .false.
  nbound = 0
  ncon = 9
  nelem = 0

  do i = 1,maxnp
    nflag(i) = .true.
    nflag0(i) = .true.
  end do

  do i = 1,maxnpe
    do j = 1,maxelm
      node(i,j) = 0
    end do
  end do

  np = 0
  npe = maxnpe

  do i = 1,2*maxny-1
    nprof(i) = 0
  end do

  nxskip = 1
  ny = 0
  nyskip = 1
  nsen = 0
  nset = 0

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
  object(19) = 'profile line'
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

  ovrlay = .false.

  do i = 1,maxnp
    do j = 0,2*maxpar
      p(i,j) = 0.0
    end do
  end do

  do i = 1,maxnp
    rho(i) = 0.0
  end do

  do i = 1,maxnp
    rmach(i) = 0.0
  end do

  scalee = 1.0
  scalen = 1.0
  scalev = 1.0

  do i = 1,maxobj
    show(i) = .false.
  end do

  show(1) = .true.
  show(3) = .false.
  show(7) = .true.
 
  smax = 0.0
  smin = 0.0

  title = ' '
  title2 = ' '

  do i = 1,maxnp
    do j = 0,2*maxpar
      u(i,j) = 0.0
      v(i,j) = 0.0
    end do
  end do

  x2max = 1.0
  x2min = 0.0

  x4max = 0.95
  x4min = 0.05

  do i = 1,maxnp
    xc(i) = 0.0
  end do

  xprof = 0.0
  xsmax = 0.0
  xsmin = 0.0
  y2max = 1.0
  y2min = 0.0
  y4max = 0.95
  y4min = 0.05

  do i = 1,maxnp
    yc(i) = 0.0
  end do

  ysmax = 0.0
  ysmin = 0.0
!
!  Set things that depend on other things.
!
  call setref(etaref,maxnpe,npe,xsiref)

  x1max = x2max+grace*(x2max-x2min)
  x1min = x2min-grace*(x2max-x2min)
  y1max = y2max+grace*(y2max-y2min)
  y1min = y2min-grace*(y2max-y2min)

  return
end
function inside ( x1, xmid, x2 )
!
!***********************************************************************
!
!! INSIDE reports whether XMID is, or is not, between X1 and X2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
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
!  X1,
!  XMID,
!  X2     Input, real X1, XMID, X2, three values to be tested.
!         
!  INSIDE Output, logical INSIDE.
!
!         INSIDE will be .TRUE. if XMID is "inside" the interval
!         spanned by X1 and X2.  That is, if
!
!           X1 < =  XMID <= X2
!         or
!           X2 < =  XMID <= X1
!
!         Otherwise, INSIDE will be .FALSE.
!
  logical inside
  real x1
  real x2
  real xmid
!
  if ( (x1 <= xmid.and.xmid <= x2).or.(x1 >= xmid.and.xmid >= x2) ) then
    inside = .true.
  else
    inside = .false.
  end if

  return
end
subroutine isogn6 ( etagon,ielem,maxnpe,nelem,node,np,npe,npts,xc,xsigon,yc)
!
!***********************************************************************
!
!! ISOGN6 draws a filled polygon in a six node isoparametric element.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
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
!  ETAGON Input, real ETAGON(NPTS), the ETA coordinates of the
!         vertices of the polygon.
!
!  IELEM  Input, integer ( kind = 4 ) IELEM, the element in which we are working.
!
!  MAXNPE Input, integer ( kind = 4 ) MAXNPE.
!         MAXNPE is the maximum number of nodes per element.
!
!    Input, integer ( kind = 4 ) NELEM, the number of elements.
!
!  NODE   Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), or NODE(MAXNPE,NELEM).
!         NODE contains, for each element, the global node numbers
!         of its NPE nodes.  
!
!         For linear elements (NPE = 3), the order of the nodes probably
!         doesn't matter, but we will draw them this way:
!
!                       2
!                      /|
!                    /  |
!                  /    |
!                /      |
!               1-------3
!
!         For quadratic elements (NPE = 6), the nodes must be given in a 
!         particular order, which is as follows:
!
!                       2
!                      /|
!                    /  |
!                  4    5
!                /      |
!               1---6---3
! 
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!  NPE    Input, integer ( kind = 4 ) NPE.
!         NPE is the number of nodes per element, which should be
!         3 for linear elements and 6 for quadratics.
!
!  NPTS   Input, integer ( kind = 4 ) NPTS, the number of vertices in the polygon.
!
!  XC     Input, real XC(NP).
!         The X coordinates of the nodes.
! 
!  XSIGON Input, real XSIGON(NPTS), the XSI coordinates of the
!         vertices of the polygon.
!
!  YC     Input, real YC(NP).
!         The Y coordinates of the nodes.
! 
  integer ( kind = 4 ) maxpts2
  parameter (maxpts2 = 100)
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
  do i = 1,npe
    xn(i) = xc(node(i,ielem))
    yn(i) = yc(node(i,ielem))
  end do
!
!  Get the coefficients of the map from the reference triangle
!  to the image triangle.
!
  call get_map6(a1,b1,c1,d1,e1,f1,xn)
  call get_map6(a2,b2,c2,d2,e2,f2,yn)
!
!  Insert two extra points between every pair of original ones.
!
!     nfill = 2
  nfill = 0
  npts2 = (nfill+1)*npts

  if ( npts2 > maxpts2 ) then
    write(*,*)' '
    write(*,*)'ISOGN6 - Fatal error!'
    write(*,*)'  The input polygon has too many points!'
    stop
  end if
!
!  Insert 2 fillin points between each pair of points.
!
  call fillin2(nfill,npts,xsigon,xsigon2)
  call fillin2(nfill,npts,etagon,etagon2)
!
!  Compute the images of the points.
!
  do i = 1,npts2

    xsi = xsigon2(i)
    eta = etagon2(i)

    x = a1*xsi**2+b1*xsi*eta+c1*eta**2+d1*xsi+e1*eta+f1
    y = a2*xsi**2+b2*xsi*eta+c2*eta**2+d2*xsi+e2*eta+f2

    xgon(i) = x
    ygon(i) = y

  end do

  call plygon(npts2,xgon,ygon)

  return
end
subroutine isoln3 ( xa, ya, xb, yb )
!
!***********************************************************************
!
!! ISOLN3 draws a line between two points in a linear isoparametric element.  
!
!  Discussion:
!
!    The line is straight in the original, reference,
!    element, and will be straight in the image element.
!
!    ISOLN3 is given the locations of the points (XA,YA) and 
!    (XB,YB), which are presumed to lie inside of the same element.
!
!    ISOLN3 draws a line between these points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!  XA,
!  YA     Input, real XA, YA, the first point to be connected.
!
!  XB,
!  YB     Input, real XB, YB, the second point to be connected.
!
  integer ( kind = 4 ) narray
  parameter (narray = 2)
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
  call plylin(narray,xarray,yarray)

  return
end
subroutine isoln6 ( xa,ya,xb,yb,ielem,maxnpe,nelem,node,np,npe,xc,yc)
!
!***********************************************************************
!
!! ISOLN6 draws a line between two points in a quadratic isoparametric
!  element.  The line is straight in the original, reference,
!  element, but will probably be curved in the image element.
!
!  ISOLN6 is given the locations of the points (XA,YA) and 
!  (XB,YB), which are presumed to lie inside of element IELEM.
!
!  ISOLN6 then creates an interpolated array of NARRAY points,
!  (XDREF(I), YDREF(I)) stretching from (XA,YA) to (XB,YB).
!
!  Then, ISOLN6 computes the image location of each point,
!  and calls PLYLIN to draw the line connecting these points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
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
!  XA,
!  YA     Input, real XA, YA, the first point to be connected.
!
!  XB,
!  YB     Input, real XB, YB, the second point to be connected.
!
!  IELEM  Input, integer ( kind = 4 ) IELEM, the element in which we are working.
!
!  MAXNPE Input, integer ( kind = 4 ) MAXNPE.
!         MAXNPE is the maximum number of nodes per element.
!
!    Input, integer ( kind = 4 ) NELEM, the number of elements.
!
!  NODE   Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), or NODE(MAXNPE,NELEM).
!         NODE contains, for each element, the global node numbers
!         of its NPE nodes.  
!
!         For linear elements (NPE = 3), the order of the nodes probably
!         doesn't matter, but we will draw them this way:
!
!                       2
!                      /|
!                    /  |
!                  /    |
!                /      |
!               1-------3
!
!         For quadratic elements (NPE = 6), the nodes must be given in a 
!         particular order, which is as follows:
!
!                       2
!                      /|
!                    /  |
!                  4    5
!                /      |
!               1---6---3
! 
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!  NPE    Input, integer ( kind = 4 ) NPE.
!         NPE is the number of nodes per element, which should be
!         3 for linear elements and 6 for quadratics.
!
!  XC     Input, real XC(NP).
!         The X coordinates of the nodes.
! 
!  YC     Input, real YC(NP).
!         The Y coordinates of the nodes.
! 
  integer ( kind = 4 ) narray
  parameter (narray = 5)
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
  do i = 1,npe
    xn(i) = xc(node(i,ielem))
    yn(i) = yc(node(i,ielem))
  end do
!
!  Get the coefficients of the map.
!
  call get_map6(a1,b1,c1,d1,e1,f1,xn)
  call get_map6(a2,b2,c2,d2,e2,f2,yn)
!
!  Interpolate between the X and Y data reference values.
!
  call fillin(narray,xa,xb,xdref)
  call fillin(narray,ya,yb,ydref)
!
!  Map each point on the reference line to the image line.
!
  do i = 1,narray

    xdimage(i) = a1*xdref(i)**2+b1*xdref(i)*ydref(i) &
      +c1*ydref(i)**2+d1*xdref(i)+e1*ydref(i)+f1

    ydimage(i) = a2*xdref(i)**2+b2*xdref(i)*ydref(i) &
      +c2*ydref(i)**2+d2*xdref(i)+e2*ydref(i)+f2

  end do
!
!  Draw the image points.
!
  call plylin(narray,xdimage,ydimage)

  return
end
subroutine list ( delx,dely,dev,echo,fildat,grace,icmax,icmin,icolor, &
  icomp,idata,ifile,iplot,iset,itable,iwrite,jbound,maxbou,maxnp, &
  maxobj,maxpar,maxsen,nbound,ncon,nelem,np,npar,npe,nset,nx,nxskip,ny, &
  nyskip,object,p,scalev,show,title,title2,u,v,x2max,x2min,xmax,xmin, &
  xprof,xsmax,xsmin,ymax,ymin,y2max,y2min,ysmax,ysmin)
!
!***********************************************************************
!
!! LIST prints out the values of the variables of interest to the user.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 January 2001
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
!  DEV    Input, character*10 DEV.
!         The graphics output device to be used.  Current legal
!         values for DEV include:
! 
!         cgmb - CGM binary file.
!         ps   - PostScript file.
!         xws  - X window screen (interactive).
!
!    Input, character ( len = 80 ) FILDAT.
!         The name of the data file to be read in, which contains
!         the information defining the mesh and the physical
!         parameters.
! 
!  ICMAX,
!  ICMIN  Input, integer ( kind = 4 ) ICMAX, ICMIN, the maximum and minimum color 
!         indices to use in color contour graphics.
!
!  ICOLOR Input, integer ( kind = 4 ) ICOLOR(MAXOBJ).
!         Contains the color indexes for each object.
!         However, in some cases, ICOLOR is actual a color table
!         index.
! 
!  ICOMP  Input, integer ( kind = 4 ) ICOMP.
!         The component of the flow to be studied.
! 
!         ICOMP = 0 studies the basic flow.
!         ICOMP = I studies the sensitivity with respect to
!                 parameter I.
! 
!  IDATA  Input, integer ( kind = 4 ) IDATA.
!         0, no problem has been defined.
!         nonzero, a problem has been defined.
!
!  IFILE  Input, integer ( kind = 4 ) IFILE.
!         Records the status of the data file whose name is FILDAT.
! 
!         -2, an error occurred while reading from the file.
!         -1, the file could not be opened.
!          0, no file is currently open.
!          1, a file has been opened, but not read from.
!          2, data has been read from a file.
! 
!  IPLOT  Input, integer ( kind = 4 ) IPLOT.
!         The number of plots made so far.
! 
!  ISET   Input, integer ( kind = 4 ) ISET.
!         The data set being examined from the file.  If no file
!         is open, or if no data set has been read, then ISET is
!         zero.
! 
!  ITABLE Output, integer ( kind = 4 ) ITABLE, the desired color table.
!  
!         1: low black to high white
!         2: low blue to high yellow
!         3: low red, high blue, with bands between.
!         4: low red, yellow, green, blue, high white.
!         5: low white, blue, green, yellow, high red.
!         6: low blue to high red
!         7: linear table between 2 user colors.
!         8: linear table between N user colors.
!         9: low white to high black.
! 
!  IWRITE Input, integer ( kind = 4 ) IWRITE.
!         Controls debugging output.
! 
!         0 means no such output.
!         1 means some.
!         2 means a lot.
! 
!  JBOUND Input, integer ( kind = 4 ) JBOUND(5,MAXBOU)
! 
!         For each line segment of the boundary:
! 
!         JBOUND(1,I) contains the element number;
! 
!         JBOUND(2,I) contains the local node number of one corner 
!           of the element, which forms the edge;
!
!         JBOUND(2,I) contains the "next" node along the edge.
!           If the element is linear, this is the other corner node.
!           If the element is quadratic, this is the midside node along
!             the edge.
!
!         JBOUND(4,I) contains the "next" node along the edge.
!           If the element is linear, this is 0.
!           If the element is quadratic, this is the other corner node 
!             along the edge.
!
!         JBOUND(5,I) contains:
!           0 if the boundary is a wall (U = V=0);
!           1 if the boundary is open.
!
!  MAXBOU Input, integer ( kind = 4 ) MAXBOU.
!         The amount of storage available for the IBOUND array.
! 
!  MAXNP  Input, integer ( kind = 4 ) MAXNP, the maximum number of nodes.
!
!  MAXOBJ Input, integer ( kind = 4 ) MAXOBJ.
!         The number of graphical "objects".
!
!  NBOUND Input, integer ( kind = 4 ) NBOUND.
!         The number of points (XBOUND(I),YBOUND(I)) used to
!         define the boundary.
! 
!  NCON   Input, integer ( kind = 4 ) NCON, the number of contours to draw.
!
!    Input, integer ( kind = 4 ) NELEM.
!         The number of elements.
!   
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!  NPAR   Input, integer ( kind = 4 ) NPAR.
!         The number of parameters.
! 
!  NSET   Input, integer ( kind = 4 ) NSET.
!         The number of sets of data contained in the data file.
! 
!  NX     Input, integer ( kind = 4 ) NX.
!         Determines the number of nodes and elements in the X
!         direction.  There will be 2*NX-1 nodes, 2*NX-2 elements.
!  
!  NXSKIP Input, integer ( kind = 4 ) NXSKIP.
!         NXSKIP is used to "thin" out a vector plot.  
!
!         If NXSKIP = 1, then a standard vector plot is made.
!
!         Otherwise, in the X direction, vectors are drawn only
!         in columns 1, 1+NXSKIP, 1+2*NXSKIP and so on.
!  
!  NY     Input, integer ( kind = 4 ) NY.
!         Determines the number of nodes and elements in the Y
!         direction.  There will be 2*NY-1 nodes, 2*NY-2 elements.
! 
!  NYSKIP Input, integer ( kind = 4 ) NYSKIP.
!         NYSKIP is used to "thin" out a vector plot.  
!
!         If NYSKIP = 1, then a standard vector plot is made.
!
!         Otherwise, in the Y direction, vectors are drawn only
!         in rows 1, 1+NYSKIP, 1+2*NYSKIP and so on.
!  
!  OBJECT Input, character ( len = 30 ) OBJECT(MAXOBJ), the names of the 
!         graphical objects.
!
!  P      Input, real P(MAXNP,0:MAXPAR), the pressure at node I
!         is contained in P(I,0).  P(I,J) contains
!         the sensitivity of pressure with respect to parameter J, for
!         J = 1 to NPAR.
!
!  SCALEV Input, real SCALEV.
!         A scale factor for velocity vectors.  This starts out at 1.0.
! 
!  SHOW   Input, logical SHOW(MAXOBJ).
!         Contains, for each object, a flag determining whether it
!         is to be shown or not.
! 
!  TITLE  Input, character ( len = 40 ) TITLE.
!         A title for the plots.
! 
!  TITLE2 Input, character ( len = 40 ) TITLE2.
!         A subtitle used in the profile plots.
!
!  U,
!  V      Input, real U(MAXNP,0:MAXPAR), V(MAXNP,0:MAXPAR),
!         the horizontal and vertical velocity at node I
!         are contained in U(I,0) and V(I,0).  U(I,J) contains
!         the sensitivity of horizontal velocity with respect to parameter J, 
!         for J = 1 to NPAR, and similarly for V.
!
!  X2MAX,
!  X2MIN  Input, real X2MAX, X2MIN, the maximum and minimum X coordinates that
!         should be used for plotting.  No plotting commands should 
!         exceed these values.  This is where the "frame" might be drawn.
!
!  XMAX   Input, real XMAX.
!         The maximum X coordinate of all the nodes.
!         The maximum entry in the XC array.
! 
!  XMIN   Input, real XMIN.
!         The minimum X coordinate of all the nodes.
!         The minimum entry in the XC array.
! 
!  XPROF  Input, real XPROF.
!         The X coordinate of the profile line.
! 
!  XSMAX  Input, real XSMAX.
!         The maximum X coordinate of the data to be displayed.
!         XSMAX defaults to XMAX, but can be made smaller to
!         focus on a portion of the region.
! 
!  XSMIN  Input, real XSMIN.
!         The minimum X coordinate of the data to be displayed.
!         XSMIN defaults to XMIN, but can be made larger to
!         focus on a portion of the region.
! 
!  Y2MAX,
!  Y2MIN  Input, real Y2MAX, Y2MIN, the maximum and minimum Y coordinates that
!         should be used for plotting.  No plotting commands should 
!         exceed these values.  This is where the "frame" might be drawn.
!
!  YMAX   Output, real YMAX.
!         The maximum Y coordinate of all the nodes.
! 
!  YMIN   Output, real YMIN.
!         The minimum Y coordinate of all the nodes.
! 
!  YSMAX  Input, real YSMAX.
!         The maximum Y coordinate of the data to be displayed.
!         YSMAX defaults to YMAX, but can be made smaller to
!         focus on a portion of the region.
! 
!  YSMIN  Input, real YSMIN.
!         The minimum Y coordinate of the data to be displayed.
!         YSMIN defaults to YMIN, but can be made larger to
!         focus on a portion of the region.
! 
  integer ( kind = 4 ) maxbou
  integer ( kind = 4 ) maxnp
  integer ( kind = 4 ) maxobj
  integer ( kind = 4 ) maxpar
  integer ( kind = 4 ) maxsen
  integer ( kind = 4 ) npar
!
  real delx
  real dely
  character ( len = 10 ) dev
  logical echo
  character ( len = 80 ) fildat
  real grace
  integer ( kind = 4 ) i
  integer ( kind = 4 ) icmax
  integer ( kind = 4 ) icmin
  integer ( kind = 4 ) icolor(maxobj)
  integer ( kind = 4 ) icomp
  integer ( kind = 4 ) idata
  integer ( kind = 4 ) ifile
  integer ( kind = 4 ) iplot
  character isay
  integer ( kind = 4 ) iset
  integer ( kind = 4 ) itable
  integer ( kind = 4 ) iwrite
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jbound(5,maxbou)
  integer ( kind = 4 ) lenc
  integer ( kind = 4 ) lent
  logical s_eqi
  character ( len = 10 ) name
  integer ( kind = 4 ) nbound
  integer ( kind = 4 ) ncon
  integer ( kind = 4 ) nxskip
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) np
  integer ( kind = 4 ) npe
  integer ( kind = 4 ) nyskip
  integer ( kind = 4 ) nset
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny
  character ( len = 30 ) object(maxobj)
  real p(maxnp,0:maxsen)
  real pmax
  real pmin
  real scalev
  logical show(maxobj)
  character ( len = 40 ) title
  character ( len = 40 ) title2
  real u(maxnp,0:maxsen)
  real umax
  real umin
  real v(maxnp,0:maxsen)
  real vmax
  real vmin
  real x2max 
  real x2min
  real xmax
  real xmin
  real xprof
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

  write(*,*)' '
  write(*,*)'Type the name of the variable to be displayed,'
  write(*,*)'BLANK to return to the main program, '
  write(*,*)'? for a list of variable names.'
  write(*,*)' '

  read(*,'(a)',end = 40,err=40)name
  write(17,'(a)')name
  if ( echo ) then
    write(*,'(a)')name
  end if

  if ( name == ' ')return

  if ( name == '?' ) then

    write(*,*)' '
    write(*,*)'LIST:'
    write(*,*)'  The following variables may be listed:'
    write(*,*)' '
    write(*,*)'  DELX,   DELY,   DEV,    FILDAT, GRACE,  ICMAX,'
    write(*,*)'  ICMIN,'
    write(*,*)'  ICOLOR, ICOMP,  IDATA,  IFILE,  IPLOT,'
    write(*,*)'  ISET,   ITABLE, JBOUND, MAXPAR, NBOUND, NCON,'
    write(*,*)'  NXSKIP, NELEM,  NP,     NPAR,   NPE,    NYSKIP,'
    write(*,*)'  NSET,'
    write(*,*)'  NX,'
    write(*,*)'  NY,     OBJECT, P,      PMAX,   PMIN,   SCALEV, '
    write(*,*)'  SHOW,'
    write(*,*)'  TITLE,  TITLE2, UMAX,   UMIN,   VMAX,   VMIN,'
    write(*,*)'  X2MAX,  X2MIN,  XMAX,   XMIN,   XPROF,  '
    write(*,*)'  XSMAX,  XSMIN,  YMAX,   YMIN,   Y2MAX,  Y2MIN,   '
    write(*,*)'  YSMAX,  YSMIN'

    go to 10
  end if

  if ( s_eqi ( name,'iset').or.s_eqi ( name,'nset') ) then
 
    if ( ifile == 1 ) then

      lenc = len_trim ( fildat )
      write(*,'(1x,a)')fildat(1:lenc)
      write(*,*)'The data file contains ',nset,' data sets.'

    else if ( ifile == 2 ) then

      write(*,*)'The data file contains ',nset,' data sets.'
      write(*,*)'We have currently read data set ',iset

    end if
  end if
!
!  Print the value of the given variable.
!
  if ( s_eqi ( name,'delx') ) then
    write(*,*)'The nominal X spacing is DELX = ',delx
  else if ( s_eqi ( name,'dely') ) then
    write(*,*)'The nominal Y spacing is DELY = ',dely
  else if ( s_eqi ( name,'dev') ) then
    if ( dev == ' ' ) then
      write(*,*)'No graphics output device has been chosen.'
    else
      write(*,*)'Graphics output is of type DEV = ',dev
    end if
  else if ( s_eqi ( name,'fildat') ) then
    lenc = len_trim ( fildat )
    write(*,*)'The current data file is FILDAT = '//fildat(1:lenc)
  else if ( s_eqi ( name,'grace') ) then
    write(*,*)'The grace margin is GRACE  =  ',grace
  else if ( s_eqi ( name,'icmax') ) then
    write(*,*)'Maximum index from table is ICMAX = ',icmax
  else if ( s_eqi ( name,'icmin') ) then
    write(*,*)'Minimum index from table is ICMIN = ',icmin
  else if ( s_eqi ( name,'idata') ) then
    if ( idata == 0 ) then
      write(*,*)'No problem has been defined, IDATA = ',idata
    else
      write(*,*)'A problem has been defined, IDATA = ',idata
    end if
  else if ( s_eqi ( name,'ifile') ) then
    if ( ifile == 0 ) then
      write(*,*)'IFILE = 0, No data file has been opened.'
    else if ( ifile == 1 ) then
      write(*,*)'IFILE = 1, A data file has been opened.'
    else if ( ifile == 2 ) then
      write(*,*)'IFILE = 2, A data file has been read'
    end if
  else if ( s_eqi ( name,'iplot') ) then
    write(*,*)'IPLOT = ',iplot,' plots have been made so far.'
  else if ( s_eqi ( name,'itable') ) then
    write(*,*)'Color table used for area fill is ITABLE = ',itable
  else if ( s_eqi ( name,'iwrite') ) then
    write(*,*)'Debugging level IWRITE = ',iwrite
  else if ( s_eqi ( name,'maxpar') ) then
    write(*,*)'Maximum number of parameters MAXPAR = ',maxpar
  else if ( s_eqi ( name,'nbound') ) then
    write(*,*)'Number of boundary segments NBOUND = ',nbound
  else if ( s_eqi ( name,'ncon') ) then
    write(*,*)'Number of contour levels NCON = ',ncon
  else if ( s_eqi ( name,'nelem') ) then
    write(*,*)'There are NELEM = ',nelem,' elements.'
  else if ( s_eqi ( name,'np') ) then
    write(*,*)'There are NP = ',np,' nodes.'
  else if ( s_eqi ( name,'npar') ) then
    write(*,*)'The number of parameters is NPAR  =  ',npar
  else if ( s_eqi ( name,'npe') ) then
    write(*,*)'The number of nodes per element, NPE  =  ',npe
  else if ( s_eqi ( name,'nx') ) then
    write(*,*)'There are NX = ',nx,' elements in X direction.'
  else if ( s_eqi ( name,'nxskip') ) then
    write(*,*)'Skip value for column nodes is NXSKIP = ',nxskip
  else if ( s_eqi ( name,'ny') ) then
    write(*,*)'There are NY = ',ny,' elements in the Y direction.'
  else if ( s_eqi ( name,'nyskip') ) then
    write(*,*)'Skip value for row nodes is    NYSKIP = ',nyskip
  else if ( s_eqi ( name,'p') ) then
    write(*,*)' '
    write(*,*)'Nodal pressures I, P(I)'
    write(*,*)' '
    do i = 1,np
      write(*,*)i,p(i,icomp)
    end do
  else if ( s_eqi ( name,'pmax').or.s_eqi ( name,'pmin') ) then
    call rrange(np,p(1,icomp),pmax,pmin)
    write(*,*)'PMIN = ',pmin,' PMAX=',pmax
  else if ( s_eqi ( name,'scalev') ) then
    write(*,*)'Scale factor for velocities is SCALEV = ',scalev
  else if ( s_eqi ( name,'title') ) then
    lent = len_trim ( title )
    if ( lent > 0 ) then
      write(*,*)'Title:'//title
    else
      write(*,*)'No title has been assigned.'
    end if
  else if ( s_eqi ( name,'title2') ) then
    lent = len_trim ( title2 )
    if ( lent > 0 ) then
      write(*,*)'Title2:'//title2
    else
      write(*,*)'No subtitle has been assigned.'
    end if
  else if ( s_eqi ( name,'umax').or.s_eqi ( name,'umin') ) then
    call rrange(np,u(1,icomp),umax,umin)
    write(*,*)'UMIN = : ',umin,' UMAX=',umax
  else if ( s_eqi ( name,'vmax').or.s_eqi ( name,'vmin') ) then
    call rrange(np,v(1,icomp),vmax,vmin)
    write(*,*)'VMIN = ',vmin,' VMAX=',vmax
  else if ( s_eqi ( name,'x2max').or.s_eqi ( name,'x2min') ) then
    write(*,*)'X2MIN = ',x2min,' X2MAX=',x2max
  else if ( s_eqi ( name,'xmax').or.s_eqi ( name,'xmin') ) then
    write(*,*)'XMIN = ',xmin,' XMAX=',xmax
  else if ( s_eqi ( name,'xsmax').or.s_eqi ( name,'xsmin') ) then
    write(*,*)'XSMIN = ',xsmin,' XSMAX=',xsmax
  else if ( s_eqi ( name,'xprof') ) then
    write(*,*)'Profile line at XPROF = ',xprof
  else if ( s_eqi ( name,'y2max').or.s_eqi ( name,'y2min') ) then
    write(*,*)'Y2MIN = ',y2min,' Y2MAX=',y2max
  else if ( s_eqi ( name,'ymax').or.s_eqi ( name,'ymin') ) then
    write(*,*)'YMIN = ',ymin,' YMAX=',ymax
  else if ( s_eqi ( name,'ysmax').or.s_eqi ( name,'ysmin') ) then
    write(*,*)'YSMIN = ',ysmin,' YSMAX=',ysmax
  end if
!
!  Color and visibility information.
!
  if ( s_eqi ( name,'object').or. &
       s_eqi ( name,'show').or. &
       s_eqi ( name,'icolor').or. &
       s_eqi ( name,'*') ) then

    write(*,*)' '
    write(*,*)' Object    Show?    Icolor'
    write(*,*)' '
    do i = 1,maxobj
      write(*,'(a25,2x,l1,2x,i3)') object(i),show(i),icolor(i)
    end do
  end if

  if ( s_eqi ( name,'icomp').or.s_eqi ( name,'*') ) then

    write(*,*)' '
    if ( icomp == 0 ) then
      write(*,*)'We are displaying the basic solution.'
    else if ( 1 <= icomp.and.icomp <= npar ) then
      write(*,*)'We are displaying the sensitivity of the'
      write(*,*)'basic solution with respect to parameter ',icomp
    else if ( npar+1 <= icomp.and.icomp <= 2*npar ) then
      write(*,*)'We are displaying the gradient of the basic'
      write(*,*)'solution with respect to parameter ',icomp-npar
    end if
  end if

    if ( s_eqi ( name,'jbound') ) then


      write(*,*)' '
      write(*,*)'    I Element   Node 1   Node 2   Node 3 Type'
      write(*,*)' '

      do i = 1,nbound
 
        write(*,*)i,(jbound(j,i),j = 1,5)
 
        if ( mod(i,22) == 0 ) then
          read(*,'(a)')isay
          write(17,'(a)')isay
          if ( echo ) then
            write(*,'(a)')isay
          end if
          if ( isay /= ' ')go to 10
        end if
 
      end do

    end if

40    continue

  go to 10
end
subroutine node_data_print ( maxnp, maxsen, nflag, np, p, u, v, xc, yc )
!
!***********************************************************************
!
!! NODE_DATA_PRINT prints values at all the visible nodes.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!  MAXNP  Input, integer ( kind = 4 ) MAXNP, the maximum number of nodes.
!
!  MAXPAR Input, integer ( kind = 4 ) MAXPAR, the maximum number of parameters.
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!  P,
!  U,
!  V      Input, real P(MAXNP,0:MAXPAR), U(MAXNP,0:MAXPAR), V(MAXNP,0:MAXPAR),
!         the pressure, horizontal, and vertical velocity at node I
!         are contained in P(I,0), U(I,0) and V(I,0).  P(I,J) contains
!         the sensitivity of pressure with respect to parameter J, for
!         J = 1 to NPAR, and similarly for U and V.
!
!  XC     Input, real XC(NP).
!         The X coordinates of the nodes.
! 
!  YC     Input, real YC(NP).
!         The Y coordinates of the nodes.
! 
  integer ( kind = 4 ) maxnp
  integer ( kind = 4 ) maxsen
  integer ( kind = 4 ) np
!
  integer ( kind = 4 ) i
  logical nflag(maxnp)
  real p(maxnp,0:maxsen)
  real u(maxnp,0:maxsen)
  real v(maxnp,0:maxsen)
  real xc(np)
  real yc(np)
!
  write(12,*)' '
  write(12,*)'I,         XC(I),     YC(I)'
  write(12,*)'U,         V,         P'
  write(12,*)' '

  do i = 1,np
    if ( nflag(i) ) then
      write(12,*)' '
      write(12,'(i14,2g14.6)')i,xc(i),yc(i)
      write(12,'(3g14.6)')u(i,0),v(i,0),p(i,0)
    end if
  end do

  return
end
subroutine opnfil ( fildat,ifile,iset )
!
!***********************************************************************
!
!! OPNFIL does the housekeeping associated with opening a new data file.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
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
!    Input, character ( len = 80 ) FILDAT.
!         The name of the data file to be read in, which contains
!         the information defining the mesh and the physical
!         parameters.
! 
!  IFILE  Output, integer ( kind = 4 ) IFILE.
!         Records the status of the data file whose name is FILDAT.
!   
!         -2, an error occurred while reading from the file.
!         -1, the file could not be opened.
!          0, no file is currently open.
!          1, a file has been opened, but not read from.
!          2, data has been read from a file.
! 
!  ISET   Output, integer ( kind = 4 ) ISET.
!         The data set being examined from the file.  If no file
!         is open, or if no data set has been read, then ISET is
!         zero.
! 
  character ( len = 6 ) chrint
  character ( len = 80 ) fildat
  integer ( kind = 4 ) ifile
  integer ( kind = 4 ) iset
  integer ( kind = 4 ) lenc
!
  ifile = 0
  iset = 0
!
!  Open the file, count the number of data sets, and close it.
!
  open(unit = 2,file=fildat,form='formatted',access='sequential', &
    status = 'old',err=20)

  ifile = 1

  lenc = len_trim ( fildat )
  write(*,*)' '
  write(*,*)'OPNFIL - Note:'
  write(*,*)'  Opening the new file '//fildat(1:lenc)

  return

20    continue
  write(*,*)' '
  write(*,*)'OPNFIL - Serious error!'
  write(*,*)'  The input file could not be opened.'
  write(*,*)' '
  ifile = -1
  return
end
subroutine pgraf ( dev,echo,filgrf,icmax,icmin,icolor,iplot,itable, &
  labelx,labely,lppro,lptpro,lupro,lutpro,lvpro,lvtpro,maxnp,maxobj, &
  maxsen,np,nprof,ny,ovrlay,p,ptar,show,title,title2,u,utar,v,vtar,x1max, &
  x1min,x2max,x2min,x4max,x4min,y1max,y1min,y2max,y2min,y4max,y4min,yc)
!
!***********************************************************************
!
!! PGRAF makes a graph of the flow profile.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
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
!  DEV    Input, character*10 DEV.
!         The graphics output device to be used.  Current legal
!         values for DEV include:
! 
!         cgmb - CGM binary file.
!         ps   - PostScript file.
!         xws  - X window screen (interactive).
!
!  FILGRF Input, character ( len = 80 ) FILGRF, the name of the output
!         graphics file.
!
!  ICMAX,
!  ICMIN  Output, integer ( kind = 4 ) ICMAX, ICMIN, the maximum and 
!         minimum color indices to use in the color bar.
!
!  ICOLOR Input, integer ( kind = 4 ) ICOLOR(MAXOBJ).
!         Contains the color indexes for each object.
!         However, in some cases, ICOLOR is actual a color table
!         index.
! 
!  IPLOT  Input, integer ( kind = 4 ) IPLOT.
!         The number of plots made so far.
! 
!  ITABLE Input, integer ( kind = 4 ) ITABLE, the desired color table.
!  
!         1: low black to high white
!         2: low blue to high yellow
!         3: low red, high blue, with bands between.
!         4: low red, yellow, green, blue, high white.
!         5: low white, blue, green, yellow, high red.
!         6: low blue to high red
!         7: linear table between 2 user colors.
!         8: linear table between N user colors.
!         9: low white to high black.
! 
!  LABELX,
!  LABELY Input, character ( len = 30 ) LABELX, LABELY.  For profile plots, these 
!         contain labels for the X and Y axes.
!
!  LPPRO  Input, logical LPPRO.
!         If TRUE, then the computed pressure should be displayed
!         in profile plots.
!
!  LPTPRO Input, logical LPTPRO.
!         If TRUE, then the target pressure should be displayed
!         in profile plots.
!
!  LUPRO  Input, logical LUPRO.
!         If TRUE, then the computed horizontal velocity should be displayed
!         in profile plots.
!
!  LUTPRO Input, logical LUTPRO.
!         If TRUE, then the target horizontal velocity should be displayed
!         in profile plots.
!
!  LVPRO  Input, logical LVPRO.
!         If TRUE, then the computed vertical velocity should be displayed
!         in profile plots.
!
!  LVTPRO Input, logical LVTPRO.
!         If TRUE, then the target vertical velocity should be displayed
!         in profile plots.
!
!  MAXNP  Input, integer ( kind = 4 ) MAXNP, the maximum number of nodes.
!
!  MAXOBJ integer MAXOBJ.
!         The number of graphical "objects".
!
!  MAXPAR Input, integer ( kind = 4 ) MAXPAR.
!         The maximum number of parameters the program can handle.
! 
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!  NPROF  Input, integer ( kind = 4 ) NPROF(MY).
!         Records the indices of the nodes that lie along the profile line.
!
!  NY     Input, integer ( kind = 4 ) NY, the number of elements in the Y direction.
!
!  OVRLAY Input, logical OVRLAY.
!         If OVRLAY is true, then the next time that a plot is
!         requested, a "new frame" command is suppressed, so that
!         the new plot is shown on top of the previous one.
! 
!  P      Input, real P(MAXNP,0:2*MAXPAR).
! 
!         P(I,0) is the pressure at node I.
! 
!         P(I,J) is the sensitivity of the pressure with respect
!         to parameter J.
! 
!  PTAR   Input, real PTAR(MAXNP)
!         The pressure field associated with the target solution, at 
!         node I.
!
!  SHOW   Input, logical SHOW(MAXOBJ).
!         Contains, for each object, a flag determining whether it
!         is to be shown or not.
!
!  TITLE  Input, character ( len = 40 ) TITLE.
!         A title for the plots.
! 
!  TITLE2 Input, character ( len = 40 ) TITLE2.
!         A subtitle used in the profile plots.
!
!  U      Input, real U(MAXNP,MAXPAR).
! 
!         U(I,0) is the horizontal fluid velocity at node I.
! 
!         U(I,J) is the sensitivity of the horizontal velocity with 
!         respect to parameter J.
!  
!  UTAR   Input, real UTAR(MAXNP)
!         The horizontal velocity field associated with the target 
!         solution, at node I.
!
!  V      Input, real V(MAXNP,MAXPAR).
! 
!         V(I,0) is the vertical fluid velocity at node I.
! 
!         V(I,J) is the sensitivity of the vertical velocity with
!         respect to parameter J.
! 
!  VTAR   Input, real VTAR(MAXNP)
!         The vertical velocity field associated with the target solution, 
!         at node I.
!
!  X1MAX,
!  X1MIN  Input, real X1MAX, X1MIN, the maximum and minimum X 
!         coordinates of the plot, which includes a small grace margin.
!
!  X2MAX,
!  X2MIN  Input, real X2MAX, X2MIN, the maximum and minimum X coordinates that
!         should be used for plotting.  No plotting commands should 
!         exceed these values.  This is where the "frame" might be drawn.
!
!  X4MAX,
!  X4MIN  Input, real X4MAX, X4MIN, the maximum and minimum X 
!         coordinates that  are used for the plot, not including axes.
!
!         For profile graphs, X2MIN = 0.20, X2MAX=0.80.
!
!  Y1MAX,
!  Y1MIN  Input, real Y1MAX, Y1MIN, the maximum and minimum Y
!         coordinates of the plot, which includes a small grace margin.
!
!  Y2MAX,
!  Y2MIN  Input, real Y2MAX, Y2MIN, the maximum and minimum Y coordinates that
!         should be used for plotting.  No plotting commands should 
!         exceed these values.  This is where the "frame" might be drawn.
!
!  Y4MAX,
!  Y4MIN  Input, real Y4MAX, Y4MIN, the maximum and minimum Y 
!         coordinates that are used for the plot, not including axes.
!
!         For profile graphs, Y2MIN = 0.20, Y2MAX=0.80.
!
!  YC     Input, real YC(NP).
!         The Y coordinates of the nodes.
! 
  integer ( kind = 4 ) maxval
  parameter (maxval = 200)
!
  integer ( kind = 4 ) maxnp
  integer ( kind = 4 ) maxobj
  integer ( kind = 4 ) maxsen
  integer ( kind = 4 ) np
  integer ( kind = 4 ) ny
!
  character ( len = 10 ) dev
  real dshsiz
  logical echo
  character ( len = 80 ) filgrf
  integer ( kind = 4 ) i
  integer ( kind = 4 ) icmax
  integer ( kind = 4 ) icmin
  integer ( kind = 4 ) icolor(maxobj)
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) iplot
  integer ( kind = 4 ) itable
  character ( len = 30 ) labelx
  character ( len = 30 ) labely
  logical lppro
  logical lptpro
  logical lunset
  logical lupro
  logical lutpro
  logical lvpro
  logical lvtpro
  integer ( kind = 4 ) nprof(2*ny-1)
  integer ( kind = 4 ) nval
  logical ovrlay
  real p(maxnp,0:maxsen)
  real pmax
  real pmin
  real ptar(np)
  real ptmax
  real ptmin
  logical show(maxobj)
  character ( len = 40 ) title
  character ( len = 40 ) title2
  real u(maxnp,0:maxsen)
  real umax
  real umin
  real utar(np)
  real utmax
  real utmin
  real v(maxnp,0:maxsen)
  real vmax
  real vmin
  real vtar(np)
  real vtmax
  real vtmin
  real x1max
  real x1min
  real x2max
  real x2min
  real x4max
  real x4min
  real xdmax
  real xdmin
  real xval(maxval)
  real xwval(maxval)
  real y1max
  real y1min
  real y2max
  real y2min
  real y4max
  real y4min
  real yc(np)
  real ydmax
  real ydmin
  real yval(maxval)
  real ywval(maxval)
!
  dshsiz = 0.025
!
  call preplt(dev,echo,filgrf,icmax,icmin,iplot,itable,ovrlay)
!
!  Set the scale of the picture.
!  We allow ourselves more or less a one percent margin.
!
  xval(1) = x1min
  yval(1) = y1min
  xval(2) = x1max
  yval(2) = y1max
  call setscl(xval,yval,2)
!
!  Set coordinates.
!
  x4min = 0.20
  x4max = 0.80
  y4min = 0.20
  y4max = 0.80
!
!  The graphics package will use coordinates in the range 0 to 1.
!
  call setwcd(x1min,y1min,x1max,y1max,ierror)
!
!  Draw a box around the region.
!
  if ( show(3) ) then
    call linclr(icolor(3))
    call box(x2min,x2max,y2min,y2max)
  end if

  lunset = .false.
  ydmin = 0.0
  ydmax = 0.0
!
!  Compute the maximum and minimum value of P along the line.
!
  if ( lppro ) then

    call profp(maxval,np,nprof,nval,ny,p,xdmax,xdmin,xval,yc, &
      pmax,pmin,yval)

    if ( lunset ) then
      ydmin = pmin
      ydmax = pmax
      lunset = .false.
    else
      ydmin = min(pmin,ydmin)
      ydmax = max(pmax,ydmax)
     end if

  end if
!
!  Compute the maximum and minimum value of PTAR along the line.
!
  if ( lptpro ) then

    call profp(maxval,np,nprof,nval,ny,ptar,xdmax,xdmin,xval,yc, &
      ptmax,ptmin,yval)

    if ( lunset ) then
      ydmin = ptmin
      ydmax = ptmax
      lunset = .false.
    else
      ydmin = min(ptmin,ydmin)
      ydmax = max(ptmax,ydmax)
     end if

  end if
!
!  Compute the maximum and minimum value of U along the line.
!
  if ( lupro ) then

    call profuv(maxval,np,nprof,nval,ny,u,xdmax,xdmin,xval,yc, &
      umax,umin,yval)

    if ( lunset ) then
      ydmin = umin
      ydmax = umax
      lunset = .false.
    else
      ydmin = min(umin,ydmin)
      ydmax = max(umax,ydmax)
     end if

  end if
!
!  Compute the maximum and minimum value of UTAR along the line.
!
  if ( lutpro ) then

    call profuv(maxval,np,nprof,nval,ny,utar,xdmax,xdmin,xval,yc, &
      utmax,utmin,yval)

    if ( lunset ) then
      ydmin = utmin
      ydmax = utmax
      lunset = .false.
    else
      ydmin = min(utmin,ydmin)
      ydmax = max(utmax,ydmax)
     end if

  end if
!
!  Compute the maximum and minimum value of V along the line.
!
  if ( lvpro ) then

    call profuv(maxval,np,nprof,nval,ny,v,xdmax,xdmin,xval,yc, &
      vmax,vmin,yval)

    if ( lunset ) then
      ydmin = vmin
      ydmax = vmax
      lunset = .false.
    else
      ydmin = min(vmin,ydmin)
      ydmax = max(vmax,ydmax)
     end if

  end if
!
!  Compute the maximum and minimum value of VTAR along the line.
!
  if ( lvtpro ) then

    call profuv(maxval,np,nprof,nval,ny,vtar,xdmax,xdmin,xval,yc, &
      vtmax,vtmin,yval)

    if ( lunset ) then
      ydmin = vtmin
      ydmax = vtmax
      lunset = .false.
    else
      ydmin = min(vtmin,ydmin)
      ydmax = max(vtmax,ydmax)
     end if

  end if
!
!  Copy P into XVAL, YVAL, convert to screen coordinates, and draw.
!
  if ( lppro ) then

    call profp(maxval,np,nprof,nval,ny,p,xdmax,xdmin,xval,yc,pmax,pmin,yval)

    do i = 1,nval
      xwval(i) = x4min+(xval(i)-xdmin)*(x4max-x4min)/(xdmax-xdmin)
      ywval(i) = y4min+(yval(i)-ydmin)*(y4max-y4min)/(ydmax-ydmin)
    end do

    call plylin(nval,xwval,ywval)

  end if
!
!  Copy PTAR into XVAL, YVAL, convert to screen coordinates, and 
!  draw with a dashed line.
!
  if ( lptpro ) then

    call profp(maxval,np,nprof,nval,ny,ptar,xdmax,xdmin,xval,yc,ptmax,ptmin,yval)

    do i = 1,nval
      xwval(i) = x4min+(xval(i)-xdmin)*(x4max-x4min)/(xdmax-xdmin)
      ywval(i) = y4min+(yval(i)-ydmin)*(y4max-y4min)/(ydmax-ydmin)
    end do

    call dshlin(nval,xwval,ywval,dshsiz)

  end if
!
!  Copy U into XVAL, YVAL, convert to screen coordinates, and draw.
!
  if ( lupro ) then

    call profuv(maxval,np,nprof,nval,ny,u,xdmax,xdmin,xval,yc,umax,umin,yval)

    do i = 1,nval
      xwval(i) = x4min+(xval(i)-xdmin)*(x4max-x4min)/(xdmax-xdmin)
      ywval(i) = y4min+(yval(i)-ydmin)*(y4max-y4min)/(ydmax-ydmin)
    end do

    call plylin(nval,xwval,ywval)

  end if
!
!  Copy UTAR into XVAL, YVAL, convert to screen coordinates, and 
!  draw with a dashed line.
!
  if ( lutpro ) then

    call profuv(maxval,np,nprof,nval,ny,utar,xdmax,xdmin,xval,yc, &
      utmax,utmin,yval)

    do i = 1,nval
      xwval(i) = x4min+(xval(i)-xdmin)*(x4max-x4min)/(xdmax-xdmin)
      ywval(i) = y4min+(yval(i)-ydmin)*(y4max-y4min)/(ydmax-ydmin)
    end do

    call dshlin(nval,xwval,ywval,dshsiz)

  end if
!
!  Copy V into XVAL, YVAL, convert to screen coordinates, and draw.
!
  if ( lvpro ) then

    call profuv(maxval,np,nprof,nval,ny,v,xdmax,xdmin,xval,yc, &
      vmax,vmin,yval)

    do i = 1,nval
      xwval(i) = x4min+(xval(i)-xdmin)*(x4max-x4min)/(xdmax-xdmin)
      ywval(i) = y4min+(yval(i)-ydmin)*(y4max-y4min)/(ydmax-ydmin)
    end do

    call plylin(nval,xwval,ywval)

  end if
!
!  Copy VTAR into XVAL, YVAL, convert to screen coordinates, and 
!  draw with a dashed line.
!
  if ( lvtpro ) then

    call profuv(maxval,np,nprof,nval,ny,vtar,xdmax,xdmin,xval,yc, &
      vtmax,vtmin,yval)

    do i = 1,nval
      xwval(i) = x4min+(xval(i)-xdmin)*(x4max-x4min)/(xdmax-xdmin)
      ywval(i) = y4min+(yval(i)-ydmin)*(y4max-y4min)/(ydmax-ydmin)
    end do

    call dshlin(nval,xwval,ywval,dshsiz)

  end if
!
!  Draw the title, the axes, the axis labels.
!
  labelx = 'Y coordinate on profile line'
  call doaxes(labelx,labely,title,title2,x4max,x4min, &
    xdmax,xdmin,y4max,y4min,ydmax,ydmin)
!
!  Pause, if we are doing X-Windows.
!
  call buzz ( dev, x1min, x1max, y1min, y1max )

  return
end
subroutine plot_file_read ( eqn, ierror, ifile, iset, isotri, maxelm, maxnp, &
  maxnpe, maxny, maxpar, maxsen, nelem, node, np, npar, npe, nprof, nsen, nx, &
  ny, p, para, ptar, rho, u, utar, v, vtar, xc, xprof, yc )
!
!***********************************************************************
!
!! PLOT_FILE_READ reads a plot file defining the mesh and the flow variables.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, character ( len = 2 )  EQN(3,NP).
!    EQN records the "type" of each equation that will be generated, and
!    which is associated with an unknown.  Note that most boundary 
!    conditions do not result in an equation.  The current values are:
!
!    'U'  The horizontal momentum equation.
!    'UW' The condition U = 0 applied at a node on a fixed wall.
!    'V'  The vertical momentum equation.
!    'VW' The condition V = 0 applied at a node on a fixed wall.
!    'P'  The continuity equation.
!    'PB' The condition P = 0 applied at (XMAX,YMAX).
!
!    Output, integer ( kind = 4 ) IERROR.
!    0, no error.
!    1, if we run out of data on any read.
!    2, format error.
!    3, other error.
!
!    Input/output, integer ( kind = 4 ) IFILE.
!    Records the status of the data file whose name is FILDAT.
! 
!    -2, an error occurred while reading from the file.
!    -1, the file could not be opened.
!     0, no file is currently open.
!     1, a file has been opened, but not read from.
!     2, data has been read from a file.
! 
!    Input, integer ( kind = 4 ) ISET.
!    The data set being examined from the file.  If no file
!    is open, or if no data set has been read, then ISET is zero.
!
!    Output, integer ( kind = 4 ) ISOTRI(MAXELM).
! 
!    0, if element I is not isoparametric.
!    1 or 2, if element I is isoparametric.
! 
!    Input, integer ( kind = 4 ) MAXELM.
!    The maximum number of elements which the program can handle.
! 
!    Input, integer ( kind = 4 ) MAXNP, the maximum number of nodes.
!
!    Input, integer ( kind = 4 ) MAXNPE.
!    MAXNPE is the maximum number of nodes per element.
!
!    Input, integer ( kind = 4 ) MAXNY, the maximum allowed value of NY.
!
!    Input, integer ( kind = 4 ) MAXPAR.
!    The maximum number of parameters the program can handle.
! 
!    Output, integer ( kind = 4 ) NELEM, the number of elements.
! 
!    Output, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), or NODE(MAXNPE,NELEM).
!    NODE contains, for each element, the global node numbers
!    of its NPE nodes.  
!
!    For linear elements (NPE = 3), the order of the nodes probably
!    doesn't matter, but we will draw them this way:
!
!               2
!              /|
!             / |
!            /  |
!           /   |
!          1----3
!
!    For quadratic elements (NPE = 6), the nodes must be given in a 
!    particular order, which is as follows:
!
!                2
!               / |
!              /  |
!             4   5
!            /    |
!           1--6--3
! 
!    Output, integer ( kind = 4 ) NP, the number of nodes.
!
!    Output, integer ( kind = 4 ) NPAR, the number of parameters.
! 
!    Output, integer ( kind = 4 ) NPE.
!    NPE is the number of nodes per element, which should be
!    3 for linear elements and 6 for quadratics.
!
!    Output, integer ( kind = 4 ) NPROF(MY).
!    Records the indices of the nodes that lie along the profile line.
!
!    Output, integer ( kind = 4 ) NX.
!    Determines the number of nodes and elements in the X
!    direction.  There will be 2*NX-1 nodes, 2*NX-2 elements.
! 
!    Output, integer ( kind = 4 ) NY.
!    Determines the number of nodes and elements in the Y
!    direction.  There will be 2*NY-1 nodes, 2*NY-2 elements.
! 
!    Output, real P(MAXNP,0:MAXPAR).
!    P(I,0) is the pressure at node I.
!    P(I,J) is the sensitivity of the pressure with respect
!    to parameter J.
! 
!    Output, real PARA(MAXPAR).
!    The value of the parameters.
! 
!    Output, real PTAR(MAXNP)
!    The pressure field associated with the target solution, at node I.
!
!    Output, real U(MAXNP,MAXPAR).
!    U(I,0) is the horizontal fluid velocity at node I.
!    U(I,J) is the sensitivity of the horizontal velocity with 
!    respect to parameter J.
! 
!    Output, real UTAR(MAXNP)
!    The horizontal velocity field associated with the target 
!    solution, at node I.
!
!    Output, real V(MAXNP,MAXPAR).
!    V(I,0) is the vertical fluid velocity at node I.
!    V(I,J) is the sensitivity of the vertical velocity with
!    respect to parameter J.
! 
!    Output, real VTAR(MAXNP)
!    The vertical velocity field associated with the target solution, at node I.
!
!    Output, real XC(MAXNP).
!    The X coordinates of the nodes.
! 
!    Output, real XPROF.
!    The X coordinate of the profile line.
!
!    Output, real YC(MAXNP).
!    The Y coordinates of the nodes.
! 
  integer ( kind = 4 ) maxelm
  integer ( kind = 4 ) maxnp
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) maxny
  integer ( kind = 4 ) maxpar
  integer ( kind = 4 ) maxsen
!
  character ( len = 2 )  eqn(3,maxnp)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) icheck
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) ifile
  integer ( kind = 4 ) iset
  integer ( kind = 4 ) isotri(maxelm)
  integer ( kind = 4 ) j
  character ( len = 6 ) name
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) node(maxnpe,maxelm)
  integer ( kind = 4 ) np
  integer ( kind = 4 ) npar
  integer ( kind = 4 ) npe
  integer ( kind = 4 ) nprof(2*maxny-1)
  integer ( kind = 4 ) nsen
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny
  real p(maxnp,0:maxsen)
  real para(maxpar)
  real ptar(maxnp)
  real rho(maxnp)
  real u(maxnp,0:maxsen)
  real utar(maxnp)
  real v(maxnp,0:maxsen)
  real vtar(maxnp)
  real xc(maxnp)
  real xprof
  real yc(maxnp)
!
  icheck = 0
  ierror = 0

  name = 'nelem'
  read ( 2, *, end = 33, err=30 ) nelem
 
  if ( nelem > maxelm ) then
    write(*,*)' '
    write(*,*)'PLOT_FILE_READ - Serious error!'
    write(*,*)'  NELEM is larger than MAXELM!'
    write(*,*)'  NELEM = ',nelem
    write(*,*)'  MAXELM = ',maxelm
    write(*,*)' '
    write(*,*)'  Display cannot handle this input file!'
    write(*,*)' '
    write(*,*)'  Increase MAXNX and MAXNY,'
    write(*,*)'  recompile Display, and try again!'
    write(*,*)' '
    stop
  end if

  name = 'np'
  read(2,*,end = 10,err=30)np
 
  if ( np > maxnp ) then
    write(*,*)' '
    write(*,*)'PLOT_FILE_READ - Serious error!'
    write(*,*)'  NP is larger than MAXNP!'
    write(*,*)'  NP = ',np
    write(*,*)'  MAXNP = ',maxnp
    write(*,*)' '
    write(*,*)'  DISPLAY cannot handle this input file!'
    write(*,*)' '
    stop
  end if

  name = 'npar'
  read(2,*,end = 10,err=30)npar

  if ( npar > maxpar ) then
    write(*,*)' '
    write(*,*)'PLOT_FILE_READ - Serious error!'
    write(*,*)'  NPAR is larger than MAXPAR!'
    write(*,*)'  NPAR = ',npar
    write(*,*)'  MAXPAR = ',maxpar
    write(*,*)' '
    write(*,*)'  DISPLAY cannot handle this input file!'
    stop
  end if

  name = 'npe'
  read(2,*,end = 10,err=30) npe

  if ( npe /= 3.and.npe /= 6 ) then
    write(*,*)' '
    write(*,*)'PLOT_FILE_READ - Fatal error!'
    write(*,*)'  NPE must be 3 or 6,'
    write(*,*)'  but the input value is NPE = ',npe
    write(*,*)' '
    write(*,*)'  DISPLAY cannot handle this input file!'
    stop
  end if

  name = 'nsen'
  read(2,*,end = 10,err=30)nsen
 
  name = 'nx'
  read(2,*,end = 10,err=30)nx
  name = 'ny'
  read(2,*,end = 10,err=30)ny

  name = 'p(i,0)'
  do i = 1,np
    read(2,*,end = 10,err=30)p(i,0)
  end do

  name = 'u(i,0)'
  do i = 1,np
    read(2,*,end = 10,err=30)u(i,0)
  end do

  name = 'v(i,0)'
  do i = 1,np
    read(2,*,end = 10,err=30)v(i,0)
  end do

  name = 'isotri'
  do i = 1,nelem
    read(2,*,end = 10,err=30)isotri(i)
  end do
 
  name = 'node'
  do i = 1,npe
    do j = 1,nelem
      read(2,*,end = 10,err=30)node(i,j)
    end do
  end do
 
  name = 'nprof'
  do i = 1,2*ny-1
    read(2,*,end = 10,err=30)nprof(i)
  end do

  name = 'para'
  do i = 1,npar
    read(2,*,end = 10,err=30)para(i)
  end do
 
  do j = 1,nsen

    name = 'p(i,j)'
    do i = 1,np
      read(2,*,end = 10,err=30)p(i,j)
    end do

    name = 'u(i,j)'
    do i = 1,np
      read(2,*,end = 10,err=30)u(i,j)
    end do

    name = 'v(i,j)'
    do i = 1,np
      read(2,*,end = 10,err=30)v(i,j)
    end do

  end do

  rho(1:np) = 1.0

  name = 'xc'
  do i = 1,np
    read(2,*,end = 10,err=30)xc(i)
  end do

  name = 'xprof'
  read(2,*,end = 10,err=30)xprof

  name = 'yc'
  do i = 1,np
    read(2,*,end = 10,err=30)yc(i)
  end do

  name = 'eqn'
  do i = 1,np
    read(2,'(3a2)',end = 10,err=30)eqn(1,i),eqn(2,i),eqn(3,i)
  end do

  name = 'icheck'
  read(2,*,end = 10,err=30)icheck

  if ( icheck /= 1953 ) then
    write(*,*)' '
    write(*,*)'PLOT_FILE_READ - Warning!'
    write(*,*)'  The data in this file seems wrong!'
    write(*,*)' '
    ierror = 4
    return
  end if

  ifile = 2
!
!  If this is the first data set in the file, we assume it's the
!  target data, so we save copies of P, U and V.
!
  if ( iset == 0 ) then
    do i = 1,np
      utar(i) = u(i,0)
      vtar(i) = v(i,0)
      ptar(i) = p(i,0)
    end do
  end if
 
  return

33    continue

  ierror = 1
  write(*,*)' '
  write(*,*)'PLOT_FILE_READ - Note:'
  write(*,*)'  Reached the end of the file.'
  return
 
10    continue
  ierror = 3
  write(*,*)' '
  write(*,*)'PLOT_FILE_READ - Warning!'
  write(*,*)'  Reached end of file while reading ' // trim ( name )
  write(*,*)'  This file may be unusable!'
  return

30    continue
  ierror = 2
  write(*,*)' '
  write(*,*)'PLOT_FILE_READ - Warning!'
  write(*,*)'  Format error while reading '// trim ( name )
  write(*,*)'  This file may be unusable!'
  return
end
subroutine pltbox ( grace,srange,x1max,x1min,x2max,x2min,xsmax, &
  xsmin,y1max,y1min,y2max,y2min,ysmax,ysmin)
!
!***********************************************************************
!
!! PLTBOX computes a square box containing the data.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
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
!    Input, real GRACE.
!         The size of the "grace" margin on the plot.
! 
!  SRANGE Output, real SRANGE.
!         The maximum of XSMAX-XSMIN and YSMAX-YSMIN.
!         This gives the size of a square containing the data
!         window.
!
!  X1MAX,
!  X1MIN  Output, real X1MAX, X1MIN, the maximum and minimum X 
!         coordinates of the plot, which includes a small grace margin.
!
!  X2MAX,
!  X2MIN  Output, real X2MAX, X2MIN, the maximum and minimum X coordinates that
!         should be used for plotting.  No plotting commands should 
!         exceed these values.  This is where the "frame" might be drawn.
!
!  XSMAX  Input, real XSMAX.
!         The maximum X coordinate of the data to be displayed.
!         XSMAX defaults to XMAX, but can be made smaller to
!         focus on a portion of the region.
! 
!  XSMIN  Input, real XSMIN.
!         The minimum X coordinate of the data to be displayed.
!         XSMIN defaults to XMIN, but can be made larger to
!         focus on a portion of the region.
! 
!  Y1MAX,
!  Y1MIN  Output, real Y1MAX, Y1MIN, the maximum and minimum Y 
!         coordinates of the plot, which includes a small grace margin.
!
!  Y2MAX,
!  Y2MIN  Output, real Y2MAX, Y2MIN, the maximum and minimum Y coordinates that
!         should be used for plotting.  No plotting commands should 
!         exceed these values.  This is where the "frame" might be drawn.
!  
!  YSMAX  Input, real YSMAX.
!         The maximum Y coordinate of the data to be displayed.
!         YSMAX defaults to YMAX, but can be made smaller to
!         focus on a portion of the region.
! 
!  YSMIN  Input, real YSMIN.
!         The minimum Y coordinate of the data to be displayed.
!         YSMIN defaults to YMIN, but can be made larger to
!         focus on a portion of the region.
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
  srange = max(xsmax-xsmin,ysmax-ysmin)
 
  if ( srange <= 0.0 ) then
    srange = 1.0
  end if
 
  x2min = 0.5*(xsmin+xsmax)-0.5*srange
  x2max = 0.5*(xsmin+xsmax)+0.5*srange
  y2min = 0.5*(ysmin+ysmax)-0.5*srange
  y2max = 0.5*(ysmin+ysmax)+0.5*srange

  x1min = x2min-grace*(x2max-x2min)
  x1max = x2max+grace*(x2max-x2min)
  y1min = y2min-grace*(y2max-y2min)
  y1max = y2max+grace*(y2max-y2min)

  write(*,*)' '
  write(*,*)'PLTBOX - Note:'
  write(*,*)'  New total picture coordinates:'
  write(*,*)' '
  write(*,*)x1min,' X1MIN < =  X <= X1MAX ',x1max
  write(*,*)y1min,' Y1MIN < =  Y <= Y1MAX ',y1max
  write(*,*)' '
  write(*,*)'  New graphing area coordinates:'
  write(*,*)' '
  write(*,*)x2min,' X2MIN < =  X <= X2MAX ',x2max
  write(*,*)y2min,' Y2MIN < =  Y <= Y2MAX ',y2max
  write(*,*)' '
  write(*,*)' '
  write(*,*)'  New data window coordinates:'
  write(*,*)' '
  write(*,*)xsmin,' XSMIN < =  X <= XSMAX ',xsmax
  write(*,*)ysmin,' YSMIN < =  Y <= YSMAX ',ysmax

  return
end
subroutine preplt ( dev,echo,filgrf,icmax,icmin,iplot,itable,ovrlay)
!
!***********************************************************************
!
!! PREPLT should be called before doing each plot.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
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
!  DEV    Input, character*10 DEV.
!         The graphics output device to be used.  Current legal
!         values for DEV include:
! 
!         cgmb - CGM binary file.
!         ps   - PostScript file.
!         xws  - X window screen (interactive).
! 
!  FILGRF Input/output, character ( len = 80 ) FILGRF, the name of the output
!         graphics file.
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
!         If IPLOT is less than or equal to 1, it is reset to 1.
!
!  ITABLE Input, integer ( kind = 4 ) ITABLE, the desired color table.
!  
!         1: low black to high white
!         2: low blue to high yellow
!         3: low red, high blue, with bands between.
!         4: low red, yellow, green, blue, high white.
!         5: low white, blue, green, yellow, high red.
!         6: low blue to high red
!         7: linear table between 2 user colors.
!         8: linear table between N user colors.
!         9: low white to high black.
! 
!  OVRLAY Input, logical OVRLAY.
!         If OVRLAY is true, then the next time that a plot is
!         requested, a "new frame" command is suppressed, so that
!         the new plot is shown on top of the previous one.
! 
  integer ( kind = 4 ) nval
  parameter (nval = 2)
!
  character ( len = * )  dev
  logical echo
  character ( len = 80 ) filgrf
  integer ( kind = 4 ) icmax
  integer ( kind = 4 ) icmin
  integer ( kind = 4 ) icolor
  integer ( kind = 4 ) iplot
  integer ( kind = 4 ) itable
  logical linit
  logical ovrlay
  real xval(nval)
  real yval(nval)
!
  save linit
!
  data linit /.false./
!
!  If it's the first picture, then
!
!    Choose an output device,
!    Give the output file a name,
!    Initialize the graphics package.
!
  if ( .not.linit ) then

    linit = .true.

    if ( dev == ' ' ) then
      write(*,*)' '
      write(*,*)'PrePlt - Warning!'
      write(*,*)'  No output device was specified.'
      write(*,*)'  PostScript output will be generated.'
      dev = 'ps'
    end if

    call device(dev)

    if ( dev == 'cgmb' ) then

      if ( filgrf == ' ' ) then
        filgrf = 'display.cgm'
      end if

      call outfil(filgrf)

    else if ( dev == 'ps' ) then

      if ( filgrf == ' ' ) then
        filgrf = 'display.ps'
      end if

      call outfil(filgrf)

    end if

    if ( itable == 0 ) then
      itable = 1
    end if

    call settab(echo,icmax,icmin,itable)

    call grfini

    xval(1) = 0.0
    xval(2) = 1.0
    yval(1) = 0.0
    yval(2) = 1.0
    call setscl(xval,yval,nval)

    icolor = 1
    call linclr(icolor)
    call filclr(icolor)
!
!  Else, if it's a later picture,
!
!    Issue a "new frame" command, unless an overlay was requested.
!
  else

    if ( .not.ovrlay ) then
      call newfrm
    end if

  end if

  iplot = iplot+1

  return
end
subroutine profp ( maxval,np,nprof,nval,ny,p,xdmax,xdmin,xval,yc, &
  ydmax,ydmin,yval)
!
!***********************************************************************
!
!! PROFP considers each node along the profile line, and copies
!  the Y coordinate into XVAL, and the value of pressure into YVAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
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
!  MAXVAL Input, integer ( kind = 4 ) MAXVAL, the maximum number of profile nodes
!         that can be handled.
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!  NPROF  Input, integer ( kind = 4 ) NPROF(MY).
!         Records the indices of the nodes that lie along the profile 
!         line.
!
!  NVAL   Output, integer ( kind = 4 ) NVAL, the number of profile nodes which
!         were handled.
!
!  NY     Input, integer ( kind = 4 ) NY, the number of elements in the Y direction.
!
!  P      Input, real P(NP), the value of the pressure at each node.
!
!  XDMAX,
!  XDMIN  Output, real XDMAX, XDMIN, the maximum and minimum values of
!         Y along the profile line.
!
!  XVAL   Output, real XVAL(2*(NY-1)), the Y coordinates of the profile nodes.
!
!  YC     Input, real YC(NP).
!         The Y coordinates of the nodes.
! 
!  YDMAX,
!  YDMIN  Output, real YDMAX, YDMIN, the maximum and minimum values of
!         P along the profile line.
!
!  YVAL   Output, real PVAL(2*(NY-1)), the value of P at each of the profile
!         nodes.
!
  integer ( kind = 4 ) maxval
  integer ( kind = 4 ) np
  integer ( kind = 4 ) ny
!
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n3
  integer ( kind = 4 ) nprof(2*ny-1)
  integer ( kind = 4 ) nval
  real p(np)
  real p1
  real p3
  real xdmax
  real xdmin
  real xval(maxval)
  real y1
  real y3
  real yc(np)
  real ydmax
  real ydmin
  real yval(maxval)
!
!  Compute the value of P along the line.
!
  nval = 0

  do i = 1,2*ny-3,2

    n1 = nprof(i)
    n3 = nprof(i+2)
    y1 = yc(n1)
    y3 = yc(n3)
    p1 = p(n1)
    p3 = p(n3)

    nval = nval+1
    xval(nval) = y1
    yval(nval) = p1
 
    nval = nval+1
    xval(nval) = y3
    yval(nval) = p3
    
  end do
!
!  Compute the range of the data.
!
  call rrange(nval,xval,xdmax,xdmin)
  call rrange(nval,yval,ydmax,ydmin)
 
  return
end
subroutine profuv ( maxval,np,nprof,nval,ny,u,xdmax,xdmin,xval,yc, &
  ydmax,ydmin,yval)
!
!***********************************************************************
!
!! PROFUV considers each node along the profile line, and copies
!  the Y coordinate into XVAL, and the value of horizontal velocity
!  into YVAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
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
!  MAXVAL Input, integer ( kind = 4 ) MAXVAL, the maximum number of profile nodes
!         that can be handled.
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!  NPROF  Input, integer ( kind = 4 ) NPROF(MY).
!         Records the indices of the nodes that lie along the profile 
!         line.
!
!  NVAL   Output, integer ( kind = 4 ) NVAL, the number of profile nodes which
!         were handled.
!
!  NY     Input, integer ( kind = 4 ) NY, the number of elements in the Y direction.
!
!  U      Input, real U(NP), the value of the horizontal velocity at 
!         each node.
!
!  XDMAX,
!  XDMIN  Output, real XDMAX, XDMIN, the maximum and minimum values of
!         Y along the profile line.
!
!  XVAL   Output, real XVAL(2*(NY-1)), the Y coordinates of the profile nodes.
!
!  YC     Input, real YC(NP).
!         The Y coordinates of the nodes.
! 
!  YDMAX,
!  YDMIN  Output, real YDMAX, YDMIN, the maximum and minimum values of
!         U along the profile line.
!
!  YVAL   Output, real PVAL(2*(NY-1)), the value of U at each of the profile
!         nodes.
!
  integer ( kind = 4 ) maxval
  integer ( kind = 4 ) np
  integer ( kind = 4 ) ny
!
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n2
  integer ( kind = 4 ) n3
  integer ( kind = 4 ) nprof(2*ny-1)
  integer ( kind = 4 ) nval
  real u(np)
  real u1
  real u2
  real u3
  real xdmax
  real xdmin
  real xval(maxval)
  real y1
  real y2
  real y3
  real yc(np)
  real ydmax
  real ydmin
  real yval(maxval)
!
!  Compute the value of U along the line.
!
  nval = 0

  do i = 1,2*ny-3,2

    n1 = nprof(i)
    n2 = nprof(i+1)
    n3 = nprof(i+2)
    y1 = yc(n1)
    y2 = yc(n2)
    y3 = yc(n3)
    u1 = u(n1)
    u2 = u(n2)
    u3 = u(n3)

    nval = nval+1
    xval(nval) = y1
    yval(nval) = u1

    nval = nval+1
    xval(nval) = y2
    yval(nval) = u2

    nval = nval+1
    xval(nval) = y3
    yval(nval) = u3
    
  end do
!
!  Compute the range of the data.
!
  call rrange(nval,xval,xdmax,xdmin)
  call rrange(nval,yval,ydmax,ydmin)
 
  return
end
subroutine quit ( dev,ifile,iplot )
!
!***********************************************************************
!
!! QUIT shuts down the program.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
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
!  DEV    Input, character*10 DEV.
!         The graphics output device to be used.  Current legal
!         values for DEV include:
! 
!         cgmb - CGM binary file.
!         ps   - PostScript file.
!         xws  - X window screen (interactive).
! 
!  IFILE  Output, integer ( kind = 4 ) IFILE.
!         Records the status of the data file whose name is FILDAT.
!   
!         -2, an error occurred while reading from the file.
!         -1, the file could not be opened.
!          0, no file is currently open.
!          1, a file has been opened, but not read from.
!          2, data has been read from a file.
! 
!  IPLOT  Input, integer ( kind = 4 ) IPLOT.
!         The number of plots made so far.
!
  character ( len = * )  dev
  integer ( kind = 4 ) ifile
  integer ( kind = 4 ) iplot
  logical s_eqi
!
  write(*,*)' '
  write(*,*)'DISPLAY - Note.'
  write(*,*)'  DISPLAY is stopping now.'
  write(*,*)'  A copy of your commands is in "display.inp".'

  if ( iplot > 0 ) then
    call grfcls
  end if

  if ( ifile > 0 ) then
    close(unit = 2)
  end if

  if ( .not. s_eqi ( dev, 'CGMB' ) ) then
    call delete('cgmout')
  end if

  close(unit = 17)

  stop
end
subroutine rdelj ( filelm,ifile,maxelm,maxnp,maxnpe,nelem,node,np,npe)
!
!***********************************************************************
!
!! RDELJ reads a JEFF element file.
!
!  A JEFF element file contains information about the
!  organization of the nodes into elements.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
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
!  FILELM Input, character ( len = 80 ) FILELM, the name of the file 
!         containing the element information.
!
!  IFILE  Output, integer ( kind = 4 ) IFILE.
!         Records the status of the data file whose name is FILDAT.
!   
!         -2, an error occurred while reading from the file.
!         -1, the file could not be opened.
!          0, no file is currently open.
!          1, a file has been opened, but not read from.
!          2, data has been read from a file.
! 
!  MAXELM Input, integer ( kind = 4 ) MAXELM, the maximum number of elements
!         allowed.
!
!  MAXNP  Input, integer ( kind = 4 ) MAXNP, the maximum number of nodes
!         allowed.
!
!  MAXNPE Input, integer ( kind = 4 ) MAXNPE.
!         MAXNPE is the maximum number of nodes per element.
!
!  NELEM  Output, integer ( kind = 4 ) NELEM, the number of elements.
!
!  NODE   Output, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), or NODE(MAXNPE,NELEM).
!         NODE contains, for each element, the global node numbers
!         of its NPE nodes.  
!
!         For linear elements (NPE = 3), the order of the nodes probably
!         doesn't matter, but we will draw them this way:
!
!                       2
!                      /|
!                    /  |
!                  /    |
!                /      |
!               1-------3
!
!         For quadratic elements (NPE = 6), the nodes must be given in a 
!         particular order, which is as follows:
!
!                       2
!                      /|
!                    /  |
!                  4    5
!                /      |
!               1---6---3
! 
!  NP     Output, integer ( kind = 4 ) NP, the number of nodes.
!
!  NPE    Input, integer ( kind = 4 ) NPE.
!         NPE is the number of nodes per element, which should be
!         3 for linear elements and 6 for quadratics.
!
  integer ( kind = 4 ) maxelm
  integer ( kind = 4 ) maxnpe
!
  character ( len = 80 ) filelm
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ielem
  integer ( kind = 4 ) ifile
  integer ( kind = 4 ) itemp(6)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) maxnp
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) node(maxnpe,maxelm)
  integer ( kind = 4 ) np
  integer ( kind = 4 ) npe
!
!  Open the data file.
!
  open(unit = 2,file=filelm,status='old',err=50)
!
!  Initialize number of elements, number of nodes.
!
  ielem = 0
  np = 0
!
!  Read the number of nodes per element.
!
  read(2,*,end = 30,err=40)nelem

  if ( nelem <= 0 ) then
    write(*,*)' '
    write(*,*)'RDELJ - Fatal error!'
    write(*,*)'  This problem has zero elements!'
    stop
  end if

  if ( nelem > maxelm ) then
    write(*,*)' '
    write(*,*)'RDELJ - Fatal error!'
    write(*,*)'  This problem has too many elements!'
    write(*,*)'  Number of elements NELEM  =  ',nelem
    write(*,*)'  DISPLAY can handle up to MAXELM  =  ',maxelm
    stop
  end if

  read(2,*,end = 30,err=40)npe

  if ( npe /= 3.and.npe /= 4.and.npe /= 6 ) then
    write(*,*)' '
    write(*,*)'RDELJ - Fatal error!'
    write(*,*)'  Legal values of NPE are 3, 4 and 6.'
    write(*,*)'  User input value is NPE  =  ',npe
    stop
  end if
!
!  Try to read the next line of the file.
!
10    continue

  read(2,*,end = 30,err=40)(itemp(j),j=1,npe)

  ielem = ielem+1

  if ( ielem > nelem ) then
    write(*,*)' '
    write(*,*)'RDELJ - Fatal error!'
    write(*,*)'  This problem has too many elements!'
    write(*,*)'  Current element number IELEM  =  ',ielem
    write(*,*)'  Declared number of elements NELEM  =  ',nelem
    stop
  end if

  do j = 1,npe

    if ( itemp(j) <= 0 ) then
      write(*,*)' '
      write(*,*)'RDELJ - Fatal error!'
      write(*,*)'  Element ',ielem,' has an illegal node.'
      write(*,*)'  Local node number ',j,' has index ',itemp(j)
      stop
    end if

  end do

  do i = 1,npe
    np = max(np,itemp(i))
    node(i,ielem) = itemp(i)
  end do

  go to 10
!
!  End of file.
!
30    continue

  close(unit = 2)

  if ( nelem /= ielem ) then
    write(*,*)' '
    write(*,*)'RDELJ - Fatal error!'
    write(*,*)'  Number of elements declared was NELEM  =  ',nelem
    write(*,*)'  Number of elements was IELEM  =  ',ielem
    stop
  end if

  if ( np > maxnp ) then
    write(*,*)' '
    write(*,*)'RDELJ - Fatal error!'
    write(*,*)'  This problem has too many nodes!'
    write(*,*)'  Number of nodes NP  =  ',np
    write(*,*)'  DISPLAY can handle up to MAXNP  =  ',maxnp
    stop
  end if

  if ( np <= 0 ) then
    write(*,*)' '
    write(*,*)'RDELJ - Fatal error!'
    write(*,*)'  This problem has NP  =  ',np
    write(*,*)'  but the number of nodes must be positive!'
    stop
  end if
!
!  All seems well.
!
  ifile = 2

  write(*,*)' '
  write(*,*)'RDELJ - Note:'
  write(*,*)'  There are NELEM  =  ',nelem,' elements,'
  write(*,*)'  and NP  =  ',np,' nodes.'

  return
!
!  Error while reading.
!
40    continue
  close(unit = 2)
  write(*,*)' '
  write(*,*)'RDELJ - Fatal error!'
  write(*,*)'  A READ "ERR" condition occurred.'
  stop
!
!  Could not open file.
!
50    continue
  write(*,*)' '
  write(*,*)'RDELJ - Fatal error!'
  write(*,*)'  Could not open the element file!'
  stop
end
subroutine rdnod ( fildat,jfile,np,p,rho,u,v,xc,yc)
!
!***********************************************************************
!
!! RDNOD reads a JEFF file containing information about data at nodes.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
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
!    Input, character ( len = * )  FILDAT, the file containing the data.
!    This file should be a "plain text" file.
!
!  JFILE  Output, integer ( kind = 4 ) JFILE, an error indicator.
!         JFILE is 2 if the file was successfully opened and read.
!         (If not, then this routine actually halts execution!)
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!  P,
!  U,
!  V,
!  XC,
!  YC     Output, real P(NP), U(NP), V(NP), XC(NP), YC(NP).
!         The pressure, horizontal and vertical velocities,
!         and X and Y coordinates of each node.
!
  integer ( kind = 4 ) np
!
  character ( len = 80 ) fildat
  integer ( kind = 4 ) i
  integer ( kind = 4 ) jfile
  real p(np)
  real rho(np)
  real u(np)
  real v(np)
  real xc(np)
  real yc(np)
!
  open(unit = 2,file=fildat,status='old',err=20)

  do i = 1,np
    read(2,*,end = 30)xc(i),yc(i),u(i),v(i),p(i)
    rho(i) = 1.0
  end do

  close(unit = 2)

  write(*,*)' '
  write(*,*)'RDNOD - Note:'
  write(*,*)'  Read the node data.'

  jfile = 2

  return

20    continue
  write(*,*)' '
  write(*,*)'RDNOD - Fatal error!'
  write(*,*)'  Could not open the node file.'
  stop

30    continue
  write(*,*)' '
  write(*,*)'RDNOD - Fatal error!'
  write(*,*)'  READ error while reading node data.'
  stop

end
subroutine rdtec ( cp,filelm,ifile,maxelm,maxnp,maxnpe,nelem,node, &
  np,npe,nx,ny,rho,rmach,u,v,xc,yc)
!
!***********************************************************************
!
!! RDTEC reads a TECPLOT data file.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
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
  integer ( kind = 4 ) maxelm
  integer ( kind = 4 ) maxnp
  integer ( kind = 4 ) maxnpe
!
  real cp(maxnp)
  character ( len = 80 ) filelm
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ielem
  integer ( kind = 4 ) ifile
  integer ( kind = 4 ) ip
  integer ( kind = 4 ) j
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) node(maxnpe,maxelm)
  integer ( kind = 4 ) np
  integer ( kind = 4 ) npe
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny
  real rho(maxnp)
  real rmach(maxnp)
  real u(maxnp)
  real v(maxnp)
  real xc(maxnp)
  real yc(maxnp)
!
!  Open the data file.
!
  open(unit = 2,file=filelm,status='old',err=50)
!
!  Initialize number of elements, number of nodes.
!
  ielem = 0
  np = 0
!
!  Skip the title line.
!
  read(2,*)
  read(2,*)
  read(2,'(22x,i7,6x,i7)')nx,ny

  write(*,*)'NX = ',nx
  write(*,*)'NY = ',ny

  np = nx*ny

  if ( np > maxnp ) then
    write(*,*)' '
    write(*,*)'RDTEC - Fatal error!'
    write(*,*)'  This problem has too many nodes!'
    write(*,*)'  Number of nodes NP  =  ',np
    write(*,*)'  DISPLAY can handle up to MAXNP  =  ',maxnp
    stop
  end if

  nelem = (nx-1)*(ny-1)

  if ( nelem > maxelm ) then
    write(*,*)' '
    write(*,*)'RDTEC - Fatal error!'
    write(*,*)'  This problem has too many elements!'
    write(*,*)'  Number of elements NELEM  =  ',nelem
    write(*,*)'  DISPLAY can handle up to MAXELM  =  ',maxelm
    stop
  end if

  npe = 4
!
!  Make up elements.
!
  ielem = 0
  do i = 1,nx-1
    do j = 1,ny-1

      ielem = ielem+1
      node(1,ielem) = j      +ny*(i-1)
      node(2,ielem) = j+1    +ny*(i-1)
      node(3,ielem) = j+1+ny +ny*(i-1)
      node(4,ielem) = j  +ny +ny*(i-1)

    end do
  end do
!
!  Read data, and reorganize to my node numbering.
!
  do j = 1,ny
    do i = 1,nx
      ip = j+(i-1)*ny
      read(2,*)xc(ip),yc(ip),rho(ip),u(ip),v(ip),cp(ip),rmach(ip)
    end do
  end do

  close(unit = 2)
!
!  All seems well.
!
  ifile = 2

  write(*,*)' '
  write(*,*)'RDTEC - Note:'
  write(*,*)'  There are NELEM  =  ',nelem,' elements,'
  write(*,*)'  and NP  =  ',np,' nodes.'

  return
!
!  Could not open file.
!
50    continue
  write(*,*)' '
  write(*,*)'RDTEC - Fatal error!'
  write(*,*)'  Could not open the element file!'
  stop
end
subroutine rrange ( n,x,xmax,xmin)
!
!***********************************************************************
!
!! RRANGE returns the maximum and minimum values in an array.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
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
!  N      Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!  X      Input, real X(N), the array.
!
!  XMAX,
!  XMIN   Output, real XMAX, XMIN, the maximum and minimum values
!         that occur in the array.
!
  integer ( kind = 4 ) n
!
  integer ( kind = 4 ) i
  real x(n)
  real xmax
  real xmin
!
  xmax = x(1)
  xmin = x(1)
  do i = 2,n
    xmax = max(x(i),xmax)
    xmin = min(x(i),xmin)
  end do

  return
end
subroutine region_size ( delx, dely, grace, nelem, nflag, np, srange, x1max, &
  x1min, x2max, x2min, xc, xmax, xmin, xsmax, xsmin, xtmax, xtmin, y1max, &
  y1min, y2max, y2min, yc, ymax, ymin, ysmax, ysmin, ytmax, ytmin )
!
!***********************************************************************
!
!! REGION_SIZE computes the size of the region.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 January 2001
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
!    Input, real GRACE.
!    The size of the "grace" margin on the plot.
! 
!    Input, integer ( kind = 4 ) NELEM.
!         The number of elements.
! 
!  NFLAG  Output, logical NFLAG(MAXNP).
!
!         NFLAG is used to "flag" which nodes are active,
!         that is, to be displayed, and which not, in the graph.
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!  SRANGE Output, real SRANGE, the maximum of the height and width
!         of the data in physical coordinates.
!
!           SRANGE  =  MAX( XSMAX-XSMIN, YSMAX-YSMIN )
!
!  X1MAX,
!  X1MIN  Output, real X1MAX, X1MIN, the maximum and minimum X 
!         coordinates of the plot, which includes a small grace margin.
!
!  X2MAX,
!  X2MIN  Output, real X2MAX, X2MIN, the maximum and minimum X 
!         coordinates that should be used for plotting.  No plotting
!         commands should  exceed these values.  This is where the 
!         "frame" might be drawn.
!
!  XC     Input, real XC(MAXNP).
!         The X coordinates of the nodes.
! 
!  XMAX   Output, real XMAX.
!         The maximum X coordinate of all the nodes.
!         The maximum entry in the XC array.
! 
!  XMIN   Output, real XMIN.
!         The minimum X coordinate of all the nodes.
!         The minimum entry in the XC array.
! 
!  XSMAX  Output, real XSMAX.
!         The maximum X coordinate of the data to be displayed.
!         XSMAX defaults to XMAX, but can be made smaller to
!         focus on a portion of the region.
! 
!  XSMIN  Output, real XSMIN.
!         The minimum X coordinate of the data to be displayed.
!         XSMIN defaults to XMIN, but can be made larger to
!         focus on a portion of the region.
! 
!  Y1MAX,
!  Y1MIN  Output, real Y1MAX, Y1MIN, the maximum and minimum Y 
!         coordinates of the plot, which includes a small grace margin.
!
!  Y2MAX,
!  Y2MIN  Output, real Y2MAX, Y2MIN, the maximum and minimum Y 
!         coordinates that should be used for plotting.  No plotting 
!         commands should exceed these values.  This is where the 
!         "frame" might be drawn.
!  
!  YC     Input, real YC(MAXNP).
!         The Y coordinates of the nodes.
! 
!  YMAX   Output, real YMAX.
!         The maximum Y coordinate of all the nodes.
!         The maximum value attained by the YC array.
! 
!  YMIN   Output, real YMIN.
!         The minimum Y coordinate of all the nodes.
!         The minimum value attained by the YC array.
!  
!  YSMAX  Output, real YSMAX.
!         The maximum Y coordinate of the data to be displayed.
!         YSMAX defaults to YMAX, but can be made smaller to
!         focus on a portion of the region.
! 
!  YSMIN  Output, real YSMIN.
!         The minimum Y coordinate of the data to be displayed.
!         YSMIN defaults to YMIN, but can be made larger to
!         focus on a portion of the region.
! 
  integer ( kind = 4 ) np
!
  real delx
  real dely
  real grace
  integer ( kind = 4 ) iwant
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
  delx = sqrt((xmax-xmin)*(ymax-ymin)/nelem)
  dely = delx

  write(*,*) ' '
  write(*,*) 'RSIZE:'
  write(*,*) '  Total physical data coordinates:'
  write(*,*) ' '
  write(*,*) xtmin,' =  XTMIN <= X <= XTMAX = ',xtmax
  write(*,*) ytmin,' =  YTMIN <= Y <= YTMAX = ',ytmax
  write(*,*) ' '
  write(*,*) '  Visible physical data coordinates:'
  write(*,*) ' '
  write(*,*) xmin,' =  XMIN <= X <= XMAX = ',xmax
  write(*,*) ymin,' =  YMIN <= Y <= YMAX = ',ymax
!
!  In order to allow display of data from repeated steps with a
!  fixed window, we will only reset the window if it hasn't been
!  set already.
!
  if ( xsmin /= xsmax .and. ysmin /= ysmax .and. &
       xmin <= xsmax .and. xsmin <= xmax .and. &
       ymin <= ysmax .and. ysmin <= ymin ) then

    write ( *, * ) ' '
    write ( *, * ) 'REGION_SIZE:'
    write ( *, * ) '  0 to keep region size from previous plot.'
    write ( *, * ) '  1 to recompute region size from current data.'
    read ( *, * ) iwant
    if ( iwant == 0 ) then
      return
    end if
  end if

  write(*,*)' '
  write(*,*)'RSIZE - Note:'
  write(*,*)'  Setting data window to physical window!'
!
!  Data window starts out the same as physical window.
!
  xsmax = xmax
  xsmin = xmin
  ysmax = ymax
  ysmin = ymin
!
!  Compute box containing data.
!
  call pltbox(grace,srange,x1max,x1min,x2max,x2min,xsmax,xsmin, &
    y1max,y1min,y2max,y2min,ysmax,ysmin)

  return
end
subroutine s_blank_delete ( s )
!
!*******************************************************************************
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
  character c
  integer ( kind = 4 ) iget
  integer ( kind = 4 ) iput
  integer ( kind = 4 ) nchar
  character ( len = * ) s
  character, parameter :: TAB = char ( 9 )
!
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
subroutine s_blanks_delete ( s )
!
!*******************************************************************************
!
!! S_BLANKS_DELETE replaces consecutive blanks by one blank.
!
!  Discussion:
!
!    The remaining characters are left justified and right padded with blanks.
!    TAB characters are converted to spaces.
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
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  character newchr
  character oldchr
  character ( len = * ) s
  character, parameter :: TAB = char ( 9 )
!
  j = 0
  newchr = ' '

  do i = 1, len ( s )

    oldchr = newchr
    newchr = s(i:i)

    if ( newchr == TAB ) then
      newchr = ' '
    end if

    s(i:i) = ' '

    if ( oldchr /= ' ' .or. newchr /= ' ' ) then
      j = j + 1
      s(j:j) = newchr
    end if

  end do

  return
end
function s_eqi ( s1, s2 )
!
!*******************************************************************************
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
  character c1
  character c2
  integer ( kind = 4 ) i
  integer ( kind = 4 ) len1
  integer ( kind = 4 ) len2
  integer ( kind = 4 ) lenc
  logical s_eqi
  character ( len = * ) s1
  character ( len = * ) s2
!
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
!
!    'L' - the string will be drawn flush left.
!    'C' - the string will be centered.
!    'R' - the string will be drawn flush right.
!
  real, parameter :: PI = 3.1415926535
  real, parameter :: DEG_TO_RAD = PI / 180.0
!
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
  scl2 = cwide / 16.0
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
  if ( angle == 0.0 ) then
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
subroutine serene ( type,ve,vn,vne,vnw,vs,vse,vsw,vw,vterp)
! 
!***********************************************************************
!
!! SERENE evaluates an interpolating function which is defined on
!  a serendipity quadrilateral, or on a section of one.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
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
!  TYPE   Input, character ( len = 2 )  TYPE, tells SERENE the geometry of the
!         finite element that surrounds the point of interest.  The options 
!         are displayed in the following table, which suggests the meaning
!         of each option by its position:
!
!             |   |
!           NW* N * NE
!             |   |
!          -*-*-*-*-*-
!             |   |
!           W * C * E
!             |   |
!          -*-*-*-*-*-
!             |   |
!           SW* S * SE
!             |   |
!
!  VE,
!  VN,
!  VNE,
!  VNW,
!  VS,
!  VSE,
!  VSW,
!  VW     Input, real VE, VN, VNE, VNW, VS, VSE, VSW, VW,
!         are the values of the function at the nodes to the east,
!         north, northeast, northwest, south, southeast, southwest and
!         west of the point of interest.  if the finite element is of
!         type 'C', then all 8 values are needed.  However, if the
!         finite element is of type 'SE', for instance, then only three
!         values are needed, namely VE, VN, and VNW, since these are
!         the only node positions defined in such a finite element.
!
!  VTERP  Output, real VTERP, the interpolated value of the
!         function at the point of interest.
!
  real eta
  real pe
  real pn
  real pne
  real pnw
  real ps
  real pse
  real psw
  real pw
  character ( len = 2 )  type
  real ve
  real vn
  real vne
  real vnw
  real vs
  real vse
  real vsw
  real vw
  real vterp
  real xsi
!
!  To make this routine more general, simply pass in the
!  values of XSI and ETA at which the interpolated value 
!  is desired.  By setting XSI = ETA=0, we are asking for the interpolated
!  value at the center of the finite element.
!  
!
  xsi = 0.0
  eta = 0.0
!
!  8 node center
!
!  Polynomial space is spanned by:
!         1
!       x    y
!    x^2  xy  y^2
!      x^2y xy^2
!
!
!    ^   1    4--7--3
!    |        !     !
!    E        !     !
!    T   0    8  X  6
!    A        !     !
!    |        !     !
!    V  -1    1--5--2
!
!            -1  0  1
!
!           <---XSI--->
!
  if ( type == 'C' ) then

    psw = -0.25*(1.0-xsi)*(1.0-eta)*(1.0+xsi+eta)
    pse = -0.25*(1.0+xsi)*(1.0-eta)*(1.0-xsi+eta)
    pne = -0.25*(1.0+xsi)*(1.0+eta)*(1.0-xsi-eta)
    pnw = -0.25*(1.0-xsi)*(1.0+eta)*(1.0+xsi-eta)
    ps =  0.5*(1.0-xsi)*(1.0+xsi)*(1.0-eta)
    pe =  0.5*(1.0+xsi)*(1.0+eta)*(1.0-eta)
    pn =  0.5*(1.0-xsi)*(1.0+xsi)*(1.0+eta)
    pw =  0.5*(1.0-xsi)*(1.0+eta)*(1.0-eta)
!
!  5 node side
!
!    ^   1    
!    |        
!    E        
!    T   0    8  X  6
!    A        !     !
!    |        !     !
!    V  -1    1--5--2
!
!            -1  0  1
!
!           <---XSI--->
!
  else if ( type == 'N' ) then

    psw =  0.5*(xsi-1.0)*(1.0+xsi+eta)
    pse = -0.5*(xsi+1.0)*(1.0-xsi+eta)
    ps =  -(xsi+1.0)*(xsi-1.0)
    pe =  0.5*(xsi+1.0)*(eta+1.0)
    pw = -0.5*(xsi-1.0)*(eta+1.0)
!
!    ^   1    4--7
!    |        !   
!    E        !   
!    T   0    8  X
!    A        !   
!    |        !   
!    V  -1    1--5
!
!            -1  0  1
!
!           <---XSI--->
!
  else if ( type == 'E' ) then

    pse =  0.5*(eta-1.0)*(1.0+xsi+eta)
    pne = -0.5*(eta+1.0)*(1.0+xsi-eta)
    ps =  -0.5*(xsi+1.0)*(eta-1.0)
    pn =   0.5*(xsi+1.0)*(eta+1.0)
    pw =  -(eta+1.0)*(eta-1.0)
!
!  5 node side
!
!    ^   1       7--3
!    |              !
!    E              !
!    T   0       X  6
!    A              !
!    |              !
!    V  -1       5--2
!
!            -1  0  1
!
!           <---XSI--->
!
  else if ( type == 'W' ) then

    pse =  0.5*(eta-1.0)*(1.0-xsi+eta)
    pne = -0.5*(eta+1.0)*(1.0-xsi-eta)
    ps =   0.5*(xsi-1.0)*(eta-1.0)
    pe =  -(eta-1.0)*(eta+1.0)
    pn =  -0.5*(xsi-1.0)*(eta+1.0)
!
!  5 node side
!
!    ^   1    4--7--3
!    |        !     !
!    E        !     !
!    T   0    8  X  6
!    A               
!    |               
!    V  -1     
!
!            -1  0  1
!
!           <---XSI--->
!
  else if ( type == 'S' ) then

    pne = -0.5*(xsi+1.0)*(1.0-xsi-eta)
    pnw =  0.5*(xsi-1.0)*(1.0+xsi-eta)
    pe =  -0.5*(eta-1.0)*(xsi+1.0)
    pn =  -(xsi+1.0)*(xsi-1.0)
    pw =   0.5*(eta-1.0)*(xsi-1.0)
!
!  3 node corner
!
!  Polynomial space is spanned by:
!         1
!       x    y
!
!
!    ^   1    
!    |        
!    E        
!    T   0    8  X
!    A        !   
!    |        !  
!    V  -1    1--5
!
!            -1  0  1
!
!           <---XSI--->
!
  else if ( type == 'NE' ) then

    psw = -(1.0+xsi+eta)
    ps =  (xsi+1.0)
    pw =  (eta+1.0)
!
!  3 node corner
!
!  Polynomial space is spanned by:
!         1
!       x    y
!
!    ^   1    
!    |         
!    E        
!    T   0       X  6
!    A              !
!    |              !
!    V  -1       5--2
!
!            -1  0  1
!
!           <---XSI--->
!
  else if ( type == 'NW' ) then

    pse = -(1.0-xsi+eta)
    ps = -(xsi-1.0)
    pe =  (eta+1.0)
!
!  3 node corner
!
!  Polynomial space is spanned by:
!         1
!       x    y
!
!
!    ^   1    4--7
!    |        !   
!    E        !  
!    T   0    8  X
!    A        
!    |        
!    V  -1    
!
!            -1  0  1
!
!           <---XSI--->
!
  else if ( type == 'SE' ) then

    pnw = -(1.0+xsi-eta)
    pn =   (xsi+1.0)
    pw =  -(eta-1.0)
!
!  3 node corner
!
!  Polynomial space is spanned by:
!         1
!       x    y
!
!    ^   1       7--3
!    |              !
!    E              !
!    T   0       X  6
!    A           
!    |           
!    V  -1    
!
!            -1  0  1
!
!           <---XSI--->
!
  else if ( type == 'SW' ) then

    pne = -(1.0-xsi-eta)
    pe =  -(eta-1.0)
    pn =  -(xsi-1.0)

  end if
!
  vterp = vsw*psw+vse*pse+vne*pne+vnw*pnw+vs*ps+ve*pe+vn*pn+vw*pw

  return
end
subroutine setref ( etaref,maxnpe,npe,xsiref)
!
!***********************************************************************
!
!! SETREF returns the values of XSI and ETA for the defining nodes
!  of a six node isoparametric element.
!
!      ^
!      |        2
!      |       /|
!  ETA |      4 5
!      |     /  |
!      |    1-6-3
!      |  
!      +------------>
!           XSI
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
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
!  ETAREF Output, real ETAREF(MAXNPE).
!         The ETA coordinates of the nodes of the reference
!         triangle.
! 
!  MAXNPE Input, integer ( kind = 4 ) MAXNPE.
!         MAXNPE is the maximum number of nodes per element.
!
!  NPE    Input, integer ( kind = 4 ) NPE.
!         NPE is the number of nodes per element, which should be
!         3 for linear elements and 6 for quadratics.
!
!  XSIREF Output, real XSIREF(MAXNPE).
!         The XSI coordinates of the nodes of the reference
!         triangle.
! 
  integer ( kind = 4 ) maxnpe
!
  real etaref(maxnpe)
  integer ( kind = 4 ) npe
  real xsiref(maxnpe)
!
  if ( npe == 3 ) then

    xsiref(1) = 0.0
    etaref(1) = 0.0

    xsiref(2) = 1.0
    etaref(2) = 1.0

    xsiref(3) = 1.0
    etaref(3) = 0.0

  else if ( npe == 4 ) then

    xsiref(1) = 0.0
    etaref(1) = 0.0

    xsiref(2) = 0.0
    etaref(2) = 1.0

    xsiref(3) = 1.0
    etaref(3) = 1.0

    xsiref(4) = 1.0
    etaref(4) = 0.0

  else if ( npe == 6 ) then

    xsiref(1) = 0.0
    etaref(1) = 0.0

    xsiref(2) = 1.0
    etaref(2) = 1.0

    xsiref(3) = 1.0
    etaref(3) = 0.0

    xsiref(4) = 0.5
    etaref(4) = 0.5

    xsiref(5) = 1.0
    etaref(5) = 0.5

    xsiref(6) = 0.5
    etaref(6) = 0.0

  else

    write(*,*)' '
    write(*,*)'SETREF - Fatal error!'
    write(*,*)'  Legal values of NPE are 3, 4 and 6.'
    write(*,*)'  Input value of NPE is ',npe
    stop

  end if

  return
end
subroutine setsiz ( echo,name,smax,smin,stmax,stmin,svmax,svmin)
!
!***********************************************************************
!
!! SETSIZ offers the user a chance to adjust the range of a contour plot.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
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
!  NAME   Input, character ( len = * ) , the name of the variable.
!
!  SMAX,
!  SMIN   Input/output, real SMAX, SMIN.  
!
!         On input, SMAX and SMIN are the maximum and minimum values
!         attained by the variable.
!
!         On output, SMAX and SMIN may have been changed by the
!         user to more suitable values.
!
  logical echo
  character isay
  logical s_eqi
  character ( len = * )  name
  real smax
  real smin
  real stmax
  real stmin
  real svmax
  real svmin
!
!
  write(*,*)' '
  write(*,'(1x,a)')name
  write(*,*)' '
  write(*,*)'SETSIZ - Input request:'
  write(*,*)'  Previous range was    ',smin, ' to ',smax
  write(*,*)'  Total data range is   ',stmin,' to ',stmax
  write(*,*)'  Visible data range is ',svmin,' to ',svmax
  write(*,*)' '
  write(*,*)'  T = total (default), V=visible, P=previous, U=user'
  read(*,'(a)',err = 10,end=10)isay
  write(17,'(a)')isay
  if ( echo ) then
    write(*,'(a)')isay
  end if

  if ( s_eqi ( isay,'u') ) then
    write(*,*)' '
    write(*,*)'  Enter new minimum and maximum values for contours.'
    read(*,*,end = 10,err=10)smin,smax
    write(17,*)smin,smax
    if ( echo ) then
      write(*,*)smin,smax
    end if
  else if ( s_eqi ( isay,'v') ) then
    smin = svmin
    smax = svmax
  else if ( s_eqi ( isay,'p') ) then

  else
    smin = stmin
    smax = stmax
  end if

  write(*,*)'  Current range is    ',smin, ' to ',smax

  return
 
10    continue

  write(*,*)' '
  write(*,*)'SETSIZ - Warning!'
  write(*,*)'  There was trouble reading your input,'
  write(*,*)'  so it was ignored.'
  smax = stmax
  smin = stmin
 
  return
end
subroutine settab ( echo,icmax,icmin,itable)
!
!***********************************************************************
!
!! SETTAB replaces SETCTB, the DRAWCGM routine for setting up the color tables.
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
!  ICMAX  Input, integer ( kind = 4 ) ICMAX, the maximum color index to be set.
!
!  ICMIN  Input, integer ( kind = 4 ) ICMIN, the minimum color index to be set.
!
!  ITABLE Input, integer ( kind = 4 ) ITABLE, the desired table.
!
!         1: low black to high white
!         2: low blue to high yellow
!         3: low red, high blue, with bands between.
!         4: low red, yellow, green, blue, high white.
!         5: low white, blue, green, yellow, high red.
!         6: low blue to high red
!         7: linear table between 2 user colors.
!         8: linear table between n user colors.
!         9: low white to high black.
!
  real pi
  parameter (pi = 3.1415926)
!
  real bhi
  real blo
  real bval
  logical echo
  real ghi
  real glo
  real gval
  integer ( kind = 4 ) i
  integer ( kind = 4 ) icmax
  integer ( kind = 4 ) icmin
  integer ( kind = 4 ) icol1
  integer ( kind = 4 ) icol2
  integer ( kind = 4 ) itable
  real rhi
  real rlo
  real rval
  real theta
!
1996  continue
!
!  1: Low black to high white
!
  if ( itable == 1 ) then
    do i = icmin,icmax
      bval = real(i-icmin)/real(icmax-icmin)
      gval = real(i-icmin)/real(icmax-icmin)
      rval = real(i-icmin)/real(icmax-icmin)
      call setclr(i,bval,gval,rval)
    end do
!
!  2: Low blue to high yellow.
!
  else if ( itable == 2 ) then
    do i = icmin,icmax
      rval = real(i-icmin)/real(icmax-icmin)
      gval = real(i-icmin)/real(icmax-icmin)
      bval = (icmax-i)/real(icmax-icmin)
      call setclr(i,bval,gval,rval)
    end do
!
!  3: Low red, high blue, with bands between.
!
  else if ( itable == 3 ) then
    do i = icmin,icmax
      theta = 0.5*pi*real(i-icmin)/real(icmax-icmin)
      rval = cos(theta)**2
      bval = sin(theta)**2
      gval = 0.8*sin(10.0*theta)**6
      call setclr(i,bval,gval,rval)
    end do
!
!  4: Low red, yellow, green, blue, high white.
!
  else if ( itable == 4 ) then
    do i = icmin,icmax
      theta = 4.0*real(i-icmin)/real(icmax-icmin)
      rval = exp(-(theta-1.0)**2)+exp(-(theta-4.0)**2)
      gval = exp(-(theta-2.0)**2)+exp(-(theta-4.0)**2)
      bval = exp(-(theta-3.0)**2)+exp(-(theta-4.0)**2)
      if ( rval > 1.0)rval = 1.0
      if ( gval > 1.0)gval = 1.0
      if ( bval > 1.0)bval = 1.0
      call setclr(i,bval,gval,rval)
    end do
!
!  5: Low white, blue, green, yellow, high red.
!
  else if ( itable == 5 ) then
    do i = icmin,icmax
      theta = 4.0*real(icmax-i)/real(icmax-icmin)
      rval = exp(-(theta-1.0)**2)+exp(-(theta-4.0)**2)
      gval = exp(-(theta-2.0)**2)+exp(-(theta-4.0)**2)
      bval = exp(-(theta-3.0)**2)+exp(-(theta-4.0)**2)
      if ( rval > 1.0)rval = 1.0
      if ( gval > 1.0)gval = 1.0
      if ( bval > 1.0)bval = 1.0
      call setclr(i,bval,gval,rval)
    end do
!
!  6: Low blue to high red
!
  else if ( itable == 6 ) then
    do i = icmin,icmax
      rval = real(i-icmin)/real(icmax-icmin)
      gval = 0.0
      bval = (icmax-i)/real(icmax-icmin)
      call setclr(i,bval,gval,rval)
    end do
!
!  7: Interpolate between two values.
!
  else if ( itable == 7 ) then

    write(*,*)' '
    write(*,*)'SETTAB - Input request:'
    write(*,*)' '
    write(*,*)'  Enter  Rlo, Glo, Blo,   Rhi, Ghi, Bhi'
    write(*,*)'  Note: 0,0,0 is black, and 1,1,1 is white!'
    write(*,*)' '

    read(*,*,end = 1952,err=1964)rlo,glo,blo,rhi,ghi,bhi

    write(17,*)rlo,glo,blo,rhi,ghi,bhi
    if ( echo ) then
      write(*,*)rlo,glo,blo,rhi,ghi,bhi
    end if
    if ( rlo < 0.0)rlo = 0.0
    if ( rhi > 1.0)rhi = 1.0
    if ( glo < 0.0)glo = 0.0
    if ( ghi > 1.0)ghi = 1.0
    if ( blo < 0.0)blo = 0.0
    if ( bhi > 1.0)bhi = 1.0
    do i = icmin,icmax
      rval = (rlo*(icmax-i)+rhi*(i-icmin))/real(icmax-icmin)
      gval = (glo*(icmax-i)+ghi*(i-icmin))/real(icmax-icmin)
      bval = (blo*(icmax-i)+bhi*(i-icmin))/real(icmax-icmin)
      call setclr(i,bval,gval,rval)
    end do
!
!  8: Interpolate between several values.
!
  else if ( itable == 8 ) then

    icol1 = icmin
    write(*,*)' '
    write(*,*)'SETTAB - Input request:'
    write(*,*)' '
    write(*,*)'  Enter (R, G, B) for color index ',icol1
    write(*,*)'      (0, 0, 0) is black.'
    write(*,*)'      (1, 1, 1) is white.'
    read(*,*)rlo,glo,blo
    write(17,*)rlo,glo,blo
    if ( echo ) then
      write(*,*)rlo,glo,blo
    end if
    if ( rlo < 0.0)rlo = 0.0
    if ( glo < 0.0)glo = 0.0
    if ( blo < 0.0)blo = 0.0

10      continue

    write(*,*)'  Enter index of next color to set'
    write(*,*)'  between ',icol1+1,' and ',icmax
    read(*,*)icol2
    write(17,*)icol2
    if ( echo ) then
      write(*,*)icol2
    end if

    if ( icol2 <= icol1.or.icol2 > icmax ) then
      write(*,*)' '
      write(*,*)'SETTAB - Warning!'
      write(*,*)'  Your color index was not accepted!'
      go to 10
    end if

    write(*,*)' '
    write(*,*)'Enter (R, G, B) for color index ',icol2
    read(*,*)rhi,ghi,bhi
    write(17,*)rhi,ghi,bhi
    if ( echo ) then
      write(*,*)rhi,ghi,bhi
    end if

    if ( rhi > 1.0)rhi = 1.0
    if ( ghi > 1.0)ghi = 1.0
    if ( bhi > 1.0)bhi = 1.0

    do i = icol1,icol2
      rval = (rlo*(icol2-i)+rhi*(i-icol1))/real(icol2-icol1)
      gval = (glo*(icol2-i)+ghi*(i-icol1))/real(icol2-icol1)
      bval = (blo*(icol2-i)+bhi*(i-icol1))/real(icol2-icol1)
      call setclr(i,bval,gval,rval)
    end do

    if ( icol2 < icmax ) then
      icol1 = icol2
      rlo = rhi
      glo = ghi
      blo = bhi
      go to 10
    end if
!
!  9: Low white to high black
!
  else if ( itable == 9 ) then
    do i = icmin,icmax
      bval = real(icmax-i)/real(icmax-icmin)
      gval = real(icmax-i)/real(icmax-icmin)
      rval = real(icmax-i)/real(icmax-icmin)
      call setclr(i,bval,gval,rval)
    end do
!
!  Unknown table.
!
  else
    write(*,*)' '
    write(*,*)'SETTAB - Fatal error!'
    write(*,*)'  Legal color table indices are '
    write(*,*)'  between 1 and 8.  Your value was ',itable
  end if
!
!  Background color 0 is to be white.
!
  i = 0
  rval = 1.0
  gval = 1.0
  bval = 1.0
  call setclr(i,rval,gval,bval)
!
!  Foreground color 1 is to be black.
!
  i = 1
  rval = 0.0
  gval = 0.0
  bval = 0.0
  call setclr(i,rval,gval,bval)
  return

1952  continue
  write(*,*)' '
  write(*,*)'SETTAB - Fatal error!'
  write(*,*)'  Unexpected end of file!'
  stop

1964  continue
  write(*,*)' '
  write(*,*)'SETTAB - Warning!'
  write(*,*)'  Illegal format for input data!'
  write(*,*)'  Try again!'
  go to 1996
end
subroutine stream ( ie,in,maxnpe,nelem,node,np,npe,rho,s,u,v,xc,yc)
!
!***********************************************************************
!
!! STREAM assigns the value of the stream function to each node
!  in a finite element grid, given the horizontal and vertical
!  velocities at those nodes.
!
!  The stream function PSI(X,Y) is related to the mass velocity 
!  (RHO*U,RHO*V) by
!
!    Curl PSI  =  (RHO*U,RHO*V)
!
!  That is,
!
!    RHO*U  =   d PSI/d Y
!    RHO*V  =  -d PSI/d X
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
!      ( (-RHO*V dX/dXSI + RHO*U dY/dXSI) dXSI/dS 
!      + (-RHO*V dX/dETA + RHO*U dY/dETA) dETA/dS ) dS.
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
!  IE     Workspace, integer IE(NELEM).
!
!  IN     Workspace, integer IN(NP).
!
!  MAXNPE Input, integer ( kind = 4 ) MAXNPE.
!         MAXNPE is the maximum number of nodes per element.
! 
!    Input, integer ( kind = 4 ) NELEM, the actual number of elements.
!
!  NODE   Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), or NODE(MAXNPE,NELEM).
!         NODE contains, for each element, the global node numbers
!         of its NPE nodes.  
!
!         For linear elements (NPE = 3), the order of the nodes probably
!         doesn't matter, but we will draw them this way:
!
!                       2
!                      /|
!                    /  |
!                  /    |
!                /      |
!               1-------3
!
!         For quadratic elements (NPE = 6), the nodes must be given in a 
!         particular order, which is as follows:
!
!                       2
!                      /|
!                    /  |
!                  4    5
!                /      |
!               1---6---3
! 
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!  NPE    Input, integer ( kind = 4 ) NPE.
!         NPE is the number of nodes per element, which should be
!         3 for linear elements and 6 for quadratics.
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

    nexnod(1,1) = 4
    nexnod(1,2) = 2
    nexnod(1,3) = 5
    nexnod(1,4) = 3
    nexnod(1,5) = 6
 
    nexnod(2,1) = 5
    nexnod(2,2) = 3
    nexnod(2,3) = 6
    nexnod(2,4) = 1
    nexnod(2,5) = 4
 
    nexnod(3,1) = 6
    nexnod(3,2) = 1
    nexnod(3,3) = 4
    nexnod(3,4) = 2
    nexnod(3,5) = 5
 
    nexnod(4,1) = 2
    nexnod(4,2) = 5
    nexnod(4,3) = 3
    nexnod(4,4) = 6
    nexnod(4,5) = 1
 
    nexnod(5,1) = 3
    nexnod(5,2) = 6
    nexnod(5,3) = 1
    nexnod(5,4) = 4
    nexnod(5,5) = 2
 
    nexnod(6,1) = 1
    nexnod(6,2) = 4
    nexnod(6,3) = 2
    nexnod(6,4) = 5
    nexnod(6,5) = 3

  end if
!
!  Zero out the stream function.
!
  do i = 1,np
    s(i) = 0.0
  end do
!
!  Zero out the vector that records that the stream function
!  has been computed at a given node.
!
  do i = 1,np
    in(i) = 0
  end do
!
!  Zero out the vector that records that the stream function
!  has been computed at all nodes of a given element.
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
  s(iglob) = 0.0
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
  do next = 1,npe-1

    jloc = kloc
    jglob = kglob

    kloc = nexnod(iloc,next)
    kglob = node(kloc,ielem)
!
!  If the next node KLOC in the element does not have its stream
!  function computed, then call GETINC to compute it.
!
    if ( in(kglob) == 0 ) then

      call get_inc(ielem,jloc,maxnpe,nelem,node,np,npe,rho,sinc,u,v,xc,yc)

      s(kglob) = s(jglob)+sinc
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
  do i = 1,nelem

    if ( ie(i) == 0 ) then

      l1 = .false.
      l0 = .false.

      do j = 1,npe
        jglob = node(j,i)
        if ( in(jglob) == 0 ) then
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
!
!*******************************************************************************
!
!! TIMESTAMP prints the current YMDHMS date as a time stamp.
!
!  Example:
!
!    May 31 2001   9:45:54.872 AM
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
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
subroutine tranl3 ( det,detadx,detady,dxdeta,dxdxsi,dxsidx,dxsidy, &
  dydeta,dydxsi,eta,ielem,maxnpe,nelem,node,np,npe,xc,xsi,yc)
!
!***********************************************************************
!
!! TRANL3 calculates the transformation which maps the reference 
!  element in (XSI,ETA) space into a particular (isoparametric) 
!  element in (X,Y) space, in the case of linear elements.
!
!  We know everything about the (isoparametric) element once we
!  specify the location of its three nodes.
!
!  TRANL3 computes the entries of the jacobian of the transformation
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
!  MAXNPE Input, integer ( kind = 4 ) MAXNPE.
!         MAXNPE is the maximum number of nodes per element.
!
!    Input, integer ( kind = 4 ) NELEM, the number of elements.
!
!  NODE   Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), or NODE(MAXNPE,NELEM).
!         NODE contains, for each element, the global node numbers
!         of its NPE nodes.  
!
!         For linear elements (NPE = 3), the order of the nodes probably
!         doesn't matter, but we will draw them this way:
!
!                       2
!                      /|
!                    /  |
!                  /    |
!                /      |
!               1-------3
!
!         For quadratic elements (NPE = 6), the nodes must be given in a 
!         particular order, which is as follows:
!
!                       2
!                      /|
!                    /  |
!                  4    5
!                /      |
!               1---6---3
! 
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!  NPE    Input, integer ( kind = 4 ) NPE.
!         NPE is the number of nodes per element, which should be
!         3 for linear elements and 6 for quadratics.
!
!  XC     Input, real XC(NP), the X coordinates of the nodes.
!
!  XSI    Input, real XSI, the XSI coordinate of the point.
!
!  YC     Input, real YC(NP), the Y coordinates of the nodes.
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
  integer ( kind = 4 ) node(maxnpe,nelem)
  integer ( kind = 4 ) npe
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
  do i = 1,npe
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
!    X(XSI,ETA)  =  A1 * XSI + B1 * ETA + C1
!
!    Y(XSI,ETA)  =  A2 * XSI + B2 * ETA + C2
!  
  a1 =  -x(1)        + x(3)
  b1 =          x(2) - x(3)
  c1 =   x(1)

  xval = a1*xsi+b1*eta+c1

  a2 =  -y(1)        + y(3)
  b2 =          y(2) - y(3)
  c2 =   y(1)

  yval = a2*xsi+b2*eta+c2
!
!  Compute the partial derivatives at the point (XSI,ETA).
!  This is the jacobian matrix 
!
!    J: (XSI,ETA) --> (X,Y).
!
  dxdxsi = a1
  dxdeta = b1

  dydxsi = a2
  dydeta = b2
!
!  Compute the determinant of the jacobian matrix:
!
!    J: (XSI,ETA) --> (X,Y)
!
  det = dxdxsi*dydeta-dxdeta*dydxsi
!
!  Watch out for a zero determinant.
!
  if ( det == 0.0 ) then
    write(*,*)' '
    write(*,*)'TRANL3 - Fatal error!'
    write(*,*)'  The jacobian J: (XSI,ETA) --> (X,Y) is singular!'
    write(*,*)'  This occurred for element number ',ielem
    write(*,*)'  Local coordinates:',xsi,eta
    write(*,*)'  Global coordinates:',xval,yval
    write(*,*)' '
    write(*,*)'  The X, Y nodes were:'
    write(*,*)' '
    do i = 1,npe
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
subroutine tranl4 ( det,detadx,detady,dxdeta,dxdxsi,dxsidx,dxsidy, &
  dydeta,dydxsi,eta,ielem,maxnpe,nelem,node,np,npe,xc,xsi,yc)
!
!***********************************************************************
!
!! TRANL4 calculates the transformation which maps the reference 
!  element in (XSI,ETA) space into a particular (isoparametric) 
!  element in (X,Y) space, in the case of linear elements.
!
!  We know everything about the (isoparametric) element once we
!  specify the location of its nodes.
!
!  TRANL4 computes the entries of the jacobian of the transformation
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
!  MAXNPE Input, integer ( kind = 4 ) MAXNPE.
!         MAXNPE is the maximum number of nodes per element.
!
!    Input, integer ( kind = 4 ) NELEM, the number of elements.
!
!  NODE   Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), or NODE(MAXNPE,NELEM).
!         NODE contains, for each element, the global node numbers
!         of its NPE nodes.  
!
!         For linear elements (NPE = 3), the order of the nodes probably
!         doesn't matter, but we will draw them this way:
!
!                       2
!                      /|
!                    /  |
!                  /    |
!                /      |
!               1-------3
!
!         For quadratic elements (NPE = 6), the nodes must be given in a 
!         particular order, which is as follows:
!
!                       2
!                      /|
!                    /  |
!                  4    5
!                /      |
!               1---6---3
! 
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!  NPE    Input, integer ( kind = 4 ) NPE.
!         NPE is the number of nodes per element, which should be
!         3 for linear elements and 6 for quadratics.
!
!  XC     Input, real XC(NP), the X coordinates of the nodes.
!
!  XSI    Input, real XSI, the XSI coordinate of the point.
!
!  YC     Input, real YC(NP), the Y coordinates of the nodes.
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
  integer ( kind = 4 ) node(maxnpe,nelem)
  integer ( kind = 4 ) npe
  real x(4)
  real xc(np)
  real xsi
  real xval
  real y(4)
  real yc(np)
  real yval
!
!  Pick off the X, Y coordinates of the local nodes that make up
!  element IELEM, and store them in two short lists, X and Y.
!
  do i = 1,npe
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
!    X(XSI,ETA)  =  A1 * XSI + B1 * XSI*ETA + C1 * ETA + D1
!
!    Y(XSI,ETA)  =  A2 * XSI + B2 * XSI*ETA + C2 * ETA + D2
!  
  a1 =  - x(1)               + x(4)
  b1 =    x(1) - x(2) + x(3) - x(4)
  c1 =  - x(1) + x(2)
  d1  =   x(1)

  xval = a1*xsi+b1*xsi*eta+c1*eta+d1

  a2 =  - y(1)               + y(4)
  b2 =    y(1) - y(2) + y(3) - y(4)
  c2 =  - y(1) + y(2)
  d2  =   y(1)

  yval = a2*xsi+b2*xsi*eta+c2*eta+d2
!
!  Compute the partial derivatives at the point (XSI,ETA).
!  This is the jacobian matrix 
!
!    J: (XSI,ETA) --> (X,Y).
!
  dxdxsi = a1+b1*eta
  dxdeta = b1*xsi+c1

  dydxsi = a2+b2*eta
  dydeta = b2*xsi+c2
!
!  Compute the determinant of the jacobian matrix:
!
!    J: (XSI,ETA) --> (X,Y)
!
  det = dxdxsi*dydeta-dxdeta*dydxsi
!
!  Watch out for a zero determinant.
!
  if ( det == 0.0 ) then
    write(*,*)' '
    write(*,*)'TRANL4 - Fatal error!'
    write(*,*)'  The jacobian J: (XSI,ETA) --> (X,Y) is singular!'
    write(*,*)'  This occurred for element IELEM ',ielem
    write(*,*)'  Local coordinates XSI, ETA = :',xsi,eta
    write(*,*)'  Global coordinates: X,Y =  ',xval,yval
    write(*,*)' '
    write(*,*)'  The local X, Y element nodes were:'
    write(*,*)' '
    do i = 1,npe
      write(*,*)i,x(i),y(i)
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
subroutine tranq6 ( det,detadx,detady,dxdeta,dxdxsi,dxsidx,dxsidy, &
  dydeta,dydxsi,eta,ielem,maxnpe,nelem,node,np,npe,xc,xsi,yc)
!
!***********************************************************************
!
!! TRANQ6 calculates the transformation which maps the reference 
!  element in (XSI,ETA) space into a particular isoparametric 
!  element in (X,Y) space, in the case of quadratic elements.
!
!  We know everything about the isoparametric element once we
!  specify the location of its six nodes.
!
!  TRANQ6 computes the entries of the jacobian of the transformation
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
!  ETA   |      4 5
!        |     /  |
!      0 +    1-6-3
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
!    Output, real DET, the determinant of the jacobian of the
!    transformation between the reference and isoparametric
!    elements.
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
!  MAXNPE Input, integer ( kind = 4 ) MAXNPE.
!         MAXNPE is the maximum number of nodes per element.
!
!    Input, integer ( kind = 4 ) NELEM, the number of elements.
!
!  NODE   Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), or NODE(MAXNPE,NELEM).
!         NODE contains, for each element, the global node numbers
!         of its NPE nodes.  
!
!         For linear elements (NPE = 3), the order of the nodes probably
!         doesn't matter, but we will draw them this way:
!
!                       2
!                      /|
!                    /  |
!                  /    |
!                /      |
!               1-------3
!
!         For quadratic elements (NPE = 6), the nodes must be given in a 
!         particular order, which is as follows:
!
!                       2
!                      /|
!                    /  |
!                  4    5
!                /      |
!               1---6---3
! 
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!  NPE    Input, integer ( kind = 4 ) NPE.
!         NPE is the number of nodes per element, which should be
!         3 for linear elements and 6 for quadratics.
!
!  XC     Input, real XC(NP), the X coordinates of the nodes.
!
!  XSI    Input, real XSI, the XSI coordinate of the point.
!
!  YC     Input, real YC(NP), the Y coordinates of the nodes.
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
  real det
  real detadx
  real detady
  real dxdeta
  real dxdxsi
  real dxsidx
  real dxsidy
  real dydeta
  real dydxsi
  real e1
  real e2
  real eta
  real f1
  real f2
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ielem
  integer ( kind = 4 ) node(maxnpe,nelem)
  integer ( kind = 4 ) npe
  real x(6)
  real xc(np)
  real xsi
  real xval
  real y(6)
  real yc(np)
  real yval
!
!  Pick off the X, Y coordinates of the nodes and store them
!  in two short lists.
!
  do i = 1,npe
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
!    X(XSI,ETA)  =  A1 * XSI**2 + B1 * XSI*ETA + C1 * ETA**2
!               + D1 * XSI    + E1 * ETA     + F1
!
!    Y(XSI,ETA)  =  A2 * XSI**2 + B2 * XSI*ETA + C2 * ETA**2
!               + D2 * XSI    + E2 * ETA     + F2
!  
  a1 =  2.0*x(1)+2.0*x(3)-4.0*x(6)
  b1 = -4.0*x(3)-4.0*x(4)+4.0*x(5)+4.0*x(6)
  c1 =  2.0*x(2)+2.0*x(3)-4.0*x(5)
  d1 = -3.0*x(1)-    x(3)+4.0*x(6)
  e1 =     -x(2)+    x(3)+4.0*x(4)-4.0*x(6)
  f1 =      x(1)

  xval = a1*xsi**2+b1*xsi*eta+c1*eta**2+d1*xsi+e1*eta+f1

  a2 =  2.0*y(1)+2.0*y(3)-4.0*y(6)
  b2 = -4.0*y(3)-4.0*y(4)+4.0*y(5)+4.0*y(6)
  c2 =  2.0*y(2)+2.0*y(3)-4.0*y(5)
  d2 = -3.0*y(1)    -y(3)+4.0*y(6)
  e2 =     -y(2)+    y(3)+4.0*y(4)-4.0*y(6)
  f2 =      y(1)

  yval = a2*xsi**2+b2*xsi*eta+c2*eta**2+d2*xsi+e2*eta+f2
!
!  Compute the partial derivatives at the point (XSI,ETA).
!  This is the jacobian matrix 
!
!    J: (XSI,ETA) --> (X,Y).
!
  dxdxsi =  2.0*a1*xsi +     b1*eta + d1
  dxdeta =      b1*xsi + 2.0*c1*eta + e1

  dydxsi =  2.0*a2*xsi +     b2*eta + d2
  dydeta =      b2*xsi + 2.0*c2*eta + e2
!
!  Compute the determinant of the jacobian matrix:
!
!    J: (XSI,ETA) --> (X,Y)
!
  det = dxdxsi*dydeta-dxdeta*dydxsi
!
!  Watch out for a zero determinant.
!
  if ( det == 0.0 ) then
    write(*,*)' '
    write(*,*)'TRANQ6 - Fatal error!'
    write(*,*)'  The jacobian J: (XSI,ETA) --> (X,Y) is singular!'
    write(*,*)'  This occurred for element number ',ielem
    write(*,*)'  Local coordinates:',xsi,eta
    write(*,*)'  Global coordinates:',xval,yval
    write(*,*)' '
    write(*,*)'  The X, Y nodes were:'
    write(*,*)' '
    do i = 1,npe
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
subroutine tricis ( eta1,eta2,eta3,ielem,jcmax,jcmin,maxnpe,ncon,nelem,node, &
  np,npe,s1,s2,s3,smax,smin,xc,xsi1,xsi2,xsi3,yc)
!
!***********************************************************************
!
!! TRICIS draws color contours for a quantity defined at three points
!  which lie on or within a standard isoparametric element.  The
!  case of linear isoparametrics is NOT handled by this routine.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
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
!  ETA1,
!  ETA2,
!  ETA3   Input, real ETA1, ETA2, ETA3, the ETA coordinates of the
!         three nodes.
!
!  IELEM  Input, integer ( kind = 4 ) IELEM, the element within which we are working.
!
!  JCMAX,
!  JCMIN  Input, integer ( kind = 4 ) JCMAX, JCMIN, the maximum and minimum color
!         indices to use for contours.
!
!  MAXNPE Input, integer ( kind = 4 ) MAXNPE.
!         MAXNPE is the maximum number of nodes per element.
!
!  NCON   Input, integer ( kind = 4 ) NCON, the number of color contours to use.
!
!    Input, integer ( kind = 4 ) NELEM.
!         The number of elements.
!
!  NODE   Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), or NODE(MAXNPE,NELEM).
!         NODE contains, for each element, the global node numbers
!         of its NPE nodes.  
!
!         For linear elements (NPE = 3), the order of the nodes probably
!         doesn't matter, but we will draw them this way:
!
!                       2
!                      /|
!                    /  |
!                  /    |
!                /      |
!               1-------3
!
!         For quadratic elements (NPE = 6), the nodes must be given in a 
!         particular order, which is as follows:
!
!                       2
!                      /|
!                    /  |
!                  4    5
!                /      |
!               1---6---3
! 
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!  NPE    Input, integer ( kind = 4 ) NPE.
!         NPE is the number of nodes per element, which should be
!         3 for linear elements and 6 for quadratics.
!
!  S1,
!  S2,
!  S3     Input, real S1, S2, S3, the values of the quantity S at the
!         three nodes.
!
!  SMAX,
!  SMIN   Input, real SMAX, SMIN, the maximum and minimum values of S.
!
!  XC     Input, real XC(NP).
!         The X coordinates of the nodes.
!
!  XSI1,
!  XSI2,
!  XSI3   Input, real XSI1, XSI2, XSI3, the XSI coordinates of the
!         three nodes.
!
!  YC     Input, real YC(NP).
!         The Y coordinates of the nodes.
!
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) np
!
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
  integer ( kind = 4 ) ncon
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
  if ( s1 <= s2.and.s2 <= s3 ) then
    sl = s1
    sm = s2
    sh = s3
    etal = eta1
    etam = eta2
    etah = eta3
    xsil = xsi1
    xsim = xsi2
    xsih = xsi3
  else if ( s1 <= s3.and.s3 <= s2 ) then
    sl = s1
    sm = s3
    sh = s2
    etal = eta1
    etam = eta3
    etah = eta2
    xsil = xsi1
    xsim = xsi3
    xsih = xsi2
  else if ( s2 <= s1.and.s1 <= s3 ) then
    sl = s2
    sm = s1
    sh = s3
    etal = eta2
    etam = eta1
    etah = eta3
    xsil = xsi2
    xsim = xsi1
    xsih = xsi3
  else if ( s2 <= s3.and.s3 <= s1 ) then
    sl = s2
    sm = s3
    sh = s1
    etal = eta2
    etam = eta3
    etah = eta1
    xsil = xsi2
    xsim = xsi3
    xsih = xsi1
  else if ( s3 <= s1.and.s1 <= s2 ) then
    sl = s3
    sm = s1
    sh = s2
    etal = eta3
    etam = eta1
    etah = eta2
    xsil = xsi3
    xsim = xsi1
    xsih = xsi2
  else if ( s3 <= s2.and.s2 <= s1 ) then
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

  do i = 0,ncon

    sc1 = ((ncon+1-i)*smin+i*smax)/real(ncon+1)
    sc2 = ((ncon-i)*smin+(i+1)*smax)/real(ncon+1)
!
!  Check that some data in the triangle lies in the range [SC1,SC2).
!
    if ( max(sc1,sl) <= min(sc2,sh) ) then

      jcolor = int(((ncon-i)*jcmin+i*jcmax)/real(ncon))
      call filclr(jcolor)
!
!  Take care of possibility that entire triangle formed by the
!  three points lies within the color contour.
!
      if ( sc1 <= sl.and.sh < sc2 ) then

        npts = 3
        xsigon(1) = xsil
        etagon(1) = etal
        xsigon(2) = xsim
        etagon(2) = etam
        xsigon(3) = xsih
        etagon(3) = etah

        if ( npe == 6 ) then

          call isogn6(etagon,ielem,maxnpe,nelem,node,np,npe,npts,xc,xsigon,yc)

        end if
!
!  Find (PXOLD,PYOLD) and (QXOLD,QYOLD), where the line S = SC1 crosses 
!  the triangle.
!
      else

        call cross(px,py,qx,qy,sl,sm,sh,sc1,xsil,xsim,xsih,etal,etam,etah)

        pxold = px
        pyold = py
        qxold = qx
        qyold = qy
!
!  Find (PX,PY) and (QX,QY), where the line S = SC2 crosses the triangle.
!
        call cross(px,py,qx,qy,sl,sm,sh,sc2,xsil,xsim,xsih,etal,etam,etah)
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

        if ( sc1 <= sm.and.sm <= sc2 ) then
          npts = 5
          xsigon(5) = xsim
          etagon(5) = etam
        end if

        if ( npe == 6 ) then

          call isogn6(etagon,ielem,maxnpe,nelem,node,np,npe,npts,xc,xsigon,yc)

        end if

      end if

    end if

  end do

  return
end
subroutine tricno ( i1,i2,i3,jcmax,jcmin,ncon,np,s,smax,smin,x,y)
!
!***********************************************************************
!
!! TRICNO is given a (non-isoperimetric) triangle, formed by nodes I1, 
!  I2, and I3 and the value of the quantity S at these nodes.
!
!  Discussion:
!
!    That portion of the triangle which is greater than contour value
!    value SCON(I) but less than SCON(I+1) is to be filled in with
!    color I.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
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
!    Input, integer ( kind = 4 ) I1, I2, I3, the numbers of three nodes that
!    form the triangle under consideration.
!
!  JCMAX,
!  JCMIN  Input, integer ( kind = 4 ) JCMAX, JCMIN, the maximum and minimum color
!         indices to use for contours.
!
!  NCON   Input, integer ( kind = 4 ) NCON, the number of color contours to use.
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!  S      Input, real S(NP).
!         S is a scalar quantity associated with the nodes.
!         This routine only looks at the values associated with
!         corner element nodes.
!
!  SMAX,
!  SMIN   Input, real SMAX, SMIN, the maximum and minimum values of S.
!
!  X      Input, real X(NP).
!         The X coordinates of the nodes.
!
!  Y      Input, real Y(NP).
!         The Y coordinates of the nodes.
!
  integer ( kind = 4 ) np
!
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
!
  s1 = s(i1)
  s2 = s(i2)
  s3 = s(i3)

  if ( s1 <= s2.and.s2 <= s3 ) then
    il = i1
    im = i2
    ih = i3
  else if ( s1 <= s3.and.s3 <= s2 ) then
    il = i1
    im = i3
    ih = i2
  else if ( s2 <= s1.and.s1 <= s3 ) then
    il = i2
    im = i1
    ih = i3
  else if ( s2 <= s3.and.s3 <= s1 ) then
    il = i2
    im = i3
    ih = i1
  else if ( s3 <= s1.and.s1 <= s2 ) then
    il = i3
    im = i1
    ih = i2
  else if ( s3 <= s2.and.s2 <= s1 ) then
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

  do i = 0,ncon

    sc1 = ((ncon+1-i)*smin+i*smax)/real(ncon+1)
    sc2 = ((ncon-i)*smin+(i+1)*smax)/real(ncon+1)
!
!  Check that some data in the triangle lies in the range [SC1,SC2).
!
!  Warning: the next comparison should be .LE., not .LT.!
!
    if ( max(sl,sc1) <= min(sh,sc2) ) then

      jcolor = int(((ncon-i)*jcmin+i*jcmax)/real(ncon))
      call filclr(jcolor)
!
!  Take care of the possibility that the entire triangle lies in 
!  the contour.
!
      if ( sc1 <= sl.and.sh < sc2 ) then

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

        if ( sc1 <= sm.and.sm <= sc2 ) then
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
subroutine ueval ( detadx,detady,dxsidx,dxsidy,eta,ielem,maxnpe, &
  nelem,node,np,npe,rho,rhoval,u,uval,v,vval,xsi)
!
!***********************************************************************
!
!! UEVAL evaluates velocities at any point in a six node quadratic element.
!
!  Discussion:
!
!    PERHAPS YOU WANT TO APPROXIMATE RHO*U directly, rather
!    than RHO and U separately!
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    14 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real DETADX, DETADY, DXSIDX, DXSIDY, the derivatives
!    d ETA/d X, d ETA/d Y, d XSI/d X and d XSI/d Y at (XSI,ETA).
!
!    Input, real ETA, the second local coordinate of the point
!    where the velocity is desired.
!
!    Input, integer ( kind = 4 ) IELEM, the number of the element in which the
!    point (XSI,ETA) lies.
!
!    Input, integer ( kind = 4 ) MAXNPE.
!    MAXNPE is the maximum number of nodes per element.
!
!    Input, integer ( kind = 4 ) NELEM, the number of elements.
!
!    Input, integer ( kind = 4 ) NODE(MAXNPE,MAXELM), or NODE(MAXNPE,NELEM).
!    NODE contains, for each element, the global node numbers
!    of its NPE nodes.  
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!    Input, integer ( kind = 4 ) NPE.
!    NPE is the number of nodes per element, which should be
!    3 for linear elements and 6 for quadratics.
!
!    Input, real U(NP), the horizontal velocity at each node.
!
!    Output, real UVAL, the horizontal velocity at the point with
!    local coordinates (XSI,ETA).
!
!    Input, real V(NP), the vertical velocity at each node.
!
!    Output, real VVAL, the vertical velocity at the point
!    with local coordinates (XSI,ETA).
!
!    Input, real XSI, the first local coordinate of the point
!    where the velocity is desired.
!
  integer ( kind = 4 ) maxnpe
  integer ( kind = 4 ) nelem
  integer ( kind = 4 ) np
!
  real detadx
  real detady
  real dwdx
  real dwdy
  real dxsidx
  real dxsidy
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
  rhoval = 0.0
  uval = 0.0
  vval = 0.0

  do iq = 1, npe

    if ( npe == 3 ) then

      call bfrefl(w,dwdx,dwdy,detadx,detady,dxsidx,dxsidy,eta,iq,xsi)
 
    else if ( npe == 4 ) then

      write ( *, * ) ' '
      write ( *, * ) 'UEVAL - Fatal error!'
      write ( *, * ) '  Code for the 4 node element has not been written.'
      stop

    else if ( npe == 6 ) then

      call bfrefq(w,dwdx,dwdy,detadx,detady,dxsidx,dxsidy,eta,iq,xsi)

    end if

    jq = node(iq,ielem)
    rhoval = rhoval+w*rho(jq)
    uval = uval+w*u(jq)
    vval = vval+w*v(jq)

  end do

  return
end
subroutine vector ( arrow, ido, jcmax, jcmin, ncon, nflag, np, vecscl, &
  u, v, vmax, vmin, xc, yc )
!
!***********************************************************************
!
!! VECTOR draws a vector field.
!
!  Discussion:
!
!    An arrow pointing in the direction (U(I), V(I)) is drawn at the 
!    point (XC(I),YC(I)).  The arrow's length is scaled by VECSCL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, logical NFLAG(NP).
!    NFLAG is used to "flag" which nodes are active,
!    that is, to be displayed, and which not, in the graph.
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!    Input, real VECSCL.
!    A scale factor for velocity vectors.  This starts out at 1.0.
! 
!    Input, real U(NP).
!    U(I) is the horizontal fluid velocity at node I, or,
!    perhaps, the sensitivity of the horizontal velocity
!    with respect to parameter J.
!   
!    Input, real V(NP).
!    V(I) is the vertical fluid velocity at node I, or, 
!    perhaps, the sensitivity of the vertical velocity
!    with respect to parameter J.
! 
!    Input, real XC(NP).
!    The X coordinates of the nodes.
! 
!    Input, real YC(NP).
!    The Y coordinates of the nodes.
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
  logical s_eqi
  integer ( kind = 4 ) ncon
  logical nflag(np)
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
  do i = 1, np

    if ( nflag(i) ) then

      if ( ido == 1 ) then
        vmag = sqrt(u(i)**2+v(i)**2)
        icon = 1+int(real(ncon)*(vmag-vmin)/(vmax-vmin))
        icon = max(icon,1)
        icon = min(icon,ncon)
        jcolor = int(((ncon-icon)*jcmin+icon*jcmax)/real(ncon))
        call linclr(jcolor)
      end if

      xtip = xc(i)+vecscl*u(i)
      ytip = yc(i)+vecscl*v(i)

      if ( s_eqi ( arrow,'line') ) then
        call arrlin(xc(i),yc(i),xtip,ytip)
      else if ( s_eqi ( arrow,'solid') .or. s_eqi ( arrow,'hollow') ) then
        call arrgon(arrow,xc(i),yc(i),xtip,ytip)
      else
        write ( *, * ) ' '
        write ( *, * ) 'VECTOR - Fatal error!'
        write ( *, * ) '  Illegal option ARROW = ' // arrow
        stop
      end if

    end if

  end do

  return
end
subroutine viznd ( echo,np,nflag0,xc,yc)
!
!***********************************************************************
!
!! VIZND sets the visibility of nodes by minimum distance.
!
!  Discussion:
!
!    The user sets a minimum distance.  The routine picks the nodes
!    to display based on the requirement that they be at least this
!    distance apart.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!    Input/output, logical NFLAG0(MAXNP), a user-defined node visibility flag.
!       
!    Input, real XC(NP).
!    The X coordinates of the nodes.
! 
!    Input, real YC(NP).
!    The Y coordinates of the nodes.
! 
  integer ( kind = 4 ) np
!
  real dismin
  real disnod
  logical echo
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) noff
  real temp
  logical nflag0(np)
  real xc(np)
  real yc(np)
!
  write(*,*)' '
  write(*,*)'VIZND:'
  write(*,*)'  Give a minimum separation for visible nodes.'

  read(*,*)dismin
  write(17,*)dismin
  if ( echo ) then
    write(*,*)dismin
  end if

  temp = dismin**2

  noff = 0
  do i = 1,np

    nflag0(i) = .true.

    do j = 1,i-1
      if ( nflag0(j) ) then
        disnod = (xc(i)-xc(j))**2+(yc(i)-yc(j))**2
        if ( disnod < temp ) then
          nflag0(i) = .false.
          noff = noff+1
          go to 10
        end if
      end if
    end do

10      continue

  end do

  write(*,*)' '
  write(*,*)'VIZND - Note:'
  write(*,*)'  Out of ',np,' nodes,'
  write(*,*)'  you have turned off ',noff
  write(*,*)'  leaving ',np-noff

  return
end
subroutine viznod ( echo, np, nflag0 )
!
!***********************************************************************
!
!! VIZNOD sets the visibility of nodes by index.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!    Input/output, logical NFLAG0(MAXNP), a user-defined node visibility flag.
!       
!    Input, real XC(NP).
!    The X coordinates of the nodes.
! 
!    Input, real YC(NP).
!    The Y coordinates of the nodes.
! 
  integer ( kind = 4 ) np
!
  logical echo
  integer ( kind = 4 ) i
  integer ( kind = 4 ) itemp
  integer ( kind = 4 ) nhi
  integer ( kind = 4 ) nlo
  integer ( kind = 4 ) non
  logical nflag0(np)
!
  write(*,*)' '
  write(*,*)'VIZNOD - Note:'
  write(*,*)'  All nodes will be invisible by default.'
  write(*,*)'  Now specify ranges of node numbers that should'
  write(*,*)'  be visible.  To finish, give a range of 0,0.'
  write(*,*)' '

  do i = 1,np
    nflag0(i) = .false.
  end do

10    continue
  write(*,*)'  Enter low node, high node:'
  read(*,*,err = 30)nlo,nhi
  write(17,*)nlo,nhi
  if ( echo ) then
    write(*,*)nlo,nhi
  end if

  if ( nlo > nhi ) then
    itemp = nlo
    nlo = nhi
    nhi = itemp
  end if

  if ( nlo <= 0.and.nhi <= 0 ) then
    go to 20
  else if ( nlo > np ) then
    write(*,*)'  Your low node was greater than ',NP
  else if ( nhi > np ) then
    write(*,*)'  Your high node was greater than ',NP
  else
    do i = nlo,nhi
      nflag0(i) = .true.
    end do
  end if

  go to 10
!
!  Count up the nodes that are on.
!
20    continue
  non = 0
  do i = 1,np
    if ( nflag0(i) ) then
      non = non+1
    end if
  end do

  write(*,*)' '
  write(*,*)'  Out of ',np,' nodes,'
  write(*,*)'  you have turned on ',non
  write(*,*)'  turning off ',np-non

  return

30    continue

  write ( *, * ) ' '
  write ( *, * ) 'VIZNOD'
  write ( *, * ) 'OUCH!'
  go to 10
end
subroutine vsize ( nflag, np, nxskip, ny, nyskip, u, v, vtmax, vtmin, &
  vvmax, vvmin )
!
!***********************************************************************
!
!! VSIZE computes the maximum visible velocity components.
!
!  Discussion:
!
!    The component maxima are needed in order to properly scale 
!    velocity arrows.
!
!    No arrow will be drawn if:
!
!      The node associated with the vector quantity has been
!      rendered "invisible" via NFLAG.
!
!      The user has requested that the node be skipped via
!      NXSKIP or NYSKIP.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
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
!    Input, logical NFLAG(NP).
!    NFLAG is used to "flag" which nodes are active,
!    that is, to be displayed, and which not, in the graph.
!
!    Input, integer ( kind = 4 ) NP, the number of nodes.
!
!    Input, integer ( kind = 4 ) NXSKIP.
!    NXSKIP is used to "thin" out a vector plot.  
!    If NXSKIP = 1, then a standard vector plot is made.
!    Otherwise, in the X direction, vectors are drawn only
!    in columns 1, 1+NXSKIP, 1+2*NXSKIP and so on.
!  
!    Input, integer ( kind = 4 ) NY.
!    Determines the number of nodes and elements in the Y
!    direction.  There will be 2*NY-1 nodes, 2*NY-2 elements.
! 
!    Input, integer ( kind = 4 ) NYSKIP.
!    NYSKIP is used to "thin" out a vector plot.  
!    If NYSKIP = 1, then a standard vector plot is made.
!    Otherwise, in the Y direction, vectors are drawn only
!    in rows 1, 1+NYSKIP, 1+2*NYSKIP and so on.
!  
!    Input, real U(MAXNP).
!    U(I) is the horizontal fluid velocity at node I, or,
!    perhaps, the sensitivity of the horizontal velocity
!    with respect to parameter J.
!   
!    Input, real V(MAXNP).
!    V(I) is the vertical fluid velocity at node I, or, 
!    perhaps, the sensitivity of the vertical velocity
!    with respect to parameter J.
! 
!    Output, real VTMAX, VTMIN,
!    the largest and smallest Euclidean norms of all the velocity vectors.
! 
!    Output, real VVMAX, VVMIN,
!    the largest and smallest Euclidean norms of the visible velocity vectors.
! 
  implicit none
!
  integer ( kind = 4 ) np
!
  logical draw
  integer ( kind = 4 ) i
  integer ( kind = 4 ) icol
  integer ( kind = 4 ) irow
  logical lfirst
  integer ( kind = 4 ) nxskip
  logical nflag(np)
  integer ( kind = 4 ) nyskip
  integer ( kind = 4 ) ny
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
!
!  Figure out whether the node is to be skipped because of
!  the value of NYSKIP or NXSKIP.
!
      draw = .true.

      irow = 1 + mod ( i - 1, ( 2 * ny - 1 ) )
      if ( mod ( irow - 1, nyskip ) /= 0 ) then
        draw = .false.
      end if

      icol = 1 + ( ( i - 1 ) / ( 2 * ny - 1 ) )
      if ( mod ( ( icol - 1 ), nxskip ) /= 0 ) then
        draw = .false.
      end if

      if ( draw ) then

        if ( lfirst ) then
          vvmax = vmag
          vvmin = vmag
          lfirst = .false.
        else
          vvmax = max ( vvmax, vmag )
          vvmin = min ( vvmin, vmag )
        end if

      end if

    end if

  end do

  return
end
subroutine word_count ( s, nword )
!
!*******************************************************************************
!
!! WORD_COUNT counts the number of "words" in a string.
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
