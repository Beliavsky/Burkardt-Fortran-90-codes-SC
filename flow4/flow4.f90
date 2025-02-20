program main

!*******************************************************************************
!
!! FLOW4 solves a steady viscous 2D Navier-Stokes flow using finite elements.
!
!
!  NX =       11,   21,    31,    41,    61,     81,     121,      161,
!  NY =        4,    7,    10,    13,    19,     25,      37,       49,
!  H =         1,  1/4,   1/6,   1/8,  1/12,   1/16,    1/24,     1/32,
!          0.5, 0.25, 0.166, 0.125, 0.083, 0.0625, 0.04166,  0.03125,
!
!  17 August 1999
!
!    Retrieved FLOW4 from floppy disk.  The data had "rotted".  There were
!    some losses, particulary in subroutine GETDER.  Right now, I just
!    want to see how the options I used in the Newton iteration, so I'm
!    not panicked.
!
!  08 May 1996
!
!    Trying to solve at RE = 100, I'm getting failures.  I think I
!    could do this, if I turn off the Newton rejection...
!
!  07 May 1996
!
!    Well, I've forgotten where I was on this code, but I need to make
!    some runs for optimization over RE and over the Bump.  I'm having
!    a little trouble getting it started up again.
!
!    Getting an index out of bounds error...Tried to run on SUN,
!    but missing damn "_CG92_USED_" symbolic name, somehow.
!    The only way I could avoid it was to compile everything at once.
!
!  10 November 1995
!
!    Well, I slaved away on the routine to compute the differences
!    between U,V,P on a coarse and a fine grid (really so that I
!    could, in turn, look at sensitivities) and I'm getting poor
!    convergence.  I haven't plotted it yet, but it doesn't look good.
!    I think I'll can this whole project shortly.
!
!  09 November 1995
!
!    I am worried that my cubic bump is not "consistent" with my
!    quadratic elements.  So for my comparison runs I'm going to
!    look at quadratic bumps only, for a while.
!
!  02 November 1995
!
!    7 node quadrature routine is in, ready to test.
!
!  01 November 1995
!
!    My studies of the convergence behavior of U don't show H**2 convergence.
!    I am going to check this by adding the option of 7 point Gaussian
!    quadrature, but this will be a mess.
!
!  26 October 1995
!
!    Got back Cray 615 run:
!
!      NX = 11, NY=4, CPU=4 seconds.
!      DS-FDC (U,V,P) = (0.177348, 0.132730, 0.124550)
!      Try full size problem.
!
!  25 October 1995
!
!    From the SIAM meeting:
!
!    "Can decompose any vector field into the gradient of a scalar field
!    and something else." -- I could use this fact to compute a
!    stream function of vector fields that don't quite satisfy the
!    divergence condition, such as Jennifer's least squares flows.
!
!    From the SIAM meeting:
!
!    "Blah, blah, blah, convert the problem to a quadratic program,
!    which only needs the gradients of the cost functional, and solve
!    it, so you don't care about consistency between the cost functional
!    and the gradients."
!
!    From the SIAM meeting: (Matthias)
!
!    "SQP method only uses gradients of cost functional."
!
!      Lagrangian L(y,g,lambda) = J + (lambda,C(y,g))
!
!      Kuhn Tucker conditions:
!
!      Grady J + Cy Lambda = 0  (adjoint)
!      Gradg J + Cg Lambda = 0 (gradient)
!      C(y,g) = 0 (state)
!
!    Use Newton method on this system.
!
!    Big goal for San Francisco:
!      Slides of O(H) or O(H^2) behavior of (U_ALPHA)^H and (U^H)_ALPHA.
!
!  20 October 1995
!
!    Renamed:
!
!      ISHAPB  -> IBSCAN
!      ISHAPBT -> IBSTAR
!      ISHAPF  -> IFSCAN
!      ISHAPFT -> IFSTAR
!      XBLEFT  -> XBLCAN
!      XBRITE  -> XBRCAN
!      YBLEFT  -> YBLCAN
!      YBLEFT  -> YBRCAN
!
!  13 October 1995
!
!    I am interested in examining the Zienkiewicz-Zhou method again.
!    I suspect that the reason I saw little improvement was that I
!    was comparing the finite differences to the discretized sensitivities,
!    when I don't know that the finite differences converge to the
!    true solution at any particular rate, and I suspect the discretized
!    sensitivities converge at an O(h*h) rate.
!
!    To verify this, I am going to solve the problem at a sequence of
!    values of h, compute dudy in various ways, and look at the behavior
!    of dudy as h goes to zero.
!
!    If this doesn't work, then I will look at the behavior of U
!    as H goes to zero, which should surely show H**2 behavior.
!
!    ALSO, I think my serendipity scheme is open to question.  I compute
!    dUdY on the original grid, then interpolate using serendipity.
!    But I don't pass in the X, Y coordinates, so I'm treating dUdY as
!    a "basic" variable, when only U should be so treated.  So I may
!    have to make a new version that can compute dUdY on a serendipity
!    element, given U.
!
!  19 August 1995
!
!    Y12M was a big loss.  For large problems, the amount of fillin must
!    have increased dramatically.  I ended up needed as much space as with
!    a banded solver.
!
!    Then I tried SLAP, and got, as usual with iterative schemes, no
!    convergence.
!
!    Then I considered GBSOL, and discovered that I had already given up
!    on that in June, because GBSOL does not save the LU factors.  It is
!    a one shot solver only.
!
!    Now I am back to a very early project, which is to rewrite SGBFA
!    and SGBSL to use a small in core workspace, and a disk file.  I have
!    actually got it to solve a small problem.  Now I have to test it out
!    on FLOW.
!
!    Switched NODE(I,J) to NODE(J,I) in DISPLAY, FLOW3 and FLOW4.
!
!  14 August 1995
!
!    I am having some success with Y12M.  I have gotten it to solve
!    a small system, and to solve two systems with the same matrix.
!
!  10 August 1995
!
!    I'm getting an error from NDRV, which looks as though it's
!    caused by the fact that the pressure equations don't have
!    a pressure coefficient, in other words, the diagonal entry
!    of the matrix is zero.
!
!    I am going to run a simple test example, but if that bears
!    out my suspicion, then either I can't use YALMAT, or I'll have
!    to try some sort of fancy manipulation to solve for
!    the pressures first.  (Matthias did suggest something along
!    these lines).
!
!    YALMAT routine NDRV fails on the matrix
!
!      1 0 0
!      0 0 1
!      0 1 1
!
!    complaining that it has a zero pivot in column 2.
!
!    The second alternative is to try ITPACK or NSPCG.
!
!  09 August 1995
!
!    After some exasperation, redimensioned INDX from INDX(MAXNP,3) to
!    INDX(3,MAXNP) which makes life easier.
!
!  27 July 1995
!
!    Starting FLOW4.F, archiving FLOW3.F, which had all that optimization
!    stuff in it.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    28 January 2009
!
!  Author:
!
!    John Burkardt
!
  integer, parameter :: maxnpe = 6
  integer, parameter :: maxnx = 21
  integer, parameter :: maxny = 7
!
!  The assignment of MAXROW should really read (maxrow = 28*min(nx,ny)).
!
  integer, parameter :: maxdim = 3
  integer, parameter :: maxrow = 29 * maxny
  integer, parameter :: maxelm = 2*(maxnx-1)*(maxny-1)
  integer, parameter :: maxeqn = 2*(2*maxnx-1)*(2*maxny-1)+maxnx*maxny
  integer, parameter :: maxnp = (2*maxnx-1)*(2*maxny-1)
  integer, parameter :: maxparf = 5
  integer, parameter :: maxparb = 5
  integer, parameter :: maxpar = maxparb+maxparf+1
  integer, parameter :: liv = 60
  integer, parameter :: lv = 78+maxpar*(maxpar+21)/2
  integer, parameter :: maxquad = 7

  real ( kind = 8 ) a(maxrow,maxeqn)
  real ( kind = 8 ) area(maxquad,maxelm)
  real ( kind = 8 ) base(maxpar,maxdim)
  real ( kind = 8 ) cost
  real ( kind = 8 ) costar
  real ( kind = 8 ) costb
  real ( kind = 8 ) costp
  real ( kind = 8 ) costu
  real ( kind = 8 ) costv
  real ( kind = 8 ) dir(maxpar,maxdim)
  real ( kind = 8 ) dopt(maxpar)
  real ( kind = 8 ) dpara3(maxpar)
  real ( kind = 8 ) dparsn(maxpar)
  real ( kind = 8 ) dparfd(maxpar)
  real ( kind = 8 ) dparfdc(maxpar)
  real ( kind = 8 ) dpdyn(maxnp)
  real ( kind = 8 ) dpdy2(maxnp)
  real ( kind = 8 ) dpdy3(maxnp)
  real ( kind = 8 ) dpdy4(maxnp)
  real ( kind = 8 ) dudyn(maxnp)
  real ( kind = 8 ) dudy2(maxnp)
  real ( kind = 8 ) dudy3(maxnp)
  real ( kind = 8 ) dudy4(maxnp)
  real ( kind = 8 ) dudy5(maxnp)
  real ( kind = 8 ) dvdyn(maxnp)
  real ( kind = 8 ) dvdy2(maxnp)
  real ( kind = 8 ) dvdy3(maxnp)
  real ( kind = 8 ) dvdy4(maxnp)
  real ( kind = 8 ) dydpn(maxnp,maxparb)
  real ( kind = 8 ) epsdif
  character ( len = 2 ) eqn(maxeqn)
  real ( kind = 8 ) etan(6)
  real ( kind = 8 ) etaq(maxquad)
  character ( len = 30 ) fileg
  character ( len = 30 ) filet
  real ( kind = 8 ) g(maxeqn)
  real ( kind = 8 ) g1(maxeqn)
  real ( kind = 8 ) g2(maxeqn)
  real ( kind = 8 ) g3(maxeqn)
  real ( kind = 8 ) gdif(maxeqn,maxpar)
  real ( kind = 8 ) gdifc(maxeqn,maxpar)
  real ( kind = 8 ) gold(maxeqn)
  real ( kind = 8 ) gopt(maxpar)
  real ( kind = 8 ) gradf(maxeqn,maxpar)
  real ( kind = 8 ) gtar(maxeqn)
  real ( kind = 8 ) hx
  real ( kind = 8 ) hy
  integer i
  integer ibc
  integer icunit
  integer ifs
  integer ifscan
  integer ifstar
  integer ibs
  integer ibscan
  integer ibstar
  integer ibump
  integer idfd
  integer ido
  integer ids
  integer ierror
  integer ifds
  integer igrad
  integer igunit
  integer ijac
  integer indx(3,maxnp)
  integer info
  integer iopt(maxpar)
  integer ip
  integer ipivot(maxeqn)
  integer iplot
  integer isotri(maxelm)
  integer istep1
  integer istep2
  integer itar
  integer itemp
  integer itunit
  integer itype
  integer iuval
  integer ivopt(liv)
  integer iwrite
  integer j
  integer jjac
  integer jstep1
  integer jstep2
  logical lmat
  logical lval
  integer maxnew
  integer maxstp
  integer ndim
  integer nelem
  integer neqn
  integer nlband
  integer node(maxnpe,maxelm)
  integer nopt
  integer np
  integer npar
  integer nparb
  integer nparbt
  integer nparf
  integer npe
  integer nprof(2*maxny-1)
  integer nquad
  integer nrow
  integer nstep3
  integer numel(maxnp)
  integer numstp
  integer nx
  integer ny
  real ( kind = 8 ) para(maxpar)
  real ( kind = 8 ) para1(maxpar)
  real ( kind = 8 ) para2(maxpar)
  real ( kind = 8 ) para3(maxpar)
  real ( kind = 8 ) partar(maxpar)
  real ( kind = 8 ) phi(maxquad,6,10,maxelm)
  real ( kind = 8 ) res(maxeqn)
  real ( kind = 8 ) sens(maxeqn,maxpar)
  real ( kind = 8 ) splbmp(4,maxparb+2,0:maxparb)
  real ( kind = 8 ) splflo(4,maxparf+2,0:maxparf)
  character ( len = 20 ) syseqn
  real ( kind = 8 ) taubmp(maxparb+2)
  real ( kind = 8 ) tauflo(maxparf+2)
  character ( len = 30 ) title
  real ( kind = 8 ) tolnew
  real ( kind = 8 ) tolopt
  real ( kind = 8 ) u(maxnp)
  real ( kind = 8 ) u1
  real ( kind = 8 ) u1max
  real ( kind = 8 ) u2
  real ( kind = 8 ) u2max
  real ( kind = 8 ) vopt(lv)
  real ( kind = 8 ) wateb
  real ( kind = 8 ) wateb1
  real ( kind = 8 ) wateb2
  real ( kind = 8 ) watep
  real ( kind = 8 ) wateu
  real ( kind = 8 ) watev
  real ( kind = 8 ) wquad(maxquad)
  real ( kind = 8 ) xbl
  real ( kind = 8 ) xblcan
  real ( kind = 8 ) xbltar
  real ( kind = 8 ) xbr
  real ( kind = 8 ) xbrcan
  real ( kind = 8 ) xbrtar
  real ( kind = 8 ) xc(maxnp)
  real ( kind = 8 ) xopt(maxpar)
  real ( kind = 8 ) xquad(maxquad,maxelm)
  real ( kind = 8 ) xprof
  real ( kind = 8 ) xsin(6)
  real ( kind = 8 ) xsiq(maxquad)
  real ( kind = 8 ) y2max
  real ( kind = 8 ) ybl
  real ( kind = 8 ) yblcan
  real ( kind = 8 ) ybltar
  real ( kind = 8 ) ybr
  real ( kind = 8 ) ybrcan
  real ( kind = 8 ) ybrtar
  real ( kind = 8 ) yc(maxnp)
  real ( kind = 8 ) yquad(maxquad,maxelm)
  real ( kind = 8 ) yval
!
  call timestamp ( )
!
!  Say hello.
!
  call hello ( maxnx, maxny )
!
!  Initialize the variables.
!
  call init(area,cost,costar,costb,costp,costu,costv,dopt,dpara3,dparsn, &
    dparfd,dparfdc,dpdyn,dudyn,dvdyn,dydpn,epsdif,eqn,etan,fileg,filet, &
    g,g1,g2,g3,gdif,gold,gopt,gradf,gtar,ibc,ifscan,ifstar,ibscan,ibstar, &
    ibump,idfd,ids,ierror,ifds,igrad,igunit,ijac,indx,iopt,iplot,isotri, &
    istep1,istep2,itar,itunit,itype,ivopt,iwrite,jjac,jstep1,jstep2,liv,lv, &
    maxelm,maxeqn,maxnew,maxnp,maxpar,maxparb,maxquad,maxstp,node,nopt,npar, &
    nparb,nparf,npe,nquad,nstep3,nx,ny,para1,para2,para3,partar,sens,syseqn, &
    tolnew,tolopt,vopt,wateb,wateb1,wateb2,watep,wateu,watev,xblcan,xbltar, &
    xbrcan,xbrtar,xprof,xsin,yblcan,ybltar,ybrcan,ybrtar)
!
!  Read the user input.
!
20    continue

  call input(epsdif,fileg,filet,ibc,ifscan,ifstar,ibscan,ibstar,ibump,idfd, &
    ids,ierror,ifds,igrad,ijac,iopt,iplot,istep1,istep2,itar,itype,iwrite, &
    jjac,jstep1,jstep2,maxnew,maxpar,maxstp,npar,nparb,nparf,nquad,nstep3,nx, &
    ny,para1,para2,para3,partar,syseqn,tolnew,tolopt,wateb,wateb1,wateb2, &
    watep,wateu,watev,xblcan,xbltar,xbrcan,xbrtar,xprof,yblcan,ybltar,ybrcan, &
    ybrtar)

  if ( ierror == 0 ) then

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FLOW4 - GO signal from user input.'

  else

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FLOW4 - STOP signal from user.'

    if ( itunit /= 0 ) then

      close ( unit = itunit )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FLOW4 - Note:'
      write ( *, '(a)' ) '  Closing the marching file ' // trim ( filet )

    end if

    if ( igunit /= 0 ) then

      close ( unit = igunit )

      write ( *, * ) ' '
      write ( *, * ) 'FLOW4 - Note:'
      write ( *, * ) '  Closing the graphics file ' // trim ( fileg )

    end if

    write ( *, * ) ' '
    write ( *, * ) 'FLOW4 - End of execution.'
    write ( *, * ) ' '
    stop
  end if
!
!  Set the number of optimization variables.
!
  nopt = sum ( iopt(1:npar) )
  nelem = 2*(nx-1)*(ny-1)
  np = (2*nx-1)*(2*ny-1)
  write ( *, * ) ' '
  write ( *, * ) 'FLOW4 - Note.'
  write ( *, * ) '  Number of elements, NELEM = ',nelem
  write ( *, * ) '  Number of nodes, NP = ',np
  write ( *, * ) '  Number of nodes per element, NPE =  ',npe
!
!  Check the data.
!
  call chkdat(ibump,idfd,ids,ifds,igrad,ijac,iopt,itype,jjac,maxpar,maxparb, &
    maxparf,nopt,npar,nparb,nparf,nstep3,para1,partar,xblcan,xbrcan)
!
!  Print the problem information.
!
  call prdat(epsdif,fileg,filet,ibc,ifscan,ifstar,ibscan,ibstar,ibump,idfd, &
    ids,ifds,igrad,ijac,iopt,iplot,istep1,istep2,itar,itype,iwrite,jjac, &
    jstep1,jstep2,maxnew,maxpar,maxstp,nopt,npar,nparb,nparf,nquad,nstep3,nx, &
    ny,para1,para2,para3,partar,syseqn,tolnew,tolopt,wateb,wateb1,wateb2, &
    watep,wateu,watev,xblcan,xbltar,xbrcan,xbrtar,xprof,yblcan,ybltar,ybrcan, &
    ybrtar)
!
!  Open the plot file.
!
  call pltopn(fileg,igunit,iplot)
!
!  Open the marching file.
!
  call maropn(filet,itunit)
!
!  Solve the optimization problem,
!  which involves repeated evaluation of the functional
!  at various flow solutions (U,V,P)+(FLO,BUM,REY).
!
  lval = .true.
  call lmemry('set','target',lval)

  ifs = ifstar
  ibs = ibstar
  xbl = xbltar
  xbr = xbrtar
  ybl = ybltar
  ybr = ybrtar
!
!  Set up the elements and nodes.
!
  write ( *, * ) 'ibump = ',ibump
  write ( *, * ) 'maxeqn = ',maxeqn
  write ( *, * ) 'nelem = ',nelem
  write ( *, * ) 'np = ',np
  write ( *, * ) 'nx = ',nx
  write ( *, * ) 'ny = ',ny
  hx = 10.0D+00 / real (2*(nx-1), kind = 8 )
  hy = 3.0D+00 / real (2*(ny-1), kind = 8 )
  write ( *, * ) 'HX = ',hx
  write ( *, * ) 'HY = ',hy

  call setnod(eqn,ibump,indx,isotri,maxeqn,nelem,neqn,node,np,nx,ny,xbl,xbr)
!
!  Find points on velocity profile sampling line.
!
  itemp = nint ( ( 2.0D+00 * real(nx-1, kind = 8 ) * xprof ) / 10.0D+00 )
  do i = 1,2*ny-1
    ip = itemp*(2*ny-1)+i
    nprof(i) = ip
  end do
!
!  Get matrix bandwidth.
!
  call setban(indx,maxrow,nelem,nlband,node,np,nrow)

  write ( *, * ) ' '
  write ( *, * ) 'FLOW4 - Note:'
  write ( *, * ) '  Lower bandwidth NLBAND =     ',nlband
  write ( *, * ) '  Total bandwidth =          ',2*nlband+1
  write ( *, * ) '  Required matrix rows NROW =  ',nrow
  write ( *, * ) '  MAXROW =                   ',maxrow

  if ( itar == 0 ) then

    write ( *, * ) ' '
    write ( *, * ) 'FLOW4 - Note:'
    write ( *, * ) '  Computing the target solution.'

    para(1:npar) = partar(1:npar)
    g(1:neqn) = 0.0D+00
!
!  Get the flow solution G.
!
    call flosol(a,area,eqn,etaq,g,g2,ifs,ibs,ierror,ijac,indx,ipivot,isotri, &
      iwrite,jjac,maxnew,nelem,neqn,nlband,node,np,npar,nparb,nparf,nquad, &
      nrow,nx,ny,para,phi,res,splbmp,splflo,syseqn,taubmp,tauflo,tolnew, &
      wquad,xbl,xbr,xc,xquad,xsiq,ybl,ybr,yc,yquad)

    if ( ierror/= 0 ) then
      write ( *, * ) ' '
      write ( *, * ) 'FLOW4 - Fatal error!'
      write ( *, * ) '  FloSol could not compute the target solution.'
      go to 16
    else
      gtar(1:neqn) = g(1:neqn)
    end if
!
!  Compute the cost function J.
!
    call getcst(cost,costb,costp,costu,costv,g,gtar,ibs,indx,neqn,np,nparb, &
      nprof,ny,splbmp,taubmp,wateb,watep,wateu,watev,xbl,xbr,ybl,ybr,yc)

    if ( iwrite == 1 ) then
      write ( *, * ) 'Cost:',cost
    else if ( iwrite >= 2 ) then
      title = 'Cost'
      call prcost2(cost,costb,costp,costu,costv,title,wateb,watep,wateu,watev)
    end if
!
!  Compute the finite difference vectors GDIF.
!
    call getgrd(a,area,cost,dpara3,epsdif,eqn,etaq,g,g1,g2,gdif,gtar,ifs, &
      ibs,ierror,ijac,indx,iopt,ipivot,isotri,iwrite,jjac,maxeqn,maxnew, &
      nelem,neqn,nlband,node,np,npar,nparb,nparf,nprof,nquad,nrow,nx,ny, &
      para,phi,res,splbmp,splflo,syseqn,taubmp,tauflo,tolnew,wateb,watep, &
      wateu,watev,wquad,xbl,xbr,xc,xquad,xsiq,ybl,ybr,yc,yquad)

    if ( ierror /= 0 ) then
      write ( *, * ) ' '
      write ( *, * ) 'FLOW4 - Fatal error!'
      write ( *, * ) '  GetGrd returns IERROR = ',ierror
      stop
    end if

    numstp = 0
!
!  Get dPdY, dUdY and dVdY.
!
    call getdu(dpdyn,dudyn,dvdyn,etan,g,indx,isotri,nelem,neqn,node,np, &
      numel,xc,xsin,yc)
!
!  Get dPdY2, dUdY2, and dVdY2, the ZZ corrections.
!
    call getdu2(dudyn,dudy2,np,nx,ny)
    call getdu2(dvdyn,dvdy2,np,nx,ny)
    call getdu2(dpdyn,dpdy2,np,nx,ny)
!
!  Get dPdY3, dUdY3, and dVdY3, the serendipity corrections.
!
    call getdu3(dudyn,dudy3,np,nx,ny)
    call getdu3(dvdyn,dvdy3,np,nx,ny)
    call getdu3(dpdyn,dpdy3,np,nx,ny)
!
!  Get dPdY4, dUdY4, and dVdY4, the ZZ modified corrections.
!
    call getdu4(dudyn,dudy4,np,nx,ny)
    call getdu4(dvdyn,dvdy4,np,nx,ny)
    call getdu4(dpdyn,dpdy4,np,nx,ny)
!
!  Get dUdY5, the second serendipity corrections.
!
    do i = 1,np
      u(i) = g(indx(1,i))
    end do

    call getdu5(dudyn,dudy5,np,nx,ny,u,xc,yc)
!
!  Compute the correction term GRADF, and the GDIFC, the corrected
!  finite difference estimate of the sensitivity.
!
    call getfix(dpdyn,dudyn,dvdyn,dydpn,gradf,ibs,indx,iopt,maxeqn,np,npar, &
      nparb,nparf,splbmp,taubmp,xbl,xbr,xc,yc)

    do i = 1,neqn
      do j = 1,npar
        gdifc(i,j) = gdif(i,j) - gradf(i,j)
      end do
    end do
!
!  Get the discretized sensitivities.
!
    lmat = .false.
    call lmemry('get','have_fp',lmat)

    if ( .not.lmat ) then

      call fp(a,area,eqn,g,indx,nelem,neqn,nlband,node,np,npar,nparb,nparf, &
        nquad,nrow,para,phi,syseqn)

      call sgbtrf(neqn,neqn,nlband,nlband,a,nrow,ipivot,info)

      if ( info/= 0 ) then
        write ( *, * ) ' '
        write ( *, * ) 'FLOW4 - Fatal error!'
        write ( *, * ) '  Jacobian factorization failed.'
        write ( *, * ) '  SGBTRF returns nonzero INFO = ',info
      else
        lmat = .true.
        call lmemry('set','have_fp',lmat)
      end if
    end if

    if ( lmat ) then
      call getsen(a,area,dudyn,dvdyn,eqn,g,ibc,ifs,ibs,indx,iopt,ipivot, &
        maxeqn,nelem,neqn,nlband,node,np,npar,nparb,nparf,nquad,nrow, &
        phi,sens,splbmp,splflo,taubmp,tauflo,xc,yc)
    end if

    gold(1:neqn) = g(1:neqn)

    if ( iwrite == 1 ) then
      write ( *, * ) 'Cost: ',cost
    else if (iwrite >= 2 ) then
      title = 'Cost'
      call prcost2(cost,costb,costp,costu,costv,title,wateb,watep,wateu,watev)
    end if

    call getder(dparfd,g,gtar,ibs,indx,maxeqn,neqn,np,npar,nparb,nparf, &
      nprof,ny,gdif,splbmp,taubmp,wateb,watep,wateu,watev,xbl,xbr,ybl,ybr,yc)

    call getder(dparfdc,g,gtar,ibs,indx,maxeqn,neqn,np,npar,nparb,nparf, &
      nprof,ny,gdifc,splbmp,taubmp,wateb,watep,wateu,watev,xbl,xbr,ybl,ybr,yc)

    call getder(dparsn,g,gtar,ibs,indx,maxeqn,neqn,np,npar,nparb,nparf,nprof, &
      ny,sens,splbmp,taubmp,wateb,watep,wateu,watev,xbl,xbr,ybl,ybr,yc)

    call prsol(dpara3,dparfd,dparfdc,dparsn,g,g2,gdif,gdifc,idfd,ids,ifds, &
      indx,iwrite,maxeqn,neqn,np,npar,nparb,nparf,numstp,para,sens)

    costar = cost

  else if (itar == 1 ) then

    write ( *, * ) 'FLOW4 - Note:'
    write ( *, * ) '  Calling GetTar2 to get target data.'

    nparbt = 0
    ibs = 0

    call setxy(ibs,np,nparbt,nx,ny,splbmp,taubmp,xbltar,xbrtar,xc,ybltar, &
      ybrtar,yc)

    g(1:neqn) = 0.0D+00

    u1max = 9.0D+00 / 4.0D+00
    y2max = ( 6.0D+00 -2.0D+00 * sqrt ( 3.0D+00 ) ) / 4.0D+00
    u2max = ( 3.0D+00 - y2max ) * ( 1.5D+00 - y2max ) * y2max

    do i = 1,2*ny-1
      iuval = indx(1,nprof(i))
      yval = real(i-1, kind = 8 )*3.0D+00 / real (2*ny-2, kind = 8 )
      yc(nprof(i)) = yval
      u1 = (3.0D+00-yval)*yval/u1max
      u2 = (3.0D+00-yval)*(1.5D+00-yval)*yval/u2max
      g(iuval) = partar(1)*u1+partar(2)*u2
    end do
!
!  Compute the cost, relative to zero.
!
    gtar(1:neqn) = 0.0D+00

    call discst(costp,costu,costv,g,gtar,indx,neqn,np,nprof,ny,yc)

    cost = costu
    costar = cost
    gtar(1:neqn) = g(1:neqn)

  end if
!
!  HARDCODE OPTION:
!  Write comparison data to file.
!
  ido = -1
  if ( ido == 0 ) then

    icunit = 17

    open ( unit = icunit, file = 'compare.dat', form = 'formatted', &
      access = 'sequential', status = 'replace' )

    call writes(ibs,icunit,nelem,neqn,np,nparb,nquad,nx,ny,xbl,xbr)

    call writev(area,g,indx,isotri,icunit,nelem,neqn,node,np,nparb,nquad, &
      splbmp,taubmp,xc,yc)

    close ( unit = icunit )

    write ( *, * ) ' '
    write ( *, * ) 'Wrote comparison data to file COMP.DAT.'
    write ( *, * ) 'Stopping now.'
    stop
  end if
!
!  Write target information to plot file.
!
!  HARDCODE OPTION:
!  Plot uncorrected finite difference sensitivities GDIF,
!  or corrected finite difference sensitivities GDIFC.
!
  if ( iplot /= 0 ) then

    ido = 1

    if ( ido == 0 ) then

      call pltwrt(eqn,g,gdif,igunit,indx,isotri,iwrite,maxeqn,nelem,neqn, &
        node,np,npar,npe,nprof,nx,ny,para,sens,xc,xprof,yc)

    else if (ido == 1 ) then

      call pltwrt(eqn,g,gdifc,igunit,indx,isotri,iwrite,maxeqn,nelem,neqn, &
        node,np,npar,npe,nprof,nx,ny,para,sens,xc,xprof,yc)

    end if

  end if
!
!  Turn off target calculation flag.
!
  lval = .false.
  call lmemry('set','target',lval)

  lmat = .false.
  call lmemry('set','have_fp',lmat)
!
!  Set data specific to the optimization.
!
  ifs = ifscan
  ibs = ibscan
  xbl = xblcan
  xbr = xbrcan
  ybl = yblcan
  ybr = ybrcan
!
!  Set the elements and nodes.
!
  if ( xblcan /= xbltar .or. xbrcan /= xbrtar .or. &
       yblcan /= ybltar .or. ybrcan /= ybrtar ) then

    call setnod(eqn,ibump,indx,isotri,maxeqn,nelem,neqn,node,np,nx,ny,xbl,xbr)

  end if
!
!  The user only wanted to compute the target point, and nothing more.
!
  if ( itype == 0 ) then

    go to 20
!
!  Is this a march?
!
  else if (itype == 1.or.itype== 2.or.itype==4 ) then

    if ( itype == 1 ) then
      ndim = 1
    else if (itype == 2 ) then
      ndim = 2
    else if (itype ==4 ) then
      ndim = 3
    end if

    call march(a,area,base,dir,dpara3,dparsn,dparfd,dparfdc,dpdyn,dudyn, &
      dvdyn,dydpn,epsdif,eqn,etan,etaq,g,g1,g2,g3,gdif,gdifc,gold,gradf, &
      gtar,ibc,ifs,ibs,idfd,ids,ifds,igunit,ijac,indx,iopt,ipivot,iplot, &
      isotri,istep1,istep2,itunit,iwrite,jjac,jstep1,jstep2,maxeqn,maxnew, &
      maxstp,ndim,nelem,neqn,nlband,node,np,npar,nparb,nparf,npe,nprof, &
      nquad,nrow,nstep3,numel,nx,ny,para,para1,para2,para3,phi,res,sens, &
      splbmp,splflo,syseqn,taubmp,tauflo,tolnew,wateb,wateb1,wateb2,watep, &
      wateu,watev,wquad,xbl,xbr,xc,xprof,xquad,xsin,xsiq,ybl,ybr,yc,yquad )
!
!  ...or a sensitivity optimization?
!  ...or an optimization using function values only?
!
  else if (itype ==3.or.itype==7 ) then

    do i = 1,npar
      para(i) = para1(i)
    end do

    call qsolve(a,area,dopt,dpara3,dparfd,dparfdc,dparsn,dpdyn,dudyn,dvdyn, &
      dydpn,epsdif,eqn,etan,etaq,g,g1,g2,gdif,gdifc,gold,gopt,gradf,gtar, &
      ibc,ifs,ibs,idfd,ids,ifds,igrad,igunit,ijac,indx,iopt,ipivot,iplot, &
      isotri,itunit,itype,ivopt,iwrite,jjac,liv,lv,maxeqn,maxnew,maxstp, &
      nelem,neqn,nlband,node,nopt,np,npar,nparb,nparf,npe,nprof,nquad,nrow, &
      numel,nx,ny,para,phi,res,sens,splbmp,splflo,syseqn,taubmp,tauflo, &
      tolnew,tolopt,vopt,wateb,watep,wateu,watev,wquad,xbl,xbr,xc,xopt,xprof, &
      xquad,xsin,xsiq,ybl,ybr,yc,yquad)

  else

    write ( *, * ) ' '
    write ( *, * ) 'FLOW4 - Fatal error!'
    write ( *, * ) '  Unknown value of ITYPE = ',itype
    stop

  end if

16    continue
!
!  If an error occurred, then wipe out the current data.
!
  if ( ierror/= 0 ) then
    ierror = 0
    write ( *, * ) 'FLOW4 - Clearing error flag after failure.'
  end if
!
!  See if user wishes to change variables and continue run.
!
  go to 20
end
subroutine bmpbc(dudyn,dvdyn,ibs,ip,iparb,np,nparb,shape,splbmp,taubmp, &
  ubc,vbc,xc)

!*******************************************************************************
!
!! BMPBC computes the value of the boundary conditions for the
!  horizontal and vertical velocity sensitivities with respect
!  to a given shape parameter.
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
!  IP     Input, the global node number.
!
!  IPARB  Input, integer IPARB, the bump parameter with respect to
!         which the sensitivities are desired.
!
!  UBC    Output, the boundary condition for the horizontal velocity
!         sensitivity.
!
!  VBC    Output, the boundary condition for the vertical velocity
!         sensitivity.
!
!
  integer np
!
  real ( kind = 8 ) dudyn(np)
  real ( kind = 8 ) dvdyn(np)
  integer ibs
  integer ip
  integer iparb
  integer jderiv
  integer nparb
  real ( kind = 8 ) shape
  real ( kind = 8 ) splbmp(4,nparb+2,0:nparb)
  real ( kind = 8 ) taubmp(nparb+2)
  real ( kind = 8 ) ubc
  real ( kind = 8 ) vbc
  real ( kind = 8 ) xc(np)
!
!  Determine the value of the "basis" shape function associated
!  with parameter IPARB, evaluated at node IP.
!
  if ( ibs  == 1 ) then
    call plval1(iparb+1,nparb+2,xc(ip),taubmp,shape)
  else if (ibs == 2 ) then
    call pqval1(iparb+1,nparb+2,xc(ip),taubmp,shape)
  else if (ibs ==3 ) then
    jderiv = 0
    call ppvalu(taubmp,splbmp(1,1,iparb),nparb+1,4,xc(ip),jderiv,shape)
  end if

  ubc = -dudyn(ip)*shape
  vbc = -dvdyn(ip)*shape

  return
end
subroutine bmpbc1(g,ibs,indx,ip,iparb,neqn,np,nparb,shape,splbmp,taubmp, &
  ubc,vbc,xc,yc)
!
!*******************************************************************************
!
!! BMPBC1 computes the value of the boundary conditions for the
!  horizontal and vertical velocity sensitivities with respect
!  to a given shape parameter using a two point finite difference
!  formula.
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
!  G      Input, real G(MAXEQN), the finite element
!         coefficients.
!
!  IP     Input, the global node number.
!
!  IPARB  Input, integer IPARB, the bump parameter with respect to
!         which the sensitivities are desired.
!
!  UBC    Output, the boundary condition for the horizontal velocity
!         sensitivity.
!
!  VBC    Output, the boundary condition for the vertical velocity
!         sensitivity.
!
!
  integer neqn
  integer np
!
  real ( kind = 8 ) dudy
  real ( kind = 8 ) dvdy
  real ( kind = 8 ) g(neqn)
  integer ibs
  integer ihor
  integer ihorn
  integer indx(3,np)
  integer ip
  integer iparb
  integer iver
  integer ivern
  integer jderiv
  integer nparb
  real ( kind = 8 ) shape
  real ( kind = 8 ) splbmp(4,nparb+2,0:nparb)
  real ( kind = 8 ) taubmp(nparb+2)
  real ( kind = 8 ) ubc
  real ( kind = 8 ) vbc
  real ( kind = 8 ) xc(np)
  real ( kind = 8 ) yc(np)
!
!  Determine the value of the "basis" shape function associated
!  with parameter IPARB, evaluated at node IP.
!
  if ( ibs == 1 ) then
    call plval1(iparb+1,nparb+2,xc(ip),taubmp,shape)
  else if (ibs == 2 ) then
    call pqval1(iparb+1,nparb+2,xc(ip),taubmp,shape)
  else if (ibs ==3 ) then
    jderiv = 0
    call ppvalu(taubmp,splbmp(1,1,iparb),nparb+1,4,xc(ip),jderiv,shape)
  end if
!
!  Estimate dUdY and dVdY at the node.
!
  ihor = indx(1,ip)
  ihorn = indx(1,ip+1)
  dudy = (g(ihorn)-g(ihor))/(yc(ip+1)-yc(ip))

  iver = indx(2,ip)
  ivern = indx(2,ip+1)
  dvdy = (g(ivern)-g(iver))/(yc(ip+1)-yc(ip))
!
!  Set the boundary conditions.
!
  ubc = -dudy*shape
  vbc = -dvdy*shape

  return
end
subroutine bmpbc2(g,ibs,indx,ip,iparb,neqn,np,nparb,shape,splbmp,taubmp, &
  ubc,vbc,xc,yc)
!
!*******************************************************************************
!
!! BMPBC2 computes the value of the boundary conditions for the
!  horizontal and vertical velocity sensitivities with respect
!  to a given shape parameter using a three point finite difference
!  formula.
!
!  We derive that formula from the following Taylor series:
!
!    u0 = u0
!    u1 = u0 + h1 u0' + h1**2 u0"/2 + O(h1**3)
!    u2 = u0 + h2 u0' + h2**2 u0"/2 + O(h2**3)
!
!  arriving at:
!
!    u0' = (h1**2 u2 - h2**2 u1 + (h2**2-h1**2) u0) / (h1*h2*(h1-h2))
!        + O(max(h1,h2)**2)
!
!  Note that these Taylor series are really only valid when all three
!  nodes lie in one element, so that U is a smooth polynomial.  This
!  is not true for midside nodes, though to correct this would be
!  painful.
!
!  When all three nodes do lie in one element, and if u is at
!  most a quadratic function, then the formula should be exact.
!  This is the case for the velocities U and V at a node.
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
!  G      Input, real G(MAXEQN), the finite element
!         coefficients.
!
!  IP     Input, the global node number.
!
!  IPARB  Input, integer IPARB, the bump parameter with respect to
!         which the sensitivities are desired.
!
!  UBC    Output, the boundary condition for the horizontal velocity
!         sensitivity.
!
!  VBC    Output, the boundary condition for the vertical velocity
!         sensitivity.
!
!
  integer neqn
  integer np
!
  real ( kind = 8 ) dudy
  real ( kind = 8 ) dvdy
  real ( kind = 8 ) g(neqn)
  real ( kind = 8 ) h1
  real ( kind = 8 ) h2
  integer ibs
  integer indx(3,np)
  integer ip
  integer iparb
  integer jderiv
  integer nparb
  real ( kind = 8 ) shape
  real ( kind = 8 ) splbmp(4,nparb+2,0:nparb)
  real ( kind = 8 ) taubmp(nparb+2)
  real ( kind = 8 ) u0
  real ( kind = 8 ) u1
  real ( kind = 8 ) u2
  real ( kind = 8 ) ubc
  real ( kind = 8 ) v0
  real ( kind = 8 ) v1
  real ( kind = 8 ) v2
  real ( kind = 8 ) vbc
  real ( kind = 8 ) xc(np)
  real ( kind = 8 ) yc(np)
!
!  Determine the value of the "basis" shape function associated
!  with parameter IPARB, evaluated at node IP.
!
  if ( ibs == 1 ) then
    call plval1(iparb+1,nparb+2,xc(ip),taubmp,shape)
  else if (ibs == 2 ) then
    call pqval1(iparb+1,nparb+2,xc(ip),taubmp,shape)
  else if (ibs ==3 ) then
    jderiv = 0
    call ppvalu(taubmp,splbmp(1,1,iparb),nparb+1,4,xc(ip),jderiv,shape)
  end if
!
!  Estimate dUdY and dVdY at the node.
!
  h1 = yc(ip+1)-yc(ip)
  h2 = yc(ip+2)-yc(ip)

  u0 = g(indx(1,ip))
  u1 = g(indx(1,ip+1))
  u2 = g(indx(1,ip+2))

  dudy = (h1**2*u2-h2**2*u1+(h2**2-h1**2)*u0)/(h1*h2*(h1-h2))

  v0 = g(indx(2,ip))
  v1 = g(indx(2,ip+1))
  v2 = g(indx(2,ip+2))

  dvdy = (h1**2*v2-h2**2*v1+(h2**2-h1**2)*v0)/(h1*h2*(h1-h2))
!
!  Set the boundary conditions.
!
  ubc = -dudy*shape
  vbc = -dvdy*shape

  return
end
subroutine bmpcst(costb,ibs,nparb,splbmp,taubmp,xbl,xbr,ybl,ybr)
!
!*******************************************************************************
!
!! BMPCST evaluates the cost of the bump control.
!
!  The bump connects the points (XBL,YBL) and (XBR,YBR).
!
!  Compute its "cost" by comparing its slope to the slope of the
!  straight line that connects those two points.
!
!    COSTB = Integral (XBL <= X <= XBR) (Bump'(X) - Line'(X))**2 dX
!
!  Here, Bump(X) represents the function describing the shape
!  of the bump, and Line(X) represents the straight line which
!  simply joins the two endpoints, (XBL,YBL) and (XBR,YBR).
!
!  This integral is approximated by numerical integration.
!
!  The interval between XBL and XBR is divided into NPARB+1
!  intervals, over each of which the bump's height is described
!  by a cubic.
!
!  For each such interval, pick NQUAD1 quadrature points,
!  evaluate the derivative of the bump function there, and
!  subtract the slope of the straight line.
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
!  COSTB  Output, real COSTB.
!
!         COSTB is the integral of the difference of the
!         derivatives of the straight line joining the two straight line
!         line segments of the bottom, and the bump that is
!         actually drawn there.
!
!         This measures the cost of bump control.
!
!
  integer nparb
!
  integer, parameter :: nquad1 = 5
!
  real ( kind = 8 ) costb
  real ( kind = 8 ) cprime
  integer i
  integer ibs
  integer j
  integer jderiv
  real ( kind = 8 ) slope
  real ( kind = 8 ) splbmp(4,nparb+2,0:nparb)
  real ( kind = 8 ) taubmp(nparb+2)
  real ( kind = 8 ) wquad1(nquad1)
  real ( kind = 8 ) xbl
  real ( kind = 8 ) xbr
  real ( kind = 8 ) xleft
  real ( kind = 8 ) xquad1(nquad1)
  real ( kind = 8 ) xrite
  real ( kind = 8 ) xx
  real ( kind = 8 ) ybl
  real ( kind = 8 ) ybr
  real ( kind = 8 ) yvec(nparb+2)
!
  costb = 0.0D+00

  if ( nparb == 0)return

  if ( xbl>= xbr)return
!
!  Get the Gauss weights and abscissas.
!
  call gquad1(nquad1,wquad1,xquad1)
!
!  Get the slope of the line joining the endpoints of the bump.
!
  slope = (ybr-ybl)/(xbr-xbl)

  do i = 1,nparb+1

    xleft = (real(nparb+2-i, kind = 8 )*xbl &
            +real(i-1, kind = 8 )*xbr) &
           /real(nparb+1, kind = 8 )
    xrite = (real(nparb+1-i, kind = 8 )*xbl+real(i, kind = 8 )*xbr)/real(nparb+1, kind = 8 )

    do j = 1,nquad1

      xx = 0.5*((1.0+xquad1(j))*xrite+(1.0-xquad1(j))*xleft)

      if ( ibs == 1 ) then
        yvec(1:nparb+2) = splbmp(1,1:nparb+2,0)
        call pldx(nparb+2,xx,taubmp,cprime,yvec)
      else if (ibs == 2 ) then
        yvec(1:nparb+2) = splbmp(1,1:nparb+2,0)
        call pqdx(nparb+2,xx,taubmp,cprime,yvec)
      else if (ibs ==3 ) then
        jderiv = 1
        call ppvalu(taubmp,splbmp,nparb+1,4,xx,jderiv,cprime)
      end if

      costb = costb+0.5*wquad1(j)*(xrite-xleft)*(cprime-slope)**2

    end do

  end do

  return
end
subroutine bmpder(dpara,ibs,npar,nparb,nparf,splbmp,taubmp,wateb,xbl,xbr, &
  ybl,ybr)
!
!*******************************************************************************
!
!! BMPDER evaluates the derivative of the cost of the bump control
!  with respect to the parameters.
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
  integer npar
  integer nparb
!
  integer, parameter :: nquad1 = 5
!
  real ( kind = 8 ) cprime
  real ( kind = 8 ) dpara(npar)
  integer i
  integer ibs
  integer j
  integer jderiv
  integer k
  integer nparf
  real ( kind = 8 ) slope
  real ( kind = 8 ) splbmp(4,nparb+2,0:nparb)
  real ( kind = 8 ) taubmp(nparb+2)
  real ( kind = 8 ) temp
  real ( kind = 8 ) wateb
  real ( kind = 8 ) wquad1(nquad1)
  real ( kind = 8 ) xbl
  real ( kind = 8 ) xbr
  real ( kind = 8 ) xleft
  real ( kind = 8 ) xquad1(nquad1)
  real ( kind = 8 ) xrite
  real ( kind = 8 ) xx
  real ( kind = 8 ) ybl
  real ( kind = 8 ) ybr
  real ( kind = 8 ) yvec(nparb+2)
!
  if ( nparb == 0)return
  if ( xbr<= xbl)return
!
!  Get the Gauss weights and abscissas.
!
  call gquad1(nquad1,wquad1,xquad1)
!
!  Get the slope of the straight line connecting the endpoints.
!
  slope = (ybr-ybl)/(xbr-xbl)

  do i = 1,nparb+1

    xleft = (real(nparb+2-i, kind = 8 )*xbl+real(i-1, kind = 8 )*xbr)/real(nparb+1, kind = 8 )
    xrite = (real(nparb+1-i, kind = 8 )*xbl+real(i, kind = 8 )*xbr)/real(nparb+1, kind = 8 )

    do j = 1,nquad1

      xx = 0.5*((1.0+xquad1(j))*xrite+(1.0-xquad1(j))*xleft)

      if ( ibs == 1 ) then
        yvec(1:nparb+2) = splbmp(1,1:nparb+2,0)
        call pldx(nparb+2,xx,taubmp,cprime,yvec)
      else if (ibs == 2 ) then
        yvec(1:nparb+2) = splbmp(1,1:nparb+2,0)
        call pqdx(nparb+2,xx,taubmp,cprime,yvec)
      else if (ibs ==3 ) then
        jderiv = 1
        call ppvalu(taubmp,splbmp,nparb+1,4,xx,jderiv,cprime)
      end if

      do k = 1,nparb

        if ( ibs == 1 ) then
          call pldx1(k+1,nparb+2,xx,taubmp,temp)
        else if (ibs == 2 ) then
          call pqdx1(k+1,nparb+2,xx,taubmp,temp)
        else if (ibs ==3 ) then
          jderiv = 1
          call ppvalu(taubmp,splbmp(1,1,k),nparb+1,4,xx,jderiv,temp)
        end if

        dpara(nparf+k) = dpara(nparf+k)+wateb*wquad1(j)* &
          (xrite-xleft)*(cprime-slope)*temp

      end do

    end do

  end do

  return
end
subroutine bmpsen(dudyn,dvdyn,eqn,f,g,ibc,ibs,indx,ipar,neqn,np,nparb,nparf, &
  splbmp,taubmp,xc,yc)
!
!*******************************************************************************
!
!! BMPSEN sets up the right hand side F associated with the
!  sensitivities of a given flow solution (U,V,P) with respect to the
!  IPAR-th bump parameter.
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
  integer neqn
  integer np
  integer nparf
!
  real ( kind = 8 ) dudyn(np)
  real ( kind = 8 ) dvdyn(np)
  character ( len = 2 ) eqn(neqn)
  real ( kind = 8 ) f(neqn)
  real ( kind = 8 ) g(neqn)
  integer i
  integer ibc
  integer ibs
  integer ihor
  integer indx(3,np)
  integer ip
  integer ipar
  integer iparb
  integer iver
  integer nparb
  real ( kind = 8 ) shape
  real ( kind = 8 ) splbmp(4,nparb+2,0:nparb)
  real ( kind = 8 ) taubmp(nparb+2)
  real ( kind = 8 ) ubc
  real ( kind = 8 ) vbc
  real ( kind = 8 ) xc(np)
  real ( kind = 8 ) yc(np)
!
  iparb = ipar-nparf

  do i = 1,neqn
    f(i) = 0.0D+00
  end do

  do ip = 1,np

    ihor = indx(1,ip)
    if ( eqn(ihor) =='UB' ) then
      if ( ibc == 0 ) then
        call bmpbc(dudyn,dvdyn,ibs,ip,iparb,np,nparb,shape,splbmp,taubmp, &
          ubc,vbc,xc)
      else if (ibc == 1 ) then
        call bmpbc1(g,ibs,indx,ip,iparb,neqn,np,nparb,shape,splbmp,taubmp, &
          ubc,vbc,xc,yc)
      else if (ibc == 2 ) then
        call bmpbc2(g,ibs,indx,ip,iparb,neqn,np,nparb,shape,splbmp,taubmp, &
          ubc,vbc,xc,yc)
      else if (ibc ==3 ) then
        call bmpbc(dudyn,dvdyn,ibs,ip,iparb,np,nparb,shape,splbmp,taubmp, &
          ubc,vbc,xc)
      end if
      f(ihor) = ubc
    end if

    iver = indx(2,ip)
    if ( eqn(iver) =='VB' ) then
      if ( ibc == 0 ) then
        call bmpbc(dudyn,dvdyn,ibs,ip,iparb,np,nparb,shape,splbmp,taubmp, &
          ubc,vbc,xc)
      else if (ibc == 1 ) then
        call bmpbc1(g,ibs,indx,ip,iparb,neqn,np,nparb,shape,splbmp,taubmp, &
          ubc,vbc,xc,yc)
      else if (ibc == 2 ) then
        call bmpbc2(g,ibs,indx,ip,iparb,neqn,np,nparb,shape,splbmp,taubmp, &
          ubc,vbc,xc,yc)
      else if (ibc ==3 ) then
        call bmpbc(dudyn,dvdyn,ibs,ip,iparb,np,nparb,shape,splbmp,taubmp, &
          ubc,vbc,xc)
      end if
      f(iver) = vbc
    end if

  end do

  return
end
subroutine bmpspl(ibs,npar,nparb,nparf,par,splbmp,taubmp,xbl,xbr,ybl,ybr)
!
!*******************************************************************************
!
!! BMPSPL sets up or updates the spline data that describes the bump.
!
!  It does this for the target parameters and the feasible parameters.
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
!  IBS    Input, integer IBS.
!         1, the bump is modeled by C0 linear splines.
!         2, the bump is modeled by C0 quadratic splines.
!         3, the bump is modeled by C1 cubic splines.
!
!
  integer npar
  integer nparb
  integer nparf
!
  integer i
  integer ibcbeg
  integer ibcend
  integer ibs
  integer ipar
  real ( kind = 8 ) par(npar)
  real ( kind = 8 ) splbmp(4,nparb+2,0:nparb)
  real ( kind = 8 ) taubmp(nparb+2)
  real ( kind = 8 ) xbl
  real ( kind = 8 ) xbr
  real ( kind = 8 ) ybl
  real ( kind = 8 ) ybr
!
  if ( nparb<= 0)return
!
!  Set up the bump arrays, including:
!
!    TAUBMP, containing the abscissas, which never change,
!    SPLBMP(1,I,0), the location of the bump at abscissa I,
!    SPLBMP(1,I,IPAR), the I-th coefficient of the partial derivative
!    of the bump shape with respect to the IPAR-th bump parameter.
!
  do i = 1,nparb+2
    taubmp(i) = (real(nparb+2-i, kind = 8 )*xbl+real(i-1, kind = 8 )*xbr)/real(nparb+1, kind = 8 )
  end do

  splbmp(1,1,0) = ybl
  do i = 2, nparb+1
    splbmp(1,i,0) = par(nparf+i-1)
  end do
  splbmp(1,nparb+2,0) = ybr

  if ( ibs  == 3 ) then

    do i = 1,nparb+2

      do ipar = 1,nparb

        if ( ipar+1/= i ) then
          splbmp(1,i,ipar) = 0.0D+00
        else
          splbmp(1,i,ipar) = 1.0D+00
        end if

      end do

    end do

    ibcbeg = 0
    ibcend = 0
    do i = 0,nparb
      call cubspl(taubmp,splbmp(1,1,i),nparb+2,ibcbeg,ibcend)
    end do

  end if

  return
end
subroutine bsp(q,dqdx,dqdy,ielem,iq,nelem,node,np,xc,xq,yc,yq)
!
!*******************************************************************************
!
!! BSP computes the value and spatial derivatives of the linear basis
!  functions associated with pressure.
!
!
!  Here is a picture of a typical finite element associated with
!  pressure:
!
!      2
!     /|
!    / |
!   /  |
!  1---3
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
!  Q      Output, real Q, the value of the IQ-th basis
!         function at the point with global coordinates (XQ,YQ).
!
!  DQDX,
!  DQDY   Output, real DQDX, DQDY, the X and Y
!         derivatives of the IQ-th basis function at the point
!         with global coordinates (XQ,YQ).
!
!  IELEM  Input, integer IELEM, the global element number about which
!         we are inquiring.
!
!  IQ     Input, integer IQ, the index of the desired basis
!         function.  This is also the node of the reference
!         triangle which is associated with the basis function.
!
!         Basis function IQ is 1 at node IQ, and zero at the
!         other two nodes.
!
!  NELEM  Input, integer NELEM, the number of elements.
!
!  NODE   Input, integer NODE(6,NELEM).  NODE(J,I) is
!         the global node number of the J-th node in the I-th
!         element.
!
!  NP     Input, integer NP, the number of nodes.
!
!  XC     Input, real XC(NP), the global X coordinates
!         of the element nodes.
!
!  XQ     Input, real XQ, the global X coordinate of
!         the point in which we are interested.
!
!  YC     Input, real YC(NP), the global Y coordinates
!         of the element nodes.
!
!  YQ     Input, real YQ, the global Y coordinate of
!         the point in which we are interested.
!
!
  integer nelem
  integer np
!
  real ( kind = 8 ) q
  real ( kind = 8 ) dqdx
  real ( kind = 8 ) dqdy
  real ( kind = 8 ) d
  integer i1
  integer i2
  integer i3
  integer ielem
  integer iq
  integer iq1
  integer iq2
  integer iq3
  integer node(6,nelem)
  real ( kind = 8 ) xc(np)
  real ( kind = 8 ) xq
  real ( kind = 8 ) yc(np)
  real ( kind = 8 ) yq
!
  if ( iq<1.or.iq>6 ) then
    write ( *, * ) ' '
    write ( *, * ) 'BSP - Fatal error!'
    write ( *, * ) '  The requested basis function is IQ = ',iq
    write ( *, * ) '  but only values from 1 to 6 are legal.'
    stop
  else if (iq>= 4.and.iq<=6 ) then
    q = 0.0D+00
    dqdx = 0.0D+00
    dqdy = 0.0D+00
    return
  end if

  iq1 = iq
  iq2 = mod(iq,3)+1
  iq3 = mod(iq+1,3)+1

  i1 = node(iq1,ielem)
  i2 = node(iq2,ielem)
  i3 = node(iq3,ielem)

  d =  (xc(i2)-xc(i1))*(yc(i3)-yc(i1))-(xc(i3)-xc(i1))*(yc(i2)-yc(i1))

  dqdx = (yc(i2)-yc(i3))/d
  dqdy = (xc(i3)-xc(i2))/d

  q = 1.0 + dqdx*(xq-xc(i1)) + dqdy*(yq-yc(i1))

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
  integer itemp
!
  itemp = ichar ( c )

  if ( 97 <= itemp .and. itemp <= 122 ) then
    c = char ( itemp - 32 )
  end if

  return
end
subroutine chkdat(ibump,idfd,ids,ifds,igrad,ijac,iopt,itype,jjac,maxpar, &
  maxparb,maxparf,nopt,npar,nparb,nparf,nstep3,para1,partar,xblcan,xbrcan)
!
!*******************************************************************************
!
!! CHKDAT performs some simple checks on the input data.
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
  integer maxpar
!
  integer i
  integer ibump
  integer idfd
  integer ids
  integer ifds
  integer igrad
  integer ijac
  integer iopt(maxpar)

  integer itype
  integer jjac
  integer maxparb
  integer maxparf
  integer nopt
  integer npar
  integer nparb
  integer nparf
  integer nstep3
  real ( kind = 8 ) para1(maxpar)
  real ( kind = 8 ) partar(maxpar)
  real ( kind = 8 ) sum
  real ( kind = 8 ) xblcan
  real ( kind = 8 ) xbrcan
!
!  IBUMP
!
  if ( ibump<0.or.ibump>3 ) then
    write ( *, * ) ' '
    write ( *, * ) 'ChkDat - Fatal error!'
    write ( *, * ) '  IBUMP is out of bounds!'
    write ( *, * ) '  Current value of IBUMP = ',ibump
    write ( *, * ) '  Legal values must be between 0 and 3.'
    stop
  end if
!
!  IGRAD
!
  if ( igrad<0.or.igrad>4 ) then
    write ( *, * ) ' '
    write ( *, * ) 'ChkDat - Fatal error!'
    write ( *, * ) '  IGRAD must be between 0 and 4,'
    write ( *, * ) '  but your value is ',igrad
    stop
  end if

  if ( igrad == 0 ) then
    if ( itype ==3 ) then
      write ( *, * ) ' '
      write ( *, * ) 'ChkDat - Fatal error!'
      write ( *, * ) '  A cost gradient approximation MUST be made'
      write ( *, * ) '  when ITYPE = ',itype
      write ( *, * ) '  but you have chosen IGRAD = ',igrad
      stop
    end if
  else if (igrad == 1 ) then
    if ( ids == 0 ) then
      write ( *, * ) ' '
      write ( *, * ) 'ChkDat - Fatal error!'
      write ( *, * ) '  Your cost gradient choice IGRAD = ',igrad
      write ( *, * ) '  requires a nonzero value of IDS,'
      write ( *, * ) '  but your value is IDS = ',ids
      stop
    end if
  else if (igrad == 2 ) then
    if ( ifds == 0 ) then
      write ( *, * ) ' '
      write ( *, * ) 'ChkDat - Fatal error!'
      write ( *, * ) '  Your cost gradient choice IGRAD = ',igrad
      write ( *, * ) '  requires a nonzero value of IFDS,'
      write ( *, * ) '  but your value is IFDS = ',ifds
      stop
    end if
  else if (igrad ==3 ) then
    if ( ifds == 0 ) then
      write ( *, * ) ' '
      write ( *, * ) 'ChkDat - Fatal error!'
      write ( *, * ) '  Your cost gradient choice IGRAD = ',igrad
      write ( *, * ) '  requires a nonzero value of IFDS,'
      write ( *, * ) '  but your value is IFDS = ',ifds
      stop
    end if
  else if (igrad ==4 ) then
    if ( idfd == 0 ) then
      write ( *, * ) ' '
      write ( *, * ) 'ChkDat - Fatal error!'
      write ( *, * ) '  Your cost gradient choice IGRAD = ',igrad
      write ( *, * ) '  requires a nonzero value of IDFD,'
      write ( *, * ) '  but your value is IDFD = ',idfd
      stop
    end if
  end if
!
!  IJAC
!
  if ( ijac<1 ) then
    write ( *, * ) ' '
    write ( *, * ) 'ChkDat - Fatal error!'
    write ( *, * ) '  IJAC is out of bounds!'
    write ( *, * ) '  Current value of IJAC = ',ijac
    write ( *, * ) '  Legal values must be 1 or greater.'
    stop
  end if
!
!  IOPT
!
  do i = 1,npar
    if ( iopt(i)/= 0.and.iopt(i)/=1 ) then
      write ( *, * ) ' '
      write ( *, * ) 'Chkdat - Fatal error!'
      write ( *, * ) '  IOPT(*) must be 0 or 1, but'
      write ( *, * ) '  IOPT(',I,') = ',iopt(i)
      stop
    end if
  end do
!
!  ITYPE
!
  if ( itype<0.or.itype>7 ) then
    write ( *, * ) ' '
    write ( *, * ) 'ChkDat - Fatal error!'
    write ( *, * ) '  Illegal value of ITYPE = ',itype
    write ( *, * ) '  Legal values are between 0 and 7.'
    write ( *, * ) '  ChkDat forces a STOP!'
    stop
  end if
!
!  JJAC
!
  if ( jjac<0.or.jjac>2 ) then
    write ( *, * ) ' '
    write ( *, * ) 'ChkDat - Fatal error!'
    write ( *, * ) '  Illegal value of JJAC = ',jjac
    write ( *, * ) '  Legal values are between 0 and 2.'
    write ( *, * ) '  ChkDat forces a STOP!'
    stop
  end if
!
!  NOPT
!
  if ( nopt<= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'ChkDat - Warning!'
    write ( *, * ) '  The number of free parameters, NOPT = ',nopt
    write ( *, * ) '  but this value should be positive!'
  end if
!
!  NPAR
!
  if ( npar>maxpar ) then
    write ( *, * ) ' '
    write ( *, * ) 'ChkDat - Fatal error!'
    write ( *, * ) '  NPAR is out of bounds!'
    write ( *, * ) '  Current value of NPAR = ',npar
    write ( *, * ) '  Maximum legal value, MAXPARA = ',maxpar
    stop
  end if
!
!  NPARB
!
  if ( nparb<0.or.nparb>maxparb ) then
    write ( *, * ) ' '
    write ( *, * ) 'ChkDat - Fatal error!'
    write ( *, * ) '  NPARB is out of bounds!'
    write ( *, * ) '  Input value of NPARB = ',nparb
    write ( *, * ) '  Maximum legal value, MAXPARB = ',maxparb
    stop
  end if
!
!  NPARF
!
  if ( nparf<= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'ChkDat - Fatal error!'
    write ( *, * ) '  NPARF is out of bounds!'
    write ( *, * ) '  The input value of NPARF is ',nparf
    write ( *, * ) '  But NPARF must be at least 1.'
    stop
  end if

  if ( nparf>maxparf ) then
    write ( *, * ) ' '
    write ( *, * ) 'ChkDat - Fatal error!'
    write ( *, * ) '  NPARF is too big!'
    write ( *, * ) '  Input value of NPARF = ',nparf
    write ( *, * ) '  Maximum legal value, MAXPARF = ',maxparf
    stop
  end if
!
!  NSTEP3
!
  if ( itype ==4 ) then
    if ( nstep3<1 ) then
      write ( *, * ) ' '
      write ( *, * ) 'ChkDat - Fatal error!'
      write ( *, * ) ' Nonpositive value for NSTEP3 = ',nstep3
      stop
    else if (nstep3 == 1 ) then
      write ( *, * ) 'ChkDat - Warning!'
      write ( *, * ) '  NSTEP3 = 1 is an unusual value!'
    end if
  end if
!
!  PARA1(1:NPARF)
!
  sum = 0.0D+00
  do i = 1,nparf
    sum = sum+abs(para1(i))
  end do

  if ( sum == 0.0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'ChkDat - Fatal error!'
    write ( *, * ) '  All PARA1 inflows are zero.'
    stop
  end if
!
!  PARA1(NPARF+NPARB+1), the value of NU_INV.
!
  if ( para1(nparf+nparb+1)<0.0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'ChkDat - Warning!'
    write ( *, * ) '  NU_INV entry of PARA1 is negative.'
    write ( *, * ) '  This is being changed to one.'
    para1(nparf+nparb+1) = 1.0D+00
  end if
!
!  PARTAR(1:NPARF)
!
  sum = 0.0D+00
  do i = 1,nparf
    sum = sum+abs(partar(i))
  end do

  if ( sum == 0.0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'ChkDat - Fatal error!'
    write ( *, * ) '  All PARTAR inflows are zero.'
    stop
  end if
!
!  PARTAR(NPARF+NPARB+1), the value of NU_INV.
!
  if ( partar(nparf+nparb+1)<0.0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'ChkDat - Warning!'
    write ( *, * ) '  NU_INV entry of PARTAR is negative.'
    write ( *, * ) '  This is being changed to one.'
    partar(nparf+nparb+1) = 1.0D+00
  end if
!
!  XBLEFT, XBRITE
!
  if ( nparb>0 ) then
    if ( xblcan>= xbrcan ) then
      write ( *, * ) ' '
      write ( *, * ) 'ChkDat - Fatal error!'
      write ( *, * ) '  XBLEFT >=  XBRITE.'
      write ( *, * ) '  XBLEFT = ',xblcan
      write ( *, * ) '  XBRITE = ',xbrcan
      stop
    end if
  end if

  return
end
subroutine chrctd(string,dval,ierror,lchar)
!
!*******************************************************************************
!
!! CHRCTD accepts a string of characters, and tries to extract a
!  real real number from the initial part of the
!  string.
!
!  CHRCTD will read as many characters as possible until it reaches
!  the end of the string, or encounters a character which cannot be
!  part of the number.
!
!  Legal input is:
!
!     1 blanks,
!     2 '+' or '-' sign,
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
!    STRING            DVAL
!
!    '1'               1.0D+00
!    '     1   '       1.0D+00
!    '1A'              1.0D+00
!    '12,34,56'        12.0D+00
!    '  34 7'          34.0D+00
!    '-1E2ABCD'        -100.0D+00
!    '-1X2ABCD'        -1.0D+00
!    ' 2E-1'           0.2
!    '23.45'           23.45
!    '-4.2E+2'         -420.0D+00
!    '17d2'            1700.0D+00
!    '-14e-2'         -0.14
!    'e2'              100.0D+00
!    '-12.73e-9.23'   -12.73 * 10.0**(-9.23)
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
!  STRING Input, CHARACTER*(*) STRING, the string containing the
!         data to be read.  Reading will begin at position 1 and
!         terminate at the end of the string, or when no more
!         characters can be read to form a legal real.  Blanks,
!         commas, or other nonnumeric data will, in particular,
!         cause the conversion to halt.
!
!  DVAL   Output, real DVAL, the value that was read
!         from the string.
!
!  IERROR Output, integer IERROR, error flag.
!
!         0, no errors occurred.
!
!         1, 2, 6 or 7, the input number was garbled.  The
!         value of IERROR is the last type of input successfully
!         read.  For instance, 1 means initial blanks, 2 means
!         a plus or minus sign, and so on.
!
!  LCHAR  Output, integer LCHAR, the number of characters read from
!         STRING to form the number, including any terminating
!         characters such as a trailing comma or blanks.
!
!
  character chrtmp
  real ( kind = 8 ) dval
  integer ierror
  integer ihave
  integer isgn
  integer iterm
  integer jbot
  integer jsgn
  integer jtop
  integer lchar
  logical s_eqi
  integer nchar
  integer ndig
  real ( kind = 8 ) rbot
  real ( kind = 8 ) rexp
  real ( kind = 8 ) rtop
  character ( len = * )  string
!
  nchar = len(string)

  ierror = 0
  dval = 0.0D+00
  lchar = -1
  isgn = 1
  rtop = 0.0D+00
  rbot = 1.0D+00
  jsgn = 1
  jtop = 0
  jbot = 1
  ihave = 1
  iterm = 0

10    continue
  lchar = lchar+1
  chrtmp = string(lchar+1:lchar+1)
!
!  Blank character.
!
  if ( chrtmp ==' ' ) then
    if ( ihave == 2.or.ihave==6.or.ihave==7 ) then
      iterm = 1
    else if (ihave>1 ) then
      ihave = 11
    end if
!
!  Comma
!
  else if (chrtmp ==',' ) then
    if ( ihave/= 1 ) then
      iterm = 1
      ihave = 12
      lchar = lchar+1
    end if
!
!  Minus sign.
!
  else if (chrtmp =='-' ) then
    if ( ihave == 1 ) then
      ihave = 2
      isgn = -1
    else if (ihave ==6 ) then
      ihave = 7
      jsgn = -1
    else
      iterm = 1
    end if
!
!  Plus sign.
!
  else if (chrtmp =='+' ) then
    if ( ihave == 1 ) then
      ihave = 2
    else if (ihave ==6 ) then
      ihave = 7
    else
      iterm = 1
    end if
!
!  Decimal point.
!
  else if (chrtmp =='.' ) then
    if ( ihave<4 ) then
      ihave = 4
    else if (ihave>= 6.and.ihave<=8 ) then
      ihave = 9
    else
      iterm = 1
    end if
!
!  Exponent marker.
!
  else if (s_eqi(chrtmp,'e').or.s_eqi(chrtmp,'d')  ) then
    if ( ihave<6 ) then
      ihave = 6
    else
      iterm = 1
    end if
!
!  Digit.
!
  else if (ihave<11.and.lge(chrtmp,'0').and.lle(chrtmp,'9')  ) then

    if ( ihave<= 2 ) then
      ihave = 3
    else if (ihave ==4 ) then
      ihave = 5
    else if (ihave ==6.or.ihave==7 ) then
      ihave = 8
    else if (ihave ==9 ) then
      ihave = 10
    end if

    read(chrtmp,'(i1)')ndig

    if ( ihave ==3 ) then
      rtop = 10*rtop+ndig
    else if (ihave ==5 ) then
      rtop = 10*rtop+ndig
      rbot = 10*rbot
    else if (ihave ==8 ) then
      jtop = 10*jtop+ndig
    else if (ihave == 10 ) then
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
  if ( iterm/= 1.and.lchar+1<nchar)go to 10
!
!  If we haven't seen a terminator, and we have examined the
!  entire string, then we're done, and LCHAR is equal to NCHAR.
!
  if ( iterm/= 1.and.lchar+1==nchar)lchar=nchar
!
!  Number seems to have terminated.  Have we got a legal number?
!  Not if we terminated in states 1, 2, 6 or 7!
!
  if ( ihave == 1.or.ihave== 2.or.ihave==6.or.ihave==7 ) then
    ierror = ihave
    write ( *, * ) ' '
    write ( *, * ) 'ChrCTD - Fatal error!'
    write ( *, * ) '  Illegal or nonnumeric input!'
    return
  end if
!
!  Number seems OK.  Form it.
!
  if ( jtop == 0 ) then
    rexp = 1.0D+00
  else
    if ( jbot == 1 ) then
      rexp = 10.0**(jsgn*jtop)
    else
      rexp = real(jsgn*jtop, kind = 8 )
      rexp = rexp/real(jbot, kind = 8 )
      rexp = 10.0**rexp
    end if
  end if

  dval = real(isgn, kind = 8 )*rexp*rtop/rbot

  return
end
subroutine chrcti(string,intval,ierror,lchar)
!
!*******************************************************************************
!
!! CHRCTI accepts a STRING of characters and reads an integer
!  from STRING into INTVAL.  The STRING must begin with an integer
!  but that may be followed by other information.
!
!  CHRCTI will read as many characters as possible until it reaches
!  the end of the STRING, or encounters a character which cannot be
!  part of the number.
!
!  Legal input is
!
!    blanks,
!    initial sign,
!    integer part,
!    blanks,
!    final comma,
!
!  with most quantities optional.
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
!  INTVAL Output, integer INTVAL, the integer read from the string.
!
!  IERROR Output, integer IERROR, error flag.
!         0 if no errors,
!         Value of IHAVE when error occurred otherwise.
!
!  LCHAR  Output, integer LCHAR, number of characters read from
!         STRING to form the number.
!
!
  character chrtmp
  integer ierror
  integer ihave
  integer intval
  integer isgn
  integer iterm
  integer itop
  integer lchar
  integer nchar
  integer ndig
  character ( len = * )  string
!
  intrinsic len
  intrinsic lge
  intrinsic lle
!
  nchar = len(string)

  ierror = 0
  intval = 0
  lchar = -1
  isgn = 1
  itop = 0
  ihave = 1
  iterm = 0
10    continue
  lchar = lchar+1
  chrtmp = string(lchar+1:lchar+1)

  if ( chrtmp ==' ' ) then
    if ( ihave == 2 ) then
      iterm = 1
    else if (ihave ==3 ) then
      ihave = 11
    end if
  else if (chrtmp ==',' ) then
    if ( ihave/= 1 ) then
      iterm = 1
      ihave = 12
      lchar = lchar+1
    end if
  else if (chrtmp =='-' ) then
    if ( ihave == 1 ) then
      ihave = 2
      isgn = -1
    else
      iterm = 1
    end if
  else if (chrtmp =='+' ) then
    if ( ihave == 1 ) then
      ihave = 2
    else
      iterm = 1
    end if
  else if (lge(chrtmp,'0').and.lle(chrtmp,'9').and.ihave<11 ) then
    ihave = 3
    read(chrtmp,'(i1)')ndig
    itop = 10*itop+ndig
  else
    iterm = 1
  end if

  if ( iterm/= 1.and.lchar+1<nchar)go to 10
  if ( iterm/= 1.and.lchar+1==nchar)lchar=nchar
!
!  Number seems to have terminated.  Have we got a legal number?
!
  if ( ihave == 1.or.ihave== 2 ) then
    ierror = ihave
    write ( *, * ) ' '
    write ( *, * ) 'ChrCTI - Fatal error!'
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
subroutine chrdb1(string)
!
!*******************************************************************************
!
!! CHRDB1 accepts a string of characters and removes all
!  blanks and nulls, left justifying the remainder and padding with
!  blanks.
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
!  STRING Input/output, CHARACTER*(*) STRING, the string to be
!         transformed.
!
!
  character chrtmp
  integer i
  integer j
  integer nchar
  character ( len = * )  string
!
  intrinsic char
  intrinsic len
!
  nchar = len(string)

  j = 0

  do i = 1,nchar

    chrtmp = string(i:i)
    string(i:i) = ' '

    if ( chrtmp/= ' '.and.chrtmp/=char(0) ) then
      j = j+1
      string(j:j) = chrtmp
    end if

  end do

  return
end
subroutine chrup2(string,strng2,strng3)
!
!*******************************************************************************
!
!! CHRUP2 copies STRING into STRNG2, up to, but not including, the
!  first occurrence of the string STRNG3.  Setting STRING = 'ABCDEFGH'
!  and STRNG3 = 'EF' results in STRNG2='ABCD'.
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
!  STRING Input, CHARACTER*(*) STRING, the string to be copied.
!
!  STRNG2 Output, CHARACTER*(*) STRNG2, the copied portion of
!         STRING.
!
!  STRNG3 Input, CHARACTER*(*) STRNG3, the 'flag' string at which
!         the copy stops.
!
!
  integer i
  integer len1
  integer len2
  integer len3
  character ( len = * )  string
  character ( len = * )  strng2
  character ( len = * )  strng3
!
  intrinsic len
!
  len1 = len(string)
  len2 = len(strng2)
  len3 = len(strng3)

  strng2 = ' '
  i = 0
10    continue
  i = i+1
  if ( i>len1)return
  if ( i>len2)return

  if ( i+len3-1<= len1 ) then
    if ( string(i:i+len3-1) ==strng3)return
  end if

  strng2(i:i) = string(i:i)
  go to 10

end
subroutine cubspl(tau,c,n,ibcbeg,ibcend)
!
!*******************************************************************************
!
!! CUBSPL is given data and boundary conditions for a cubic
!  spline, and computes information that defines that cubic
!  spline.
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
!    FORTRAN90 version by John Burkardt
!
!  Parameters:
!
!  TAU    Input, real TAU(N), the abscissas or X values of
!         the data points.  The entries of TAU are assumed to be
!         strictly increasing.
!
!  N      Input, integer N, the number of data points.  N is
!         assumed to be at least 2.
!
!  C      Input/output, real C(4,N).
!
!         On input, if IBCBEG or IBCBEG is 1 or 2, then C(2,1)
!         or C(2,N) should have been set to the desired derivative
!         values, as described further under IBCBEG and IBCEND.
!
!         On output, C contains the polynomial coefficients of
!         the cubic interpolating spline with interior knots
!         TAU(2) through TAU(N-1).
!
!         In the interval interval (TAU(I), TAU(I+1)), the spline
!         F is given by
!
!           F(X) = C(1,I)+H*C(2,I)+(1/2)*H*H*C(3,I)
!                  +(1/6)*H*H*H*C(4,I)
!
!         where H = X - TAU(I).  The routine PPVALU may be used to
!         evaluate F or its derivatives from TAU, C, L = N-1,
!         and K = 4.
!
!  IBCBEG,
!  IBCEND Input, integer IBCBEG, IBCEND, boundary condition
!         indicators.
!
!         IBCBEG = 0 means no boundary condition at TAU(1) is given.
!         In this case, the "not-a-knot condition" is used.  That
!         is, the jump in the third derivative across TAU(2) is
!         forced to zero.  Thus the first and the second cubic
!         polynomial pieces are made to coincide.
!
!         IBCBEG = 1 means that the slope at TAU(1) is to equal the
!         input value C(2,1).
!
!         IBCBEG = 2 means that the second derivative at TAU(1) is
!         to equal C(2,1).
!
!         IBCEND = 0, 1, or 2 has analogous meaning concerning the
!         boundary condition at TAU(N), with the additional
!         information taken from C(2,N).
!
!
  integer n
!
  real ( kind = 8 ) c(4,n)
  real ( kind = 8 ) divdf1
  real ( kind = 8 ) divdf3
  real ( kind = 8 ) dtau
  real ( kind = 8 ) g
  integer i
  integer ibcbeg
  integer ibcend
  integer j
  integer m
  real ( kind = 8 ) tau(n)
!
!  A tridiagonal linear system for the unknown slopes S(I) of
!  F at TAU(I), I = 1,..., N, is generated and then solved by Gauss
!  elimination, with S(I) ending up in C(2,I), for all I.
!
!  C(3,*) and C(4,*) are used initially for temporary storage.
!
!  Store first differences of the TAU sequence in C(3,*).
!
!  Store first divided difference of data in C(4,*).
!
  do m = 2,n
    c(3,m) = tau(m)-tau(m-1)
    c(4,m) = (c(1,m)-c(1,m-1))/c(3,m)
  end do
!
!  Construct the first equation from the boundary condition, of
!  the form:
!
!    C(4,1)*S(1) + C(3,1)*S(2) = C(2,1)
!
  if ( ibcbeg == 1 ) then
    c(4,1) = 1.0D+00
    c(3,1) = 0.0D+00
    go to 60
  end if

  if ( ibcbeg<= 1 ) then
!
!  No condition at left end and N = 2.
!
    if ( n<= 2 ) then
      c(4,1) = 1.0D+00
      c(3,1) = 1.0D+00
      c(2,1) = 2.0*c(4,2)
      go to 120
    end if
!
!  Not-a-knot condition at left end and N is greater than 2.
!
    c(4,1) = c(3,3)
    c(3,1) = c(3,2)+c(3,3)
    c(2,1) = ((c(3,2)+2.0*c(3,1))*c(4,2)*c(3,3)+c(3,2)**2*c(4,3))/c(3,1)
    go to 70

  end if
!
!  Second derivative prescribed at left end.
!
  c(4,1) = 2.0D+00
  c(3,1) = 1.0D+00
  c(2,1) = 3.0*c(4,2)-c(3,2)/2.0*c(2,1)

60    continue

  if ( n == 2)go to 120
!
!  If there are interior knots, generate the corresponding
!  equations and carry out the forward pass of Gauss elimination,
!  after which the M-th equation reads:
!
!    C(4,M)*S(M) + C(3,M)*S(M+1) = C(2,M).
!
70    continue

  do m = 2,n-1
    g = -c(3,m+1)/c(4,m-1)
    c(2,m) = g*c(2,m-1)+3.0*(c(3,m)*c(4,m+1)+c(3,m+1)*c(4,m))
    c(4,m) = g*c(3,m-1)+2.0*(c(3,m)+c(3,m+1))
  end do
!
!  Construct last equation from the second boundary condition, of
!  the form
!
!    (-G*C(4,N-1))*S(N-1) + C(4,N)*S(N) = C(2,N)
!
!  If slope is prescribed at right end, one can go directly to
!  back-substitution, since the C array happens to be set up just
!  right for it at this point.
!
  if ( ibcend == 1)go to 160
  if ( ibcend>1)go to 110
!
!  Not-a-knot and N >=  3, and either N>3 or also not-a-knot
!  at left end point.
!
  if ( n/= 3.or.ibcbeg/=0 ) then
    g = c(3,n-1)+c(3,n)
    c(2,n) = ((c(3,n)+2.0*g)*c(4,n)*c(3,n-1)+c(3,n)**2*(c(1,n-1)-c(1,n-2)) &
      /c(3,n-1))/g
    g = -g/c(4,n-1)
    c(4,n) = c(3,n-1)
    c(4,n) = g*c(3,n-1)+c(4,n)
    c(2,n) = (g*c(2,n-1)+c(2,n))/c(4,n)
    go to 160
  end if
!
!  Either (N = 3 and not-a-knot also at left) or (N=2 and not not-a-
!  knot at left end point).
!
100   continue

  c(2,n) = 2.0*c(4,n)
  c(4,n) = 1.0D+00
  g = -1.0/c(4,n-1)
  c(4,n) = g*c(3,n-1)+c(4,n)
  c(2,n) = (g*c(2,n-1)+c(2,n))/c(4,n)
  go to 160
!
!  Second derivative prescribed at right endpoint.
!
110   continue

  c(2,n) = 3.0*c(4,n)+c(3,n)/2.0*c(2,n)
  c(4,n) = 2.0D+00
  g = -1.0/c(4,n-1)
  c(4,n) = g*c(3,n-1)+c(4,n)
  c(2,n) = (g*c(2,n-1)+c(2,n))/c(4,n)
  go to 160

120   continue

  if ( ibcend/= 1 ) then

    if ( ibcend>1 ) then
      c(2,n) = 3.0*c(4,n)+c(3,n)/2.0*c(2,n)
      c(4,n) = 2.0D+00
      g = -1.0/c(4,n-1)
      c(4,n) = g*c(3,n-1)+c(4,n)
      c(2,n) = (g*c(2,n-1)+c(2,n))/c(4,n)
      go to 160
    end if

    if ( ibcbeg>0)go to 100
!
!  Not-a-knot at right endpoint and at left endpoint and N = 2.
!
    c(2,n) = c(4,n)

  end if
!
!  Carry out the back substitution
!
160   continue

  do j = n-1,1,-1
    c(2,j) = (c(2,j)-c(3,j)*c(2,j+1))/c(4,j)
  end do
!
!  Generate cubic coefficients in each interval, that is, the
!  derivatives at its left endpoint, from value and slope at its
!endpoints.
!
  do i = 2,n
    dtau = c(3,i)
    divdf1 = (c(1,i)-c(1,i-1))/dtau
    divdf3 = c(2,i-1)+c(2,i)-2.0*divdf1
    c(3,i-1) = 2.0*(divdf1-c(2,i-1)-divdf3)/dtau
    c(4,i-1) = (divdf3/dtau)*(6.0/dtau)
  end do

  return
end
subroutine discst(costp,costu,costv,g,gtar,indx,neqn,np,nprof,ny,yc)
!
!*******************************************************************************
!
!! DISCST computes the discrepancy integrals along the profile line.
!
!  Discussion:
!
!    There are discrepancy integrals for the pressure,
!    horizontal and vertical velocities.
!
!    This integration scheme assumes that the profile line, and
!    the element sides that define it, are straight.  Otherwise,
!    the integration scheme used is not correct.
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
  integer, parameter :: nquad1 = 5
!
  integer neqn
  integer np
  integer ny
!
  real ( kind = 8 ) bval
  real ( kind = 8 ) costp
  real ( kind = 8 ) costu
  real ( kind = 8 ) costv
  real ( kind = 8 ) g(neqn)
  real ( kind = 8 ) gtar(neqn)
  integer i
  integer ii
  integer indx(3,np)
  integer j
  integer k
  integer npol
  integer nprof(2*ny-1)
  real ( kind = 8 ) pcof(2)
  real ( kind = 8 ) pval
  real ( kind = 8 ) ucof(3)
  real ( kind = 8 ) uval
  real ( kind = 8 ) vcof(3)
  real ( kind = 8 ) vval
  real ( kind = 8 ) wquad1(nquad1)
  real ( kind = 8 ) xquad1(nquad1)
  real ( kind = 8 ) yc(np)
  real ( kind = 8 ) yhi
  real ( kind = 8 ) ylo
  real ( kind = 8 ) ypol(3)
  real ( kind = 8 ) yval
!
!  Get the weights and abscissas to approximate a line integral.
!
  call gquad1(nquad1,wquad1,xquad1)
!
!  Compute the integral of the difference squared between the
!  current velocity and the target values.
!
  costu = 0.0D+00
  costv = 0.0D+00
!
!  The line along which we integrate is broken into NY-1
!  subintervals, over each of which, U and V are represented
!  by quadratic functions.
!
  do i = 1,ny-1
!
!  Get the values of U and V at the beginning, middle, and
!end of the subinterval.  Use these to compute the quadratic
!  representation of U and V for any point on the subinterval.
!
    ylo = yc(nprof(2*i-1))
    yhi = yc(nprof(2*i+1))

    npol = 3

    do k = 1,npol

      ii = 2*i-2+k
      ypol(k) = yc(nprof(ii))

      j = indx(1,nprof(ii))
      ucof(k) = g(j)-gtar(j)

      j = indx(2,nprof(ii))
      vcof(k) = g(j)-gtar(j)

    end do
!
!  Evaluate the discrepancy at each quadrature point.
!
    do j = 1,nquad1

      call rint_to_rint ( -1.0D+00, +1.0D+00, xquad1(j), ylo, yhi, yval )

      uval = 0.0D+00
      vval = 0.0D+00

      do k = 1,npol
        call lbase(k,npol,bval,ypol,yval)
        uval = uval+bval*ucof(k)
        vval = vval+bval*vcof(k)
      end do

      costu = costu + 0.5 * wquad1(j)*(yhi-ylo)*uval**2
      costv = costv + 0.5 * wquad1(j)*(yhi-ylo)*vval**2

    end do
  end do
!
!  Compute the square root of the integral of the difference
!  squared between the current pressure and the target values.
!
  costp = 0.0D+00

  do i = 1,ny-1

    ylo = yc(nprof(2*i-1))
    yhi = yc(nprof(2*i+1))

    npol = 2

    do k = 1,npol

      ii = 2*i-3+2*k

      ypol(k) = yc(nprof(ii))

      j = indx(3,nprof(ii))
      if ( j<= 0 ) then
        pcof(k) = 0.0D+00
      else
        pcof(k) = g(j)-gtar(j)
      end if

    end do

    do j = 1,nquad1

      call rint_to_rint ( -1.0D+00, +1.0D+00, xquad1(j), ylo, yhi, yval )

      pval = 0.0D+00

      do k = 1,npol
        call lbase(k,npol,bval,ypol,yval)
        pval = pval+bval*pcof(k)
      end do

      costp = costp+0.5*wquad1(j)*(yhi-ylo)*pval**2

    end do
  end do

  return
end
subroutine disder(dpara,g,gtar,indx,maxeqn,neqn,np,npar,nprof,ny,sens, &
  watep,wateu,watev,yc)
!
!*******************************************************************************
!
!! DISDER computes the derivative of the discrepancy integrals for the
!  pressure, horizontal and vertical velocities, with respect to the
!  various parameters.
!
!    In practice, the dummy parameter DPARA will actually be DPARA1 or
!    DPARA2, and the dummy input parameter SENS will actually be SENS or
!    GDIF, depending on which estimate of the cost derivatives is
!    desired.
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
!  DPARA  Input/output, real DPARA(NPAR).
!
!         DPARA contains the derivatives of the cost function with
!         respect to the various parameters.  In particular,
!         DPARA(I) = D cost / D parameter(I).
!
!
  integer, parameter :: mpar = 10
  integer, parameter :: nquad1 = 5
!
  integer maxeqn
  integer neqn
  integer np
  integer npar
  integer ny
!
  real ( kind = 8 ) bval
  real ( kind = 8 ) dpara(npar)
  real ( kind = 8 ) dpcof(mpar,mpar)
  real ( kind = 8 ) dpval(mpar)
  real ( kind = 8 ) ducof(mpar,mpar)
  real ( kind = 8 ) duval(mpar)
  real ( kind = 8 ) dvcof(mpar,mpar)
  real ( kind = 8 ) dvval(mpar)
  real ( kind = 8 ) g(neqn)
  real ( kind = 8 ) gtar(neqn)
  integer i
  integer ii
  integer indx(3,np)
  integer j
  integer k
  integer l
  integer npol
  integer nprof(2*ny-1)
  real ( kind = 8 ) pcof(2)
  real ( kind = 8 ) pval
  real ( kind = 8 ) sens(maxeqn,npar)
  real ( kind = 8 ) ucof(3)
  real ( kind = 8 ) uval
  real ( kind = 8 ) vcof(3)
  real ( kind = 8 ) vval
  real ( kind = 8 ) watep
  real ( kind = 8 ) wateu
  real ( kind = 8 ) watev
  real ( kind = 8 ) wquad1(nquad1)
  real ( kind = 8 ) xquad1(nquad1)
  real ( kind = 8 ) yc(np)
  real ( kind = 8 ) yhi
  real ( kind = 8 ) ylo
  real ( kind = 8 ) ypol(3)
  real ( kind = 8 ) yval
!
  if ( npar>mpar ) then
    write ( *, * ) ' '
    write ( *, * ) 'DisDer - Fatal error!'
    write ( *, * ) '  The number of parameters is too high.'
    write ( *, * ) '  This routine can handle NPAR = ',mpar
    write ( *, * ) '  Your problem has NPAR = ',npar
    stop
  end if
!
!  Get the Gauss weights and abscissas to approximate a line
!  integral.
!
  call gquad1(nquad1,wquad1,xquad1)
!
!  The line along which we integrate is broken into NY-1
!  subintervals, over each of which, U and V are represented
!  by quadratic functions.
!
  do i = 1,ny-1
!
!  Get the values of U and V at the beginning, middle, and
!end of the subinterval.  Use these to compute the quadratic
!  representation of U and V for any point on the subinterval.
!
    ylo = yc(nprof(2*i-1))
    yhi = yc(nprof(2*i+1))

    npol = 3

    do k = 1,npol

      ii = 2*i-2+k

      ypol(k) = yc(nprof(ii))

      j = indx(1,nprof(ii))
      ucof(k) = g(j)-gtar(j)

      do l = 1,npar
        ducof(k,l) = sens(j,l)
      end do

      j = indx(2,nprof(ii))
      vcof(k) = g(j)-gtar(j)

      do l = 1,npar
        dvcof(k,l) = sens(j,l)
      end do

    end do
!
!  Evaluate the discrepancy at each quadrature point.
!
    do j = 1,nquad1

      call rint_to_rint ( -1.0D+00, +1.0D+00, xquad1(j), ylo, yhi, yval )

      uval = 0.0D+00
      vval = 0.0D+00
      do l = 1,npar
        duval(l) = 0.0D+00
        dvval(l) = 0.0D+00
      end do

      do k = 1,npol
        call lbase(k,npol,bval,ypol,yval)
        uval = uval+bval*ucof(k)
        vval = vval+bval*vcof(k)
        do l = 1,npar
          duval(l) = duval(l)+bval*ducof(k,l)
          dvval(l) = dvval(l)+bval*dvcof(k,l)
        end do
      end do

      do l = 1,npar
        dpara(l) = dpara(l)+0.5*wquad1(j)*(yhi-ylo) &
             *2.0*(wateu*uval*duval(l)+watev*vval*dvval(l))
      end do

    end do
  end do
!
!  Compute the square root of the integral of the difference
!  squared between the current pressure and the target values.
!

  do i = 1,ny-1

    ylo = yc(nprof(2*i-1))
    yhi = yc(nprof(2*i+1))

    npol = 2

    do k = 1,npol

      ii = 2*i-3+2*k

      ypol(k) = yc(nprof(ii))

      j = indx(3,nprof(ii))
      if ( j<= 0 ) then
        pcof(k) = 0.0D+00
      else
        pcof(k) = g(j)-gtar(j)
      end if

      do l = 1,npar
        if ( j<= 0 ) then
          dpcof(k,l) = 0.0D+00
        else
          dpcof(k,l) = sens(j,l)
        end if
      end do

    end do

    do j = 1,nquad1

      call rint_to_rint ( -1.0D+00, +1.0D+00, xquad1(j), ylo, yhi, yval )

      pval = 0.0D+00
      do l = 1,npar
        dpval(l) = 0.0D+00
      end do

      do k = 1,npol
        call lbase(k,npol,bval,ypol,yval)
        pval = pval+bval*pcof(k)
        do l = 1,npar
          dpval(l) = dpval(l)+bval*dpcof(k,l)
        end do

      end do

      do l = 1,npar
        dpara(l) = dpara(l)+0.5*wquad1(j)*(yhi-ylo)*2.0*watep*pval*dpval(l)
      end do

    end do
  end do

  return
end
subroutine floduv(ifs,ipar,nparf,splflo,tauflo,ubc,vbc,yy)
!
!***************************************************************************
!
!! FLODUV sets the derivative of the parabolic inflow in terms of
!  the value of the inflow parameters.
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
!  UBC    Output, real UBC.
!         The value of the partial derivative of the horizontal
!         boundary velocity component with respect to a given
!         parameter at the specified point.
!
!  VBC    Output, real VBC.
!         The value of the partial derivative of the vertical
!         boundary velocity component with respect to a given
!         parameter at the specified point.
!
!
  integer nparf
!
  integer ifs
  integer ipar
  integer jderiv
  real ( kind = 8 ) splflo(4,nparf+2,0:nparf)
  real ( kind = 8 ) tauflo(nparf+2)
  real ( kind = 8 ) ubc
  real ( kind = 8 ) vbc
  real ( kind = 8 ) yy
!
  ubc = 0.0D+00
  vbc = 0.0D+00
!
!  Handle the case where there is only one inflow parameter, and it
!  is specified as a constant.
!
  if ( nparf<= 0.and.ipar== 0 ) then
    ubc = 4.0*(3.0-yy)*yy/(9.0)
    return
  end if

  if ( ipar<1.or.ipar>nparf)return
!
!  Evaluate the basis function which is zero at the other parameters
!  and 1 at parameter IPAR.
!
  if ( ifs == 1 ) then
    call plval1(ipar+1,nparf+2,yy,tauflo,ubc)
  else if (ifs == 2 ) then
    call pqval1(ipar+1,nparf+2,yy,tauflo,ubc)
  else if (ifs ==3 ) then
    jderiv = 0
    call ppvalu(tauflo,splflo(1,1,ipar),nparf+1,4,yy,jderiv,ubc)
  else
    write ( *, * ) ' '
    write ( *, * ) 'FloDUV - Fatal error!'
    write ( *, * ) '  Illegal value of ISHAPF = ',ifs
    stop
  end if

  return
end
subroutine flosen(eqn,f,ifs,indx,ipar,neqn,np,nparf,splflo,tauflo,yc)
!
!*******************************************************************************
!
!! FLOSEN sets up the right hand side F associated with the
!  sensitivities of a given flow solution (U,V,P) with
!  respect to the IPAR-th inflow parameter.
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
!  F      Output, real F(NEQN), the right hand
!         side of the sensitivity equations associated with
!         the IPAR-th inflow parameter.
!
!
  integer neqn
  integer np
  integer nparf
!
  character ( len = 2 ) eqn(neqn)
  real ( kind = 8 ) f(neqn)
  integer i
  integer ifs
  integer ihor
  integer indx(3,np)
  integer ip
  integer ipar
  integer iver
  real ( kind = 8 ) splflo(4,nparf+2,0:nparf)
  real ( kind = 8 ) tauflo(nparf+2)
  real ( kind = 8 ) ubc
  real ( kind = 8 ) vbc
  real ( kind = 8 ) yc(np)
!
  do i = 1,neqn
    f(i) = 0.0D+00
  end do

  do ip = 1,np

    ihor = indx(1,ip)
    if ( eqn(ihor) =='UI' ) then
      call floduv(ifs,ipar,nparf,splflo,tauflo,ubc,vbc,yc(ip))
      f(ihor) = ubc
    end if

    iver = indx(2,ip)
    if ( eqn(iver) =='VI' ) then
      call floduv(ifs,ipar,nparf,splflo,tauflo,ubc,vbc,yc(ip))
      f(iver) = vbc
    end if

  end do

  return
end
subroutine flosol(a,area,eqn,etaq,g,g2,ifs,ibs,ierror,ijac,indx,ipivot, &
  isotri,iwrite,jjac,maxnew,nelem,neqn,nlband,node,np,npar,nparb,nparf, &
  nquad,nrow,nx,ny,para,phi,res,splbmp,splflo,syseqn,taubmp,tauflo,tolnew, &
  wquad,xbl,xbr,xc,xquad,xsiq,ybl,ybr,yc,yquad)
!
!*******************************************************************************
!
!! FLOSOL is given a set of flow parameters in PARA, and an
!  approximate solution vector G, and proceeds to set up the
!  constraints associated with PARA, and use Newton iteration
!  to correct G to a solution that satisfies the constraints
!  to within some tolerance.
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
!    Output, real G(NEQN), the computed solution vector, in which are stored
!    pressures and velocities.
!
!    Output, integer IERROR, an error flag.  
!    0 means no error, nonzero means an error.
!
  integer nelem
  integer neqn
  integer np
  integer npar
  integer nparb
  integer nparf
  integer nquad
  integer nrow
  integer nx
  integer ny
!
  real ( kind = 8 ) a(nrow,neqn)
  real ( kind = 8 ) area(nquad,nelem)
  character ( len = 2 ) eqn(neqn)
  real ( kind = 8 ) etaq(nquad)
  real ( kind = 8 ) g(neqn)
  real ( kind = 8 ) g2(neqn)
  integer ifs
  integer ibs
  integer ierror
  integer ijac
  integer indx(3,np)
  integer ipivot(neqn)
  integer isotri(nelem)
  integer iwrite
  integer jjac
  logical s_eqi
  integer maxnew
  integer nlband
  integer node(6,nelem)
  real ( kind = 8 ) para(npar)
  real ( kind = 8 ) phi(nquad,6,10,nelem)
  real ( kind = 8 ) res(neqn)
  real ( kind = 8 ) splbmp(4,nparb+2,0:nparb)
  real ( kind = 8 ) splflo(4,nparf+2,0:nparf)
  character ( len = 20 ) syseqn
  real ( kind = 8 ) taubmp(nparb+2)
  real ( kind = 8 ) tauflo(nparf+2)
  real ( kind = 8 ) tolnew
  real ( kind = 8 ) wquad(nquad)
  real ( kind = 8 ) xbl
  real ( kind = 8 ) xbr
  real ( kind = 8 ) xc(np)
  real ( kind = 8 ) xquad(nquad,nelem)
  real ( kind = 8 ) xsiq(nquad)
  real ( kind = 8 ) ybl
  real ( kind = 8 ) ybr
  real ( kind = 8 ) yc(np)
  real ( kind = 8 ) yquad(nquad,nelem)
!
  external s_eqi
!
!  Set the spline coefficients for the bump.
!
  call bmpspl(ibs,npar,nparb,nparf,para,splbmp,taubmp,xbl,xbr,ybl,ybr)
!
!  Set the spline coefficients for the inflow.
!
  call flospl(ifs,npar,nparf,para,splflo,tauflo)
!
!  Set the X and Y coordinates of the nodes that form the grid.
!
  call setxy(ibs,np,nparb,nx,ny,splbmp,taubmp,xbl,xbr,xc,ybl,ybr,yc)
!
!  Set the quadrature points, which move every step if there
!  are bump parameters.
!
  if ( nquad ==3 ) then
    call setq3(area,etaq,isotri,nelem,node,np,nquad,wquad,xc, &
      xquad,xsiq,yc,yquad)
  else if (nquad ==7 ) then
    call setq7(area,etaq,isotri,nelem,node,np,nquad,wquad,xc, & 
      xquad,xsiq,yc,yquad)
  end if
!
!  Set the value of the basis functions at all quadrature points.
!
  call setbas(area,etaq,isotri,nelem,node,np,nquad,phi,xc,xquad,xsiq,yc,yquad)
!
!  Try to solve the full nonlinear system.
!
  call newton(a,area,eqn,g,g2,ifs,ierror,ijac,indx,ipivot,iwrite,jjac,maxnew, &
    nelem,neqn,nlband,node,np,npar,nparb,nparf,nquad,nrow,para,phi,res,splflo, &
    syseqn,tauflo,tolnew,yc)

  if ( ierror/= 0.and.s_eqi(syseqn,'NavierStokes') ) then
    write ( *, * ) ' '
    write ( *, * ) 'FloSol - Warning!'
    write ( *, * ) '  Newton failed on '//syseqn
    write ( *, * ) '  The parameters at which failure occurred:'
    write ( *, * ) ' '
    call prpar(nparb,nparf,para)
    syseqn = 'Stokes'
    write ( *, * ) '  Switching to '//syseqn
!
!  Try to solve the linearized system.
!
    call newton ( a, area, eqn, g, g2, ifs, ierror, ijac, indx, ipivot, &
      iwrite,jjac,maxnew, &
      nelem,neqn,nlband,node,np,npar,nparb,nparf,nquad,nrow,para,phi,res, &
      splflo,syseqn,tauflo,tolnew,yc)

    syseqn = 'NavierStokes'

    if ( ierror/= 0 ) then
      write ( *, * ) ' '
      write ( *, * ) 'FloSol - Fatal error!'
      write ( *, * ) '  Newton failed!'
      write ( *, * ) '  The parameters at which failure occurred:'
      write ( *, * ) ' '
      call prpar(nparb,nparf,para)
      ierror = 1
      return
    end if

  end if

  return
end
subroutine flospl(ifs,npar,nparf,par,splflo,tauflo)
!
!*******************************************************************************
!
!! FLOSPL sets up or updates the spline data that describes the inflow.
!
!  It does this for the target parameters and the feasible parameters.
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
  integer npar
  integer nparf
!
  integer i
  integer ibcbeg
  integer ibcend
  integer ifs
  integer ihi
  integer ipar
  real ( kind = 8 ) par(npar)
  real ( kind = 8 ) splflo(4,nparf+2,0:nparf)
  real ( kind = 8 ) tauflo(nparf+2)
!
!  Set up the abscissas in TAUFLO, which will be evenly spaced.
!  If there are no flow parameters, then we generate three evenly
!  spaced nodes.
!
  ihi = nparf+2

  do i = 1,ihi
    tauflo(i) = real(i-1, kind = 8 )*3.0/real(ihi-1, kind = 8 )
  end do
!
!  Set up the coefficient arrays, including
!  SPLFLO(1,I,0), the strength of the inflow at abscissa I,
!  SPLFLO(1,I,IPAR), the I-th coefficient of the partial derivative
!  of the inflow with respect to the IPAR-th inflow parameter.
!
  if ( ifs == 1.or.ifs== 2 ) then

    do i = 1,nparf+2

      if ( i == 1 ) then
        splflo(i,1,0) = 0.0D+00
      else if (2<= i.and.i<=nparf+1 ) then
        splflo(i,1,0) = par(i-1)
      else if (i ==nparf+2 ) then
        splflo(i,1,0) = 0.0D+00
      end if

    end do

  else if (ifs ==3 ) then

    do i = 1,nparf+2

      if ( i == 1 ) then
        splflo(1,i,0) = 0.0D+00
      else if (2<= i.and.i<=nparf+1 ) then
        splflo(1,i,0) = par(i-1)
      else if (i ==nparf+2 ) then
        splflo(1,i,0) = 0.0D+00
      end if

      do ipar = 1,nparf
        if ( ipar+1/= i ) then
          splflo(1,i,ipar) = 0.0D+00
        else
          splflo(1,i,ipar) = 1.0D+00
        end if
      end do

    end do

    ibcbeg = 0
    ibcend = 0
    do i = 0,nparf
      call cubspl(tauflo,splflo(1,1,i),nparf+2,ibcbeg,ibcend)
    end do

  end if

  return
end
subroutine flouv(ifs,nparf,splflo,tauflo,ubc,vbc,yy)
!
!***************************************************************************
!
!! FLOUV computes the specified boundary values of velocity for a
!  given position as determined by the value of the parameters.
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
  integer nparf
!
  integer ifs
  integer jderiv
  real ( kind = 8 ) splflo(4,nparf+2,0:nparf)
  real ( kind = 8 ) tauflo(nparf+2)
  real ( kind = 8 ) ubc
  real ( kind = 8 ) vbc
  real ( kind = 8 ) yy
!
  if ( ifs == 1 ) then
    call plval(nparf+2,yy,tauflo,ubc,splflo)
  else if (ifs == 2 ) then
    call pqval(nparf+2,yy,tauflo,ubc,splflo)
  else if (ifs ==3 ) then
    jderiv = 0
    call ppvalu(tauflo,splflo(1,1,0),nparf+1,4,yy,jderiv,ubc)
  else
    write ( *, * ) ' '
    write ( *, * ) 'FloUV - Fatal error!'
    write ( *, * ) '  Illegal value of ISHAPF = ',ifs
    stop
  end if

  vbc = 0.0D+00

  return
end
subroutine fp ( a, area, eqn, g, indx, nelem, neqn, nlband, node, np, &
  npar, nparb, nparf, nquad, nrow, para, phi, syseqn )
!
!*******************************************************************************
!
!! FP sets up a matrix associated with Navier-Stokes or Stokes flow.
!
!  Discussion:
!
!    If the calling routine is trying to solve the Stokes equations,
!    (which are linearized Navier-Stokes equations), then
!    this routine sets up the appropriate system matrix.
!
!    If the calling routine is trying to solve the nonlinear Navier-Stokes
!    equations, then this routine computes the jacobian of the 
!    finite element form of the Navier Stokes equations.
!
!    The differentiated finite element Navier Stokes functions have
!    the form:
!
!
!    d U-Eqn/d U-Coef:
!
!      Integral
!
!      dWj/dx * dWi/dx + dWj/dy * dWi/dy
!      + nu_inv * (Wj*dUold/dx + Uold*dWj/dx+ Vold*dWj/dy) * Wi dx dy
!
!    d U-Eqn/d V-Coef:
!
!      Integral
!
!      nu_inv * Wj*dUold/dy * Wi dx dy
!
!    d U-Eqn/d P-Coef:
!
!      Integral
!
!      nu_inv * dQj/dx * Wi dx dy
!
!    d V-Eqn/d U-Coef:
!
!      Integral
!
!      nu_inv * Wj*dVold/dx * Wi dx dy
!
!    d V-Eqn/d V-Coef:
!
!      Integral
!
!      dWj/dx * dWi/dx + dWj/dy * dWi/dy
!      + nu_inv * (Uold*dWj/dx + Wj*dVold/dy + Vold*dWj/dy) * Wi dx dy
!
!    d V-Eqn/d P-Coef:
!
!      Integral
!
!      nu_inv * dQj/dy * Wi dx dy
!
!    d P-Eqn/d U-Coef:
!
!      Integral
!
!      dWj/dx * Qi dx dy
!
!    d P-Eqn/d V-Coef:
!
!      Integral
!
!      dWj/dy * Qi dx dy
!
!
!    The Stokes equations have the form:
!
!
!    d U-Eqn/d U-Coef:
!
!      Integral
!
!      dWj/dx * dWi/dx + dWj/dy * dWi/dy dx dy
!
!    d U-Eqn/d P-Coef:
!
!      Integral
!
!      nu_inv * dQj/dx * Wi dx dy
!
!    d V-Eqn/d V-Coef:
!
!      Integral
!
!      dWj/dx * dWi/dx + dWj/dy * dWi/dy dx dy
!
!    d V-Eqn/d P-Coef:
!
!      Integral
!
!      nu_inv * dQj/dy * Wi dx dy
!
!    d P-Eqn/d U-Coef:
!
!      Integral
!
!      dWj/dx * Qi dx dy
!
!    d P-Eqn/d V-Coef:
!
!      Integral
!
!      dWj/dy * Qi dx dy
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
!  A      Output, real A(NROW,NEQN), contains the
!         value of D F(I)/D X(J) for each of the NEQN residual
!         functions F(I) with respect to each of the unknown
!         coefficients X(J).
!
!  AREA   Input, real AREA(3,NELEM).
!
!         AREA contains a common factor multiplying the term associated
!         with a quadrature point in a given element, namely,
!
!           AREA(IQUAD,IELEM) = Ar(IELEM) * WQUAD(IQUAD)
!
!         or, if the element is isoperimetric,
!
!           AREA(IQUAD,IELEM) = DET * Ar(IELEM) * WQUAD(IQUAD)
!
!         Here Ar(IELEM) represents the area of element IELEM.
!
!  EQN    Input, character ( len = 2 ) EQN(NEQN).
!         EQN records the "type" of each equation that will be generated, and
!         which is associated with an unknown.  Note that most boundary
!         conditions do not result in an equation.  The current values are:
!
!         'U'  The horizontal momentum equation.
!         'UB' The condition U = 0 applied at a node on the bump.
!         'UI' The condition U = UInflow(Y,Lambda) at the inflow.
!         'UW' The condition U = 0 applied at a node on a fixed wall.
!
!         'V'  The vertical momentum equation.
!         'VB' The condition V = 0 applied at a node on the bump.
!         'VI' The condition V = VInflow(Y,Lambda) at the inflow.
!         'VW' The condition V = 0 applied at a node on a fixed wall.
!
!         'P'  The continuity equation.
!         'PB' The condition P = 0 applied at (XMAX,YMAX).
!
!  G      Input, real G(NEQN).
!
!         G is the current solution vector, in which are stored
!         the finite element coefficients that define the velocity
!         and pressure functions, U, V and P.
!
!  INDX   Input, integer INDX(3,NP).
!
!         INDX(I,J) contains, for each node J, the index of U, V and P at
!         that node, or 0 or a negative value.
!
!         If K = INDX(I,J) is positive, then the value of the degree
!         of freedom is stored in the solution vector entry G(K),
!         and an equation will be generated to determine its value.
!
!         If INDX(I,J) is not positive, then no equation is
!         generated to determine for variable I at node J, either because
!         the variable is specified in some other way, or because
!         (in the case of pressure), there is no coefficient associated
!         with that node.
!
!  NELEM  Input, integer NELEM, the number of elements.
!
!  NEQN   Input, integer NEQN, the number of finite element equations used
!         to define the horizontal and vertical velocities and the
!         pressure.
!
!  NLBAND Input, integer NLBAND.
!
!         The lower bandwidth of the matrix A.  The zero structure of A
!         is assumed to be symmetric, and so NLBAND is also the upper
!         bandwidth of A.
!
!  NODE   Input, integer NODE(6,NELEM).
!
!         NODE(I,J) contains, for an element J, the global node index of
!         the element node whose local number is I.
!
!         The local ordering of the nodes is suggested by this diagram:
!
!               2
!              /|
!             4 5
!            /  |
!           1-6-3
!
!  NP     Input, integer NP, the number of nodes used to define the finite
!         element mesh.  NP = (2*NX-1)*(2*NY-1).
!
!  NPAR   Input, integer NPAR.
!
!         The number of parameters.  NPAR = NPARF + NPARB + 1.
!
!         The parameters control the shape of the inflow,
!         the shape of the bump obstacle, and the strength of the
!         flow.
!
!  NPARB  Input, integer NPARB.
!
!         The number of parameters associated with the position and
!         shape of the bump.
!
!         Note that if NPARB = 0, the bump is replaced by a flat wall.
!
!  NPARF  Input, integer NPARF.
!
!         NPARF is the number of parameters associated with the
!         inflow.  NPARF must be at least 1.
!
!  NROW   Input, integer NROW.
!
!         The number of rows need to store the matrix A, using the
!         LINPACK/LAPACK general banded storage format.  NROW must be
!         at least 3*NLBAND+1.
!
!  PARA   Input, real PARA(NPAR).
!
!         PARA is the current set of parameter values, including the
!         inverse viscosity, the flow parameters, and the bump parameters.
!
!  PHI    Input, real PHI(NQUAD,6,10,NELEM).
!
!         PHI contains the value of a basis function, its derivative,
!         or other information, evaluated at a quadrature point.
!
!         For a particular element I, quadrature point J, and basis
!         function K, we use the following shorthand for the 10
!         entries of PHI:
!
!           W, dWdX, dWdY
!           Q, dQdX, dQdY
!           dXsidX, dXsidY, dEtadX, dEtadY
!
!         W is the quadratic basis function associated with velocity,
!         Q the linear basis function associated with pressure,
!         Xsi and Eta the reference coordinates for the point.
!
!         In particular, PHI(J,K,1,I) is the value of the quadratic
!         basis function associated with local node K in element I,
!         evaluated at quadrature point J.
!
!         Note that PHI(J,K,4,I) = PHI(J,K,5,I)=PHI(J,K,6,I)=0 for
!         K = 4, 5, or 6, since there are only three linear basis
!         functions.
!
!  SYSEQN Input, character ( len = 20 ) SYSEQN.
!
!         Input to the FX and FP routines, SYSEQN is either 'NavierStokes'
!         or 'Stokes', and specifies which state equation is to be set up.
!
!
  integer nelem
  integer neqn
  integer np
  integer npar
  integer nparf
  integer nquad
  integer nrow
!
  real ( kind = 8 ) a(nrow,neqn)
  real ( kind = 8 ) ar
  real ( kind = 8 ) area(nquad,nelem)
  real ( kind = 8 ) dpdx
  real ( kind = 8 ) dpdy
  real ( kind = 8 ) dqjdx
  real ( kind = 8 ) dqjdy
  real ( kind = 8 ) dudx
  real ( kind = 8 ) dudy
  real ( kind = 8 ) dvdx
  real ( kind = 8 ) dvdy
  real ( kind = 8 ) dwidx
  real ( kind = 8 ) dwidy
  real ( kind = 8 ) dwjdx
  real ( kind = 8 ) dwjdy
  character ( len = 2 ) eqn(neqn)
  real ( kind = 8 ) g(neqn)
  integer i
  integer ielem
  integer ihor
  integer indx(3,np)
  integer ip
  integer iprs
  integer iq
  integer iquad
  integer iuse
  integer iver
  integer j
  integer jhor
  integer jp
  integer jprs
  integer jq
  integer jver
  logical s_eqi
  integer nlband
  integer node(6,nelem)
  integer nparb
  real ( kind = 8 ) pold
  real ( kind = 8 ) para(npar)
  real ( kind = 8 ) phi(nquad,6,10,nelem)
  real ( kind = 8 ) qi
  real ( kind = 8 ) nu_inv
  character ( len = * )  syseqn
  real ( kind = 8 ) term
  real ( kind = 8 ) uold
  real ( kind = 8 ) vold
  real ( kind = 8 ) wi
  real ( kind = 8 ) wj
!
  external s_eqi
!
  nu_inv = para(nparf+nparb+1)

  do i = 1,nrow
    do j = 1,neqn
      a(i,j) = 0.0D+00
    end do
  end do
!
!  Approximate the integral by summing over all elements.
!
  do ielem = 1,nelem
!
!  Evaluate the integrand at the quadrature points.
!
    do iquad = 1,nquad

      ar = area(iquad,ielem)
!
!  For the given quadrature point, evaluate P, U and V.
!
      call uvalq(dpdx,dpdy,dudx,dudy,dvdx,dvdy,g,ielem,indx, &
        iquad,nelem,neqn,node,np,nquad,pold,phi,uold,vold)
!
!  Consider each node in the element.
!
      do iq = 1,6

        ip = node(iq,ielem)

        wi = phi(iquad,iq,1,ielem)
        dwidx = phi(iquad,iq,2,ielem)
        dwidy = phi(iquad,iq,3,ielem)
        qi = phi(iquad,iq,4,ielem)

        ihor = indx(1,ip)
        iver = indx(2,ip)
        iprs = indx(3,ip)
!
!  Now compute the derivatives of the functions associated
!  with U, V and P, with respect to the coefficients associated
!  with basis vectors at each node of the element.
!
        do jq = 1,6

          jp = node(jq,ielem)

          wj = phi(iquad,jq,1,ielem)
          dwjdx = phi(iquad,jq,2,ielem)
          dwjdy = phi(iquad,jq,3,ielem)

          dqjdx = phi(iquad,jq,5,ielem)
          dqjdy = phi(iquad,jq,6,ielem)

          jhor = indx(1,jp)
          jver = indx(2,jp)
          jprs = indx(3,jp)
!
!  Contributions of the JHOR horizontal velocity to the U, V, and
!  P equations.
!
          iuse = ihor-jhor+2*nlband+1

          if ( eqn(ihor) == 'U' ) then

            if ( s_eqi(syseqn,'NavierStokes') ) then
              term = ar * ( dwjdx * dwidx + dwjdy * dwidy + &
                nu_inv * ( wj * dudx + uold * dwjdx + vold * dwjdy ) * wi )
            else if (s_eqi(syseqn,'Stokes') ) then
              term = ar * ( dwjdx * dwidx + dwjdy * dwidy )
            end if

            a(iuse,jhor) = a(iuse,jhor) + term

          end if

          if ( eqn(iver) == 'V' ) then
            if ( s_eqi(syseqn,'NavierStokes') ) then
              iuse = iver - jhor+2*nlband+1
              term = ar * nu_inv * wj * dvdx * wi
              a(iuse,jhor) = a(iuse,jhor)+term
            end if
          end if

          if ( 0 < iprs ) then
            if ( eqn(iprs) == 'P' ) then
              iuse = iprs-jhor+2*nlband+1
              term = ar * dwjdx * qi
              a(iuse,jhor) = a(iuse,jhor)+term
            end if
          end if
!
!  Contributions of the JVER vertical velocity variable to the
!  U, V and P equations.
!
          if ( eqn(ihor) == 'U' ) then
            if ( s_eqi(syseqn,'NavierStokes') ) then
              iuse = ihor-jver+2*nlband+1
              term = ar * nu_inv * wj * dudy * wi
              a(iuse,jver) = a(iuse,jver)+term
            end if
          end if

          iuse = iver-jver+2*nlband+1
          if ( eqn(iver) == 'V' ) then
            if ( s_eqi(syseqn,'NavierStokes') ) then
              term = ar * ( dwjdx * dwidx + dwjdy * dwidy &
                +nu_inv * ( uold * dwjdx + wj * dvdy + vold * dwjdy ) * wi )
            else if (s_eqi(syseqn,'Stokes') ) then
              term = ar * ( dwjdx * dwidx + dwjdy * dwidy )
            end if
            a(iuse,jver) = a(iuse,jver) + term
          end if

          if ( iprs>0 ) then
            if ( eqn(iprs) == 'P' ) then
              iuse = iprs-jver+2*nlband+1
              term = ar * dwjdy * qi
              a(iuse,jver) = a(iuse,jver)+term
            end if
          end if
!
!  Contributions of the JPRS pressure to the U and V equations.
!
          if ( jprs>0 ) then

            if ( eqn(ihor) =='U' ) then
              iuse = ihor-jprs+2*nlband+1
              term = ar * nu_inv * dqjdx * wi
              a(iuse,jprs) = a(iuse,jprs)+term
            end if

            if ( eqn(iver) =='V' ) then
              iuse = iver-jprs+2*nlband+1
              term = ar * nu_inv * dqjdy * wi
              a(iuse,jprs) = a(iuse,jprs)+term
            end if

          end if

        end do
      end do
    end do
  end do
!
!  Set up the equations that enforce boundary conditions.
!
  do ip = 1,np

    ihor = indx(1,ip)
    iver = indx(2,ip)
    iprs = indx(3,ip)

    if ( eqn(ihor) =='UB'.or.eqn(ihor)=='UI'.or.eqn(ihor)=='UW' ) then
      a(2*nlband+1,ihor) = 1.0D+00
    end if

    if ( eqn(iver) =='VB'.or.eqn(iver)=='VI'.or.eqn(iver)=='VW' ) then
      a(2*nlband+1,iver) = 1.0D+00
    end if

    if ( iprs>0 ) then
      if ( eqn(iprs) =='PB' ) then
        a(2*nlband+1,iprs) = 1.0D+00
      end if
    end if

  end do

  return
end
subroutine fx ( area,eqn,g,ifs,indx,nelem,neqn,node,np,npar,nparb,nparf, &
  nquad,para,phi,res,splflo,syseqn,tauflo,yc)
!
!*******************************************************************************
!
!! FX computes the residual of the Navier Stokes or Stokes equations.
!
!  Discussion:
!
!    The Navier Stokes equations have the form:
!
!      Integral
!
!        dU/dx * dW/dx + dU/dy * dW/dy
!      + nu_inv * (U*dU/dx + V*dU/dy + dP/dx) * W dx dy = 0
!
!      Integral
!
!        dV/dx * dW/dx + dV/dy * dW/dy
!      + nu_inv * (U*dV/dx + V*dV/dy + dP/dy) * W dx dy = 0
!
!      Integral
!
!        (dU/dx + dV/dy) * Q dx dy = 0
!
!    The Stokes equations have the form:
!
!      Integral
!
!        dU/dx * dW/dx + dU/dy * dW/dy + nu_inv * dP/dx * W dx dy = 0
!
!      Integral
!
!        dV/dx * dW/dx + dV/dy * dW/dy + nu_inv * dP/dy * W dx dy = 0
!
!      Integral
!
!        (dU/dx + dV/dy) * Q dx dy = 0
!
!    Here W is a basis function for U and V, and Q is a basis
!    function for P.
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
!    Input, real AREA(3,NELEM), contains a common factor multiplying the
!    term associated with a quadrature point in a given element, namely,
!      AREA(IQUAD,IELEM) = Ar(IELEM) * WQUAD(IQUAD)
!    or, if the element is isoperimetric,
!      AREA(IQUAD,IELEM) = DET * Ar(IELEM) * WQUAD(IQUAD)
!    Here Ar(IELEM) represents the area of element IELEM.
!
!  EQN    Input, character ( len = 2 ) EQN(NEQN).
!         EQN records the "type" of each equation that will be generated, and
!         which is associated with an unknown.  Note that most boundary
!         conditions do not result in an equation.  The current values are:
!
!         'U'  The horizontal momentum equation.
!         'UB' The condition U = 0 applied at a node on the bump.
!         'UI' The condition U = UInflow(Y,Lambda) at the inflow.
!         'UW' The condition U = 0 applied at a node on a fixed wall.
!
!         'V'  The vertical momentum equation.
!         'VB' The condition V = 0 applied at a node on the bump.
!         'VI' The condition V = VInflow(Y,Lambda) at the inflow.
!         'VW' The condition V = 0 applied at a node on a fixed wall.
!
!         'P'  The continuity equation.
!         'PB' The condition P = 0 applied at (XMAX,YMAX).
!
!  G      Input, real G(NEQN).
!
!         G is the current solution vector, in which are stored
!         the finite element coefficients that define the velocity
!         and pressure functions, U, V and P.
!
!  IFS    Input, integer IFS.
!         1, the inflow is modeled by C0 linear splines.
!         2, the inflow is modeled by C0 quadratic splines.
!         3, the inflow is modeled by C1 cubic splines.
!
!  INDX   Input, integer INDX(3,NP).
!
!         INDX(I,J) contains, for each node J, the index of U, V and P at
!         that node, or 0 or a negative value.
!
!         If K = INDX(I,J) is positive, then the value of the degree
!         of freedom is stored in the solution vector entry G(K),
!         and an equation will be generated to determine its value.
!
!         If INDX(I,J) is not positive, then no equation is
!         generated to determine for variable I at node J, either because
!         the variable is specified in some other way, or because
!         (in the case of pressure), there is no coefficient associated
!         with that node.
!
!  NELEM  Input, integer NELEM, the number of elements.
!
!  NEQN   Input, integer NEQN, the number of finite element equations used
!         to define the horizontal and vertical velocities and the
!         pressure.
!
!  NODE   Input, integer NODE(6,NELEM).
!
!         NODE(I,J) contains, for an element J, the global node index of
!         the element node whose local number is I.
!
!         The local ordering of the nodes is suggested by this diagram:
!
!               2
!              /|
!             4 5
!            /  |
!           1-6-3
!
!  NP     Input, integer NP, the number of nodes used to define the finite
!         element mesh.  NP = (2*NX-1)*(2*NY-1).
!
!  NPAR   Input, integer NPAR.
!
!         The number of parameters.  NPAR = NPARF + NPARB + 1.
!
!         The parameters control the shape of the inflow,
!         the shape of the bump obstacle, and the strength of the
!         flow.
!
!  NPARB  Input, integer NPARB.
!
!         The number of parameters associated with the position and
!         shape of the bump.
!
!         Note that if NPARB = 0, the bump is replaced by a flat wall.
!
!  NPARF  Input, integer NPARF.
!
!         NPARF is the number of parameters associated with the
!         inflow.  NPARF must be at least 1.
!
!  NQUAD  Input, integer NQUAD, the number of quadrature points.
!
!  PARA   Input, real PARA(NPAR).
!
!         PARA is the current set of parameter values, including the
!         inverse viscosity, the flow parameters, and the bump parameters.
!
!  PHI    Input, real PHI(NQUAD,6,10,NELEM).
!
!         PHI contains the value of a basis function, its derivative,
!         or other information, evaluated at a quadrature point.
!
!         For a particular element I, quadrature point J, and basis
!         function K, we use the following shorthand for the 10
!         entries of PHI:
!
!           W, dWdX, dWdY
!           Q, dQdX, dQdY
!           dXsidX, dXsidY, dEtadX, dEtadY
!
!         W is the quadratic basis function associated with velocity,
!         Q the linear basis function associated with pressure,
!         Xsi and Eta the reference coordinates for the point.
!
!         In particular, PHI(J,K,1,I) is the value of the quadratic
!         basis function associated with local node K in element I,
!         evaluated at quadrature point J.
!
!         Note that PHI(J,K,4,I) = PHI(J,K,5,I)=PHI(J,K,6,I)=0 for
!         K = 4, 5, or 6, since there are only three linear basis
!         functions.
!
!  RES    Output, real RES(NEQN), contains the value
!         of the residual.
!
!  SPLFLO Input, real SPLFLO(4,NPARF+2,0:NPARF).
!
!         SPLFLO contains the spline coefficients for the inflow
!         in SPLFLO(*,*,0).
!
!  SYSEQN Input, character ( len = 20 ) SYSEQN.
!
!         Input to the FX and FP routines, SYSEQN is either 'NavierStokes'
!         or 'Stokes', and specifies which state equation is to be set up.
!
!  TAUFLO Input, real TAUFLO(NPARF+2).
!
!         TAUFLO contains the location of the spline abscissas for
!         the inflow.  There are NPARF+2 of them, because the end
!         values of the spline are constrained to have particular
!         values.
!
!  YC     Input, real YC(NP).
!
!         The Y coordinates of the nodes.
!
!
  integer nelem
  integer neqn
  integer np
  integer npar
  integer nparf
  integer nquad
!
  real ( kind = 8 ) ar
  real ( kind = 8 ) area(nquad,nelem)
  real ( kind = 8 ) dpdx
  real ( kind = 8 ) dpdy
  real ( kind = 8 ) dudx
  real ( kind = 8 ) dudy
  real ( kind = 8 ) dvdx
  real ( kind = 8 ) dvdy
  real ( kind = 8 ) dwidx
  real ( kind = 8 ) dwidy
  character ( len = 2 ) eqn(neqn)
  real ( kind = 8 ) g(neqn)
  integer i
  integer ifs
  integer ielem
  integer ihor
  integer indx(3,np)
  integer ip
  integer iprs
  integer iq
  integer iquad
  integer iver
  logical s_eqi
  integer node(6,nelem)
  integer nparb
  real ( kind = 8 ) p
  real ( kind = 8 ) para(npar)
  real ( kind = 8 ) phi(nquad,6,10,nelem)
  real ( kind = 8 ) qi
  real ( kind = 8 ) res(neqn)
  real ( kind = 8 ) nu_inv
  real ( kind = 8 ) splflo(4,nparf+2,0:nparf)
  character ( len = 20 ) syseqn
  real ( kind = 8 ) tauflo(nparf+2)
  real ( kind = 8 ) u
  real ( kind = 8 ) ubc
  real ( kind = 8 ) v
  real ( kind = 8 ) vbc
  real ( kind = 8 ) wi
  real ( kind = 8 ) yc(np)
!
  external s_eqi
!
  nu_inv = para(nparf+nparb+1)

  res(1:neqn) = 0.0D+00
!
!  Consider an element.
!
  do ielem = 1,nelem
!
!  Evaluate the integrand at the quadrature points.
!
    do iquad = 1,nquad

      ar = area(iquad,ielem)
!
!  Evaluate P, U and V.
!
      call uvalq(dpdx,dpdy,dudx,dudy,dvdx,dvdy,g,ielem,indx, &
        iquad,nelem,neqn,node,np,nquad,p,phi,u,v)
!
!  Look at nearby basis functions.
!
      do iq = 1,6

        ip = node(iq,ielem)

        wi = phi(iquad,iq,1,ielem)
        dwidx = phi(iquad,iq,2,ielem)
        dwidy = phi(iquad,iq,3,ielem)
        qi = phi(iquad,iq,4,ielem)
!
!  The horizontal velocity equations.
!
        ihor = indx(1,ip)

        if ( eqn(ihor) == 'U' ) then

          if ( s_eqi ( syseqn, 'NavierStokes') ) then
            res(ihor) = res(ihor) + ar * ( dudx * dwidx + dudy * dwidy &
              + nu_inv * ( u * dudx + v * dudy + dpdx ) * wi )
          else if ( s_eqi ( syseqn, 'Stokes' ) ) then
            res(ihor) = res(ihor) + ar * ( dudx * dwidx + dudy * dwidy &
              + nu_inv * dpdx * wi )
          end if

        else if ( eqn(ihor) =='UB' ) then

          res(ihor) = g(ihor)

        else if ( eqn(ihor) =='UI' ) then

          call flouv ( ifs, nparf, splflo, tauflo, ubc, vbc, yc(ip) )
          res(ihor) = g(ihor) - ubc

        else if ( eqn(ihor) =='UW' ) then

          res(ihor) = g(ihor)

        end if
!
!  The vertical velocity equations.
!
        iver = indx(2,ip)

        if ( eqn(iver) == 'V' ) then

          if ( s_eqi ( syseqn, 'NavierStokes' ) ) then
            res(iver) = res(iver) + ar * ( dvdx * dwidx + dvdy * dwidy &
              + nu_inv * ( u * dvdx + v * dvdy + dpdy ) * wi )
          else if ( s_eqi ( syseqn, 'Stokes' ) ) then
            res(iver) = res(iver) + ar * ( dvdx * dwidx + dvdy * dwidy &
              + nu_inv * dpdy * wi )
          end if

        else if ( eqn(iver) =='VB' ) then

          res(iver) = g(iver)

        else if ( eqn(iver) =='VI' ) then

          call flouv ( ifs, nparf, splflo, tauflo, ubc, vbc, yc(ip) )
          res(iver) = g(iver) - vbc

        else if ( eqn(iver) =='VW' ) then

          res(iver) = g(iver)

        end if
!
!  The pressure equations.
!
        iprs = indx(3,ip)
        if ( 0 < iprs ) then
          if ( eqn(iprs) =='P' ) then
            res(iprs) = res(iprs) + ar * ( dudx + dvdy ) * qi
          else if ( eqn(iprs) =='PB' ) then
            res(iprs) = g(iprs)
          end if
        end if

      end do
    end do
  end do

  return
end
subroutine getcst(cost,costb,costp,costu,costv,g,gtar,ibs,indx,neqn,np, &
  nparb,nprof,ny,splbmp,taubmp,wateb,watep,wateu,watev,xbl,xbr,ybl,ybr,yc)
!
!*******************************************************************************
!
!! GETCST is given the value of the solution, G, the target
!  solution GTAR, and information about the shape of the bump,
!  and returns the value of the overall and individual cost
!  functions.
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
  integer neqn
  integer np
  integer nparb
  integer ny
!
  real ( kind = 8 ) cost
  real ( kind = 8 ) costb
  real ( kind = 8 ) costp
  real ( kind = 8 ) costu
  real ( kind = 8 ) costv
  real ( kind = 8 ) g(neqn)
  real ( kind = 8 ) gtar(neqn)
  integer ibs
  integer indx(3,np)
  integer nprof(2*ny-1)
  real ( kind = 8 ) splbmp(4,nparb+2,0:nparb)
  real ( kind = 8 ) taubmp(nparb+2)
  real ( kind = 8 ) wateb
  real ( kind = 8 ) watep
  real ( kind = 8 ) wateu
  real ( kind = 8 ) watev
  real ( kind = 8 ) xbl
  real ( kind = 8 ) xbr
  real ( kind = 8 ) ybl
  real ( kind = 8 ) ybr
  real ( kind = 8 ) yc(np)
!
  call bmpcst(costb,ibs,nparb,splbmp,taubmp,xbl,xbr,ybl,ybr)

  call discst(costp,costu,costv,g,gtar,indx,neqn,np,nprof,ny,yc)

  cost = wateb*costb+watep*costp+wateu*costu+watev*costv

  return
end
subroutine getder(dpara,g,gtar,ibs,indx,maxeqn,neqn,np,npar,nparb,nparf, &
  nprof,ny,sens,splbmp,taubmp,wateb,watep,wateu,watev,xbl,xbr,ybl,ybr,yc)
!
!*******************************************************************************
!
!! GETDER returns the derivatives of the cost with respect to the parameters.
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
!  DPARA  Output, real DPARA(NPAR), the partial
!         derivative of the cost with respect to each parameter,
!         computed using sensitivities or finite differences.
!
  integer maxeqn
  integer neqn
  integer np
  integer npar
  integer nparb
  integer nparf
  integer ny
!
  real ( kind = 8 ) dpara(npar)
  real ( kind = 8 ) g(neqn)
  real ( kind = 8 ) gtar(neqn)
  integer i
  integer ibs
  integer indx(3,np)
  integer nprof(2*ny-1)
  real ( kind = 8 ) sens(maxeqn,npar)
  real ( kind = 8 ) splbmp(4,nparb+2,0:nparb)
  real ( kind = 8 ) taubmp(nparb+2)
  real ( kind = 8 ) wateb
  real ( kind = 8 ) watep
  real ( kind = 8 ) wateu
  real ( kind = 8 ) watev
  real ( kind = 8 ) xbl
  real ( kind = 8 ) xbr
  real ( kind = 8 ) ybl
  real ( kind = 8 ) ybr
  real ( kind = 8 ) yc(np)

  return
end
subroutine getdu(dpdyn,dudyn,dvdyn,etan,g,indx,isotri,nelem,neqn,node,np, &
  numel,xc,xsin,yc)
!
!*******************************************************************************
!
!! GETDU uses the finite element method to estimate the value of
!  the spatial derivatives dPdY, dUdY and dVdY at every node.
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
  integer nelem
  integer neqn
  integer np
!
  real ( kind = 8 ) det
  real ( kind = 8 ) detadx
  real ( kind = 8 ) detady
  real ( kind = 8 ) dpdx
  real ( kind = 8 ) dpdy
  real ( kind = 8 ) dpdyn(np)
  real ( kind = 8 ) dudx
  real ( kind = 8 ) dudy
  real ( kind = 8 ) dudyn(np)
  real ( kind = 8 ) dvdx
  real ( kind = 8 ) dvdy
  real ( kind = 8 ) dvdyn(np)
  real ( kind = 8 ) dxsidx
  real ( kind = 8 ) dxsidy
  real ( kind = 8 ) eta
  real ( kind = 8 ) etan(6)
  real ( kind = 8 ) g(neqn)
  integer ielem
  integer indx(np,3)
  integer ip
  integer iq
  integer isotri(nelem)
  integer node(nelem,6)
  integer numel(np)
  real ( kind = 8 ) p
  real ( kind = 8 ) u
  real ( kind = 8 ) v
  real ( kind = 8 ) xc(np)
  real ( kind = 8 ) xq
  real ( kind = 8 ) xsi
  real ( kind = 8 ) xsin(6)
  real ( kind = 8 ) yc(np)
  real ( kind = 8 ) yq
!
!  Estimate dUdY, dVdY, dPdY at each node.
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

  do ip = 1,np
    numel(ip) = 0
    dpdyn(ip) = 0.0D+00
    dudyn(ip) = 0.0D+00
    dvdyn(ip) = 0.0D+00
  end do

  do ielem = 1,nelem

    do iq = 1,6

      ip = node(ielem,iq)
      numel(ip) = numel(ip)+1

      xq = xc(ip)
      yq = yc(ip)

      eta = etan(iq)
      xsi = xsin(iq)

      if ( isotri(ielem) == 2 ) then
        call trans(det,detadx,detady,dxsidx,dxsidy,eta,ielem,nelem,node,np, &
          xc,xsi,yc)
      end if

      call uval(detadx,detady,dpdx,dpdy,dudx,dudy,dvdx,dvdy, &
        dxsidx,dxsidy,eta,g,ielem,indx,isotri,nelem,neqn, &
        node,np,p,u,v,xc,xq,xsi,yc,yq)

      dpdyn(ip) = dpdyn(ip)+dpdy
      dudyn(ip) = dudyn(ip)+dudy
      dvdyn(ip) = dvdyn(ip)+dvdy
    end do

  end do
!
!  Take the average value of the quantities over all the
!  different elements along whose boundaries they are defined.
!
  do ip = 1,np
    dpdyn(ip) = dpdyn(ip)/real(numel(ip), kind = 8 )
    dudyn(ip) = dudyn(ip)/real(numel(ip), kind = 8 )
    dvdyn(ip) = dvdyn(ip)/real(numel(ip), kind = 8 )
  end do

  return
end
subroutine getdu2 ( dudyn, dudy2, np, nx, ny )
!
!*******************************************************************************
!
!! GETDU2 is damaged code.
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
  integer, parameter :: maxdat = 12
  integer, parameter :: ncoef = 100
!
  integer np
!
  real ( kind = 8 ) ac(maxdat,6)
  real ( kind = 8 ) ae(maxdat,6)
  real ( kind = 8 ) an(maxdat,6)
  real ( kind = 8 ) as(maxdat,6)
  real ( kind = 8 ) atac(ncoef,ncoef)
  real ( kind = 8 ) atae(ncoef,ncoef)
  real ( kind = 8 ) atan(ncoef,ncoef)
  real ( kind = 8 ) atas(ncoef,ncoef)
  real ( kind = 8 ) ataw(ncoef,ncoef)
  real ( kind = 8 ) aw(maxdat,6)
  real ( kind = 8 ) b(ncoef)
  real ( kind = 8 ) dat(maxdat)
  real ( kind = 8 ) dudyn(np)
  real ( kind = 8 ) dudy2(np)
  integer i
  integer ic
  integer icol
  integer ie
  integer iee
  integer in
  integer ine
  integer inee
  integer info
  integer inne
  integer inw
  integer ipivc(ncoef)
  integer ipive(ncoef)
  integer ipivn(ncoef)
  integer ipivs(ncoef)
  integer ipivw(ncoef)
  integer irow
  integer is
  integer ise
  integer iss
  integer issw
  integer isw
  integer isww
  integer iw
  integer iww
  integer j
  integer k
  integer ndat
  integer nodat(maxdat)
  integer nrhs
  integer nx
  integer ny
  real ( kind = 8 ) x
  real ( kind = 8 ) xdat(maxdat)
  real ( kind = 8 ) y
  real ( kind = 8 ) ydat(maxdat)
!
!  Copy the data
!
  do i = 1,np
    dudy2(i) = dudyn(i)
  end do
!
!  Here is a picture of the element patch around a node on the
!  northern boundary:
!
!
!    WW---W----C----E----EE
!    |        /|        /|
!    |       / |       / |
!    |      /  |      /  |
!    |     /   |     /   |
!    SWW  SW   S    SE   .
!    |   /     |   /     |
!    |  /      |  /      |
!    | /       | /       |
!    |/        |/        |
!    SSWW-SSW--SS---.----.
!
!  NORTH
!
  ndat = 7

  xdat(1) = 0.0D+00
  ydat(1) = 1.0D+00

  xdat(2) = 1.0D+00
  ydat(2) = 0.0D+00

  xdat(3) = 1.0D+00
  ydat(3) = 1.0D+00

  xdat(4) = 1.0D+00
  ydat(4) = 2.0D+00

  xdat(5) = 2.0D+00
  ydat(5) = 1.0D+00

  xdat(6) = 3.0D+00
  ydat(6) = 1.0D+00

  xdat(7) = 3.0D+00
  ydat(7) = 2.0D+00
!
!  Compute matrix of basis polynomials (1,x,y,x**2,xy,y**2) evaluated
!  at the data points.
!
  do i = 1,ndat
    an(i,1) = 1.0D+00
    an(i,2) = xdat(i)
    an(i,3) = ydat(i)
    an(i,4) = xdat(i)**2
    an(i,5) = xdat(i)*ydat(i)
    an(i,6) = ydat(i)**2
  end do
!
!  Compute the normal equations matrix.
!
  do i = 1,ncoef
    do j = 1,ncoef
      atan(i,j) = 0.0D+00
      do k = 1,ndat
        atan(i,j) = atan(i,j)+an(k,i)*an(k,j)
      end do
    end do
  end do
!
!  Factor the normal equations matrix.
!
  call sgetrf(ncoef,ncoef,atan,ncoef,ipivn,info)

  if ( info/= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'GetDU2 - Fatal error!'
    write ( *, * ) '  SGETRF returned INFO = ',info
    write ( *, * ) '  while factoring matrix ATAN.'
    stop
  end if
!
!  EAST
!
!  Here is a picture of the element patch around a node
!  on the eastern boundary:
!
!    .----.----NN
!    |        /|
!    |       / |
!    |      /  |
!    |     /   |
!    .    NW   N
!    |   /     |
!    |  /      |
!    | /       |
!    |/        |
!    WW---W----C
!    |        /|
!    |       / |
!    |      /  |
!    |     /   |
!    SWW  SW   S
!    |   /     |
!    |  /      |
!    | /       |
!    |/        |
!    SSWW-SSW--SS
!
  ndat = 7

  xdat(1) = 0.0D+00
  ydat(1) = 1.0D+00

  xdat(2) = 1.0D+00
  ydat(2) = 0.0D+00

  xdat(3) = 1.0D+00
  ydat(3) = 1.0D+00

  xdat(4) = 1.0D+00
  ydat(4) = 2.0D+00

  xdat(5) = 1.0D+00
  ydat(5) = 3.0D+00

  xdat(6) = 2.0D+00
  ydat(6) = 1.0D+00

  xdat(7) = 2.0D+00
  ydat(7) = 3.0D+00
!
!  Compute matrix of basis polynomials (1,x,y,x**2,xy,y**2) evaluated
!  at the data points.
!
  do i = 1,ndat
    ae(i,1) = 1.0D+00
    ae(i,2) = xdat(i)
    ae(i,3) = ydat(i)
    ae(i,4) = xdat(i)**2
    ae(i,5) = xdat(i)*ydat(i)
    ae(i,6) = ydat(i)**2
  end do
!
!  Compute the normal equations matrix.
!
  do i = 1,ncoef
    do j = 1,ncoef
      atae(i,j) = 0.0D+00
      do k = 1,ndat
        atae(i,j) = atae(i,j)+ae(k,i)*ae(k,j)
      end do
    end do
  end do
!
!  Factor the normal equations matrix.
!
  call sgetrf(ncoef,ncoef,atae,ncoef,ipive,info)

  if ( info/= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'GetDU2 - Fatal error!'
    write ( *, * ) '  SGETRF returned INFO = ',info
    write ( *, * ) '  while factoring matrix ATAE.'
    stop
  end if
!
!  WEST
!
!  Here is a picture of the element patch around a node on the
!  western boundary:
!
!    NN---NNE--NNEE
!    |        /|
!    |       / |
!    |      /  |
!    |     /   |
!    N    NE   NEE
!    |   /     |
!    |  /      |
!    | /       |
!    |/        |
!    C----E----EE
!    |        /|
!    |       / |
!    |      /  |
!    |     /   |
!    S    SE   .
!    |   /     |
!    |  /      |
!    | /       |
!    |/        |
!    SS---.----.
!
  ndat = 7

  xdat(1) = 2.0D+00
  ydat(1) = 1.0D+00

  xdat(2) = 2.0D+00
  ydat(2) = 3.0D+00

  xdat(3) = 3.0D+00
  ydat(3) = 1.0D+00

  xdat(4) = 3.0D+00
  ydat(4) = 2.0D+00

  xdat(5) = 3.0D+00
  ydat(5) = 3.0D+00

  xdat(6) = 3.0D+00
  ydat(6) = 4.0D+00

  xdat(7) = 4.0D+00
  ydat(7) = 3.0D+00
!
!  Compute matrix of basis polynomials (1,x,y,x**2,xy,y**2) evaluated
!  at the data points.
!
  do i = 1,ndat
    aw(i,1) = 1.0D+00
    aw(i,2) = xdat(i)
    aw(i,3) = ydat(i)
    aw(i,4) = xdat(i)**2
    aw(i,5) = xdat(i)*ydat(i)
    aw(i,6) = ydat(i)**2
  end do
!
!  Compute the normal equations matrix.
!
  do i = 1,ncoef
    do j = 1,ncoef
      ataw(i,j) = 0.0D+00
      do k = 1,ndat
        ataw(i,j) = ataw(i,j)+aw(k,i)*aw(k,j)
      end do
    end do
  end do
!
!  Factor the normal equations matrix.
!
  call sgetrf(ncoef,ncoef,ataw,ncoef,ipivw,info)

  if ( info/= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'GetDU2 - Fatal error!'
    write ( *, * ) '  SGETRF returned INFO = ',info
    write ( *, * ) '  while factoring matrix ATAW.'
    stop
  end if
!
!  SOUTH
!
!  Here is a picture of the element patch around a node on the
!  southern boundary:
!
!    .----.----NN---NNE--NNEE
!    |        /|        /|
!    |       / |       / |
!    |      /  |      /  |
!    |     /   |     /   |
!    .    NW   N    NE   NEE
!    |   /     |   /     |
!    |  /      |  /      |
!    | /       | /       |
!    |/        |/        |
!    WW---W----C----E----EE
!
  ndat = 7

  xdat(1) = 1.0D+00
  ydat(1) = 2.0D+00

  xdat(2) = 1.0D+00
  ydat(2) = 3.0D+00

  xdat(3) = 2.0D+00
  ydat(3) = 3.0D+00

  xdat(4) = 3.0D+00
  ydat(4) = 2.0D+00

  xdat(5) = 3.0D+00
  ydat(5) = 3.0D+00

  xdat(6) = 3.0D+00
  ydat(6) = 4.0D+00

  xdat(7) = 4.0D+00
  ydat(7) = 3.0D+00
!
!  Compute matrix of basis polynomials (1,x,y,x**2,xy,y**2) evaluated
!  at the data points.
!
  do i = 1,ndat
    as(i,1) = 1.0D+00
    as(i,2) = xdat(i)
    as(i,3) = ydat(i)
    as(i,4) = xdat(i)**2
    as(i,5) = xdat(i)*ydat(i)
    as(i,6) = ydat(i)**2
  end do
!
!  Compute the normal equations matrix.
!
  do i = 1,ncoef
    do j = 1,ncoef
      atas(i,j) = 0.0D+00
      do k = 1,ndat
        atas(i,j) = atas(i,j)+as(k,i)*as(k,j)
      end do
    end do
  end do
!
!  Factor the normal equations matrix.
!
  call sgetrf(ncoef,ncoef,atas,ncoef,ipivs,info)

  if ( info /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'GetDU2 - Fatal error!'
    write ( *, * ) '  SGETRF returned INFO = ',info
    write ( *, * ) '  while factoring matrix ATAS.'
    stop
  end if
!
!  CENTRAL.
!
  ndat = 12

  xdat(1) = 0.0D+00
  ydat(1) = 1.0D+00

  xdat(2) = 1.0D+00
  ydat(2) = 0.0D+00

  xdat(3) = 1.0D+00
  ydat(3) = 1.0D+00

  xdat(4) = 1.0D+00
  ydat(4) = 2.0D+00

  xdat(5) = 1.0D+00
  ydat(5) = 3.0D+00

  xdat(6) = 2.0D+00
  ydat(6) = 1.0D+00

  xdat(7) = 2.0D+00
  ydat(7) = 3.0D+00

  xdat(8) = 3.0D+00
  ydat(8) = 1.0D+00

  xdat(9) = 3.0D+00
  ydat(9) = 2.0D+00

  xdat(10) = 3.0D+00
  ydat(10) = 3.0D+00

  xdat(11) = 3.0D+00
  ydat(11) = 4.0D+00

  xdat(12) = 4.0D+00
  ydat(12) = 3.0D+00
!
!  Compute matrix of basis polynomials (1,x,y,x**2,xy,y**2) evaluated
!  at the data points.
!
  do i = 1,ndat
    ac(i,1) = 1.0D+00
    ac(i,2) = xdat(i)
    ac(i,3) = ydat(i)
    ac(i,4) = xdat(i)**2
    ac(i,5) = xdat(i)*ydat(i)
    ac(i,6) = ydat(i)**2
  end do
!
!  Compute the normal equations matrix.
!
  do i = 1,ncoef
    do j = 1,ncoef
      atac(i,j) = 0.0D+00
      do k = 1,ndat
        atac(i,j) = atac(i,j)+ac(k,i)*ac(k,j)
      end do
    end do
  end do
!
!  Factor the normal equations matrix.
!
  call sgetrf(ncoef,ncoef,atac,ncoef,ipivc,info)

  if ( info /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'GetDU2 - Fatal error!'
    write ( *, * ) '  SGETRF returned INFO = ',info
    write ( *, * ) '  while factoring matrix ATAC.'
    stop
  end if
!
!  Each computation has its "center" at a pressure node, so we
!  can organize our do loops by counting through each presure node.
!
  do icol = 1,nx
    do irow = 1,ny

      ic = 2*(icol-1)*(2*ny-1)+(2*irow-1)

      isww = ic-2*(2*ny-1)-1
      iww = isww+1
      issw = ic-(2*ny-1)-2
      isw = issw+1
      iw = isw+1
      inw = iw+1
      iss = ic-2
      is = iss+1
      in = ic+1
      ise = ic+(2*ny-1)-1
      ie = ise+1
      ine = ie+1
      inne = ine+1
      iee = ic+2*(2*ny-1)
      inee = iee+1
!
!  CASE: CORNER NODES, SKIP EM
!
      if ( icol == 1.and.irow== 1 ) then
      else if (icol == 1.and.irow==ny ) then
      else if (icol ==nx.and.irow== 1 ) then
      else if (icol ==nx.and.irow==ny ) then
!
!  CASE: THE NORTHERN BOUNDARY
!
      else if (irow ==ny ) then

        nodat(1) = isww
        nodat(2) = issw
        nodat(3) = isw
        nodat(4) = iw
        nodat(5) = is
        nodat(6) = ise
        nodat(7) = ie

        ndat = 7
!
!  Copy out the value of dUdY at the data points.
!
        do i = 1,ndat
          dat(i) = dudyn(nodat(i))
        end do
!
!  Compute right hand side of normal equations.
!
        do i = 1,ncoef
          b(i) = 0.0D+00
          do j = 1,ndat
            b(i) = b(i)+an(j,i)*dat(j)
          end do
        end do
!
!  Solve the normal equations.
!
        nrhs = 1
        call sgetrs('N',ncoef,nrhs,atan,ncoef,ipivn,b,ncoef,info)
!
!  Now evaluate the least squares polynomial at the nodes.
!
        if ( icol == 2 ) then
          x = 0.0D+00
          y = 2.0D+00
          call lspoly(b,ncoef,x,y,dudy2(iww))
        end if

        x = 2.0D+00
        y = 2.0D+00
        call lspoly(b,ncoef,x,y,dudy2(ic))

        if ( icol ==nx-1 ) then
          x = 4.0D+00
          y = 2.0D+00
          call lspoly(b,ncoef,x,y,dudy2(iee))
        end if
!
!  CASE: THE EASTERN BOUNDARY
!
      else if (icol ==nx ) then

        nodat(1) = isww
        nodat(2) = issw
        nodat(3) = isw
        nodat(4) = iw
        nodat(5) = inw
        nodat(6) = is
        nodat(7) = in

        ndat = 7
!
!  Copy out the value of dUdY at the data points.
!
        do i = 1,ndat
          dat(i) = dudyn(nodat(i))
        end do
!
!  Compute right hand side of normal equations.
!
        do i = 1,ncoef
          b(i) = 0.0D+00
          do j = 1,ndat
            b(i) = b(i)+ae(j,i)*dat(j)
          end do
        end do
!
!  Solve the normal equations.
!
        nrhs = 1
        call sgetrs('N',ncoef,nrhs,atae,ncoef,ipive,b,ncoef,info)
!
!  Now evaluate the least squares polynomial at the nodes.
!
        x = 2.0D+00
        y = 2.0D+00
        call lspoly(b,ncoef,x,y,dudy2(ic))
!
!  CASE: THE WESTERN BOUNDARY
!
      else if (icol == 1 ) then

        nodat(1) = is
        nodat(2) = in
        nodat(3) = ise
        nodat(4) = ie
        nodat(5) = ine
        nodat(6) = inne
        nodat(7) = inee

        ndat = 7
!
!  Copy out the value of dUdY at the data points.
!
        do i = 1,ndat
          dat(i) = dudyn(nodat(i))
        end do
!
!  Compute right hand side of normal equations.
!
        do i = 1,ncoef
          b(i) = 0.0D+00
          do j = 1,ndat
            b(i) = b(i)+aw(j,i)*dat(j)
          end do
        end do
!
!  Solve the normal equations.
!
        nrhs = 1
        call sgetrs('N',ncoef,nrhs,ataw,ncoef,ipivw,b,ncoef,info)
!
!  Now evaluate the least squares polynomial at the nodes.
!
        x = 2.0D+00
        y = 2.0D+00
        call lspoly(b,ncoef,x,y,dudy2(ic))
!
!  CASE: THE SOUTHERN BOUNDARY
!
      else if (irow == 1 ) then

        nodat(1) = iw
        nodat(2) = inw
        nodat(3) = in
        nodat(4) = ie
        nodat(5) = ine
        nodat(6) = inne
        nodat(7) = inee

        ndat = 7
!
!  Copy out the value of dUdY at the data points.
!
        do i = 1,ndat
          dat(i) = dudyn(nodat(i))
        end do
!
!  Compute right hand side of normal equations.
!
        do i = 1,ncoef
          b(i) = 0.0D+00
          do j = 1,ndat
            b(i) = b(i)+as(j,i)*dat(j)
          end do
        end do
!
!  Solve the normal equations.
!
        nrhs = 1
        call sgetrs('N',ncoef,nrhs,atas,ncoef,ipivs,b,ncoef,info)
!
!  Now evaluate the least squares polynomial at the nodes.
!
        if ( icol == 2 ) then
          x = 0.0D+00
          y = 2.0D+00
          call lspoly(b,ncoef,x,y,dudy2(iww))
        end if

        x = 2.0D+00
        y = 2.0D+00
        call lspoly(b,ncoef,x,y,dudy2(ic))

        if ( icol ==nx-1 ) then
          x = 4.0D+00
          y = 2.0D+00
          call lspoly(b,ncoef,x,y,dudy2(iee))
        end if
!
!  CASE: CENTRAL
!
      else

        nodat(1) = isww
        nodat(2) = issw
        nodat(3) = isw
        nodat(4) = iw
        nodat(5) = inw
        nodat(6) = is
        nodat(7) = in
        nodat(8) = ise
        nodat(9) = ie
        nodat(10) = ine
        nodat(11) = inne
        nodat(12) = inee

        ndat = 12
!
!  Copy out the value of dUdY at the data points.
!
        do i = 1,ndat
          dat(i) = dudyn(nodat(i))
        end do
!
!  Compute right hand side of normal equations.
!
        do i = 1,ncoef
          b(i) = 0.0D+00
          do j = 1,ndat
            b(i) = b(i)+ac(j,i)*dat(j)
          end do
        end do
!
!  Solve the normal equations.
!
        nrhs = 1
        call sgetrs('N',ncoef,nrhs,atac,ncoef,ipivc,b,ncoef,info)
!
!  Now evaluate the least squares polynomial at the nodes.
!
        x = 2.0D+00
        y = 2.0D+00
        call lspoly(b,ncoef,x,y,dudy2(ic))

      end if

    end do
  end do

  return
end
subroutine getdu3(dudyn,dudy3,np,nx,ny)
!
!*******************************************************************************
!
!! GETDU3 uses interpolating serendipity quadrilaterals in an attempt
!  to improve the accuracy of the computed values of dPdY, dUdY and
!  dVdY at every node.
!
!  Note a possible objection to this scheme: we are treating
!  dUdY as primary data, instead of U.  Perhaps, instead, we should
!  read in the U values, and then compute dUdY in the serendipity
!  element.
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
  integer np
!
  real ( kind = 8 ) dudyn(np)
  real ( kind = 8 ) dudy3(np)
  integer i
  integer ic
  integer icol
  integer ie
  integer in
  integer ine
  integer inw
  integer irow
  integer is
  integer ise
  integer isw
  integer iw
  integer nx
  integer ny
  character ( len = 2 ) type
  real ( kind = 8 ) ve
  real ( kind = 8 ) vn
  real ( kind = 8 ) vne
  real ( kind = 8 ) vnw
  real ( kind = 8 ) vs
  real ( kind = 8 ) vse
  real ( kind = 8 ) vsw
  real ( kind = 8 ) vw
!
  do i = 1,np
    dudy3(i) = dudyn(i)
  end do
!
  do icol = 1,nx
    do irow = 1,ny

      ic = 2*(icol-1)*(2*ny-1)+(2*irow-1)

      isw = ic-(2*ny-1)-1
      iw = isw+1
      inw = iw+1
      is = ic-1
      in = ic+1
      ise = ic+(2*ny-1)-1
      ie = ise+1
      ine = ie+1

      if ( 1<= ie.and.ie<=np ) then
        ve = dudyn(ie)
      else
        ve = 0.0D+00
      end if

      if ( 1<= in.and.in<=np ) then
        vn = dudyn(in)
      else
        vn = 0.0D+00
      end if

      if ( 1<= ine.and.ine<=np ) then
        vne = dudyn(ine)
      else
        vne = 0.0D+00
      end if

      if ( 1<= inw.and.inw<=np ) then
        vnw = dudyn(inw)
      else
        vnw = 0.0D+00
      end if

      if ( 1<= is.and.is<=np ) then
        vs = dudyn(is)
      else
        vs = 0.0D+00
      end if

      if ( 1<= ise.and.ise<=np ) then
        vse = dudyn(ise)
      else
        vse = 0.0D+00
      end if

      if ( 1<= isw.and.isw<=np ) then
        vsw = dudyn(isw)
      else
        vsw = 0.0D+00
      end if

      if ( 1<= iw.and.iw<=np ) then
        vw = dudyn(iw)
      else
        vw = 0.0D+00
      end if
!
!  CASE: CORNER NODES
!
      if ( icol == 1.and.irow== 1 ) then
        type = 'SW'
      else if (icol == 1.and.irow==ny ) then
        type = 'NW'
      else if (icol ==nx.and.irow== 1 ) then
        type = 'SE'
      else if (icol ==nx.and.irow==ny ) then
        type = 'NE'
      else if (irow ==ny ) then
        type = 'N'
      else if (icol ==nx ) then
        type = 'E'
      else if (icol == 1 ) then
        type = 'W'
      else if (irow == 1 ) then
        type = 'S'
      else
        type = 'C'
      end if

      call serene(type,ve,vn,vne,vnw,vs,vse,vsw,vw,dudy3(ic))

    end do
  end do

  return
end
subroutine getdu4(dudyn,dudy4,np,nx,ny)
!
!*******************************************************************************
!
!! GETDU4 uses the Zienkiewicz-Zhou technique to attempt to improve the
!  accuracy of the computed values of dPdY, dUdY and dVdY at corner
!  nodes.
!
!  It differs from GETDU2 in that it also modifies the values of
!  dUdY at the midside nodes.
!
!  Here is a picture of the element patch around an interior node "C":
!
!    .----.----NN---NNE--NNEE
!    |        /|        /|
!    |       / |       / |
!    |      /  |      /  |
!    |     /   |     /   |
!    .    NW   N    NE   NEE
!    |   /     |   /     |
!    |  /      |  /      |
!    | /       | /       |
!    |/        |/        |
!    WW---W----C----E----EE
!    |        /|        /|
!    |       / |       / |
!    |      /  |      /  |
!    |     /   |     /   |
!    SWW  SW   S    SE   .
!    |   /     |   /     |
!    |  /      |  /      |
!    | /       | /       |
!    |/        |/        |
!    SSWW-SSW--SS---.----.
!
!  The midside nodes SWW, SSW, SW, W, NW, S, N, SE, E, NE, NNE, NEE
!  are all quadrature points, at which a higher rater of convergence
!  is expected in the solution.  The nodes labeled SSWW, WW, SS, C,
!  NN, EE and NNEE are corner nodes, whose values of dUdY we wish
!  to improve.  On this step, we will try to compute improved values
!  of the node "C", and the side nodes.  The step will be repeated
!  so that every pressure node gets to be the "C" node of a patch
!  (except for corner nodes).  Nodes labeled "." are not included in
!  the patch, and are shown only for completeness.
!
!  The value of the quantity dUdY at each of the midside nodes is
!  taken as data, to be fitted by a least squares polynomial based
!  on the six basis functions (1, x, y, x*x, x*y, y*y).  For the
!  case where the node C is in the interior of the region, this
!  makes 12 equations in 6 unknowns.
!
!  The above diagram is assigned "logical" coordinates that
!  range from 0 <=  x,y <= 4, with SWW having coordinates (0,1)
!  and C having coordinates (2,2).  (These are really a sort of
!  XSI, ETA coordinate system, rather than a "physical" X, Y
!  system.)
!
!  Using standard discrete least squares methods, we write our
!  original set of equations as
!
!    AC x = b
!
!  where x is the set of 6 unknown polynomial coefficients,
!  row i of matrix AC is the value of (1, x, y, x*x, x*y, y*y)
!  at the i-th of our 12 quadrature points.
!
!  To get a solvable system, we multiply by the transpose of AC,
!  getting the square system known as the normal equations:
!
!    ACT AC x = ACT b
!
!  The matrix ACT AC only has to be set up and factored once, and
!  we can use the factors over and over for all the patches
!  associated with interior nodes.  (If we really like this method,
!  we should stop using the normal equations and use instead
!  a QR routine like the LINPACK SQRDC/SQRSL).
!
!  Once the coefficients of the polynomial are found, the least
!  squares polynomial may be evaluated at any point in the
!  "patch" of six elements.  In this routine, we choose to
!  evaluate the polynomial only at the point C, and use this
!  value in place of the value computed by the finite element
!  method.  The values at the midside nodes are unchanged.
!
!  Special treatment must be made for nodes which lie on the
!  north, east, west or south sides, or in the corners.
!
!  In the case of side nodes, we simply adapt the process to
!  account for 7 data values, using the six basis functions.
!
!  In the case of a corner node, we simply apply the relevant
!  value computed for the patch along the north or south boundary
!  nearest to the node.
!
!  In particular, separate matrices AN, AE, AW and AS must be set
!  up to represent the least squares linear systems that occur for
!  nodes which lie along the north, east, west or south boundaries.
!
!
!  Since the midside nodes can occur in more than one element,
!  we ADD the improved estimate to a running total, and average
!  later.
!
!  Note: this routine explicitly assumes that the nodes are ordered
!  from top to bottom, then from left to right, as in this example:
!
!    5 10 15
!    4  9 14
!    3  8 13
!    2  7 12
!    1  6 11
!
!  It also assumes that EVERY node has an associated value of DUDYN,
!  and that the pressure nodes, at which we will smooth the value of
!  DUDYN, are exactly those nodes in an odd row and odd column.
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
!  DUDYN  Input/output, REAL DUDYN(NP).
!
!         On input, DUDYN contains the original value of DUDY at
!         every node.
!
!         On output, DUDYN contains the smoothed values of dUdY.
!
!  NP     Input, INTEGER NP, the number of nodes.
!
!  NX     Input, INTEGER NX, the number of columns of pressure nodes
!         in the X direction.  The actual number of columns of nodes
!         is 2*NX-1.
!
!  NY     Input, INTEGER NY, the number of rows of pressure nodes
!         in the Y direction.  The actual number of rows of nodes
!         is 2*NY-1.
!
  integer, parameter :: ncoef = 6
  integer, parameter :: npmax = 7889
!
  integer np
!
  real ( kind = 8 ) ac(12,ncoef)
  real ( kind = 8 ) ae(7,ncoef)
  real ( kind = 8 ) an(7,ncoef)
  real ( kind = 8 ) as(7,ncoef)
  real ( kind = 8 ) aw(7,ncoef)
  real ( kind = 8 ) atac(ncoef,ncoef)
  real ( kind = 8 ) atae(ncoef,ncoef)
  real ( kind = 8 ) atan(ncoef,ncoef)
  real ( kind = 8 ) atas(ncoef,ncoef)
  real ( kind = 8 ) ataw(ncoef,ncoef)
  real ( kind = 8 ) b(ncoef)
  real ( kind = 8 ) dat(12)
  real ( kind = 8 ) dudyn(np)
  real ( kind = 8 ) dudy4(np)
  integer i
  integer ic
  integer icol
  integer ie
  integer iee
  integer in
  integer ine
  integer inee
  integer info
  integer inn
  integer inne
  integer inw
  integer ipivc(ncoef)
  integer ipive(ncoef)
  integer ipivn(ncoef)
  integer ipivs(ncoef)
  integer ipivw(ncoef)
  integer irow
  integer is
  integer ise
  integer iss
  integer issw
  integer isw
  integer isww
  integer iw
  integer iww
  integer j
  integer k
  integer ndat
  integer nodat(12)
  integer nrhs
  integer nrep(npmax)
  integer nx
  integer ny
  real ( kind = 8 ) temp
  real ( kind = 8 ) x
  real ( kind = 8 ) xdatc(12)
  real ( kind = 8 ) xdatn(7)
  real ( kind = 8 ) xdate(7)
  real ( kind = 8 ) xdatw(7)
  real ( kind = 8 ) xdats(7)
  real ( kind = 8 ) y
  real ( kind = 8 ) ydatc(12)
  real ( kind = 8 ) ydatn(7)
  real ( kind = 8 ) ydate(7)
  real ( kind = 8 ) ydatw(7)
  real ( kind = 8 ) ydats(7)
!
  nrep(1:np) = 0
  dudy4(1:np) = 0.0D+00

  if ( npmax < np ) then
    write ( *, * ) ' '
    write ( *, * ) 'GetDU4 - Fatal error!'
    write ( *, * ) '  NP must be less than NPMAX, but'
    write ( *, * ) '  NP = ',np
    write ( *, * ) '  NPMAX = ',npmax
    write ( *, * ) '  Change NP or NPMAX and try again!'
    stop
  end if
!
!  Here is a picture of the element patch around a node on the
!  northern boundary:
!
!
!    WW---W----C----E----EE
!    |        /|        /|
!    |       / |       / |
!    |      /  |      /  |
!    |     /   |     /   |
!    SWW  SW   S    SE   .
!    |   /     |   /     |
!    |  /      |  /      |
!    | /       | /       |
!    |/        |/        |
!    SSWW-SSW--SS---.----.
!
!  NORTH
!
  ndat = 7

  xdatn(1) = 0.0D+00
  ydatn(1) = 1.0D+00

  xdatn(2) = 1.0D+00
  ydatn(2) = 0.0D+00

  xdatn(3) = 1.0D+00
  ydatn(3) = 1.0D+00

  xdatn(4) = 1.0D+00
  ydatn(4) = 2.0D+00

  xdatn(5) = 2.0D+00
  ydatn(5) = 1.0D+00

  xdatn(6) = 3.0D+00
  ydatn(6) = 1.0D+00

  xdatn(7) = 3.0D+00
  ydatn(7) = 2.0D+00
!
!  Compute matrix of basis polynomials (1,x,y,x**2,xy,y**2) evaluated
!  at the data points.
!
  do i = 1,ndat
    an(i,1) = 1.0D+00
    an(i,2) = xdatn(i)
    an(i,3) = ydatn(i)
    an(i,4) = xdatn(i)**2
    an(i,5) = xdatn(i)*ydatn(i)
    an(i,6) = ydatn(i)**2
  end do
!
!  Compute the normal equations matrix.
!
  do i = 1,ncoef
    do j = 1,ncoef
      atan(i,j) = 0.0D+00
      do k = 1,ndat
        atan(i,j) = atan(i,j)+an(k,i)*an(k,j)
      end do
    end do
  end do
!
!  Factor the normal equations matrix.
!
  call sgetrf(ncoef,ncoef,atan,ncoef,ipivn,info)

  if ( info/= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'GetDU4 - Fatal error!'
    write ( *, * ) '  SGETRF returned INFO = ',info
    write ( *, * ) '  while factoring matrix ATAN.'
    stop
  end if
!
!  EAST
!
!  Here is a picture of the element patch around a node
!  on the eastern boundary:
!
!    .----.----NN
!    |        /|
!    |       / |
!    |      /  |
!    |     /   |
!    .    NW   N
!    |   /     |
!    |  /      |
!    | /       |
!    |/        |
!    WW---W----C
!    |        /|
!    |       / |
!    |      /  |
!    |     /   |
!    SWW  SW   S
!    |   /     |
!    |  /      |
!    | /       |
!    |/        |
!    SSWW-SSW--SS
!
  ndat = 7

  xdate(1) = 0.0D+00
  ydate(1) = 1.0D+00

  xdate(2) = 1.0D+00
  ydate(2) = 0.0D+00

  xdate(3) = 1.0D+00
  ydate(3) = 1.0D+00

  xdate(4) = 1.0D+00
  ydate(4) = 2.0D+00

  xdate(5) = 1.0D+00
  ydate(5) = 3.0D+00

  xdate(6) = 2.0D+00
  ydate(6) = 1.0D+00

  xdate(7) = 2.0D+00
  ydate(7) = 3.0D+00
!
!  Compute matrix of basis polynomials (1,x,y,x**2,xy,y**2) evaluated
!  at the data points.
!
  do i = 1,ndat
    ae(i,1) = 1.0D+00
    ae(i,2) = xdate(i)
    ae(i,3) = ydate(i)
    ae(i,4) = xdate(i)**2
    ae(i,5) = xdate(i)*ydate(i)
    ae(i,6) = ydate(i)**2
  end do
!
!  Compute the normal equations matrix.
!
  do i = 1,ncoef
    do j = 1,ncoef
      atae(i,j) = 0.0D+00
      do k = 1,ndat
        atae(i,j) = atae(i,j)+ae(k,i)*ae(k,j)
      end do
    end do
  end do
!
!  Factor the normal equations matrix.
!
  call sgetrf(ncoef,ncoef,atae,ncoef,ipive,info)

  if ( info/= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'GetDU4 - Fatal error!'
    write ( *, * ) '  SGETRF returned INFO = ',info
    write ( *, * ) '  while factoring matrix ATAE.'
    stop
  end if
!
!  WEST
!
!  Here is a picture of the element patch around a node on the
!  western boundary:
!
!    NN---NNE--NNEE
!    |        /|
!    |       / |
!    |      /  |
!    |     /   |
!    N    NE   NEE
!    |   /     |
!    |  /      |
!    | /       |
!    |/        |
!    C----E----EE
!    |        /|
!    |       / |
!    |      /  |
!    |     /   |
!    S    SE   .
!    |   /     |
!    |  /      |
!    | /       |
!    |/        |
!    SS---.----.
!
  ndat = 7

  xdatw(1) = 2.0D+00
  ydatw(1) = 1.0D+00

  xdatw(2) = 2.0D+00
  ydatw(2) = 3.0D+00

  xdatw(3) = 3.0D+00
  ydatw(3) = 1.0D+00

  xdatw(4) = 3.0D+00
  ydatw(4) = 2.0D+00

  xdatw(5) = 3.0D+00
  ydatw(5) = 3.0D+00

  xdatw(6) = 3.0D+00
  ydatw(6) = 4.0D+00

  xdatw(7) = 4.0D+00
  ydatw(7) = 3.0D+00
!
!  Compute matrix of basis polynomials (1,x,y,x**2,xy,y**2) evaluated
!  at the data points.
!
  do i = 1,ndat
    aw(i,1) = 1.0D+00
    aw(i,2) = xdatw(i)
    aw(i,3) = ydatw(i)
    aw(i,4) = xdatw(i)**2
    aw(i,5) = xdatw(i)*ydatw(i)
    aw(i,6) = ydatw(i)**2
  end do
!
!  Compute the normal equations matrix.
!
  do i = 1,ncoef
    do j = 1,ncoef
      ataw(i,j) = 0.0D+00
      do k = 1,ndat
        ataw(i,j) = ataw(i,j)+aw(k,i)*aw(k,j)
      end do
    end do
  end do
!
!  Factor the normal equations matrix.
!
  call sgetrf(ncoef,ncoef,ataw,ncoef,ipivw,info)

  if ( info/= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'GetDU4 - Fatal error!'
    write ( *, * ) '  SGETRF returned INFO = ',info
    write ( *, * ) '  while factoring matrix ATAW.'
    stop
  end if
!
!  SOUTH
!
!  Here is a picture of the element patch around a node on the
!  southern boundary:
!
!    .----.----NN---NNE--NNEE
!    |        /|        /|
!    |       / |       / |
!    |      /  |      /  |
!    |     /   |     /   |
!    .    NW   N    NE   NEE
!    |   /     |   /     |
!    |  /      |  /      |
!    | /       | /       |
!    |/        |/        |
!    WW---W----C----E----EE
!
  ndat = 7

  xdats(1) = 1.0D+00
  ydats(1) = 2.0D+00

  xdats(2) = 1.0D+00
  ydats(2) = 3.0D+00

  xdats(3) = 2.0D+00
  ydats(3) = 3.0D+00

  xdats(4) = 3.0D+00
  ydats(4) = 2.0D+00

  xdats(5) = 3.0D+00
  ydats(5) = 3.0D+00

  xdats(6) = 3.0D+00
  ydats(6) = 4.0D+00

  xdats(7) = 4.0D+00
  ydats(7) = 3.0D+00
!
!  Compute matrix of basis polynomials (1,x,y,x**2,xy,y**2) evaluated
!  at the data points.
!
  do i = 1,ndat
    as(i,1) = 1.0D+00
    as(i,2) = xdats(i)
    as(i,3) = ydats(i)
    as(i,4) = xdats(i)**2
    as(i,5) = xdats(i)*ydats(i)
    as(i,6) = ydats(i)**2
  end do
!
!  Compute the normal equations matrix.
!
  do i = 1,ncoef
    do j = 1,ncoef
      atas(i,j) = 0.0D+00
      do k = 1,ndat
        atas(i,j) = atas(i,j)+as(k,i)*as(k,j)
      end do
    end do
  end do
!
!  Factor the normal equations matrix.
!
  call sgetrf(ncoef,ncoef,atas,ncoef,ipivs,info)

  if ( info/= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'GetDU4 - Fatal error!'
    write ( *, * ) '  SGETRF returned INFO = ',info
    write ( *, * ) '  while factoring matrix ATAS.'
    stop
  end if
!
!  CENTRAL.
!
  ndat = 12

  xdatc(1) = 0.0D+00
  ydatc(1) = 1.0D+00

  xdatc(2) = 1.0D+00
  ydatc(2) = 0.0D+00

  xdatc(3) = 1.0D+00
  ydatc(3) = 1.0D+00

  xdatc(4) = 1.0D+00
  ydatc(4) = 2.0D+00

  xdatc(5) = 1.0D+00
  ydatc(5) = 3.0D+00

  xdatc(6) = 2.0D+00
  ydatc(6) = 1.0D+00

  xdatc(7) = 2.0D+00
  ydatc(7) = 3.0D+00

  xdatc(8) = 3.0D+00
  ydatc(8) = 1.0D+00

  xdatc(9) = 3.0D+00
  ydatc(9) = 2.0D+00

  xdatc(10) = 3.0D+00
  ydatc(10) = 3.0D+00

  xdatc(11) = 3.0D+00
  ydatc(11) = 4.0D+00

  xdatc(12) = 4.0D+00
  ydatc(12) = 3.0D+00
!
!  Compute matrix of basis polynomials (1,x,y,x**2,xy,y**2) evaluated
!  at the data points.
!
  do i = 1,ndat
    ac(i,1) = 1.0D+00
    ac(i,2) = xdatc(i)
    ac(i,3) = ydatc(i)
    ac(i,4) = xdatc(i)**2
    ac(i,5) = xdatc(i)*ydatc(i)
    ac(i,6) = ydatc(i)**2
  end do
!
!  Compute the normal equations matrix.
!
  do i = 1,ncoef
    do j = 1,ncoef
      atac(i,j) = 0.0D+00
      do k = 1,ndat
        atac(i,j) = atac(i,j)+ac(k,i)*ac(k,j)
      end do
    end do
  end do
!
!  Factor the normal equations matrix.
!
  call sgetrf(ncoef,ncoef,atac,ncoef,ipivc,info)

  if ( info/= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'GetDU4 - Fatal error!'
    write ( *, * ) '  SGETRF returned INFO = ',info
    write ( *, * ) '  while factoring matrix ATAC.'
    stop
  end if
!
!  Each computation has its "center" at a pressure node, so we
!  can organize our do loops by counting through each presure node.
!
  do icol = 1,nx
    do irow = 1,ny

      ic = 2*(icol-1)*(2*ny-1)+(2*irow-1)

      isww = ic-2*(2*ny-1)-1
      iww = isww+1
      issw = ic-(2*ny-1)-2
      isw = issw+1
      iw = isw+1
      inw = iw+1
      iss = ic-2
      is = iss+1
      in = ic+1
      inn = ic+2
      ise = ic+(2*ny-1)-1
      ie = ise+1
      ine = ie+1
      inne = ine+1
      iee = ic+2*(2*ny-1)
      inee = iee+1
!
!  CASE: CORNER NODES, SKIP EM
!
      if ( icol == 1.and.irow== 1 ) then
      else if (icol == 1.and.irow==ny ) then
      else if (icol ==nx.and.irow== 1 ) then
      else if (icol ==nx.and.irow==ny ) then
!
!  CASE: THE NORTHERN BOUNDARY
!
      else if (irow ==ny ) then

        nodat(1) = isww
        nodat(2) = issw
        nodat(3) = isw
        nodat(4) = iw
        nodat(5) = is
        nodat(6) = ise
        nodat(7) = ie

        ndat = 7
!
!  Copy out the value of dUdY at the data points.
!
        do i = 1,ndat
          dat(i) = dudyn(nodat(i))
        end do
!
!  Compute right hand side of normal equations.
!
        do i = 1,ncoef
          b(i) = 0.0D+00
          do j = 1,ndat
            b(i) = b(i)+an(j,i)*dat(j)
          end do
        end do
!
!  Solve the normal equations.
!
        nrhs = 1
        call sgetrs('N',ncoef,nrhs,atan,ncoef,ipivn,b,ncoef,info)
!
!  Now evaluate the least squares polynomial at the nodes.
!
        if ( icol == 2 ) then

          x = 0.0D+00
          y = 2.0D+00
          call lspoly(b,ncoef,x,y,temp)
          nrep(iww) = nrep(iww)+1
          dudy4(iww) = dudy4(iww)+temp

          x = 0.0D+00
          y = 1.0D+00
          call lspoly(b,ncoef,x,y,temp)
          nrep(isww) = nrep(isww)+1
          dudy4(isww) = dudy4(isww)+temp

        end if

        do i = 1,ndat
          x = xdatn(i)
          y = ydatn(i)
          call lspoly(b,ncoef,x,y,temp)
          nrep(nodat(i)) = nrep(nodat(i))+1
          dudy4(nodat(i)) = dudy4(nodat(i))+temp
        end do

        x = 2.0D+00
        y = 2.0D+00
        call lspoly(b,ncoef,x,y,temp)
        nrep(ic) = nrep(ic)+1
        dudy4(ic) = dudy4(ic)+temp

        if ( icol ==nx-1 ) then
          x = 4.0D+00
          y = 2.0D+00
          call lspoly(b,ncoef,x,y,temp)
          nrep(iee) = nrep(iee)+1
          dudy4(iee) = dudy4(iee)+temp
        end if
!
!  CASE: THE EASTERN BOUNDARY
!
      else if (icol ==nx ) then

        nodat(1) = isww
        nodat(2) = issw
        nodat(3) = isw
        nodat(4) = iw
        nodat(5) = inw
        nodat(6) = is
        nodat(7) = in

        ndat = 7
!
!  Copy out the value of dUdY at the data points.
!
        do i = 1,ndat
          dat(i) = dudyn(nodat(i))
        end do
!
!  Compute right hand side of normal equations.
!
        do i = 1,ncoef
          b(i) = 0.0D+00
          do j = 1,ndat
            b(i) = b(i)+ae(j,i)*dat(j)
          end do
        end do
!
!  Solve the normal equations.
!
        nrhs = 1
        call sgetrs('N',ncoef,nrhs,atae,ncoef,ipive,b,ncoef,info)
!
!  Now evaluate the least squares polynomial at the nodes.
!
        if ( irow ==ny-1 ) then

          x = 2.0D+00
          y = 4.0D+00
          call lspoly(b,ncoef,x,y,temp)
          nrep(inn) = nrep(inn)+1
          dudy4(inn) = dudy4(inn)+temp

        end if

        if ( irow ==ny-1 ) then

          x = 2.0D+00
          y = 1.0D+00
          call lspoly(b,ncoef,x,y,temp)
          nrep(is) = nrep(is)+1
          dudy4(is) = dudy4(is)+temp

          x = 2.0D+00
          y = 0.0D+00
          call lspoly(b,ncoef,x,y,temp)
          nrep(iss) = nrep(iss)+1
          dudy4(iss) = dudy4(iss)+temp

        end if

        do i = 1,ndat
          x = xdate(i)
          y = ydate(i)
          call lspoly(b,ncoef,x,y,temp)
          nrep(nodat(i)) = nrep(nodat(i))+1
          dudy4(nodat(i)) = dudy4(nodat(i))+temp
        end do

        x = 2.0D+00
        y = 2.0D+00
        call lspoly(b,ncoef,x,y,temp)
        nrep(ic) = nrep(ic)+1
        dudy4(ic) = dudy4(ic)+temp
!
!  CASE: THE WESTERN BOUNDARY
!
      else if (icol == 1 ) then

        nodat(1) = is
        nodat(2) = in
        nodat(3) = ise
        nodat(4) = ie
        nodat(5) = ine
        nodat(6) = inne
        nodat(7) = inee

        ndat = 7
!
!  Copy out the value of dUdY at the data points.
!
        do i = 1,ndat
          dat(i) = dudyn(nodat(i))
        end do
!
!  Compute right hand side of normal equations.
!
        do i = 1,ncoef
          b(i) = 0.0D+00
          do j = 1,ndat
            b(i) = b(i)+aw(j,i)*dat(j)
          end do
        end do
!
!  Solve the normal equations.
!
        nrhs = 1
        call sgetrs('N',ncoef,nrhs,ataw,ncoef,ipivw,b,ncoef,info)
!
!  Now evaluate the least squares polynomial at the nodes.
!
        if ( irow ==ny-1 ) then

          x = 2.0D+00
          y = 4.0D+00
          call lspoly(b,ncoef,x,y,temp)
          nrep(inn) = nrep(inn)+1
          dudy4(inn) = dudy4(inn)+temp

          x = 3.0D+00
          y = 4.0D+00
          call lspoly(b,ncoef,x,y,temp)
          nrep(inne) = nrep(inne)+1
          dudy4(inne) = dudy4(inne)+temp

        end if

        if ( irow == 2 ) then

          x = 2.0D+00
          y = 0.0D+00
          call lspoly(b,ncoef,x,y,temp)
          nrep(iss) = nrep(iss)+1
          dudy4(iss) = dudy4(iss)+temp

        end if

        do i = 1,ndat
          x = xdatw(i)
          y = ydatw(i)
          call lspoly(b,ncoef,x,y,temp)
          nrep(nodat(i)) = nrep(nodat(i))+1
          dudy4(nodat(i)) = dudy4(nodat(i))+temp
        end do

        x = 2.0D+00
        y = 2.0D+00
        call lspoly(b,ncoef,x,y,temp)
        nrep(ic) = nrep(ic)+1
        dudy4(ic) = dudy4(ic)+temp
!
!  CASE: THE SOUTHERN BOUNDARY
!
      else if (irow == 1 ) then

        nodat(1) = iw
        nodat(2) = inw
        nodat(3) = in
        nodat(4) = ie
        nodat(5) = ine
        nodat(6) = inne
        nodat(7) = inee

        ndat = 7
!
!  Copy out the value of dUdY at the data points.
!
        do i = 1,ndat
          dat(i) = dudyn(nodat(i))
        end do
!
!  Compute right hand side of normal equations.
!
        do i = 1,ncoef
          b(i) = 0.0D+00
          do j = 1,ndat
            b(i) = b(i)+as(j,i)*dat(j)
          end do
        end do
!
!  Solve the normal equations.
!
        nrhs = 1
        call sgetrs('N',ncoef,nrhs,atas,ncoef,ipivs,b,ncoef,info)
!
!  Now evaluate the least squares polynomial at the nodes.
!
        if ( icol == 2 ) then
          x = 0.0D+00
          y = 2.0D+00
          call lspoly(b,ncoef,x,y,temp)
          nrep(iww) = nrep(iww)+1
          dudy4(iww) = dudy4(iww)+temp
        end if

        do i = 1,ndat
          x = xdats(i)
          y = ydats(i)
          call lspoly(b,ncoef,x,y,temp)
          nrep(nodat(i)) = nrep(nodat(i))+1
          dudy4(nodat(i)) = dudy4(nodat(i))+temp
        end do

        if ( icol ==nx-1 ) then

          x = 4.0D+00
          y = 2.0D+00
          call lspoly(b,ncoef,x,y,temp)
          nrep(iee) = nrep(iee)+1
          dudy4(iee) = dudy4(iee)+temp

          x = 4.0D+00
          y = 3.0D+00
          call lspoly(b,ncoef,x,y,temp)
          nrep(inee) = nrep(inee)+1
          dudy4(inee) = dudy4(inee)+temp

        end if

        x = 2.0D+00
        y = 2.0D+00
        call lspoly(b,ncoef,x,y,temp)
        nrep(ic) = nrep(ic)+1
        dudy4(ic) = dudy4(ic)+temp
!
!  CASE: CENTRAL
!
      else

        nodat(1) = isww
        nodat(2) = issw
        nodat(3) = isw
        nodat(4) = iw
        nodat(5) = inw
        nodat(6) = is
        nodat(7) = in
        nodat(8) = ise
        nodat(9) = ie
        nodat(10) = ine
        nodat(11) = inne
        nodat(12) = inee

        ndat = 12
!
!  Copy out the value of dUdY at the data points.
!
        do i = 1,ndat
          dat(i) = dudyn(nodat(i))
        end do
!
!  Compute right hand side of normal equations.
!
        do i = 1,ncoef
          b(i) = 0.0D+00
          do j = 1,ndat
            b(i) = b(i)+ac(j,i)*dat(j)
          end do
        end do
!
!  Solve the normal equations.
!
        nrhs = 1
        call sgetrs('N',ncoef,nrhs,atac,ncoef,ipivc,b,ncoef,info)
!
!  Now evaluate the least squares polynomial at the nodes.
!
        do i = 1,ndat
          x = xdatc(i)
          y = ydatc(i)
          call lspoly(b,ncoef,x,y,temp)
          nrep(nodat(i)) = nrep(nodat(i))+1
          dudy4(nodat(i)) = dudy4(nodat(i))+temp
        end do

        x = 2.0D+00
        y = 2.0D+00
        call lspoly(b,ncoef,x,y,temp)
        nrep(ic) = nrep(ic)+1
        dudy4(ic) = dudy4(ic)+temp

      end if

    end do
  end do
!
!  Average the smoothed data.
!
  do i = 1,np

    if ( nrep(i) == 0 ) then
      write ( *, * ) ' '
      write ( *, * ) 'GetDU4 - Fatal error!'
      write ( *, * ) '  NREP <=  0 for node ',I
      stop
    end if

    dudy4(i) = dudy4(i)/real(nrep(i), kind = 8 )

  end do

  return
end
subroutine getdu5(dudyn,dudy5,np,nx,ny,u,xc,yc)
!
!*******************************************************************************
!
!! GETDU5 uses interpolating serendipity quadrilaterals in an attempt
!  to improve the accuracy of the computed values of dPdY, dUdY and
!  dVdY at every node.
!
!  GETDU5 interpolates the U values, and then compute dUdY in the
!  serendipity element.
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
  integer np
!
  real ( kind = 8 ) dudyn(np)
  real ( kind = 8 ) dudy5(np)
  integer i
  integer ic
  integer icol
  integer irow
  integer nx
  integer ny
  character ( len = 2 ) type
  real ( kind = 8 ) u(np)
  real ( kind = 8 ) xc(np)
  real ( kind = 8 ) yc(np)
!
!  Copy DUDYN.
!  Most of the values of DUDYN will not be changed.
!
  do i = 1,np
    dudy5(i) = dudyn(i)
  end do
!
  do icol = 1,nx
    do irow = 1,ny

      ic = 2*(icol-1)*(2*ny-1)+(2*irow-1)

      if ( icol == 1.and.irow== 1 ) then
        type = 'SW'
      else if (icol == 1.and.irow==ny ) then
        type = 'NW'
      else if (icol ==nx.and.irow== 1 ) then
        type = 'SE'
      else if (icol ==nx.and.irow==ny ) then
        type = 'NE'
      else if (irow ==ny ) then
        type = 'N'
      else if (icol ==nx ) then
        type = 'E'
      else if (icol == 1 ) then
        type = 'W'
      else if (irow == 1 ) then
        type = 'S'
      else
        type = 'C'
      end if

      if ( ic == 15 ) then
        write ( *, * ) ' '
        write ( *, * ) 'GETDU5 - Note:'
        write ( *, * ) '  Type of IC is '//type
      end if

      call sertrn(dudy5(ic),ic,np,ny,type,u,xc,yc)

    end do
  end do

  return
end
subroutine getfix(dpdyn,dudyn,dvdyn,dydpn,gradf,ibs,indx,iopt,maxeqn,np, &
  npar,nparb,nparf,splbmp,taubmp,xbl,xbr,xc,yc)
!
!*******************************************************************************
!
!! GETFIX corrects the finite difference estimate of the sensitivities.
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
  integer maxeqn
  integer np
  integer npar
  integer nparb
  integer nparf
!
  real ( kind = 8 ) dpdyn(np)
  real ( kind = 8 ) dudyn(np)
  real ( kind = 8 ) dvdyn(np)
  real ( kind = 8 ) dydpn(np,nparb)
  real ( kind = 8 ) gradf(maxeqn,npar)
  integer ibs
  integer ihor
  integer indx(3,np)
  integer iopt(npar)
  integer ip
  integer ipar
  integer iparb
  integer iprs
  integer iver
  integer jderiv
  real ( kind = 8 ) shape
  real ( kind = 8 ) splbmp(4,nparb+2,0:nparb)
  real ( kind = 8 ) taubmp(nparb+2)
  real ( kind = 8 ) xbl
  real ( kind = 8 ) xbr
  real ( kind = 8 ) xc(np)
  real ( kind = 8 ) xq
  real ( kind = 8 ) ybot
  real ( kind = 8 ) yc(np)
  real ( kind = 8 ) yq
!
!  Compute dYdParameter
!
  do ip = 1,np

    xq = xc(ip)
    yq = yc(ip)

    do iparb = 1,nparb

      dydpn(ip,iparb) = 0.0D+00
!
!  Nodes only move if their X coordinate is (strictly) between XBL
!  and XBR.
!
      if ( xbl<xq.and.xq<xbr ) then
!
!  Compute YBOT, the height of the bump below the current point.
!  and SHAPE, the height at XQ of the basis function
!  associated with parameter IPARB.
!
        if ( ibs == 1 ) then
          call plval(nparb+2,xq,taubmp,ybot,splbmp)
          call plval1(iparb+1,nparb+2,xq,taubmp,shape)
        else if (ibs == 2 ) then
          call pqval(nparb+2,xq,taubmp,ybot,splbmp)
          call pqval1(iparb+1,nparb+2,xq,taubmp,shape)
        else if (ibs ==3 ) then
          jderiv = 0
          call ppvalu(taubmp,splbmp(1,1,0),nparb+1,4,xq,jderiv,ybot)
          jderiv = 0
          call ppvalu(taubmp,splbmp(1,1,iparb),nparb+1,4,xq,jderiv,shape)
        end if

        dydpn(ip,iparb) = ((3.0-yq)/(3.0-ybot))*shape

      end if

   end do

  end do
!
!  Use these results to compute corrections for those entries of the
!  finite difference vector that correspond to shape parameters, and which
!  are based at nodes which move during the estimation.
!
  do ipar = nparf+1,nparf+nparb

    if ( iopt(ipar) == 1 ) then

      do ip = 1,np

        ihor = indx(1,ip)
        gradf(ihor,ipar) = dudyn(ip)*dydpn(ip,ipar-nparf)

        iver = indx(2,ip)
        gradf(iver,ipar) = dvdyn(ip)*dydpn(ip,ipar-nparf)

        iprs = indx(3,ip)
        if ( iprs>0 ) then
          gradf(iprs,ipar) = dpdyn(ip)*dydpn(ip,ipar-nparf)
        end if

      end do

    end if

  end do

  return
end
subroutine getgrd(a,area,cost,dpara3,epsdif,eqn,etaq,g,g1,g2,gdif,gtar,ifs, &
  ibs,ierror,ijac,indx,iopt,ipivot,isotri,iwrite,jjac,maxeqn,maxnew,nelem, &
  neqn,nlband,node,np,npar,nparb,nparf,nprof,nquad,nrow,nx,ny,para,phi,res, &
  splbmp,splflo,syseqn,taubmp,tauflo,tolnew,wateb,watep,wateu,watev,wquad, &
  xbl,xbr,xc,xquad,xsiq,ybl,ybr,yc,yquad)
!
!*******************************************************************************
!
!! GETGRD uses finite differences to compute GDIF, an estimate of the
!  partial derivatives of the state variables with respect to the
!  parameters.
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
!
!    PARA should contain the current parameter values,
!    G the corresponding state variables, and
!    COST the resulting cost.
!
!
  integer maxeqn
  integer nelem
  integer neqn
  integer np
  integer npar
  integer nparb
  integer nparf
  integer nquad
  integer nrow
  integer nx
  integer ny
!
  real ( kind = 8 ) a(nrow,neqn)
  real ( kind = 8 ) area(nquad,nelem)
  real ( kind = 8 ) cost
  real ( kind = 8 ) cost1
  real ( kind = 8 ) costb
  real ( kind = 8 ) costp
  real ( kind = 8 ) costu
  real ( kind = 8 ) costv
  real ( kind = 8 ) dpara3(npar)
  real ( kind = 8 ) epsdif
  character ( len = 2 ) eqn(neqn)
  real ( kind = 8 ) etaq(nquad)
  real ( kind = 8 ) g(neqn)
  real ( kind = 8 ) g1(neqn)
  real ( kind = 8 ) g2(neqn)
  real ( kind = 8 ) gdif(maxeqn,npar)
  real ( kind = 8 ) gtar(neqn)
  real ( kind = 8 ) h
  integer i
  integer ifs
  integer ibs
  integer ieqn
  integer ierror
  integer ijac
  integer indx(3,np)
  integer iopt(npar)
  integer ipar
  integer ipivot(neqn)
  integer isotri(nelem)
  integer iwrite
  integer jjac
  logical lgdif
  integer maxnew
  integer nlband
  integer node(6,nelem)
  integer nprof(2*ny-1)
  real ( kind = 8 ) one
  real ( kind = 8 ) para(npar)
  real ( kind = 8 ) phi(nquad,6,10,nelem)
  real ( kind = 8 ) psave
  real ( kind = 8 ) res(neqn)
  real ( kind = 8 ) splbmp(4,nparb+2,0:nparb)
  real ( kind = 8 ) splflo(4,nparf+2,0:nparf)
  character ( len = 20 ) syseqn
  real ( kind = 8 ) taubmp(nparb+2)
  real ( kind = 8 ) tauflo(nparf+2)
  real ( kind = 8 ) tolnew
  real ( kind = 8 ) wateb
  real ( kind = 8 ) watep
  real ( kind = 8 ) wateu
  real ( kind = 8 ) watev
  real ( kind = 8 ) wquad(nquad)
  real ( kind = 8 ) xbl
  real ( kind = 8 ) xbr
  real ( kind = 8 ) xc(np)
  real ( kind = 8 ) xquad(nquad,nelem)
  real ( kind = 8 ) xsiq(nquad)
  real ( kind = 8 ) ybl
  real ( kind = 8 ) ybr
  real ( kind = 8 ) yc(np)
  real ( kind = 8 ) yquad(nquad,nelem)
!
!  Set a logical flag, so that all routines know that we are doing
!  a finite difference computation.
!
  lgdif = .true.
  call lmemry('set','fd_grad',lgdif)
!
!  Consider each parameter.
!
  do ipar = 1,npar
!
!  If the parameter is allowed to vary,
!
    if ( iopt(ipar) == 1 ) then

      do i = 1,neqn
        g1(i) = g(i)
      end do
!
!  ...then perturb the parameter by the amount H.
!
      psave = para(ipar)
      one = 1.0D+00
      h = epsdif*sign(one,psave)*(abs(psave)+one)
      para(ipar) = para(ipar)+h
!
!  Solve the flow problem.
!
      call flosol(a,area,eqn,etaq,g1,g2,ifs,ibs,ierror,ijac,indx,ipivot, &
        isotri,iwrite,jjac,maxnew,nelem,neqn,nlband,node,np,npar,nparb,nparf, &
        nquad,nrow,nx,ny,para,phi,res,splbmp,splflo,syseqn,taubmp,tauflo, &
        tolnew,wquad,xbl,xbr,xc,xquad,xsiq,ybl,ybr,yc,yquad)

      if ( ierror/= 0 ) then
        write ( *, * ) ' '
        write ( *, * ) 'GetGrd - Fatal error!'
        write ( *, * ) '  FloSol returns IERROR = ',ierror
        lgdif = .false.
        call lmemry('set','fd_grad',lgdif)
        return
      end if
!
!  Set the estimate of dG/dP(Ipar)
!
      do ieqn = 1,neqn
        gdif(ieqn,ipar) = (g1(ieqn)-g(ieqn))/h
      end do
!
!  Compute the COST of the perturbed solution and evaluate the
!  finite difference quotient.
!
      call getcst(cost1,costb,costp,costu,costv,g1,gtar,ibs,indx,neqn,np, &
        nparb,nprof,ny,splbmp,taubmp,wateb,watep,wateu,watev,xbl,xbr,ybl,ybr,yc)

      dpara3(ipar) = (cost1-cost)/h
!
!  Restore the value of the parameter.
!
      para(ipar) = psave
!
!  If the parameter is not varied, set dG/dP(Ipar) and DPARA3 to zero.
!
    else

      do ieqn = 1,neqn
        gdif(ieqn,ipar) = 0.0D+00
      end do

      dpara3(ipar) = 0.0D+00

    end if

  end do
!
!  Restore old data.
!
  call flosol(a,area,eqn,etaq,g,g2,ifs,ibs,ierror,ijac,indx,ipivot,isotri, &
    iwrite,jjac,maxnew,nelem,neqn,nlband,node,np,npar,nparb,nparf,nquad,nrow, &
    nx,ny,para,phi,res,splbmp,splflo,syseqn,taubmp,tauflo,tolnew,wquad,xbl, &
    xbr,xc,xquad,xsiq,ybl,ybr,yc,yquad)

  if ( ierror/= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'GetGrd - Fatal error!'
    write ( *, * ) '  FloSol returns IERROR = ',ierror
    lgdif = .false.
    call lmemry('set','fd_grad',lgdif)
    return
  end if
!
!  Turn off the flag that warns other routines we are doing a
!  finite difference computation.
!
  lgdif = .false.
  call lmemry('set','fd_grad',lgdif)

  return
end
subroutine getsen(a,area,dudyn,dvdyn,eqn,g,ibc,ifs,ibs,indx,iopt,ipivot, &
  maxeqn,nelem,neqn,nlband,node,np,npar,nparb,nparf,nquad,nrow,phi,sens, &
  splbmp,splflo,taubmp,tauflo,xc,yc)
!
!*******************************************************************************
!
!! GETSEN computes the sensitivities of the state variables U, V and
!  P with respect to the parameters.
!
!  It assumes that the jacobian matrix A has already been factored.
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
!  A      Input, real A(NROW,NEQN), contains the
!         value of D F(I)/D X(J) for each of the NEQN residual
!         functions F(I) with respect to each of the unknown
!         coefficients X(J).
!
!  SENS   Output, real SENS(MAXEQN,NPAR).
!
!         SENS(I,J) contains the sensitivity of the I-th unknown
!         with respect to the J-th parameter.
!
!
  integer maxeqn
  integer nelem
  integer neqn
  integer np
  integer npar
  integer nparb
  integer nparf
  integer nquad
  integer nrow
!
  real ( kind = 8 ) a(nrow,neqn)
  real ( kind = 8 ) area(nquad,nelem)
  real ( kind = 8 ) dudyn(np)
  real ( kind = 8 ) dvdyn(np)
  character ( len = 2 ) eqn(neqn)
  real ( kind = 8 ) g(neqn)
  integer i
  integer ibc
  integer ifs
  integer ibs
  integer indx(3,np)
  integer info
  integer iopt(npar)
  integer ipar
  integer ipivot(neqn)
  integer nlband
  integer node(6,nelem)
  real ( kind = 8 ) phi(nquad,6,10,nelem)
  real ( kind = 8 ) sens(maxeqn,npar)
  real ( kind = 8 ) splbmp(4,nparb+2,0:nparb)
  real ( kind = 8 ) splflo(4,nparf+2,0:nparf)
  real ( kind = 8 ) taubmp(nparb+2)
  real ( kind = 8 ) tauflo(nparf+2)
  real ( kind = 8 ) xc(np)
  real ( kind = 8 ) yc(np)
!
!  Set up the right hand sides.
!
  do ipar = 1,npar

    if ( iopt(ipar) == 1 ) then

      if ( 1<= ipar.and.ipar<=nparf ) then

        call flosen(eqn,sens(1,ipar),ifs,indx,ipar,neqn,np,nparf,splflo, &
          tauflo,yc)

      else if (nparf+1<= ipar.and.ipar<=nparf+nparb ) then

        call bmpsen(dudyn,dvdyn,eqn,sens(1,ipar),g,ibc,ibs,indx,ipar,neqn, &
          np,nparb,nparf,splbmp,taubmp,xc,yc)

      else if (ipar ==nparf+nparb+1 ) then

        call reysen(area,eqn,g,indx,nelem,neqn,node,np,nquad,phi,sens(1,ipar))

      end if

    else

      do i = 1,neqn
        sens(i,ipar) = 0.0D+00
      end do

    end if

  end do
!
!  Solve the linear systems.
!
  call sgbtrs('N',neqn,nlband,nlband,npar,a,nrow,ipivot,sens,maxeqn,info)

  if ( info/= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'GetSen - Warning!'
    write ( *, * ) '  Failure solving for sensitivity ',ipar
    write ( *, * ) '  SGBTRS returns nonzero INFO = ',info
  end if

  return
end
subroutine gquad1(nquad1,wquad1,xquad1)
!
!*******************************************************************************
!
!! GQUAD1 returns the weights and abscissas for a 1 dimensional,
!  3 or 5 point Gauss quadrature rule defined on the interval [-1,1].
!
!
!  Discussion:
!
!    The integral of a function F(X) over the interval [-1,1]
!
!      Integral ( -1 <= X <= 1 ) F(X) dX
!
!    may then be approximated by
!
!      Sum ( 1 <= I <= NQUAD1) WQUAD1(I) * F(XQUAD1(I))
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
!    Input, integer NQUAD1, the quadrature order.
!    Only values of 3 or 5 are legal.
!
!    Output, real WQUAD1(NQUAD1), the quadrature weights.
!
!    Output, real XQUAD1(NQUAD1), the quadrature points.
!
!
  integer nquad1
!
  real ( kind = 8 ) wquad1(nquad1)
  real ( kind = 8 ) xquad1(nquad1)
!
  if ( nquad1 == 3 ) then

    xquad1(1) = -0.7745966692D+00
    xquad1(2) =  0.0D+00
    xquad1(3) =  0.7745966692D+00

    wquad1(1) = 5.0D+00/9.0D+00
    wquad1(2) = 8.0D+00/9.0D+00
    wquad1(3) = 5.0D+00/9.0D+00

  else if ( nquad1 == 5 ) then

    xquad1(1) = -0.906179845938664D+00
    xquad1(2) = -0.538469310105683D+00
    xquad1(3) =  0.0D+00
    xquad1(4) =  0.538469310105683D+00
    xquad1(5) =  0.906179845938664D+00

    wquad1(1) = 0.236926885056189D+00
    wquad1(2) = 0.478628670499366D+00
    wquad1(3) = 0.568888888888889D+00
    wquad1(4) = 0.478628670499366D+00
    wquad1(5) = 0.236926885056189D+00

  else

    write ( *, * ) ' '
    write ( *, * ) 'GQuad1 - Fatal error!'
    write ( *, * ) '  An illegal value of NQUAD1 was input.'
    write ( *, * ) '  Only NQUAD1 = 3 or 5 are legal.'
    write ( *, * ) '  The input value was ',nquad1
    write ( *, * ) '  The code is stopping now.'
    stop

  end if

  return
end
subroutine hello(maxnx,maxny)
!
!*******************************************************************************
!
!! HELLO prints the program name, date, and computer name.
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
  character ( len = 8 ) date
  integer maxnx
  integer maxny
  character ( len = 10 ) time
!
  call date_and_time ( date, time )

  write ( *, * ) ' '
  write ( *, * ) 'Flow4'
  write ( *, * ) '  This is the version of 09 May 1996.'
  write ( *, * ) ' '
  write ( *, * ) '  Today''s date: ', date
  write ( *, * ) '  Today''s time: ', time
  write ( *, * ) ' '
  write ( *, * ) '  The maximum problem size is ',maxnx,' by ',maxny

  return
end
function ilaenv(ispec,name,opts,n1,n2,n3,n4)
!
!*******************************************************************************
!
!! ILAENV is called from the LAPACK routines to choose problem-dependent
!  parameters for the local environment.  See ISPEC for a description of
!  the parameters.
!
!  This version provides a set of parameters which should give good,
!  but not optimal, performance on many of the currently available
!  computers.  Users are encouraged to modify this subroutine to set
!  the tuning parameters for their particular machine using the option
!  and problem size information in the arguments.
!
!  This routine will not function correctly if it is converted to all
!  lower case.  Converting it to all upper case is allowed.
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
!  Arguments
!   =========
!
!  ISPEC   (input) INTEGER
!          Specifies the parameter to be returned as the value of
!          ILAENV.
!          = 1: the optimal blocksize; if this value is 1, an unblocked
!               algorithm will give the best performance.
!          = 2: the minimum block size for which the block routine
!               should be used; if the usable block size is less than
!               this value, an unblocked routine should be used.
!          = 3: the crossover point (in a block routine, for N less
!               than this value, an unblocked routine should be used)
!          = 4: the number of shifts, used in the nonsymmetric
!               eigenvalue routines
!          = 5: the minimum column dimension for blocking to be used;
!               rectangular blocks must have dimension at least k by m,
!               where k is given by ILAENV(2,...) and m by ILAENV(5,...)
!          = 6: the crossover point for the SVD (when reducing an m by n
!               matrix to bidiagonal form, if max(m,n)/min(m,n) exceeds
!               this value, a QR factorization is used first to reduce
!               the matrix to a triangular form.)
!          = 7: the number of processors
!          = 8: the crossover point for the multishift QR and QZ methods
!               for nonsymmetric eigenvalue problems.
!
!  NAME    (input) CHARACTER*(*)
!          The name of the calling subroutine, in either upper case or
!          lower case.
!
!  OPTS    (input) CHARACTER*(*)
!          The character options to the subroutine NAME, concatenated
!          into a single character string.  For example, UPLO = 'U',
!          TRANS = 'T', and DIAG = 'N' for a triangular routine would
!          be specified as OPTS = 'UTN'.
!
!  N1      (input) INTEGER
!  N2      (input) INTEGER
!  N3      (input) INTEGER
!  N4      (input) INTEGER
!          Problem dimensions for the subroutine NAME; these may not all
!          be required.
!
! (ILAENV) (output) INTEGER
!          >=  0: the value of the parameter specified by ISPEC
!          < 0:  if ILAENV = -k, the k-th argument had an illegal value.
!
!  Further Details
!   ===============
!
!  The following conventions have been used when calling ILAENV from the
!  LAPACK routines:
!  1)  OPTS is a concatenation of all of the character options to
!    subroutine NAME, in the same order that they appear in the
!      argument list for NAME, even if they are not used in determining
!      the value of the parameter specified by ISPEC.
!  2)  The problem dimensions N1, N2, N3, N4 are specified in the order
!      that they appear in the argument list for NAME.  N1 is used
!      first, N2 second, and so on, and unused problem dimensions are
!      passed a value of -1.
!  3)  The parameter value returned by ILAENV is checked for validity in
!      the calling subroutine.  For example, ILAENV is used to retrieve
!      the optimal blocksize for STRTRI as follows:
!
!      NB = ILAENV( 1, 'STRTRI', UPLO // DIAG, N, -1, -1, -1 )
!      IF( NB.LE.1 ) NB = MAX( 1, N )
!
  character ( len = * )    name, opts
  integer ilaenv
  integer            ispec, n1, n2, n3, n4
  logical            cname, sname
  character        c1
  character ( len = 2 )        c2, c4
  character ( len = 3 )        c3
  character ( len = 6 )        subnam
  integer            i, ic, iz, nb, nbmin, nx
!     ..
!     .. Intrinsic Functions ..
  intrinsic          char, ichar, int, min, real
!     ..
!     .. Executable Statements ..
!
  go to ( 100, 100, 100, 400, 500, 600, 700, 800 ) ispec
!
!     Invalid value for ISPEC
!
  ilaenv = -1
  return
!
  100 continue
!
!     Convert NAME to upper case if the first character is lower case.
!
  ilaenv = 1
  subnam = name
  ic = ichar( subnam( 1:1 ) )
  iz = ichar( 'z' )
  if ( iz ==90 .or. iz== 122 ) then
!
!        ASCII character set
!
     if ( ic>= 97 .and. ic<=122 ) then
        subnam( 1:1 ) = char( ic-32 )
        do 10 i = 2, 6
           ic = ichar( subnam( i:i ) )
           if ( ic>= 97 .and. ic<=122 ) &
                subnam( i:i ) = char( ic-32 )
   10       continue
     end if
!
  else if ( iz == 233 .or. iz== 169 ) then
!
!        EBCDIC character set
!
     if ( ( ic>= 129 .and. ic<=137 ) .or. &
           ( ic>= 145 .and. ic<=153 ) .or. &
           ( ic>= 162 .and. ic<=169 ) ) then
        subnam( 1:1 ) = char( ic+64 )
        do 20 i = 2, 6
           ic = ichar( subnam( i:i ) )
           if ( ( ic>= 129 .and. ic<=137 ) .or. &
                 ( ic>= 145 .and. ic<=153 ) .or. &
                 ( ic>= 162 .and. ic<=169 ) ) &
                subnam( i:i ) = char( ic+64 )
   20       continue
     end if
!
  else if ( iz == 218 .or. iz==250 ) then
!
!        Prime machines:  ASCII+128
!
     if ( ic>= 225 .and. ic<=250 ) then
        subnam( 1:1 ) = char( ic-32 )
        do 30 i = 2, 6
           ic = ichar( subnam( i:i ) )
           if ( ic>= 225 .and. ic<=250 ) &
                subnam( i:i ) = char( ic-32 )
   30       continue
     end if
  end if
!
  c1 = subnam( 1:1 )
  sname = c1=='s' .or. c1=='d'
  cname = c1=='c' .or. c1=='z'
  if ( .not.( cname .or. sname ) ) return
  c2 = subnam( 2:3 )
  c3 = subnam( 4:6 )
  c4 = c3( 2:3 )
!
  go to ( 110, 200, 300 ) ispec
!
  110 continue
!
!     ISPEC = 1:  block size
!
!     In these examples, separate code is provided for setting NB for
!     real and complex.  We assume that NB will take the same value in
!     single or real.
!
  nb = 1
!
  if ( c2 =='ge' ) then
     if ( c3 =='trf' ) then
        if ( sname ) then
           nb = 64
        else
           nb = 64
        end if
     else if ( c3 =='qrf' .or. c3=='rqf' .or. c3=='lqf' .or. c3=='qlf' ) then
        if ( sname ) then
           nb = 32
        else
           nb = 32
        end if
     else if ( c3 =='hrd' ) then
        if ( sname ) then
           nb = 32
        else
           nb = 32
        end if
     else if ( c3 =='brd' ) then
        if ( sname ) then
           nb = 32
        else
           nb = 32
        end if
     else if ( c3 =='tri' ) then
        if ( sname ) then
           nb = 64
        else
           nb = 64
        end if
     end if
  else if ( c2 =='po' ) then
     if ( c3 =='trf' ) then
        if ( sname ) then
           nb = 64
        else
           nb = 64
        end if
     end if
  else if ( c2 =='sy' ) then
     if ( c3 =='trf' ) then
        if ( sname ) then
           nb = 64
        else
           nb = 64
        end if
     else if ( sname .and. c3 =='trd' ) then
        nb = 1
     else if ( sname .and. c3 =='gst' ) then
        nb = 64
     end if
  else if ( cname .and. c2 =='he' ) then
     if ( c3 =='trf' ) then
        nb = 64
     else if ( c3 =='trd' ) then
        nb = 1
     else if ( c3 =='gst' ) then
        nb = 64
     end if
  else if ( sname .and. c2 =='or' ) then
     if ( c3( 1:1 ) =='g' ) then
        if ( c4 =='qr' .or. c4=='rq' .or. c4=='lq' .or. &
               c4 =='ql' .or. c4=='hr' .or. c4=='tr' .or. &
               c4 =='br' ) then
           nb = 32
        end if
     else if ( c3( 1:1 ) =='m' ) then
        if ( c4 =='qr' .or. c4=='rq' .or. c4=='lq' .or. &
              c4 =='ql' .or. c4=='hr' .or. c4=='tr' .or. &
               c4 =='br' ) then
           nb = 32
        end if
     end if
  else if ( cname .and. c2 =='un' ) then
     if ( c3( 1:1 ) =='g' ) then
        if ( c4 =='qr' .or. c4=='rq' .or. c4=='lq' .or. &
               c4 =='ql' .or. c4=='hr' .or. c4=='tr' .or. &
               c4 =='br' ) then
           nb = 32
        end if
     else if ( c3( 1:1 ) =='m' ) then
        if ( c4 =='qr' .or. c4=='rq' .or. c4=='lq' .or. &
              c4 =='ql' .or. c4=='hr' .or. c4=='tr' .or. &
               c4 =='br' ) then
           nb = 32
        end if
     end if
  else if ( c2 =='gb' ) then
     if ( c3 =='trf' ) then
        if ( sname ) then
           if ( n4<= 64 ) then
              nb = 1
           else
              nb = 32
           end if
        else
           if ( n4<= 64 ) then
              nb = 1
           else
              nb = 32
           end if
        end if
     end if
  else if ( c2 =='pb' ) then
     if ( c3 =='trf' ) then
        if ( sname ) then
           if ( n2<= 64 ) then
              nb = 1
           else
              nb = 32
           end if
        else
           if ( n2<= 64 ) then
              nb = 1
           else
              nb = 32
           end if
        end if
     end if
  else if ( c2 =='tr' ) then
     if ( c3 =='tri' ) then
        if ( sname ) then
           nb = 64
        else
           nb = 64
        end if
     end if
  else if ( c2 =='la' ) then
     if ( c3 =='uum' ) then
        if ( sname ) then
           nb = 64
        else
           nb = 64
        end if
     end if
  else if ( sname .and. c2 =='st' ) then
     if ( c3 =='ebz' ) then
        nb = 1
     end if
  end if
  ilaenv = nb
  return
!
  200 continue
!
!     ISPEC = 2:  minimum block size
!
  nbmin = 2
  if ( c2 =='ge' ) then
     if ( c3 =='qrf' .or. c3=='rqf' .or. c3=='lqf' .or.c3=='qlf' ) then
        if ( sname ) then
           nbmin = 2
        else
           nbmin = 2
        end if
     else if ( c3 =='hrd' ) then
        if ( sname ) then
           nbmin = 2
        else
           nbmin = 2
        end if
     else if ( c3 =='brd' ) then
        if ( sname ) then
           nbmin = 2
        else
           nbmin = 2
        end if
     else if ( c3 =='tri' ) then
        if ( sname ) then
           nbmin = 2
        else
           nbmin = 2
        end if
     end if
  else if ( c2 =='sy' ) then
     if ( c3 =='trf' ) then
        if ( sname ) then
           nbmin = 2
        else
           nbmin = 2
        end if
     else if ( sname .and. c3 =='trd' ) then
        nbmin = 2
     end if
  else if ( cname .and. c2 =='he' ) then
     if ( c3 =='trd' ) then
        nbmin = 2
     end if
  else if ( sname .and. c2 =='or' ) then
     if ( c3( 1:1 ) =='g' ) then
        if ( c4 =='qr' .or. c4=='rq' .or. c4=='lq' .or. &
               c4 =='ql' .or. c4=='hr' .or. c4=='tr' .or. &
               c4 =='br' ) then
           nbmin = 2
        end if
     else if ( c3( 1:1 ) =='m' ) then
        if ( c4 =='qr' .or. c4=='rq' .or. c4=='lq' .or. &
              c4 =='ql' .or. c4=='hr' .or. c4=='tr' .or. &
               c4 =='br' ) then
           nbmin = 2
        end if
     end if
  else if ( cname .and. c2 =='un' ) then
     if ( c3( 1:1 ) =='g' ) then
        if ( c4 =='qr' .or. c4=='rq' .or. c4=='lq' .or. &
              c4 =='ql' .or. c4=='hr' .or. c4=='tr' .or. &
               c4 =='br' ) then
           nbmin = 2
        end if
     else if ( c3( 1:1 ) =='m' ) then
        if ( c4 =='qr' .or. c4=='rq' .or. c4=='lq' .or. &
              c4 =='ql' .or. c4=='hr' .or. c4=='tr' .or. &
               c4 =='br' ) then
           nbmin = 2
        end if
     end if
  end if
  ilaenv = nbmin
  return
!
  300 continue
!
!     ISPEC = 3:  crossover point
!
  nx = 0
  if ( c2 =='ge' ) then
     if ( c3 =='qrf' .or. c3=='rqf' .or. c3=='lqf' .or. c3=='qlf' ) then
        if ( sname ) then
           nx = 128
        else
           nx = 128
        end if
     else if ( c3 =='hrd' ) then
        if ( sname ) then
           nx = 128
        else
           nx = 128
        end if
     else if ( c3 =='brd' ) then
        if ( sname ) then
           nx = 128
        else
           nx = 128
        end if
     end if
  else if ( c2 =='sy' ) then
     if ( sname .and. c3 =='trd' ) then
        nx = 1
     end if
  else if ( cname .and. c2 =='he' ) then
     if ( c3 =='trd' ) then
        nx = 1
     end if
  else if ( sname .and. c2 =='or' ) then
     if ( c3( 1:1 ) =='g' ) then
        if ( c4 =='qr' .or. c4=='rq' .or. c4=='lq' .or. &
              c4 =='ql' .or. c4=='hr' .or. c4=='tr' .or. &
               c4 =='br' ) then
           nx = 128
        end if
     end if
  else if ( cname .and. c2 =='un' ) then
     if ( c3( 1:1 ) =='g' ) then
        if ( c4 =='qr' .or. c4=='rq' .or. c4=='lq' .or. &
              c4 =='ql' .or. c4=='hr' .or. c4=='tr' .or. &
               c4 =='br' ) then
           nx = 128
        end if
     end if
  end if
  ilaenv = nx
  return
!
  400 continue
!
!     ISPEC = 4:  number of shifts (used by xHSEQR)
!
  ilaenv = 6
  return
!
  500 continue
!
!     ISPEC = 5:  minimum column dimension (not used)
!
  ilaenv = 2
  return
!
  600 continue
!
!     ISPEC = 6:  crossover point for SVD (used by xGELSS and xGESVD)
!
  ilaenv = int( real( min ( n1, n2 ), kind = 8 ) * 1.6D+00 )
  return
!
  700 continue
!
!     ISPEC = 7:  number of processors (not used)
!
  ilaenv = 1
  return
!
  800 continue
!
!     ISPEC = 8:  crossover point for multishift (used by xHSEQR)
!
  ilaenv = 50
  return
!
!     End of ILAENV
!
end
subroutine init(area,cost,costar,costb,costp,costu,costv,dopt,dpara3,dparsn, &
  dparfd,dparfdc,dpdyn,dudyn,dvdyn,dydpn,epsdif,eqn,etan,fileg,filet,g,g1,g2, &
  g3,gdif,gold,gopt,gradf,gtar,ibc,ifscan,ifstar,ibscan,ibstar,ibump,idfd, &
  ids,ierror,ifds,igrad,igunit,ijac,indx,iopt,iplot,isotri,istep1,istep2,itar, &
  itunit,itype,ivopt,iwrite,jjac,jstep1,jstep2,liv,lv,maxelm,maxeqn,maxnew, &
  maxnp,maxpar,maxparb,maxquad,maxstp,node,nopt,npar,nparb,nparf,npe,nquad, &
  nstep3,nx,ny,para1,para2,para3,partar,sens,syseqn,tolnew,tolopt,vopt,wateb, &
  wateb1,wateb2,watep,wateu,watev,xblcan,xbltar,xbrcan,xbrtar,xprof,xsin, &
  yblcan,ybltar,ybrcan,ybrtar)
!
!*******************************************************************************
!
!! INIT initializes the parameters.
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
  integer liv
  integer lv
  integer maxelm
  integer maxeqn
  integer maxnp
  integer maxpar
  integer maxparb
  integer maxquad
!
  real ( kind = 8 ) area(maxquad,maxelm)
  real ( kind = 8 ) cost
  real ( kind = 8 ) costar
  real ( kind = 8 ) costb
  real ( kind = 8 ) costp
  real ( kind = 8 ) costu
  real ( kind = 8 ) costv
  real ( kind = 8 ) dopt(maxpar)
  real ( kind = 8 ) dpara3(maxpar)
  real ( kind = 8 ) dparsn(maxpar)
  real ( kind = 8 ) dparfd(maxpar)
  real ( kind = 8 ) dparfdc(maxpar)
  real ( kind = 8 ) dpdyn(maxnp)
  real ( kind = 8 ) dudyn(maxnp)
  real ( kind = 8 ) dvdyn(maxnp)
  real ( kind = 8 ) dydpn(maxnp,maxparb)
  real ( kind = 8 ) epsdif
  character ( len = 2 ) eqn(maxeqn)
  real ( kind = 8 ) etan(6)
  character ( len = 30 ) fileg
  character ( len = 30 ) filet
  real ( kind = 8 ) g(maxeqn)
  real ( kind = 8 ) g1(maxeqn)
  real ( kind = 8 ) g2(maxeqn)
  real ( kind = 8 ) g3(maxeqn)
  real ( kind = 8 ) gdif(maxeqn,maxpar)
  real ( kind = 8 ) gold(maxeqn)
  real ( kind = 8 ) gopt(maxpar)
  real ( kind = 8 ) gradf(maxeqn,maxpar)
  real ( kind = 8 ) gtar(maxeqn)
  integer i
  integer ibc
  integer ifscan
  integer ifstar
  integer ibscan
  integer ibstar
  integer ibump
  integer idfd
  integer ids
  integer ierror
  integer ifds
  integer igrad
  integer igunit
  integer ijac
  integer indx(3,maxnp)
  integer iopt(maxpar)
  integer iplot
  integer isotri(maxelm)
  integer istep1
  integer istep2
  integer itar
  integer itunit
  integer itype
  integer ivopt(liv)
  integer iwrite
  integer j
  integer jjac
  integer jstep1
  integer jstep2
  logical lval
  integer maxnew
  integer maxstp
  integer node(6,maxelm)
  integer nopt
  integer npar
  integer nparb
  integer nparf
  integer npe
  integer nquad
  integer nstep3
  integer nx
  integer ny
  real ( kind = 8 ) para1(maxpar)
  real ( kind = 8 ) para2(maxpar)
  real ( kind = 8 ) para3(maxpar)
  real ( kind = 8 ) partar(maxpar)
  real ( kind = 8 ) sens(maxeqn,maxpar)
  character ( len = 20 ) syseqn
  real ( kind = 8 ) tolnew
  real ( kind = 8 ) tolopt
  real ( kind = 8 ) vopt(lv)
  real ( kind = 8 ) wateb
  real ( kind = 8 ) wateb1
  real ( kind = 8 ) wateb2
  real ( kind = 8 ) watep
  real ( kind = 8 ) wateu
  real ( kind = 8 ) watev
  real ( kind = 8 ) xblcan
  real ( kind = 8 ) xbltar
  real ( kind = 8 ) xbrcan
  real ( kind = 8 ) xbrtar
  real ( kind = 8 ) xprof
  real ( kind = 8 ) xsin(6)
  real ( kind = 8 ) yblcan
  real ( kind = 8 ) ybltar
  real ( kind = 8 ) ybrcan
  real ( kind = 8 ) ybrtar
!
!  Initialize the logical flags.
!
  lval = .false.
  call lmemry('init','nothing',lval)
!
!  Add the names of individual logical flags.
!
  lval = .false.
  call lmemry('name','fd_grad',lval)
  call lmemry('name','have_fp',lval)
  call lmemry('name','target',lval)
!
!  Assign initial values to the flags.
!
  lval = .false.
  call lmemry('set','fd_grad',lval)
  lval = .false.
  call lmemry('set','have_fp',lval)
  lval = .false.
  call lmemry('set','target',lval)
!
!  Set plain old variables.
!
  area(1:maxquad,1:maxelm) = 0.0D+00
  cost = 0.0D+00
  costar = 0.0D+00
  costb = 0.0D+00
  costp = 0.0D+00
  costu = 0.0D+00
  costv = 0.0D+00
  dopt(1:maxpar) = 1.0D+00

  do i = 1,maxpar
    dpara3(i) = 0.0D+00
    dparfd(i) = 0.0D+00
    dparfdc(i) = 0.0D+00
    dparsn(i) = 0.0D+00
  end do

  do i = 1,maxnp
    dpdyn(i) = 0.0D+00
    dudyn(i) = 0.0D+00
    dvdyn(i) = 0.0D+00
  end do

  do i = 1,maxnp
    do j = 1,maxparb
      dydpn(i,j) = 0.0D+00
    end do
  end do

  epsdif = 2.0E-07
  eqn(1:maxeqn) = '??'
  etan(1) = 0.0D+00
  etan(2) = 1.0D+00
  etan(3) = 0.0D+00
  etan(4) = 0.5
  etan(5) = 0.5
  etan(6) = 0.0D+00
  fileg = 'display.dat'
  filet = 'march.txt'
  g(1:maxeqn) = 0.0D+00
  g1(1:maxeqn) = 0.0D+00
  g2(1:maxeqn) = 0.0D+00
  g3(1:maxeqn) = 0.0D+00
  gold(1:maxeqn) = 0.0D+00
  gopt(1:maxpar) = 0.0D+00

  do i = 1,maxeqn
    do j = 1,maxpar
      gdif(i,j) = 0.0D+00
    end do
  end do

  gradf(1:maxeqn,1:maxpar) = 0.0D+00
  gtar(1:maxeqn) = 0.0D+00
  ibc = 0
  ifscan = 2
  ifstar = 2
  ibscan = 2
  ibstar = 2
  ibump = 2
  idfd = 1
  ids = 1
  ierror = 0
  ifds = 1
  igrad = 1
  igunit = 0
  ijac = 1
  indx(1:3,1:maxnp) = 0
  iopt(1:maxpar) = 0

  iplot = 0
  do i = 1,maxelm
    isotri(i) = 0
  end do
  istep1 = 1
  istep2 = 1
  itar = 0
  itunit = 0
  itype = 3
  ivopt(1:liv) = 0
  iwrite = 0
  jjac = 1
  jstep1 = 1
  jstep2 = 1
  maxnew = 10
  maxstp = 10

  do i = 1,6
    do j = 1,maxelm
      node(i,j) = 0
    end do
  end do

  nopt = 0
  npar = 1
  nparb = 0
  nparf = 0
  npe = 6
  nquad = 3
  nstep3 = 0
  nx = 11
  ny = 4
  para1(1:maxpar) = 0.0D+00
  para2(1:maxpar) = 0.0D+00
  para3(1:maxpar) = 0.0D+00
  partar(1:maxpar) = 0.0D+00
  sens(1:maxeqn,1:maxpar) = 0.0D+00
  syseqn = 'NavierStokes'
  tolnew = 0.0000000001D+00
  tolopt = 0.00000000001D+00
  vopt(1:lv) = 0.0D+00
  wateb = 1.0D+00
  wateb1 = 0.0D+00
  wateb2 = 1.0D+00
  watep = 1.0D+00
  wateu = 1.0D+00
  watev = 1.0D+00
  xblcan = 1.0D+00
  xbltar = 1.0D+00
  xbrcan = 3.0D+00
  xbrtar = 3.0D+00
  xprof = 3.0D+00
  xsin(1) = 0.0D+00
  xsin(2) = 1.0D+00
  xsin(3) = 1.0D+00
  xsin(4) = 0.5D+00
  xsin(5) = 1.0D+00
  xsin(6) = 0.5D+00
  yblcan = 0.0D+00
  ybltar = 0.0D+00
  ybrcan = 0.0D+00
  ybrtar = 0.0D+00

  return
end
subroutine input(epsdif,fileg,filet,ibc,ifscan,ifstar,ibscan,ibstar,ibump, &
  idfd,ids,ierror,ifds,igrad,ijac,iopt,iplot,istep1,istep2,itar,itype,iwrite, &
  jjac,jstep1,jstep2,maxnew,maxpar,maxstp,npar,nparb,nparf,nquad,nstep3,nx,ny, &
  para1,para2,para3,partar,syseqn,tolnew,tolopt,wateb,wateb1,wateb2,watep, &
  wateu,watev,xblcan,xbltar,xbrcan,xbrtar,xprof,yblcan,ybltar,ybrcan,ybrtar)
!
!*******************************************************************************
!
!! INPUT takes symbolic commands of the form "name = value" and
!  carries them out, assigning values to program variables.
!
!
!  For information on the meaning and legal values of the variables,
!  please refer to the glossary!
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
  integer maxpar
!
  real ( kind = 8 ) epsdif
  character ( len = 30 ) fileg
  character ( len = 30 ) filet
  integer ibc
  integer ifscan
  integer ifstar
  integer ibscan
  integer ibstar
  integer ibump
  integer idfd
  integer ids
  integer ierror
  integer ifds
  integer igrad
  integer ijac
  integer iopt(maxpar)
  integer iplot
  integer istep1
  integer istep2
  integer itar
  integer itype
  integer ival
  integer iwrite
  integer jjac
  integer jstep1
  integer jstep2
  integer lchar
  integer lchar2
  logical s_eqi
  integer maxnew
  integer maxstp
  character ( len = 80 ) name
  integer npar
  integer nparb
  integer nparf
  integer nquad
  integer nrec
  integer nstep3
  integer nx
  integer ny
  real ( kind = 8 ) para1(maxpar)
  real ( kind = 8 ) para2(maxpar)
  real ( kind = 8 ) para3(maxpar)
  real ( kind = 8 ) partar(maxpar)
  character ( len = 80 ) rhs
  character ( len = 20 ) syseqn
  real ( kind = 8 ) tolnew
  real ( kind = 8 ) tolopt
  real ( kind = 8 ) value
  real ( kind = 8 ) wateb
  real ( kind = 8 ) wateb1
  real ( kind = 8 ) wateb2
  real ( kind = 8 ) watep
  real ( kind = 8 ) wateu
  real ( kind = 8 ) watev
  real ( kind = 8 ) xblcan
  real ( kind = 8 ) xbltar
  real ( kind = 8 ) xbrcan
  real ( kind = 8 ) xbrtar
  real ( kind = 8 ) xprof
  real ( kind = 8 ) yblcan
  real ( kind = 8 ) ybltar
  real ( kind = 8 ) ybrcan
  real ( kind = 8 ) ybrtar
!
  write ( *, * ) ' '
  write ( *, * ) 'Input - Note:'
  write ( *, * ) '  Reading user input file.'
  write ( *, * ) ' '

  nrec = 0
!
!  Read the next line of input.
!
10    continue

  call namels(name,ierror,rhs,value)

  lchar = len_trim ( name )

  if ( ierror == 0 ) then
!
!  Echo the input line.  If the input quantity had an "integer"
!  name, then print it as an integer.
!
    if ( s_eqi(name(1:4),'file') ) then
      lchar2 = len_trim ( rhs )
      write(*,'(a,'' = '',a)')name(1:lchar),rhs(1:lchar2)
    else if (s_eqi(name(1:6),'syseqn') ) then
      lchar2 = len_trim ( rhs )
      write(*,'(a,'' = '',a)')name(1:lchar),rhs(1:lchar2)
    else if ((lge(name(1:1),'I').and.lle(name(1:1),'N')).or. &
      (lge(name(1:1),'i').and.lle(name(1:1),'n')) ) then
      write(*,'(a,'' = '',i14)')name(1:lchar),int(value)
    else
      write(*,'(a,'' = '',g14.6)')name(1:lchar),value
    end if

    if ( s_eqi(name,'epsdif') ) then
      epsdif = value
    else if (s_eqi(name,'fileg') ) then
      fileg = rhs(1:30)
    else if (s_eqi(name,'filet') ) then
      filet = rhs(1:30)
    else if (s_eqi(name,'ibc') ) then
      ibc = int(value)
    else if (s_eqi(name,'ibump') ) then
      ibump = int(value)
    else if (s_eqi(name,'idfd') ) then
      idfd = int(value)
    else if (s_eqi(name,'ids') ) then
      ids = int(value)
    else if (s_eqi(name,'ifds') ) then
      ifds = int(value)
    else if (s_eqi(name,'igrad') ) then
      igrad = int(value)
    else if (s_eqi(name,'ijac') ) then
      ijac = int(value)
    else if (s_eqi(name(1:5),'iopt(') ) then

      call chrcti(name(6:),ival,ierror,lchar)

      if ( ierror/= 0 ) then
        write ( *, * ) ' '
        write ( *, * ) 'Input - Fatal error!'
        write ( *, * ) '  ChrCTI returned nonzero error flag!'
        stop
      end if

      if ( ival<1.or.ival>maxpar ) then
        write ( *, * ) ' '
        write ( *, * ) 'Input - Fatal error!'
        write ( *, * ) '  Index of IOPT is out of bounds!'
        stop
      end if

      iopt(ival) = int(value)

    else if (s_eqi(name,'iplot') ) then
      iplot = int(value)
    else if (s_eqi(name,'ishapb') ) then
      ibscan = int(value)
    else if (s_eqi(name,'ishapbt') ) then
      ibstar = int(value)
    else if (s_eqi(name,'ishapf') ) then
      ifscan = int(value)
    else if (s_eqi(name,'ishapft') ) then
      ifstar = int(value)
    else if (s_eqi(name,'istep1') ) then
      istep1 = int(value)
    else if (s_eqi(name,'istep2') ) then
      istep2 = int(value)
    else if (s_eqi(name,'itar') ) then
      itar = int(value)
    else if (s_eqi(name,'itype') ) then
      itype = int(value)
    else if (s_eqi(name,'iwrite') ) then
      iwrite = int(value)
    else if (s_eqi(name,'jjac') ) then
      jjac = int(value)
    else if (s_eqi(name,'jstep1') ) then
      jstep1 = int(value)
    else if (s_eqi(name,'jstep2') ) then
      jstep2 = int(value)
    else if (s_eqi(name,'maxnew') ) then
      maxnew = int(value)
    else if (s_eqi(name,'maxstp') ) then
      maxstp = int(value)
    else if (s_eqi(name,'nparb') ) then
      nparb = int(value)
      npar = nparf+nparb+1
    else if (s_eqi(name,'nparf') ) then
      nparf = int(value)
      npar = nparf+nparb+1
    else if (s_eqi(name,'nquad') ) then
      nquad = int(value)
      if ( nquad/= 3.and.nquad/=7 ) then
        write ( *, * ) ' '
        write ( *, * ) 'Input - Fatal error!'
        write ( *, * ) '  NQUAD must be 3 or 7.'
        write ( *, * ) '  Your value was ',nquad
        stop
      end if
    else if (s_eqi(name,'nstep3') ) then
      nstep3 = int(value)
    else if (s_eqi(name,'nx') ) then
      nx = int(value)
    else if (s_eqi(name,'ny') ) then
      ny = int(value)
    else if (s_eqi(name(1:6),'para1(') ) then

      call chrcti(name(7:),ival,ierror,lchar)

      if ( ierror /= 0 ) then
        write ( *, * ) ' '
        write ( *, * ) 'Input - Fatal error!'
        write ( *, * ) '  ChrCTI returned nonzero error flag!'
        stop
      end if

      if ( ival<1.or.ival>maxpar ) then
        write ( *, * ) ' '
        write ( *, * ) 'Input - Fatal error!'
        write ( *, * ) '  Index of PARA1 is out of bounds!'
        stop
      end if

      para1(ival) = value

    else if (s_eqi(name(1:6),'para2(') ) then

      call chrcti(name(7:),ival,ierror,lchar)

      if ( ierror/= 0 ) then
        write ( *, * ) ' '
        write ( *, * ) 'Input - Fatal error!'
        write ( *, * ) '  ChrCTI returned nonzero error flag!'
        stop
      end if

      if ( ival<1.or.ival>maxpar ) then
        write ( *, * ) ' '
        write ( *, * ) 'Input - Fatal error!'
        write ( *, * ) '  Index of PARA2 is out of bounds!'
        stop
      end if

      para2(ival) = value

    else if (s_eqi(name(1:6),'para3(') ) then

      call chrcti(name(7:),ival,ierror,lchar)

      if ( ierror/= 0 ) then
        write ( *, * ) ' '
        write ( *, * ) 'Input - Fatal error!'
        write ( *, * ) '  ChrCTI returned nonzero error flag!'
        stop
      end if

      if ( ival<1.or.ival>maxpar ) then
        write ( *, * ) ' '
        write ( *, * ) 'Input - Fatal error!'
        write ( *, * ) '  Index of PARA3 is out of bounds!'
        stop
      end if

      para3(ival) = value

    else if (s_eqi(name(1:7),'partar(') ) then

      call chrcti(name(8:),ival,ierror,lchar)

      if ( ierror/= 0 ) then
        write ( *, * ) ' '
        write ( *, * ) 'Input - Fatal error!'
        write ( *, * ) '  ChrCTI returned nonzero error flag!'
        stop
      end if

      if ( ival<1.or.ival>maxpar ) then
        write ( *, * ) ' '
        write ( *, * ) 'Input - Fatal error!'
        write ( *, * ) '  Index of PARTAR is out of bounds!'
        stop
      end if

      partar(ival) = value

    else if (s_eqi(name,'syseqn') ) then
      syseqn = rhs(1:20)
    else if (s_eqi(name,'tolnew') ) then
      tolnew = value
    else if (s_eqi(name,'tolopt') ) then
      tolopt = value
    else if (s_eqi(name,'wateu') ) then
      wateu = value
    else if (s_eqi(name,'watev') ) then
      watev = value
    else if (s_eqi(name,'watep') ) then
      watep = value
    else if (s_eqi(name,'wateb') ) then
      wateb = value
    else if (s_eqi(name,'wateb1') ) then
      wateb1 = value
    else if (s_eqi(name,'wateb2') ) then
      wateb2 = value
    else if (s_eqi(name,'xbleft') ) then
      xblcan = value
    else if (s_eqi(name,'xbltar') ) then
      xbltar = value
    else if (s_eqi(name,'xbrite') ) then
      xbrcan = value
    else if (s_eqi(name,'xbrtar') ) then
      xbrtar = value
    else if (s_eqi(name,'xprof') ) then
      xprof = value
    else if (s_eqi(name,'ybleft') ) then
      yblcan = value
    else if (s_eqi(name,'ybltar') ) then
      ybltar = value
    else if (s_eqi(name,'ybrite') ) then
      ybrcan = value
    else if (s_eqi(name,'ybrtar') ) then
      ybrtar = value
!
!  Unknown name.
!
    else
      write ( *, * ) ' '
      write ( *, * ) 'Input - Unknown variable!'
      write ( *, * ) '  Variable name = '//name(1:lchar)
      write ( *, * ) '  Assigned value = ',value
      write ( *, * ) ' '
    end if
!
!  IERROR = 2, possible "STOP" or "GO" statement.
!
  else if (ierror == 2 ) then
    if ( s_eqi(name,'go') ) then
      write ( *, * ) ' '
      write ( *, * ) 'GO command!'
      ierror = 0
      return
    else if (s_eqi(name,'stop') ) then
      ierror = 1
      write ( *, * ) 'STOP command!'
      return
    else
      write ( *, * ) ' '
      write ( *, * ) 'Input - Fatal error!'
      write ( *, * ) '  NameLS error of type ',ierror
      write ( *, * ) '  Card follows:'
      write(*,'(a)')name
      write ( *, * ) ' '
      stop
    end if
!
!  IERROR = 1, blank line.
!
  else if (ierror == 1 ) then
    write ( *, * ) ' '
!
!  IERROR = 3, or 4, miscellaneous error.
!
  else if (ierror ==3.or.ierror==4 ) then
    write ( *, * ) ' '
    write ( *, * ) 'Input - Warning!'
    write ( *, * ) '  NameLS error of type ',ierror
    write ( *, * ) ' '
!
!  IERROR = 6, comment.
!
  else if (ierror ==6 ) then
    write(*,'(a)')name(1:lchar)
!
!  IERROR = 5, hard end of input.
!
  else if (ierror ==5 ) then
    write ( *, * ) ' '
    write ( *, * ) 'Input - Warning!'
    write ( *, * ) '  "Hard" end of input.'
    write ( *, * ) '  A total of ',nrec,' records were read.'
    write ( *, * ) ' '
    return
!
!  IERROR = 7, "soft" end of input.
!
  else if (ierror ==7 ) then
    write ( *, * ) ' '
    write ( *, * ) 'Input - Warning!'
    write ( *, * ) '  "Soft" end of input.'
    write ( *, * ) '  A total of ',nrec,' records were read.'
    return
!
!  Unrecognized error.
!
  else
    write ( *, * ) ' '
    write ( *, * ) 'Input - Fatal error!'
    write ( *, * ) '  Unrecognized error from NameLS.'
    write ( *, * ) '  IERROR = ',ierror
    write ( *, * ) '  Forcing a STOP!'
    stop
  end if

  nrec = nrec+1
  go to 10
end
subroutine interv(xt,lxt,x,left,mflag)
!
!*******************************************************************************
!
!! INTERV computes LEFT, the maximum value of I so that
!
!    1 <=  I <= XT
!
!  and
!
!    XT(I) <=  X.
!
!  The routine is designed to be efficient in the common situation
!  that it is called repeatedly, with X taken from an increasing
!  or decreasing sequence.
!
!  This will happen when a piecewise polynomial is to be graphed.
!  The first guess for LEFT is therefore taken to be the value
!  returned at the previous call and stored in the local variable
!  ILO.
!
!  A first check ascertains that ILO < LXT.  This is necessary
!  since the present call may have nothing to do with the previous
!  call.  Then, if XT(ILO) <=  X < XT(ILO+1), we set LEFT=ILO
!  and are done after just three comparisons.
!
!  Otherwise, we repeatedly double the difference ISTEP = IHI-ILO
!  while also moving ILO and IHI in the direction of X, until
!    XT(ILO) <=  X < XT(IHI)
!  after which we use bisection to get, in addition, ILO+1 = IHI.
!  LEFT = ILO is then returned.
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
!    FORTRAN90 version by John Burkardt
!
!  Parameters:
!
!  XT     Input, real XT(LXT), a nondecreasing sequence of
!         values.
!
!  LXT    Input, integer LXT, the dimension of XT.
!
!  X      Input, real X, the point whose location with
!         respect to the sequence XT is to be determined.
!
!  LEFT,
!  MFLAG  Output, integer LEFT, integer MFLAG, whose value is
!
!         1     -1      if               X <  XT(1)
!         I      0      if   XT(I)  <=  X < XT(I+1)
!         LXT    1      if  XT(LXT) <=  X
!
!        In particular, MFLAG = 0 is the 'usual' case.  MFLAG/=0
!        indicates that X lies outside the half open interval
!        XT(1)<= Y<XT(LXT).  The asymmetric treatment of the
!        interval is due to the decision to make all piecewise
!        polynomials continuous from the right.
!
  integer lxt
!
  integer left
  integer mflag
  integer ihi
  integer, save :: ilo = 1
  integer istep
  integer middle
  real ( kind = 8 ) x
  real ( kind = 8 ) xt(lxt)
!
  ihi = ilo+1
!
  if ( ihi>= lxt ) then

    if ( x>= xt(lxt))go to 110

    if ( lxt<= 1 ) then
      mflag = -1
      left = 1
      return
    end if

    ilo = lxt-1
    ihi = lxt

  end if
!
  if (x >=  xt(ihi))go to 40

  if ( x>= xt(ilo) ) then
    mflag = 0
    left = ilo
    return
  end if
!
!  Now X < XT(ILO).  Decrease ILO to capture X.
!
  istep = 1

   31 continue

  ihi = ilo
  ilo = ihi - istep

  if ( ilo > 1 ) then
    if (x >=  xt(ilo))go to 50
    istep = istep*2
    go to 31
  end if

  ilo = 1

  if ( x<xt(1) ) then
    mflag = -1
    left = 1
    return
  end if

  go to 50
!
!  Now X  = > XT(IHI).  Increase IHI to capture X.
!
   40 continue

  istep = 1

   41 continue

  ilo = ihi
  ihi = ilo + istep

  if ( ihi<lxt ) then
    if ( x<xt(ihi))go to 50
    istep = istep*2
    go to 41
  end if

  if (x >=  xt(lxt))go to 110

  ihi = lxt
!
!  Now XT(ILO) <=  X < XT(IHI).  Narrow the interval.
!
   50 continue

  middle = (ilo + ihi)/2

  if ( middle ==ilo ) then
    mflag = 0
    left = ilo
    return
  end if
!
!  It is assumed that MIDDLE = ILO in case IHI = ILO+1.
!
  if ( x>= xt(middle) ) then
    ilo = middle
  else
    ihi = middle
  end if

  go to 50
!
!  Set output and return.
!
  110 continue

  mflag = 1

  if ( x ==xt(lxt) ) then
    mflag = 0
  end if

  do left = lxt,1,-1
    if ( xt(left)<xt(lxt))return
  end do

  return
end
function isamax(n,dx,incx)
!
!*******************************************************************************
!
!! ISAMAX FINDS THE INDEX OF ELEMENT HAVING MAX. ABSOLUTE VALUE.
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
  real ( kind = 8 ) dmax
  real ( kind = 8 ) dx(*)
  integer i
  integer isamax
  integer incx
  integer ix
  integer n
!
  isamax = 0

  if ( n < 1 ) return

  isamax = 1
  if ( n == 1)return

  if ( incx == 1)go to 20
!
  ix = 1
  dmax = abs(dx(1))
  ix = ix + incx

  do i = 2,n

    if ( abs(dx(ix))>dmax ) then
      isamax = i
      dmax = abs(dx(ix))
      ix = ix + incx
    end if

  end do

  return
!
   20 dmax = abs(dx(1))

  do i = 2,n
    if ( abs(dx(i))>dmax ) then
      isamax = i
      dmax = abs(dx(i))
    end if
  end do

  return
end
subroutine lbase(ival,npol,pval,xpol,xval)
!
!*******************************************************************************
!
!! LBASE evalualates the IVAL-th Lagrange polynomial.
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
!  IVAL   Input, integer IVAL, the polynomial to evaluate.
!         IVAL should be between 1 and NPOL.
!
!  NPOL   Input, integer NPOL, the number of points that define
!         the Lagrange polynomials.
!
!  PVAL   Output, real PVAL, the value of the IVAL-th
!         Lagrange polynomial at the point XVAL.
!
!  XPOL   Input, real XPOL(NPOL), the abscissas of the
!         Lagrange polynomials.  The entries in XPOL should be
!         distinct.
!
!  XVAL   Input, real XVAL, the point at which the
!         IVAL-th Lagrange polynomial is to be evaluated.
!
!
  integer npol
!
  integer i
  integer ival
  real ( kind = 8 ) pval
  real ( kind = 8 ) xpol(npol)
  real ( kind = 8 ) xval
!
  pval = 1.0D+00
  do i = 1,npol
    if ( i/= ival ) then
      pval = pval*(xval-xpol(i))/(xpol(ival)-xpol(i))
    end if
  end do

  return
end
subroutine lmemry(action,name,lval)
!
!*******************************************************************************
!
!! LMEMRY allows the user to define the name of a logical variable,
!  set it, increment it, or get the value.
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
!  ACTION Input, Character*(*) ACTION, desired action.
!
!         'Init', reset all values to FALSE, wipe out all names.
!         'Name', add a variable of the given name.
!         'Not',  replace value of NAME by NOT.NAME.
!         'Set',  set variable NAME to LVAL.
!         'Get',  return value of NAME in LVAL.
!
!  NAME   Input, Character*(*) NAME, the name of the variable.
!
!  LVAL   Input/output, Logical LVAL.
!
!         For the 'Set' command, LVAL must contain the set value.
!
!         For the 'Get' command, LVAL will contain the value of the
!         named variable on output.
!
  integer, parameter :: maxnam = 100
!
  character ( len = * )  action
  integer i
  integer lenc
  logical lval
  logical, save, dimension ( maxnam ) :: lvals
  logical s_eqi
  character ( len = * )  name
  character ( len = 20 ) names(maxnam)
  integer numnam
!
  save names
  save numnam
!
  lenc = len_trim ( name )
!
!  Initialize everything.
!
  if ( s_eqi(action,'init') ) then

    numnam = 0

    do i = 1,maxnam
      lvals(i) = .false.
      names(i) = ' '
    end do
!
!  Name something.
!
  else if (s_eqi(action,'name') ) then

    do i = 1,numnam

      if ( s_eqi(name,names(i)) ) then
        write ( *, * ) ' '
        write ( *, * ) 'LMemry - Warning!'
        write(*,'(''  There is ALREADY a variable '',a)') name(1:lenc)
        return
      end if

    end do

    if ( numnam<maxnam ) then
      numnam = numnam+1
      names(numnam) = name
    else
      write ( *, * ) ' '
      write ( *, * ) 'LMemry - Fatal error!'
      write ( *, * ) '  No more name space.'
      stop
    end if
!
!  Switch something.
!
  else if (s_eqi(action,'not') ) then

    do i = 1,numnam

      if ( s_eqi(name,names(i)) ) then
        lvals(i) = .not.lvals(i)
        return
      end if

    end do

    write ( *, * ) ' '
    write ( *, * ) 'LMemry - Fatal error!'
    write ( *, * ) '  Attempt to NOT unknown variable.'
    write(*,'(''  Variable name is '',a)')name(1:lenc)
    stop
!
!  Set something.
!
  else if (s_eqi(action,'set') ) then

    do i = 1,numnam

      if ( s_eqi(name,names(i)) ) then
        lvals(i) = lval
        return
      end if

    end do

    write ( *, * ) ' '
    write ( *, * ) 'LMemry - Fatal error!'
    write ( *, * ) '  Attempt to set unknown variable.'
    write(*,'(''  Variable name is '',a)')name(1:lenc)
    stop
!
!  Get something.
!
  else if (s_eqi(action,'get') ) then

    do i = 1,numnam

      if ( s_eqi(name,names(i)) ) then
        lval = lvals(i)
        return
      end if

    end do

    write ( *, * ) ' '
    write ( *, * ) 'LMemry - Fatal error!'
    write ( *, * ) '  Attempt to get value of unknown variable.'
    write(*,'(''  Variable name is '',a)')name(1:lenc)
    stop
  else
    write ( *, * ) ' '
    write ( *, * ) 'LMemry - Fatal error!'
    write ( *, * ) '  Unrecognized action.'
    write ( *, '(a)' ) action
    stop
  end if

  return
end
function lsame(ca,cb)
!
!*******************************************************************************
!
!! LSAME returns .TRUE. if CA is the same letter as CB regardless of
!  case.
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
!
!  CA      (input) CHARACTER*1
!  CB      (input) CHARACTER*1
!          CA and CB specify the single characters to be compared.
!
  character          ca, cb
  logical lsame
  intrinsic          ichar
!     ..
!     .. Local Scalars ..
  integer            inta, intb, zcode
!     ..
!     .. Executable Statements ..
!
!     Test if the characters are equal
!
  lsame = ca==cb
  if ( lsame ) return
!
!     Now test for equivalence if both characters are alphabetic.
!
  zcode = ichar( 'z' )
!
!     Use 'Z' rather than 'A' so that ASCII can be detected on Prime
!     machines, on which ICHAR returns a value with bit 8 set.
!     ICHAR('A') on Prime machines returns 193 which is the same as
!     ICHAR('A') on an EBCDIC machine.
!
  inta = ichar( ca )
  intb = ichar( cb )
!
  if ( zcode ==90 .or. zcode== 122 ) then
!
!        ASCII is assumed - ZCODE is the ASCII code of either lower or
!        upper case 'Z'.
!
     if ( inta>= 97 .and. inta<=122 ) inta = inta - 32
     if ( intb>= 97 .and. intb<=122 ) intb = intb - 32
!
  else if ( zcode == 233 .or. zcode== 169 ) then
!
!        EBCDIC is assumed - ZCODE is the EBCDIC code of either lower or
!        upper case 'Z'.
!
     if ( inta>= 129 .and. inta<=137 .or. &
           inta>= 145 .and. inta<=153 .or. &
            inta>= 162 .and. inta<=169 ) inta = inta + 64
     if ( intb>= 129 .and. intb<=137 .or. &
            intb>= 145 .and. intb<=153 .or. &
           intb>= 162 .and. intb<=169 ) intb = intb + 64
!
  else if ( zcode == 218 .or. zcode==250 ) then
!
!        ASCII is assumed, on Prime machines - ZCODE is the ASCII code
!        plus 128 of either lower or upper case 'Z'.
!
     if ( inta>= 225 .and. inta<=250 ) inta = inta - 32
     if ( intb >=  225 .and. intb <= 250 ) intb = intb - 32
  end if

  lsame = ( inta == intb )

  return
end
subroutine lspoly(coef,ncoef,x,y,val)
!
!*******************************************************************************
!
!! LSPOLY evaluates a polynomial in (1, x, y, x*x, xy, y*y) at a given point (x,y).
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
  integer ncoef
!
  real ( kind = 8 ) coef(ncoef)
  real ( kind = 8 ) val
  real ( kind = 8 ) x
  real ( kind = 8 ) y
!
  val = coef(1) + coef(2)*x + coef(3)*y+ coef(4)*x*x + coef(5)*x*y + coef(6)*y*y

  return
end
subroutine march(a,area,base,dir,dpara3,dparsn,dparfd,dparfdc,dpdyn,dudyn, &
  dvdyn,dydpn,epsdif,eqn,etan,etaq,g,g1,g2,g3,gdif,gdifc,gold,gradf,gtar,ibc, &
  ifs,ibs,idfd,ids,ifds,igunit,ijac,indx,iopt,ipivot,iplot,isotri,istep1, &
  istep2,itunit,iwrite,jjac,jstep1,jstep2,maxeqn,maxnew,maxstp,ndim,nelem, &
  neqn,nlband,node,np,npar,nparb,nparf,npe,nprof,nquad,nrow,nstep3,numel,nx, &
  ny,para,para1,para2,para3,phi,res,sens,splbmp,splflo,syseqn,taubmp, &
  tauflo,tolnew,wateb,wateb1,wateb2,watep,wateu,watev,wquad,xbl,xbr,xc,xprof, &
  xquad,xsin,xsiq,ybl,ybr,yc,yquad)
!
!*******************************************************************************
!
!! MARCH carries out a one, two or three dimensional "march".
!
!  In a one dimensional march:
!
!    Step:                      ParNew
!
!    IStep1                     Para1
!    IStep2                     Para2
!
!    Para(IStep) = ( (IStep2-IStep)*Para1 + (IStep-IStep1)*Para2)
!                  / (IStep2-IStep1)
!
!  In a two dimensional march:
!
!    Step:                      ParNew
!
!    IStep1, JStep1             Para1
!    IStep2, JStep2             Para3
!
!    Para(Istep,Jstep) = Para1 + (IStep-IStep1)*Dir1
!    + (J (JStep-JStep1)*Dir2
!
!  The vector Para2 is used to determine the orthogonal directions Dir1
!  and Dir2.  Dir1 is the direction from Para1 to Para2.  Dir2 is the
!  vector which is normal to Dir1 and passes through Para3.
!
!  In a three dimensional march:
!
!    Step:                      ParNew    WateB
!
!    IStep1, JStep1, 1          Para1     WateB1
!    IStep2, JStep2, 1          Para3     WateB1
!
!    IStep1, JStep1, NStep3     Para1     WateB2
!    IStep2, JStep2, NStep3     Para3     WateB2
!
!    Para(IStep,JStep.KStep)  = Para1 + (IStep-IStep1)*Dir1
!                                     + (JStep-JStep1)*Dir2
!    WateB(IStep,JStep,KStep) = ((NStep3-KStep)*WateB1+(KStep-1)*WateB2)
!                               / (NStep3-1)
!
!  The vector Para2 is used to determine the orthogonal directions Dir1
!  and Dir2.  Dir1 is the direction from Para1 to Para2.  Dir2 is the
!  vector which is normal to Dir1 and passes through Para3.
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
  integer maxeqn
  integer ndim
  integer nelem
  integer neqn
  integer np
  integer npar
  integer nparb
  integer nparf
  integer nquad
  integer nrow
  integer nx
  integer ny
!
  real ( kind = 8 ) a(nrow,neqn)
  real ( kind = 8 ) area(nquad,nelem)
  real ( kind = 8 ) base(npar,ndim)
  real ( kind = 8 ) cost
  real ( kind = 8 ) costb
  real ( kind = 8 ) costp
  real ( kind = 8 ) costu
  real ( kind = 8 ) costv
  real ( kind = 8 ) dir(npar,ndim)
  real ( kind = 8 ) dist(3)
  real ( kind = 8 ) dpara3(npar)
  real ( kind = 8 ) dparsn(npar)
  real ( kind = 8 ) dparfd(npar)
  real ( kind = 8 ) dparfdc(npar)
  real ( kind = 8 ) dpdyn(np)
  real ( kind = 8 ) dudyn(np)
  real ( kind = 8 ) dvdyn(np)
  real ( kind = 8 ) dydpn(np,nparb)
  real ( kind = 8 ) epsdif
  character ( len = 2 ) eqn(neqn)
  real ( kind = 8 ) etan(6)
  real ( kind = 8 ) etaq(nquad)
  real ( kind = 8 ) g(neqn)
  real ( kind = 8 ) g1(neqn)
  real ( kind = 8 ) g2(neqn)
  real ( kind = 8 ) g3(neqn)
  real ( kind = 8 ) gdif(maxeqn,npar)
  real ( kind = 8 ) gdifc(maxeqn,npar)
  real ( kind = 8 ) gold(neqn)
  real ( kind = 8 ) gpoptfd(3)
  real ( kind = 8 ) gpoptsn(3)
  real ( kind = 8 ) gpopt3(3)
  real ( kind = 8 ) gradf(maxeqn,npar)
  real ( kind = 8 ) gtar(neqn)
  integer i
  integer ibc
  integer ifs
  integer ibs
  integer idfd
  integer ids
  integer ierror
  integer ifds
  integer igunit
  integer ijac
  integer indx(3,np)
  integer info
  integer iopt(npar)
  integer ipar
  integer ipivot(neqn)
  integer iplot
  integer isotri(nelem)
  integer istep
  integer istep1
  integer istep2
  integer itunit
  integer iwrite
  integer j
  integer jjac
  integer jstep
  integer jstep1
  integer jstep2
  integer kstep
  logical lmat
  integer maxnew
  integer maxstp
  integer nlband
  integer node(6,nelem)
  integer npe
  integer nprof(2*ny-1)
  integer nstep1
  integer nstep2
  integer nstep3
  integer numel(np)
  integer numstp
  real ( kind = 8 ) para(npar)
  real ( kind = 8 ) para1(npar)
  real ( kind = 8 ) para2(npar)
  real ( kind = 8 ) para3(npar)
  real ( kind = 8 ) phi(nquad,6,10,nelem)
  real ( kind = 8 ) res(neqn)
  real ( kind = 8 ) sens(maxeqn,npar)
  real ( kind = 8 ) splbmp(4,nparb+2,0:nparb)
  real ( kind = 8 ) splflo(4,nparf+2,0:nparf)
  character ( len = 20 ) syseqn
  real ( kind = 8 ) taubmp(nparb+2)
  real ( kind = 8 ) tauflo(nparf+2)
  character ( len = 30 ) title
  real ( kind = 8 ) tolnew
  real ( kind = 8 ) wateb
  real ( kind = 8 ) wateb1
  real ( kind = 8 ) wateb2
  real ( kind = 8 ) watep
  real ( kind = 8 ) wateu
  real ( kind = 8 ) watev
  real ( kind = 8 ) wquad(nquad)
  real ( kind = 8 ) xbl
  real ( kind = 8 ) xbr
  real ( kind = 8 ) xc(np)
  real ( kind = 8 ) xprof
  real ( kind = 8 ) xquad(nquad,nelem)
  real ( kind = 8 ) xsin(6)
  real ( kind = 8 ) xsiq(nquad)
  real ( kind = 8 ) ybl
  real ( kind = 8 ) ybr
  real ( kind = 8 ) yc(np)
  real ( kind = 8 ) yquad(nquad,nelem)
!
  if ( maxstp<= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'MARCH - Warning!'
    write ( *, * ) '  No marching steps were requested!'
    return
  end if

  write ( *, * ) ' '
  write ( *, * ) 'MARCH - Note:'
  write ( *, * ) '  Begin march!'

  numstp = 0

  gpoptfd(1:3) = 0.0D+00
  gpoptsn(1:3) = 0.0D+00
  gpopt3(1:3) = 0.0D+00
!
!  Set up certain marching indices and variables so that they
!  are correct no matter whether this is a 1, 2 or 3 dimensional march.
!
  if ( ndim == 1 ) then
    jstep1 = 1
    jstep2 = 1
    nstep1 = maxstp
    nstep2 = 1
    nstep3 = 1
    wateb1 = wateb
    wateb2 = wateb
  else if (ndim == 2 ) then
    nstep1 = maxstp
    nstep2 = maxstp
    nstep3 = 1
  else if (ndim ==3 ) then
    nstep1 = maxstp
    nstep2 = maxstp
    wateb1 = wateb
    wateb2 = wateb
  end if
!
  if ( itunit/= 0 ) then
    write(itunit,*)'NSTEP1 = ',nstep1
    write(itunit,*)'NSTEP2 = ',nstep2
    write(itunit,*)'NSTEP3 = ',nstep3
  end if
!
!  Compute the direction vectors for the marching plane.
!  In 3 dimensions, we only compute two direction vectors, since the
!  bump weight is assumed to be the only variable in the third
!  direction.
!
  do ipar = 1,npar

    dir(ipar,1) = para2(ipar)-para1(ipar)

    if ( ndim>= 2 ) then
      dir(ipar,2) = para3(ipar)-para1(ipar)
    end if

    if ( ndim>= 3 ) then
      dir(ipar,3) = 0.0D+00
    end if

  end do
!
!  Set up the vectors that span the marching space.
!
  do i = 1,npar

    base(i,1) = para2(i)-para1(i)

    if ( ndim>= 2 ) then
      base(i,2) = para3(i)-para1(i)
    end if

    if ( ndim>= 3 ) then
      base(i,3) = 0.0D+00
    end if

  end do
!
!  Use orthonormalization to get a marching basis.
!
  call probas(base,npar,ndim)
!
!  Compute DIST, the length of a single marching step in each direction.
!
  if ( ndim == 1 ) then

    dist(1) = 0.0D+00
    do j = 1,npar
      dist(1) = dist(1)+(para2(j)-para1(j))**2
    end do
    dist(1) = sqrt(dist(1))
    dist(1) = dist(1)/real(istep2-istep1, kind = 8 )

  else if (ndim == 2.or.ndim==3 ) then

    do j = 1,npar
      dir(j,1) = para3(j)-para1(j)
    end do

    write ( *, * ) ' '
    write ( *, * ) 'March: Para3-Para1:'
    write ( *, * ) ' '
    call prpar(nparb,nparf,dir)

    call projec(base,npar,2,dir,dist)

    dist(1) = dist(1)/real(istep2-istep1, kind = 8 )
    dist(2) = dist(2)/real(jstep2-jstep1, kind = 8 )
    write ( *, * ) ' '
    write ( *, * ) 'March: Distances = ',dist(1),dist(2)
  end if
!
!  Now do the marching.
!
  do kstep = 1,nstep3

    write(itunit,*)'KSTEP = ',kstep

    if ( nstep3>1 ) then
      wateb = (real(nstep3-kstep, kind = 8 )*wateb1+real(kstep-1, kind = 8 )*wateb2)/ real(nstep3-1, kind = 8 )
      write ( *, * ) ' '
      write ( *, * ) 'March - Value of WATEB is now ',wateb
    end if
!
!  Because of special nature of the third dimension, we
!  wait til now to start G and PARNEW at zero.
!
    do i = 1,neqn
      g(i) = 0.0D+00
    end do
!
!  Now compute MAXSTP lines of solutions, each containing
!  MAXSTP points.
!
    do jstep = 1,nstep2

      write ( *, * ) 'JSTEP = ',jstep

      do istep = 1,nstep1

        write(*,'(1x,3i6)')istep,jstep,kstep
        write(itunit,*)'ISTEP = ',istep
!
!  Copy the old solution and parameters.
!
        do i = 1,neqn
          gold(i) = g(i)
        end do
!
!  Set the current parameters, PARA.
!
        if ( ndim == 1 ) then

          do j = 1,npar
            para(j) = ( real(istep2-istep, kind = 8 )*para1(j) + real(istep-istep1, kind = 8 )*para2(j) ) &
              / real(istep2-istep1, kind = 8 )
          end do

        else

          do j = 1,npar
            para(j) = para1(j)+(istep-istep1)*dist(1)*base(j,1) &
                 +(jstep-jstep1)*dist(2)*base(j,2)
          end do

        end if
!
!  For the first step along a line of solutions, retrieve the
!  solution computed on the previous line.
!
        if ( istep == 1.and.jstep/=1 ) then
          do i = 1,neqn
            g(i) = g3(i)
          end do
        end if
!
!  Compute the flow variables G that correspond to PARNEW.
!
        call flosol(a,area,eqn,etaq,g,g2,ifs,ibs,ierror,ijac,indx,ipivot, &
          isotri,iwrite,jjac,maxnew,nelem,neqn,nlband,node,np,npar,nparb, &
          nparf,nquad,nrow,nx,ny,para,phi,res,splbmp,splflo,syseqn,taubmp, &
          tauflo,tolnew,wquad,xbl,xbr,xc,xquad,xsiq,ybl,ybr,yc,yquad)
!
!  Get the cost function J.
!
        call getcst(cost,costb,costp,costu,costv,g,gtar,ibs,indx,neqn,np, &
          nparb,nprof,ny,splbmp,taubmp,wateb,watep,wateu,watev,xbl,xbr,ybl, &
          ybr,yc)
!
!  Get the finite difference sensitivities dU/dPARA.
!
        call getgrd(a,area,cost,dpara3,epsdif,eqn,etaq,g,g1,g2,gdif,gtar, &
          ifs,ibs,ierror,ijac,indx,iopt,ipivot,isotri,iwrite,jjac,maxeqn, &
          maxnew,nelem,neqn,nlband,node,np,npar,nparb,nparf,nprof,nquad, &
          nrow,nx,ny,para,phi,res,splbmp,splflo,syseqn,taubmp,tauflo,tolnew, &
          wateb,watep,wateu,watev,wquad,xbl,xbr,xc,xquad,xsiq,ybl, &
          ybr,yc,yquad)
!
!  Get dPdY, dUdY and dVdY, compute the correction term
!  GRADF, and the GDIFC, the corrected finite difference estimate
!  of the sensitivity.
!
        call getdu(dpdyn,dudyn,dvdyn,etan,g,indx,isotri,nelem,neqn,node,np, &
          numel,xc,xsin,yc)

        call getfix(dpdyn,dudyn,dvdyn,dydpn,gradf,ibs,indx,iopt,maxeqn,np, &
          npar,nparb,nparf,splbmp,taubmp,xbl,xbr,xc,yc)

        do i = 1,neqn
          do j = 1,npar
            gdifc(i,j) = gdif(i,j)-gradf(i,j)
          end do
        end do
!
!  Get the discretized sensitivities.
!
        lmat = .false.
        call lmemry('get','have_fp',lmat)

        if ( .not.lmat ) then

          call fp(a,area,eqn,g,indx,nelem,neqn,nlband,node,np,npar,nparb, &
            nparf,nquad,nrow,para,phi,syseqn)

          call sgbtrf(neqn,neqn,nlband,nlband,a,nrow,ipivot,info)

          if ( info/= 0 ) then
            write ( *, * ) ' '
            write ( *, * ) 'March - Fatal error!'
            write ( *, * ) '  Jacobian factorization failed.'
            write ( *, * ) '  SGBTRF returns nonzero INFO = ',info
          else
            lmat = .true.
            call lmemry('set','have_fp',lmat)
          end if
        end if

        if ( lmat ) then
          call getsen(a,area,dudyn,dvdyn,eqn,g,ibc,ifs,ibs,indx,iopt,ipivot, &
            maxeqn,nelem,neqn,nlband,node,np,npar,nparb,nparf,nquad,nrow, &
            phi,sens,splbmp,splflo,taubmp,tauflo,xc,yc)
        end if
!
!  For the first step along a line of solutions, save the
!  solution for use on the next line.
!
        if ( istep == 1 ) then
          do i = 1,neqn
            g3(i) = g(i)
          end do
        end if

        if ( iwrite == 1 ) then
          write ( *, * ) 'Cost:',cost
        else if (iwrite>= 2 ) then
          title = 'Cost'
          call prcost2(cost,costb,costp,costu,costv,title,wateb,watep, &
            wateu,watev)
        end if
!
!  Print stuff.
!
        numstp = numstp+1

        call getder(dparfd,g,gtar,ibs,indx,maxeqn,neqn,np,npar,nparb,nparf, &
          nprof,ny,gdif,splbmp,taubmp,wateb,watep,wateu,watev,xbl,xbr,ybl, &
          ybr,yc)

        call getder(dparfdc,g,gtar,ibs,indx,maxeqn,neqn,np,npar,nparb,nparf, &
          nprof,ny,gdifc,splbmp,taubmp,wateb,watep,wateu,watev,xbl,xbr,ybl, &
          ybr,yc)

        call getder(dparsn,g,gtar,ibs,indx,maxeqn,neqn,np,npar,nparb,nparf, &
          nprof,ny,sens,splbmp,taubmp,wateb,watep,wateu,watev,xbl,xbr,ybl, &
          ybr,yc)

        call prsol(dpara3,dparfd,dparfdc,dparsn,g,g2,gdif,gdifc,idfd,ids,ifds, &
          indx,iwrite,maxeqn,neqn,np,npar,nparb,nparf,numstp,para,sens)

        if ( itunit/= 0 ) then
          call projec(base,npar,ndim,dpara3,gpopt3)
          call projec(base,npar,ndim,dparfd,gpoptfd)
          call projec(base,npar,ndim,dparsn,gpoptsn)
          write(itunit,*)'COST = ',cost
          write(itunit,'(1x,6g14.6)')(para(i),i = 1,npar)
          write(itunit,'(1x,5g14.6)')(gpoptsn(i),i = 1,ndim)
          write(itunit,'(1x,5g14.6)')(gpoptfd(i),i = 1,ndim)
          write(itunit,'(1x,5g14.6)')(gpopt3(i),i = 1,ndim)
        end if
!
!  Shall we save DISPLAY graphics information to a file?
!
        if ( iplot<0 ) then
          if ( (istep ==istep1.and.jstep==jstep1.and.kstep== 1).or. &
             (istep ==istep2.and.jstep==jstep2.and.kstep==nstep3) ) then

            call pltwrt(eqn,g,gdif,igunit,indx,isotri,iwrite,maxeqn,nelem, &
              neqn,node,np,npar,npe,nprof,nx,ny,para,sens,xc,xprof,yc)
          end if
        end if

      end do
    end do
  end do
!
!  The last record in a marching file that includes finite differences
!  should be the lengths of the sides.
!
  if ( itunit/= 0 ) then
    write(itunit,'(1x,5g14.6)')(dist(i),i = 1,ndim)
  end if

  return
end
subroutine maropn(filet,itunit)
!
!*******************************************************************************
!
!! MAROPN opens the marching file.
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
!  FILET  Input, character ( len = 30 ) FILET.
!
!         FILET contains the name of the file into which the marching
!         information will be stored.  If FILET = 'none', or ' ', then
!         no marching file is opened.
!
!  ITUNIT Output, integer ITUNIT.
!
!         The FORTRAN unit used for writing data to the marching file.
!
!
  character ( len = 30 ) filet
  integer itunit
  logical s_eqi
!
!  Return immediately if no marching file was requested.
!
  if ( s_eqi(filet,'none').or.filet ==' ' ) then
    itunit = 0
    return
  end if

  if ( itunit == 0 ) then

    write ( *, * ) ' '
    write ( *, * ) 'MarOpn - Note:'
    write ( *, * ) '  Opening the marching file FILET = ' // trim ( filet )
    write ( *, * ) ' '
!
!  Delete any old copy of the file.
!
    itunit = 12
    open ( unit = itunit, file=filet,status='unknown',form='formatted', &
      access = 'sequential',err=10)

    return
!
!  Write a warning if the file could not be opened.
!
10      continue

    write ( *, * ) ' '
    write ( *, * ) 'MarOpn - Warning!'
    write ( *, * ) '  The marching file could not be opened.'
    itunit = 0
!
!  Marching file is already open.
!
  else
    write ( *, * ) ' '
    write ( *, * ) 'MarOpn - Note:'
    write ( *, * ) '  The marching file is already opened.'
    write ( *, * ) '  New information will be appended.'
  end if

  return
end
subroutine namels ( name, ierror, rhs, value )
!
!*******************************************************************************
!
!! NAMELS reads a line of "namelist" data.
!
!  Discussion:
!
!    This routine reads a line of user input which is similar in form
!    to NAMELIST input, and returns the name of the variable
!    and its value.
!
!    NAMELS is a simple routine, and can only handle simple input.
!    In particular, it cannot handle:
!
!      multiple assignments on one line,
!      a single assignment extended over multiple lines,
!      assignments to character or complex variables,
!      assignments to arrays.
!
!    Typical input would be of the form:
!
!      name = value
!
!    including, for instance:
!
!      a = 1.0D+00
!      n = -17
!      scale = +5.3E-2
!
!    Spaces are ignored, and case is not important.  Integer values
!    will be returned as real, but this is never a
!    problem as long as the integers are "small".
!
!    If a line begins with the character "#", it is assumed to be
!    a comment, and is ignored.  IERROR is returned as 6.
!
!    If a line begins with the characters "end-of-input", it is
!    assumed to be an "end-of-input" marker, and IERROR is returned
!    as 7.
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
!  NAME   Output, character ( len = * )  NAME.
!
!         NAME contains the left hand side of the assignment
!         statement.
!
!         Normally, this will be the name of a variable.
!
!         If the input line was blank, then NAME will equal ' '.
!
!         If an error occurred while trying to process the
!         input line, NAME will contain the text of the line..
!
!         If the line began with "#", then NAME will contain the
!         text of the line.
!
!         If the line equals "end-of-input", then NAME will contain
!         the text of the line.
!
!  IERROR Output, integer IERROR.
!
!         0, no errors were detected.
!         1, the line was blank.
!         2, the line did not contain an " = " sign.
!         3, the line did not contain a variable name to the
!            left of the " = " sign.
!         4, the right hand side of the assignment did not make
!            sense.
!         5, end of input.
!         6, the line began with "#", signifying a comment.
!            The text of the line is returned in NAME.
!         7, the line began with "end-of-input".
!
!  VALUE  Output, real VALUE.
!
!         VALUE contains the right hand side of the assignment
!         statement.
!
!         Normally, this will be a real value.
!
!         But if the input line was blank, or if an error occurred
!         while trying to process the input line, or if input
!         terminated, then VALUE will simply be set to 0.
!
!
  integer ierror
  integer lchar
  logical s_eqi
  character ( len = 80 ) line
  character ( len = * )  name
  integer nchar
  character ( len = 80 ) rhs
  real ( kind = 8 ) value
!
!  Set default values
!
  ierror = 0
  name = ' '
  value = 0.0D+00
!
!  Read a line
!
  read(*,'(a)',end = 20)line
!
!  Empty lines are OK
!
  if ( len_trim ( line ) <= 0 ) then
    ierror = 1
    return
  end if
!
!  Check for comment.
!
  if ( line(1:1) =='#' ) then
    ierror = 6
    name = line
    return
  end if
!
!  Check for "end-of-line".
!
  if ( s_eqi(line,'end-of-input') ) then
    ierror = 7
    name = line
    return
  end if
!
!  Does the line contain an = sign?
!
  if ( index(line,' = ')<=0 ) then
    ierror = 2
    value = 0
    name = line
    return
  end if
!
!  Find the name of the variable to be assigned.
!
  call chrup2(line,name,' = ')
  call chrdb1(name)
  if ( len_trim ( name ) <= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'NameLS - Warning!'
    write ( *, * ) '  The following input line was ignored, because'
    write ( *, * ) '  there was no variable name on the left hand'
    write ( *, * ) '  side of the assignment statement:'
    write(*,'(a)')line
    write ( *, * ) ' '
    ierror = 3
    return
  end if
!
!  Read the value, as a real number.
!
  nchar = index(line,'=')

  rhs = line(nchar+1:)

  if ( s_eqi(name(1:4),'file'))return
  if ( s_eqi(name(1:6),'syseqn'))return

  call chrctd(line(nchar+1:),value,ierror,lchar)

  if ( ierror/= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'NameLS - Warning!'
    write ( *, * ) '  The following input line was ignored, because'
    write ( *, * ) '  the right hand side of the assignment statement'
    write ( *, * ) '  did not seem to make sense:'
    write(*,'(a)')line
    write ( *, * ) ' '
    ierror = 4
  end if
  return
!
!  On end of input, return.
!
20    continue
  write ( *, * ) ' '
  write ( *, * ) 'NameLS - Reached end of input.'
  write ( *, * ) ' '
  ierror = 5

  return
end
subroutine newton ( a, area, eqn, g, g2, ifs, ierror, ijac, indx, ipivot, &
  iwrite, jjac, maxnew, nelem, neqn, nlband, node, np, npar, nparb, &
  nparf, nquad, nrow, para, phi, res, splflo, syseqn, tauflo, tolnew, &
  yc )
!
!*******************************************************************************
!
!! NEWTON seeks a solution G of the nonlinear state equations.
!
!  Discussion:
!
!    The exact solution would have a zero residual, as computed by
!    the routine FX.  NEWTON uses Newton's method to seek a solution
!    whose maximum residual is no more than TOLNEW.  The routine FP
!    is used to compute the Jacobian of the residual functions.
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
  integer nelem
  integer neqn
  integer np
  integer npar
  integer nparf
  integer nquad
  integer nrow
!
  real ( kind = 8 ) a(nrow,neqn)
  real ( kind = 8 ) area(nquad,nelem)
  real ( kind = 8 ) dmax
  character ( len = 2 ) eqn(neqn)
  logical falter
  real ( kind = 8 ) g(neqn)
  real ( kind = 8 ) g2(neqn)
  integer i
  integer ifs
  integer isamax
  integer idmax
  integer ierror
  integer ijac
  integer indx(3,np)
  integer info
  integer ipivot(neqn)
  integer irmax
  integer iter
  integer iwrite
  integer ixmax
  integer jjac
  logical leval
  logical lgdif
  logical lmat
  logical ltarg
  logical lzero
  integer maxnew
  integer nlband
  integer node(6,nelem)
  integer nparb
  integer nsys
  real ( kind = 8 ) para(npar)
  real ( kind = 8 ) phi(nquad,6,10,nelem)
  real ( kind = 8 ) res(neqn)
  real ( kind = 8 ) rmax
  real ( kind = 8 ) rmax0
  real ( kind = 8 ) rmax2
  real ( kind = 8 ) splflo(4,nparf+2,0:nparf)
  character ( len = 20 ) syseqn
  real ( kind = 8 ) tauflo(nparf+2)
  real ( kind = 8 ) tolnew
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmax0
  real ( kind = 8 ) xmax2
  real ( kind = 8 ) yc(np)
!
!  Initialize the flag that notes whether we have evaluated the
!  jacobian during this process.
!
  leval = .false.
!
!  Find out if this is a finite difference calculation.
!
  lgdif = .false.
  call lmemry('get','fd_grad',lgdif)
!
!  Find out if this is a target calculation.
!
  ltarg = .false.
  call lmemry('get','target',ltarg)
!
!  Find out if we have a valid, factored, jacobian available.
!
  lmat = .false.
  call lmemry('get','have_fp',lmat)

  nsys = 1
  falter = .false.
!
!  If there is no inflow, then the answer is easy.  G = 0.
!  For some reason, the code has trouble figuring this out,
!  so make it easy for it!
!
  lzero = .true.
  do i = 1,nparf
    if ( para(i)/= 0.0)lzero=.false.
  end do

  if ( lzero ) then

    do i = 1,neqn
      g(i) = 0.0D+00
    end do

    return

  end if
!
!  Based on the user input quantities JJAC and KJAC, and
!  LGDIF, which tells us whether we are calculating a finite difference,
!  and LMAT, which tells us whether we have a valid jacobian,
!  determine the frequency with which we expect to update
!  the jacobian during this process.
!
!  We may revise this value if the process fails.
!
  if ( jjac == 0 ) then
    lmat = .false.
  else if (jjac == 1 ) then
    if ( .not.lgdif ) then
      lmat = .false.
    end if
  else if (jjac == 2 ) then
  end if
!
!  Start G2 at G.
!  Update G2, if necessary, so that it always contains the point
!  with the lowest residual.
!
  do i = 1,neqn
    g2(i) = g(i)
  end do
!
!  If the first Newton iteration failed, you may want to try again
!  by coming back here.
!
10    continue

  ierror = 0
  iter = 0
!
!  Compute the norm of the initial X value.
!
  ixmax = isamax(neqn,g,1)
  xmax = abs(g(ixmax))
  xmax0 = xmax
  xmax2 = xmax

  if ( 4 <= iwrite ) then
    write ( *, * ) ' '
    write ( *, * ) 'Newton - MaxNorm(X) =  ',xmax,' index=',ixmax
  end if
!
!  Evaluate the residual of the initial X value.
!
  call fx(area,eqn,g,ifs,indx,nelem,neqn,node,np,npar, &
    nparb,nparf,nquad,para,phi,res,splflo,syseqn,tauflo,yc)

  irmax = isamax(neqn,res,1)
  rmax = abs(res(irmax))

  rmax0 = rmax
  rmax2 = rmax

  if ( 4 <= iwrite ) then
    write ( *, * ) 'Newton - MaxNorm(FX) = ',rmax,' index=',irmax
  end if
!
!  Accept the point immediately,
!    if FX is small enough, and
!    this is NOT a finite difference point.
!
!  In this case, the jacobian will not be evaluated and factored.
!
  if ( rmax <= tolnew.and.(.not.lgdif) ) then
    if ( 3 <= iwrite ) then
      write ( *, * ) 'Newton - Iterate ',iter,' accepted immediately.'
    end if
    return
  end if
!
!  The initial X value is NOT acceptable.  We must carry out
!  Newton iteration, and attempt to improve it.
!
  do iter = 1, maxnew
!
!  If we have a valid, factored jacobian already, then we may
!  reuse it, if it's not too old, and if we're allowed.
!
    if ( lmat ) then
      if ( jjac == 0 ) then
        lmat = .false.
      else if (jjac == 1 ) then
        if ( (.not.lgdif).and.mod(iter-1,ijac) == 0 ) then
          lmat = .false.
        end if
      else if (jjac == 2 ) then
      end if
    end if
!
!  If it's time, evaluate and factor the jacobian.
!
    if ( .not.lmat ) then

      call fp(a,area,eqn,g,indx,nelem,neqn,nlband, &
        node,np,npar,nparb,nparf,nquad,nrow,para,phi,syseqn)

      call sgbtrf(neqn,neqn,nlband,nlband,a,nrow,ipivot,info)

      if ( info /= 0 ) then
        write ( *, * ) ' '
        write ( *, * ) 'Newton - Fatal error!'
        write ( *, * ) '  The jacobian is singular.'
        write ( *, * ) '  SGBTRF returns INFO = ',info
        ierror = 1
        return
      end if

      leval = .true.
      lmat = .true.
      call lmemry('set','have_fp',lmat)

    end if
!
!  Solve the linear system A*DX = RES
!
    call sgbtrs('N',neqn,nlband,nlband,nsys,a,nrow,ipivot,res,neqn,info)

    if ( info/= 0 ) then
      write ( *, * ) ' '
      write ( *, * ) 'Newton - Fatal error!'
      write ( *, * ) '  SGBTRS returns nonzero INFO = ',info
      ierror = 1
      return

    end if

    idmax = isamax(neqn,res,1)
    dmax = abs(res(idmax))

    if ( iwrite>= 4 ) then
      write ( *, * ) ' '
      write ( *, * ) 'Newton - MaxNorm(DX) = ',dmax,' Index=',idmax
    end if
!
!  Update the estimated solution G.
!
    lzero = .true.
    do i = 1,nparf
      if ( para(i) /= 0.0 ) then
        lzero=.false.
      end if
    end do

    if ( lzero ) then
      g(1:neqn) = 0.0D+00
    else
      do i = 1,neqn
        g(i) = g(i)-res(i)
      end do
    end if
!
!  Compute the norm of the updated solution G.
!
    ixmax = isamax(neqn,g,1)
    xmax = abs(g(ixmax))

    if ( 4 <= iwrite ) then
      write ( *, * ) ' '
      write ( *, * ) 'Newton - MaxNorm(X) =  ',xmax,' index=',ixmax
    end if
!
!  Evaluate the residual RES of the current estimated solution.
!
    call fx(area,eqn,g,ifs,indx,nelem,neqn,node,np,npar, &
      nparb,nparf,nquad,para,phi,res,splflo,syseqn,tauflo,yc)

    irmax = isamax(neqn,res,1)
    rmax = abs(res(irmax))

    if ( iwrite>= 4 ) then
      write ( *, * ) 'Newton - MaxNorm(FX) = ',rmax,' index=',irmax
    end if
!
!  If RMAX is less than RMAX2, copy current G into G2.
!
    if ( rmax<rmax2 ) then
      xmax2 = xmax
      rmax2 = rmax
      do i = 1,neqn
        g2(i) = g(i)
      end do
    end if
!
!  Accept the iterate if the residual is small enough.
!
    if ( rmax<= tolnew ) then
      if ( iwrite>= 4.or.falter ) then
        write ( *, * ) ' '
        write ( *, * ) 'Newton - Note:'
        write ( *, * ) '  Iterate ',iter,' accepted.'
      end if
      return
    end if
!
!  Reject the iterate if the residual has grown too large.
!
    if ( rmax>10.0*(rmax0+tolnew).and.iter>1 ) then
      write ( *, * ) ' '
      write ( *, * ) 'Newton - Warning!'
      write ( *, * ) '  Residual too big on step ',iter
      write ( *, * ) '  Initial residual  = ',rmax0
      write ( *, * ) '  Current residual  = ',rmax
      go to 20
    end if

  end do
!
!  The iteration has failed to converge, or may actually
!  have been terminated early.
!
20    continue

  ierror = 1

  write ( *, * ) ' '
  write ( *, * ) 'Newton - Warning!'
  write ( *, * ) '  No Newton convergence after   ',iter,' steps.'
  write ( *, * ) '  The final stepsize was        ',dmax
  write ( *, * ) '  The initial X norm was        ',xmax0
  write ( *, * ) '  The final X norm was          ',xmax
  write ( *, * ) '  The initial residual norm was ',rmax0
  write ( *, * ) '  The final residual norm was   ',rmax
!
!  If the Newton process did not converge, and we are using the
!  very miserly jacobian method, then set a flag to evaluate the
!  jacobian and try again.
!
!  But we will only do this if we did not already evaluate the
!  jacobian during this particular process.
!
  if ( jjac == 0 ) then

  else if (jjac == 1 ) then

  else if (jjac == 2 ) then

    if ( .not.leval ) then

      write ( *, * ) ' '
      write ( *, * ) 'Newton - Note:'
      write ( *, * ) '  Retrying Newton process with new jacobian.'
      do i = 1,neqn
        g(i) = g2(i)
      end do
      rmax0 = rmax2
      xmax0 = xmax2


      lmat = .false.
      falter = .true.
      go to 10

    end if

  end if

  write ( *, * ) ' '
  write ( *, * ) 'Newton - Note:'
  write ( *, * ) '  The failed Newton process could not be retried.'

  if ( lgdif ) then
    write ( *, * ) '  This is a finite difference point calculation.'
  else
    write ( *, * ) '  This is NOT a finite difference point calculation.'
  end if

  if ( ltarg ) then
    write ( *, * ) '  This is a target point calculation.'
  else
    write ( *, * ) '  This is NOT a target point calculation.'
  end if

  return
end
subroutine pldx(nvec,xval,xvec,yder,yvec)
!
!*******************************************************************************
!
!! PLDX evaluates the derivative of a piecewise linear function with
!  respect to its argument at a given point.
!
!  Note that if XVAL falls to the left of XVEC(1), then YDER = 0,
!  and similarly, if XDER is greater than XVEC(NVEC), YVAL = 0.
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
!  NVEC   Input, integer NVEC, the number of abscissas and coefficients
!         that define the piecewise linear.  NVEC must be odd, and
!         at least 3.
!
!  XVAL   Input, real XVAL, the point at which the
!         derivative with respect to X is to be evaluated.
!
!  XVEC   Input, real XVEC(NVEC), the abscissas of the
!         function.  These should be distinct and in ascending order.
!
!  YDER   Output, real YDER, the value of the derivative of
!         the piecewise linear function with respect to X, at the point
!         XVAL.
!
!  YVEC   Input, real YVEC(NVEC), the value of the
!         piecewise linear function at each of the abscissas.
!
!
  integer nvec
!
  integer i
  integer ival
  real ( kind = 8 ) xval
  real ( kind = 8 ) xvec(nvec)
  real ( kind = 8 ) yder
  real ( kind = 8 ) yvec(nvec)
!
!  Step 1: Check if XVAL lies outside the intervals.
!
  if ( xval<= xvec(1) ) then
    yder = 0
    return
  else if (xval>= xvec(nvec) ) then
    yder = 0
    return
  end if
!
!  Step 2: Find index I so that XVEC(I) <=  XVAL < XVEC(I+1)
!
  do i = 1,nvec-1

    if ( xvec(i)<= xval.and.xval<=xvec(i+1) ) then
      ival = i
      go to 10
    end if

  end do

  write ( *, * ) ' '
  write ( *, * ) 'PLVal - Fatal error!'
  write ( *, * ) '  Could not bracket XVAL = ',xval
  stop

10    continue
!
!  Step 3: Evaluate the slope of the linear function at XVAL.
!
  i = ival

  yder = (yvec(i+1)-yvec(i))/(xvec(i+1)-xvec(i))

  return
end
subroutine pldx1 ( ivec, nvec, xval, xvec, yder )
!
!*******************************************************************************
!
!! PLDX1 evaluates the derivative of a piecewise linear basis polynomial.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Discussion:
!
!    The piecewise linear basis polynomial is 1 at the IVEC-th node,
!    and 0 at all the other nodes.
!
!    Note that if XVAL falls to the left of XVEC(1), then YDER = 0,
!    and similarly, if XVAL is greater than XVEC(NVEC), YDER = 0.
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
!    Input, integer IVEC, the coefficient with respect to which
!    the partial derivative is desired.
!
!    Input, integer NVEC, the number of abscissas and coefficients
!    that define the piecewise linear.  NVEC must be odd, and
!    at least 3.
!
!    Input, real XVAL, the point at which the function
!    is to be evaluated.
!
!    Input, real XVEC(NVEC), the abscissas of the
!    function.  These should be distinct and in ascending order.
!
!    Output, real YDER, the value of the derivative of
!    the piecewise linear function at the point XVAL.
!
!
  integer nvec
!
  integer i
  integer ival
  integer ivec
  real ( kind = 8 ) xval
  real ( kind = 8 ) xvec(nvec)
  real ( kind = 8 ) yder
!
!  Step 1: Check if XVAL lies outside the intervals.
!
  if ( xval <= xvec(1) ) then
    yder = 0.0D+00
    return
  else if ( xvec(nvec) <= xval ) then
    yder = 0.0D+00
    return
  end if
!
!  Step 2: Find index I so that XVEC(I) <=  XVAL < XVEC(I+1)
!
  ival = -1

  do i = 1, nvec-1

    if ( xvec(i) <= xval .and. xval <= xvec(i+1) ) then
      ival = i
      exit
    end if

  end do

  if ( ival < 1 ) then
    write ( *, * ) ' '
    write ( *, * ) 'PLDX1 - Fatal error!'
    write ( *, * ) '  Could not bracket XVAL = ', xval
    stop
  end if
!
!  Step 3: Evaluate the slope of the IVEC-th linear function at XVAL.
!
  if ( ival == ivec ) then
    yder = ( 0.0D+00 - 1.0D+00 ) / ( xvec(ival+1) - xvec(ival) )
  else if ( ival+1 == ivec ) then
    yder = ( 1.0D+00 - 0.0D+00 ) / ( xvec(ival+1) - xvec(ival) )
  else
    yder = 0.0D+00
  end if

  return
end
subroutine pltopn(fileg,igunit,iplot)
!
!*******************************************************************************
!
!! PLTOPN opens the plotting file.
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
!  FILEG  Input, character ( len = 30 ) FILEG.
!
!         FILEG contains the name of the file into which the DISPLAY
!         graphics information will be stored.
!
!  IGUNIT Input/output, integer IGUNIT.
!
!         On input, if IGUNIT is zero, then the routine believes
!         that the graphics unit has not yet been opened.
!
!         If the FORTRAN unit has already been opened, then IGUNIT
!         should be nonzero, and the routine will know not to try
!         to open the file, since it is already open.
!
!         On output, IGUNIT is the FORTRAN unit used for writing data
!         to the plotfile FILEG.
!
!  IPLOT  Input/output, integer IPLOT.
!
!         On input, IPLOT has been set by the user.  On output, IPLOT
!         may have been reset to 0, if the graphics file could not
!         be opened.
!
!         IPLOT controls whether or not graphics files
!         suitable for use with DISPLAY will be created.
!
!         IPLOT = 0 means no such file will be created.
!
!         IPLOT>0 means plot data will be generated for each step
!         which is evenly divisible by IPLOT, or which is less than
!         or equal to IPLOT.
!
!         IPLOT = -1 means plot data will be generated for the target, the
!         first and the last steps only.
!
!
  character ( len = 30 ) fileg
  integer igunit
  integer iplot
!
  if ( iplot/= 0 ) then
!
!  If IGUNIT is not zero, then the graphics unit has already
!  been opened.
!
    if ( igunit == 0 ) then

      write ( *, * ) ' '
      write ( *, * ) 'PltOpn - Note:'
      write ( *, * ) '  Opening the plot file ' // trim ( fileg )
      write ( *, * ) ' '
!
!  Delete any old copy of the file.
!
      igunit = 11
      open(unit = igunit,file=fileg,status='unknown',form='formatted', &
        access = 'sequential',err=10)

      return
!
!  Write a warning if the plot file could not be opened.
!
10        continue

      write ( *, * ) ' '
      write ( *, * ) 'PltOpn - Warning!'
      write ( *, * ) '  The plot file could not be opened.'
      write ( *, * ) '  Resetting IPLOT to 0.'
      igunit = 0
      iplot = 0
!
!  Else plotfile is already open.
!
    else
      write ( *, * ) ' '
      write ( *, * ) 'PltOpn - Note'
      write ( *, * ) '  The plot file is already open.'
      write ( *, * ) '  New information will be appended to it.'
    end if

  end if

  return
end
subroutine pltwrt(eqn,g,gdif,igunit,indx,isotri,iwrite,maxeqn, &
  nelem,neqn,node,np,npar,npe,nprof,nx,ny,para,sens,xc,xprof,yc)
!
!*******************************************************************************
!
!! PLTWRT writes information to a file which can be used to create graphics images.
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
  integer maxeqn
  integer nelem
  integer neqn
  integer np
  integer npar
  integer ny
!
  character ( len = 2 ) ctemp
  character ( len = 2 ) eqn(neqn)
  real ( kind = 8 ) g(neqn)
  real ( kind = 8 ) gdif(maxeqn,npar)
  integer i
  integer icheck
  integer igunit
  integer ihor
  integer indx(3,np)
  integer ip
  integer ipar
  integer iprs
  integer, save :: iset = 0
  integer isotri(nelem)
  integer iver
  integer iwrite
  integer j
  integer node(6,nelem)
  integer npe
  integer nprof(2*ny-1)
  integer nx
  real ( kind = 8 ) para(npar)
  real ( kind = 8 ) rtemp
  real ( kind = 8 ) rtemp2
  real ( kind = 8 ) sens(maxeqn,npar)
  real ( kind = 8 ) xc(np)
  real ( kind = 8 ) xprof
  real ( kind = 8 ) yc(np)
!
  iset = iset + 1
!
!  Number of elements, nodes, parameters,
!  elements in the X direction, elements in the Y direction.
!
  write(igunit,*)nelem
  write(igunit,*)np
  write(igunit,*)npar
  write(igunit,*)npe
  write(igunit,*)nx
  write(igunit,*)ny
!
!  Pressures, P.
!
  do ip = 1,np
    iprs = indx(3,ip)
    if ( iprs<= 0 ) then
      rtemp = 0.0D+00
    else
      rtemp = g(iprs)
    end if
    write(igunit,*)rtemp
  end do
!
!  Horizontal velocities, U.
!
  do ip = 1,np
    ihor = indx(1,ip)
    write(igunit,*)g(ihor)
  end do
!
!  Vertical velocities, V
!
  do ip = 1,np

    iver = indx(2,ip)
    write(igunit,*)g(iver)
  end do
!
!  Indicator of element type (isoparametric or not).
!
  do i = 1,nelem
    write(igunit,*)isotri(i)
  end do
!
!  Nodes that make up each element.
!
  do i = 1,6
    do j = 1,nelem
      write(igunit,*)node(i,j)
    end do
  end do
!
!  Indices of the nodes along the profile line.
!
  do i = 1,2*ny-1
    write(igunit,*)nprof(i)
  end do
!
!  Parameters.
!
  do i = 1,npar
    write(igunit,*)para(i)
  end do
!
!  Pressure sensitivities, dP/dpar
!
  do ipar = 1,npar

    do ip = 1,np
      iprs = indx(3,ip)
      rtemp = 0.0D+00
      rtemp2 = 0.0D+00
      if ( iprs>0 ) then
        rtemp = sens(iprs,ipar)
        rtemp2 = gdif(iprs,ipar)
      end if
      write(igunit,*)rtemp,rtemp2
    end do
!
!  Horizontal velocity sensitivities, dU/dpar
!
    do ip = 1,np
      ihor = indx(1,ip)
      write(igunit,*)sens(ihor,ipar),gdif(ihor,ipar)
    end do
!
!  Vertical velocity sensitivities, dV/dpar
!
    do ip = 1,np
      iver = indx(2,ip)
      write(igunit,*)sens(iver,ipar),gdif(iver,ipar)
    end do

  end do
!
!  X coordinates of nodes.
!
  do i = 1,np
    write(igunit,*)xc(i)
  end do
!
!  X coordinate of profile line.
!
  write(igunit,*)xprof
!
!  Y coordinates of nodes.
!
  do i = 1,np
    write(igunit,*)yc(i)
  end do
!
!  Nodal equation types.
!
  do i = 1,np
    ihor = indx(1,i)
    iver = indx(2,i)
    iprs = indx(3,i)
    if ( iprs<= 0 ) then
      ctemp = '  '
    else
      ctemp = eqn(iprs)
    end if
    write(igunit,'(3a2)')eqn(ihor),eqn(iver),ctemp
  end do
!
!  Write a check at the the end.
!
  icheck = 1953
  write(igunit,*)icheck

  if ( iwrite>= 1 ) then
    write ( *, * ) ' '
    write ( *, * ) 'PltWrt wrote data set ',iset,' to file.'
  end if

  return
end
subroutine plval(nvec,xval,xvec,yval,yvec)
!
!*******************************************************************************
!
!! PLVAL evaluates a piecewise linear function at a given point.
!
!  Note that if XVAL falls to the left of XVEC(1), then YVAL = YVEC(1),
!  and similarly, if XVAL is greater than XVEC(NVEC), YVAL = YVEC(NVEC).
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
!  NVEC   Input, integer NVEC, the number of abscissas and coefficients
!         that define the piecewise linear.  NVEC must be at least 1.
!
!  XVAL   Input, real XVAL, the point at which the function
!         is to be evaluated.
!
!  XVEC   Input, real XVEC(NVEC), the abscissas of the
!         function.  These should be distinct and in ascending order.
!
!  YVAL   Output, real YVAL, the value of the piecewise
!         linear function at the point XVAL.
!
!  YVEC   Input, real YVEC(NVEC), the value of the piecewise
!         function at each of the abscissas.
!
!
  integer nvec
!
  integer i
  integer ival
  real ( kind = 8 ) xval
  real ( kind = 8 ) xvec(nvec)
  real ( kind = 8 ) yval
  real ( kind = 8 ) yvec(nvec)
!
!  Step 1: Check if XVAL lies outside the intervals.
!
  if ( xval<= xvec(1) ) then
    yval = yvec(1)
    return
  else if (xval>= xvec(nvec) ) then
    yval = yvec(nvec)
    return
  end if
!
!  Step 2: Find index I so that XVEC(I) <=  XVAL < XVEC(I+1)
!
  do i = 1,nvec-1

    if ( xvec(i)<= xval.and.xval<=xvec(i+1) ) then
      ival = i
      go to 10
    end if

  end do

  write ( *, * ) ' '
  write ( *, * ) 'PLVal - Fatal error!'
  write ( *, * ) '  Could not bracket XVAL = ',xval
  stop

10    continue
!
!  Step 3: Evaluate the linear function at XVAL.
!
  i = ival

  if ( xval == xvec(i+1) ) then
    yval = yvec(i+1)
  else if (xval == xvec(i) ) then
    yval = yvec(i)
  else
    yval = ( yvec(i)*(xvec(i+1)-xval)+yvec(i+1)*(xval-xvec(i)) ) &
      / (xvec(i+1)-xvec(i))
  end if

  return
end
subroutine plval1(ivec,nvec,xval,xvec,yval)
!
!*******************************************************************************
!
!! PLVAL1 evaluates the piecewise linear polynomial which is 1
!  at node IVEC and 0 at the other nodes.
!
!  Note that if XVAL falls to the left of XVEC(1), then YVAL = 0,
!  and similarly, if XVAL is greater than XVEC(NVEC), YVAL = 0.
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
!  IVEC   Input, integer IVEC, the coefficient with respect to which
!         the partial derivative is desired.
!
!  NVEC   Input, integer NVEC, the number of abscissas and coefficients
!         that define the piecewise linear.  NVEC must be odd, and
!         at least 3.
!
!  XVAL   Input, real XVAL, the point at which the function
!         is to be evaluated.
!
!  XVEC   Input, real XVEC(NVEC), the abscissas of the
!         function.  These should be distinct and in ascending order.
!
!  YDER   Output, real YDER, the value of the derivative of
!         the piecewise linear function at the point XVAL.
!
!
  integer nvec
!
  integer i
  integer ival
  integer ivec
  real ( kind = 8 ) xval
  real ( kind = 8 ) xvec(nvec)
  real ( kind = 8 ) yval
!
!  Step 1: Check if XVAL lies outside the intervals.
!
  if ( xval<= xvec(1) ) then
    yval = 0.0D+00
    return
  else if (xval>= xvec(nvec) ) then
    yval = 0.0D+00
    return
  end if
!
!  Step 2: Find index I so that XVEC(I) <=  XVAL < XVEC(I+1)
!
  do i = 1,nvec-1

    if ( xvec(i)<= xval.and.xval<=xvec(i+1) ) then
      ival = i
      go to 10
    end if

  end do

  write ( *, * ) ' '
  write ( *, * ) 'PLVAL1 - Fatal error!'
  write ( *, * ) '  Could not bracket XVAL = ',xval
  stop

10    continue
!
!  Step 3: Determine the index of the left endpoint of the least and
!  greatest intervals that IVEC can affect.
!
  i = ival
  if ( ival == ivec ) then
    if ( xval == xvec(ival) ) then
      yval = 1.0D+00
    else
      yval = (xvec(ival+1)-xval)/(xvec(ival+1)-xvec(ival))
    end if
  else if (ival+1 == ivec ) then
    if ( xval == xvec(ival+1) ) then
      yval = 1.0D+00
    else
      yval = (xval-xvec(ival)) / (xvec(ival+1)-xvec(ival))
    end if
  else
    yval = 0.0D+00
  end if

  return
end
subroutine ppvalu(break,coef,l,k,x,jderiv,value)
!
!*******************************************************************************
!
!! PPVALU calculates the value at X of the JDERIV-th derivative of
!  the piecewise polynomial function F from its piecewise
!  polynomial representation.
!
!  The interval index I, appropriate for X, is found through a
!  call to INTERV.  The formula above for the JDERIV-th derivative
!  of F is then evaluated by nested multiplication.
!
!  The J-th derivative of F is given by:
!
!    (d**j)f(x) = coef(j+1,i)
!               + h*(coef(j+2,i)
!               + h*(...(coef(k-1,i) +
!               + h*coef(k,i)/(k-j-1))/(k-j-2) ... )/2)/1
!
!  with
!
!    H = X - BREAK(I)
!
!  and
!
!    i  =  max( 1 , max( j ,  break(j) <= x , 1 <= j <= l ) ).
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
!  BREAK,
!  COEF,
!  L      Input, real BREAK(L+1), REAL COEF(*), integer L, for
!         piecewise polynomial representation of the function F to
!         be evaluated.
!
!  K      Input, integer K, the order of the polynomial pieces
!         that make up the function F.  The most usual value for
!         K is 4, signifying a piecewise cubic polynomial.
!
!  X      Input, real X, the point at which to evaluate F or
!         of its derivatives.
!
!  JDERIV Input, integer JDERIV, the order of the derivative to be
!         evaluated.  If JDERIV is 0, then F itself is evaluated,
!         which is actually the most common case.  It is assumed
!         that JDERIV is zero or positive.
!
!  VALUE  Output, real VALUE, the value of the JDERIV-th
!         derivative of F at X.
!
!
  integer k
  integer l
!
  real ( kind = 8 ) break(l)
  real ( kind = 8 ) coef(k,l)
  real ( kind = 8 ) fmmjdr
  real ( kind = 8 ) h
  integer i
  integer jderiv
  integer m
  integer ndummy
  real ( kind = 8 ) value
  real ( kind = 8 ) x
!
  value = 0.0D+00

  fmmjdr = k-jderiv
!
!  Derivatives of order K or higher are identically zero.
!
  if ( k<= jderiv)return
!
!  Find the index I of the largest breakpoint to the left of X.
!
  call interv(break,l+1,x,i,ndummy)
!
!  Evaluate the JDERIV-th derivative of the I-th polynomial piece
!  at X.
!
  h = x-break(i)
  m = k

10    continue

  value = (value/fmmjdr)*h+coef(m,i)
  m = m-1
  fmmjdr = fmmjdr-1
  if ( fmmjdr>0.0D+00 )go to 10

  return
end
subroutine pqdx(nvec,xval,xvec,yder,yvec)
!
!*******************************************************************************
!
!! PQDX evaluates the derivative of a piecewise quadratic function with
!  respect to its argument at a given point.
!
!  Note that if XDER falls to the left of XVEC(1), then YVAL = 0,
!  and similarly, if XVAL is greater than XVEC(NVEC), YDER = 0.
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
!  NVEC   Input, integer NVEC, the number of abscissas and coefficients
!         that define the piecewise quadratic.  NVEC must be odd, and
!         at least 3.
!
!  XVAL   Input, real XVAL, the point at which the
!         derivative with respect to X is to be evaluated.
!
!  XVEC   Input, real XVEC(NVEC), the abscissas of the
!         function.  These should be distinct and in ascending order.
!
!  YDER   Output, real YDER, the value of the derivative
!         of the piecewise  quadratic function with respect to X,
!         at the point XVAL.
!
!  YVEC   Input, real YVEC(NVEC), the value of the piecewise
!         quadratic function at each of the abscissas.
!
!
  integer nvec
!
  integer i
  integer ival
  real ( kind = 8 ) xval
  real ( kind = 8 ) xvec(nvec)
  real ( kind = 8 ) yder
  real ( kind = 8 ) yvec(nvec)
!
!  Step 0: Check data.
!
  if ( nvec<3 ) then
    write ( *, * ) ' '
    write ( *, * ) 'PQDX - Fatal error.'
    write ( *, * ) '  NVEC is ',nvec
    write ( *, * ) '  but NVEC must be at least 3.'
    stop
  end if

  if ( mod(nvec,2)/= 1 ) then
    write ( *, * ) ' '
    write ( *, * ) 'PQDX - Fatal error!'
    write ( *, * ) '  Even value of NVEC = ',nvec
    stop
  end if
!
!  Step 1: Find odd index I so that XVEC(I) <=  XVAL < XVEC(I+2)
!
  if ( xval<= xvec(1) ) then
    yder = yvec(1)
    return
  else if (xval>= xvec(nvec) ) then
    yder = yvec(nvec)
    return
  end if

  do i = 1,nvec-2,2

    if ( xvec(i)<= xval.and.xval<=xvec(i+2) ) then
      ival = i
      go to 10
    end if

  end do

  write ( *, * ) ' '
  write ( *, * ) 'PQDX - Fatal error!'
  write ( *, * ) '  Could not bracket XVAL = ',xval
  stop

10    continue
!
!  Step 2: Evaluate the derivative of the quadratic function at XVAL.
!
  i = ival

  yder = yvec(i)*(2*xval-xvec(i+1)-xvec(i+2)) &
    /((xvec(i)-xvec(i+1))*(xvec(i)-xvec(i+2))) &
    +yvec(i+1)*(2*xval-xvec(i)-xvec(i+2)) &
    /((xvec(i+1)-xvec(i))*(xvec(i+1)-xvec(i+2))) &
    +yvec(i+1)*(2*xval-xvec(i)-xvec(i+2)) &
    /((xvec(i+1)-xvec(i))*(xvec(i+1)-xvec(i+2)))

  return
end
subroutine pqdx1(ivec,nvec,xval,xvec,yder)
!
!*******************************************************************************
!
!! PQDX1 evaluates the X derivative of the piecewise quadratic
!  polynomial which is 1 at the IVEC-th node and 0 at the others.
!
!  Note that if XVAL falls to the left of XVEC(1), then YDER = 0,
!  and similarly, if XVAL is greater than XVEC(NVEC), YDER = 0.
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
!  IVEC   Input, integer IVEC, the coefficient with respect to which
!         the partial derivative is desired.
!
!  NVEC   Input, integer NVEC, the number of abscissas and coefficients
!         that define the piecewise quadratic.  NVEC must be odd, and
!         at least 3.
!
!  XVAL   Input, real XVAL, the point at which the function
!         be evaluated.
!
!  XVEC   Input, real XVEC(NVEC), the abscissas of the
!         function.  These should be distinct and in ascending order.
!
!  YDER   Output, real YDER, the value of the derivative of
!         the piecewise quadratic function at the point XVAL.
!
!
  integer nvec
!
  integer i
  integer ihi
  integer ilo
  integer ival
  integer ivec
  real ( kind = 8 ) xval
  real ( kind = 8 ) xvec(nvec)
  real ( kind = 8 ) yder
!
!  Step 0: Check data.
!
  if ( nvec<3 ) then
    write ( *, * ) ' '
    write ( *, * ) 'PQDX1 - Fatal error!'
    write ( *, * ) '  NVEC = ',nvec
    write ( *, * ) '  but NVEC must be at least 3.'
    stop
  end if

  if ( mod(nvec,2)/= 1 ) then
    write ( *, * ) ' '
    write ( *, * ) 'PQDX1 - Fatal error!'
    write ( *, * ) '  Even value of NVEC = ',nvec
    stop
  end if
!
!  Step 1: Find odd index I so that XVEC(I) <=  XVAL < XVEC(I+2)
!
  if ( xval<= xvec(1) ) then
    yder = 0
    return
  else if (xval>= xvec(nvec) ) then
    yder = 0
    return
  end if

  do i = 1,nvec-2,2

    if ( xvec(i)<= xval.and.xval<=xvec(i+2) ) then
      ival = i
      go to 10
    end if

  end do

  write ( *, * ) ' '
  write ( *, * ) 'PQDX1 - Fatal error!'
  write ( *, * ) '  Could not bracket XVAL = ',xval
  stop

10    continue
!
!  Step 2: Determine the index of the left endpoint of the least and
!  greatest intervals that IVEC can affect.
!
  if ( mod(ivec,2) == 0 ) then
    ilo = ivec-1
    ihi = ivec-1
  else
    ilo = max(ivec-2,1)
    ihi = ivec
  end if
!
!  Step 3: If XVAL is outside of the intervals that IVEC can affect,
!  the derivative is zero.
!
  if ( ival<ilo.or.ival>ihi ) then
    yder = 0
    return
  end if
!
!  Step 3: Evaluate the derivative of the quadratic function at XVAL.
!
  i = ival

  if ( ivec ==ival ) then
    yder = (2*xval-xvec(i+1)-xvec(i+2))/((xvec(i)-xvec(i+1))*(xvec(i)-xvec(i+2)))
  else if (ivec ==ival+1 ) then
    yder = (2*xval-xvec(i)-xvec(i+2))/((xvec(i+1)-xvec(i))*(xvec(i+1)-xvec(i+2)))
  else if (ivec ==ival+2 ) then
    yder = (2*xval-xvec(i)-xvec(i+1))/((xvec(i+2)-xvec(i))*(xvec(i+2)-xvec(i+1)))
  else
    write ( *, * ) ' '
    write ( *, * ) 'PQDX1 - Fatal error!'
    write ( *, * ) '  IVEC = ',ivec
    write ( *, * ) '  IVAL = ',ival
  end if

  return
end
subroutine pqval(nvec,xval,xvec,yval,yvec)
!
!*******************************************************************************
!
!! PQVAL evaluates a piecewise quadratic function at a given point.
!
!
!  The piecewise quadratic is defined by NVEC values, where NVEC
!  is odd, and at least 3.  The function is defined by specifying
!  a list of nodes XVEC(I), and specifying its value YVEC(I) at each
!  node.
!
!  The function will be a quadratic polynomial over each of
!  (NVEC-1)/2 intervals that are made up a set of three consecutive
!  nodes, with the first one odd.  Thus, XVEC(1), XVEC(2) and XVEC(3)
!  lie in the first interval.
!
!  At the odd nodes, the quadratic that defines the function may
!  change, but the function remains continuous there, though not
!  differentiable.
!
!  Note that if XVAL falls to the left of XVEC(1), then YVAL = YVEC(1),
!  and similarly, if XVAL is greater than XVEC(NVEC), YVAL = YVEC(NVEC).
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
!  NVEC   Input, integer NVEC, the number of abscissas and coefficients
!         that define the piecewise quadratic.
!
!         NVEC must be odd, and at least 3.
!
!  XVAL   Input, real XVAL, the point at which the function
!         is be evaluated.
!
!  XVEC   Input, real XVEC(NVEC), the abscissas of the
!         function.  These should be distinct and in ascending order.
!
!  YVAL   Output, real YVAL, the value of the piecewise
!         quadratic function at the point XVAL.
!
!  YVEC   Input, real YVEC(NVEC), the value of the
!         piecewise quadratic function at each of the abscissas.
!
!
  integer nvec
!
  integer i
  integer ival
  real ( kind = 8 ) xval
  real ( kind = 8 ) xvec(nvec)
  real ( kind = 8 ) yval
  real ( kind = 8 ) yvec(nvec)
!
!  Step 0: Check data.
!
  if ( nvec<3 ) then
    write ( *, * ) ' '
    write ( *, * ) 'PQVal - Fatal error!'
    write ( *, * ) '  Value of NVEC = ',nvec
    write ( *, * ) '  but NVEC must be at least 3.'
    stop
  end if

  if ( mod(nvec,2)/= 1 ) then
    write ( *, * ) ' '
    write ( *, * ) 'PQVal - Fatal error!'
    write ( *, * ) '  Even value of NVEC = ',nvec
    stop
  end if
!
!  Step 1: Find odd index I so that XVEC(I) <=  XVAL < XVEC(I+2)
!
  if ( xval<= xvec(1) ) then
    yval = yvec(1)
    return
  else if (xval>= xvec(nvec) ) then
    yval = yvec(nvec)
    return
  end if

  do i = 1,nvec-2,2

    if ( xvec(i)<= xval.and.xval<=xvec(i+2) ) then
      ival = i
      go to 10
    end if

  end do

  write ( *, * ) ' '
  write ( *, * ) 'PQVal - Fatal error!'
  write ( *, * ) '  Could not bracket XVAL = ',xval
  write ( *, * ) '  There are ',nvec,' nodes.'
  write ( *, * ) '  First node is at ',xvec(1)
  write ( *, * ) '  Last node is at  ',xvec(nvec)

  do i = 1,nvec
    write ( *, * ) xvec(i)
  end do
  stop

10    continue
!
!  Step 2: Evaluate the quadratic function at XVAL.
!
  i = ival

  yval = yvec(i)*(xval-xvec(i+1)) * (xval-xvec(i+2)) &
         /((xvec(i)-xvec(i+1))*(xvec(i)-xvec(i+2))) &
         +yvec(i+1)*(xval-xvec(i)) * (xval-xvec(i+2)) &
         /((xvec(i+1)-xvec(i))*(xvec(i+1)-xvec(i+2))) &
         +yvec(i+2)*(xval-xvec(i)) * (xval-xvec(i+1)) &
         /((xvec(i+2)-xvec(i))*(xvec(i+2)-xvec(i+1)))

  return
end
subroutine pqval1(ivec,nvec,xval,xvec,yval)
!
!*******************************************************************************
!
!! PQVAL1 evaluates the piecewise quadratic polynomial which is 1
!  at node IVEC and 0 at the other nodes.
!
!  Note that if XVAL falls to the left of XVEC(1), then YVAL = 0,
!  and similarly, if XVAL is greater than XVEC(NVEC), YVAL = 0.
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
!  IVEC   Input, integer IVEC, the coefficient with respect to which
!         the partial derivative is desired.
!
!  NVEC   Input, integer NVEC, the number of abscissas and coefficients
!         that define the piecewise quadratic.  NVEC must be odd, and
!         at least 3.
!
!  XVAL   Input, real XVAL, the point at which the function
!         is to be evaluated.
!
!  XVEC   Input, real XVEC(NVEC), the abscissas of the
!         function.  These should be distinct and in ascending order.
!
!  YDER   Output, real YDER, the value of the derivative of
!         the piecewise quadratic function at the point XVAL.
!
!
  integer nvec
!
  integer i
  integer ihi
  integer ilo
  integer ival
  integer ivec
  real ( kind = 8 ) xval
  real ( kind = 8 ) xvec(nvec)
  real ( kind = 8 ) yval
!
!  Step 0: Check data.
!
  if ( nvec<3 ) then
    write ( *, * ) ' '
    write ( *, * ) 'PQVal1 - Fatal error!'
    write ( *, * ) '  Value of NVEC is ',nvec
    write ( *, * ) '  but NVEC must be at least 3.'
    stop
  end if

  if ( mod(nvec,2)/= 1 ) then
    write ( *, * ) ' '
    write ( *, * ) 'PQVal1 - Fatal error!'
    write ( *, * ) '  Even value of NVEC = ',nvec
    stop
  end if
!
!  Step 1: Find odd index I so that XVEC(I) <=  XVAL < XVEC(I+2)
!
  if ( xval<= xvec(1) ) then
    yval = 0.0D+00
    return
  else if (xval>= xvec(nvec) ) then
    yval = 0.0D+00
    return
  end if

  do i = 1,nvec-2,2

    if ( xvec(i)<= xval.and.xval<=xvec(i+2) ) then
      ival = i
      go to 10
    end if

  end do

  write ( *, * ) ' '
  write ( *, * ) 'PQVal1 - Fatal error!'
  write ( *, * ) '  Could not bracket XVAL = ',xval
  stop

10    continue
!
!  Step 2: Determine the index of the left endpoint of the least and
!  greatest intervals that IVEC can affect.
!
  if ( mod(ivec,2) == 0 ) then
    ilo = ivec-1
    ihi = ivec-1
  else
    ilo = max(ivec-2,1)
    ihi = ivec
  end if
!
!  Step 3: If XVAL is outside of the intervals that IVEC can affect,
!  the value is zero.
!
  if ( ival<ilo.or.ival>ihi ) then
    yval = 0
    return
  end if
!
!  Step 3: Evaluate the quadratic function at XVAL.
!
  i = ival

  if ( ivec ==ival ) then
    yval = (xval-xvec(i+1)) * (xval-xvec(i+2)) &
      /((xvec(i)-xvec(i+1))*(xvec(i)-xvec(i+2)))
  else if (ivec ==ival+1 ) then
     yval = (xval-xvec(i)) * (xval-xvec(i+2)) &
       /((xvec(i+1)-xvec(i))*(xvec(i+1)-xvec(i+2)))
  else if (ivec ==ival+2 ) then
      yval = (xval-xvec(i)) * (xval-xvec(i+1)) &
        /((xvec(i+2)-xvec(i))*(xvec(i+2)-xvec(i+1)))
  else
    write ( *, * ) ' '
    write ( *, * ) 'PQVal1 - Fatal error!'
    write ( *, * ) '  IVEC = ',ivec
    write ( *, * ) '  IVAL = ',ival
  end if

  return
end
subroutine prcost2(cost,costb,costp,costu,costv,title,wateb,watep,wateu,watev)
!
!*******************************************************************************
!
!! PRCOST2 prints out the current cost function.
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
  real ( kind = 8 ) cost
  real ( kind = 8 ) costb
  real ( kind = 8 ) costp
  real ( kind = 8 ) costu
  real ( kind = 8 ) costv
  integer llen
  character ( len = * )  title
  real ( kind = 8 ) wateb
  real ( kind = 8 ) watep
  real ( kind = 8 ) wateu
  real ( kind = 8 ) watev
!
  llen = len_trim ( title )

  write ( *, * ) ' '
  if ( llen>0 ) then
    write(*,'(a)')title(1:llen)
  end if

  write(*,'(a)')'PrCost:               B         P' // &
    '         U         V'

  write(*,'(a,g11.4)')'  Cost function:   ',cost

  write(*,'(a,4g10.3)')'  Component costs: ',costb,costp,costu,costv

  write(*,'(a,4g10.3)')'  Weighted costs:  ',wateb*costb, &
    watep*costp,wateu*costu,watev*costv

  return
end
subroutine prcsen(dpara,npar,title)
!
!*******************************************************************************
!
!! PRCSEN prints out the cost sensitivities.
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
!  DPARA  Input, real DPARA(NPAR).
!
!         DPARA contains an estimate for the derivatives of the cost
!         function with respect to the various parameters.
!
!           DPARA(I) = D cost / D parameter(I).
!
!  NPAR   Input, integer NPAR.
!
!         The number of parameters.  NPAR = NPARF + NPARB + 1.
!
!         The parameters control the shape of the inflow,
!         the shape of the bump obstacle, and the strength of the
!         flow.
!
!
  integer npar
!
  real ( kind = 8 ) dpara(npar)
  integer i
  integer ihi
  integer ilo
  character ( len = * )  title
!
  write ( *, * ) ' '
  write(*,'(a)')title
  write ( *, * ) ' '

  do ilo = 1,npar,5
    ihi = min(ilo+4,npar)
    write(*,'(5g13.5)')(dpara(i),i = ilo,ihi)
  end do

  return
end
subroutine prdat(epsdif,fileg,filet,ibc,ifscan,ifstar,ibscan,ibstar,ibump, &
  idfd,ids,ifds,igrad,ijac,iopt,iplot,istep1,istep2,itar,itype,iwrite,jjac, &
  jstep1,jstep2,maxnew,maxpar,maxstp,nopt,npar,nparb,nparf,nquad,nstep3,nx, &
  ny,para1,para2,para3,partar,syseqn,tolnew,tolopt,wateb,wateb1,wateb2,watep, &
  wateu,watev,xblcan,xbltar,xbrcan,xbrtar,xprof,yblcan,ybltar,ybrcan,ybrtar)
!
!*******************************************************************************
!
!! PRDAT prints the user input file data.
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
  integer maxpar
!
  real ( kind = 8 ) epsdif
  character ( len = 30 ) fileg
  character ( len = 30 ) filet
  integer i
  integer ibc
  integer ifscan
  integer ifstar
  integer ibscan
  integer ibstar
  integer ibump
  integer idfd
  integer ids
  integer ifds
  integer igrad
  integer ijac
  integer iopt(maxpar)
  integer iplot
  integer istep1
  integer istep2
  integer itar
  integer itype
  integer iwrite
  integer jjac
  integer jstep1
  integer jstep2
  integer maxnew
  integer maxstp
  integer nopt
  integer npar
  integer nparb
  integer nparf
  integer nquad
  integer nstep3
  integer nx
  integer ny
  real ( kind = 8 ) para1(maxpar)
  real ( kind = 8 ) para2(maxpar)
  real ( kind = 8 ) para3(maxpar)
  real ( kind = 8 ) partar(maxpar)
  character ( len = 20 ) syseqn
  real ( kind = 8 ) tolnew
  real ( kind = 8 ) tolopt
  character ( len = 6 ) type
  real ( kind = 8 ) wateb
  real ( kind = 8 ) wateb1
  real ( kind = 8 ) wateb2
  real ( kind = 8 ) watep
  real ( kind = 8 ) wateu
  real ( kind = 8 ) watev
  real ( kind = 8 ) xblcan
  real ( kind = 8 ) xbltar
  real ( kind = 8 ) xbrcan
  real ( kind = 8 ) xbrtar
  real ( kind = 8 ) xprof
  real ( kind = 8 ) yblcan
  real ( kind = 8 ) ybltar
  real ( kind = 8 ) ybrcan
  real ( kind = 8 ) ybrtar
  character ( len = 3 ) yesno
!
  write ( *, * ) ' '
  write ( *, * ) 'PrDat - Note:'
  write ( *, * ) '  Values of user-definable variables:'
  write ( *, * ) ' '
  write ( *, * ) '  Number of horizontal elements, NX = ',nx
  write ( *, * ) '  Number of vertical elements, NY =   ',ny
  write ( *, * ) ' '
  write ( *, * ) '  NPARF = ',nparf,' inflow parameters'
  write ( *, * ) '  NPARB = ',nparb,' bump parameters'
  write ( *, * ) '  NPARR = 1 inverse viscosity parameters.'
  write ( *, * ) '  NPAR =  ',npar,' total parameters'
  write ( *, * ) ' '
  write ( *, * ) '  Variable  Type  Free to Vary?'
  write ( *, * ) ' '
  do i = 1,npar
    if ( i<= nparf ) then
      type = 'Inflow'
    else if (i<= nparf+nparb ) then
      type = 'Shape'
    else
      type = 'Nu_Inv'
    end if
    if ( iopt(i) == 0 ) then
      yesno = 'No'
    else
      yesno = 'Yes'
    end if
    write(*,'(3x,i5,2x,a6,2x,a3)')i,type,yesno
  end do

  write ( *, * ) ' '
  write ( *, * ) '  NOPT = ',nopt,' optimization parameters.'
  write ( *, * ) ' '
  write ( *, * ) '  NQUAD, number of quadrature points = ',nquad
!
!  Target data.
!
  write ( *, * ) ' '

  if ( itar == 0 ) then

    write ( *, * ) 'The target data is computed from a flow.'
    write ( *, * ) ' '

    if ( ibstar == 1 ) then
      write ( *, * ) 'Target bump modeled by C0 linear splines.'
    else if (ibstar == 2 ) then
      write ( *, * ) 'Target bump modeled by C0 quadratic splines.'
    else if (ibstar ==3 ) then
      write ( *, * ) 'Target bump modeled by C1 cubic splines.'
    end if

    if ( ifstar == 1 ) then
      write ( *, * ) 'Target inflow modeled by C0 linear splines.'
    else if (ifstar == 2 ) then
      write ( *, * ) 'Target inflow modeled by C0 quadratic splines.'
    else if (ifstar ==3 ) then
      write ( *, * ) 'Target inflow modeled by C1 cubic splines.'
    end if

    if ( nparb > 0 ) then
      write ( *, * ) ' '
      write ( *, * ) '  Target bump from x,y =   ',xbltar,ybltar
      write ( *, * ) '                to x,y =   ',xbrtar,ybrtar
    end if
    write ( *, * ) ' '
    write ( *, * ) '  Parameter values at target point:'
    call prpar(nparb,nparf,partar)

  else if (itar == 1 ) then

    write ( *, * ) 'The target data is an arbitrary formula.'

  end if
!
!  Feasible data.
!
  write ( *, * ) ' '

  if ( ibscan == 1 ) then
    write ( *, * ) 'Bump modeled by C0 piecewise linears.'
  else if (ibscan == 2 ) then
    write ( *, * ) 'Bump modeled by C0 piecewise quadratics.'
  else if (ibscan ==3 ) then
    write ( *, * ) 'Bump modeled by C1 cubic splines.'
  end if

  if ( ifscan == 1 ) then
    write ( *, * ) 'Inflow modeled by C0 piecewise linears.'
  else if (ibscan == 2 ) then
    write ( *, * ) 'Inflow modeled by C0 piecewise quadratics.'
  else if (ibscan ==3 ) then
    write ( *, * ) 'Inflow modeled by C1 cubic splines.'
  end if

  if ( nparb>0 ) then
    write ( *, * ) '  Feasible  bump from x,y = ',xblcan,yblcan
    write ( *, * ) '                   to x,y = ',xbrcan,ybrcan
  end if
  write ( *, * ) ' '
  write ( *, * ) '  The flow discrepancy is measured at XPROF = ',xprof
!
!  Cost function.
!
  write ( *, * ) ' '
  write ( *, * ) ' '
  write ( *, * ) '  Cost function is weighted sum of these costs:'
  write ( *, * ) ' '
  write ( *, * ) ' '
  write ( *, * ) '  U, V and P discrepancies at profile line,'
  write ( *, * ) '  SQRT(Integral (InflowSlope)**2 ),'
  write ( *, * ) '  SQRT(Integral (BumpSlope-LineSlope)**2).'
  write ( *, * ) ' '
  write ( *, * ) 'Weight factors:'
  write ( *, * ) ' '
  write ( *, * ) '  Bump control cost,   WATEB =  ',wateb
  write ( *, * ) '  Pressure discrepancy, WATEP = ',watep
  write ( *, * ) '  U discrepancy, WATEU =        ',wateu
  write ( *, * ) '  V discrepancy, WATEV =        ',watev
!
!  Data relating to Newton iteration.
!
  write ( *, * ) ' '
  write ( *, * ) ' '
  write ( *, * ) 'Generation of starting point for Newton:'

  write ( *, * ) 'Perturbation used for finite differences is ',epsdif
  write ( *, * ) ' '
  if ( jjac == 0 ) then
    write ( *, * ) '  JJAC = 0, update the jacobian for regular points'
    write ( *, * ) '  and for finite difference points.  A new point'
    write ( *, * ) '  always gets a new jacobian.'
  else if (jjac == 1 ) then
    write ( *, * ) '  JJAC = 1, do NOT update the jacobian for'
    write ( *, * ) '  finite difference points.  A new regular point'
    write ( *, * ) '  always gets a new jacobian.'
  else if (jjac == 2 ) then
    write ( *, * ) '  JJAC = 2, Only update the jacobian when necessary.'
  end if

  if ( ijac == 1 ) then
    write ( *, * ) '  IJAC = 1, Jacobian is evaluated on every step.'
  else if (ijac>1 ) then
    write ( *, * ) '  IJAC = N, Jacobian is evaluated on steps'
    write ( *, * ) '  0, N, 2*N, 3*N, ...'
  end if

  write ( *, * ) ' '
  write ( *, * ) ' '
  write ( *, * ) '  Up to MAXNEW = ',maxnew,' Newton iterations.'
  write ( *, * ) '  TOLNEW, Newton iteration tolerance = ',tolnew
!
!  Data relating to continuation.
!
!
!  Derivative information.
!
  write ( *, * ) ' '
  write ( *, * ) 'Derivative information:'
  write ( *, * ) ' '
  if ( ids == 0 ) then
    write ( *, * ) 'Discretized sensitivities will NOT be computed.'
  else
    write ( *, * ) 'Discretized sensitivities will be computed.'
  end if

  if ( ifds == 0 ) then
    write ( *, * ) 'Finite difference sensitivities will NOT be computed.'
  else
    write ( *, * ) 'Finite difference sensitivities will be computed.'
  end if

  if ( idfd == 0 ) then
    write ( *, * ) 'Direct cost finite differences will NOT be computed.'
  else
    write ( *, * ) 'Direct cost finite differences will be computed.'
  end if
!
!  Cost gradient approximation.
!
  write ( *, * ) ' '
  write ( *, * ) 'Cost gradient approximation option IGRAD = ',igrad
  if ( igrad == 0 ) then
    write ( *, * ) '  No cost gradient approximation is made.'
  else if (igrad == 1 ) then
    write ( *, * ) '  Chain rule on discretized sensitivities.'
  else if (igrad == 2 ) then
    write ( *, * ) '  Chain rule on finite difference sensitivities.'
  else if (igrad ==3 ) then
    write ( *, * ) '  Chain rule on corrected finite difference sensitivities.'
  else if (igrad ==4 ) then
    write ( *, * ) '  Direct finite differences.'
  end if

  write ( *, * ) ' '
  write ( *, * ) 'ITYPE = ',itype
!
!  ITYPE = 0, just the target.
!
  if ( itype == 0 ) then
    write ( *, * ) '  Just compute the target point.'
!
!  ITYPE = 1, 1D March
!
  else if (itype == 1 ) then

    write ( *, * ) ' '
    write ( *, * ) '  This is a 1D march.'
    write ( *, * ) ' '
    write ( *, * ) '  The first parameter set will be associated with'
    write ( *, * ) '  point number          ',istep1
    write ( *, * ) '  The second with point ',istep2
    write ( *, * ) ' '
    write ( *, * ) '  The first parameter set:'
    write ( *, * ) ' '
    call prpar(nparb,nparf,para1)
    write ( *, * ) ' '
    write ( *, * ) '  The second parameter set:'
    write ( *, * ) ' '
    call prpar(nparb,nparf,para2)
!
!  ITYPE = 2, 2D March
!
  else if (itype == 2 ) then

    write ( *, * ) ' '
    write ( *, * ) '  This is a 2D march.'
    write ( *, * ) ' '
    write ( *, * ) '  The first parameter set will be associated with'
    write ( *, * ) '  point number          ',istep1,jstep1
    write ( *, * ) '  The third with point  ',istep2,jstep2
    write ( *, * ) ' '
    write ( *, * ) '  The first parameter set:'
    write ( *, * ) ' '
    call prpar(nparb,nparf,para1)
    write ( *, * ) ' '
    write ( *, * ) '  The second parameter set:'
    write ( *, * ) ' '
    call prpar(nparb,nparf,para2)
    write ( *, * ) ' '
    write ( *, * ) '  Third parameter set:'
    write ( *, * ) ' '
    call prpar(nparb,nparf,para3)
!
!  ITYPE = 3, Optimization.
!
  else if (itype ==3 ) then

    write ( *, * ) ' '
    write ( *, * ) '  This is an optimization run.'
    write ( *, * ) '  TOLOPT, optimization tolerance = ',tolopt
    write ( *, * ) '  At most ',maxstp,' optimization steps will be used.'
    write ( *, * ) '  The starting parameter set:'
    write ( *, * ) ' '
    call prpar(nparb,nparf,para1)
!
!  ITYPE = 4, 3D March
!
  else if (itype ==4 ) then

    write ( *, * ) ' '
    write ( *, * ) 'This is a 3D march.'
    write ( *, * ) ' '
    write ( *, * ) 'The first parameter set will be associated with'
    write ( *, * ) 'point number          ',istep1,jstep1
    write ( *, * ) 'The third with point  ',istep2,jstep2
    write ( *, * ) ' '
    write ( *, * ) '  The first parameter set:'
    write ( *, * ) ' '
    call prpar(nparb,nparf,para1)
    write ( *, * ) ' '
    write ( *, * ) 'The second parameter set:'
    write ( *, * ) ' '
    call prpar(nparb,nparf,para2)
    write ( *, * ) ' '
    write ( *, * ) 'Third parameter set:'
    write ( *, * ) ' '
    call prpar(nparb,nparf,para3)

    write ( *, * ) ' '
    if ( nstep3>1 ) then
      write ( *, * ) 'The penalty parameter WATEB will vary from ', &
        wateb1,' to ',wateb2,' in ',nstep3,' steps.'
    else if (nstep3 == 1 ) then
      write ( *, * ) 'This is an unusual march, since NSTEP3 = 1.'
      write ( *, * ) 'The penalty parameter WATEB will be set to', &
        wateb1,' and cannot march to ',wateb2
    end if
    write ( *, * ) ' '
    write ( *, * ) 'The values of the functional will be written'
    write ( *, * ) 'to the text file:'
    write(*,'(5x,a)')filet
!
!  ITYPE = 5, one step Navier-Stokes solve.
!
  else if (itype ==5 ) then
    write ( *, * ) ' '
    write ( *, * ) 'This is a one-step Navier Stokes solution.'
    write ( *, * ) ' '
!
!  ITYPE = 7, Optimization.
!
  else if (itype ==7 ) then

    write ( *, * ) ' '
    write ( *, * ) '  This is an optimization run.'
    write ( *, * ) '  TOLOPT, optimization tolerance = ',tolopt
    write ( *, * ) '  At most ',maxstp,' optimization steps will be used.'
    write ( *, * ) '  The starting parameters:'
    write ( *, * ) ' '
    call prpar(nparb,nparf,para1)
    write ( *, * ) ' '
    write ( *, * ) '  Internal approximations to the gradient are made.'

  end if

  write ( *, * ) ' '
  write ( *, * ) 'IBC = ',ibc
  if ( ibc == 0 ) then
    write ( *, * ) '  Bump sensitivity boundary condition uses'
    write ( *, * ) '  finite element estimate of dUdY, dVdY.'
  else if (ibc == 1 ) then
    write ( *, * ) '  Bump sensitivity boundary condition uses'
    write ( *, * ) '  two point finite difference estimate of '
    write ( *, * ) '  dUdY, dVdY.'
  else if (ibc == 2 ) then
    write ( *, * ) '  Bump sensitivity boundary condition uses'
    write ( *, * ) '  three point finite difference estimate of '
    write ( *, * ) '  dUdY, dVdY.'
  else if (ibc ==3 ) then
    write ( *, * ) '  Bump sensitivity boundary condition uses'
    write ( *, * ) '  data from run on a finer grid.'
  end if

  write ( *, * ) ' '
  if ( ibump == 0 ) then
    write ( *, * ) 'IBUMP = 0:'
    write ( *, * ) '  No isoparametric elements will be used.'
    write ( *, * ) '  Y coordinates of midside nodes above the bump'
    write ( *, * ) '  will be adjusted to preserve straight sides.'
  else if (ibump == 1 ) then
    write ( *, * ) 'IBUMP = 1:'
    write ( *, * ) '  Isoparametric elements directly on bump.'
    write ( *, * ) '  Y coordinates of midside nodes above the bump'
    write ( *, * ) '  will be adjusted to preserve straight sides.'
  else if (ibump == 2 ) then
    write ( *, * ) 'IBUMP = 2:'
    write ( *, * ) '  All elements above bump are isoparametric.'
    write ( *, * ) '  Y coordinates of midside nodes above the bump'
    write ( *, * ) '  need not lie on a straight line.'
  else if (ibump ==3 ) then
    write ( *, * ) 'IBUMP = 3:'
    write ( *, * ) '  All elements are isoparametric.'
    write ( *, * ) '  Y coordinates of midside nodes above the bump'
    write ( *, * ) '  need not lie on a straight line.'
  else
    write ( *, * ) ' '
    write ( *, * ) 'PrDat - Fatal error!'
    write ( *, * ) '  Unexpected value of IBUMP = ',ibump
    stop
  end if

  write ( *, * ) ' '
  if ( iplot<0 ) then
    write ( *, * ) '  DISPLAY graphics written for target and '
    write ( *, * ) '    last points.'
  else if (iplot == 0 ) then
    write ( *, * ) '  DISPLAY graphics file is not written.'
  else
    write ( *, * ) '  DISPLAY graphics written every ',iplot,' points.'
  end if

  if ( iplot/= 0 ) then
    write ( *, * ) '  Graphics data will be written to the file:'
    write(*,'(1x,a)')fileg
  end if

  write ( *, * ) '  Diagnostic output option, IWRITE = ',iwrite
  write ( *, * ) ' '
  write ( *, * ) '  The flow system to be solved is '//syseqn

  return
end
subroutine prgs2(area,gdif,gdifc,gradf,indx,iopt,maxeqn,nelem,np,npar,nquad, &
  ny,sens,xc,yc)
!
!*******************************************************************************
!
!! PRGS2 prints out the maximum entries in the finite difference and
!  sensitivity vectors, and the maximum difference.
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
  integer maxeqn
  integer nelem
  integer np
  integer npar
  integer nquad
!
  real ( kind = 8 ) area(nquad,nelem)
  real ( kind = 8 ) flarea
  real ( kind = 8 ) gdif(maxeqn,npar)
  real ( kind = 8 ) gdifc(maxeqn,npar)
  real ( kind = 8 ) gradf(maxeqn,npar)
  integer i
  integer icol
  integer ielem
  integer ieqn
  integer indx(3,np)
  integer iopt(npar)
  integer ipar
  integer ipmax
  integer irow
  integer iumax
  integer ivmax
  integer j
  character ( len = 15 ) label
  integer npmax
  integer numax
  integer nvmax
  integer ny
  real ( kind = 8 ) pmax
  real ( kind = 8 ) psum
  real ( kind = 8 ) sens(maxeqn,npar)
  real ( kind = 8 ) umax
  real ( kind = 8 ) usum
  real ( kind = 8 ) val
  real ( kind = 8 ) vmax
  real ( kind = 8 ) vsum
  real ( kind = 8 ) xc(np)
  real ( kind = 8 ) yc(np)
!
!  Compute the current flow area.
!
  flarea = 0.0D+00
  do ielem = 1,nelem
    do i = 1,nquad
      flarea = flarea+area(i,ielem)
    end do
  end do
!
  write ( *, * ) ' '
  write ( *, * ) 'PrGS2, compute Maximum (Sensitivity-Finite Difference)'
  write ( *, * ) ' '
  write ( *, * ) '          Par  Var  L1-Norm  Max-Norm  Index  Node  ' // &
    'X       Y  Row  Column'
  do i = 1,6

    write ( *, * ) ' '
    if ( i == 1 ) then
      label = 'Sens'
    else if (i == 2 ) then
      label = 'FD'
    else if (i ==3 ) then
      label = 'FD-FDFix'
    else if (i ==4 ) then
      label = 'FDFix'
    else if (i ==5 ) then
      label = 'FD-Sens'
    else if (i ==6 ) then
      label = 'FD-FDFix-Sens'
    end if

    do ipar = 1,npar

      if ( iopt(ipar) == 1 ) then

        ieqn = 0

        ipmax = 0
        iumax = 0
        ivmax = 0

        npmax = 0
        numax = 0
        nvmax = 0

        pmax = 0.0D+00
        umax = 0.0D+00
        vmax = 0.0D+00

        psum = 0.0D+00
        usum = 0.0D+00
        vsum = 0.0D+00

        do j = 1,np

          ieqn = ieqn+1

          if ( i == 1 ) then
            val = sens(ieqn,ipar)
          else if (i == 2 ) then
            val = gdif(ieqn,ipar)
          else if (i ==3 ) then
            val = gdifc(ieqn,ipar)
          else if (i ==4 ) then
            val = gradf(ieqn,ipar)
          else if (i ==5 ) then
            val = sens(ieqn,ipar)-gdif(ieqn,ipar)
          else if (i ==6 ) then
            val = sens(ieqn,ipar)-gdifc(ieqn,ipar)
          end if

          usum = usum+abs(val)

          if ( abs(val)>= umax ) then
            umax = abs(val)
            numax = j
            iumax = ieqn
          end if

          ieqn = ieqn+1

          if ( i == 1 ) then
            val = sens(ieqn,ipar)
          else if (i == 2 ) then
            val = gdif(ieqn,ipar)
          else if (i ==3 ) then
            val = gdifc(ieqn,ipar)
          else if (i ==4 ) then
            val = gradf(ieqn,ipar)
          else if (i ==5 ) then
            val = sens(ieqn,ipar)-gdif(ieqn,ipar)
          else if (i ==6 ) then
            val = sens(ieqn,ipar)-gdifc(ieqn,ipar)
          end if

          vsum = vsum+abs(val)

          if ( abs(val)>= vmax ) then
            vmax = abs(val)
            nvmax = j
            ivmax = ieqn
          end if

          if ( indx(3,j)>0 ) then

            ieqn = ieqn+1

            if ( i == 1 ) then
              val = sens(ieqn,ipar)
            else if (i == 2 ) then
              val = gdif(ieqn,ipar)
            else if (i ==3 ) then
              val = gdifc(ieqn,ipar)
            else if (i ==4 ) then
              val = gradf(ieqn,ipar)
            else if (i ==5 ) then
              val = sens(ieqn,ipar)-gdif(ieqn,ipar)
            else if (i ==6 ) then
              val = sens(ieqn,ipar)-gdifc(ieqn,ipar)
            end if

            psum = psum+abs(val)

            if ( abs(val)>= pmax ) then
              pmax = abs(val)
              npmax = j
              ipmax = ieqn
            end if

          end if

        end do
!
!  Normalize L1 quantities by dividing by current flow area.
!
        usum = usum/flarea
        vsum = vsum/flarea
        psum = psum/flarea

        write ( *, * ) ' '

        icol = ((numax-1)/(2*ny-1))+1
        irow = numax-(icol-1)*(2*ny-1)
        write(*,'(a15,1x,i1,1x,a1,2g11.3,2i6,2f8.3,2i4)')label,ipar,'U',usum, &
          umax,iumax,numax,xc(numax),yc(numax),irow,icol

        icol = ((nvmax-1)/(2*ny-1))+1
        irow = nvmax-(icol-1)*(2*ny-1)
        write(*,'(a15,1x,i1,1x,a1,2g11.3,2i6,2f8.3,2i4)')label,ipar,'V',vsum, &
          vmax,ivmax,nvmax,xc(nvmax),yc(nvmax),irow,icol

        icol = ((npmax-1)/(2*ny-1))+1
        irow = npmax-(icol-1)*(2*ny-1)
        write(*,'(a15,1x,i1,1x,a1,2g11.3,2i6,2f8.3,2i4)')label,ipar,'P',psum, &
          pmax,ipmax,npmax,xc(npmax),yc(npmax),irow,icol

      end if

    end do

  end do

  return
end
subroutine probas(base,m,n)
!
!*******************************************************************************
!
!! PROBAS accepts N vectors of length M, when N <=  M, and orthonormalizes the vectors.
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
!  BASE   Input/output, real BASE(M,N).
!
!         On input, the columns of BASE contain N vectors, each of
!         length M, which span a space of dimension N.
!
!         On output, the columns of BASE form a basis for the same
!         space, that is, the columns are of unit Euclidean norm, and
!         orthogonal.
!
!  M      Input, integer M, the dimension of the higher order space.
!
!  N      Input, integer N, the dimension of the lower order space.
!
!
  integer m
  integer n
!
  real ( kind = 8 ) base(m,n)
  integer i
  integer j
  integer k
  real ( kind = 8 ) temp
!
!  For each column J:
!
  do j = 1,n
!
!  ...consider a previous column, I, ...
    do i = 1,j-1
!
!  ...compute the projection of column J onto column I...
!
      temp = 0.0D+00
      do k = 1,m
        temp = temp+base(k,i)*base(k,j)
      end do
!
!  ...subtract off this projection...
!
      do k = 1,m
        base(k,j) = base(k,j)-temp*base(k,i)
      end do
    end do
!
!  Then compute the Euclidean norm of what's left of column J...
!
    temp = 0.0D+00
    do i = 1,m
      temp = temp+base(i,j)**2
    end do
    temp = sqrt(temp)
!
!  ...and normalize the column.
!
    if ( temp>0.0D+00 ) then
      do i = 1,m
        base(i,j) = base(i,j)/temp
      end do
    else
      write ( *, * ) 'ProBas - Warning!'
      write ( *, * ) '  Column ',j,' of the basis is now 0.'
    end if

  end do

  return
end
subroutine projec(base,m,n,vecm,vecn)
!
!*******************************************************************************
!
!! PROJEC computes the projection of an M vector into an N dimensional subspace.
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
!  BASE   Input, real BASE(M,N).  The columns of BASE
!         contain N vectors, each of length M, which form the
!         orthonormal basis for a space of dimension N.
!
!  M      Input, integer M, the dimension of the higher order space.
!
!  N      Input, integer N, the dimension of the lower order space.
!
!  VECM   Input, real VECM(M), is an M dimensional vector.
!
!  VECN   Output, real VECN(N), the projection of VECM into
!         the lower dimensional space.  These values represent
!         coordinates in the lower order space.
!
!
  integer m
  integer n
!
  real ( kind = 8 ) base(m,n)
  integer j
  integer k
  real ( kind = 8 ) vecm(m)
  real ( kind = 8 ) vecn(n)
!
!  The J-th coordinate of the projection of the vector
!  is simply the dot product of the vector with basis vector J.
!
  do j = 1,n
    vecn(j) = 0.0D+00
    do k = 1,m
      vecn(j) = vecn(j)+vecm(k)*base(k,j)
    end do
  end do

  return
end
subroutine prpar(nparb,nparf,para)
!
!*******************************************************************************
!
!! PRPAR prints out the current parameters.
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
  integer nparb
  integer nparf
!
  integer i
  integer ihi
  integer ilo
  real ( kind = 8 ) para(nparf+nparb+1)
!
  write ( *, * ) ' '

  do ilo = 1,nparf,5

    ihi = min(ilo+4,nparf)

    if ( ilo == 1 ) then
      write(*,'(''  Inflow '',5g14.6)')(para(i),i = ilo,ihi)
    else
      write(*,'(''         '',5g14.6)')(para(i),i = ilo,ihi)
    end if

  end do

  do ilo = nparf+1,nparf+nparb,5

    ihi = min(nparf+ilo+4,nparf+nparb)

    if ( ilo ==nparf+1 ) then
      write(*,'(''  Bump   '',5g14.6)')(para(i),i = ilo,ihi)
    else
      write(*,'(''         '',5g14.6)')(para(i),i = ilo,ihi)
    end if

  end do

  write(*,'(''  NU_INV '',5g14.6)')para(nparf+nparb+1)

  return
end
subroutine prpuv(g,indx,neqn,np,pnorm,unorm,vnorm)
!
!*******************************************************************************
!
!! PRPUV prints out the nodal values of the finite differences and sensitivities.
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
  integer neqn
  integer np
!
  real ( kind = 8 ) g(neqn)
  integer i
  integer indx(3,np)
  integer j
  real ( kind = 8 ) pnorm
  real ( kind = 8 ) unorm
  real ( kind = 8 ) vnorm
!
  pnorm = 0.0D+00
  unorm = 0.0D+00
  vnorm = 0.0D+00

  do i = 1,np

    j = indx(1,i)
    unorm = max(unorm,abs(g(j)))

    j = indx(2,i)
    vnorm = max(vnorm,abs(g(j)))

    j = indx(3,i)
    if ( j>0 ) then
      pnorm = max(pnorm,abs(g(j)))
    end if

  end do

  return
end
subroutine prsol(dpara3,dparfd,dparfdc,dparsn,g,g2,gdif,gdifc,idfd,ids,ifds, &
  indx,iwrite,maxeqn,neqn,np,npar,nparb,nparf,numstp,para,sens)
!
!*******************************************************************************
!
!! PRSOL prints out information about a single solution.
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
  integer maxeqn
  integer neqn
  integer np
  integer npar
  integer nparb
  integer nparf
!
  real ( kind = 8 ) dpara3(npar)
  real ( kind = 8 ) dparfd(npar)
  real ( kind = 8 ) dparfdc(npar)
  real ( kind = 8 ) dparsn(npar)
  real ( kind = 8 ) g(neqn)
  real ( kind = 8 ) g2(neqn)
  real ( kind = 8 ) gdif(maxeqn,npar)
  real ( kind = 8 ) gdifc(maxeqn,npar)
  integer i
  integer idfd
  integer ids
  integer ifds
  integer indx(3,np)
  integer iwrite
  integer j
  integer numstp
  real ( kind = 8 ) para(npar)
  real ( kind = 8 ) pnorm
  real ( kind = 8 ) sens(maxeqn,npar)
  character ( len = 70 ) title
  real ( kind = 8 ) unorm
  real ( kind = 8 ) vnorm
!
!  Print out stuff.
!
  if ( iwrite>= 2 ) then

    write ( *, * ) ' '
    write ( *, * ) 'Parameters at step ',numstp

    call prpar(nparb,nparf,para)
  end if
!
!  Temporary stuff.
!
  if ( iwrite>= 3 ) then

    call prpuv(g,indx,neqn,np,pnorm,unorm,vnorm)

    write ( *, * ) ' '
    write ( *, * ) 'U norm = ',unorm
    write ( *, * ) 'V norm = ',vnorm
    write ( *, * ) 'P norm = ',pnorm

    do i = 1,npar

      write ( *, * ) ' '
      write ( *, * ) 'Parameter ',i

      call prpuv(sens(1,i),indx,neqn,np,pnorm,unorm,vnorm)

      write ( *, * ) ' '
      write(*,'(a,3g14.6)')'Disc Sens:',unorm,vnorm,pnorm

      call prpuv(gdif(1,i),indx,neqn,np,pnorm,unorm,vnorm)

      write(*,'(a,3g14.6)')'Fin Dif  :',unorm,vnorm,pnorm

      call prpuv(gdifc(1,i),indx,neqn,np,pnorm,unorm,vnorm)

      write(*,'(a,3g14.6)')'Fin Dif C:',unorm,vnorm,pnorm

      do j = 1,neqn
        g2(j) = sens(j,i)-gdifc(j,i)
      end do

      call prpuv(g2,indx,neqn,np,pnorm,unorm,vnorm)

      write(*,'(a,3g14.6)')'DS-FDC:   ',unorm,vnorm,pnorm
    end do

  end if
!
!  Print finite difference sensitivity.
!
  if ( ifds/= 0 ) then

    if ( iwrite>= 2 ) then
      title = 'Chain rule on finite difference sensitivities:'
      call prcsen(dparfd,npar,title)
    end if

  end if
!
!  Print adjustment to finite difference sensitivity.
!
  if ( ifds/= 0 ) then

    if ( iwrite>= 2 ) then
      title = 'Chain rule on corrected finite difference sens:'
      call prcsen(dparfdc,npar,title)
    end if

  end if
!
!  Print discretized sensitivity.
!
  if ( ids/= 0 ) then

    if ( iwrite>= 2 ) then
      title = 'Chain rule on discretized sensitivities:'
      call prcsen(dparsn,npar,title)
    end if

  end if
!
!  Print direct finite difference.
!
  if ( idfd/= 0 ) then
    if ( iwrite>= 2 ) then
      title = 'Direct finite differences:'
      call prcsen(dpara3,npar,title)
    end if
  end if

  return
end
subroutine prspl(nspl,spl,tau)
!
!*******************************************************************************
!
!! PRSPL prints the raw spline data for the simple cases ISHAPE = 1 or 2.
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
  integer nspl
!
  integer i
  real ( kind = 8 ) spl(nspl)
  real ( kind = 8 ) tau(nspl)
!
  write ( *, * ) ' '
  write ( *, * ) 'PrSpl'
  write ( *, * ) ' '
  write ( *, * ) 'Raw spline data.'
  write ( *, * ) ' '
  write ( *, * ) 'I, TAU(I), SPL(I)'

  do i = 1,nspl
    write ( *, * ) i,tau(i),spl(i)
  end do

  return
end
subroutine prspln(jderiv,ipar,ishape,npts,npar,spl,tau,xhi,xlo)
!
!*******************************************************************************
!
!! PRSPLN prints out the value of a spline function or one of its
!  derivatives at evenly spaced points between given limits.
!
!  It can also print out the value or derivatives of one of the
!  "Lagrangian" spline functions, associated with any of NPAR
!  parameters.
!
!  The IPAR-th Lagrangian spline is 1 at the point associated with
!  the IPAR-th parameter, and 0 at the points associated with the
!  other parameters.  Using the notation S(IPAR)(X) for the value
!  of the IPAR-th Lagrangian spline at the point X, we can write:
!
!    S(X) = Sum (IPAR=1 to NPAR) C(IPAR) * S(IPAR)(X)
!
!  where S(X) is our resulting spline, which has the value C(IPAR)
!  at the IPAR-th point.
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
  integer npar
!
  integer i
  integer ipar
  integer ishape
  integer jderiv
  integer npts
  real ( kind = 8 ) spl(4,npar+2,0:npar)
  real ( kind = 8 ) tau(npar+2)
  real ( kind = 8 ) xhi
  real ( kind = 8 ) xlo
  real ( kind = 8 ) xx
  real ( kind = 8 ) yvec(npar+2)
  real ( kind = 8 ) yy
!
  intrinsic real
!
  if ( npts<1 ) then
    write ( *, * ) ' '
    write ( *, * ) 'PrSpln - Serious error!'
    write ( *, * ) '  NPTS must be at least 1, but you entered ',npts
    return
  end if

  write ( *, * ) ' '
  write ( *, * ) 'PrSpln:'
  write ( *, * ) ' '

  do i = 1,npts

    if ( npts == 1 ) then
      xx = 0.5 * (xlo+xhi)
    else
      xx = ( real(npts-i, kind = 8 )*xlo + real(i-1, kind = 8 )*xhi ) / real(npts-1, kind = 8 )
    end if

    if ( ishape == 1 ) then

      if ( ipar == 0 ) then

        if ( jderiv == 0 ) then
          yvec(1:npar+2) = spl(1,1:npar+2,0)
          call plval(npar+2,xx,tau,yy,yvec)
        else
          yvec(1:npar+2) = spl(1,1:npar+2,0)
          call pldx(npar+2,xx,tau,yy,yvec)
        end if
      else
        call plval1(ipar+1,npar+2,xx,tau,yy)
      end if

    else if (ishape == 2 ) then

      if ( ipar == 0 ) then
        if ( jderiv == 0 ) then
          call pqval(npar+2,xx,tau,yy,spl)
        else
          call pqdx(npar+2,xx,tau,yy,spl)
        end if
      else
        call pqval1(ipar+1,npar+2,xx,tau,yy)
      end if

    else if (ishape ==3 ) then

      call ppvalu(tau,spl(1,1,ipar),npar+1,4,xx,jderiv,yy)

    end if

    write(*,'(2g12.4)')xx,yy

  end do

  return
end
subroutine qbf(ielem,in,w,dwdx,dwdy,nelem,node,np,xc,xq,yc,yq)
!
!*******************************************************************************
!
!! QBF evaluates a particular quadratic basis function at a point
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
!    03 April 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!  IELEM  Input, integer IELEM, the number of the element we are
!         examining.  This will be a value between 1 and NELEM.
!
!  IN     Input, integer IN, the number of the basis function we
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
!  NELEM  Input, integer NELEM, the number of elements.
!
!  NODE   Input, integer NODE(6,NELEM), contains the numbers
!         of the nodes that make up each element.  Element number
!         I is associated with nodes NODE(1,I) through NODE(6,I).
!
!  NP     Input, integer NP, the number of nodes.
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
!
  integer nelem
  integer np
!
  real ( kind = 8 ) c
  real ( kind = 8 ) d
  real ( kind = 8 ) dwdx
  real ( kind = 8 ) dwdy
  integer i1
  integer i2
  integer i3
  integer ielem
  integer in
  integer in1
  integer in2
  integer in3
  integer node(6,nelem)
  real ( kind = 8 ) s
  real ( kind = 8 ) t
  real ( kind = 8 ) w
  real ( kind = 8 ) xc(np)
  real ( kind = 8 ) xq
  real ( kind = 8 ) yc(np)
  real ( kind = 8 ) yq
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
  if ( 1<= in.and.in<=3 ) then

    in1 = in
    in2 = mod(in,3)+1
    in3 = mod(in+1,3)+1

    i1 = node(in1,ielem)
    i2 = node(in2,ielem)
    i3 = node(in3,ielem)

    d = (xc(i2)-xc(i1))*(yc(i3)-yc(i1))-(xc(i3)-xc(i1))*(yc(i2)-yc(i1))

    if ( d == 0.0 ) then
      write ( *, * ) ' '
      write ( *, * ) 'QBF - Fatal error!'
      write ( *, * ) '  D = 0'
      write ( *, * ) '  Element IELEM = ',ielem
      write ( *, * ) '  I1, XC(I1), YC(I1) = ',i1,xc(i1),yc(i1)
      write ( *, * ) '  I2, XC(I2), YC(I2) = ',i2,xc(i2),yc(i2)
      write ( *, * ) '  I3, XC(I3), YC(I3) = ',i3,xc(i3),yc(i3)
      stop
    end if

    t = 1.0D+00+( (xq    -xc(i1))*(yc(i2)-yc(i3))+(xc(i3)-xc(i2))*(yq    -yc(i1)) )/d

    w = t*(2.0D+00*t-1.0D+00)

    dwdx = (yc(i2)-yc(i3))*(4.0D+00*t-1.0D+00)/d
    dwdy = (xc(i3)-xc(i2))*(4.0D+00*t-1.0D+00)/d
!
!  Case 2: We are inquiring about a basis function associated
!  with a midpoint.
!
  else if (in>= 4.and.in<=6 ) then

    in1 = in-3
    in2 = mod(in-3,3)+1
    in3 = mod(in-2,3)+1

    i1 = node(in1,ielem)
    i2 = node(in2,ielem)
    i3 = node(in3,ielem)

    d =     (xc(i2)-xc(i1))*(yc(i3)-yc(i1))-(xc(i3)-xc(i1))*(yc(i2)-yc(i1))

    if ( d == 0.0D+00 ) then
      write ( *, * ) ' '
      write ( *, * ) 'QBF - Fatal error!'
      write ( *, * ) '  D = 0'
      write ( *, * ) '  Element IELEM = ',ielem
      write ( *, * ) '  I1, XC(I1), YC(I1) = ',i1,xc(i1),yc(i1)
      write ( *, * ) '  I2, XC(I2), YC(I2) = ',i2,xc(i2),yc(i2)
      write ( *, * ) '  I3, XC(I3), YC(I3) = ',i3,xc(i3),yc(i3)
      stop
    end if

    c =     (xc(i3)-xc(i2))*(yc(i1)-yc(i2))-(xc(i1)-xc(i2))*(yc(i3)-yc(i2))

    if ( c == 0.0D+00 ) then
      write ( *, * ) ' '
      write ( *, * ) 'QBF - Fatal error!'
      write ( *, * ) '  C = 0'
      write ( *, * ) '  Element IELEM = ',ielem
      write ( *, * ) '  I1, XC(I1), YC(I1) = ',i1,xc(i1),yc(i1)
      write ( *, * ) '  I2, XC(I2), YC(I2) = ',i2,xc(i2),yc(i2)
      write ( *, * ) '  I3, XC(I3), YC(I3) = ',i3,xc(i3),yc(i3)
      stop
    end if

    t = 1.0D+00+( (xq    -xc(i1))*(yc(i2)-yc(i3))+(xc(i3)-xc(i2))*(yq    -yc(i1)) )/d

    s = 1.0D+00+( (xq    -xc(i2))*(yc(i3)-yc(i1))+(xc(i1)-xc(i3))*(yq    -yc(i2)) )/c

    w = 4.0D+00 * s*t
    dwdx = 4.0D+00 * ((yc(i3)-yc(i1))*t/c + (yc(i2)-yc(i3))*s/d)
    dwdy = 4.0D+00 * ((xc(i1)-xc(i3))*t/c + (xc(i3)-xc(i2))*s/d)

  else

    write ( *, * ) ' '
    write ( *, * ) 'QBF - Fatal error!'
    write ( *, * ) '  Request for basis function IN = ',in
    write ( *, * ) '  but IN must be between 1 and 6.'
    stop

  end if

  return
end
subroutine qsolve(a,area,dopt,dpara3,dparfd,dparfdc,dparsn,dpdyn,dudyn,dvdyn, &
  dydpn,epsdif,eqn,etan,etaq,g,g1,g2,gdif,gdifc,gold,gopt,gradf,gtar,ibc,ifs, &
  ibs,idfd,ids,ifds,igrad,igunit,ijac,indx,iopt,ipivot,iplot,isotri,itunit, &
  itype,ivopt,iwrite,jjac,liv,lv,maxeqn,maxnew,maxstp,nelem,neqn,nlband,node, &
  nopt,np,npar,nparb,nparf,npe,nprof,nquad,nrow,numel,nx,ny,para,phi,res,sens, &
  splbmp,splflo,syseqn,taubmp,tauflo,tolnew,tolopt,vopt,wateb,watep,wateu, &
  watev,wquad,xbl,xbr,xc,xopt,xprof,xquad,xsin,xsiq,ybl,ybr,yc,yquad)
!
!*******************************************************************************
!
!! QSOLVE carries out the optimization algorithm for a fixed grid,
!  and computes finite differences or sensitivities to use in the
!  optimization.
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
  integer liv
  integer lv
  integer maxeqn
  integer nelem
  integer neqn
  integer np
  integer npar
  integer nparb
  integer nparf
  integer nquad
  integer nrow
  integer nx
  integer ny
!
  real ( kind = 8 ) a(nrow,neqn)
  real ( kind = 8 ) area(nquad,nelem)
  real ( kind = 8 ) cgrad
  real ( kind = 8 ) cost
  real ( kind = 8 ) costb
  real ( kind = 8 ) costp
  real ( kind = 8 ) costu
  real ( kind = 8 ) costv
  real ( kind = 8 ) dopt(npar)
  real ( kind = 8 ) dpara3(npar)
  real ( kind = 8 ) dparfd(npar)
  real ( kind = 8 ) dparfdc(npar)
  real ( kind = 8 ) dparsn(npar)
  real ( kind = 8 ) dpdyn(np)
  real ( kind = 8 ) dudyn(np)
  real ( kind = 8 ) dvdyn(np)
  real ( kind = 8 ) dydpn(np,nparb)
  real ( kind = 8 ) epsdif
  character ( len = 2 ) eqn(neqn)
  real ( kind = 8 ) etan(6)
  real ( kind = 8 ) etaq(nquad)
  real ( kind = 8 ) g(neqn)
  real ( kind = 8 ) g1(neqn)
  real ( kind = 8 ) g2(neqn)
  real ( kind = 8 ) gdif(maxeqn,npar)
  real ( kind = 8 ) gdifc(maxeqn,npar)
  real ( kind = 8 ) gold(neqn)
  real ( kind = 8 ) gopt(npar)
  real ( kind = 8 ) gradf(maxeqn,npar)
  real ( kind = 8 ) gtar(neqn)
  integer i
  integer ibc
  integer ifs
  integer ibs
  integer idfd
  integer ids
  integer ierror
  integer ifds
  integer igrad
  integer igunit
  integer ijac
  integer indx(3,np)
  integer info
  integer iopt(npar)
  integer ipivot(neqn)
  integer iplot
  integer isotri(nelem)
  integer itunit
  integer itype
  integer ival
  integer ivopt(liv)
  integer iwrite
  integer j
  integer jjac
  integer jopt
  logical lmat
  integer maxnew
  integer maxstp
  integer nfail
  integer nlband
  integer node(6,nelem)
  integer nopt
  integer npe
  integer nprof(2*ny-1)
  integer numel(np)
  integer numstp
  real ( kind = 8 ) para(npar)
  real ( kind = 8 ) phi(nquad,6,10,nelem)
  real ( kind = 8 ) res(neqn)
  real ( kind = 8 ) sens(maxeqn,npar)
  real ( kind = 8 ) splbmp(4,nparb+2,0:nparb)
  real ( kind = 8 ) splflo(4,nparf+2,0:nparf)
  character ( len = 20 ) syseqn
  real ( kind = 8 ) taubmp(nparb+2)
  real ( kind = 8 ) tauflo(nparf+2)
  character ( len = 70 ) title
  real ( kind = 8 ) tolnew
  real ( kind = 8 ) tolopt
  real ( kind = 8 ) vopt(lv)
  real ( kind = 8 ) wateb
  real ( kind = 8 ) watep
  real ( kind = 8 ) wateu
  real ( kind = 8 ) watev
  real ( kind = 8 ) wquad(nquad)
  real ( kind = 8 ) xbl
  real ( kind = 8 ) xbr
  real ( kind = 8 ) xc(np)
  real ( kind = 8 ) xopt(npar)
  real ( kind = 8 ) xprof
  real ( kind = 8 ) xquad(nquad,nelem)
  real ( kind = 8 ) xsin(6)
  real ( kind = 8 ) xsiq(nquad)
  real ( kind = 8 ) ybl
  real ( kind = 8 ) ybr
  real ( kind = 8 ) yc(np)
  real ( kind = 8 ) yquad(nquad,nelem)
!
!  Initialize some things.
!
  nfail = 0
  numstp = 0
!
!  Set default values for the optimizer, and override some of them.
!
  ival = 2
  call deflt(ival,ivopt,liv,lv,vopt)

  if ( tolopt>0.0D+00 ) then
    vopt(31) = tolopt
    vopt(32) = tolopt
    vopt(33) = tolopt
    vopt(34) = tolopt
    vopt(37) = tolopt
  end if

  ivopt(1) = 12
  ivopt(19) = 0
  g(1:neqn) = 0.0D+00
  gold(1:neqn) = 0.0D+00

  do i = 1,npar
    dpara3(i) = 0.0D+00
    dparfd(i) = 0.0D+00
    dparfdc(i) = 0.0D+00
    dparsn(i) = 0.0D+00
  end do
!
!  Optimization loop
!
10    continue
!
!  Call the optimizer to get a new set of parameter values, PARA.
!
  cgrad = 0.0D+00
  jopt = 0
  do i = 1,npar

    if ( iopt(i) == 1 ) then

      jopt = jopt+1
      xopt(jopt) = para(i)

      if ( itype ==3 ) then
        if ( igrad == 1 ) then
          gopt(jopt) = dparsn(i)
        else if (igrad == 2 ) then
          gopt(jopt) = dparfd(i)
        else if (igrad ==3 ) then
          gopt(jopt) = dparfdc(i)
        else if (igrad ==4 ) then
          gopt(jopt) = dpara3(i)
        end if

        cgrad = max(cgrad,abs(gopt(jopt)))

      end if

    end if

  end do

11    continue

  if ( itype ==3 ) then
    call sumit(dopt,cost,gopt,ivopt,liv,lv,nopt,vopt,xopt)
  else if (itype ==7 ) then
    call snoit(dopt,cost,ivopt,liv,lv,nopt,vopt,xopt)
  end if

  jopt = 0
  do i = 1,npar
    if ( iopt(i) == 1 ) then
      jopt = jopt+1
      para(i) = xopt(jopt)
    end if
  end do
!
!  Did the optimizer converge, so that we're done?
!
  if ( ivopt(1)>= 3.and.ivopt(1)<=8 ) then

    write ( *, * ) ' '
    write ( *, * ) 'QSolve - Convergence to a minimizer was achieved!'
    write ( *, * ) '  IVOPT(1) = ',ivopt(1)

    go to 20
!
!  ...or did the optimizer find a serious problem, so that we're
!  "finished"?
!
  else if (ivopt(1)>8 ) then

    write ( *, * ) ' '
    write ( *, * ) 'QSolve - Warning!'
    write ( *, * ) '  IVOPT(1) = ',ivopt(1)
    go to 20
!
!  The optimizer has not converged, and it hasn't had a failure.
!
!  The optimizer has returned a new, rough, guess for the
!  minimizer, and wants us to evaluate the cost function there.
!
!  To do this, we must find the flow solution (U,V,P) corresponding
!  to the given parameters.
!
  else if (ivopt(1) == 1.or.(ivopt(1)== 2.and.itype==7) ) then

    numstp = numstp+1
!
!  Get the flow solution (U,V,P).
!
    call flosol(a,area,eqn,etaq,g,g2,ifs,ibs,ierror,ijac,indx,ipivot,isotri, &
      iwrite,jjac,maxnew,nelem,neqn,nlband,node,np,npar,nparb,nparf,nquad, &
      nrow,nx,ny,para,phi,res,splbmp,splflo,syseqn,taubmp,tauflo,tolnew, &
      wquad,xbl,xbr,xc,xquad,xsiq,ybl,ybr,yc,yquad)
!
!  Get the cost function J.
!
    call getcst(cost,costb,costp,costu,costv,g,gtar,ibs,indx,neqn,np,nparb, &
      nprof,ny,splbmp,taubmp,wateb,watep,wateu,watev,xbl,xbr,ybl,ybr,yc)

    if ( ivopt(1) == 2.and.itype==7)go to 10
!
!  Get the finite difference sensitivities dU/dPARA.
!
    call getgrd(a,area,cost,dpara3,epsdif,eqn,etaq,g,g1,g2,gdif,gtar,ifs,ibs, &
      ierror,ijac,indx,iopt,ipivot,isotri,iwrite,jjac,maxeqn,maxnew,nelem, &
      neqn,nlband,node,np,npar,nparb,nparf,nprof,nquad,nrow,nx,ny,para,phi, &
      res,splbmp,splflo,syseqn,taubmp,tauflo,tolnew,wateb,watep,wateu,watev, &
      wquad,xbl,xbr,xc,xquad,xsiq,ybl,ybr,yc,yquad)
!
!  Get dPdY, dUdY and dVdY, compute the correction term
!  GRADF, and the GDIFC, the corrected finite difference estimate
!  of the sensitivity.
!
    call getdu(dpdyn,dudyn,dvdyn,etan,g,indx,isotri,nelem,neqn,node,np,numel, &
      xc,xsin,yc)

    call getfix(dpdyn,dudyn,dvdyn,dydpn,gradf,ibs,indx,iopt,maxeqn,np,npar, &
      nparb,nparf,splbmp,taubmp,xbl,xbr,xc,yc)

    do i = 1,neqn
      do j = 1,npar
        gdifc(i,j) = gdif(i,j)-gradf(i,j)
      end do
    end do
!
!  Get the discretized sensitivities.
!
    lmat = .false.
    call lmemry('get','have_fp',lmat)

    if ( .not.lmat ) then

      call fp(a,area,eqn,g,indx,nelem,neqn,nlband,node, &
        np,npar,nparb,nparf,nquad,nrow,para,phi,syseqn)

      call sgbtrf(neqn,neqn,nlband,nlband,a,nrow,ipivot,info)

      if ( info/= 0 ) then
        write ( *, * ) ' '
        write ( *, * ) 'QSolve - Fatal error!'
        write ( *, * ) '  Jacobian factorization failed.'
        write ( *, * ) '  SGBTRF returns nonzero INFO = ',info
      else
        lmat = .true.
        call lmemry('set','have_fp',lmat)
      end if
    end if

    if ( lmat ) then
      call getsen(a,area,dudyn,dvdyn,eqn,g,ibc,ifs,ibs,indx,iopt,ipivot, &
        maxeqn,nelem,neqn,nlband,node,np,npar,nparb,nparf,nquad,nrow, &
        phi,sens,splbmp,splflo,taubmp,tauflo,xc,yc)
    end if

    if ( ierror/= 0 ) then
      nfail = nfail+1
      if ( nfail<= 1 ) then
        write ( *, * ) ' '
        write ( *, * ) 'QSolve - Warning!'
        write ( *, * ) '  SolCon returns IERROR = ',ierror
        write ( *, * ) '  Requesting that SUMIT try a smaller step.'
        ivopt(2) = 1
        go to 11
      else
        write ( *, * ) ' '
        write ( *, * ) 'QSolve - Fatal error!'
        write ( *, * ) '  SolCon returns IERROR = ',ierror
        return
      end if
    else
      nfail = 0
    end if

    gold(1:neqn) = g(1:neqn)
!
!  Print stuff out.
!
    if ( iwrite<2 ) then
      write ( *, * ) ' '
      write ( *, * ) 'QSolve - Point ',numstp
      call prpar(nparb,nparf,para)
    end if

    if ( iwrite == 1 ) then
      write ( *, * ) 'Cost: ',cost
    else if (iwrite>= 2 ) then
      title = 'Cost'
      call prcost2(cost,costb,costp,costu,costv,title,wateb,watep,wateu,watev)
    end if

    call getder(dparfd,g,gtar,ibs,indx,maxeqn,neqn,np,npar,nparb,nparf,nprof, &
      ny,gdif,splbmp,taubmp,wateb,watep,wateu,watev,xbl,xbr,ybl,ybr,yc)

    call getder(dparfdc,g,gtar,ibs,indx,maxeqn,neqn,np,npar,nparb,nparf, &
      nprof,ny,gdifc,splbmp,taubmp,wateb,watep,wateu,watev,xbl,xbr, &
      ybl,ybr,yc)

    call getder(dparsn,g,gtar,ibs,indx,maxeqn,neqn,np,npar,nparb,nparf,nprof, &
      ny,sens,splbmp,taubmp,wateb,watep,wateu,watev,xbl,xbr,ybl,ybr,yc)

    call prsol(dpara3,dparfd,dparfdc,dparsn,g,g2,gdif,gdifc,idfd,ids,ifds, &
      indx,iwrite,maxeqn,neqn,np,npar,nparb,nparf,numstp,para,sens)

    if ( iwrite>= 3 ) then
      call prgs2(area,gdif,gdifc,gradf,indx,iopt,maxeqn,nelem, &
        np,npar,nquad,ny,sens,xc,yc)
    end if

    if ( itunit /= 0 ) then
      write(itunit,'(1x,6g14.6)') cost, para(1:npar)
      write(itunit,'(1x,5g14.6)') dparsn(1:npar)
      write(itunit,'(1x,5g14.6)') dparfd(1:npar)
      write(itunit,'(1x,5g14.6)') dpara3(1:npar)
    end if

    if ( iplot>0.and.mod(numstp,iplot) == 0 ) then

      call pltwrt(eqn,g,gdif,igunit,indx,isotri,iwrite,maxeqn,nelem,neqn, &
        node,np,npar,npe,nprof,nx,ny,para,sens,xc,xprof,yc)

    end if

    if ( numstp>= maxstp ) then

      if ( iwrite<2 ) then
        write ( *, * ) ' '
        write ( *, * ) 'QSolve - Point ',numstp
        call prpar(nparb,nparf,para)
        write ( *, * ) 'Cost:',cost
      end if

      write ( *, * ) ' '
      write ( *, * ) 'QSolve - Warning!'
      write ( *, * ) '  Number of steps exceeded.'

      go to 20
    end if

    go to 10
!
!  Did the optimizer ask us to return the derivative values
!  associated with the current set of parameters?
!
  else if (ivopt(1) == 2 ) then

    go to 10
!
!  We can't figure out what SUMIT is trying to tell us.
!
  else
    write ( *, * ) ' '
    write ( *, * ) 'QSolve - Warning!'
    write ( *, * ) '  Unknown value of IVOPT(1) = ',ivopt(1)
    go to 20
  end if

20    continue
!
!  If IPLOT < 0, we want to write out graphics information
!  for the last point computed.
!
  if ( iplot<0 ) then
    call pltwrt(eqn,g,gdif,igunit,indx,isotri,iwrite,maxeqn,nelem,neqn,node, &
      np,npar,npe,nprof,nx,ny,para,sens,xc,xprof,yc)
  end if

  return
end
subroutine refbsp(q,dqdx,dqdy,detadx,detady,iq,dxsidx,dxsidy,eta,xsi)
!
!*******************************************************************************
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
!    dF/dX = dF/dXsi dXsi/dX + dF/dEta dEta/dX
!    dF/dY = dF/dXsi dXsi/dY + dF/dEta dEta/dY
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
!    03 April 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!  Q,
!  DQDX,
!  DQDY   Output, real Q, DQDX, DQDY, the value of the basis
!         function, and its derivatives with respect to X and Y, at
!         the point (ETA,XSI).
!
!  DETADX,
!  DETADY Input, real DETADX, DETADY, the partial derivative
!         d ETA/d X and d ETA/d Y at (ETA,XSI).
!
!  IQ     Input, integer IQ, the local node number, between 1 and
!         3, whose basis function is being evaluated.
!
!  DXSIDX,
!  DXSIDY Input, real DXSIDX, DXSIDY, the partial derivative
!         d XSI/d X and d XSI/d Y at (ETA,XSI).
!
!  ETA,
!  XSI    Input, real ETA, XSI, the local coordinates of the
!         at which the basis information is desired.
!
!
  real ( kind = 8 ) detadx
  real ( kind = 8 ) detady
  real ( kind = 8 ) dqdeta
  real ( kind = 8 ) dqdx
  real ( kind = 8 ) dqdxsi
  real ( kind = 8 ) dqdy
  real ( kind = 8 ) dxsidx
  real ( kind = 8 ) dxsidy
  real ( kind = 8 ) eta
  integer iq
  real ( kind = 8 ) q
  real ( kind = 8 ) xsi
!
  if ( iq == 1 ) then
    q = 1.0D+00-xsi
    dqdxsi = -1.0D+00
    dqdeta =  0.0D+00
  else if (iq == 2 ) then
    q = eta
    dqdxsi = 0.0D+00
    dqdeta = 1.0D+00
  else if (iq ==3 ) then
    q = xsi-eta
    dqdxsi = 1.0D+00
    dqdeta = -1.0D+00
  else if (iq>= 4.and.iq<=6 ) then
    q = 0.0D+00
    dqdxsi = 0.0D+00
    dqdeta = 0.0D+00
  else
    write ( *, * ) ' '
    write ( *, * ) 'RefBSP - Fatal error!'
    write ( *, * ) '  Request for basis function IQ = ',iq
    write ( *, * ) '  but IQ must be between 1 and 6.'
    stop
  end if

  dqdx = dqdxsi*dxsidx+dqdeta*detadx
  dqdy = dqdxsi*dxsidy+dqdeta*detady

  return
end
subroutine refqbf(w,dwdx,dwdy,detadx,detady,dxsidx,dxsidy,eta,iq,xsi)
!
!*******************************************************************************
!
!! REFQBF evaluates one of the six quadratic basis functions,
!  and its X and Y derivatives, at a particular point in a
!  particular element, by referring to the reference triangle.
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
!    d F(X,Y)/dY  =  (d XSI/dY  d ETA/dY ) * ( d F(XSI, ETA)/d ETA )
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
!    03 April 2003
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
!  DETADY Input, real DETADX, DETADY, the partial derivative
!         d ETA/d X and d ETA/d Y at (XSI,ETA).
!
!  DXSIDX,
!  DXSIDY Input, real DXSIDX, DXSIDY, the partial derivative
!         d XSI/d X and d XSI/d Y at (XSI,ETA).
!
!  ETA    Input, real ETA, the ETA coordinate of the point.
!
!  IQ     Input, integer IQ, the local node number, between 1 and
!         6, whose basis function is being evaluated.
!
!  XSI    Input, real XSI, the XSI coordinate of the point.
!
!
  real ( kind = 8 ) detadx
  real ( kind = 8 ) detady
  real ( kind = 8 ) dwdeta
  real ( kind = 8 ) dwdx
  real ( kind = 8 ) dwdxsi
  real ( kind = 8 ) dwdy
  real ( kind = 8 ) dxsidx
  real ( kind = 8 ) dxsidy
  real ( kind = 8 ) eta
  integer iq
  real ( kind = 8 ) w
  real ( kind = 8 ) xsi
!
!  Evaluate W, the quadratic basis function.
!  Evaluate DWDXSI and DWDETA, the partial derivatives d W/d XSI
!  and d W/d ETA.
!
!  Basis 1 is zero if XSI = 0.5 or XSI=1.
!
  if ( iq == 1 ) then
    w =  (2.0D+00*xsi-1.0D+00) * (xsi-1.0D+00)
    dwdxsi = -3.0D+00+4.0D+00*xsi
    dwdeta = 0.0D+00
!
!  Basis 2 is zero if ETA = 0 or ETA=0.5.
!
  else if (iq == 2 ) then
    w =  eta * (2.0D+00*eta-1.0D+00)
    dwdxsi = 0.0D+00
    dwdeta = -1.0D+00+4.0D+00*eta
!
!  Basis 3 is zero if XSI = ETA, or XSI=ETA+0.5
!
  else if (iq ==3 ) then
    w =  (xsi-eta) * (2.0D+00*xsi-2.0D+00*eta-1.0D+00)
    dwdxsi = -1.0D+00+4.0D+00*xsi-4.0D+00*eta
    dwdeta = 1.0D+00-4.0D+00*xsi+4.0D+00*eta
!
!  Basis 4 is zero if ETA = 0 or XSI=1.
!
  else if (iq ==4 ) then
    w =  4.0D+00 * eta * (1.0D+00-xsi)
    dwdxsi = -4.0D+00*eta
    dwdeta = 4.0D+00-4.0D+00*xsi
!
!  Basis 5 is zero if ETA = 0 or XSI=ETA.
!
  else if (iq ==5 ) then
    w = 4.0D+00 * eta * (xsi-eta)
    dwdxsi = 4.0D+00*eta
    dwdeta = 4.0D+00*xsi-8.0D+00*eta
!
!  Basis 6 is zero if XSI = ETA or XSI=1.
!
  else if (iq ==6 ) then
    w = 4.0D+00 * (xsi-eta) * (1.0D+00-xsi)
    dwdxsi = 4.0D+00-8.0D+00*xsi+4.0D+00*eta
    dwdeta = -4.0D+00+4.0D+00*xsi
!
!  Stop if we were given an unexpected value of IQ.
!
  else
    write ( *, * ) ' '
    write ( *, * ) 'RefQBF - Fatal error!'
    write ( *, * ) '  A basis function index must be between 1 and 6,'
    write ( *, * ) '  but you input the value IQ = ',iq
    stop
  end if
!
!  Convert the d W/d XSI and d W/d ETA derivatives to d W/d X
!  and d W/d Y.
!
  dwdx = dwdxsi*dxsidx + dwdeta*detadx
  dwdy = dwdxsi*dxsidy + dwdeta*detady

  return
end
subroutine reysen(area,eqn,g,indx,nelem,neqn,node,np,nquad,phi,rhs)
!
!*******************************************************************************
!
!! REYSEN sets up the right hand side RHS associated with the first
!  order sensitivities with respect to the NU_INV parameter of a
!  given state function (U,V,P).
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
!  AREA   Input, real AREA(NQUAD,NELEM).
!
!         AREA contains a common factor multiplying the term associated
!         with a quadrature point in a given element, namely,
!
!           AREA(IQUAD,IELEM) = Ar(IELEM) * WQUAD(IQUAD)
!
!         or, if the element is isoperimetric,
!
!           AREA(IQUAD,IELEM) = DET * Ar(IELEM) * WQUAD(IQUAD)
!
!         Here Ar(IELEM) represents the area of element IELEM.
!
!  EQN    Input, character ( len = 2 ) EQN(MAXEQN).
!         EQN records the "type" of each equation that will be generated, and
!         which is associated with an unknown.  Note that most boundary
!         conditions do not result in an equation.  The current values are:
!
!         'U'  The horizontal momentum equation.
!         'UB' The condition U = 0 applied at a node on the bump.
!         'UI' The condition U = UInflow(Y,Lambda) at the inflow.
!         'UW' The condition U = 0 applied at a node on a fixed wall.
!
!         'V'  The vertical momentum equation.
!         'VB' The condition V = 0 applied at a node on the bump.
!         'VI' The condition V = VInflow(Y,Lambda) at the inflow.
!         'VW' The condition V = 0 applied at a node on a fixed wall.
!
!         'P'  The continuity equation.
!         'PB' The condition P = 0 applied at (XMAX,YMAX).
!
!  G      Input, real G(NEQN), the finite element
!         coefficients for the state functions (U,V,P).
!
!  INDX   Input, integer INDX(3,NP).
!
!         INDX(I,J) contains, for each node J, the index of U, V and P at
!         that node, or 0 or a negative value.
!
!         If K = INDX(I,J) is positive, then the value of the degree
!         of freedom is stored in the solution vector entry G(K).
!
!         If INDX(I,J) is positive, then that means that a degree of
!         freedom for variable I (U, V or P) is associated with node
!         J, and an equation will be generated to determine its value.
!
!         If INDX(I,J) is not positive, then no equation is
!         generated to determine for variable I at node J.
!
!  NELEM  Input, integer NELEM, the number of elements.
!
!  NEQN   Input, integer NEQN, the number of finite element equations used
!         to define the horizontal and vertical velocities and the
!         pressure.
!
!  NODE   Input, integer NODE(6,NELEM).
!
!         NODE(I,J) contains, for an element J, the global node index of
!         the element node whose local number is I.
!
!         The local ordering of the nodes is suggested by this diagram:
!
!               2
!              /|
!             4 5
!            /  |
!           1-6-3
!
!  NP     Input, integer NP, the number of nodes used to define the finite
!         element mesh.  NP = (2*NX-1)*(2*NY-1).
!
!  NQUAD  Input, integer NQUAD, the number of quadrature points used to
!         approximate the integrals in the finite element versions of
!         the state equations.
!
!         NQUAD is usually 3, but can also be 7.
!
!  PHI    real PHI(NQUAD,6,10,NELEM).
!
!         PHI contains the value of a basis function, its derivative,
!         or other information, evaluated at a quadrature point.
!
!         For a particular element I, quadrature point J, and basis
!         function K, we use the following shorthand for the ten
!         entries of PHI:
!
!           W, dWdX, dWdY
!           Q, dQdX, dQdY
!           dXsidX, dXsidY, dEtadX, dEtadY
!
!         W is the quadratic basis function associated with velocity,
!         Q the linear basis function associated with pressure,
!         Xsi and Eta the reference coordinates for the point.
!
!         In particular, PHI(J,K,1,I) is the value of the quadratic
!         basis function associated with local node K in element I,
!         evaluated at quadrature point J.
!
!         Note that PHI(J,K,4,I) = PHI(J,K,5,I)=PHI(J,K,6,I)=0 for
!         K = 4, 5, or 6, since there are only three linear basis
!         functions.
!
!  RHS    Output, real RHS(NEQN), the right hand
!         side of the first order sensitivity equations associated
!         with the NU_INV parameter.
!
!  REYSEN sets up the right hand side F associated with the
!  sensitivities of a given flow solution (U,V,P) with
!  respect to the NU_INV parameter.
!
!  F      Output, real F(NEQN), the right hand
!         side of the sensitivity equations associated with
!         the NU_INV parameter.
!
!
  integer nelem
  integer neqn
  integer np
  integer nquad
!
  real ( kind = 8 ) ar
  real ( kind = 8 ) area(nquad,nelem)
  real ( kind = 8 ) dpdx
  real ( kind = 8 ) dpdy
  real ( kind = 8 ) dudx
  real ( kind = 8 ) dudy
  real ( kind = 8 ) dvdx
  real ( kind = 8 ) dvdy
  character ( len = 2 ) eqn(neqn)
  real ( kind = 8 ) g(neqn)
  integer i
  integer ielem
  integer ihor
  integer indx(3,np)
  integer ip
  integer iq
  integer iquad
  integer iver
  integer node(6,nelem)
  real ( kind = 8 ) p
  real ( kind = 8 ) phi(nquad,6,10,nelem)
  real ( kind = 8 ) rhs(neqn)
  real ( kind = 8 ) u
  real ( kind = 8 ) v
  real ( kind = 8 ) wi
!
!  Zero out the right hand side vector.
!
  rhs(1:neqn) = 0.0D+00
!
!  Approximate the integrated state equations by adding the
!  contribution from element IELEM.
!
  do ielem = 1,nelem
!
!  In element IELEM, approximate the integral by moving to
!  quadrature point IQUAD and evaluating the state solution (U,V,P)
!  and its spatial derivatives.
!
    do iquad = 1,nquad

      ar = area(iquad,ielem)

      call uvalq(dpdx,dpdy,dudx,dudy,dvdx,dvdy,g,ielem,indx, &
        iquad,nelem,neqn,node,np,nquad,p,phi,u,v)
!
!  Now consider a node with local index IQ, and global index IP,
!  whose quadratic basis function evaluated at the quadrature point
!  IQUAD has value WI.  Evaluate the right hand sides of equations
!  IHOR and IVER and add the contribution to the total.
!
      do iq = 1,6

        ip = node(iq,ielem)
        wi = phi(iquad,iq,1,ielem)

        ihor = indx(1,ip)
        if ( eqn(ihor) =='U' ) then
          rhs(ihor) = rhs(ihor)-ar*(u*dudx+v*dudy+dpdx)*wi
        end if

        iver = indx(2,ip)
        if ( eqn(iver) =='V' ) then
          rhs(iver) = rhs(iver)-ar*(u*dvdx+v*dvdy+dpdy)*wi
        end if

      end do
    end do
  end do

  return
end
subroutine rint_to_rint ( rmin, rmax, r, r2min, r2max, r2 )
!
!*******************************************************************************
!
!! RINT_TO_RINT maps a real interval to another real interval.
!
!  Discussion:
!
!    R2 : =  R2MIN + ( R2MAX - R2MIN ) * ( R - RMIN ) / ( RMAX - RMIN )
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
!    Input, real RMIN, RMAX, the first real range.
!
!    Input, real R, the real number to be converted.
!
!    Input, real R2MAX, R2MIN, the second real range.
!
!    Output, real R2, the corresponding value in the range [R2MIN,R2MAX].
!
  real ( kind = 8 ) r
  real ( kind = 8 ) rmax
  real ( kind = 8 ) rmin
  real ( kind = 8 ) r2
  real ( kind = 8 ) r2max
  real ( kind = 8 ) r2min
!
  if ( rmax  == rmin ) then

    r2 = ( r2max + r2min ) / 2.0D+00

  else

    r2 = ( ( ( rmax - r ) * r2min + ( r - rmin ) * r2max ) / ( rmax - rmin ) )

  end if

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
  integer i
  integer len1
  integer len2
  integer lenc
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
subroutine scopy(n,dx,incx,dy,incy)
!
!*******************************************************************************
!
!! SCOPY copies a vector, x, to a vector, y.
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
  real ( kind = 8 ) dx(*)
  real ( kind = 8 ) dy(*)
  integer i
  integer incx
  integer incy
  integer ix
  integer iy
  integer m
  integer n
!
  if ( n<= 0)return

  if ( incx == 1.and.incy== 1)go to 20

  ix = 1
  iy = 1
  if ( incx<0)ix = (-n+1)*incx + 1
  if ( incy<0)iy = (-n+1)*incy + 1

  do i = 1,n
    dy(iy) = dx(ix)
    ix = ix + incx
    iy = iy + incy
  end do

  return
!
   20 m = mod(n,7)

  do i = 1,m
    dy(i) = dx(i)
  end do

  do i = m+1,n,7
    dy(i) = dx(i)
    dy(i + 1) = dx(i + 1)
    dy(i + 2) = dx(i + 2)
    dy(i + 3) = dx(i + 3)
    dy(i + 4) = dx(i + 4)
    dy(i + 5) = dx(i + 5)
    dy(i + 6) = dx(i + 6)
  end do

  return
end
function sdot(n,dx,incx,dy,incy)
!
!*******************************************************************************
!
!! SDOT forms the dot product of two vectors.
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
  real ( kind = 8 ) sdot
  real ( kind = 8 ) dtemp
  real ( kind = 8 ) dx(*)
  real ( kind = 8 ) dy(*)
  integer i
  integer incx
  integer incy
  integer ix
  integer iy
  integer m
  integer n
!
  sdot = 0.0D+00
  dtemp = 0.0D+00

  if ( n<= 0)return

  if ( incx == 1.and.incy== 1)go to 20

  if ( incx<0 ) then
    ix = (-n+1)*incx + 1
  else
    ix = 1
  end if

  if ( incy<0 ) then
    iy = (-n+1)*incy + 1
  else
    iy = 1
  end if

  do i = 1,n
    dtemp = dtemp + dx(ix)*dy(iy)
    ix = ix + incx
    iy = iy + incy
  end do

  sdot = dtemp
  return
!
   20 m = mod(n,5)

  do i = 1,m
    dtemp = dtemp + dx(i)*dy(i)
  end do

  do i = m+1,n,5
    dtemp = dtemp + dx(i)*dy(i) + dx(i + 1)*dy(i + 1) + &
      dx(i + 2)*dy(i + 2) + dx(i + 3)*dy(i + 3) + dx(i + 4)*dy(i + 4)
  end do

  sdot = dtemp

  return
end
subroutine serene(type,ve,vn,vne,vnw,vs,vse,vsw,vw,vterp)
!
!*******************************************************************************
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
!  TYPE   Input, CHARACTER*2 TYPE, tells SERENE the geometry of the
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
  real ( kind = 8 ) eta
  real ( kind = 8 ) pe
  real ( kind = 8 ) pn
  real ( kind = 8 ) pne
  real ( kind = 8 ) pnw
  real ( kind = 8 ) ps
  real ( kind = 8 ) pse
  real ( kind = 8 ) psw
  real ( kind = 8 ) pw
  character ( len = 2 ) type
  real ( kind = 8 ) ve
  real ( kind = 8 ) vn
  real ( kind = 8 ) vne
  real ( kind = 8 ) vnw
  real ( kind = 8 ) vs
  real ( kind = 8 ) vse
  real ( kind = 8 ) vsw
  real ( kind = 8 ) vw
  real ( kind = 8 ) vterp
  real ( kind = 8 ) xsi
!
!  To make this routine more general, simply pass in the
!  values of XSI and ETA at which the interpolated value
!  is desired.  By setting XSI = ETA=0, we are asking for the interpolated
!  value at the center of the finite element.
!
!
  xsi = 0.0D+00
  eta = 0.0D+00
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
  if ( type =='C' ) then

    psw = -0.25D+00*(1.0-xsi)*(1.0-eta)*(1.0+xsi+eta)
    pse = -0.25D+00*(1.0+xsi)*(1.0-eta)*(1.0-xsi+eta)
    pne = -0.25D+00*(1.0+xsi)*(1.0+eta)*(1.0-xsi-eta)
    pnw = -0.25D+00*(1.0-xsi)*(1.0+eta)*(1.0+xsi-eta)
    ps =  0.5D+00*(1.0-xsi)*(1.0+xsi)*(1.0-eta)
    pe =  0.5D+00*(1.0+xsi)*(1.0+eta)*(1.0-eta)
    pn =  0.5D+00*(1.0-xsi)*(1.0+xsi)*(1.0+eta)
    pw =  0.5D+00*(1.0-xsi)*(1.0+eta)*(1.0-eta)
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
  else if (type =='N' ) then

    psw =  0.5D+00*(xsi-1.0D+00)*(1.0D+00+xsi+eta)
    pse = -0.5D+00*(xsi+1.0D+00)*(1.0D+00-xsi+eta)
    ps =  -(xsi+1.0D+00)*(xsi-1.0D+00)
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
  else if (type =='E' ) then

    pse =  0.5D+00*(eta-1.0)*(1.0+xsi+eta)
    pne = -0.5D+00*(eta+1.0)*(1.0+xsi-eta)
    ps =  -0.5D+00*(xsi+1.0)*(eta-1.0)
    pn =   0.5D+00*(xsi+1.0)*(eta+1.0)
    pw =  -(eta+1.0D+00)*(eta-1.0D+00)
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
  else if (type =='W' ) then

    pse =  0.5D+00*(eta-1.0)*(1.0-xsi+eta)
    pne = -0.5D+00*(eta+1.0)*(1.0-xsi-eta)
    ps =   0.5D+00*(xsi-1.0)*(eta-1.0)
    pe =  -(eta-1.0)*(eta+1.0)
    pn =  -0.5D+00*(xsi-1.0)*(eta+1.0)
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
  else if (type =='S' ) then

    pne = -0.5D+00*(xsi+1.0)*(1.0-xsi-eta)
    pnw =  0.5D+00*(xsi-1.0)*(1.0+xsi-eta)
    pe =  -0.5D+00*(eta-1.0)*(xsi+1.0)
    pn =  -(xsi+1.0)*(xsi-1.0)
    pw =   0.5D+00*(eta-1.0)*(xsi-1.0)
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
  else if (type =='NE' ) then

    psw = -(1.0D+00+xsi+eta)
    ps =  (xsi+1.0D+00)
    pw =  (eta+1.0D+00)
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
  else if (type =='NW' ) then

    pse = -(1.0D+00-xsi+eta)
    ps = -(xsi-1.0D+00)
    pe =  (eta+1.0D+00)
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
  else if (type =='SE' ) then

    pnw = -(1.0D+00+xsi-eta)
    pn =   (xsi+1.0D+00)
    pw =  -(eta-1.0D+00)
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
  else if (type =='SW' ) then

    pne = -(1.0D+00-xsi-eta)
    pe =  -(eta-1.0D+00)
    pn =  -(xsi-1.0D+00)

  end if
!
  vterp = vsw*psw+vse*pse+vne*pne+vnw*pnw+vs*ps+ve*pe+vn*pn+vw*pw

  return
end
subroutine sertrn(dudy,ic,np,ny,type,u,xc,yc)
!
!*******************************************************************************
!
!! SERTRN computes the value of dUdY at the central node of an
!  isoparametric serendipity element.
!
!  In some cases, the serendipity element may be incomplete,
!  having only 5 or 3 nodes instead of the expected 8.
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
!  DUDY   Output, real DUDY, the value of dUdY at the
!         central node.
!
!  IC     Input, integer IC, the global node number of the central
!         node.
!
!  NP     Input, integer NP, the number of nodes.
!
!  NY     Input, integer NY, the number of nodes in the Y direction.
!
!  TYPE   Input, character TYPE*2, a code for the sort of serendipity
!         element we are given, which is suggested by the following
!         diagram, where the value of TYPE is placed at the position
!         of the central node in each possible serendipity element.
!
!         NW .  N  . NE
!            |     |
!         .--.--.--.--.
!            |     |
!         W  .  C  .  E
!            |     |
!         .--.--.--.--.
!            |     |
!         SW .  S  . SE
!
!  XC     Input, real XC(NP), the X coordinates of the
!         nodes.
!
!  YC     Input, real YC(NP), the Y coordinates of the
!         nodes.
!
!
  integer np
!
  real ( kind = 8 ) det
  real ( kind = 8 ) detady
  real ( kind = 8 ) dpsideta(8)
  real ( kind = 8 ) dpsidxsi(8)
  real ( kind = 8 ) dudy
  real ( kind = 8 ) dxdeta
  real ( kind = 8 ) dxdxsi
  real ( kind = 8 ) dxsidy
  real ( kind = 8 ) dydeta
  real ( kind = 8 ) dydxsi
  real ( kind = 8 ) eta
  integer i
  integer ic
  integer mp
  integer nfunc
  integer nodes(8)
  integer ny
  real ( kind = 8 ) psi(8)
  character type*2
  real ( kind = 8 ) u(np)
  real ( kind = 8 ) uval
  real ( kind = 8 ) x(8)
  real ( kind = 8 ) xc(np)
  real ( kind = 8 ) xsi
  real ( kind = 8 ) xval
  real ( kind = 8 ) y(8)
  real ( kind = 8 ) yc(np)
  real ( kind = 8 ) yval
!
  if ( ic == 15 ) then
    write ( *, * ) 'ic = ',ic
  end if

  mp = 2*ny-1
!
!  We are going to calculate at XSI = ETA=0.0D+00
!
  xsi = 0.0D+00
  eta = 0.0D+00
!
!  Evaluate the basis functions and derivatives at (XSI,ETA).
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
  if ( type =='C' ) then

    nfunc = 8

    psi(1) = -0.25D+00*(1.0-xsi)*(1.0-eta)*(1.0+xsi+eta)
    psi(2) = -0.25D+00*(1.0+xsi)*(1.0-eta)*(1.0-xsi+eta)
    psi(3) = -0.25D+00*(1.0+xsi)*(1.0+eta)*(1.0-xsi-eta)
    psi(4) = -0.25D+00*(1.0-xsi)*(1.0+eta)*(1.0+xsi-eta)
    psi(5) =  0.5D+00*(1.0-xsi)*(1.0+xsi)*(1.0-eta)
    psi(6) =  0.5D+00*(1.0+xsi)*(1.0+eta)*(1.0-eta)
    psi(7) =  0.5D+00*(1.0-xsi)*(1.0+xsi)*(1.0+eta)
    psi(8) =  0.5D+00*(1.0-xsi)*(1.0+eta)*(1.0-eta)

    dpsidxsi(1) = -0.25D+00*(1.0-eta)*(-eta-2.0*xsi)
    dpsidxsi(2) = -0.25D+00*(1.0-eta)*(eta-2.0*xsi)
    dpsidxsi(3) = -0.25D+00*(1.0+eta)*(-eta-2.0*xsi)
    dpsidxsi(4) = -0.25D+00*(1.0+eta)*(eta-2.0*xsi)
    dpsidxsi(5) =  0.5D+00*(1.0-eta)*(-2.0*xsi)
    dpsidxsi(6) =  0.5D+00*(1.0+eta)*(1.0-eta)
    dpsidxsi(7) =  0.5D+00*(1.0+eta)*(-2.0*xsi)
    dpsidxsi(8) = -0.5D+00*(1.0+eta)*(1.0-eta)

    dpsideta(1) = -0.25*(1.0-xsi)*(-xsi-2.0*eta)
    dpsideta(2) = -0.25*(1.0+xsi)*(xsi-2.0*eta)
    dpsideta(3) = -0.25*(1.0+xsi)*(-xsi-2.0*eta)
    dpsideta(4) = -0.25*(1.0-xsi)*(xsi-2.0*eta)
    dpsideta(5) = -0.5*(1.0-xsi)*(1.0+xsi)
    dpsideta(6) =  0.5*(1.0+xsi)*(-2.0*eta)
    dpsideta(7) =  0.5*(1.0-xsi)*(1.0+xsi)
    dpsideta(8) =  0.5*(1.0-xsi)*(-2.0*eta)

    nodes(1) = ic-mp-1
    nodes(2) = ic+mp-1
    nodes(3) = ic+mp+1
    nodes(4) = ic-mp+1
    nodes(5) = ic-1
    nodes(6) = ic+mp
    nodes(7) = ic+1
    nodes(8) = ic-mp
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
  else if (type =='N' ) then

    nfunc = 5

    psi(1) =  0.5*(xsi-1.0)*(1.0+xsi+eta)
    psi(2) = -0.5*(xsi+1.0)*(1.0-xsi+eta)
    psi(3) =  -(xsi+1.0)*(xsi-1.0)
    psi(4) =  0.5*(xsi+1.0)*(eta+1.0)
    psi(5) = -0.5*(xsi-1.0)*(eta+1.0)

    dpsidxsi(1) =  0.5*( 2.0*xsi+eta)
    dpsidxsi(2) = -0.5*(-2.0*xsi+eta)
    dpsidxsi(3) = -2.0*xsi
    dpsidxsi(4) =  0.5*(eta+1.0)
    dpsidxsi(5) = -0.5*(eta+1.0)

    dpsideta(1) =  0.5*(xsi-1.0)
    dpsideta(2) = -0.5*(xsi+1.0)
    dpsideta(3) =  0.0D+00
    dpsideta(4) =  0.5*(xsi+1.0)
    dpsideta(5) = -0.5*(xsi-1.0)

    nodes(1) = ic-mp-1
    nodes(2) = ic+mp-1
    nodes(3) = ic-1
    nodes(4) = ic+mp
    nodes(5) = ic-mp
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
  else if (type =='E' ) then

    nfunc = 5

    psi(1) =  0.5*(eta-1.0)*(1.0+xsi+eta)
    psi(2) = -0.5*(eta+1.0)*(1.0+xsi-eta)
    psi(3) =  -0.5*(xsi+1.0)*(eta-1.0)
    psi(4) =   0.5*(xsi+1.0)*(eta+1.0)
    psi(5) =  -(eta+1.0)*(eta-1.0)

    dpsidxsi(1) =  0.5*(eta-1.0)
    dpsidxsi(2) = -0.5*(eta+1.0)
    dpsidxsi(3) = -0.5*(eta-1.0)
    dpsidxsi(4) =  0.5*(eta+1.0)
    dpsidxsi(5) =  0.0D+00

    dpsideta(1) =  0.5*(xsi+2.0*eta)
    dpsideta(2) = -0.5*(xsi-2.0*eta)
    dpsideta(3) = -0.5*(xsi+1.0)
    dpsideta(4) =  0.5*(xsi+1.0)
    dpsideta(5) = -2.0*eta

    nodes(1) = ic-mp-1
    nodes(2) = ic-mp+1
    nodes(3) = ic-1
    nodes(4) = ic+1
    nodes(5) = ic-mp
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
  else if (type =='W' ) then

    nfunc = 5

    psi(1) =  0.5*(eta-1.0)*(1.0-xsi+eta)
    psi(2) = -0.5*(eta+1.0)*(1.0-xsi-eta)
    psi(3) =   0.5*(xsi-1.0)*(eta-1.0)
    psi(4) =  -(eta-1.0)*(eta+1.0)
    psi(5) =  -0.5*(xsi-1.0)*(eta+1.0)

    dpsidxsi(1) = -0.5*(eta-1.0)
    dpsidxsi(2) =  0.5*(eta+1.0)
    dpsidxsi(3) =  0.5*(eta-1.0)
    dpsidxsi(4) =  0.0D+00
    dpsidxsi(5) = -0.5*(eta+1.0)

    dpsideta(1) =  0.5*(-xsi+2.0*eta)
    dpsideta(2) = -0.5*(-xsi-2.0*eta)
    dpsideta(3) =  0.5*(xsi-1.0)
    dpsideta(4) = -2.0*eta
    dpsideta(5) = -0.5*(xsi-1.0)

    nodes(1) = ic+mp-1
    nodes(2) = ic+mp+1
    nodes(3) = ic-1
    nodes(4) = ic+mp
    nodes(5) = ic+1
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
  else if (type =='S' ) then

    nfunc = 5

    psi(1) = -0.5*(xsi+1.0)*(1.0-xsi-eta)
    psi(2) =  0.5*(xsi-1.0)*(1.0+xsi-eta)
    psi(3) = -0.5*(eta-1.0)*(xsi+1.0)
    psi(4) =     -(xsi+1.0)*(xsi-1.0)
    psi(5) =  0.5*(eta-1.0)*(xsi-1.0)

    dpsidxsi(1) = -0.5*(-2.0*xsi-eta)
    dpsidxsi(2) =  0.5*( 2.0*xsi-eta)
    dpsidxsi(3) = -0.5*(eta-1.0)
    dpsidxsi(4) = -2.0*xsi
    dpsidxsi(5) =  0.5*(eta-1.0)

    dpsideta(1) =  0.5*(xsi+1.0)
    dpsideta(2) = -0.5*(xsi-1.0)
    dpsideta(3) = -0.5*(xsi+1.0)
    dpsideta(4) =  0.0D+00
    dpsideta(5) =  0.5*(xsi-1.0)

    nodes(1) = ic+mp+1
    nodes(2) = ic-mp+1
    nodes(3) = ic+mp
    nodes(4) = ic+1
    nodes(5) = ic-mp
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
  else if (type =='NE' ) then

    nfunc = 3

    psi(1) = -(1.0D+00+xsi+eta)
    psi(2) =  (xsi+1.0D+00)
    psi(3) =  (eta+1.0D+00)

    dpsidxsi(1) = -1.0D+00
    dpsidxsi(2) =  1.0D+00
    dpsidxsi(3) =  0.0D+00

    dpsideta(1) = -1.0D+00
    dpsideta(2) =  0.0D+00
    dpsideta(3) =  1.0D+00

    nodes(1) = ic-mp-1
    nodes(2) = ic-1
    nodes(3) = ic-mp
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
  else if (type =='NW' ) then

    nfunc = 3

    psi(1) = -(1.0D+00-xsi+eta)
    psi(2) = -(xsi-1.0D+00)
    psi(3) =  (eta+1.0D+00)

    dpsidxsi(1) =  1.0D+00
    dpsidxsi(2) = -1.0D+00
    dpsidxsi(3) =  0.0D+00

    dpsideta(1) = -1.0D+00
    dpsideta(2) =  0.0D+00
    dpsideta(3) =  1.0D+00

    nodes(1) = ic+mp-1
    nodes(2) = ic-1
    nodes(3) = ic+mp
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
  else if (type =='SE' ) then

    nfunc = 3

    psi(1) = -(1.0D+00+xsi-eta)
    psi(2) =   (xsi+1.0D+00)
    psi(3) =  -(eta-1.0D+00)

    dpsidxsi(1) = -1.0D+00
    dpsidxsi(2) =  1.0D+00
    dpsidxsi(3) =  0.0D+00

    dpsideta(1) =  1.0D+00
    dpsideta(2) =  0.0D+00
    dpsideta(3) = -1.0D+00

    nodes(1) = ic-mp+1
    nodes(2) = ic+1
    nodes(3) = ic-mp
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
  else if (type =='SW' ) then

    nfunc = 3

    psi(1) = -(1.0D+00-xsi-eta)
    psi(2) =  -(eta-1.0D+00)
    psi(3) =  -(xsi-1.0D+00)

    dpsidxsi(1) =  1.0D+00
    dpsidxsi(2) =  0.0D+00
    dpsidxsi(3) = -1.0D+00

    dpsideta(1) =  1.0D+00
    dpsideta(2) = -1.0D+00
    dpsideta(3) =  0.0D+00

    nodes(1) = ic+mp+1
    nodes(2) = ic+mp
    nodes(3) = ic+1

  end if
!
!  Get the X, Y coordinates of the nodes.
!
  do i = 1,nfunc
    x(i) = xc(nodes(i))
    y(i) = yc(nodes(i))
  end do
!
!  Evaluate the mapping X(XSI,ETA), Y(XSI,ETA).
!
  xval = 0.0D+00
  yval = 0.0D+00
  do i = 1,nfunc
    xval = xval+x(i)*psi(i)
    yval = yval+y(i)*psi(i)
  end do
!
!  Compute the partial derivatives
!
!    DXDXSI(XSI,ETA), DXDETA(XSI,ETA)
!    DYDXSI(XSI,ETA), DYDETA(XSI,ETA)
!
!  This is the jacobian matrix
!
!    J: (XSI,ETA) --> (X,Y).
!
  dxdxsi = 0.0D+00
  dxdeta = 0.0D+00
  dydxsi = 0.0D+00
  dydeta = 0.0D+00
  do i = 1,nfunc
    dxdxsi = dxdxsi+x(i)*dpsidxsi(i)
    dxdeta = dxdeta+x(i)*dpsideta(i)
    dydxsi = dydxsi+y(i)*dpsidxsi(i)
    dydeta = dydeta+y(i)*dpsideta(i)
  end do
!
!  Compute the determinant of the jacobian matrix:
!
  det = dxdxsi*dydeta-dxdeta*dydxsi

  if ( det == 0.0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'SERTRN - Fatal error!'
    write ( *, * ) '  The jacobian J: (XSI,ETA) --> (X,Y) is singular!'
    write ( *, * ) '  This occurred for node IC = ',ic
    write ( *, * ) '  Local coordinates XSI,ETA = ',xsi,eta
    write ( *, * ) '  Global coordinates X,Y = ',xval,yval
    write ( *, * ) ' '
    write ( *, * ) '  The X, Y nodes were:'
    write ( *, * ) ' '
    do i = 1,8
      write ( *, * ) x(i),y(i)
    end do

    stop
  end if
!
!  Compute
!
!    d ETA/d X, d ETA/d Y,
!    d XSI/d X, d XSI/d Y
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
!     dxsidx =  dydeta/det
  dxsidy = -dxdeta/det

!     detadx = -dydxsi/det
  detady =  dxdxsi/det
!
!  Now evaluate dUdY at the central "non-node":
!
  uval = 0.0D+00
  dudy = 0.0D+00
  do i = 1,nfunc
    uval = uval+u(nodes(i))*psi(i)
    dudy = dudy+u(nodes(i))*(dpsidxsi(i)*dxsidy+dpsideta(i)*detady)
  end do

  return
end
subroutine setban(indx,maxrow,nelem,nlband,node,np,nrow)
!
!*******************************************************************************
!
!! SETBAN computes NLBAND, the lower band width of the Jacobian matrix,
!  and NROW, the total number of rows required to store the matrix
!  in LINPACK general band storage format.
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
!  INDX   Input, integer INDX(3,NP).
!
!         INDX(I,J) contains, for each node J, the index of U, V and P at
!         that node, or 0 or a negative value.
!
!         If K = INDX(I,J) is positive, then the value of the degree
!         of freedom is stored in the solution vector entry G(K),
!         and an equation will be generated to determine its value.
!
!         If INDX(I,J) is not positive, then no equation is
!         generated to determine for variable I at node J, either because
!         the variable is specified in some other way, or because
!         (in the case of pressure), there is no coefficient associated
!         with that node.
!
!  MAXROW Input, integer MAXROW.
!
!         The first dimension of the matrix A.
!
!  NELEM  Input, integer NELEM, the number of elements.
!
!  NLBAND Output, integer NLBAND.
!
!         The lower bandwidth of the matrix A.  The zero structure of A
!         is assumed to be symmetric, and so NLBAND is also the upper
!         bandwidth of A.
!
!  NODE   Input, integer NODE(6,NELEM).
!
!         NODE(I,J) contains, for an element J, the global node index of
!         the element node whose local number is I.
!
!         The local ordering of the nodes is suggested by this diagram:
!
!               2
!              /|
!             4 5
!            /  |
!           1-6-3
!
!  NP     Input, integer NP, the number of nodes used to define the finite
!         element mesh.  NP = (2*NX-1)*(2*NY-1).
!
!  NROW   Output, integer NROW.
!
!         The number of rows need to store the matrix A, using the
!         LINPACK/LAPACK general banded storage format.  NROW must be
!         at least 3*NLBAND+1.
!
!
  integer nelem
  integer np
!
  integer i
  integer ielem
  integer indx(3,np)
  integer ip
  integer ipp
  integer iq
  integer iqq
  integer iuk
  integer iukk
  integer j
  integer maxrow
  integer nlband
  integer node(6,nelem)
  integer nrow
!
  nlband = 0

  do ielem = 1,nelem
    do iq = 1,6
      ip = node(iq,ielem)
      do iuk = 1,3
        i = indx(iuk,ip)
        if ( i>0 ) then
          do iqq = 1,6
            ipp = node(iqq,ielem)
            do iukk = 1,3
              j = indx(iukk,ipp)
              if ( j>0 ) then
                if ( j-i>nlband ) then
                  nlband = j-i
                end if
              end if
            end do
          end do
        end if
      end do
    end do
  end do

  nrow = 3*nlband+1

  if ( nrow>maxrow ) then
    write ( *, * ) ' '
    write ( *, * ) 'SetBan - Fatal error!'
    write ( *, * ) '  NROW is too large!  NROW =      ',nrow
    write ( *, * ) '  The maximum allowed is MAXROW = ',maxrow
    stop
  end if

  return
end
subroutine setbas(area,etaq,isotri,nelem,node,np,nquad,phi,xc,xquad,xsiq, &
  yc,yquad)
!
!*******************************************************************************
!
!! SETBAS computes the value of the basis functions at each
!  quadrature point.  The basis functions are computed and saved
!  in this way for efficiency.
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
!    Input, real AREA(NQUAD,NELEM).
!    AREA contains the area of each element.  These values are
!    needed when computed the integrals associated with the
!    finite element method.
!    For runs in which the region is allowed to change from
!    step to step, AREA must be recalculated at each step.
!
!  ISOTRI Input, integer ISOTRI(NELEM).
!         0, the element is NOT isoparametric.  The six node
!         triangle has straight sides.
!         1, the element is isoparametric.  The six node triangle
!         has curved sides.  Many computations involving such an
!         element must be computed by using a reference triangle,
!         and evaluating the jacobian of a transformation between
!         that triangle and the element.
!
!  NELEM  Input, integer NELEM, the number of elements.
!
!  NODE   Input, integer NODE(6,NELEM), contains the numbers
!         of the nodes that make up each element.  Element number
!         I is associated with nodes NODE(1,I) through NODE(6,I).
!
!  NP     Input, integer NP, the number of nodes.
!
!  PHI    Output, real PHI(NQUAD,6,10,NELEM),
!         contains lots of basis function values.  In particular,
!
!           PHI(I,J,K,1) contains the value of the basis function
!           associated with velocity (U or V), in the I-th element,
!           at the J-th quadrature point, associated with the
!           K-th node.
!
!           PHI(I,J,K,2) contains the X derivative, and
!           PHI(I,J,K,3) contains the Y derivative.
!
!           PHI(I,J,K,4) contains the value of the basis function
!           associated with pressure (P) in the I-th element,
!           at the J-th quadrature point, associated with the
!           K-th node.
!
!  XC     Input, real XC(NP), contains the X coordinates
!         of the nodes.
!
!  XQUAD  Input, real XQUAD(NQUAD,NELEM), contains the
!         X coordinates  of the quadrature points in a given element.
!
!  YC     Input, real YC(NP), contains the Y coordinates
!         of the nodes.
!
!  YQUAD  Input, real YQUAD(NQUAD,NELEM), contains the
!         Y coordinates of the quadrature points in a given element.
!
!
  integer nelem
  integer np
  integer nquad
!
  real ( kind = 8 ) area(nquad,nelem)
  real ( kind = 8 ) det
  real ( kind = 8 ) detadx
  real ( kind = 8 ) detady
  real ( kind = 8 ) dqdx
  real ( kind = 8 ) dqdy
  real ( kind = 8 ) dwdx
  real ( kind = 8 ) dwdy
  real ( kind = 8 ) dxsidx
  real ( kind = 8 ) dxsidy
  real ( kind = 8 ) eta
  real ( kind = 8 ) etaq(nquad)
  integer ielem
  integer iq
  integer isotri(nelem)
  integer j
  integer node(6,nelem)
  real ( kind = 8 ) phi(nquad,6,10,nelem)
  real ( kind = 8 ) q
  real ( kind = 8 ) w
  real ( kind = 8 ) xc(np)
  real ( kind = 8 ) xquad(nquad,nelem)
  real ( kind = 8 ) xq
  real ( kind = 8 ) xsi
  real ( kind = 8 ) xsiq(nquad)
  real ( kind = 8 ) yc(np)
  real ( kind = 8 ) yquad(nquad,nelem)
  real ( kind = 8 ) yq
!
!  Consider a particular element,
!  and a particular quadrature point (XQ,YQ) in that element.
!
!  Compute, at (XQ,YQ), the local values of the jacobian matrix
!  and its determinant.
!
!  Adjust the AREA array
!
  do ielem = 1,nelem

    do j = 1,nquad

      xq = xquad(j,ielem)
      yq = yquad(j,ielem)

      if ( isotri(ielem) == 2 ) then
        eta = etaq(j)
        xsi = xsiq(j)
        call trans(det,detadx,detady,dxsidx,dxsidy,eta,ielem,nelem,node, &
          np,xc,xsi,yc)
        area(j,ielem) = det*area(j,ielem)
      end if
!
!  Now consider each of the basis functions associated with a
!  node in the given element.
!
      do iq = 1,6
!
!  If the element is NOT isoparametric, compute the basis values
!  directly.
!
!  For isoparametric elements, use the reference triangle method.
!
        if ( isotri(ielem) == 0.or.isotri(ielem)== 1 ) then

          call bsp(q,dqdx,dqdy,ielem,iq,nelem,node,np,xc,xq,yc,yq)

          call qbf(ielem,iq,w,dwdx,dwdy,nelem,node,np,xc,xq,yc,yq)

          dxsidx = 1.0D+00
          dxsidy = 0.0D+00
          detadx = 0.0D+00
          detady = 1.0D+00

        else

          call refqbf(w,dwdx,dwdy,detadx,detady,dxsidx,dxsidy,eta,iq,xsi)

          call refbsp(q,dqdx,dqdy,detadx,detady,iq,dxsidx,dxsidy,eta,xsi)

        end if
!
!  Store the values into PHI.
!
        phi(j,iq,1,ielem) = w
        phi(j,iq,2,ielem) = dwdx
        phi(j,iq,3,ielem) = dwdy
        phi(j,iq,4,ielem) = q
        phi(j,iq,5,ielem) = dqdx
        phi(j,iq,6,ielem) = dqdy

        phi(j,iq,7,ielem) = dxsidx
        phi(j,iq,8,ielem) = dxsidy
        phi(j,iq,9,ielem) = detadx
        phi(j,iq,10,ielem) = detady

      end do
    end do
  end do

  return
end
subroutine setnod(eqn,ibump,indx,isotri,maxeqn,nelem,neqn,node,np,nx,ny, &
  xbl,xbr)
!
!*******************************************************************************
!
!! SETNOD assigns numbers to the nodes.
!
!  It numbers the elements, decides which elements shall be
!  isoparametric, (ISOTRI) and assigns six nodes to each (NODE).
!
!  It associates global unknown indices with each node (INDX).
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
!
  integer maxeqn
  integer nelem
  integer np
!
  character ( len = 2 ) eqn(maxeqn)
  integer ibump
  integer icol
  integer icol2
  integer ielem
  integer indx(3,np)
  integer ip
  integer irow
  integer irow2
  integer isotri(nelem)
  integer nbleft
  integer nbrite
  integer neqn
  integer node(6,nelem)
  integer nx
  integer ny
  real ( kind = 8 ) xbl
  real ( kind = 8 ) xbr
!
!  Compute the global node numbers that will be assigned to the
!  beginning and ending of the bump.  These numbers are only used to
!  determine which elements are isoparametric.
!
!  Here, we explicitly assume the region is 10.0 units long.
!
  nbleft = nint(xbl*(2*nx-2)/10.0)+1
  nbrite = nint(xbr*(2*nx-2)/10.0)+1
!
!  Consider each of the NP nodes, which logically lie in an MX by MY
!  rectangular array.  A pair of new elements must be generated every
!  time we reach a node that lies in an odd row and column, (except for
!  the top row, and last column, of course).  At every node, we
!  will have to decide how many equations to generate.
!
  ielem = 0
  neqn = 0

  do ip = 1,np
!
!  Determine the row and column of this node, and also whether each
!  of these quantities is odd or even.
!
    icol = ((ip-1)/(2*ny-1))+1
    irow = mod((ip-1),2*ny-1)+1

    icol2 = mod(icol,2)
    irow2 = mod(irow,2)
!
!  If both the row and the column are odd, and we're not in the last
!  column or top row, then we can define two new triangular elements
!  based at the node.
!
!  Given the following arrangement of nodes, for instance:
!
!    05 10 15 20 25
!    04 09 14 19 24
!    03 08 13 18 23
!    02 07 12 17 22
!    01 06 11 16 21
!
!  when we arrive at node 13, we will define
!
!    element 7: (25, 13, 15, 19, 14, 20)
!    element 8: (13, 25, 23, 19, 24, 18)
!
    if ( (irow2 == 1.and.icol2== 1).and.(icol/=2*nx-1).and.(irow/=2*ny-1) ) then

      ielem = ielem+1

      node(1,ielem) = ip+2*(2*ny-1)+2
      node(2,ielem) = ip
      node(3,ielem) = ip+2
      node(4,ielem) = ip+(2*ny-1)+1
      node(5,ielem) = ip+1
      node(6,ielem) = ip+(2*ny-1)+2

      if ( ibump == 0 ) then

        if ( icol>= nbleft.and.icol<nbrite ) then
          isotri(ielem) = 1
        else
          isotri(ielem) = 0
        end if

      else if (ibump == 1 ) then

        if ( icol>= nbleft.and.icol<nbrite ) then
          isotri(ielem) = 1
        else
          isotri(ielem) = 0
        end if

      else if (ibump == 2 ) then

        if ( icol>= nbleft.and.icol<nbrite ) then
          isotri(ielem) = 2
        else
          isotri(ielem) = 0
        end if

      else

        isotri(ielem) = 2

      end if

      ielem = ielem+1

      node(1,ielem) = ip
      node(2,ielem) = ip+2*(2*ny-1)+2
      node(3,ielem) = ip+2*(2*ny-1)
      node(4,ielem) = ip+(2*ny-1)+1
      node(5,ielem) = ip+2*(2*ny-1)+1
      node(6,ielem) = ip+(2*ny-1)

      if ( ibump == 0 ) then

        if ( icol>= nbleft.and.icol<nbrite ) then
          isotri(ielem) = 1
        else
          isotri(ielem) = 0
        end if

      else if (ibump == 1 ) then

        if ( irow == 1.and.icol>=nbleft.and.icol<nbrite ) then
          isotri(ielem) = 2
        else if (icol>= nbleft.and.icol<nbrite ) then
          isotri(ielem) = 1
        else
          isotri(ielem) = 0
        end if

      else if (ibump == 2 ) then

        if ( icol>= nbleft.and.icol<nbrite ) then
          isotri(ielem) = 2
        else
          isotri(ielem) = 0
        end if

      else

        isotri(ielem) = 2

      end if

    end if

    if ( neqn+2>maxeqn ) then
      write ( *, * ) ' '
      write ( *, * ) 'SetNod - Fatal error!'
      write ( *, * ) '  Too many unknowns!'
      write ( *, * ) '  Processing node IP = ',ip
      write ( *, * ) '  The maximum allowed is MAXEQN = ',maxeqn
      write ( *, * ) '  This problem requires NEQN = ',neqn+2
      stop
    end if
!
!  Now determine what equations to associate with this node.
!
!  The node lies on the left hand inflow boundary.
!  The horizontal and vertical velocities are specified.
!
    if ( icol == 1.and.1<irow.and.irow<2*ny-1 ) then

      neqn = neqn+1
      indx(1,ip) = neqn
      eqn(neqn) = 'UI'

      neqn = neqn+1
      indx(2,ip) = neqn
      eqn(neqn) = 'VI'
!
!  The node lies on the right hand boundary.
!  The horizontal velocity is an unknown, the vertical velocity is zero.
!
    else if (icol == 2*nx-1.and.1<irow.and.irow<2*ny-1 ) then

      neqn = neqn+1
      indx(1,ip) = neqn
      eqn(neqn) = 'U'

      neqn = neqn+1
      indx(2,ip) = neqn
      eqn(neqn) = 'VW'
!
!  The node lies on the moving bump surface.
!  The horizontal and vertical velocities are zero.
!
    else if (irow == 1.and.icol>nbleft.and.icol<nbrite ) then

      neqn = neqn+1
      indx(1,ip) = neqn
      eqn(neqn) = 'UB'

      neqn = neqn+1
      indx(2,ip) = neqn
      eqn(neqn) = 'VB'
!
!  The node lies on a fixed wall.
!  The horizontal and vertical velocities are zero.
!
    else if (icol == 1.or.icol== 2*nx-1.or.(irow== 1.and.icol<=nbleft).or. &
      (irow == 1.and.icol>=nbrite).or.irow== 2*ny-1 ) then

      neqn = neqn+1
      indx(1,ip) = neqn
      eqn(neqn) = 'UW'

      neqn = neqn+1
      indx(2,ip) = neqn
      eqn(neqn) = 'VW'
!
!  The node is a normal interior node.
!  The horizontal and vertical velocities are unknown.
!
    else

      neqn = neqn+1
      indx(1,ip) = neqn
      eqn(neqn) = 'U'

      neqn = neqn+1
      indx(2,ip) = neqn
      eqn(neqn) = 'V'

    end if
!
!  On nodes in an odd row and column, add a pressure equation.
!
    if ( irow2 == 1.and.icol2== 1 ) then

      neqn = neqn+1

      if ( neqn>maxeqn ) then
        write ( *, * ) ' '
        write ( *, * ) 'SetNod - Fatal error!'
        write ( *, * ) '  Too many unknowns!'
        write ( *, * ) '  Processing node IP = ',ip
        write ( *, * ) '  The maximum allowed is MAXEQN = ',maxeqn
        write ( *, * ) '  This problem requires NEQN = ',neqn
        stop
      end if

      indx(3,ip) = neqn
      eqn(neqn) = 'P'
    else
      indx(3,ip) = 0
    end if

  end do
!
!  The last equation, which is guaranteed to be a pressure equation,
!  is replaced by a pressure boundary condition, associated with
!  an unknown.  (Even though we know this pressure will be zero).
!
  eqn(neqn) = 'PB'

  write ( *, * ) ' '
  write ( *, * ) 'SetNod - Note:'
  write ( *, * ) '  The number of unknowns is NEQN = ',neqn

  return
end
subroutine setq3(area,etaq,isotri,nelem,node,np,nquad,wquad,xc,xquad, &
  xsiq,yc,yquad)
!
!*******************************************************************************
!
!! SETQ3 sets the abscissas and weights for a three point quadrature
!  rule on a triangle.
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
!  AREA   Output, real AREA(NQUAD,NELEM).
!
!  XQUAD  Output, real XQUAD(NQUAD,NELEM).
!
!         The X coordinates of the quadrature points for
!         each element.
!
!  YQUAD  Output, real YQUAD(NQUAD,NELEM).
!
!         The Y coordinates of the quadrature points for
!         each element.
!
!
  integer nelem
  integer np
  integer nquad
!
  real ( kind = 8 ) area(nquad,nelem)
  real ( kind = 8 ) eta
  real ( kind = 8 ) etaq(nquad)
  integer i
  integer ielem
  integer ip1
  integer ip2
  integer ip3
  integer iquad
  integer isotri(nelem)
  integer node(6,nelem)
  real ( kind = 8 ) wquad(nquad)
  real ( kind = 8 ) x
  real ( kind = 8 ) xc(np)
  real ( kind = 8 ) xquad(nquad,nelem)
  real ( kind = 8 ) xsi
  real ( kind = 8 ) xsiq(nquad)
  real ( kind = 8 ) y
  real ( kind = 8 ) yc(np)
  real ( kind = 8 ) yquad(nquad,nelem)
!
!  Set the weights.
!
  do i = 1,nquad
    wquad(i) = 1.0D+00/6.0D+00
  end do
!
!  Set the quadrature points in the reference element.
!
  xsiq(1) = 0.5D+00
  etaq(1) = 0.5D+00

  xsiq(2) = 1.0D+00
  etaq(2) = 0.5D+00

  xsiq(3) = 0.5D+00
  etaq(3) = 0.0D+00
!
!  Set the X, Y coordinates of quadrature points for each element.
!
  do ielem = 1,nelem

    do i = 1,nquad
      xsi = xsiq(i)
      eta = etaq(i)
      call xofxsi(eta,ielem,nelem,node,np,x,xc,xsi,y,yc)
      xquad(i,ielem) = x
      yquad(i,ielem) = y
    end do
!
!  We only calculate true areas for nonisoparametric elements.
!
    ip1 = node(1,ielem)
    ip2 = node(2,ielem)
    ip3 = node(3,ielem)

    do iquad = 1,nquad

      if ( isotri(ielem) == 0.or.isotri(ielem)== 1 ) then

        area(iquad,ielem) = wquad(iquad)*abs( &
              (yc(ip1)+yc(ip2))*(xc(ip2)-xc(ip1)) &
             +(yc(ip2)+yc(ip3))*(xc(ip3)-xc(ip2)) &
             +(yc(ip3)+yc(ip1))*(xc(ip1)-xc(ip3)) )

      else

        area(iquad,ielem) = wquad(iquad)

      end if

    end do

  end do

  return
end
subroutine setq7(area,etaq,isotri,nelem,node,np,nquad,wquad,xc,xquad, &
  xsiq,yc,yquad)
!
!*******************************************************************************
!
!! SETQ7 sets the abscissas and weights for a seven point quadrature
!  rule on a triangle.
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
!  AREA   Output, real AREA(NQUAD,NELEM).
!
!  XQUAD  Output, real XQUAD(NQUAD,NELEM).
!
!         The X coordinates of the quadrature points for
!         each element.
!
!  YQUAD  Output, real YQUAD(NQUAD,NELEM).
!
!         The Y coordinates of the quadrature points for
!         each element.
!
!
  integer nelem
  integer np
  integer nquad
!
  real ( kind = 8 ) area(nquad,nelem)
  real ( kind = 8 ) eta
  real ( kind = 8 ) etaq(nquad)
  integer i
  integer ielem
  integer ip1
  integer ip2
  integer ip3
  integer iquad
  integer isotri(nelem)
  integer node(6,nelem)
  real ( kind = 8 ) wquad(nquad)
  real ( kind = 8 ) x
  real ( kind = 8 ) xc(np)
  real ( kind = 8 ) xquad(nquad,nelem)
  real ( kind = 8 ) xsi
  real ( kind = 8 ) xsiq(nquad)
  real ( kind = 8 ) y
  real ( kind = 8 ) yc(np)
  real ( kind = 8 ) yquad(nquad,nelem)
!
!  Set the weights.
!
  wquad(1) = 0.1125165D+00
  wquad(2) = (155.0D+00+sqrt(15.0D+00))/2400.0D+00
  wquad(3) = (155.0D+00+sqrt(15.0D+00))/2400.0D+00
  wquad(4) = (155.0D+00+sqrt(15.0D+00))/2400.0D+00
  wquad(5) = (155.0D+00-sqrt(15.0D+00))/2400.0D+00
  wquad(6) = (155.0D+00-sqrt(15.0D+00))/2400.0D+00
  wquad(7) = (155.0D+00-sqrt(15.0D+00))/2400.0D+00
!
!  WQUAD(1) is approximate.  Revise it to force the weights
!  to sum to 1/2.
!
  wquad(1) = 0.5D+00
  do i = 2,7
    wquad(1) = wquad(1)-wquad(i)
  end do
!
!  Set the quadrature abscissas for the standard element.
!
  xsiq(1) = 1.0D+00/3.0D+00
  etaq(1) = 1.0D+00/3.0D+00

  xsiq(2) = (6.0D+00+sqrt(15.0))/21.0D+00
  etaq(2) = (6.0D+00+sqrt(15.0))/21.0D+00

  xsiq(3) = (9.0D+00-2.0*sqrt(15.0))/21.0D+00
  etaq(3) = (6.0D+00+sqrt(15.0))/21.0D+00

  xsiq(4) = (6.0D+00+sqrt(15.0))/21.0D+00
  etaq(4) = (9.0D+00-2.0*sqrt(15.0))/21.0D+00

  xsiq(5) = (6.0D+00-sqrt(15.0))/21.0D+00
  etaq(5) = (6.0D+00-sqrt(15.0))/21.0D+00

  xsiq(6) = (9.0D+00+2.0*sqrt(15.0))/21.0D+00
  etaq(6) = (6.0D+00-sqrt(15.0))/21.0D+00

  xsiq(7) = (6.0D+00-sqrt(15.0))/21.0D+00
  etaq(7) = (9.0D+00+2.0*sqrt(15.0))/21.0D+00
!
!  Now reset the quadrature abscissas for our nonstandard element.
!
  do i = 1,nquad
    xsiq(i) = 1.0D+00-xsiq(i)
  end do
!
!  Set the X, Y coordinates of quadrature points for each element.
!
  do ielem = 1,nelem

    do i = 1,nquad
      xsi = xsiq(i)
      eta = etaq(i)
      call xofxsi(eta,ielem,nelem,node,np,x,xc,xsi,y,yc)
      xquad(i,ielem) = x
      yquad(i,ielem) = y
    end do
!
!  We only calculate true areas for nonisoparametric elements.
!
    ip1 = node(1,ielem)
    ip2 = node(2,ielem)
    ip3 = node(3,ielem)

    do iquad = 1,nquad

      if ( isotri(ielem) == 0.or.isotri(ielem)== 1 ) then

        area(iquad,ielem) = wquad(iquad)*abs( &
              (yc(ip1)+yc(ip2))*(xc(ip2)-xc(ip1)) &
             +(yc(ip2)+yc(ip3))*(xc(ip3)-xc(ip2)) &
             +(yc(ip3)+yc(ip1))*(xc(ip1)-xc(ip3)) )

      else

        area(iquad,ielem) = wquad(iquad)

      end if

    end do

  end do

  return
end
subroutine setxy(ibs,np,nparb,nx,ny,splbmp,taubmp,xbl,xbr,xc,ybl,ybr,yc)
!
!*******************************************************************************
!
!! SETXY sets the X and Y coordinates of the nodes.
!
!
!  This version of SETXY assumes that the nodes are numbered
!  in "stacks", starting with the least X and Y coordinates,
!  then fixing X and running through all values of Y, then
!  increasing X to the next value and running through all
!  values of Y, and so on.  For example:
!
!    5  10  15
!    4   9  14
!    3   8  13
!    2   7  12
!    1   6  11
!
!  This allows us to treat the vectors XC and YC as two dimensional
!  arrays, instead of the one dimensional vectors they are declared
!  to be.
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
  integer np
  integer nparb
  integer nx
  integer ny

  integer i
  integer ibs
  integer ip
  integer j
  integer jderiv
  real ( kind = 8 ) splbmp(4,nparb+2,0:nparb)
  real ( kind = 8 ) taubmp(nparb+2)
  real ( kind = 8 ) x
  real ( kind = 8 ) xbl
  real ( kind = 8 ) xbr
  real ( kind = 8 ) xc(np)
  real ( kind = 8 ) y
  real ( kind = 8 ) ybl
  real ( kind = 8 ) ybot
  real ( kind = 8 ) ybr
  real ( kind = 8 ) yc(np)

  ip = 0

  do i = 1,2*nx-1

    x = real ( i - 1, kind = 8 ) * 10.0D+00 &
      / real ( 2 * nx - 2, kind = 8 )

    if ( abs(x-xbl)*(2*nx-2)<= 0.5D+00 ) then
      x = xbl
    else if (abs(x-xbr)*(2*nx-2)<= 0.5D+00 ) then
      x = xbr
    end if

    do j = 1,2*ny-1

      ip = ip+1

      if ( x<= xbl ) then
        ybot = ybl
      else if (x>= xbl.and.x<=xbr ) then
        if ( ibs == 1 ) then
          call plval(nparb+2,x,taubmp,ybot,splbmp)
        else if (ibs == 2 ) then
          call pqval(nparb+2,x,taubmp,ybot,splbmp)
        else if (ibs ==3 ) then
          jderiv = 0
          call ppvalu(taubmp,splbmp(1,1,0),nparb+1,4,x,jderiv,ybot)
        end if
      else
        ybot = ybr
      end if

      y = (real(2*ny-1-j, kind = 8 )*ybot+real(j-1, kind = 8 )*3.0D+00)/real(2*ny-2, kind = 8 )

      xc(ip) = x
      yc(ip) = y

    end do
  end do

  return
end
subroutine sgbtf2(m,n,kl,ku,ab,ldab,ipiv,info)

!*******************************************************************************
!
!! SGBTF2 computes an LU factorization of a real m-by-n band matrix A
!  using partial pivoting with row interchanges.
!
!  This is the unblocked version of the algorithm, calling Level 2 BLAS.
!
!
!  Parameters:
!
!
!  M       (input) INTEGER
!          The number of rows of the matrix A.  M >=  0.
!
!  N       (input) INTEGER
!          The number of columns of the matrix A.  N >=  0.
!
!  KL      (input) INTEGER
!          The number of subdiagonals within the band of A.  KL >=  0.
!
!  KU      (input) INTEGER
!          The number of superdiagonals within the band of A.  KU >=  0.
!
!  AB      (input/output) real array, dimension (LDAB,N)
!          On entry, the matrix A in band storage, in rows KL+1 to
!          2*KL+KU+1; rows 1 to KL of the array need not be set.
!          The j-th column of A is stored in the j-th column of the
!          array AB as follows:
!          AB(kl+ku+1+i-j,j) = A(i,j) for max(1,j-ku)<=i<=min(m,j+kl)
!
!          On exit, details of the factorization: U is stored as an
!          upper triangular band matrix with KL+KU superdiagonals in
!          rows 1 to KL+KU+1, and the multipliers used during the
!          factorization are stored in rows KL+KU+2 to 2*KL+KU+1.
!          See below for further details.
!
!  LDAB    (input) INTEGER
!          The leading dimension of the array AB.  LDAB >=  2*KL+KU+1.
!
!  IPIV    (output) INTEGER array, dimension (min(M,N))
!          The pivot indices; for 1 <=  i <= min(M,N), row i of the
!          matrix was interchanged with row IPIV(i).
!
!  INFO    (output) INTEGER
!          = 0: successful exit
!          < 0: if INFO = -i, the i-th argument had an illegal value
!          > 0: if INFO = +i, U(i,i) is exactly zero. The factorization
!               has been completed, but the factor U is exactly
!               singular, and division by zero will occur if it is used
!               to solve a system of equations.
!
!  Further Details
!   ===============
!
!  The band storage scheme is illustrated by the following example, when
!  M = N = 6, KL = 2, KU = 1:
!
!  On entry:                       On exit:
!
!      *    *    *    +    +    +       *    *    *   u14  u25  u36
!      *    *    +    +    +    +       *    *   u13  u24  u35  u46
!      *   a12  a23  a34  a45  a56      *   u12  u23  u34  u45  u56
!     a11  a22  a33  a44  a55  a66     u11  u22  u33  u44  u55  u66
!     a21  a32  a43  a54  a65   *      m21  m32  m43  m54  m65   *
!     a31  a42  a53  a64   *    *      m31  m42  m53  m64   *    *
!
!  Array elements marked * are not used by the routine; elements marked
!  + need not be set on entry, but are required by the routine to store
!  elements of U, because of fill-in resulting from the row
!  interchanges.
!
  integer            info, kl, ku, ldab, m, n
  integer            ipiv( * )
  real ( kind = 8 )   ab( ldab, * )

  real ( kind = 8 ), parameter :: one = 1.0D+00
  real ( kind = 8 ), parameter :: zero = 0.0D+00
!     ..
!     .. Local Scalars ..
  integer            i, j, jp, ju, km, kv
!     ..
!     .. External Functions ..
  integer            isamax
  external           isamax
!     ..
!     .. External Subroutines ..
  external           sger, sscal, sswap, xerbla
!     ..
!     .. Intrinsic Functions ..
  intrinsic          max, min
!     ..
!     .. Executable Statements ..
!
!     KV is the number of superdiagonals in the factor U, allowing for
!     fill-in.
!
  kv = ku + kl
!
!     Test the input parameters.
!
  info = 0
  if ( m<0 ) then
     info = -1
  else if ( n<0 ) then
     info = -2
  else if ( kl<0 ) then
     info = -3
  else if ( ku<0 ) then
     info = -4
  else if ( ldab<kl+kv+1 ) then
     info = -6
  end if
  if ( info/= 0 ) then
     call xerbla( 'sgbtf2', -info )
     return
  end if
!
!     Quick return if possible
!
  if ( m == 0 .or. n==0 ) return
!
!     Gaussian elimination with partial pivoting
!
!     Set fill-in elements in columns KU+2 to KV to zero.
!
  do j = ku + 2, min( kv, n )
    do i = kv - j + 2, kl
      ab( i, j ) = zero
    end do
  end do
!
!     JU is the index of the last column affected by the current stage
!     of the factorization.
!
  ju = 1
!
  do 40 j = 1, min( m, n )
!
!        Set fill-in elements in column J+KV to zero.
!
     if ( j+kv<= n ) then
        do 30 i = 1, kl
           ab( i, j+kv ) = zero
   30       continue
     end if
!
!        Find pivot and test for singularity. KM is the number of
!        subdiagonal elements in the current column.
!
     km = min( kl, m-j )
     jp = isamax( km+1, ab( kv+1, j ), 1 )
     ipiv( j ) = jp + j - 1
     if ( ab( kv+jp, j )/= zero ) then
        ju = max( ju, min( j+ku+jp-1, n ) )
!
!           Apply interchange to columns J to JU.
!
        if ( jp/= 1 ) call sswap ( ju-j+1, ab(kv+jp,j), ldab-1, ab(kv+1,j), &
          ldab-1 )
!
        if ( km>0 ) then
!
!              Compute multipliers.
!
           call sscal( km, one / ab( kv+1, j ), ab( kv+2, j ), 1 )
!
!              Update trailing submatrix within the band.
!
           if ( ju>j ) call sger( km, ju-j, -one, ab( kv+2, j ), 1, &
             ab( kv, j+1 ), ldab-1, ab( kv+1, j+1 ), ldab-1 )
        end if
     else
!
!           If pivot is zero, set INFO to the index of the pivot
!           unless a zero pivot has already been found.
!
        if ( info == 0 ) info = j
     end if
   40 continue
  return
!
!     End of SGBTF2
!
end
subroutine sgbtrf(m,n,kl,ku,ab,ldab,ipiv,info)
!
!*******************************************************************************
!
!! SGBTRF computes an LU factorization of a real m-by-n band matrix A
!  using partial pivoting with row interchanges.
!
!  This is the blocked version of the algorithm, calling Level 3 BLAS.
!
!  Parameters:
!
!  M       (input) INTEGER
!          The number of rows of the matrix A.  M >=  0.
!
!  N       (input) INTEGER
!          The number of columns of the matrix A.  N >=  0.
!
!  KL      (input) INTEGER
!          The number of subdiagonals within the band of A.  KL >=  0.
!
!  KU      (input) INTEGER
!          The number of superdiagonals within the band of A.  KU >=  0.
!
!  AB      (input/output) real array, dimension (LDAB,N)
!          On entry, the matrix A in band storage, in rows KL+1 to
!          2*KL+KU+1; rows 1 to KL of the array need not be set.
!          The j-th column of A is stored in the j-th column of the
!          array AB as follows:
!          AB(kl+ku+1+i-j,j) = A(i,j) for max(1,j-ku)<=i<=min(m,j+kl)
!
!          On exit, details of the factorization: U is stored as an
!          upper triangular band matrix with KL+KU superdiagonals in
!          rows 1 to KL+KU+1, and the multipliers used during the
!          factorization are stored in rows KL+KU+2 to 2*KL+KU+1.
!          See below for further details.
!
!  LDAB    (input) INTEGER
!          The leading dimension of the array AB.  LDAB >=  2*KL+KU+1.
!
!  IPIV    (output) INTEGER array, dimension (min(M,N))
!          The pivot indices; for 1 <=  i <= min(M,N), row i of the
!          matrix was interchanged with row IPIV(i).
!
!  INFO    (output) INTEGER
!          = 0: successful exit
!          < 0: if INFO = -i, the i-th argument had an illegal value
!          > 0: if INFO = +i, U(i,i) is exactly zero. The factorization
!               has been completed, but the factor U is exactly
!               singular, and division by zero will occur if it is used
!               to solve a system of equations.
!
!  Further Details
!   ===============
!
!  The band storage scheme is illustrated by the following example, when
!  M = N = 6, KL = 2, KU = 1:
!
!  On entry:                       On exit:
!
!      *    *    *    +    +    +       *    *    *   u14  u25  u36
!      *    *    +    +    +    +       *    *   u13  u24  u35  u46
!      *   a12  a23  a34  a45  a56      *   u12  u23  u34  u45  u56
!     a11  a22  a33  a44  a55  a66     u11  u22  u33  u44  u55  u66
!     a21  a32  a43  a54  a65   *      m21  m32  m43  m54  m65   *
!     a31  a42  a53  a64   *    *      m31  m42  m53  m64   *    *
!
!  Array elements marked * are not used by the routine; elements marked
!  + need not be set on entry, but are required by the routine to store
!  elements of U because of fill-in resulting from the row interchanges.
!

  integer            info, kl, ku, ldab, m, n

  integer            ipiv( * )
  real ( kind = 8 )   ab( ldab, * )
!     .. Parameters ..

  real ( kind = 8 ), parameter :: one = 1.0E+0
  real ( kind = 8 ), parameter :: zero = 0.0E+0

  integer, parameter :: nbmax = 64
  integer, parameter :: ldwork = nbmax + 1
!     ..
!     .. Local Scalars ..
  integer            i, i2, i3, ii, ip, j, j2, j3, jb, jj, jm, jp
  integer ju, k2, km, kv, nb, nw
  real ( kind = 8 )   temp
!     ..
!     .. Local Arrays ..
  real ( kind = 8 )   work13( ldwork, nbmax ), work31( ldwork, nbmax )
!     ..
!     .. External Functions ..
  integer            isamax, ilaenv
  external           isamax, ilaenv
!     ..
!     .. External Subroutines ..
  external scopy, sgbtf2, sgemm, sger, slaswp, sscal,sswap, strsm, xerbla
!     ..
!     .. Intrinsic Functions ..
  intrinsic          max, min
!     ..
!     .. Executable Statements ..
!
!     KV is the number of superdiagonals in the factor U, allowing for
!     fill-in
!
  kv = ku + kl
!
!     Test the input parameters.
!
  info = 0
  if ( m<0 ) then
     info = -1
  else if ( n<0 ) then
     info = -2
  else if ( kl<0 ) then
     info = -3
  else if ( ku<0 ) then
     info = -4
  else if ( ldab<kl+kv+1 ) then
     info = -6
  end if
  if ( info/= 0 ) then
     call xerbla( 'sgbtrf', -info )
     return
  end if
!
!     Quick return if possible
!
  if ( m == 0 .or. n==0 ) return
!
!     Determine the block size for this environment
!
  nb = ilaenv( 1, 'sgbtrf', ' ', m, n, kl, ku )
!
!     The block size must not exceed the limit set by the size of the
!     local arrays WORK13 and WORK31.
!
  nb = min( nb, nbmax )
!
  if ( nb<= 1 .or. nb>kl ) then
!
!        Use unblocked code
!
     call sgbtf2( m, n, kl, ku, ab, ldab, ipiv, info )
  else
!
!        Use blocked code
!
!        Zero the superdiagonal elements of the work array WORK13
!
     do j = 1, nb
        do i = 1, j - 1
           work13( i, j ) = zero
        end do
     end do
!
!        Zero the subdiagonal elements of the work array WORK31
!
     do 40 j = 1, nb
        do 30 i = j + 1, nb
           work31( i, j ) = zero
   30       continue
   40    continue
!
!        Gaussian elimination with partial pivoting
!
!        Set fill-in elements in columns KU+2 to KV to zero
!
     do 60 j = ku + 2, min( kv, n )
        do 50 i = kv - j + 2, kl
           ab( i, j ) = zero
   50       continue
   60    continue
!
!        JU is the index of the last column affected by the current
!        stage of the factorization
!
     ju = 1
!
     do 180 j = 1, min( m, n ), nb
        jb = min( nb, min( m, n )-j+1 )
!
!           The active part of the matrix is partitioned
!
!              A11   A12   A13
!              A21   A22   A23
!              A31   A32   A33
!
!           Here A11, A21 and A31 denote the current block of JB columns
!           which is about to be factorized. The number of rows in the
!           partitioning are JB, I2, I3 respectively, and the numbers
!           of columns are JB, J2, J3. The superdiagonal elements of A13
!           and the subdiagonal elements of A31 lie outside the band.
!
        i2 = min( kl-jb, m-j-jb+1 )
        i3 = min( jb, m-j-kl+1 )
!
!           J2 and J3 are computed after JU has been updated.
!
!           Factorize the current block of JB columns
!
        do 80 jj = j, j + jb - 1
!
!              Set fill-in elements in column JJ+KV to zero
!
           if ( jj+kv<= n ) then
              do 70 i = 1, kl
                 ab( i, jj+kv ) = zero
   70             continue
           end if
!
!              Find pivot and test for singularity. KM is the number of
!              subdiagonal elements in the current column.
!
           km = min( kl, m-jj )
           jp = isamax( km+1, ab( kv+1, jj ), 1 )
           ipiv( jj ) = jp + jj - j
           if ( ab( kv+jp, jj )/= zero ) then
              ju = max( ju, min( jj+ku+jp-1, n ) )
              if ( jp/= 1 ) then
!
!                    Apply interchange to columns J to J+JB-1
!
                 if ( jp+jj-1<j+kl ) then
!
                    call sswap( jb, ab( kv+1+jj-j, j ), ldab-1, &
                                    ab( kv+jp+jj-j, j ), ldab-1 )
                 else
!
!                       The interchange affects columns J to JJ-1 of A31
!                       which are stored in the work array WORK31
!
                    call sswap( jj-j, ab( kv+1+jj-j, j ), ldab-1, &
                                   work31( jp+jj-j-kl, 1 ), ldwork )
                    call sswap( j+jb-jj, ab( kv+1, jj ), ldab-1, &
                                    ab( kv+jp, jj ), ldab-1 )
                 end if
              end if
!
!                 Compute multipliers
!
              call sscal( km, one / ab( kv+1, jj ), ab( kv+2, jj ), 1 )
!
!                 Update trailing submatrix within the band and within
!                 the current block. JM is the index of the last column
!                 which needs to be updated.
!
              jm = min( ju, j+jb-1 )
              if ( jm>jj ) call sger( km, jm-jj, -one, ab( kv+2, jj ), 1, &
                ab( kv, jj+1 ), ldab-1,ab( kv+1, jj+1 ), ldab-1 )
           else
!
!                 If pivot is zero, set INFO to the index of the pivot
!                 unless a zero pivot has already been found.
!
              if ( info == 0 ) info = jj
           end if
!
!              Copy current column of A31 into the work array WORK31
!
           nw = min( jj-j+1, i3 )
           if ( nw>0 ) call scopy( nw, ab(kv+kl+1-jj+j,jj), 1, &
             work31(1,jj-j+1), 1 )
   80       continue
        if ( j+jb<= n ) then
!
!              Apply the row interchanges to the other blocks.
!
           j2 = min( ju-j+1, kv ) - jb
           j3 = max( 0, ju-j-kv+1 )
!
!              Use slaswp to apply the row interchanges to A12, A22, and
!              A32.
!
           call slaswp( j2, ab( kv+1-jb, j+jb ), ldab-1, 1, jb, ipiv(j), 1 )
!
!              Adjust the pivot indices.
!
           do 90 i = j, j + jb - 1
              ipiv( i ) = ipiv( i ) + j - 1
   90          continue
!
!              Apply the row interchanges to A13, A23, and A33
!              columnwise.
!
           k2 = j - 1 + jb + j2
           do 110 i = 1, j3
              jj = k2 + i
              do 100 ii = j + i - 1, j + jb - 1
                 ip = ipiv( ii )
                 if ( ip/= ii ) then
                    temp = ab( kv+1+ii-jj, jj )
                    ab( kv+1+ii-jj, jj ) = ab( kv+1+ip-jj, jj )
                    ab( kv+1+ip-jj, jj ) = temp
                 end if
  100             continue
  110          continue
!
!              Update the relevant part of the trailing submatrix
!
           if ( j2>0 ) then
!
!                 Update A12
!
              call strsm( 'left', 'lower', 'no transpose', 'unit', &
                jb, j2, one, ab( kv+1,j), ldab-1, ab(kv+1-jb, j+jb ), ldab-1 )
!
              if ( i2>0 ) then
!
!                    Update A22
!
                 call sgemm( 'no transpose', 'no transpose', i2, j2, &
                                 jb, -one, ab( kv+1+jb, j ), ldab-1, &
                                 ab( kv+1-jb, j+jb ), ldab-1, one, &
                                 ab( kv+1, j+jb ), ldab-1 )
              end if
!
              if ( i3>0 ) then
!
!                    Update A32
!
                 call sgemm( 'no transpose', 'no transpose', i3, j2, &
                                 jb, -one, work31, ldwork, &
                                 ab( kv+1-jb, j+jb ), ldab-1, one, &
                                 ab( kv+kl+1-jb, j+jb ), ldab-1 )
              end if
           end if
!
           if ( j3>0 ) then
!
!                 Copy the lower triangle of A13 into the work array
!                 WORK13
!
              do 130 jj = 1, j3
                 do 120 ii = jj, jb
                    work13( ii, jj ) = ab( ii-jj+1, jj+j+kv-1 )
  120                continue
  130             continue
!
!                 Update A13 in the work array
!
              call strsm( 'left', 'lower', 'no transpose', 'unit', &
                              jb, j3, one, ab( kv+1, j ), ldab-1, &
                              work13, ldwork )
!
              if ( i2>0 ) then
!
!                    Update A23
!
                 call sgemm( 'no transpose', 'no transpose', i2, j3, &
                                 jb, -one, ab( kv+1+jb, j ), ldab-1, &
                                 work13, ldwork, one, ab( 1+jb, j+kv ), &
                                 ldab-1 )
              end if
!
              if ( i3>0 ) then
!
!                    Update A33
!
                 call sgemm( 'no transpose', 'no transpose', i3, j3, &
                                 jb, -one, work31, ldwork, work13, &
                                 ldwork, one, ab( 1+kl, j+kv ), ldab-1 )
              end if
!
!                 Copy the lower triangle of A13 back into place
!
              do 150 jj = 1, j3
                 do 140 ii = jj, jb
                    ab( ii-jj+1, jj+j+kv-1 ) = work13( ii, jj )
  140                continue
  150             continue
           end if
        else
!
!              Adjust the pivot indices.
!
           do 160 i = j, j + jb - 1
              ipiv( i ) = ipiv( i ) + j - 1
  160          continue
        end if
!
!           Partially undo the interchanges in the current block to
!           restore the upper triangular form of A31 and copy the upper
!           triangle of A31 back into place
!
        do 170 jj = j + jb - 1, j, -1
           jp = ipiv( jj ) - jj + 1
           if ( jp/= 1 ) then
!
!                 Apply interchange to columns J to JJ-1
!
              if ( jp+jj-1<j+kl ) then
!
!                    The interchange does not affect A31
!
                 call sswap( jj-j, ab( kv+1+jj-j, j ), ldab-1, &
                                 ab( kv+jp+jj-j, j ), ldab-1 )
              else
!
!                    The interchange does affect A31
!
                 call sswap( jj-j, ab( kv+1+jj-j, j ), ldab-1, &
                                 work31( jp+jj-j-kl, 1 ), ldwork )
              end if
           end if
!
!              Copy the current column of A31 back into place
!
           nw = min( i3, jj-j+1 )
           if ( nw>0 )call scopy( nw, work31( 1, jj-j+1 ), 1, &
                              ab( kv+kl+1-jj+j, jj ), 1 )
  170       continue
  180    continue
  end if
!
  return
!
!     End of SGBTRF
!
end
subroutine sgbtrs(trans,n,kl,ku,nrhs,ab,ldab,ipiv,b,ldb,info)
!
!*******************************************************************************
!
!! SGBTRS solves a system of linear equations
!     A * X = B  or  A' * X = B
!  with a general band matrix A using the LU factorization computed
!  by SGBTRF.
!
!  Parameters:
!
!  TRANS   (input) CHARACTER*1
!          Specifies the form of the system of equations.
!          = 'N':  A * X = B  (No transpose)
!          = 'T':  A'* X = B  (Transpose)
!          = 'C':  A'* X = B  (Conjugate transpose = Transpose)
!
!  N       (input) INTEGER
!          The order of the matrix A.  N >=  0.
!
!  KL      (input) INTEGER
!          The number of subdiagonals within the band of A.  KL >=  0.
!
!  KU      (input) INTEGER
!          The number of superdiagonals within the band of A.  KU >=  0.
!
!  NRHS    (input) INTEGER
!          The number of right hand sides, i.e., the number of columns
!          of the matrix B.  NRHS >=  0.
!
!  AB      (input) real array, dimension (LDAB,N)
!          Details of the LU factorization of the band matrix A, as
!          computed by SGBTRF.  U is stored as an upper triangular band
!          matrix with KL+KU superdiagonals in rows 1 to KL+KU+1, and
!          the multipliers used during the factorization are stored in
!          rows KL+KU+2 to 2*KL+KU+1.
!
!  LDAB    (input) INTEGER
!          The leading dimension of the array AB.  LDAB >=  2*KL+KU+1.
!
!  IPIV    (input) INTEGER array, dimension (N)
!          The pivot indices; for 1 <=  i <= N, row i of the matrix was
!          interchanged with row IPIV(i).
!
!  B       (input/output) real array, dimension (LDB,NRHS)
!          On entry, the right hand side vectors B for the system of
!          linear equations.
!          On exit, the solution vectors, X.
!
!  LDB     (input) INTEGER
!          The leading dimension of the array B.  LDB >=  max(1,N).
!
!  INFO    (output) INTEGER
!          = 0:  successful exit
!          < 0: if INFO = -k, the k-th argument had an illegal value
!
  character          trans
  integer            info, kl, ku, ldab, ldb, n, nrhs
!     ..
!     .. Array Arguments ..
  integer            ipiv( * )
  real ( kind = 8 )   ab( ldab, * ), b( ldb, * )
!
!     .. Parameters ..
  real ( kind = 8 ), parameter :: one = 1.0E+0
!     ..
!     .. Local Scalars ..
  logical            lnoti, notran
  integer            i, j, kd, l, lm
!     ..
!     .. External Functions ..
  logical            lsame
  external           lsame
!     ..
!     .. External Subroutines ..
  external           sgemv, sger, sswap, stbsv, xerbla
!     ..
!     .. Intrinsic Functions ..
  intrinsic          max, min
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
  info = 0
  notran = lsame( trans, 'n' )
  if ( .not.notran .and. .not.lsame( trans, 't' ) .and. .not. &
         lsame( trans, 'c' ) ) then
     info = -1
  else if ( n<0 ) then
     info = -2
  else if ( kl<0 ) then
     info = -3
  else if ( ku<0 ) then
     info = -4
  else if ( nrhs<0 ) then
     info = -5
  else if ( ldab<( 2*kl+ku+1 ) ) then
     info = -7
  else if ( ldb<max( 1, n ) ) then
     info = -10
  end if
  if ( info/= 0 ) then
     call xerbla( 'sgbtrs', -info )
     return
  end if
!
!     Quick return if possible
!
  if ( n == 0 .or. nrhs==0 ) return
!
  kd = ku + kl + 1
  lnoti = kl>0
!
  if ( notran ) then
!
!        Solve  A*X = B.
!
!        Solve L*X = B, overwriting B with X.
!
!        L is represented as a product of permutations and unit lower
!        triangular matrices L = P(1) * L(1) * ... * P(n-1) * L(n-1),
!        where each transformation L(i) is a rank-one modification of
!        the identity matrix.
!
     if ( lnoti ) then
        do 10 j = 1, n - 1
           lm = min( kl, n-j )
           l = ipiv( j )
           if ( l/= j )call sswap( nrhs, b( l, 1 ), ldb, b( j, 1 ), ldb )
           call sger( lm, nrhs, -one, ab( kd+1, j ), 1, b( j, 1 ), &
                          ldb, b( j+1, 1 ), ldb )
   10       continue
     end if
!
     do 20 i = 1, nrhs
!
!           Solve U*X = B, overwriting B with X.
!
        call stbsv( 'upper', 'no transpose', 'non-unit', n, kl+ku, &
                      ab, ldab, b( 1, i ), 1 )
   20    continue
!
  else
!
!        Solve A'*X = B.
!
     do 30 i = 1, nrhs
!
!           Solve U'*X = B, overwriting B with X.
!
        call stbsv( 'upper', 'transpose', 'non-unit', n, kl+ku, ab, &
                      ldab, b( 1, i ), 1 )
   30    continue
!
!        Solve L'*X = B, overwriting B with X.
!
     if ( lnoti ) then
        do 40 j = n - 1, 1, -1
           lm = min( kl, n-j )
           call sgemv( 'transpose', lm, nrhs, -one, b( j+1, 1 ), &
                         ldb, ab( kd+1, j ), 1, one, b( j, 1 ), ldb )
           l = ipiv( j )
           if ( l/= j ) &
                call sswap( nrhs, b( l, 1 ), ldb, b( j, 1 ), ldb )
   40       continue
     end if
  end if
  return
!
!     End of SGBTRS
!
end
subroutine sgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
!
!*******************************************************************************
!
!! SGEMM performs one of the matrix-matrix operations
!
!     C : =  alpha*op( A )*op( B ) + beta*C,
!
!  where  op( X ) is one of
!
!     op( X ) = X   or   op( X ) = X',
!
!  alpha and beta are scalars, and A, B and C are matrices, with op( A )
!  an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
!
!  Parameters:
!
!  TRANSA - CHARACTER*1.
!           On entry, TRANSA specifies the form of op( A ) to be used in
!           the matrix multiplication as follows:
!
!              TRANSA = 'N' or 'n',  op( A ) = A.
!
!              TRANSA = 'T' or 't',  op( A ) = A'.
!
!              TRANSA = 'C' or 'c',  op( A ) = A'.
!
!           Unchanged on exit.
!
!  TRANSB - CHARACTER*1.
!           On entry, TRANSB specifies the form of op( B ) to be used in
!           the matrix multiplication as follows:
!
!              TRANSB = 'N' or 'n',  op( B ) = B.
!
!              TRANSB = 'T' or 't',  op( B ) = B'.
!
!              TRANSB = 'C' or 'c',  op( B ) = B'.
!
!           Unchanged on exit.
!
!  M      - INTEGER.
!           On entry,  M  specifies  the number  of rows  of the  matrix
!           op( A )  and of the  matrix  C.  M  must  be at least  zero.
!           Unchanged on exit.
!
!  N      - INTEGER.
!           On entry,  N  specifies the number  of columns of the matrix
!           op( B ) and the number of columns of the matrix C. N must be
!           at least zero.
!           Unchanged on exit.
!
!  K      - INTEGER.
!           On entry,  K  specifies  the number of columns of the matrix
!           op( A ) and the number of rows of the matrix op( B ). K must
!           be at least  zero.
!           Unchanged on exit.
!
!  ALPHA  - real.
!           On entry, ALPHA specifies the scalar alpha.
!           Unchanged on exit.
!
!  A      - real array of DIMENSION ( LDA, ka ), where ka is
!           k  when  TRANSA = 'N' or 'n',  and is  m  otherwise.
!           Before entry with  TRANSA = 'N' or 'n',  the leading  m by k
!           part of the array  A  must contain the matrix  A,  otherwise
!           the leading  k by m  part of the array  A  must contain  the
!           matrix A.
!           Unchanged on exit.
!
!  LDA    - INTEGER.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. When  TRANSA = 'N' or 'n' then
!           LDA must be at least  max( 1, m ), otherwise  LDA must be at
!           least  max( 1, k ).
!           Unchanged on exit.
!
!  B      - real array of DIMENSION ( LDB, kb ), where kb is
!           n  when  TRANSB = 'N' or 'n',  and is  k  otherwise.
!           Before entry with  TRANSB = 'N' or 'n',  the leading  k by n
!           part of the array  B  must contain the matrix  B,  otherwise
!           the leading  n by k  part of the array  B  must contain  the
!           matrix B.
!           Unchanged on exit.
!
!  LDB    - INTEGER.
!           On entry, LDB specifies the first dimension of B as declared
!           in the calling (sub) program. When  TRANSB = 'N' or 'n' then
!           LDB must be at least  max( 1, k ), otherwise  LDB must be at
!           least  max( 1, n ).
!           Unchanged on exit.
!
!  BETA   - real.
!           On entry,  BETA  specifies the scalar  beta.  When  BETA  is
!           supplied as zero then C need not be set on input.
!           Unchanged on exit.
!
!  C      - real array of DIMENSION ( LDC, n ).
!           Before entry, the leading  m by n  part of the array  C must
!           contain the matrix  C,  except when  beta  is zero, in which
!           case C need not be set on entry.
!           On exit, the array  C  is overwritten by the  m by n  matrix
!           ( alpha*op( A )*op( B ) + beta*C ).
!
!  LDC    - INTEGER.
!           On entry, LDC specifies the first dimension of C as declared
!           in  the  calling  (sub)  program.   LDC  must  be  at  least
!           max( 1, m ).
!           Unchanged on exit.
!
!
!  Level 3 Blas routine.
!
!  -- Written on 8-February-1989.
!     Jack Dongarra, Argonne National Laboratory.
!     Iain Duff, AERE Harwell.
!     Jeremy Du Croz, Numerical Algorithms Group Ltd.
!     Sven Hammarling, Numerical Algorithms Group Ltd.
!
!
  character        transa, transb
  integer            m, n, k, lda, ldb, ldc
  real ( kind = 8 )   alpha, beta

  real ( kind = 8 )   a( lda, * ), b( ldb, * ), c( ldc, * )
!     .. External Functions ..
  logical            lsame
  external           lsame
!     .. External Subroutines ..
  external           xerbla
!     .. Intrinsic Functions ..
  intrinsic          max
!     .. Local Scalars ..
  logical            nota, notb
  integer            i, info, j, l, nrowa, nrowb
  real ( kind = 8 )   temp
!     .. Parameters ..
  real ( kind = 8 ), parameter :: one = 1.0D+00
  real ( kind = 8 ), parameter :: zero = 0.0D+00
!     ..
!     .. Executable Statements ..
!
!     Set  NOTA  and  NOTB  as  true if  A  and  B  respectively are not
!     transposed and set  NROWA, NCOLA and  NROWB  as the number of rows
!     and  columns of  A  and the  number of  rows  of  B  respectively.
!
  nota  = lsame( transa, 'n' )
  notb  = lsame( transb, 'n' )
  if ( nota  ) then
     nrowa = m
  else
     nrowa = k
  end if
  if ( notb  ) then
     nrowb = k
  else
     nrowb = n
  end if
!
!     Test the input parameters.
!
  info = 0
  if (      ( .not.nota                 ).and. &
             ( .not.lsame( transa, 'c' ) ).and. &
             ( .not.lsame( transa, 't' ) )       ) then
     info = 1
  else if ( ( .not.notb                 ).and. &
             ( .not.lsame( transb, 'c' ) ).and. &
              ( .not.lsame( transb, 't' ) )       ) then
     info = 2
  else if ( m  <0                ) then
     info = 3
  else if ( n  <0                ) then
     info = 4
  else if ( k  <0                ) then
     info = 5
  else if ( lda<max( 1, nrowa )  ) then
     info = 8
  else if ( ldb<max( 1, nrowb )  ) then
     info = 10
  else if ( ldc<max( 1, m     )  ) then
     info = 13
  end if
  if ( info/= 0  ) then
     call xerbla( 'sgemm ', info )
     return
  end if
!
!     Quick return if possible.
!
  if ( ( m == 0 ).or.( n==0 ).or.( ( ( alpha==zero ).or.( k==0 ) ).and. &
    ( beta ==one ) ) ) return
!
!     And if  alpha ==zero.
!
  if ( alpha ==zero  ) then
     if ( beta ==zero  ) then
        do 20, j = 1, n
           do 10, i = 1, m
              c( i, j ) = zero
   10          continue
   20       continue
     else
        do 40, j = 1, n
           do 30, i = 1, m
              c( i, j ) = beta*c( i, j )
   30          continue
   40       continue
     end if
     return
  end if
!
!     Start the operations.
!
  if ( notb  ) then
     if ( nota  ) then
!
!           Form  C : =  alpha*A*B + beta*C.
!
        do 90, j = 1, n
           if ( beta ==zero  ) then
              do 50, i = 1, m
                 c( i, j ) = zero
   50             continue
           else if ( beta/= one  ) then
              do 60, i = 1, m
                 c( i, j ) = beta*c( i, j )
   60             continue
           end if
           do 80, l = 1, k
              if ( b( l, j )/= zero  ) then
                 temp = alpha*b( l, j )
                 do 70, i = 1, m
                    c( i, j ) = c( i, j ) + temp*a( i, l )
   70                continue
              end if
   80          continue
   90       continue
     else
!
!           Form  C : =  alpha*A'*B + beta*C
!
        do 120, j = 1, n
           do 110, i = 1, m
              temp = zero
              do 100, l = 1, k
                 temp = temp + a( l, i )*b( l, j )
  100             continue
              if ( beta ==zero  ) then
                 c( i, j ) = alpha*temp
              else
                 c( i, j ) = alpha*temp + beta*c( i, j )
              end if
  110          continue
  120       continue
     end if
  else
     if ( nota  ) then
!
!           Form  C : =  alpha*A*B' + beta*C
!
        do 170, j = 1, n
           if ( beta ==zero  ) then
              do 130, i = 1, m
                 c( i, j ) = zero
  130             continue
           else if ( beta/= one  ) then
              do 140, i = 1, m
                 c( i, j ) = beta*c( i, j )
  140             continue
           end if
           do 160, l = 1, k
              if ( b( j, l )/= zero  ) then
                 temp = alpha*b( j, l )
                 do 150, i = 1, m
                    c( i, j ) = c( i, j ) + temp*a( i, l )
  150                continue
              end if
  160          continue
  170       continue
     else
!
!           Form  C : =  alpha*A'*B' + beta*C
!
        do 200, j = 1, n
           do 190, i = 1, m
              temp = zero
              do 180, l = 1, k
                 temp = temp + a( l, i )*b( j, l )
  180             continue
              if ( beta ==zero  ) then
                 c( i, j ) = alpha*temp
              else
                 c( i, j ) = alpha*temp + beta*c( i, j )
              end if
  190          continue
  200       continue
     end if
  end if
!
  return
!
!     End of sgemm .
!
end
subroutine sgemv(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
!
!*******************************************************************************
!
!! SGEMV performs one of the matrix-vector operations
!
!     y : =  alpha*A*x + beta*y,   or   y := alpha*A'*x + beta*y,
!
!  where alpha and beta are scalars, x and y are vectors and A is an
!  m by n matrix.
!
!  Parameters:
!
!  TRANS  - CHARACTER*1.
!           On entry, TRANS specifies the operation to be performed as
!           follows:
!
!              TRANS = 'N' or 'n'   y := alpha*A*x + beta*y.
!
!              TRANS = 'T' or 't'   y := alpha*A'*x + beta*y.
!
!              TRANS = 'C' or 'c'   y := alpha*A'*x + beta*y.
!
!           Unchanged on exit.
!
!  M      - INTEGER.
!           On entry, M specifies the number of rows of the matrix A.
!           M must be at least zero.
!           Unchanged on exit.
!
!  N      - INTEGER.
!           On entry, N specifies the number of columns of the matrix A.
!           N must be at least zero.
!           Unchanged on exit.
!
!  ALPHA  - real.
!           On entry, ALPHA specifies the scalar alpha.
!           Unchanged on exit.
!
!  A      - real array of DIMENSION ( LDA, n ).
!           Before entry, the leading m by n part of the array A must
!           contain the matrix of coefficients.
!           Unchanged on exit.
!
!  LDA    - INTEGER.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. LDA must be at least
!           max( 1, m ).
!           Unchanged on exit.
!
!  X      - real array of DIMENSION at least
!           ( 1 + ( n - 1 )*abs( INCX ) ) when TRANS = 'N' or 'n'
!           and at least
!           ( 1 + ( m - 1 )*abs( INCX ) ) otherwise.
!           Before entry, the incremented array X must contain the
!           vector x.
!           Unchanged on exit.
!
!  INCX   - INTEGER.
!           On entry, INCX specifies the increment for the elements of
!           X. INCX must not be zero.
!           Unchanged on exit.
!
!  BETA   - real.
!           On entry, BETA specifies the scalar beta. When BETA is
!           supplied as zero then Y need not be set on input.
!           Unchanged on exit.
!
!  Y      - real array of DIMENSION at least
!           ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = 'N' or 'n'
!           and at least
!           ( 1 + ( n - 1 )*abs( INCY ) ) otherwise.
!           Before entry with BETA non-zero, the incremented array Y
!           must contain the vector y. On exit, Y is overwritten by the
!           updated vector y.
!
!  INCY   - INTEGER.
!           On entry, INCY specifies the increment for the elements of
!           Y. INCY must not be zero.
!           Unchanged on exit.
!
!
!  Level 2 Blas routine.
!
!  -- Written on 22-October-1986.
!     Jack Dongarra, Argonne National Lab.
!     Jeremy Du Croz, Nag Central Office.
!     Sven Hammarling, Nag Central Office.
!     Richard Hanson, Sandia National Labs.
!
!     .. Scalar Arguments ..
  real ( kind = 8 )   alpha, beta
  integer            incx, incy, lda, m, n
  character        trans
!     .. Array Arguments ..
  real ( kind = 8 )   a( lda, * ), x( * ), y( * )
!     ..
!
!  Purpose
!   =======
!
!     .. Parameters ..
  real ( kind = 8 ), parameter :: one = 1.0D+00
  real ( kind = 8 ), parameter :: zero = 0.0D+00

!     .. Local Scalars ..
  real ( kind = 8 )   temp
  integer            i, info, ix, iy, j, jx, jy, kx, ky, lenx, leny
!     .. External Functions ..
  logical            lsame
  external           lsame
!     .. External Subroutines ..
  external           xerbla
!     .. Intrinsic Functions ..
  intrinsic          max
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
  info = 0
  if     ( .not.lsame( trans, 'n' ).and. &
             .not.lsame( trans, 't' ).and. &
             .not.lsame( trans, 'c' )       ) then
     info = 1
  else if ( m<0  ) then
     info = 2
  else if ( n<0  ) then
     info = 3
  else if ( lda<max( 1, m )  ) then
     info = 6
  else if ( incx == 0  ) then
     info = 8
  else if ( incy == 0  ) then
     info = 11
  end if
  if ( info/= 0  ) then
     call xerbla( 'sgemv ', info )
     return
  end if
!
!     Quick return if possible.
!
  if ( ( m == 0 ).or.( n==0 ).or.( ( alpha==zero ).and.( beta==one ) ) )return
!
!     Set  LENX  and  LENY, the lengths of the vectors x and y, and set
!     up the start points in  X  and  Y.
!
  if ( lsame( trans, 'n' )  ) then
     lenx = n
     leny = m
  else
     lenx = m
     leny = n
  end if
  if ( incx>0  ) then
     kx = 1
  else
     kx = 1 - ( lenx - 1 )*incx
  end if
  if ( incy>0  ) then
     ky = 1
  else
     ky = 1 - ( leny - 1 )*incy
  end if
!
!     Start the operations. In this version the elements of A are
!     accessed sequentially with one pass through A.
!
!     First form  y : =  beta*y.
!
  if ( beta/= one  ) then
     if ( incy == 1  ) then
        if ( beta ==zero  ) then
           do 10, i = 1, leny
              y( i ) = zero
   10          continue
        else
           do 20, i = 1, leny
              y( i ) = beta*y( i )
   20          continue
        end if
     else
        iy = ky
        if ( beta ==zero  ) then
           do 30, i = 1, leny
              y( iy ) = zero
              iy      = iy   + incy
   30          continue
        else
           do 40, i = 1, leny
              y( iy ) = beta*y( iy )
              iy      = iy           + incy
   40          continue
        end if
     end if
  end if
  if ( alpha ==zero )return

  if ( lsame( trans, 'n' )  ) then
!
!        Form  y : =  alpha*A*x + y.
!
     jx = kx
     if ( incy == 1  ) then
        do 60, j = 1, n
           if ( x( jx )/= zero  ) then
              temp = alpha*x( jx )
              do 50, i = 1, m
                 y( i ) = y( i ) + temp*a( i, j )
   50             continue
           end if
           jx = jx + incx
   60       continue
     else
        do 80, j = 1, n
           if ( x( jx )/= zero  ) then
              temp = alpha*x( jx )
              iy   = ky
              do 70, i = 1, m
                 y( iy ) = y( iy ) + temp*a( i, j )
                 iy      = iy      + incy
   70             continue
           end if
           jx = jx + incx
   80       continue
     end if
  else
!
!        Form  y : =  alpha*A'*x + y.
!
     jy = ky
     if ( incx == 1  ) then
        do 100, j = 1, n
           temp = zero
           do 90, i = 1, m
              temp = temp + a( i, j )*x( i )
   90          continue
           y( jy ) = y( jy ) + alpha*temp
           jy      = jy      + incy
  100       continue
     else
        do 120, j = 1, n
           temp = zero
           ix   = kx
           do 110, i = 1, m
              temp = temp + a( i, j )*x( ix )
              ix   = ix   + incx
  110          continue
           y( jy ) = y( jy ) + alpha*temp
           jy      = jy      + incy
  120       continue
     end if
  end if
!
  return
!
!     End of DGEMV .
!
end
subroutine sger( m, n, alpha, x, incx, y, incy, a, lda )
!
!*******************************************************************************
!
!! SGER performs the rank 1 operation
!
!     A : =  alpha*x*y' + A,
!
!  where alpha is a scalar, x is an m element vector, y is an n element
!  vector and A is an m by n matrix.
!
!  Parameters:
!
!  M      - INTEGER.
!           On entry, M specifies the number of rows of the matrix A.
!           M must be at least zero.
!           Unchanged on exit.
!
!  N      - INTEGER.
!           On entry, N specifies the number of columns of the matrix A.
!           N must be at least zero.
!           Unchanged on exit.
!
!  ALPHA  - real.
!           On entry, ALPHA specifies the scalar alpha.
!           Unchanged on exit.
!
!  X      - real array of dimension at least
!           ( 1 + ( m - 1 )*abs( INCX ) ).
!           Before entry, the incremented array X must contain the m
!           element vector x.
!           Unchanged on exit.
!
!  INCX   - INTEGER.
!           On entry, INCX specifies the increment for the elements of
!           X. INCX must not be zero.
!           Unchanged on exit.
!
!  Y      - real array of dimension at least
!           ( 1 + ( n - 1 )*abs( INCY ) ).
!           Before entry, the incremented array Y must contain the n
!           element vector y.
!           Unchanged on exit.
!
!  INCY   - INTEGER.
!           On entry, INCY specifies the increment for the elements of
!           Y. INCY must not be zero.
!           Unchanged on exit.
!
!  A      - real array of DIMENSION ( LDA, n ).
!           Before entry, the leading m by n part of the array A must
!           contain the matrix of coefficients. On exit, A is
!           overwritten by the updated matrix.
!
!  LDA    - INTEGER.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. LDA must be at least
!           max( 1, m ).
!           Unchanged on exit.
!
!
!  Level 2 Blas routine.
!
!  -- Written on 22-October-1986.
!     Jack Dongarra, Argonne National Lab.
!     Jeremy Du Croz, Nag Central Office.
!     Sven Hammarling, Nag Central Office.
!     Richard Hanson, Sandia National Labs.
!
  real ( kind = 8 )   alpha
  integer            incx, incy, lda, m, n
  real ( kind = 8 )   a( lda, * ), x( * ), y( * )
!     .. Parameters ..
  real ( kind = 8 ), parameter :: zero = 0.0D+00

!     .. Local Scalars ..
  real ( kind = 8 )   temp
  integer            i, info, ix, j, jy, kx
!     .. External Subroutines ..
  external           xerbla
!     .. Intrinsic Functions ..
  intrinsic          max
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
  info = 0
  if     ( m<0  ) then
     info = 1
  else if ( n<0  ) then
     info = 2
  else if ( incx == 0  ) then
     info = 5
  else if ( incy == 0  ) then
     info = 7
  else if ( lda<max( 1, m )  ) then
     info = 9
  end if
  if ( info/= 0  ) then
     call xerbla( 'sger  ', info )
     return
  end if
!
!     Quick return if possible.
!
  if ( ( m == 0 ).or.( n==0 ).or.( alpha==zero ) ) return
!
!     Start the operations. In this version the elements of A are
!     accessed sequentially with one pass through A.
!
  if ( incy>0  ) then
     jy = 1
  else
     jy = 1 - ( n - 1 )*incy
  end if
  if ( incx == 1  ) then
     do 20, j = 1, n
        if ( y( jy )/= zero  ) then
           temp = alpha*y( jy )
           do 10, i = 1, m
              a( i, j ) = a( i, j ) + x( i )*temp
   10          continue
        end if
        jy = jy + incy
   20    continue
  else
     if ( incx>0  ) then
        kx = 1
     else
        kx = 1 - ( m - 1 )*incx
     end if
     do 40, j = 1, n
        if ( y( jy )/= zero  ) then
           temp = alpha*y( jy )
           ix   = kx
           do 30, i = 1, m
              a( i, j ) = a( i, j ) + x( ix )*temp
              ix        = ix        + incx
   30          continue
        end if
        jy = jy + incy
   40    continue
  end if
!
  return
!
!     End of sger  .
!
end
subroutine sgetf2(m,n,a,lda,ipiv,info)
!
!*******************************************************************************
!
!! SGETF2 computes an LU factorization of a general m-by-n matrix A
!  using partial pivoting with row interchanges.
!
!  The factorization has the form
!     A = P * L * U
!  where P is a permutation matrix, L is lower triangular with unit
!  diagonal elements (lower trapezoidal if m > n), and U is upper
!  triangular (upper trapezoidal if m < n).
!
!  This is the right-looking Level 2 BLAS version of the algorithm.
!
!  Parameters:
!
!  M       (input) INTEGER
!          The number of rows of the matrix A.  M >=  0.
!
!  N       (input) INTEGER
!          The number of columns of the matrix A.  N >=  0.
!
!  A       (input/output) real array, dimension (LDA,N)
!          On entry, the m by n matrix to be factored.
!          On exit, the factors L and U from the factorization
!          A = P*L*U; the unit diagonal elements of L are not stored.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >=  max(1,M).
!
!  IPIV    (output) INTEGER array, dimension (min(M,N))
!          The pivot indices; for 1 <=  i <= min(M,N), row i of the
!          matrix was interchanged with row IPIV(i).
!
!  INFO    (output) INTEGER
!          = 0: successful exit
!          < 0: if INFO = -k, the k-th argument had an illegal value
!          > 0: if INFO = k, U(k,k) is exactly zero. The factorization
!               has been completed, but the factor U is exactly
!               singular, and division by zero will occur if it is used
!               to solve a system of equations.
!
  INTEGER            INFO, LDA, M, N
!     ..
!     .. Array Arguments ..
  INTEGER            IPIV( * )
  real ( kind = 8 )   A( LDA, * )

!     .. Parameters ..
  real ( kind = 8 )   ONE, ZERO
  PARAMETER          ( ONE = 1.0E+0, ZERO = 0.0E+0 )
!     ..
!     .. Local Scalars ..
  INTEGER            J, JP
!     ..
!     .. External Functions ..
  INTEGER            ISAMAX
  EXTERNAL           ISAMAX
!     ..
!     .. External Subroutines ..
  EXTERNAL           sger, sscal, sswap, XERBLA
!     ..
!     .. Intrinsic Functions ..
  INTRINSIC          MAX, MIN
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
  INFO = 0
  IF( M < 0 ) THEN
     INFO = -1
  ELSE IF( N < 0 ) THEN
     INFO = -2
  ELSE IF( LDA < MAX( 1, M ) ) THEN
     INFO = -4
  END IF
  IF( INFO /= 0 ) THEN
     CALL XERBLA( 'sgetf2', -INFO )
     RETURN
  END IF
!
!     Quick return if possible
!
  IF( M.EQ.0 .OR. N.EQ.0 ) RETURN
!
  DO 10 J = 1, MIN( M, N )
!
!        Find pivot and test for singularity.
!
     JP = J - 1 + ISAMAX( M-J+1, A( J, J ), 1 )
     IPIV( J ) = JP
     IF( A( JP, J ) /= ZERO ) THEN
!
!           Apply the interchange to columns 1:N.
!
        IF( JP /= J ) call sswap( N, A( J, 1 ), LDA, A( JP, 1 ), LDA )
!
!           Compute elements J+1:M of J-th column.
!
        IF( J < M ) call sscal( M-J, ONE / A( J, J ), A( J+1, J ), 1 )
!
     ELSE IF( INFO.EQ.0 ) THEN
!
        INFO = J
     END IF
!
     IF( J < MIN( M, N ) ) THEN
!
!           Update trailing submatrix.
!
        call sger( M-J, N-J, -ONE, A( J+1, J ), 1, A( J, J+1 ), LDA, &
                     A( J+1, J+1 ), LDA )
     END IF
   10 CONTINUE
  RETURN
!
!     End of sgetf2
!
  END
subroutine sgetrf( M, N, A, LDA, IPIV, INFO )
!
!*******************************************************************************
!
!! SGETRF computes an LU factorization of a general M-by-N matrix A
!  using partial pivoting with row interchanges.
!
!  The factorization has the form
!     A = P * L * U
!  where P is a permutation matrix, L is lower triangular with unit
!  diagonal elements (lower trapezoidal if m > n), and U is upper
!  triangular (upper trapezoidal if m < n).
!
!  This is the right-looking Level 3 BLAS version of the algorithm.
!
!  Parameters:
!
!  M       (input) INTEGER
!          The number of rows of the matrix A.  M >=  0.
!
!  N       (input) INTEGER
!          The number of columns of the matrix A.  N >=  0.
!
!  A       (input/output) real array, dimension (LDA,N)
!          On entry, the M-by-N matrix to be factored.
!          On exit, the factors L and U from the factorization
!          A = P*L*U; the unit diagonal elements of L are not stored.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >=  max(1,M).
!
!  IPIV    (output) INTEGER array, dimension (min(M,N))
!          The pivot indices; for 1 <=  i <= min(M,N), row i of the
!          matrix was interchanged with row IPIV(i).
!
!  INFO    (output) INTEGER
!          = 0:  successful exit
!          < 0:  if INFO = -i, the i-th argument had an illegal value
!          > 0:  if INFO = i, U(i,i) is exactly zero. The factorization
!                has been completed, but the factor U is exactly
!                singular, and division by zero will occur if it is used
!                to solve a system of equations.
!
  INTEGER            INFO, LDA, M, N
  INTEGER            IPIV( * )
  real ( kind = 8 )   A( LDA, * )
  real ( kind = 8 )   ONE
  PARAMETER          ( ONE = 1.0E+0 )
!     ..
!     .. Local Scalars ..
  INTEGER            I, IINFO, J, JB, NB
!     ..
!     .. External Subroutines ..
  EXTERNAL           sgemm, sgetf2, slaswp, strsm, XERBLA
!     ..
!     .. External Functions ..
  INTEGER            ILAENV
  EXTERNAL           ILAENV
!     ..
!     .. Intrinsic Functions ..
  INTRINSIC          MAX, MIN
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
  INFO = 0
  IF( M < 0 ) THEN
     INFO = -1
  ELSE IF( N < 0 ) THEN
     INFO = -2
  ELSE IF( LDA < MAX( 1, M ) ) THEN
     INFO = -4
  END IF
  IF( INFO /= 0 ) THEN
     CALL XERBLA( 'SGETRF', -INFO )
     RETURN
  END IF
!
!     Quick return if possible
!
  IF( M.EQ.0 .OR. N.EQ.0 ) RETURN
!
!     Determine the block size for this environment.
!
  NB = ILAENV( 1, 'SGETRF', ' ', M, N, -1, -1 )
  IF( NB.LE.1 .OR. NB.GE.MIN( M, N ) ) THEN
!
!        Use unblocked code.
!
     call sgetf2( M, N, A, LDA, IPIV, INFO )
  ELSE
!
!        Use blocked code.
!
     DO 20 J = 1, MIN( M, N ), NB
        JB = MIN( MIN( M, N )-J+1, NB )
!
!           Factor diagonal and subdiagonal blocks and test for exact
!           singularity.
!
        call sgetf2( M-J+1, JB, A( J, J ), LDA, IPIV( J ), IINFO )
!
!           Adjust INFO and the pivot indices.
!
        IF( INFO.EQ.0 .AND. IINFO.GT.0 ) INFO = IINFO + J - 1
        DO 10 I = J, MIN( M, J+JB-1 )
           IPIV( I ) = J - 1 + IPIV( I )
   10       CONTINUE
!
!           Apply interchanges to columns 1:J-1.
!
        call slaswp( J-1, A, LDA, J, J+JB-1, IPIV, 1 )
!
        IF( J+JB.LE.N ) THEN
!
!              Apply interchanges to columns J+JB:N.
!
           call slaswp( N-J-JB+1, A( 1, J+JB ), LDA, J, J+JB-1, IPIV, 1 )
!
!              Compute block row of U.
!
           call strsm( 'Left', 'Lower', 'No transpose', 'Unit', JB, &
                         N-J-JB+1, ONE, A( J, J ), LDA, A( J, J+JB ), &
                          LDA )
           IF( J+JB.LE.M ) THEN
!
!                 Update trailing submatrix.
!
              call sgemm( 'No transpose', 'No transpose', M-J-JB+1, &
                            N-J-JB+1, JB, -ONE, A( J+JB, J ), LDA, &
                             A( J, J+JB ), LDA, ONE, A( J+JB, J+JB ), &
                             LDA )
           END IF
        END IF
   20    CONTINUE
  END IF
  RETURN
!
!     End of SGETRF
!
  END
subroutine sgetrs( TRANS, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
!
!*******************************************************************************
!
!! SGETRS solves a system of linear equations
!     A * X = B  or  A' * X = B
!  with a general N-by-N matrix A using the LU factorization computed
!  by SGETRF.
!
!  Parameters:
!
!  TRANS   (input) CHARACTER*1
!          Specifies the form of the system of equations:
!          = 'N':  A * X = B  (No transpose)
!          = 'T':  A'* X = B  (Transpose)
!          = 'C':  A'* X = B  (Conjugate transpose = Transpose)
!
!  N       (input) INTEGER
!          The order of the matrix A.  N >=  0.
!
!  NRHS    (input) INTEGER
!          The number of right hand sides, i.e., the number of columns
!          of the matrix B.  NRHS >=  0.
!
!  A       (input) real array, dimension (LDA,N)
!          The factors L and U from the factorization A = P*L*U
!          as computed by SGETRF.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >=  max(1,N).
!
!  IPIV    (input) INTEGER array, dimension (N)
!          The pivot indices from SGETRF; for 1<= i<=N, row i of the
!          matrix was interchanged with row IPIV(i).
!
!  B       (input/output) real array, dimension (LDB,NRHS)
!          On entry, the right hand side matrix B.
!          On exit, the solution matrix X.
!
!  LDB     (input) INTEGER
!          The leading dimension of the array B.  LDB >=  max(1,N).
!
!  INFO    (output) INTEGER
!          = 0:  successful exit
!          < 0:  if INFO = -i, the i-th argument had an illegal value
!
!   =====================================================================
!
  CHARACTER          TRANS
  INTEGER            INFO, LDA, LDB, N, NRHS
  INTEGER            IPIV( * )
  real ( kind = 8 )   A( LDA, * ), B( LDB, * )
  real ( kind = 8 )   ONE
  PARAMETER          ( ONE = 1.0E+0 )
!     ..
!     .. Local Scalars ..
  LOGICAL            NOTRAN
!     ..
!     .. External Functions ..
  LOGICAL            LSAME
  EXTERNAL           LSAME
!     ..
!     .. External Subroutines ..
  EXTERNAL           slaswp, strsm, XERBLA
!     ..
!     .. Intrinsic Functions ..
  INTRINSIC          MAX
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
  INFO = 0
  NOTRAN = LSAME( TRANS, 'N' )
  IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'T' ) .AND. .NOT. &
    LSAME( TRANS, 'C' ) ) THEN
     INFO = -1
  ELSE IF( N < 0 ) THEN
     INFO = -2
  ELSE IF( NRHS < 0 ) THEN
     INFO = -3
  ELSE IF( LDA < MAX( 1, N ) ) THEN
     INFO = -5
  ELSE IF( LDB < MAX( 1, N ) ) THEN
     INFO = -8
  END IF
  IF( INFO /= 0 ) THEN
     CALL XERBLA( 'SGETRS', -INFO )
     RETURN
  END IF
!
!     Quick return if possible
!
  IF( N.EQ.0 .OR. NRHS.EQ.0 ) RETURN
!
  IF( NOTRAN ) THEN
!
!        Solve A * X = B.
!
!        Apply row interchanges to the right hand sides.
!
     call slaswp( NRHS, B, LDB, 1, N, IPIV, 1 )
!
!        Solve L*X = B, overwriting B with X.
!
     call strsm( 'Left', 'Lower', 'No transpose', 'Unit', N, NRHS, &
                    ONE, A, LDA, B, LDB )
!
!        Solve U*X = B, overwriting B with X.
!
     call strsm( 'Left', 'Upper', 'No transpose', 'Non-unit', N, &
                   NRHS, ONE, A, LDA, B, LDB )
  ELSE
!
!        Solve A' * X = B.
!
!        Solve U'*X = B, overwriting B with X.
!
     call strsm( 'Left', 'Upper', 'Transpose', 'Non-unit', N, NRHS, &
                   ONE, A, LDA, B, LDB )
!
!        Solve L'*X = B, overwriting B with X.
!
     call strsm( 'Left', 'Lower', 'Transpose', 'Unit', N, NRHS, ONE, &
                   A, LDA, B, LDB )
!
!        Apply row interchanges to the solution vectors.
!
     CALL SLASWP( NRHS, B, LDB, 1, N, IPIV, -1 )
  END IF
!
  RETURN
END
subroutine slaswp(n,a,lda,k1,k2,ipiv,incx)
!
!*******************************************************************************
!
!! SLASWP performs a series of row interchanges on the matrix A.
!  One row interchange is initiated for each of rows K1 through K2 of A.
!
!  Parameters:
!
!  N       (input) INTEGER
!          The number of columns of the matrix A.
!
!  A       (input/output) real array, dimension (LDA,N)
!          On entry, the matrix of column dimension N to which the row
!          interchanges will be applied.
!          On exit, the permuted matrix.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.
!
!  K1      (input) INTEGER
!          The first element of IPIV for which a row interchange will
!          be done.
!
!  K2      (input) INTEGER
!          The last element of IPIV for which a row interchange will
!          be done.
!
!  IPIV    (input) INTEGER array, dimension (M*abs(INCX))
!          The vector of pivot indices.  Only the elements in positions
!          K1 through K2 of IPIV are accessed.
!          IPIV(K) = L implies rows K and L are to be interchanged.
!
!  INCX    (input) INTEGER
!          The increment between successive values of IPIV.  If IPIV
!          is negative, the pivots are applied in reverse order.
!
  integer            incx, k1, k2, lda, n
  integer            ipiv( * )
  real ( kind = 8 )   a( lda, * )

  integer            i, ip, ix
!     ..
!     .. External Subroutines ..
  external           sswap
!     ..
!     .. Executable Statements ..
!
!     Interchange row I with row IPIV(I) for each of rows K1 through K2.
!
  if ( incx == 0 ) return

  if ( incx>0 ) then
     ix = k1
  else
     ix = 1 + ( 1-k2 )*incx
  end if

  if ( incx == 1 ) then
     do 10 i = k1, k2
        ip = ipiv( i )
        if ( ip/= i ) call sswap( n, a( i, 1 ), lda, a( ip, 1 ), lda )
   10    continue
  else if ( incx>1 ) then
     do 20 i = k1, k2
        ip = ipiv( ix )
        if ( ip/= i ) call sswap( n, a( i, 1 ), lda, a( ip, 1 ), lda )
        ix = ix + incx
   20    continue
  else if ( incx<0 ) then
     do 30 i = k2, k1, -1
        ip = ipiv( ix )
        if ( ip/= i ) call sswap( n, a( i, 1 ), lda, a( ip, 1 ), lda )
        ix = ix + incx
   30    continue
  end if
!
  return
end
subroutine sscal(n,da,dx,incx)
!
!*******************************************************************************
!
!! SSCAL scales a vector by a constant.
!
!
!  Parameters:
!
  real ( kind = 8 ) da
  real ( kind = 8 ) dx(*)
  integer i
  integer incx
  integer ix
  integer m
  integer n
!
  if ( n<= 0)return

  if ( incx == 1)go to 20
!
  if ( incx<0 ) then
    ix = (-n+1)*incx + 1
  else
    ix = 1
  end if

  do i = 1,n
    dx(ix) = da*dx(ix)
    ix = ix + incx
  end do

  return
!
   20 m = mod(n,5)

  do i = 1,m
    dx(i) = da*dx(i)
  end do

  do i = m+1,n,5
    dx(i) = da*dx(i)
    dx(i + 1) = da*dx(i + 1)
    dx(i + 2) = da*dx(i + 2)
    dx(i + 3) = da*dx(i + 3)
    dx(i + 4) = da*dx(i + 4)
  end do

  return
end
subroutine sswap(n,dx,incx,dy,incy)
!
!*******************************************************************************
!
!! SSWAP interchanges two vectors.
!
! 
!  Parameters:
!
  real ( kind = 8 ) dtemp
  real ( kind = 8 ) dx(*)
  real ( kind = 8 ) dy(*)
  integer i
  integer incx
  integer incy
  integer ix
  integer iy
  integer m
  integer n
!
  if ( n<= 0)return

  if ( incx == 1.and.incy== 1)go to 20
!
  ix = 1
  iy = 1
  if ( incx<0)ix = (-n+1)*incx + 1
  if ( incy<0)iy = (-n+1)*incy + 1

  do i = 1,n
    dtemp = dx(ix)
    dx(ix) = dy(iy)
    dy(iy) = dtemp
    ix = ix + incx
    iy = iy + incy
  end do

  return
!
   20 m = mod(n,3)

  do i = 1,m
    dtemp = dx(i)
    dx(i) = dy(i)
    dy(i) = dtemp
  end do

  do i = m+1,n,3
    dtemp = dx(i)
    dx(i) = dy(i)
    dy(i) = dtemp
    dtemp = dx(i + 1)
    dx(i + 1) = dy(i + 1)
    dy(i + 1) = dtemp
    dtemp = dx(i + 2)
    dx(i + 2) = dy(i + 2)
    dy(i + 2) = dtemp
  end do

  return
end
subroutine stbsv(uplo,trans,diag,n,k,a,lda,x,incx)
!
!*******************************************************************************
!
!! STBSV  solves one of the systems of equations
!
!     A*x = b,   or   A'*x = b,
!
!  where b and x are n element vectors and A is an n by n unit, or
!  non-unit, upper or lower triangular band matrix, with ( k + 1 )
!  diagonals.
!
!  No test for singularity or near-singularity is included in this
!  routine. Such tests must be performed before calling this routine.
!
!  Parameters:
!
!  UPLO   - CHARACTER*1.
!           On entry, UPLO specifies whether the matrix is an upper or
!           lower triangular matrix as follows:
!
!              UPLO = 'U' or 'u'   A is an upper triangular matrix.
!
!              UPLO = 'L' or 'l'   A is a lower triangular matrix.
!
!           Unchanged on exit.
!
!  TRANS  - CHARACTER*1.
!           On entry, TRANS specifies the equations to be solved as
!           follows:
!
!              TRANS = 'N' or 'n'   A*x = b.
!
!              TRANS = 'T' or 't'   A'*x = b.
!
!              TRANS = 'C' or 'c'   A'*x = b.
!
!           Unchanged on exit.
!
!  DIAG   - CHARACTER*1.
!           On entry, DIAG specifies whether or not A is unit
!           triangular as follows:
!
!              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
!
!              DIAG = 'N' or 'n'   A is not assumed to be unit
!                                  triangular.
!
!           Unchanged on exit.
!
!  N      - INTEGER.
!           On entry, N specifies the order of the matrix A.
!           N must be at least zero.
!           Unchanged on exit.
!
!  K      - INTEGER.
!           On entry with UPLO = 'U' or 'u', K specifies the number of
!           super-diagonals of the matrix A.
!           On entry with UPLO = 'L' or 'l', K specifies the number of
!           sub-diagonals of the matrix A.
!           K must satisfy  0 <=  K.
!           Unchanged on exit.
!
!  A      - real array of DIMENSION ( LDA, n ).
!           Before entry with UPLO = 'U' or 'u', the leading ( k + 1 )
!           by n part of the array A must contain the upper triangular
!           band part of the matrix of coefficients, supplied column by
!           column, with the leading diagonal of the matrix in row
!           ( k + 1 ) of the array, the first super-diagonal starting at
!           position 2 in row k, and so on. The top left k by k triangle
!           of the array A is not referenced.
!           The following program segment will transfer an upper
!           triangular band matrix from conventional full matrix storage
!           to band storage:
!
!                 DO 20, J = 1, N
!                    M = K + 1 - J
!                    DO 10, I = MAX( 1, J - K ), J
!                       A( M + I, J ) = matrix( I, J )
!              10    CONTINUE
!              20 CONTINUE
!
!           Before entry with UPLO = 'L' or 'l', the leading ( k + 1 )
!           by n part of the array A must contain the lower triangular
!           band part of the matrix of coefficients, supplied column by
!           column, with the leading diagonal of the matrix in row 1 of
!           the array, the first sub-diagonal starting at position 1 in
!           row 2, and so on. The bottom right k by k triangle of the
!           array A is not referenced.
!           The following program segment will transfer a lower
!           triangular band matrix from conventional full matrix storage
!           to band storage:
!
!                 DO 20, J = 1, N
!                    M = 1 - J
!                    DO 10, I = J, MIN( N, J + K )
!                       A( M + I, J ) = matrix( I, J )
!              10    CONTINUE
!              20 CONTINUE
!
!           Note that when DIAG = 'U' or 'u' the elements of the array A
!           corresponding to the diagonal elements of the matrix are not
!           referenced, but are assumed to be unity.
!           Unchanged on exit.
!
!  LDA    - INTEGER.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. LDA must be at least
!           ( k + 1 ).
!           Unchanged on exit.
!
!  X      - real array of dimension at least
!           ( 1 + ( n - 1 )*abs( INCX ) ).
!           Before entry, the incremented array X must contain the n
!           element right-hand side vector b. On exit, X is overwritten
!           with the solution vector x.
!
!  INCX   - INTEGER.
!           On entry, INCX specifies the increment for the elements of
!           X. INCX must not be zero.
!           Unchanged on exit.
!
!
!  Level 2 Blas routine.
!
!  -- Written on 22-October-1986.
!     Jack Dongarra, Argonne National Lab.
!     Jeremy Du Croz, Nag Central Office.
!     Sven Hammarling, Nag Central Office.
!     Richard Hanson, Sandia National Labs.
!
!
  integer incx, k, lda, n
  character        diag, trans, uplo
  real ( kind = 8 )   a( lda, * ), x( * )

  real ( kind = 8 ), parameter :: zero = 0.0D+00

!     .. Local Scalars ..
  real ( kind = 8 )   temp
  integer            i, info, ix, j, jx, kplus1, kx, l
  logical            nounit
!     .. External Functions ..
  logical            lsame
  external           lsame
!     .. External Subroutines ..
  external           xerbla
!     .. Intrinsic Functions ..
  intrinsic          max, min
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
  info = 0
  if     ( .not.lsame( uplo , 'u' ).and. &
             .not.lsame( uplo , 'l' )       ) then
     info = 1
  else if ( .not.lsame( trans, 'n' ).and. &
             .not.lsame( trans, 't' ).and. &
              .not.lsame( trans, 'c' )       ) then
     info = 2
  else if ( .not.lsame( diag , 'u' ).and. &
              .not.lsame( diag , 'n' )       ) then
     info = 3
  else if ( n<0  ) then
     info = 4
  else if ( k<0  ) then
     info = 5
  else if ( lda<( k + 1 )  ) then
     info = 7
  else if ( incx == 0  ) then
     info = 9
  end if
  if ( info/= 0  ) then
     call xerbla( 'stbsv ', info )
     return
  end if
!
!     Quick return if possible.
!
  if ( n == 0 ) return
!
  nounit = lsame( diag, 'n' )
!
!     Set up the start point in X if the increment is not unity. This
!     will be  ( N - 1 )*INCX  too small for descending loops.
!
  if ( incx<= 0  ) then
     kx = 1 - ( n - 1 )*incx
  else if ( incx/= 1  ) then
     kx = 1
  end if
!
!     Start the operations. In this version the elements of A are
!     accessed by sequentially with one pass through A.
!
  if ( lsame( trans, 'n' )  ) then
!
!        Form  x : =  inv( A )*x.
!
     if ( lsame( uplo, 'u' )  ) then
        kplus1 = k + 1
        if ( incx == 1  ) then
           do 20, j = n, 1, -1
              if ( x( j )/= zero  ) then
                 l = kplus1 - j
                 if ( nounit ) x( j ) = x( j )/a( kplus1, j )
                 temp = x( j )
                 do 10, i = j - 1, max( 1, j - k ), -1
                    x( i ) = x( i ) - temp*a( l + i, j )
   10                continue
              end if
   20          continue
        else
           kx = kx + ( n - 1 )*incx
           jx = kx
           do 40, j = n, 1, -1
              kx = kx - incx
              if ( x( jx )/= zero  ) then
                 ix = kx
                 l  = kplus1 - j
                 if ( nounit ) &
                      x( jx ) = x( jx )/a( kplus1, j )
                 temp = x( jx )
                 do 30, i = j - 1, max( 1, j - k ), -1
                    x( ix ) = x( ix ) - temp*a( l + i, j )
                    ix      = ix      - incx
   30                continue
              end if
              jx = jx - incx
   40          continue
        end if
     else
        if ( incx == 1  ) then
           do 60, j = 1, n
              if ( x( j )/= zero  ) then
                 l = 1 - j
                 if ( nounit ) &
                      x( j ) = x( j )/a( 1, j )
                 temp = x( j )
                 do 50, i = j + 1, min( n, j + k )
                    x( i ) = x( i ) - temp*a( l + i, j )
   50                continue
              end if
   60          continue
        else
           jx = kx
           do 80, j = 1, n
              kx = kx + incx
              if ( x( jx )/= zero  ) then
                 ix = kx
                 l  = 1  - j
                 if ( nounit ) &
                      x( jx ) = x( jx )/a( 1, j )
                 temp = x( jx )
                 do 70, i = j + 1, min( n, j + k )
                    x( ix ) = x( ix ) - temp*a( l + i, j )
                    ix      = ix      + incx
   70                continue
              end if
              jx = jx + incx
   80          continue
        end if
     end if
  else
!
!        Form  x : =  inv( A')*x.
!
     if ( lsame( uplo, 'u' )  ) then
        kplus1 = k + 1
        if ( incx == 1  ) then
           do 100, j = 1, n
              temp = x( j )
              l    = kplus1 - j
              do 90, i = max( 1, j - k ), j - 1
                 temp = temp - a( l + i, j )*x( i )
   90             continue
              if ( nounit ) temp = temp/a( kplus1, j )
              x( j ) = temp
  100          continue
        else
           jx = kx
           do 120, j = 1, n
              temp = x( jx )
              ix   = kx
              l    = kplus1  - j
              do 110, i = max( 1, j - k ), j - 1
                 temp = temp - a( l + i, j )*x( ix )
                 ix   = ix   + incx
  110             continue
              if ( nounit ) temp = temp/a( kplus1, j )
              x( jx ) = temp
              jx      = jx   + incx
              if ( j>k ) kx = kx + incx
  120          continue
        end if
     else
        if ( incx == 1  ) then
           do 140, j = n, 1, -1
              temp = x( j )
              l    = 1      - j
              do 130, i = min( n, j + k ), j + 1, -1
                 temp = temp - a( l + i, j )*x( i )
  130             continue
              if ( nounit ) temp = temp/a( 1, j )
              x( j ) = temp
  140          continue
        else
           kx = kx + ( n - 1 )*incx
           jx = kx
           do 160, j = n, 1, -1
              temp = x( jx )
              ix   = kx
              l    = 1       - j
              do 150, i = min( n, j + k ), j + 1, -1
                 temp = temp - a( l + i, j )*x( ix )
                 ix   = ix   - incx
  150             continue
              if ( nounit ) temp = temp/a( 1, j )
              x( jx ) = temp
              jx      = jx   - incx
              if ( ( n - j )>= k ) kx = kx - incx
  160          continue
        end if
     end if
  end if

  return
end
subroutine strsm(side,uplo,transa,diag,m,n,alpha,a,lda,b,ldb)
!
!*******************************************************************************
!
!! STRSM  solves one of the matrix equations
!
!     op( A )*X = alpha*B,   or   X*op( A ) = alpha*B,
!
!  where alpha is a scalar, X and B are m by n matrices, A is a unit, or
!  non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
!
!     op( A ) = A   or   op( A ) = A'.
!
!  The matrix X is overwritten on B.
!
!  Parameters:
!
!  SIDE   - CHARACTER*1.
!           On entry, SIDE specifies whether op( A ) appears on the left
!           or right of X as follows:
!
!              SIDE = 'L' or 'l'   op( A )*X = alpha*B.
!
!              SIDE = 'R' or 'r'   X*op( A ) = alpha*B.
!
!           Unchanged on exit.
!
!  UPLO   - CHARACTER*1.
!           On entry, UPLO specifies whether the matrix A is an upper or
!           lower triangular matrix as follows:
!
!              UPLO = 'U' or 'u'   A is an upper triangular matrix.
!
!              UPLO = 'L' or 'l'   A is a lower triangular matrix.
!
!           Unchanged on exit.
!
!  TRANSA - CHARACTER*1.
!           On entry, TRANSA specifies the form of op( A ) to be used in
!           the matrix multiplication as follows:
!
!              TRANSA = 'N' or 'n'   op( A ) = A.
!
!              TRANSA = 'T' or 't'   op( A ) = A'.
!
!              TRANSA = 'C' or 'c'   op( A ) = A'.
!
!           Unchanged on exit.
!
!  DIAG   - CHARACTER*1.
!           On entry, DIAG specifies whether or not A is unit triangular
!           as follows:
!
!              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
!
!              DIAG = 'N' or 'n'   A is not assumed to be unit
!                                  triangular.
!
!           Unchanged on exit.
!
!  M      - INTEGER.
!           On entry, M specifies the number of rows of B. M must be at
!           least zero.
!           Unchanged on exit.
!
!  N      - INTEGER.
!           On entry, N specifies the number of columns of B.  N must be
!           at least zero.
!           Unchanged on exit.
!
!  ALPHA  - real.
!           On entry,  ALPHA specifies the scalar  alpha. When  alpha is
!           zero then  A is not referenced and  B need not be set before
!           entry.
!           Unchanged on exit.
!
!  A      - real array of DIMENSION ( LDA, k ), where k is m
!           when  SIDE = 'L' or 'l'  and is  n  when  SIDE = 'R' or 'r'.
!           Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
!           upper triangular part of the array  A must contain the upper
!           triangular matrix  and the strictly lower triangular part of
!           A is not referenced.
!           Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
!           lower triangular part of the array  A must contain the lower
!           triangular matrix  and the strictly upper triangular part of
!           A is not referenced.
!           Note that when  DIAG = 'U' or 'u',  the diagonal elements of
!           A  are not referenced either,  but are assumed to be  unity.
!           Unchanged on exit.
!
!  LDA    - INTEGER.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
!           LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
!           then LDA must be at least max( 1, n ).
!           Unchanged on exit.
!
!  B      - real array of DIMENSION ( LDB, n ).
!           Before entry,  the leading  m by n part of the array  B must
!           contain  the  right-hand  side  matrix  B,  and  on exit  is
!           overwritten by the solution matrix  X.
!
!  LDB    - INTEGER.
!           On entry, LDB specifies the first dimension of B as declared
!           in  the  calling  (sub)  program.   LDB  must  be  at  least
!           max( 1, m ).
!           Unchanged on exit.
!
!
!  Level 3 Blas routine.
!
!
!  -- Written on 8-February-1989.
!     Jack Dongarra, Argonne National Laboratory.
!     Iain Duff, AERE Harwell.
!     Jeremy Du Croz, Numerical Algorithms Group Ltd.
!     Sven Hammarling, Numerical Algorithms Group Ltd.
!
  character        side, uplo, transa, diag
  integer            m, n, lda, ldb
  real ( kind = 8 )   alpha
  real ( kind = 8 )   a( lda, * ), b( ldb, * )
!     .. External Functions ..
  logical            lsame
  external           lsame
!     .. External Subroutines ..
  external           xerbla
!     .. Intrinsic Functions ..
  intrinsic          max
!     .. Local Scalars ..
  logical            lside, nounit, upper
  integer            i, info, j, k, nrowa
  real ( kind = 8 )   temp
!     .. Parameters ..
  real ( kind = 8 ), parameter :: one = 1.0D+00
  real ( kind = 8 ), parameter :: zero = 0.0D+00
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
  lside  = lsame( side  , 'l' )
  if ( lside  ) then
     nrowa = m
  else
     nrowa = n
  end if
  nounit = lsame( diag  , 'n' )
  upper  = lsame( uplo  , 'u' )
!
  info   = 0
  if (      ( .not.lside                ).and. &
             ( .not.lsame( side  , 'r' ) )       ) then
     info = 1
  else if ( ( .not.upper                ).and. &
             ( .not.lsame( uplo  , 'l' ) )       ) then
     info = 2
  else if ( ( .not.lsame( transa, 'n' ) ).and. &
             ( .not.lsame( transa, 't' ) ).and. &
             ( .not.lsame( transa, 'c' ) )       ) then
     info = 3
  else if ( ( .not.lsame( diag  , 'u' ) ).and. &
             ( .not.lsame( diag  , 'n' ) )       ) then
     info = 4
  else if ( m  <0                ) then
     info = 5
  else if ( n  <0                ) then
     info = 6
  else if ( lda<max( 1, nrowa )  ) then
     info = 9
  else if ( ldb<max( 1, m     )  ) then
     info = 11
  end if
  if ( info/= 0  ) then
     call xerbla( 'strsm ', info )
     return
  end if
!
!     Quick return if possible.
!
  if ( n == 0 )return
!
!     And when  alpha ==zero.
!
  if ( alpha ==zero  ) then
     do 20, j = 1, n
        do 10, i = 1, m
           b( i, j ) = zero
   10       continue
   20    continue
     return
  end if
!
!     Start the operations.
!
  if ( lside  ) then
     if ( lsame( transa, 'n' )  ) then
!
!           Form  B : =  alpha*inv( A )*B.
!
        if ( upper  ) then
           do 60, j = 1, n
              if ( alpha/= one  ) then
                 do 30, i = 1, m
                    b( i, j ) = alpha*b( i, j )
   30                continue
              end if
              do 50, k = m, 1, -1
                 if ( b( k, j )/= zero  ) then
                    if ( nounit ) &
                         b( k, j ) = b( k, j )/a( k, k )
                    do 40, i = 1, k - 1
                       b( i, j ) = b( i, j ) - b( k, j )*a( i, k )
   40                   continue
                 end if
   50             continue
   60          continue
        else
           do 100, j = 1, n
              if ( alpha/= one  ) then
                 do 70, i = 1, m
                    b( i, j ) = alpha*b( i, j )
   70                continue
              end if
              do 90 k = 1, m
                 if ( b( k, j )/= zero  ) then
                    if ( nounit ) &
                         b( k, j ) = b( k, j )/a( k, k )
                    do 80, i = k + 1, m
                       b( i, j ) = b( i, j ) - b( k, j )*a( i, k )
   80                   continue
                 end if
   90             continue
  100          continue
        end if
     else
!
!           Form  B : =  alpha*inv( A' )*B.
!
        if ( upper  ) then
           do 130, j = 1, n
              do 120, i = 1, m
                 temp = alpha*b( i, j )
                 do 110, k = 1, i - 1
                    temp = temp - a( k, i )*b( k, j )
  110                continue
                 if ( nounit ) &
                      temp = temp/a( i, i )
                 b( i, j ) = temp
  120             continue
  130          continue
        else
           do 160, j = 1, n
              do 150, i = m, 1, -1
                 temp = alpha*b( i, j )
                 do 140, k = i + 1, m
                    temp = temp - a( k, i )*b( k, j )
  140                continue
                 if ( nounit ) &
                      temp = temp/a( i, i )
                 b( i, j ) = temp
  150             continue
  160          continue
        end if
     end if
  else
     if ( lsame( transa, 'n' )  ) then
!
!           Form  B : =  alpha*B*inv( A ).
!
        if ( upper  ) then
           do 210, j = 1, n
              if ( alpha/= one  ) then
                 do 170, i = 1, m
                    b( i, j ) = alpha*b( i, j )
  170                continue
              end if
              do 190, k = 1, j - 1
                 if ( a( k, j )/= zero  ) then
                    do 180, i = 1, m
                       b( i, j ) = b( i, j ) - a( k, j )*b( i, k )
  180                   continue
                 end if
  190             continue
              if ( nounit  ) then
                 temp = one/a( j, j )
                 do 200, i = 1, m
                    b( i, j ) = temp*b( i, j )
  200                continue
              end if
  210          continue
        else
           do 260, j = n, 1, -1
              if ( alpha/= one  ) then
                 do 220, i = 1, m
                    b( i, j ) = alpha*b( i, j )
  220                continue
              end if
              do 240, k = j + 1, n
                 if ( a( k, j )/= zero  ) then
                    do 230, i = 1, m
                       b( i, j ) = b( i, j ) - a( k, j )*b( i, k )
  230                   continue
                 end if
  240             continue
              if ( nounit  ) then
                 temp = one/a( j, j )
                 do 250, i = 1, m
                   b( i, j ) = temp*b( i, j )
  250                continue
              end if
  260          continue
        end if
     else
!
!           Form  B : =  alpha*B*inv( A' ).
!
        if ( upper  ) then
           do 310, k = n, 1, -1
              if ( nounit  ) then
                 temp = one/a( k, k )
                 do 270, i = 1, m
                    b( i, k ) = temp*b( i, k )
  270                continue
              end if
              do 290, j = 1, k - 1
                 if ( a( j, k )/= zero  ) then
                    temp = a( j, k )
                    do 280, i = 1, m
                       b( i, j ) = b( i, j ) - temp*b( i, k )
  280                   continue
                 end if
  290             continue
              if ( alpha/= one  ) then
                 do 300, i = 1, m
                    b( i, k ) = alpha*b( i, k )
  300                continue
              end if
  310          continue
        else
           do 360, k = 1, n
              if ( nounit  ) then
                 temp = one/a( k, k )
                 do 320, i = 1, m
                    b( i, k ) = temp*b( i, k )
  320                continue
              end if
              do 340, j = k + 1, n
                 if ( a( j, k )/= zero  ) then
                    temp = a( j, k )
                    do 330, i = 1, m
                       b( i, j ) = b( i, j ) - temp*b( i, k )
  330                   continue
                 end if
  340             continue
              if ( alpha/= one  ) then
                 do 350, i = 1, m
                    b( i, k ) = alpha*b( i, k )
  350                continue
              end if
  360          continue
        end if
     end if
  end if
!
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
  integer d
  character ( len = 8 ) date
  integer h
  integer m
  integer mm
  character ( len = 9 ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer n
  integer s
  character ( len = 10 )  time
  integer values(8)
  integer y
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
subroutine trans(det,detadx,detady,dxsidx,dxsidy,eta,ielem,nelem,node,np, &
  xc,xsi,yc)
!
!*******************************************************************************
!
!! TRANS calculates the biquadratic transformation which maps the
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
!    d F(X,Y)/dY  =  (d XSI/dY  d ETA/dY ) * ( d F(XSI, ETA)/d ETA )
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
!  DET    Output, real DET, the determinant of the jacobian
!         of the transformation between the reference and isoparametric
!         elements.
!
!  DETADX,
!  DETADY Output, real DETADX, DETADY, the partial
!         derivative d ETA/d X and d ETA/d Y at (XSI,ETA).
!
!  DXSIDX,
!  DXSIDY Output, real DXSIDX, DXSIDY, the partial
!         derivative d XSI/d X and d XSI/d Y at (XSI,ETA).
!
!  ETA    Input, real ETA, the ETA coordinate of the point.
!
!  IELEM  Input, integer IELEM, the number of the isoparametric
!         element we are examining.
!
!  NELEM  Input, INTEGER NELEM, the number of elements.
!
!  NODE   Input, integer NODE(6,NELEM), contains the numbers
!         of the nodes that make up each element.  Element number
!         I is associated with nodes NODE(1,I) through NODE(6,I).
!
!  NP     Input, integer NP, the number of nodes.
!
!  XC     Input, real XC(NP), the X coordinates of the
!         nodes.
!
!  XSI    Input, real XSI, the XSI coordinate of the point.
!
!  YC     Input, real YC(NP), the Y coordinates of the
!         nodes.
!
!
  integer nelem
  integer np
!
  real ( kind = 8 ) a1
  real ( kind = 8 ) a2
  real ( kind = 8 ) b1
  real ( kind = 8 ) b2
  real ( kind = 8 ) c1
  real ( kind = 8 ) c2
  real ( kind = 8 ) d1
  real ( kind = 8 ) d2
  real ( kind = 8 ) det
  real ( kind = 8 ) detadx
  real ( kind = 8 ) detady
  real ( kind = 8 ) dxdeta
  real ( kind = 8 ) dxdxsi
  real ( kind = 8 ) dxsidx
  real ( kind = 8 ) dxsidy
  real ( kind = 8 ) dydeta
  real ( kind = 8 ) dydxsi
  real ( kind = 8 ) e1
  real ( kind = 8 ) e2
  real ( kind = 8 ) eta
  real ( kind = 8 ) f1
  real ( kind = 8 ) f2
  integer i
  integer ielem
  integer node(6,nelem)
  real ( kind = 8 ) x
  real ( kind = 8 ) xn(6)
  real ( kind = 8 ) xc(np)
  real ( kind = 8 ) xsi
  real ( kind = 8 ) y
  real ( kind = 8 ) yn(6)
  real ( kind = 8 ) yc(np)
!
!  Pick off the X, Y coordinates of the nodes and store them
!  in two short lists.
!
  do i = 1,6
    xn(i) = xc(node(i,ielem))
    yn(i) = yc(node(i,ielem))
  end do
!
!  Set the coefficients in the transformation
!
!    (XSI,ETA) --> (X,Y).
!
!  The mapping has the form:
!
!    X(XSI,ETA) = A1 * XSI**2 + B1 * XSI*ETA + C1 * ETA**2
!               + D1 * XSI    + E1 * ETA     + F1
!
!    Y(XSI,ETA) = A2 * XSI**2 + B2 * XSI*ETA + C2 * ETA**2
!               + D2 * XSI    + E2 * ETA     + F2
!
  a1 =  2.0D+00*xn(1)+2.0D+00*xn(3)-4.0D+00*xn(6)
  b1 = -4.0D+00*xn(3)-4.0D+00*xn(4)+4.0D+00*xn(5)+4.0D+00*xn(6)
  c1 =  2.0D+00*xn(2)+2.0D+00*xn(3)-4.0D+00*xn(5)
  d1 = -3.0D+00*xn(1)      -xn(3)+4.0D+00*xn(6)
  e1 =       -xn(2)      +xn(3)+4.0D+00*xn(4)-4.0D+00*xn(6)
  f1 =        xn(1)

  a2 =  2.0D+00*yn(1)+2.0D+00*yn(3)-4.0D+00*yn(6)
  b2 = -4.0D+00*yn(3)-4.0D+00*yn(4)+4.0D+00*yn(5)+4.0D+00*yn(6)
  c2 =  2.0D+00*yn(2)+2.0D+00*yn(3)-4.0D+00*yn(5)
  d2 = -3.0D+00*yn(1)      -yn(3)+4.0D+00*yn(6)
  e2 =       -yn(2)      +yn(3)+4.0D+00*yn(4)-4.0D+00*yn(6)
  f2 =        yn(1)
!
!  Compute the partial derivatives at the point (XSI,ETA).
!  This is the jacobian matrix
!
!    J: (XSI,ETA) --> (X,Y).
!
  dxdxsi =  2.0D+00*a1*xsi +       b1*eta + d1
  dxdeta =        b1*xsi + 2.0D+00*c1*eta + e1

  dydxsi =  2.0D+00*a2*xsi +       b2*eta + d2
  dydeta =        b2*xsi + 2.0D+00*c2*eta + e2
!
!  Compute the determinant of the jacobian matrix:
!
!    J: (XSI,ETA) --> (X,Y)
!
  det = dxdxsi*dydeta-dxdeta*dydxsi
!
!  Watch out for a zero determinant.
!
  if ( det == 0.0D+00 ) then
    write ( *, * ) ' '
    write ( *, * ) 'Trans - Fatal error!'
    write ( *, * ) '  The jacobian J: (XSI,ETA) --> (X,Y) is singular!'
    write ( *, * ) '  This occurred for element number ',ielem
    write ( *, * ) '  Local coordinates XSI,ETA = ',xsi,eta
    x = a1*xsi**2+b1*xsi*eta+c1*eta**2+d1*xsi+e1*eta+f1
    y = a2*xsi**2+b2*xsi*eta+c2*eta**2+d2*xsi+e2*eta+f2
    write ( *, * ) '  Global coordinates X,Y = ',x,y
    write ( *, * ) ' '
    write ( *, * ) '  The X, Y nodes were:'
    write ( *, * ) ' '
    do i = 1,6
      write ( *, * ) xn(i),yn(i)
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
  dxsidx =  dydeta / det
  dxsidy = -dxdeta / det

  detadx = -dydxsi / det
  detady =  dxdxsi / det

  return
end
subroutine upvalq(dppdx,dppdy,dupdx,dupdy,dvpdx,dvpdy,gp,ielem,indx,iquad, &
  nelem,neqn,node,np,nquad,phi,pp,up,vp)
!
!*******************************************************************************
!
!! UPVALQ evaluates the sensitivities of the velocities and pressure,
!  and their X and Y derivatives, at a quadrature point in a given
!  element.
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
  integer nelem
  integer neqn
  integer np
  integer nquad
!
  real ( kind = 8 ) coef
  real ( kind = 8 ) dppdx
  real ( kind = 8 ) dppdy
  real ( kind = 8 ) dqdx
  real ( kind = 8 ) dqdy
  real ( kind = 8 ) dupdx
  real ( kind = 8 ) dupdy
  real ( kind = 8 ) dvpdx
  real ( kind = 8 ) dvpdy
  real ( kind = 8 ) dwdx
  real ( kind = 8 ) dwdy
  real ( kind = 8 ) gp(neqn)
  integer ielem
  integer indx(3,np)
  integer ip
  integer iq
  integer iquad
  integer iun
  integer node(6,nelem)
  real ( kind = 8 ) phi(nquad,6,10,nelem)
  real ( kind = 8 ) pp
  real ( kind = 8 ) q
  real ( kind = 8 ) up
  real ( kind = 8 ) vp
  real ( kind = 8 ) w
!
!  Start all the functions at zero.
!
  pp = 0.0D+00
  up = 0.0D+00
  vp = 0.0D+00
  dppdx = 0.0D+00
  dppdy = 0.0D+00
  dupdx = 0.0D+00
  dupdy = 0.0D+00
  dvpdx = 0.0D+00
  dvpdy = 0.0D+00
!
!  Now each of these functions is represented as the sum of
!  coefficients times basis functions.  In this particular
!  element, at this particular quadrature point, we know that
!  exactly 6 basis functions are nonzero.  So if
!  we simply look up the values of the basis functions (and
!  their X and Y derivatives), and multiply by the appropriate
!  coefficients, we can evaluate the functions.
!
!  W, DWDX and DWDY represent the value of a quadratic basis
!  function and its X and Y derivative.
!
!  Q, DQDX and DQDY represent the value of a linear basis
!  function and its X and Y derivatives.
!
  do iq = 1,6

    w = phi(iquad,iq,1,ielem)
    dwdx = phi(iquad,iq,2,ielem)
    dwdy = phi(iquad,iq,3,ielem)

    q = phi(iquad,iq,4,ielem)
    dqdx = phi(iquad,iq,5,ielem)
    dqdy = phi(iquad,iq,6,ielem)
!
!  Now that we have the basis function values, we need to look
!  up the coefficient COEF that multiplies the basis function.
!
    ip = node(iq,ielem)

    iun = indx(1,ip)
    coef = gp(iun)
    up = up+coef*w
    dupdx = dupdx+coef*dwdx
    dupdy = dupdy+coef*dwdy

    iun = indx(2,ip)
    coef = gp(iun)
    vp = vp+coef*w
    dvpdx = dvpdx+coef*dwdx
    dvpdy = dvpdy+coef*dwdy

    iun = indx(3,ip)
    if ( iun>0 ) then
      coef = gp(iun)
      pp = pp+coef*q
      dppdx = dppdx+coef*dqdx
      dppdy = dppdy+coef*dqdy
    end if

  end do

  return
end
subroutine uval(detadx,detady,dpdx,dpdy,dudx,dudy,dvdx,dvdy,dxsidx,dxsidy, &
  eta,g,ielem,indx,isotri,nelem,neqn,node,np,p,u,v,xc,xq,xsi,yc,yq)
!
!*******************************************************************************
!
!! UVAL evaluates the velocities and pressure at an arbitrary point.
!
!
!  If the element is not isoparametric, then UVAL requires the
!  physical X and Y coordinates of the point.
!
!  If the element is isoparametric, UVAL requires the XSI, ETA
!  coordinates of the point.
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
!    Input, real DETADX, DETADY, the partial derivative
!    d ETA/d X and d ETA/d Y at (XSI,ETA).
!
!    Input, real DXSIDX, DXSIDY, the partial derivative
!    d XSI/d X and d XSI/d Y at (XSI,ETA).
!
!    Input, real ETA, the ETA coordinate of the point,
!    needed only if the element is isoparametric.
!
!    Input, real G(NEQN), the computed solution vector, in which are stored
!    pressures and velocities.
!
!    Input, integer IELEM, the element in which the point lies
!    at which the quantities are desired.
!
!    Input, real XQ, the X coordinate of the point.
!
!    Input, real XSI, the XSI coordinate of the point,
!    needed only if the element is isoparametric.
!
!    Input, real YQ, the Y coordinate of the point.
!
  integer nelem
  integer neqn
  integer np
!
  real ( kind = 8 ) coef
  real ( kind = 8 ) detadx
  real ( kind = 8 ) detady
  real ( kind = 8 ) dpdx
  real ( kind = 8 ) dpdy
  real ( kind = 8 ) dqdx
  real ( kind = 8 ) dqdy
  real ( kind = 8 ) dudx
  real ( kind = 8 ) dudy
  real ( kind = 8 ) dvdx
  real ( kind = 8 ) dvdy
  real ( kind = 8 ) dwdx
  real ( kind = 8 ) dwdy
  real ( kind = 8 ) dxsidx
  real ( kind = 8 ) dxsidy
  real ( kind = 8 ) eta
  real ( kind = 8 ) g(neqn)
  integer ielem
  integer indx(3,np)
  integer ip
  integer iq
  integer isotri(nelem)
  integer iun
  integer node(6,nelem)
  real ( kind = 8 ) p
  real ( kind = 8 ) q
  real ( kind = 8 ) u
  real ( kind = 8 ) v
  real ( kind = 8 ) w
  real ( kind = 8 ) xc(np)
  real ( kind = 8 ) xq
  real ( kind = 8 ) xsi
  real ( kind = 8 ) yc(np)
  real ( kind = 8 ) yq
!
  p = 0.0D+00
  u = 0.0D+00
  v = 0.0D+00
  dpdx = 0.0D+00
  dpdy = 0.0D+00
  dudx = 0.0D+00
  dudy = 0.0D+00
  dvdx = 0.0D+00
  dvdy = 0.0D+00

  do iq = 1,6
!
!  Evaluate the basis functions W and Q, and their derivatives
!  DQDX, DQDY, DWDX, DWDY at XQ, YQ.
!
    if ( isotri(ielem) == 0.or.isotri(ielem)== 1 ) then

      call qbf(ielem,iq,w,dwdx,dwdy,nelem,node,np,xc,xq,yc,yq)

      call bsp(q,dqdx,dqdy,ielem,iq,nelem,node,np,xc,xq,yc,yq)

    else

      call refqbf(w,dwdx,dwdy,detadx,detady,dxsidx,dxsidy,eta,iq,xsi)

      call refbsp(q,dqdx,dqdy,detadx,detady,iq,dxsidx,dxsidy,eta,xsi)

    end if
!
!  Compute the coefficients at the node at XP, YP.
!
    ip = node(iq,ielem)

    iun = indx(1,ip)
    coef = g(iun)
    u = u+coef*w
    dudx = dudx+coef*dwdx
    dudy = dudy+coef*dwdy

    iun = indx(2,ip)
    coef = g(iun)
    v = v+coef*w
    dvdx = dvdx+coef*dwdx
    dvdy = dvdy+coef*dwdy

    iun = indx(3,ip)
    if ( iun>0 ) then
      coef = g(iun)
      p = p+coef*q
      dpdx = dpdx+coef*dqdx
      dpdy = dpdy+coef*dqdy
    end if

  end do

  return
end
subroutine uvalq(dpdx,dpdy,dudx,dudy,dvdx,dvdy,g,ielem,indx,iquad,nelem, &
  neqn,node,np,nquad,p,phi,u,v)
!
!*******************************************************************************
!
!! UVALQ evaluates the velocities and pressure at a quadrature point.
!
!
!  UVALQ also evaluates the X and Y derivatives of the quantities.
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
!  DPDX,
!  DPDY   Output, real DPDX, DPDY, the derivatives of the
!         pressure function with respect to X and Y.
!
!  DUDX,
!  DUDY   Output, real DUDX, DUDY, the derivatives of the
!         horizontal velocity function with respect to X and Y.
!
!  DVDX,
!  DVDY   Output, real DVDX, DVDY, the derivatives of the
!         vertical velocity function with respect to X and Y.
!
!  P      Output, real P, the value of the pressure.
!
!  U      Output, real U, the value of the horizontal
!         velocity.
!
!  V      Output, real V, the value of the vertical
!         velocity.
!
!
  integer nelem
  integer neqn
  integer np
  integer nquad
!
  real ( kind = 8 ) coef
  real ( kind = 8 ) dpdx
  real ( kind = 8 ) dpdy
  real ( kind = 8 ) dqdx
  real ( kind = 8 ) dqdy
  real ( kind = 8 ) dudx
  real ( kind = 8 ) dudy
  real ( kind = 8 ) dvdx
  real ( kind = 8 ) dvdy
  real ( kind = 8 ) dwdx
  real ( kind = 8 ) dwdy
  real ( kind = 8 ) g(neqn)
  integer ielem
  integer indx(3,np)
  integer ip
  integer iq
  integer iquad
  integer iun
  integer node(6,nelem)
  real ( kind = 8 ) p
  real ( kind = 8 ) phi(nquad,6,10,nelem)
  real ( kind = 8 ) q
  real ( kind = 8 ) u
  real ( kind = 8 ) v
  real ( kind = 8 ) w
!
!  Start all the functions at zero.
!
  p = 0.0D+00
  u = 0.0D+00
  v = 0.0D+00
  dpdx = 0.0D+00
  dpdy = 0.0D+00
  dudx = 0.0D+00
  dudy = 0.0D+00
  dvdx = 0.0D+00
  dvdy = 0.0D+00
!
!  Now each of these functions is represented as the sum of
!  coefficients times basis functions.  In this particular
!  element, at this particular quadrature point, we know that
!  exactly 6 basis functions are nonzero.  So if
!  we simply look up the values of the basis functions (and
!  their X and Y derivatives), and multiply by the appropriate
!  coefficients, we can evaluate the functions.
!
!  W, DWDX and DWDY represent the value of a quadratic basis
!  function and its X and Y derivative.
!
!  Q, DQDX and DQDY represent the value of a linear basis
!  function and its X and Y derivatives.
!
  do iq = 1,6

    w = phi(iquad,iq,1,ielem)
    dwdx = phi(iquad,iq,2,ielem)
    dwdy = phi(iquad,iq,3,ielem)

    q = phi(iquad,iq,4,ielem)
    dqdx = phi(iquad,iq,5,ielem)
    dqdy = phi(iquad,iq,6,ielem)
!
!  Now that we have the basis function values, we need to look
!  up the coefficient COEF that multiplies the basis function.
!
    ip = node(iq,ielem)

    iun = indx(1,ip)
    coef = g(iun)
    u = u+coef*w
    dudx = dudx+coef*dwdx
    dudy = dudy+coef*dwdy

    iun = indx(2,ip)
    coef = g(iun)
    v = v+coef*w
    dvdx = dvdx+coef*dwdx
    dvdy = dvdy+coef*dwdy

    iun = indx(3,ip)
    if ( iun>0 ) then
      coef = g(iun)
      p = p+coef*q
      dpdx = dpdx+coef*dqdx
      dpdy = dpdy+coef*dqdy
    end if

  end do

  return
end
subroutine whodat (a,area,eqn,etaq,g,g2,ifs,ibs,ierror,ijac,indx,ipivot, &
  isotri,iwrite,jjac,maxnew,nelem,neqn,nlband,node,np,npar,nparb,nparf, &
  nquad,nrow,nx,ny,para,phi,res,splbmp,splflo,syseqn,taubmp,tauflo,tolnew, &
  wquad,xbl,xbr,xc,xquad,xsiq,ybl,ybr,yc,yquad)
!
!*******************************************************************************
!
!! WHODAT is damaged code.
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
  integer nelem
  integer neqn
  integer np
  integer npar
  integer nparb
  integer nparf
  integer nquad
  integer nrow
  integer nx
  integer ny
!
  real ( kind = 8 ) a(nrow,neqn)
  real ( kind = 8 ) area(nquad,nelem)
  character ( len = 2 ) eqn(neqn)
  real ( kind = 8 ) etaq(nquad)
  real ( kind = 8 ) g(neqn)
  real ( kind = 8 ) g2(neqn)
  integer ifs
  integer ibs
  integer ierror
  integer ijac
  integer indx(3,np)
  integer ipivot(neqn)
  integer isotri(nelem)
  integer iwrite
  integer jjac
  logical s_eqi
  integer maxnew
  integer nlband
  integer node(6,nelem)
  real ( kind = 8 ) para(npar)
  real ( kind = 8 ) phi(nquad,6,10,nelem)
  real ( kind = 8 ) res(neqn)
  real ( kind = 8 ) splbmp(4,nparb+2,0:nparb)
  real ( kind = 8 ) splflo(4,nparf+2,0:nparf)
  character ( len = 20 ) syseqn
  real ( kind = 8 ) taubmp(nparb+2)
  real ( kind = 8 ) tauflo(nparf+2)
  real ( kind = 8 ) tolnew
  real ( kind = 8 ) wquad(nquad)
  real ( kind = 8 ) xbl
  real ( kind = 8 ) xbr
  real ( kind = 8 ) xc(np)
  real ( kind = 8 ) xquad(nquad,nelem)
  real ( kind = 8 ) xsiq(nquad)
  real ( kind = 8 ) ybl
  real ( kind = 8 ) ybr
  real ( kind = 8 ) yc(np)
  real ( kind = 8 ) yquad(nquad,nelem)
!
  external s_eqi
!
!  Set the spline coefficients for the bump.
!
  call bmpspl(ibs,npar,nparb,nparf,para,splbmp,taubmp,xbl,xbr,ybl,ybr)
!
!  Set the spline coefficients for the inflow.
!
  call flospl(ifs,npar,nparf,para,splflo,tauflo)
!
!  Set the X and Y coordinates of the nodes that form the grid.
!
  call setxy(ibs,np,nparb,nx,ny,splbmp,taubmp,xbl,xbr,xc,ybl,ybr,yc)
!
!  Set the quadrature points, which move every step if there
!  are bump parameters.
!
  if ( nquad ==3 ) then
    call setq3(area,etaq,isotri,nelem,node,np,nquad,wquad,xc,xquad,xsiq,yc,yquad)
  else if (nquad ==7 ) then
    call setq7(area,etaq,isotri,nelem,node,np,nquad,wquad,xc,xquad,xsiq,yc,yquad)
  end if
!
!  Set the value of the basis functions at all quadrature points.
!
  call setbas(area,etaq,isotri,nelem,node,np,nquad,phi,xc,xquad,xsiq,yc,yquad)
!
!  Try to solve the full nonlinear system.
!
  call newton(a,area,eqn,g,g2,ifs,ierror,ijac,indx,ipivot,iwrite,jjac,maxnew, &
    nelem,neqn,nlband,node,np,npar,nparb,nparf,nquad,nrow,para,phi,res,splflo, &
    syseqn,tauflo,tolnew,yc)

  if ( ierror/= 0.and.s_eqi(syseqn,'NavierStokes') ) then
    write ( *, * ) ' '
    write ( *, * ) 'FloSol - Warning!'
    write ( *, * ) '  Newton failed on '//syseqn
    write ( *, * ) '  The parameters at which failure occurred:'
    write ( *, * ) ' '
    call prpar(nparb,nparf,para)
    syseqn = 'Stokes'
    write ( *, * ) '  Switching to '//syseqn
!
!  Try to solve the linearized system.
!
    call newton(a,area,eqn,g,g2,ifs,ierror,ijac,indx,ipivot,iwrite,jjac,maxnew, &
      nelem,neqn,nlband,node,np,npar,nparb,nparf,nquad,nrow,para,phi,res, &
      splflo,syseqn,tauflo,tolnew,yc)

    syseqn = 'NavierStokes'

    if ( ierror/= 0 ) then
      write ( *, * ) ' '
      write ( *, * ) 'FloSol - Fatal error!'
      write ( *, * ) '  Newton failed!'
      write ( *, * ) '  The parameters at which failure occurred:'
      write ( *, * ) ' '
      call prpar(nparb,nparf,para)
      ierror = 1
      return
    end if

  end if

  return
end
subroutine writes(ibs,iunit,nelem,neqn,np,nparb,nquad,nx,ny,xbl,xbr)
!
!*******************************************************************************
!
!! WRITES writes out scalar data for use by SAMPLE2.
!
!
!  SAMPLE2 compares the solution on different grids.
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
  integer ibs
  integer iunit
  integer nelem
  integer neqn
  integer np
  integer nparb
  integer nquad
  integer nx
  integer ny
  character ( len = 30 ) string
  real ( kind = 8 ) xbl
  real ( kind = 8 ) xbr
!
  string = 'ibs:'
  write(iunit,'(a)')string
  write(iunit,*)ibs

  string = 'nelem:'
  write(iunit,'(a)')string
  write(iunit,*)nelem

  string = 'neqn:'
  write(iunit,'(a)')string
  write(iunit,*)neqn

  string = 'np:'
  write(iunit,'(a)')string
  write(iunit,*)np

  string = 'nparb:'
  write(iunit,'(a)')string
  write(iunit,*)nparb

  string = 'nquad:'
  write(iunit,'(a)')string
  write(iunit,*)nquad

  string = 'nx:'
  write(iunit,'(a)')string
  write(iunit,*)nx

  string = 'ny:'
  write(iunit,'(a)')string
  write(iunit,*)ny

  string = 'xbl:'
  write(iunit,'(a)')string
  write(iunit,*)xbl

  string = 'xbr:'
  write(iunit,'(a)')string
  write(iunit,*)xbr

  return
end
subroutine writev(area,g,indx,isotri,iunit,nelem,neqn,node,np,nparb,nquad, &
  splbmp,taubmp,xc,yc)
!
!*******************************************************************************
!
!! WRITEV writes out vector data for use by SAMPLE2.
!
!  Discussion:
!
!    SAMPLE2 compares the solution on different grids.
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
  integer nelem
  integer neqn
  integer np
  integer nparb
  integer nquad
!
  real ( kind = 8 ) area(nquad,nelem)
  real ( kind = 8 ) g(neqn)
  integer i
  integer indx(3,np)
  integer isotri(nelem)
  integer iunit
  integer j
  integer k
  integer node(6,nelem)
  real ( kind = 8 ) splbmp(4,nparb+2,0:nparb)
  character ( len = 40 ) string
  real ( kind = 8 ) taubmp(nparb+2)
  real ( kind = 8 ) xc(np)
  real ( kind = 8 ) yc(np)
!
  string = '((area(i,j),j=1,nelem),i=1,nquad):'
  write(iunit,'(a)')string
  do i = 1,nquad
    do j = 1,nelem
      write(iunit,*)area(i,j)
    end do
  end do

  string = '(g(i),i=1,neqn):'
  write(iunit,'(a)')string
  do i = 1,neqn
    write(iunit,*)g(i)
  end do

  string = '((indx(i,j),j=1,np),i=1,3):'
  write(iunit,'(a)')string
  do i = 1,3
    do j = 1,np
      write(iunit,*)indx(i,j)
    end do
  end do

  string = '(isotri(i),i=1,nelem):'
  write(iunit,'(a)')string
  do i = 1,nelem
    write(iunit,*)isotri(i)
  end do

  string = '((node(i,j),j=1,nelem),i=1,6):'
  write(iunit,'(a)')string
  do i = 1,6
    do j = 1,nelem
      write(iunit,*)node(i,j)
    end do
  end do

  string = '(((splbmp(i,j,k),k=0,nparb),j=1,nparb+2),i=1,4):'
  write(iunit,'(a)')string
  do i = 1,4
    do j = 1,nparb+2
      do k = 0,nparb
        write(iunit,*)splbmp(i,j,k)
      end do
    end do
  end do

  string = '(taubmp(i),i=1,nparb+2):'
  write(iunit,'(a)')string
  do i = 1,nparb+2
    write(iunit,*)taubmp(i)
  end do

  string = '(xc(i),i=1,np):'
  write(iunit,'(a)')string
  do i = 1,np
    write(iunit,*)xc(i)
  end do

  string = '(yc(i),i=1,np):'
  write(iunit,'(a)')string
  do i = 1,np
    write(iunit,*)yc(i)
  end do

  return
end
subroutine xerbla(srname,info)
!
!*******************************************************************************
!
!! XERBLA  is an error handler for the LAPACK routines.
!  It is called by an LAPACK routine if an input parameter has an
!  invalid value.  A message is printed and execution stops.
!
!  Installers may consider modifying the STOP statement in order to
!  call system-specific exception-handling facilities.
!
!  Parameters:
!
!  SRNAME  (input) CHARACTER*6
!          The name of the routine which called XERBLA.
!
!  INFO    (input) INTEGER
!          The position of the invalid parameter in the parameter list
!          of the calling routine.
!
  character ( len = 6 )        srname
  integer            info
!
  write( *, fmt = 9999 )srname, info
!
  stop
!
 9999 format( ' ** on entry to ', a6, ' parameter number ', i2, ' had ', &
     'an illegal value' )
end
subroutine xofxsi(eta,ielem,nelem,node,np,x,xc,xsi,y,yc)
!
!*******************************************************************************
!
!! XOFXSI is given the XSI, ETA coordinates of a point in an
!  isoparametric element and determines its X, Y coordinates.
!
!  Here is a graph of the (XSI, ETA) reference triangle we will use.
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
!  ETA    Input, real ETA, the ETA coordinate of the point.
!
!  IELEM  Input, integer IELEM, the number of the isoparametric
!         element we are examining.
!
!  NELEM  Input, integer NELEM, the number of elements.
!
!  NODE   Input, integer NODE(6,NELEM), contains the numbers
!         of the nodes that make up each element.  Element number
!         I is associated with nodes NODE(1,I) through NODE(6,I).
!
!  NP     Input, integer NP, the number of nodes.
!
!  X      Output, real X, the X coordinate of the point.
!
!  XC     Input, real XC(NP), the X coordinates of the
!         nodes.
!
!  XSI    Input, real XSI, the XSI coordinate of the point.
!
!  Y      Output, real Y, the Y coordinate of the point.
!
!  YC     Input, real YC(NP), the Y coordinates of the
!         nodes.
!
!
  integer nelem
  integer np

  real ( kind = 8 ) a1
  real ( kind = 8 ) a2
  real ( kind = 8 ) b1
  real ( kind = 8 ) b2
  real ( kind = 8 ) c1
  real ( kind = 8 ) c2
  real ( kind = 8 ) d1
  real ( kind = 8 ) d2
  real ( kind = 8 ) e1
  real ( kind = 8 ) e2
  real ( kind = 8 ) eta
  real ( kind = 8 ) f1
  real ( kind = 8 ) f2
  integer i
  integer ielem
  integer node(6,nelem)
  real ( kind = 8 ) x
  real ( kind = 8 ) xn(6)
  real ( kind = 8 ) xc(np)
  real ( kind = 8 ) xsi
  real ( kind = 8 ) y
  real ( kind = 8 ) yn(6)
  real ( kind = 8 ) yc(np)
!
!  Pick off the X, Y coordinates of the nodes and store them
!  in two short lists.
!
  do i = 1,6
    xn(i) = xc(node(i,ielem))
    yn(i) = yc(node(i,ielem))
  end do
!
!  Set the coefficients in the transformation
!
!    (XSI,ETA) --> (X,Y).
!
!  The mapping has the form:
!
!    X(ETA,XSI) = A1 * XSI**2 + B1 * XSI*ETA + C1 * ETA**2
!               + D1 * XSI    + E1 * ETA     + F1
!
!    Y(ETA,XSI) = A2 * XSI**2 + B2 * XSI*ETA + C2 * ETA**2
!               + D2 * XSI    + E2 * ETA     + F2
!
  a1 =  2.0D+00*xn(1)+2.0D+00*xn(3)-4.0D+00*xn(6)
  b1 = -4.0D+00*xn(3)-4.0D+00*xn(4)+4.0D+00*xn(5)+4.0D+00*xn(6)
  c1 =  2.0D+00*xn(2)+2.0D+00*xn(3)-4.0D+00*xn(5)
  d1 = -3.0D+00*xn(1)      -xn(3)+4.0D+00*xn(6)
  e1 =       -xn(2)      +xn(3)+4.0D+00*xn(4)-4.0D+00*xn(6)
  f1 =        xn(1)

  a2 =  2.0D+00*yn(1)+2.0D+00*yn(3)-4.0D+00*yn(6)
  b2 = -4.0D+00*yn(3)-4.0D+00*yn(4)+4.0D+00*yn(5)+4.0D+00*yn(6)
  c2 =  2.0D+00*yn(2)+2.0D+00*yn(3)-4.0D+00*yn(5)
  d2 = -3.0D+00*yn(1)      -yn(3)+4.0D+00*yn(6)
  e2 =       -yn(2)      +yn(3)+4.0D+00*yn(4)-4.0D+00*yn(6)
  f2 =        yn(1)

  x = a1*xsi**2 + b1*xsi*eta + c1*eta**2 + d1*xsi + e1*eta + f1

  y = a2*xsi**2 + b2*xsi*eta + c2*eta**2 + d2*xsi + e2*eta + f2

  return
end
