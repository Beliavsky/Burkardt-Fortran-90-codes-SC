program main

!*****************************************************************************80
!
!! MAIN is the main program for FLOW7.
!
!  Discussion:
!
!    FLOW7 solves a 2D steady incompressible flow using finite elements.
!
!    FLOW7 is a version of FLOW6 that is being modified to include the option
!    of iterative solution of the linear system.
!
!    FLOW6 is an experimental version of FLOW5.  A new free slip boundary
!    condition is being added.
!
!    FLOW5 is a pared down and simplified version of the research code FLOW4.  
!
!    FLOW4 includes parameterization, sensitivity analysis, the use of 
!    isoparametric elements, a curved bump on the bottom of the channel region,
!    more choices for quadrature points and other features which have 
!    been removed from FLOW5.
!
!    This program was developed by John Burkardt, based on programs
!    of Janet Peterson and Max Gunzburger.
!
!
!    The program works on an underlying fluid flow problem, whose
!    behavior is determined by a particular version of the Navier Stokes
!    equations.
!
!    The fluid flow in the region is described by three state functions:
!
!      U(X,Y), the horizontal velocity,
!      V(X,Y), the vertical velocity, 
!      P(X,Y), the pressure.  
!
!    In theory, these functions may be determined once we know the partial 
!    differential equations that govern them within the region, and
!    the value of the functions or certain derivatives of them along the 
!    boundary of the region.
!
!    For our work, we assume that at every point within the flow region, 
!    the functions obey the Navier Stokes equations for stationary, 
!    incompressible, viscous flow:
!
!    - nu * ( ddU/dxdx + ddU/dydy ) + U * dU/dx + V * dU/dy + dP/dx  = 0
!
!    - nu * ( ddV/dxdx + ddV/dydy ) + U * dV/dx + V * dV/dy + dP/dy  = 0
!
!    dU/dx + dV/dy = 0
!
!    The first two equations are sometimes called the "momentum" equations,
!    and the third the "continuity" equation.  Nu is a physical parameter called 
!    the "dynamic viscosity".
!
!    We prefer the equivalent formulation (when nu is nonzero):
!
!    - ddU/dxdx - ddU/dydy + nu_inv * ( U * dU/dx + V * dU/dy + dP/dx ) = 0
!
!    - ddV/dxdx - ddV/dydy + nu_inv * ( U * dV/dx + V * dV/dy + dP/dy ) = 0
!
!    dU/dx + dV/dy = 0
!
!    where nu_inv = ( 1 / nu ).
!
!
!    To complete the specification of the problem, we specify boundary 
!    conditions for the flow functions.
!
!*****************************************************************************80
!
!  THE ROLE OF THE DYNAMIC VISCOSITY:
!
!    Nu is a physical parameter called the "dynamic viscosity".  We explicitly 
!    assume that nu is not zero.  In the momentum equations, Nu multiplies
!    the Laplacian operator, and thus, determines the relative weight of
!    the smoothing or diffusion terms, as compared to the nonlinear terms.
!
!    For a particular fluid, Nu is a physical parameter, which is usually
!    taken to be a constant.  When comparing two fluids, the fluid with
!    the higher value of Nu will behave more like syrup; velocity tends
!    to diffuse; a small moving particle will slow down, feeling a heavy drag, 
!    while neighboring particles begin to move along.  A fluid with a lower
!    value of Nu is more "slippery", making it possible for large variations
!    in velocity to occur over a short distance, with less of a tendency
!    to be damped out.
!
!    Engineers use a combination of the dynamic viscosity, a typical 
!    length, and a typical velocity, to define the Reynolds number for
!    a given flow:
!
!      Re = v * L / nu
!
!    As the Reynolds number increases, (because, perhaps, the average
!    velocity is increasing), the character of the flow will tend to
!    change from a regular laminar flow to an unsteady turbulent flow.
!    A large value of Nu delays this transition.
!
!    Correspondingly, as the Reynolds number gets larger, the mathematical
!    problem becomes more difficult to solve, because the nonlinear part
!    of the momentum equations becomes predominant.  For large enough 
!    Reynolds number there may be no solution, or multiple solutions.
!
!*****************************************************************************80
!
!  DERIVATION OF FINITE ELEMENT EQUATIONS
!
!    Except for special cases, such as the Poiseuille flow solution 
!    discussed elsewhere, there are no methods of producing the exact 
!    solution functions U, V and P for a general Navier Stokes problem.  
!    In order to get any insight into flow problems, we must replace the 
!    original problem by one that is much weaker.  It's important that the 
!    weaker problem be possible to solve, and that the solutions produced 
!    are in general close to solutions of the original problem, and that 
!    these solutions can be made even closer,if desired.
!
!    A standard method of doing this uses finite elements.
!
!    To do so, we assume that instead of being smooth but otherwise 
!    completely arbitrary functions, that U, V and P are representable 
!    as linear combinations of a finite set of basis functions.
!
!    We multiply the first two equations by an arbitrary velocity basis
!    function Wi, and the third equation by an arbitrary pressure basis
!    function Qi, and integrate over the region.  The integrand of the
!    resulting finite element equations is then transformed, using
!    integration by parts, into:
!
!    UM-Eqn(I):
!
!      ( dU/dx * dW(I)/dx + dU/dy * dW(I)/dy ) 
!      + nu_inv * ( U * dU/dx + V * dU/dy + dP/dx ) * W(I)
!
!    VM-Eqn(I):
!
!      ( dV/dx * dW(I)/dx + dV/dy * dW(I)/dy ) 
!      + nu_inv * ( U * dV/dx + V * dV/dy + dP/dy ) * W(I)
!
!    PC-Eqn(I):
!
!      ( dU/dx + dV/dy ) * Q(I)
!
!    These integrands may be rewritten using the program's variable names:
!
!      dUdx * dwidx + dUdy * dwidy 
!      + nu_inv * ( U * dUdx + V * dUdy + dPdx ) * wi
!
!      dVdx * dwidx + dVdy * dwidy 
!      + nu_inv * ( U * dVdx + V * dVdy + dPdy ) * wi
!
!      ( dUdx + dVdy ) * qi
!
!    This system of nonlinear equations is then solved by Newton's method.
!    That means that we have to differentiate each nonlinear equation
!    with respect to the unknowns, getting the Jacobian matrix, and
!    solving DF(X) * DEL(X) = -F(X).  If we abuse notation, we can
!    consider the linear system DF(X) * DEL(X).
!
!    Here, variables U, V and P in capital letters are to be solved for, 
!    but the same variable names in lowercase represent the current
!    values of those same variables.
!
!    d UM-Eqn(I) / d U-coefficient * U coefficient:
!
!      dUdx * dwidx + dUdy * dwidy 
!      + nu_inv * ( U * dudx + u * dUdx + v * dUdy ) * wi
!
!    d UM-Eqn(I) / d V coefficient * V coefficient:
!
!      nu_inv * V * dudy * wi
!
!    d UM-Eqn(I) / d P coefficient * P coefficient:
!
!      nu_inv * dPdx * wi
!
!    d VM-Eqn(I) / d U coefficient * U coefficient:
!
!      nu_inv * U * dvdx * wi
!
!    d VM-Eqn(I) / d V coefficient * V coefficient:
!
!      dVdx * dwidx + dVdy * dwidy 
!      + nu_inv * ( u * dVdx + v * dVdy + V * dvdy ) * wi
!  
!    d VM-Eqn(I) / d P coefficient * P coefficient:
!
!      nu_inv * dPdy * wi
!
!    d PC-Eqn(I) / d U coefficient * U coefficient:
!
!      dUdx * qi
!
!    d PC-Eqn(I) / d V coefficient * V coefficient:
!
!      dVdx * qi
!
!*****************************************************************************80
!
!  Modified:
!
!    03 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Max Gunzburger,
!    Finite Element Methods for Viscous Incompressible Flows,
!    A Guide to Theory, Practice, and Algorithms,
!    Academic Press, 1989,
!    LC: TA357.G86
!
!  Parameters:
!
!    real A(NROW,MAXEQN), the value of D F(I)/D X(J) 
!    for each of the NEQN residual functions F(I) with respect to each 
!    of the unknown coefficients X(J).  The information is stored as
!    a LINPACK/LAPACK general band format matrix.
!
!    real DETMAP(MAXQUAD2,MAXELM), the reference map determinant.
!
!    character ( len = 3 ) EQN(MAXEQN), the type of each equation.
!      'UM'  the horizontal momentum equation;
!      'UI'  horizontal velocity specified at node.
!      'U0'  U = 0 at node.
!      'VM'  the vertical momentum equation;
!      'VI'  V = specified value at node.
!      'V0'  V = 0 at node.
!      'N0'  the velocity component normal to the wall is 0.
!      'T0'  the velocity component tangential to the wall is 0.
!      'TMP' momentum equation for the velocity component tangential
!            to the wall, plus penalty term.
!      'PC'  the continuity equation;
!      'PI'  P = specified value at node.
!      'P0'  P = 0 at node.
!
!      'FS'  free slip, Integral (U,V) dot (boundary tangent) is penalized;
!
!    real ETAQUAD(MAXQUAD2), the ETA quadrature coordinates.
!
!    real G(MAXEQN), the finite element coefficients.
!
!    integer IERROR, 0/nonzero, an error DID NOT/DID occur.
!
!    integer INDX(3,MAXNP), the mapping from nodes to degrees of freedom.
!    INDEX(1,J) = horizontal velocity degree of freedom index.
!    INDEX(2,J) = vertical velocity degree of freedom index.
!    INDEX(3,J) = pressure degree of freedom index, or 0 if no pressure.
!
!    integer IPIVOT(MAXEQN), pivoting space for the linear system solver.
!
!    integer JAC, analytic/finite difference jacobian option.
!    0, use analytic jacobian.
!    1, use finite difference jacobian.
!
!    integer MAXELM, the maximum number of elements, = 2 * MAXNX * MAXNY.
!
!    integer MAXEQN, the maximum number of equations.
!
!    integer NEWTON_MAX, the maximum number of Newton iterations.
!
!    integer MAXNP, the maximum number of nodes.
!
!    integer MAXNX, the maximum number of elements in the X direction.
!
!    integer MAXNY, the maximum number of elements in the Y direction.
!
!    integer MAXQUAD1, the maximum number of 1D quadrature points.
!
!    integer MAXQUAD2, the maximum number of 2D quadrature points.
!
!    integer MAXROW, the first dimension of the matrix A.
!
!    integer NELEM, the number of elements.
!
!    integer NEQN, the number of finite element equations.
!
!    integer NLBAND, the lower bandwidth of the matrix A.
!
!    integer NODE(6,MAXELM), the nodes that make up each element.
!
!    integer NP, the number of nodes.
!
!    integer NQUAD1, the number of 1D quadrature points.
!
!    integer NQUAD2, the number of 2D quadrature points.
!
!    integer NROW, the number of rows needed to store the matrix A.
!
!    integer NX, the number of elements along a horizontal line.
!
!    integer NY, the number of elements along a vertical line.
!
!    real PENALTY1, the TMP equation friction penalty coefficient.
!
!    real PENALTY2, the TMP momentum penalty coefficient.
!  
!    real PHI(MAXQUAD2,6,6,MAXELM), basis function values.
!    PHI contains the value of a basis function, its derivative,
!    or other information, evaluated at a quadrature point.
!    For a particular element I, quadrature point J, and basis
!    function K, we use the following shorthand for the 
!    entries of PHI:
!      W, dWdX, dWdY
!      Q, dQdX, dQdY
!    W is the quadratic basis function associated with velocity,
!    Q the linear basis function associated with pressure,
!    Xsi and Eta the reference coordinates for the point.
!    In particular, PHI(J,K,1,I) is the value of the quadratic
!    basis function associated with local node K in element I,
!    evaluated at quadrature point J.
!
!    real PNORM_LMAX, the max-norm of the pressure.
!
!    real PNORM_L2, the L2-norm of the pressure.
!
!    character ( len = 20 ) REGION, the flow problem, 'CAVITY', 'CHANNEL',
!    'FREESLIP', or 'STEP'.
!
!    real RES(MAXEQN), the finite element Newton residual.
!
!    real NU_INV, the inverse viscosity.
!
!    character ( len = 20 ) SOLVER, the linear system solver, 'GAUSS' or 'CGS'.
!
!    real SQUAD1(MAXQUAD1), the 1D reference quadrature coordinates.
!
!    real NEWTON_TOLERANCE, the Newton convergence tolerance.
!
!    real UVNORM_LMAX, the max-norm of the velocity magnitude.
!
!    real UVNORM_L2, the L2 norm of the velocity magnitude.
!
!    real WQUAD1(MAXQUAD1), the 1D quadrature weights.
!
!    real WQUAD2(MAXQUAD2), the 2D quadrature weights.
!
!    real XC(MAXNP), the X coordinates of the nodes.
!
!    real XRANGE, the width of the region.
!
!    real XSIQUAD(MAXQUAD2), the XSI quadrature coordinates.
!
!    real YC(MAXNP), the Y coordinates of the nodes.
!
!    real YRANGE, the height of the region.
!
  implicit none

  integer, parameter :: maxnx = 20
  integer, parameter :: maxny = 20
  integer, parameter :: maxquad1 = 3
  integer, parameter :: maxquad2 = 7
!
!  The assignment of MAXROW assumes that the nodes are ordered starting
!  at the bottom left corner, proceeding upwards in a column, and then
!  moving back to the bottom of the next column to the right.  For
!  our choice of elements, this allows us to estimate the greatest
!  difference between indices of two variables which occur in the
!  same equation.  
!
  integer, parameter :: maxrow = 29 * ( maxny + 1 )
  integer, parameter :: maxelm = 2 * maxnx * maxny
  integer, parameter :: maxeqn = 2 * ( 2 * maxnx + 1 ) &
    * ( 2 * maxny + 1 ) + ( maxnx + 1 ) * ( maxny + 1 ) 
  integer, parameter :: maxnp = ( 2 * maxnx + 1 ) & 
     * ( 2 * maxny + 1 )
  integer, parameter :: maxside = 4 * maxnx * maxny
  integer, parameter :: node_nabor_max = 19
  integer, parameter :: eqn_nabor_max = 45

  real ( kind = 8 ) a(maxrow,maxeqn)
  integer bc_tag(maxnp)
  real ( kind = 8 ) detmap(maxquad2,maxelm)
  character ( len = 3 ) eqn(maxeqn)
  integer eqn_nabor(eqn_nabor_max,maxeqn)
  integer eqn_nabor_num(maxeqn)
  real ( kind = 8 ) etaquad(maxquad2)
  character ( len = 20 ) file_name
  real ( kind = 8 ) g(maxeqn)
  real ( kind = 8 ) g2(maxeqn)
  integer i
  integer ierror
  integer indx(3,maxnp)
  integer ipivot(maxeqn)
  integer iplot
  integer iprint
  integer jac
  integer ncol
  integer node_nabor(node_nabor_max,maxnp)
  integer node_nabor_num(maxnp)
  integer nelem
  integer neqn
  integer neqn2
  integer newton_max
  character ( len = 10 ) newton_start
  integer newton_stutter
  real ( kind = 8 ) newton_tolerance
  integer nlband
  integer node(6,maxelm)
  integer np
  integer nquad1
  integer nquad2
  integer nrow
  integer nside
  integer nx
  integer nx2
  integer ny
  integer ny2
  real ( kind = 8 ) p(maxnp)
  real ( kind = 8 ) penalty1
  real ( kind = 8 ) penalty2
  real ( kind = 8 ) phi(maxquad2,6,6,maxelm)
  real ( kind = 8 ) pnorm_h1
  real ( kind = 8 ) pnorm_l2
  real ( kind = 8 ) pnorm_lmax
  character ( len = 20 ) region
  character ( len = 20 ) region2
  real ( kind = 8 ) region_xmax
  real ( kind = 8 ) region_ymax
  real ( kind = 8 ) res(maxeqn)
  real ( kind = 8 ) res2(maxeqn)
  real ( kind = 8 ) nu_inv
  real ( kind = 8 ) nu_inv2
  logical s_eqi
  integer side_basis(3,maxside)
  integer side_elem(maxside)
  character ( len = 3 ) side_eqn(maxside)
  real ( kind = 8 ) side_etam(maxside)
  real ( kind = 8 ) side_etap(maxside)
  integer side_indx(3,maxside)
  real ( kind = 8 ) side_xsim(maxside)
  real ( kind = 8 ) side_xsip(maxside)
  character ( len = 20 ) solver
  real ( kind = 8 ) squad1(maxquad1)
  character ( len = 20 ) system
  real ( kind = 8 ) uvnorm_h1
  real ( kind = 8 ) uvnorm_l2
  real ( kind = 8 ) uvnorm_lmax
  real ( kind = 8 ) wquad1(maxquad1)
  real ( kind = 8 ) wquad2(maxquad2)
  real ( kind = 8 ) xc(maxnp)
  real ( kind = 8 ) xsiquad(maxquad2)
  real ( kind = 8 ) yc(maxnp)

  call timestamp ( )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'FLOW7'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Last modified on 09 May 2000.'
!
!  Say hello.
!
  call hello ( maxelm, maxeqn, maxnp, maxnx, maxny, maxquad2, maxrow, maxside )
!
!  Zero out the variables.
!
  call init ( a, bc_tag, detmap, eqn, etaquad, g, ierror, indx, ipivot, jac, &
    maxelm, maxeqn, maxnp, maxquad1, maxquad2, maxrow, maxside, nelem, neqn, &
    newton_max, newton_stutter, newton_tolerance, nlband, node, np, nquad1, &
    nquad2, nrow, nside, nx, ny, p, penalty1, penalty2, phi, pnorm_l2, &
    pnorm_lmax, region, res, nu_inv, side_basis, side_elem, side_eqn, &
    side_etam, side_etap, side_indx, side_xsim, side_xsip, solver, squad1, &
    uvnorm_l2, uvnorm_lmax, wquad1, wquad2, xc, xsiquad, yc )
!
!  Now we set some values which will define all the others.
!  These values could be read in from a file.
!
  region = 'CAVITY'
!
!  Specify:
!
!    JAC, 0 to use jacobian routine, nonzero to estimate the jacobian.
!    NEWTON_STUTTER, number of times jacobian is used before recomputed;
!    NX, the number of elements in the horizontal direction, and
!    NY, the number of elements in the vertical direction;
!    PENALTY1, the tangential flow penalty coefficient applied to TMP nodes;
!    PENALTY2, the momentum penalty coefficient applied to TMP nodes;
!    REGION_XMAX, the width and 
!    REGION_YMAX, the height of the region;
!    NU_INV, the value of the inverse of the viscosity;
!    SYSTEM, 'NAVIER-STOKES' or 'STOKES'.
!
!  For the cavity, it would be nice if
!
!    NX = NY.
!
  if ( region == 'CAVITY' ) then

    jac = 0
    newton_stutter = 1
    nx = 3
    ny = 3
    penalty1 = 0.0D+00
    penalty2 = 1.0D+00
    region_xmax = 1.0D+00
    region_ymax = 1.0D+00
    nu_inv = 10.0D+00
    solver = 'GAUSS'
!   solver = 'CGS'
    system = 'NAVIER-STOKES'
!
!  For the channel, it would be nice if 
!
!    NX / NY = REGION_XMAX / REGION_YMAX
!
  else if ( region == 'CHANNEL' ) then

    jac = 0
    newton_stutter = 1
    nx = 10
    ny = 3
    penalty1 = 0.0D+00
    penalty2 = 1.0D+00
    region_xmax = 10.0D+00
    region_ymax = 3.0D+00
    nu_inv = 10.0D+00
    solver = 'GAUSS'
    system = 'NAVIER-STOKES'
!
!  For the freeslip channel, it would be nice if
!
!    NX / NY = REGION_XMAX / REGION_YMAX
!
  else if ( region == 'FREESLIP' ) then

    jac = 0
    newton_stutter = 1
    nx = 10
    ny = 3
    penalty1 = 1.0D+00
    penalty2 = 0.0D+00
    region_xmax = 10.0D+00
    region_ymax = 3.0D+00
    nu_inv = 10.0D+00
    solver = 'GAUSS'
    system = 'NAVIER-STOKES'
!
!  For the step, it would be nice if 
!
!    NX / NY = REGION_XMAX / REGION_YMAX
!
!  and if 
!
!    NX is divisible by 8.
!
  else if ( region == 'STEP' ) then

    jac = 0
    newton_stutter = 1
    nx = 32
    ny = 8
    penalty1 = 1.0D+00
    penalty2 = 0.0D+00
    region_xmax = 8.0D+00
    region_ymax = 2.0D+00
    nu_inv = 10.0D+00
    solver = 'GAUSS'
    system = 'NAVIER-STOKES'

  else

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FLOW7 - Fatal error!'
    write ( *, '(a)' ) '  An unexpected region was specified.'
    write ( *, '(a)' ) '  REGION = ' // trim ( region )
    stop

  end if
!
!  Set up the problem geometry.
!
  call geometry ( bc_tag, eqn, indx, maxelm, maxeqn, maxnp, nelem, neqn, node, &
    np, nx, ny, region, region_xmax, region_ymax, xc, yc )
!
!  Determine the node neighbor array.
!
  if ( solver == 'CGS' ) then

    call node_nabor_set ( nelem, node, np, node_nabor, node_nabor_max, &
      node_nabor_num )

    call node_nabor_print ( np, node_nabor, node_nabor_max, node_nabor_num )

  end if
!
!  Determine the equation neighbor array.
!
  if ( solver == 'CGS' ) then

    call eqn_nabor_set ( eqn_nabor, eqn_nabor_max, eqn_nabor_num, indx, &
      neqn, np, node_nabor, node_nabor_max, node_nabor_num )

    call eqn_nabor_print ( eqn_nabor, eqn_nabor_max, eqn_nabor_num, neqn )

  end if
!
!  Compute boundary integral information.
!
  call boundary_integral ( maxside, nside, nx, ny, region, side_basis, &
    side_elem, side_eqn, side_etam, side_etap, side_indx, side_xsim, side_xsip )
!
!  Print data.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  The flow region is ' // trim ( region )
  write ( *, '(a)' ) '  The state equations are ' // trim ( system )
  write ( *, '(a)' ) '  The linear solver is ' // trim ( solver )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  The number of:'
  write ( *, '(a,i8)' ) '    Horizontal elements NX = ', nx
  write ( *, '(a,i8)' ) '    Vertical elements NY=    ', ny
  write ( *, '(a,i8)' ) '    Elements, NELEM =        ', nelem
  write ( *, '(a,i8)' ) '    Nodes, NP =              ', np
  write ( *, '(a,i8)' ) '    Unknowns, NEQN =         ', neqn

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Newton method parameters:'
  write ( *, '(a)' ) ' '
  if ( jac == 0 ) then
    write ( *, '(a)' ) '    JAC = 0, use analytic jacobian.'
  else
    write ( *, '(a)' ) '    JAC nonzero, use finite difference jacobian.'
  end if
  write ( *, '(a,i8)' ) '    Iteration limit, NEWTON_MAX =', newton_max
  write ( *, '(a,i8)' ) '    Jacobian reuse, NEWTON_STUTTER =', newton_stutter
  write ( *, '(a,g14.6)' ) &
    '    Iteration tolerance, NEWTON_TOLERANCE =', newton_tolerance
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) &
    '  TMP friction term penalty coefficient PENALTY1 = ', penalty1
  write ( *, '(a,g14.6)' ) &
    '  TMP momentum term penalty coefficient PENALTY2 = ', penalty2

  iprint = 0

  if ( iprint == 1 ) then
    call xy_print ( maxnp, np, xc, yc )
  end if

  iprint = 0

  if ( iprint == 1 ) then
    call element_print ( maxelm, nelem, node )
  end if

  iprint = 0

  if ( iprint == 1 ) then
    call equation_print ( eqn, indx, neqn, np )
  end if

  iprint = 0

  if ( iprint == 1 ) then
    call boundary_integral_print ( maxside, nside, side_basis, side_elem, &
      side_eqn, side_etam, side_etap, side_indx, side_xsim, side_xsip )
  end if
!
!  Determine the lower matrix bandwidth, and NROW,
!  the number of rows we will use in the matrix A.
!
  if ( s_eqi ( solver, 'GAUSS' ) ) then

    call fp_band_width ( indx, maxelm, maxnp, nelem, nlband, node, nrow )

    ncol = neqn

    if ( maxrow < nrow ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FLOW7 - Fatal error!'
      write ( *, '(a,i8)' ) '  NROW is too large!  NROW =      ', nrow
      write ( *, '(a,i8)' ) '  The maximum allowed is MAXROW = ', maxrow
      stop
    end if

    write ( *, '(a)' ) ' ' 
    write ( *, '(a,i8)' ) '  Lower bandwidth NLBAND =      ', nlband
    write ( *, '(a,i8)' ) '  Available matrix rows, MAXROW = ', maxrow

  else if ( s_eqi ( solver, 'CGS' ) ) then

    nrow = neqn
    ncol = node_nabor_max

  end if

  write ( *, '(a,i8)' ) '  Required matrix rows NROW =    ', nrow
  write ( *, '(a,i8)' ) '  Required matrix columns NCOL = ', ncol

  if ( solver == 'CGS' ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FLOW7 - Warning!'
    write ( *, '(a)' ) '  The CGS solver has been requested.'
    write ( *, '(a)' ) '  But this solver is not available.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Abnormal end of execution.'
    stop
  end if
!
!  Set the boundary quadrature rule.
!
  call ref_quad1 ( maxquad1, nquad1, squad1, wquad1 )
!
!  Set the reference element quadrature rule.
!
  call ref_quad2 ( etaquad, maxquad2, nquad2, wquad2, xsiquad )
!
!  Set the value of the basis functions at all quadrature points.
!
  call setbas ( detmap, etaquad, maxelm, maxnp, maxquad2, nelem, node, &
    nquad2, phi, xc, xsiquad, yc )
!
!  Solve the linear STOKES system, or the nonlinear NAVIER-STOKES system.
!
  if ( system == 'STOKES' ) then

    call stokes ( a, bc_tag, detmap, eqn, g, ierror, indx, ipivot, jac, &
      maxelm, maxeqn, maxnp, maxquad1, maxquad2, maxside, ncol, nelem, neqn, &
      nlband, node, np, nquad1, nquad2, nrow, nside, penalty1, penalty2, phi, &
      region, region_ymax, res, res2, nu_inv, side_basis, side_elem, &
      side_eqn, side_etam, side_etap, side_indx, side_xsim, side_xsip, &
      solver, squad1, wquad1, wquad2, xc, yc )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FLOW7:'
    write ( *, '(a)' ) '  STOKES computed the Stokes solution.'

  else if ( system == 'NAVIER-STOKES' ) then
!
!  Get an initial estimate of the solution.
!
    newton_start = 'ZERO'

    if ( newton_start == 'STOKES' ) then

      write ( *, '(a)' ) 'Get Newton start via Stokes solution.'

      call stokes ( a, bc_tag, detmap, eqn, g, ierror, indx, ipivot, jac, &
        maxelm, maxeqn, maxnp, maxquad1, maxquad2, maxside, ncol, nelem, &
        neqn, nlband, node, np, nquad1, nquad2, nrow, nside, penalty1, &
        penalty2, phi, region, region_ymax, res, res2, nu_inv, side_basis, &
        side_elem, side_eqn, side_etam, side_etap, side_indx, side_xsim, &
        side_xsip, solver, squad1, wquad1, wquad2, xc, yc )

    else if ( newton_start == 'ZERO' ) then

      write ( *, '(a)' ) 'Set Newton starting point to zero.'
      g(1:neqn) = 0.0D+00

    else if ( newton_start == 'FILE' ) then

      write ( *, '(a)' ) 'Get Newton starting point from file.'

      file_name = 'g.txt'

      call coef_read ( file_name, g, ierror, maxeqn, neqn2, nx2, ny2, &
        region2, nu_inv2 )
  
      if ( ierror == 0 ) then
        write ( *, '(a,g14.6)' ) '  Saved solution has NU_INV = ', nu_inv2
      end if

      if ( neqn2 /= neqn ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FLOW7 - Warning!'
        write ( *, '(a,i8)' ) '  Saved solution file has NEQN = ', neqn2
        write ( *, '(a,i8)' ) '  But we need NEQN = ', neqn
        ierror = 1
      end if

      if ( ierror /= 0 ) then
        g(1:neqn) = 0.0D+00
        write ( *, '(a)' ) 'File reading failed.  Setting G to zero.'
      end if

    end if
!
!  Solve the nonlinear system.
!
    call newton ( a, bc_tag, detmap, eqn, g, ierror, indx, ipivot, jac, &
      maxelm, maxeqn, maxnp, maxquad1, maxquad2, maxside, ncol, nelem, &
      neqn, newton_max, newton_stutter, newton_tolerance, nlband, node, &
      np, nquad1, nquad2, nrow, nside, penalty1, penalty2, phi, region, &
      region_ymax, res, res2, nu_inv, side_basis, side_elem, side_eqn, &
      side_etam, side_etap, side_indx, side_xsim, side_xsip, solver, squad1, &
      wquad1, wquad2, xc, yc )
 
    if ( ierror /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FLOW7 - Fatal error!'
      write ( *, '(a)' ) '  The Newton iteration failed!'
      stop
    end if

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FLOW7:'
    write ( *, '(a)' ) '  NEWTON computed the Navier Stokes solution.'

  else

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FLOW7 - Fatal error!'
    write ( *, '(a)' ) '  Unrecognized state equation name.'
    stop

  end if

  call uvp_norm_lmax ( g, indx, maxeqn, maxnp, np, pnorm_lmax, uvnorm_lmax )

  write ( *, '(a,g14.6)' ) '  Max-norm of velocity magnitude = ', uvnorm_lmax
  write ( *, '(a,g14.6)' ) '  Max-norm of pressure           = ', pnorm_lmax

  call uvp_norm_l2 ( detmap, g, indx, maxelm, maxeqn, maxnp, maxquad2, &
    nelem, node, nquad2, phi, pnorm_l2, uvnorm_l2, wquad2 )

  write ( *, '(a,g14.6)' ) '  L2-norm of velocity magnitude  = ', uvnorm_l2
  write ( *, '(a,g14.6)' ) '  L2-norm of pressure            = ', pnorm_l2

  call uvp_norm_h1 ( detmap, g, indx, maxelm, maxeqn, maxnp, maxquad2, &
    nelem, node, nquad2, phi, pnorm_h1, uvnorm_h1, wquad2 )

  write ( *, '(a,g14.6)' ) '  H1-norm of velocity magnitude  = ', uvnorm_h1
  write ( *, '(a,g14.6)' ) '  H1-norm of pressure            = ', pnorm_h1
!
!  Interpolate values of the pressure at nodes which do not
!  have an associated pressure unknown, and create the P array.
!
  call press_interp ( g, indx, maxelm, maxeqn, maxnp, nelem, node, p )
!
!  Print the solution.
!
  iprint = 0

  if ( iprint == 1 ) then

    call uvp_print ( g, indx, maxeqn, maxnp, np, p, xc, yc )

  end if
!
!  Write element and solution data.
!
  iplot = 1

  if ( iplot == 1 ) then

    file_name = 'elements.txt'

    call element_write ( file_name, ierror, maxelm, nelem, node )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FLOW7:'
    write ( *, '(a)' ) '  Wrote the element data file "' // &
      trim ( file_name ) // '".'

    file_name = 'uvp.txt'

    call node_write ( file_name, g, ierror, indx, maxeqn, maxnp, np, p, xc, yc )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FLOW7:'
    write ( *, '(a)' ) '  Wrote the UVP(XY) data file "' // &
      trim ( file_name ) // '".'

    file_name = 'g.txt'

    call coef_write ( file_name, g, ierror, maxeqn, neqn, nx, ny, region, &
      nu_inv )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FLOW7:'
    write ( *, '(a)' ) '  Wrote the coefficient data file "' // &
      trim ( file_name ) // '".'

  end if

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'FLOW7:'
  write ( *, '(a)' ) '  Normal end of execution.'

  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop
end
subroutine boundary_integral ( maxside, nside, nx, ny, region, side_basis, &
  side_elem, side_eqn, side_etam, side_etap, side_indx, side_xsim, side_xsip )

!*****************************************************************************80
!
!! BOUNDARY_INTEGRAL sets data useful for boundary integrals.
!
!  Discussion:
!
!    This routine catalogs each element side that is along the boundary.
!    For each side, the routine records:
!
!    * the element to which the side belongs;
!    * the three basis functions, in counter-clockwise order, along the side;
!    * the values of ETA and XSI at the beginning and end of the side.
!    * the local degree of freedom associated with the boundary condition.
!    * the boundary condition being applied.
!
!  Modified:
!
!    08 June 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer MAXSIDE, the maximum number of boundary element sides.
!
!    Output, integer NSIDE, the number of boundary element sides.
!
!    Input, integer NX, NY, the number of elements along a horizontal, or
!    vertical line.
!
!    Input, character ( len = 20 ) REGION, the flow problem.
!
!    Output, integer SIDE_BASIS(3,MAXSIDE), the local indices of the
!    three basis functions along the boundary side.  These are given in
!    an order consistent with the counter clockwise ordering of the
!    entire boundary.
!
!    Output, integer SIDE_ELEM(MAXSIDE), the element to which the boundary
!    side belongs.
!
!    Output, character ( len = 3 ) SIDE_EQN(MAXSIDE), indicates the boundary
!    condition being applied:
!
!    '???', no condition is being applied;
!    'UI', a specified value of horizontal velocity;
!    'VI', a specified value of vertical velocity;
!    'FS'  free slip, Integral (U,V) dot (boundary tangent) is penalized.
!
!    Output, real ( kind = 8 ) SIDE_ETAM(MAXSIDE), SIDE_ETAP(MAXSIDE), the
!    ETA value of the first and last points on the boundary side, consistent
!    with the counter-clockwise ordering of the boundary.
!
!    Output, integer SIDE_INDX(3,MAXSIDE), the degree of freedom ( 1, 2, or
!    3 ) associated with the local basis function, which is being controlled
!    by this boundary condition.
!
!    Output, real SIDE_XSIM(MAXSIDE), SIDE_XSIP(MAXSIDE), the
!    XSI value of the first and last points on the boundary side, consistent
!    with the counter-clockwise ordering of the boundary.
!
  implicit none

  integer maxside

  integer nside
  integer nx
  integer ny
  character ( len = 20 ) region
  integer side_basis(3,maxside)
  integer side_elem(maxside)
  character ( len = 3 ) side_eqn(maxside)
  real ( kind = 8 ) side_etam(maxside)
  real ( kind = 8 ) side_etap(maxside)
  integer side_indx(3,maxside)
  real ( kind = 8 ) side_xsim(maxside)
  real ( kind = 8 ) side_xsip(maxside)

  nside = 0

  if ( region == 'CAVITY' ) then

  else if ( region == 'CHANNEL' ) then

  else if ( region == 'FREESLIP' ) then

    call boundary_integral_freeslip ( maxside, nside, nx, ny, side_basis, &
      side_elem, side_eqn, side_etam, side_etap, side_indx, side_xsim, &
      side_xsip )

  else if ( region == 'STEP' ) then

    call boundary_integral_step ( maxside, nside, nx, ny, side_basis, &
      side_elem, side_eqn, side_etam, side_etap, side_indx, side_xsim, &
      side_xsip )
 
  end if

  return
end
subroutine boundary_integral_freeslip ( maxside, nside, nx, ny, side_basis, &
  side_elem, side_eqn, side_etam, side_etap, side_indx, side_xsim, side_xsip )

!*****************************************************************************80
!
!! BOUNDARY_INTEGRAL_FREESLIP sets boundary integral data for the freeslip.
!
!  Discussion:
!
!    This routine catalogs each element side that is along the boundary.
!    For each side, the routine records:
!
!    * the element to which the side belongs;
!    * the three basis functions, in counter-clockwise order, along the side;
!    * the values of ETA and XSI at the beginning and end of the side.
!    * the local degree of freedom associated with the boundary condition.
!    * the boundary condition being applied.
!
!  Modified:
!
!    08 June 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer MAXSIDE, the maximum number of boundary element sides.
!
!    Output, integer NSIDE, the number of boundary element sides.
!
!    Input, integer NX, NY, the number of elements along a horizontal, or
!    vertical line.
!
!    Output, integer SIDE_BASIS(3,MAXSIDE), the local indices of the
!    three basis functions along the boundary side.  These are given in
!    an order consistent with the counter clockwise ordering of the
!    entire boundary.
!
!    Output, integer SIDE_ELEM(MAXSIDE), the element to which the boundary
!    side belongs.
!
!    Output, character ( len = 3 ) SIDE_EQN(MAXSIDE), indicates the boundary
!    condition being applied:
!    '???', no condition is being applied;
!    'UI', a specified value of horizontal velocity;
!    'VI', a specified value of vertical velocity;
!    'FS'  free slip, Integral (U,V) dot (boundary tangent) is penalized.
!
!    Output, real SIDE_ETAM(MAXSIDE), SIDE_ETAP(MAXSIDE), the
!    ETA value of the first and last points on the boundary side, consistent
!    with the counter-clockwise ordering of the boundary.
!
!    Output, integer SIDE_INDX(3,MAXSIDE), the degree of freedom ( 1, 2, or
!    3 ) associated with the local basis function, which is being controlled
!    by this boundary condition.
!
!    Output, real SIDE_XSIM(MAXSIDE), SIDE_XSIP(MAXSIDE), the
!    XSI value of the first and last points on the boundary side, consistent
!    with the counter-clockwise ordering of the boundary.
!
  implicit none

  integer maxside

  integer icol
  integer ielem
  integer iq
  integer irow
  integer nside
  integer nx
  integer ny
  integer side_basis(3,maxside)
  integer side_elem(maxside)
  character ( len = 3 ) side_eqn(maxside)
  real ( kind = 8 ) side_etam(maxside)
  real ( kind = 8 ) side_etap(maxside)
  integer side_indx(3,maxside)
  real ( kind = 8 ) side_xsim(maxside)
  real ( kind = 8 ) side_xsip(maxside)
!
!  Bottom boundary.
!  Freeslip.
!
  do icol = 1, nx

    ielem = 2 * ( icol - 1 ) * ny + 1

    nside = nside + 1

    side_eqn(nside) = 'FS'

    side_elem(nside) = ielem

    iq = 3
    side_basis(1,nside) = iq
    side_indx(1,nside) = 2

    iq = 4
    side_basis(2,nside) = iq
    side_indx(2,nside) = 2

    iq = 1
    side_basis(3,nside) = iq
    side_indx(3,nside) = 2

    side_etam(nside) = 0.0D+00
    side_etap(nside) = 0.0D+00
    side_xsim(nside) = 0.0D+00
    side_xsip(nside) = 1.0D+00

  end do
!
!  Right boundary.
!  Vertical velocity is 0.
!
  do irow = 1, ny

    ielem = 2 * ny * ( nx - 1 ) + 2 * irow

    nside = nside + 1

    side_eqn(nside) = 'V0'
    side_elem(nside) = ielem

    iq = 2
    side_basis(1,nside) = iq
    side_indx(1,nside) = 2

    iq = 6
    side_basis(2,nside) = iq
    side_indx(2,nside) = 2

    iq = 3
    side_basis(3,nside) = iq
    side_indx(3,nside) = 2

    side_etam(nside) = 1.0D+00
    side_etap(nside) = 0.0D+00
    side_xsim(nside) = 0.0D+00
    side_xsip(nside) = 0.0D+00

  end do
!
!  Top boundary.
!  Free slip
!
  do icol = 1, nx

    ielem = 2 * icol * ny

    nside = nside + 1

    side_eqn(nside) = 'FS'

    side_elem(nside) = ielem

    iq = 3
    side_basis(1,nside) = iq
    side_indx(1,nside) = 2

    iq = 4
    side_basis(2,nside) = iq
    side_indx(2,nside) = 2

    iq = 1
    side_basis(3,nside) = iq
    side_indx(3,nside) = 2

    side_etam(nside) = 0.0D+00
    side_etap(nside) = 0.0D+00
    side_xsim(nside) = 0.0D+00
    side_xsip(nside) = 1.0D+00

  end do
!
!  Left boundary.
!  Horizontal velocity is specified.
!  Vertical velocity is 0.
!
  do irow = 1, ny

    ielem = 2 * irow - 1

    nside = nside + 1

    side_eqn(nside) = 'UI'

    side_elem(nside) = ielem

    iq = 2
    side_basis(1,nside) = iq
    side_indx(1,nside) = 1

    iq = 6
    side_basis(2,nside) = iq
    side_indx(2,nside) = 1

    iq = 3
    side_basis(3,nside) = iq
    side_indx(3,nside) = 1

    side_etam(nside) = 1.0D+00
    side_etap(nside) = 0.0D+00
    side_xsim(nside) = 0.0D+00
    side_xsip(nside) = 0.0D+00

    nside = nside + 1

    side_eqn(nside) = 'V0'

    side_elem(nside) = ielem

    iq = 2
    side_basis(1,nside) = iq
    side_indx(1,nside) = 2

    iq = 6
    side_basis(2,nside) = iq
    side_indx(2,nside) = 2

    iq = 3
    side_basis(3,nside) = iq
    side_indx(3,nside) = 2

    side_etam(nside) = 1.0D+00
    side_etap(nside) = 0.0D+00
    side_xsim(nside) = 0.0D+00
    side_xsip(nside) = 0.0D+00

  end do

  return
end
subroutine boundary_integral_step ( maxside, nside, nx, ny, side_basis, &
  side_elem, side_eqn, side_etam, side_etap, side_indx, side_xsim, side_xsip )

!*****************************************************************************80
!
!! BOUNDARY_INTEGRAL_STEP sets boundary integral data for the step.
!
!  Discussion:
!
!    This routine catalogs each element side that is along the boundary.
!    For each side, the routine records:
!
!    * the element to which the side belongs;
!    * the three basis functions, in counter-clockwise order, along the side;
!    * the values of ETA and XSI at the beginning and end of the side.
!    * the local degree of freedom associated with the boundary condition.
!    * the boundary condition being applied.
!
!  Modified:
!
!    08 June 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer MAXSIDE, the maximum number of boundary element sides.
!
!    Output, integer NSIDE, the number of boundary element sides.
!
!    Input, integer NX, NY, the number of elements along a horizontal, or
!    vertical line.
!
!    Output, integer SIDE_BASIS(3,MAXSIDE), the local indices of the
!    three basis functions along the boundary side.  These are given in
!    an order consistent with the counter clockwise ordering of the
!    entire boundary.
!
!    Output, integer SIDE_ELEM(MAXSIDE), the element to which the boundary
!    side belongs.
!
!    Output, character ( len = 3 ) SIDE_EQN(MAXSIDE), indicates the boundary
!    condition being applied:
!    '???', no condition is being applied;
!    'UI', a specified value of horizontal velocity;
!    'VI', a specified value of vertical velocity;
!    'FS'  free slip, Integral (U,V) dot (boundary tangent) is penalized.
!
!    Output, real SIDE_ETAM(MAXSIDE), SIDE_ETAP(MAXSIDE), the
!    ETA value of the first and last points on the boundary side, consistent
!    with the counter-clockwise ordering of the boundary.
!
!    Output, integer SIDE_INDX(3,MAXSIDE), the degree of freedom ( 1, 2, or
!    3 ) associated with the local basis function, which is being controlled
!    by this boundary condition.
!
!    Output, real SIDE_XSIM(MAXSIDE), SIDE_XSIP(MAXSIDE), the
!    XSI value of the first and last points on the boundary side, consistent
!    with the counter-clockwise ordering of the boundary.
!
  implicit none

  integer maxside

  integer ielem
  integer ihi
  integer ilo
  integer iq
  integer nside
  integer nx
  integer nx1
  integer nx2
  integer nx3
  integer ny
  integer ny1
  integer ny2
  integer side_basis(3,maxside)
  integer side_elem(maxside)
  character ( len = 3 ) side_eqn(maxside)
  real ( kind = 8 ) side_etam(maxside)
  real ( kind = 8 ) side_etap(maxside)
  integer side_indx(3,maxside)
  real ( kind = 8 ) side_xsim(maxside)
  real ( kind = 8 ) side_xsip(maxside)

  nside = 0

  nx1 = nint ( 0.375D+00 * real ( nx, kind = 8 ) )
  nx1 = max ( 1, nx1 )
  nx2 = nint ( 0.125D+00 * real ( nx, kind = 8 ) )
  nx2 = max ( 1, nx2 )
  nx3 = nx - nx1 - nx2

  ny1 = max ( 1, nint ( 0.500D+00 * real ( ny, kind = 8 ) ) )
  ny2 = ny - ny1
!
!  Segment 1: horizontal bottom near inflow.
!
  ilo = 1
  ihi = 2 * ( nx1 - 1 ) * ny + 1
  do ielem = ilo, ihi, 2 * ny

    nside = nside + 1

    side_eqn(nside) = 'FS'

    side_elem(nside) = ielem

    iq = 3
    side_basis(1,nside) = iq
    side_indx(1,nside) = 2

    iq = 4
    side_basis(2,nside) = iq
    side_indx(2,nside) = 2

    iq = 1
    side_basis(3,nside) = iq
    side_indx(3,nside) = 2

    side_etam(nside) = 0.0D+00
    side_etap(nside) = 0.0D+00
    side_xsim(nside) = 0.0D+00
    side_xsip(nside) = 1.0D+00

  end do
!
!  Segment 2: vertical riser of step.
!
  ilo = 2 * ( nx1 - 1 ) * ny + 2
  ihi = 2 * ( nx1 - 1 ) * ny + 2 * ny1
  do ielem = ilo, ihi, 2

    nside = nside + 1

    side_eqn(nside) = 'FS'

    side_elem(nside) = ielem

    iq = 2
    side_basis(1,nside) = iq
    side_indx(1,nside) = 2

    iq = 6
    side_basis(2,nside) = iq
    side_indx(2,nside) = 2

    iq = 3
    side_basis(3,nside) = iq
    side_indx(3,nside) = 2

    side_etam(nside) = 1.0D+00
    side_etap(nside) = 0.0D+00
    side_xsim(nside) = 0.0D+00
    side_xsip(nside) = 0.0D+00

  end do
!
!  Segment 3: horizontal step.
!
  ilo = 2 * nx1 * ny + 1
  ihi = 2 * nx1 * ny + 1 + 2 * ny2 * ( nx2 - 1 )
  do ielem = ilo, ihi, 2 * ny2

    nside = nside + 1

    side_eqn(nside) = 'FS'

    side_elem(nside) = ielem

    iq = 3
    side_basis(1,nside) = iq
    side_indx(1,nside) = 2

    iq = 4
    side_basis(2,nside) = iq
    side_indx(2,nside) = 2

    iq = 1
    side_basis(3,nside) = iq
    side_indx(3,nside) = 2

    side_etam(nside) = 0.0D+00
    side_etap(nside) = 0.0D+00
    side_xsim(nside) = 0.0D+00
    side_xsip(nside) = 1.0D+00

  end do
!
!  Segment 4: vertical descender of step.
!
  ilo = 2 * nx1 * ny + 2 * nx2 * ny2 + 2 * ( ny1 - 1 ) + 1
  ihi = 2 * nx1 * ny + 2 * nx2 * ny2 + 1
  do ielem = ilo, ihi, -2

    nside = nside + 1

    side_eqn(nside) = 'FS'

    side_elem(nside) = ielem

    iq = 2
    side_basis(1,nside) = iq
    side_indx(1,nside) = 2

    iq = 6
    side_basis(2,nside) = iq
    side_indx(2,nside) = 2

    iq = 3
    side_basis(3,nside) = iq
    side_indx(3,nside) = 2

    side_etam(nside) = 1.0D+00
    side_etap(nside) = 0.0D+00
    side_xsim(nside) = 0.0D+00
    side_xsip(nside) = 0.0D+00

  end do
!
!  Segment 5: horizontal bottom near outflow
!
  ilo = 2 * nx1 * ny + 2 * nx2 * ny2 + 1
  ihi = 2 * nx1 * ny + 2 * nx2 * ny2 + 2 * ( nx3 - 1 ) * ny + 1
  do ielem = ilo, ihi, 2 * ny

    nside = nside + 1

    side_eqn(nside) = 'FS'

    side_elem(nside) = ielem

    iq = 3
    side_basis(1,nside) = iq
    side_indx(1,nside) = 2

    iq = 4
    side_basis(2,nside) = iq
    side_indx(2,nside) = 2

    iq = 1
    side_basis(3,nside) = iq
    side_indx(3,nside) = 2

    side_etam(nside) = 0.0D+00
    side_etap(nside) = 0.0D+00
    side_xsim(nside) = 0.0D+00
    side_xsip(nside) = 1.0D+00

  end do
!
!  Segment 6: top horizontal near outflow
!
  ilo = 2 * ( nx1 * ( ny1 + ny2 ) + nx2 * ny2 + nx3 * ( ny1 + ny2 ) )
  ihi = 2 * ( nx1 * ( ny1 + ny2 ) + nx2 * ny2 + ny1 + ny2 )

  do ielem = ilo, ihi, - 2 * ny

    nside = nside + 1

    side_eqn(nside) = 'FS'

    side_elem(nside) = ielem

    iq = 3
    side_basis(1,nside) = iq
    side_indx(1,nside) = 2

    iq = 4
    side_basis(2,nside) = iq
    side_indx(2,nside) = 2

    iq = 1
    side_basis(3,nside) = iq
    side_indx(3,nside) = 2

    side_etam(nside) = 0.0D+00
    side_etap(nside) = 0.0D+00
    side_xsim(nside) = 0.0D+00
    side_xsip(nside) = 1.0D+00

  end do
!
!  Segment 7: top horizontal over step
!
  ilo = 2 * nx1 * ny + 2 * nx2 * ny2
  ihi = 2 * nx1 * ny + 2 * ny2

  do ielem = ilo, ihi, - 2 * ny2

    nside = nside + 1

    side_eqn(nside) = 'FS'

    side_elem(nside) = ielem

    iq = 3
    side_basis(1,nside) = iq
    side_indx(1,nside) = 2

    iq = 4
    side_basis(2,nside) = iq
    side_indx(2,nside) = 2

    iq = 1
    side_basis(3,nside) = iq
    side_indx(3,nside) = 2

    side_etam(nside) = 0.0D+00
    side_etap(nside) = 0.0D+00
    side_xsim(nside) = 0.0D+00
    side_xsip(nside) = 1.0D+00

  end do
!
!  Segment 8: top horizontal near inflow
!
  ilo = 2 * ny * nx1
  ihi = 2 * ny
  do ielem = ilo, ihi, - 2 * ny

    nside = nside + 1

    side_eqn(nside) = 'FS'

    side_elem(nside) = ielem

    iq = 3
    side_basis(1,nside) = iq
    side_indx(1,nside) = 2

    iq = 4
    side_basis(2,nside) = iq
    side_indx(2,nside) = 2

    iq = 1
    side_basis(3,nside) = iq
    side_indx(3,nside) = 2

    side_etam(nside) = 0.0D+00
    side_etap(nside) = 0.0D+00
    side_xsim(nside) = 0.0D+00
    side_xsip(nside) = 1.0D+00

  end do

  return
end
subroutine boundary_integral_print ( maxside, nside, side_basis, side_elem, &
  side_eqn, side_etam, side_etap, side_indx, side_xsim, side_xsip )

!*****************************************************************************80
!
!! BOUNDARY_INTEGRAL_PRINT prints data useful for boundary integrals.
!
!  Modified:
!
!    12 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer MAXSIDE, the maximum number of boundary element sides.
!
!    Input, integer NSIDE, the number of boundary element sides.
!
!    Input, integer SIDE_BASIS(3,MAXSIDE), the local indices of the
!    three basis functions along the boundary side.  These are given in
!    an order consistent with the counter clockwise ordering of the 
!    entire boundary.
!
!    Input, integer SIDE_ELEM(MAXSIDE), the element to which the boundary
!    side belongs.
!
!    Input, character ( len = 3 ) SIDE_EQN(MAXSIDE), indicates the boundary
!    condition being applied:
!
!    '???', no condition is being applied;
!    'UI', a specified value of horizontal velocity;
!    'VI', a specified value of vertical velocity;
!    'FS', a no slip condition, applied to velocity.
!
!    Input, real SIDE_ETAM(MAXSIDE), SIDE_ETAP(MAXSIDE), the
!    ETA value of the first and last points on the boundary side, consistent
!    with the counter-clockwise ordering of the boundary.
!
!    Input, integer SIDE_INDX(3,MAXSIDE), the degree of freedom ( 1, 2, or 
!    3 ) associated with the local basis function, which is being controlled
!    by this boundary condition.
!
!    Input, real SIDE_XSIM(MAXSIDE), SIDE_XSIP(MAXSIDE), the
!    XSI value of the first and last points on the boundary side, consistent
!    with the counter-clockwise ordering of the boundary.
!
  implicit none

  integer maxside

  integer iside
  integer nside
  integer side_basis(3,maxside)
  integer side_elem(maxside)
  character ( len = 3 ) side_eqn(maxside)
  real ( kind = 8 ) side_etam(maxside)
  real ( kind = 8 ) side_etap(maxside)
  integer side_indx(3,maxside)
  real ( kind = 8 ) side_xsim(maxside)
  real ( kind = 8 ) side_xsip(maxside)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BOUNDARY_INTEGRAL_PRINT:'
  write ( *, '(a,i8)' ) '  Number of boundary sides is ', nside
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  ISIDE  ELEMENT  EQN_TYPE'
  write ( *, '(a)' ) '    IQ1, INDX1, XSI, ETA'
  write ( *, '(a)' ) '    IQ2, INDX2'
  write ( *, '(a)' ) '    IQ3, INDX3, XSI, ETA'
  write ( *, '(a)' ) ' '
  do iside = 1, nside
    write ( *, '(a)' ) ' '
    write ( *, '(i8,3x,i8,3x,a3)' ) iside, side_elem(iside), side_eqn(iside)
    write ( *, '(6x,i8,3x,i8,3x,2g14.6)' ) side_basis(1,iside), &
      side_indx(1,iside), side_xsim(iside), side_etam(iside)
    write ( *, '(6x,i8,3x,i8,3x,2g14.6)' ) side_basis(2,iside), &
      side_indx(2,iside)
    write ( *, '(6x,i8,3x,i8,3x,2g14.6)' ) side_basis(3,iside), &
      side_indx(3,iside), side_xsip(iside), side_etap(iside)
  end do

  return
end
subroutine boundary_shape ( bc_tag, normal, region, tangent )

!*****************************************************************************80
!
!! BOUNDARY_SHAPE returns the tangent and normal vectors along the boundary.
!
!  Modified:
!
!    08 June 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer BC_TAG, a code which is nonzero at every boundary node,
!    and which indicates the type of boundary conditions at that node.
!    The code used can vary from problem to problem.
!
!    Output, real NORMAL(2), the X and Y components of the normal
!    vector to the boundary at the given point.  The normal vector should
!    point out of the region.
!
!    Input, character ( len = 20 ) REGION, the flow problem.
!
!    Output, real TANGENT(2), the X and Y components of the
!    tangential vector to the boundary, at the given point.  The tangent
!    vector should point in the counter-clockwise direction, relative to
!    a point within the interior of the region.
!
  implicit none

  integer bc_tag
  real ( kind = 8 ) normal(2)
  character ( len = * ) region
  real ( kind = 8 ) tangent(2)

  if ( region == 'CAVITY' ) then

    call boundary_shape_cavity ( bc_tag, normal, tangent )

  else if ( region == 'CHANNEL' ) then

    call boundary_shape_channel ( bc_tag, normal, tangent )

  else if ( region == 'FREESLIP' ) then

    call boundary_shape_freeslip ( bc_tag, normal, tangent )

  else if ( region == 'STEP' ) then

    call boundary_shape_step ( bc_tag, normal, tangent )

  else

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'BOUNDARY_SHAPE - Fatal error!'
    write ( *, '(a)' ) '  Unrecognized region!'
    stop

  end if

  return
end
subroutine boundary_shape_cavity ( bc_tag, normal, tangent )

!*****************************************************************************80
!
!! BOUNDARY_SHAPE_CAVITY returns the boundary of the cavity problem.
!
!  Discussion:
!
!    Here is a schematic of the values of BC_TAG along the boundary
!    for this problem:
!
!      7 6 6 6 6 6 5
!      8 0 0 0 0 0 4
!      8 0 0 0 0 0 4
!      8 0 0 0 0 0 4
!      1 2 2 2 2 2 3
!
!  Modified:
!
!    08 June 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer BC_TAG, a code which is nonzero at every boundary node,
!    and which indicates the type of boundary conditions at that node.
!    The code used can vary from problem to problem.
!
!    Output, real NORMAL(2), the X and Y components of the normal
!    vector to the boundary at the given point.  The normal vector should
!    point out of the region.
!
!    Output, real TANGENT(2), the X and Y components of the
!    tangential vector to the boundary, at the given point.  The tangent
!    vector should point in the counter-clockwise direction, relative to
!    a point within the interior of the region.
!
  implicit none

  integer bc_tag
  real ( kind = 8 ) normal(2)
  real ( kind = 8 ) tangent(2)

  if ( bc_tag == 1 ) then

    normal(1) = - sqrt ( 2.0D+00 ) / 2.0D+00
    normal(2) = - sqrt ( 2.0D+00 ) / 2.0D+00

    tangent(1) =   sqrt ( 2.0D+00 ) / 2.0D+00
    tangent(2) = - sqrt ( 2.0D+00 ) / 2.0D+00

  else if ( bc_tag == 2 ) then

    normal(1) =   0.0D+00
    normal(2) = - 1.0D+00

    tangent(1) =   1.0D+00
    tangent(2) =   0.0D+00

  else if ( bc_tag == 3 ) then

    normal(1) =   sqrt ( 2.0D+00 ) / 2.0D+00
    normal(2) = - sqrt ( 2.0D+00 ) / 2.0D+00

    tangent(1) =   sqrt ( 2.0D+00 ) / 2.0D+00
    tangent(2) =   sqrt ( 2.0D+00 ) / 2.0D+00

  else if ( bc_tag == 4 ) then

    normal(1) = 1.0D+00
    normal(2) = 0.0D+00

    tangent(1) =   0.0D+00
    tangent(2) =   1.0D+00

  else if ( bc_tag == 5 ) then

    normal(1) =   sqrt ( 2.0D+00 ) / 2.0D+00
    normal(2) =   sqrt ( 2.0D+00 ) / 2.0D+00

    tangent(1) = - sqrt ( 2.0D+00 ) / 2.0D+00
    tangent(2) =   sqrt ( 2.0D+00 ) / 2.0D+00

  else if ( bc_tag == 6 ) then

    normal(1) = 0.0D+00
    normal(2) = 1.0D+00

    tangent(1) = - 1.0D+00
    tangent(2) =   0.0D+00

  else if ( bc_tag == 7 ) then

    normal(1) = - sqrt ( 2.0D+00 ) / 2.0D+00
    normal(2) =   sqrt ( 2.0D+00 ) / 2.0D+00

    tangent(1) = - sqrt ( 2.0D+00 ) / 2.0D+00
    tangent(2) = - sqrt ( 2.0D+00 ) / 2.0D+00

  else if ( bc_tag == 8 ) then

    normal(1) = - 1.0D+00
    normal(2) =   0.0D+00

    tangent(1) =   0.0D+00
    tangent(2) = - 1.0D+00

  else

    normal(1) = 0.0D+00
    normal(2) = 0.0D+00

    tangent(1) = 0.0D+00
    tangent(2) = 0.0D+00

  end if

  return
end
subroutine boundary_shape_channel ( bc_tag, normal, tangent )

!*****************************************************************************80
!
!! BOUNDARY_SHAPE_CHANNEL returns the boundary of the channel problem.
!
!  Discussion:
!
!    Here is a schematic of the values of BC_TAG along the boundary
!    for this problem:
!
!      7 6 6 6 6 6 5
!      8 0 0 0 0 0 4
!      8 0 0 0 0 0 4
!      8 0 0 0 0 0 4
!      1 2 2 2 2 2 3
!
!  Modified:
!
!    08 June 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer BC_TAG, a code which is nonzero at every boundary node,
!    and which indicates the type of boundary conditions at that node.
!    The code used can vary from problem to problem.
!
!    Output, real NORMAL(2), the X and Y components of the normal
!    vector to the boundary at the given point.  The normal vector should
!    point out of the region.
!
!    Output, real TANGENT(2), the X and Y components of the
!    tangential vector to the boundary, at the given point.  The tangent
!    vector should point in the counter-clockwise direction, relative to
!    a point within the interior of the region.
!
  implicit none

  integer bc_tag
  real ( kind = 8 ) normal(2)
  real ( kind = 8 ) tangent(2)

  if ( bc_tag == 1 ) then

    normal(1) =   0.0D+00
    normal(2) = - 1.0D+00

    tangent(1) =   1.0D+00
    tangent(2) =   0.0D+00

  else if ( bc_tag == 2 ) then

    normal(1) =   0.0D+00
    normal(2) = - 1.0D+00

    tangent(1) =   1.0D+00
    tangent(2) =   0.0D+00

  else if ( bc_tag == 3 ) then

    normal(1) =   0.0D+00
    normal(2) = - 1.0D+00

    tangent(1) =   1.0D+00
    tangent(2) =   0.0D+00

  else if ( bc_tag == 4 ) then

    normal(1) = 1.0D+00
    normal(2) = 0.0D+00

    tangent(1) =   0.0D+00
    tangent(2) =   1.0D+00

  else if ( bc_tag == 5 ) then

    normal(1) = 0.0D+00
    normal(2) = 1.0D+00

    tangent(1) = - 1.0D+00
    tangent(2) =   0.0D+00

  else if ( bc_tag == 6 ) then

    normal(1) = 0.0D+00
    normal(2) = 1.0D+00

    tangent(1) = - 1.0D+00
    tangent(2) =   0.0D+00

  else if ( bc_tag == 7 ) then

    normal(1) = 0.0D+00
    normal(2) = 1.0D+00

    tangent(1) = - 1.0D+00
    tangent(2) =   0.0D+00

  else if ( bc_tag == 8 ) then

    normal(1) = - 1.0D+00
    normal(2) =   0.0D+00

    tangent(1) =   0.0D+00
    tangent(2) = - 1.0D+00

  else

    normal(1) = 0.0D+00
    normal(2) = 0.0D+00

    tangent(1) = 0.0D+00
    tangent(2) = 0.0D+00

  end if

  return
end
subroutine boundary_shape_freeslip ( bc_tag, normal, tangent )

!*****************************************************************************80
!
!! BOUNDARY_SHAPE_FREESLIP returns the boundary of the freeslip problem.
!
!  Discussion:
!
!    12 March 1999:
!    I tried using the "diagonal" normals and tangents in the corners,
!    but that just caused the code to compute nonzero velocities there
!    that didn't make sense.  So I'm just extending the flatness.
!
!    Here is a schematic of the values of BC_TAG along the boundary
!    for this problem:
!
!    7 6 6 6 6 6 5
!    8 0 0 0 0 0 4
!    8 0 0 0 0 0 4
!    8 0 0 0 0 0 4
!    1 2 2 2 2 2 3
!
!  Modified:
!
!    08 June 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer BC_TAG, a code which is nonzero at every boundary node,
!    and which indicates the type of boundary conditions at that node.
!    The code used can vary from problem to problem.
!
!    Output, real NORMAL(2), the X and Y components of the normal
!    vector to the boundary at the given point.  The normal vector should
!    point out of the region.
!
!    Output, real TANGENT(2), the X and Y components of the
!    tangential vector to the boundary, at the given point.  The tangent
!    vector should point in the counter-clockwise direction, relative to
!    a point within the interior of the region.
!
  implicit none

  integer bc_tag
  real ( kind = 8 ) normal(2)
  real ( kind = 8 ) tangent(2)

  if ( bc_tag == 1 ) then

    normal(1) =   0.0D+00
    normal(2) = - 1.0D+00

    tangent(1) =   1.0D+00
    tangent(2) =   0.0D+00

  else if ( bc_tag == 2 ) then

    normal(1) =   0.0D+00
    normal(2) = - 1.0D+00

    tangent(1) =   1.0D+00
    tangent(2) =   0.0D+00

  else if ( bc_tag == 3 ) then

    normal(1) =   0.0D+00
    normal(2) = - 1.0D+00

    tangent(1) =   1.0D+00
    tangent(2) =   0.0D+00

  else if ( bc_tag == 4 ) then

    normal(1) = 1.0D+00
    normal(2) = 0.0D+00

    tangent(1) =   0.0D+00
    tangent(2) =   1.0D+00

  else if ( bc_tag == 5 ) then

    normal(1) = 0.0D+00
    normal(2) = 1.0D+00

    tangent(1) = - 1.0D+00
    tangent(2) =   0.0D+00

  else if ( bc_tag == 6 ) then

    normal(1) = 0.0D+00
    normal(2) = 1.0D+00

    tangent(1) = - 1.0D+00
    tangent(2) =   0.0D+00

  else if ( bc_tag == 7 ) then

    normal(1) = 0.0D+00
    normal(2) = 1.0D+00

    tangent(1) = - 1.0D+00
    tangent(2) =   0.0D+00

  else if ( bc_tag == 8 ) then

    normal(1) = - 1.0D+00
    normal(2) =   0.0D+00

    tangent(1) =   0.0D+00
    tangent(2) = - 1.0D+00

  else

    normal(1) = 0.0D+00
    normal(2) = 0.0D+00

    tangent(1) = 0.0D+00
    tangent(2) = 0.0D+00

  end if

  return
end
subroutine boundary_shape_step ( bc_tag, normal, tangent )

!*****************************************************************************80
!
!! BOUNDARY_SHAPE_STEP returns the boundary of the step problem.
!
!  Discussion:
!
!    Here is a schematic of the values of BC_TAG along the boundary
!    for this problem:
!
!  15 14 14 14 14 14 14 14 14 13
!  16  0  0  0  0  0  0  0  0 12
!  16  0  0  0  0  0  0  0  0 12
!  16  0  0  0  0  0  0  0  0 12
!  16  0  0  5  6  6  7  0  0 12
!  16  0  0  4 -1 -1  8  0  0 12
!  16  0  0  4 -1 -1  8  0  0 12
!   1  2  2  3 -1 -1  9 10 10 11
!
!  Modified:
!
!    08 June 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer BC_TAG, a code which is nonzero at every boundary node,
!    and which indicates the type of boundary conditions at that node.
!    The code used can vary from problem to problem.
!
!    Output, real NORMAL(2), the X and Y components of the normal
!    vector to the boundary at the given point.  The normal vector should
!    point out of the region.
!
!    Output, real TANGENT(2), the X and Y components of the
!    tangential vector to the boundary, at the given point.  The tangent
!    vector should point in the counter-clockwise direction, relative to
!    a point within the interior of the region.
!
  implicit none

  integer bc_tag
  real ( kind = 8 ) normal(2)
  real ( kind = 8 ) tangent(2)

  if ( bc_tag == 1 ) then

    normal(1) =   0.0D+00
    normal(2) = - 1.0D+00

    tangent(1) =   1.0D+00
    tangent(2) =   0.0D+00

  else if ( bc_tag == 2 ) then

    normal(1) =   0.0D+00
    normal(2) = - 1.0D+00

    tangent(1) =   1.0D+00
    tangent(2) =   0.0D+00

  else if ( bc_tag == 3 ) then

    normal(1) =   sqrt ( 2.0D+00 ) / 2.0D+00
    normal(2) = - sqrt ( 2.0D+00 ) / 2.0D+00

    tangent(1) =   sqrt ( 2.0D+00 ) / 2.0D+00
    tangent(2) =   sqrt ( 2.0D+00 ) / 2.0D+00

  else if ( bc_tag == 4 ) then

    normal(1) = 1.0D+00
    normal(2) = 0.0D+00

    tangent(1) =   0.0D+00
    tangent(2) =   1.0D+00

  else if ( bc_tag == 5 ) then

    normal(1) =   sqrt ( 2.0D+00 ) / 2.0D+00
    normal(2) = - sqrt ( 2.0D+00 ) / 2.0D+00

    tangent(1) =   sqrt ( 2.0D+00 ) / 2.0D+00
    tangent(2) =   sqrt ( 2.0D+00 ) / 2.0D+00

  else if ( bc_tag == 6 ) then

    normal(1) =   0.0D+00
    normal(2) = - 1.0D+00

    tangent(1) =   1.0D+00
    tangent(2) =   0.0D+00

  else if ( bc_tag == 7 ) then

    normal(1) = - sqrt ( 2.0D+00 ) / 2.0D+00
    normal(2) = - sqrt ( 2.0D+00 ) / 2.0D+00

    tangent(1) =   sqrt ( 2.0D+00 ) / 2.0D+00
    tangent(2) = - sqrt ( 2.0D+00 ) / 2.0D+00

  else if ( bc_tag == 8 ) then

    normal(1) = - 1.0D+00
    normal(2) =   0.0D+00

    tangent(1) =   0.0D+00
    tangent(2) = - 1.0D+00

  else if ( bc_tag == 9 ) then

    normal(1) = - sqrt ( 2.0D+00 ) / 2.0D+00
    normal(2) = - sqrt ( 2.0D+00 ) / 2.0D+00

    tangent(1) =   sqrt ( 2.0D+00 ) / 2.0D+00
    tangent(2) = - sqrt ( 2.0D+00 ) / 2.0D+00

  else if ( bc_tag == 10 ) then

    normal(1) =   0.0D+00
    normal(2) = - 1.0D+00

    tangent(1) =   1.0D+00
    tangent(2) =   0.0D+00

  else if ( bc_tag == 11 ) then

    normal(1) =   0.0D+00
    normal(2) = - 1.0D+00

    tangent(1) =   1.0D+00
    tangent(2) =   0.0D+00

  else if ( bc_tag == 12 ) then

    normal(1) = 1.0D+00
    normal(2) = 0.0D+00

    tangent(1) =   0.0D+00
    tangent(2) =   1.0D+00

  else if ( bc_tag == 13 ) then

    normal(1) = 0.0D+00
    normal(2) = 1.0D+00

    tangent(1) = - 1.0D+00
    tangent(2) =   0.0D+00

  else if ( bc_tag == 14 ) then

    normal(1) = 0.0D+00
    normal(2) = 1.0D+00

    tangent(1) = - 1.0D+00
    tangent(2) =   0.0D+00

  else if ( bc_tag == 15 ) then

    normal(1) = 0.0D+00
    normal(2) = 1.0D+00

    tangent(1) = - 1.0D+00
    tangent(2) =   0.0D+00

  else if ( bc_tag == 16 ) then

    normal(1) = - 1.0D+00
    normal(2) =   0.0D+00

    tangent(1) =   0.0D+00
    tangent(2) = - 1.0D+00

  else

    normal(1) = 0.0D+00
    normal(2) = 0.0D+00

    tangent(1) = 0.0D+00
    tangent(2) = 0.0D+00

  end if

  return
end
subroutine boundary_value ( bc_tag, pbc, region, region_ymax, ubc, vbc, y )

!*****************************************************************************80
!
!! BOUNDARY_VALUE evaluates boundary condition functions.
!
!  Modified:
!
!    11 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer BC_TAG, the value of the boundary condition "tag" 
!    for the node.  The meaning of this tag depends on the problem
!    being solved.
!
!    Output, real PBC, the value specified for pressure, at 
!    the point (X,Y), for the problem specified by REGION.  If no boundary
!    condition is imposed for the particular variable, location and
!    problem, simply set the quantity to zero.
!
!    Input, character ( len = 20 ) REGION, the flow problem.
!
!    Output, real UBC, VBC, the values specified
!    for horizontal and vertical velocity, at the point
!    (X,Y), for the problem specified by REGION.  If no boundary
!    condition is imposed for the particular variable, location and
!    problem, simply set the quantity to zero.
!
!    Input, real Y, the coordinates of the node.  
!
  implicit none

  integer bc_tag
  real ( kind = 8 ) pbc
  character ( len = * ) region
  real ( kind = 8 ) region_ymax
  real ( kind = 8 ) ubc
  real ( kind = 8 ) vbc
  real ( kind = 8 ) y

  pbc = 0.0D+00
  ubc = 0.0D+00
  vbc = 0.0D+00
!
!  CAVITY
!
!  7 6 6 6 6 6 5
!  8 0 0 0 0 0 4
!  8 0 0 0 0 0 4
!  8 0 0 0 0 0 4
!  1 2 2 2 2 2 3
!
  if ( region == 'CAVITY' ) then

    if ( bc_tag == 6 ) then
      ubc = 1.0D+00
    end if
!
!  CHANNEL
!
!  7 6 6 6 6 6 5
!  8 0 0 0 0 0 4
!  8 0 0 0 0 0 4
!  8 0 0 0 0 0 4
!  1 2 2 2 2 2 3
!
  else if ( region == 'CHANNEL' ) then

    if ( bc_tag == 4 ) then

      vbc = 0.0D+00

    else if ( bc_tag == 5 ) then

      pbc = 100.0D+00

    else if ( bc_tag == 8 ) then

      ubc = y * ( region_ymax - y )
      vbc = 0.0D+00

    end if
!
!  FREESLIP
!
!  7 6 6 6 6 6 5
!  8 0 0 0 0 0 4
!  8 0 0 0 0 0 4
!  8 0 0 0 0 0 4
!  1 2 2 2 2 2 3
!
  else if ( region == 'FREESLIP' ) then

    if ( bc_tag == 4 ) then

      vbc = 0.0D+00

    else if ( bc_tag == 8 ) then

      ubc = y * ( region_ymax - y )
      vbc = 0.0D+00

    end if
!
!  STEP
!
!  15 14 14 14 14 14 14 14 14 13
!  16  0  0  0  0  0  0  0  0 12
!  16  0  0  0  0  0  0  0  0 12
!  16  0  0  0  0  0  0  0  0 12
!  16  0  0  5  6  6  7  0  0 12
!  16  0  0  4 -1 -1  8  0  0 12
!  16  0  0  4 -1 -1  8  0  0 12
!   1  2  2  3 -1 -1  9 10 10 11
!
  else if ( region == 'STEP' ) then

    if ( bc_tag == 12 ) then

      vbc = 0.0D+00

    else if ( bc_tag == 16 ) then

      ubc = y * ( region_ymax - y )
      vbc = 0.0D+00

    end if

  else

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'BOUNDARY_VALUE - Fatal error!'
    write ( *, '(a)' ) '  Unrecognized region!'
    stop

  end if

  return
end
subroutine ch_cap ( c )

!*****************************************************************************80
!
!! CH_CAP capitalizes a single character.
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
  integer itemp

  itemp = ichar ( c )

  if ( 97 <= itemp .and. itemp <= 122 ) then
    c = char ( itemp - 32 )
  end if

  return
end
subroutine coef_read ( file_name, g, ierror, maxeqn, neqn, nx, ny, region, &
  nu_inv )

!*****************************************************************************80
!
!! COEF_READ reads the coefficient data from a file.
!
!  Modified:
!
!    29 June 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) FILE_NAME, the name of the file to read.
!
!    Output, real G(MAXEQN), the current solution vector.
!
!    Output, integer IERROR, error flag.
!    0, no error occurred.
!    nonzero, an error occurred.
!
!    Input, integer MAXEQN, the maximum number of equations.
!
!    Output, integer NEQN, the number of equations.
!
!    Output, integer NX, NY, the number of elements along a horizontal, or
!    vertical line.
!
!    Output, character ( len = 20 ) REGION, the flow problem.
!
!    Output, real NU_INV, the inverse dynamic viscosity.
!
  implicit none

  integer maxeqn

  character ( len = * ) file_name
  real ( kind = 8 ) g(maxeqn)
  integer i
  integer ierror
  integer ios
  integer iunit
  integer neqn
  integer nx
  integer ny
  character ( len = * ) region
  real ( kind = 8 ) nu_inv

  ierror = 0

  neqn = 0
  nx = 0
  ny = 0
  region = ' '
  nu_inv = 0.0D+00

  call get_unit ( iunit )

  open ( unit = iunit, file = file_name, status = 'old', &
    iostat = ios )

  if ( ios /= 0 ) then
    ierror = ios
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'COEF_READ - Warning!'
    write ( *, '(a)' ) '  Could not open the file:'
    write ( *, '(a)' ) trim ( file_name )
    return
  end if

  do i = 1, 10

    read ( iunit, '(1x)', iostat = ios )

    if ( ios /= 0 ) then
      ierror = ios
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'COEF_READ - Warning!'
      write ( *, '(a)' ) '  Read error!'
      return
    end if

  end do

  read ( iunit, *, iostat = ios ) neqn

  if ( ios /= 0 ) then
    ierror = ios
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'COEF_READ - Warning!'
    write ( *, '(a)' ) '  Read error!'
    return
  end if

  read ( iunit, *, iostat = ios ) nx

  if ( ios /= 0 ) then
    ierror = ios
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'COEF_READ - Warning!'
    write ( *, '(a)' ) '  Read error!'
    return
  end if

  read ( iunit, *, iostat = ios ) ny

  if ( ios /= 0 ) then
    ierror = ios
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'COEF_READ - Warning!'
    write ( *, '(a)' ) '  Read error!'
    return
  end if

  read ( iunit, '(a)', iostat = ios ) region

  if ( ios /= 0 ) then
    ierror = ios
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'COEF_READ - Warning!'
    write ( *, '(a)' ) '  Read error!'
    return
  end if

  read ( iunit, *, iostat = ios ) nu_inv

  if ( ios /= 0 ) then
    ierror = ios
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'COEF_READ - Warning!'
    write ( *, '(a)' ) '  Read error!'
    return
  end if

  read ( iunit, '(1x)', iostat = ios )

  if ( ios /= 0 ) then
    ierror = ios
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'COEF_READ - Warning!'
    write ( *, '(a)' ) '  Read error!'
    return
  end if

  if ( maxeqn < neqn ) then
    ierror = 3
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'COEF_READ - Warning!'
    write ( *, '(a,i8)' ) '  Input NEQN = ', neqn
    write ( *, '(a,i8)' ) '  Internal MAXEQN = ', maxeqn
    close ( unit = iunit )
    return
  end if

  do i = 1, neqn

    read ( iunit, *, iostat = ios ) g(i)

    if ( ios /= 0 ) then
      ierror = ios
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'COEF_READ - Warning!'
      write ( *, '(a)' ) '  Read error!'
      return
    end if

  end do

  close ( unit = iunit )

  return
end
subroutine coef_write ( file_name, g, ierror, maxeqn, neqn, nx, ny, region,  &
  nu_inv )

!*****************************************************************************80
!
!! COEF_WRITE writes the coefficient data to a file for possible restart.
!
!  Modified:
!
!    29 June 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) FILE_NAME, the name of the file to write.
!
!    Input, real G(MAXEQN), the current solution vector.
!
!    Output, integer IERROR, error flag.
!    0, no error occurred.
!    nonzero, an error occurred.
!
!    Input, integer MAXEQN, the maximum number of equations.
!
!    Input, integer NEQN, the number of equations.
!
!    Input, integer NX, NY, the number of elements along a horizontal, or
!    vertical line.
!
!    Input, character ( len = 20 ) REGION, the flow problem.
!
!    Input, real NU_INV, the inverse dynamic viscosity.
!
  implicit none

  integer maxeqn

  character ( len = * ) file_name
  real ( kind = 8 ) g(maxeqn)
  integer i
  integer ierror
  integer ios
  integer iunit
  integer neqn
  integer nx
  integer ny
  character ( len = * ) region
  real ( kind = 8 ) nu_inv

  ierror = 0

  call get_unit ( iunit )

  open ( unit = iunit, file = file_name, status = 'replace', &
    iostat = ios )

  if ( ios /= 0 ) then
    ierror = ios
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'COEF_WRITE - Warning!'
    write ( *, '(a)' ) '  Could not open the file.'
    return
  end if

  write ( iunit, '(a)' ) '#  g.txt  FLOW7 solution vector'
  write ( iunit, '(a)' ) '#  '
  write ( iunit, '(a)' ) '#  NEQN'
  write ( iunit, '(a)' ) '#  NX'
  write ( iunit, '(a)' ) '#  NY'
  write ( iunit, '(a)' ) '#  REGION'
  write ( iunit, '(a)' ) '#  NU_INV'
  write ( iunit, '(a)' ) '#  '
  write ( iunit, '(a)' ) '#  (G(I),I=1,NEQN)'
  write ( iunit, '(a)' ) '#'

  write ( iunit, '(i8)' ) neqn
  write ( iunit, '(i8)' ) nx
  write ( iunit, '(i8)' ) ny
  write ( iunit, '(a)' ) trim ( region )
  write ( iunit, '(g14.6)' ) nu_inv
  write ( iunit, '(1x)' )

  do i = 1, neqn
    write ( iunit, '(g14.6)' ) g(i)
  end do

  close ( unit = iunit )
  
  return
end     
subroutine element_print ( maxelm, nelem, node )

!*****************************************************************************80
!
!! ELEMENT_PRINT prints out the elements.
!
!  Modified:
!
!    07 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer MAXELM, the maximum number of elements.
!
!    Input, integer NELEM, the number of elements.
!
!    Input, integer NODE(6,MAXELM), the nodes that make up each element.
!
  implicit none

  integer maxelm

  integer i
  integer j
  integer nelem
  integer node(6,maxelm)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ELEMENT_PRINT -'
  write ( *, '(a)' ) '  Decomposition of region into elements:'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Element  Nodes'
  write ( *, '(a)' ) ' '
  do i = 1, nelem
    write ( *, '(i8,6x,6i8)' ) i, ( node(j,i), j = 1, 6 )
  end do

  return
end
subroutine element_write ( file_name, ierror, maxelm, nelem, node )

!*****************************************************************************80
!
!! ELEMENT_WRITE writes element information to a file.
!
!  Discussion:
!
!    The data may be used to plot the finite element mesh and solution.
!
!  Modified:
!
!    29 June 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer IERROR, error flag.
!    0, no error occurred.
!    nonzero, an error occurred.
!
!    Input, integer MAXELM, the maximum number of elements.
!
!    Input, integer NELEM, the number of elements.
!
!    Input, integer NODE(6,MAXELM), the nodes that make up each element.
!
  implicit none

  integer maxelm

  character ( len = * ) file_name
  integer i
  integer ielem
  integer ierror
  integer ios
  integer iunit
  integer nelem
  integer node(6,maxelm)
  integer npe

  ierror = 0

  call get_unit ( iunit )

  open ( unit = iunit, file = file_name, status = 'replace', &
    iostat = ios )

  if ( ios /= 0 ) then
    ierror = ios
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'ELEMENT_WRITE - Warning!'
    write ( *, '(a)' ) '  Could not open the file!'
    return
  end if

  npe = 6

  write ( iunit, * ) nelem
  write ( iunit, * ) npe
  do ielem = 1, nelem
    write ( iunit, * ) node(1:npe,ielem)
  end do

  close ( unit = iunit )

  return
end
subroutine eqn_nabor_print ( eqn_nabor, eqn_nabor_max, eqn_nabor_num, neqn )

!*****************************************************************************80
!
!! EQN_NABOR_PRINT prints the equation neighbor array.
!
!  Modified:
!
!    18 May 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer EQN_NABOR(EQN_NABOR_MAX,NEQN), the equation neigbors.
!
!    Input, integer EQN_NABOR_MAX, the maximum number of equation neighbors.
!
!    Input, integer EQN_NABOR_NUM(NEQN), the number of equation neighbors.
!
!    Input, integer NEQN, the number of equations and variables.
!
  implicit none

  integer neqn
  integer eqn_nabor_max

  integer i
  integer j
  integer jhi
  integer jlo
  integer eqn_nabor(eqn_nabor_max,neqn)
  integer eqn_nabor_num(neqn)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Equation Neighbor Array'
  write ( *, '(a)' ) ' '
  do i = 1, neqn
    do jlo = 1, eqn_nabor_num(i), 19
      jhi = min ( jlo + 18, eqn_nabor_num(i) )
      if ( jlo == 1 ) then
        write ( *, '(i4,(19i4))' ) i, ( eqn_nabor(j,i), j = jlo, jhi )
      else
        write ( *, '(4x,(19i4))' )    ( eqn_nabor(j,i), j = jlo, jhi )
      end if
    end do
  end do

  return
end
subroutine eqn_nabor_set ( eqn_nabor, eqn_nabor_max, eqn_nabor_num, indx, &
  neqn, np, node_nabor, node_nabor_max, node_nabor_num )

!*****************************************************************************80
!
!! EQN_NABOR_SET sets up the equation neighbor array.
!
!  Discussion:
!
!    The equation neighbor array allows us to quickly find out, for each 
!    equation or variable, the equations or variables that are its "neighbors".
!    Equations I and J are neighbors if variable I influences equation J or
!    variable J influences equation I.
!
!    For our grid, there should be at most 45 equations or variables in
!    each neighborhood. 
!
!  Modified:
!
!    17 May 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer NP, the number of nodes.
!
!    Input, integer NODE_NABOR(NABOR_MAX,NP), the node neigbors.
!
!    Input, integer NODE_NABOR_MAX, the maximum number of node neighbors.
!
!    Input, integer NODE_NABOR_NUM(NP), the number of node neighbors.
!
  implicit none

  integer eqn_nabor_max
  integer node_nabor_max
  integer neqn
  integer np

  integer elem
  integer eqn_nabor(eqn_nabor_max,neqn)
  integer eqn_nabor_num(neqn)
  integer i
  integer idof
  integer ieqn
  integer indx(3,np)
  integer ip
  integer jp
  integer k
  integer nab(3)
  integer nb
  integer node_nabor(node_nabor_max,np)
  integer node_nabor_num(np)
!
!  Initialize.
!
  eqn_nabor(1:eqn_nabor_max,1:neqn) = 0

  do ieqn = 1, neqn
    eqn_nabor(1,ieqn) = ieqn
  end do

  eqn_nabor_num(1:neqn) = 1
!
!  For each node...
!
  do ip = 1, np
!
!  For each node neighbor...
!
    do k = 1, node_nabor_num(ip)

      jp = node_nabor(k,ip)

      nb = 1
      nab(nb) = indx(1,jp)
      nb = 2
      nab(nb) = indx(2,jp)
      if ( indx(3,jp) /= 0 ) then
        nb = 3
        nab(nb) = indx(3,jp)
      end if
!
!  For each possible degree of freedom...
!
      do idof = 1, 3

        ieqn = indx(idof,ip)

        if ( ieqn /= 0 ) then

          call i4vec_merge_a ( eqn_nabor_num(ieqn), eqn_nabor(1,ieqn), &
                            nb, nab, &
                            eqn_nabor_num(ieqn), eqn_nabor(1,ieqn) )

          if ( eqn_nabor_max < eqn_nabor_num(ieqn) ) then
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) 'EQN_NABOR_SET - Fatal error!'
            write ( *, '(a)' ) '  Number of EQN neighbors exceeded.'
            write ( *, '(a,i8)' ) '  EQN_NABOR_MAX = ', eqn_nabor_max
            write ( *, '(a,i8)' ) &
              '  EQN_NABOR_NUM(IEQN) = ', eqn_nabor_num(ieqn)
            write ( *, '(a,i8)' ) '  IEQN = ', ieqn
            stop
          end if

        end if

      end do

    end do

  end do

  return
end
subroutine equation_print ( eqn, indx, neqn, np )

!*****************************************************************************80
!
!! EQUATION_PRINT prints the equation types.
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
!    Input, character ( len = 3 ) EQN(NEQN), the type of each equation.
!
!    Input, integer INDX(3,NP), mapping from nodes to degrees of freedom.
!
!    Input, integer NEQN, the number of finite element equations.
!
!    Input, integer NP, the number of nodes.
!
  implicit none

  integer neqn
  integer np

  character ( len = 3 ) eqn(neqn)
  integer indx(3,np)
  integer ip

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'EQUATION_PRINT:'
  write ( *, '(a,i8)' ) '  Number of unknowns/equations = ', neqn
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Node     U      V      P'
  write ( *, '(a)' ) ' '

  do ip = 1, np
 
    if ( indx(3,ip) /= 0 ) then
      write ( *, '(i8,2x,i8,2x,a3,2x,i8,2x,a3,i8,2x,a3)' ) ip, &
        indx(1,ip), eqn(indx(1,ip)), &
        indx(2,ip), eqn(indx(2,ip)), &
        indx(3,ip), eqn(indx(3,ip)) 
    else
      write ( *, '(i8,2x,i8,2x,a3,2x,i8,2x,a3,i8,2x,a3)' ) ip, &
        indx(1,ip), eqn(indx(1,ip)), &
        indx(2,ip), eqn(indx(2,ip))
    end if

  end do
 
  return
end
subroutine fp ( a, bc_tag, detmap, eqn, g, indx, maxelm, maxeqn, maxnp, &
  maxquad1, maxquad2, maxside, navier, ncol, nelem, neqn, nlband, node, &
  np, nquad1, nquad2, nrow, nside, penalty1, penalty2, phi, region, nu_inv, &
  side_basis, side_elem, side_eqn, side_etam, side_etap, side_indx, &
  side_xsim, side_xsip, solver, squad1, wquad1, wquad2, xc, yc )

!***********************************************************************
!
!! FP computes the jacobian matrix A of the Navier Stokes residual. 
!
!  Discussion:
!
!    FP differentiates the equations computed in FX.
!
!    The differentiated Navier Stokes equations have the form:
!
!    d UM-Eqn(I) / d U-Coef(J) = 
!
!      Integral dW(J)/dx * dW(I)/dx + dW(J)/dy * dW(I)/dy 
!      + nu_inv * ( W(J) * dUold/dx + Uold * dW(J) /dx + Vold * dW(J)/dy ) 
!      * W(I) dx dy
!
!    d UM-Eqn(I) / d V-Coef(J) = 
!
!      Integral nu_inv * W(J) * dUold/dy * W(I) dx dy
!
!    d UM-Eqn(I) / d P-Coef(J) = 
!
!      Integral nu_inv * dQ(J)/dx * W(I) dx dy
!
!    d VM-Eqn(I) / d U-Coef(J) = 
!
!      Integral nu_inv * W(J) * dVold/dx * W(I) dx dy
!
!    d VM-Eqn(I) / d V-Coef(J) = 
!
!      Integral dW(J)/dx * dW(I)/dx + dW(J)/dy * dW(I)/dy 
!      + nu_inv * ( Uold * dW(J)/dx + W(J) * dVold/dy + Vold * dW(J)/dy ) 
!      * W(I) dx dy
!
!    d VM-Eqn(I) / d P-Coef(J) = 
!
!      Integral nu_inv * dQ(J)/dy * W(I) dx dy
!
!    d PC-Eqn(I) / d U-Coef(J) = 
!
!      Integral dW(J)/dx * Q(I) dx dy
!
!    d PC-Eqn(I) / d V-Coef(J) = 
!
!      Integral dW(J)/dy * Q(I) dx dy
!
!    The boundary conditions must also be differentiated.
!
!  Modified:
!
!    11 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real A(NROW,MAXEQN), the banded jacobian.
!
!    real DETMAP(MAXQUAD2,MAXELM), the reference map determinant.
!
!    Input, character ( len = 3 ) EQN(MAXEQN), the type of each equation.
!
!    Input, real G(MAXEQN), the current solution vector.
!
!    Input, integer INDX(3,MAXNP), mapping from nodes to degrees of freedom.
!
!    Input, integer MAXELM, the maximum number of elements.
!
!    Input, integer MAXNP, the maximum number of nodes.
!
!    Input, integer NELEM, the number of elements.
! 
!    Input, integer NEQN, the number of finite element equations.
! 
!    Input, integer NLBAND, the lower bandwidth of the matrix A.  
! 
!    Input, integer NODE(6,MAXELM), the nodes that make up each element.
! 
!    Input, integer NP, the number of nodes.
! 
!    Input, integer NROW, the number of rows need to store the matrix A.
!
!    Input, real PHI(MAXQUAD2,6,6,MAXELM), basis function values.  
!
!    Input, character ( len = 20 ) REGION, the flow problem.
!
!    Input, real NU_INV, the value of the inverse viscosity.
!
  implicit none

  integer maxelm
  integer maxeqn
  integer maxnp
  integer maxquad1
  integer maxquad2
  integer maxside
  integer ncol
  integer nrow

  real ( kind = 8 ) a(nrow,ncol)
  integer bc_tag(maxnp)
  integer col
  real ( kind = 8 ) detmap(maxquad2,maxelm)
  real ( kind = 8 ) dpdx
  real ( kind = 8 ) dpdy
  real ( kind = 8 ) dqjdr
  real ( kind = 8 ) dqjdx
  real ( kind = 8 ) dqjdy
  real ( kind = 8 ) dtdr
  real ( kind = 8 ) dtds
  real ( kind = 8 ) dudr
  real ( kind = 8 ) duds
  real ( kind = 8 ) dudx
  real ( kind = 8 ) dudy
  real ( kind = 8 ) dvdr
  real ( kind = 8 ) dvds
  real ( kind = 8 ) dvdx
  real ( kind = 8 ) dvdy
  real ( kind = 8 ) dwideta
  real ( kind = 8 ) dwidxsi
  real ( kind = 8 ) dwidr
  real ( kind = 8 ) dwids
  real ( kind = 8 ) dwidx
  real ( kind = 8 ) dwidy
  real ( kind = 8 ) dwjdeta
  real ( kind = 8 ) dwjdxsi
  real ( kind = 8 ) dwjdr
  real ( kind = 8 ) dwjds
  real ( kind = 8 ) dwjdx
  real ( kind = 8 ) dwjdy
  character ( len = 3 ) eqn(maxeqn)
  real ( kind = 8 ) eta
  real ( kind = 8 ) etam
  real ( kind = 8 ) etap
  real ( kind = 8 ) g(maxeqn)
  integer i
  integer idof
  integer ielem
  integer ieqn
  integer ihor
  integer indx(3,maxnp)
  integer ip
  integer iprs
  integer iq
  integer iquad1
  integer iquad2
  integer iside
  integer iver
  integer j
  integer jhor
  integer jp
  integer jprs
  integer jq
  integer jver
  real ( kind = 8 ) length
  real ( kind = 8 ) n
  real ( kind = 8 ) navier
  integer nelem
  integer neqn
  integer nlband
  integer node(6,maxelm)
  real ( kind = 8 ) normal(2)
  integer np
  integer nquad1
  integer nquad2
  integer nside
  real ( kind = 8 ) p
  real ( kind = 8 ) penalty1
  real ( kind = 8 ) penalty2
  real ( kind = 8 ) phi(maxquad2,6,6,maxelm)
  real ( kind = 8 ) qi
  character ( len = 20 ) region
  real ( kind = 8 ) nu_inv
  integer row
  real ( kind = 8 ) s
  integer side_basis(3,maxside)
  integer side_elem(maxside)
  character ( len = 3 ) side_eqn(maxside)
  real ( kind = 8 ) side_etam(maxside)
  real ( kind = 8 ) side_etap(maxside)
  integer side_indx(3,maxside)
  real ( kind = 8 ) side_xsim(maxside)
  real ( kind = 8 ) side_xsip(maxside)
  character ( len = 20 ) solver
  real ( kind = 8 ) squad1(maxquad1)
  real ( kind = 8 ) t
  real ( kind = 8 ) tangent(2)
  real ( kind = 8 ) u
  real ( kind = 8 ) v
  real ( kind = 8 ) weight
  real ( kind = 8 ) wi
  real ( kind = 8 ) wj
  real ( kind = 8 ) wquad1(maxquad1)
  real ( kind = 8 ) wquad2(maxquad2)
  real ( kind = 8 ) x
  real ( kind = 8 ) x1
  real ( kind = 8 ) x2
  real ( kind = 8 ) x3
  real ( kind = 8 ) xc(maxnp)
  real ( kind = 8 ) xsi
  real ( kind = 8 ) xsim
  real ( kind = 8 ) xsip
  real ( kind = 8 ) y
  real ( kind = 8 ) y1
  real ( kind = 8 ) y2
  real ( kind = 8 ) y3
  real ( kind = 8 ) yc(maxnp)
!
!  Zero out the matrix.
!
  a(1:nrow,1:ncol) = 0.0D+00
!
!*****************************************************************************80
!  Handle equations associated with area integrals.
!*****************************************************************************80
!
  do ielem = 1, nelem
!
!  Evaluate the integrand at the quadrature points.
!
    do iquad2 = 1, nquad2
 
      weight = 0.5D+00 * wquad2(iquad2) * detmap(iquad2,ielem)
!
!  For the given quadrature point, evaluate P, U and V.
!
      call uvp_quad_value ( dpdx, dpdy, dudx, dudy, dvdx, dvdy, &
        g, ielem, indx, iquad2, maxelm, maxeqn, maxnp, maxquad2, &
        node, p, phi, u, v )
!
!  Look at all the basis functions in the element IELEM.
!
      do iq = 1, 6
 
        ip = node(iq,ielem)

        wi =    phi(iquad2,iq,1,ielem)
        dwidx = phi(iquad2,iq,2,ielem)
        dwidy = phi(iquad2,iq,3,ielem)
        qi =    phi(iquad2,iq,4,ielem)

        x = xc(ip)
        y = yc(ip)
        call boundary_shape ( bc_tag(ip), normal, region, tangent )

        t     = tangent(1) * u     + tangent(2) * v
        dudr  = tangent(1) * dudx  + tangent(2) * dudy
        dvdr  = tangent(1) * dvdx  + tangent(2) * dvdy
        dwidr = tangent(1) * dwidx + tangent(2) * dwidy

        n     = normal(1)  * u     + normal(2)  * v
        duds  = normal(1)  * dudx  + normal(2)  * dudy
        dvds  = normal(1)  * dvdx  + normal(2)  * dvdy
        dwids = normal(1)  * dwidx + normal(2)  * dwidy

        dtdr  = tangent(1) * dudr  + tangent(2) * dvdr
        dtds  = tangent(1) * duds  + tangent(2) * dvds

        ihor = indx(1,ip)
        iver = indx(2,ip)
        iprs = indx(3,ip)
!
!  Now compute the derivatives of the functions associated
!  with U, V and P, with respect to the coefficients associated
!  with basis vectors at each node of the element.
!
        do jq = 1, 6
 
          jp = node(jq,ielem)
 
          wj =    phi(iquad2,jq,1,ielem)
          dwjdx = phi(iquad2,jq,2,ielem)
          dwjdy = phi(iquad2,jq,3,ielem)

          dwjdr = tangent(1) * dwjdx + tangent(2) * dwjdy
          dwjds = normal(1)  * dwjdx + normal(2)  * dwjdy
 
          dqjdx = phi(iquad2,jq,5,ielem)
          dqjdy = phi(iquad2,jq,6,ielem)

          dqjdr = tangent(1) * dqjdx + tangent(2) * dqjdy
 
          jhor = indx(1,jp)
          jver = indx(2,jp)
          jprs = indx(3,jp)
!
!  Contribution of the JHOR variable to the U equation.
!
          if ( eqn(ihor) == 'UM' ) then

            row = ihor - jhor + 2*nlband + 1
            col = jhor

            a(row,col) = a(row,col) + weight * &
              ( dwjdx * dwidx + dwjdy * dwidy + nu_inv * navier * &
              ( wj * dudx + u * dwjdx + v * dwjdy ) * wi  )

          end if
!
!  Contribution of the JHOR variable to the V equation.
!
          if ( eqn(iver) == 'VM' ) then

            row = iver - jhor + 2*nlband + 1
            col = jhor

            a(row,col) = a(row,col) + weight * nu_inv * navier * dvdx * wj * wi

          else if ( eqn(iver) == 'TMP' ) then

            row = iver - jhor + 2*nlband + 1
            col = jhor

            a(row,col) = a(row,col) + weight * penalty2 * &
              ( tangent(1) * dwjdr * dwidr + tangent(1) * dwjds * dwids &
              + nu_inv * navier * (   normal(1) * wj * dtds &
              + n * tangent(1) * dwjds + tangent(1) * wj * dtdr &
              + t * tangent(1) * dwjdr ) * wi )

          end if
!
!  Contribution of the JHOR variable to the P equation.
!
          if ( 0 < iprs ) then
            if ( eqn(iprs) == 'PC' ) then
              row = iprs - jhor + 2*nlband + 1
              col = jhor
              a(row,col) = a(row,col) + weight * dwjdx * qi
            end if
          end if
!
!  Contribution of the JVER variable to the U equation.
!
          if ( eqn(ihor) == 'UM' ) then
            row = ihor - jver + 2*nlband + 1
            col = jver
            a(row,col) = a(row,col) + weight * nu_inv * navier * wj * dudy * wi

          end if
!
!  Contribution of the JVER variable to the V equation.
!
          if ( eqn(iver) == 'VM' ) then

            row = iver - jver + 2*nlband + 1
            col = jver
            a(row,col) = a(row,col) + weight * ( dwjdx * dwidx &
              + dwjdy * dwidy + nu_inv * navier * &
              ( u * dwjdx + wj * dvdy + v * dwjdy ) * wi )

          else if ( eqn(iver) == 'TMP' ) then

            row = iver - jver + 2*nlband + 1
            col = jver
            a(row,col) = a(row,col) + weight * penalty2 * &
              ( tangent(2) * dwjdr * dwidr + tangent(2) * dwjds * dwids &
              + nu_inv * navier * (   normal(2) * wj * dtds &
              + n * tangent(2) * dwjds + tangent(2) * wj * dtdr &
              + t * tangent(2) * dwjdr ) * wi )

          end if
!
!  Contribution of the JVER variable to the P equation.
!
          if ( 0 < iprs ) then
            if ( eqn(iprs) == 'PC' ) then
              row = iprs - jver + 2*nlband + 1
              col = jver
              a(row,col) = a(row,col) + weight * dwjdy * qi
            end if
          end if
!
!  Contribution of the JPRS variable to the U equation.
!
          if ( 0 < jprs ) then
 
            if ( eqn(ihor) == 'UM' ) then
              row = ihor - jprs + 2*nlband + 1
              col = jprs
              a(row,col) = a(row,col) + weight * nu_inv * dqjdx * wi

            end if
!
!  Contribution of the JPRS variable to the V equation.
!
            if ( eqn(iver) == 'VM' ) then

              row = iver - jprs + 2*nlband + 1
              col = jprs
              a(row,col) = a(row,col) + weight * nu_inv * dqjdy * wi

            else if ( eqn(iver) == 'TMP' ) then

              row = iver - jprs + 2*nlband + 1
              col = jprs
              a(row,col) = a(row,col) + weight * penalty2 * nu_inv * dqjdr * wi

            end if
 
          end if
 
        end do
      end do
    end do
  end do
!
!*****************************************************************************80
!  Handle equations associated with line integrals along the boundary.
!*****************************************************************************80
!
!  For each element side that is along the boundary.
!
  do iside = 1, nside
!
!  If the element side condition is 'FS'...
!
    if ( side_eqn(iside) == 'FS' ) then

      ielem = side_elem(iside)
      etam = side_etam(iside)
      etap = side_etap(iside)
      xsim = side_xsim(iside)
      xsip = side_xsip(iside)
!
!  Rough estimate of the length of the side.
!
      iq = side_basis(1,iside)
      ip = node(iq,ielem)
      x1 = xc(ip)
      y1 = yc(ip)

      iq = side_basis(2,iside)
      ip = node(iq,ielem)
      x2 = xc(ip)
      y2 = yc(ip)

      iq = side_basis(3,iside)
      ip = node(iq,ielem)
      x3 = xc(ip)
      y3 = yc(ip)

      length = sqrt ( ( x2 - x1 )**2 + ( y2 - y1 )**2 ) &
             + sqrt ( ( x3 - x2 )**2 + ( y3 - y2 )**2 )
!
!  KLUDGE.  But it's right, I think.
!
      if ( x3 < x1 ) then
        length = - length
      end if

      do iquad1 = 1, nquad1

        s = squad1(iquad1)
        weight = 0.5D+00 * wquad1(iquad1) * length

        eta = 0.5D+00 * ( ( 1.0D+00 - s ) * etam + ( 1.0D+00 + s ) * etap )
        xsi = 0.5D+00 * ( ( 1.0D+00 - s ) * xsim + ( 1.0D+00 + s ) * xsip )

        call x_of_xsi ( eta, ielem, maxelm, maxnp, node, x, xc, xsi, y, yc )
!
!  There are three basis functions on this portion of the boundary.
!
        do i = 1, 3

          iq = side_basis(i,iside)
          ip = node(iq,ielem)
          idof = side_indx(i,iside)
          ieqn = indx(idof,ip)

          call ref_bf_q6 ( wi, dwideta, dwidxsi, eta, iq, xsi )

          call boundary_shape ( bc_tag(ip), normal, region, tangent )
!
!  There are three basis functions that contribute to the values of
!  U and V on this portion of the boundary.
!
          do j = 1, 3

            jq = side_basis(j,iside)
            jp = node(jq,ielem)
            jhor = indx(1,jp)
            jver = indx(2,jp)

            call ref_bf_q6 ( wj, dwjdeta, dwjdxsi, eta, jq, xsi )

            if ( eqn(ieqn) == 'TMP' ) then

            row = ieqn - jhor + 2*nlband+1
            col = jhor
            a(row,col) = a(row,col) + penalty1 * weight &
              * tangent(1) * ( tangent(1) + tangent(2) ) * wj * wi

            row = ieqn - jver + 2*nlband+1
            col = jver
            a(row,col) = a(row,col) + penalty1 * weight &
              * tangent(2) * ( tangent(1) + tangent(2) ) * wj * wi

            end if

          end do

        end do

      end do

    end if

  end do
!
!*****************************************************************************80
!  Handle conditions associated with nodal values.
!*****************************************************************************80
!
  do ip = 1, np

    ihor = indx(1,ip)
    iver = indx(2,ip)
    iprs = indx(3,ip)

    x = xc(ip)
    y = yc(ip)

    call boundary_shape ( bc_tag(ip), normal, region, tangent )

    if ( eqn(ihor) == 'UI' .or. eqn(ihor) == 'U0' ) then

      row = ihor-ihor+2*nlband+1
      col = ihor
      a(row,col) = 1.0D+00

    else if ( eqn(ihor) == 'N0' ) then

      row = ihor-ihor+2*nlband+1
      col = ihor
      a(row,col) = normal(1)
      row = ihor-iver+2*nlband+1
      col = ihor
      a(row,col) = normal(2)

    end if

    if ( eqn(iver) == 'VI' .or. eqn(iver) == 'V0' ) then

      row = iver-iver+2*nlband+1
      col = iver
      a(row,col) = 1.0D+00

    else if ( eqn(iver) == 'T0' ) then

      row = iver-ihor+2*nlband+1
      col = ihor
      a(row,col) = tangent(1)

      row = iver-iver+2*nlband+1
      col = iver
      a(row,col) = tangent(2)

    end if

    if ( 0 < iprs ) then
      if ( eqn(iprs) == 'PI' .or. eqn(iprs) == 'P0' ) then
        row = iprs-iprs+2*nlband+1
        col = iprs
        a(row,col) = 1.0D+00
      end if
    end if

  end do

  return
end
subroutine fp_band_info_print ( a, nrow, ncol, nlband )

!*****************************************************************************80
!
!! FP_BAND_INFO_PRINT prints information about the banded jacobian.
!
!  Modified:
!
!    17 May 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real A(NROW,NCOL), the banded jacobian.
!
!    Input, integer NROW, the number of rows need to store the matrix A.
!
!    Input, integer NCOL, the number of columns, and the actual number of
!    variables and equations.
!
!    Input, integer NLBAND, the lower bandwidth of the matrix A.  
!
  implicit none

  integer ncol
  integer nrow

  real ( kind = 8 ) a(nrow,ncol)
  integer i
  integer ihi
  integer ilo
  integer irow
  integer j
  integer jhi
  integer jlo
  integer nlband
  real ( kind = 8 ) norm

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'FP_BAND_INFO_PRINT:'
!
!  Row sums.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'Row sums of absolute values:'
  write ( *, '(a)' ) ' '
  do i = 1, ncol
    norm = 0.0D+00
    jlo = max ( 1, i - nlband )
    jhi = min ( ncol, i + nlband )
    do j = jlo, jhi
      norm = norm + abs ( a(i-j+2*nlband+1,j) )
    end do
    write ( *, '(i8,g14.6)' ) i, norm
  end do
!
!  Column sums.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'Column sums of absolute values:'
  write ( *, '(a)' ) ' '
  do j = 1, ncol
    norm = 0.0D+00
    ilo = max ( 1, j - nlband )
    ihi = min ( ncol, j + nlband )
    do i = ilo, ihi
      norm = norm + abs ( a(i-j+2*nlband+1,j) )
    end do
    write ( *, '(i8,g14.6)' ) j, norm
  end do
!
!  Print a particular row.
!
  irow = 160
  if ( 1 <= irow .and. irow <= ncol ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) 'Entries of row ', irow
    write ( *, '(a)' ) ' '
    i = irow
    jlo = max ( 1, i - nlband )
    jhi = min ( ncol, i + nlband )
    do j = jlo, jhi
      if ( a(i-j+2*nlband+1,j) /= 0.0D+00 ) then
        write ( *, '(2i8,g14.6)' ) irow, j, a(i-j+2*nlband+1,j)
      end if
    end do

  end if

  return
end
subroutine fp_band_width ( indx, maxelm, maxnp, nelem, nlband, node, nrow )

!*****************************************************************************80
!
!! FP_BAND_WIDTH computes the lower band width of the Jacobian matrix.
!
!  Discussion:
!
!    The routine also finds NROW, the total number of rows required 
!    to store the matrix in LINPACK general band storage format.
!
!  Modified:
!
!    07 December 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer INDX(3,MAXNP), mapping from nodes to degrees of freedom.
!
!    Input, integer MAXELM, the maximum number of elements.
!
!    Input, integer MAXNP, the maximum number of nodes.
!
!    Input, integer NELEM, the number of elements.
! 
!    Output, integer NLBAND, the lower bandwidth of the matrix A.
! 
!    Input, integer NODE(6,MAXELM), the nodes that make up each element.
! 
!    Input, integer NP, the number of nodes.
! 
!    Output, integer NROW, the number of rows needed to store the matrix A.
!
  implicit none

  integer maxelm
  integer maxnp

  integer i
  integer ielem
  integer indx(3,maxnp)
  integer ip
  integer ipp
  integer iq
  integer iqq
  integer iuk
  integer iukk
  integer j
  integer nelem
  integer nlband
  integer node(6,maxelm)
  integer nrow

  nlband = 0
 
  do ielem = 1, nelem
    do iq = 1, 6
      ip = node(iq,ielem)
      do iuk = 1, 3
        i = indx(iuk,ip)
        if ( 0 < i ) then
          do iqq = 1, 6
            ipp = node(iqq,ielem)
            do iukk = 1, 3
              j = indx(iukk,ipp)
              if ( 0 < j ) then
                if ( nlband < j - i ) then
                  nlband = j - i
                end if
              end if
            end do
          end do
        end if
      end do
    end do
  end do
 
  nrow = 3 * nlband + 1
 
  return
end
subroutine fp_dif ( a, bc_tag, detmap, eqn, g, indx, maxelm, maxeqn, maxnp, &
  maxquad1, maxquad2, maxside, navier, ncol, nelem, neqn, nlband, node, np, &
  nquad1, nquad2, nrow, nside, penalty1, penalty2, phi, region, region_ymax, &
  res, res2, nu_inv, side_basis, side_elem, side_eqn, side_etam, side_etap, &
  side_indx, side_xsim, side_xsip, solver, squad1, wquad1, wquad2, xc, yc )

!*****************************************************************************80
!
!! FP_DIF estimates the jacobian matrix by finite differences.
!
!  Modified:
!
!    19 May 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real A(NROW,MAXEQN), the banded jacobian.
!
!    Input, real DETMAP(MAXQUAD2,MAXELM), the reference map determinant.
!
!    Input, character ( len = 3 ) EQN(MAXEQN), the type of each equation.
!
!    Input, real G(MAXEQN), the current solution vector.
!
!    Input, integer INDX(3,MAXNP), mapping from nodes to degrees of freedom.
!
!    Input, integer MAXELM, the maximum number of elements.
!
!    Input, integer MAXNP, the maximum number of nodes.
!
!    Input, integer NELEM, the number of elements.
!
!    Input, integer NEQN, the number of finite element equations.
!
!    Input, integer NLBAND, the lower bandwidth of the matrix A.
!
!    Input, integer NODE(6,MAXELM), the nodes that make up each element.
!
!    Input, integer NP, the number of nodes.
!
!    Input, integer NROW, the number of rows need to store the matrix A.
!
!    Input, real PHI(MAXQUAD2,6,6,MAXELM), basis function values.
!
!    Input, character ( len = 20 ) REGION, the flow problem.
!
!    Input, real NU_INV, the value of the inverse viscosity.
!
  implicit none

  integer maxelm
  integer maxeqn
  integer maxnp
  integer maxquad1
  integer maxquad2
  integer maxside
  integer ncol
  integer nrow

  real ( kind = 8 ) a(nrow,ncol)
  integer bc_tag(maxnp)
  real ( kind = 8 ) detmap(maxquad2,maxelm)
  real ( kind = 8 ) dg
  real ( kind = 8 ), parameter :: EPS = 0.001D+00
  character ( len = 3 ) eqn(maxeqn)
  real ( kind = 8 ) g(maxeqn)
  real ( kind = 8 ) gsave
  integer i
  integer ieqn
  integer ihi
  integer ilo
  integer indx(3,maxnp)
  integer irow
  integer j
  real ( kind = 8 ) navier
  integer nelem
  integer neqn
  integer nlband
  integer node(6,maxelm)
  integer np
  integer nquad1
  integer nquad2
  integer nside
  real ( kind = 8 ) penalty1
  real ( kind = 8 ) penalty2
  real ( kind = 8 ) phi(maxquad2,6,6,maxelm)
  character ( len = 20 ) region
  real ( kind = 8 ) region_ymax
  real ( kind = 8 ) res(maxeqn)
  real ( kind = 8 ) res2(maxeqn)
  real ( kind = 8 ) nu_inv
  integer side_basis(3,maxside)
  integer side_elem(maxside)
  character ( len = 3 ) side_eqn(maxside)
  real ( kind = 8 ) side_etam(maxside)
  real ( kind = 8 ) side_etap(maxside)
  integer side_indx(3,maxside)
  real ( kind = 8 ) side_xsim(maxside)
  real ( kind = 8 ) side_xsip(maxside)
  character ( len = 20 ) solver
  real ( kind = 8 ) squad1(maxquad1)
  real ( kind = 8 ) wquad1(maxquad1)
  real ( kind = 8 ) wquad2(maxquad2)
  real ( kind = 8 ) xc(maxnp)
  real ( kind = 8 ) yc(maxnp)
!
!  Zero out the matrix.
!
  a(1:nrow,1:ncol) = 0.0D+00
!
!  Evaluate F(X).
!
  call fx ( bc_tag, detmap, eqn, g, indx, maxelm, maxeqn, maxnp, maxquad1, &
    maxquad2, maxside, navier, nelem, neqn, node, np, nquad1, nquad2, nside, &
    penalty1, penalty2, phi, region, region_ymax, res, nu_inv, side_basis, &
    side_elem, side_eqn, side_etam, side_etap, side_indx, side_xsim, &
    side_xsip, squad1, wquad1, wquad2, xc, yc )
!
!  Compare with F(X+deltaX(J))
!
  do j = 1, neqn

    gsave = g(j)
    g(j) = ( 1.0D+00 + EPS ) * g(j) + sign ( EPS, g(j) )
    dg = g(j) - gsave

    call fx ( bc_tag, detmap, eqn, g, indx, maxelm, maxeqn, maxnp, maxquad1, &
      maxquad2, maxside, navier, nelem, neqn, node, np, nquad1, nquad2, nside, &
      penalty1, penalty2, phi, region, region_ymax, res2, nu_inv, side_basis, &
      side_elem, side_eqn,  side_etam, side_etap, side_indx, side_xsim, &
      side_xsip, squad1, wquad1, wquad2, xc, yc )

    ilo = max ( 1, j - nlband )
    ihi = min ( neqn, j + nlband )

    do ieqn = ilo, ihi

      irow = ieqn - j + 2 * nlband + 1

      a(irow,j) = ( res2(ieqn) - res(ieqn) ) / dg

    end do

    g(j) = gsave
 
  end do

  return
end
subroutine fx ( bc_tag, detmap, eqn, g, indx, maxelm, maxeqn, maxnp, &
  maxquad1, maxquad2, maxside, navier, nelem, neqn, node, np, nquad1, &
  nquad2, nside, penalty1, penalty2, phi, region, region_ymax, res, nu_inv, &
  side_basis, side_elem, side_eqn, side_etam, side_etap, side_indx, &
  side_xsim, side_xsip, squad1, wquad1, wquad2, xc, yc )

!*****************************************************************************80
!
!! FX computes the residual of the Navier Stokes equations.
!
!  Discussion:
!
!    By setting the variable NAVIER to 0, you get the residual for
!    the Stokes equations!
!
!
!    The Navier Stokes equations have the form:
!
!    UM-Eqn(I) = 
!
!    Integral (
!      dU/dx * dW(I)/dx + dU/dy * dW(I)/dy +
!      nu_inv * ( U * dU/dx + V * dU/dy + dP/dx ) * W(I) 
!    ) dx dy = 0
!
!    VM-Eqn(I) =
!
!    Integral (
!      dV/dx * dW(I)/dx + dV/dy * dW(I)/dy +
!      nu_inv * ( U * dV/dx + V * dV/dy + dP/dy ) * W(I) 
!    ) dx dy = 0
!
!    PC-Eqn(I) =
!  
!    Integral ( 
!      ( dU/dx + dV/dy ) * Q(I) 
!    ) dx dy = 0
!
!    W is a basis function for U and V, and Q is a basis function for P.
!
!    A penalty term may be included with the momentum equations:
!
!    UMP-Eqn(I) = 
!
!    Integral (
!      dU/dx * dW(I)/dx + dU/dy * dW(I)/dy +
!      (
!        nu_inv * ( U * dU/dx + V * dU/dy + dP/dx ) +
!        ??? / penalty
!      ) * W(I) 
!    ) dx dy = 0
!
!    VMP-Eqn(I) =
!
!    Integral (
!      dV/dx * dW(I)/dx + dV/dy * dW(I)/dy +
!      (
!        nu_inv * ( U * dV/dx + V * dV/dy + dP/dy ) +
!        ??? / penalty
!      ) * W(I) 
!    ) dx dy = 0
!
!    where PENALTY is a user specified nonzero scalar.
!
!  Modified:
!
!    11 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real DETMAP(MAXQUAD2,MAXELM), the reference map determinant.
!
!    Input, character ( len = 3 ) EQN(MAXEQN), the type of each equation.
!
!    Input, real G(MAXEQN), the current solution.
!
!    Input, integer INDX(3,MAXNP), mapping from nodes to degrees of freedom.
!
!    Input, integer MAXELM, the maximum number of elements.
!
!    Input, integer MAXNP, the maximum number of nodes.
!
!    Input, integer NELEM, the number of elements.
! 
!    Input, integer NEQN, the number of finite element equations.
! 
!    Input, integer NODE(6,MAXELM), the nodes that make up each element.
! 
!    Input, integer NP, the number of nodes.
! 
!    Input, real PHI(MAXQUAD2,6,6,MAXELM), basis function values.  
!
!    Input, character ( len = 20 ) REGION, the flow problem.
!
!    Output, real RES(MAXEQN), the residual.
!
!    Input, real NU_INV, the inverse viscosity.
!
!    Input, real XC(MAXNP), the X coordinates of the nodes.
!
!    Input, real YC(MAXNP), the Y coordinates of the nodes.
!
  implicit none

  integer maxelm
  integer maxeqn
  integer maxnp
  integer maxquad1
  integer maxquad2
  integer maxside

  integer bc_tag(maxnp)
  real ( kind = 8 ) detmap(maxquad2,maxelm)
  real ( kind = 8 ) dpdr
  real ( kind = 8 ) dpdx
  real ( kind = 8 ) dpdy
  real ( kind = 8 ) dtdr
  real ( kind = 8 ) dtds
  real ( kind = 8 ) dudr
  real ( kind = 8 ) duds
  real ( kind = 8 ) dudx
  real ( kind = 8 ) dudy
  real ( kind = 8 ) dvdr
  real ( kind = 8 ) dvds
  real ( kind = 8 ) dvdx
  real ( kind = 8 ) dvdy
  real ( kind = 8 ) dwdeta
  real ( kind = 8 ) dwdxsi
  real ( kind = 8 ) dwidr
  real ( kind = 8 ) dwids
  real ( kind = 8 ) dwidx
  real ( kind = 8 ) dwidy
  character ( len = 3 ) eqn(maxeqn)
  real ( kind = 8 ) eta
  real ( kind = 8 ) etam
  real ( kind = 8 ) etap
  real ( kind = 8 ) g(maxeqn)
  integer i
  integer idof
  integer ielem
  integer ieqn
  integer ihor
  integer indx(3,maxnp)
  integer ip
  integer iprs
  integer iq
  integer iquad1
  integer iquad2
  integer iside
  integer iver
  real ( kind = 8 ) length
  real ( kind = 8 ) n
  real ( kind = 8 ) navier
  integer nelem
  integer neqn
  integer node(6,maxelm)
  real ( kind = 8 ) normal(2)
  integer np
  integer nquad1
  integer nquad2
  integer nside
  real ( kind = 8 ) p
  real ( kind = 8 ) pbc
  real ( kind = 8 ) penalty1
  real ( kind = 8 ) penalty2
  real ( kind = 8 ) phi(maxquad2,6,6,maxelm)
  real ( kind = 8 ) qi
  character ( len = 20 ) region
  real ( kind = 8 ) region_ymax
  real ( kind = 8 ) res(maxeqn)
  real ( kind = 8 ) nu_inv
  real ( kind = 8 ) s
  integer side_basis(3,maxside)
  integer side_elem(maxside)
  character ( len = 3 ) side_eqn(maxside)
  real ( kind = 8 ) side_etam(maxside)
  real ( kind = 8 ) side_etap(maxside)
  integer side_indx(3,maxside)
  real ( kind = 8 ) side_xsim(maxside)
  real ( kind = 8 ) side_xsip(maxside)
  real ( kind = 8 ) squad1(maxquad1)
  real ( kind = 8 ) t
  real ( kind = 8 ) tangent(2)
  real ( kind = 8 ) term
  real ( kind = 8 ) u
  real ( kind = 8 ) ubc
  real ( kind = 8 ) v
  real ( kind = 8 ) vbc
  real ( kind = 8 ) weight
  real ( kind = 8 ) wi
  real ( kind = 8 ) wquad1(maxquad1)
  real ( kind = 8 ) wquad2(maxquad2)
  real ( kind = 8 ) x
  real ( kind = 8 ) x1
  real ( kind = 8 ) x2
  real ( kind = 8 ) x3
  real ( kind = 8 ) xc(maxnp)
  real ( kind = 8 ) xsi
  real ( kind = 8 ) xsim
  real ( kind = 8 ) xsip
  real ( kind = 8 ) y
  real ( kind = 8 ) y1
  real ( kind = 8 ) y2
  real ( kind = 8 ) y3
  real ( kind = 8 ) yc(maxnp)
!
!  Zero out the residual vector.
!
  res(1:neqn) = 0.0D+00
!
!*****************************************************************************80
!  Handle equations associated with area integrals.
!*****************************************************************************80
!
!  Consider an element.
!
  do ielem = 1, nelem
!
!  Evaluate the integrand at each of the quadrature points.
!
    do iquad2 = 1, nquad2
 
      weight = 0.5D+00 * wquad2(iquad2) * detmap(iquad2,ielem)
!
!  Evaluate P, U and V at a quadrature point.
!
      call uvp_quad_value ( dpdx, dpdy, dudx, dudy, dvdx, dvdy, g, ielem, &
        indx, iquad2, maxelm, maxeqn, maxnp, maxquad2, node, p, phi, u, v )
!
!  Look at all basis functions in the element IELEM.
!
      do iq = 1, 6
 
        ip = node(iq,ielem)
 
        wi =    phi(iquad2,iq,1,ielem)
        dwidx = phi(iquad2,iq,2,ielem)
        dwidy = phi(iquad2,iq,3,ielem)
        qi =    phi(iquad2,iq,4,ielem)

        x = xc(ip)
        y = yc(ip)
        call boundary_shape ( bc_tag(ip), normal, region, tangent )

        t     = tangent(1) * u     + tangent(2) * v
        dudr  = tangent(1) * dudx  + tangent(2) * dudy
        dvdr  = tangent(1) * dvdx  + tangent(2) * dvdy
        dpdr  = tangent(1) * dpdx  + tangent(2) * dpdy
        dwidr = tangent(1) * dwidx + tangent(2) * dwidy

        n     = normal(1)  * u     + normal(2)  * v
        duds  = normal(1)  * dudx  + normal(2)  * dudy
        dvds  = normal(1)  * dvdx  + normal(2)  * dvdy
        dwids = normal(1)  * dwidx + normal(2)  * dwidy

        dtdr  = tangent(1) * dudr  + tangent(2) * dvdr
        dtds  = tangent(1) * duds  + tangent(2) * dvds

        ihor = indx(1,ip)
        iver = indx(2,ip)
        iprs = indx(3,ip)
!
!  The horizontal velocity momentum equation.
!
        if ( eqn(ihor) == 'UM' ) then

          res(ihor) = res(ihor) + weight * ( dudx * dwidx + dudy * dwidy &
            + nu_inv * ( navier * u * dudx + navier * v * dudy + dpdx ) * wi )

        end if
!
!  The vertical velocity momentum equation, or
!  the tangential momentum equation, penalized.  
!
        if ( eqn(iver) == 'VM' ) then

          res(iver) = res(iver) + weight * ( dvdx * dwidx + dvdy * dwidy &
            + nu_inv * ( navier * u * dvdx + navier * v * dvdy + dpdy ) * wi )

        else if ( eqn(iver) == 'TMP' ) then

          res(iver) = res(iver) + weight * penalty2 * &
            ( dtdr * dwidr + dtds * dwids + nu_inv * &
            ( navier * n * dtds + navier * t * dtdr + dpdr ) * wi )
 
        end if
!
!  The pressure equations.
!
        if ( 0 < iprs ) then
          if ( eqn(iprs) == 'PC' ) then
            res(iprs) = res(iprs) + weight * ( dudx + dvdy ) * qi
          end if
        end if
 
      end do
    end do
  end do
!
!*****************************************************************************80
!  Handle equations associated with line integrals along the boundary.
!*****************************************************************************80
!
!  For each element side that is along the boundary.
!
  do iside = 1, nside
!
!  If the element side condition is 'FS'...
!
    if ( side_eqn(iside) == 'FS' ) then

      ielem = side_elem(iside)
      etam = side_etam(iside)
      etap = side_etap(iside)
      xsim = side_xsim(iside)
      xsip = side_xsip(iside)
!
!  Rough estimate of the length of the side.
!
      iq = side_basis(1,iside)
      ip = node(iq,ielem)
      x1 = xc(ip)
      y1 = yc(ip)

      iq = side_basis(2,iside)
      ip = node(iq,ielem)
      x2 = xc(ip)
      y2 = yc(ip)

      iq = side_basis(3,iside)
      ip = node(iq,ielem)
      x3 = xc(ip)
      y3 = yc(ip)

      length = sqrt ( ( x2 - x1 )**2 + ( y2 - y1 )**2 ) &
             + sqrt ( ( x3 - x2 )**2 + ( y3 - y2 )**2 )
!
!  KLUDGE.  But it's right, I think.
!
      if ( x3 < x1 ) then
        length = - length
      end if

      do iquad1 = 1, nquad1

        s = squad1(iquad1)
        weight = 0.5D+00 * wquad1(iquad1) * length

        eta = 0.5D+00 * ( ( 1.0D+00 - s ) * etam + ( 1.0D+00 + s ) * etap )
        xsi = 0.5D+00 * ( ( 1.0D+00 - s ) * xsim + ( 1.0D+00 + s ) * xsip )

        call x_of_xsi ( eta, ielem, maxelm, maxnp, node, x, xc, xsi, y, yc )

        call uvp_value ( eta, g, ielem, indx, maxelm, maxeqn, maxnp, node, &
          p, u, v, xsi )
!
!  There are three basis functions on this portion of the boundary.
!
        do i = 1, 3

          iq = side_basis(i,iside)
          ip = node(iq,ielem)
          idof = side_indx(i,iside)
          ieqn = indx(idof,ip)

          call ref_bf_q6 ( wi, dwdeta, dwdxsi, eta, iq, xsi )

          call boundary_shape ( bc_tag(ip), normal, region, tangent )
!
!  This term is independent of the sign of the tangent...
!
          term = ( u * tangent(1) + v * tangent(2) ) &
            * ( tangent(1) + tangent(2) ) * wi

          if ( eqn(ieqn) == 'TMP' ) then
            res(ieqn) = res(ieqn) + penalty1 * weight * term
          end if

        end do

      end do

    end if

  end do
!
!*****************************************************************************80
!  Handle conditions associated with nodal values.
!*****************************************************************************80
!
  do ip = 1, np

    ihor = indx(1,ip)
    iver = indx(2,ip)
    iprs = indx(3,ip)

    x = xc(ip)
    y = yc(ip)

    call boundary_value ( bc_tag(ip), pbc, region, region_ymax, ubc, vbc, y )

    call boundary_shape ( bc_tag(ip), normal, region, tangent )
!
!  Specified value of horizontal velocity at a node.
!
    if ( eqn(ihor) == 'UI' ) then

      res(ihor) = g(ihor) - ubc

    else if ( eqn(ihor) == 'U0' ) then

      res(ihor) = g(ihor)
!
!  Velocity dot local normal vector at a node is zero.
!
    else if ( eqn(ihor) == 'N0' ) then

      res(ihor) = g(ihor) * normal(1) + g(iver) * normal(2)

    end if
!
!  Specified value of vertical velocity at a node.
!
    if ( eqn(iver) == 'VI' ) then

      res(iver) = g(iver) - vbc

    else if ( eqn(iver) == 'V0' ) then

      res(iver) = g(iver)
!
!  Velocity dot local tangent vector at a node is zero.
!
    else if ( eqn(iver) == 'T0' ) then

      res(iver) = g(ihor) * tangent(1) + g(iver) * tangent(2)

    end if
!
!  Specified value of pressure at a node.
!
    if ( 0 < iprs ) then
      if ( eqn(iprs) == 'PI' ) then
        res(iprs) = g(iprs) - pbc
      else if ( eqn(iprs) == 'P0' ) then
        res(iprs) = g(iprs)
      end if
    end if

  end do

  return
end
subroutine geometry ( bc_tag, eqn, indx, maxelm, maxeqn, maxnp, nelem, neqn, &
  node, np, nx, ny, region, region_xmax, region_ymax, xc, yc )

!*****************************************************************************80
!
!! GEOMETRY sets up the geometry for any problem.
!
!  Modified:
!
!    11 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer MAXELM, the maximum number of elements.
!
!    Input, integer NELEM, the number of elements.
!
!    Input, integer NODE(6,MAXELM), the nodes that make up each element.
!
!    Input, integer NP, the number of nodes.
!
!    Input, integer NX, the number of elements along a horizontal line.
!
!    Input, integer NY, the number of elements along a vertical line.
!
  implicit none

  integer maxelm
  integer maxeqn
  integer maxnp

  integer bc_tag(maxnp)
  character ( len = 3 ) eqn(maxeqn)
  integer indx(3,maxnp)
  integer nelem
  integer neqn
  integer node(6,maxelm)
  integer np
  integer nx
  integer ny
  character ( len = * ) region
  real ( kind = 8 ) region_xmax
  real ( kind = 8 ) region_ymax
  real ( kind = 8 ) xc(maxnp)
  real ( kind = 8 ) yc(maxnp)

  if ( region == 'CAVITY' ) then

    call geometry_cavity ( bc_tag, eqn, indx, maxelm, maxeqn, maxnp, nelem, &
      neqn, node, np, nx, ny, region_xmax, region_ymax, xc, yc )

  else if ( region == 'CHANNEL' ) then

    call geometry_channel ( bc_tag, eqn, indx, maxelm, maxeqn, maxnp, nelem, &
      neqn, node, np, nx, ny, region_xmax, region_ymax, xc, yc )

  else if ( region == 'FREESLIP' ) then

    call geometry_freeslip ( bc_tag, eqn, indx, maxelm, maxeqn, maxnp, nelem, &
      neqn, node, np, nx, ny, region_xmax, region_ymax, xc, yc )

  else if ( region == 'STEP' ) then

    call geometry_step ( bc_tag, eqn, indx, maxelm, maxeqn, maxnp, nelem, &
      neqn, node, np, nx, ny, region_xmax, region_ymax, xc, yc )

  else

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GEOMETRY - Fatal error!'
    write ( *, '(a)' ) '  Illegal region.'
    stop

  end if

  return
end
subroutine geometry_cavity ( bc_tag, eqn, indx, maxelm, maxeqn, maxnp, &
  nelem, neqn, node, np, nx, ny, region_xmax, region_ymax, xc, yc )

!*****************************************************************************80
!
!! GEOMETRY_CAVITY sets up the geometry for the cavity problem.
!
!  Discussion:
!
!    The flow region is a square, of width and height 1, with no slip walls on 
!    the sides and bottom, and open on the top.  A tangential force is 
!    applied uniformly along the top, enforced with a boundary condition 
!    of the form:
!
!    U(X,1) = 1
!    V(X,1) = 0
!
!  Modified:
!
!    11 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer MAXELM, the maximum number of elements.
!
!    Input, integer NELEM, the number of elements.
!
!    Input, integer NODE(6,MAXELM), the nodes that make up each element.
!
!    Input, integer NP, the number of nodes.
!
!    Input, integer NX, the number of elements along a horizontal line.
!
!    Input, integer NY, the number of elements along a vertical line.
!
  implicit none

  integer maxelm
  integer maxeqn
  integer maxnp

  integer bc_tag(maxnp)
  integer cc
  integer ee
  character ( len = 3 ) eqn(maxeqn)
  integer icol
  integer indx(3,maxnp)
  integer irow
  integer ncol
  integer ne
  integer nelem
  integer neqn
  integer nn
  integer node(6,maxelm)
  integer np
  integer nrow
  integer nw
  integer nx
  integer ny
  logical pset
  real ( kind = 8 ) region_xmax
  real ( kind = 8 ) region_ymax
  integer se
  integer ss
  integer sw
  integer ww
  real ( kind = 8 ) xc(maxnp)
  real ( kind = 8 ) yc(maxnp)

  pset = .FALSE.

  nelem = 0
  neqn = 0
  np = 0

  ncol = 2 * nx + 1
  nrow = 2 * ny + 1

  do icol = 1, ncol
    do irow = 1, nrow
!
!  1. Generate the next node.
!
      np = np + 1
!
!  2.  Assign X, Y coordinates to the node.
!
      xc(np) = region_xmax * real ( icol - 1, kind = 8 ) &
        / real ( ncol - 1, kind = 8 )
      yc(np) = region_ymax * real ( irow - 1, kind = 8 ) &
        / real ( nrow - 1, kind = 8 )
!
!  3.  Assign a BC_TAG value.
!
      if ( irow == 1 ) then
        if ( icol == 1 ) then
          bc_tag(np) = 1
        else if ( icol < ncol ) then
          bc_tag(np) = 2
        else
          bc_tag(np) = 3
        end if
      else if ( irow < nrow ) then
        if ( icol == 1 ) then
          bc_tag(np) = 8
        else if ( icol < ncol ) then
          bc_tag(np) = 0
        else
          bc_tag(np) = 4
        end if
      else if ( irow == nrow ) then
        if ( icol == 1 ) then
          bc_tag(np) = 7
        else if ( icol < ncol ) then
          bc_tag(np) = 6
        else
          bc_tag(np) = 5
        end if
      end if
!
!  4. Assign equation indices to the node.
!
!
!  Node along top.
!  "Horizontal Velocity Specified"
!  "Vertical Velocity 0".
!
      if ( irow == nrow ) then
 
        neqn = neqn + 1
        indx(1,np) = neqn
        eqn(neqn) = 'UI'

        neqn = neqn + 1
        indx(2,np) = neqn
        eqn(neqn) = 'V0'
!
!  Node along sides or bottom.
!  "Horizontal Velocity 0"
!  "Vertical Velocity 0".
!
      else if ( &
        icol == 1 .or. &
        icol == ncol .or. &
        irow == 1 ) then
 
        neqn = neqn + 1
        indx(1,np) = neqn
        eqn(neqn) = 'U0'
 
        neqn = neqn + 1
        indx(2,np) = neqn
        eqn(neqn) = 'V0'
!
!  Interior node.
!
      else
 
        neqn = neqn + 1
        indx(1,np) = neqn
        eqn(neqn) = 'UM'
 
        neqn = neqn + 1
        indx(2,np) = neqn
        eqn(neqn) = 'VM'
 
      end if
!
!  Set the pressure condition.
!
      if ( mod ( irow, 2 ) == 0 .or. mod ( icol, 2 ) == 0 ) then

        indx(3,np) = 0

      else if ( &
        .not. pset .and. &
        2 <= neqn .and. &
        eqn(neqn-1) == 'UM' .and. &
        eqn(neqn) == 'VM' ) then

        neqn = neqn + 1
        indx(3,np) = neqn
        eqn(neqn) = 'P0'
        pset = .true.

      else

        neqn = neqn + 1
        indx(3,np) = neqn
        eqn(neqn) = 'PC'

      end if
!
!  5. Generate the next pair of elements?
!
!  If both the row and the column are odd, and we're not in the first
!  column or row, then we can define two new triangular elements 
!  based at the node.
!

!    NW---NN---NE
!     |\   |
!     | \  |
!     |  \     |
!    WW   CC   EE
!     |     \  |
!     |  \ |
!     |   \|
!    SW---SS---SE
!
      if ( &
        1 < irow .and. mod ( irow, 2 ) == 1 .and. &
        1 < icol .and. mod ( icol, 2 ) == 1 ) then

        sw = np - 2 * nrow - 2
        ww = np - 2 * nrow - 1
        nw = np - 2 * nrow

        ss = np - nrow - 2
        cc = np - nrow - 1
        nn = np - nrow

        se = np - 2
        ee = np - 1
        ne = np 

        nelem = nelem + 1
  
        node(1,nelem) = se
        node(2,nelem) = nw
        node(3,nelem) = sw
        node(4,nelem) = ss
        node(5,nelem) = cc
        node(6,nelem) = ww
 
        nelem = nelem + 1

        node(1,nelem) = nw
        node(2,nelem) = se
        node(3,nelem) = ne
        node(4,nelem) = nn
        node(5,nelem) = cc
        node(6,nelem) = ee
 
      end if

    end do

  end do

  return
end
subroutine geometry_channel ( bc_tag, eqn, indx, maxelm, maxeqn, maxnp, &
  nelem, neqn, node, np, nx, ny, region_xmax, region_ymax, xc, yc )

!*****************************************************************************80
!
!! GEOMETRY_CHANNEL sets up the geometry for the channel problem.
!
!  Discussion:
!
!    Consider a horizontal channel of constant height H, and of length L.
!
!    Suppose a parabolic inflow is specified at the left hand opening,
!    where X=0, of the form
!
!  U(0,Y) = S * Y * (H-Y)
!  V(0,Y) = 0
!  P(0,Y) = 0
!
!    where S is any value.
!
!    Then the following functions (U,V,P) solve the Navier Stokes
!    equations in the region:
!
!  U(X,Y) = S * Y * (H-Y)
!  V(X,Y) = 0
!  P(X,Y) = -2 * S * X * RE
!
!    The standard problem we use has H = 3, L = 10, and chooses 
!    S = (4/9)*Lambda, so that the maximum value of the parabolic inflow, 
!    at Y = H/2, is Lambda.  Then our formula becomes:
!
!  U(X,Y) = (4/9) * Lambda * Y * (3-Y)
!  V(X,Y) = 0
!  P(X,Y) = -2 * (4/9) * Lambda * X * RE
!
!  Modified:
!
!    22 September 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer MAXELM, the maximum number of elements.
!
!    Input, integer NELEM, the number of elements.
!
!    Input, integer NODE(6,MAXELM), the nodes that make up each element.
!
!    Input, integer NP, the number of nodes.
!
!    Input, integer NX, the number of elements along a horizontal line.
!
!    Input, integer NY, the number of elements along a vertical line.
!
  implicit none

  integer maxelm
  integer maxeqn
  integer maxnp

  integer bc_tag(maxnp)
  integer cc
  integer ee
  character ( len = 3 ) eqn(maxeqn)
  integer icol
  integer indx(3,maxnp)
  integer irow
  integer ncol
  integer ne
  integer nelem
  integer neqn
  integer nn
  integer node(6,maxelm)
  integer np
  integer nrow
  integer nw
  integer nx
  integer ny
  real ( kind = 8 ) region_xmax
  real ( kind = 8 ) region_ymax
  integer se
  integer ss
  integer sw
  integer ww
  real ( kind = 8 ) xc(maxnp)
  real ( kind = 8 ) yc(maxnp)

  nelem = 0
  neqn = 0
  np = 0

  ncol = 2 * nx + 1
  nrow = 2 * ny + 1

  do icol = 1, ncol
    do irow = 1, nrow
!
!  1. Generate the next node.
!
      np = np + 1
!
!  2.  Assign X, Y coordinates to the node.
!
      xc(np) = region_xmax * real ( icol - 1 ) &
        / real ( ncol - 1, kind = 8 )
      yc(np) = region_ymax * real ( irow - 1 ) &
        / real ( nrow - 1, kind = 8 )
!
!  3.  Assign a BC_TAG value.
!
      if ( irow == 1 ) then
        if ( icol == 1 ) then
          bc_tag(np) = 1
        else if ( icol < ncol ) then
          bc_tag(np) = 2
        else
          bc_tag(np) = 3
        end if
      else if ( irow < nrow ) then
        if ( icol == 1 ) then
          bc_tag(np) = 8
        else if ( icol < ncol ) then
          bc_tag(np) = 0
        else
          bc_tag(np) = 4
        end if
      else if ( irow == nrow ) then
        if ( icol == 1 ) then
          bc_tag(np) = 7
        else if ( icol < ncol ) then
          bc_tag(np) = 6
        else
          bc_tag(np) = 5
        end if
      end if
!
!  4. Assign equation indices to the node.
!
!  Corners.
!
      if ( &
        ( icol == 1 .or. icol == ncol ) .and. &
        ( irow == 1 .or. irow == nrow ) ) then

        neqn = neqn + 1
        indx(1,np) = neqn
        eqn(neqn) = 'U0'
 
        neqn = neqn + 1
        indx(2,np) = neqn
        eqn(neqn) = 'V0'
!
!  Node within left hand inflow boundary.
!  H specified.
!  V 0.
!
      else if ( icol == 1 ) then
 
        neqn = neqn + 1
        indx(1,np) = neqn
        eqn(neqn) = 'UI'
 
        neqn = neqn + 1
        indx(2,np) = neqn
        eqn(neqn) = 'V0'
!
!  Node within right hand outflow boundary.
!  V = 0.
!
      else if ( icol == ncol ) then
 
        neqn = neqn + 1
        indx(1,np) = neqn
        eqn(neqn) = 'UM'
 
        neqn = neqn + 1
        indx(2,np) = neqn
        eqn(neqn) = 'V0'
!
!  Node within fixed wall.
!  "Normal Velocity Zero"
!  "Tangential Velocity Zero".
!
      else if ( irow == 1 .or. irow == nrow ) then
 
        neqn = neqn + 1
        indx(1,np) = neqn
        eqn(neqn) = 'N0'

        neqn = neqn + 1
        indx(2,np) = neqn
        eqn(neqn) = 'T0'
!
!  Interior node.
!
      else
 
        neqn = neqn + 1
        indx(1,np) = neqn
        eqn(neqn) = 'UM'
 
        neqn = neqn + 1
        indx(2,np) = neqn
        eqn(neqn) = 'VM'
 
      end if
!
!  Set the pressure condition.
!
      if ( mod ( irow, 2 ) == 0 .or. mod ( icol, 2 ) == 0 ) then

        indx(3,np) = 0

      else if ( irow == nrow .and. icol == ncol ) then

        neqn = neqn + 1
        indx(3,np) = neqn
        eqn(neqn) = 'PI'

      else

        neqn = neqn + 1
        indx(3,np) = neqn
        eqn(neqn) = 'PC'

      end if
!
!  5. Generate the next pair of elements?
!
!  If both the row and the column are odd, and we're not in the first
!  column or row, then we can define two new triangular elements 
!  based at the node.
!
!    NW---NN---NE
!     |\   |
!     | \  |
!     |  \     |
!    WW   CC   EE
!     |     \  |
!     |  \ |
!     |   \|
!    SW---SS---SE
!
      if ( 1 < irow .and. mod ( irow, 2 ) == 1 .and. &
           1 < icol .and. mod ( icol, 2 ) == 1 ) then

        sw = np - 2 * nrow - 2
        ww = np - 2 * nrow - 1
        nw = np - 2 * nrow

        ss = np - nrow - 2
        cc = np - nrow - 1
        nn = np - nrow

        se = np - 2
        ee = np - 1
        ne = np 

        nelem = nelem + 1
  
        node(1,nelem) = se
        node(2,nelem) = nw
        node(3,nelem) = sw
        node(4,nelem) = ss
        node(5,nelem) = cc
        node(6,nelem) = ww
 
        nelem = nelem + 1

        node(1,nelem) = nw
        node(2,nelem) = se
        node(3,nelem) = ne
        node(4,nelem) = nn
        node(5,nelem) = cc
        node(6,nelem) = ee
 
      end if

    end do

  end do

  return
end
subroutine geometry_freeslip ( bc_tag, eqn, indx, maxelm, maxeqn, maxnp, &
  nelem, neqn, node, np, nx, ny, region_xmax, region_ymax, xc, yc )

!*****************************************************************************80
!
!! GEOMETRY_FREESLIP sets up the geometry for the freeslip problem.
!
!  Modified:
!
!    11 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer MAXELM, the maximum number of elements.
!
!    Input, integer NELEM, the number of elements.
!
!    Input, integer NODE(6,MAXELM), the nodes that make up each element.
!
!    Input, integer NP, the number of nodes.
!
!    Input, integer NX, the number of elements along a horizontal line.
!
!    Input, integer NY, the number of elements along a vertical line.
!
  implicit none

  integer maxelm
  integer maxeqn
  integer maxnp

  integer bc_tag(maxnp)
  integer cc
  integer ee
  character ( len = 3 ) eqn(maxeqn)
  integer icol
  integer indx(3,maxnp)
  integer irow
  integer ncol
  integer ne
  integer nelem
  integer neqn
  integer nn
  integer node(6,maxelm)
  integer np
  integer nrow
  integer nw
  integer nx
  integer ny
  real ( kind = 8 ) region_xmax
  real ( kind = 8 ) region_ymax
  integer se
  integer ss
  integer sw
  integer ww
  real ( kind = 8 ) xc(maxnp)
  real ( kind = 8 ) yc(maxnp)

  nelem = 0
  neqn = 0
  np = 0

  ncol = 2 * nx + 1
  nrow = 2 * ny + 1

  do icol = 1, ncol
    do irow = 1, nrow
!
!  1. Generate the next node.
!
      np = np + 1
!
!  2.  Assign X, Y coordinates to the node.
!
      xc(np) = region_xmax * real ( icol - 1, kind = 8 ) &
        / real ( ncol - 1, kind = 8 )
      yc(np) = region_ymax * real ( irow - 1, kind = 8 ) &
        / real ( nrow - 1, kind = 8 )
!
!  3.  Assign a BC_TAG value.
!
      if ( irow == 1 ) then
        if ( icol == 1 ) then
          bc_tag(np) = 1
        else if ( icol < ncol ) then
          bc_tag(np) = 2
        else
          bc_tag(np) = 3
        end if
      else if ( irow < nrow ) then
        if ( icol == 1 ) then
          bc_tag(np) = 8
        else if ( icol < ncol ) then
          bc_tag(np) = 0
        else
          bc_tag(np) = 4
        end if
      else if ( irow == nrow ) then
        if ( icol == 1 ) then
          bc_tag(np) = 7
        else if ( icol < ncol ) then
          bc_tag(np) = 6
        else
          bc_tag(np) = 5
        end if
      end if
!
!  4. Assign equation indices to the node.
!
!  Corners.
!
      if ( icol == 1 .and. &
           ( irow == 1 .or. irow == nrow ) ) then

        neqn = neqn + 1
        indx(1,np) = neqn
        eqn(neqn) = 'N0'
 
        neqn = neqn + 1
        indx(2,np) = neqn
        eqn(neqn) = 'TMP'

      else if ( icol == ncol .and. &
        ( irow == 1 .or. irow == nrow ) ) then

        neqn = neqn + 1
        indx(1,np) = neqn
        eqn(neqn) = 'N0'
 
        neqn = neqn + 1
        indx(2,np) = neqn
        eqn(neqn) = 'TMP'
!
!  Node within left hand inflow boundary.
!  H specified.
!  V = 0.
!
      else if ( icol == 1 ) then
 
        neqn = neqn + 1
        indx(1,np) = neqn
        eqn(neqn) = 'UI'
 
        neqn = neqn + 1
        indx(2,np) = neqn
        eqn(neqn) = 'V0'
!
!  Node within right hand outflow boundary.
!  V = 0.
!
      else if ( icol == ncol ) then
 
        neqn = neqn + 1
        indx(1,np) = neqn
        eqn(neqn) = 'UM'
 
        neqn = neqn + 1
        indx(2,np) = neqn
        eqn(neqn) = 'V0'
!
!  Node within fixed wall.
!  "Normal Velocity Zero"
!  "Tangential Slip / Momentum / Penalty term".
!
      else if ( irow == 1 .or. irow == nrow ) then
 
        neqn = neqn + 1
        indx(1,np) = neqn
        eqn(neqn) = 'N0'

        neqn = neqn + 1
        indx(2,np) = neqn
        eqn(neqn) = 'TMP'
!
!  Interior node.
!
      else
 
        neqn = neqn + 1
        indx(1,np) = neqn
        eqn(neqn) = 'UM'
 
        neqn = neqn + 1
        indx(2,np) = neqn
        eqn(neqn) = 'VM'
 
      end if
!
!  Set the pressure condition.
!
      if ( mod ( irow, 2 ) == 0 .or. mod ( icol, 2 ) == 0 ) then

        indx(3,np) = 0

      else if ( irow == nrow .and. icol == ncol ) then

        neqn = neqn + 1
        indx(3,np) = neqn
        eqn(neqn) = 'P0'

      else

        neqn = neqn + 1
        indx(3,np) = neqn
        eqn(neqn) = 'PC'

      end if
!
!  5. Generate the next pair of elements?
!
!  If both the row and the column are odd, and we're not in the first
!  column or row, then we can define two new triangular elements 
!  based at the node.
!
!    NW---NN---NE
!     |\   |
!     | \  |
!     |  \     |
!    WW   CC   EE
!     |     \  |
!     |  \ |
!     |   \|
!    SW---SS---SE
!
      if ( 1 < irow .and. mod ( irow, 2 ) == 1 .and. &
           1 < icol .and. mod ( icol, 2 ) == 1 ) then

        sw = np - 2 * nrow - 2
        ww = np - 2 * nrow - 1
        nw = np - 2 * nrow

        ss = np - nrow - 2
        cc = np - nrow - 1
        nn = np - nrow

        se = np - 2
        ee = np - 1
        ne = np 

        nelem = nelem + 1
  
        node(1,nelem) = se
        node(2,nelem) = nw
        node(3,nelem) = sw
        node(4,nelem) = ss
        node(5,nelem) = cc
        node(6,nelem) = ww
 
        nelem = nelem + 1

        node(1,nelem) = nw
        node(2,nelem) = se
        node(3,nelem) = ne
        node(4,nelem) = nn
        node(5,nelem) = cc
        node(6,nelem) = ee
 
      end if

    end do

  end do

  return
end
subroutine geometry_step ( bc_tag, eqn, indx, maxelm, maxeqn, maxnp, nelem, &
  neqn, node, np, nx, ny, region_xmax, region_ymax, xc, yc )

!*****************************************************************************80
!
!! GEOMETRY_STEP sets up the geometry for the step problem.
!
!  Decomposition of 2 by 8 STEP region into nodes:
!
!    2.0D+00  05-10-15-20-25-30-35-38-43-48-53-58-63-68-73-78-83
!      |\    |\    |\    |\    |\    |\    |\    |\    |
!      | \   | \   | \   | \   | \   | \   | \   | \   |
!     04 09 14 19 24 29 34 37 42 47 52 57 62 67 72 77 82
!      |   \ |   \ |   \ |   \ |   \ |   \ |   \ |   \ |
!      |    \|    \|    \|    \|    \|    \|    \|    \|
!    1.0D+00  03-08-13-18-23-28-33-36-41-46-51-56-61-66-71-76-81
!      |\    |\    |\    |     |\    |\    |\    |\    |
!      | \   | \   | \   |     | \   | \   | \   | \   |
!     02 07 12 17 22 27 32    40 45 50 55 60 65 70 75 80
!      |   \ |   \ |   \ |     |   \ |   \ |   \ |   \ |
!      |    \|    \|    \|     |    \|    \|    \|    \|
!    0.0D+00  01-06-11-16-21-26-31    39-44-49-54-59-64-69-74-79
!
!     0.0D+00   1.0D+00   2.0D+00   3.0D+00   4.0D+00   5.0D+00   6.0D+00   7.0D+00   8.0D+00
!
!  Decomposition of 2 by 8 STEP region into elements:
!
!      +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
!      |\ 4  |\ 8  |\12  |\14  |\18  |\22  |\26  |\30  |
!      | \   | \   | \   | \   | \   | \   | \   | \   |
!      +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +
!      |   \ |   \ |   \ |   \ |   \ |   \ |   \ |   \ |
!      |  3 \|  7 \| 11 \| 13 \| 17 \| 21 \| 25 \| 29 \|
!      +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
!      |\ 2  |\ 6  |\10  |     |\16  |\20  |\24  |\28  |
!      | \   | \   | \   |     | \   | \   | \   | \   |
!      +  +  +  +  +  +  +     +  +  +  +  +  +  +  +  +
!      |   \ |   \ |   \ |     |   \ |   \ |   \ |   \ |
!      |  1 \|  5 \|  9 \|     | 15 \| 19 \| 23 \| 27 \|
!      +--+--+--+--+--+--+     +--+--+--+--+--+--+--+--+
!
!  Velocity equation types in 2 by 8 STEP region:
!
!    i0: U specified, V = 0
!    nt: Normal velocity = 0, Tangential/Momentum Penalty;
!    00: U = V = 0.
!    M0: U momentum, V = 0;
!    MM: U and V momentum.
!
!    nt-nt-nt-nt-nt-nt-nt-nt-nt-nt-nt-nt-nt-nt-nt-nt-nt
!     |\    |\    |\    |\    |\    |\    |\    |\    |
!     | \   | \   | \   | \   | \   | \   | \   | \   |
!    i0 MM MM MM MM MM MM MM MM MM MM MM MM MM MM MM M0
!     |   \ |   \ |   \ |   \ |   \ |   \ |   \ |   \ |
!     |    \|    \|    \|    \|    \|    \|    \|    \|
!    i0-MM-MM-MM-MM-MM-00-nt-00-MM--+-MM-MM-MM-MM-MM-M0
!     |\    |\    |\    |     |\    |\    |\    |\    |
!     | \   | \   | \   |     | \   | \   | \   | \   |
!    i0 MM MM MM MM MM nt    nt MM MM MM MM MM MM MM M0
!     |   \ |   \ |   \ |     |   \ |   \ |   \ |   \ |
!     |    \|    \|    \|     |    \|    \|    \|    \|
!    nt-nt-nt-nt-nt-nt-00    00-nt-nt-nt-nt-nt-nt-nt-nt
!
!  Modified:
!
!    12 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer MAXELM, the maximum number of elements.
!
!    Input, integer NELEM, the number of elements.
!
!    Input, integer NODE(6,MAXELM), the nodes that make up each element.
!
!    Input, integer NP, the number of nodes.
!
!    Input, integer NX, the number of elements along a horizontal line.
!
!    Input, integer NY, the number of elements along a vertical line.
!
  implicit none

  integer maxelm
  integer maxeqn
  integer maxnp

  integer bc_tag(maxnp)
  integer cc
  integer ee
  character ( len = 3 ) eqn(maxeqn)
  integer icol
  integer inc1
  integer inc2
  integer indx(3,maxnp)
  integer irow
  integer ncol
  integer ncol1
  integer ncol2
  integer ncol3
  integer ne
  integer nelem
  integer neqn
  integer nn
  integer node(6,maxelm)
  integer np
  integer nrow
  integer nrow1
  integer nrow2
  integer nw
  integer nx
  integer nx1
  integer nx2
  integer nx3
  integer ny
  integer ny1
  integer ny2
  real ( kind = 8 ) region_xmax
  real ( kind = 8 ) region_ymax
  integer se
  integer ss
  integer sw
  integer ww
  real ( kind = 8 ) xc(maxnp)
  real ( kind = 8 ) yc(maxnp)
!
!  Try to divide the horizontal and vertical number of elements into groups.
!
  nx1 = nint ( 0.375D+00 * real ( nx, kind = 8 ) )
  nx1 = max ( 1, nx1 )
  nx2 = nint ( 0.125D+00 * real ( nx, kind = 8 ) )
  nx2 = max ( 1, nx2 )
  nx3 = nx - nx1 - nx2

  if ( nx3 <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GEOMETRY_STEP - Fatal error!'
    write ( *, '(a)' ) '  NX is too small!'
    stop
  end if

  ny1 = max ( 1, nint ( 0.500D+00 * real ( ny, kind = 8 ) ) )
  ny2 = ny - ny1

  if ( ny2 <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GEOMETRY_STEP - Fatal error!'
    write ( *, '(a)' ) '  NY is too small!'
    stop
  end if

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'GEOMETRY_STEP:'
  write ( *, '(a)' ) '  Horizontal elements arranged in groups of'
  write ( *, '(3i8)' ) nx1, nx2, nx3
  write ( *, '(a)' ) '  Vertical elements arranged in groups of'
  write ( *, '(2i8)' ) ny1, ny2
!
!  Determine the node rows and columns which represent boundaries
!  between the regions.
!
  ncol = 2 * nx + 1
  nrow = 2 * ny + 1

  ncol1 = 2 * nx1 + 1
  ncol2 = 2 * ( nx1 + nx2 ) + 1
  ncol3 = 2 * ( nx1 + nx2 + nx3 ) + 1

  nrow1 = 2 * ny1 + 1
  nrow2 = 2 * ( ny1 + ny2 ) + 1

  nelem = 0
  neqn = 0
  np = 0

  do icol = 1, ncol
    do irow = 1, nrow
!
!  1. Generate the next node, but don't generate nodes "under" the step.
!
      if ( icol <= ncol1 .or. ncol2 <= icol .or. nrow1 <= irow ) then

        np = np + 1
!
!  2.  Assign X, Y coordinates to the node.
!
        xc(np) = region_xmax * real ( icol - 1, kind = 8 ) &
          / real ( ncol - 1, kind = 8 )
        yc(np) = region_ymax * real ( irow - 1, kind = 8 ) &
          / real ( nrow - 1, kind = 8 )
!
!  3.  Assign a BC_TAG value.
!
!  NROW2 -> 15 14 14 14 14 14 14 14 14 13
!       16  0  0  0  0  0  0  0  0 12
!       16  0  0  0  0  0  0  0  0 12
!       16  0  0  0  0  0  0  0  0 12
!  NROW1 -> 16  0  0  5  6  6  7  0  0 12
!       16  0  0  4 -1 -1  8  0  0 12
!       16  0  0  4 -1 -1  8  0  0 12
!  1     ->  1  2  2  3 -1 -1  9 10 10 11
!
!        ^        ^        ^        ^
!        |        |        |        |
!        1        N        N        N
!                 C        C        C
!                 O        O        O
!                 L        L        L
!                 1        2        3
!
        if ( irow == 1 ) then

          if ( icol == 1 ) then
            bc_tag(np) = 1
          else if ( icol < ncol1 ) then
            bc_tag(np) = 2
          else if ( icol == ncol1 ) then
            bc_tag(np) = 3
          else if ( icol < ncol2 ) then
            bc_tag(np) = -1
          else if ( icol == ncol2 ) then
            bc_tag(np) = 9
          else if ( icol < ncol3 ) then
            bc_tag(np) = 10
          else if ( icol == ncol3 ) then
            bc_tag(np) = 11
          end if

        else if ( irow < nrow1 ) then

          if ( icol == 1 ) then
            bc_tag(np) = 16
          else if ( icol < ncol1 ) then
            bc_tag(np) = 0
          else if ( icol == ncol1 ) then
            bc_tag(np) = 4
          else if ( icol < ncol2 ) then
            bc_tag(np) = -1
          else if ( icol == ncol2 ) then
            bc_tag(np) = 8
          else if ( icol < ncol3 ) then
            bc_tag(np) = 0
          else if ( icol == ncol3 ) then
            bc_tag(np) = 12
          end if

        else if ( irow == nrow1 ) then

          if ( icol == 1 ) then
            bc_tag(np) = 16
          else if ( icol < ncol1 ) then
            bc_tag(np) = 0
          else if ( icol == ncol1 ) then
            bc_tag(np) = 5
          else if ( icol < ncol2 ) then
            bc_tag(np) = 6
          else if ( icol == ncol2 ) then
            bc_tag(np) = 7
          else if ( icol < ncol3 ) then
            bc_tag(np) = 0
          else if ( icol == ncol3 ) then
            bc_tag(np) = 12
          end if

        else if ( irow < nrow2 ) then

          if ( icol == 1 ) then
            bc_tag(np) = 16
          else if ( icol < ncol3 ) then
            bc_tag(np) = 0
          else if ( icol == ncol3 ) then
            bc_tag(np) = 12
          end if

        else if ( irow == nrow2 ) then

          if ( icol == 1 ) then
            bc_tag(np) = 15
          else if ( icol < ncol3 ) then
            bc_tag(np) = 14
          else if ( icol == ncol3 ) then
            bc_tag(np) = 13
          end if

        end if
!
!  4. Assign equation indices to the node.
!
        if ( bc_tag(np) == -1 ) then

          neqn = neqn + 1
          indx(1,np) = neqn
          eqn(neqn) = 'U0'

          neqn = neqn + 1
          indx(2,np) = neqn
          eqn(neqn) = 'V0'

        else if ( bc_tag(np) == 0 ) then

          neqn = neqn + 1
          indx(1,np) = neqn
          eqn(neqn) = 'UM'

          neqn = neqn + 1
          indx(2,np) = neqn
          eqn(neqn) = 'VM'

        else if ( bc_tag(np) == 1 ) then

          neqn = neqn + 1
          indx(1,np) = neqn
          eqn(neqn) = 'N0'

          neqn = neqn + 1
          indx(2,np) = neqn
          eqn(neqn) = 'TMP'

        else if ( bc_tag(np) == 2 ) then

          neqn = neqn + 1
          indx(1,np) = neqn
          eqn(neqn) = 'N0'

          neqn = neqn + 1
          indx(2,np) = neqn
          eqn(neqn) = 'TMP'

        else if ( bc_tag(np) == 3 ) then

          neqn = neqn + 1
          indx(1,np) = neqn
          eqn(neqn) = 'U0'

          neqn = neqn + 1
          indx(2,np) = neqn
          eqn(neqn) = 'V0'

        else if ( bc_tag(np) == 4 ) then

          neqn = neqn + 1
          indx(1,np) = neqn
          eqn(neqn) = 'N0'

          neqn = neqn + 1
          indx(2,np) = neqn
          eqn(neqn) = 'TMP'

        else if ( bc_tag(np) == 5 ) then

          neqn = neqn + 1
          indx(1,np) = neqn
          eqn(neqn) = 'U0'

          neqn = neqn + 1
          indx(2,np) = neqn
          eqn(neqn) = 'V0'

        else if ( bc_tag(np) == 6 ) then

          neqn = neqn + 1
          indx(1,np) = neqn
          eqn(neqn) = 'N0'

          neqn = neqn + 1
          indx(2,np) = neqn
          eqn(neqn) = 'TMP'

        else if ( bc_tag(np) == 7 ) then

          neqn = neqn + 1
          indx(1,np) = neqn
          eqn(neqn) = 'U0'

          neqn = neqn + 1
          indx(2,np) = neqn
          eqn(neqn) = 'V0'

        else if ( bc_tag(np) == 8 ) then

          neqn = neqn + 1
          indx(1,np) = neqn
          eqn(neqn) = 'N0'

          neqn = neqn + 1
          indx(2,np) = neqn
          eqn(neqn) = 'TMP'

        else if ( bc_tag(np) == 9 ) then

          neqn = neqn + 1
          indx(1,np) = neqn
          eqn(neqn) = 'U0'

          neqn = neqn + 1
          indx(2,np) = neqn
          eqn(neqn) = 'V0'

        else if ( bc_tag(np) == 10 ) then

          neqn = neqn + 1
          indx(1,np) = neqn
          eqn(neqn) = 'N0'

          neqn = neqn + 1
          indx(2,np) = neqn
          eqn(neqn) = 'TMP'

        else if ( bc_tag(np) == 11 ) then

          neqn = neqn + 1
          indx(1,np) = neqn
          eqn(neqn) = 'N0'

          neqn = neqn + 1
          indx(2,np) = neqn
          eqn(neqn) = 'TMP'

        else if ( bc_tag(np) == 12 ) then

          neqn = neqn + 1
          indx(1,np) = neqn
          eqn(neqn) = 'UM'

          neqn = neqn + 1
          indx(2,np) = neqn
          eqn(neqn) = 'V0'

        else if ( bc_tag(np) == 13 ) then

          neqn = neqn + 1
          indx(1,np) = neqn
          eqn(neqn) = 'N0'

          neqn = neqn + 1
          indx(2,np) = neqn
          eqn(neqn) = 'TMP'

        else if ( bc_tag(np) == 14 ) then

          neqn = neqn + 1
          indx(1,np) = neqn
          eqn(neqn) = 'N0'

          neqn = neqn + 1
          indx(2,np) = neqn
          eqn(neqn) = 'TMP'

        else if ( bc_tag(np) == 15 ) then

          neqn = neqn + 1
          indx(1,np) = neqn
          eqn(neqn) = 'N0'

          neqn = neqn + 1
          indx(2,np) = neqn
          eqn(neqn) = 'TMP'

        else if ( bc_tag(np) == 16 ) then

          neqn = neqn + 1
          indx(1,np) = neqn
          eqn(neqn) = 'UI'

          neqn = neqn + 1
          indx(2,np) = neqn
          eqn(neqn) = 'V0'

        end if
!
!  Set the pressure condition.
!
        if ( mod ( irow, 2 ) == 0 .or. & 
             mod ( icol, 2 ) == 0 ) then

          indx(3,np) = 0
  
        else if ( irow == nrow .and. icol == ncol ) then

          neqn = neqn + 1
          indx(3,np) = neqn
          eqn(neqn) = 'P0'

        else

          neqn = neqn + 1
          indx(3,np) = neqn
          eqn(neqn) = 'PC'

        end if
!
!  5. Generate the next pair of elements?
!
!  If both the row and the column are odd, and we're not in the first
!  column or row, then we can define two new triangular elements
!  based at the node.
!
!    NW---NN---NE
!     |\   |
!     | \  |
!     |  \     |
!    WW   CC   EE
!     |     \  |
!     |  \ |
!     |   \|
!    SW---SS---SE
!
        if ( .not. ( &
          irow == 1 .or. &
          icol == 1 .or. &
          ( ncol1 < icol .and. icol <= ncol2 .and. &
            irow <= nrow1 ) .or. &
          mod ( irow, 2 ) == 0 .or. &
          mod ( icol, 2 ) == 0 ) ) then

          if ( icol <= ncol1 .or. ncol2 < icol  ) then
            inc1 = nrow
            inc2 = nrow
          else if ( ncol1 < icol .and. icol < ncol2 ) then
            inc1 = 2 * ny2 + 1
            inc2 = 2 * ny2 + 1
          else if ( icol == ncol2 ) then
            inc1 = nrow
            inc2 = 2 * ny2 + 1
          end if

          sw = np - inc1 - inc2 - 2
          ww = np - inc1 - inc2 - 1
          nw = np - inc1 - inc2

          ss = np - inc1 - 2
          cc = np - inc1 - 1
          nn = np - inc1

          se = np - 2
          ee = np - 1
          ne = np

          nelem = nelem + 1

          node(1,nelem) = se
          node(2,nelem) = nw
          node(3,nelem) = sw
          node(4,nelem) = ss
          node(5,nelem) = cc
          node(6,nelem) = ww

          nelem = nelem + 1

          node(1,nelem) = nw
          node(2,nelem) = se
          node(3,nelem) = ne
          node(4,nelem) = nn
          node(5,nelem) = cc
          node(6,nelem) = ee

        end if

      end if

    end do

  end do

  return
end
subroutine get_unit ( iunit )

!*****************************************************************************80
!
!! GET_UNIT returns a free FORTRAN unit number.
!
!  Discussion:
!
!    A "free" FORTRAN unit number is an integer between 1 and 99 which
!    is not currently associated with an I/O device.  A free FORTRAN unit
!    number is needed in order to open a file with the OPEN command.
!
!    If IUNIT = 0, then no free FORTRAN unit could be found, although
!    all 99 units were checked (except for units 5, 6 and 9, which
!    are commonly reserved for console I/O).
!
!    Otherwise, IUNIT is an integer between 1 and 99, representing a
!    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
!    are special, and will never return those values.
!
!  Modified:
!
!    18 September 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer IUNIT, the free unit number.
!
  implicit none

  integer i
  integer ios
  integer iunit
  logical lopen

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
subroutine hello ( maxelm, maxeqn, maxnp, maxnx, maxny, maxquad2, maxrow, &
  maxside )

!*****************************************************************************80
!
!! HELLO says hello, prints the program name, date and limits.
!
!  Modified:
!
!    09 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    integer MAXELM, the maximum number of elements.
!
!    integer MAXEQN, the maximum number of equations.
!
!    integer MAXNP, the maximum number of nodes.
!
!    integer MAXNX, the maximum number of elements in the X direction.
!
!    integer MAXNY, the maximum number of elements in the Y direction.
!
!    integer MAXROW, the leading dimension of the system matrix.
!
!    integer MAXSIDE, the maximum number of boundary condition sides.
!
  implicit none

  integer maxelm
  integer maxeqn
  integer maxnp
  integer maxnx
  integer maxny
  integer maxquad2
  integer maxrow
  integer maxside
!
!  Print the program name, date, and computer name.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Primary size parameters:'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '    MAXNX =       ', maxnx
  write ( *, '(a,i8)' ) '    MAXNY =	   ', maxny
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Secondary size parameters:'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '    MAXELM =      ', maxelm
  write ( *, '(a,i8)' ) '    MAXEQN =      ', maxeqn
  write ( *, '(a,i8)' ) '    MAXNP =       ', maxnp
  write ( *, '(a,i8)' ) '    MAXROW =      ', maxrow
  write ( *, '(a,i8)' ) '    MAXSIDE =     ', maxside
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Variable sizes:'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i12)' ) '    Array storage ', maxrow * maxeqn
  write ( *, '(a,i8)' ) '    NODE          ', 6 * maxnp
  write ( *, '(a,i8)' ) '    PHI           ', maxquad2 * 6 * 6 * maxelm

  return
end
subroutine init ( a, bc_tag, detmap, eqn, etaquad, g, ierror, indx, ipivot, &
  jac, maxelm, maxeqn, maxnp, maxquad1, maxquad2, maxrow, maxside, nelem, &
  neqn, newton_max, newton_stutter, newton_tolerance, nlband, node, np, &
  nquad1, nquad2, nrow, nside, nx, ny, p, penalty1, penalty2, phi, pnorm_l2, &
  pnorm_lmax, region, res, nu_inv, side_basis, side_elem, side_eqn, &
  side_etam, side_etap, side_indx, side_xsim, side_xsip, solver, squad1, &
  uvnorm_l2, uvnorm_lmax, wquad1, wquad2, xc, xsiquad, yc )

!*****************************************************************************80
!
!! INIT zeroes out data used by the program.
!
!  Modified:
!
!    08 May 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, character ( len = 3 ) EQN(MAXEQN), the type of each equation.
!
!    Input, integer MAXELM, the maximum number of elements.
!
!    Input, integer MAXNP, the maximum number of nodes.
!
!    Output, integer NEWTON_MAX, the maximum number of Newton iterations.
!
!    Output, integer NEWTON_STUTTER, determines how many times the jacobian
!    can be used before it must be re-evaluated.  A value of 1 means the
!    jacobian is never re-used.  A value of 2 means that it can be used
!    two times, and so on.
!
!    Output, real NEWTON_TOLERANCE, the Newton tolerance.
!    NEWTON is asked to find an approximate solution so that
!    the maximum absolute value of all the residuals is no more
!    than NEWTON_TOLERANCE.  A value such as 10E-7 is often reasonable,
!    though this depends on the actual equations being solved.
!
!    Output, integer NX, the number of elements along a horizontal line.
!
!    Output, integer NY, the number of elements along a vertical line.
!
!    Output, character ( len = 20 ) REGION, the flow problem.
!
!    Output, character ( len = 20 ) SOLVER, the linear system solver, 
!    'GAUSS' or 'CGS'.
!
  implicit none

  integer maxelm
  integer maxeqn
  integer maxnp
  integer maxquad1
  integer maxquad2
  integer maxrow
  integer maxside

  real ( kind = 8 ) a(maxrow,maxeqn)
  integer bc_tag(maxnp)
  real ( kind = 8 ) detmap(maxquad2,maxelm)
  character ( len = 3 ) eqn(maxeqn)
  real ( kind = 8 ) etaquad(maxquad2)
  real ( kind = 8 ) g(maxeqn)
  integer ierror
  integer indx(3,maxnp)
  integer ipivot(maxeqn)
  integer jac
  integer nelem
  integer neqn
  integer newton_max
  integer newton_stutter
  real ( kind = 8 ) newton_tolerance
  integer nlband
  integer node(6,maxelm)
  integer np
  integer nquad1
  integer nquad2
  integer nrow
  integer nside
  integer nx
  integer ny
  real ( kind = 8 ) p(maxnp)
  real ( kind = 8 ) penalty1
  real ( kind = 8 ) penalty2
  real ( kind = 8 ) phi(maxquad2,6,6,maxelm)
  real ( kind = 8 ) pnorm_l2
  real ( kind = 8 ) pnorm_lmax
  character ( len = 20 ) region
  real ( kind = 8 ) res(maxeqn)
  real ( kind = 8 ) nu_inv
  integer side_basis(3,maxside)
  integer side_elem(maxside)
  character ( len = 3 ) side_eqn(maxside)
  real ( kind = 8 ) side_etam(maxside)
  real ( kind = 8 ) side_etap(maxside)
  integer side_indx(3,maxside)
  real ( kind = 8 ) side_xsim(maxside)
  real ( kind = 8 ) side_xsip(maxside)
  character ( len = 20 ) solver
  real ( kind = 8 ) squad1(maxquad1)
  real ( kind = 8 ) uvnorm_l2
  real ( kind = 8 ) uvnorm_lmax
  real ( kind = 8 ) wquad1(maxquad1)
  real ( kind = 8 ) wquad2(maxquad2)
  real ( kind = 8 ) xc(maxnp)
  real ( kind = 8 ) xsiquad(maxquad2)
  real ( kind = 8 ) yc(maxnp)

  a = 0.0D+00
  bc_tag = 0
  detmap = 0.0D+00
  eqn = '???'
  etaquad = 0.0D+00
  g = 0.0D+00
  ierror = 0
  indx = 0
  ipivot = 0
  jac = 0
  nelem = 0
  neqn = 0
  newton_max = 20
  newton_stutter = 3
  newton_tolerance = 0.0001D+00
  nlband = 0
  node = 0
  np = 0
  nquad1 = 3
  nquad2 = 7
  nrow = 0
  nside = 0
  nx = 0
  ny = 0
  p = 0.0D+00
  penalty1 = 0.0D+00
  penalty2 = 1.0D+00
  phi = 0.0D+00
  pnorm_l2 = 0.0D+00
  pnorm_lmax = 0.0D+00
  region = '??'
  res = 0.0D+00     
  nu_inv = 0.0D+00
  side_basis = 0
  side_elem = 0
  side_eqn = '???'
  side_etam = 0.0D+00
  side_etap = 0.0D+00
  side_indx = 0
  side_xsim = 0.0D+00
  side_xsip = 0.0D+00
  solver = 'GAUSS'
  squad1 = 0.0D+00
  uvnorm_l2 = 0.0D+00
  uvnorm_lmax = 0.0D+00
  wquad1 = 0.0D+00
  wquad2 = 0.0D+00
  xc = 0.0D+00
  xsiquad = 0.0D+00
  yc = 0.0D+00

  return
end
subroutine i4vec_merge_a ( na, a, nb, b, nc, c )

!*****************************************************************************80
!
!! I4VEC_MERGE_A merges two ascending sorted integer arrays.
!
!  Discussion:
!
!    The elements of A and B should be sorted in ascending order.
!
!    The elements in the output array C will also be in ascending order,
!    and unique.
!
!    The output vector C may share storage with A or B.
!
!  Modified:
!
!    17 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer NA, the dimension of A.
!
!    Input, integer A(NA), the first sorted array.
!
!    Input, integer NB, the dimension of B.
!
!    Input, integer B(NB), the second sorted array.
!
!    Output, integer NC, the number of elements in the output array.
!    Note that C should usually be dimensioned at least NA+NB in the
!    calling routine.
!
!    Output, integer C(NC), the merged unique sorted array.
!
  implicit none

  integer na
  integer nb

  integer a(na)
  integer b(nb)
  integer c(na+nb)
  integer d(na+nb)
  integer j
  integer ja
  integer jb
  integer na2
  integer nb2
  integer nc
  integer order

  na2 = na
  nb2 = nb

  ja = 0
  jb = 0
  nc = 0

  call i4vec_order_type ( na2, a, order )

  if ( order < 0 .or. 2 < order ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'I4VEC_MERGE - Fatal error!'
    write ( *, '(a)' ) '  The input array A is not ascending sorted!'
    stop
  end if

  call i4vec_order_type ( nb2, b, order )

  if ( order < 0 .or. 2 < order ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'I4VEC_MERGE - Fatal error!'
    write ( *, '(a)' ) '  The input array B is not ascending sorted!'
    stop
  end if

  do
!
!  If we've used up all the entries of A, stick the rest of B on the end.
!
    if ( na2 <= ja ) then

      do j = 1, nb2 - jb
        jb = jb + 1
        if ( nc == 0 ) then
          nc = nc + 1
          d(nc) = b(jb)
        else if ( d(nc) < b(jb) ) then
          nc = nc + 1
          d(nc) = b(jb)
        end if
      end do

      c(1:nc) = d(1:nc)

      exit
!
!  If we've used up all the entries of B, stick the rest of A on the end.
!
    else if ( nb2 <= jb ) then

      do j = 1, na2 - ja
        ja = ja + 1
        if ( nc == 0 ) then
          nc = nc + 1
          d(nc) = a(ja)
        else if ( d(nc) < a(ja) ) then
          nc = nc + 1
          d(nc) = a(ja)
        end if
      end do

      c(1:nc) = d(1:nc)

      exit
!
!  Otherwise, if the next entry of A is smaller, that's our candidate.
!
    else if ( a(ja+1) <= b(jb+1) ) then

      ja = ja + 1
      if ( nc == 0 ) then
        nc = nc + 1
        d(nc) = a(ja)
      else if ( d(nc) < a(ja) ) then
        nc = nc + 1
        d(nc) = a(ja)
      end if
!
!  ...or if the next entry of B is the smaller, consider that.
!
    else

      jb = jb + 1
      if ( nc == 0 ) then
        nc = nc + 1
        d(nc) = b(jb)
      else if ( d(nc) < b(jb) ) then
        nc = nc + 1
        d(nc) = b(jb)
      end if
    end if

  end do

  return
end
subroutine i4vec_order_type ( n, a, order )

!*****************************************************************************80
!
!! I4VEC_ORDER_TYPE determines if an integer array is (non)strictly ascending/descending.
!
!  Modified:
!
!    17 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of entries of the array.
!
!    Input, integer A(N), the array to be checked.
!
!    Output, integer ORDER, order indicator:
!    -1, no discernable order;
!    0, all entries are equal;
!    1, ascending order;
!    2, strictly ascending order;
!    3, descending order;
!    4, strictly descending order.
!
  implicit none

  integer n

  integer a(n)
  integer i
  integer order
!
!  Search for the first value not equal to A(1).
!
  i = 1

  do

    i = i + 1

    if ( n < i ) then
      order = 0
      return
    end if

    if ( a(1) < a(i) ) then

      if ( i == 2 ) then
        order = 2
      else
        order = 1
      end if

      exit

    else if ( a(i) < a(1) ) then

      if ( i == 2 ) then
        order = 4
      else
        order = 3
      end if

      exit

    end if

  end do
!
!  Now we have a "direction".  Examine subsequent entries.
!
  do while ( i < n )

    i = i + 1

    if ( order == 1 ) then

      if ( a(i) < a(i-1) ) then
        order = -1
        exit
      end if

    else if ( order == 2 ) then

      if ( a(i) < a(i-1) ) then
        order = -1
        exit
      else if ( a(i) == a(i-1) ) then
        order = 1
      end if

    else if ( order == 3 ) then

      if ( a(i-1) < a(i) ) then
        order = -1
        exit
      end if

    else if ( order == 4 ) then

      if ( a(i-1) < a(i) ) then
        order = -1
        exit
      else if ( a(i) == a(i-1) ) then
        order = 3
      end if

    end if

  end do

  return
end
subroutine i4vec_sort_bubble_a ( n, a )

!*****************************************************************************80
!
!! I4VEC_SORT_BUBBLE_A ascending sorts an integer array using bubble sort.
!
!  Modified:
!
!    11 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of entries in the array.
!
!    Input/output, integer A(N).
!    On input, the array to be sorted;
!    On output, the array has been sorted.
!
  implicit none
!
  integer n

  integer a(n)
  integer i
  integer j
  integer temp

  do i = 1, n-1
    do j = i+1, n
      if ( a(j) < a(i) ) then
        temp = a(i)
        a(i) = a(j)
        a(j) = temp
      end if
    end do
  end do

  return
end
subroutine node_nabor_print ( np, node_nabor, node_nabor_max, node_nabor_num )

!*****************************************************************************80
!
!! NODE_NABOR_PRINT prints the node neighbor array.
!
!  Modified:
!
!    08 May 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer NP, the number of nodes.
!
!    Input, integer NODE_NABOR(NODE_NABOR_MAX,NP), the node neigbors.
!
!    Input, integer NODE_NABOR_MAX, the maximum number of node neighbors.
!
!    Input, integer NODE_NABOR_NUM(NP), the number of node neighbors.
!
  implicit none

  integer node_nabor_max
  integer np

  integer i
  integer j
  integer node_nabor(node_nabor_max,np)
  integer node_nabor_num(np)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Node Neighbor Array'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Total number of node neighbors = ',  &
    sum ( node_nabor_num )
  write ( *, '(a)' ) ' '
  do i = 1, np
    write ( *, '(20i4)' ) i, ( node_nabor(j,i), j = 1, node_nabor_num(i) )
  end do

  return
end
subroutine node_nabor_set ( nelem, node, np, node_nabor, node_nabor_max, &
  node_nabor_num )

!*****************************************************************************80
!
!! NODE_NABOR_SET sets up the node neighbor array.
!
!  Discussion:
!
!    The node neighbor array allows us to quickly find out, for each node,
!    the nodes that are its "neighbors", that is, which appear together
!    in some element.  For our grid, there should be at most 18 neighbor 
!    nodes, but we include the node as its own neighbor, for a maximum of 19.
!
!    The computation is carried out in a straightforward way.  Two nodes
!    are neighbors only if they appear in the same element.  So we simply
!    look at each node, and add every node in the element to the list of
!    neighbors of all the other nodes in the element.
!
!    NODE_NABOR(K,I) = J means, if J is not zero, that global node J is
!    the K-th neighbor of node I.
!
!  Modified:
!
!    09 May 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer NELEM, the number of elements.
!
!    Input, integer NODE(6,NELEM), the nodes in each element.
!
!    Input, integer NP, the number of nodes.
!
!    Output, integer NODE_NABOR(NABOR_MAX,NP), the node neigbors.
!
!    Input, integer NODE_NABOR_MAX, the maximum number of node neighbors.
!
!    Output, integer NODE_NABOR_NUM(NP), the number of node neighbors.
!
  implicit none

  integer node_nabor_max
  integer nelem
  integer np

  integer elem
  integer na
  integer node_nabor(node_nabor_max,np)
  integer node_nabor_num(np)
  integer nb
  integer nc
  integer node(6,nelem)
  integer node_global
  integer node_local
  integer temp(6)
!
!  Initialize.
!
  node_nabor(1:node_nabor_max,1:np) = 0
  node_nabor_num(1:np) = 0
!
!  Start by listing every node as its own neighbor.
!
  do node_global = 1, np
    node_nabor(1,node_global) = node_global
  end do

  node_nabor_num(1:np) = 1
!
!  Consider each element ELEM.
!
  do elem = 1, nelem
!
!  Consider each local node in the element.
!
    temp = node(1:6,elem)

    call i4vec_sort_bubble_a ( 6, temp )

    do node_local = 1, 6

      node_global = node(node_local,elem)
!
!  Merge the nodes in this element into the neighbor list of the node.
!
      na = node_nabor_num(node_global)
      nb = 6

      call i4vec_merge_a ( na, node_nabor(1,node_global), &
                        nb, temp, &
                        nc, node_nabor(1,node_global) )

      node_nabor_num(node_global) = nc

    end do

  end do

  return
end
subroutine newton ( a, bc_tag, detmap, eqn, g, ierror, indx, ipivot, jac, &
  maxelm, maxeqn, maxnp, maxquad1, maxquad2, maxside, ncol, nelem, neqn, &
  newton_max, newton_stutter, newton_tolerance, nlband, node, np, nquad1, &
  nquad2, nrow, nside, penalty1, penalty2, phi, region, region_ymax, res, &
  res2, nu_inv, side_basis, side_elem, side_eqn, side_etam, side_etap, &
  side_indx, side_xsim, side_xsip, solver, squad1, wquad1, wquad2, xc, yc )

!*****************************************************************************80
!
!! NEWTON applies Newton iteration, seeking a solution of FX(G) = 0.
!
!  Discussion:
!
!    NEWTON is given an initial estimate of the solution of the nonlinear
!    state equations in G, and seeks a better solution.
!
!    The exact solution would have a zero residual, as computed by
!    the routine FX.  NEWTON uses Newton's method to seek a solution
!    whose maximum residual is no more than NEWTON_TOLERANCE.  The routine FP
!    is used to compute the Jacobian of the residual functions.
!
!  Modified:
!
!    23 December 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Workspace, real A(NROW,MAXEQN), is used to store
!    the jacobian matrix, and then its LU factors.
!
!    Input, real DETMAP(MAXQUAD2,MAXELM), the reference map determinant.
!
!    Input, character ( len = 3 ) EQN(MAXEQN), the type of each equation.
!
!    Input/output, real G(MAXEQN).
!
!    On input, some estimate for the solution.  If no estimate
!    is known, set G to zero.
!
!    On output, G is the improved solution computed by Newton's method.
!
!    Output, integer IERROR, error flag.
!    0, no error occurred.
!    1, an error occurred, and the improved solution could not be computed.
!
!    Input, integer INDX(3,MAXNP), mapping from nodes to degrees of freedom.
!
!    Workspace, integer IPIVOT(MAXEQN), pivot space needed by the
!    matrix factorization and solving routines.
!
!    Input, integer MAXELM, the maximum number of elements.
!
!    Input, integer MAXNP, the maximum number of nodes.
!
!    Input, integer NELEM, the number of elements.
! 
!    Input, integer NEQN, the number of finite element equations.
! 
!    Input, integer NEWTON_MAX, the maximum number of Newton iterations.
!
!    Input, integer NEWTON_STUTTER, determines how many times the jacobian
!    can be used before it must be re-evaluated.  A value of 1 means the
!    jacobian is never re-used.  A value of 2 means that it can be used
!    two times, and so on.
!
!    Input, real NEWTON_TOLERANCE, the Newton tolerance.
!    NEWTON is asked to find an approximate solution so that
!    the maximum absolute value of all the residuals is no more
!    than NEWTON_TOLERANCE.  A value such as 10E-7 is often reasonable,
!    though this depends on the actual equations being solved.
!
!    Input, integer NLBAND, the lower bandwidth of the matrix A.
! 
!    Input, integer NODE(6,MAXELM), the nodes that make up each element.
! 
!    Input, integer NP, the number of nodes.
! 
!    Input, integer NROW, the number of rows need to store the matrix A.
!
!    Input, real PHI(MAXQUAD2,6,6,MAXELM), basis function values.  
!
!    Input, character ( len = 20 ) REGION, the flow problem.
!
!    Workspace, real RES(MAXEQN), the residual.
!
!    Input, real NU_INV, the inverse viscosity.
!
!    Input, real XC(MAXNP), the X coordinates of the nodes.
!
!    Input, real YC(MAXNP), the Y coordinates of the nodes.
!
  implicit none

  integer maxelm
  integer maxeqn
  integer maxnp
  integer maxquad1
  integer maxquad2
  integer maxside
  integer ncol
  integer nrow

  real ( kind = 8 ) a(nrow,ncol)
  integer bc_tag(maxnp)
  real ( kind = 8 ) detmap(maxquad2,maxelm)
  real ( kind = 8 ) dmax
  character ( len = 3 ) eqn(maxeqn)
  real ( kind = 8 ) g(maxeqn)
  integer i
  integer idmax
  integer ierror
  integer indx(3,maxnp)
  integer info
  integer ipivot(maxeqn)
  integer iprint
  integer irmax
  integer it_jac
  integer it_sol
  integer ixmax
  integer jac
  integer job
  integer max_jac
  real ( kind = 8 ) navier
  real ( kind = 8 ) navier_fp
  real ( kind = 8 ) navier_fx
  integer nelem
  integer neqn
  integer newton_max
  integer newton_stutter
  real ( kind = 8 ) newton_tolerance
  integer nlband
  integer node(6,maxelm)
  integer np
  integer nquad1
  integer nquad2
  integer nside
  integer numnew
  real ( kind = 8 ) penalty1
  real ( kind = 8 ) penalty2
  real ( kind = 8 ) phi(maxquad2,6,6,maxelm)
  character ( len = 20 ) region
  real ( kind = 8 ) region_ymax
  real ( kind = 8 ) res(maxeqn)
  real ( kind = 8 ) res2(maxeqn)
  real ( kind = 8 ) nu_inv
  real ( kind = 8 ) rmax
  real ( kind = 8 ) rmax0
  logical s_eqi
  integer side_basis(3,maxside)
  integer side_elem(maxside)
  character ( len = 3 ) side_eqn(maxside)
  real ( kind = 8 ) side_etam(maxside)
  real ( kind = 8 ) side_etap(maxside)
  integer side_indx(3,maxside)
  real ( kind = 8 ) side_xsim(maxside)
  real ( kind = 8 ) side_xsip(maxside)
  character ( len = 20 ) solver
  real ( kind = 8 ) squad1(maxquad1)
  real ( kind = 8 ) wquad1(maxquad1)
  real ( kind = 8 ) wquad2(maxquad2)
  real ( kind = 8 ) xc(maxnp)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmax0
  real ( kind = 8 ) yc(maxnp)

  ierror = 0

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'Newton iteration:'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    ||X||           ||FX||           ||Delta X||' &
    // '  Navier'
  write ( *, '(a)' ) ' '
!
!  Compute the max-norm of the initial X value.
!
  call r4vec_amax ( neqn, g, xmax, ixmax )
  xmax0 = xmax
!
!  Evaluate the residual RES of the initial X value.
!
  navier_fx = 1.0D+00
  navier = navier_fx

  call fx ( bc_tag, detmap, eqn, g, indx, maxelm, maxeqn, maxnp, maxquad1, &
    maxquad2, maxside, navier, nelem, neqn, node, np, nquad1, nquad2, nside, &
    penalty1, penalty2, phi, region, region_ymax, res, nu_inv, side_basis, &
    side_elem, side_eqn, side_etam, side_etap, side_indx, side_xsim, &
    side_xsip, squad1, wquad1, wquad2, xc, yc )
 
  call r4vec_amax ( neqn, res, rmax, irmax )
  rmax0 = rmax

  write ( *, '(g12.4,i8,g12.4,i8,18x,f6.3)' ) xmax, ixmax, rmax, irmax, &
    navier_fx
!
!  Carry out NEWTON_MAX steps of Newton iteration.
!
  newton_stutter = max ( newton_stutter, 1 )
  newton_stutter = min ( newton_stutter, newton_max )

  max_jac = newton_max / newton_stutter

  numnew = 0
!
!  Evaluate the system matrix and factor it.
!
!  The formula for NAVIER_FP here is "arbitrary".
!  We really want NAVIER_FP to be 1, but if the system is highly
!  nonlinear, and the iterate is far from the solution, this can
!  make the iteration fail.  So we start with NAVIER_FP close to 0,
!  and work up to 1.  If this doesn't work, but you really believe
!  the iteration has a chance to converge, you can try increasing
!  MAX_NEW (to take more steps), or altering the formula for
!  NAVIER_FP so that it rises more slowly.
!
  do it_jac = 1, max_jac

    if ( .false. ) then
      navier_fp = real ( it_jac, kind = 8 ) / real ( max_jac, kind = 8 )
    else
      navier_fp = 1.0D+00
    end if

    navier = navier_fp

    if ( jac == 0 ) then

      call fp ( a, bc_tag, detmap, eqn, g, indx, maxelm, maxeqn, maxnp, &
        maxquad1, maxquad2, maxside, navier, ncol, nelem, neqn, nlband, &
        node, np, nquad1, nquad2, nrow, nside, penalty1, penalty2, phi, &
        region, nu_inv, side_basis, side_elem, side_eqn, side_etam, &
        side_etap, side_indx, side_xsim, side_xsip, solver, squad1, wquad1, & 
        wquad2, xc, yc )
 
    else

      call fp_dif ( a, bc_tag, detmap, eqn, g, indx, maxelm, maxeqn, maxnp, &
        maxquad1, maxquad2, maxside, navier, ncol, nelem, neqn, nlband, node, &
        np, nquad1, nquad2, nrow, nside, penalty1, penalty2, phi, region, &
        region_ymax, res, res2, nu_inv, side_basis, side_elem, side_eqn, &
        side_etam, side_etap, side_indx, side_xsim, side_xsip, solver, squad1, & 
        wquad1, wquad2, xc, yc )

    end if
!
!  For debugging, it is sometimes desirable to print the jacobian matrix.
!
    iprint = 1

    if ( iprint == 1 ) then
      if ( s_eqi ( solver, 'GAUSS' ) ) then
        call sgb_print ( a, nrow, neqn, neqn, nlband, nlband, &
          1, neqn, 1, neqn )
      else if ( s_eqi ( solver, 'CGS' ) ) then
      end if
      stop
    end if

    if ( s_eqi ( solver, 'GAUSS' ) ) then

      call sgb_fa ( a, nrow, neqn, nlband, nlband, ipivot, info )

      if ( info /= 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'NEWTON - Warning!'
        write ( *, '(a)' ) '  The jacobian is singular.'
        write ( *, '(a,i8)' ) '  SGB_FA returns INFO=', info
        ierror = 1
        return
      end if

    end if
!
!  Solve the linear system with a fixed matrix NEWTON_STUTTER times.
!
    do it_sol = 1, newton_stutter

      numnew = numnew + 1

      if ( s_eqi ( solver, 'GAUSS' ) ) then

        job = 0
        call sgb_sl ( a, nrow, neqn, nlband, nlband, ipivot, res, job )

      else if ( s_eqi ( solver, 'CGS' ) ) then

      end if
 
      call r4vec_amax ( neqn, res, dmax, idmax )
      write ( *, '(36x,g12.4,i8,f6.3)' ) dmax, idmax, navier_fp

      g(1:neqn) = g(1:neqn) - res(1:neqn)

      call r4vec_amax ( neqn, g, xmax, ixmax )

      navier_fx = 1.0D+00
      navier = navier_fx

      call fx ( bc_tag, detmap, eqn, g, indx, maxelm, maxeqn, maxnp, maxquad1, &
        maxquad2, maxside, navier, nelem, neqn, node, np, nquad1, nquad2, &
        nside, penalty1, penalty2, phi, region, region_ymax, res, nu_inv, &
        side_basis, side_elem, side_eqn, side_etam, side_etap, side_indx, &
        side_xsim, side_xsip, squad1, wquad1, wquad2, xc, yc )
 
      call r4vec_amax ( neqn, res, rmax, irmax )
      write ( *, '(g12.4,i8,g12.4,i8,18x,f6.3)' ) xmax, ixmax, rmax, irmax, &
        navier_fx
!
!  Accept or reject the iterate.
!
      if ( rmax <= newton_tolerance ) then
        return
      end if

      if ( 10.0D+00 * ( rmax0 + newton_tolerance ) < rmax .and. &
          1 < numnew ) then
        ierror = 1
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'NEWTON - Warning!'
        write ( *, '(a,i8)' ) '  Residual too big on step ', numnew
        write ( *, '(a,g14.6)' ) '  The final stepsize was        ', dmax
        write ( *, '(a,g14.6)' ) '  The initial X norm was        ', xmax0
        write ( *, '(a,g14.6)' ) '  The final X norm was          ', xmax
        write ( *, '(a,g14.6)' ) '  The initial residual norm was ', rmax0
        write ( *, '(a,g14.6)' ) '  The final residual norm was   ', rmax
        return
      end if

    end do
 
  end do
!
!  The iteration has failed to converge, or may actually
!  have been terminated early.
!
  ierror = 1
 
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'NEWTON - Warning!'
  write ( *, '(a,i8,a)' ) '  No convergence after ', numnew, ' steps.'
  write ( *, '(a,g14.6)' ) '  The final stepsize was        ', dmax
  write ( *, '(a,g14.6)' ) '  The initial X norm was        ', xmax0
  write ( *, '(a,g14.6)' ) '  The final X norm was          ', xmax
  write ( *, '(a,g14.6)' ) '  The initial residual norm was ', rmax0
  write ( *, '(a,g14.6)' ) '  The final residual norm was   ', rmax
 
  return
end 
subroutine node_write ( file_name, g, ierror, indx, maxeqn, maxnp, np, p, &
  xc, yc )

!*****************************************************************************80
!
!! NODE_WRITE writes solution information to a file.
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
!    Input, real G(MAXEQN), the current solution vector.
!
!    Output, integer IERROR, error flag.
!    0, no error occurred.
!    nonzero, an error occurred.
!
!    Input, integer MAXNP, the maximum number of nodes.
!
!    Input, integer NP, the number of nodes.
!
!    Input, real P(MAXNP), the pressure at each node.
!
!    Input, real XC(MAXNP), YC(MAXNP), the coordinates of the nodes.
!
  implicit none

  integer maxeqn
  integer maxnp

  character ( len = * ) file_name
  real ( kind = 8 ) g(maxeqn)
  integer i
  integer ierror
  integer indx(3,maxnp)
  integer ios
  integer iunit
  integer np
  real ( kind = 8 ) p(maxnp)
  real ( kind = 8 ) xc(maxnp)
  real ( kind = 8 ) yc(maxnp)

  ierror = 0

  call get_unit ( iunit )

  open ( unit = iunit, file = file_name, status = 'replace', &
    iostat = ios )

  if ( ios /= 0 ) then
    ierror = ios
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'NODE_WRITE - Warning!'
    write ( *, '(a)' ) '  Could not open the node file.'
    return
  end if

  do i = 1, np
    write ( iunit, '(2g12.4,3g14.6)' ) &
      xc(i), yc(i), g(indx(1,i)), g(indx(2,i)), p(i)
  end do

  close ( unit = iunit )
  
  return
end     
subroutine press_interp ( g, indx, maxelm, maxeqn, maxnp, nelem, node, p )

!*****************************************************************************80
!
!! PRESS_INTERP interpolates pressure values at non-pressure nodes.
!
!  Modified:
!
!    28 July 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real G(MAXEQN), the flow solution coefficients.
!
!    Input, integer INDX(3,MAXNP), mapping from nodes to degrees of freedom.
!
!    Input, integer MAXELM, the maximum number of elements.
!
!    Input, integer MAXNP, the maximum number of nodes.
!
!    Input, integer NELEM, the number of elements.
! 
!    Input, integer NODE(6,MAXELM), the nodes that make up each element.
! 
!    Output, real P(MAXNP), the interpolated pressures.
!
  implicit none

  integer maxelm
  integer maxeqn
  integer maxnp

  real ( kind = 8 ) dqdeta
  real ( kind = 8 ) dqdxsi
  real ( kind = 8 ) eta
  real ( kind = 8 ) g(maxeqn)
  integer ielem
  integer in1
  integer in2
  integer in3
  integer in4
  integer in5
  integer in6
  integer indx(3,maxnp)
  integer iq
  integer nelem
  integer node(6,maxelm)
  real ( kind = 8 ) p(maxnp)
  real ( kind = 8 ) pp(6)
  real ( kind = 8 ) q
  real ( kind = 8 ) xsi
!
!  For each element,...
!
  do ielem = 1, nelem
!
!  Get the six global node numbers.
!
    in1 = node(1,ielem)
    in2 = node(2,ielem)
    in3 = node(3,ielem)
    in4 = node(4,ielem)
    in5 = node(5,ielem)
    in6 = node(6,ielem)
!
!  Read off the three computed pressure values.
!
    pp(1) = g ( indx(3,in1) )
    pp(2) = g ( indx(3,in2) )
    pp(3) = g ( indx(3,in3) )
!
!  Linear interpolation of the pressure at non-pressure nodes (or
!  anywhere within the element) can be accomplished simply by evaluating 
!  the linear pressure interpolating function defined by the finite 
!  element coefficients.
!
    xsi = 0.5D+00
    eta = 0.0D+00

    pp(4) = 0.0D+00
    do iq = 1, 3
      call ref_bf_l3 ( q, dqdeta, dqdxsi, eta, iq, xsi )
      pp(4) = pp(4) + q * pp(iq)
    end do

    xsi = 0.5D+00
    eta = 0.5D+00

    pp(5) = 0.0D+00
    do iq = 1, 3
      call ref_bf_l3 ( q, dqdeta, dqdxsi, eta, iq, xsi )
      pp(5) = pp(5) + q * pp(iq)
    end do

    xsi = 0.0D+00
    eta = 0.5D+00

    pp(6) = 0.0D+00
    do iq = 1, 3
      call ref_bf_l3 ( q, dqdeta, dqdxsi, eta, iq, xsi )
      pp(6) = pp(6) + q * pp(iq)
    end do
!
!  Copy the local data into the global P array.
!
    p(in1) = pp(1)
    p(in2) = pp(2)
    p(in3) = pp(3)
    p(in4) = pp(4)
    p(in5) = pp(5)
    p(in6) = pp(6)

  end do

  return
end
subroutine r4vec_amax ( n, x, xmax, ixmax )

!*****************************************************************************80
!
!! R4VEC_AMAX returns the maximum absolute value in an R4VEC.
!
!  Modified:
!
!    22 December 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of entries in the array.
!
!    Input, real X(N), the array.
!
!    Output, real XMAX, the largest absolute value of the entries in
!    the array.
!
!    Output, integer IXMAX, the index of the entry of largest absolute value.
!
  implicit none

  integer n

  integer i
  integer ixmax
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xmax

  if ( n <= 0 ) then

    xmax = 0.0D+00
    ixmax = 0

  else

    xmax = abs ( x(1) )
    ixmax = 1

    do i = 2, n

      if ( xmax < abs ( x(i) ) ) then
        xmax = abs ( x(i) )
        ixmax = i
      end if

    end do

  end if

  return
end
subroutine ref_bf_l3 ( q, dqdeta, dqdxsi, eta, iq, xsi )

!*****************************************************************************80
!
!! REF_BF_L3 evaluates a reference element linear basis function.
!
!  Diagram:
!
!      ^
!      |
!    1 +    2
!      |    |\
!    ETA   |    | \
!      |    |  \
!    0 +    3---1
!      |
!      +----+---+--->
!           0   1
!
!          XSI
!
!  Discussion:
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
!    Output, real Q, DQDETA, DQDXSI, the value of the basis
!    function, and its derivatives with respect to ETA and XSI, at
!    the point with reference coordinates (ETA,XSI).
!
!    Input, real ETA, XSI, the local coordinates of the
!    point at which the basis information is desired.
!
!    Input, integer IQ, the basis function, between 1 and 3.
!
  implicit none

  real ( kind = 8 ) dqdeta
  real ( kind = 8 ) dqdxsi
  real ( kind = 8 ) eta
  integer iq
  real ( kind = 8 ) q
  real ( kind = 8 ) xsi

  if ( iq == 1 ) then

    q = xsi
    dqdxsi = 1.0D+00
    dqdeta =  0.0D+00

  else if ( iq == 2 ) then

    q = eta
    dqdxsi = 0.0D+00
    dqdeta = 1.0D+00

  else if ( iq == 3 ) then

    q = 1.0D+00 - xsi - eta
    dqdxsi = - 1.0D+00
    dqdeta = - 1.0D+00

  else

    q = 0.0D+00
    dqdxsi = 0.0D+00
    dqdeta = 0.0D+00

  end if
 
  return
end
subroutine ref_bf_q6 ( w, dwdeta, dwdxsi, eta, iq, xsi )

!*****************************************************************************80
!
!! REF_BF_Q6 evaluates shape functions for a 6 node triangle.
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
!    Output, real W, DWDETA, DWDXSI, the value of the basis function, and 
!    its derivatives with respect to ETA and XSI, at the point (XSI,ETA).
!
!    Input, real ETA, the ETA coordinate of the point.
!
!    Input, integer IQ, the basis function, between 1 and 6.
!
!    Input, real XSI, the XSI coordinate of the point.
!
  implicit none

  real ( kind = 8 ) dwdeta
  real ( kind = 8 ) dwdxsi
  real ( kind = 8 ) eta
  integer iq
  real ( kind = 8 ) w
  real ( kind = 8 ) xsi

  if ( iq == 1 ) then

    w =        2.0D+00 * xsi * ( xsi - 0.5D+00 )
    dwdxsi = - 1.0D+00 + 4.0D+00 * xsi
    dwdeta =   0.0D+00

  else if ( iq == 2 ) then

    w =        2.0D+00 * eta * ( eta - 0.5D+00 )
    dwdxsi =   0.0D+00
    dwdeta = - 1.0D+00           + 4.0D+00 * eta

  else if ( iq == 3 ) then

    w =        2.0D+00 * ( 1.0D+00 - xsi - eta ) * ( 0.5D+00 - xsi - eta )
    dwdxsi = - 3.0D+00 + 4.0D+00 * xsi + 4.0D+00 * eta
    dwdeta = - 3.0D+00 + 4.0D+00 * xsi + 4.0D+00 * eta

  else if ( iq == 4 ) then

    w =        4.0D+00 * xsi * ( 1.0D+00 - xsi - eta )
    dwdxsi =   4.0D+00 - 8.0D+00 * xsi - 4.0D+00 * eta
    dwdeta =       - 4.0D+00 * xsi

  else if ( iq == 5 ) then

    w =      4.0D+00 * xsi * eta
    dwdxsi = 4.0D+00       * eta
    dwdeta = 4.0D+00 * xsi

  else if ( iq == 6 ) then

    w =      4.0D+00 * eta * ( 1.0D+00 - xsi - eta )
    dwdxsi =                 - 4.0D+00 * eta
    dwdeta = 4.0D+00 - 4.0D+00 * xsi - 8.0D+00 * eta

  else

    w = 0.0D+00
    dwdxsi = 0.0D+00
    dwdeta = 0.0D+00

  end if

  return
end
subroutine ref_map_q6 ( qdata, a, b, c, d, e, f )

!*****************************************************************************80
!
!! REF_MAP_Q6 returns the interpolation map for data on a 6 node triangle.
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

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) d
  real ( kind = 8 ) e
  real ( kind = 8 ) f
  real ( kind = 8 ) qdata(6)

  a =  2.0D+00 * qdata(1) - 4.0D+00 * qdata(4) + 2.0D+00 * qdata(3)
  b =  4.0D+00 * qdata(3) - 4.0D+00 * qdata(4) + 4.0D+00 * qdata(5) - 4.0D+00 * qdata(6)
  c =  2.0D+00 * qdata(2) - 4.0D+00 * qdata(6) + 2.0D+00 * qdata(3)
  d =      - qdata(1) + 4.0D+00 * qdata(4) - 3.0D+00 * qdata(3)
  e =      - qdata(2) + 4.0D+00 * qdata(6) - 3.0D+00 * qdata(3)
  f =        qdata(3)

  return
end
subroutine ref_quad1 ( maxquad1, nquad1, squad1, wquad1 )

!*****************************************************************************80
!
!! REF_QUAD1 sets abscissas and wquad1s for 1D Gauss-Legendre quadrature.
!
!  Integration interval:
!
!    [ -1, 1 ]
!
!  Weight function:
!
!    1.
!
!  Integral to approximate:
!
!    INTEGRAL ( -1 <=  X <= 1 ) F(X) dX.
!
!  Approximate integral:
!
!    SUM ( I = 1 to NORDER ) WEIGHT(I) * F ( XTAB(I) ).
!
!  Precision:
!
!    The quadrature rule will integrate exactly all polynomials up to
!    X**(2*NORDER-1).
!
!  Note:
!
!    The abscissas of the rule are the zeroes of the Legendre polynomial
!    P(NORDER)(X).
!
!    The integral produced by a Gauss-Legendre rule is equal to the
!    integral of the unique polynomial of degree NORDER-1 which
!    agrees with the function at the NORDER abscissas of the rule.
!
!  Reference:
!
!    Abramowitz and Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964.
!
!    Vladimir Krylov,
!    Approximate Calculation of Integrals,
!    MacMillan, 1962.
!
!    Arthur Stroud and Don Secrest,
!    Gaussian Quadrature Formulas,
!    Prentice Hall, 1966.
!
!    Daniel Zwillinger, editor,
!    Standard Mathematical Tables and Formulae,
!    30th Edition,
!    CRC Press, 1996.
!
!  Modified:
!
!    02 December 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer MAXQUAD1, the maximum size of SQUAD1 and WQUAD1.
!
!    Input, integer NQUAD1, the order of the rule, between 1 and 7.
!
!    Output, real SQUAD1(NQUAD1), the abscissas of the rule.
!
!    Output, real WQUAD1(NQUAD1), the weights of the rule.
!    The weights are positive, symmetric and should sum to 2.
!
  implicit none

  integer maxquad1

  integer nquad1
  real ( kind = 8 ) squad1(maxquad1)
  real ( kind = 8 ) wquad1(maxquad1)

  if ( nquad1 == 1 ) then
 
    squad1(1) =   0.0D+00
 
    wquad1(1) = 2.0D+00
 
  else if ( nquad1 == 2 ) then
 
    squad1(1) = - 0.577350269189625764509148780502D+00
    squad1(2) =   0.577350269189625764509148780502D+00
 
    wquad1(1) = 1.0D+00
    wquad1(2) = 1.0D+00
 
  else if ( nquad1 == 3 ) then
 
    squad1(1) = - 0.774596669241483377035853079956D+00
    squad1(2) =   0.0D+00
    squad1(3) =   0.774596669241483377035853079956D+00
 
    wquad1(1) = 5.0D+00 / 9.0D+00
    wquad1(2) = 8.0D+00 / 9.0D+00
    wquad1(3) = 5.0D+00 / 9.0D+00
 
  else if ( nquad1 == 4 ) then
 
    squad1(1) = - 0.861136311594052575223946488893D+00
    squad1(2) = - 0.339981043584856264802665759103D+00
    squad1(3) =   0.339981043584856264802665759103D+00
    squad1(4) =   0.861136311594052575223946488893D+00
 
    wquad1(1) = 0.347854845137453857373063949222D+00
    wquad1(2) = 0.652145154862546142626936050778D+00
    wquad1(3) = 0.652145154862546142626936050778D+00
    wquad1(4) = 0.347854845137453857373063949222D+00
 
  else if ( nquad1 == 5 ) then

    squad1(1) = - 0.906179845938663992797626878299D+00
    squad1(2) = - 0.538469310105683091036314420700D+00
    squad1(3) =   0.0D+00
    squad1(4) =   0.538469310105683091036314420700D+00
    squad1(5) =   0.906179845938663992797626878299D+00
 
    wquad1(1) = 0.236926885056189087514264040720D+00
    wquad1(2) = 0.478628670499366468041291514836D+00
    wquad1(3) = 0.568888888888888888888888888889D+00
    wquad1(4) = 0.478628670499366468041291514836D+00
    wquad1(5) = 0.236926885056189087514264040720D+00
 
  else if ( nquad1 == 6 ) then
 
    squad1(1) = - 0.932469514203152027812301554494D+00
    squad1(2) = - 0.661209386466264513661399595020D+00
    squad1(3) = - 0.238619186083196908630501721681D+00
    squad1(4) =   0.238619186083196908630501721681D+00
    squad1(5) =   0.661209386466264513661399595020D+00
    squad1(6) =   0.932469514203152027812301554494D+00
 
    wquad1(1) = 0.171324492379170345040296142173D+00
    wquad1(2) = 0.360761573048138607569833513838D+00
    wquad1(3) = 0.467913934572691047389870343990D+00
    wquad1(4) = 0.467913934572691047389870343990D+00
    wquad1(5) = 0.360761573048138607569833513838D+00
    wquad1(6) = 0.171324492379170345040296142173D+00
 
  else if ( nquad1 == 7 ) then
 
    squad1(1) = - 0.949107912342758524526189684048D+00
    squad1(2) = - 0.741531185599394439863864773281D+00
    squad1(3) = - 0.405845151377397166906606412077D+00
    squad1(4) =   0.0D+00
    squad1(5) =   0.405845151377397166906606412077D+00
    squad1(6) =   0.741531185599394439863864773281D+00
    squad1(7) =   0.949107912342758524526189684048D+00
 
    wquad1(1) = 0.129484966168869693270611432679D+00
    wquad1(2) = 0.279705391489276667901467771424D+00
    wquad1(3) = 0.381830050505118944950369775489D+00
    wquad1(4) = 0.417959183673469387755102040816D+00
    wquad1(5) = 0.381830050505118944950369775489D+00
    wquad1(6) = 0.279705391489276667901467771424D+00
    wquad1(7) = 0.129484966168869693270611432679D+00

  else

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'REF_QUAD1 - Fatal error!'
    write ( *, '(a,i8)' ) '  Input NQUAD1 = ', nquad1
    write ( *, '(a)' ) '  NQUAD1 must be between 1 and 7.'
    stop

  end if

  return
end
subroutine ref_quad2 ( etaquad, maxquad2, nquad2, wquad2, xsiquad )

!*****************************************************************************80
!
!! REF_QUAD2 sets up the 2D reference triangle quadrature rule.
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
!    Output, real ETAQUAD(MAXQUAD2), the ETA quadrature coordinates.
! 
!    Input, integer MAXQUAD2, the maximum number of 2D quadrature points.
!
!    Input, integer NQUAD2, the number of 2D quadrature points.
!
!    Output, real WQUAD2(MAXQUAD2), the 2D quadrature weights.
!
!    Output, real XSIQUAD(MAXQUAD2), the XSI quadrature coordinates.
!
  implicit none

  integer maxquad2

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) etaquad(maxquad2)
  integer i
  integer nquad2
  real ( kind = 8 ) w
  real ( kind = 8 ) wquad2(maxquad2)
  real ( kind = 8 ) xsiquad(maxquad2)

  if ( nquad2 == 3 ) then

    wquad2(1:3) = 1.0D+00 / 3.0D+00

    xsiquad(1) = 0.5D+00
    etaquad(1) = 0.5D+00

    xsiquad(2) = 0.0D+00
    etaquad(2) = 0.5D+00

    xsiquad(3) = 0.5D+00
    etaquad(3) = 0.0D+00

  else if ( nquad2 == 7 ) then
 
    a = 1.0D+00 / 3.0D+00
    b = 1.0D+00 / 3.0D+00
    w = 0.225D+00

    xsiquad(1) = a
    etaquad(1) = b
    wquad2(1) = w

    a = (   9.0D+00 + 2.0D+00 * sqrt ( 15.0D+00 ) ) / 21.0D+00
    b = (   6.0D+00 -       sqrt ( 15.0D+00 ) ) / 21.0D+00
    w = ( 155.0D+00 -       sqrt ( 15.0D+00 ) ) / 1200.0D+00

    xsiquad(2) = a
    etaquad(2) = b
    wquad2(2) = w

    xsiquad(3) = b
    etaquad(3) = a
    wquad2(3) = w

    xsiquad(4) = b
    etaquad(4) = b
    wquad2(4) = w

    a = (   9.0D+00 - 2.0D+00 * sqrt ( 15.0D+00 ) ) / 21.0D+00
    b = (   6.0D+00 +       sqrt ( 15.0D+00 ) ) / 21.0D+00
    w = ( 155.0D+00 +       sqrt ( 15.0D+00 ) ) / 1200.0D+00

    xsiquad(5) = a
    etaquad(5) = b
    wquad2(5) = w

    xsiquad(6) = b
    etaquad(6) = a
    wquad2(6) = w

    xsiquad(7) = b
    etaquad(7) = b
    wquad2(7) = w

  else

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'REF_QUAD2 - Fatal error!'
    write ( *, '(a,i8)' ) '  Illegal value of NQUAD2 = ', nquad2
    write ( *, '(a)' ) '  Legal values are 3 and 7.'
    stop

  end if
 
  return
end
function s_eqi ( s1, s2 )

!*****************************************************************************80
!
!! S_EQI is a case insensitive comparison of two strings for equality.
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
!    Input, character ( len = * ) S1, S2, the strings to compare.
!
!    Output, logical S_EQI, the result of the comparison.
!
  implicit none

  character c1
  character c2
  integer i
  integer len1
  integer len2
  integer lenc
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
subroutine setbas ( detmap, etaquad, maxelm, maxnp, maxquad2, nelem, node, &
  nquad2, phi, xc, xsiquad, yc )

!*****************************************************************************80
!
!! SETBAS evaluates the basis functions at each quadrature point.  
!
!  Discussion:
!
!    The basis functions are computed and saved in this way for efficiency.
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
!    Output, real DETMAP(MAXQUAD2,MAXELM), the reference map determinant.
!
!    Input, real ETAQUAD(MAXQUAD2), the ETA quadrature coordinates.
!
!    Input, integer MAXELM, the maximum number of elements.
!
!    Input, integer MAXNP, the maximum number of nodes.
!
!    Input, integer MAXQUAD2, the maximum number of 2D quadrature points.
!
!    Input, integer NELEM, the number of elements.
!
!    Input, integer NODE(6,MAXELM), the nodes that make up each element.
!
!    Input, integer NP, the number of nodes.
!
!    Input, integer NQUAD2, the number of 2D quadrature points.
!
!    Output, real PHI(MAXQUAD2,6,6,MAXELM), basis function values.
!
!    Input, real XC(MAXNP), the X coordinates of the nodes.
!
!    Input, real XSIQUAD(MAXQUAD2), the XSI quadrature coordinates.
!
!    Input, real YC(MAXNP), the Y coordinates of the nodes.
!
  implicit none

  integer maxelm
  integer maxnp
  integer maxquad2

  real ( kind = 8 ) det
  real ( kind = 8 ) detadx
  real ( kind = 8 ) detady
  real ( kind = 8 ) detmap(maxquad2,maxelm)
  real ( kind = 8 ) dqdeta
  real ( kind = 8 ) dqdx
  real ( kind = 8 ) dqdxsi
  real ( kind = 8 ) dqdy
  real ( kind = 8 ) dwdeta
  real ( kind = 8 ) dwdx
  real ( kind = 8 ) dwdxsi
  real ( kind = 8 ) dwdy
  real ( kind = 8 ) dxsidx
  real ( kind = 8 ) dxsidy
  real ( kind = 8 ) eta
  real ( kind = 8 ) etaquad(maxquad2)
  integer ielem
  integer iq
  integer iquad
  integer nelem
  integer node(6,maxelm)
  integer nquad2
  real ( kind = 8 ) phi(maxquad2,6,6,maxelm)
  real ( kind = 8 ) q
  real ( kind = 8 ) w
  real ( kind = 8 ) xc(maxnp)
  real ( kind = 8 ) xsi
  real ( kind = 8 ) xsiquad(maxquad2)
  real ( kind = 8 ) yc(maxnp)
!
!  Consider a particular element,
!  and a particular quadrature point (XQ,YQ) in that element.
!
!  Compute, at (XQ,YQ), the local values of the jacobian matrix
!  and its determinant.
!
  do ielem = 1, nelem
 
    do iquad = 1, nquad2

      eta = etaquad(iquad)
      xsi = xsiquad(iquad)

      call trans_q6 ( det, detadx, detady, dxsidx, dxsidy, eta, &
        ielem, maxelm, maxnp, node, xc, xsi, yc )

      detmap(iquad,ielem) = det
!
!  Now consider each of the basis functions associated with a
!  node in the given element.
!
      do iq = 1, 3
 
        call ref_bf_l3 ( q, dqdeta, dqdxsi, eta, iq, xsi )

        dqdx = dqdxsi * dxsidx + dqdeta * detadx
        dqdy = dqdxsi * dxsidy + dqdeta * detady
 
        phi(iquad,iq,4,ielem) = q
        phi(iquad,iq,5,ielem) = dqdx
        phi(iquad,iq,6,ielem) = dqdy
 
      end do

      do iq = 1, 6
 
        call ref_bf_q6 ( w, dwdeta, dwdxsi, eta, iq, xsi )
 
        dwdx = dwdxsi * dxsidx + dwdeta * detadx
        dwdy = dwdxsi * dxsidy + dwdeta * detady
 
        phi(iquad,iq,1,ielem) = w
        phi(iquad,iq,2,ielem) = dwdx
        phi(iquad,iq,3,ielem) = dwdy 

      end do
    end do
  end do

  return
end
subroutine sgb_check ( lda, m, n, ml, mu, ierror )

!*****************************************************************************80
!
!! SGB_CHECK checks the dimensions of a general band matrix.
!
!  Modified:
!
!    18 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer LDA, the leading dimension of the array.
!    LDA must be at least 2 * ML + MU + 1.
!
!    Input, integer M, the number of rows of the matrix.
!    M must be positive.
!
!    Input, integer N, the number of columns of the matrix.
!    N must be positive.
!
!    Input, integer ML, MU, the lower and upper bandwidths.
!    ML and MU must be nonnegative, and no greater than min(M,N)-1.
!
!    Output, integer IERROR, reports whether any errors were detected.
!    IERROR is set to 0 before the checks are made, and then:
!    IERROR = IERROR + 1 if LDA is illegal;
!    IERROR = IERROR + 2 if M is illegal;
!    IERROR = IERROR + 4 if ML is illegal;
!    IERROR = IERROR + 8 if MU is illegal;
!    IERROR = IERROR + 16 if N is illegal.
!
  implicit none

  integer ierror
  integer lda
  integer m
  integer ml
  integer mu
  integer n

  ierror = 0

  if ( lda < 2 * ml + mu + 1 ) then
    ierror = ierror + 1
    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) 'SGB_CHECK - Illegal LDA = ', lda
  end if

  if ( m < 1 ) then
    ierror = ierror + 2
    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) 'SGB_CHECK - Illegal M = ', m
  end if

  if ( ml < 0 .or. min ( m, n ) - 1 < ml ) then
    ierror = ierror + 4
    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) 'SGB_CHECK - Illegal ML = ', ml
  end if

  if ( mu < 0 .or. min ( m, n ) - 1 < mu ) then
    ierror = ierror + 8
    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) 'SGB_CHECK - Illegal MU = ', mu
  end if

  if ( n < 1 ) then
    ierror = ierror + 16
    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) 'SGB_CHECK - Illegal N = ', n
  end if

  return
end
subroutine sgb_fa ( a, lda, n, ml, mu, ipivot, info )

!*****************************************************************************80
!
!! SGB_FA factors a matrix stored in LINPACK general band storage.
!
!  Discussion:
!
!    The matrix is stored in the array using LINPACK general band storage.
!    The following program segment will set up the input.
!
!      m = ml + mu + 1
!      do j = 1, n
!        i1 = max ( 1, j-mu )
!        i2 = min ( n, j+ml )
!        do i = i1, i2
!          k = i - j + m
!          a(k,j) = afull(i,j)
!        end do
!      end do
!
!    This uses rows ML+1 through 2*ML+MU+1 of the array A.
!    In addition, the first ML rows in the array are used for
!    elements generated during the triangularization.
!    The total number of rows needed in A is 2*ML+MU+1.
!    The ML+MU by ML+MU upper left triangle and the
!    ML by ML lower right triangle are not referenced.
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
!    Input/output, real A(LDA,N), the matrix in band storage.  The
!    columns of the matrix are stored in the columns of the array,
!    and the diagonals of the matrix are stored in rows ML+1 through
!    2*ML+MU+1.  On return, A has been overwritten by the LU factors.
!
!    Input, integer LDA, the leading dimension of the array.
!    LDA must be at least 2*ML+MU+1.
!
!    Input, integer N, the order of the matrix.
!    N must be positive.
!
!    Input, integer ML, MU, the lower and upper bandwidths.
!    ML and MU must be nonnegative, and no greater than N-1.
!
!    Output, integer IPIVOT(N), the pivot vector.
!
!    Output, integer INFO, singularity flag.
!    0, no singularity detected.
!    nonzero, the factorization failed on the INFO-th step.
!
  implicit none

  integer lda
  integer n

  real ( kind = 8 ) a(lda,n)
  integer i
  integer i0
  integer ierror
  integer info
  integer ipivot(n)
  integer j
  integer j0
  integer j1
  integer ju
  integer jz
  integer k
  integer l
  integer lm
  integer m
  integer ml
  integer mm
  integer mu
  real ( kind = 8 ) t
!
!  Check the dimensions.
!
  call sgb_check ( lda, n, n, ml, mu, ierror )

  if ( ierror /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SGB_FA - Warning!'
    write ( *, '(a)' ) '  Illegal dimensions.'
    return
  end if

  m = ml + mu + 1
  info = 0
!
!  Zero out the initial fill-in columns.
!
  j0 = mu + 2
  j1 = min ( n, m ) - 1

  do jz = j0, j1
    i0 = m + 1 - jz
    do i = i0, ml
      a(i,jz) = 0.0D+00
    end do
  end do

  jz = j1
  ju = 0

  do k = 1, n-1
!
!  Zero out the next fill-in column.
!
    jz = jz + 1
    if ( jz <= n ) then
      do i = 1, ml
        a(i,jz) = 0.0D+00
      end do
    end if
!
!  Find L = pivot index.
!
    lm = min ( ml, n-k )

    l = m
    do j = m+1, m+lm
      if ( abs ( a(l,k) ) < abs ( a(j,k) ) ) then
        l = j
      end if
    end do

    ipivot(k) = l + k - m
!
!  Zero pivot implies this column already triangularized.
!
    if ( a(l,k) == 0.0D+00 ) then
      info = k
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SGB_FA - Warning!'
      write ( *, '(a,i8)' ) '  Zero pivot on step ', info
      return
    end if
!
!  Interchange if necessary.
!
    if ( l /= m ) then
      t = a(l,k)
      a(l,k) = a(m,k)
      a(m,k) = t
    end if
!
!  Compute multipliers.
!
    do j = m+1, m+lm
      a(j,k) = - a(j,k) / a(m,k)
    end do
!
!  Row elimination with column indexing.
!
    ju = max ( ju, mu+ipivot(k) )
    ju = min ( ju, n )
    mm = m

    do j = k+1, ju

      l = l - 1
      mm = mm - 1
      t = a(l,j)
      if ( l /= mm ) then
        a(l,j) = a(mm,j)
        a(mm,j) = t
      end if

      do i = 1, lm
        a(mm+i,j) = a(mm+i,j) + t * a(m+i,k)
      end do

    end do

  end do

  ipivot(n) = n

  if ( a(m,n) == 0.0D+00 ) then
    info = n
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SGB_FA - Warning!'
    write ( *, '(a,i8)' ) '  Zero pivot on step ', info
  end if

  return
end
subroutine sgb_print ( a, lda, m, n, ml, mu, ilo, ihi, jlo, jhi )

!*****************************************************************************80
!
!! SGB_PRINT prints nonzero entries of a banded matrix.
!
!  Discussion:
!
!    Only entries in rows ILO to IHI, columns JLO to JHI are considered.
!
!  Modified:
!
!    19 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real A(LDA,N), the M by N band matrix, stored in LINPACK
!    or LAPACK general band storage mode.
!
!    Input, integer LDA, the leading dimension of the array.
!    LDA must be at least 2*ML+MU+1.
!
!    Input, integer M, the number of rows of the matrix.
!    M must be positive.
!
!    Input, integer N, the number of columns of the matrix.
!    N must be positive.
!
!    Input, integer ML, MU, the lower and upper bandwidths.
!    ML and MU must be nonnegative, and no greater than min(M,N)-1..
!
!    Input, integer ILO, IHI, JLO, JHI, designate the first and last
!    row, and first and last columns, to be considered.
!
  implicit none

  integer, parameter :: incx = 5

  integer lda
  integer n

  real ( kind = 8 ) a(lda,n)
  character ( len = 14 ) ctemp(incx)
  integer i
  integer i2hi
  integer i2lo
  integer ierror
  integer ihi
  integer ilo
  integer inc
  integer j
  integer j2
  integer j2hi
  integer j2lo
  integer jhi
  integer jlo
  integer m
  integer ml
  integer mu
!
!  Check the dimensions.
!
  call sgb_check ( lda, m, n, ml, mu, ierror )

  if ( ierror /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SGB_PRINT - Warning!'
    write ( *, '(a)' ) '  Illegal dimensions.'
    return
  end if
!
!  Print the columns of the matrix, in strips of 5.
!
  do j2lo = jlo, jhi, incx

    j2hi = j2lo + incx - 1
    j2hi = min ( j2hi, n )
    j2hi = min ( j2hi, jhi )

    inc = j2hi + 1 - j2lo

    write ( *, '(a)' ) ' '

    do j = j2lo, j2hi
      j2 = j + 1 - j2lo
      write ( ctemp(j2), '(i7,7x)' ) j
    end do

    write ( *, '(''Columns:'',5a14)' ) ( ctemp(j2), j2 = 1, inc )
    write ( *, '(a)' ) '  Row'
    write ( *, '(a)' ) '  ---'
!
!  Determine the range of the rows in this strip.
!
    i2lo = ilo
    i2lo = max ( ilo, 1 )
    i2lo = max ( i2lo, j2lo - mu )

    i2hi = ihi
    i2hi = min ( i2hi, m )
    i2hi = min ( i2hi, j2hi + ml )

    do i = i2lo, i2hi
!
!  Print out (up to) 5 entries in row I, that lie in the current strip.
!
      do j2 = 1, inc

        j = j2lo - 1 + j2

        if ( ml < i-j .or. mu < j-i ) then
          ctemp(j2) = '              '
        else if ( a(i-j+ml+mu+1,j) == 0.0D+00 ) then
          ctemp(j2) = '    0.0'
        else
          write ( ctemp(j2), '(g14.6)' ) a(i-j+ml+mu+1,j)
        end if

      end do

      write ( *, '(i8,1x,5a14)' ) i, ( ctemp(j2), j2 = 1, inc )

    end do

  end do

  return
end
subroutine sgb_sl ( a, lda, n, ml, mu, ipivot, b, job )

!*****************************************************************************80
!
!! SGB_SL solves a system factored by DGB_FA.
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
!    Input, real A(LDA,N), the LU factors from SGB_FA.
!
!    Input, integer LDA, the leading dimension of the array.
!    LDA must be at least 2*ML+MU+1.
!
!    Input, integer N, the order of the matrix.
!    N must be positive.
!
!    Input, integer ML, MU, the lower and upper bandwidths.
!    ML and MU must be nonnegative, and no greater than N-1.
!
!    Input, integer IPIVOT(N), the pivot vector from SGB_FA.
!
!    Input/output, real B(N).
!    On input, the right hand side vector.
!    On output, the solution.
!
!    Input, integer JOB.
!    0, solve A*X=B.
!    nonzero, solve transpose(A)*X=B.
!
  implicit none

  integer lda
  integer n

  real ( kind = 8 ) a(lda,n)
  real ( kind = 8 ) b(n)
  integer ierror
  integer ipivot(n)
  integer j
  integer job
  integer k
  integer l
  integer la
  integer lb
  integer lm
  integer m
  integer ml
  integer mu
  real ( kind = 8 ) t
!
!  Check the dimensions.
!
  call sgb_check ( lda, n, n, ml, mu, ierror )

  if ( ierror /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SGB_SL - Warning!'
    write ( *, '(a)' ) '  Illegal dimensions!'
    return
  end if

  m = mu + ml + 1
!
!  Solve A * X = B.
!
  if ( job == 0 ) then
!
!  Solve L * Y = B.
!
    if ( 1 <= ml ) then

      do k = 1, n-1

        lm = min ( ml, n-k )
        l = ipivot(k)
        t = b(l)

        if ( l /= k ) then
          b(l) = b(k)
          b(k) = t
        end if

        do j = 1, lm
          b(k+j) = b(k+j) + t * a(m+j,k)
        end do

      end do
    end if
!
!  Solve U * X = Y.
!
    do k = n, 1, -1

      b(k) = b(k) / a(m,k)
      lm = min ( k, m ) - 1
      la = m - lm
      lb = k - lm
      t = -b(k)

      do j = 0, lm-1
        b(lb+j) = b(lb+j) + t * a(la+j,k)
      end do

    end do
!
!  Solve transpose(A) * X = B.
!
  else
!
!  Solve transpose(U) * Y = B.
!
    do k = 1, n
      lm = min ( k, m ) - 1
      la = m - lm
      lb = k - lm
      t = 0.0D+00
      do j = 0, lm-1
        t = t + a(la+j,k) * b(lb+j)
      end do
      b(k) = ( b(k) - t ) / a(m,k)
    end do
!
!  Solve transpose(L) * X = Y.
!
    if ( 1 <= ml ) then

      do k = n-1, 1, -1

        lm = min ( ml, n-k )

        t = 0.0D+00
        do j = 1, lm
          t = t + a(m+j,k) * b(k+j)
        end do

        b(k) = b(k) + t
        l = ipivot(k)

        if ( l /= k ) then
          t = b(l)
          b(l) = b(k)
          b(k) = t
        end if

      end do

    end if

  end if

  return
end
subroutine stokes ( a, bc_tag, detmap, eqn, g, ierror, indx, ipivot, jac, &
  maxelm, maxeqn, maxnp, maxquad1, maxquad2, maxside, ncol, nelem, neqn, &
  nlband, node, np, nquad1, nquad2, nrow, nside, penalty1, penalty2, phi, &
  region, region_ymax, res, res2, nu_inv, side_basis, side_elem, side_eqn, &
  side_etam, side_etap, side_indx, side_xsim, side_xsip, solver, squad1, &
  wquad1, wquad2, xc, yc )

!*****************************************************************************80
!
!! STOKES solves the Stokes equations.
!
!  Discussion:
!
!    This routine is used to get a starting point for the Newton iteration.
!
!  Modified:
!
!    23 December 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Workspace, real A(NROW,MAXEQN), is used to store
!    the jacobian matrix, and then its LU factors.
!
!    Input, real DETMAP(MAXQUAD2,MAXELM), the reference map determinant.
!
!    Input, character ( len = 3 ) EQN(MAXEQN), the type of each equation.
!
!    Input/output, real G(MAXEQN).
!
!    On input, some estimate for the solution.  If no estimate
!    is known, set G to zero.
!
!    On output, G is the improved solution computed by Newton's method.
!
!    Output, integer IERROR, error flag.
!    0, no error occurred.
!    1, an error occurred, and the improved solution could not be computed.
!
!    Input, integer INDX(3,MAXNP), mapping from nodes to degrees of freedom.
!
!    Workspace, integer IPIVOT(MAXEQN), pivot space needed by the
!    matrix factorization and solving routines.
!
!    Input, integer MAXELM, the maximum number of elements.
!
!    Input, integer MAXNP, the maximum number of nodes.
!
!    Input, integer NELEM, the number of elements.
! 
!    Input, integer NEQN, the number of finite element equations.
! 
!    Input, integer NLBAND, the lower bandwidth of the matrix A.
! 
!    Input, integer NODE(6,MAXELM), the nodes that make up each element.
! 
!    Input, integer NP, the number of nodes.
! 
!    Input, integer NROW, the number of rows need to store the matrix A.
!
!    Input, real PHI(MAXQUAD2,6,6,MAXELM), basis function values.  
!
!    Input, character ( len = 20 ) REGION, the flow problem.
!
!    Workspace, real RES(MAXEQN), the residual.
!
!    Input, real NU_INV, the inverse viscosity.
!
!    Input, real XC(MAXNP), the X coordinates of the nodes.
!
!    Input, real YC(MAXNP), the Y coordinates of the nodes.
!
  implicit none

  integer maxelm
  integer maxeqn
  integer maxnp
  integer maxquad1
  integer maxquad2
  integer maxside
  integer ncol
  integer nrow

  real ( kind = 8 ) a(nrow,ncol)
  integer bc_tag(maxnp)
  real ( kind = 8 ) detmap(maxquad2,maxelm)
  character ( len = 3 ) eqn(maxeqn)
  real ( kind = 8 ) g(maxeqn)
  integer i
  integer ierror
  integer indx(3,maxnp)
  integer info
  integer ipivot(maxeqn)
  integer irmax
  integer jac
  integer job
  real ( kind = 8 ) navier
  integer nelem
  integer neqn
  integer nlband
  integer node(6,maxelm)
  integer np
  integer nquad1
  integer nquad2
  integer nside
  real ( kind = 8 ) penalty1
  real ( kind = 8 ) penalty2
  real ( kind = 8 ) phi(maxquad2,6,6,maxelm)
  character ( len = 20 ) region
  real ( kind = 8 ) region_ymax
  real ( kind = 8 ) res(maxeqn)
  real ( kind = 8 ) res2(maxeqn)
  real ( kind = 8 ) nu_inv
  real ( kind = 8 ) rmax
  logical s_eqi
  integer side_basis(3,maxside)
  integer side_elem(maxside)
  character ( len = 3 ) side_eqn(maxside)
  real ( kind = 8 ) side_etam(maxside)
  real ( kind = 8 ) side_etap(maxside)
  integer side_indx(3,maxside)
  real ( kind = 8 ) side_xsim(maxside)
  real ( kind = 8 ) side_xsip(maxside)
  character ( len = 20 ) solver
  real ( kind = 8 ) squad1(maxquad1)
  real ( kind = 8 ) wquad1(maxquad1)
  real ( kind = 8 ) wquad2(maxquad2)
  real ( kind = 8 ) xc(maxnp)
  real ( kind = 8 ) yc(maxnp)

  ierror = 0
  navier = 0.0D+00
!
!  Compute the right hand side of the Stokes equations.
!
  g(1:neqn) = 0.0D+00

  call fx ( bc_tag, detmap, eqn, g, indx, maxelm, maxeqn, maxnp, maxquad1, &  
    maxquad2, maxside, navier, nelem, neqn, node, np, nquad1, nquad2, nside, &
    penalty1, penalty2, phi, region, region_ymax, res, nu_inv, side_basis, &
    side_elem, side_eqn, side_etam, side_etap, side_indx, side_xsim, &
    side_xsip, squad1, wquad1, wquad2, xc, yc )

  res(1:neqn) = - res(1:neqn)

  call r4vec_amax ( neqn, res, rmax, irmax )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'STOKES'
  write ( *, '(a,g14.6)' ) '  Initial norm of Stokes residual is ', rmax
!
!  Compute the Stokes system matrix.
!
  if ( jac == 0 ) then

    call fp ( a, bc_tag, detmap, eqn, g, indx, maxelm, maxeqn, maxnp, &
      maxquad1, maxquad2, maxside, navier, ncol, nelem, neqn, nlband, node, &
      np, nquad1, nquad2, nrow, nside, penalty1, penalty2, phi, region, &
      nu_inv, side_basis, side_elem, side_eqn, side_etam, side_etap, &
      side_indx, side_xsim, side_xsip, solver, squad1, wquad1, wquad2, xc, yc )
 
  else

    call fp_dif ( a, bc_tag, detmap, eqn, g, indx, maxelm, maxeqn, maxnp, &
      maxquad1, maxquad2, maxside, navier, ncol, nelem, neqn, nlband, node, &
      np, nquad1, nquad2, nrow, nside, penalty1, penalty2, phi, region, & 
      region_ymax, res, res2, nu_inv, side_basis, side_elem, side_eqn, &
      side_etam, side_etap, side_indx, side_xsim, side_xsip, solver, squad1, & 
      wquad1, wquad2, xc, yc )

  end if

  if ( s_eqi ( solver, 'GAUSS' ) ) then

    call sgb_fa ( a, nrow, neqn, nlband, nlband, ipivot, info )

    if ( info /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'STOKES - Warning!'
      write ( *, '(a)' ) '  The jacobian is singular.'
      write ( *, '(a,i8)' ) '  SGB_FA returns INFO=', info
      ierror = 1
      return
    end if

    job = 0
    call sgb_sl ( a, nrow, neqn, nlband, nlband, ipivot, res, job )

    g(1:neqn) = res(1:neqn)

  else if ( s_eqi ( solver, 'CGS' ) ) then

  end if

  navier = 1.0D+00

  call fx ( bc_tag, detmap, eqn, g, indx, maxelm, maxeqn, maxnp, maxquad1, &
    maxquad2, maxside, navier, nelem, neqn, node, np, nquad1, nquad2, nside, &
    penalty1, penalty2, phi, region, region_ymax, res, nu_inv, side_basis, &
    side_elem, side_eqn, side_etam, side_etap, side_indx, side_xsim, &
    side_xsip, squad1, wquad1, wquad2, xc, yc )
 
  call r4vec_amax ( neqn, res, rmax, irmax )
  write ( *, '(a,g14.6)' ) '  Norm of Navier-Stokes residual is ', rmax

  navier = 0.0D+00

  call fx ( bc_tag, detmap, eqn, g, indx, maxelm, maxeqn, maxnp, maxquad1, &
    maxquad2, maxside, navier, nelem, neqn, node, np, nquad1, nquad2, nside, &
    penalty1, penalty2, phi, region, region_ymax, res, nu_inv, side_basis, &
    side_elem, side_eqn, side_etam, side_etap, side_indx, side_xsim, &
    side_xsip, squad1, wquad1, wquad2, xc, yc )

  call r4vec_amax ( neqn, res, rmax, irmax )

  write ( *, '(a,g14.6)' ) '  Norm of Stokes residual is ', rmax

  return
end 
subroutine timestamp ( )

!*****************************************************************************80
!
!! TIMESTAMP prints the current YMDHMS date as a time stamp.
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
subroutine trans_q6 ( det, detadx, detady, dxsidx, dxsidy, eta, ielem, &
  maxelm, maxnp, node, xc, xsi, yc )

!*****************************************************************************80
!
!! TRANS_Q6 calculates the biquadratic reference element transformation.
!
!  Discussion:
!
!    This transformation maps the reference element in (XSI,ETA) space 
!    into a particular isoparametric element in (X,Y) space.
!
!    We know everything about the isoparametric element once we
!    specify the location of its six nodes.
!
!    This routine computes the entries of the jacobian of the transformation
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
!  d F(X,Y)/dX     (d XSI/dX  d ETA/dX )   ( d F(XSI, ETA)/d XSI )
!  d F(X,Y)/dY  =  (d XSI/dY  d ETA/dY ) * ( d F(XSI, ETA)/d ETA )
!
!  Modified:
!
!    19 September 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real DET, the determinant of the jacobian of the 
!    isoparametric transformation from the reference element to the 
!    physical element.
!
!    Output, real DETADX, DETADY, the partial 
!    derivative d ETA/d X and d ETA/d Y at (XSI,ETA).
!
!    Output, real DXSIDX, DXSIDY, the partial 
!    derivative d XSI/d X and d XSI/d Y at (XSI,ETA).
!
!    Input, real ETA, the ETA coordinate of the point.
!
!    Input, integer IELEM, the index of the element.
!
!    Input, integer MAXELM, the maximum number of elements.
!
!    Input, integer MAXNP, the maximum number of nodes.
!
!    Input, integer NELEM, the number of elements.
!
!    Input, integer NODE(6,MAXELM), the nodes that make up each element.
!
!    Input, integer NP, the number of nodes.
!
!    Input, real XC(MAXNP), the X coordinates of the nodes.
!
!    Input, real XSI, the XSI coordinate of the point.
!
!    Input, real YC(MAXNP), the Y coordinates of the nodes.
!
  implicit none

  integer maxelm
  integer maxnp

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) d
  real ( kind = 8 ) det
  real ( kind = 8 ) detadx
  real ( kind = 8 ) detady
  real ( kind = 8 ) dxdeta
  real ( kind = 8 ) dxdxsi
  real ( kind = 8 ) dxsidx
  real ( kind = 8 ) dxsidy
  real ( kind = 8 ) dydeta
  real ( kind = 8 ) dydxsi
  real ( kind = 8 ) e
  real ( kind = 8 ) eta
  real ( kind = 8 ) f
  integer i
  integer ielem
  integer node(6,maxelm)
  real ( kind = 8 ) xc(maxnp)
  real ( kind = 8 ) xsi
  real ( kind = 8 ) xx(6)
  real ( kind = 8 ) yc(maxnp)
!
!  Compute dX/dXSI(XSI,ETA) and dX/dETA(XSI,ETA).
!
  do i = 1, 6
    xx(i) = xc ( node(i,ielem) )
  end do

  call ref_map_q6 ( xx, a, b, c, d, e, f )

  dxdxsi = 2.0D+00 * a * xsi +       b * eta + d
  dxdeta =       b * xsi + 2.0D+00 * c * eta + e
!
!  Compute dY/dXSI(XSI,ETA) and dY/dETA(XSI,ETA).
!
  do i = 1, 6
    xx(i) = yc ( node(i,ielem) )
  end do

  call ref_map_q6 ( xx, a, b, c, d, e, f )

  dydxsi = 2.0D+00 * a * xsi +       b * eta + d
  dydeta =       b * xsi + 2.0D+00 * c * eta + e
!
!  Compute the determinant of the jacobian matrix:
!
!    J: (XSI,ETA) --> (X,Y)
!
  det = dxdxsi * dydeta - dxdeta * dydxsi

  if ( det == 0.0D+00 ) then

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TRANSQ6 - Fatal error!'
    write ( *, '(a)' ) '  J: (XSI,ETA) --> (X,Y) is singular!'
    write ( *, '(a,i8)' ) '  This occurred for element number ', ielem
    write ( *, '(a,2g14.6)' ) '  Local coordinates XSI,ETA=', xsi, eta
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  The X, Y nodes were:'
    write ( *, '(a)' ) ' '

    do i = 1, 6
      write ( *, '(2g14.6)' ) xc(node(i,ielem) ), yc(node(i,ielem))
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
!            (-c  a)
!
  dxsidx =   dydeta / det
  dxsidy = - dxdeta / det
 
  detadx = - dydxsi / det
  detady =   dxdxsi / det
 
  return
end
subroutine uvp_norm_h1 ( detmap, g, indx, maxelm, maxeqn, maxnp, maxquad2, &
  nelem, node, nquad2, phi, p_norm_h1, uv_norm_h1, wquad2 )

!*****************************************************************************80
!
!! UVP_NORM_H1 returns the H1 norm of the solution.
!
!  Definition:
!
!    UV_NORM_H1 = sqrt ( Integral 
!      u**2 + v**2 + dudx**2 + dvdx**2+ dudy**2 + dvdy**2 dOmega )
!
!    P_NORM_H1 = sqrt ( Integral p**2 + dpdx**2 + dpdy**2 dOmega )
!
!  Modified:
!
!    20 September 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!  
!    Input, real DETMAP(MAXQUAD2,MAXELM), the reference map determinant.
!
!    Input, real G(MAXEQN), the solutions whose norm is desired.
!
!    Input, integer INDX(3,MAXNP), mapping from nodes to degrees of freedom.
!
!    Input, integer MAXELM, the maximum number of elements.
!
!    Input, integer MAXEQN, the maximum number of equations.
!
!    Input, integer MAXNP, the maximum number of nodes.
!
!    Input, integer MAXQUAD2, the maximum number of 2D quadrature points.
!
!    Input, integer NELEM, the number of elements.
! 
!    Input, integer NODE(6,MAXELM), the nodes that make up each element.
!
!    Input, integer NQUAD2, the number of 2D quadrature points.
!
!    Input, real PHI(MAXQUAD2,6,6,MAXELM), basis function values.
!
!    Output, real P_NORM_H1, the H1 norm of the pressure.
!
!    Output, real UV_NORM_H1, the H1 norm of the velocity.
!
!    Input, real WQUAD2(MAXQUAD2), the 2D quadrature weights.
!
  implicit none

  integer maxelm
  integer maxeqn
  integer maxnp
  integer maxquad2

  real ( kind = 8 ) detmap(maxquad2,maxelm)
  real ( kind = 8 ) dpdx
  real ( kind = 8 ) dpdy
  real ( kind = 8 ) dudx
  real ( kind = 8 ) dudy
  real ( kind = 8 ) dvdx
  real ( kind = 8 ) dvdy
  real ( kind = 8 ) g(maxeqn)
  integer ielem
  integer indx(3,maxnp)
  integer iquad
  integer nelem
  integer node(6,maxelm)
  integer nquad2
  real ( kind = 8 ) p
  real ( kind = 8 ) phi(maxquad2,6,6,maxelm)
  real ( kind = 8 ) p_norm_h1
  real ( kind = 8 ) u
  real ( kind = 8 ) uv_norm_h1
  real ( kind = 8 ) v
  real ( kind = 8 ) weight
  real ( kind = 8 ) wquad2(maxquad2)

  uv_norm_h1 = 0.0D+00
  p_norm_h1 = 0.0D+00
!
!  Consider an element.
!
  do ielem = 1, nelem
!
!  Consider a quadrature point.
!
    do iquad = 1, nquad2
 
      weight = 0.5D+00 * wquad2(iquad) * detmap(iquad,ielem)
!
!  Evaluate P, U and V at the quadrature point.
!
      call uvp_quad_value ( dpdx, dpdy, dudx, dudy, dvdx, dvdy, g, ielem, &
        indx, iquad, maxelm, maxeqn, maxnp, maxquad2, node, p, phi, u, v )

      uv_norm_h1 = uv_norm_h1 + weight * ( u**2 + v**2 + dudx**2 + dvdx**2 &
        + dudy**2 + dvdy**2 )

      p_norm_h1 = p_norm_h1 + weight * ( p**2 + dpdx**2 + dpdy**2 )
 
    end do
  end do

  p_norm_h1 = sqrt ( p_norm_h1 )
  uv_norm_h1 = sqrt ( uv_norm_h1 )

  return
end
subroutine uvp_norm_l2 ( detmap, g, indx, maxelm, maxeqn, maxnp, maxquad2, &
  nelem, node, nquad2, phi, pnorm_l2, uvnorm_l2, wquad2 )

!*****************************************************************************80
!
!! UVP_NORM_L2 returns the L2 norms of velocity magnitude, and pressure.  
!
!  Definition:
!
!    UVNORM_L2 = sqrt ( ( Integral u**2 + v**2 dOmega ) )
!    PNORM_L2 = sqrt ( ( Integral p**2 dOmega ) )
!
!  Modified:
!
!    01 December 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!  
!    Input, real DETMAP(MAXQUAD2,MAXELM), the reference map determinant.
!
!    Input, real G(MAXEQN), the current solution.
!
!    Input, integer INDX(3,MAXNP), mapping from nodes to degrees of freedom.
!
!    Input, integer MAXELM, the maximum number of elements.
!
!    Input, integer MAXEQN, the maximum number of equations.
!
!    Input, integer MAXNP, the maximum number of nodes.
!
!    Input, integer MAXQUAD2, the maximum number of 2D quadrature points.
!
!    Input, integer NELEM, the number of elements.
! 
!    Input, integer NODE(6,MAXELM), the nodes that make up each element.
!
!    Input, integer NQUAD2, the number of 2D quadrature points.
!
!    Input, real PHI(MAXQUAD2,6,6,MAXELM), basis function values.
!
!    Output, real PNORM_L2, the L2 norm of the pressure.
!
!    Output, real UVNORM_L2, the L2 norm of the velocity magnitude.
!
!    Input, real WQUAD2(MAXQUAD2), the 2D quadrature weights.
!
  implicit none

  integer maxelm
  integer maxeqn
  integer maxnp
  integer maxquad2

  real ( kind = 8 ) detmap(maxquad2,maxelm)
  real ( kind = 8 ) dpdx
  real ( kind = 8 ) dpdy
  real ( kind = 8 ) dudx
  real ( kind = 8 ) dudy
  real ( kind = 8 ) dvdx
  real ( kind = 8 ) dvdy
  real ( kind = 8 ) g(maxeqn)
  integer ielem
  integer indx(3,maxnp)
  integer iquad
  integer nelem
  integer node(6,maxelm)
  integer nquad2
  real ( kind = 8 ) p
  real ( kind = 8 ) phi(maxquad2,6,6,maxelm)
  real ( kind = 8 ) pnorm_l2
  real ( kind = 8 ) u
  real ( kind = 8 ) uvnorm_l2
  real ( kind = 8 ) v
  real ( kind = 8 ) weight
  real ( kind = 8 ) wquad2(maxquad2)

  uvnorm_l2 = 0.0D+00
  pnorm_l2 = 0.0D+00
!
!  Consider an element.
!
  do ielem = 1, nelem
!
!  Consider a quadrature point.
!
    do iquad = 1, nquad2
 
      weight = 0.5D+00 * wquad2(iquad) * detmap(iquad,ielem)
!
!  Evaluate P, U and V at the quadrature point.
!
      call uvp_quad_value ( dpdx, dpdy, dudx, dudy, dvdx, dvdy, g, &
        ielem, indx, iquad, maxelm, maxeqn, maxnp, maxquad2, &
        node, p, phi, u, v )

      uvnorm_l2 = uvnorm_l2 + weight * ( u**2 + v**2 )

      pnorm_l2 = pnorm_l2 + weight * p**2
 
    end do
  end do

  pnorm_l2 = sqrt ( pnorm_l2  )
  uvnorm_l2 = sqrt ( uvnorm_l2 )

  return
end
subroutine uvp_norm_lmax ( g, indx, maxeqn, maxnp, np, pnorm_lmax, &
  uvnorm_lmax )
!
!*****************************************************************************80
!
!! UVP_NORM_LMAX returns the infinity norms of velocity magnitude and pressure.
!
!  Definition:
!
!    UVNORM_LMAX = the maximum velocity magnitude at a node,
!    PNORM_LMAX = the maximum pressure at a node.
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
!    Input, real G(MAXEQN), the current solution..
!
!    Input, integer INDX(3,MAXNP), mapping from nodes to degrees of freedom.
!
!    Input, integer MAXEQN, the maximum number of equations.
!
!    Input, integer MAXNP, the maximum number of nodes.
!
!    Input, integer NP, the number of nodes.
!
!    Output, real PNORM_LMAX, the infinity norm of the pressure.
!
!    Output, real UVNORM_LMAX, the infinity norm of the velocity magnitude.
!
  implicit none

  integer maxeqn
  integer maxnp

  real ( kind = 8 ) g(maxeqn)
  integer i
  integer indx(3,maxnp)
  integer np
  real ( kind = 8 ) p
  real ( kind = 8 ) pnorm_lmax
  real ( kind = 8 ) u
  real ( kind = 8 ) uvnorm_lmax
  real ( kind = 8 ) v

  uvnorm_lmax = 0.0D+00
  pnorm_lmax = 0.0D+00

  do i = 1, np

    u = g ( indx(1,i) )
    v = g ( indx(2,i) )
    uvnorm_lmax = max ( uvnorm_lmax, sqrt ( u**2 + v**2 ) )

    if ( 0 < indx(3,i) ) then
      p = g ( indx(3,i) )
      pnorm_lmax = max ( pnorm_lmax, abs ( p ) )
    end if

  end do

  return
end
subroutine uvp_print ( g, indx, maxeqn, maxnp, np, p, xc, yc )

!*****************************************************************************80
!
!! UVP_PRINT prints out the solution.
!
!  Modified:
!
!    03 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer MAXNP, the maximum number of nodes.
!
!    Input, integer NP, the number of nodes.
!
!    Input, real P(MAXNP), the pressure.
!
!    Input, real XC(MAXNP), the X coordinates of the nodes.
!
!    Input, real YC(MAXNP), the Y coordinates of the nodes.
!
  implicit none

  integer maxeqn
  integer maxnp

  real ( kind = 8 ) g(maxeqn)
  integer i
  integer indx(3,maxnp)
  integer np
  real ( kind = 8 ) p(maxnp)
  real ( kind = 8 ) xc(maxnp)
  real ( kind = 8 ) xold
  real ( kind = 8 ) yc(maxnp)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'UVP_PRINT:'
  write ( *, '(a)' ) '  Flow solution values at nodes:'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '     X         Y           U       V           P'
  write ( *, '(a)' ) ' '
!
!  Write out the data at each node.
!
  xold = xc(1) - 1.0D+00

  do i = 1, np

    if ( xold < xc(i) ) then
      write ( *, '(a)' ) ' '
      xold = xc(i)
    end if

    write ( *, '(2f10.4,2x,3f12.6)' ) &
      xc(i), yc(i), g(indx(1,i)), g(indx(2,i)), p(i)

  end do

  return
end
subroutine uvp_quad_value ( dpdx, dpdy, dudx, dudy, dvdx, dvdy, g, ielem, &
  indx, iquad, maxelm, maxeqn, maxnp, maxquad2, node, p, phi, u, v )

!*****************************************************************************80
!
!! UVP_QUAD_VALUE evaluates U, V and P at a quadrature point in a given element.
!
!  Discussion:
!
!    You could call UVP_VALUE to do this operation.  But if you know that
!    you are evaluating at a quadrature point, this routine is significantly
!    faster, since the values of the basis functions and derivatives have
!    been stored in arrays exactly to speed up this process.
!
!  Modified:
!
!    15 September 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real DPDX, DPDY, the derivatives of the
!    pressure function with respect to X and Y.
!
!    Output, real DUDX, DUDY, the derivatives of the
!    horizontal velocity function with respect to X and Y.
!
!    Output, real DVDX, DVDY, the derivatives of the
!    vertical velocity function with respect to X and Y.
!
!    Input, real G(MAXEQN), the current solution vector.
!
!    Input, integer IELEM, the index of the element.
!
!    Input, integer INDX(3,MAXNP), mapping from nodes to degrees of freedom.
!
!    Input, integer IQUAD, the index of the quadrature point.
!
!    Input, integer MAXELM, the maximum number of elements.
!
!    Input, integer MAXEQN, the maximum number of equations.
!
!    Input, integer MAXNP, the maximum number of nodes.
!
!    Input, integer NODE(6,MAXELM), the nodes that make up each element.
! 
!    Output, real P, the value of the pressure.
!
!    Input, real PHI(MAXQUAD2,6,6,MAXELM), basis function values.
!
!    Output, real U, the value of the horizontal velocity.
!
!    Output, real V, the value of the vertical velocity.
!
  implicit none

  integer maxelm
  integer maxeqn
  integer maxnp
  integer maxquad2

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
  real ( kind = 8 ) g(maxeqn)
  integer ielem
  integer indx(3,maxnp)
  integer ip
  integer iq
  integer iquad
  integer iun
  integer node(6,maxelm)
  real ( kind = 8 ) p
  real ( kind = 8 ) phi(maxquad2,6,6,maxelm)
  real ( kind = 8 ) q
  real ( kind = 8 ) u
  real ( kind = 8 ) v
  real ( kind = 8 ) w

  u = 0.0D+00
  v = 0.0D+00
  dudx = 0.0D+00
  dudy = 0.0D+00
  dvdx = 0.0D+00
  dvdy = 0.0D+00

  do iq = 1, 6
 
    w    = phi(iquad,iq,1,ielem)
    dwdx = phi(iquad,iq,2,ielem)
    dwdy = phi(iquad,iq,3,ielem)

    ip = node(iq,ielem)

    iun = indx(1,ip)
    u    = u    + g(iun) * w
    dudx = dudx + g(iun) * dwdx
    dudy = dudy + g(iun) * dwdy

    iun = indx(2,ip)
    v    = v    + g(iun) * w
    dvdx = dvdx + g(iun) * dwdx
    dvdy = dvdy + g(iun) * dwdy

  end do

  p = 0.0D+00
  dpdx = 0.0D+00
  dpdy = 0.0D+00

  do iq = 1, 3
  
    q    = phi(iquad,iq,4,ielem)
    dqdx = phi(iquad,iq,5,ielem)
    dqdy = phi(iquad,iq,6,ielem)

    ip = node(iq,ielem)
    iun = indx(3,ip)

    if ( 0 < iun ) then
      p    = p    + g(iun) * q
      dpdx = dpdx + g(iun) * dqdx
      dpdy = dpdy + g(iun) * dqdy
    end if
 
  end do
 
  return
end
subroutine uvp_value ( eta, g, ielem, indx, maxelm, maxeqn, maxnp, node, p, &
  u, v, xsi )

!*****************************************************************************80
!
!! UVP_VALUE evaluates U, V and P at any point in a given element.
!
!  Modified:
!
!    19 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ETA, the ETA coordinate of the point.
!
!    Input, real G(MAXEQN), the current solution vector.
!
!    Input, integer IELEM, the index of the element.
!
!    Input, integer INDX(3,MAXNP), mapping from nodes to degrees of freedom.
!
!    Input, integer MAXELM, the maximum number of elements.
!
!    Input, integer MAXEQN, the maximum number of equations.
!
!    Input, integer MAXNP, the maximum number of nodes.
!
!    Input, integer NODE(6,MAXELM), the nodes that make up each element.
! 
!    Output, real P, the value of the pressure.
!
!    Output, real U, the value of the horizontal velocity.
!
!    Output, real V, the value of the vertical velocity.
!
!    Input, real XSI, the XSI coordinate of the point.
!
  implicit none

  integer maxelm
  integer maxeqn
  integer maxnp

  real ( kind = 8 ) dqdeta
  real ( kind = 8 ) dqdxsi
  real ( kind = 8 ) dwdeta
  real ( kind = 8 ) dwdxsi
  real ( kind = 8 ) eta
  real ( kind = 8 ) g(maxeqn)
  integer icof
  integer ielem
  integer indx(3,maxnp)
  integer ip
  integer iq
  integer node(6,maxelm)
  real ( kind = 8 ) p
  real ( kind = 8 ) q
  real ( kind = 8 ) u
  real ( kind = 8 ) v
  real ( kind = 8 ) w
  real ( kind = 8 ) xsi

  p = 0.0D+00

  do iq = 1, 3
 
    call ref_bf_l3 ( q, dqdeta, dqdxsi, eta, iq, xsi )

    ip = node(iq,ielem)

    icof = indx(3,ip)
    p = p + g(icof) * q

  end do

  u = 0.0D+00
  v = 0.0D+00
  do iq = 1, 6

    ip = node(iq,ielem)
     
    call ref_bf_q6 ( w, dwdeta, dwdxsi, eta, iq, xsi )

    ip = node(iq,ielem)

    icof = indx(1,ip)
    u = u + g(icof) * w

    icof = indx(2,ip)
    v = v + g(icof) * w

  end do

  return
end
subroutine x_of_xsi ( eta, ielem, maxelm, maxnp, node, x, xc, xsi, y, yc )

!*****************************************************************************80
!
!! X_OF_XSI computes X and Y given XSI and ETA coordinates.
!
!  Modified:
!
!    16 September 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ETA, the ETA coordinate of the point.
!
!    Input, integer IELEM, the index of the element.
!
!    Input, integer MAXELM, the maximum number of elements.
!
!    Input, integer MAXNP, the maximum number of nodes.
!
!    Input, integer NODE(6,MAXELM), the nodes that make up each element.
!
!    Output, real X, the X coordinate of the point.
!
!    Input, real XC(MAXNP), the X coordinates of the nodes.
!
!    Input, real XSI, the XSI coordinate of the point.
!
!    Output, real Y, the Y coordinate of the point.
!
!    Input, real YC(MAXNP), the Y coordinates of the nodes.
!
  implicit none

  integer maxelm
  integer maxnp

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) d
  real ( kind = 8 ) e
  real ( kind = 8 ) eta
  real ( kind = 8 ) f
  integer i
  integer ielem
  integer node(6,maxelm)
  real ( kind = 8 ) x
  real ( kind = 8 ) xc(maxnp)
  real ( kind = 8 ) xsi
  real ( kind = 8 ) xx(6)
  real ( kind = 8 ) y
  real ( kind = 8 ) yc(maxnp)
!
!  Compute X(XSI,ETA).
!
  do i = 1, 6
    xx(i) = xc(node(i,ielem))
  end do

  call ref_map_q6 ( xx, a, b, c, d, e, f )

  x = a * xsi**2 + b * xsi * eta + c * eta**2 + d * xsi + e * eta + f
!
!  Compute Y(XSI,ETA).
!
  do i = 1, 6
    xx(i) = yc(node(i,ielem))
  end do

  call ref_map_q6 ( xx, a, b, c, d, e, f )

  y = a * xsi**2 + b * xsi * eta + c * eta**2 + d * xsi + e * eta + f
 
  return
end
subroutine xy_print ( maxnp, np, xc, yc )

!*****************************************************************************80
!
!! XY_PRINT prints the X and Y coordinates of the nodes.
!
!  Modified:
!
!    21 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer MAXNP, the maximum number of nodes.
!
!    Input, integer NP, the number of nodes.
!
!    Input, real XC(MAXNP), the X coordinates of the nodes.
!
!    Input, real YC(MAXNP), the Y coordinates of the nodes.
!
  implicit none

  integer maxnp

  integer i
  integer np
  real ( kind = 8 ) xc(maxnp)
  real ( kind = 8 ) xold
  real ( kind = 8 ) yc(maxnp)
  real ( kind = 8 ) yold

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'XY_PRINT'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  I, XC(I), YC(I)'
  write ( *, '(a)' ) ' '

  xold = 0.0D+00
  yold = 0.0D+00

  do i = 1, np

    if ( xold /= xc(i) .and. yold /= yc(i) ) then
      write ( *, '(a)' ) ' '
    end if

    write ( *, '(i8,2g14.6)' ) i, xc(i), yc(i)

    xold = xc(i)
    yold = yc(i)

  end do
 
  return
end
