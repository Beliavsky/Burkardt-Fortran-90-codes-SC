subroutine boundary_type ( node_num, node_xy, node_boundary, node_type, &
  node_u_condition, node_v_condition, node_p_condition )

!*****************************************************************************80
!
!! BOUNDARY_TYPE determines the type of boundary conditions imposed.
!
!  Discussion:
!
!    On input, the calling program has already determined the "type"
!    of every node (vertex or midside), and whether or not it lies
!    on the boundary.
!
!    The program has also set up an initial guess for the boundary
!    conditions, by setting every boundary node to have Dirichlet
!    conditions for U and V, and by setting a single vertex boundary
!    node to have Dirichlet boundary conditions for P.
!
!    The user is free to adjust these boundary conditions in any
!    reasonable way.  
!
!    The most obvious adjustment is to change some velocity boundary
!    conditions to Neumann conditions.  Keep in mind that, for the moment, 
!    we are only supporting zero Neumann conditions.
!
!    However, it is also possible to constrain ANY variable, whether it
!    is on the boundary or not, or to UNCONSTRAIN any variable that
!    has been tentatively constrained.  You simply have to "warn" the code,
!    by setting U_TYPE, V_TYPE or P_TYPE appropriately, and by supplying
!    a value for the right hand side if you are doing a Dirichlet condition.
!
!
!    For the cavity flow, we intend for the Dirichlet boundary conditions 
!    to be applied for velocity at all boundary points. 
!
!    The calling program has already found the boundary, and guessed that
!    all boundary velocities are constrained by Dirichlet conditions.  So
!    this routine has nothing to do!
!
!
!    The pressure is specified to be zero at a single node, but we let
!    the main program take care of that specification.
!
!                           Dirichlet
!                           U = 1  V = 0
!
!                       1 +--> --> --> -->+
!                         |               |
!                         |               |
!            Dirichlet    |               |  Dirichlet
!            U = V = 0    |               |  U = V = 0
!                         |               |
!                         |               |
!                         |               |
!                       0 +---------------+
!
!                         0               1
!
!                            Dirichlet
!                            U = V = 0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 October 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NODE_NUM, the number of nodes
!
!    Input, real ( kind = 8 ) NODE_XY(2,NODE_NUM), the coordinates of the nodes.
!
!    Input, logical NODE_BOUNDARY(NODE_NUM), is TRUE if the node is
!    found to lie on the boundary of the region.
!
!    Input, integer ( kind = 4 ) NODE_TYPE(NODE_NUM), determines if the node 
!    is a vertex or midside node.
!    1, the node is a vertex (P, U, V variables are associated with it).
!    2, the node is a midside node (only U and V variables are associated.)
!
!    Input/output, integer ( kind = 4 ) NODE_U_CONDITION(NODE_NUM), 
!    indicates the condition used to determine horizontal velocity at a node.
!    0, there is no condition (and no variable) at this node.
!    1, a finite element equation is used;
!    2, a Dirichlet condition is used. 
!    3, a Neumann condition is used. 
!
!    Input/output, integer ( kind = 4 ) NODE_V_CONDITION(NODE_NUM), 
!    indicates the condition used to determine vertical velocity at a node.
!    0, there is no condition (and no variable) at this node.
!    1, a finite element equation is used;
!    2, a Dirichlet condition is used.
!    3, a Neumann condition is used.  
!
!    Input/output, integer ( kind = 4 ) NODE_P_CONDITION(NODE_NUM), 
!    indicates the condition used to determine pressure at a node.
!    0, there is no condition (and no variable) at this node.
!    1, a finite element equation is used;
!    2, a Dirichlet condition is used. 
!    3, a Neumann condition is used. 
!
  implicit none

  integer ( kind = 4 ) node_num

  logical node_boundary(node_num)
  integer ( kind = 4 ) node_p_condition(node_num)
  integer ( kind = 4 ) node_type(node_num)
  integer ( kind = 4 ) node_u_condition(node_num)
  integer ( kind = 4 ) node_v_condition(node_num)
  real ( kind = 8 ) node_xy(2,node_num)

  return
end
subroutine dirichlet_condition ( node_num, node_xy, u_bc, v_bc, p_bc )

!*****************************************************************************80
!
!! DIRICHLET_CONDITION sets the value of a Dirichlet boundary condition.
!
!  Discussion:
!
!                           Dirichlet
!                           U = 1  V = 0
!
!                       1 +--> --> --> -->+
!                         |               |
!                         |               |
!            Dirichlet    |               |  Dirichlet
!            U = V = 0    |               |  U = V = 0
!                         |               |
!                         |               |
!                         |               |
!                       0 +---------------+
!
!                         0               1
!
!                            Dirichlet
!                            U = V = 0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 October 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NODE_NUM, the number of nodes.
!
!    Input, real ( kind = 8 ) NODE_XY(2,NODE_NUM), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) U_BC(NODE_NUM), V_BC(NODE_NUM), P_BC(NODE_NUM), 
!    the values of the Dirichlet boundary conditions on horizontal velocity, 
!    vertical velocity, and pressure.
!
  implicit none

  integer ( kind = 4 ) node_num

  integer ( kind = 4 ) node
  real ( kind = 8 ) node_xy(2,node_num)
  real ( kind = 8 ) p_bc(node_num)
  real ( kind = 8 ), parameter :: tol = 0.0001D+00
  real ( kind = 8 ) u_bc(node_num)
  real ( kind = 8 ) v_bc(node_num)
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  do node = 1, node_num

    x = node_xy(1,node)
    y = node_xy(2,node)
!
!  Nodes along the top surface have U = 1 condition.
!
    if ( abs ( y - 1.0D+00 ) <= tol ) then
      u_bc(node) = 1.0D+00
      v_bc(node) = 0.0D+00
      p_bc(node) = 0.0D+00
!
!  All other constrained nodes have zero condition.
!
    else
      u_bc(node) = 0.0D+00
      v_bc(node) = 0.0D+00 
      p_bc(node) = 0.0D+00
    end if

  end do

  return
end
subroutine neumann_condition ( node_num, node_xy, u_bc, v_bc, p_bc )

!*****************************************************************************80
!
!! NEUMANN_CONDITION sets the value of any Neumann boundary conditions.
!
!  Discussion:
!
!    Note that, at the moment, we are simply trying to implement
!    ZERO Neumann boundary conditions, and are not ready to try
!    to implement the NONZERO case.  Therefore, setting nonzero values
!    here is unlikely to work for some time!
!
!
!    Neumann boundary conditions might be applied to none, some, or
!    all the boundary nodes, and might apply to any combination of
!    U, V, and P.
!
!    This routine will be asked to supply a right hand side for the
!    Neumann conditions for U, V and P at EVERY node.  Simply set
!    the value to zero for nodes and variables at which a Neumann
!    condition is not being applied.  But set an appropriate value
!    to U_BC, V_BC or P_BC in cases where a Neumann condition is
!    being applied for that degree of freedom at that node.
!
!
!    For the cavity flow, there are no Neumann conditions, so this
!    routine has nothing to do!
!
!                           Dirichlet
!                           U = 1  V = 0
!
!                       1 +--> --> --> -->+
!                         |               |
!                         |               |
!            Dirichlet    |               |  Dirichlet
!            U = V = 0    |               |  U = V = 0
!                         |               |
!                         |               |
!                         |               |
!                       0 +---------------+
!
!                         0               1
!
!                            Dirichlet
!                            U = V = 0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 October 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NODE_NUM, the number of nodes.
!
!    Input, real ( kind = 8 ) NODE_XY(2,NODE_NUM), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) U_BC(NODE_NUM), V_BC(NODE_NUM), P_BC(NODE_NUM),
!    the values of the boundary conditions on horizontal velocity, 
!    vertical velocity, and pressure.
!
  implicit none

  integer ( kind = 4 ) node_num

  real ( kind = 8 ) node_xy(2,node_num)
  real ( kind = 8 ) p_bc(node_num)
  real ( kind = 8 ) u_bc(node_num)
  real ( kind = 8 ) v_bc(node_num)

  u_bc(1:node_num) = 0.0D+00
  v_bc(1:node_num) = 0.0D+00
  p_bc(1:node_num) = 0.0D+00

  return
end
subroutine rhs ( node_num, node_xy, u_rhs, v_rhs, p_rhs )

!*****************************************************************************80
!
!! RHS gives the right-hand side of the differential equation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 October 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NODE_NUM, the number of nodes.
!
!    Input, real ( kind = 8 ) NODE_XY(2,NODE_NUM), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) U_RHS(NODE_NUM), V_RHS(NODE_NUM), 
!    P_RHS(NODE_NUM), the right hand sides of the differential equations
!    at the nodes.
!
  implicit none

  integer ( kind = 4 ) node_num

  real ( kind = 8 ) node_xy(2,node_num)
  real ( kind = 8 ) p_rhs(node_num)
  real ( kind = 8 ) u_rhs(node_num)
  real ( kind = 8 ) v_rhs(node_num)

  p_rhs(1:node_num) = 0.0D+00
  u_rhs(1:node_num) = 0.0D+00
  v_rhs(1:node_num) = 0.0D+00

  return 
end
