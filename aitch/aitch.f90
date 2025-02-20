program main

!*****************************************************************************80
!
!! MAIN is the main program for AITCH.
!
!  Discussion:
!
!    AITCH tests the AITCH geometry routines.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 April 2004
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) bandwidth
  real ( kind = 8 ), dimension ( 4 ) :: box_x = (/ 0.0D+00, 0.625D+00, 0.75D+00, 1.0D+00 /)
  real ( kind = 8 ), dimension ( 4 ) :: box_y = (/ 0.0D+00, 0.25D+00, 0.375D+00, 0.5D+00 /)
  integer ( kind = 4 ) dof_num
  integer ( kind = 4 ) element
  logical, allocatable, dimension ( : ) :: element_mask
  integer ( kind = 4 ), allocatable, dimension ( :, : ) :: element_node
  integer ( kind = 4 ) element_num
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ml
  integer ( kind = 4 ) mu
  integer ( kind = 4 ) node
  character ( len = 3 ), allocatable, dimension ( :, : ) :: node_dof_type
  integer ( kind = 4 ), allocatable, dimension ( :, : ) :: node_dof_index
  integer ( kind = 4 ) node_num
  logical, allocatable, dimension ( : ) :: node_mask
  real ( kind = 8 ), allocatable, dimension ( : ) :: node_x
  real ( kind = 8 ), allocatable, dimension ( : ) :: node_y
  integer ( kind = 4 ), dimension ( 3 ) :: nx = (/ 5, 1, 2 /)
  integer ( kind = 4 ), dimension ( 3 ) :: ny = (/ 4, 2, 2 /)
  character ( len = 80 ) title

  call timestamp ( )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'AITCH'
  write ( *, '(a)' ) '  FORTRAN90 version.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Geometry model for time-dependent fluid flow'
  write ( *, '(a)' ) '  in a 2-D H-shaped region.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Region widths:'
  write ( *, '(a)' ) ' '
  write ( *, '(4x,4f10.4)' ) box_x(1:4)
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Region heights'
  write ( *, '(a)' ) ' '
  write ( *, '(4x,4f10.4)' ) box_y(1:4)
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Element counts in X direction:'
  write ( *, '(4x,3i6)' )  nx(1:3)
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Element counts in Y direction:'
  write ( *, '(4x,3i6)' )  ny(1:3)
!
!  Determine the number of nodes.
!
  call aitch_node_num ( nx, ny, node_num )
!
!  Allocate space for vectors associated with nodes.
!
  allocate ( node_dof_type(3,node_num) )
  allocate ( node_dof_index(3,node_num) )

  allocate ( node_x(1:node_num) )
  allocate ( node_y(1:node_num) )
!
!  Determine the coordinates of the nodes.
!
  call aitch_node_xy ( nx, ny, node_num, box_x, box_y, node_x, node_y )
!
!  Print SOME of the nodes.
!
  allocate ( node_mask(node_num) )

  title = 'Nodes in top row, middle column.'

  do node = 1, node_num

    node_mask(node) = (              &
      box_x(2) <= node_x(node) .and. &
      node_x(node) <= box_x(3) .and. &
      box_y(3) <= node_y(node) )

  end do

  call node_xy_print ( node_num, node_mask, node_x, node_y, title )

  deallocate ( node_mask )
!
!  Plot SOME of the nodes.
!
  allocate ( node_mask(node_num) )

  do node = 1, node_num
    node_mask(node) = ( node <= 100 .or. mod ( node, 3 ) == 0 )
  end do

! node_mask(1:node_num) = .true.

  title = 'The nodes'

  call node_eps ( 'aitch_test_node.eps', node_num, node_mask, node_x, node_y, title )

  deallocate ( node_mask )
!
!  Determine the number of degrees of freedom.
!
  call aitch_dof_count ( nx, ny, dof_num )
!
!  Assign the degrees of freedom.
!
  call aitch_node_dof_set ( nx, ny, node_num, node_dof_index, node_dof_type )
!
!  Print SOME of the degrees of freedom.
!
  allocate ( node_mask(node_num) )

  title = 'Nodes 1 through 5, and 100 through 120.'

  do node = 1, node_num
    node_mask(node) = ( 1   <= node .and. node <= 5   ) .or. &
                      ( 100 <= node .and. node <= 120 )
  end do

  call node_dof_print ( node_num, node_mask, node_dof_index, node_dof_type, &
    title )

  deallocate ( node_mask )
!
!  Determine the number of elements.
!
  call aitch_element_num ( nx, ny, element_num )
!
!  Determine the nodes that make up the elements.
!
  allocate ( element_node(6,element_num) )

  call aitch_element_node ( nx, ny, element_num, element_node )
!
!  Plot SOME of the elements.
!
  allocate ( element_mask(element_num) )

  do element = 1, element_num
    element_mask(element) = .false.
    do i = 1, 6
      if ( element_node(i,element) == 155 ) then
        element_mask(element) = .true.
      end if
    end do
  end do

! element_mask(1:element_num) = .true.

  title = 'The elements containing node 155'

  call element6_eps ( 'aitch_test_element.eps', node_num, node_x, node_y, &
    element_num, element_mask, element_node, title )

  deallocate ( element_mask )
!
!  Print SOME of the elements.
!
  allocate ( element_mask(element_num) )

  title = 'Elements containing node 155.'

  do element = 1, element_num
    element_mask(element) = .false.
    do i = 1, 6
      if ( element_node(i,element) == 155 ) then
        element_mask(element) = .true.
      end if
    end do
  end do

  call element_node_print ( 6, element_num, element_mask, element_node, title )

  deallocate ( element_mask )
!
!  Compute the bandwidth.
!
  call element_node_bandwidth ( 6, element_num, element_node, dof_num, &
    node_num, node_dof_index, ml, mu, bandwidth )
!
!  Free memory.
!
  deallocate ( element_node )
  deallocate ( node_dof_index )
  deallocate ( node_dof_type )
  deallocate ( node_x )
  deallocate ( node_y )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'AITCH'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine aitch_dof_count ( nx, ny, dof_num )

!*****************************************************************************80
!
!! AITCH_DOF_NUM determines the number of degrees of freedom in the region.
!
!  Diagram:
!
!          +----------------------------+
!          |              :     :       |
!          |              :     :       |
!    NY(3) |              :     :       |
!          |   (3,1)      :(3,2): (3,3) |
!          +--------------+.....+-------+
!    NY(2) :              |     |       :
!          :   empty      |(2,2)| empty :
!          +--------------+.....+-------+
!          |              :     :       |
!    NY(1) |   (1,1)      :(1,2): (1,3) |
!          +----------------------------+
!
!              NX(1)       NX(2)  NX(3)
!
!  Discussion:
!
!    The region is divided into a 3 by 3 grid.  The structure of
!    grid region (I,J) is determined by NX(J) and NY(I).  This
!    region is first subdivided into NX(J) * NY(I) squares.
!    Then each square is split into two triangles, with the diagonal
!    going from the upper left to lower right.  Thus, subregion (I,J)
!    comprises 2 * NX(J) * NY(I) elements.
!
!    Each element, in turn, is made up of 6 nodes.  The corner
!    nodes have 3 degrees of freedom ( U, V, and P), while the
!    side nodes have 2 degrees of freedom (U and V only).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    29 March 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NX(3), the density of elements in the three columns.
!
!    Input, integer ( kind = 4 ) NY(3), the density of elements in the three rows.
!
!    Output, integer ( kind = 4 ) DOF_NUM, the number of degrees of freedom.
!
  implicit none

  integer ( kind = 4 ) dof_num
  integer ( kind = 4 ) node_num_p
  integer ( kind = 4 ) node_num_u
  integer ( kind = 4 ) nx(3)
  integer ( kind = 4 ) ny(3)

  node_num_u = &
      ( 2 * nx(1) + 1 ) * ( 2 * ny(1) + 1 ) &
    + ( 2 * nx(1) + 1 ) * ( 2 * ny(3) + 1 ) &
    + ( 2 * nx(2) - 1 ) * ( 2 * ny(1) + 1 ) &
    + ( 2 * nx(2) + 1 ) * ( 2 * ny(2) - 1 ) &
    + ( 2 * nx(2) - 1 ) * ( 2 * ny(3) + 1 ) &
    + ( 2 * nx(3) + 1 ) * ( 2 * ny(1) + 1 ) &
    + ( 2 * nx(3) + 1 ) * ( 2 * ny(3) + 1 )

  node_num_p = &
      ( nx(1) + 1 ) * ( ny(1) + 1 ) &
    + ( nx(1) + 1 ) * ( ny(3) + 1 ) &
    + ( nx(2) - 1 ) * ( ny(1) + 1 ) &
    + ( nx(2) + 1 ) * ( ny(2) - 1 ) &
    + ( nx(2) - 1 ) * ( ny(3) + 1 ) &
    + ( nx(3) + 1 ) * ( ny(1) + 1 ) &
    + ( nx(3) + 1 ) * ( ny(3) + 1 )

  dof_num = 2 * node_num_u + node_num_p

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'AITCH_DOF_NUM:'
  write ( *, '(a,i6)' ) '  Number of degrees of freedom = ', dof_num

  return
end
subroutine aitch_element_node ( nx, ny, element_num, element_node )

!*****************************************************************************80
!
!! AITCH_ELEMENT_NODE determines the nodes that make up each element.
!
!  Diagram:
!
!          +----------------------------+
!          |              :     :       |
!          |              :     :       |
!    NY(3) |              :     :       |
!          |   (3,1)      :(3,2): (3,3) |
!          +--------------+.....+-------+
!    NY(2) :              |     |       :
!          :   empty      |(2,2)| empty :
!          +--------------+.....+-------+
!          |              :     :       |
!    NY(1) |   (1,1)      :(1,2): (1,3) |
!          +----------------------------+
!
!              NX(1)       NX(2)  NX(3)
!
!  Discussion:
!
!    The region is divided into a 3 by 3 grid.  The structure of
!    grid region (I,J) is determined by NX(J) and NY(I).  This
!    region is first subdivided into NX(J) * NY(I) squares.
!    Then each square is split into two triangles, with the diagonal
!    going from the upper left to lower right.  Thus, subregion (I,J)
!    comprises 2 * NX(J) * NY(I) elements.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 March 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NX(3), the density of elements in the three columns.
!
!    Input, integer ( kind = 4 ) NY(3), the density of elements in the three rows.
!
!    Input, integer ( kind = 4 ) ELEMENT_NUM, the number of elements.
!
!    Output, integer ( kind = 4 ) ELEMENT_NODE(6,ELEMENT_NUM), the nodes that make up
!    each element.
!
  implicit none

  integer ( kind = 4 ) element_num

  integer ( kind = 4 ) col
  integer ( kind = 4 ) col2
  integer ( kind = 4 ) element
  integer ( kind = 4 ) element_node(6,element_num)
  integer ( kind = 4 ) inc1
  integer ( kind = 4 ) inc2
  integer ( kind = 4 ) node_sw
  integer ( kind = 4 ) nx(3)
  integer ( kind = 4 ) ny(3)
  integer ( kind = 4 ) row
  integer ( kind = 4 ) row2

  element = 0

  do col = 1, 3
    do col2 = 1, nx(col)

      do row = 1, 3

        if ( row /= 2 .or. col == 2 ) then

          if ( col == 1 ) then

            if ( col2 < nx(1) ) then

              if ( row == 1 ) then

                if ( col2 == 1 ) then
                  node_sw = 1
                else
                  node_sw = node_sw + inc1 + 1
                end if

              else

                node_sw = node_sw + 1

              end if

              inc1 = ( 2 * ny(1) + 1 ) &
                   + ( 2 * ny(3) + 1 )

              inc2 = ( 2 * ny(1) + 1 ) &
                   + ( 2 * ny(3) + 1 )

            else if ( row == 1 ) then

              node_sw = node_sw + inc1 + 1

              inc1 = ( 2 * ny(1) + 1 ) &
                   + ( 2 * ny(3) + 1 )

              inc2 = ( 2 * ny(1) + 1 ) &
                   + ( 2 * ny(3) + 1 )

            else if ( row == 3 ) then

              node_sw = node_sw + 1

              inc1 = ( 2 * ny(1) + 1 ) &
                   + ( 2 * ny(3) + 1 )

              inc2 = ( 2 * ny(1) + 1 ) &
                   + ( 2 * ny(2) - 1 ) &
                   + ( 2 * ny(3) + 1 )

            end if

          else if ( col == 2 ) then

            if ( row == 1 ) then

              node_sw = node_sw + inc1 + 1

            end if

            inc1 = ( 2 * ny(1) + 1 ) &
                 + ( 2 * ny(2) - 1 ) &
                 + ( 2 * ny(3) + 1 )

            inc2 = ( 2 * ny(1) + 1 ) &
                 + ( 2 * ny(2) - 1 ) &
                 + ( 2 * ny(3) + 1 )

          else if ( col == 3 ) then

            if ( 1 < col2 ) then

              if ( row == 1 ) then
                node_sw = node_sw + inc1 + 1
              else
                node_sw = node_sw + 1
              end if

              inc1 = ( 2 * ny(1) + 1 ) &
                   + ( 2 * ny(3) + 1 )

              inc2 = ( 2 * ny(1) + 1 ) &
                   + ( 2 * ny(3) + 1 )

            else if ( row == 1 ) then

              node_sw = node_sw + inc1 + 1

              inc1 = ( 2 * ny(1) + 1 ) &
                   + ( 2 * ny(2) - 1 ) &
                   + ( 2 * ny(3) + 1 )

              inc2 = ( 2 * ny(1) + 1 ) &
                   + ( 2 * ny(3) + 1 )

            else if ( row == 3 ) then

              node_sw = node_sw + ( 2 * ny(2) - 1 ) + 1

              inc1 = ( 2 * ny(1) + 1 ) &
                   + ( 2 * ny(3) + 1 )

              inc2 = ( 2 * ny(1) + 1 ) &
                   + ( 2 * ny(3) + 1 )

            end if

          end if

          do row2 = 1, ny(row)

            element = element + 1
            element_node(1,element) = node_sw
            element_node(2,element) = node_sw + inc1 + inc2
            element_node(3,element) = node_sw               + 2
            element_node(4,element) = node_sw + inc1
            element_node(5,element) = node_sw + inc1        + 1
            element_node(6,element) = node_sw               + 1

            element = element + 1
            element_node(1,element) = node_sw + inc1 + inc2 + 2
            element_node(2,element) = node_sw               + 2
            element_node(3,element) = node_sw + inc1 + inc2
            element_node(4,element) = node_sw + inc1        + 2
            element_node(5,element) = node_sw + inc1        + 1
            element_node(6,element) = node_sw + inc1 + inc2 + 1

            node_sw = node_sw + 2

          end do

        end if

      end do
    end do
  end do

  return
end
subroutine aitch_element_num ( nx, ny, element_num )

!*****************************************************************************80
!
!! AITCH_ELEMENT_NUM determines the number of elements in the region.
!
!  Diagram:
!
!          +----------------------------+
!          |              :     :       |
!          |              :     :       |
!    NY(3) |              :     :       |
!          |   (3,1)      :(3,2): (3,3) |
!          +--------------+.....+-------+
!    NY(2) :              |     |       :
!          :   empty      |(2,2)| empty :
!          +--------------+.....+-------+
!          |              :     :       |
!    NY(1) |   (1,1)      :(1,2): (1,3) |
!          +----------------------------+
!
!              NX(1)       NX(2)  NX(3)
!
!  Discussion:
!
!    The region is divided into a 3 by 3 grid.  The structure of
!    grid region (I,J) is determined by NX(J) and NY(I).  This
!    region is first subdivided into NX(J) * NY(I) squares.
!    Then each square is split into two triangles, with the diagonal
!    going from the upper left to lower right.  Thus, subregion (I,J)
!    comprises 2 * NX(J) * NY(I) elements.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    25 March 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NX(3), the density of elements in the three columns.
!
!    Input, integer ( kind = 4 ) NY(3), the density of elements in the three rows.
!
!    Output, integer ( kind = 4 ) ELEMENT_NUM, the number of elements.
!
  implicit none

  integer ( kind = 4 ) element_num
  integer ( kind = 4 ) nx(3)
  integer ( kind = 4 ) ny(3)

  element_num = &
      2 * nx(1) * ny(1) &
    + 2 * nx(1) * ny(3) &
    + 2 * nx(2) * ny(1) &
    + 2 * nx(2) * ny(2) &
    + 2 * nx(2) * ny(3) &
    + 2 * nx(3) * ny(1) &
    + 2 * nx(3) * ny(3)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'AITCH_ELEMENT_NUM:'
  write ( *, '(a,i6)' ) '  Number of elements = ', element_num

  return
end
subroutine aitch_node_dof_set ( nx, ny, node_num, node_dof_index, &
  node_dof_type )

!*****************************************************************************80
!
!! AITCH_NODE_DOF_SET assigns degrees of freedom to each node.
!
!  Diagram:
!
!          +----------------------------+
!          |              :     :       |
!          |              :     :       |
!    NY(3) |              :     :       |
!          |              :     :       |
!          +--------------+.....+-------+
!    NY(2) :              |     |       :
!          :   empty      |     | empty :
!          +--------------+.....+-------+
!          |              :     :       |
!    NY(1) |              :     :       |
!          +----------------------------+
!
!              NX(1)       NX(2)  NX(3)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 April 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NX(3), the density of elements in the three columns.
!
!    Input, integer ( kind = 4 ) NY(3), the density of elements in the three rows.
!
!    Input, integer ( kind = 4 ) NODE_NUM, the number of nodes.
!
!    Output, integer ( kind = 4 ) NODE_DOF_INDEX(3,NODE_NUM), the nodal degrees of freedom.
!
!    Output, character ( len = 3 ) NODE_DOF_TYPE(3,NODE_NUM), the type of
!    each nodal degree of freedom.
!
  implicit none

  integer ( kind = 4 ) node_num

  integer ( kind = 4 ) col
  integer ( kind = 4 ) dof
  logical found
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) node
  integer ( kind = 4 ) node_dof_index(3,node_num)
  character ( len = 3 ) node_dof_type(3,node_num)
  integer ( kind = 4 ) nx(3)
  integer ( kind = 4 ) ny(3)
  integer ( kind = 4 ) row

  node = 0
  dof = 0
  j = 0

  do col = 1, 2 * nx(1)

    j = j + 1
    i = 0

    do row = 1, 2 * ny(1) + 1

      i = i + 1
      node = node + 1

      dof = dof + 1
      node_dof_index(1,node) = dof
      node_dof_type(1,node) = 'UM'

      dof = dof + 1
      node_dof_index(2,node) = dof
      node_dof_type(2,node) = 'VM'

      if ( mod ( j, 2 ) == 1 .and. mod ( i, 2 ) == 1 ) then
        dof = dof + 1
        node_dof_index(3,node) = dof
        node_dof_type(3,node) = 'PC'
      else
        node_dof_index(3,node) = -1
        node_dof_type(3,node) = '-1'
      end if

    end do

    i = i + 2 * ny(2) - 1

    do row = 1, 2 * ny(3) + 1

      i = i + 1
      node = node + 1

      dof = dof + 1
      node_dof_index(1,node) = dof
      node_dof_type(1,node) = 'UM'

      dof = dof + 1
      node_dof_index(2,node) = dof
      node_dof_type(2,node) = 'VM'

      if ( mod ( j, 2 ) == 1 .and. mod ( i, 2 ) == 1 ) then
        dof = dof + 1
        node_dof_index(3,node) = dof
        node_dof_type(3,node) = 'PC'
      else
        node_dof_index(3,node) = -1
        node_dof_type(3,node) = '-1'
      end if

    end do

  end do

  do col = 1, 2 * nx(2) + 1

    j = j + 1
    i = 0

    do row = 1, 2 * ny(1) + 1

      i = i + 1
      node = node + 1

      dof = dof + 1
      node_dof_index(1,node) = dof
      node_dof_type(1,node) = 'UM'
      dof = dof + 1
      node_dof_index(2,node) = dof
      node_dof_type(2,node) = 'VM'

      if ( mod ( j, 2 ) == 1 .and. mod ( i, 2 ) == 1 ) then
        dof = dof + 1
        node_dof_index(3,node) = dof
        node_dof_type(3,node) = 'PC'
      else
        node_dof_index(3,node) = -1
        node_dof_type(3,node) = '**'
      end if

    end do

    do row = 2, 2 * ny(2)

      i = i + 1
      node = node + 1

      dof = dof + 1
      node_dof_index(1,node) = dof
      node_dof_type(1,node) = 'UM'

      dof = dof + 1
      node_dof_index(2,node) = dof
      node_dof_type(2,node) = 'VM'

      if ( mod ( j, 2 ) == 1 .and. mod ( i, 2 ) == 1 ) then
        dof = dof + 1
        node_dof_index(3,node) = dof
        node_dof_type(3,node) = 'PC'
      else
        node_dof_index(3,node) = -1
        node_dof_type(3,node) = '**'
      end if

    end do

    do row = 1, 2 * ny(3) + 1

      i = i + 1
      node = node + 1

      dof = dof + 1
      node_dof_index(1,node) = dof
      node_dof_type(1,node) = 'UM'

      dof = dof + 1
      node_dof_index(2,node) = dof
      node_dof_type(2,node) = 'VM'

      if ( mod ( j, 2 ) == 1 .and. mod ( i, 2 ) == 1 ) then
        dof = dof + 1
        node_dof_index(3,node) = dof
        node_dof_type(3,node) = 'PC'
      else
        node_dof_index(3,node) = -1
        node_dof_type(3,node) = '**'
      end if

    end do

  end do

  do col = 2, 2 * nx(3) + 1

    j = j + 1
    i = 0

    do row = 1, 2 * ny(1) + 1

      i = i + 1
      node = node + 1

      dof = dof + 1
      node_dof_index(1,node) = dof
      node_dof_type(1,node) = 'UM'

      dof = dof + 1
      node_dof_index(2,node) = dof
      node_dof_type(2,node) = 'VM'

      if ( mod ( j, 2 ) == 1 .and. mod ( i, 2 ) == 1 ) then
        dof = dof + 1
        node_dof_index(3,node) = dof
        node_dof_type(3,node) = 'PC'
      else
        node_dof_index(3,node) = -1
        node_dof_type(3,node) = '**'
      end if

    end do

    i = i + 2 * ny(2) - 1

    do row = 1, 2 * ny(3) + 1

      i = i + 1
      node = node + 1

      dof = dof + 1
      node_dof_index(1,node) = dof
      node_dof_type(1,node) = 'UM'

      dof = dof + 1
      node_dof_index(2,node) = dof
      node_dof_type(2,node) = 'UM'

      if ( mod ( j, 2 ) == 1 .and. mod ( i, 2 ) == 1 ) then
        dof = dof + 1
        node_dof_index(3,node) = dof
        node_dof_type(3,node) = 'PC'
      else
        node_dof_index(3,node) = -1
        node_dof_type(3,node) = '**'
      end if

    end do

  end do
!
!  Replace one continuity equation by a pressure = 0 equation.
!
  found = .false.

  do node = 1, node_num

    if ( node_dof_type(3,node) == 'PC' ) then
      node_dof_type(3,node) = 'PD0'
      found = .true.
      exit
    end if

  end do

  if ( .not. found ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'AITCH_NODE_DOF_SET - Fatal error!'
    write ( *, '(a)' ) '  Could not find a degree of freedom associated'
    write ( *, '(a)' ) '  with the continuity equation.'
    stop
  end if

  return
end
subroutine aitch_node_num ( nx, ny, node_num )

!*****************************************************************************80
!
!! AITCH_NODE_NUM determines the number of nodes in the region.
!
!  Diagram:
!
!          +----------------------------+
!          |              :     :       |
!          |              :     :       |
!    NY(3) |              :     :       |
!          |   (3,1)      :(3,2): (3,3) |
!          +--------------+.....+-------+
!    NY(2) :              |     |       :
!          :   empty      |(2,2)| empty :
!          +--------------+.....+-------+
!          |              :     :       |
!    NY(1) |   (1,1)      :(1,2): (1,3) |
!          +----------------------------+
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    22 March 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NX(3), the density of elements in the three columns.
!
!    Input, integer ( kind = 4 ) NY(3), the density of elements in the three rows.
!
!    Output, integer ( kind = 4 ) NODE_NUM, the number of nodes.
!
  implicit none

  integer ( kind = 4 ) node_num
  integer ( kind = 4 ) nx(3)
  integer ( kind = 4 ) ny(3)
!
!  Count the nodes.
!
  node_num = &
      ( 2 * nx(1) + 1 ) * ( 2 * ny(1) + 1 ) &
    + ( 2 * nx(1) + 1 ) * ( 2 * ny(3) + 1 ) &
    + ( 2 * nx(2) - 1 ) * ( 2 * ny(1) + 1 ) &
    + ( 2 * nx(2) + 1 ) * ( 2 * ny(2) - 1 ) &
    + ( 2 * nx(2) - 1 ) * ( 2 * ny(3) + 1 ) &
    + ( 2 * nx(3) + 1 ) * ( 2 * ny(1) + 1 ) &
    + ( 2 * nx(3) + 1 ) * ( 2 * ny(3) + 1 )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'AITCH_NODE_NUM:'
  write ( *, '(a,i6)' ) '  Number of nodes = ', node_num

  return
end
subroutine aitch_node_xy ( nx, ny, node_num, box_x, box_y, node_x, node_y )

!*****************************************************************************80
!
!! AITCH_NODE_XY assigns coordinates to each node.
!
!  Diagram:
!
!          +----------------------------+
!          |              :     :       |
!          |              :     :       |
!    NY(3) |              :     :       |
!          |              :     :       |
!          +--------------+.....+-------+
!    NY(2) :              |     |       :
!          :   empty      |     | empty :
!          +--------------+.....+-------+
!          |              :     :       |
!    NY(1) |              :     :       |
!          +----------------------------+
!
!              NX(1)       NX(2)  NX(3)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    31 March 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NX(3), the density of elements in the three columns.
!
!    Input, integer ( kind = 4 ) NY(3), the density of elements in the three rows.
!
!    Input, integer ( kind = 4 ) NODE_NUM, the number of nodes.
!
!    Input, real ( kind = 8 ) BOX_X(4), the X coordinates that define the
!    ranges of the three columns.
!
!    Input, real ( kind = 8 ) BOX_Y(4), the Y coordinates that define the
!    ranges of the three rows.
!
!    Output, real ( kind = 8 ) NODE_X(NODE_NUM), NODE_Y(NODE_NUM),
!    the X and Y coordinates of the nodes.
!
  implicit none

  integer ( kind = 4 ) node_num

  real ( kind = 8 ) box_x(4)
  real ( kind = 8 ) box_y(4)
  integer ( kind = 4 ) col
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) node
  real ( kind = 8 ) node_x(node_num)
  real ( kind = 8 ) node_y(node_num)
  integer ( kind = 4 ) nx(3)
  integer ( kind = 4 ) ny(3)
  integer ( kind = 4 ) row

  node = 0
  j = 0

  do col = 1, 2 * nx(1)

    j = j + 1
    i = 0

    do row = 1, 2 * ny(1) + 1

      i = i + 1
      node = node + 1
      node_x(node) = ( real ( 2 * nx(1) + 1 - col, kind = 8 ) * box_x(1)   &
                     + real (           - 1 + col, kind = 8 ) * box_x(2) ) &
                     / real ( 2 * nx(1),           kind = 8)
      node_y(node) = ( real ( 2 * ny(1) + 1 - row, kind = 8 ) * box_y(1)   &
                     + real (           - 1 + row, kind = 8 ) * box_y(2) ) &
                     / real ( 2 * ny(1),           kind = 8)

    end do

    i = i + 2 * ny(2) - 1

    do row = 1, 2 * ny(3) + 1

      i = i + 1
      node = node + 1
      node_x(node) = ( real ( 2 * nx(1) + 1 - col, kind = 8 ) * box_x(1)   &
                     + real (           - 1 + col, kind = 8 ) * box_x(2) ) &
                     / real ( 2 * nx(1), kind = 8)
      node_y(node) = ( real ( 2 * ny(3) + 1 - row, kind = 8 ) * box_y(3)   &
                     + real (           - 1 + row, kind = 8 ) * box_y(4) ) &
                     / real ( 2 * ny(3), kind = 8)

    end do

  end do

  do col = 1, 2 * nx(2) + 1

    j = j + 1
    i = 0

    do row = 1, 2 * ny(1) + 1

      i = i + 1
      node = node + 1
      node_x(node) = ( real ( 2 * nx(2) + 1 - col, kind = 8 ) * box_x(2)   &
                     + real (           - 1 + col, kind = 8 ) * box_x(3) ) &
                     / real ( 2 * nx(2), kind = 8)
      node_y(node) = ( real ( 2 * ny(1) + 1 - row, kind = 8 ) * box_y(1)   &
                     + real (           - 1 + row, kind = 8 ) * box_y(2) ) &
                     / real ( 2 * ny(1), kind = 8)

    end do

    do row = 2, 2 * ny(2)

      i = i + 1
      node = node + 1
      node_x(node) = ( real ( 2 * nx(2) + 1 - col, kind = 8 ) * box_x(2)   &
                     + real (           - 1 + col, kind = 8 ) * box_x(3) ) &
                     / real ( 2 * nx(2), kind = 8)
      node_y(node) = ( real ( 2 * ny(2) + 1 - row, kind = 8 ) * box_y(2)   &
                     + real (           - 1 + row, kind = 8 ) * box_y(3) ) &
                     / real ( 2 * ny(2), kind = 8)

    end do

    do row = 1, 2 * ny(3) + 1

      i = i + 1
      node = node + 1
      node_x(node) = ( real ( 2 * nx(2) + 1 - col, kind = 8 ) * box_x(2)   &
                     + real (           - 1 + col, kind = 8 ) * box_x(3) ) &
                     / real ( 2 * nx(2), kind = 8)
      node_y(node) = ( real ( 2 * ny(3) + 1 - row, kind = 8 ) * box_y(3)   &
                     + real (           - 1 + row, kind = 8 ) * box_y(4) ) &
                     / real ( 2 * ny(3), kind = 8)

    end do

  end do

  do col = 2, 2 * nx(3) + 1

    j = j + 1
    i = 0

    do row = 1, 2 * ny(1) + 1

      i = i + 1
      node = node + 1
      node_x(node) = ( real ( 2 * nx(3) + 1 - col, kind = 8 ) * box_x(3)   &
                     + real (           - 1 + col, kind = 8 ) * box_x(4) ) &
                     / real ( 2 * nx(3), kind = 8)
      node_y(node) = ( real ( 2 * ny(1) + 1 - row, kind = 8 ) * box_y(1)   &
                     + real (           - 1 + row, kind = 8 ) * box_y(2) ) &
                     / real ( 2 * ny(1), kind = 8)

    end do

    i = i + 2 * ny(2) - 1

    do row = 1, 2 * ny(3) + 1
      i = i + 1
      node = node + 1
      node_x(node) = ( real ( 2 * nx(3) + 1 - col, kind = 8 ) * box_x(3)   &
                     + real (           - 1 + col, kind = 8 ) * box_x(4) ) &
                     / real ( 2 * nx(3), kind = 8)
      node_y(node) = ( real ( 2 * ny(3) + 1 - row, kind = 8 ) * box_y(3)   &
                     + real (           - 1 + row, kind = 8 ) * box_y(4) ) &
                     / real ( 2 * ny(3), kind = 8)
    end do

  end do

  return
end
subroutine element6_eps ( file_name, node_num, node_x, node_y, element_num, &
  element_mask, element_node, title )

!*****************************************************************************80
!
!! ELEMENT6_EPS creates an EPS file containing an image of the mesh.
!
!  Discussion:
!
!    This routine has been specialized to deal correctly ONLY with
!    a mesh of 6 node elements, with the property that starting
!    from local node 1 and traversing the edges of the element will
!    result in encountering local nodes 1, 4, 2, 5, 3, 6 in that order.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    05 April 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) FILE_NAME, the name of the file to create.
!
!    Input, integer ( kind = 4 ) NODE_NUM, the number of nodes.
!
!    Input, real ( kind = 8 ) NODE_X(NODE_NUM), NODE_Y(NODE_NUM), the coordinates
!    of the nodes.
!
!    Input, integer ( kind = 4 ) ELEMENT_NUM, the number of elements.
!
!    Input, logical ELEMENT_MASK(ELEMENT_NUM), a mask for the elements.
!
!    Input, integer ( kind = 4 ) ELEMENT_NODE(6,ELEMENT_NUM), the element->node data.
!
!    Input, character ( len = * ) TITLE, a title for the plot.
!
  implicit none

  integer ( kind = 4 ) element_num
  integer ( kind = 4 ) node_num

  real ( kind = 8 ) ave_x
  real ( kind = 8 ) ave_y
  integer ( kind = 4 ), parameter :: circle_size = 3
  real ( kind = 8 ) dif
  integer ( kind = 4 ) element
  logical element_mask(element_num)
  integer ( kind = 4 ) element_node(6,element_num)
  integer ( kind = 4 ) eps_unit
  integer ( kind = 4 ) eps_x
  integer ( kind = 4 ) eps_y
  character ( len = * ) file_name
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ios
  integer ( kind = 4 ) ip1
  integer ( kind = 4 ) j
  integer ( kind = 4 ) node
  logical node_mask(node_num)
  real ( kind = 8 ) node_x(node_num)
  real ( kind = 8 ) node_x_max
  real ( kind = 8 ) node_x_min
  real ( kind = 8 ) node_y(node_num)
  real ( kind = 8 ) node_y_max
  real ( kind = 8 ) node_y_min
  integer ( kind = 4 ), dimension ( 6 ) :: order = (/ 1, 4, 2, 5, 3, 6 /)
  real ( kind = 8 ) scale
  character ( len = 40 ) string
  character ( len = * ) title
!
!  Determine the range of the unmasked elements.
!
  node_x_min =  huge ( node_x_min )
  node_x_max = -huge ( node_x_max )
  node_y_min =  huge ( node_y_min )
  node_y_max = -huge ( node_y_max )

  node_mask(1:node_num) = .false.

  do element = 1, element_num
    if ( element_mask(element) ) then
      do j = 1, 6
        node = element_node(j,element)
        node_mask(node) = .true.
        node_x_min = min ( node_x_min, node_x(node) )
        node_x_max = max ( node_x_max, node_x(node) )
        node_y_min = min ( node_y_min, node_y(node) )
        node_y_max = max ( node_y_max, node_y(node) )
      end do
    end if
  end do

  if ( node_y_max - node_y_min < node_x_max - node_x_min ) then
    scale = node_x_max - node_x_min
    dif = ( node_x_max - node_x_min ) - ( node_y_max - node_y_min )
    node_y_max = node_y_max + 0.5 * dif
    node_y_min = node_y_min - 0.5 * dif
  else
    scale = node_y_max - node_y_min
    dif = ( node_y_max - node_y_min ) - ( node_x_max - node_x_min )
    node_x_max = node_x_max + 0.5 * dif
    node_x_min = node_x_min - 0.5 * dif
  end if

  call get_unit ( eps_unit )

  open ( unit = eps_unit, file = file_name, status = 'replace', iostat = ios )

  if ( ios /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'ELEMENT6_EPS - Fatal error!'
    write ( *, '(a)' ) '  Could not open the output EPS file.'
    stop
  end if

  write ( eps_unit, '(a)' ) '%!PS-Adobe-3.0 EPSF-3.0'
  write ( eps_unit, '(a)' ) '%%Creator: element6_eps(aitch.f90)'
  write ( eps_unit, '(a)' ) '%%Title: ' // trim ( file_name )
  write ( eps_unit, '(a)' ) '%%Pages: 1'
  write ( eps_unit, '(a)' ) '%%BoundingBox:    36    36   576   756'
  write ( eps_unit, '(a)' ) '%%Document-Fonts: Times-Roman'
  write ( eps_unit, '(a)' ) '%%LanguageLevel: 1'
  write ( eps_unit, '(a)' ) '%%EndComments'
  write ( eps_unit, '(a)' ) '%%BeginProlog'
  write ( eps_unit, '(a)' ) '/inch {72 mul} def'
  write ( eps_unit, '(a)' ) '%%EndProlog'
  write ( eps_unit, '(a)' ) '%%Page:      1     1'
  write ( eps_unit, '(a)' ) 'save'
  write ( eps_unit, '(a)' ) '%'
  write ( eps_unit, '(a)' ) '% Set RGB line color.'
  write ( eps_unit, '(a)' ) '%'
  write ( eps_unit, '(a)' ) ' 0.9000 0.9000 0.9000 setrgbcolor'
  write ( eps_unit, '(a)' ) '%'
  write ( eps_unit, '(a)' ) '% Draw a gray border around the page.'
  write ( eps_unit, '(a)' ) '%'
  write ( eps_unit, '(a)' ) 'newpath'
  write ( eps_unit, '(a)' ) '    36   126 moveto'
  write ( eps_unit, '(a)' ) '   576   126 lineto'
  write ( eps_unit, '(a)' ) '   576   666 lineto'
  write ( eps_unit, '(a)' ) '    36   666 lineto'
  write ( eps_unit, '(a)' ) '    36   126 lineto'
  write ( eps_unit, '(a)' ) 'stroke'
  write ( eps_unit, '(a)' ) '%'
  write ( eps_unit, '(a)' ) '% Set RGB line color.'
  write ( eps_unit, '(a)' ) '%'
  write ( eps_unit, '(a)' ) ' 0.0000 0.0000 0.0000 setrgbcolor'

  write ( eps_unit, '(a)' ) '%'
  write ( eps_unit, '(a)' ) '%  Label the plot:'
  write ( eps_unit, '(a)' ) '%'
  write ( eps_unit, '(a)' ) ' 0.0000 0.0000 0.0000 setrgbcolor'
  write ( eps_unit, '(a)' ) '/Times-Roman findfont 0.50 inch scalefont setfont'
  write ( eps_unit, '(a)' ) '    36   666 moveto'
  write ( eps_unit, '(a)' ) '(' // trim ( title ) // ') show'

  write ( eps_unit, '(a)' ) '%'
  write ( eps_unit, '(a)' ) '% Define a clipping polygon'
  write ( eps_unit, '(a)' ) '%'
  write ( eps_unit, '(a)' ) '    36   126 moveto'
  write ( eps_unit, '(a)' ) '   576   126 lineto'
  write ( eps_unit, '(a)' ) '   576   666 lineto'
  write ( eps_unit, '(a)' ) '    36   666 lineto'
  write ( eps_unit, '(a)' ) '    36   126 lineto'
  write ( eps_unit, '(a)' ) 'clip newpath'

  write ( eps_unit, '(a)' ) '%'
  write ( eps_unit, '(a)' ) '%  Draw filled dots at each node:'
  write ( eps_unit, '(a)' ) '%'
  write ( eps_unit, '(a)' ) ' 0.0000 0.0000 0.9000 setrgbcolor'

  do node = 1, node_num

    if ( node_mask(node) ) then

      eps_x = int ( &
        ( ( node_x_max - node_x(node)              ) *  61.0E+00   &
        + (            + node_x(node) - node_x_min ) * 551.0E+00 ) &
        / scale )

      eps_y = int ( &
        ( ( node_y_max - node_y(node)              ) * 151.0E+00   &
        + (              node_y(node) - node_y_min ) * 641.0E+00 ) &
        / scale )

      write ( eps_unit, '(a,i4,2x,i4,2x,i4,a)' ) &
        'newpath  ', eps_x, eps_y, circle_size, ' 0 360 arc closepath fill'

    end if

  end do

  write ( eps_unit, '(a)' ) '%'
  write ( eps_unit, '(a)' ) '%  Label the nodes:'
  write ( eps_unit, '(a)' ) '%'
  write ( eps_unit, '(a)' ) ' 0.0000 0.0000 1.0000 setrgbcolor'
  write ( eps_unit, '(a)' ) '/Times-Roman findfont 0.20 inch scalefont setfont'

  do node = 1, node_num

    if ( node_mask(node) ) then

      eps_x = int ( &
        ( ( node_x_max - node_x(node)              ) *  61.0E+00   &
        + (            + node_x(node) - node_x_min ) * 551.0E+00 ) &
        / scale )

      eps_y = int ( &
        ( ( node_y_max - node_y(node)              ) * 151.0E+00   &
        + (              node_y(node) - node_y_min ) * 641.0E+00 ) &
        / scale )

      write ( string, '(i4)' ) node
      string = adjustl ( string )

      write ( eps_unit, '(i4,2x,i4,a)' ) eps_x, eps_y+5, &
        ' moveto (' // trim ( string ) // ') show'

    end if

  end do

  write ( eps_unit, '(a)' ) '%'
  write ( eps_unit, '(a)' ) '%  Draw the element sides:'
  write ( eps_unit, '(a)' ) '%'
  write ( eps_unit, '(a)' ) ' 0.9000 0.0000 0.0000 setrgbcolor'

  do element = 1, element_num

    if ( .not. element_mask(element) ) then
      cycle
    end if

    node = element_node(order(1),element)

    eps_x = int ( &
      ( ( node_x_max - node_x(node)              ) *  61.0E+00   &
      + (            + node_x(node) - node_x_min ) * 551.0E+00 ) &
      / scale )

    eps_y = int ( &
      ( ( node_y_max - node_y(node)              ) * 151.0E+00   &
      + (              node_y(node) - node_y_min ) * 641.0E+00 ) &
      / scale )

    write ( eps_unit, '(a,i4,2x,i4,a)' ) 'newpath ', eps_x, eps_y, ' moveto'

    do i = 1, 6

      ip1 = mod ( i, 6 ) + 1;
      node = element_node(order(ip1),element)

      eps_x = int ( &
        ( ( node_x_max - node_x(node)              ) *  61.0E+00   &
        + (            + node_x(node) - node_x_min ) * 551.0E+00 ) &
        / scale )

      eps_y = int ( &
        ( ( node_y_max - node_y(node)              ) * 151.0E+00   &
        + (              node_y(node) - node_y_min ) * 641.0E+00 ) &
        / scale )

      write ( eps_unit, '(i4,2x,i4,a)' ) eps_x, eps_y, ' lineto'

    end do

    write ( eps_unit, '(a)' ) 'stroke'

  end do

  write ( eps_unit, '(a)' ) '%'
  write ( eps_unit, '(a)' ) '%  Label the elements:'
  write ( eps_unit, '(a)' ) '%'
  write ( eps_unit, '(a)' ) ' 1.0000 0.0000 0.0000 setrgbcolor'
  write ( eps_unit, '(a)' ) '/Times-Roman findfont 0.30 inch scalefont setfont'

  do element = 1, element_num

    if ( .not. element_mask(element) ) then
      cycle
    end if

    ave_x = 0.0E+00
    ave_y = 0.0E+00

    do i = 4, 6

      node = element_node(i,element)

      ave_x = ave_x + node_x(node)
      ave_y = ave_y + node_y(node)

    end do

    ave_x = ave_x / 3.0D+00 
    ave_y = ave_y / 3.0D+00

    eps_x = int ( &
      ( ( node_x_max - ave_x              ) *  61.0E+00   &
      + (            + ave_x - node_x_min ) * 551.0E+00 ) &
      / scale )

    eps_y = int ( &
      ( ( node_y_max - ave_y              ) * 151.0E+00   &
      + (              ave_y - node_y_min ) * 641.0E+00 ) &
      / scale )

    write ( string, '(i4)' ) element
    string = adjustl ( string )

    write ( eps_unit, '(i4,2x,i4,a)' ) eps_x, eps_y, ' moveto (' &
      // trim ( string ) // ') show'

  end do

  write ( eps_unit, '(a)' ) '%'
  write ( eps_unit, '(a)' ) 'restore showpage'
  write ( eps_unit, '(a)' ) '%'
  write ( eps_unit, '(a)' ) '% End of page'
  write ( eps_unit, '(a)' ) '%'
  write ( eps_unit, '(a)' ) '%%Trailer'
  write ( eps_unit, '(a)' ) '%%EOF'

  close ( unit = eps_unit )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ELEMENT6_EPS:'
  write ( *, '(a)' ) '  An encapsulated PostScript file was created'
  write ( *, '(a)' ) '  containing an image of the nodes and elements.'
  write ( *, '(a)' ) '  The file is named "' // trim ( file_name ) // '".'

  return
end
subroutine element_node_bandwidth ( n, element_num, element_node, dof_num, &
  node_num, node_dof_index, ml, mu, bandwidth )

!*****************************************************************************80
!
!! ELEMENT_NODE_BANDWIDTH determines the bandwidth associated with the grid.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    31 March 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the elements.
!
!    Input, integer ( kind = 4 ) ELEMENT_NUM, the number of elements.
!
!    Input, integer ( kind = 4 ) ELEMENT_NODE(N,ELEMENT_NUM), the nodes in each element.
!
!    Input, integer ( kind = 4 ) DOF_NUM, the number of degrees of freedom.
!
!    Input, integer ( kind = 4 ) NODE_NUM, the number of nodes.
!
!    Input, integer ( kind = 4 ) NODE_DOF_INDEX(3,NODE_NUM), the nodal degrees of freedom.
!
!    Output, integer ( kind = 4 ) ML, MU, BANDWIDTH, the lower, upper and total
!    bandwidths.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) dof_num
  integer ( kind = 4 ) element_num
  integer ( kind = 4 ) node_num

  integer ( kind = 4 ) bandwidth
  integer ( kind = 4 ) dof
  integer ( kind = 4 ) dof_max(dof_num)
  integer ( kind = 4 ) dof_min(dof_num)
  integer ( kind = 4 ) element
  integer ( kind = 4 ) element_node(n,element_num)
  integer ( kind = 4 ) l1
  integer ( kind = 4 ) l2
  integer ( kind = 4 ) ml
  integer ( kind = 4 ) mu
  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n2
  integer ( kind = 4 ) node_dof_index(3,node_num)
  integer ( kind = 4 ) p1
  integer ( kind = 4 ) p2
  integer ( kind = 4 ) u1
  integer ( kind = 4 ) u2
  integer ( kind = 4 ) v1
  integer ( kind = 4 ) v2

  do dof = 1, dof_num
    dof_min(dof) = dof
    dof_max(dof) = dof
  end do

  do element = 1, element_num

    do l1 = 1, n

      n1 = element_node(l1,element)

      u1 = node_dof_index(1,n1)
      v1 = node_dof_index(2,n1)
      p1 = node_dof_index(3,n1)

      do l2 = 1, n

        n2 = element_node(l2,element)

        u2 = node_dof_index(1,n2)
        v2 = node_dof_index(2,n2)
        p2 = node_dof_index(3,n2)

        dof_min(u1) = min ( dof_min(u1), u2 )
        dof_max(u1) = max ( dof_max(u1), u2 )
        dof_min(u1) = min ( dof_min(u1), v2 )
        dof_max(u1) = max ( dof_max(u1), v2 )

        if ( 1 <= p2 ) then
          dof_min(u1) = min ( dof_min(u1), p2 )
          dof_max(u1) = max ( dof_max(u1), p2 )
        end if

        dof_min(v1) = min ( dof_min(v1), u2 )
        dof_max(v1) = max ( dof_max(v1), u2 )
        dof_min(v1) = min ( dof_min(v1), v2 )
        dof_max(v1) = max ( dof_max(v1), v2 )

        if ( 1 <= p2 ) then
          dof_min(v1) = min ( dof_min(v1), p2 )
          dof_max(v1) = max ( dof_max(v1), p2 )
        end if

        if ( 1 <= p1 ) then
          dof_min(p1) = min ( dof_min(p1), u2 )
          dof_max(p1) = max ( dof_max(p1), u2 )
          dof_min(p1) = min ( dof_min(p1), v2 )
          dof_max(p1) = max ( dof_max(p1), v2 )
        end if

      end do
    end do
  end do

  ml = 0
  mu = 0
  do dof = 1, dof_num
    ml = max ( ml, dof_max(dof) - dof )
    mu = max ( mu, dof - dof_min(dof) )
  end do

  bandwidth = ml + mu + 1

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ELEMENT_NODE_BANDWIDTH:'
  write ( *, '(a,i6)' ) '  Lower half bandwidth = ', ml
  write ( *, '(a,i6)' ) '  Upper half bandwidth = ', mu
  write ( *, '(a,i6)' ) '  Total bandwidth =      ', bandwidth

  return
end
subroutine element_node_print ( n, element_num, element_mask, element_node, &
  title )

!*****************************************************************************80
!
!! ELEMENT_NODE_PRINT prints the elements.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    01 April 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the elements.
!
!    Input, integer ( kind = 4 ) ELEMENT_NUM, the number of elements.
!
!    Input, logical ELEMENT_MASK(ELEMENT_NUM), is true for those
!    elements to be printed.
!
!    Input, integer ( kind = 4 ) ELEMENT_NODE(N,ELEMENT_NUM), the nodes in each element.
!
!    Input, character ( len = * ) TITLE, a title for the printout.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) element_num

  integer ( kind = 4 ) element
  logical element_mask(element_num)
  integer ( kind = 4 ) element_node(n,element_num)
  integer ( kind = 4 ) i
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ELEMENT_NODE_PRINT:'
  write ( *, '(a)' ) '  Element -> Node table.'
  if ( 0 < len_trim ( title ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(2x,a)' ) trim ( title )
  end if
  write ( *, '(a)' ) ' '
  write ( *, '(a,i6)' ) '  Number of elements = ', element_num
  write ( *, '(a,i6)' ) '  Number of nodes per element = ', n
  write ( *, '(a)' ) ' '
  write ( *, '(4x,a,3x,20i4)' ) '#', ( i, i = 1, n )
  write ( *, '(a)' ) ' '

  do element = 1, element_num
    if ( element_mask(element) ) then
      write ( *, '(2x,i3,3x,20i4)' ) element, element_node(1:n,element)
    end if
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
subroutine node_dof_print ( node_num, node_mask, node_dof_index, &
  node_dof_type, title )

!*****************************************************************************80
!
!! NODE_DOF_INDEX_PRINT prints the nodal degrees of freedom.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 April 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NODE_NUM, the number of nodes.
!
!    Input, logical NODE_MASK(NODE_NUM), is TRUE for the nodes to be printed.
!
!    Input, integer ( kind = 4 ) NODE_DOF_INDEX(3,NODE_NUM), the nodal degree of
!    freedom array.
!
!    Input, character ( len = 3 ) NODE_DOF_TYPE(3,NODE_NUM), the type of
!    each nodal degree of freedom.
!
!    Input, character ( len = * ) TITLE, a title for the printout.
!
  implicit none

  integer ( kind = 4 ) node_num

  integer ( kind = 4 ) node
  integer ( kind = 4 ) node_dof_index(3,node_num)
  character ( len = 3 ) node_dof_type(3,node_num)
  logical node_mask(node_num)
  character ( len = * ) title

  if ( 0 < len_trim ( title ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(2x,a)' ) trim ( title )
  end if

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'NODE_DOF_PRINT:'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Nodes and associated degrees of freedom'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Table of types:'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    Horizontal velocity:'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    UM:  momentum equation;'
  write ( *, '(a)' ) '    UD0: zero value;'
  write ( *, '(a)' ) '    UDC: constant value;'
  write ( *, '(a)' ) '    UDT: value depends on time;'
  write ( *, '(a)' ) '    UN0: normal derivative is zero;'
  write ( *, '(a)' ) '    UNC: normal derivative constant;'
  write ( *, '(a)' ) '    UNT: normal derivative depends on time;'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    Vertical velocity:'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    VM:  momentum equation;'
  write ( *, '(a)' ) '    VD0: zero value;'
  write ( *, '(a)' ) '    VDC: constant value;'
  write ( *, '(a)' ) '    VDT: value depends on time;'
  write ( *, '(a)' ) '    VN0: normal derivative is zero;'
  write ( *, '(a)' ) '    VNC: normal derivative constant;'
  write ( *, '(a)' ) '    VNT: normal derivative depends on time;'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    Pressure:'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    PC:  continuity equation;'
  write ( *, '(a)' ) '    PD0: zero value;'
  write ( *, '(a)' ) '    PDC: constant value;'
  write ( *, '(a)' ) '    PDT: value depends on time;'
  write ( *, '(a)' ) '    PN0: normal derivative is zero;'
  write ( *, '(a)' ) '    PNC: normal derivative constant;'
  write ( *, '(a)' ) '    PNT: normal derivative depends on time;'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Table of nodes and associated degrees of freedom:'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  NODE   Horizontal   Vertical     Pressure'
  write ( *, '(a)' ) '         velocity     velocity             '
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Index  Type   Index  Type   Index  Type   Index'
  write ( *, '(a)' ) ' '

  do node = 1, node_num

    if ( node_mask(node) ) then

      if ( 0 < node_dof_index(3,node) ) then
        write ( *, '(3x,i4,3(3x,a3,2x,i6))' ) node,         &
          node_dof_type(1,node), node_dof_index(1,node), &
          node_dof_type(2,node), node_dof_index(2,node), &
          node_dof_type(3,node), node_dof_index(3,node)
      else
        write ( *, '(3x,i4,3(3x,a3,2x,i6))' ) node,         &
          node_dof_type(1,node), node_dof_index(1,node), &
          node_dof_type(2,node), node_dof_index(2,node)
      end if

    end if

  end do

  return
end
subroutine node_eps ( file_name, node_num, node_mask, node_x, node_y, title )

!*****************************************************************************80
!
!! NODE_EPS creates an EPS file containing an image of the nodes.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    05 April 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) FILE_NAME, the name of the file to create.
!
!    Input, integer ( kind = 4 ) NODE_NUM, the number of nodes.
!
!    Input, logical NODE_MASK(NODE_NUM), is TRUE for those nodes to be plotted.
!
!    Input, real ( kind = 8 ) NODE_X(NODE_NUM), NODE_Y(NODE_NUM), the 
!    coordinates of the nodes.
!
!    Input, character ( len = * ) TITLE, a title for the plot.
!
  implicit none

  integer ( kind = 4 ) node_num

  integer ( kind = 4 ), parameter :: circle_size = 3
  real ( kind = 8 ) dif
  integer ( kind = 4 ) eps_unit
  integer ( kind = 4 ) eps_x
  integer ( kind = 4 ) eps_y
  character ( len = * ) file_name
  integer ( kind = 4 ) ios
  integer ( kind = 4 ) node
  logical node_mask(node_num)
  real ( kind = 8 ) node_x(node_num)
  real ( kind = 8 ) node_x_max
  real ( kind = 8 ) node_x_min
  real ( kind = 8 ) node_y(node_num)
  real ( kind = 8 ) node_y_max
  real ( kind = 8 ) node_y_min
  real ( kind = 8 ) scale
  character ( len = 80 ) string
  character ( len = * ) title
!
!  Determine the range of the unmasked nodes.
!
  node_x_min =  huge ( node_x_min )
  node_x_max = -huge ( node_x_max )
  node_y_min =  huge ( node_y_min )
  node_y_max = -huge ( node_y_max )

  do node = 1, node_num
    if ( node_mask(node) ) then
      node_x_min = min ( node_x_min, node_x(node) )
      node_x_max = max ( node_x_max, node_x(node) )
      node_y_min = min ( node_y_min, node_y(node) )
      node_y_max = max ( node_y_max, node_y(node) )
    end if
  end do

  if ( node_y_max - node_y_min < node_x_max - node_x_min ) then
    scale = node_x_max - node_x_min
    dif = ( node_x_max - node_x_min ) - ( node_y_max - node_y_min )
    node_y_max = node_y_max + 0.5 * dif
    node_y_min = node_y_min - 0.5 * dif
  else
    scale = node_y_max - node_y_min
    dif = ( node_y_max - node_y_min ) - ( node_x_max - node_x_min )
    node_x_max = node_x_max + 0.5 * dif
    node_x_min = node_x_min - 0.5 * dif
  end if

  call get_unit ( eps_unit )

  open ( unit = eps_unit, file = file_name, status = 'replace', iostat = ios )

  if ( ios /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'NODE_EPS - Fatal error!'
    write ( *, '(a)' ) '  Could not open the output EPS file.'
    stop
  end if

  write ( eps_unit, '(a)' ) '%!PS-Adobe-3.0 EPSF-3.0'
  write ( eps_unit, '(a)' ) '%%Creator: node_eps(aitch.f90)'
  write ( eps_unit, '(a)' ) '%%Title: ' // trim ( file_name )
  write ( eps_unit, '(a)' ) '%%Pages: 1'
  write ( eps_unit, '(a)' ) '%%BoundingBox:    36    36   576   756'
  write ( eps_unit, '(a)' ) '%%Document-Fonts: Times-Roman'
  write ( eps_unit, '(a)' ) '%%LanguageLevel: 1'
  write ( eps_unit, '(a)' ) '%%EndComments'
  write ( eps_unit, '(a)' ) '%%BeginProlog'
  write ( eps_unit, '(a)' ) '/inch {72 mul} def'
  write ( eps_unit, '(a)' ) '%%EndProlog'
  write ( eps_unit, '(a)' ) '%%Page:      1     1'
  write ( eps_unit, '(a)' ) 'save'
  write ( eps_unit, '(a)' ) '%'
  write ( eps_unit, '(a)' ) '% Set RGB line color.'
  write ( eps_unit, '(a)' ) '%'
  write ( eps_unit, '(a)' ) ' 0.9000 0.9000 0.9000 setrgbcolor'
  write ( eps_unit, '(a)' ) '%'
  write ( eps_unit, '(a)' ) '% Draw a gray border around the page.'
  write ( eps_unit, '(a)' ) '%'
  write ( eps_unit, '(a)' ) 'newpath'
  write ( eps_unit, '(a)' ) '    36   126 moveto'
  write ( eps_unit, '(a)' ) '   576   126 lineto'
  write ( eps_unit, '(a)' ) '   576   666 lineto'
  write ( eps_unit, '(a)' ) '    36   666 lineto'
  write ( eps_unit, '(a)' ) '    36   126 lineto'
  write ( eps_unit, '(a)' ) 'stroke'
  write ( eps_unit, '(a)' ) '%'
  write ( eps_unit, '(a)' ) '% Set RGB line color.'
  write ( eps_unit, '(a)' ) '%'
  write ( eps_unit, '(a)' ) ' 0.0000 0.0000 0.0000 setrgbcolor'

  write ( eps_unit, '(a)' ) '%'
  write ( eps_unit, '(a)' ) '%  Label the plot:'
  write ( eps_unit, '(a)' ) '%'
  write ( eps_unit, '(a)' ) ' 0.0000 0.0000 0.0000 setrgbcolor'
  write ( eps_unit, '(a)' ) '/Times-Roman findfont 0.50 inch scalefont setfont'
  write ( eps_unit, '(a)' ) '    36   666 moveto'
  write ( eps_unit, '(a)' ) '(' // trim ( title ) // ') show'

  write ( eps_unit, '(a)' ) '%'
  write ( eps_unit, '(a)' ) '% Define a clipping polygon'
  write ( eps_unit, '(a)' ) '%'
  write ( eps_unit, '(a)' ) '    36   126 moveto'
  write ( eps_unit, '(a)' ) '   576   126 lineto'
  write ( eps_unit, '(a)' ) '   576   666 lineto'
  write ( eps_unit, '(a)' ) '    36   666 lineto'
  write ( eps_unit, '(a)' ) '    36   126 lineto'
  write ( eps_unit, '(a)' ) 'clip newpath'

  write ( eps_unit, '(a)' ) '%'
  write ( eps_unit, '(a)' ) '%  Draw filled dots at each node:'
  write ( eps_unit, '(a)' ) '%'
  write ( eps_unit, '(a)' ) ' 0.0000 0.0000 1.0000 setrgbcolor'

  do node = 1, node_num

    if ( node_mask(node) ) then

      eps_x = int ( &
        ( ( node_x_max - node_x(node)              ) *  61.0E+00   &
        + (            + node_x(node) - node_x_min ) * 551.0E+00 ) &
        / scale )

      eps_y = int ( &
        ( ( node_y_max - node_y(node)              ) * 151.0E+00   &
        + (              node_y(node) - node_y_min ) * 641.0E+00 ) &
        / scale )

      write ( eps_unit, '(a,i4,2x,i4,2x,i4,a)' ) &
        'newpath  ', eps_x, eps_y, circle_size, ' 0 360 arc closepath fill'

    end if

  end do

  write ( eps_unit, '(a)' ) '%'
  write ( eps_unit, '(a)' ) '%  Label the nodes:'
  write ( eps_unit, '(a)' ) '%'
  write ( eps_unit, '(a)' ) ' 0.0000 0.0000 0.0000 setrgbcolor'
  write ( eps_unit, '(a)' ) '/Times-Roman findfont 0.20 inch scalefont setfont'

  do node = 1, node_num

    if ( node_mask(node) ) then

      eps_x = int ( &
        ( ( node_x_max - node_x(node)              ) *  61.0E+00   &
        + (            + node_x(node) - node_x_min ) * 551.0E+00 ) &
        / scale )

      eps_y = int ( &
        ( ( node_y_max - node_y(node)              ) * 151.0E+00   &
        + (              node_y(node) - node_y_min ) * 641.0E+00 ) &
        / scale )

      write ( string, '(i4)' ) node
      string = adjustl ( string )

      write ( eps_unit, '(i4,2x,i4,a)' ) eps_x, eps_y+5, &
        ' moveto (' // trim ( string ) // ') show'

    end if

  end do

  write ( eps_unit, '(a)' ) '%'
  write ( eps_unit, '(a)' ) 'restore showpage'
  write ( eps_unit, '(a)' ) '%'
  write ( eps_unit, '(a)' ) '% End of page'
  write ( eps_unit, '(a)' ) '%'
  write ( eps_unit, '(a)' ) '%%Trailer'
  write ( eps_unit, '(a)' ) '%%EOF'

  close ( unit = eps_unit )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'NODE_EPS:'
  write ( *, '(a)' ) '  An encapsulated PostScript file was created'
  write ( *, '(a)' ) '  containing an image of the nodes.'
  write ( *, '(a)' ) '  The file is named "' // trim ( file_name ) // '".'

  return
end
subroutine node_xy_print ( node_num, node_mask, node_x, node_y, title )

!*****************************************************************************80
!
!! NODE_XY_PRINT prints the node coordinates.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    01 April 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NODE_NUM, the number of nodes.
!
!    Input, logical NODE_MASK(NODE_NUM), is TRUE for the nodes to be printed.
!
!    Input, real ( kind = 8 ) NODE_X(NODE_NUM), NODE_Y(NODE_NUM), the nodal coordinates.
!
!    Input, character ( len = * ) TITLE, a title for the printout.
!
  implicit none

  integer ( kind = 4 ) node_num

  integer ( kind = 4 ) node
  logical node_mask(node_num)
  real ( kind = 8 ) node_x(node_num)
  real ( kind = 8 ) node_y(node_num)
  character ( len = * ) title

  if ( 0 < len_trim ( title ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(2x,a)' ) trim ( title )
  end if

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  NODE         X              Y'
  write ( *, '(a)' ) ' '

  do node = 1, node_num
    if ( node_mask(node) ) then
      write ( *, '(2x,i4,2x,2g14.6)' ) node, node_x(node), node_y(node)
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

  character ( len = 8 ) ampm
  integer ( kind = 4 ) d
  integer ( kind = 4 ) h
  integer ( kind = 4 ) m
  integer ( kind = 4 ) mm
  character ( len = 9  ), parameter, dimension(12) :: month = (/ &
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

  write ( *, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end
