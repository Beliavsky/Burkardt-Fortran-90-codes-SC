program main

!*****************************************************************************80
!
!! MAIN is the main program for SPARSE_GRID_CLOSED_TEST.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 December 2009
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) dim_max
  integer ( kind = 4 ) dim_min
  integer ( kind = 4 ) dim_num
  integer ( kind = 4 ) level_max
  integer ( kind = 4 ) level_max_max
  integer ( kind = 4 ) level_max_min

  call timestamp ( )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SPARSE_GRID_CLOSED_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the SPARSE_GRID_CLOSED library.'

  dim_min = 1
  dim_max = 5
  level_max_min = 0
  level_max_max = 10

  call test01 ( dim_min, dim_max, level_max_min, level_max_max )

  dim_min = 6
  dim_max = 10
  level_max_min = 0
  level_max_max = 10

  call test01 ( dim_min, dim_max, level_max_min, level_max_max )

  dim_num = 2
  level_max = 3
  call test02 ( 2, 3 )

  dim_num = 2
  level_max = 4
  call test02 ( 2, 4 )

  dim_num = 3
  level_max = 2
  call test02 ( 3, 2 )

  dim_num = 6
  level_max = 2
  call test02 ( 6, 2 )

  dim_num = 2
  level_max = 3
  call test03 ( 2, 3 )

  dim_num = 2
  level_max = 4
  call test04 ( 2, 4 )

  dim_num = 2
  do level_max = 2, 4
    call test05 ( dim_num, level_max )
  end do
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SPARSE_GRID_CLOSED_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( dim_min, dim_max, level_max_min, level_max_max )

!*****************************************************************************80
!
!! TEST01 tests SPARSE_GRID_CFN_SIZE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 December 2009
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) dim_max
  integer ( kind = 4 ) dim_min
  integer ( kind = 4 ) dim_num
  integer ( kind = 4 ) level_max
  integer ( kind = 4 ) level_max_max
  integer ( kind = 4 ) level_max_min
  integer ( kind = 4 ) point_num(dim_min:dim_max)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  SPARSE_GRID_CFN_SIZE returns the number of distinct'
  write ( *, '(a)' ) '  points in a sparse grid, made up of product grids'
  write ( *, '(a)' ) '  formed from closed fully nested quadrature rules.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  The sparse grid is the sum of all product grids'
  write ( *, '(a)' ) '  of order LEVEL, with'
  write ( *, '(a)' ) '    0 <= LEVEL <= LEVEL_MAX.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  LEVEL is the sum of the levels of the 1D rules,'
  write ( *, '(a)' ) '  the order of the 1D rule is 2^LEVEL + 1,'
  write ( *, '(a)' ) '  the region is [-1,1]^DIM_NUM.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  For this kind of rule, there is complete nesting,'
  write ( *, '(a)' ) '  that is, a sparse grid of a given level includes'
  write ( *, '(a)' ) '  ALL the points on grids of lower levels.'
  write ( *, '(a)' ) ' '

  do dim_num = dim_min, dim_max
    point_num(dim_num) = dim_num
  end do

  write ( *, '(a8,6(2x,i8))' ) '   DIM: ', point_num(dim_min:dim_max)
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   LEVEL_MAX'
  write ( *, '(a)' ) ' '

  do level_max = level_max_min, level_max_max

    do dim_num = dim_min, dim_max
      call sparse_grid_cfn_size ( dim_num, level_max, point_num(dim_num) )
    end do

    write ( *, '(a4,i4,6(2x,i8))' ) '    ', level_max, point_num(dim_min:dim_max)

  end do

  return
end
subroutine test02 ( dim_num, level_max )

!*****************************************************************************80
!
!! TEST02 tests LEVELS_CLOSED_INDEX.
!
!  Discussion:
!
!    The routine under study computes the indices of the unique points
!    used in a sparse multidimensional grid whose size is controlled
!    by a parameter LEVEL.
!
!    Once these indices are returned, they can be converted into the
!    abscissas of a particular closed nested rule, such as Clenshaw Curtis
!    or Newton Cotes Closed.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) dim_num
  integer ( kind = 4 ), allocatable, dimension ( :, : ) :: grid_index
  integer ( kind = 4 ) grid_num
  integer ( kind = 4 ) j
  integer ( kind = 4 ) level
  integer ( kind = 4 ) level_max
  integer ( kind = 4 ) point_num

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST02:'
  write ( *, '(a)' ) '  LEVELS_CLOSED_INDEX returns all grid indexes'
  write ( *, '(a)' ) '  whose level value satisfies'
  write ( *, '(a)' ) '    0 <= LEVEL <= LEVEL_MAX.'
  write ( *, '(a)' ) '  Here, LEVEL is the sum of the levels of the 1D rules,'
  write ( *, '(a)' ) '  and the order of the rule is 2^LEVEL + 1.'

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  LEVEL_MAX = ', level_max
  write ( *, '(a,i8)' ) '  Spatial dimension DIM_NUM = ', dim_num

  call sparse_grid_cfn_size ( dim_num, level_max, point_num )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of unique points in the grid = ', point_num
!
!  Allocate the space.
!
  allocate ( grid_index(1:dim_num,1:point_num) )
!
!  Compute the grid index values.
!
  call levels_closed_index ( dim_num, level_max, point_num, grid_index )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Grid index:'
  write ( *, '(a)' ) ' '
  do j = 1, point_num
    write ( *, '(2x,i4,2x,6i6)' ) j, grid_index(1:dim_num,j)
  end do

  deallocate ( grid_index)

  return
end
subroutine test03 ( dim_num, level_max )

!*****************************************************************************80
!
!! TEST03 tests LEVELS_CLOSED_INDEX to create a Clenshaw Curtis grid.
!
!  Discussion:
!
!    This routine gets the sparse grid indices and determines the
!    corresponding sparse grid abscissas for a Clenshaw Curtis scheme.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real    ( kind = 8 ) cc_abscissa
  integer ( kind = 4 ) dim
  integer ( kind = 4 ) dim_num
  integer ( kind = 4 ), allocatable, dimension ( :, : ) :: grid_index
  integer ( kind = 4 ) grid_num
  real    ( kind = 8 ), allocatable, dimension ( :, : ) :: grid_point
  integer ( kind = 4 ) j
  integer ( kind = 4 ) level
  integer ( kind = 4 ) level_max
  integer ( kind = 4 ) order_max
  integer ( kind = 4 ) point_num

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST03:'
  write ( *, '(a)' ) '  Make a Clenshaw-Curtis sparse grid.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  LEVELS_CLOSED_INDEX returns all grid indexes'
  write ( *, '(a)' ) '  whose level value satisfies'
  write ( *, '(a)' ) '    0 <= LEVEL <= LEVEL_MAX.'
  write ( *, '(a)' ) '  Here, LEVEL is the sum of the levels of the 1D rules,'
  write ( *, '(a)' ) '  and the order of the rule is 2^LEVEL + 1.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Now we demonstrate how to convert grid indices'
  write ( *, '(a)' ) '  into physical grid points.  In this case, we'
  write ( *, '(a)' ) '  want points on [-1,+1]^DIM_NUM.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  LEVEL_MAX = ', level_max
  write ( *, '(a,i8)' ) '  Spatial dimension DIM_NUM = ', dim_num

  call sparse_grid_cfn_size ( dim_num, level_max, point_num )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of unique points in the grid = ', point_num
!
!  Allocate the space.
!
  allocate ( grid_index(1:dim_num,1:point_num) )
!
!  Compute the grid index values.
!
  call levels_closed_index ( dim_num, level_max, point_num, grid_index )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Grid index:'
  write ( *, '(a)' ) ' '
  do j = 1, point_num
    write ( *, '(2x,i4,2x,6i6)' ) &
      j, grid_index(1:dim_num,j)
  end do
!
!  Convert index information to physical information.
!
  if ( 0 == level_max ) then
    order_max = 1
  else
    order_max = 2**level_max + 1
  end if

  allocate ( grid_point(1:dim_num,1:point_num) )

  do j = 1, point_num
    do dim = 1, dim_num
      grid_point(dim,j) = cc_abscissa ( order_max, grid_index(dim,j) + 1 )
    end do
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Grid points:'
  write ( *, '(a)' ) ' '
  do j = 1, point_num
    write ( *, '(2x,i8,2x,6f10.6)' ) j, grid_point(1:dim_num,j)
  end do

  deallocate ( grid_index )
  deallocate ( grid_point )

  return
end
subroutine test04 ( dim_num, level_max )

!*****************************************************************************80
!
!! TEST04 tests LEVELS_CLOSED_INDEX to make a Newton Cotes Closed grid.
!
!  Discussion:
!
!    This routine gets the sparse grid indices and determines the
!    corresponding sparse grid abscissas for a Newton Cotes closed scheme.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) dim
  integer ( kind = 4 ) dim_num
  integer ( kind = 4 ), allocatable, dimension ( :, : ) :: grid_index
  integer ( kind = 4 ) grid_num
  real    ( kind = 8 ), allocatable, dimension ( :, : ) :: grid_point
  integer ( kind = 4 ) j
  integer ( kind = 4 ) level
  integer ( kind = 4 ) level_max
  real    ( kind = 8 ) ncc_abscissa
  integer ( kind = 4 ) order_max
  integer ( kind = 4 ) point_num

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST04:'
  write ( *, '(a)' ) '  Make a Newton Cotes Closed sparse grid.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  LEVELS_CLOSED_INDEX returns all grid indexes'
  write ( *, '(a)' ) '  whose level value satisfies'
  write ( *, '(a)' ) '    0 <= LEVEL <= LEVEL_MAX.'
  write ( *, '(a)' ) '  Here, LEVEL is the sum of the levels of the 1D rules,'
  write ( *, '(a)' ) '  and the order of the rule is 2^LEVEL + 1.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Now we demonstrate how to convert grid indices'
  write ( *, '(a)' ) '  into physical grid points.  In this case, we'
  write ( *, '(a)' ) '  want points on [0,+1]^DIM_NUM.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  LEVEL_MAX = ', level_max
  write ( *, '(a,i8)' ) '  Spatial dimension DIM_NUM = ', dim_num

  call sparse_grid_cfn_size ( dim_num, level_max, point_num )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of unique points in the grid = ', point_num
!
!  Allocate the space.
!
  allocate ( grid_index(1:dim_num,1:point_num) )
!
!  Compute the grid index values.
!
  call levels_closed_index ( dim_num, level_max, point_num, grid_index )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Grid index:'
  write ( *, '(a)' ) ' '
  do j = 1, point_num
    write ( *, '(2x,i4,2x,6i6)' ) j, grid_index(1:dim_num,j)
  end do
!
!  Convert index information to physical information.
!
  if ( 0 == level_max ) then
    order_max = 1
  else
    order_max = 2**level_max + 1
  end if

  allocate ( grid_point(1:dim_num,1:point_num) )

  do j = 1, point_num
    do dim = 1, dim_num
      grid_point(dim,j) = ncc_abscissa ( order_max, grid_index(dim,j) + 1 )
    end do
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Grid points:'
  write ( *, '(a)' ) ' '
  do j = 1, point_num
    write ( *, '(2x,i8,2x,6f10.6)' ) j, grid_point(1:dim_num,j)
  end do

  deallocate ( grid_index )
  deallocate ( grid_point )

  return
end
subroutine test05 ( dim_num, level_max )

!*****************************************************************************80
!
!! TEST05 creates and writes sparse grid files.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real    ( kind = 8 ) cc_abscissa
  integer ( kind = 4 ) dim
  integer ( kind = 4 ) dim_num
  real    ( kind = 8 ) f1_abscissa
  real    ( kind = 8 ) f2_abscissa
  character ( len = 80 ) file_name
  integer ( kind = 4 ), allocatable, dimension ( :, : ) :: grid_index
  integer ( kind = 4 ) grid_num
  real    ( kind = 8 ), allocatable, dimension ( :, : ) :: grid_point
  integer ( kind = 4 ) j
  integer ( kind = 4 ) level
  integer ( kind = 4 ) level_max
  real    ( kind = 8 ) ncc_abscissa
  real    ( kind = 8 ) nco_abscissa
  real    ( kind = 8 ) ncoh_abscissa
  integer ( kind = 4 ) order_max
  integer ( kind = 4 ) point_num

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST05:'
  write ( *, '(a)' ) '  Make sparse grids and write to files.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  LEVEL_MAX = ', level_max
  write ( *, '(a,i8)' ) '  Spatial dimension DIM_NUM = ', dim_num

  call sparse_grid_cfn_size ( dim_num, level_max, point_num )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of unique points in the grid = ', point_num
!
!  Allocate the space.
!
  allocate ( grid_index(1:dim_num,1:point_num) )
!
!  Compute the orders and points.
!
  call levels_closed_index ( dim_num, level_max, point_num, grid_index )
!
!  Now we're done.  Print the merged grid data.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Grid index:'
  write ( *, '(a)' ) ' '
  do j = 1, point_num
    write ( *, '(2x,i4,2x,6i6)' ) j, grid_index(1:dim_num,j)
  end do
!
!  Convert index information to physical information.
!
  if ( 0 == level_max ) then
    order_max = 1
  else
    order_max = 2**level_max + 1
  end if

  allocate ( grid_point(1:dim_num,1:point_num) )
!
!  Create CC data and write to file.
!
  do j = 1, point_num
    do dim = 1, dim_num
      grid_point(dim,j) = cc_abscissa ( order_max, grid_index(dim,j) + 1 )
    end do
  end do

  write ( file_name, '(a,i2,a,i2,a)' ) &
    'cc_d', dim_num, '_level', level_max, '.txt'

  call s_blank_delete ( file_name )

  call r8mat_write ( file_name, dim_num, point_num, grid_point )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Wrote file "' // trim ( file_name ) // '".'
!
!  Create NCC data and write to file.
!
  do j = 1, point_num
    do dim = 1, dim_num
      grid_point(dim,j) = ncc_abscissa ( order_max, grid_index(dim,j) + 1 )
    end do
  end do

  write ( file_name, '(a,i2,a,i2,a)' ) &
    'ncc_d', dim_num, '_level', level_max, '.txt'

  call s_blank_delete ( file_name )

  call r8mat_write ( file_name, dim_num, point_num, grid_point )

  write ( *, '(a)' ) '  Wrote file "' // trim ( file_name ) // '".'

  deallocate ( grid_index )
  deallocate ( grid_point )

  return
end
