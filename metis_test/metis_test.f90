program main

!*****************************************************************************80
!
!  Purpose:
!
!    METIS_TEST tests the METIS library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 February 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'METIS_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the METIS library for graph partitioning.'

  call partgraphkway_test ( )
  call partgraphrecursive_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'METIS_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop
end
subroutine partgraphkway_test ( )

!*****************************************************************************80
!
!  Purpose:
!
!    PARTGRAPHKWAY_TEST tests PARTGRAPHKWAY.
!
!  Discussion:
!
!    The graph has the following form:
!
!      0 --- 1 --- 2
!      |     |     |
!      3 --- 4 --- 5
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 February 2017
!
!  Author:
!
!    John Burkardt
!
  use iso_c_binding

  implicit none

  integer ( kind = 4 ) i
!
!  The number of vertices.
!
  integer ( kind = 4 ), parameter :: nvtxs = 6
!
! Number of balancing constraints, which must be at least 1.
!
  integer ( kind = 4 ), parameter :: ncon = 1
!
!  Pointers to initial entries in adjacency array.
!
  integer ( kind = 4 ) :: xadj(nvtxs+1) = (/ 0, 2, 5, 7, 9, 12, 14 /)
!
! Adjacent vertices in consecutive index order.
!
  integer ( kind = 4 ), parameter :: nEdges = 7
  integer ( kind = 4 ) :: adjncy(2 * nEdges) = (/1,3,0,4,2,1,5,0,4,3,1,5,4,2/)

  integer ( kind = 4 ) :: vwgt(nvtxs) = (/ 1, 1, 1, 1, 1, 1 /)
  integer ( kind = 4 ) :: vsize(nvtxs) = (/ 1, 1, 1, 1, 1, 1 /)
  integer ( kind = 4 ) :: adjwgt(2*nEdges) = (/1,1,1,1,1,1,1,1,1,1,1,1,1,1/)
!
!  The number of parts requested for the partition.
!
  integer ( kind = 4 ), parameter :: nparts = 2

  real ( kind = 4 ) :: tpwgts ( nparts * ncon ) = (/ 0.5, 0.5 /)
  real ( kind = 4 ) :: ubvec(ncon) = (/ 1.001 /)
  integer ( kind = 4 ) :: options(40) = (/ &
    0,0,0,0,0,0,10,1,0,0, &
    0,0,0,0,0,1,30,0,0,0, &
    0,0,0,0,0,0,0,0,0,0, &
    0,0,0,0,0,0,0,0,0,0 /)
!
!  On return, the edge cut volume of the partitioning solution.
!
  integer ( kind = 4 ) objval
!
!  On return, the partition vector for the graph.
!
  integer ( kind = 4 ) part(nvtxs)
!
!  Return code.
!
  integer ( kind = 4 ) ret

  integer ( kind = 4 ) METIS_PartGraphKway

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PARTGRAPHKWAY_TEST:'
  write ( *, '(a)' ) '  METIS_PartGraphKway partitions a graph into K parts'
  write ( *, '(a)' ) '  using multilevel K-way partition.'

  ret = METIS_PartGraphKway ( nvtxs, ncon, xadj, adjncy, vwgt, vsize, &
    adjwgt, nParts, tpwgts, ubvec, options, objval, part)

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Return code = ', ret
  write ( *, '(a,i4)' ) '  Edge cuts for partition = ', objval
    
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Partition vector:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Node  Part'
  write ( *, '(a)' ) ''
  do i = 1, nvtxs
    write ( *, '(2x,i4,2x,i4)' ) i, part(i)
  end do
  
  return
end
subroutine partgraphrecursive_test ( )

!*****************************************************************************80
!
!  Purpose:
!
!    PARTGRAPHRECURSIVE_TEST tests PARTGRAPHRECURSIVE.
!
!  Discussion:
!
!    The graph has the following form:
!
!      0 --- 1 --- 2
!      |     |     |
!      3 --- 4 --- 5
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 February 2017
!
!  Author:
!
!    John Burkardt
!
  use iso_c_binding

  implicit none

  integer ( kind = 4 ) i
!
!  The number of vertices.
!
  integer ( kind = 4 ), parameter :: nvtxs = 6
!
! Number of balancing constraints, which must be at least 1.
!
  integer ( kind = 4 ), parameter :: ncon = 1
!
!  Pointers to initial entries in adjacency array.
!
  integer ( kind = 4 ) :: xadj(nvtxs+1) = (/ 0, 2, 5, 7, 9, 12, 14 /)
!
! Adjacent vertices in consecutive index order.
!
  integer ( kind = 4 ), parameter :: nEdges = 7
  integer ( kind = 4 ) :: adjncy(2 * nEdges) = (/1,3,0,4,2,1,5,0,4,3,1,5,4,2/)

  integer ( kind = 4 ) :: vwgt(nvtxs) = (/ 1, 1, 1, 1, 1, 1 /)
  integer ( kind = 4 ) :: vsize(nvtxs) = (/ 1, 1, 1, 1, 1, 1 /)
  integer ( kind = 4 ) :: adjwgt(2*nEdges) = (/1,1,1,1,1,1,1,1,1,1,1,1,1,1/)
!
!  The number of parts requested for the partition.
!
  integer ( kind = 4 ), parameter :: nparts = 2

  real ( kind = 4 ) :: tpwgts ( nparts * ncon ) = (/ 0.5, 0.5 /)
  real ( kind = 4 ) :: ubvec(ncon) = (/ 1.001 /)
  integer ( kind = 4 ) :: options(40) = (/ &
    0,0,0,0,0,0,10,1,0,0, &
    0,0,0,0,0,1,1,0,0,0, &
    0,0,0,0,0,0,0,0,0,0, &
    0,0,0,0,0,0,0,0,0,0 /)
!
!  On return, the edge cut volume of the partitioning solution.
!
  integer ( kind = 4 ) objval
!
!  On return, the partition vector for the graph.
!
  integer ( kind = 4 ) part(nvtxs)
!
!  Return code.
!
  integer ( kind = 4 ) ret

  integer ( kind = 4 ) METIS_PartGraphRecursive

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PARTGRAPHRECURSIVE_TEST:'
  write ( *, '(a)' ) '  METIS_PartGraphRecursive partitions a graph into K parts'
  write ( *, '(a)' ) '  using multilevel recursive bisection.'

  ret = METIS_PartGraphRecursive ( nvtxs, ncon, xadj, adjncy, vwgt, vsize, &
    adjwgt, nParts, tpwgts, ubvec, options, objval, part)

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Return code = ', ret
  write ( *, '(a,i4)' ) '  Edge cuts for partition = ', objval
    
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Partition vector:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Node  Part'
  write ( *, '(a)' ) ''
  do i = 1, nvtxs
    write ( *, '(2x,i4,2x,i4)' ) i, part(i)
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
!    18 May 2013
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
  character ( len = 9 ), parameter, dimension(12) :: month = (/ &
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

  write ( *, '(i2.2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end
