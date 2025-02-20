program main

!*****************************************************************************80
!
!! MAIN is the main program for POLYOMINO_LP_WRITE_TEST.
!
!  Discussion:
!
!    POLYOMINO_LP_WRITE_TEST tests POLYOMINO_LP_WRITE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 May 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), allocatable :: a(:,:)
  integer ( kind = 4 ), allocatable :: b(:)
  character ( len = 80 ) filename
  character ( len = 80 ) label
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  call timestamp ( );
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYOMINO_LP_WRITE_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  POLYOMINO_LP_WRITE writes an LP file associated'
  write ( *, '(a)' ) '  with a binary programming problem for tiling a region'
  write ( *, '(a)' ) '  with copies of a single polyomino.'
!
!  Get the coefficients and right hand side for the Reid system.
!
  call polyomino_monohedral_example_reid_size ( m, n )

  allocate ( a(1:m,1:n) )
  allocate ( b(1:m) )

  call polyomino_monohedral_example_reid_system ( m, n, a, b )
!
!  Create the LP file.
!
  filename = 'reid.lp'
  label = '\ LP file for the Reid example.'

  call polyomino_lp_write ( filename, label, m, n, a, b );

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  POLYOMINO_LP_WRITE created the LP file "' // filename // '"'
!
!  Free memory.
!
  deallocate ( a )
  deallocate ( b )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYOMINO_LP_WRITE_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  return
end
subroutine polyomino_monohedral_example_reid_size ( m, n )

!*****************************************************************************80
!
!! POLYOMINO_MONOHEDRAL_EXAMPLE_REID_SIZE returns the size of the system.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 May 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) M, N, the number of rows and columns
!    in the system.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  m = 9
  n = 10

  return
end
subroutine polyomino_monohedral_example_reid_system ( m, n, a, b )

!*****************************************************************************80
!
!! POLYOMINO_MONOHEDRAL_EXAMPLE_REID_SYSTEM sets up the Reid linear system.
!
!  Discussion:
!
!    This function sets up the linear system A*x=b associated with
!    the Reid polyomino tiling problem.
!
!    While it is desirable to have a general procedure that can automatically
!    deduce the linear system from the problem specification, for simplicity
!    in this example, we simply provide the linear system directly.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 May 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns in the system.
!
!    Output, integer ( kind = 4 ) A(M,N), the system matrix.
!
!    Output, integer ( kind = 4 ) B(M), the right hand side.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) b(m)
!
!  Note that the matrix is given in column major order.
!
  a = reshape ( (/ &
    1,1,0,0,0,0,0,0,2, &
    0,0,1,1,0,0,0,0,2, &
    0,0,0,1,1,0,0,0,2, &
    0,0,0,0,0,1,1,0,2, &
    0,0,0,0,0,0,1,1,2, &
    1,0,1,0,0,0,0,0,2, &
    0,1,0,1,0,0,0,0,2, &
    0,0,1,0,0,1,0,0,2, &
    0,0,0,1,0,0,1,0,2, &
    0,0,0,0,1,0,0,1,2 /), (/ m, n /) )

  b = (/ 1, 1, 1, 1, 1, 1, 1, 1, 8 /)

  return
end

