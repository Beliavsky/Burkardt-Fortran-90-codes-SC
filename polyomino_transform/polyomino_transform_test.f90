program main

!******************************************************************************/
!
!! MAIN is the main program for POLYOMINO_TRANSFORM_TEST.
!
!  Discussion:
!
!    POLYOMINO_TRANSFORM_TEST tests POLYOMINO_TRANSFORM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 April 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Local, integer ( kind = 4 ) M, N, the number of rows and columns in the 
!    representation of the polyomino P.
!
!    Local, integer ( kind = 4 ) P(M*N), a matrix of 0's and 1's representing 
!    the polyomino.  The matrix should be 'tight', that is, there should be a
!    1 in row 1, and in column 1, and in row M, and in column N.
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n = 3

  character ( len = 80 ) label
  integer ( kind = 4 ) mq
  integer ( kind = 4 ) nq
!
!  P is given by columns, not rows.
!
  integer ( kind = 4 ) :: p(3,3) = reshape ( (/ & 
    0, 1, 0, &
    1, 1, 1, &
    1, 0, 0 /), (/ 3, 3 /) )
  integer q(m*n)
  integer ( kind = 4 ) reflect
  integer ( kind = 4 ) rotate

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYOMINO_TRANSFORM_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  POLYOMINO_TRANSFORM can transform a polyomino.'
  write ( *, '(a)' ) '  Generate all 8 combinations of rotation and reflection'
  write ( *, '(a)' ) '  applied to a polyomino represented by a binary matrix.'

  call polyomino_print ( m, n, p, '  The given polyomino P:' )

  do reflect = 0, 1
    do rotate = 0, 3

      call polyomino_transform ( m, n, p, rotate, reflect, mq, nq, q )
      write ( label, '(a,i1,a,i1,a)' ) '  P after ', reflect, ' reflections and ', &
        rotate, ' rotations:'

      call polyomino_print ( mq, nq, q, label )

    end do
  end do
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYOMINO_TRANSFORM_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
