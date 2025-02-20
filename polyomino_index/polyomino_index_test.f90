program main

!*****************************************************************************80
!
!! MAIN is the main program for POLYOMINO_INDEX_TEST
!
!  Discussion:
!
!    POLYOMINO_INDEX_TEST tests POLYOMINO_INDEX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 April 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) :: m = 3
  integer ( kind = 4 ) :: n = 4
!
!  P is listed in column-major order
!
  integer ( kind = 4 ) :: p(3*4) = (/ &
    1, 1, 0,&
    0, 1, 1,&
    1, 1, 1,&
    1, 0, 0 /)
  integer ( kind = 4 ) pin(3*4)

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYOMINO_INDEX_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  POLYOMINO_INDEX assigns an index to each nonzero entry'
  write ( *, '(a)' ) '  of a polyomino.'

  call polyomino_print ( m, n, p, '  The polyomino P:' )

  call polyomino_index ( m, n, p, pin )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  PIN: Index vector for P:'
  write ( *, '(a)' ) ''
  do i = 1, m
    do j = 1, n
      write ( *, '(2x,i2)', advance = 'no' ) pin(i+(j-1)*m)
    end do
    write ( *, '(a)' ) ''
  end do
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYOMINO_INDEX_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
