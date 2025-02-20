program main

!*****************************************************************************80
!
!! MAIN is the main program for POLYOMINO_CONDENSE_TEST.
!
!  Discussion:
!
!    POLYOMINO_CONDENSE_TEST tests POLYOMINO_CONDENSE.
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
!    Local, integer ( kind = 4 ) MP, NP, the number of rows and columns in the 
!    representation of the polyomino P.
!
!    Local, integer ( kind = 4 ) P(MP*NP), a matrix representing the polyomino.  
!
  implicit none

  integer ( kind = 4 ) mp
  integer ( kind = 4 ) np
  integer ( kind = 4 ) :: p1(9) = (/ 0, 1, 1, 1, 1, 0, 0, 1, 0 /)
  integer ( kind = 4 ) :: p2(9) = (/ 0, 1, 2, 1, 3, 0, 0, -9, 0 /)
  integer ( kind = 4 ) :: p3(12) = (/ 0, 0, 0, 0, 1, 3, 0, 0, 0, 0, 0, 0 /)
  integer ( kind = 4 ) :: p4(8) = (/ 0, 0, 0, 0, 0, 0, 0, 0 /)

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYOMINO_CONDENSE_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  POLYOMINO_CONDENSE "cleans up" a matrix that is supposed'
  write ( *, '(a)' ) '  to represent a polyomino:'
  write ( *, '(a)' ) '  * nonzero entries are set to 1'
  write ( *, '(a)' ) '  * initial and final zero rows and columns are deleted.'
!
!  Nothing happens:
!
  mp = 3
  np = 3
  call condense_demo ( mp, np, p1 )
!
!  Nonzero, but non-one entries are set to 1.
!
  mp = 3
  np = 3
  call condense_demo ( mp, np, p2 )
!
!  Extraneous zero rows and columns are removed.
!
  mp = 3
  np = 4
  call condense_demo ( mp, np, p3 )
!
!  Null matrices are detected.
!
  mp = 2
  np = 4
  call condense_demo ( mp, np, p4 )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYOMINO_CONDENSE_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine condense_demo ( mp, np, p )

!*****************************************************************************80
!
!! CONDENSE_DEMO demonstrates the result of calling POLYOMINO_CONDENSE.
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
!    Input, integer ( kind = 4 ) MP, NP, the number of rows and columns in the representation
!    of the polyomino P.
!
!    Input, integer ( kind = 4 ) P(MP*NP), a matrix representing the polyomino.  
!
!    Local, integer ( kind = 4 ) MQ, NQ, the number of rows and columns in the representation
!    of the condensed polyomino Q.
!
!    Local, integer ( kind = 4 ) Q(MQ*NQ), a matrix representing the condensed polyomino.  
!
  implicit none

  character ( len = 80 ) label
  integer ( kind = 4 ) mp
  integer ( kind = 4 ) mq
  integer ( kind = 4 ) np
  integer ( kind = 4 ) nq
  integer ( kind = 4 ) p(mp*np)
  integer ( kind = 4 ) q(mp*np)

  write ( label, '(a,i2,a,i2,a)' ) '  The initial (', mp, ',', np, ') polynomino P:'
  call polyomino_print ( mp, np, p, label )

  call polyomino_condense ( mp, np, p, mq, nq, q )

  write ( label, '(a,i2,a,i2,a)' ) '  The condensed (', mq, ',', nq, ') polynomino Q:'
  call polyomino_print ( mq, nq, q, label )

  return
end
