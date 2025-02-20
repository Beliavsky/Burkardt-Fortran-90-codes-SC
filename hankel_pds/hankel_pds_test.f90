program main

!*****************************************************************************80
!
!! MAIN is the main program for HANKEL_PDS_TEST.
!
!  Discussion:
!
!    HANKEL_PDS_TEST tests HANKEL_PDS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 January 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HANKEL_PDS_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the HANKEL_PDS library.'

  call hankel_pds_cholesky_lower_test01 ( )
  call hankel_pds_cholesky_lower_test02 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HANKEL_PDS_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine hankel_pds_cholesky_lower_test01 ( )

!*****************************************************************************80
!
!! HANKEL_PDS_CHOLESKY_LOWER_TEST01 tests HANKEL_PDS_CHOLESKY_LOWER.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 January 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) flag
  real ( kind = 8 ) h(n,n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) l(n,n)
  real ( kind = 8 ) lii(n)
  real ( kind = 8 ) liim1(n-1)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HANKEL_PDS_CHOLESKY_LOWER_TEST01'
  write ( *, '(a)' ) '  HANKEL_PDS_CHOLESKY_LOWER computes a lower Cholesky'
  write ( *, '(a)' ) '  matrix L such that the matrix H = L * L'' is a'
  write ( *, '(a)' ) '  positive definite (symmetric) Hankel matrix.'

  lii(1:n) = 1.0D+00
  liim1(1:n-1) = 1.0D+00

  call hankel_pds_cholesky_lower ( n, lii, liim1, l )

  call r8mat_print ( n, n, l, '  The Cholesky factor L:' )

  h = matmul ( l, transpose ( l ) )

  call r8mat_print ( n, n, h, '  The Hankel matrix H = L * L'':' )

  do i = 1, n
    lii(i) = real ( i, kind = 8 )
  end do
  do i = 1, n - 1
    liim1(i) = real ( n - i, kind = 8 )
  end do

  call hankel_pds_cholesky_lower ( n, lii, liim1, l )

  call r8mat_print ( n, n, l, '  The Cholesky factor L:' )

  h = matmul ( l, transpose ( l ) )

  call r8mat_print ( n, n, h, '  The Hankel matrix H = L * L'':' )

  seed = 123456789
  call r8vec_uniform_01 ( n, seed, lii )
  call r8vec_uniform_01 ( n - 1, seed, liim1 )

  call hankel_pds_cholesky_lower ( n, lii, liim1, l )

  call r8mat_print ( n, n, l, '  The Cholesky factor L:' )

  h = matmul ( l, transpose ( l ) )

  call r8mat_print ( n, n, h, '  The Hankel matrix H = L * L'':' )

  return
end
subroutine hankel_pds_cholesky_lower_test02 ( )

!*****************************************************************************80
!
!! HANKEL_PDS_CHOLESKY_LOWER_TEST02 tests HANKEL_PDS_CHOLESKY_LOWER.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 January 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) flag
  real ( kind = 8 ) h(n,n)
  real ( kind = 8 ) h2(n,n)
  real ( kind = 8 ) l(n,n)
  real ( kind = 8 ) l2(n,n)
  real ( kind = 8 ) lii(n)
  real ( kind = 8 ) liim1(n-1)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HANKEL_PDS_CHOLESKY_LOWER_TEST02'
  write ( *, '(a)' ) '  HANKEL_PDS_CHOLESKY_LOWER computes a lower Cholesky'
  write ( *, '(a)' ) '  matrix L such that the matrix H = L * L'' is a'
  write ( *, '(a)' ) '  positive definite (symmetric) Hankel matrix.'

  lii(1:n) = 1.0D+00
  liim1(1:n-1) = 1.0D+00

  call hankel_pds_cholesky_lower ( n, lii, liim1, l )

  call r8mat_print ( n, n, l, '  The Cholesky factor L:' )

  h = matmul ( l, transpose ( l ) )

  call r8mat_print ( n, n, h, '  The Hankel matrix H = L * L'':' )

  call r8mat_cholesky_factor ( n, h, l2, flag )

  call r8mat_print ( n, n, l2, '  The Cholesky factor L2 of H:' )

  h2 = matmul ( l2, transpose ( l2 ) )

  call r8mat_print ( n, n, h2, '  The Hankel matrix H2 = L2 * L2'':' )

  return
end
