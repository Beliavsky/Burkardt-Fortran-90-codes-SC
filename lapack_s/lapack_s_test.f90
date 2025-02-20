program main

!*****************************************************************************80
!
!! MAIN is the main program for LAPACK_S_TEST.
!
!  Discussion:
!
!    LAPACK_S_TEST tests the LAPACK_S library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 January 2011
!
!  Author:
!
!    John Burkardt
!
  implicit none

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LAPACK_S_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the LAPACK_S library.'

  call sgbtrf_test ( )
  call sgecon_test ( )
  call sgeqrf_test ( )
  call sgesvd_test ( )
  call sgetrf_test ( )
  call sgetri_test ( )
  call sgtsv_test ( )
  call sormgqr_test ( )
  call spbtrf_test ( )
  call spbtrs_test ( )
  call spotrf_test ( )
  call spotri_test ( )
  call ssbgvx_test ( )
  call ssyev_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LAPACK_S_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'

  stop 0
end
subroutine sgbtrf_test ( )

!*****************************************************************************80
!
!! SGBTRF_TEST tests SGBTRF.
!
!  Discussion:
!
!    The problem is just an enlarged version of the
!    problem for n = 5, which is:
!
!    Matrix A is ( 2 -1  0  0  0)    right hand side b is  (1)
!                (-1  2 -1  0  0)                          (0)
!                ( 0 -1  2 -1  0)                          (0)
!                ( 0  0 -1  2 -1)                          (0)
!                ( 0  0  0 -1  2)                          (1)
!
!
!    Solution is   (1)
!                  (1)
!                  (1)
!                  (1)
!                  (1)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 September 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 25
  integer ( kind = 4 ), parameter :: ml = 1
  integer ( kind = 4 ), parameter :: mu = 1

  integer ( kind = 4 ), parameter :: lda = 2 * ml + mu + 1

  real ( kind = 4 ) a(lda,n)
  real ( kind = 4 ) b(n)
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ipiv(n)
  integer ( kind = 4 ) m

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SGBTRF_TEST'
  write ( *, '(a)' ) '  SGBTRF factors a general band matrix.'
  write ( *, '(a)' ) '  SGBTRS solves a factored system.'
  write ( *, '(a)' ) '  For a double precision real matrix (D)'
  write ( *, '(a)' ) '  in general band storage mode (GB):'
!
!  Assign values to matrix A and right hand side b.
!
  b(1) = 1.0E+00
  b(2:n-1) = 0.0E+00
  b(n) = 1.0E+00
!
!  Zero out the matrix.
!
  a(1:lda,1:n) = 0.0E+00

  m = ml + mu + 1
!
!  Superdiagonal,
!  Diagonal,
!  Subdiagonal.
!
  a(m-1,2:n) = -1.0E+00
  a(m,1:n) = 2.0E+00
  a(m+1,1:n-1) = -1.0E+00
!
!  Factor the matrix.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Bandwidth is ', m
  write ( *, '(a)' ) ' '

  call sgbtrf ( n, n, ml, mu, a, lda, ipiv, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST01'
    write ( *, '(a,i8)' ) '  Factorization failed, INFO = ', info
    return
  end if
!
!  Solve the linear system.
!
  call sgbtrs ( 'n', n, ml, mu, 1, a, lda, ipiv, b, n, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST01'
    write ( *, '(a,i8)' ) '  Solution failed, INFO = ', info
    return
  end if

  call r4vec_print_some ( n, b, 1, 5, '  Partial solution (all should be 1)' )

  return
end
subroutine sgecon_test ( )

!*****************************************************************************80
!
!! SGECON_TEST tests SGECON and SGETRF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 October 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 3

  integer ( kind = 4 ), parameter :: lda = n
  integer ( kind = 4 ), parameter :: lwork = 4 * n

  real ( kind = 4 ) a(lda,n)
  real ( kind = 4 ) anorm
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ipiv(n)
  integer ( kind = 4 ) iwork(n)
  real ( kind = 4 ) rcond
  real ( kind = 4 ) r4mat_norm_li
  real ( kind = 4 ) work(lwork)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SGECON_TEST'
  write ( *, '(a)' ) '  SGECON computes the condition number '
  write ( *, '(a)' ) '  of a factored matrix'
  write ( *, '(a)' ) '  SGETRF computes the LU factorization;'
  write ( *, '(a)' ) '  For a double precision real matrix (D)'
  write ( *, '(a)' ) '  in general storage mode (GE):'
!
!  Set the matrix.
!
  a(1,1) = 1.0E+00
  a(1,2) = 2.0E+00
  a(1,3) = 3.0E+00

  a(2,1) = 4.0E+00
  a(2,2) = 5.0E+00
  a(2,3) = 6.0E+00

  a(3,1) = 7.0E+00
  a(3,2) = 8.0E+00
  a(3,3) = 0.0E+00

  call r4mat_print ( n, n, a, '  The matrix A:' )
!
!  Get the infinity norm of the matrix.
!
  anorm = r4mat_norm_li ( n, n, a )
!
!  Factor the matrix.
!
  call sgetrf ( n, n, a, lda, ipiv, info )
!
!  Get the condition number.
!
  call sgecon ( 'I', n, a, lda, anorm, rcond, work, iwork, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Condition number calculation failed!'
    write ( *, '(a,i8)' ) '  INFO = ', info
    return
  end if

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Matrix reciprocal condition number = ', rcond

  return
end
subroutine sgeqrf_test ( )

!*****************************************************************************80
!
!! SGEQRF_TEST tests SGEQRF.
!
!  Discussion:
!
!    SGEQRF computes the QR factorization of an M by N matrix A:
!
!      A(MxN) = Q(MxK) * R(KxN)
!
!    where K = min ( M, N ).
!
!    SORGQR computes the explicit form of the Q factor.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 June 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 8
  integer ( kind = 4 ), parameter :: n = 6

  integer ( kind = 4 ), parameter :: k = min ( m, n )
  integer ( kind = 4 ), parameter :: lwork = n

  real ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  integer ( kind = 4 ) lda
  real ( kind = 4 ) q(m,k)
  real ( kind = 4 ) r(k,n)
  integer ( kind = 4 ) seed
  real ( kind = 4 ) tau(k)
  real ( kind = 4 ) work(lwork)

  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SGEQRF_TEST'
  write ( *, '(a)' ) '  SGEQRF computes the QR factorization:'
  write ( *, '(a)' ) '    A = Q * R'
  write ( *, '(a)' ) '  SORGQR computes the explicit form of the Q factor.'
  write ( *, '(a)' ) '  For a double precision real matrix (D)'
  write ( *, '(a)' ) '  in general storage mode (GE):'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  In this case, our M x N matrix A has more rows'
  write ( *, '(a)' ) '  than columns:'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  M = ', m
  write ( *, '(a,i8)' ) '  N = ', n
!
!  Set A.
!
  call r4mat_uniform_01 ( m, n, seed, a )

  call r4mat_print ( m, n, a, '  The matrix A:' )
!
!  Compute the QR factorization.
!
  lda = m

  call sgeqrf ( m, n, a, lda, tau, work, lwork, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  SGEQRF returned nonzero INFO = ', info
    return
  end if

  r(1:k,1:n) = 0.0E+00
  do i = 1, k
    r(i,i:n) = a(i,i:n)
  end do
!
!  Construct Q explicitly.
!
  q(1:m,1:k) = a(1:m,1:k)

  call sorgqr ( m, k, k, q, lda, tau, work, lwork, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  SORGQR returned nonzero INFO = ', info
    return
  end if

  call r4mat_print ( m, k, q, '  The Q factor:' )

  call r4mat_print ( k, n, r, '  The R factor:' )

  a(1:m,1:n) = matmul ( q(1:m,1:k), r(1:k,1:n) )

  call r4mat_print ( m, n, a, '  The product Q * R:' )

  return
end
subroutine sormgqr_test ( )

!*****************************************************************************80
!
!! SORMGQR_TEST tests SORMGQR.
!
!  Discussion:
!
!    We want to solve the MxN linear system A*x=b using the QR approach:
!
!    Factor A:
!
!      A = Q * R                        (step 1)
!
!    Transform:
!
!               A * x =               b
!      ==>  Q * R * x =               b
!      ==>      R * x =          Q' * b  (step 2)
!      ==>          x = inv(R) * Q' * b. (step 3)
!
!    Step 1) SGEQRF computes the QR factorization of an M by N matrix A:
!    A(MxN) = Q(MxK) * R(KxN) where K = min ( M, N ).
!
!    Step 2) SORMQR can multiply Q' * b, putting the result back into b.
!
!    Step 3) We could call a LAPACK routine to solve the upper triangular
!    system R * x = Q' * b.  Instead, we will try this part ourselves.
!
!
!    LAPACK makes this process tricky because of two things it does
!    for efficiency:
!
!    *) LAPACK computes the Q and R factors in a
!       compressed and encoded form, overwriting the matrix A and
!       storing some extra information in a vector called TAU.
!
!    *) LAPACK defines K = min ( M, N ), and
!       does NOT compute the QR factorization as an MxM Q
!       times an MxN R.  Instead, it computes an MxK Q times
!       a KxN R.  This saves it worrying about zeroes, but it
!       means the programmer has to worry about proper storage
!       and correct dimensioning.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 January 2011
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 8
  integer ( kind = 4 ), parameter :: n = 6

  integer ( kind = 4 ), parameter :: k = min ( m, n )
  integer ( kind = 4 ), parameter :: lwork = n

  real ( kind = 4 ) a(m,n)
  real ( kind = 4 ) b(m)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  integer ( kind = 4 ) j
  integer ( kind = 4 ) lda
  real ( kind = 4 ) r(k,n)
  integer ( kind = 4 ) seed
  real ( kind = 4 ) tau(k)
  real ( kind = 4 ) work(lwork)
  real ( kind = 4 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SORMGQR_TEST'
  write ( *, '(a)' ) '  SORMQR can compute Q'' * b.'
  write ( *, '(a)' ) '  after SGEQRF computes the QR factorization:'
  write ( *, '(a)' ) '    A = Q * R'
  write ( *, '(a)' ) '  storing a double precision real matrix (D)'
  write ( *, '(a)' ) '  in general storage mode (GE).'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  We use these routines to carry out a QR'
  write ( *, '(a)' ) '  solve of an M by N linear system A * x = b.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  In this case, our M x N matrix A has more rows'
  write ( *, '(a)' ) '  than columns:'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  M = ', m
  write ( *, '(a,i8)' ) '  N = ', n
!
!  STEP 0: Set random A, simple x, and compute b.
!
  seed = 123456789

  call r4mat_uniform_01 ( m, n, seed, a )

  do i = 1, n
    x(i) = real ( i, kind = 4 )
  end do

  b(1:m) = matmul ( a(1:m,1:n), x(1:n) )
!
!  Wipe out X so we believe it when we get it back...
!
  x(1:n) =  0.0E+00

  call r4mat_print ( m, n, a, '  The matrix A:' )
!
!  STEP 1: Compute the QR factorization.
!
  lda = m

  call sgeqrf ( m, n, a, lda, tau, work, lwork, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  SGEQRF returned nonzero INFO = ', info
    return
  end if
!
!  Extract and save the K by N matrix R.
!
  r(1:k,1:n) = 0.0E+00
  do i = 1, k
    r(i,i:n) = a(i,i:n)
  end do
!
!  STEP 2: Multiply Q' * b to get the N by 1 result in b.
!
  call sormqr ( 'L', 'T', m, 1, k, a, m, tau, b, m, work, lwork, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  SORMQR returned nonzero INFO = ', info
    return
  end if
!
!  STEP 3: Compute inv(R) * Q' * b, or, equivalently,
!  solve R * x = Q' * b.
!
  do j = n, 1, -1
    x(j) = b(j) / r(j,j)
    do i = 1, n - 1
      b(i) = b(i) - r(i,j) * x(j)
    end do
  end do

  call r4vec_print ( n, x, '  The solution X:' )

  return
end
subroutine sgesvd_test ( )

!*****************************************************************************80
!
!! SGESVD_TEST tests SGESVD.
!
!  Discussion:
!
!    SGESVD computes the singular value decomposition:
!
!      A = U * S * V'
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 February 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 6
  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ), parameter :: lwork = 3*min(m,n) + max ( max(m,n), 2*min(m,n) )

  real ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) ldu
  integer ( kind = 4 ) ldvt
  character jobu
  character jobvt
  real ( kind = 4 ) s(min(m,n))
  integer ( kind = 4 ) seed
  real ( kind = 4 ) sigma(m,n)
  real ( kind = 4 ) u(m,m)
  real ( kind = 4 ) vt(n,n)
  real ( kind = 4 ) work(lwork)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SGESVD_TEST'
  write ( *, '(a)' ) '  For a double precision real matrix (D)'
  write ( *, '(a)' ) '  in general storage mode (GE):'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  SGESVD computes the singular value decomposition:'
  write ( *, '(a)' ) '    A = U * S * V'''
!
!  Set A.
!
  seed = 123456789

  call r4mat_uniform_01 ( m, n, seed, a )

  call r4mat_print ( m, n, a, '  The matrix A:' )
!
!  Compute the singular values and singular vectors.
!
  jobu = 'A'
  jobvt = 'A'
  lda = m
  ldu = m
  ldvt = n

  call sgesvd ( jobu, jobvt, m, n, a, lda, s, u, ldu, vt, ldvt, work, &
    lwork, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  SGESVD returned nonzero INFO = ', info
    return
  end if

  call r4vec_print ( min ( m, n ), s, '  Singular values' )

  call r4mat_print ( m, m, u, '  Left singular vectors U:' )
  call r4mat_print ( n, n, vt, '  Right singular vectors V'':' )

  sigma(1:m,1:n) = 0.0E+00
  do i = 1, min ( m, n )
    sigma(i,i) = s(i)
  end do

  a(1:m,1:n) = matmul ( u(1:m,1:m), matmul ( sigma(1:m,1:n), vt(1:n,1:n) ) )

  call r4mat_print ( m, n, a, '  The product U * S * V'':' )

  return
end
subroutine sgetri_test ( )

!*****************************************************************************80
!
!! SGETRI_TEST tests SGETRI.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 September 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 3

  integer ( kind = 4 ), parameter :: lda = n
  integer ( kind = 4 ), parameter :: lwork = n

  real ( kind = 4 ) a(lda,n)
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ipiv(n)
  real ( kind = 4 ) work(lwork)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SGETRI_TEST'
  write ( *, '(a)' ) '  SGETRI computes the inverse'
  write ( *, '(a)' ) '  of a double precision real matrix (D)'
  write ( *, '(a)' ) '  in general storage mode (GE):'
!
!  Set the matrix.
!
  a(1,1) = 1.0E+00
  a(1,2) = 2.0E+00
  a(1,3) = 3.0E+00

  a(2,1) = 4.0E+00
  a(2,2) = 5.0E+00
  a(2,3) = 6.0E+00

  a(3,1) = 7.0E+00
  a(3,2) = 8.0E+00
  a(3,3) = 0.0E+00

  call r4mat_print ( n, n, a, '  The matrix A:' )
!
!  Factor the matrix.
!
  call sgetrf ( n, n, a, lda, ipiv, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  SGETRF returned INFO = ', info
    write ( *, '(a)' ) '  The matrix is numerically singular.'
    return
  end if
!
!  Compute the inverse matrix.
!
  call sgetri ( n, a, lda, ipiv, work, lwork, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  The inversion procedure failed!'
    write ( *, '(a,i8)' ) '  INFO = ', info
    return
  end if

  call r4mat_print ( n, n, a, '  The inverse matrix:' )

  return
end
subroutine sgetrf_test ( )

!*****************************************************************************80
!
!! SGETRF_TEST tests SGETRF.
!
!  Discussion:
!
!    The problem is just an enlarged version of the
!    problem for n = 5, which is:
!
!    Matrix A is ( N -1 -1 -1 -1)    right hand side b is  (1)
!                (-1  N -1 -1 -1)                          (1)
!                (-1 -1  N -1 -1)                          (1)
!                (-1 -1 -1  N -1)                          (1)
!                (-1 -1 -1 -1  N)                          (1)
!
!    Solution is   (1)
!                  (1)
!                  (1)
!                  (1)
!                  (1)
!
!    For this problem, no pivoting is required.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 September 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 25
  integer ( kind = 4 ), parameter :: lda = n

  real ( kind = 4 ) a(lda,n)
  real ( kind = 4 ) b(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ipiv(n)
  integer ( kind = 4 ) j

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SGETRF_TEST'
  write ( *, '(a)' ) '  SGETRF factors a general matrix;'
  write ( *, '(a)' ) '  SGETRS solves a linear system;'
  write ( *, '(a)' ) '  For a double precision real matrix (D)'
  write ( *, '(a)' ) '  in general storage mode (GE):'
!
!  Assign values to matrix A and right hand side b.
!
  do i = 1, n
    do j = 1, n
      if ( i == j ) then
        a(i,j) = dble ( n )
      else
        a(i,j) = -1.0E+00
      end if
    end do
  end do

  b(1:n) = 1.0E+00
!
!  Factor the matrix.
!
  call sgetrf ( n, n, a, lda, ipiv, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  Matrix is singular, INFO = ', info
    return
  end if
!
!  Solve the linear system.
!
  call sgetrs ( 'n', n, 1, a, lda, ipiv, b, n, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  Solution procedure failed, INFO = ', info
    return
  end if

  call r4vec_print_some ( n, b, 1, 5, '  Partial solution (all should be 1)' )

  return
end
subroutine sgtsv_test ( )

!*****************************************************************************80
!
!! SGTSV_TEST tests SGTSV.
!
!  Modified:
!
!    12 September 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 100

  real ( kind = 4 ) b(n)
  real ( kind = 4 ) c(n-1)
  real ( kind = 4 ) d(n)
  real ( kind = 4 ) e(n-1)
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ldb
  integer ( kind = 4 ) nrhs

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SGTSV_TEST'
  write ( *, '(a)' ) '  SGTSV factors and solves a linear system'
  write ( *, '(a)' ) '  with a general tridiagonal matrix'
  write ( *, '(a)' ) '  for a double precision real matrix (D)'
  write ( *, '(a)' ) '  in general tridiagonal storage mode (GT).'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  The system is of order N = ', n
  write ( *, '(a)' ) ' '
!
!  Right hand side.
!
  b(1:n-1) = 0.0E+00
  b(n) = n + 1
!
!  Subdiagonal.
!  Diagonal.
!  Superdiagonal.
!
  c(1:n-1) = -1.0E+00
  d(1:n) = 2.0E+00
  e(1:n-1) = -1.0E+00

  nrhs = 1
  ldb = n
!
!  Factor and solve the linear system.
!
  call sgtsv ( n, nrhs, c, d, e, b, ldb, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Solution procedure failed.'
    write ( *, '(a,i8)' ) '  INFO = ', info
    return
  end if

  call r4vec_print_some ( n, b, 1, 5, '  Partial solution (Should be 1,2,3...)' )

  return
end
subroutine spbtrf_test ( )

!*****************************************************************************80
!
!! SPBTRF_TEST tests SPBTRF.
!
!  Discussion:
!
!    We want to compute the lower triangular Cholesky factor L
!    of a positive definite symmetric band matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 September 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5
  integer ( kind = 4 ), parameter :: nband = 1

  real ( kind = 4 ) a(nband+1,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  integer ( kind = 4 ) j
  real ( kind = 4 ) l(nband+1,n)
  real ( kind = 4 ) l_row(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SPBTRF_TEST'
  write ( *, '(a)' ) '  SPBTRF computes'
  write ( *, '(a)' ) '    the lower Cholesky factor A = L*L'' or'
  write ( *, '(a)' ) '    the upper Cholesky factor A = U''*U;'
  write ( *, '(a)' ) '  For a double precision real matrix (D)'
  write ( *, '(a)' ) '  in positive definite band storage mode (PB):'
!
!  Zero out the matrix.
!
  a(1:nband+1,1:n) = 0.0E+00
!
!  Store the diagonal of a symmetric band matrix.
!
  a(1,1:n) = 2.0E+00
!
!  Store the subdiagonal of a symmetric band matrix.
!
  a(2,1:n-1) = -1.0E+00
!
!  Get the lower triangular Cholesky factor L:
!
  l(1:nband+1,1:n) = a(1:nband+1,1:n)

  call spbtrf ( 'L', n, nband, l, nband+1, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  Factorization failed, INFO = ', info
    return
  end if
!
!  Print the relevant entries of L:
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  The lower Cholesky factor L:'
  write ( *, '(a)' ) ' '

  do i = 1, n
    do j = 1, n

      if ( 0 <= i - j .and. i-j <= nband ) then
        l_row(j) = l(i-j+1,j)
      else
        l_row(j) = 0.0E+00
      end if

    end do

    write ( *, '(5f10.6)' ) l_row(1:n)

  end do

  return
end
subroutine spbtrs_test ( )

!*****************************************************************************80
!
!! SPBTRS_TEST tests SPBTRS.
!
!  Discussion:
!
!    The problem is just an enlarged version of the
!    problem for n = 5, which is:
!
!    Matrix A is ( 2 -1  0  0  0)    right hand side b is  (1)
!                (-1  2 -1  0  0)                          (0)
!                ( 0 -1  2 -1  0)                          (0)
!                ( 0  0 -1  2 -1)                          (0)
!                ( 0  0  0 -1  2)                          (1)
!
!
!    Solution is   (1)
!                  (1)
!                  (1)
!                  (1)
!                  (1)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 September 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 25
  integer ( kind = 4 ), parameter :: nband = 1

  integer ( kind = 4 ), parameter :: lda = nband + 1

  real ( kind = 4 ) a(lda,n)
  real ( kind = 4 ) b(n)
  integer ( kind = 4 ) info
  integer ( kind = 4 ) nrhs

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SPBTRS_TEST'
  write ( *, '(a)' ) '  SPBTRS solves linear systems'
  write ( *, '(a)' ) '  for a positive definite symmetric band matrix,'
  write ( *, '(a)' ) '  stored as a double precision real matrix (D)'
  write ( *, '(a)' ) '  in positive definite band storage mode (PB):'
!
!  Zero out the matrix.
!
  a(1:lda,1:n) = 0.0E+00
!
!  Super (and sub) diagonal.
!
  a(1,2:n) = -1.0E+00
!
!  Diagonal.
!
  a(2,1:n) = 2.0E+00
!
!  Set the right hand side.
!
  b(1) = 1.0E+00
  b(2:n-1) = 0.0E+00
  b(n) = 1.0E+00
!
!  Factor the matrix.
!
  call spbtrf ( 'u', n, nband, a, lda, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  Factorization failed, INFO = ', info
    return
  end if
!
!  Solve the linear system.
!
  nrhs = 1
  call spbtrs ( 'u', n, nband, nrhs, a, lda, b, n, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  Solution failed, INFO = ', info
  end if

  call r4vec_print_some ( n, b, 1, 5, '  Partial solution (all should be 1)' )

  return
end
subroutine spotrf_test ( )

!*****************************************************************************80
!
!! SPOTRF_TEST tests SPOTRF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 September 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ), parameter :: lda = n

  real ( kind = 4 ) a(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  real ( kind = 4 ) r(n,n)
  real ( kind = 4 ) temp(n,n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SPOTRF_TEST'
  write ( *, '(a)' ) '  SPOTRF computes the Cholesky factorization R''*R'
  write ( *, '(a)' ) '  for a double precision real matrix (D)'
  write ( *, '(a)' ) '  in positive definite storage mode (PO).'
  write ( *, '(a)' ) ' '
!
!  Zero out the matrix.
!
  a(1:n,1:n) = 0.0E+00
!
!  Subdiagonal.
!
  do i = 2, n
    a(i,i-1) = -1.0E+00
  end do
!
!  Diagonal.
!
  do i = 1, n
    a(i,i) = 2.0E+00
  end do
!
!  Superdiagonal.
!
  do i = 1, n - 1
    a(i,i+1) = -1.0E+00
  end do

  call r4mat_print ( n, n, a, '  The matrix A:' )
!
!  Factor the matrix.
!
  r(1:n,1:n) = a(1:n,1:n)

  call spotrf ( 'u', n, r, lda, info )

  if ( info /= 0 ) then
    write ( *, '(a,i8)' ) '  SPOTRF returns INFO = ', info
    return
  end if

  do i = 1, n
    r(i,1:i-1) = 0.0E+00
  end do

  call r4mat_print ( n, n, r, '  The Cholesky factor R:' )

  temp(1:n,1:n) = matmul ( transpose ( r(1:n,1:n) ) , r(1:n,1:n) )

  call r4mat_print ( n, n, temp, '  The product R'' * R' )

  return
end
subroutine spotri_test ( )

!*****************************************************************************80
!
!! SPOTRI_TEST tests SPOTRI.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 September 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ), parameter :: lda = n

  real ( kind = 4 ) a(n,n)
  real ( kind = 4 ) a_inv(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  real ( kind = 4 ) r(n,n)
  real ( kind = 4 ) temp(n,n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SPOTRI_TEST'
  write ( *, '(a)' ) '  SPOTRI computes the inverse'
  write ( *, '(a)' ) '  for a double precision real matrix (D)'
  write ( *, '(a)' ) '  in positive definite storage mode (PO).'
  write ( *, '(a)' ) ' '
!
!  Zero out the matrix.
!
  a(1:n,1:n) = 0.0E+00
!
!  Subdiagonal.
!
  do i = 2, n
    a(i,i-1) = -1.0E+00
  end do
!
!  Diagonal.
!
  do i = 1, n
    a(i,i) = 2.0E+00
  end do
!
!  Superdiagonal.
!
  do i = 1, n - 1
    a(i,i+1) = -1.0E+00
  end do

  call r4mat_print ( n, n, a, '  The matrix A:' )
!
!  Factor the matrix.
!
  r(1:n,1:n) = a(1:n,1:n)

  call spotrf ( 'u', n, r, lda, info )

  if ( info /= 0 ) then
    write ( *, '(a,i8)' ) '  SPOTRF returns INFO = ', info
    return
  end if

  do i = 1, n
    r(i,1:i-1) = 0.0E+00
  end do

  call r4mat_print ( n, n, r, '  The Cholesky factor R:' )

  temp(1:n,1:n) = matmul ( transpose ( r(1:n,1:n) ) , r(1:n,1:n) )

  call r4mat_print ( n, n, temp, '  The product R'' * R' )
!
!  Compute the inverse matrix.
!
  a_inv(1:n,1:n) = r(1:n,1:n)

  call spotri ( 'u', n, a_inv, lda, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  The inversion procedure failed, INFO = ', info
    return
  end if

  do i = 1, n
    a_inv(i,1:i-1) = a_inv(1:i-1,i)
  end do

  call r4mat_print ( n, n, a_inv, '  The inverse matrix B:' )

  temp(1:n,1:n) = matmul ( transpose ( a_inv(1:n,1:n) ) , a(1:n,1:n) )

  call r4mat_print ( n, n, temp, '  The product B * A' )

  return
end
subroutine ssbgvx_test ( )

!*****************************************************************************80
!
!! SSBGVX_TEST tests SSBGVX.
!
!  Discussion:
!
!    SSBGVX deals with the generalized eigenvalue problem:
!
!      A * x = lambda * B * x
!
!    where A and B are symmetric and banded (and stored in LAPACK symmetric
!    band storage mode).  B is additionally assumed to be positive definite.
!
!    This is an "expert" interface, and the user is requesting
!    only some of the eigenvalues and eigenvectors.  In this example,
!    only the largest and smallest (in magnitude) eigenvalues will
!    be requested.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 March 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    real ( kind = 4 ) AB(LDAB,N), contains, on input, the upper or lower
!    triangle of the symmetric band matrix A, stored in the first KA+1 rows
!    of the array AB.
!    If UPLO = 'U', then
!      AB(KA+1+I-J,J) = A(I,J) for max(1,J-KA) <= I <= J;
!    If UPLO = 'L', then
!      AB(1+I-J,J) = A(I,J) for J <= I <= min(N,J+KA).
!
!    real ( kind = 4 ) ABSTOL, the absolute error tolerance for the eigenvalues.
!    If the input value of ABSTOL is not positive, then an appropriate
!    value will be determined internally and used instead.
!
!    real ( kind = 4 ) BB(LDBB,N), contains, on input, the upper or lower
!    triangle of the positive definite symmetric band matrix B, stored in
!    the first KB+1 rows of the array BB.
!    If UPLO = 'U', then
!      BB(KB+1+I-J,J) = B(I,J) for max(1,J-KB) <= I <= J;
!    If UPLO = 'L', then
!      BB(1+I-J,J) = B(I,J) for J <= I <= min(N,J+KB).
!
!    integer ( kind = 4 ) IFAIL(N), if JOBZ = 'V', then if INFO is 0, the first
!    M elements of IFAIL have been set to zero by SSBGVX, but if INFO
!    is nonzero, IFAIL contains the indices of the eigenvalues
!    for which the eigenvectors failed to converge.  If JOBZ = 'N',
!    then IFAIL is not referenced.
!
!    integer ( kind = 4 ) IL, IU, the indices of the first (smallest) and last
!    (largest) eigenvalues to be returned.  These values are only
!    used if RANGE = 'I'.  It must be the case that 1 <= IL <= IU <= N.
!
!    Integer INFO, is 0 for a successful computation,
!    negative if an input argument was illegal (the index of that
!    argument is the value of -INFO), or positive, in which case,
!    if 0 < INFO <= N, then INFO off diagonal elements of an
!    intermediate tridiagonal form did not converge to zero, or
!    if N < INFO, B is not positive definite and the computation
!    could not be completed.
!
!    integer ( kind = 4 ) IWORK(5*N), workspace.
!
!    character JOBZ, is 'N' if only eigenvalues are desired, or 'V'
!    if eigenvectors will also be required.
!
!    Integer KA, the number of superdiagonals (if UPLO = 'U') or
!    subdiagonals (if UPLO = 'L') of A that are nonzero.
!
!    integer ( kind = 4 ) KB, the number of superdiagonals (if UPLO = 'U') or
!    subdiagonals (if UPLO = 'L') of B that are nonzero.
!
!    integer ( kind = 4 ) LDAB, the leading dimension of the array AB, which
!    must be at least KA+1.
!
!    integer ( kind = 4 ) LDBB, the leading dimension of the array BB, which
!    must be at least KB+1.
!
!    integer ( kind = 4 ) LDQ, the leading dimension of the array Q.
!    If JOBZ = 'N', then Q is not used, and LDQ should be any
!    positive value such as 1.  If JOBZ = 'V', then LDQ must be
!    at least N.
!
!    integer ( kind = 4 ) LDZ, the leading dimension of the array Z.
!    If JOBZ = 'N', then Z is not used, and LDZ should be any
!    positive value such as 1.  If JOBZ = 'V', then LDZ must be
!    at least N.
!
!    integer ( kind = 4 ) M, the number of eigenvalues found by SSBGVX.
!
!    integer ( kind = 4 ) N, the order of the matrices A and B.
!
!    real ( kind = 4 ) Q(LDQ,N), if JOBZ = 'V', the N by N matrix used to
!    reduce the problem to standard form: "C * x = lambda * x"
!    and then to reduce the matrix C to tridiagonal form.  But
!    if JOBZ is not 'V', Q is not referenced.
!
!    character RANGE, specifies which eigenvalues are desired.
!    'A' means all, 'V' means a real interval will be specified in which
!    eigenvalues are to be sought, 'I' means a range of indices will
!    be specified.
!
!    character UPLO, is 'U' if the upper triangles of A and B are stored,
!    'L' if the lower triangles are stored.
!
!    real ( kind = 4 ) VL, VU, the lower and upper bounds of an interval to be
!    searched for eigenvalues.  In this case, VL must be less than VU.
!    These values are used only if RANGE = 'V'.
!
!    real ( kind = 4 ) W(N), the requested eigenvalues, in ascending order.
!
!    real ( kind = 4 ) WORK(7*N), workspace.
!
!    real ( kind = 4 ) Z(LDZ,N), if JOBZ = 'V', the I-th column of Z contains
!    the eigenvector associated with the I-th eigenvalue W(I).
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 11
  integer ( kind = 4 ), parameter :: ka = 2
  integer ( kind = 4 ), parameter :: kb = 1

  integer ( kind = 4 ), parameter :: ldab = ka+1
  integer ( kind = 4 ), parameter :: ldbb = kb+1
  integer ( kind = 4 ), parameter :: ldq = 1
  integer ( kind = 4 ), parameter :: ldz = 1

  real ( kind = 4 ) ab(ldab,n)
  real ( kind = 4 ), parameter :: abstol = 0.0E+00
  real ( kind = 4 ) bb(ldbb,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ifail(n)
  integer ( kind = 4 ) il
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) info
  integer ( kind = 4 ) iu
  integer ( kind = 4 ) iwork(5*n)
  integer ( kind = 4 ) j
  character :: jobz = 'N'
  integer ( kind = 4 ) m
  real ( kind = 4 ) q(ldq,n)
  character :: range = 'I'
  integer ( kind = 4 ) test
  character :: uplo = 'U'
  real ( kind = 4 ) value
  real ( kind = 4 ), parameter :: vl = 0.0E+00
  real ( kind = 4 ), parameter :: vu = 1.0E+00
  real ( kind = 4 ) w(n)
  real ( kind = 4 ) work(7*n)
  real ( kind = 4 ) z(ldz,n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SSBGVX_TEST'
  write ( *, '(a)' ) '  SSBGVX solves the generalized eigenvalue problem'
  write ( *, '(a)' ) '    A * X = LAMBDA * B * X'
  write ( *, '(a)' ) '  for a symmetric banded NxN matrix A, and a symmetric'
  write ( *, '(a)' ) '  banded positive definite NxN matrix B,'
  write ( *, '(a)' ) ' '

  do test = 1, 2
!
!  Set A.
!
    do j = 1, n
      ilo = max ( j - ka, 1 )
      do i = ilo, j

        if ( j == i-2 ) then
          value = -1.0E+00
        else if ( j == i-1 ) then
          value = -1.0E+00
        else if ( j == i ) then
          value = +4.0E+00
        else if ( j == i+1 ) then
          value = -1.0E+00
        else if ( j == i+2 ) then
          value = -1.0E+00
        else
          value = 0.0E+00
        end if

        ab(ka+1+i-j,j) = value

      end do
    end do
!
!  Set B.
!
    do j = 1, n
      ilo = max ( j - kb, 1 )
      do i = ilo, j

        if ( j == i-1 ) then
          value = -1.0E+00
        else if ( j == i ) then
          value = +2.0E+00
        else if ( j == i+1 ) then
          value = -1.0E+00
        else
          value = 0.0E+00
        end if

        bb(kb+1+i-j,j) = value

      end do
    end do
!
!  Request the value of the SMALLEST or LARGEST eigenvalue:
!
    if ( test == 1 ) then
      il = 1
      iu = 1
    else if ( test == 2 ) then
      il = n
      iu = n
    end if

    call ssbgvx ( jobz, range, uplo, n, ka, kb, ab, ldab, bb, ldbb, q, &
      ldq, vl, vu, il, iu, abstol, m, w, z, ldz, work, iwork, ifail, info )

    if ( info < 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Illegal value for input argument ', -info
      return
    else if ( 0 < info ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The eigenvalue or eigenvector iterations'
      write ( *, '(a)' ) '  did not converge.'
      cycle
    end if

    call r4vec_print ( m, w, '  Computed eigenvalues' )

  end do

  return
end
subroutine ssyev_test ( )

!*****************************************************************************80
!
!! SSYEV_TEST tests SSYEV.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 September 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 7

  integer ( kind = 4 ), parameter :: lwork = 3 * n - 1

  real ( kind = 4 ) a(n,n)
  integer ( kind = 4 ) info
  character jobz
  real ( kind = 4 ) lambda(n)
  character uplo
  real ( kind = 4 ) work(lwork)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SSYEV_TEST'
  write ( *, '(a)' ) '  SSYEV computes eigenvalues and eigenvectors'
  write ( *, '(a)' ) '  For a double precision real matrix (D)'
  write ( *, '(a)' ) '  in symmetric storage mode (SY).'
  write ( *, '(a)' ) ' '
!
!  Set A.
!
  call clement2 ( n, a )

  call r4mat_print ( n, n, a, '  The matrix A:' )
!
!  Compute the eigenvalues and eigenvectors.
!
  jobz = 'V'
  uplo = 'U'

  call ssyev ( jobz, uplo, n, a, n, lambda, work, lwork, info )

  if ( info /= 0 ) then

    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  SSYEV returned nonzero INFO = ', info

  else

    call r4vec_print ( n, lambda, '  The eigenvalues:' )

    if ( jobz == 'V' ) then
      call r4mat_print ( n, n, a, '  The eigenvector matrix:' )
    end if

  end if

  return
end
subroutine clement2 ( n, a )

!*****************************************************************************80
!
!! CLEMENT2 returns the Clement2 matrix.
!
!  Formula:
!
!    if ( J = I+1 )
!      A(I,J) = sqrt(I*(N-I))
!    else if ( I = J+1 )
!      A(I,J) = sqrt(J*(N-J))
!    else
!      A(I,J) = 0
!
!  Example:
!
!    N = 5
!
!       .    sqrt(4)    .       .       .
!    sqrt(4)    .    sqrt(6)    .       .
!       .    sqrt(6)    .    sqrt(6)    .
!       .       .    sqrt(6)    .    sqrt(4)
!       .       .       .    sqrt(4)    .
!
!  Properties:
!
!    A is tridiagonal.
!
!    Because A is tridiagonal, it has property A (bipartite).
!
!    A is symmetric: A' = A.
!
!    Because A is symmetric, it is normal.
!
!    Because A is normal, it is diagonalizable.
!
!    A is persymmetric: A(I,J) = A(N+1-J,N+1-I).
!
!    The diagonal of A is zero.
!
!    A is singular if N is odd.
!
!    About 64 percent of the entries of the inverse of A are zero.
!
!    The eigenvalues are plus and minus the numbers
!
!      N-1, N-3, N-5, ..., (1 or 0).
!
!    If N is even,
!
!      det ( A ) = (-1)**(N/2) * (N-1) * (N+1)**(N/2)
!
!    and if N is odd,
!
!      det ( A ) = 0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    P A Clement,
!    A class of triple-diagonal matrices for test purposes,
!    SIAM Review,
!    Volume 1, 1959, pages 50-52.
!
!  Parameters:
!
!    Input, integer N, the order of A.
!
!    Output, real ( kind = 4 ) A(N,N), the matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 4 ) a(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  do i = 1, n
    do j = 1, n

      if ( j == i + 1 ) then
        a(i,j) = sqrt ( real ( i * ( n - i ), kind = 4 ) )
      else if ( i == j + 1 ) then
        a(i,j) = sqrt ( real ( j * ( n - j ), kind = 4 ) )
      else
        a(i,j) = 0.0E+00
      end if

    end do
  end do

  return
end
function r4mat_norm_li ( m, n, a )

!*****************************************************************************80
!
!! R4MAT_NORM_LI returns the matrix L-oo norm of an R4MAT.
!
!  Discussion:
!
!    An R4MAT is an MxN array of R4's, stored by (I,J) -> [I+J*M].
!
!    The matrix L-oo norm is defined as:
!
!      R4MAT_NORM_LI =  max ( 1 <= I <= M ) sum ( 1 <= J <= N ) abs ( A(I,J) ).
!
!    The matrix L-oo norm is derived from the vector L-oo norm,
!    and satisifies:
!
!      r4vec_norm_li ( A * x ) <= r4mat_norm_li ( A ) * r4vec_norm_li ( x ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of rows in A.
!
!    Input, integer ( kind = 4 ) N, the number of columns in A.
!
!    Input, real ( kind = 4 ) A(M,N), the matrix whose L-oo
!    norm is desired.
!
!    Output, real ( kind = 4 ) R4MAT_NORM_LI, the L-oo norm of A.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) i
  real ( kind = 4 ) r4mat_norm_li
  real ( kind = 4 ) row_sum

  r4mat_norm_li = 0.0E+00

  do i = 1, m
    row_sum = sum ( abs ( a(i,1:n) ) )
    r4mat_norm_li = max ( r4mat_norm_li, row_sum )
  end do

  return
end
subroutine r4mat_print ( m, n, a, title )

!*****************************************************************************80
!
!! R4MAT_PRINT prints an R4MAT.
!
!  Discussion:
!
!    An R4MAT is a two dimensional matrix of double precision real values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer M, the number of rows in A.
!
!    Input, integer N, the number of columns in A.
!
!    Input, real ( kind = 4 ) A(M,N), the matrix.
!
!    Input, character ( len = * ) TITLE, a title to be printed.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 4 ) a(m,n)
  character ( len = * ) title

  call r4mat_print_some ( m, n, a, 1, 1, m, n, title )

  return
end
subroutine r4mat_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )

!*****************************************************************************80
!
!! R4MAT_PRINT_SOME prints some of an R4MAT.
!
!  Discussion:
!
!    An R4MAT is a two dimensional matrix of double precision real values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer M, N, the number of rows and columns.
!
!    Input, real ( kind = 4 ) A(M,N), an M by N matrix to be printed.
!
!    Input, integer ILO, JLO, the first row and column to print.
!
!    Input, integer IHI, JHI, the last row and column to print.
!
!    Input, character ( len = * ) TITLE, an optional title.
!
  implicit none

  integer ( kind = 4 ), parameter :: incx = 5
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 4 ) a(m,n)
  character ( len = 14 ) ctemp(incx)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i2hi
  integer ( kind = 4 ) i2lo
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) inc
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) j2hi
  integer ( kind = 4 ) j2lo
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  character ( len = * ) title

  if ( 0 < len_trim ( title ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) trim ( title )
  end if

  do j2lo = max ( jlo, 1 ), min ( jhi, n ), incx

    j2hi = j2lo + incx - 1
    j2hi = min ( j2hi, n )
    j2hi = min ( j2hi, jhi )

    inc = j2hi + 1 - j2lo

    write ( *, '(a)' ) ' '

    do j = j2lo, j2hi
      j2 = j + 1 - j2lo
      write ( ctemp(j2), '(i8,6x)' ) j
    end do

    write ( *, '(''  Col   '',5a14)' ) ctemp(1:inc)
    write ( *, '(a)' ) '  Row'
    write ( *, '(a)' ) ' '

    i2lo = max ( ilo, 1 )
    i2hi = min ( ihi, m )

    do i = i2lo, i2hi

      do j2 = 1, inc

        j = j2lo - 1 + j2

        if ( a(i,j) == real ( int ( a(i,j) ), kind = 4 ) ) then
          write ( ctemp(j2), '(f8.0,6x)' ) a(i,j)
        else
          write ( ctemp(j2), '(g14.6)' ) a(i,j)
        end if

      end do

      write ( *, '(i5,1x,5a14)' ) i, ( ctemp(j), j = 1, inc )

    end do

  end do

  write ( *, '(a)' ) ' '

  return
end
subroutine r4mat_uniform_01 ( m, n, seed, r )

!*****************************************************************************80
!
!! R4MAT_UNIFORM_01 fills an R4MAT with unit pseudorandom numbers.
!
!  Discussion:
!
!    An R4MAT is a two dimensional matrix of double precision real values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Springer Verlag, pages 201-202, 1983.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, pages 362-376, 1986.
!
!    Peter Lewis, Allen Goodman, James Miller,
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, pages 136-143, 1969.
!
!  Parameters:
!
!    Input, integer M, N, the number of rows and columns in the array.
!
!    Input/output, integer SEED, the "seed" value, which should NOT be 0.
!    On output, SEED has been updated.
!
!    Output, real ( kind = 4 ) R(M,N), the array of pseudorandom values.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) seed
  real ( kind = 4 ) r(m,n)

  do j = 1, n

    do i = 1, m

      k = seed / 127773

      seed = 16807 * ( seed - k * 127773 ) - k * 2836

      if ( seed < 0 ) then
        seed = seed + huge ( seed )
      end if

      r(i,j) = real ( seed, kind = 4 ) * 4.656612875E-10

    end do
  end do

  return
end
subroutine r4vec_print ( n, a, title )

!*****************************************************************************80
!
!! R4VEC_PRINT prints an R4VEC.
!
!  Discussion:
!
!    An R4VEC is an array of double precision real values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 August 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of components of the vector.
!
!    Input, real ( kind = 4 ) A(N), the vector to be printed.
!
!    Input, character ( len = * ) TITLE, an optional title.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 4 ) a(n)
  integer ( kind = 4 ) i
  character ( len = * ) title

  if ( 0 < len_trim ( title ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) trim ( title )
  end if

  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,i8,2x,g16.8)' ) i, a(i)
  end do

  return
end
subroutine r4vec_print_some ( n, a, i_lo, i_hi, title )

!*****************************************************************************80
!
!! R4VEC_PRINT_SOME prints "some" of an R4VEC.
!
!  Discussion:
!
!    An R4VEC is a vector of R4 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 October 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of entries of the vector.
!
!    Input, real ( kind = 4 ) A(N), the vector to be printed.
!
!    Input, integer I_LO, I_HI, the first and last indices to print.
!    The routine expects 1 <= I_LO <= I_HI <= N.
!
!    Input, character ( len = * ) TITLE, an optional title.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 4 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i_hi
  integer ( kind = 4 ) i_lo
  character ( len = * ) title

  if ( 0 < len_trim ( title ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) trim ( title )
  end if

  write ( *, '(a)' ) ' '
  do i = max ( i_lo, 1 ), min ( i_hi, n )
    write ( *, '(2x,i8,2x,g16.8)' ) i, a(i)
  end do

  return
end
