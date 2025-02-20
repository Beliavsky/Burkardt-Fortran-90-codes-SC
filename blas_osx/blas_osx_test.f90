program main

!*****************************************************************************80
!
!! MAIN is the main program for BLAS_OSX_TEST.
!
!  Discussion:
!
!    BLAS_OSX_TEST tests the BLAS library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 February 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BLAS_OSX_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the BLAS library on a Macintosh with OSX.'

  call dgemm_test ( )
  call dtrmm_test ( )
  call dtrsm_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BLAS_OSX_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine dgemm_test ( )

!*****************************************************************************80
!
!! DGEMM_TEST tests DGEMM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 February 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  real ( kind = 8 ) alpha
  real ( kind = 8 ), allocatable :: b(:,:)
  real ( kind = 8 ) beta
  real ( kind = 8 ), allocatable :: c(:,:)
  integer ( kind = 4 ) k
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) ldb
  integer ( kind = 4 ) ldc
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  character transa
  character transb
  character transc

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DGEMM_TEST'
  write ( *, '(a)' ) '  DGEMM multiplies two matrices A and B.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  1: C = alpha * A  * B  + beta * C;'
  write ( *, '(a)' ) '  2: C = alpha * A'' * B  + beta * C;'
  write ( *, '(a)' ) '  3: C = alpha * A  * B'' + beta * C;'
  write ( *, '(a)' ) '  4: C = alpha * A'' * B'' + beta * C;'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  We carry out all four calculations, but in each case,'
  write ( *, '(a)' ) '  we choose our input matrices so that we get the same result.'
!
!  C = alpha * A * B + beta * C.
!
  transa = 'N'
  transb = 'N'
  transc = 'N'
  m = 4
  n = 5
  k = 3
  alpha = 2.0D+00
  lda = m
  allocate ( a(1:lda,1:k) )
  call r8mat_test ( transa, lda, m, k, a )
  ldb = k
  allocate ( b(1:ldb,1:n) )
  call r8mat_test ( transb, ldb, k, n, b )
  beta = 3.0D+00
  ldc = m
  allocate ( c(1:ldc,1:n) )
  call r8mat_test ( transc, ldc, m, n, c )

  call dgemm ( transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc )

  call r8mat_print ( m, n, c, '  C = alpha * A * B + beta * C:' );

  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  C = alpha * A' * B + beta * C.
!
  transa = 'T'
  transb = 'N'
  transc = 'N'
  m = 4
  n = 5
  k = 3
  alpha = 2.0D+00
  lda = k
  allocate ( a(1:lda,1:m) )
  call r8mat_test ( transa, lda, m, k, a )
  ldb = k
  allocate ( b(1:ldb,1:n) )
  call r8mat_test ( transb, ldb, k, n, b )
  beta = 3.0D+00
  ldc = m
  allocate ( c(1:ldc,1:n) )
  call r8mat_test ( transc, ldc, m, n, c )

  call dgemm ( transa, transb,  m, n, k, alpha, a, lda, b, ldb, beta, c, ldc )

  call r8mat_print ( m, n, c, '  C = alpha * A'' * B + beta * C:' )

  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  C = alpha * A * B' + beta * C.
!
  transa = 'N'
  transb = 'T'
  transc = 'N'
  m = 4
  n = 5
  k = 3
  alpha = 2.0D+00
  lda = m
  allocate ( a(1:lda,1:k) )
  call r8mat_test ( transa, lda, m, k, a )
  ldb = n
  allocate ( b(1:ldb,1:k) )
  call r8mat_test ( transb, ldb, k, n, b )
  beta = 3.0D+00
  ldc = m
  allocate ( c(1:ldc,1:n) )
  call r8mat_test ( transc, ldc, m, n, c )

  call dgemm ( transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc )

  call r8mat_print ( m, n, c, '  C = alpha * A * B'' + beta * C:' )

  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  C = alpha * A' * B' + beta * C.
!
  transa = 'T'
  transb = 'T'
  transc = 'N'
  m = 4
  n = 5
  k = 3
  alpha = 2.0D+00
  lda = k
  allocate ( a(1:lda,1:m) )
  call r8mat_test ( transa, lda, m, k, a )
  ldb = n
  allocate ( b(1:ldb,1:k) )
  call r8mat_test ( transb, ldb, k, n, b )
  beta = 3.0D+00
  ldc = m
  allocate ( c(1:ldc,1:n) )
  call r8mat_test ( transc, ldc, m, n, c )

  call dgemm ( transa, transb,  m, n, k, alpha, a, lda, b, ldb, beta, c, ldc )

  call r8mat_print ( m, n, c, '  C = alpha * A'' * B'' + beta * C:' )

  deallocate ( a )
  deallocate ( b )
  deallocate ( c )

  return
end
subroutine dtrmm_test ( )

!*****************************************************************************80
!
!! DTRMM_TEST tests DTRMM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 April 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  real ( kind = 8 ) alpha
  real ( kind = 8 ), allocatable :: b(:,:)
  character diag
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) ldb
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  character side
  character transa
  character transb
  character uplo

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DTRMM_TEST'
  write ( *, '(a)' ) '  DTRMM multiplies a triangular matrix A and a'
  write ( *, '(a)' ) '  rectangular matrix B'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  1: B = alpha * A  * B;'
  write ( *, '(a)' ) '  2: B = alpha * A'' * B;'
!
!  B = alpha * A * B.
!
  side = 'L'
  uplo = 'U'
  transa = 'N'
  diag = 'N'
  m = 4
  n = 5
  alpha = 2.0D+00
  lda = m
  ldb = m

  allocate ( a(1:lda,1:m) )
  do j = 1, m
    do i = 1, j
      a(i,j) = real ( i + j, kind = 8 )
    end do
    do i = j + 1, m
      a(i,j) = 0.0D+00
    end do
  end do

  allocate ( b(1:ldb,1:n) )
  transb = 'N'
  call r8mat_test ( transb, ldb, m, n, b )

  call dtrmm ( side, uplo, transa, diag, m, n, alpha, a, lda, b, ldb )

  call r8mat_print ( m, n, b, '  B = alpha * A * B:' );

  deallocate ( a )
  deallocate ( b )
!
!  B = alpha * A' * B.
!
  side = 'L'
  uplo = 'U'
  transa = 'T'
  diag = 'N'
  m = 4
  n = 5
  alpha = 2.0D+00
  lda = m
  ldb = m

  allocate ( a(1:lda,1:m) )
  do j = 1, m
    do i = 1, j
      a(i,j) = real ( i + j, kind = 8 )
    end do
    do i = j + 1, m
      a(i,j) = 0.0D+00
    end do
  end do

  allocate ( b(1:ldb,1:n) )
  transb = 'N'
  call r8mat_test ( transb, ldb, m, n, b )

  call dtrmm ( side, uplo, transa, diag, m, n, alpha, a, lda, b, ldb )

  call r8mat_print ( m, n, b, '  B = alpha * A * B:' );

  deallocate ( a )
  deallocate ( b )

  return
end
subroutine dtrsm_test ( )

!*****************************************************************************80
!
!! DTRSM_TEST tests DTRSM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 April 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  real ( kind = 8 ) alpha
  real ( kind = 8 ), allocatable :: b(:,:)
  character diag
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) ldb
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  character side
  character transa
  character transb
  character uplo

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DTRSM_TEST'
  write ( *, '(a)' ) '  DTRSM solves a linear system involving a triangular'
  write ( *, '(a)' ) '  matrix A and a rectangular matrix B.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  1: Solve A  * X  = alpha * B;'
  write ( *, '(a)' ) '  2: Solve A'' * X  = alpha * B;'
  write ( *, '(a)' ) '  3: Solve X  * A  = alpha * B;'
  write ( *, '(a)' ) '  4: Solve X  * A'' = alpha * B;'
!
!  Solve A * X = alpha * B.
!
  side = 'L'
  uplo = 'U'
  transa = 'N'
  diag = 'N'
  m = 4
  n = 5
  alpha = 2.0D+00
  lda = m
  ldb = m

  allocate ( a(1:lda,1:m) )
  do j = 1, m
    do i = 1, j
      a(i,j) = real ( i + j, kind = 8 )
    end do
    do i = j + 1, m
      a(i,j) = 0.0D+00
    end do
  end do

  allocate ( b(1:ldb,1:n) )
  transb = 'N'
  call r8mat_test ( transb, ldb, m, n, b )

  call dtrsm ( side, uplo, transa, diag, m, n, alpha, a, lda, b, ldb )

  call r8mat_print ( m, n, b, '  X = inv ( A ) * alpha * B:' );

  deallocate ( a )
  deallocate ( b )
!
!  Solve A' * X = alpha * B.
!
  side = 'L'
  uplo = 'U'
  transa = 'T'
  diag = 'N'
  m = 4
  n = 5
  alpha = 2.0D+00
  lda = m
  ldb = m

  allocate ( a(1:lda,1:m) )
  do j = 1, m
    do i = 1, j
      a(i,j) = real ( i + j, kind = 8 )
    end do
    do i = j + 1, m
      a(i,j) = 0.0D+00
    end do
  end do

  allocate ( b(1:ldb,1:n) )
  transb = 'N'
  call r8mat_test ( transb, ldb, m, n, b )

  call dtrsm ( side, uplo, transa, diag, m, n, alpha, a, lda, b, ldb )

  call r8mat_print ( m, n, b, '  X = inv ( A'' ) * alpha * B:' );

  deallocate ( a )
  deallocate ( b )
!
!  Solve X * A = alpha * B.
!
  side = 'R'
  uplo = 'U'
  transa = 'N'
  diag = 'N'
  m = 4
  n = 5
  alpha = 2.0D+00
  lda = n
  ldb = m

  allocate ( a(1:lda,1:n) )
  do j = 1, n
    do i = 1, j
      a(i,j) = real ( i + j, kind = 8 )
    end do
    do i = j + 1, n
      a(i,j) = 0.0D+00
    end do
  end do

  allocate ( b(1:ldb,1:n) )
  transb = 'N'
  call r8mat_test ( transb, ldb, m, n, b )

  call dtrsm ( side, uplo, transa, diag, m, n, alpha, a, lda, b, ldb )

  call r8mat_print ( m, n, b, '  X = alpha * B * inv ( A ):' );

  deallocate ( a )
  deallocate ( b )
!
!  Solve X * A'' = alpha * B.
!
  side = 'R'
  uplo = 'U'
  transa = 'T'
  diag = 'N'
  m = 4
  n = 5
  alpha = 2.0D+00
  lda = n
  ldb = m

  allocate ( a(1:lda,1:n) )
  do j = 1, n
    do i = 1, j
      a(i,j) = real ( i + j, kind = 8 )
    end do
    do i = j + 1, n
      a(i,j) = 0.0D+00
    end do
  end do

  allocate ( b(1:ldb,1:n) )
  transb = 'N'
  call r8mat_test ( transb, ldb, m, n, b )

  call dtrsm ( side, uplo, transa, diag, m, n, alpha, a, lda, b, ldb )

  call r8mat_print ( m, n, b, '  X = alpha * B * inv ( A'' ):' );

  deallocate ( a )
  deallocate ( b )

  return
end
subroutine r8mat_print ( m, n, a, title )

!*****************************************************************************80
!
!! R8MAT_PRINT prints an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
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
!    Input, integer ( kind = 4 ) M, the number of rows in A.
!
!    Input, integer ( kind = 4 ) N, the number of columns in A.
!
!    Input, real ( kind = 8 ) A(M,N), the matrix.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  character ( len = * ) title

  call r8mat_print_some ( m, n, a, 1, 1, m, n, title )

  return
end
subroutine r8mat_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )

!*****************************************************************************80
!
!! R8MAT_PRINT_SOME prints some of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 September 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input, real ( kind = 8 ) A(M,N), an M by N matrix to be printed.
!
!    Input, integer ( kind = 4 ) ILO, JLO, the first row and column to print.
!
!    Input, integer ( kind = 4 ) IHI, JHI, the last row and column to print.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ), parameter :: incx = 5
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
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

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )

  if ( m <= 0 .or. n <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  (None)'
    return
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

        if ( a(i,j) == real ( int ( a(i,j) ), kind = 8 ) ) then
          write ( ctemp(j2), '(f8.0,6x)' ) a(i,j)
        else
          write ( ctemp(j2), '(g14.6)' ) a(i,j)
        end if

      end do

      write ( *, '(i5,a,5a14)' ) i, ':', ( ctemp(j), j = 1, inc )

    end do

  end do

  return
end
subroutine r8mat_test ( trans, lda, m, n, a )

!*****************************************************************************80
!
!! R8MAT_TEST sets up a test matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!  
!  Modified:
!
!    10 February 2014
!
!  Author:
!
!    John Burkardt.
!
!  Parameters:
!
!    Input, character * ( 1 ) TRANS, indicates whether matrix is to be 
!    transposed.
!    'N', no transpose.
!    'T', transpose the matrix.
!
!    Input, integer ( kind = 4 ) LDA, the leading dimension of the matrix.
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns of 
!    the matrix.
!
!    Output, real ( kind = 8 ) A(LDA,*), the matrix.
!    if TRANS is 'N', then the matrix is stored in LDA*N entries,
!    as an M x N matrix;
!    if TRANS is 'T', then the matrix is stored in LDA*M entries,
!    as an N x M matrix.
!
  implicit none

  integer ( kind = 4 ) lda

  real ( kind = 8 ) a(lda,*)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  character * ( 1 ) trans

  if ( trans == 'N' ) then

    do j = 1, n
      do i = 1, m
        a(i,j) = real ( 10 * i + j, kind = 8 )
      end do
    end do

  else

    do j = 1, n
      do i = 1, m
        a(j,i) = real ( 10 * i + j, kind = 8 )
      end do
    end do

  end if

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

  write ( *, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end

