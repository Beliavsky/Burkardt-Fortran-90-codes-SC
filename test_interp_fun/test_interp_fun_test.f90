program main

!*****************************************************************************80
!
!! MAIN is the main program for TEST_INTERP_FUN_TEST.
!
!  Discussion:
!
!    TEST_INTERP_FUN_TEST tests TEST_INTERP_FUN.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 June 2011
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp (  )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST_INTERP_FUN_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the TEST_INTERP_FUN library.'

  call test01 ( )
  call test02 ( )
  call test03 ( )
  call test04 ( )
  call test05 ( )
  call test06 ( )
  call test07 ( )
  call test08 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST_INTERP_FUN_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 shows how P00_TITLE can be called.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 June 2011
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) prob
  integer ( kind = 4 ) prob_num
  character ( len = 80 ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  Demonstrate some of the bookkeeping routines.'
  write ( *, '(a)' ) '  P00_PROB_NUM returns the number of problems.'
  write ( *, '(a)' ) '  P00_TITLE returns the problem title.'
  write ( *, '(a)' ) '  P00_LIMIT returns the problem limits.'

  call p00_prob_num ( prob_num )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of problems = ', prob_num

  do prob = 1, prob_num

    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  Problem ', prob
    call p00_title ( prob, title )
    write ( *, '(a)' ) '  Problem TITLE = "' // trim ( title ) // '".'
    call p00_lim ( prob, a, b )
    write ( *, '(a,g14.6)' ) '  Problem lower limit A = ', a
    write ( *, '(a,g14.6)' ) '  Problem upper limit B = ', b

  end do

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 shows how P00_STORY can be called.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 June 2011
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) prob
  integer ( kind = 4 ) prob_num

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  P00_STORY prints the problem "story".'

  call p00_prob_num ( prob_num )

  do prob = 1, prob_num

    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  Problem ', prob

    call p00_story ( prob )

  end do

  return
end
subroutine test03 ( )

!*****************************************************************************80
!
!! TEST03 uses equally spaced polynomial interpolation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 May 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: max_tab = 21

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) diftab(max_tab)
  real ( kind = 8 ) error_max
  integer ( kind = 4 ) i
  integer ( kind = 4 ) imax
  integer ( kind = 4 ) n
  integer ( kind = 4 ) prob
  integer ( kind = 4 ) prob_num
  real ( kind = 8 ) p00_fun
  character ( len = 80 ) title
  integer ( kind = 4 ) type
  real ( kind = 8 ) x
  real ( kind = 8 ) xtab(max_tab)
  real ( kind = 8 ) yapprox
  real ( kind = 8 ) ytab(max_tab)
  real ( kind = 8 ) ytrue

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST03'
  write ( *, '(a)' ) '  Equally spaced polynomial interpolation.'
  write ( *, '(a)' ) '  Evaluate the function at N equally spaced points.'
  write ( *, '(a)' ) '  Determine the N-1 degree polynomial interpolant.'
  write ( *, '(a)' ) '  Estimate the maximum difference between the function'
  write ( *, '(a)' ) '  and the interpolant.'

  call p00_prob_num ( prob_num )

  do prob = 1, prob_num

    call p00_title ( prob, title )
    call p00_lim ( prob, a, b )

    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  Problem ', prob
    write ( *, '(2x,a)' ) trim ( title )
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '     N   Max ||Error||'
    write ( *, '(a)' ) ' '

    do n = 1, max_tab, 4
!
!  Evaluate the function at N equally spaced points.
!
      do i = 1, n

        if ( n == 1 ) then
          xtab(i) = 0.5D+00 * ( a + b )
        else
          xtab(i) = ( real ( n - i,     kind = 8 ) * a &
                    + real (     i - 1, kind = 8 ) * b &
                  ) / real ( n     - 1, kind = 8 )
        end if

        ytab(i) = p00_fun ( prob, xtab(i) )

      end do
!
!  Construct the interpolating polynomial via finite differences.
!
      call data_to_dif ( n, xtab, ytab, diftab )
!
!  Now examine the approximation error.
!
      error_max = 0.0D+00
      imax = 100

      do i = 0, imax

        if ( imax == 0 ) then
          x = 0.5D+00 * ( a + b )
        else
          x = ( real ( imax - i, kind = 8 ) * a    &
              + real (        i, kind = 8 ) * b  ) &
              / real ( imax,     kind = 8 )
        end if

        ytrue = p00_fun ( prob, x )
        call dif_val ( n, xtab, diftab, x, yapprox )
        error_max = max ( error_max, abs ( ytrue - yapprox ) )

      end do

      write ( *, '(2x,i4,2x,g14.6)' ) n, error_max

    end do

  end do

  return
end
subroutine test04 ( )

!*****************************************************************************80
!
!! TEST04 uses Bernstein polynomial approximation on functional problems.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 August 2011
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: max_tab = 21

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) error_max
  integer ( kind = 4 ) i
  integer ( kind = 4 ) imax
  integer ( kind = 4 ) ndata
  integer ( kind = 4 ) prob
  integer ( kind = 4 ) prob_num
  real ( kind = 8 ) p00_fun
  character ( len = 80 ) title
  real ( kind = 8 ), allocatable, dimension ( : ) :: xdata
  real ( kind = 8 ) xval
  real ( kind = 8 ), allocatable, dimension ( : ) :: ydata
  real ( kind = 8 ) ytrue
  real ( kind = 8 ) yval

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST04'
  write ( *, '(a)' ) '  Bernstein polynomial approximation.'
  write ( *, '(a)' ) '  Evaluate the function at N equally spaced points.'
  write ( *, '(a)' ) '  Determine the N-1 degree Bernstein polynomial approximant.'
  write ( *, '(a)' ) '  Estimate the maximum difference between the function'
  write ( *, '(a)' ) '  and the approximant.'

  call p00_prob_num ( prob_num )

  do prob = 1, prob_num

    call p00_title ( prob, title )
    call p00_lim ( prob, a, b )

    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  Problem ', prob
    write ( *, '(2x,a)' ) trim ( title )
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '     N   Max ||Error||'
    write ( *, '(a)' ) ' '

    do ndata = 1, max_tab, 4
!
!  Evaluate the function at NDATA equally spaced points.
!
      allocate ( xdata(1:ndata) )
      allocate ( ydata(1:ndata) )

      do i = 1, ndata

        if ( ndata == 1 ) then
          xdata(i) = 0.5D+00 * ( a + b )
        else
          xdata(i) = ( real ( ndata - i,     kind = 8 ) * a    &
                     + real (         i - 1, kind = 8 ) * b  ) &
                     / real ( ndata     - 1, kind = 8 )
        end if

        ydata(i) = p00_fun ( prob, xdata(i) )

      end do
!
!  Now examine the approximation error.
!
      error_max = 0.0D+00
      imax = 100

      do i = 0, imax

        if ( imax == 0 ) then
          xval = 0.5D+00 * ( a + b )
        else
          xval = ( real ( imax - i, kind = 8 ) * a    &
                 + real (        i, kind = 8 ) * b  ) &
                 / real ( imax,     kind = 8 )
        end if

        ytrue = p00_fun ( prob, xval )

        call bpab_approx ( ndata - 1, a, b, ydata, xval, yval )

        error_max = max ( error_max, abs ( ytrue - yval ) )

      end do

      write ( *, '(2x,i4,2x,g14.6)' ) ndata, error_max

      deallocate ( xdata )
      deallocate ( ydata )

    end do

  end do

  return
end
subroutine test05 ( )

!*****************************************************************************80
!
!! TEST05 uses linear spline interpolation on all problems.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 June 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) error_max
  integer ( kind = 4 ) i
  integer ( kind = 4 ) imax
  character mark
  integer ( kind = 4 ), parameter :: max_data = 21
  integer ( kind = 4 ) ndata
  integer ( kind = 4 ) prob
  integer ( kind = 4 ) prob_num
  real ( kind = 8 ) p00_fun
  character ( len = 80 ) title
  real ( kind = 8 ), allocatable, dimension ( : ) :: xdata
  real ( kind = 8 ) xval
  real ( kind = 8 ), allocatable, dimension ( : ) :: ydata
  real ( kind = 8 ) ypval
  real ( kind = 8 ) ytrue
  real ( kind = 8 ) yval

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST05'
  write ( *, '(a)' ) '  Linear spline interpolation.'

  call p00_prob_num ( prob_num )

  do prob = 1, prob_num

    call p00_title ( prob, title )
    call p00_lim ( prob, a, b )

    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  Problem ', prob
    write ( *, '(2x,a)' ) trim ( title )
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '     N   Max ||Error||'
    write ( *, '(a)' ) ' '

    do ndata = 2, max_data, 4

      allocate ( xdata(1:ndata) )
      allocate ( ydata(1:ndata) )
!
!  Evaluate the function at NDATA equally spaced points.
!
      do i = 1, ndata

        if ( ndata == 1 ) then
          xdata(i) = 0.5D+00 * ( a + b )
        else
          xdata(i) = ( real ( ndata - i,     kind = 8 ) * a    &
                     + real (         i - 1, kind = 8 ) * b  ) &
                     / real ( ndata     - 1, kind = 8 )
        end if

        ydata(i) = p00_fun ( prob, xdata(i) )

      end do
!
!  Evaluate the interpolation function.
!
      error_max = 0.0D+00
      imax = 100

      do i = 0, imax

        if ( imax == 0 ) then
          xval = 0.5D+00 * ( a + b )
        else
          xval = ( real ( imax - i, kind = 8 ) * a    &
                 + real (        i, kind = 8 ) * b  ) &
                 / real ( imax,     kind = 8 )
        end if

        call spline_linear_val ( ndata, xdata, ydata, xval, yval, ypval )

        ytrue = p00_fun ( prob, xval )

        error_max = max ( error_max, abs ( yval - ytrue ) );

      end do

      write ( *, '(2x,i4,2x,g14.6)' ) ndata, error_max

      deallocate ( xdata )
      deallocate ( ydata )

    end do

  end do

  return
end
subroutine test06 ( )

!*****************************************************************************80
!
!! TEST06 uses Overhauser spline interpolation on all problems.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 June 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: num_dim = 1

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jmax
  character mark
  integer ( kind = 4 ) ndata
  integer ( kind = 4 ) prob
  integer ( kind = 4 ) prob_num
  real ( kind = 8 ) p00_fun
  character ( len = 80 ) title
  real ( kind = 8 ), allocatable, dimension ( : ) :: xdata
  real ( kind = 8 ) xval
  real ( kind = 8 ), allocatable, dimension ( : ) :: ydata
  real ( kind = 8 ) yval

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST06'
  write ( *, '(a)' ) '  Overhauser spline interpolation.'

  call p00_prob_num ( prob_num )

  do prob = 1, prob_num

    call p00_title ( prob, title )

    call p00_lim ( prob, a, b )

    ndata = 11
    allocate ( xdata(1:ndata) )
    allocate ( ydata(1:ndata) )

    do i = 1, ndata
      xdata(i) = ( real ( ndata - i,     kind = 8 ) * a   &
                 + real (         i - 1, kind = 8 ) * b ) &
                 / real ( ndata     - 1, kind = 8 )
      ydata(i) = p00_fun ( prob, xdata(i) )
    end do

    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  Problem ', prob
    write ( *, '(a)' ) trim ( title )
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  X   Y'
    write ( *, '(a)' ) ' '
!
!  Evaluate the interpolation function.
!
    do i = 1, ndata - 1

      jmax = 3

      if ( i == ndata - 1 ) then
        jhi = jmax
      else
        jhi = jmax - 1
      end if

      do j = 1, jhi

        xval = ( real ( jmax - j,     kind = 8 ) * xdata(i)     &
               + real (        j - 1, kind = 8 ) * xdata(i+1) ) &
               / real ( jmax     - 1, kind = 8 )

        call spline_overhauser_val ( num_dim, ndata, xdata, ydata, xval, &
          yval )

        if ( j == 1 .or. j == 3 ) then
          mark = '*'
        else
          mark = ' '
        end if

        write ( *, '(2x,a,2g14.6)' ) mark, xval, yval

      end do

    end do

    deallocate ( xdata )
    deallocate ( ydata )

  end do

  return
end
subroutine test07 ( )

!*****************************************************************************80
!
!! TEST07 uses cubic spline interpolation on all problems.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 June 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ibcbeg
  integer ( kind = 4 ) ibcend
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jmax
  character mark
  integer ( kind = 4 ) ndata
  integer ( kind = 4 ) prob
  integer ( kind = 4 ) prob_num
  real ( kind = 8 ) p00_fun
  character ( len = 80 ) title
  real ( kind = 8 ), allocatable, dimension ( : ) :: xdata
  real ( kind = 8 ) xval
  real ( kind = 8 ) ybcbeg
  real ( kind = 8 ) ybcend
  real ( kind = 8 ), allocatable, dimension ( : ) :: ydata
  real ( kind = 8 ), allocatable, dimension ( : ) :: ypp
  real ( kind = 8 ) yppval
  real ( kind = 8 ) ypval
  real ( kind = 8 ) yval

  ibcbeg = 0
  ibcend = 0
  ybcbeg = 0.0D+00
  ybcend = 0.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST07'
  write ( *, '(a)' ) '  Cubic spline interpolation.'

  call p00_prob_num ( prob_num )

  do prob = 1, prob_num

    call p00_title ( prob, title )
    call p00_lim ( prob, a, b )

    ndata = 11
    allocate ( xdata(1:ndata) )
    allocate ( ydata(1:ndata) )
    allocate ( ypp(1:ndata) )

    do i = 1, ndata
      xdata(i) = ( real ( ndata - i,     kind = 8 ) * a   &
                 + real (         i - 1, kind = 8 ) * b ) &
                 / real ( ndata     - 1, kind = 8 )
      ydata(i) = p00_fun ( prob, xdata(i) )
    end do
!
!  Set up the interpolation function.
!
    call spline_cubic_set ( ndata, xdata, ydata, ibcbeg, ybcbeg, &
      ibcend, ybcend, ypp )

    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  Problem ', prob
    write ( *, '(2x,a)' ) trim ( title )
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '    X   Y'
    write ( *, '(a)' ) ' '
!
!  Evaluate the interpolation function.
!
    do i = 1, ndata - 1

      jmax = 3

      if ( i == ndata - 1 ) then
        jhi = jmax
      else
        jhi = jmax - 1
      end if

      do j = 1, jhi

        xval = ( real ( jmax - j,     kind = 8 ) * xdata(i)     &
               + real (        j - 1, kind = 8 ) * xdata(i+1) ) &
               / real ( jmax     - 1, kind = 8 )

        call spline_cubic_val ( ndata, xdata, ydata, ypp, xval, yval, ypval, &
          yppval )

        if ( j == 1 .or. j == 3 ) then
          mark = '*'
        else
          mark = ' '
        end if

        write ( *, '(2x,a,2g14.6)' ) mark, xval, yval

      end do

    end do

    deallocate ( xdata )
    deallocate ( ydata )
    deallocate ( ypp )

  end do

  return
end
subroutine test08 ( )

!*****************************************************************************80
!
!! TEST08 uses B spline approximation on all problems.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 June 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jmax
  character mark
  integer ( kind = 4 ) ndata
  integer ( kind = 4 ) prob
  integer ( kind = 4 ) prob_num
  real ( kind = 8 ) p00_fun
  character ( len = 80 ) title
  real ( kind = 8 ), allocatable, dimension ( : ) :: xdata
  real ( kind = 8 ) xval
  real ( kind = 8 ), allocatable, dimension ( : ) :: ydata
  real ( kind = 8 ) yval

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST08'
  write ( *, '(a)' ) '  B spline approximation.'

  call p00_prob_num ( prob_num )

  do prob = 1, prob_num

    call p00_title ( prob, title )
    call p00_lim ( prob, a, b )

    ndata = 11
    allocate ( xdata(1:ndata) )
    allocate ( ydata(1:ndata) )

    do i = 1, ndata
      xdata(i) = ( real ( ndata - i,     kind = 8 ) * a   &
                 + real (         i - 1, kind = 8 ) * b ) &
                 / real ( ndata     - 1, kind = 8 )
      ydata(i) = p00_fun ( prob, xdata(i) )
    end do

    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  Problem ', prob
    write ( *, '(2x,a)' ) trim ( title )
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '       X        Y'
    write ( *, '(a)' ) ' '
!
!  Evaluate the interpolation function.
!
    do i = 1, ndata - 1

      jmax = 3

      if ( i == ndata - 1 ) then
        jhi = jmax
      else
        jhi = jmax - 1
      end if

      do j = 1, jhi

        xval = ( real ( jmax - j,     kind = 8 ) * xdata(i)     &
               + real (        j - 1, kind = 8 ) * xdata(i+1) ) &
               / real ( jmax     - 1, kind = 8 )

        call spline_b_val ( ndata, xdata, ydata, xval, yval )

        if ( j == 1 .or. j == 3 ) then
          mark = '*'
        else
          mark = ' '
        end if

        write ( *, '(2x,a,2g14.6)' ) mark, xval, yval

      end do

    end do

    deallocate ( xdata )
    deallocate ( ydata )

  end do

  return
end
