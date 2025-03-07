program main

!*****************************************************************************80
!
!! MAIN is the main program for QWV_2D_TEST.
!
!  Discussion:
!
!    QWV_2D_TEST tests the QWV_2D library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 February 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'QWV_2D_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the QWV_2D library.'

  call test01 ( )
  call test02 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'QWV_2D_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 tests QWV_2D for a trio of points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 May 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) d
  integer ( kind = 4 ) n
  integer ( kind = 4 ) t
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ), allocatable :: y(:)

  a = -1.0D+00
  b = +1.0D+00
  c = -1.0D+00
  d = +1.0D+00

  t = 1
  n = ( ( t + 1 ) * ( t + 2 ) ) / 2

  allocate ( w(1:n) )
  allocate ( x(1:n) )
  allocate ( y(1:n) )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01:'
  write ( *, '(a)' ) '  Compute the weights associated with an interpolatory'
  write ( *, '(a)' ) '  quadrature rule defined by N=(T+1)*(T+2)/2 points,'
  write ( *, '(a)' ) '  exact for polynomials of total degree T or less.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i4)' ) '  Degree T = ', t
  write ( *, '(a,i4)' ) '  Number of points N = ', n
  write ( *, '(a,f10.4,a,f10.4,a)' ) '  X Interval = [', a, ',', b, ']'
  write ( *, '(a,f10.4,a,f10.4,a)' ) '  Y Interval = [', c, ',', d, ']'
!
!  Set the points.
!
  x(1) =  1.0D+00
  y(1) = -1.0D+00
  x(2) =  1.0D+00
  y(2) =  1.0D+00
  x(3) = -1.0D+00
  y(3) =  1.0D+00
  call r8vec2_print ( n, x, y, '  Abscissas:' )
!
!  Compute the weights.
!
  call qwv_2d ( t, n, a, b, c, d, x, y, w )

  call r8vec_print ( n, w, '  Weights:' )
!
!  Free memory.
!
  deallocate ( w )
  deallocate ( x )
  deallocate ( y )

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 tests QWV_2D for Padua points
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 May 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) d
  integer ( kind = 4 ) n
  integer ( kind = 4 ) t
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ), allocatable :: y(:)

  a = -1.0D+00
  b = +1.0D+00
  c = -1.0D+00
  d = +1.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST02:'
  write ( *, '(a)' ) '  Compute the weights associated with an interpolatory'
  write ( *, '(a)' ) '  quadrature rule defined by N=(T+1)*(T+2)/2 points,'
  write ( *, '(a)' ) '  exact for polynomials of total degree T or less.'

  write ( *, '(a,f10.4,a,f10.4,a)' ) '  X Interval = [', a, ',', b, ']'
  write ( *, '(a,f10.4,a,f10.4,a)' ) '  Y Interval = [', c, ',', d, ']'

  do t = 0, 10

    n = ( ( t + 1 ) * ( t + 2 ) ) / 2

    write ( *, '(a)' ) ' '
    write ( *, '(a,i4)' ) '  Degree T = ', t
    write ( *, '(a,i4)' ) '  Number of points N = ', n

    allocate ( w(1:n) )
    allocate ( x(1:n) )
    allocate ( y(1:n) )

    call padua_point_set ( t, x, y )
!
!  Compute the weights.
!
    call qwv_2d ( t, n, a, b, c, d, x, y, w )

    call r8vec_print_16 ( n, w, '  Weights:' )
!
!  Free memory.
!
    deallocate ( w )
    deallocate ( x )
    deallocate ( y )

  end do

  return
end
subroutine padua_point_set ( l, x, y )

!*****************************************************************************80
!
!! PADUA_POINT_SET sets the Padua points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 May 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Marco Caliari, Stefano de Marchi, Marco Vianello,
!    Bivariate interpolation on the square at new nodal sets,
!    Applied Mathematics and Computation,
!    Volume 165, Number 2, 2005, pages 261-274.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) L, the level.
!    0 <= L <= 10.
!
!    Output, real ( kind = 8 ) X(N), Y(N), the Padua points.
!
  implicit none

  integer ( kind = 4 ) l

  real ( kind = 8 ) x(((l+1)*(l+2))/2)
  real ( kind = 8 ) y(((l+1)*(l+2))/2)

  if ( l == 0 ) then
    x( 1) =  0.000000000000000D+00
    y( 1) =  0.000000000000000D+00    
  else if ( l == 1 ) then
    x( 1) = -1.000000000000000D+00
    y( 1) = -1.000000000000000D+00    
    x( 2) = -1.000000000000000D+00
    y( 2) =  1.000000000000000D+00    
    x( 3) =  1.000000000000000D+00
    y( 3) =  0.000000000000000D+00    
  else if ( l == 2 ) then
    x( 1) = -1.000000000000000D+00
    y( 1) = -1.000000000000000D+00    
    x( 2) = -1.000000000000000D+00
    y( 2) =  0.5000000000000001D+00
    x( 3) =  0.000000000000000D+00
    y( 3) = -0.4999999999999998D+00    
    x( 4) =  0.000000000000000D+00
    y( 4) =  1.000000000000000D+00    
    x( 5) =  1.000000000000000D+00
    y( 5) = -1.000000000000000D+00
    x( 6) =  1.000000000000000D+00
    y( 6) =  0.5000000000000001D+00    
  else if ( l == 3 ) then
    x( 1) = -1.000000000000000D+00
    y( 1) = -1.000000000000000D+00    
    x( 2) = -1.000000000000000D+00
    y( 2) =  0.000000000000000D+00
    x( 3) = -1.000000000000000D+00
    y( 3) =  1.000000000000000D+00    
    x( 4) = -0.4999999999999998D+00
    y( 4) = -0.7071067811865475D+00    
    x( 5) = -0.4999999999999998D+00
    y( 5) =  0.7071067811865476D+00    
    x( 6) =  0.5000000000000001D+00
    y( 6) = -1.000000000000000D+00    
    x( 7) =  0.5000000000000001D+00
    y( 7) =  0.000000000000000D+00    
    x( 8) =  0.5000000000000001D+00
    y( 8) =  1.000000000000000D+00    
    x( 9) =  1.000000000000000D+00
    y( 9) = -0.7071067811865475D+00    
    x(10) =  1.000000000000000D+00
    y(10) =  0.7071067811865476D+00    
  else if ( l == 4 ) then
    x( 1) = -1.000000000000000D+00
    y( 1) = -1.000000000000000D+00   
    x( 2) = -1.000000000000000D+00
    y( 2) = -0.3090169943749473D+00    
    x( 3) = -1.000000000000000D+00
    y( 3) =  0.8090169943749475D+00    
    x( 4) = -0.7071067811865475D+00
    y( 4) = -0.8090169943749473D+00    
    x( 5) = -0.7071067811865475D+00
    y( 5) =  0.3090169943749475D+00
    x( 6) = -0.7071067811865475D+00
    y( 6) =  1.000000000000000D+00    
    x( 7) =  0.000000000000000D+00
    y( 7) = -1.000000000000000D+00   
    x( 8) =  0.000000000000000D+00
    y( 8) = -0.3090169943749473D+00    
    x( 9) =  0.000000000000000D+00
    y( 9) =  0.8090169943749475D+00    
    x(10) =  0.7071067811865476D+00
    y(10) = -0.8090169943749473D+00    
    x(11) =  0.7071067811865476D+00
    y(11) =  0.3090169943749475D+00 
    x(12) =  0.7071067811865476D+00
    y(12) =  1.000000000000000D+00    
    x(13) =  1.000000000000000D+00
    y(13) = -1.000000000000000D+00   
    x(14) =  1.000000000000000D+00
    y(14) = -0.3090169943749473D+00    
    x(15) =  1.000000000000000D+00
    y(15) =  0.8090169943749475D+00    
  else if ( l == 5 ) then
    x( 1) = -1.000000000000000D+00
    y( 1) = -1.000000000000000D+00
    x( 2) = -1.000000000000000D+00
    y( 2) = -0.4999999999999998D+00    
    x( 3) = -1.000000000000000D+00
    y( 3) =  0.5000000000000001D+00    
    x( 4) = -1.000000000000000D+00
    y( 4) =  1.000000000000000D+00    
    x( 5) = -0.8090169943749473D+00
    y( 5) = -0.8660254037844387D+00
    x( 6) = -0.8090169943749473D+00
    y( 6) =  0.000000000000000D+00   
    x( 7) = -0.8090169943749473D+00
    y( 7) =  0.8660254037844387D+00
    x( 8) = -0.3090169943749473D+00
    y( 8) = -1.000000000000000D+00   
    x( 9) = -0.3090169943749473D+00
    y( 9) = -0.4999999999999998D+00    
    x(10) = -0.3090169943749473D+00
    y(10) =  0.5000000000000001D+00
    x(11) = -0.3090169943749473D+00
    y(11) =  1.000000000000000D+00   
    x(12) =  0.3090169943749475D+00
    y(12) = -0.8660254037844387D+00
    x(13) =  0.3090169943749475D+00
    y(13) =  0.000000000000000D+00    
    x(14) =  0.3090169943749475D+00
    y(14) =  0.8660254037844387D+00    
    x(15) =  0.8090169943749475D+00
    y(15) = -1.000000000000000D+00    
    x(16) =  0.8090169943749475D+00
    y(16) = -0.4999999999999998D+00    
    x(17) =  0.8090169943749475D+00
    y(17) =  0.5000000000000001D+00
    x(18) =  0.8090169943749475D+00
    y(18) =  1.000000000000000D+00
    x(19) =  1.000000000000000D+00
    y(19) = -0.8660254037844387D+00    
    x(20) =  1.000000000000000D+00
    y(20) =  0.000000000000000D+00
    x(21) =  1.000000000000000D+00
    y(21) =  0.8660254037844387D+00 
  else if ( l == 6 ) then   
    x( 1) = -1.000000000000000D+00
    y( 1) = -1.000000000000000D+00   
    x( 2) = -1.000000000000000D+00
    y( 2) = -0.6234898018587335D+00    
    x( 3) = -1.000000000000000D+00
    y( 3) =  0.2225209339563144D+00    
    x( 4) = -1.000000000000000D+00
    y( 4) =  0.9009688679024191D+00    
    x( 5) = -0.8660254037844387D+00
    y( 5) = -0.9009688679024190D+00    
    x( 6) = -0.8660254037844387D+00
    y( 6) = -0.2225209339563143D+00    
    x( 7) = -0.8660254037844387D+00
    y( 7) =  0.6234898018587336D+00
    x( 8) = -0.8660254037844387D+00
    y( 8) =  1.000000000000000D+00
    x( 9) = -0.4999999999999998D+00
    y( 9) = -1.000000000000000D+00   
    x(10) = -0.4999999999999998D+00
    y(10) = -0.6234898018587335D+00    
    x(11) = -0.4999999999999998D+00
    y(11) =  0.2225209339563144D+00    
    x(12) = -0.4999999999999998D+00
    y(12) =  0.9009688679024191D+00    
    x(13) =  0.000000000000000D+00
    y(13) = -0.9009688679024190D+00    
    x(14) =  0.000000000000000D+00
    y(14) = -0.2225209339563143D+00
    x(15) =  0.000000000000000D+00
    y(15) =  0.6234898018587336D+00    
    x(16) =  0.000000000000000D+00
    y(16) =  1.000000000000000D+00
    x(17) =  0.5000000000000001D+00
    y(17) = -1.000000000000000D+00    
    x(18) =  0.5000000000000001D+00
    y(18) = -0.6234898018587335D+00    
    x(19) =  0.5000000000000001D+00
    y(19) =  0.2225209339563144D+00    
    x(20) =  0.5000000000000001D+00
    y(20) =  0.9009688679024191D+00    
    x(21) =  0.8660254037844387D+00
    y(21) = -0.9009688679024190D+00    
    x(22) =  0.8660254037844387D+00
    y(22) = -0.2225209339563143D+00    
    x(23) =  0.8660254037844387D+00
    y(23) =  0.6234898018587336D+00
    x(24) =  0.8660254037844387D+00
    y(24) =  1.000000000000000D+00    
    x(25) =  1.000000000000000D+00
    y(25) = -1.000000000000000D+00   
    x(26) =  1.000000000000000D+00
    y(26) = -0.6234898018587335D+00    
    x(27) =  1.000000000000000D+00
    y(27) =  0.2225209339563144D+00
    x(28) =  1.000000000000000D+00
    y(28) =  0.9009688679024191D+00
  else if ( l == 7 ) then    
    x( 1) = -1.000000000000000D+00
    y( 1) = -1.000000000000000D+00
    x( 2) = -1.000000000000000D+00
    y( 2) = -0.7071067811865475D+00    
    x( 3) = -1.000000000000000D+00
    y( 3) =  0.000000000000000D+00   
    x( 4) = -1.000000000000000D+00
    y( 4) =  0.7071067811865476D+00 
    x( 5) = -1.000000000000000D+00
    y( 5) =  1.000000000000000D+00    
    x( 6) = -0.9009688679024190D+00
    y( 6) = -0.9238795325112867D+00    
    x( 7) = -0.9009688679024190D+00
    y( 7) = -0.3826834323650897D+00    
    x( 8) = -0.9009688679024190D+00
    y( 8) =  0.3826834323650898D+00    
    x( 9) = -0.9009688679024190D+00
    y( 9) =  0.9238795325112867D+00
    x(10) = -0.6234898018587335D+00
    y(10) = -1.000000000000000D+00   
    x(11) = -0.6234898018587335D+00
    y(11) = -0.7071067811865475D+00
    x(12) = -0.6234898018587335D+00
    y(12) =  0.000000000000000D+00   
    x(13) = -0.6234898018587335D+00
    y(13) =  0.7071067811865476D+00
    x(14) = -0.6234898018587335D+00
    y(14) =  1.000000000000000D+00   
    x(15) = -0.2225209339563143D+00
    y(15) = -0.9238795325112867D+00    
    x(16) = -0.2225209339563143D+00
    y(16) = -0.3826834323650897D+00    
    x(17) = -0.2225209339563143D+00
    y(17) =  0.3826834323650898D+00    
    x(18) = -0.2225209339563143D+00
    y(18) =  0.9238795325112867D+00
    x(19) =  0.2225209339563144D+00
    y(19) = -1.000000000000000D+00   
    x(20) =  0.2225209339563144D+00
    y(20) = -0.7071067811865475D+00
    x(21) =  0.2225209339563144D+00
    y(21) =  0.000000000000000D+00
    x(22) =  0.2225209339563144D+00
    y(22) =  0.7071067811865476D+00
    x(23) =  0.2225209339563144D+00
    y(23) =  1.000000000000000D+00   
    x(24) =  0.6234898018587336D+00
    y(24) = -0.9238795325112867D+00    
    x(25) =  0.6234898018587336D+00
    y(25) = -0.3826834323650897D+00    
    x(26) =  0.6234898018587336D+00
    y(26) =  0.3826834323650898D+00    
    x(27) =  0.6234898018587336D+00
    y(27) =  0.9238795325112867D+00
    x(28) =  0.9009688679024191D+00
    y(28) = -1.000000000000000D+00   
    x(29) =  0.9009688679024191D+00
    y(29) = -0.7071067811865475D+00
    x(30) =  0.9009688679024191D+00
    y(30) =  0.000000000000000D+00   
    x(31) =  0.9009688679024191D+00
    y(31) =  0.7071067811865476D+00
    x(32) =  0.9009688679024191D+00
    y(32) =  1.000000000000000D+00   
    x(33) =  1.000000000000000D+00
    y(33) = -0.9238795325112867D+00    
    x(34) =  1.000000000000000D+00
    y(34) = -0.3826834323650897D+00    
    x(35) =  1.000000000000000D+00
    y(35) =  0.3826834323650898D+00    
    x(36) =  1.000000000000000D+00
    y(36) =  0.9238795325112867D+00 
  else if ( l == 8 ) then 
    x( 1) = -1.000000000000000D+00
    y( 1) = -1.000000000000000D+00   
    x( 2) = -1.000000000000000D+00
    y( 2) = -0.7660444431189779D+00    
    x( 3) = -1.000000000000000D+00
    y( 3) = -0.1736481776669303D+00    
    x( 4) = -1.000000000000000D+00
    y( 4) =  0.5000000000000001D+00    
    x( 5) = -1.000000000000000D+00
    y( 5) =  0.9396926207859084D+00    
    x( 6) = -0.9238795325112867D+00
    y( 6) = -0.9396926207859083D+00    
    x( 7) = -0.9238795325112867D+00
    y( 7) = -0.4999999999999998D+00    
    x( 8) = -0.9238795325112867D+00
    y( 8) =  0.1736481776669304D+00    
    x( 9) = -0.9238795325112867D+00
    y( 9) =  0.7660444431189780D+00
    x(10) = -0.9238795325112867D+00
    y(10) =  1.000000000000000D+00
    x(11) = -0.7071067811865475D+00
    y(11) = -1.000000000000000D+00   
    x(12) = -0.7071067811865475D+00
    y(12) = -0.7660444431189779D+00    
    x(13) = -0.7071067811865475D+00
    y(13) = -0.1736481776669303D+00    
    x(14) = -0.7071067811865475D+00
    y(14) =  0.5000000000000001D+00    
    x(15) = -0.7071067811865475D+00
    y(15) =  0.9396926207859084D+00    
    x(16) = -0.3826834323650897D+00
    y(16) = -0.9396926207859083D+00    
    x(17) = -0.3826834323650897D+00
    y(17) = -0.4999999999999998D+00    
    x(18) = -0.3826834323650897D+00
    y(18) =  0.1736481776669304D+00    
    x(19) = -0.3826834323650897D+00
    y(19) =  0.7660444431189780D+00
    x(20) = -0.3826834323650897D+00
    y(20) =  1.000000000000000D+00    
    x(21) =  0.000000000000000D+00
    y(21) = -1.000000000000000D+00   
    x(22) =  0.000000000000000D+00
    y(22) = -0.7660444431189779D+00    
    x(23) =  0.000000000000000D+00
    y(23) = -0.1736481776669303D+00    
    x(24) =  0.000000000000000D+00
    y(24) =  0.5000000000000001D+00    
    x(25) =  0.000000000000000D+00
    y(25) =  0.9396926207859084D+00    
    x(26) =  0.3826834323650898D+00
    y(26) = -0.9396926207859083D+00    
    x(27) =  0.3826834323650898D+00
    y(27) = -0.4999999999999998D+00    
    x(28) =  0.3826834323650898D+00
    y(28) =  0.1736481776669304D+00    
    x(29) =  0.3826834323650898D+00
    y(29) =  0.7660444431189780D+00
    x(30) =  0.3826834323650898D+00
    y(30) =  1.000000000000000D+00
    x(31) =  0.7071067811865476D+00
    y(31) = -1.000000000000000D+00   
    x(32) =  0.7071067811865476D+00
    y(32) = -0.7660444431189779D+00    
    x(33) =  0.7071067811865476D+00
    y(33) = -0.1736481776669303D+00    
    x(34) =  0.7071067811865476D+00
    y(34) =  0.5000000000000001D+00    
    x(35) =  0.7071067811865476D+00
    y(35) =  0.9396926207859084D+00    
    x(36) =  0.9238795325112867D+00
    y(36) = -0.9396926207859083D+00    
    x(37) =  0.9238795325112867D+00
    y(37) = -0.4999999999999998D+00    
    x(38) =  0.9238795325112867D+00
    y(38) =  0.1736481776669304D+00    
    x(39) =  0.9238795325112867D+00
    y(39) =  0.7660444431189780D+00
    x(40) =  0.9238795325112867D+00
    y(40) =  1.000000000000000D+00    
    x(41) =  1.000000000000000D+00
    y(41) = -1.000000000000000D+00   
    x(42) =  1.000000000000000D+00
    y(42) = -0.7660444431189779D+00    
    x(43) =  1.000000000000000D+00
    y(43) = -0.1736481776669303D+00    
    x(44) =  1.000000000000000D+00
    y(44) =  0.5000000000000001D+00
    x(45) =  1.000000000000000D+00
    y(45) =  0.9396926207859084D+00   
  else if ( l == 9 ) then 
    x( 1) = -1.000000000000000D+00
    y( 1) = -1.000000000000000D+00   
    x( 2) = -1.000000000000000D+00
    y( 2) = -0.8090169943749473D+00    
    x( 3) = -1.000000000000000D+00
    y( 3) = -0.3090169943749473D+00    
    x( 4) = -1.000000000000000D+00
    y( 4) =  0.3090169943749475D+00
    x( 5) = -1.000000000000000D+00
    y( 5) =  0.8090169943749475D+00    
    x( 6) = -1.000000000000000D+00
    y( 6) =  1.000000000000000D+00   
    x( 7) = -0.9396926207859083D+00
    y( 7) = -0.9510565162951535D+00    
    x( 8) = -0.9396926207859083D+00
    y( 8) = -0.5877852522924730D+00
    x( 9) = -0.9396926207859083D+00
    y( 9) =  0.000000000000000D+00   
    x(10) = -0.9396926207859083D+00
    y(10) =  0.5877852522924731D+00    
    x(11) = -0.9396926207859083D+00
    y(11) =  0.9510565162951535D+00
    x(12) = -0.7660444431189779D+00
    y(12) = -1.000000000000000D+00   
    x(13) = -0.7660444431189779D+00
    y(13) = -0.8090169943749473D+00    
    x(14) = -0.7660444431189779D+00
    y(14) = -0.3090169943749473D+00    
    x(15) = -0.7660444431189779D+00
    y(15) =  0.3090169943749475D+00    
    x(16) = -0.7660444431189779D+00
    y(16) =  0.8090169943749475D+00 
    x(17) = -0.7660444431189779D+00
    y(17) =  1.000000000000000D+00   
    x(18) = -0.4999999999999998D+00
    y(18) = -0.9510565162951535D+00    
    x(19) = -0.4999999999999998D+00
    y(19) = -0.5877852522924730D+00
    x(20) = -0.4999999999999998D+00
    y(20) =  0.000000000000000D+00   
    x(21) = -0.4999999999999998D+00
    y(21) =  0.5877852522924731D+00    
    x(22) = -0.4999999999999998D+00
    y(22) =  0.9510565162951535D+00
    x(23) = -0.1736481776669303D+00
    y(23) = -1.000000000000000D+00   
    x(24) = -0.1736481776669303D+00
    y(24) = -0.8090169943749473D+00    
    x(25) = -0.1736481776669303D+00
    y(25) = -0.3090169943749473D+00    
    x(26) = -0.1736481776669303D+00
    y(26) =  0.3090169943749475D+00    
    x(27) = -0.1736481776669303D+00
    y(27) =  0.8090169943749475D+00
    x(28) = -0.1736481776669303D+00
    y(28) =  1.000000000000000D+00   
    x(29) =  0.1736481776669304D+00
    y(29) = -0.9510565162951535D+00    
    x(30) =  0.1736481776669304D+00
    y(30) = -0.5877852522924730D+00
    x(31) =  0.1736481776669304D+00
    y(31) =  0.000000000000000D+00   
    x(32) =  0.1736481776669304D+00
    y(32) =  0.5877852522924731D+00    
    x(33) =  0.1736481776669304D+00
    y(33) =  0.9510565162951535D+00
    x(34) =  0.5000000000000001D+00
    y(34) = -1.000000000000000D+00   
    x(35) =  0.5000000000000001D+00
    y(35) = -0.8090169943749473D+00    
    x(36) =  0.5000000000000001D+00
    y(36) = -0.3090169943749473D+00    
    x(37) =  0.5000000000000001D+00
    y(37) =  0.3090169943749475D+00    
    x(38) =  0.5000000000000001D+00
    y(38) =  0.8090169943749475D+00
    x(39) =  0.5000000000000001D+00
    y(39) =  1.000000000000000D+00   
    x(40) =  0.7660444431189780D+00
    y(40) = -0.9510565162951535D+00    
    x(41) =  0.7660444431189780D+00
    y(41) = -0.5877852522924730D+00
    x(42) =  0.7660444431189780D+00
    y(42) =  0.000000000000000D+00   
    x(43) =  0.7660444431189780D+00
    y(43) =  0.5877852522924731D+00    
    x(44) =  0.7660444431189780D+00
    y(44) =  0.9510565162951535D+00
    x(45) =  0.9396926207859084D+00
    y(45) = -1.000000000000000D+00   
    x(46) =  0.9396926207859084D+00
    y(46) = -0.8090169943749473D+00    
    x(47) =  0.9396926207859084D+00
    y(47) = -0.3090169943749473D+00    
    x(48) =  0.9396926207859084D+00
    y(48) =  0.3090169943749475D+00    
    x(49) =  0.9396926207859084D+00
    y(49) =  0.8090169943749475D+00
    x(50) =  0.9396926207859084D+00
    y(50) =  1.000000000000000D+00   
    x(51) =  1.000000000000000D+00
    y(51) = -0.9510565162951535D+00
    x(52) =  1.000000000000000D+00
    y(52) = -0.5877852522924730D+00    
    x(53) =  1.000000000000000D+00
    y(53) =  0.000000000000000D+00   
    x(54) =  1.000000000000000D+00
    y(54) =  0.5877852522924731D+00
    x(55) =  1.000000000000000D+00
    y(55) =  0.9510565162951535D+00    
  else if ( l == 10 ) then
    x( 1) = -1.000000000000000D+00
    y( 1) = -1.000000000000000D+00   
    x( 2) = -1.000000000000000D+00
    y( 2) = -0.8412535328311811D+00    
    x( 3) = -1.000000000000000D+00
    y( 3) = -0.4154150130018863D+00    
    x( 4) = -1.000000000000000D+00
    y( 4) =  0.1423148382732851D+00    
    x( 5) = -1.000000000000000D+00
    y( 5) =  0.6548607339452851D+00    
    x( 6) = -1.000000000000000D+00
    y( 6) =  0.9594929736144974D+00    
    x( 7) = -0.9510565162951535D+00
    y( 7) = -0.9594929736144974D+00    
    x( 8) = -0.9510565162951535D+00
    y( 8) = -0.6548607339452850D+00    
    x( 9) = -0.9510565162951535D+00
    y( 9) = -0.1423148382732850D+00    
    x(10) = -0.9510565162951535D+00
    y(10) =  0.4154150130018864D+00    
    x(11) = -0.9510565162951535D+00
    y(11) =  0.8412535328311812D+00
    x(12) = -0.9510565162951535D+00
    y(12) =  1.000000000000000D+00
    x(13) = -0.8090169943749473D+00
    y(13) = -1.000000000000000D+00   
    x(14) = -0.8090169943749473D+00
    y(14) = -0.8412535328311811D+00    
    x(15) = -0.8090169943749473D+00
    y(15) = -0.4154150130018863D+00    
    x(16) = -0.8090169943749473D+00
    y(16) =  0.1423148382732851D+00    
    x(17) = -0.8090169943749473D+00
    y(17) =  0.6548607339452851D+00    
    x(18) = -0.8090169943749473D+00
    y(18) =  0.9594929736144974D+00    
    x(19) = -0.5877852522924730D+00
    y(19) = -0.9594929736144974D+00    
    x(20) = -0.5877852522924730D+00
    y(20) = -0.6548607339452850D+00    
    x(21) = -0.5877852522924730D+00
    y(21) = -0.1423148382732850D+00    
    x(22) = -0.5877852522924730D+00
    y(22) =  0.4154150130018864D+00    
    x(23) = -0.5877852522924730D+00
    y(23) =  0.8412535328311812D+00
    x(24) = -0.5877852522924730D+00
    y(24) =  1.000000000000000D+00
    x(25) = -0.3090169943749473D+00
    y(25) = -1.000000000000000D+00   
    x(26) = -0.3090169943749473D+00
    y(26) = -0.8412535328311811D+00    
    x(27) = -0.3090169943749473D+00
    y(27) = -0.4154150130018863D+00    
    x(28) = -0.3090169943749473D+00
    y(28) =  0.1423148382732851D+00    
    x(29) = -0.3090169943749473D+00
    y(29) =  0.6548607339452851D+00    
    x(30) = -0.3090169943749473D+00
    y(30) =  0.9594929736144974D+00    
    x(31) =  0.000000000000000D+00
    y(31) = -0.9594929736144974D+00    
    x(32) =  0.000000000000000D+00
    y(32) = -0.6548607339452850D+00    
    x(33) =  0.000000000000000D+00
    y(33) = -0.1423148382732850D+00    
    x(34) =  0.000000000000000D+00
    y(34) =  0.4154150130018864D+00
    x(35) =  0.000000000000000D+00
    y(35) =  0.8412535328311812D+00    
    x(36) =  0.000000000000000D+00
    y(36) =  1.000000000000000D+00 
    x(37) =  0.3090169943749475D+00
    y(37) = -1.000000000000000D+00    
    x(38) =  0.3090169943749475D+00
    y(38) = -0.8412535328311811D+00    
    x(39) =  0.3090169943749475D+00
    y(39) = -0.4154150130018863D+00    
    x(40) =  0.3090169943749475D+00
    y(40) =  0.1423148382732851D+00    
    x(41) =  0.3090169943749475D+00
    y(41) =  0.6548607339452851D+00    
    x(42) =  0.3090169943749475D+00
    y(42) =  0.9594929736144974D+00    
    x(43) =  0.5877852522924731D+00
    y(43) = -0.9594929736144974D+00    
    x(44) =  0.5877852522924731D+00
    y(44) = -0.6548607339452850D+00    
    x(45) =  0.5877852522924731D+00
    y(45) = -0.1423148382732850D+00    
    x(46) =  0.5877852522924731D+00
    y(46) =  0.4154150130018864D+00    
    x(47) =  0.5877852522924731D+00
    y(47) =  0.8412535328311812D+00
    x(48) =  0.5877852522924731D+00
    y(48) =  1.000000000000000D+00
    x(49) =  0.8090169943749475D+00
    y(49) = -1.000000000000000D+00   
    x(50) =  0.8090169943749475D+00
    y(50) = -0.8412535328311811D+00    
    x(51) =  0.8090169943749475D+00
    y(51) = -0.4154150130018863D+00    
    x(52) =  0.8090169943749475D+00
    y(52) =  0.1423148382732851D+00    
    x(53) =  0.8090169943749475D+00
    y(53) =  0.6548607339452851D+00    
    x(54) =  0.8090169943749475D+00
    y(54) =  0.9594929736144974D+00    
    x(55) =  0.9510565162951535D+00
    y(55) = -0.9594929736144974D+00    
    x(56) =  0.9510565162951535D+00
    y(56) = -0.6548607339452850D+00    
    x(57) =  0.9510565162951535D+00
    y(57) = -0.1423148382732850D+00    
    x(58) =  0.9510565162951535D+00
    y(58) =  0.4154150130018864D+00    
    x(59) =  0.9510565162951535D+00
    y(59) =  0.8412535328311812D+00
    x(60) =  0.9510565162951535D+00
    y(60) =  1.000000000000000D+00    
    x(61) =  1.000000000000000D+00
    y(61) = -1.000000000000000D+00   
    x(62) =  1.000000000000000D+00
    y(62) = -0.8412535328311811D+00    
    x(63) =  1.000000000000000D+00
    y(63) = -0.4154150130018863D+00    
    x(64) =  1.000000000000000D+00
    y(64) =  0.1423148382732851D+00    
    x(65) =  1.000000000000000D+00
    y(65) =  0.6548607339452851D+00 
    x(66) =  1.000000000000000D+00
    y(66) =  0.9594929736144974D+00
  else
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'PADUA_POINT_SET - Fatal error!'
    write ( *, '(a,i8)' ) '  Illegal value of L = ', l
    write ( *, '(a)' ) '  Legal values are 1 through 10.'
    stop 1
  end if

  return
end
