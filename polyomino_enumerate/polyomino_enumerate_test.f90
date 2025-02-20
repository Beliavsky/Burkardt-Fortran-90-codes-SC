program main

!*****************************************************************************80
!
!! MAIN is the main program for POLYOMINO_ENUMERATE_TEST.
!
!  Discussion:
!
!    POLYOMINO_ENUMERATE_TEST tests the POLYOMINO_ENUMERATE library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 May 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POLYOMINO_ENUMERATE_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the POLYOMINO_ENUMERATE library.'

  call polyomino_enumerate_chiral_test ( )
  call polyomino_enumerate_fixed_test ( )
  call polyomino_enumerate_free_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POLYOMINO_ENUMERATE_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine polyomino_enumerate_chiral_test ( )

!*****************************************************************************80
!
!! POLYOMINO_ENUMERATE_CHIRAL_TEST tests POLYOMINO_ENUMERATE_CHIRAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 May 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n_data
  integer ( kind = 8 ) number
  integer ( kind = 4 ) order

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POLYOMINO_ENUMERATE_CHIRAL_TEST:'
  write ( *, '(a)' ) '  POLYOMINO_ENUMERATE_CHIRAL returns values of '
  write ( *, '(a)' ) '  the number of chiral polyominoes of given order.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   ORDER         NUMBER'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call polyomino_enumerate_chiral ( n_data, order, number )

    if ( n_data == 0 ) then
      exit
    end if

    write ( *, '(2x,i8,2x,i24)' ) order, number

  end do

  return
end
subroutine polyomino_enumerate_fixed_test ( )

!*****************************************************************************80
!
!! POLYOMINO_ENUMERATE_FIXED_TEST tests POLYOMINO_ENUMERATE_FIXED.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 May 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n_data
  integer ( kind = 8 ) number
  integer ( kind = 4 ) order

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POLYOMINO_ENUMERATE_FIXED_TEST:'
  write ( *, '(a)' ) '  POLYOMINO_ENUMERATE_FIXED returns values of '
  write ( *, '(a)' ) '  the number of fixed polyominoes of given order.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   ORDER         NUMBER'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call polyomino_enumerate_fixed ( n_data, order, number )

    if ( n_data == 0 ) then
      exit
    end if

    write ( *, '(2x,i8,2x,i24)' ) order, number

  end do

  return
end
subroutine polyomino_enumerate_free_test ( )

!*****************************************************************************80
!
!! POLYOMINO_ENUMERATE_FREE_TEST tests POLYOMINO_ENUMERATE_FREE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 May 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n_data
  integer ( kind = 8 ) number
  integer ( kind = 4 ) order

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POLYOMINO_ENUMERATE_FREE_TEST:'
  write ( *, '(a)' ) '  POLYOMINO_ENUMERATE_FREE returns values of '
  write ( *, '(a)' ) '  the number of free polyominoes of given order.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   ORDER         NUMBER'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call polyomino_enumerate_free ( n_data, order, number )

    if ( n_data == 0 ) then
      exit
    end if

    write ( *, '(2x,i8,2x,i24)' ) order, number

  end do

  return
end
