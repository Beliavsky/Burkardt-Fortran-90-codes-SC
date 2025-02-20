program main

!*****************************************************************************80
!
!! MAIN is the main program for POLYOMINO_EMBED_TEST.
!
!  Discussion:
!
!    POLYOMINO_EMBED_TEST tests POLYOMINO_EMBED.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 May 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYOMINO_EMBED_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the POLYOMINO_EMBED library.'

  call polyomino_embed_number_test ( )
  call polyomino_embed_list_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYOMINO_EMBED_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine polyomino_embed_list_test ( )

!*****************************************************************************80
!
!! POLYOMINO_EMBED_LIST_TEST tests POLYOMINO_EMBED_LIST.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 May 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) embed_number
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ), allocatable :: list(:,:)
  integer ( kind = 4 ) mk
  integer ( kind = 4 ) nk
  integer ( kind = 4 ) :: mp = 3
  integer ( kind = 4 ) mq
  integer ( kind = 4 ) :: mr = 4
  integer ( kind = 4 ) :: np = 2
  integer ( kind = 4 ) nq
  integer ( kind = 4 ) :: nr = 4

  integer ( kind = 4 ) :: p(3,2) = reshape ( (/ &
    0, 0, 1, &
    1, 1, 1 /), (/ 3, 2 /) )
  integer ( kind = 4 ) q(4,4)
  integer ( kind = 4 ) :: r(4,4) = reshape ( (/ &
    0, 1, 1, 1, &
    1, 1, 1, 0, &
    1, 0, 1, 1, &
    1, 1, 1, 1 /), (/ 4, 4 /) )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYOMINO_EMBED_LIST_TEST:'
  write ( *, '(a)' ) '  POLYOMINO_EMBED_LIST lists the offsets used'
  write ( *, '(a)' ) '  to embed a fixed polyomino in a region.'

  call polyomino_print ( mr, nr, r, '  The given region R:' )

  call polyomino_print ( mp, np, p, '  The given polyomino P:' )
!
!  Get the number of embeddings.
!
  call polyomino_embed_number ( mr, nr, r, mp, np, p, embed_number )

  write ( *, '(a)' ) ''
  write ( *, '(a,i2,a)' ) '  As a fixed polyomino, P can be embedded in R in ', embed_number, ' ways.'
!
!  Get the list of embeddings.
!
  allocate ( list(1:embed_number,1:2) )

  call polyomino_embed_list ( mr, nr, r, mp, np, p, embed_number, list )

  do k = 1, embed_number
    mk = list(k,1)
    nk = list(k,2)
    mq = mr
    nq = nr
    q(1:mq,1:nq) = r(1:mr,1:nr)

    q(1+mk:mp+mk,1+nk:np+nk) = q(1+mk:mp+mk,1+nk:np+nk) + p(1:mp,1:np)

    write ( *, '(a)' ) ''
    write ( *, '(a,i2)' ) '  Embedding number ', k
    write ( *, '(a)' ) ''
    do i = 1, mq
      do j = 1, nq
        write ( *, '(1x,i1)', advance = 'no' ) q(i,j)
      end do
      write ( *, '(a)' ) ''
    end do
  end do

  deallocate ( list )

  return
end
subroutine polyomino_embed_number_test ( )

!*****************************************************************************80
!
!! POLYOMINO_EMBED_NUMBER_TEST tests POLYOMINO_EMBED_NUMBER.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 May 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) embed_number
  integer ( kind = 4 ) :: mp = 3
  integer ( kind = 4 ) :: mr = 4
  integer ( kind = 4 ) :: np = 2
  integer ( kind = 4 ) :: nr = 4
  integer ( kind = 4 ) :: p(3,2) = reshape ( (/ &
    0, 0, 1, &
    1, 1, 1 /), (/ 3, 2 /) )
  integer ( kind = 4 ) :: r(4,4) = reshape ( (/ &
    0, 1, 1, 1, &
    1, 1, 1, 0, &
    1, 0, 1, 1, &
    1, 1, 1, 1 /), (/ 4, 4 /) )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYOMINO_EMBED_NUMBER_TEST:'
  write ( *, '(a)' ) '  POLYOMINO_EMBED_NUMBER reports the number of ways a'
  write ( *, '(a)' ) '  fixed polyomino can be embedded in a region.'

  call polyomino_print ( mr, nr, r, '  The given region R:' )

  call polyomino_print ( mp, np, p, '  The given polyomino P:' )

  call polyomino_embed_number ( mr, nr, r, mp, np, p, embed_number )

  write ( *, '(a)' ) ''
  write ( *, '(a,i2,a)' ) '  As a fixed polyomino, P can be embedded in R in ', embed_number, ' ways.'

  return
end
