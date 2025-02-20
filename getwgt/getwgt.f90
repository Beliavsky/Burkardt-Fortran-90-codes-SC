subroutine getwgt ( aacnts, pseudocount )

!*****************************************************************************80
!
!! GETWGT updates the Dirichlet mixture weights based on a set of counts.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    25 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) AACNTS(ACID_NUM), the number of counts of 
!    each amino acid.  The implicit order used is: 
!
!      1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
!      A  C  D  E  F  G  H  I  K  L  M  N  P  Q  R  S  T  V  W  Y
!
!    Output, real ( kind = 8 ) PSEUDOCOUNT(ACID_NUM), the estimated 
!    pseudocount vector.
!
!  Internals:
!
!    Internal, integer ( kind = 4 ) ACID_NUM, the number of amino acids, 
!    which is 20.
!
!    Internal, integer ( kind = 4 ) COMP_NUM, the number of mixture components,
!    which is assumed to be 10.
!
!    Internal, real ( kind = 8 ) ALPHA(COMP_NUM), the estimated Dirichlet 
!    parameters for the mixture.
!
!    Internal, real ( kind = 8 ) BETA(ACID_NUM,COMP_NUM); BETA(I,J) is the 
!    parameter for the J-th acid in the I-th Dirichlet mixture component.
!
!    Internal, real ( kind = 8 ) COMP_WEIGHT_ESTIMATE(COMP_NUM); the estimated
!    value of the weights for the components of the mixture.
!
!    Internal, integer ( kind = 4 ) NCALL, the number of times this routine has
!    been called.
!
!    Internal, real ( kind = 8 ) P(COMP_NUM), P(I) is the Bayesian posterior 
!    probability of component I, given the observation of the most recent event,
!    which is proportional to the probability of the event under the 
!    component I PDF, times the prior probability of component I.
!
!    Internal, real ( kind = 8 ) P_HAT(COMP_NUM), the prior probabilities of the
!    components.
!
  implicit none

  integer ( kind = 4 ), parameter :: acid_num = 20
  integer ( kind = 4 ), parameter :: comp_max = 10

  integer ( kind = 4 ) aacnts(acid_num)
  integer ( kind = 4 ) acid_i
  character acid_sym(acid_num)
  real ( kind = 8 ), save, dimension ( comp_max ) :: alpha
  real ( kind = 8 ), save, dimension ( acid_num, comp_max ) :: beta
  real ( kind = 8 ) beta_sum(comp_max)
  integer ( kind = 4 ) comp_i
  integer ( kind = 4 ) comp_label(comp_max)
  integer ( kind = 4 ), save :: comp_num = 0
  real ( kind = 8 ), save, dimension ( comp_max ) :: comp_weight
  real ( kind = 8 ), save, dimension ( comp_max ) :: comp_weight_estimate
  character ( len = 255 ) file_name
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) input_status
  integer ( kind = 4 ) iunit
  integer ( kind = 4 ), save :: ncall = 0
  real ( kind = 8 ), save, dimension ( comp_max ) :: p
  real ( kind = 8 ), save, dimension ( comp_max ) :: p_hat
  real ( kind = 8 ) pseudocount(acid_num)
  integer ( kind = 4 ) site_num

  ncall = ncall + 1
!
!  On first call, we need to retrieve some information.
!
  if ( ncall == 1 ) then

    file_name = 'weights.txt'
    call get_unit ( iunit )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GETWGT:'
    write ( *, '(a)' ) '  Reading file "' // trim ( file_name ) // '".'

    open ( unit = iunit, file = file_name, status = 'old', form = 'formatted', &
      iostat = input_status )

    if ( input_status /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GETWGT - Fatal error!'
      write ( *, '(a)' ) '  Could not open the data file:'
      write ( *, '(a)' ) file_name
      stop
    end if

    call mixture_read ( acid_num, acid_sym, beta, beta_sum, comp_label, &
      comp_max, comp_num, comp_weight, ierror, iunit )

    close ( unit = iunit )

    if ( comp_max < comp_num ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GETWGT - Fatal error!'
      write ( *, '(a)' ) '  Number of components exceeds internal max.'
      stop
    end if

    call comp_param_print ( acid_num, acid_sym, comp_max, comp_num, beta, &
      beta_sum, comp_weight )

  end if
!
!  Initialize the ALPHA's.
!
!  Options:
!    Use the weights stored in the file as ALPHA's.
!    Use equal ALPHA's of 1 / comp_num.
!    Multiply these values to increase the importance of the initial values.
!
  alpha(1:comp_num) = comp_weight(1:comp_num)

  call r8vec_print ( comp_num, alpha, '  Initial Alphas:' )

  call r8vec_print ( comp_num, comp_weight, '  Initial Weights:' )
!
!  Initialize the prior probabilities.
!
  p(1:comp_num) = comp_weight(1:comp_num)
  p_hat(1:comp_num) = comp_weight(1:comp_num)

  site_num = sum ( aacnts(1:acid_num) )
!
!  Process the new information.
!
  call event_process ( acid_num, alpha, beta, comp_max, comp_num, p, p_hat, &
    site_num, aacnts )
!
!  From the new ALPHA's we update the estimated weights.
!
  call r8vec_print ( comp_num, alpha, '  New Alphas:' )

  call dirichlet_mean ( comp_num, alpha, comp_weight_estimate )

  call r8vec_print ( comp_num, comp_weight_estimate, '  New Weights:' )
!
!  From the updated weight estimates, compute the corresponding pseudo count.
!
  do acid_i = 1, acid_num
    pseudocount(acid_i) = 0.0D+00
    do comp_i = 1, comp_num
      pseudocount(acid_i) = pseudocount(acid_i) &
        + comp_weight_estimate(comp_i) * beta(acid_i,comp_i)
    end do
  end do

  return
end
subroutine ch_cap ( c )

!*****************************************************************************80
!
!! CH_CAP capitalizes a single character.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    19 July 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character C, the character to capitalize.
!
  implicit none

  character c
  integer ( kind = 4 ) itemp

  itemp = ichar ( c )

  if ( 97 <= itemp .and. itemp <= 122 ) then
    c = char ( itemp - 32 )
  end if

  return
end
function ch_eqi ( c1, c2 )

!*****************************************************************************80
!
!! CH_EQI is a case insensitive comparison of two characters for equality.
!
!  Example:
!
!    CH_EQI ( 'A', 'a' ) is .TRUE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    14 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character C1, C2, the characters to compare.
!
!    Output, logical CH_EQI, the result of the comparison.
!
  implicit none

  logical ch_eqi
  character c1
  character c2
  character cc1
  character cc2

  cc1 = c1
  cc2 = c2

  call ch_cap ( cc1 )
  call ch_cap ( cc2 )

  if ( cc1 == cc2 ) then
    ch_eqi = .true.
  else
    ch_eqi = .false.
  end if

  return
end
subroutine ch_next ( line, cval, done )

!*****************************************************************************80
!
!! CH_NEXT "reads" space-separated characters from a string, one at a time.
!
!  Example:
!
!    Input:
!
!      LINE = ' A  B, C    DE  F'
!
!    Output:
!
!      'A', 'B', 'C', 'D', 'E', 'F', and then blanks.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    18 November 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) LINE, a string, presumably containing
!    characters, possibly separated by spaces or commas.
!
!    Output, character CVAL.  If DONE is FALSE, then CVAL contains the
!    "next" character read from LINE.  If DONE is TRUE, then
!    CVAL is blank.
!
!    Input/output, logical DONE.
!    On input with a fresh value of LINE, the user should set
!    DONE to TRUE.
!    On output, the routine sets DONE to FALSE if another character
!    was read, or TRUE if no more characters could be read.
!
  implicit none

  character cval
  logical done
  integer ( kind = 4 ) i
  character ( len = * ) line
  integer ( kind = 4 ), save :: next = 1

  if ( done ) then
    next = 1
    done = .false.
  end if

  do i = next, len_trim ( line )

    if ( line(i:i) /= ' ' .and. line(i:i) /= ',' ) then
      cval = line(i:i)
      next = i + 1
      return
    end if

  end do

  done = .true.
  next = 1
  cval = ' '

  return
end
subroutine ch_to_digit ( c, digit )

!*****************************************************************************80
!
!! CH_TO_DIGIT returns the value of a base 10 digit.
!
!  Example:
!
!     C   DIGIT
!    ---  -----
!    '0'    0
!    '1'    1
!    ...  ...
!    '9'    9
!    ' '    0
!    'X'   -1
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    04 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character C, the decimal digit, '0' through '9' or blank
!    are legal.
!
!    Output, integer ( kind = 4 ) DIGIT, the corresponding value.  If C was
!    'illegal', then DIGIT is -1.
!
  implicit none

  character c
  integer ( kind = 4 ) digit

  if ( lge ( c, '0' ) .and. lle ( c, '9' ) ) then

    digit = ichar ( c ) - 48

  else if ( c == ' ' ) then

    digit = 0

  else

    digit = -1

  end if

  return
end
subroutine comp_param_print ( acid_num, acid_sym, comp_max, comp_num, beta, &
  beta_sum, comp_weight )

!*****************************************************************************80
!
!! COMP_PARAM_PRINT prints the parameters for the mixture components.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    24 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) ACID_NUM, the number of amino acids.
!
!    Input, character ACID_SYM(ACID_NUM), the one letter amino acid codes.
!
!    Input, integer ( kind = 4 ) COMP_MAX, the maximum number of Dirichlet 
!    mixture components.
!
!    Input, integer ( kind = 4 ) COMP_NUM, the number of components in the 
!    Dirichlet mixture.
!
!    Input, real ( kind = 8 ) BETA(ACID_NUM,COMP_MAX); BETA(I,J) is the 
!    parameter for the J-th acid in the I-th Dirichlet mixture component.
!
!    Input, real ( kind = 8 ) BETA_SUM(COMP_MAX), the sum of the values of
!    BETA(ACID_I,COMP_I) for a given component COMP_I.
!
!    Input, real ( kind = 8 ) COMP_WEIGHT(COMP_NUM), the mixture weight of each
!    component.  These values should be nonnegative, and sum to 1.  They 
!    represent the relative proportion of each component in the mixture.
!
  implicit none

  integer ( kind = 4 ) acid_num
  integer ( kind = 4 ) comp_max

  integer ( kind = 4 ) acid_i
  character acid_sym(acid_num)
  integer ( kind = 4 ) comp_i
  real ( kind = 8 ) beta(acid_num,comp_max)
  real ( kind = 8 ) beta_sum(comp_max)
  integer ( kind = 4 ) comp_num
  real ( kind = 8 ) comp_weight(comp_max)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'COMP_PARAM_PRINT:'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i6)' ) '  Number of components = ', comp_num
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) ' '
  write ( *, '(''Compon:'',20i8)' ) ( comp_i, comp_i = 1, comp_num )
  write ( *, '(''Weight:'',20f8.4)' ) comp_weight(1:comp_num)
  write ( *, '(a)' ) ' '

  do acid_i = 1, acid_num
    write ( *, '(i2,2x,a1,2x,20f8.4)' ) acid_i, acid_sym(acid_i), &
      beta(acid_i,1:comp_num)
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a3,4x,20f8.4)' ) 'Sum', beta_sum(1:comp_num)

  return
end
subroutine dirichlet_mean ( n, a, mean )

!*****************************************************************************80
!
!! DIRICHLET_MEAN returns the means of the Dirichlet PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    23 November 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components.
!
!    Input, real ( kind = 8 ) A(N), the probabilities for each component.
!    Each A(I) should be nonnegative, and at least one should be positive.
!
!    Output, real ( kind = 8 ) MEAN(N), the means of the PDF.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) mean(n)

  mean(1:n) = a(1:n)

  call r8vec_unit_sum ( n, mean )

  return
end
subroutine dirichlet_multinomial_pdf ( x, a, b, c, pdf )

!*****************************************************************************80
!
!! DIRICHLET_MULTINOMIAL_PDF evaluates a Dirichlet Multinomial PDF.
!
!  Discussion:
!
!    PDF(X)(A,B,C) = Comb(A,B,X) * ( Gamma(C_Sum) / Gamma(C_Sum+A) )
!      Product ( 1 <= I <= B ) Gamma(C(I)+X(I)) / Gamma(C(I))
!
!    where:
!
!      Comb(A,B,X) is the multinomial coefficient C( A; X(1), X(2), ..., X(B) ),
!      C_Sum = Sum ( 1 <= I <= B ) C(I)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    17 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Kenneth Lange,
!    Mathematical and Statistical Methods for Genetic Analysis,
!    Springer, 1997, page 45.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X(B); X(I) counts the number of occurrences of
!    outcome I, out of the total of A trials.
!
!    Input, integer ( kind = 4 ) A, the total number of trials.
!
!    Input, integer ( kind = 4 ) B, the number of different possible outcomes on
!    one trial.
!
!    Input, integer ( kind = 4 ) C(B); C(I) is the Dirichlet parameter 
!    associated with outcome I.
!
!    Output, real ( kind = 8 ) PDF, the value of the Dirichlet multinomial PDF.
!
  implicit none

  integer ( kind = 4 ) b

  integer ( kind = 4 ) a
  real ( kind = 8 ) c(b)
  real ( kind = 8 ) c_sum
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  real ( kind = 8 ) pdf_log
  real ( kind = 8 ) r8_gamma_log
  integer ( kind = 4 ) x(b)

  c_sum = sum ( c(1:b) )

  pdf_log = - r8_gamma_log ( c_sum + real ( a, kind = 8 ) ) &
            + r8_gamma_log ( c_sum ) &
            + r8_gamma_log ( real ( a + 1, kind = 8 ) )

  do i = 1, b

    pdf_log = pdf_log &
      + r8_gamma_log ( c(i) + real ( x(i), kind = 8 ) ) &
      - r8_gamma_log ( c(i) ) &
      - r8_gamma_log ( real ( x(i) + 1, kind = 8 ) )
  end do

  pdf = exp ( pdf_log )

  return
end
subroutine event_process ( acid_num, alpha, beta, comp_max, comp_num, p, &
  p_hat, site_num, x_sample )

!*****************************************************************************80
!
!! EVENT_PROCESS updates the mixture weight distribution parameters.
!
!  Discussion:
!
!    This routine updates the values of ALPHA.  It does this by
!    considering the results of the most recent event.  If we knew
!    which component PDF had generated the event, then we would 
!    simply add 1 to the ALPHA for that component.  Instead, we
!    use Bayesian analysis to estimate the proportion of the event
!    that is to be added to each ALPHA.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    18 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    BS Everitt, DJ Hand,
!    Finite Mixture Distributions,
!    Chapman and Hall, 1981.
!
!    AFM Smith, UE Makov,
!    A Quasi-Bayes Sequential Procedure for Mixtures,
!    Journal of the Royal Statistical Society,
!    Volume 40, Number 1, B, 1978, pages 106-112.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) ACID_NUM, the number of amino acids.
!
!    Input/output, real ( kind = 8 ) ALPHA(COMP_NUM), the Dirichlet parameters 
!    for the weights.
!
!    Input, real ( kind = 8 ) BETA(ACID_NUM,COMP_MAX); BETA(I,J) is the 
!    multinomial Dirichlet parameter for the J-th acid in the I-th Dirichlet 
!    mixture component.
!
!    Input, integer ( kind = 4 ) COMP_NUM, the number of components in the 
!    Dirichlet mixture.
!
!    Input/output, real ( kind = 8 ) P(COMP_NUM); P(I) is the Bayesian posterior
!    probability of component I, given the observation of the most recent event,
!    which is proportional to the probability of the event under the 
!    component I PDF, times the prior probability of component I.
!
!    Input/output, real ( kind = 8 ) P_HAT(COMP_NUM), the prior probabilities 
!    of the components.
!
!    Input, integer ( kind = 4 ) SITE_NUM, the number of sites observed for this
!    event.  This value might change from call to call, although in the 
!    demonstration I'm keeping it fixed.
!
!    Input, integer ( kind = 4 ) X_SAMPLE(ACID_NUM), the "current event", 
!    namely, the count vector for the number of occurrences of each acid out of
!    the total of SITE_NUM sites analyzed.  This is the evidence used to update
!    the "theory" for the value of ALPHA.
!
  implicit none

  integer ( kind = 4 ) acid_num
  integer ( kind = 4 ) comp_max

  real ( kind = 8 ) alpha(comp_max)
  real ( kind = 8 ) alpha_sum
  real ( kind = 8 ) beta(acid_num,comp_max)
  integer ( kind = 4 ) comp_i
  integer ( kind = 4 ) comp_num
  real ( kind = 8 ) comp_pdf
  real ( kind = 8 ) comp_sum
  real ( kind = 8 ) p(comp_max)
  real ( kind = 8 ) p_hat(comp_max)
  integer ( kind = 4 ) site_num
  real ( kind = 8 ) sum
  integer ( kind = 4 ) x_sample(acid_num)
!
!  Sum the parameters.
!
  alpha_sum = sum ( alpha(1:comp_num) )
!
!  Update P_HAT.
!
  p_hat(1:comp_num) = ( ( alpha_sum - 1.0D+00 ) * p_hat(1:comp_num) &
    + p(1:comp_num) ) / alpha_sum
!
!  Generate the new P's.
!  P(COMP_I) = the Bayesian posterior probability of component I,
!  given the observation of event EVENT_I, which is proportional
!  to the probability of event EVENT_I in the component I PDF,
!  times the prior probability of component I.
!
  do comp_i = 1, comp_num
!
!  Compute the probability of this event, for a given component.
!
    call dirichlet_multinomial_pdf ( x_sample, site_num, acid_num, &
      beta(1,comp_i), comp_pdf )
!
!  Multiply by the probability of that component to get the relative
!  probability of the event.
!
    p(comp_i) = comp_pdf * p_hat(comp_i)

  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  BAYES:'
  write ( *, '(a)' ) &
    '   I    PDF           P_Hat         P             P Normalized'
  write ( *, '(a)' ) ' '

  comp_sum = 0.0D+00
  do comp_i = 1, comp_num
    comp_sum = comp_sum + p(comp_i)
  end do

  do comp_i = 1, comp_num
    call dirichlet_multinomial_pdf ( x_sample, site_num, acid_num, &
      beta(1,comp_i), comp_pdf )
    write ( *, '(i4,4g14.6)' ) comp_i, comp_pdf, p_hat(comp_i), p(comp_i), &
      p(comp_i) / comp_sum
  end do
!
!  Normalize the P's to get the absolute Bayesian probability.
!
  call r8vec_unit_sum ( comp_num, p )
!
!  Update the alpha's by adding adding appropriate portions of
!  the most recent event to each component's parameter.
!
  alpha(1:comp_num) = alpha(1:comp_num) + p(1:comp_num)

  return
end
subroutine get_unit ( iunit )

!*****************************************************************************80
!
!! GET_UNIT returns a free FORTRAN unit number.
!
!  Discussion:
!
!    A "free" FORTRAN unit number is a value between 1 and 99 which
!    is not currently associated with an I/O device.  A free FORTRAN unit
!    number is needed in order to open a file with the OPEN command.
!
!    If IUNIT = 0, then no free FORTRAN unit could be found, although
!    all 99 units were checked (except for units 5, 6 and 9, which
!    are commonly reserved for console I/O).
!
!    Otherwise, IUNIT is a value between 1 and 99, representing a
!    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
!    are special, and will never return those values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    26 October 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) IUNIT, the free unit number.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ios
  integer ( kind = 4 ) iunit
  logical lopen
 
  iunit = 0
 
  do i = 1, 99
 
    if ( i /= 5 .and. i /= 6 .and. i /= 9 ) then

      inquire ( unit = i, opened = lopen, iostat = ios )
 
      if ( ios == 0 ) then
        if ( .not. lopen ) then
          iunit = i
          return
        end if
      end if

    end if
 
  end do

  return
end
subroutine i4_next ( line, ival, done )

!*****************************************************************************80
!
!! I4_NEXT "reads" integers from a string, one at a time.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    14 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) LINE, a string, presumably containing
!    integers.  These may be separated by spaces or commas.
!
!    Output, integer ( kind = 4 ) IVAL.  If DONE is FALSE, then IVAL contains
!    the "next" integer read from LINE.  If DONE is TRUE, then
!    IVAL is zero.
!
!    Input/output, logical DONE.
!    On input with a fresh value of LINE, the user should set
!    DONE to TRUE.
!    On output, the routine sets DONE to FALSE if another integer
!    was read, or TRUE if no more integers could be read.
!
  implicit none

  logical done
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) ival
  integer ( kind = 4 ) lchar
  character ( len = * ) line
  integer ( kind = 4 ), save :: next = 1

  ival = 0

  if ( done ) then
    next = 1
    done = .false.
  end if

  if ( len_trim ( line ) < next ) then
    done = .true.
    return
  end if

  call s_to_i4 ( line(next:), ival, ierror, lchar )

  if ( ierror /= 0 .or. lchar == 0 ) then
    done = .true.
    next = 1
  else
    done = .false.
    next = next + lchar
  end if

  return
end
subroutine mixture_read ( acid_num, acid_sym, beta, beta_sum, comp_label, &
  comp_max, comp_num, comp_weight, ierror, iunit )

!*****************************************************************************80
!
!! MIXTURE_READ reads the Dirichlet mixture parameters from a file.
!
!  Discussion:
!
!    The data in the file is delimited by keywords.
!
!    The first lines (not necessarily in order!) may include
!
!      ClassName = string
!      NumDistr = N           the number of components in the mixture.
!      Alphabet = string
!      Order = A C D E ...    the order of the amino acids.
!      AlphaChar = 20
!      NumDistr = 9           the number of distributions
!      EndClassName = string
!
!    For each component, there are four lines:
!
!      Number= N              the component number, starting with 0
!      Mixture= N             the mixture weight, out of a total of 1.0
!      Alpha=  |A| A1 A2 ...  the parameter sum, and individual parameters
!      Comment=               a comment, which describes the frequencies.
!
!    In the comment, the symbol "><" indicates the mean background frequency;
!    residues to the left of that symbol occur more frequently
!    than background, residues to the right less frequently.  Commas separate 
!    residues differing in frequency by a factor of 2.
!
!    For example, the comment
!      S A T , C G P >< N V M , Q H R I K F L D W , E Y
!    indicates that for this component, the frequency of
!    proline is just above the mean, and serine, alanine and
!    threonine are twice as frequent in this component than they
!    are on average.  By contrast, tyrosine and glutamic acid are
!    between 4 and 8 times less likely in this component than on
!    average.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) ACID_NUM, the number of amino acids.
!
!    Output, character ACID_SYM(ACID_NUM), the one letter amino acid codes.
!
!    Output, real ( kind = 8 ) BETA(ACID_NUM,COMP_MAX); BETA(I,J) is the 
!    parameter for the J-th acid in the I-th Dirichlet mixture component.
!
!    Output, real ( kind = 8 ) BETA_SUM(COMP_MAX), the sum of the values of
!    BETA(ACID_I,COMP_I) for a given component COMP_I.
!
!    Output, integer ( kind = 4 ) COMP_LABEL(COMP_NUM), the label of each 
!    component.  Normally, component I has label I.
!
!    Input, integer ( kind = 4 ) COMP_MAX, the maximum number of Dirichlet 
!    mixture components.
!
!    Output, integer ( kind = 4 ) COMP_NUM, the number of components in the 
!    Dirichlet mixture.
!
!    Output, real ( kind = 8 ) COMP_WEIGHT(COMP_NUM), the mixture weight of 
!    each component.  These values should be nonnegative, and sum to 1.  They 
!    represent the relative proportion of each component in the mixture.
!
!    Output, integer ( kind = 4 ) IERROR, error indicator.
!    0: no error occurred; nonzero: an error occurred.
!
!    Input, integer ( kind = 4 ) IUNIT, the FORTRAN unit from which the data 
!    is to be read.
!
  implicit none

  integer ( kind = 4 ) acid_num
  integer ( kind = 4 ) comp_max

  integer ( kind = 4 ) acid_i
  character acid_sym(acid_num)
  real ( kind = 8 ) beta(acid_num,comp_max)
  real ( kind = 8 ) beta_sum(comp_max)
  integer ( kind = 4 ) comp_i
  integer ( kind = 4 ) comp_label(comp_max)
  integer ( kind = 4 ) comp_num
  real ( kind = 8 ) comp_weight(comp_max)
  logical done
  integer ( kind = 4 ) iequal
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) input_status
  integer ( kind = 4 ) iunit
  integer ( kind = 4 ) ngoofy
  integer ( kind = 4 ) nrec
  logical s_begin
  character ( len = 500 ) string

  ierror = 0
  comp_i = 0
  comp_num = 0
  nrec = 0
  ngoofy = 0

  do

    read ( iunit, '(a)', iostat = input_status ) string

    if ( input_status /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'MIXTURE_READ:'
      write ( *, '(a)' ) '  End of input.'
      exit
    end if

    nrec = nrec + 1
!
!  Ignore blank lines.
!
    if ( string == ' ' ) then
!
!  Ignore the CLASSNAME field.
!
    else if ( s_begin ( string, 'CLASSNAME' ) ) then
!
!  Ignore the ENDCLASSNAME field.
!
   else if ( s_begin ( string, 'ENDCLASSNAME' ) ) then
!
!  Ignore the NAME field.
!
    else if ( s_begin ( string, 'NAME' ) ) then
!
!  Ignore the ALPHABET field.
!
    else if ( s_begin ( string, 'ALPHABET' ) ) then
!
!  Read the ORDER field, since it tells us how to interpret the ALPHA's.
!
    else if ( s_begin ( string, 'ORDER' ) ) then

      iequal = index ( string, '=' )
      done = .true.
      do acid_i = 1, acid_num
        call ch_next ( string(iequal+1:), acid_sym(acid_i), done )
      end do
!
!  Ignore the ALPHACHAR field.
!
    else if ( s_begin ( string, 'ALPHACHAR' ) ) then
!
!  Read the NUMDISTR field.
!
    else if ( s_begin ( string, 'NUMDISTR' ) ) then

      iequal = index ( string, '=' )
      done = .true.
      call i4_next ( string(iequal+1:), comp_num, done )

      if ( comp_num < 1 ) then
        ierror = 1
        return
      else if ( comp_max < comp_num ) then
        ierror = 2
        return
      end if
!
!  Read the NUMBER field.
!
    else if ( s_begin ( string, 'NUMBER' ) ) then

      comp_i = comp_i + 1

      if ( comp_num < comp_i ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MIXTURE_READ - Fatal error!'
        write ( *, '(a,i6)' ) '  Number of components = ', comp_i
        write ( *, '(a,i6)' ) '  exceeding reported value of ', comp_num
        stop
      end if

      iequal = index ( string, '=' )
      done = .true.
      call i4_next ( string(iequal+1:), comp_label(comp_i), done )
!
!  Read the MIXTURE field.
!
    else if ( s_begin ( string, 'MIXTURE' ) ) then

      iequal = index ( string, '=' )
      done = .true.
      call r8_next ( string(iequal+1:), comp_weight(comp_i), done )
!
!  Read the ALPHA field.
!
    else if ( s_begin ( string, 'ALPHA' ) ) then

      iequal = index ( string, '=' )
      done = .true.
      call r8_next ( string(iequal+1:), beta_sum(comp_i), done )
      do acid_i = 1, acid_num
        call r8_next ( string(iequal+1:), beta(acid_i,comp_i), done )
      end do
!
!  Ignore the COMMENT field.
!
    else if ( s_begin ( string, 'COMMENT' ) ) then
!
!  Unexpected field:
!
    else

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'MIXTURE_READ - Warning!'
      write ( *, '(a)' ) '  Goofy record: '
      write ( *, '(a)' ) string(1:20)

      ngoofy = ngoofy + 1

    end if

  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'MIXTURE_READ - Note:'
  write ( *, '(a,i6)' ) '  Number of records read was ', nrec
  write ( *, '(a,i6)' ) '  Number of goofy records was ', ngoofy

  return
end
function r8_gamma_log ( x )

!*****************************************************************************80
!
!! R8_GAMMA_LOG evaluates the logarithm of the gamma function.
!
!  Discussion:
!
!    This routine calculates the LOG(GAMMA) function for a positive real
!    argument X.  Computation is based on an algorithm outlined in
!    references 1 and 2.  The program uses rational functions that
!    theoretically approximate LOG(GAMMA) to at least 18 significant
!    decimal digits.  The approximation for X > 12 is from reference
!    3, while approximations for X < 12.0 are similar to those in
!    reference 1, but are unpublished.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 April 2013
!
!  Author:
!
!    Original FORTRAN77 version by William Cody, Laura Stoltz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    William Cody, Kenneth Hillstrom,
!    Chebyshev Approximations for the Natural Logarithm of the
!    Gamma Function,
!    Mathematics of Computation,
!    Volume 21, Number 98, April 1967, pages 198-203.
!
!    Kenneth Hillstrom,
!    ANL/AMD Program ANLC366S, DGAMMA/DLGAMA,
!    May 1969.
!
!    John Hart, Ward Cheney, Charles Lawson, Hans Maehly,
!    Charles Mesztenyi, John Rice, Henry Thatcher,
!    Christoph Witzgall,
!    Computer Approximations,
!    Wiley, 1968,
!    LC: QA297.C64.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) R8_GAMMA_LOG, the value of the function.
!
  implicit none

  real ( kind = 8 ), dimension ( 7 ) :: c = (/ &
    -1.910444077728D-03, &
     8.4171387781295D-04, &
    -5.952379913043012D-04, &
     7.93650793500350248D-04, &
    -2.777777777777681622553D-03, &
     8.333333333333333331554247D-02, &
     5.7083835261D-03 /)
  real ( kind = 8 ) corr
  real ( kind = 8 ) :: d1 = -5.772156649015328605195174D-01
  real ( kind = 8 ) :: d2 = 4.227843350984671393993777D-01
  real ( kind = 8 ) :: d4 = 1.791759469228055000094023D+00
  real ( kind = 8 ), parameter :: frtbig = 2.25D+76
  integer ( kind = 4 ) i
  real ( kind = 8 ), dimension ( 8 ) :: p1 = (/ &
    4.945235359296727046734888D+00, &
    2.018112620856775083915565D+02, &
    2.290838373831346393026739D+03, &
    1.131967205903380828685045D+04, &
    2.855724635671635335736389D+04, &
    3.848496228443793359990269D+04, &
    2.637748787624195437963534D+04, &
    7.225813979700288197698961D+03 /)
  real ( kind = 8 ), dimension ( 8 ) :: p2 = (/ &
    4.974607845568932035012064D+00, &
    5.424138599891070494101986D+02, &
    1.550693864978364947665077D+04, &
    1.847932904445632425417223D+05, &
    1.088204769468828767498470D+06, &
    3.338152967987029735917223D+06, &
    5.106661678927352456275255D+06, &
    3.074109054850539556250927D+06 /)
  real ( kind = 8 ), dimension ( 8 ) :: p4 = (/ &
    1.474502166059939948905062D+04, &
    2.426813369486704502836312D+06, &
    1.214755574045093227939592D+08, &
    2.663432449630976949898078D+09, &
    2.940378956634553899906876D+10, &
    1.702665737765398868392998D+11, &
    4.926125793377430887588120D+11, &
    5.606251856223951465078242D+11 /)
  real ( kind = 8 ), dimension ( 8 ) :: q1 = (/ &
    6.748212550303777196073036D+01, &
    1.113332393857199323513008D+03, &
    7.738757056935398733233834D+03, &
    2.763987074403340708898585D+04, &
    5.499310206226157329794414D+04, &
    6.161122180066002127833352D+04, &
    3.635127591501940507276287D+04, &
    8.785536302431013170870835D+03 /)
  real ( kind = 8 ), dimension ( 8 ) :: q2 = (/ &
    1.830328399370592604055942D+02, &
    7.765049321445005871323047D+03, &
    1.331903827966074194402448D+05, &
    1.136705821321969608938755D+06, &
    5.267964117437946917577538D+06, &
    1.346701454311101692290052D+07, &
    1.782736530353274213975932D+07, &
    9.533095591844353613395747D+06 /)
  real ( kind = 8 ), dimension ( 8 ) :: q4 = (/ &
    2.690530175870899333379843D+03, &
    6.393885654300092398984238D+05, &
    4.135599930241388052042842D+07, &
    1.120872109616147941376570D+09, &
    1.488613728678813811542398D+10, &
    1.016803586272438228077304D+11, &
    3.417476345507377132798597D+11, &
    4.463158187419713286462081D+11 /)
  real ( kind = 8 ) r8_gamma_log
  real ( kind = 8 ) res
  real ( kind = 8 ), parameter :: sqrtpi = 0.9189385332046727417803297D+00
  real ( kind = 8 ) x
  real ( kind = 8 ), parameter :: xbig = 2.55D+305
  real ( kind = 8 ) xden
  real ( kind = 8 ), parameter :: xinf = 1.79D+308
  real ( kind = 8 ) xm1
  real ( kind = 8 ) xm2
  real ( kind = 8 ) xm4
  real ( kind = 8 ) xnum
  real ( kind = 8 ) y
  real ( kind = 8 ) ysq

  y = x

  if ( 0.0D+00 < y .and. y <= xbig ) then

    if ( y <= epsilon ( y ) ) then

      res = - log ( y )
!
!  EPS < X <= 1.5.
!
    else if ( y <= 1.5D+00 ) then

      if ( y < 0.6796875D+00 ) then
        corr = -log ( y )
        xm1 = y
      else
        corr = 0.0D+00
        xm1 = ( y - 0.5D+00 ) - 0.5D+00
      end if

      if ( y <= 0.5D+00 .or. 0.6796875D+00 <= y ) then

        xden = 1.0D+00
        xnum = 0.0D+00
        do i = 1, 8
          xnum = xnum * xm1 + p1(i)
          xden = xden * xm1 + q1(i)
        end do

        res = corr + ( xm1 * ( d1 + xm1 * ( xnum / xden ) ) )

      else

        xm2 = ( y - 0.5D+00 ) - 0.5D+00
        xden = 1.0D+00
        xnum = 0.0D+00
        do i = 1, 8
          xnum = xnum * xm2 + p2(i)
          xden = xden * xm2 + q2(i)
        end do

        res = corr + xm2 * ( d2 + xm2 * ( xnum / xden ) )

      end if
!
!  1.5 < X <= 4.0.
!
    else if ( y <= 4.0D+00 ) then

      xm2 = y - 2.0D+00
      xden = 1.0D+00
      xnum = 0.0D+00
      do i = 1, 8
        xnum = xnum * xm2 + p2(i)
        xden = xden * xm2 + q2(i)
      end do

      res = xm2 * ( d2 + xm2 * ( xnum / xden ) )
!
!  4.0 < X <= 12.0.
!
    else if ( y <= 12.0D+00 ) then

      xm4 = y - 4.0D+00
      xden = -1.0D+00
      xnum = 0.0D+00
      do i = 1, 8
        xnum = xnum * xm4 + p4(i)
        xden = xden * xm4 + q4(i)
      end do

      res = d4 + xm4 * ( xnum / xden )
!
!  Evaluate for 12 <= argument.
!
    else

      res = 0.0D+00

      if ( y <= frtbig ) then

        res = c(7)
        ysq = y * y

        do i = 1, 6
          res = res / ysq + c(i)
        end do

      end if

      res = res / y
      corr = log ( y )
      res = res + sqrtpi - 0.5D+00 * corr
      res = res + y * ( corr - 1.0D+00 )

    end if
!
!  Return for bad arguments.
!
  else

    res = xinf

  end if
!
!  Final adjustments and return.
!
  r8_gamma_log = res

  return
end
subroutine r8_next ( line, rval, done )

!*****************************************************************************80
!
!! R8_NEXT "reads" real numbers from a string, one at a time.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    14 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) LINE, a string, presumably containing real
!    numbers.  These may be separated by spaces or commas.
!
!    Output, real ( kind = 8 ) RVAL.  If DONE is FALSE, then RVAL contains the
!    "next" real value read from LINE.  If DONE is TRUE, then
!    RVAL is zero.
!
!    Input/output, logical DONE.
!    On input with a fresh value of LINE, the user should set
!    DONE to TRUE.
!    On output, the routine sets DONE to FALSE if another real
!    value was read, or TRUE if no more reals could be read.
!
  implicit none

  logical done
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) lchar
  character ( len = * ) line
  integer ( kind = 4 ), save :: next = 1
  real ( kind = 8 ) rval

  rval = 0.0D+00

  if ( done ) then
    next = 1
    done = .false.
  end if

  if ( len_trim ( line ) < next ) then
    done = .true.
    return
  end if

  call s_to_r8_old ( line(next:), rval, ierror, lchar )

  if ( ierror /= 0 .or. lchar == 0 ) then
    done = .true.
    next = 1
  else
    done = .false.
    next = next + lchar
  end if

  return
end
subroutine r8vec_print ( n, a, title )

!*****************************************************************************80
!
!! R8VEC_PRINT prints an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
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
!    Input, integer ( kind = 4 ) N, the number of components of the vector.
!
!    Input, real ( kind = 8 ) A(N), the vector to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '

  do i = 1, n
    write ( *, '(2x,i8,a,1x,g16.8)' ) i, ':', a(i)
  end do

  return
end
subroutine r8vec_unit_sum ( n, a )

!*****************************************************************************80
!
!! R8VEC_UNIT_SUM normalizes an R8VEC to have unit sum.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    17 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input/output, real ( kind = 8 ) A(N), the vector to be normalized.  On 
!    output, the entries of A should have unit sum.  However, if the input 
!    vector has zero sum, the routine halts.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)

  a(1:n) = a(1:n) / sum ( a(1:n) )

  return
end
function s_begin ( s1, s2 )

!*****************************************************************************80
!
!! S_BEGIN is TRUE if one string matches the beginning of the other.
!
!  Discussion:
!
!    The strings are compared, ignoring blanks, spaces and capitalization.
!
!  Example:
!
!     S1              S2      S_BEGIN
!
!    'Bob'          'BOB'     TRUE
!    '  B  o b '    ' bo b'   TRUE
!    'Bob'          'Bobby'   TRUE
!    'Bobo'         'Bobb'    FALSE
!    ' '            'Bob'     FALSE    (Do not allow a blank to match
!                                       anything but another blank string.)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S1, S2, the strings to be compared.
!
!    Output, logical S_BEGIN, is TRUE if the strings match up to
!    the end of the shorter string, ignoring case.
!
  implicit none

  logical ch_eqi
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  logical s_begin
  character ( len = * ) s1
  integer ( kind = 4 ) s1_length
  character ( len = * ) s2
  integer ( kind = 4 ) s2_length

  s1_length = len_trim ( s1 )
  s2_length = len_trim ( s2 )
!
!  If either string is blank, then both must be blank to match.
!  Otherwise, a blank string matches anything, which is not
!  what most people want.
!
  if ( s1_length == 0 .or. s2_length == 0 ) then

    if ( s1_length == 0 .and. s2_length == 0 ) then
      s_begin = .true.
    else
      s_begin = .false.
    end if

    return

  end if

  i1 = 0
  i2 = 0
!
!  Find the next nonblank in S1.
!
  do

    do

      i1 = i1 + 1

      if ( s1_length < i1 ) then
        s_begin = .true.
        return
      end if

      if ( s1(i1:i1) /= ' ' ) then
        exit
      end if

    end do
!
!  Find the next nonblank in S2.
!
    do

      i2 = i2 + 1

      if ( s2_length < i2 ) then
        s_begin = .true.
        return
      end if

      if ( s2(i2:i2) /= ' ' ) then
        exit
      end if

    end do
!
!  If the characters match, get the next pair.
!
    if ( .not. ch_eqi ( s1(i1:i1), s2(i2:i2) ) ) then
      exit
    end if

  end do

  s_begin = .false.

  return
end
subroutine s_to_i4 ( s, value, ierror, length )

!*****************************************************************************80
!
!! S_TO_I4 reads an I4 from a string.
!
!  Discussion:
!
!    Instead of ICHAR, we now use the IACHAR function, which
!    guarantees the ASCII collating sequence.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 January 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, a string to be examined.
!
!    Output, integer ( kind = 4 ) VALUE, the value read from the string.
!    If the string is blank, then VALUE will be returned 0.
!
!    Output, integer ( kind = 4 ) IERROR, an error flag.
!    0, no error.
!    1, an error occurred.
!
!    Output, integer ( kind = 4 ) LENGTH, the number of characters
!    of S used to make the integer.
!
  implicit none

  character c
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) isgn
  integer ( kind = 4 ) length
  character ( len = * ) s
  integer ( kind = 4 ) state
  character :: TAB = achar ( 9 )
  integer ( kind = 4 ) value

  value = 0
  ierror = 0
  length = 0

  state = 0
  isgn = 1

  do i = 1, len_trim ( s )

    c = s(i:i)
!
!  STATE = 0, haven't read anything.
!
    if ( state == 0 ) then

      if ( c == ' ' .or. c == TAB ) then

      else if ( c == '-' ) then
        state = 1
        isgn = -1
      else if ( c == '+' ) then
        state = 1
        isgn = +1
      else if ( lle ( '0', c ) .and. lle ( c, '9' ) ) then
        state = 2
        value = iachar ( c ) - iachar ( '0' )
      else
        ierror = 1
        return
      end if
!
!  STATE = 1, have read the sign, expecting digits or spaces.
!
    else if ( state == 1 ) then

      if ( c == ' ' .or. c == TAB ) then

      else if ( lle ( '0', c ) .and. lle ( c, '9' ) ) then
        state = 2
        value = iachar ( c ) - iachar ( '0' )
      else
        ierror = 1
        return
      end if
!
!  STATE = 2, have read at least one digit, expecting more.
!
    else if ( state == 2 ) then

      if ( lle ( '0', c ) .and. lle ( c, '9' ) ) then

        value = 10 * value + iachar ( c ) - iachar ( '0' )

      else

        value = isgn * value
        ierror = 0
        length = i - 1
        return

      end if

    end if

  end do
!
!  If we read all the characters in the string, see if we're OK.
!
  if ( state == 2 ) then

    value = isgn * value
    ierror = 0
    length = len_trim ( s )

  else

    value = 0
    ierror = 1
    length = 0

  end if

  return
end
subroutine s_to_r8_old ( s, dval, ierror, length )

!*****************************************************************************80
!
!! S_TO_R8_OLD reads an R8 value from a string.
!
!  Discussion:
!
!    An "R8" value is simply a real number to be stored as a
!    variable of type "real ( kind = 8 )".
!
!    The routine will read as many characters as possible until it reaches
!    the end of the string, or encounters a character which cannot be
!    part of the number.
!
!    Legal input is:
!
!       1 blanks,
!       2 '+' or '-' sign,
!       2.5 blanks
!       3 integer part,
!       4 decimal point,
!       5 fraction part,
!       6 'E' or 'e' or 'D' or 'd', exponent marker,
!       7 exponent sign,
!       8 exponent integer part,
!       9 exponent decimal point,
!      10 exponent fraction part,
!      11 blanks,
!      12 final comma or semicolon,
!
!    with most quantities optional.
!
!  Example:
!
!    S                 DVAL
!
!    '1'               1.0
!    '     1   '       1.0
!    '1A'              1.0
!    '12,34,56'        12.0
!    '  34 7'          34.0
!    '-1E2ABCD'        -100.0
!    '-1X2ABCD'        -1.0
!    ' 2E-1'           0.2
!    '23.45'           23.45
!    '-4.2E+2'         -420.0
!    '17d2'            1700.0
!    '-14e-2'         -0.14
!    'e2'              100.0
!    '-12.73e-9.23'   -12.73 * 10.0^(-9.23)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 January 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string containing the
!    data to be read.  Reading will begin at position 1 and
!    terminate at the end of the string, or when no more
!    characters can be read to form a legal real.  Blanks,
!    commas, or other nonnumeric data will, in particular,
!    cause the conversion to halt.
!
!    Output, real ( kind = 8 ) DVAL, the value read from the string.
!
!    Output, integer ( kind = 4 ) IERROR, error flag.
!    0, no errors occurred.
!    1, 2, 6 or 7, the input number was garbled.  The
!    value of IERROR is the last type of input successfully
!    read.  For instance, 1 means initial blanks, 2 means
!    a plus or minus sign, and so on.
!
!    Output, integer ( kind = 4 ) LENGTH, the number of characters read
!    to form the number, including any terminating
!    characters such as a trailing comma or blanks.
!
  implicit none

  character c
  logical ch_eqi
  real ( kind = 8 ) dval
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) ihave
  integer ( kind = 4 ) isgn
  integer ( kind = 4 ) iterm
  integer ( kind = 4 ) jbot
  integer ( kind = 4 ) jsgn
  integer ( kind = 4 ) jtop
  integer ( kind = 4 ) length
  integer ( kind = 4 ) ndig
  real ( kind = 8 ) rbot
  real ( kind = 8 ) rexp
  real ( kind = 8 ) rtop
  character ( len = * ) s
  integer ( kind = 4 ) s_length
  character  :: TAB = achar ( 9 )

  s_length = len_trim ( s )

  ierror = 0
  dval = 0.0D+00
  length = -1
  isgn = 1
  rtop = 0
  rbot = 1
  jsgn = 1
  jtop = 0
  jbot = 1
  ihave = 1
  iterm = 0

  do

    length = length + 1

    if ( s_length < length + 1 ) then
      exit
    end if

    c = s(length+1:length+1)
!
!  Blank character.
!
    if ( c == ' ' .or. c == TAB ) then

      if ( ihave == 2 ) then

      else if ( ihave == 6 .or. ihave == 7 ) then
        iterm = 1
      else if ( 1 < ihave ) then
        ihave = 11
      end if
!
!  Comma.
!
    else if ( c == ',' .or. c == ';' ) then

      if ( ihave /= 1 ) then
        iterm = 1
        ihave = 12
        length = length + 1
      end if
!
!  Minus sign.
!
    else if ( c == '-' ) then

      if ( ihave == 1 ) then
        ihave = 2
        isgn = -1
      else if ( ihave == 6 ) then
        ihave = 7
        jsgn = -1
      else
        iterm = 1
      end if
!
!  Plus sign.
!
    else if ( c == '+' ) then

      if ( ihave == 1 ) then
        ihave = 2
      else if ( ihave == 6 ) then
        ihave = 7
      else
        iterm = 1
      end if
!
!  Decimal point.
!
    else if ( c == '.' ) then

      if ( ihave < 4 ) then
        ihave = 4
      else if ( 6 <= ihave .and. ihave <= 8 ) then
        ihave = 9
      else
        iterm = 1
      end if
!
!  Scientific notation exponent marker.
!
    else if ( ch_eqi ( c, 'E' ) .or. ch_eqi ( c, 'D' ) ) then

      if ( ihave < 6 ) then
        ihave = 6
      else
        iterm = 1
      end if
!
!  Digit.
!
    else if (  ihave < 11 .and. lle ( '0', c ) .and. lle ( c, '9' ) ) then

      if ( ihave <= 2 ) then
        ihave = 3
      else if ( ihave == 4 ) then
        ihave = 5
      else if ( ihave == 6 .or. ihave == 7 ) then
        ihave = 8
      else if ( ihave == 9 ) then
        ihave = 10
      end if

      call ch_to_digit ( c, ndig )

      if ( ihave == 3 ) then
        rtop = 10.0D+00 * rtop + real ( ndig, kind = 8 )
      else if ( ihave == 5 ) then
        rtop = 10.0D+00 * rtop + real ( ndig, kind = 8 )
        rbot = 10.0D+00 * rbot
      else if ( ihave == 8 ) then
        jtop = 10 * jtop + ndig
      else if ( ihave == 10 ) then
        jtop = 10 * jtop + ndig
        jbot = 10 * jbot
      end if
!
!  Anything else is regarded as a terminator.
!
    else
      iterm = 1
    end if
!
!  If we haven't seen a terminator, and we haven't examined the
!  entire string, go get the next character.
!
    if ( iterm == 1 ) then
      exit
    end if

  end do
!
!  If we haven't seen a terminator, and we have examined the
!  entire string, then we're done, and LENGTH is equal to S_LENGTH.
!
  if ( iterm /= 1 .and. length + 1 == s_length ) then
    length = s_length
  end if
!
!  Number seems to have terminated.  Have we got a legal number?
!  Not if we terminated in states 1, 2, 6 or 7!
!
  if ( ihave == 1 .or. ihave == 2 .or. ihave == 6 .or. ihave == 7 ) then
    ierror = ihave
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'S_TO_R8_OLD - Serious error!'
    write ( *, '(a)' ) '  Illegal or nonnumeric input:'
    write ( *, '(a)' ) '    "' // trim ( s ) // '"'
    return
  end if
!
!  Number seems OK.  Form it.
!
  if ( jtop == 0 ) then
    rexp = 1.0D+00
  else
    if ( jbot == 1 ) then
      rexp = 10.0D+00 ** ( jsgn * jtop )
    else
      rexp = 10.0D+00 ** ( real ( jsgn * jtop, kind = 8 ) &
        / real ( jbot, kind = 8 ) )
    end if
  end if

  dval = real ( isgn, kind = 8 ) * rexp * rtop / rbot

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
