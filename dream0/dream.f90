module dream

   use rand_generator
   use pdf_density 

   implicit none

! -- Private variables

   private par_dim,  range, ob_dim,                                                &
           nchain, max_gen, npair, jumpstep,  restart_flag,                        &
           nCR, CR, CR_ind, pCR, L_CR, Dis_CR,                                     &
           Gel_Ru_R, printstep, GR_count, conv_flag, GR_threshold,                 &
           jumprate_table, jumprate, jump_num, jump_dim,                           &
           Z, fit, Zp, AR,                                                         &   
           mnor_dim, uni_dim, mnor_ind, uni_ind, mnor_mean, cov_mat, parm, logdet, &
           prior_den


! -- Private subroutines           
 
   private likelihood,       & 
           get_prior_sample, &
           prior_density,    &
           gen_candidate,    &
           outofbound,       &
           init_CR,          &
           choose_CR,        &
           update_CR_dis,    &
           update_pCR,       &
           comp_std,         &
           choose_jumprate,  &
           comp_diff,        &
           Gel_Ru,           &
           Output,           &
           get_unit
           
           

!*****************************************************
!           PARAMETER DEFINITION
!*****************************************************

!  Number of parameters
   integer :: par_dim

!  Range of each parameter
   real  :: range( 2, 1000 )

!  observed data
   character(50) :: ob_file
   integer :: ob_dim
   real*8, allocatable :: ob_data(:)   

!  Number of Markov chains
   integer :: nchain   

!  Maximum generations for a single chain
   integer :: max_gen

!  Number of pairs of chains to generate candidate samples
   integer :: npair

!  Number of steps to reset jump rate Gamma
   integer :: jumpstep

!  Restart the chain from last generation of last time simulation or not
   character(10) :: restart_flag
   

!*** Other variables used in this module
!  Crossover parameters
   integer :: nCR
   real  :: CR(20)
   integer :: CR_ind
   real    :: pCR(20) 
   integer :: L_CR(20)
   real  :: Dis_CR(20)

!  Gelman Rubin R statistic used to check MC convergence
   real, allocatable :: Gel_Ru_R(:,:)
   integer :: printstep
   integer :: GR_count
   logical :: conv_flag
   real  :: GR_threshold 

!  Jump rate table
   real, allocatable :: jumprate_table(:)
   real  :: jumprate
   integer :: jump_num 
   integer, allocatable :: jump_dim(:)

!  Markov chain 
   real, allocatable :: Z(:,:,:)
   real, allocatable :: fit(:,:)
   real, allocatable :: Zp(:)

!  Acceptance rate
   real :: AR

!  Define the prior density type
   type density
      character*40 :: type                    ! prior density type
      integer      :: numrealpar              ! number of density parameters
      real         :: realpar(2)              ! array to save density parameters 
   end type density

!  Variables for multinormal density
   integer :: mnor_dim, uni_dim
   integer :: mnor_ind(1000), uni_ind(1000)
   real, allocatable :: mnor_mean(:)          ! mean vector
   real, allocatable :: cov_mat(:,:)          ! covariance matrix
   real, allocatable :: parm(:)               ! used for calculate inverse covariance matrix of multi-normal distribution 
   real :: logdet                             ! log determinant of covariance matrix

   type( density ), allocatable :: prior_den(:)


   contains 

 
!**************************************************************************************
!
!                       FUNCTIONS FOR INITIALIZATING CHAINS
!
!**************************************************************************************
!--------------------------------------------------------------------------------------
!***  Read input parameters from the input file


   subroutine read_vars( )

      implicit none

      integer :: iunit
      integer :: i

      call get_unit(iunit)
      
      open(iunit, file = 'DreamTest.in' )

      read(iunit,*) 
      read(iunit,*) par_dim 
      read(iunit,*) 
      read(iunit,*) range(1,1:par_dim) 
      read(iunit,*) 
      read(iunit,*) range(2,1:par_dim) 
      read(iunit,*) 
      read(iunit,*) max_gen 
      read(iunit,*)
      read(iunit,*) nchain
      read(iunit,*)
      read(iunit,'(a50)') ob_file
      read(iunit,*)
      read(iunit,*) ob_dim
      read(iunit,*) 
      read(iunit,*) npair 
      read(iunit,*) 
      read(iunit,*) nCR 
      read(iunit,*) 
      read(iunit,*) jumpstep 
      read(iunit,*) 
      read(iunit,*) printstep 
      read(iunit,*) 
      read(iunit,*) GR_threshold 
      read(iunit,*) 
      read(iunit,*) restart_flag

      close(iunit)

   end subroutine

!**********************************************************
! import observation data

   subroutine import_observ()

      implicit none

      integer :: iunit, i

      allocate( ob_data( ob_dim ) )

      call get_unit( iunit )
      open(iunit, file = trim(ob_file) )

      read(iunit,*)

      do i = 1, ob_dim

         read(iunit,*) ob_data(i)

      end do

      close(iunit)

   end subroutine


!--------------------------------------------------------------------------------------
!***  Start markov chains

   subroutine init_chain( )

      implicit none

      integer :: i, j, iunit, dummyi

      allocate( Z(par_dim, nchain, max_gen) )
      allocate( fit( nchain, max_gen ) )
      allocate( Zp( par_dim) ) 

    ! if restart the chain, continue the chain from the last sample of last time simulation
      
      if( trim(restart_flag) == 'yes' )then

         call get_unit( iunit )
         open(iunit, file = 'Restart.out' )
         read(iunit,*) 
         
         do i = 1, nchain
            read(iunit, *)dummyi, fit(i,1), Z(:,i,1)
         end do

         close(iunit)

    ! initialize the chain from a centrain prior distribution
      
      else

         do i =1, nchain
            Z(:,i,1) = get_prior_sample( )
            fit(i,1) = likelihood( Z(:,i,1) )
         end do

      end if

   end subroutine


!**************************************************************************************
!
!                       FUNCTIONS TO CALCULATE LOG LIKELIHOOD  
!
!**************************************************************************************

!--------------------------------------------------------------------------------------
!***  Compute log likelihood function 


   function likelihood( Zx )

      !use ifport
      implicit none

      real  :: Zx(par_dim)
      real  :: likelihood,pi
      integer :: i
      pi=3.1415926

      !-------- TEST --------------------
      !case 1: 10-D  bimodal distribution

      likelihood = log(1.0/3.0/(2.0*pi)*exp(-0.5d0*sum((Zx+5.0d0)**2)) + 2.0/3.0/(2.0*pi)*exp(-0.5d0*sum((Zx-5.0d0)**2)) )

   end function


!**************************************************************************************
!
!                       FUNCTIONS ABOUT PRIOR INFORMATION 
!
!**************************************************************************************

!--------------------------------------------------------------------------------------
!***  Read prior information 


   subroutine init_prior( )

      implicit none
      
      integer :: iunit, i, cov_dim      
      allocate( prior_den( par_dim ) )
   
    ! number of parameters have multinormal distribution
      mnor_dim = 0
      
    ! number of parameters have univariate distribution  
      uni_dim = 0
    
      call get_unit(iunit) 

    ! In the file specify the prior distribution    
      open(iunit, file = 'prior.in' )

      read(iunit,*) 
      do i = 1, par_dim
         read(iunit, '(a20)') prior_den(i)%type
         read(iunit,*) prior_den(i)%numrealpar, prior_den(i)%realpar(1:prior_den(i)%numrealpar) 

         if( trim(prior_den(i)%type) == 'multinormal') then
            mnor_dim = mnor_dim + 1
            mnor_ind(mnor_dim) = i
         else
            uni_dim = uni_dim + 1
            uni_ind(uni_dim) = i
         end if
      end do 
      
      close(iunit)


      if( mnor_dim == 1 )then
         write(*,*) '*** ERROR: Only one parameter follows multinormal distribution. please use normal instead!'
         stop

      elseif( mnor_dim > 1 )then

         allocate( mnor_mean( mnor_dim ) )
         allocate( cov_mat(mnor_dim, mnor_dim) )
         allocate( parm( mnor_dim*(mnor_dim+3)/2+1 ) )
       
       ! read the covariance matrix of the multinormal density from matrix_file input block
         call get_unit(iunit) 
         
         open(iunit, file = 'covmatrix.dat' )
         
         read(iunit,*) 
         read(iunit,*) cov_dim

         if( cov_dim /= mnor_dim )then
	    write(*,*) '*** ERROR: Covariance Matrix dimension in matrix file must be the same with  &
	             the number of parameters having multinormal density type in MCMC_Prior_PDF block!'
            stop
         end if

         do i = 1, mnor_dim 
            read(iunit,*) cov_mat(i,:)
         end do
         
         close(iunit)

       ! collect the mean values of the multinormal desity
         do i = 1, mnor_dim
            mnor_mean(i) = prior_den(mnor_ind(i))%realpar(1)
         end do

         call setgmn( mnor_dim, mnor_mean, cov_mat, parm, logdet)
         
      end if


   end subroutine


!--------------------------------------------------------------------------------------
!***  Get a Sample from the specified prior distribution to start MCMC

   
   function get_prior_sample( )

      implicit none

      real :: get_prior_sample( par_dim )
      integer :: i
      real :: work(mnor_dim)
      real :: tmp(mnor_dim)


    ! generate samples follow univariate density    
      if( uni_dim > 0 )then  
         do i = 1, uni_dim
   
            select case( trim(prior_den(uni_ind(i))%type) )

               case('uniform')
                  get_prior_sample(uni_ind(i)) = genunf( prior_den(uni_ind(i))%realpar(1), &
                                                         prior_den(uni_ind(i))%realpar(2) ) 
         
               case('normal')
                  get_prior_sample(uni_ind(i)) = gennor( prior_den(uni_ind(i))%realpar(1), &
                                                         prior_den(uni_ind(i))%realpar(2) )

               case('beta')
                  get_prior_sample(uni_ind(i)) = genbet( prior_den(uni_ind(i))%realpar(1), &
                                                         prior_den(uni_ind(i))%realpar(2) )

               case('chi-square')
                  get_prior_sample(uni_ind(i)) = genchi( prior_den(uni_ind(i))%realpar(1) )
      
               case('inv-chi-square')
                  get_prior_sample(uni_ind(i)) = gengam( 0.5, 0.5*prior_den(uni_ind(i))%realpar(1) )
                  if( get_prior_sample(uni_ind(i)) .ne. 0.0 ) then
                     get_prior_sample(uni_ind(i)) = 1.0 / get_prior_sample(uni_ind(i))
                  end if
   
               case('scaled-inv-chi-square')
                  get_prior_sample(uni_ind(i)) = gengam( 0.5*prior_den(uni_ind(i))%realpar(1)  & 
                                                            *prior_den(uni_ind(i))%realpar(2), &
                                                         0.5*prior_den(uni_ind(i))%realpar(1) )

                  if( get_prior_sample(uni_ind(i)) .ne. 0.0 ) then
                     get_prior_sample(uni_ind(i)) = 1.0 / get_prior_sample(uni_ind(i))
                  end if

               case('gamma')
                  get_prior_sample(uni_ind(i)) = gengam( prior_den(uni_ind(i))%realpar(1), &
                                                         prior_den(uni_ind(i))%realpar(2) )
               
               case('inv-gamma')
                  get_prior_sample(uni_ind(i)) = gengam( prior_den(uni_ind(i))%realpar(1), &
                                                         prior_den(uni_ind(i))%realpar(2) )
                  if( get_prior_sample(uni_ind(i)) .ne. 0.0 ) then
                     get_prior_sample(uni_ind(i)) = 1.0 / get_prior_sample(uni_ind(i))
                  end if

               case('exponential')
                  get_prior_sample(uni_ind(i)) = genexp( prior_den(uni_ind(i))%realpar(1) )

               case default
                  write(*,*)'*** ERROR: unknown density type!' 
                  stop 

            end select
      
         end do

      end if

      
    ! generate samples follow multinormal density
      if( mnor_dim > 1 )then
      
         call genmn( parm, tmp, work )
         
         do i = 1, mnor_dim
            get_prior_sample(mnor_ind(i)) = tmp(i)
         end do
         
      end if

   end function


!--------------------------------------------------------------------------------------
!***  Compute density values of the specified prior density type to calculate Metropolis ratio 


   function prior_density( rval )

      implicit none

      real :: rval( par_dim )
      real :: prior_density
      integer :: i
      real :: tmp( mnor_dim )

      prior_density = 1.0
      

    ! calculate density values for univariate density     
      if( uni_dim > 0 )then
         do i = 1, uni_dim
   
            select case( trim(prior_den(uni_ind(i))%type) )
   
               case('uniform')
                  prior_density =   prior_density &
                                  * getpdfunf( prior_den(uni_ind(i))%realpar(1), &
                                               prior_den(uni_ind(i))%realpar(2), rval(uni_ind(i)) ) 
         
               case('normal')
                  prior_density =   prior_density &
                                  * getpdfnor( prior_den(uni_ind(i))%realpar(1), &
                                               prior_den(uni_ind(i))%realpar(2), rval(uni_ind(i)) ) 
   
               case('beta')
                  prior_density =   prior_density &
                                  * getpdfbet( prior_den(uni_ind(i))%realpar(1), &
                                               prior_den(uni_ind(i))%realpar(2), rval(uni_ind(i)) ) 
   
               case('chi-square')
                  prior_density =   prior_density &
                                  * getpdfchi( prior_den(uni_ind(i))%realpar(1), rval(uni_ind(i)) ) 
            
               case('inv-chi-square')
                  prior_density =   prior_density &
                                  * getpdfinvchi( prior_den(uni_ind(i))%realpar(1), rval(uni_ind(i)) ) 
   
               case('scaled-inv-chi-square')
                  prior_density =   prior_density &
                                  * getpdfscinvchi( prior_den(uni_ind(i))%realpar(1), &
                                                    prior_den(uni_ind(i))%realpar(2), rval(uni_ind(i)) ) 
   
               case('gamma')
                  prior_density =   prior_density &
                                  * getpdfgam( prior_den(uni_ind(i))%realpar(1), &
                                               prior_den(uni_ind(i))%realpar(2), rval(uni_ind(i)) ) 
            
               case('inv-gamma')
                  prior_density =   prior_density &
                                  * getpdfinvgam( prior_den(uni_ind(i))%realpar(1), &
                                                  prior_den(uni_ind(i))%realpar(2), rval(uni_ind(i)) ) 

               case('exponential')
                  prior_density =   prior_density &
                                  * getpdfexp( prior_den(uni_ind(i))%realpar(1), rval(uni_ind(i)) ) 
  
            end select

         end do

      end if
      

    ! calculate density values for multinormal density     
      if( mnor_dim > 1 )then
      
         do i = 1, mnor_dim
            tmp(i) = rval(mnor_ind(i))
         end do
         
         prior_density = getpdfmnor( mnor_dim, mnor_mean(1:mnor_dim), cov_mat, logdet, tmp ) 
         
      end if

   end function


!**************************************************************************************
!
!                       FUNCTIONS ABOUT DREAM ALGORITHM  
!
!**************************************************************************************

!--------------------------------------------------------------------------------------
!***  Use DREAM algorithm to get a candicate parameter sample


   subroutine DREAM_algm( )
     
     implicit none
     
     integer :: i, j, accept, Zp_count
     real  :: ratio
     
     Zp_count = 0
     accept = 0


   ! Initialize the CR values
     call init_CR()
     
     do i = 2, max_gen
     
        do j = 1, nchain

         ! Choose a CR value
           call choose_CR( )
	   
         ! Generate a candidate
           call gen_candidate( i, j )
           Zp_count = Zp_count + 1
	
         ! Compute log likelihood function 
           fit(j,i) = likelihood( Zp )
	
         ! Compute the metroplis ratio 

            ratio = min( exp( ( fit(j,i) + log( prior_density(Zp) ) ) - ( fit(j,i-1) + log( prior_density(Z(:,j,i-1)) ) ) ), 1.0 )

	   if( ratio >= rand_uni01() )then
	      Z(:,j,i) = Zp
	      accept = accept + 1
	   else
	      Z(:,j,i) = Z(:,j,i-1)
	      fit(j,i) = fit(j,i-1)
           end if

         ! Update CR distance
           if( conv_flag .eqv. .false. .and. nCR > 1 )then  
              call update_CR_dis( i, j )
           end if
	
        end do

      ! Update the multinomial distribution of CR
        if( conv_flag .eqv. .false. .and. nCR > 1 .and. mod(i,10)==0 ) then
           call update_pCR( i )
        end if

      ! Compute Gelman Rubin R and export result
        if( mod(i, printstep) == 0 )then
           call Gel_Ru(i)
           call Output( i-printstep+1, i )
        end if
        
      ! Outlier test
        if( conv_flag .eqv. .false. .and. mod(i,10) == 0 )then
           call outliertest( i )
        end if        

     end do        
     
   ! Compute the acceptance rate

     AR = dfloat(accept) / dfloat(Zp_count)         

    
   end subroutine


!--------------------------------------------------------------------------------------
!***  Generate candidate parameter samples


   subroutine gen_candidate( gen_num, chain_num )

      implicit none

      integer :: gen_num, chain_num
      integer :: R(2, npair)
      real  :: noise_e(par_dim), b, eps(par_dim)
      integer :: i
     
     
      b=0.0  ! used to calculate e follow U(-b,b)


    ! Pick n pairs of other chains for crossover
      do i = 1, npair
         R(:,i) = rand_R( nchain )
         do while ( R(1,i) == R(2,i) .or. R(1,i) == chain_num .or. R(2,i) == chain_num )
            R(:,i) = rand_R( nchain )
         end do
      end do
	
    ! Determine a jump rate
      call choose_jumprate( gen_num )

    ! Calculate e in eq.4 of Vrugt et al. 2009
      noise_e = b * (2 * rand_uni01_dim(par_dim) - 1) 

    ! Get epsilon value from multinormal distribution                      
      do i =1, par_dim
         eps(i) = gennor(0.0, 1.0e-10)
      end do

    ! Generate the candidate sample based on eq.4 of Vrugt et al. 2009
      Zp = Z(:,chain_num,gen_num-1) + (1.0 + noise_e) * jumprate * comp_diff(gen_num, chain_num, R) + eps

    ! Check whether candidate sample is out of parameter bound
      call outofbound( )

   end subroutine
   
   
!--------------------------------------------------------------------------------------
!*** Out of parameter bound test

!  Test whether generated candidate sample is out of the bound set by users
!  if it is out of bound then fold the sample into the bound


   subroutine outofbound( )
   
     implicit none
   
     integer :: i
     
   
     do i = 1, par_dim
   
        if( Zp(i) < range(1,i) )then
            Zp(i) = range(2,i) - range(1,i) + Zp(i)
        
        else if( Zp(i) > range(2,i) )then
            Zp(i) = range(1,i) - range(2,i) + Zp(i)

        end if

! Just in case the sample is still outside bound

        if( Zp(i) < range(1,i) .or. Zp(i) > range(2,i) )then
            Zp(i) = range(1,i) + rand_uni01() * ( range(2,i) - range(1,i) )
        
        end if
     
     end do

   
   end subroutine
   

!********************* Compute crossover probability CR *******************************
!--------------------------------------------------------------------------------------
!*** initialize the CR values

   
   subroutine init_CR( )

      implicit none

      integer :: i
      

      do i = 1, nCR

         CR(i) = dfloat(i) / dfloat(nCR)  
         pCR(i) = 1.0d0 / dfloat(nCR)
         L_CR(i) = 1
         Dis_CR(i) = 1.0d0

      end do

      pCR(nCR) = 1.0d0 - sum(pCR(1:nCR-1))

   end subroutine
   
   
!--------------------------------------------------------------------------------------
!***  choose a CR value


   subroutine choose_CR()

      implicit none
      
      integer :: tmp_ind(nCR) 
      integer :: i
      

      if( nCR == 1 ) then
         CR_ind = 1
         
      else
 
         call genmul(1, pCR(1:nCR), nCR, tmp_ind)

         do i = 1, nCR
            if( tmp_ind(i) == 1 ) then
               CR_ind = i
               exit
            end if
         end do

      end if

   end subroutine


!--------------------------------------------------------------------------------------
!***  update CR distance


   subroutine update_CR_dis( gen_num, chain_num ) 

      implicit none
      integer :: gen_num, chain_num
      real  :: std( par_dim )
      integer :: i
      

    ! Compute standard deviation for all parameters
      std = comp_std( gen_num )

      L_CR(CR_ind) = L_CR(CR_ind) + 1

      do i = 1, par_dim 

         Dis_CR(CR_ind) = Dis_CR(CR_ind) + ( Z(i, chain_num, gen_num ) - Z(i, chain_num, gen_num-1) )**2 / std(i)**2

      end do 

   end subroutine


!--------------------------------------------------------------------------------------
!***  update CR probabilities 

 
   subroutine update_pCR( gen_num )

      implicit none
      
      integer :: i, gen_num


      do i = 1, nCR-1

         pCR(i) = (Dis_CR(i)/L_CR(i)) / sum(Dis_CR(1:nCR) / L_CR(1:nCR)) 

      end do

      pCR(nCR) = 1.0d0 - sum( pCR(1:nCR-1) )
        
   end subroutine
   

!--------------------------------------------------------------------------------------
!***  compute standard deviation 

 
   function comp_std( gen_num )
 
      implicit none

      integer :: gen_num
      real  :: comp_std( par_dim )
      real  :: mean( par_dim )
      integer :: i
      

      do i = 1, par_dim

         mean(i) = sum( sum( Z(i,:,1:gen_num),1 ), 1) / nchain / gen_num
         comp_std(i) = sqrt(sum( sum( (Z(i,:,1:gen_num) - mean(i))**2, 1), 1 ) / (nchain*gen_num-1) )
       
      end do

   end function


!************************* Compute jump rate Gamma ************************************
!--------------------------------------------------------------------------------------
!***  Initiailize the jump rate table


   subroutine init_jumprate( )

      implicit none

      integer :: i
      
      allocate( jump_dim( par_dim ) )
      allocate( jumprate_table( par_dim ) )
      
 
      do i = 1, par_dim
         jumprate_table(i) = 2.38d0 / sqrt( 2.0d0 * npair * i )
      end do

   end subroutine


!--------------------------------------------------------------------------------------
!***  Choose a jump rate from the jump rate table
 
 
   subroutine choose_jumprate( gen_num )

      implicit none
   
      integer :: i
      integer :: gen_num

    ! Determine the dimensions that will be updated
      jump_num = 0
      jump_dim = 0
      

      do i = 1, par_dim
         if( rand_CR( ) > 1-CR(CR_ind) )then
            jump_num = jump_num + 1
            jump_dim(jump_num) = i
         end if
      end do

    ! Calculate general jump rate        
      if( jump_num == 0 )then
         jumprate = 0.0
      else
         jumprate = jumprate_table(jump_num)
      endif

    ! If parameter dimension is 1, 2, or 3, fix the jump rate to 0.6
      if( par_dim == 1 .or. par_dim == 2 .or. par_dim == 3 )then
         jumprate = 0.6
      end if

    ! Determine if do a long jump 
      if( mod(gen_num-1,jumpstep) == 0 )then
         jumprate = 0.98
         return
      end if

   end subroutine


!--------------------------------------------------------------------------------------
!***  Calculate the differential evoluation


   function comp_diff( gen_num, chain_num, R ) 

      implicit none

      integer :: gen_num, chain_num
      real    :: comp_diff( par_dim )
      integer :: R(2, npair)
      integer :: i, j
    
    ! Do differential evolution
      Zp = Z(:,chain_num,gen_num-1)

    ! Produce the difference of the pairs used for population evolution
      comp_diff = 0.0
      
      do i = 1, npair
         do j = 1, jump_num
            comp_diff(jump_dim(j)) = comp_diff(jump_dim(j)) &
            + (Z(jump_dim(j),R(1,i),gen_num-1) &
            -Z(jump_dim(j),R(2,i),gen_num-1))         
         end do
      end do

   end function


!**************************************************************************************
!
!               FUNCTIONS ABOUT CONVERGENCE CRETERIA GELMAN-RUBIN R
!
!**************************************************************************************

!--------------------------------------------------------------------------------------
!***  Initialize the Gelman Rubin statistic


   subroutine init_GR()

      implicit none

      allocate( Gel_Ru_R( par_dim, max_gen ) )

      GR_count = 0
      conv_flag = .false.

   end subroutine


!--------------------------------------------------------------------------------------
!***  Compute Gelman Rubin statistics R used to check convergence


   subroutine Gel_Ru( gen_num )

      implicit none

      integer :: gen_num
      real  :: mean_chain( nchain )
      real  :: mean_all
      real  :: S( nchain )
      real  :: W_var, B_var, Var 
      integer :: i, j, ind0


      GR_count = GR_count + 1
      ind0 = 0.5d0*gen_num

      do i = 1, par_dim
    
         do j = 1, nchain
            mean_chain(j) = sum( Z(i,j,ind0:gen_num) ) / dfloat(ind0)
         end do

         mean_all = sum( mean_chain ) / nchain
 
         B_var = dfloat(ind0) / dfloat( nchain - 1 ) * sum( (mean_chain - mean_all)**2 )   

         do j = 1, nchain
            S(j) = sum( (Z(i,j,ind0:gen_num) - mean_chain(j))**2 ) / (ind0 - 1.0d0)
         end do
 
         W_var = sum(S) / nchain

         Var = dfloat(ind0-1)/dfloat(ind0) * W_var + B_var / dfloat(ind0)   

         Gel_Ru_R( i, GR_count ) = sqrt( Var / W_var )

      end do

      conv_flag = .true.
      
      do i = 1, par_dim
         if( Gel_Ru_R( i, GR_count ) > GR_threshold )then
            conv_flag = .false.
            exit
         end if
      end do

      if (conv_flag .eqv. .true.) write(*,*)'Converged at iteration: ',gen_num

   end subroutine 


!**************************************************************************************
!
!                       FUNCTIONS FOR EXPORTING RESULTS 
!
!**************************************************************************************

!--------------------------------------------------------------------------------------
!***  Write parameter samples into file

   subroutine Output( ind1, ind2 )

      implicit none

      integer :: ind1, ind2
      integer :: iunit, i, j 
      character*10 :: ic
   

!     write parameter samples of all chains
      do i = 1, nchain

       ! open file
         if( ind1 == 1 )then
            write(ic,'(i2.2)') i
            call get_unit( iunit )
            open(iunit, file = 'ParSamples._chain'//trim(ic),STATUS='REPLACE')
            write(iunit,11)trim(ic)
         else
            write(ic,'(i2.2)') i
            call get_unit( iunit )
            open(iunit, file = 'ParSamples._chain'//trim(ic),STATUS='OLD', position = 'append') 
         end if

       ! Write loglikelihood function and parameter samples  
         do j = ind1, ind2
            write(iunit,10)j, fit(i,j), Z(:,i,j) 
         end do

       ! close file
         close(iunit)   

      end do


!     write Gelman-Rubin R
    ! open file
      if( ind1 == 1 )then
         call get_unit( iunit )
         open( iunit, file = 'Gel_Ru.out',STATUS='REPLACE' )
         write(iunit,21)
      else
         open( iunit, file = 'Gel_Ru.out', STATUS='OLD', position = 'append'  ) 
      end if

    ! Write Gelman-Rubin statistic R
      write(iunit,20) printstep*GR_count, Gel_Ru_R(:,GR_count)

    ! close file
      close(iunit)


11    format(1x,'"MONITORED PARAMETER SAMPLE VALUES AND ASSOCIATED LOG LIKELIHOOD FUNCTION VALUES FOR CHAIN # ',A,'"')
21    format(1x,'"MONITORED PARAMETER INTER-CHAINS GELMAN RUBIN R STATISTIC"')
10    format(1x,i7,6x,es14.7,6x,1000(es14.7,2x)) 
20    format(1x,i9,6x,1000(f14.4,2x)) 

   
   end subroutine


!--------------------------------------------------------------------------------------
!***  Write the last parameter samples into restart file used for the next time simulation


   subroutine OutputRestart( )
   
      implicit none
   
      integer :: iunit, i, j 
   
  
      call get_unit( iunit )
      
      open( iunit, file = 'Restart.out' )
      
      write(iunit,11)

      do i = 1, nchain
         write(iunit,10)i, fit(i, max_gen), Z(:,i,max_gen)
      end do
      
      close(iunit)

         
10    format(1x,i7,7x,es14.7,6x,1000(es14.7,2x)) 
11    format(1x,'"PARAMETER VALUES OF THE LAST GENERATION FOR ALL CHAINS FOR USE IN RESTARTING THE CHAINS"')     


    ! Print out the acceptance rate on creen.   
      write(*,*)'The acceptance rate is: ', AR

   end subroutine


!**************************************************************************************
!
!                       FUNCTIONS TO DETECT OUTLIER CHAIN 
!
!**************************************************************************************   
   

!--------------------------------------------------------------------------------------
! ******* Test outlier chain in burn-in period 

   subroutine outliertest( gen_num )

      implicit none

      integer :: gen_num

      real  :: avg(nchain)
      real  :: avg_tmp(nchain)
      integer :: i, j
      integer :: ind1, ind2
      real  :: IQR, Q1, Q3
      integer :: best(1)


      do i = 1,nchain
         avg(i) = sum( fit(i, gen_num/2:gen_num) ) / size( fit(i, gen_num/2:gen_num) )
      end do 

      best = maxloc( avg )
      avg_tmp = avg

      call sort( nchain, avg_tmp )

      ind1 = nchain * 0.25 + 1
      ind2 = nchain * 0.75 + 1
     
      Q1 = avg_tmp(ind1)
      Q3 = avg_tmp(ind2)

      IQR = Q3 - Q1
  
      do i = 1, nchain
         if( avg(i) < Q1 - 2.0 * IQR )then
            Z(:,i,gen_num) = Z(:,best(1),gen_num)
            fit(i, gen_num/2:gen_num) = fit(best(1), gen_num/2:gen_num)
            write(*,201)i,gen_num,best(1)
         end if
      end do
      
201 FORMAT(2X,'Chain ',i2,' is an outlier chain at iteration ',i10, / &
          '  its samples at this iteration are replaced by those from the chain ',i2,' with the largest log likelihood function.'/)

   end subroutine outliertest

!--------------------------------------------------------------------------------------
!****** Sorting an array in ascending order ************
!****** called by subroutine outliertest ************** 

   SUBROUTINE SORT(n,a)
   
   implicit none

   integer :: i, j, k
   integer :: n
   real  :: a(n), tmp
   
   do i = 1, n-1
      do j = i+1, n
         if( a(i) > a(j) )then
            tmp = a(i)
            a(i) = a(j)
            a(j) = tmp
         end if
      end do
   end do
   
   end subroutine sort


!**********************************************************
   subroutine get_unit ( iunit )

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

    end subroutine


end module
