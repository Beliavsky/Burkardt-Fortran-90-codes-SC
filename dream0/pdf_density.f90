module pdf_density

    use rand_generator
    
    implicit none
    
    real     :: pi=3.14159265

! -- Private subroutines

   private dloggamma
     
   
   contains

      
!!! $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!!! $$$ Functions Calculating Density Values for a certain Prior Density Type $$$
!!! $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

!--------------------------------------------------------------------------------
real function getpdfunf(lower,upper,rval)

! --- Function getpdfunf calculates the pdf value for a uniform distribution.

      implicit none
      
      real, intent(in)     :: lower   ! lower bound of distribution
      real, intent(in)     :: upper   ! upper bound of distribution
      real, intent(in)     :: rval    ! variable value at which pdf is to be calculated
      
      if(rval.lt.lower)then
        getpdfunf=0.0
      else if(rval.gt.upper)then
        getpdfunf=0.0
      else
        if(upper.le.lower)then
          getpdfunf=0.0
        else
          getpdfunf=1.0/(upper-lower)
        end if
      end if
      
      return
      
end function getpdfunf


!--------------------------------------------------------------------------------
real function getpdfnor(av,sd,rval)

! --- Function getpdfnor calculates the pdf value for a normal distribution.

      implicit none
      
      real, intent(in)     :: av      ! mean
      real, intent(in)     :: sd      ! standard deviation
      real, intent(in)     :: rval    ! variable value at which pdf is to be calculated
      
      double precision     :: rtemp
      
      if(sd.eq.0.0)then
        getpdfnor=0.0
      else
        rtemp=(rval-av)*(rval-av)*0.5/(sd*sd)
        getpdfnor=exp(-rtemp)/sd/sqrt(2.0*pi)
      end if
      
      return
      
end function getpdfnor


!--------------------------------------------------------------------------------
real function getpdfbet(alpha,beta,rval)

! --- Function getpdfbet calculates the pdf value for a beta distribution.

      implicit none
      
      real, intent(in)     :: alpha   ! alpha, shape parameter
      real, intent(in)     :: beta    ! beta, shape parameter
      real, intent(in)     :: rval    ! variable value at which pdf is to be calculated
      
      double precision     :: dalpha,dbeta,dval,dratio

      if((rval.lt.0.0).or.(rval.gt.1.0))then
        getpdfbet=0.0
      else
        dval=rval
        dalpha=alpha
        dbeta=beta
        dratio=dloggamma(dalpha+dbeta)-dloggamma(dalpha)-dloggamma(dbeta)
        if(dratio.gt.80.59d0)then
          dratio=1.0d35
        else
          dratio=exp(dratio)
        end if
        getpdfbet=dval**(dalpha-1.0d0)*(1.0d0-dval)**(dbeta-1.0d0)*dratio
      end if
      
      return
      
end function getpdfbet


!--------------------------------------------------------------------------------
real function getpdfchi(df,rval)

! --- Function getpdfchi calculates the pdf value for a chi-square distribution.

      implicit none
      
      real, intent(in)     :: df      ! degrees of freedom
      real, intent(in)     :: rval    ! variable value at which pdf is to be calculated
      
      double precision     :: ddf,dtemp1,dtemp2,dval
      
      if((rval.le.0.0).or.(df.le.0.0))then
        getpdfchi=0.0
      else
        dval=rval
        ddf=df
        dtemp2=ddf*0.5d0
        dtemp1=(dtemp2-1.0d0)*log(dval)-0.5d0*dval-dtemp2*log(2.0d0)-dloggamma(dtemp2)
        if(dtemp1.gt.80.59d0)then
          getpdfchi=1.0e35
        else
          getpdfchi=exp(dtemp1)
        end if
      end if
      
      return
      
end function getpdfchi


!--------------------------------------------------------------------------------
real function getpdfinvchi(df,rval)

! --- Function getpdfinvchi calculates the pdf value for an inverse-chi-square distribution.

      implicit none
      
      real, intent(in)     :: df      ! degrees of freedom
      real, intent(in)     :: rval    ! variable value at which pdf is to be calculated
      
      double precision     :: ddf,dtemp1,dtemp2,dval
      
      if((rval.le.0.0).or.(df.le.0.0))then
        getpdfinvchi=0.0
      else
        dval=rval
        ddf=df
        dtemp2=ddf*0.5d0
        dtemp1=-dtemp2*log(2.0d0)-(dtemp2+1.0d0)*log(dval)-0.5d0/dval-dloggamma(dtemp2)
        if(dtemp1.gt.80.59d0)then
          getpdfinvchi=1.0e35
        else
          getpdfinvchi=exp(dtemp1)
        end if
      end if
      
      return
      
end function getpdfinvchi


!--------------------------------------------------------------------------------
real function getpdfscinvchi(df,s,rval)

! --- Function getpdfscinvchi calculates the pdf value for a scaled inverse-chi-square distribution.

      implicit none
      
      real      :: df      ! degrees of freedom
      real      :: s       ! scale parameter, sigma^2
      real      :: rval    ! variable value at which pdf is to be calculated
      
      double precision     :: ddf,dtemp1,dtemp2,dval,ds
      
      if((rval.le.0.0).or.(df.le.0.0).or.(s.le.0.0))then
        getpdfscinvchi=0.0
      else
        dval=rval
        ddf=df
        ds=s
        dtemp2=ddf*0.5d0

        dtemp1=dtemp2*log(dtemp2)+dtemp2*log(ds)-(dtemp2*ds/dval)  &     
              -(dtemp2+1.0d0)*log(dval)-dloggamma(dtemp2)

        if(dtemp1.gt.80.59d0)then
          getpdfscinvchi=1.0e35
        else
          getpdfscinvchi=exp(dtemp1)
        end if
      end if
      
      return
      
end function getpdfscinvchi


!--------------------------------------------------------------------------------
real function getpdfgam(beta,alpha,rval)

! --- Function getpdfgam calculates the pdf value for a gamma distribution.

      implicit none
      
      real, intent(in)     :: beta    ! beta, rate parameter
      real, intent(in)     :: alpha   ! alpha, shape parameter
      real, intent(in)     :: rval    ! variable value at which pdf is to be calculated
      
      double precision     :: dbeta,dalpha,dtemp,dval
      
      if((rval.le.0.0).or.(alpha.le.0.0).or.(beta.le.0.0))then
        getpdfgam=0.0
      else
        dval=rval
        dalpha=alpha
        dbeta=beta
        dtemp=dalpha*log(dbeta)+(dalpha-1.0)*log(dval)-dbeta*dval-dloggamma(dalpha)
        if(dtemp.gt.80.59d0)then
          getpdfgam=1.0e35
        else
          getpdfgam=exp(dtemp)
        end if
      end if
      
      return
      
end function getpdfgam


!--------------------------------------------------------------------------------
real function getpdfinvgam(beta,alpha,rval)

! --- Function getpdfinvgam calculates the pdf value for an inverse gamma distribution.

      implicit none
      
      real, intent(in)     :: beta    ! beta, scale parameter
      real, intent(in)     :: alpha   ! alpha, shape parameter
      real, intent(in)     :: rval    ! variable value at which pdf is to be calculated
      
      double precision     :: dbeta,dalpha,dtemp,dval
      
      if((rval.le.0.0).or.(alpha.le.0.0).or.(beta.le.0.0))then
        getpdfinvgam=0.0
      else
        dval=rval
        dalpha=alpha
        dbeta=beta
        dtemp=dalpha*log(dbeta)-(dalpha+1.0)*log(dval)-dbeta/dval-dloggamma(dalpha)
        if(dtemp.gt.80.59d0)then
          getpdfinvgam=1.0e35
        else
          getpdfinvgam=exp(dtemp)
        end if
      end if
      
      return
      
end function getpdfinvgam


!--------------------------------------------------------------------------------
real function getpdfexp(beta,rval)

! --- Function gtpdfexp calculates the pdf value for an exponential distribution.

      implicit none
      
      real, intent(in)     :: beta    ! scale parameter
      real, intent(in)     :: rval    ! variable value at which pdf is to be calculated

      if((rval.le.0.0).or.(beta.le.0.0))then
        getpdfexp=0.0
      else
        getpdfexp=exp(-rval/beta)/beta
      end if
      
      return
      
end function getpdfexp


!--------------------------------------------------------------------------------
real function getpdfmnor(ndim,mu,cov,logdet,rvals)

! -- Function getpdfmnor calculates the pdf value for a multi-normal distribution.

      implicit none
      
      integer, intent(in) :: ndim                    ! number of dimensions
      real, intent(in)    :: mu(ndim)                ! vector of parameters                                  
      real, intent(in)    :: cov(ndim, ndim)         ! covariance matrix
      real, intent(in)    :: rvals(ndim)             ! vector of random numbers
      real    :: covinv(ndim, ndim)      ! inverse of covariance matrix
      real    :: cparm(ndim*(ndim+3)/2+1)

      integer          :: i,j
      real    :: logdet
      real*8  :: rsum

     
! -- calculate the inverse of the covariance matrix
     do i=1,ndim             
        do j=1,i
           covinv(j,i)=cov(j,i)
        end do
        if(i.ne.ndim)then
           do j=i+1,ndim
              covinv(j,i)=cov(i,j)
           end do
        end if
      end do
              

! -- calculate pdf of multivariate normal distribution

      rsum=0.0d0
      do i=1,ndim
        do j=1,ndim
          rsum=rsum+(rvals(i)-mu(i))*(rvals(j)-mu(j))*covinv(i,j)
        end do
      end do
      
      rsum=-0.5d0*ndim*log(2.0d0*pi)-0.5d0*logdet-0.5d0*rsum  
      
      getpdfmnor=exp(rsum)
     
      return
     
end function getpdfmnor     


!!! $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!!! $$$$$$$$ Functions called by Gamma PDF $$$$$$$$$$$$$$$$$$
!!! $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 
! this function is calculated based on Gamma function approximation found in wiki

double precision function dloggamma(dval)

! -- Subroutine dloggamma calculates the natural log of the gamma function 
!    of real argument.

     double precision, intent (in)        :: dval
     if (dval .le. 0.0d0)then      
       dloggamma=0.0d0
     else
       dloggamma=(dval-0.5d0)*log(dval)-dval+log(sqrt(2*pi))  &
         +log(1+1/(12*dval)+1/(288*dval**2)-139/(51840*dval**3)-571/(2488320*dval**4))
     endif             
     
     return
end function dloggamma


end module 
