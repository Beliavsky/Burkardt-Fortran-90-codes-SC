module rand_generator 

   implicit none

! -- Private variables

   private point_num, rand_pool, count
   

! -- Private subroutines

   private ignbin, snorm, sgamma, sexpo  
   
   
! -- Parameter definition

   integer, parameter :: point_num = 1000000
   real  :: rand_pool( point_num )
   integer :: count 

     

   contains
   
!********************************************************
!************ Generate Random Numbers *******************
!********************************************************

  SUBROUTINE init_random_seed()
     INTEGER :: i, n, clock
     INTEGER, DIMENSION(:), ALLOCATABLE :: seed

     CALL RANDOM_SEED(size = n)
     ALLOCATE(seed(n))

     CALL SYSTEM_CLOCK(COUNT=clock)

     seed = clock + 37 * (/ (i - 1, i = 1, n) /)
     CALL RANDOM_SEED(PUT = seed)

     DEALLOCATE(seed)
  END SUBROUTINE



!-------------------------------------------------------
!***  generate random numbers pool from uniform distribution U(0, 1)

   subroutine gen_uniform_01( )

      implicit none

      call init_random_seed( )
      call random_number( rand_pool )

      count = 1
    
      return

   end subroutine 

!--------------------------------------------------------
!***  generate a random pair of chains for generating candidate sample

   function rand_R( N )

      implicit none
      integer :: N
      integer :: rand_R(2)

      if( count+1 > 1000000 ) count = 1

      rand_R = rand_pool(count:count+1) * N + 1

      count = count + 2 

   end function
   
!--------------------------------------------------------
!***  generate a random number from U(0, 1)

   function rand_uni01()

      implicit none

      real  :: rand_uni01

      if( count > 1000000 ) count = 1
      
      rand_uni01 = rand_pool(count)

      count = count + 1

   end function   
 
!--------------------------------------------------------
!***  generate a randon number to compare with CR

   function rand_CR() 

      implicit none

      real  :: rand_CR
      
      if( count > 1000000 ) count = 1

      rand_CR = rand_pool(count)

      count = count + 1

   end function
   
!--------------------------------------------------------
!***  generate par_dim U (0,1) randon numbers to calculate e in eq.(4) of Vrugt et al 2009

   function rand_uni01_dim(dim) 

      implicit none
 
      integer :: dim  !number of parameters, par_dim
      real  :: rand_uni01_dim(dim)    
      integer :: i    
      
      do i=1, dim
      
         if( count > 1000000 ) count = 1
         
         rand_uni01_dim(i) = rand_pool(count)
         
         count = count + 1

      end do

   end function  
   
  

!************************************************************************
!****** Generate Random Numbers from Multinomial Distribution ***********
!******          Used for Update CR in DREAM algorithm          ********
!************************************************************************

SUBROUTINE genmul(n,p,ncat,ix)
!**********************************************************************
!
!            SUBROUTINE GENMUL( N, P, NCAT, IX )
!     GENerate an observation from the MULtinomial distribution
!
!                              Arguments
!
!     N --> Number of events that will be classified into one of
!           the categories 1..NCAT
!                         INTEGER N
!
!     P --> Vector of probabilities.  P(i) is the probability that
!           an event will be classified into category i.  Thus, P(i)
!           must be [0,1]. Only the first NCAT-1 P(i) must be defined
!           since P(NCAT) is 1.0 minus the sum of the first
!           NCAT-1 P(i).
!                         REAL P(NCAT-1)
!
!     NCAT --> Number of categories.  Length of P and IX.
!                         INTEGER NCAT
!
!     IX <-- Observation from multinomial distribution.  All IX(i)
!            will be nonnegative and their sum will be N.
!                         INTEGER IX(NCAT)
!
!                              Method
!
!     Algorithm from page 559 of
!
!     Devroye, Luc
!
!     Non-Uniform Random Variate Generation.  Springer-Verlag,
!     New York, 1986.
!
!**********************************************************************
!     .. Scalar Arguments ..
      INTEGER n,ncat
!     ..
!     .. Array Arguments ..
      REAL p(*)
      INTEGER ix(*)
!     ..
!     .. Local Scalars ..
      REAL prob,ptot,sum
      INTEGER i,icat,ntot
!     ..
!     .. External Functions ..
!      INTEGER ignbin
!      EXTERNAL ignbin
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC abs
!     ..
!     .. Executable Statements ..

!     Check Arguments
      IF (n.LT.0) STOP 'N < 0 in GENMUL'
      IF (ncat.LE.1) STOP 'NCAT <= 1 in GENMUL'
      ptot = 0.0
      DO 10,i = 1,ncat - 1
          IF (p(i).LT.0.0) STOP 'Some P(i) < 0 in GENMUL'
          IF (p(i).GT.1.0) STOP 'Some P(i) > 1 in GENMUL'
          ptot = ptot + p(i)
   10 CONTINUE
      !IF (ptot.GT.0.99999) STOP 'Sum of P(i) > 1 in GENMUL'
      IF (ptot.GT.1.0) STOP 'Sum of P(i) > 1 in GENMUL'

!     Initialize variables
      ntot = n
      sum = 1.0
      DO 20,i = 1,ncat
          ix(i) = 0
   20 CONTINUE

!     Generate the observation
      DO 30,icat = 1,ncat - 1
          prob = p(icat)/sum
          ix(icat) = ignbin(ntot,prob)
          ntot = ntot - ix(icat)
!         IF (ntot.LE.0) exit
          sum = sum - p(icat)
   30 CONTINUE
      ix(ncat) = ntot

!     Finished
      RETURN

      END subroutine


INTEGER FUNCTION ignbin(n,pp)
!**********************************************************************
!
!     INTEGER FUNCTION IGNBIN( N, P )
!
!                    GENerate BINomial random deviate
!
!                              Function
!
!     Generates a single random deviate from a binomial
!     distribution whose number of trials is N and whose
!     probability of an event in each trial is P.
!
!                              Arguments
!
!     N  --> The number of trials in the binomial distribution
!            from which a random deviate is to be generated.
!                              INTEGER N
!
!     P  --> The probability of an event in each trial of the
!            binomial distribution from which a random deviate
!            is to be generated.
!                              REAL P
!
!     IGNBIN <-- A random deviate yielding the number of events
!                from N independent trials, each of which has
!                a probability of event P.
!                              INTEGER IGNBIN
!
!                              Method
!
!     This is algorithm BTPE from:
!
!         Kachitvichyanukul, V. and Schmeiser, B. W.
!
!         Binomial Random Variate Generation.
!         Communications of the ACM, 31, 2
!         (February, 1988) 216.
!
!**********************************************************************
!     SUBROUTINE BTPEC(N,PP,ISEED,JX)
!
!     BINOMIAL RANDOM VARIATE GENERATOR
!     MEAN .LT. 30 -- INVERSE CDF
!       MEAN .GE. 30 -- ALGORITHM BTPE:  ACCEPTANCE-REJECTION VIA
!       FOUR REGION COMPOSITION.  THE FOUR REGIONS ARE A TRIANGLE
!       (SYMMETRIC IN THE CENTER), A PAIR OF PARALLELOGRAMS (ABOVE
!       THE TRIANGLE), AND EXPONENTIAL LEFT AND RIGHT TAILS.
!
!     BTPE REFERS TO BINOMIAL-TRIANGLE-PARALLELOGRAM-EXPONENTIAL.
!     BTPEC REFERS TO BTPE AND "COMBINED."  THUS BTPE IS THE
!       RESEARCH AND BTPEC IS THE IMPLEMENTATION OF A COMPLETE
!       USABLE ALGORITHM.
!     REFERENCE:  VORATAS KACHITVICHYANUKUL AND BRUCE SCHMEISER,
!       "BINOMIAL RANDOM VARIATE GENERATION,"
!       COMMUNICATIONS OF THE ACM, FORTHCOMING
!     WRITTEN:  SEPTEMBER 1980.
!       LAST REVISED:  MAY 1985, JULY 1987
!     REQUIRED SUBPROGRAM:  RAND() -- A UNIFORM (0,1) RANDOM NUMBER
!                           GENERATOR
!     ARGUMENTS
!
!       N : NUMBER OF BERNOULLI TRIALS            (INPUT)
!       PP : PROBABILITY OF SUCCESS IN EACH TRIAL (INPUT)
!       ISEED:  RANDOM NUMBER SEED                (INPUT AND OUTPUT)
!       JX:  RANDOMLY GENERATED OBSERVATION       (OUTPUT)
!
!     VARIABLES
!       PSAVE: VALUE OF PP FROM THE LAST CALL TO BTPEC
!       NSAVE: VALUE OF N FROM THE LAST CALL TO BTPEC
!       XNP:  VALUE OF THE MEAN FROM THE LAST CALL TO BTPEC
!
!       P: PROBABILITY USED IN THE GENERATION PHASE OF BTPEC
!       FFM: TEMPORARY VARIABLE EQUAL TO XNP + P
!       M:  INTEGER VALUE OF THE CURRENT MODE
!       FM:  FLOATING POINT VALUE OF THE CURRENT MODE
!       XNPQ: TEMPORARY VARIABLE USED IN SETUP AND SQUEEZING STEPS
!       P1:  AREA OF THE TRIANGLE
!       C:  HEIGHT OF THE PARALLELOGRAMS
!       XM:  CENTER OF THE TRIANGLE
!       XL:  LEFT END OF THE TRIANGLE
!       XR:  RIGHT END OF THE TRIANGLE
!       AL:  TEMPORARY VARIABLE
!       XLL:  RATE FOR THE LEFT EXPONENTIAL TAIL
!       XLR:  RATE FOR THE RIGHT EXPONENTIAL TAIL
!       P2:  AREA OF THE PARALLELOGRAMS
!       P3:  AREA OF THE LEFT EXPONENTIAL TAIL
!       P4:  AREA OF THE RIGHT EXPONENTIAL TAIL
!       U:  A U(0,P4) RANDOM VARIATE USED FIRST TO SELECT ONE OF THE
!           FOUR REGIONS AND THEN CONDITIONALLY TO GENERATE A VALUE
!           FROM THE REGION
!       V:  A U(0,1) RANDOM NUMBER USED TO GENERATE THE RANDOM VALUE
!           (REGION 1) OR TRANSFORMED INTO THE VARIATE TO ACCEPT OR
!           REJECT THE CANDIDATE VALUE
!       IX:  INTEGER CANDIDATE VALUE
!       X:  PRELIMINARY CONTINUOUS CANDIDATE VALUE IN REGION 2 LOGIC
!           AND A FLOATING POINT IX IN THE ACCEPT/REJECT LOGIC
!       K:  ABSOLUTE VALUE OF (IX-M)
!       F:  THE HEIGHT OF THE SCALED DENSITY FUNCTION USED IN THE
!           ACCEPT/REJECT DECISION WHEN BOTH M AND IX ARE SMALL
!           ALSO USED IN THE INVERSE TRANSFORMATION
!       R: THE RATIO P/Q
!       G: CONSTANT USED IN CALCULATION OF PROBABILITY
!       MP:  MODE PLUS ONE, THE LOWER INDEX FOR EXPLICIT CALCULATION
!            OF F WHEN IX IS GREATER THAN M
!       IX1:  CANDIDATE VALUE PLUS ONE, THE LOWER INDEX FOR EXPLICIT
!             CALCULATION OF F WHEN IX IS LESS THAN M
!       I:  INDEX FOR EXPLICIT CALCULATION OF F FOR BTPE
!       AMAXP: MAXIMUM ERROR OF THE LOGARITHM OF NORMAL BOUND
!       YNORM: LOGARITHM OF NORMAL BOUND
!       ALV:  NATURAL LOGARITHM OF THE ACCEPT/REJECT VARIATE V
!
!       X1,F1,Z,W,Z2,X2,F2, AND W2 ARE TEMPORARY VARIABLES TO BE
!       USED IN THE FINAL ACCEPT/REJECT TEST
!
!       QN: PROBABILITY OF NO SUCCESS IN N TRIALS
!
!     REMARK
!       IX AND JX COULD LOGICALLY BE THE SAME VARIABLE, WHICH WOULD
!       SAVE A MEMORY POSITION AND A LINE OF CODE.  HOWEVER, SOME
!       COMPILERS (E.G.,CDC MNF) OPTIMIZE BETTER WHEN THE ARGUMENTS
!       ARE NOT INVOLVED.
!
!     ISEED NEEDS TO BE DOUBLE PRECISION IF THE IMSL ROUTINE
!     GGUBFS IS USED TO GENERATE UNIFORM RANDOM NUMBER, OTHERWISE
!     TYPE OF ISEED SHOULD BE DICTATED BY THE UNIFORM GENERATOR
!
!**********************************************************************
!
!
!*****DETERMINE APPROPRIATE ALGORITHM AND WHETHER SETUP IS NECESSARY
!
!     ..
!     .. Scalar Arguments ..
      REAL pp
      INTEGER n
!     ..
!     .. Local Scalars ..
      REAL al,alv,amaxp,c,f,f1,f2,ffm,fm,g,p,p1,p2,p3,p4,psave,q,qn,r,u, &
           v,w,w2,x,x1,x2,xl,xll,xlr,xm,xnp,xnpq,xr,ynorm,z,z2
      INTEGER i,ix,ix1,k,m,mp,nsave
!     ..
!     .. External Functions ..
!     REAL rand_uni01
!     EXTERNAL rand_uni01
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC abs,alog,amin1,iabs,int,sqrt
!     ..
!     .. Data statements ..
      DATA psave,nsave/-1.,-1/
!     ..
!     .. Executable Statements ..
      IF (pp.NE.psave) GO TO 10
      IF (n.NE.nsave) GO TO 20
      IF (xnp-30.) 150,30,30
!
!*****SETUP, PERFORM ONLY WHEN PARAMETERS CHANGE
!
   10 psave = pp
      p = amin1(psave,1.-psave)
      q = 1. - p
   20 xnp = n*p
      nsave = n
      IF (xnp.LT.30.) GO TO 140
      ffm = xnp + p
      m = ffm
      fm = m
      xnpq = xnp*q
      p1 = int(2.195*sqrt(xnpq)-4.6*q) + 0.5
      xm = fm + 0.5
      xl = xm - p1
      xr = xm + p1
      c = 0.134 + 20.5/ (15.3+fm)
      al = (ffm-xl)/ (ffm-xl*p)
      xll = al* (1.+.5*al)
      al = (xr-ffm)/ (xr*q)
      xlr = al* (1.+.5*al)
      p2 = p1* (1.+c+c)
      p3 = p2 + c/xll
      p4 = p3 + c/xlr
!      WRITE(6,100) N,P,P1,P2,P3,P4,XL,XR,XM,FM
!  100 FORMAT(I15,4F18.7/5F18.7)
!
!*****GENERATE VARIATE
!
   30 u = rand_uni01()*p4
      v = rand_uni01()
!
!     TRIANGULAR REGION
!
      IF (u.GT.p1) GO TO 40
      ix = xm - p1*v + u
      GO TO 170
!
!     PARALLELOGRAM REGION
!
   40 IF (u.GT.p2) GO TO 50
      x = xl + (u-p1)/c
      v = v*c + 1. - abs(xm-x)/p1
      IF (v.GT.1. .OR. v.LE.0.) GO TO 30
      ix = x
      GO TO 70
!
!     LEFT TAIL
!
   50 IF (u.GT.p3) GO TO 60
      ix = xl + alog(v)/xll
      IF (ix.LT.0) GO TO 30
      v = v* (u-p2)*xll
      GO TO 70
!
!     RIGHT TAIL
!
   60 ix = xr - alog(v)/xlr
      IF (ix.GT.n) GO TO 30
      v = v* (u-p3)*xlr
!
!*****DETERMINE APPROPRIATE WAY TO PERFORM ACCEPT/REJECT TEST
!
   70 k = iabs(ix-m)
      IF (k.GT.20 .AND. k.LT.xnpq/2-1) GO TO 130
!
!     EXPLICIT EVALUATION
!
      f = 1.0
      r = p/q
      g = (n+1)*r
      IF (m-ix) 80,120,100
   80 mp = m + 1
      DO 90 i = mp,ix
          f = f* (g/i-r)
   90 CONTINUE
      GO TO 120

  100 ix1 = ix + 1
      DO 110 i = ix1,m
          f = f/ (g/i-r)
  110 CONTINUE
  120 IF (v-f) 170,170,30
!
!     SQUEEZING USING UPPER AND LOWER BOUNDS ON ALOG(F(X))
!
  130 amaxp = (k/xnpq)* ((k* (k/3.+.625)+.1666666666666)/xnpq+.5)
      ynorm = -k*k/ (2.*xnpq)
      alv = alog(v)
      IF (alv.LT.ynorm-amaxp) GO TO 170
      IF (alv.GT.ynorm+amaxp) GO TO 30
!
!     STIRLING'S FORMULA TO MACHINE ACCURACY FOR
!     THE FINAL ACCEPTANCE/REJECTION TEST
!
      x1 = ix + 1
      f1 = fm + 1.
      z = n + 1 - fm
      w = n - ix + 1.
      z2 = z*z
      x2 = x1*x1
      f2 = f1*f1
      w2 = w*w
      IF (alv- (xm*alog(f1/x1)+ (n-m+.5)*alog(z/w)+ (ix- &
         m)*alog(w*p/ (x1*q))+ (13860.- (462.- (132.- (99.- &
         140./f2)/f2)/f2)/f2)/f1/166320.+ (13860.- (462.- (132.- (99.- &
         140./z2)/z2)/z2)/z2)/z/166320.+ (13860.- (462.- (132.- (99.-  &
         140./x2)/x2)/x2)/x2)/x1/166320.+ (13860.- (462.- (132.- (99.- &
         140./w2)/w2)/w2)/w2)/w/166320.)) 170,170,30
!
!     INVERSE CDF LOGIC FOR MEAN LESS THAN 30
!
  140 qn = q**n
      r = p/q
      g = r* (n+1)
  150 ix = 0
      f = qn
      u = rand_uni01()
  160 IF (u.LT.f) GO TO 170
      IF (ix.GT.110) GO TO 150
      u = u - f
      ix = ix + 1
      f = f* (g/ix-r)
      GO TO 160

  170 IF (psave.GT.0.5) ix = n - ix
      ignbin = ix
      RETURN

      END function
      
      
!************************************************************************
!****** Generate Random Numbers from Certain Density Type ***********
!******          Used for Generate Initial Samples          ********   
!************************************************************************      

      REAL FUNCTION genunf(low,high)
!**********************************************************************
!
!     REAL FUNCTION GENUNF( LOW, HIGH )
!
!               GeNerate Uniform Real between LOW and HIGH
!
!                              Function
!
!     Generates a real uniformly distributed between LOW and HIGH.
!
!                              Arguments
!
!     LOW --> Low bound (exclusive) on real value to be generated
!                         REAL LOW
!
!     HIGH --> High bound (exclusive) on real value to be generated
!                         REAL HIGH
!
!**********************************************************************
!     .. Scalar Arguments ..
      REAL high,low
!     ..
!     .. External Functions ..
!      REAL rand_uni01
!      EXTERNAL rand_uni01
!     ..
!     .. Executable Statements ..
      IF (.NOT. (low.GT.high)) GO TO 10
      WRITE (*,*) 'LOW > HIGH in GENUNF: LOW ',low,' HIGH: ',high
      WRITE (*,*) 'Abort'
      STOP 'LOW > High in GENUNF - Abort'

   10 genunf = low + (high-low)*rand_uni01()

      RETURN

      END FUNCTION genunf


      REAL FUNCTION gennor(av,sd)
!**********************************************************************
!
!     REAL FUNCTION GENNOR( AV, SD )
!
!         GENerate random deviate from a NORmal distribution
!
!                              Function
!
!     Generates a single random deviate from a normal distribution
!     with mean, AV, and standard deviation, SD.
!
!                              Arguments
!
!     AV --> Mean of the normal distribution.
!                              REAL AV
!
!     SD --> Standard deviation of the normal distribution.
!                              REAL SD
!
!     GENNOR <-- Generated normal deviate.
!                              REAL GENNOR
!
!                              Method
!
!     Renames SNORM from TOMS as slightly modified by BWB to use RANF
!     instead of SUNIF.
!
!     For details see:
!               Ahrens, J.H. and Dieter, U.
!               Extensions of Forsythe's Method for Random
!               Sampling from the Normal Distribution.
!               Math. Comput., 27,124 (Oct. 1973), 927 - 937.
!
!
!**********************************************************************
!     .. Scalar Arguments ..
      REAL av,sd
!     ..
!     .. External Functions ..
!     REAL snorm
!     EXTERNAL snorm
!     ..
!     .. Executable Statements ..
      gennor = sd*snorm() + av
      RETURN

      END function gennor            

      
      REAL FUNCTION snorm()
!**********************************************************************C
!                                                                      C
!     (STANDARD-)  N O R M A L  DISTRIBUTION                           C
!                                                                      C
!**********************************************************************C
!**********************************************************************C
!                                                                      C
!     FOR DETAILS SEE:                                                 C
!                                                                      C
!               AHRENS, J.H. AND DIETER, U.                            C
!               EXTENSIONS OF FORSYTHE'S METHOD FOR RANDOM             C
!               SAMPLING FROM THE NORMAL DISTRIBUTION.                 C
!               MATH. COMPUT., 27,124 (OCT. 1973), 927 - 937.          C
!                                                                      C
!     ALL STATEMENT NUMBERS CORRESPOND TO THE STEPS OF ALGORITHM 'FL'  C
!     (M=5) IN THE ABOVE PAPER     (SLIGHTLY MODIFIED IMPLEMENTATION)  C
!                                                                      C
!     Modified by Barry W. Brown, Feb 3, 1988 to use RANF instead of   C
!     SUNIF.  The argument IR thus goes away.                          C
!                                                                      C
!**********************************************************************C
!
      real a,d,t,h,u,s,ustar,aa,w,y,tt
      integer i

      DIMENSION a(32),d(31),t(31),h(31)
!
!     THE DEFINITIONS OF THE CONSTANTS A(K), D(K), T(K) AND
!     H(K) ARE ACCORDING TO THE ABOVEMENTIONED ARTICLE
!
      DATA a/0.0,.3917609E-1,.7841241E-1,.1177699,.1573107,.1970991, &
          .2372021,.2776904,.3186394,.3601299,.4022501,.4450965, &
          .4887764,.5334097,.5791322,.6260990,.6744898,.7245144, &
          .7764218,.8305109,.8871466,.9467818,1.009990,1.077516, &
          1.150349,1.229859,1.318011,1.417797,1.534121,1.675940, &
          1.862732,2.153875/
      DATA d/5*0.0,.2636843,.2425085,.2255674,.2116342,.1999243, &
          .1899108,.1812252,.1736014,.1668419,.1607967,.1553497, &
          .1504094,.1459026,.1417700,.1379632,.1344418,.1311722, &
          .1281260,.1252791,.1226109,.1201036,.1177417,.1155119, &
          .1134023,.1114027,.1095039/
      DATA t/.7673828E-3,.2306870E-2,.3860618E-2,.5438454E-2, &
          .7050699E-2,.8708396E-2,.1042357E-1,.1220953E-1,.1408125E-1, &
          .1605579E-1,.1815290E-1,.2039573E-1,.2281177E-1,.2543407E-1, &
          .2830296E-1,.3146822E-1,.3499233E-1,.3895483E-1,.4345878E-1, &
          .4864035E-1,.5468334E-1,.6184222E-1,.7047983E-1,.8113195E-1, &
          .9462444E-1,.1123001,.1364980,.1716886,.2276241,.3304980,    &
          .5847031/
      DATA h/.3920617E-1,.3932705E-1,.3950999E-1,.3975703E-1, &
          .4007093E-1,.4045533E-1,.4091481E-1,.4145507E-1,.4208311E-1, &
          .4280748E-1,.4363863E-1,.4458932E-1,.4567523E-1,.4691571E-1, &
          .4833487E-1,.4996298E-1,.5183859E-1,.5401138E-1,.5654656E-1, &
          .5953130E-1,.6308489E-1,.6737503E-1,.7264544E-1,.7926471E-1, &
          .8781922E-1,.9930398E-1,.1155599,.1404344,.1836142,.2790016, &
          .7010474/
!
   10 u = rand_uni01()
      s = 0.0
      IF (u.GT.0.5) s = 1.0
      u = u + u - s
   20 u = 32.0*u
      i = int(u)
      IF (i.EQ.32) i = 31
      IF (i.EQ.0) GO TO 100
!
!                                START CENTER
!
   30 ustar = u - float(i)
      aa = a(i)
   40 IF (ustar.LE.t(i)) GO TO 60
      w = (ustar-t(i))*h(i)
!
!                                EXIT   (BOTH CASES)
!
   50 y = aa + w
      snorm = y
      IF (s.EQ.1.0) snorm = -y
      RETURN
!
!                                CENTER CONTINUED
!
   60 u = rand_uni01()
      w = u* (a(i+1)-aa)
      tt = (0.5*w+aa)*w
      GO TO 80

   70 tt = u
      ustar = rand_uni01()
   80 IF (ustar.GT.tt) GO TO 50
   90 u = rand_uni01()
      IF (ustar.GE.u) GO TO 70
      ustar = rand_uni01()
      GO TO 40
!
!                                START TAIL
!
  100 i = 6
      aa = a(32)
      GO TO 120

  110 aa = aa + d(i)
      i = i + 1
  120 u = u + u
      IF (u.LT.1.0) GO TO 110
  130 u = u - 1.0
  140 w = u*d(i)
      tt = (0.5*w+aa)*w
      GO TO 160

  150 tt = u
  160 ustar = rand_uni01()
      IF (ustar.GT.tt) GO TO 50
  170 u = rand_uni01()
      IF (ustar.GE.u) GO TO 150
      u = rand_uni01()
      GO TO 140

      END function snorm
      

      SUBROUTINE setgmn(p,meanv,covm,parm,logdet)
!***********************************************************************
!
!     SUBROUTINE SETGMN( MEANV, COVM, P, PARM)
!            SET Generate Multivariate Normal random deviate
!            Used by the following subrountine GENMN
!
!                              Function
!
!      Places P, MEANV, and the Cholesky factoriztion of COVM
!      in GENMN.
!
!                              Arguments
!
!     P     --> Dimension of the normal, or length of MEANV.
!                                        INTEGER P
!
!     MEANV --> Mean vector of multivariate normal distribution.
!                                        REAL MEANV(P)
!
!     COVM   <--> (Input) Covariance   matrix    of  the  multivariate
!                 normal distribution
!                 (Output) Destroyed on output
!                                        REAL COVM(P,P)
!
!     PARM <-- Array of parameters needed to generate multivariate norma
!                deviates (P, MEANV and Cholesky decomposition of
!                COVM).
!                1 : 1                - P
!                2 : P + 1            - MEANV
!                P+2 : P*(P+3)/2 + 1  - Cholesky decomposition of COVM
!                                             REAL PARM(P*(P+3)/2 + 1)
!
!**********************************************************************
!     .. Scalar Arguments ..
      INTEGER p
!     ..
!     .. Array Arguments ..
      REAL covm(p,p),meanv(p),parm(p* (p+3)/2+1)
      real det(2)
      real logdet
!     ..
!     .. Local Scalars ..
      INTEGER i,icount,info,j
!     ..
!     .. External Subroutines ..
      EXTERNAL spofa
!     ..
!     TEST THE INPUT
!
      IF (.NOT. (p.LE.0)) GO TO 10
      WRITE (*,*) 'P nonpositive in SETGMN'
      WRITE (*,*) 'Value of P: ',p
      STOP 'P nonpositive in SETGMN'

   10 parm(1) = p
!
!     PUT P AND MEANV INTO PARM
!
      DO 20,i = 2,p + 1
          parm(i) = meanv(i-1)
   20 CONTINUE
!
!      Cholesky decomposition to find A s.t. trans(A)*(A) = COVM
!
      CALL spofa(covm,p,p,info)
      IF (.NOT. (info.NE.0)) GO TO 30
      WRITE (*,*) ' Supplied covariance matrix is not positive definite'
      STOP ' COVM not positive definite in SETGMN'

   30 icount = p + 1
!
!     PUT UPPER HALF OF A, WHICH IS NOW THE CHOLESKY FACTOR, INTO PARM
!          COVM(1,1) = PARM(P+2)
!          COVM(1,2) = PARM(P+3)
!                    :
!          COVM(1,P) = PARM(2P+1)
!          COVM(2,2) = PARM(2P+2)  ...
!
      DO 50,i = 1,p
          DO 40,j = i,p
              icount = icount + 1
              parm(icount) = covm(i,j)
   40     CONTINUE
   50 CONTINUE
   
! -- The following code has been added to calculate the inverse and the determinant 
!    of the covariance matrix. Subroutine spodi in linpack produces the upper half of inverse (covm) 

      call spodi(covm,p,p,det,11) 
      if(det(1).eq.0.0)then
        logdet=0.0
      else
        logdet=log(det(1) * 10.0**det(2))  !modified by dan since det of cov matrix is too small 
      end if
   
      RETURN
!
      END SUBROUTINE setgmn
        
     
     SUBROUTINE genmn(parm,x,work)
!**********************************************************************
!
!     SUBROUTINE GENMN(PARM,X,WORK)
!              GENerate Multivariate Normal random deviate
!
!                              Arguments
!
!     PARM --> Parameters needed to generate multivariate normal
!               deviates (MEANV and Cholesky decomposition of
!               COVM). Set by a previous call to SETGMN.
!               1 : 1                - size of deviate, P
!               2 : P + 1            - mean vector
!               P+2 : P*(P+3)/2 + 1  - upper half of cholesky
!                                       decomposition of cov matrix
!                                             REAL PARM(*)
!
!     X    <-- Vector deviate generated.
!                                             REAL X(P)
!
!     WORK <--> Scratch array
!                                             REAL WORK(P)
!
!                              Method
!
!     1) Generate P independent standard normal deviates - Ei ~ N(0,1)
!
!     2) Using Cholesky decomposition find A s.t. trans(A)*A = COVM
!
!     3) trans(A)E + MEANV ~ N(MEANV,COVM)
!
!**********************************************************************
!     .. Array Arguments ..
      REAL parm(*),work(*),x(*)
!     ..
!     .. Local Scalars ..
      REAL ae
      INTEGER i,icount,j,p
!     ..
!     .. External Functions ..
!      REAL snorm
!      EXTERNAL snorm
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC int
!     ..
!     .. Executable Statements ..
      p = int(parm(1))
!
!     Generate P independent normal deviates - WORK ~ N(0,1)
!
      DO 10,i = 1,p
          work(i) = snorm()
   10 CONTINUE
      DO 30,i = 1,p
!
!     PARM (P+2 : P*(P+3)/2 + 1) contains A, the Cholesky
!      decomposition of the desired covariance matrix.
!          trans(A)(1,1) = PARM(P+2)
!          trans(A)(2,1) = PARM(P+3)
!          trans(A)(2,2) = PARM(P+2+P)
!          trans(A)(3,1) = PARM(P+4)
!          trans(A)(3,2) = PARM(P+3+P)
!          trans(A)(3,3) = PARM(P+2-1+2P)  ...
!
!     trans(A)*WORK + MEANV ~ N(MEANV,COVM)
!
          icount = 0
          ae = 0.0
          DO 20,j = 1,i
              icount = icount + j - 1
              ae = ae + parm(i+ (j-1)*p-icount+p+1)*work(j)
   20     CONTINUE
          x(i) = ae + parm(i+1)
   30 CONTINUE
      RETURN
!
      END SUBROUTINE genmn      
      

      REAL FUNCTION genbet(aa,bb)
!**********************************************************************
!
!     REAL FUNCTION GENBET( A, B )
!               GeNerate BETa random deviate
!
!                              Function
!
!     Returns a single random deviate from the beta distribution with
!     parameters A and B.  The density of the beta is
!               x^(a-1) * (1-x)^(b-1) / B(a,b) for 0 < x < 1
!
!                              Arguments
!
!     A --> First parameter of the beta distribution
!                         REAL A
!
!     B --> Second parameter of the beta distribution
!                         REAL B
!
!                              Method
!
!     R. C. H. Cheng
!     Generating Beta Variatew with Nonintegral Shape Parameters
!     Communications of the ACM, 21:317-322  (1978)
!     (Algorithms BB and BC)
!
!**********************************************************************
!     .. Parameters ..
!     Close to the largest number that can be exponentiated
      REAL expmax
      PARAMETER (expmax=89.0)
!     Close to the largest representable single precision number
      REAL infnty
      PARAMETER (infnty=1.0E38)
!     ..
!     .. Scalar Arguments ..
      REAL aa,bb
!     ..
!     .. Local Scalars ..
      REAL a,alpha,b,beta,delta,gamma,k1,k2,olda,oldb,r,s,t,u1,u2,v,w,y,z
      LOGICAL qsame
!     ..
!     .. External Functions ..
!      REAL rand_uni01
!      EXTERNAL rand_uni01
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC exp,log,max,min,sqrt
!     ..
!     .. Save statement ..
      SAVE olda,oldb,alpha,beta,gamma,k1,k2
!     ..
!     .. Data statements ..
      DATA olda,oldb/-1,-1/
!     ..
!     .. Executable Statements ..
      qsame = (olda.EQ.aa) .AND. (oldb.EQ.bb)
      IF (qsame) GO TO 20
      IF (.NOT. (aa.LE.0.0.OR.bb.LE.0.0)) GO TO 10
      WRITE (*,*) ' AA or BB <= 0 in GENBET - Abort!'
      WRITE (*,*) ' AA: ',aa,' BB ',bb
      STOP ' AA or BB <= 0 in GENBET - Abort!'

!     modified from olda = aa, oldb = bb
   10 olda = -1
      oldb = -1
   20 IF (.NOT. (min(aa,bb).GT.1.0)) GO TO 100


!     Alborithm BB

!
!     Initialize
!
      IF (qsame) GO TO 30
      a = min(aa,bb)
      b = max(aa,bb)
      alpha = a + b
      beta = sqrt((alpha-2.0)/ (2.0*a*b-alpha))
      gamma = a + 1.0/beta
   30 CONTINUE
   40 u1 = rand_uni01()
!
!     Step 1
!
      u2 = rand_uni01()
      v = beta*log(u1/ (1.0-u1))
      IF (.NOT. (v.GT.expmax)) GO TO 50
      w = infnty
      GO TO 60

   50 w = a*exp(v)
   60 z = u1**2*u2
      r = gamma*v - 1.3862944
      s = a + r - w
!
!     Step 2
!
      IF ((s+2.609438).GE. (5.0*z)) GO TO 70
!
!     Step 3
!
      t = log(z)
      IF (s.GT.t) GO TO 70
!
!     Step 4
!
      IF ((r+alpha*log(alpha/ (b+w))).LT.t) GO TO 40
!
!     Step 5
!
   70 IF (.NOT. (aa.EQ.a)) GO TO 80
      genbet = w/ (b+w)
      GO TO 90

   80 genbet = b/ (b+w)
   90 GO TO 230


!     Algorithm BC

!
!     Initialize
!
  100 IF (qsame) GO TO 110
      a = max(aa,bb)
      b = min(aa,bb)
      alpha = a + b
      beta = 1.0/b
      delta = 1.0 + a - b
      k1 = delta* (0.0138889+0.0416667*b)/ (a*beta-0.777778)
      k2 = 0.25 + (0.5+0.25/delta)*b
  110 CONTINUE
  120 u1 = rand_uni01()
!
!     Step 1
!
      u2 = rand_uni01()
      IF (u1.GE.0.5) GO TO 130
!
!     Step 2
!
      y = u1*u2
      z = u1*y
      IF ((0.25*u2+z-y).GE.k1) GO TO 120
      GO TO 170
!
!     Step 3
!
  130 z = u1**2*u2
      IF (.NOT. (z.LE.0.25)) GO TO 160
      v = beta*log(u1/ (1.0-u1))
      IF (.NOT. (v.GT.expmax)) GO TO 140
      w = infnty
      GO TO 150

  140 w = a*exp(v)
  150 GO TO 200

  160 IF (z.GE.k2) GO TO 120
!
!     Step 4
!
!
!     Step 5
!
  170 v = beta*log(u1/ (1.0-u1))
      IF (.NOT. (v.GT.expmax)) GO TO 180
      w = infnty
      GO TO 190

  180 w = a*exp(v)
  190 IF ((alpha* (log(alpha/ (b+w))+v)-1.3862944).LT.log(z)) GO TO 120
!
!     Step 6
!
  200 IF (.NOT. (a.EQ.aa)) GO TO 210
      genbet = w/ (b+w)
      GO TO 220

  210 genbet = b/ (b+w)
  220 CONTINUE
  230 RETURN

      END FUNCTION 

!----------------------------------------------------------------------------------------

      REAL FUNCTION gengam(a,r)
!**********************************************************************
!
!     REAL FUNCTION GENGAM( A, R )
!           GENerates random deviates from GAMma distribution
!
!                              Function
!
!     Generates random deviates from the gamma distribution whose
!     density is
!          (A**R)/Gamma(R) * X**(R-1) * Exp(-A*X)
!
!                              Arguments
!
!     A --> Location parameter of Gamma distribution
!                              REAL A
!
!     R --> Shape parameter of Gamma distribution
!                              REAL R
!
!                              Method
!
!     Renames SGAMMA from TOMS as slightly modified by BWB to use RANF
!     instead of SUNIF.
!
!     For details see:
!               (Case R >= 1.0)
!               Ahrens, J.H. and Dieter, U.
!               Generating Gamma Variates by a
!               Modified Rejection Technique.
!               Comm. ACM, 25,1 (Jan. 1982), 47 - 54.
!     Algorithm GD
!
!               (Case 0.0 <= R <= 1.0)
!               Ahrens, J.H. and Dieter, U.
!               Computer Methods for Sampling from Gamma,
!               Beta, Poisson and Binomial Distributions.
!               Computing, 12 (1974), 223-246/
!     Adapted algorithm GS.
!
!**********************************************************************
!     .. Scalar Arguments ..
      REAL a,r
!     ..
!     .. External Functions ..
!      REAL sgamma
!      EXTERNAL sgamma
!     ..
!     .. Executable Statements ..
      gengam = sgamma(r)
      gengam = gengam/a
      RETURN

      END FUNCTION gengam


      REAL FUNCTION sgamma(a)
!**********************************************************************C
!                                                                      C
!     (STANDARD-)  G A M M A  DISTRIBUTION                             C
!                                                                      C
!**********************************************************************C
!**********************************************************************C
!                                                                      C
!               PARAMETER  A >= 1.0  !                                 C
!                                                                      C
!**********************************************************************C
!                                                                      C
!     FOR DETAILS SEE:                                                 C
!                                                                      C
!               AHRENS, J.H. AND DIETER, U.                            C
!               GENERATING GAMMA VARIATES BY A                         C
!               MODIFIED REJECTION TECHNIQUE.                          C
!               COMM. ACM, 25,1 (JAN. 1982), 47 - 54.                  C
!                                                                      C
!     STEP NUMBERS CORRESPOND TO ALGORITHM 'GD' IN THE ABOVE PAPER     C
!                                 (STRAIGHTFORWARD IMPLEMENTATION)     C
!                                                                      C
!     Modified by Barry W. Brown, Feb 3, 1988 to use RANF instead of   C
!     SUNIF.  The argument IR thus goes away.                          C
!                                                                      C
!**********************************************************************C
!                                                                      C
!               PARAMETER  0.0 < A < 1.0  !                            C
!                                                                      C
!**********************************************************************C
!                                                                      C
!     FOR DETAILS SEE:                                                 C
!                                                                      C
!               AHRENS, J.H. AND DIETER, U.                            C
!               COMPUTER METHODS FOR SAMPLING FROM GAMMA,              C
!               BETA, POISSON AND BINOMIAL DISTRIBUTIONS.              C
!               COMPUTING, 12 (1974), 223 - 246.                       C
!                                                                      C
!     (ADAPTED IMPLEMENTATION OF ALGORITHM 'GS' IN THE ABOVE PAPER)    C
!                                                                      C
!**********************************************************************C
!
!
!     INPUT: A =PARAMETER (MEAN) OF THE STANDARD GAMMA DISTRIBUTION
!     OUTPUT: SGAMMA = SAMPLE FROM THE GAMMA-(A)-DISTRIBUTION
!
!     COEFFICIENTS Q(K) - FOR Q0 = SUM(Q(K)*A**(-K))
!     COEFFICIENTS A(K) - FOR Q = Q0+(T*T/2)*SUM(A(K)*V**K)
!     COEFFICIENTS E(K) - FOR EXP(Q)-1 = SUM(E(K)*Q**K)
!
      real a
      real q1,q2,q3,q4,q5,q6,q7, a1,a2,a3,a4,a5,a6,a7, e1,e2,e3,e4,e5
      real aa,aaa,sqrt32
      real s2,s,d,t,x,u,r,q0,b,si,c,v,q,e,w,p

      DATA q1,q2,q3,q4,q5,q6,q7/.04166669,.02083148,.00801191,.00144121,  &
           -.00007388,.00024511,.00024240/
      DATA a1,a2,a3,a4,a5,a6,a7/.3333333,-.2500030,.2000062,-.1662921,    &
           .1423657,-.1367177,.1233795/
      DATA e1,e2,e3,e4,e5/1.,.4999897,.1668290,.0407753,.0102930/
!
!     PREVIOUS A PRE-SET TO ZERO - AA IS A', AAA IS A"
!     SQRT32 IS THE SQUAREROOT OF 32 = 5.656854249492380
!
      DATA aa/0.0/,aaa/0.0/,sqrt32/5.656854/
!
!     SAVE STATEMENTS
!
      SAVE aa,aaa,s2,s,d,q0,b,si,c
!
      IF (a.EQ.aa) GO TO 10
      IF (a.LT.1.0) GO TO 140
!
!     STEP  1:  RECALCULATIONS OF S2,S,D IF A HAS CHANGED
!
      aa = a
      s2 = a - 0.5
      s = sqrt(s2)
      d = sqrt32 - 12.0*s
!
!     STEP  2:  T=STANDARD NORMAL DEVIATE,
!               X=(S,1/2)-NORMAL DEVIATE.
!               IMMEDIATE ACCEPTANCE (I)
!
   10 t = snorm()
      x = s + 0.5*t
      sgamma = x*x
      IF (t.GE.0.0) RETURN
!
!     STEP  3:  U= 0,1 -UNIFORM SAMPLE. SQUEEZE ACCEPTANCE (S)
!
      u = rand_uni01()
      IF (d*u.LE.t*t*t) RETURN
!
!     STEP  4:  RECALCULATIONS OF Q0,B,SI,C IF NECESSARY
!
      IF (a.EQ.aaa) GO TO 40
      aaa = a
      r = 1.0/a
      q0 = ((((((q7*r+q6)*r+q5)*r+q4)*r+q3)*r+q2)*r+q1)*r
!
!               APPROXIMATION DEPENDING ON SIZE OF PARAMETER A
!               THE CONSTANTS IN THE EXPRESSIONS FOR B, SI AND
!               C WERE ESTABLISHED BY NUMERICAL EXPERIMENTS
!
      IF (a.LE.3.686) GO TO 30
      IF (a.LE.13.022) GO TO 20
!
!               CASE 3:  A .GT. 13.022
!
      b = 1.77
      si = .75
      c = .1515/s
      GO TO 40
!
!               CASE 2:  3.686 .LT. A .LE. 13.022
!
   20 b = 1.654 + .0076*s2
      si = 1.68/s + .275
      c = .062/s + .024
      GO TO 40
!
!               CASE 1:  A .LE. 3.686
!
   30 b = .463 + s + .178*s2
      si = 1.235
      c = .195/s - .079 + .16*s
!
!     STEP  5:  NO QUOTIENT TEST IF X NOT POSITIVE
!
   40 IF (x.LE.0.0) GO TO 70
!
!     STEP  6:  CALCULATION OF V AND QUOTIENT Q
!
      v = t/ (s+s)
      IF (abs(v).LE.0.25) GO TO 50
      q = q0 - s*t + 0.25*t*t + (s2+s2)*alog(1.0+v)
      GO TO 60

   50 q = q0 + 0.5*t*t* ((((((a7*v+a6)*v+a5)*v+a4)*v+a3)*v+a2)*v+a1)*v
!
!     STEP  7:  QUOTIENT ACCEPTANCE (Q)
!
   60 IF (alog(1.0-u).LE.q) RETURN
!
!     STEP  8:  E=STANDARD EXPONENTIAL DEVIATE
!               U= 0,1 -UNIFORM DEVIATE
!               T=(B,SI)-DOUBLE EXPONENTIAL (LAPLACE) SAMPLE
!
   70 e = sexpo()
      u = rand_uni01()
      u = u + u - 1.0
      t = b + sign(si*e,u)
      IF (.NOT. (u.GE.0.0)) GO TO 80
      t = b + si*e
      GO TO 90

   80 t = b - si*e

!
!     STEP  9:  REJECTION IF T .LT. TAU(1) = -.71874483771719
!
   90 IF (t.LT. (-.7187449)) GO TO 70
!
!     STEP 10:  CALCULATION OF V AND QUOTIENT Q
!
      v = t/ (s+s)
      IF (abs(v).LE.0.25) GO TO 100
      q = q0 - s*t + 0.25*t*t + (s2+s2)*alog(1.0+v)
      GO TO 110

  100 q = q0 + 0.5*t*t* ((((((a7*v+a6)*v+a5)*v+a4)*v+a3)*v+a2)*v+a1)*v
!
!     STEP 11:  HAT ACCEPTANCE (H) (IF Q NOT POSITIVE GO TO STEP 8)
!
  110 IF (q.LE.0.0) GO TO 70
      IF (q.LE.0.5) GO TO 120
      w = exp(q) - 1.0
      GO TO 130

  120 w = ((((e5*q+e4)*q+e3)*q+e2)*q+e1)*q
!
!               IF T IS REJECTED, SAMPLE AGAIN AT STEP 8
!
  130 IF (c*abs(u).GT.w*exp(e-0.5*t*t)) GO TO 70
      x = s + 0.5*t
      sgamma = x*x
      RETURN
!
!     ALTERNATE METHOD FOR PARAMETERS A BELOW 1  (.3678794=EXP(-1.))
!
  140 aa = 0.0
      b = 1.0 + .3678794*a
  150 p = b*rand_uni01()
      IF (p.GE.1.0) GO TO 160
      sgamma = exp(alog(p)/a)
      IF (sexpo().LT.sgamma) GO TO 150
      RETURN

  160 sgamma = -alog((b-p)/a)
      IF (sexpo().LT. (1.0-a)*alog(sgamma)) GO TO 150
      RETURN

      END FUNCTION sgamma      


      REAL FUNCTION genchi(df)
!**********************************************************************
!
!     REAL FUNCTION GENCHI( DF )
!                Generate random value of CHIsquare variable
!
!                              Function
!
!     Generates random deviate from the distribution of a chisquare
!     with DF degrees of freedom random variable.
!
!                              Arguments
!
!     DF --> Degrees of freedom of the chisquare
!            (Must be positive)
!                         REAL DF
!
!                              Method
!
!     Uses relation between chisquare and gamma.
!
!**********************************************************************
!     .. Scalar Arguments ..
      REAL df
!     ..
!     .. External Functions ..
!      REAL gengam
!      EXTERNAL gengam
!     ..
!     .. Executable Statements ..
      IF (.NOT. (df.LE.0.0)) GO TO 10
      WRITE (*,*) 'DF <= 0 in GENCHI - ABORT'
      WRITE (*,*) 'Value of DF: ',df
      STOP 'DF <= 0 in GENCHI - ABORT'

   10 genchi = 2.0*gengam(1.0,df/2.0)
      RETURN

      END FUNCTION genchi

      REAL FUNCTION sexpo()
!**********************************************************************C
!                                                                      C
!     (STANDARD-)  E X P O N E N T I A L   DISTRIBUTION                C
!                                                                      C
!**********************************************************************C
!**********************************************************************C
!                                                                      C
!     FOR DETAILS SEE:                                                 C
!                                                                      C
!               AHRENS, J.H. AND DIETER, U.                            C
!               COMPUTER METHODS FOR SAMPLING FROM THE                 C
!               EXPONENTIAL AND NORMAL DISTRIBUTIONS.                  C
!               COMM. ACM, 15,10 (OCT. 1972), 873 - 882.               C
!                                                                      C
!     ALL STATEMENT NUMBERS CORRESPOND TO THE STEPS OF ALGORITHM       C
!     'SA' IN THE ABOVE PAPER (SLIGHTLY MODIFIED IMPLEMENTATION)       C
!                                                                      C
!     Modified by Barry W. Brown, Feb 3, 1988 to use RANF instead of   C
!     SUNIF.  The argument IR thus goes away.                          C
!                                                                      C
!**********************************************************************C
!
      real q,q1
      DIMENSION q(8)
      EQUIVALENCE (q(1),q1)
      real a,u,ustar,umin
      integer i
       
!
!     Q(N) = SUM(ALOG(2.0)**K/K!)    K=1,..,N ,      THE HIGHEST N
!     (HERE 8) IS DETERMINED BY Q(N)=1.0 WITHIN STANDARD PRECISION
!
      DATA q/.6931472,.9333737,.9888778,.9984959,.9998293,.9999833,   &
           .9999986,.9999999/
!
   10 a = 0.0
      u = rand_uni01()
      GO TO 30

   20 a = a + q1
   30 u = u + u
      IF (u.LE.1.0) GO TO 20
   40 u = u - 1.0
      IF (u.GT.q1) GO TO 60
   50 sexpo = a + u
      RETURN

   60 i = 1
      ustar = rand_uni01()
      umin = ustar
   70 ustar = rand_uni01()
      IF (ustar.LT.umin) umin = ustar
   80 i = i + 1
      IF (u.GT.q(i)) GO TO 70
   90 sexpo = a + umin*q1
      RETURN

      END FUNCTION sexpo

      
      REAL FUNCTION genexp(av)

!**********************************************************************
!
!     REAL FUNCTION GENEXP( AV )
!
!                    GENerate EXPonential random deviate
!
!                              Function
!
!     Generates a single random deviate from an exponential
!     distribution with mean AV.
!
!                              Arguments
!
!     AV --> The mean of the exponential distribution from which
!            a random deviate is to be generated.
!                              REAL AV
!
!     GENEXP <-- The random deviate.
!                              REAL GENEXP
!
!                              Method
!
!     Renames SEXPO from TOMS as slightly modified by BWB to use RANF
!     instead of SUNIF.
!
!     For details see:
!
!               Ahrens, J.H. and Dieter, U.
!               Computer Methods for Sampling From the
!               Exponential and Normal Distributions.
!               Comm. ACM, 15,10 (Oct. 1972), 873 - 882.
!
!**********************************************************************
!     .. Scalar Arguments ..
      REAL av
!     ..
!     .. External Functions ..
!      REAL sexpo
!      EXTERNAL sexpo
!     ..
!     .. Executable Statements ..

      genexp = sexpo()*av
      RETURN

      END FUNCTION GENEXP

end module

