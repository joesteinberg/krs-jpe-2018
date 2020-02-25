module calibrate

  use globals
  use csv_file
  implicit none

  ! ///////////////////////////////////////////////////////////////////////
  ! model parameters
  ! ///////////////////////////////////////////////////////////////////////

  ! ......................................................................
  ! retailer parameters
  ! ......................................................................

  ! combine final demand from different sectors
  real(DP), dimension(NF) :: rho ! elasticity
  real(DP), dimension(NC,NF,NS) :: eps ! shares
  real(DP), dimension(NC,NF) :: G ! scale factor

  ! combine final demand from different countries into sector-specific bundles
  real(DP), dimension(NS) :: sig ! elasticity (allow to differ across sectors!)
  real(DP), dimension(NC,NF,NS,NC) :: theta ! shares
  real(DP), dimension(NC,NF,NS) :: H ! scale factor
  
  ! ......................................................................
  ! gross output parameters
  ! ......................................................................

  ! combine value added and intermediate bundle
  real(DP) :: eta ! elasticity
  real(DP), dimension(NC,NS) :: lam ! share
  real(DP), dimension(NC,NS) :: A ! scale factor

  ! value added
  real(DP), dimension(NS) :: alpha
  real(DP), dimension(NC,NS) :: B ! scale factor
  real(DP), dimension(NC,NS) :: ggam ! labor productivity growth rates from data
  real(DP) :: gbgp ! unbiased long-term productivity growth rate
  real(DP) :: sc_speed ! convergence speed for productivity

  ! combine intermediate bundles from different sectors
  real(DP) :: xi ! elasticity
  real(DP), dimension(NC,NS,NS) :: pi ! shares
  real(DP), dimension(NC,NS) :: C ! scale factor

  ! combine intermediates from different countries into sector-specific bundles
  real(DP), dimension(NS) :: zeta ! elasticity (allow to differ across sectors!)
  real(DP), dimension(NC,NS,NS,NC) :: mu ! shares
  real(DP), dimension(NC,NS,NS) :: D ! scale factors

  ! ......................................................................
  ! households
  ! ......................................................................
  
  ! capital formation
  real(DP) :: delta ! depreciation rate
  real(DP) :: tauk, tauk1 ! capital tax rate
  real(DP) :: rbgp ! balanced growth path real interest rate
  real(DP) :: etaK = 0.65 ! concavity in capital production with Prescott and Lucas (1971) capital formation
  
  ! household preferences
  real(DP), dimension(NC) :: beta ! discount factors
  real(DP) :: psi ! intertemporal elasticity
  real(DP), dimension(NC) :: phi ! consumption share
  real(DP) :: om_speed, om_speed_ss ! convergence rate for discount factor wedge
  real(DP) :: ag_frac = 0.0460179_dp ! fraction of agriculture in US goods cons in 1995 from WIOT
  !real(DP) :: cbar_ag_ratio = 0.67, cbar_s_ratio = 0.73 ! linear weighted average of 1947 and 2010
  real(DP) :: cbar_ag_ratio = 0.47, cbar_s_ratio = 0.25 ! linear weighted average of 1947 and 2010
  real(DP), dimension(NC,2) :: cbar0 ! Stone-Geary preference parameters

  ! ......................................................................
  !  time series parameters
   ! ......................................................................
  real(DP), dimension(3,NT+1) :: cg_pct_ts, bg_pct_ts
  real(DP), dimension(NC,NT+1) :: popt_ts, popw_ts, pope_ts, lbar_ts ! demographic data
  real(DP), dimension(TSG1) :: tbdata_ts, rerdata_ts, lgdata_ts ! trade balance, RER, goods labor share data
  real(DP), dimension(NC,NS,NT+1) :: gam_t ! sector-level productivities
  real(DP), dimension(NC,2,NT+1) :: cbar_ts

  ! ......................................................................
  ! base-period equilibrium values
  ! ......................................................................
  real(DP), dimension(NS1(1)+NS1(2)+2,NS1(1)+NS1(2)+NF1(1)+NF1(2)+1) :: iomat
  real(DP) :: kk0, b0, r0, bg0, agg_cap_share
  real(DP), dimension(NC) :: ii0, g0, ll0, lbar0
  real(DP), dimension(NC,NS) :: y0, c0, va0, k0, l0, m0, ex0, im0, nx0
  real(DP), dimension(NC,NF) :: q0
  real(DP), dimension(NC,NS,NS) :: m01
  real(DP), dimension(NC,NS,NS,NC) :: m02
  real(DP), dimension(NC,NF,NS) :: q01
  real(DP), dimension(NC,NF,NS,NC) :: q02
  real(DP), dimension(NS1(1)) :: lshare0

contains 

  ! ///////////////////////////////////////////////////////////////////////
  ! set some initial conditions and other non-targeted parameters
  ! ///////////////////////////////////////////////////////////////////////
  subroutine set_nontargeted_params()

    implicit none
    
    ! initial conditions
    kk0 = 277.8927_dp ! initial capital stock
    bg0 = -48.05711_dp ! initial gov't bonds
    b0 = -6.60818_dp - bg0 ! calibrate private bonds so public + private matches NFA in 1990
    agg_cap_share = 0.339029 ! aggregate capital share from NIPA
    lshare0(1) = 0.196896
    lshare0(2) = 0.759383
    lshare0(3) = ONE - lshare0(1) - lshare0(2) ! sector shares of labor comp from, Industry Value Added data

    ! balanced growth stuff and other things
    rbgp = 0.03_dp ! balanced growth interest rate: calibrate to bgp capital/output
    gbgp = 1.02_dp ! unbiased long-term productivity growth
    sc_speed = 0.75_dp ! persistence of biased growth
    delta=0.052811_dp ! depreciation
    tauk = 0.39_dp ! statutory rate from Devereux et al (2002)
    tauk1 = tauk ! initialize second-period tax to long-run value

    ! capital shares: now come from IO matrix
    !alpha(:)  = 0.32

    ! productivity growth rates
    if(asym_growth_flag .eq. 0) then
       ggam(:,:) = gbgp
    else
       ! these three come from the data for the United States
       !ggam(1,1) = 1.044152371_dp
       !ggam(1,2) = 1.013035010_dp
       !ggam(1,3) = 0.991557198_dp

       ggam(1,1) = 1.0557_dp
       ggam(1,2) = 1.0082_dp
       ggam(1,3) = 0.985773_dp
       
       ggam(2,1) = 1.035_dp
       ggam(2,2) = 1.009_dp
    endif

    if(homotopy_flag .eq. 1) then
       ggam(:,:) = 0.95*ggam(:,:) + 0.05*gbgp
    endif

    ! final use elasticities (across sectors)
    rho(1) = ONE - ONE/0.65_dp ! consumption (same for pub and priv):
    rho(2) = ONE - ONE/0.65_dp ! to match 1992-2011 change in goods share of pub+priv consumption
    rho(3) = ONE - ONE/0.999_dp ! investment: to match 1992-2011 change in goods share of inv

    if(noio_flag .eq. 1 .and. noio_leontief_flag .eq. 1) then
       !rho(1) = ONE - ONE/0.034_dp
       !rho(2) = ONE - ONE/0.034_dp
       rho(1:2) = ONE - ONE/0.034_dp
    endif

    ! final use elasticities (across countries)
    sig(1) = ONE - ONE/2.0_dp ! goods: to match 1992-2011 home share of goods final demand
    sig(2) = ONE - ONE/1.01_dp ! services: to match 1992-2006 change in home share of services final demand
    sig(3) = ONE ! construction not traded

    ! production elasticity (value added-intermediates)
    eta = ONE - ONE/0.046_dp ! to match 1992-2011 change in intermediate share of gross output
    ! compare to 0.046 (Enghin), 0.9 from Oberfield and Raval
    
    ! Cobb-Douglas sensitivity 1: value added and intermediates
    if(cobb1_flag .eq. 1) then
       eta = ONE - ONE/0.99_dp
    endif

    ! production elasticity (intermediates across sectors)
    xi = ONE - ONE/0.034_dp ! to match 1992-2011 change in goods share of intermediates
    ! compare to 0.034 (Enghin)

    ! Cobb-Douglas sensitivity 2: goods and services in intermediates
    if(cobb2_flag .eq. 1) then
       xi = ONE - ONE/0.99_dp
    endif

    ! production elasticity (intermediates across countries)
    zeta(1) = ONE - ONE/3.0_dp ! goods: 2.5 to match home share of goods intermediates
    zeta(2) = ONE - ONE/1.01_dp ! services: 1.01 to match home share of services intermediates
    zeta(3) = ONE ! construction not traded

    psi = -ONE ! IER

  end subroutine set_nontargeted_params

  ! ///////////////////////////////////////////////////////////////////////
  ! production functions and preferences
  ! ///////////////////////////////////////////////////////////////////////

  ! gross output
  real(DP) function prod_go(i,s,va,m)
    implicit none
    integer, intent(in) :: i,s
    real(DP), intent(in) :: va,m
    if(noio_flag .eq. 0) then
       prod_go = A(i,s) * (lam(i,s) * (va**eta) + (ONE-lam(i,s)) * (m**eta))**(ONE/eta)
    else
       prod_go = A(i,s) * va
    endif
    return
  end function prod_go

  ! value added
  real(DP) function prod_va_us(i,s,k,l,gam)
    implicit none
    integer, intent(in) :: i,s
    real(DP), intent(in) :: k,l,gam
    prod_va_us = B(i,s) * (k**alpha(s)) * ((gam*l)**(ONE-alpha(s)))
    return
  end function prod_va_us

  real(DP) function prod_va_rw(i,s,l,gam)
    implicit none
    integer, intent(in) :: i,s
    real(DP), intent(in) :: l,gam
    prod_va_rw = B(i,s) * gam * l
    return
  end function prod_va_rw

  real(DP) function prod_m(i,s,m1)
    implicit none
    integer, intent(in) :: i,s
    real(DP), dimension(NS), intent(in) :: m1
    real(DP), dimension(NS) :: tmp
    if(C(i,s)>ZERO) then
       tmp = m1**xi
       where(pi(i,s,:)>ZERO .neqv. .true.) tmp=ZERO
       prod_m = C(i,s) * (dot_product(pi(i,s,:),tmp))**(ONE/xi)
    else
       prod_m=ZERO
    endif
    return

  end function prod_m

  real(DP) function prod_m1(i,s,r,m2)
    implicit none
    integer, intent(in) :: i,s,r
    real(DP), dimension(NC), intent(in) :: m2
    real(DP), dimension(NC) :: tmp
    if(D(i,s,r)>ZERO) then
       tmp = m2**zeta(r)
       where(mu(i,s,r,:)>ZERO .neqv. .true.) tmp=ZERO
       prod_m1 = D(i,s,r) * (dot_product(mu(i,s,r,:),tmp))**(ONE/zeta(r))
    else
       prod_m1 = ZERO
    endif
    return
  end function prod_m1

  real(DP) function prod_q(i,f,q1)
    implicit none
    integer, intent(in) :: i,f
    real(DP), dimension(NS), intent(in) :: q1
    real(DP), dimension(NS) :: tmp
    if(G(i,f)>ZERO) then
       tmp = q1**rho(f)
       where(eps(i,f,:)>ZERO .neqv. .true.) tmp=ZERO
       prod_q = G(i,f) * (dot_product(eps(i,f,:),tmp))**(ONE/rho(f))
    else
       prod_q=ZERO
    endif
  end function prod_q

  real(DP) function prod_q1(i,f,r,q2)
    implicit none
    integer, intent(in) :: i,f,r
    real(DP), dimension(NC), intent(in) :: q2
    real(DP), dimension(NC) :: tmp
    if(H(i,f,r)>ZERO) then
       tmp = q2**sig(r)
       where(theta(i,f,r,:) > TINY .neqv. .true.) tmp = ZERO
       prod_q1 = H(i,f,r) * (dot_product(theta(i,f,r,:),tmp))**(ONE/sig(r))
    else
       prod_q1 = ZERO
    endif
    return
  end function prod_q1

  !real(DP) function muc(i,c,l,lbar,ne,nw)
  !  implicit none
  !  integer, intent(in) :: i
  !  real(DP), intent(in) :: c,l,lbar,ne,nw
  !  real(DP) :: leisure

  !  ! we cannot allow for negative leisure
  !  if(lbar-l .gt. 0.001) then
  !     leisure=lbar-l
  !  else
  !     leisure = 0.001 / log(0.001-(lbar-l))
       !write(*,*) lbar, l, leisure
       !stop 1
  !  endif

  !  muc = ((c/ne)**phi(i) * (leisure/nw)**(ONE-phi(i)))**(psi-ONE) * &
  !       phi(i) * ((c/ne)/(leisure/nw))**(phi(i)-ONE)

  !end function muc

    real(DP) function muc(i,s,c,l,lbar,ne,nw,cbar)
      implicit none
      integer, intent(in) :: i,s
      real(DP), intent(in) :: l,lbar,ne,nw
      real(DP), dimension(2), intent(in) :: c, cbar
      real(DP) :: leisure, tmp

      ! we cannot allow for negative leisure
      if(lbar-l .gt. 0.0001) then
         leisure=lbar-l
      else
         leisure = 0.0001 / log(0.0001-(lbar-l))
      endif

      if(s.eq.1) then
         tmp = c(s)-cbar(s)
      else
         tmp = c(s)+cbar(s)
      endif
      muc = phi(i) * eps(i,1,s) * (tmp/ne)**(rho(1)-ONE) * &
           (eps(i,1,1)*((c(1)-cbar(1))/ne)**(rho(1)) + eps(i,1,2)*((c(2)+cbar(2))/ne)**(rho(1)))**(psi*phi(i)/rho(1)-ONE) * &
           (leisure/nw)**((ONE-phi(i))*psi)

    end function muc
    
    real(DP) function mul(i,c,l,lbar,ne,nw,cbar)
      implicit none
      integer, intent(in) :: i
      real(DP), intent(in) :: l,lbar,ne,nw
      real(DP), dimension(2), intent(in) :: c,cbar
      real(DP) :: leisure

      ! we cannot allow for negative leisure
      if(lbar-l .gt. 0.0001) then
         leisure=lbar-l
      else
         leisure = 0.0001 / log(0.0001-(lbar-l))
      endif

      mul = (ONE-phi(i)) * &
           (eps(i,1,1)*((c(1)-cbar(1))/ne)**(rho(1)) + eps(i,1,2)*((c(2)+cbar(2))/ne)**(rho(1)))**(psi*phi(i)/rho(1)) * &
           (leisure/nw)**((ONE-phi(i))*psi-ONE)

    end function mul


  ! concave capital adjustment function (Prescott and Lucas 1971)
  real(DP) function phiK(x)
    real(DP), intent(in) :: x
    phiK = ((delta+gbgp-ONE)**(ONE-etaK) * x**etaK - (ONE-etaK)*(delta+gbgp-ONE) ) / etaK
  end function phiK

  real(DP) function dphiK(x)
    real(DP), intent(in) :: x
    dphiK = (delta+gbgp-ONE)**(ONE-etaK) * x**(etaK-ONE)
  end function dphiK

  ! ///////////////////////////////////////////////////////////////////////
  ! load data
  ! ///////////////////////////////////////////////////////////////////////

  ! input-output matrix
  subroutine load_iomat()

    integer :: i,j
    if(noio_flag .eq. 1 .and. noio2_flag .eq. 0) then
       open(unit=1, file=trim(INPATH)//'input-output-data-noio.csv')
    elseif(noio_flag .eq. 1 .and. noio2_flag .eq. 1) then
       open(unit=1, file=trim(INPATH)//'input-output-data-noio2.csv')
    elseif(more_svcs_trd_flag .eq. 1) then
       open(unit=1, file=trim(INPATH)//'input-output-data-moresvcstrd.csv')
    elseif(altio1_flag .eq. 1) then
       open(unit=1, file=trim(INPATH)//'input-output-data-altio1.csv')
    else
       open(unit=1, file=trim(INPATH)//'input-output-data.csv')
    endif
    do i=1,(NS1(1)+NS1(2)+2)
       read(1,*) (iomat(i,j),j=1,(NS1(1)+NS1(2)+NF1(1)+NF1(2)+1))
    enddo
    close(unit=1)
  end subroutine load_iomat

  ! time series parameters
  subroutine load_ts_params()

    integer :: i, t

    if(no_demo_flag .eq. 1) then
       pope_ts(:,:) = ONE
       popw_ts(:,:) = ONE
    else
       if(long_flag .eq. 0) then
          open(unit=4,file=trim(INPATH)//'demo.csv')
       else
          open(unit=4,file=trim(INPATH)//'demo_longer.csv')
       endif
       do t=1,NT+1
          read(4,*) popt_ts(1,t),popw_ts(1,t),popt_ts(2,t),popw_ts(2,t)
       enddo
       close(unit=4)

       do i=1,NC
          pope_ts(i,:) = (popw_ts(i,:)*0.5608_dp + 0.5_dp*(popt_ts(i,:)-popw_ts(i,:)*0.5608))/1.5608_dp
          pope_ts(i,:) = pope_ts(i,:) / pope_ts(i,1)
       enddo
    endif

    open(unit=4, file=trim(INPATH)//'tbdata.csv')
    do t=1,TSG1
       read(4,*) tbdata_ts(t)
    enddo
    close(unit=4)

    open(unit=4, file=trim(INPATH)//'rerdata.csv')
    do t=1,TSG1
       read(4,*) rerdata_ts(t)
    enddo
    close(unit=4)

    open(unit=4, file=trim(INPATH)//'lgdata.csv')
    do t=1,TSG1
       read(4,*) lgdata_ts(t)
    enddo
    close(unit=4)

    open(unit=4, file=trim(INPATH)//'govt-base.csv')
    do t=1,NT+1
       read(4,*) cg_pct_ts(1,t), bg_pct_ts(1,t)
    enddo
    close(unit=4)
    open(unit=4, file=trim(INPATH)//'govt-sg.csv')
    do t=1,NT+1
       read(4,*) cg_pct_ts(2,t), bg_pct_ts(2,t)
    enddo
    close(unit=4)
    open(unit=4, file=trim(INPATH)//'govt-ss.csv')
    do t=1,NT+1
       read(4,*) cg_pct_ts(3,t), bg_pct_ts(3,t)
    enddo
    close(unit=4)
    bg_pct_ts(:,:) = -bg_pct_ts(:,:)
    
    open(unit=5,file=trim(OUTPATH)//'demo.csv')
    call csv_write(5,'popt(1),popw(1),pope(1),popt(2),popw(2),pope(2)',.false.)
    write(5,*)
    do t=1,NT+1
       call csv_write(5,popt_ts(1,t),.false.)
       call csv_write(5,popw_ts(1,t),.false.)
       call csv_write(5,pope_ts(1,t),.false.)
       call csv_write(5,popt_ts(2,t),.false.)
       call csv_write(5,popw_ts(2,t),.false.)
       call csv_write(5,pope_ts(2,t),.false.)
       write(5,*)
    enddo
    close(5)

  end subroutine load_ts_params

  ! ///////////////////////////////////////////////////////////////////////
  ! store base-period equilibrium values
  ! ///////////////////////////////////////////////////////////////////////
  subroutine store_base_period_values()

    integer :: s,f,i,r
    real(DP) :: dky, rdky, rky, tmp, mkt_clear_tol

    mkt_clear_tol = 1.0e-5_dp

    ! initialize everything to zero
    y0(:,:) = ZERO
    va0(:,:) = ZERO
    k0(:,:) = ZERO
    l0(:,:) = ZERO
    m0(:,:) = ZERO
    q0(:,:) = ZERO
    m01(:,:,:) = ZERO
    q01(:,:,:) = ZERO
    m02(:,:,:,:) = ZERO
    q02(:,:,:,:) = ZERO
    ex0(:,:) = ZERO
    im0(:,:) = ZERO
    nx0(:,:) = ZERO
    c0(:,:) = ZERO
    ii0(:) = ZERO
    g0(:) = ZERO

    ! some calculations for US capital stock
    rdky = agg_cap_share*sum(iomat(6,1:3)) ! gross payments to capital
    dky = delta*kk0 ! depreciation
    rky = rdky - dky ! returns to capital net of depreciation
    r0 = ((ONE-tauk)*rdky-dky)/kk0 ! rental rate in base period

    ! US values
    do s=1,NS1(1)
       y0(1,s) = iomat(7,s) ! gross output
       va0(1,s) = iomat(6,s) ! value added
       !l0(1,s) = (ONE-alpha(s))*va0(1,s) ! labor
       l0(1,s) = 100.0_dp * (ONE-agg_cap_share) * lshare0(s) ! set initial labor to match labor comp shares
       alpha(s) = ONE - l0(1,s) / va0(1,s) ! now back out implied capital share
       k0(1,s) = alpha(s)*va0(1,s) / ((r0+delta)/(ONE-tauk)) ! capital
       m0(1,s) = iomat(1,s)+iomat(2,s)+iomat(3,s)+iomat(4,s)+iomat(5,s) ! intermediates
       m01(1,s,1) = iomat(1,s)+iomat(4,s) ! intermediate goods
       m01(1,s,2) = iomat(2,s)+iomat(5,s) ! intermediate services
       m01(1,s,3) = iomat(3,s) ! intermediate construction
       m02(1,s,1,1) = iomat(1,s) ! US intermediate goods
       m02(1,s,1,2) = iomat(4,s) ! RW intermediate goods
       m02(1,s,2,1) = iomat(2,s) ! US intermediate services
       m02(1,s,2,2) = iomat(5,s) ! RW intermediate services
       m02(1,s,3,1) = iomat(3,s) ! US intermediate construction
       ex0(1,s) = iomat(s,4)+iomat(s,5)+iomat(s,9) ! exports
    enddo
    ll0(1) = sum(l0(1,:))
    call assert1(abs(kk0-sum(k0(1,:)))<mkt_clear_tol,'US capital demand != supply! kk0 = '&
         //trim(strd(kk0))//', sum k0 = '//trim(strd(sum(k0(1,:)))))

    do f=1,NF1(1)
       q0(1,f) = iomat(7,5+f) ! aggregate final use (c/g/i)
       q01(1,f,1) = iomat(1,5+f)+iomat(4,5+f) ! final use of goods
       q01(1,f,2) = iomat(2,5+f)+iomat(5,5+f) ! final use of services
       q01(1,f,3) = iomat(3,5+f)  ! final use of construction
       q02(1,f,1,1) = iomat(1,5+f) ! final use of US goods
       q02(1,f,1,2) = iomat(4,5+f) ! final use of RW goods
       q02(1,f,2,1) = iomat(2,5+f) ! final use of US services
       q02(1,f,2,2) = iomat(5,5+f) ! final use of RW services
       q02(1,f,3,1) = iomat(3,5+f) ! final use of US construction
    enddo
    c0(1,1:2) = q01(1,1,1:2)
    ii0(1) = q0(1,3)
    g0(1) = q0(1,2)

    ! rest of the world values
    do s=1,NS1(2)
       y0(2,s) = iomat(7,NS1(1)+s) ! gross output
       va0(2,s) = iomat(6,NS1(1)+s) ! value added
       l0(2,s) = va0(2,s) ! labor
       m0(2,s) = iomat(1,NS1(1)+s)+iomat(2,NS1(1)+s)+iomat(4,NS1(1)+s)+iomat(5,NS1(1)+s) ! intermediates
       m01(2,s,1) = iomat(1,NS1(1)+s)+iomat(4,NS1(1)+s) ! intermediate goods
       m01(2,s,2) = iomat(2,NS1(1)+s)+iomat(5,NS1(1)+s) ! intermediate services
       m02(2,s,1,1) = iomat(1,NS1(1)+s) ! US intermediate goods
       m02(2,s,1,2) = iomat(4,NS1(1)+s) ! RW intermediate goods
       m02(2,s,2,1) = iomat(2,NS1(1)+s) ! US intermediate services
       m02(2,s,2,2) = iomat(5,NS1(1)+s) ! RW intermediate services
       ex0(2,s) = iomat(NS1(1)+s,1)+iomat(NS1(1)+s,2)+iomat(NS1(1)+s,3)+&
            iomat(NS1(1)+s,6)+iomat(NS1(1)+s,7)+iomat(NS1(1)+s,8) ! exports
    enddo
    ll0(2) = sum(l0(2,:))

    do f=1,NF1(2)
       q0(2,f) = iomat(7,5+3+f) ! aggregate final use (c/g/i)
       q01(2,f,1) = iomat(1,5+3+f)+iomat(4,5+3+f) ! final use of goods
       q01(2,f,2) = iomat(2,5+3+f)+iomat(5,5+3+f) ! final use of services
       q02(2,f,1,1) = iomat(1,5+3+f) ! final use of US goods
       q02(2,f,1,2) = iomat(4,5+3+f) ! final use of RW goods
       q02(2,f,2,1) = iomat(2,5+3+f) ! final use of US services
       q02(2,f,2,2) = iomat(5,5+3+f) ! final use of RW services
    enddo
    c0(2,1:2) = q01(2,1,1:2)

    ! US imports = RW exports (and vice versa)
    im0(1,1:2) = ex0(2,1:2)
    im0(2,1:2) = ex0(1,1:2)

    ! net exports
    nx0(1,:) = ex0(1,:) - im0(1,:)
    nx0(2,:) = ex0(2,:) - im0(2,:)

    do i=1,NC

       ! gdp = c+i+g+nx
       tmp = sum(va0(i,:)) - (sum(q0(i,:)) + sum(ex0(i,:)) - sum(im0(i,:)))
       call assert1(abs(tmp)<mkt_clear_tol,'gdp != c+i+g+nx for country '//str(i))

       ! go (supply) = intermediate + final uses (demand)
       do s=1,NS1(i)
          tmp = y0(i,s) - (sum(q02(:,:,s,i))+sum(m02(:,:,s,i)))
          call assert1(abs(tmp)<mkt_clear_tol,'go != c+i+g+m = for country-sector '//str(i)//str(s))
       end do

       do s=1,NS
          ! go  = value added + intermediates
          tmp = y0(i,s) - (va0(i,s)+sum(m02(i,s,:,:)))
          call assert1(abs(tmp)<mkt_clear_tol,'go != va+m = for country-sector '//str(i)//str(s))

          ! total intermediate bundle should equal intermediate bundles from each sector, as well as 
          ! sum of all intermediates
          tmp = m0(i,s) - sum(m01(i,s,:))
          call assert1(abs(tmp)<TINY,'m0 != sum(m01) for is = '//trim(str(i))//trim(str(s)))

          tmp = m0(i,s) - sum(m02(i,s,:,:))
          call assert1(abs(tmp)<TINY,'m0 != sum(m02) for country-sector '//str(i)//str(s))

          ! intermediate bundle at sector level should equal sum of intermediates in that sector from all sources
          do r=1,NS
             tmp = m01(i,s,r)-sum(m02(i,s,r,:))
             call assert1(abs(tmp)<TINY,'m01 != sum(m02) for country-sector-sector '//str(i)//str(s)//str(r))
          enddo
       end do

    enddo

  end subroutine store_base_period_values

  ! ///////////////////////////////////////////////////////////////////////
  ! calibrate production function share and scale parameters, taking elasticities as given
  ! ///////////////////////////////////////////////////////////////////////
  subroutine calibrate_prod_params()
    implicit none
    
    integer :: i,s,j,r,idx,cnt,t
    real(DP) :: tmp, weight
    logical, dimension(NC) :: mask1
    logical, dimension(NS) :: mask2
    real(DP), dimension(NC) :: tmp1
    real(DP), dimension(NS) :: tmp2
    real(DP), dimension(NC,NS) :: hh

    mu(:,:,:,:)=ZERO
    pi(:,:,:)=ZERO
    lam(:,:)=ZERO
    A(:,:)=ZERO
    B(:,:)=ZERO
    C(:,:)=ZERO
    D(:,:,:)=ZERO

    ! step 1: value added scale factors
    B(1,:) = va0(1,:) / ((k0(1,:)**alpha(:)) * (l0(1,:)**(ONE-alpha(:))))
    B(2,:) = va0(2,:) / l0(2,:)
    B(2,3) = ZERO
    
    ! step 2: inner CES shares and scale factors
    do i=1,NC
       do s=1,NS
          if(y0(i,s)>TINY) then
             do r=1,NS
                mask1(:) = m02(i,s,r,:)>TINY
                if(count(mask1)==0) then
                   mu(i,s,r,:) = ZERO
                   call assert1(m01(i,s,r)<TINY,'m01>0 when sum(m02)=0!')
                   D(i,s,r)=ZERO
                else if(count(mask1)==1) then
                   where(mask1)
                      mu(i,s,r,:)=ONE
                   elsewhere
                      mu(i,s,r,:)=ZERO
                   endwhere
                   call assert1(abs(m01(i,s,r)-sum(array=m02(i,s,r,:),mask=mask1))<TINY,'m01 != sum(m02) when cnt=1!')
                   D(i,s,r)=ONE
                else
                   idx=maxloc(array=m02(i,s,r,:),dim=1,mask=mask1)
                   tmp1 = (m02(i,s,r,:)/m02(i,s,r,idx))**(ONE-zeta(r))
                   mu(i,s,r,idx)=ONE/sum(array=tmp1,mask=mask1)
                   cnt=0
                   do j=1,NC
                      if(mask1(j)) then
                         cnt=cnt+1
                         if(j .ne. idx) then
                            if(cnt<count(mask1)) then
                               mu(i,s,r,j) = mu(i,s,r,idx)*tmp1(j)
                            else
                               mu(i,s,r,j) = ONE-sum(array=mu(i,s,r,:),mask=mask1)
                            endif
                         endif
                      endif
                   enddo
                   tmp1=m02(i,s,r,:)**zeta(r)
                   where(mask1 .neqv. .true.) tmp1=ZERO
                   tmp = (dot_product(mu(i,s,r,:),tmp1))**(ONE/zeta(r))
                   D(i,s,r) = m01(i,s,r)/tmp
                endif
             enddo
          endif
       enddo
    enddo

    ! step 3: outer CES shares and scaling factors
    do i=1,NC
       do s=1,NS
          if(y0(i,s)>TINY) then
             mask2(:) = m01(i,s,:)>TINY
             if(count(mask2)==0) then
                pi(i,s,:)=ZERO
                call assert1(abs(m0(i,s))<TINY,'m0>0 when m01=0!')
                C(i,s)=ZERO
             else if(count(mask2)==1) then
                where(mask2)
                   pi(i,s,:)=ONE
                elsewhere
                   pi(i,s,:)=ZERO
                endwhere
                call assert1(abs(m0(i,s)-sum(array=m01(i,s,:),mask=mask2))<TINY,'m0 != sum(m01) when cnt=1')
                C(i,s) = ONE
             else
                idx = 1 ! use goods sector (all sectors use goods as inputs)
                j=i ! use home country as index country since all sectors use intermediates
                ! from all other home sectors
                tmp2 = (mu(i,s,idx,j)/mu(i,s,:,j)) &
                     * (D(i,s,idx))**zeta(idx) * (D(i,s,:))**(-zeta(:)) &
                     * (m01(i,s,:)/m01(i,s,idx))**(ONE-xi) &
                     * (m01(i,s,:)/m02(i,s,:,j))**(zeta(:)-ONE) &
                     * (m01(i,s,idx)/m02(i,s,idx,j))**(ONE-zeta(idx))
                pi(i,s,idx) = ONE/sum(array=tmp2,mask=mask2)
                cnt=0
                do r=1,NS
                   if(mask2(r)) then
                      cnt=cnt+1
                      if(r.ne.idx) then
                         if(cnt<count(mask2)) then
                            pi(i,s,r) = pi(i,s,idx) * tmp2(r)
                         else
                            pi(i,s,r) = ONE-sum(array=pi(i,s,:),mask=mask2)
                         endif
                      endif
                   endif
                enddo
                tmp2=m01(i,s,:)**xi
                where(mask2 .neqv. .true.) tmp2=ZERO
                tmp=(dot_product(pi(i,s,:),tmp2))**(ONE/xi)
                C(i,s) = m0(i,s)/tmp
             endif
          endif
       enddo
    enddo

    ! step 4: value added shares
    if(noio_flag .eq. 1) then
       lam(:,:) = ONE
    else
       do i=1,NC
          do s=1,NS
             if(y0(i,s)>TINY) then
                mask2=pi(i,s,:)>TINY
                r=1
                mask1=mu(i,s,r,:)>TINY
                j=i

                if(i.eq.1) then
                   tmp = ((ONE-alpha(s))*B(i,s))/ &
                        (pi(i,s,r)*mu(i,s,r,j)*(C(i,s)**xi)*(D(i,s,r)**zeta(r))) &
                        * (m0(i,s)/va0(i,s))**(ONE-eta) &
                        * (k0(i,s)/l0(i,s))**alpha(s) &
                        * (m0(i,s)/m01(i,s,r))**(xi-ONE) & 
                        * (m01(i,s,r)/m02(i,s,r,j))**(zeta(r)-ONE)
                   lam(i,s)=ONE/(ONE+tmp)
                else
                   tmp = (B(i,s))/ &
                        (pi(i,s,r)*mu(i,s,r,j)*(C(i,s)**xi)*(D(i,s,r)**zeta(r))) &
                        * (m0(i,s)/va0(i,s))**(ONE-eta) &
                        * (m0(i,s)/m01(i,s,r))**(xi-ONE) & 
                        * (m01(i,s,r)/m02(i,s,r,j))**(zeta(r)-ONE)
                   lam(i,s)=ONE/(ONE+tmp)
                endif

             endif
          enddo
       enddo
    endif

    ! step 5: gross output scaling factors
    if(noio_flag .eq. 1) then
       A(:,:) = ONE
    else
       do i=1,NC
          do s=1,NS
             if(y0(i,s)>TINY) then
                A(i,s) = y0(i,s) / &
                     (lam(i,s)*va0(i,s)**eta + (ONE-lam(i,s))*m0(i,s)**eta)**(ONE/eta)
             endif
          enddo
       enddo
    endif

    ! step 6: check equilibrium conditions are satisfied
    do i=1,NC
       do s=1,NS
          if(y0(i,s)>TINY) then

             ! gross output production functions are correct
             tmp = prod_go(i,s,va0(i,s),m0(i,s)) - y0(i,s)
             call assert1(abs(tmp)<TINY,'prod_go != y0, is = '//trim(str(i))//trim(str(s)))
             
             ! value added production functions are correct
             if(i.eq.1) then
                tmp = prod_va_us(i,s,k0(i,s),l0(i,s),ONE) - va0(i,s)
             else
                tmp = prod_va_rw(i,s,l0(i,s),ONE) - va0(i,s)
             endif
             call assert1(abs(tmp)<TINY,'prod_va != va0, is = '//trim(str(i))//trim(str(s)))

             ! intermediate production functions are correct
             tmp = prod_m(i,s,m01(i,s,:)) - m0(i,s)
             call assert1(abs(tmp)<TINY,'prod_m != m0, is = '//trim(str(i))//trim(str(s))//&
                  trim(' m0 = ')//trim(strd(m0(i,s)))//trim(' prod_m = ')//trim(strd(prod_m(i,s,m01(i,s,:)))))
             do r=1,NS
                tmp = prod_m1(i,s,r,m02(i,s,r,:)) - m01(i,s,r)
                if(abs(tmp)>TINY) then
                   write(*,*) tmp
                endif
                call assert1(abs(tmp)<TINY,'prod_m1 != m01, isr = '//trim(str(i))//trim(str(s))//trim(str(r)))
             enddo

             ! zero profit
             tmp = y0(i,s) - va0(i,s) - m0(i,s)
             call assert1(abs(tmp)<TINYSQ,'positive profits!, is = '//trim(str(i))//trim(str(s)))

             ! foc for labor
             if(i.eq.1) then                
                tmp = lam(i,s) * (ONE-alpha(s))* A(i,s)**eta * B(i,s) &
                     * (y0(i,s) / va0(i,s))**(ONE-eta) &
                     * (k0(i,s)/l0(i,s))**(alpha(s)) - ONE
             else
                tmp = lam(i,s) * A(i,s)**eta * B(i,s) &
                     * (y0(i,s) / va0(i,s))**(ONE-eta) - ONE                
             endif
             call assert1(abs(tmp)<TINYSQ,'labor foc!, is = '//trim(str(i))//trim(str(s))//', tmp = '//trim(strd(tmp)))

             ! foc for capital
             if(i.eq.1) then
                tmp = lam(i,s) * alpha(s) * A(i,s)**eta * B(i,s) &
                     * (y0(i,s) / va0(i,s))**(ONE-eta) &
                     * (k0(i,s)/l0(i,s))**(alpha(s)-ONE) - (r0+delta)/(ONE-tauk)
                call assert1(abs(tmp)<TINYSQ,'capital foc!, is = '//trim(str(i))//trim(str(s)))
             endif

             ! focs for intermediate inputs
             do r=1,NS
                do j=1,NC
                   if(mu(i,s,r,j)>ZERO) then
                      tmp = (ONE-lam(i,s)) * pi(i,s,r) * mu(i,s,r,j) &
                           * A(i,s)**eta * C(i,s)**xi * D(i,s,r)**zeta(r) &
                           * (y0(i,s) / m0(i,s))**(ONE-eta) &
                           * (m0(i,s) / m01(i,s,r))**(ONE-xi) &
                           * (m01(i,s,r) / m02(i,s,r,j))**(ONE-zeta(r)) - ONE
                      call assert1(abs(tmp)<TINYSQ,'intermediate foc!, isrj = '//&
                           trim(str(i))//trim(str(s))//trim(str(r))//trim(str(j))//strd(tmp))
                   endif
                enddo
             enddo
             
          endif
       enddo
    enddo

    ! set productivity levels and Stone-Geary parameters throughout growth path
    gam_t(:,:,:) = ONE
    hh(:,:) = gbgp
    do t=2,NT+1
       do i=1,NC
          do s=1,NS1(i)

             ! if we're before period in which
             ! structural change turns off, gamma grows at asymmetric rates taken from data             
             if(t.lt.TPC0) then
                gam_t(i,s,t) = gam_t(i,s,t-1) * ggam(i,s) 
                hh(i,s) = ggam(i,s)

             ! if we're in between periods where parameter convergence begins and ends,
             ! gamma is linear combo of previous value and BGP value, with (T-TPC0)/(TPC1-TPC0) as weight
             else if(t.lt.TPC1) then
                weight = (dble(t-TPC0))/(TPC1-TPC0)
                hh(i,s) = hh(i,s) * (ONE-weight) + gbgp * weight
                gam_t(i,s,t) = hh(i,s) * gam_t(i,s,t-1)

             ! if we're in BGP parameter space, everything is constant
             else
                gam_t(i,s,t) = gam_t(i,s,t-1) * gbgp ! otherwise grow at constant 2% in all countries/sectors
             endif
          enddo
       enddo
    enddo

  end subroutine calibrate_prod_params

  subroutine calibrate_fin_params()

    implicit none

    integer :: i,s,f,r,j,idx,cnt
    real(DP) :: tmp
    logical, dimension(NC) :: mask1
    logical, dimension(NS) :: mask2
    real(DP), dimension(NC) :: tmp1
    real(DP), dimension(NS) :: tmp2

    G(:,:) = ZERO
    H(:,:,:) = ZERO
    eps(:,:,:) = ZERO
    theta(:,:,:,:) = ZERO
    
    ! step 1: inner CES shares and scale factors
    do i=1,NC
       do f=1,NF
          if(q0(i,f)>TINY) then
             do r=1,NS
                mask1 = q02(i,f,r,:)>TINY
                if(count(mask1)==0) then
                   call assert1(q01(i,f,r)<TINY,'q01>0 when sum(q02)=0! ifr = '//trim(str(i))//trim(str(f))//trim(str(r)))
                   theta(i,f,r,:) = ZERO
                   H(i,f,r) = ZERO
                else if(count(mask1)==1) then
                    call assert1(abs(q01(i,f,r)-sum(array=q02(i,f,r,:),mask=mask1))<TINY,&
                        'q01 != sum(q02) when cnt=1! ifr = '//trim(str(i))//trim(str(f))//trim(str(r)))
                   where(mask1)
                      theta(i,f,r,:) = ONE
                   elsewhere
                      theta(i,f,r,:) = ZERO
                   endwhere
                   H(i,f,r) = ONE
                else
                   idx = i ! everyone always consumes from home country
                   tmp1 = (q02(i,f,r,:)/q02(i,f,r,idx))**(ONE-sig(r))
                   theta(i,f,r,idx) = ONE/sum(array=tmp1,mask=mask1)
                   cnt=0
                   do j=1,NC
                      if(mask1(j)) then
                         cnt=cnt+1
                         if(j .ne. idx) then
                            if(cnt<count(mask1)) then
                               theta(i,f,r,j) = theta(i,f,r,idx) * tmp1(j)
                            else
                               theta(i,f,r,j) = ONE-sum(array=theta(i,f,r,:),mask=mask1)
                            endif
                         endif
                      endif
                   enddo
                   tmp1 = q02(i,f,r,:)**sig(r)
                   where(mask1 .neqv. .true.) tmp1=ZERO
                   tmp = (dot_product(theta(i,f,r,:),tmp1))**(ONE/sig(r))
                   H(i,f,r) = q01(i,f,r)/tmp
                endif
             enddo
          endif
       enddo
    enddo

    ! step 2: outer CES shares and scaling factors for government and investment
    i=1
    !do i=1,NC
    do f=2,3
       if(q0(i,f)>TINY) then
          mask2 = q01(i,f,:)>TINY
          if(count(mask2)==0) then
             call assert1(q0(i,f)<TINY,'q0 > 0 when sum(q01)=0! if = '//trim(str(i))//trim(str(f)))
             eps(i,f,:) = ZERO
             G(i,f) = ZERO
          else if(count(mask2)==1) then
             call assert1(abs(q0(i,f)-sum(array=q01(i,f,:),mask=mask2))<TINY,&
                  'q0 != sum(q01) when cnt=1! if = '//trim(str(i))//trim(str(f)))
             where(mask2)
                eps(i,f,:) = ONE
             elsewhere
                eps(i,f,:) = ZERO
             endwhere
             G(i,f) = ONE
          else
             idx = 2 ! all final sectors consume goods regardless of country
             j = i ! home country goods always consumed too
             tmp2 = (theta(i,f,idx,j)/theta(i,f,:,j)) &
                  * (H(i,f,idx))**sig(idx) * (H(i,f,:))**(-sig(:)) &
                  * (q01(i,f,:)/q01(i,f,idx))**(ONE-rho(f)) &
                  * (q01(i,f,:)/q02(i,f,:,j))**(sig(:)-ONE) &
                  * (q01(i,f,idx)/q02(i,f,idx,j))**(ONE-sig(idx))
             eps(i,f,idx) = ONE/sum(array=tmp2,mask=mask2)
             cnt=0
             do r=1,NS
                if(mask2(r)) then
                   cnt=cnt+1
                   if(r .ne. idx) then
                      if(cnt<count(mask2)) then
                         eps(i,f,r) = eps(i,f,idx) * tmp2(r)
                      else
                         eps(i,f,r) = ONE-sum(array=eps(i,f,:),mask=mask2)
                      endif
                   endif
                endif
             enddo
             tmp2 = q01(i,f,:)**rho(f)
             where(mask2 .neqv. .true.) tmp2=ZERO
             tmp = (dot_product(eps(i,f,:),tmp2))**(ONE/rho(f))
             G(i,f) = q0(i,f)/tmp
          endif
       endif
    enddo
    !enddo

    ! step 3: check equilibrium conditions
    do i=1,NC
       do s=1,2
          tmp = prod_q1(i,1,s,q02(i,1,s,:)) - c0(i,s)
          call assert1(abs(tmp)<TINY,'prod_c != c0! is = '//trim(str(i))//trim(str(s)))

          do j=1,NC
             tmp = theta(i,1,s,j) &
                  * H(i,1,s)**sig(s) &
                  * (c0(i,s) / q02(i,1,s,j))**(ONE-sig(s)) - ONE
             call assert1(abs(tmp)<TINYSQ,'cons foc! isj = '//trim(str(i))//trim(str(s))//trim(str(j))// &
                  ', tmp = '//trim(strd(tmp)))             
          enddo
       enddo
    enddo

    i=1
    !do i=1,NC
    do f=2,3
       if(q0(i,f)>TINY) then
          ! production function is correct
          tmp = prod_q(i,f,q01(i,f,:)) - q0(i,f)
          call assert1(abs(tmp)<TINY,'prod_q != q0! if = '//trim(str(i))//trim(str(f)))
          do r=1,NS
             if(q01(i,f,r)>0) then
                tmp = prod_q1(i,f,r,q02(i,f,r,:)) - q01(i,f,r)
                call assert1(abs(tmp)<TINY,'prod_q1 != q01! ifr = '//trim(str(i))//trim(str(f))//trim(str(r)))
             endif
          enddo

          ! first order conditions
          do r=1,NS
             if(eps(i,f,r)>ZERO) then
                do j=1,NC
                   if(theta(i,f,r,j)>ZERO) then
                      tmp = eps(i,f,r) * theta(i,f,r,j) &
                           * G(i,f)**rho(f) * H(i,f,r)**sig(r) &
                           * (q0(i,f) / q01(i,f,r))**(ONE-rho(f)) &
                           * (q01(i,f,r) / q02(i,f,r,j))**(ONE-sig(r)) - ONE
                      call assert1(abs(tmp)<TINYSQ,'final foc! ifrj = '//trim(str(i))//trim(str(f))//trim(str(r))//trim(str(j)))
                   endif
                enddo
             endif
          enddo
       endif
    enddo
    !enddo

  end subroutine calibrate_fin_params

  subroutine calibrate_hh_params()

    implicit none
    
    integer :: i,t
    real(DP) :: tmp, weight

    lbar0(:) = 3.0_dp * ll0(:) ! steady state labor time = 1/3 labor endowment

    ! first calibrate consumption sector share parameters and Stone-Geary parameters
    if(nonhomo_prefs_flag .eq. 0) then
       cbar0(:,:) = ZERO
       do i=1,NC
          tmp = (c0(i,1)/c0(i,2))**(ONE-rho(1))
          eps(i,1,1) = tmp/(ONE+tmp)
          eps(i,1,2) = ONE-eps(i,1,1)
       enddo
    else
       ! first approach: same subsistence requirement across countries (per capita)
       if(nonhomo_prefs_approach .eq. 0) then
          cbar0(1,2) = cbar_s_ratio * c0(1,2)
          cbar0(1,1) = ((ag_frac * c0(1,1)) * cbar_ag_ratio) * c0(1,1)
          !cbar0(2,1) = (cbar0(1,1)/c0(1,1)) * c0(2,1)
          !cbar0(2,2) = (cbar0(1,2)/c0(1,2)) * c0(2,2)
          cbar0(2,:) = cbar0(1,:) * (lbar0(2)/lbar0(1)) 
         do i=1,NC
             tmp = ((c0(i,1)-cbar0(i,1))/(c0(i,2)+cbar0(i,2)))**(ONE-rho(1))
             eps(i,1,1) = tmp/(ONE+tmp)
             eps(i,1,2) = ONE-eps(i,1,1)
          enddo
       else
       endif
    endif

    ! now calibrate share of consumption in utility
    do i=1,NC
       tmp = (eps(i,1,1)*(c0(i,1)-cbar0(i,1))**rho(1) + eps(i,1,2)*(c0(i,2)+cbar0(i,2))**rho(1)) / &
            ((lbar0(i)-ll0(i))) / eps(i,1,1) / ((c0(i,1)-cbar0(i,1))**(rho(1)-ONE))
       phi(i) = tmp/(ONE+tmp) ! now get implied value of phi from that ratio
    end do

    ! now check FOCs
    do i=1,NC

       tmp = muc(i,1,c0(i,:),ll0(i),lbar0(i),ONE,ONE,cbar0(i,:))/muc(i,2,c0(i,:),ll0(i),lbar0(i),ONE,ONE,cbar0(i,:)) - ONE
       call assert1(abs(tmp)<TINYSQ,'HH intratemp FOC 1! i = '//trim(str(i)))

       tmp = muc(i,1,c0(i,:),ll0(i),lbar0(i),ONE,ONE,cbar0(i,:))/mul(i,c0(i,:),ll0(i),lbar0(i),ONE,ONE,cbar0(i,:)) - ONE
       call assert1(abs(tmp)<TINYSQ,'HH consumption-leisure FOC! i = '//trim(str(i))//', val = '//strd(tmp))
    enddo

    ! calculate Stone-Geary parameters throughout all periods
    cbar_ts(:,:,1) = cbar0(:,:)
    do t=2,NT+1

       ! if we're before period in which
       ! structural change turns off, S-G params are same as base period
       if(t.lt.TPC0) then
          cbar_ts(:,:,t) = cbar0(:,:)

       ! if we're in between periods where parameter convergence begins and ends,
       ! use same linear combo as for gamma
       else if(t.lt.TPC1) then
          weight = (dble(t-TPC0))/(TPC1-TPC0)
          cbar_ts(:,:,t) = (ONE-weight) * cbar_ts(:,:,t-1) + weight * ZERO

       ! if we're in BGP parameter space, everything is constant
       else
          cbar_ts(:,:,t) = ZERO
       endif
    enddo

    ! calibrate discount factor
    do i=1,NC
       beta(i) = muc(i,1,c0(i,:),ll0(i),lbar0(i),ONE,ONE,cbar_ts(i,:,NT+1)) / &
            muc(i,1,gbgp*c0(i,:),ll0(i),lbar0(i),ONE,ONE,cbar_ts(i,:,NT+1))/(ONE+rbgp)
    enddo

    om_speed = 0.9_dp
    om_speed_ss = 0.5_dp

    ! set labor time endowment in all periods
    do t=1,NT+1
       do i=1,NC
          lbar_ts(i,t) = lbar0(i) * popw_ts(i,t)
       enddo
    enddo

  end subroutine calibrate_hh_params

  ! ///////////////////////////////////////////////////////////////////////
  ! calibrate everything!
  ! ///////////////////////////////////////////////////////////////////////
  subroutine calibrate_all()
    call load_iomat
    call load_ts_params
    call set_nontargeted_params
    call store_base_period_values
    call calibrate_prod_params
    call calibrate_fin_params
    call calibrate_hh_params

    if(fixed_govt_flag .eq. 1) then
       cg_pct_ts(:,:) = g0(1)
       bg_pct_ts(:,:) = bg0
    elseif(fixed_govt2_flag .eq. 1) then
       cg_pct_ts(1,:) = cg_pct_ts(2,:)
       bg_pct_ts(1,:) = bg_pct_ts(2,:)
    endif

  end subroutine calibrate_all

end module calibrate
