module equilibrium

  use globals
  use calibrate
  use csv_file
  implicit none

  ! ///////////////////////////////////////////////////////////////////////
  ! module-scope flags and other things
  ! ///////////////////////////////////////////////////////////////////////

  integer :: flag=0 ! module-scope indicator of balanced-growth (small) or unbalanced-growth (large) eqm
  integer :: NBGP = & ! number of vars/eqns for balanced-growth path
       NC + & ! wages
       NS1(1) + & ! capital in us
       NS1(1) + & ! labor in us
       NS1(2) + & ! labor in rw
       NS1(1)*4 + & ! intermediate inputs in us
       NS1(2)*4 + & ! intermediate inputs in rw
       NS1(1) + & ! gross output prices in us
       NS1(2) + & ! gross output prices in rw
       2*4 + & ! final demand for cons and govt in us
       3 + & ! final demand for domestic inv in us
       2 + & ! final demand for foreign inv in us
       4 + & ! rw consumption
       4 + & ! goods and services consumption prices in both countries
       2 ! us govt and inv prices

  integer :: NEQM0 = & ! number of vars/eqns for baseline no-saving glut scenario
       NC*(NT+1) + & ! wages
       NS1(1)*(NT) + & ! investment in us (period 1 thru NT only)
       NS1(1)*(NT+1) +  & ! labor in us
       NS1(2)*(NT+1) + & ! labor in rw
       NS1(1)*4*(NT+1) + & ! intermediate inputs in us
       NS1(2)*4*(NT+1) + & ! intermediate inputs in rw
       NS1(1)*(NT+1) + & ! gross output prices in us
       NS1(2)*(NT+1) + & ! gross output prices in rw
       2*4*(NT+1) + & ! final demand for cons and govt in us
       3*(NT+1) + & ! final demand for domestic inv in us
       2*(NT+1) + & ! final demand for foreign inv in us
       4*(NT+1) + & ! rw consumption
       2*(NT+1) + & ! us final demand prices for gov and inv
       4*(NT+1) + & ! goods and services prices in both countries
       1*(NT) + & ! bonds (periods 2 thru NT+1 only)
       1*(NT) + & ! bonds price (periods 1 thru NT only)
       1 + & ! period-1 discount factor wedge to match period-1 TB
       1 ! period-2 investment wedge

  integer :: NEQM0fi = & ! number of vars/eqns for baseline no-saving glut scenario
       NC*(NT+1) + & ! wages
       NS1(1)*(NT) + & ! investment in us (period 1 thru NT only)
       NS1(1)*(NT+1) +  & ! labor in us
       NS1(2)*(NT+1) + & ! labor in rw
       NS1(1)*4*(NT+1) + & ! intermediate inputs in us
       NS1(2)*4*(NT+1) + & ! intermediate inputs in rw
       NS1(1)*(NT+1) + & ! gross output prices in us
       NS1(2)*(NT+1) + & ! gross output prices in rw
       2*4*(NT+1) + & ! final demand for cons and govt in us
       3*(NT+1) + & ! final demand for domestic inv in us
       2*(NT+1) + & ! final demand for foreign inv in us
       4*(NT+1) + & ! rw consumption
       2*(NT+1) + & ! us final demand prices for gov and inv
       4*(NT+1) + & ! rw and us consumption price
       1*(NT) + & ! bonds (periods 2 thru NT+1 only)
       1*(NT) + & ! bonds price (periods 1 thru NT only)
       1 + & ! period-1 discount factor wedge to match period-1 TB
       1 + & ! period-2 investment wedge
       NT-1 ! periods 3-NT+1 investment wedges

  integer :: NEQM1 = & ! number of vars/eqns for baseline no-saving glut scenario
       NC*(NT+2-TSG0) + & ! wages
       NS1(1)*(NT+1-TSG0) + & ! investment in us (period 1 thru NT only)
       NS1(1)*(NT+2-TSG0) +  & ! labor in us
       NS1(2)*(NT+2-TSG0) + & ! labor in rw
       NS1(1)*4*(NT+2-TSG0) + & ! intermediate inputs in us
       NS1(2)*4*(NT+2-TSG0) + & ! intermediate inputs in rw
       NS1(1)*(NT+2-TSG0) + & ! gross output prices in us
       NS1(2)*(NT+2-TSG0) + & ! gross output prices in rw
       2*4*(NT+2-TSG0) + & ! final demand for cons and govt in us
       3*(NT+2-TSG0) + & ! final demand for domestic inv in us
       2*(NT+2-TSG0) + & ! final demand for foreign inv in us
       4*(NT+2-TSG0) + & ! rw consumption
       2*(NT+2-TSG0) + & ! us final demand prices for gov and inv
       4*(NT+2-TSG0) + & ! rw and us consumption prices
       1*(NT+1-TSG0) + & ! bonds (periods TSG0+1 thru NT+1 only)
       1*(NT+1-TSG0) + & ! bond price (periods TSG0 thru NT only)
       (TSG1-TSG0+1) ! discount factor wedges for periods TSG0 thru TSG1

  integer :: NEQM1fi = & ! number of vars/eqns for baseline no-saving glut scenario
       NC*(NT+2-TSG0) + & ! wages
       NS1(1)*(NT+1-TSG0) + & ! investment in us (period 1 thru NT only)
       NS1(1)*(NT+2-TSG0) +  & ! labor in us
       NS1(2)*(NT+2-TSG0) + & ! labor in rw
       NS1(1)*4*(NT+2-TSG0) + & ! intermediate inputs in us
       NS1(2)*4*(NT+2-TSG0) + & ! intermediate inputs in rw
       NS1(1)*(NT+2-TSG0) + & ! gross output prices in us
       NS1(2)*(NT+2-TSG0) + & ! gross output prices in rw
       2*4*(NT+2-TSG0) + & ! final demand for cons and govt in us
       3*(NT+2-TSG0) + & ! final demand for domestic inv in us
       2*(NT+2-TSG0) + & ! final demand for foreign inv in us
       4*(NT+2-TSG0) + & ! rw consumption
       2*(NT+2-TSG0) + & ! us final demand prices for gov and inv
       4*(NT+2-TSG0) + & ! rw and us consumption price
       1*(NT+1-TSG0) + & ! bonds (periods TSG0+1 thru NT+1 only)
       1*(NT+1-TSG0) + & ! bond price (periods TSG0 thru NT only)
       (TSG1-TSG0+1) + & ! discount factor wedges for periods TSG0 thru TSG1
       (NT-TSG0+1) ! investment wedges in periods TSGO+1 thru NT+1

  integer :: NEQM1jc = & ! number of vars/eqns for baseline no-saving glut scenario with j-curve
       NC*(NT+2-TSG0) + & ! wages
       NS1(1)*(NT+1-TSG0) + & ! investment in us (period 1 thru NT only)
       NS1(1)*(NT+2-TSG0) +  & ! labor in us
       NS1(2)*(NT+2-TSG0) + & ! labor in rw
       NS1(1)*4*(NT+2-TSG0) + & ! intermediate inputs in us
       NS1(2)*4*(NT+2-TSG0) + & ! intermediate inputs in rw
       NS1(1)*(NT+2-TSG0) + & ! gross output prices in us
       NS1(2)*(NT+2-TSG0) + & ! gross output prices in rw
       2*4*(NT+2-TSG0) + & ! final demand for cons and govt in us
       3*(NT+2-TSG0) + & ! final demand for domestic inv in us
       2*(NT+2-TSG0) + & ! final demand for foreign inv in us
       4*(NT+2-TSG0) + & ! rw consumption
       2*(NT+2-TSG0) + & ! us final demand prices for gov and inv
       4*(NT+2-TSG0) + & ! rw and us consumption price
       1*(NT+1-TSG0) + & ! bonds (periods TSG0+1 thru NT+1 only)
       1*(NT+1-TSG0) + & ! bond price (periods TSG0 thru NT only)
       (TSG1-TSG0+1) + & ! discount factor wedges for periods TSG0 thru TSG1
       (TSG1-TSG0+1) ! trade wedges for periods TSG0 thru TSG1

  integer :: NEQM1bkw = & ! number of vars/eqns for baseline no-saving glut scenario with Buera-Kaboski (2009) wedges
       NC*(NT+2-TSG0) + & ! wages
       NS1(1)*(NT+1-TSG0) + & ! investment in us (period 1 thru NT only)
       NS1(1)*(NT+2-TSG0) +  & ! labor in us
       NS1(2)*(NT+2-TSG0) + & ! labor in rw
       NS1(1)*4*(NT+2-TSG0) + & ! intermediate inputs in us
       NS1(2)*4*(NT+2-TSG0) + & ! intermediate inputs in rw
       NS1(1)*(NT+2-TSG0) + & ! gross output prices in us
       NS1(2)*(NT+2-TSG0) + & ! gross output prices in rw
       2*4*(NT+2-TSG0) + & ! final demand for cons and govt in us
       3*(NT+2-TSG0) + & ! final demand for domestic inv in us
       2*(NT+2-TSG0) + & ! final demand for foreign inv in us
       4*(NT+2-TSG0) + & ! rw consumption
       2*(NT+2-TSG0) + & ! us final demand prices for gov and inv
       4*(NT+2-TSG0) + & ! rw and us consumption price
       1*(NT+1-TSG0) + & ! bonds (periods TSG0+1 thru NT+1 only)
       1*(NT+1-TSG0) + & ! bond price (periods TSG0 thru NT only)
       (TSG1-TSG0+1) + & ! discount factor wedges for periods TSG0 thru TSG1
       (TSG1-TSG0+1) ! BK2009 wedges for periods TSG0 thru TSG1

  integer :: NEQM1jcbkw = & ! number of vars/eqns for baseline no-saving glut scenario with j-curve AND BK wedges
       NC*(NT+2-TSG0) + & ! wages
       NS1(1)*(NT+1-TSG0) + & ! investment in us (period 1 thru NT only)
       NS1(1)*(NT+2-TSG0) +  & ! labor in us
       NS1(2)*(NT+2-TSG0) + & ! labor in rw
       NS1(1)*4*(NT+2-TSG0) + & ! intermediate inputs in us
       NS1(2)*4*(NT+2-TSG0) + & ! intermediate inputs in rw
       NS1(1)*(NT+2-TSG0) + & ! gross output prices in us
       NS1(2)*(NT+2-TSG0) + & ! gross output prices in rw
       2*4*(NT+2-TSG0) + & ! final demand for cons and govt in us
       3*(NT+2-TSG0) + & ! final demand for domestic inv in us
       2*(NT+2-TSG0) + & ! final demand for foreign inv in us
       4*(NT+2-TSG0) + & ! rw consumption
       2*(NT+2-TSG0) + & ! us final demand prices for gov and inv
       4*(NT+2-TSG0) + & ! rw and us consumption price
       1*(NT+1-TSG0) + & ! bonds (periods TSG0+1 thru NT+1 only)
       1*(NT+1-TSG0) + & ! bond price (periods TSG0 thru NT only)
       (TSG1-TSG0+1) + & ! discount factor wedges for periods TSG0 thru TSG1
       (TSG1-TSG0+1)*2 ! trade and BK2009 wedges for periods TSG0 thru TSG1

  integer :: NEQM2 = & ! number of vars/eqns for sudden stop scenario
       NC*(NT+2-TSS0) + & ! wages
       NS1(1)*(NT+1-TSS0) + & ! investment in us (period 1 thru NT only)
       NS1(1)*(NT+2-TSS0) +  & ! labor in us
       NS1(2)*(NT+2-TSS0) + & ! labor in rw
       NS1(1)*4*(NT+2-TSS0) + & ! intermediate inputs in us
       NS1(2)*4*(NT+2-TSS0) + & ! intermediate inputs in rw
       NS1(1)*(NT+2-TSS0) + & ! gross output prices in us
       NS1(2)*(NT+2-TSS0) + & ! gross output prices in rw
       2*4*(NT+2-TSS0) + & ! final demand for cons and govt in us
       3*(NT+2-TSS0) + & ! final demand for domestic inv in us
       2*(NT+2-TSS0) + & ! final demand for foreign inv in us
       4*(NT+2-TSS0) + & ! rw consumption
       2*(NT+2-TSS0) + & ! us final demand prices for gov and inv
       4*(NT+2-TSS0) + & ! rw and us consumption price
       1*(NT+1-TSS0) + & ! bonds (periods TSG0+1 thru NT+1 only)
       1*(NT+1-TSS0)  ! bond price (periods TSG0 thru NT only)

  real(DP) :: bbgp ! balanced growth path bonds (state variable for BGP)
  integer :: scenario ! 0 = baseline no-saving glut model, 1 = saving glut model

  ! ///////////////////////////////////////////////////////////////////////
  ! equilibrium variables
  ! ///////////////////////////////////////////////////////////////////////

  ! ......................................................................
  ! aggregates
  ! ......................................................................

  ! b_t: bonds
  ! bg_t: gov't bonds
  ! qb_t: bond price
  ! rer_t: US real exchange rate
  ! rir_t: US real interest rate
  ! cg_aux_ts: vector for saving government consumption (in quantities)
  real(DP), dimension(NT+1) :: b_t, bg_t, qb_t, pb_t, rer_t, rw_t, rir_t, cg_aux_ts

  ! ii_t: investment
  ! g_t: government spending
  ! ll_t: total labor supply
  ! kk_t: total capital stock
  ! pc_t: consumption price
  ! pi_t: investment price
  ! w_t: wage
  ! rk_t: return on capital
  ! ngdp_t: nominal gdp
  ! rgdp_t: real gdp
  ! iy_t: investment/gdp
  ! om_t: discount factor wedges/shocks/whathaveyou
  ! tw_t: trade wedges for j-curve model
  ! iw_t: investment wedges for the constant investment model
  real(DP), dimension(NC,NT+1) :: ii_t, g_t, ll_t, kk_t, pi_t, w_t, rk_t, ngdp_t, rgdp_t, iy_t, om_t, tw_t, iw_t, cpi_t

  ! exty_t: total exports
  ! imty_t: total imports
  ! nxty_t: total net exports
  ! exfy_t: final exports
  ! imfy_t: final imports
  ! nxfy_t: final net exports
  ! exmy_t: intermediate exports
  ! immy_t: intermediate imports
  ! nxmy_t: intermediate net exports
  ! excy_t: consumption exports
  ! imcy_t: consumption imports
  ! nxcy_t: consumption net exports
  ! exiy_t: investment exports
  ! imiy_t: investment imports
  ! nxiy_t: investment net exports
  real(DP), dimension(NC,NT+1) :: ext_t, imt_t, nxt_t, exf_t, imf_t, nxf_t, exm_t, imm_t, nxm_t
  !excy_t, imcy_t, nxcy_t, exiy_t, imiy_t, nxiy_t

  ! ......................................................................
  ! production
  ! ......................................................................

  ! y_t: gross output
  ! py_t: gross output price
  ! v_t: value added
  ! m_t: intermediate bundle
  ! k_t: capital input
  ! l_t: labor input
  ! i_t: sector-level investment
  ! exsy_t: sector-level exports
  ! imsy_t: sector-level imports
  ! nxsy_t: sector-level net exports
  ! z_t: productivity shock
  real(DP), dimension(NC,NS,NT+1) :: y_t, py_t, va_t, m_t, k_t, l_t, i_t, exs_t, ims_t, nxs_t, lshare_t, bkw_t, z_t, lp_t, rva_t

  ! m1_t: sector-level intermediate bundle
  real(DP), dimension(NC,NS,NS,NT+1) :: m1_t

  ! m2_t: country-sector intermediate inputs
  real(DP), dimension(NC,NS,NS,NC,NT+1) :: m2_t

  ! ......................................................................
  ! final use
  ! ......................................................................

  real(DP), dimension(NC,2,NT+1) :: c_t, pc_t

  ! q_t: final demand
  ! pq_t: final demand prices
  real(DP), dimension(NC,NF,NT+1) :: q_t, pq_t

  ! q1_t: sector-level final demand bundles
  real(DP), dimension(NC,NF,NS,NT+1) :: q1_t

  ! q1_t: country-sector final demand quantities
  real(DP), dimension(NC,NF,NS,NC,NT+1) :: q2_t

  ! ......................................................................
  ! share changes and other data moments we may target
  ! ......................................................................  

  real(DP), dimension(NT+1) :: us_goods_fshare_t ! goods share of final demand
  real(DP), dimension(NT+1) :: us_home_fshare_t ! home share of final demand
  real(DP), dimension(NT+1) :: us_home_fgshare_t ! home share of goods final demand
  real(DP), dimension(NT+1) :: us_home_fsshare_t ! home share of services final demand

  real(DP), dimension(NT+1) :: us_goods_cshare_t ! goods share of consumption
  real(DP), dimension(NT+1) :: us_home_cshare_t !  home share of consumption
  real(DP), dimension(NT+1) :: us_home_cgshare_t ! home share of goods consumption
  real(DP), dimension(NT+1) :: us_home_csshare_t ! home share of services consumption

  real(DP), dimension(NT+1) :: us_goods_ishare_t ! goods share of investment
  real(DP), dimension(NT+1) :: us_home_ishare_t ! home share of investment
  real(DP), dimension(NT+1) :: us_home_igshare_t ! home share of goods investment
  real(DP), dimension(NT+1) :: us_home_isshare_t ! home share of services investment

  real(DP), dimension(NT+1) :: us_m_yshare_t ! intermediate share of gross output
  real(DP), dimension(NT+1) :: us_goods_mshare_t ! goods share of intermediates
  real(DP), dimension(NT+1) :: us_home_mshare_t ! home share of intermediates
  real(DP), dimension(NT+1) :: us_home_mgshare_t ! home share of goods intermediates
  real(DP), dimension(NT+1) :: us_home_msshare_t ! home share of services intermediates

  real(DP), dimension(NT+1) :: rw_goods_yshare_t ! rw goods share of gross output
  real(DP), dimension(NT+1) :: rw_share_worldy_t ! rw share of world gross output
  real(DP), dimension(NT+1) :: rw_yg_share_worldyg_t ! rw goods share of world goods gross output
  real(DP), dimension(NT+1) :: rw_ys_share_worldys_t ! rw services share of world goods gross output

contains

  ! ///////////////////////////////////////////////////////////////////////
  ! equilibrium conditions
  ! ///////////////////////////////////////////////////////////////////////
  real(DP) function mpk_rk_us(i,s,t)
    implicit none
    integer, intent(in) :: i,s,t
    if(k_adj_cost .eq. 0 .or. t .ge. NT) then
       mpk_rk_us = bkw_t(i,s,t) * py_t(i,s,t) * lam(i,s) * alpha(s) * A(i,s)**eta * B(i,s) &
            * (y_t(i,s,t) / va_t(i,s,t))**(ONE-eta) &
            * (k_t(i,s,t)/(gam_t(i,s,t)*z_t(i,s,t)*l_t(i,s,t)))**(alpha(s)-ONE) - &
            rk_t(i,t)/(ONE-tauk)/iw_t(i,t)
    else
       mpk_rk_us = (ONE-tauk)*iw_t(i,t) * bkw_t(i,s,t) * py_t(i,s,t) * lam(i,s) * alpha(s) * A(i,s)**eta * B(i,s) &
            * (y_t(i,s,t) / va_t(i,s,t))**(ONE-eta) &
            * (k_t(i,s,t)/(gam_t(i,s,t)*z_t(i,s,t)*l_t(i,s,t)))**(alpha(s)-ONE) - &
            pq_t(i,3,t-1) * (pq_t(1,1,t)/qb_t(t-1)) / dphiK(i_t(i,s,t-1)/k_t(i,s,t-1)) - &
            (pq_t(i,3,t)/dphiK(i_t(i,s,t)/k_t(i,s,t))) * (dphiK(i_t(i,s,t)/k_t(i,s,t)) * i_t(i,s,t)/k_t(i,s,t) &
            - phiK(i_t(i,s,t)/k_t(i,s,t)) - (ONE-delta))
    endif
    mpk_rk_us = 100.0_dp * mpk_rk_us
  end function mpk_rk_us

  real(DP) function mpl_w(i,s,t)
    implicit none
    integer, intent(in) :: i,s,t

    if(i.eq.1) then
       mpl_w = bkw_t(i,s,t) * py_t(i,s,t) * lam(i,s) * (ONE-alpha(s)) * A(i,s)**eta * B(i,s) * gam_t(i,s,t) * z_t(i,s,t) &
            * (y_t(i,s,t) / va_t(i,s,t))**(ONE-eta) &
            * (k_t(i,s,t)/(gam_t(i,s,t)*z_t(i,s,t) * l_t(i,s,t)))**(alpha(s)) - w_t(i,t)
    else
       mpl_w = bkw_t(i,s,t) * py_t(i,s,t) * lam(i,s) * A(i,s)**eta * B(i,s) * gam_t(i,s,t) * z_t(i,s,t) &
            * (y_t(i,s,t) / va_t(i,s,t))**(ONE-eta) &
            - w_t(i,t)
    endif
    mpl_w = 1.0_dp * mpl_w
  end function mpl_w

  real(DP) function mpm_py(i,s,r,j,t)
    implicit none
    integer, intent(in) :: i,s,r,j,t
    if(noio_flag .eq. 0) then
       if(i.eq.j) then
          mpm_py = bkw_t(i,s,t) * py_t(i,s,t) * (ONE-lam(i,s)) * pi(i,s,r) * mu(i,s,r,j) &
               * A(i,s)**eta * C(i,s)**xi * D(i,s,r)**zeta(r) &
               * (y_t(i,s,t) / m_t(i,s,t))**(ONE-eta) &
               * (m_t(i,s,t) / m1_t(i,s,r,t))**(ONE-xi) &
               * (m1_t(i,s,r,t) / m2_t(i,s,r,j,t))**(ONE-zeta(r)) - py_t(j,r,t)
       else
          mpm_py = tw_t(i,t)**(ONE-zeta(r)) * bkw_t(i,s,t) * py_t(i,s,t) * (ONE-lam(i,s)) * pi(i,s,r) * mu(i,s,r,j) &
               * A(i,s)**eta * C(i,s)**xi * D(i,s,r)**zeta(r) &
               * (y_t(i,s,t) / m_t(i,s,t))**(ONE-eta) &
               * (m_t(i,s,t) / m1_t(i,s,r,t))**(ONE-xi) &
               * (m1_t(i,s,r,t) / m2_t(i,s,r,j,t))**(ONE-zeta(r)) - py_t(j,r,t)
       endif
       mpm_py = 1.0_dp * mpm_py
    else
       mpm_py = m2_t(i,s,r,j,t)
    endif
  end function mpm_py

  real(DP) function mpc_py(i,s,j,t)
    implicit none
    integer, intent(in) :: i,s,j,t
    if(i.eq.j) then
       mpc_py = pc_t(i,s,t) * theta(i,1,s,j) &
            * H(i,1,s)**sig(s) &
            * (c_t(i,s,t) / q2_t(i,1,s,j,t))**(ONE-sig(s)) - py_t(j,s,t)
    else
       mpc_py = tw_t(i,t)**(ONE-sig(s)) * pc_t(i,s,t) * theta(i,1,s,j) &
            * H(i,1,s)**sig(s) &
            * (c_t(i,s,t) / q2_t(i,1,s,j,t))**(ONE-sig(s)) - py_t(j,s,t)
    endif
    mpc_py = 1.0_dp * mpc_py
  end function mpc_py

  real(DP) function mpq_py(i,f,r,j,t)
    implicit none
    integer, intent(in) :: i,f,r,j,t
    mpq_py = pq_t(i,f,t) * eps(i,f,r) * theta(i,f,r,j) &
            * G(i,f)**rho(f) * H(i,f,r)**sig(r) &
            * (q_t(i,f,t) / q1_t(i,f,r,t))**(ONE-rho(f)) &
            * (q1_t(i,f,r,t) / q2_t(i,f,r,j,t))**(ONE-sig(r)) - py_t(j,r,t)
    if(i.ne.j) then
       mpq_py = tw_t(i,t)**(ONE-sig(r)) * mpq_py
    endif
  end function mpq_py

  real(DP) function mkt_clear_y(i,s,t)
    implicit none
    integer, intent(in) :: i,s,t
    mkt_clear_y = y_t(i,s,t) - sum(m2_t(:,:,s,i,t)) - sum(q2_t(:,:,s,i,t))
  end function mkt_clear_y

  real(DP) function mkt_clear_i_us(i,t)
    implicit none
    integer, intent(in) :: i,t
    mkt_clear_i_us = q_t(i,3,t) - ii_t(i,t)
  end function mkt_clear_i_us

  !real(DP) function muc_mul(i,t)
  !  implicit none
  !  integer, intent(in) :: i,t
  !  real(DP) :: leisure

  !  if(fixed_labor_flag .eq. 0) then
       ! we cannot allow for negative leisure
  !     if(lbar_ts(i,t)-ll_t(i,t) .gt. 0.0001) then
  !        leisure=lbar_ts(i,t)-ll_t(i,t)
  !     else
   !       leisure = 0.0001 / log(0.0001-(lbar_ts(i,t)-ll_t(i,t)))
   !    endif

   !    muc_mul = (((1-phi(i))/phi(i)) * &
   !         (c_t(i,t)/pope_ts(i,t)) / (leisure/popw_ts(i,t)) - &
   !         w_t(i,t)/pc_t(i,t))
   ! else
   !    muc_mul = ll_t(i,t) - ll0(i)*popw_ts(i,t)
   ! endif

  !end function muc_mul

  !real(DP) function euler(i,t)
  !  implicit none
  !  integer, intent(in) :: i,t

  !if(scenario .eq. 2 .and. t .le. TSS1 .and. i.eq.2) then
  !     euler = b_t(t+1)-b_t(TSS0)
  !  else
  !     euler = qb_t(t) * om_t(i,t) * &
  !          muc(i,c_t(i,t),ll_t(i,t),lbar_ts(i,t),pope_ts(i,t),popw_ts(i,t)) - & 
  !          beta(i) * pc_t(1,t+1) * (pc_t(i,t)/pc_t(i,t+1)) * om_t(i,t+1) * &
  !          muc(i,c_t(i,t+1),ll_t(i,t+1),lbar_ts(i,t+1),pope_ts(i,t+1),popw_ts(i,t+1))
  !     euler = 10000.0_dp * euler
  !  endif
  !end function euler

 real(DP) function mucg_mucs(i,t)
    implicit none
    integer, intent(in) :: i,t
    !mucg_mucs = (eps(i,1,1)/eps(i,1,2)) * &
    !     ((c_t(i,1,t)-cbar_ts(i,1,t))/(c_t(i,2,t)+cbar_ts(i,2,t)))**(rho(1)-ONE) &
    !     - p_t(i,1,t)/p_t(i,2,t)

    mucg_mucs = eps(i,1,1)*(c_t(i,1,t)-cbar_ts(i,1,t))**(rho(1)-ONE)/pc_t(i,1,t) - &
         eps(i,1,2)*(c_t(i,2,t)+cbar_ts(i,2,t))**(rho(1)-ONE)/pc_t(i,2,t)
    !mucg_mucs = 100.0_dp * mucg_mucs
  end function mucg_mucs

  ! household intratemporal FOC (goods consumption vs leisure)
  real(DP) function muc_mul(i,t)
    implicit none
    integer, intent(in) :: i,t

    if(fixed_labor_flag .eq. 0) then
       muc_mul = muc(i,1,c_t(i,:,t),ll_t(i,t),lbar_ts(i,t),pope_ts(i,t),popw_ts(i,t),cbar_ts(i,:,t))/pc_t(i,1,t)/pope_ts(i,t) &
            - mul(i,c_t(i,:,t),ll_t(i,t),lbar_ts(i,t),pope_ts(i,t),popw_ts(i,t),cbar_ts(i,:,t))/w_t(i,t)/popw_ts(i,t)
       !muc_mul = muc(i,1,c_t(i,:,t),ll_t(i,t),lbar_ts(i,t),pope_ts(i,t),popw_ts(i,t),cbar_ts(i,:,t)) / &
       !     mul(i,c_t(i,:,t),ll_t(i,t),lbar_ts(i,t),pope_ts(i,t),popw_ts(i,t),cbar_ts(i,:,t)) - &
       !     pc_t(i,1,t)/w_t(i,t)
    else
       muc_mul = ll_t(i,t) - ll0(i)*popw_ts(i,t)
    endif

  end function muc_mul

 real(DP) function euler(i,t)
    implicit none
    integer, intent(in) :: i,t

    if(scenario .eq. 2 .and. t .le. TSS1 .and. i.eq.2) then
       euler = b_t(t+1)-b_t(TSS0)
    else
       euler = qb_t(t) * om_t(i,t) * &
            muc(i,1,c_t(i,:,t),ll_t(i,t),lbar_ts(i,t),pope_ts(i,t),popw_ts(i,t),cbar_ts(i,:,t)) - &
            beta(i) * pb_t(t+1) * (pc_t(i,1,t)/pc_t(i,1,t+1)) * om_t(i,t+1) * &
            muc(i,1,c_t(i,:,t+1),ll_t(i,t+1),lbar_ts(i,t+1),pope_ts(i,t+1),popw_ts(i,t+1),cbar_ts(i,:,t+1))
       euler = 10000.0_dp * euler
    endif
  end function euler

  real(DP) function bop(t)
    implicit none
    integer, intent(in) :: t
    if(t.lt.NT+1) then
       if(scenario .eq. 2 .and. t.le.TSS1) then
          bop = nxt_t(1,t) + pb_t(t) * (pb_t(TSS0-1)/qb_t(TSS0-1) - ONE) * (b_t(t)+bg_t(t))
       else
          bop = (nxt_t(1,t) + (bg_t(t) + b_t(t))*pb_t(t) - qb_t(t)*(b_t(t+1)+bg_t(t+1)))
       endif
    else
       bop = (nxt_t(1,t) + (bg_t(t) + b_t(t))*pb_t(t) - qb_t(t)*(bg_t(t) + b_t(t))*gbgp)
    endif
  end function bop

  real(DP) function price_norm(t)
    implicit none
    integer, intent(in) :: t
    price_norm = cpi_t(1,t) - ONE
  end function price_norm

  real(DP) function mkt_clear_g_us(t)
    implicit none
    integer, intent(in) :: t

    if(welfare_flag .eq. 1 .and. welfare_gsaved_flag .eq. 1) then
       mkt_clear_g_us = g_t(1,t) - cg_aux_ts(t)
    else
       mkt_clear_g_us = 100.0_dp * pq_t(1,2,t) * g_t(1,t) / ngdp_t(1,t) - cg_pct_ts(scenario+1,t)
    endif
  end function mkt_clear_g_us

  real(DP) function match_tbdata(t)
    implicit none
    integer, intent(in) :: t
    match_tbdata = (100.0_dp * nxt_t(1,t) / ngdp_t(1,t) - tbdata_ts(t))
    match_tbdata = match_tbdata
  end function match_tbdata

  real(DP) function match_rerdata(t)
    implicit none
    integer, intent(in) :: t
    match_rerdata = rer_t(t) - rerdata_ts(t)
    match_rerdata = match_rerdata
  end function match_rerdata

  real(DP) function match_inv(t)
    implicit none
    integer, intent(in) :: t
    match_inv = 100.0_dp * pq_t(1,3,t) * ii_t(1,t) / ngdp_t(1,t) - ii0(1)
  end function match_inv

  real(DP) function match_lg(t)
    implicit none
    integer, intent(in) :: t
    match_lg = lshare_t(1,1,t) - lgdata_ts(t)
  end function match_lg

  ! ///////////////////////////////////////////////////////////////////////
  ! initialize all equilibrium vars to zero
  ! ///////////////////////////////////////////////////////////////////////
  subroutine init_vars()
    z_t(:,:,:) = ONE
    bkw_t(:,:,:) = ONE
    om_t(:,:) = ONE
    tw_t(:,:) = ONE
    iw_t(:,:) = ONE
    b_t(:) = ZERO
    qb_t(:) = ZERO
    rer_t(:) = ZERO
    rw_t(:) = ZERO
    c_t(:,:,:) = ZERO
    pc_t(:,:,:) = ZERO
    ii_t(:,:) = ZERO
    g_t(:,:) = ZERO
    ll_t(:,:) = ZERO
    kk_t(:,:) = ZERO
    pi_t(:,:) = ZERO
    w_t(:,:) = ZERO
    rk_t(:,:) = ZERO
    ngdp_t(:,:) = ZERO
    rgdp_t(:,:) = ZERO
    iy_t(:,:) = ZERO
    lshare_t(:,:,:) = ZERO
    ext_t(:,:) = ZERO
    imt_t(:,:) = ZERO
    nxt_t(:,:) = ZERO
    exf_t(:,:) = ZERO
    imf_t(:,:) = ZERO
    nxf_t(:,:) = ZERO
    exm_t(:,:) = ZERO
    imm_t(:,:) = ZERO
    nxm_t(:,:) = ZERO
    y_t(:,:,:) = ZERO
    py_t(:,:,:) = ZERO
    va_t(:,:,:) = ZERO
    m_t(:,:,:) = ZERO
    k_t(:,:,:) = ZERO
    l_t(:,:,:) = ZERO
    i_t(:,:,:) = ZERO
    exs_t(:,:,:) = ZERO
    ims_t(:,:,:) = ZERO
    nxs_t(:,:,:) = ZERO
    m1_t(:,:,:,:) = ZERO
    m2_t(:,:,:,:,:) = ZERO
    q_t(:,:,:) = ZERO
    pq_t(:,:,:) = ZERO
    q1_t(:,:,:,:) = ZERO
    q2_t(:,:,:,:,:) = ZERO

  end subroutine init_vars

  ! ///////////////////////////////////////////////////////////////////////
  ! vectorize BGP variables for use in solver
  ! ///////////////////////////////////////////////////////////////////////
  subroutine vectorize_bgp_vars(myX,dir)

    implicit none

    integer, intent(in) :: dir ! 0 = stack time series vars, 1 = unstack X vector
    real(DP), dimension(NBGP), intent(inout) :: myX

    integer :: nx ! count variables

    nx=0

    if(dir .eq. 0) then

       ! wage rate in each country
       myX( (nx+1) : (nx+NC) ) = log(pack(w_t(:,NT+1),.true.))
       nx = nx+NC

       ! capital in us
       myX( (nx+1) : (nx+NS1(1)) ) = log(pack(k_t( 1, 1:NS1(1), NT+1 ),.true.))
       nx = nx+NS1(1)

       ! labor in us (first NS(1)-1 sectors)
       myX( (nx+1) : (nx+NS1(1)) ) = log(pack(l_t( 1, 1:(NS1(1)), NT+1) ,.true.))
       nx = nx+NS1(1)

       ! labor in rw
       myX( (nx+1) : (nx+NS1(2)) ) = log(pack(l_t( 2, 1:(NS1(2)), NT+1 ),.true.))
       nx = nx+NS1(2)

       ! intermediate inputs in us
       ! note only need goods and services from each country (4 values)
       if(noio_flag .eq. 0) then
          myX( (nx+1) : (nx+NS1(1)*4) ) = log(pack(m2_t( 1, 1:NS1(1), 1:2, 1:2, NT+1 ), .true.))
       else
          myX( (nx+1) : (nx+NS1(1)*4) ) = (pack(m2_t( 1, 1:NS1(1), 1:2, 1:2, NT+1 ), .true.))
       endif
       nx = nx+NS1(1)*4

       ! intermediate inputs in rw
       ! same 4 as above, except only for 2 destinations (goods and services) rather than 3
       if(noio_flag .eq. 0) then
          myX( (nx+1) : (nx+NS1(2)*4) ) = log(pack(m2_t( 2, 1:NS1(2), 1:2, 1:2, NT+1 ), .true.))
       else
          myX( (nx+1) : (nx+NS1(2)*4) ) = (pack(m2_t( 2, 1:NS1(2), 1:2, 1:2, NT+1 ), .true.))
       endif
       nx = nx+NS1(2)*4

       ! gross output prices in us
       myX( (nx+1) : (nx+NS1(1)) ) = log(pack(py_t( 1, 1:NS1(1), NT+1 ), .true.))
       nx = nx+NS1(1)

       ! gross output prices in rw
       myX( (nx+1) : (nx+NS1(2)) ) = log(pack(py_t( 2, 1:NS1(2), NT+1 ), .true.))
       nx = nx+NS1(2)

       ! final demand for cons and govt in us
       myX( (nx+1) : (nx+2*4) ) = log(pack(q2_t( 1, 1:2, 1:2, 1:2, NT+1 ), .true.))
       nx = nx+2*4

       ! final demand for us domestic investment inputs
       myX( (nx+1) : (nx+3) ) = log(pack(q2_t( 1, 3, 1:3, 1, NT+1 ), .true.))
       nx = nx+3

       ! final demand for us foreign investment inputs
       myX( (nx+1) : (nx+2) ) = log(pack(q2_t( 1, 3, 1:2, 2, NT+1 ), .true.))
       nx = nx+2

       ! final demand for rw consumption
       myX( (nx+1) : (nx+4) ) = log(pack(q2_t( 2, 1, 1:2, 1:2, NT+1 ), .true.))
       nx = nx+4

       ! final demand prices for gov and inv
       myX( (nx+1) : (nx+3) ) = log(pack(pq_t(1, 2:NF1(1), NT+1 ), .true.))
       nx = nx+2

       ! consumption prices
       myX( (nx+1) : (nx+1) ) = log(pack(pc_t(1:2, 1:2, NT+1 ), .true.))
       nx = nx+4

    else

       w_t(:,NT+1) = exp(reshape(myX( (nx+1) : (nx+NC) ), &
            (/NC/) ))
       nx = nx+NC

       k_t(1,1:NS1(1),NT+1) = exp(reshape(myX( (nx+1) : (nx+NS1(1)) ), &
            (/NS1(1)/) ))
       nx = nx+NS1(1)

       l_t(1,1:(NS1(1)),NT+1) = exp(reshape(myX( (nx+1) : (nx+NS1(1)) ), &
            (/NS1(1)/) ))
       nx = nx+NS1(1)

       l_t(2,1:(NS1(2)),NT+1) = exp(reshape(myX( (nx+1) : (nx+NS1(2)) ), &
            (/NS1(2)/) ))
       nx = nx+NS1(2)

       if(noio_flag .eq. 0) then
          m2_t(1,1:NS1(1),1:2,1:2,NT+1) = exp(reshape(myX( (nx+1) : (nx+NS1(1)*4) ), &
               (/NS1(1),2,2/) ))
       else
          m2_t(1,1:NS1(1),1:2,1:2,NT+1) = (reshape(myX( (nx+1) : (nx+NS1(1)*4) ), &
               (/NS1(1),2,2/) ))
       endif
       nx = nx+NS1(1)*4

       if(noio_flag .eq. 0) then
          m2_t(2,1:NS1(2),1:2,1:2,NT+1) = exp(reshape(myX( (nx+1) : (nx+NS1(2)*4) ), &
               (/NS1(2),2,2/) ))
       else
          m2_t(2,1:NS1(2),1:2,1:2,NT+1) = (reshape(myX( (nx+1) : (nx+NS1(2)*4) ), &
               (/NS1(2),2,2/) ))
       endif
       nx = nx+NS1(2)*4

       py_t(1,1:NS1(1),NT+1) = exp(reshape(myX( (nx+1) : (nx+NS1(1)) ), &
            (/NS1(1)/) ))
       nx = nx+NS1(1)

       py_t(2,1:NS1(2),NT+1) = exp(reshape(myX( (nx+1) : (nx+NS1(2)) ), &
            (/NS1(2)/) ))
       nx = nx+NS1(2)

       q2_t(1, 1:2, 1:2, 1:2, NT+1) = exp(reshape(myX( (nx+1) : (nx+2*4) ), &
            (/2,2,2/) ))
       nx = nx+2*4

       q2_t(1, 3, 1:3, 1, NT+1) = exp(reshape(myX( (nx+1) : (nx+3) ), &
            (/3/) ))
       nx = nx+3

       q2_t(1, 3, 1:2, 2, NT+1) = exp(reshape(myX( (nx+1) : (nx+2) ), &
            (/2/) ))
       nx = nx+2

       q2_t(2, 1, 1:2, 1:2, NT+1) = exp(reshape(myX( (nx+1) : (nx+4) ), &
            (/2,2/) ))
       nx = nx+4         

       pq_t(1, 2:3, NT+1) = exp(reshape(myX( (nx+1) : (nx+2) ), &
            (/2/) ))
       nx = nx+2

       pc_t(1:2, 1:2, NT+1) = exp(reshape(myX( (nx+1) : (nx+4) ), &
            (/2,2/) ))
       nx = nx+4

    endif

    call assert1(nx.eq.NBGP,'nx != NBGP in vectorize_bgp_eqs! nx = '//trim(str(nx))//', NBGP = '//trim(str(NBGP)))      

  end subroutine vectorize_bgp_vars

  ! ///////////////////////////////////////////////////////////////////////
  ! vectorize equilibrium variables for use in solver
  ! ///////////////////////////////////////////////////////////////////////
  subroutine vectorize_eqm_vars(myX,dir)

    implicit none

    integer, intent(in) :: dir ! 0 = stack time series vars, 1 = unstack X vector
    real(DP), dimension(:), intent(inout) :: myX

    integer :: t0, TT, nx, NN ! count variables

    nx=0

    if(scenario .eq. 0) then ! baseline scenario without saving glut
       t0 = 1
       if(fixed_inv_flag .eq. 0) then
          NN = NEQM0
       else
          NN = NEQM0fi
       endif
    else if(scenario .eq. 1) then ! saving glut
       t0 = TSG0
       if (fixed_inv_flag .eq. 1) then
          NN = NEQM1fi
       else if(jcurve_flag .eq. 1 .and. bk2009_flag .eq. 0) then
          NN = NEQM1jc
       else if(bk2009_flag .eq. 1 .and. jcurve_flag .eq. 0) then
          NN = NEQM1bkw
       else if(bk2009_flag .eq. 1 .and. jcurve_flag .eq. 1) then
          NN = NEQM1jcbkw
       else
          NN = NEQM1
       endif
    else if(scenario .eq. 2) then
       t0 = TSS0
       NN = NEQM2
    else
       stop 'invalid scenario number supplied to vectorize_eqm_vars!'
    end if
    TT = NT+2-t0

    if(dir .eq. 0) then

       ! wage rate in each country
       myX( (nx+1) : (nx+NC*TT) ) = log(pack(w_t(:,t0:(NT+1) ),.true.))
       nx = nx+NC*TT

       ! investment (periods t0 through NT only --- investment in period NT pins down
       ! capital in period NT+1)
       if(k_adj_cost .eq. 0) then
          myX( (nx+1) : (nx+NS1(1)*(TT-1)) ) = pack(i_t( 1, 1:NS1(1), t0:NT ),.true.)
       else
          myX( (nx+1) : (nx+NS1(1)*(TT-1)) ) = log(pack(i_t( 1, 1:NS1(1), t0:NT ),.true.))
       endif
       nx = nx+NS1(1)*(TT-1)

       ! labor in us
       myX( (nx+1) : (nx+NS1(1)*TT) ) = log(pack(l_t( 1, 1:NS1(1), t0:(NT+1) ) ,.true.))
       nx = nx+NS1(1)*TT

       ! labor in rw
       myX( (nx+1) : (nx+NS1(2)*TT) ) = log(pack(l_t( 2, 1:NS1(2), t0:(NT+1) ),.true.))
       nx = nx+NS1(2)*TT

       ! intermediate inputs in us
       ! note only need goods and services from each country (4 values)
       if(noio_flag .eq. 0) then
          myX( (nx+1) : (nx+NS1(1)*4*TT) ) = log(pack(m2_t( 1, 1:NS1(1), 1:2, 1:2, t0:(NT+1) ), .true.))
       else
          myX( (nx+1) : (nx+NS1(1)*4*TT) ) = (pack(m2_t( 1, 1:NS1(1), 1:2, 1:2, t0:(NT+1) ), .true.))
       endif
       nx = nx+NS1(1)*4*TT

       ! intermediate inputs in rw
       ! same 4 as above, except only for 2 destinations (goods and services) rather than 3
       if(noio_flag .eq. 0) then
          myX( (nx+1) : (nx+NS1(2)*4*TT) ) = log(pack(m2_t( 2, 1:NS1(2), 1:2, 1:2, t0:(NT+1) ), .true.))
       else
          myX( (nx+1) : (nx+NS1(2)*4*TT) ) = (pack(m2_t( 2, 1:NS1(2), 1:2, 1:2, t0:(NT+1) ), .true.))
       endif
       nx = nx+NS1(2)*4*TT

       ! gross output prices in us
       myX( (nx+1) : (nx+NS1(1)*TT) ) = log(pack(py_t( 1, 1:NS1(1), t0:(NT+1) ), .true.))
       nx = nx+NS1(1)*TT

       ! gross output prices in rw
       myX( (nx+1) : (nx+NS1(2)*TT) ) = log(pack(py_t( 2, 1:NS1(2), t0:(NT+1) ), .true.))
       nx = nx+NS1(2)*TT

       ! final demand for cons and govt in us
       myX( (nx+1) : (nx+2*4*TT) ) = log(pack(q2_t( 1, 1:2, 1:2, 1:2, t0:(NT+1) ), .true.))
       nx = nx+2*4*TT

       ! final demand for us domestic investment inputs
       myX( (nx+1) : (nx+3*TT) ) = log(pack(q2_t( 1, 3, 1:3, 1, t0:(NT+1) ), .true.))
       nx = nx+3*TT

       ! final demand for us foreign investment inputs
       myX( (nx+1) : (nx+2*TT) ) = log(pack(q2_t( 1, 3, 1:2, 2, t0:(NT+1) ), .true.))
       nx = nx+2*TT

       ! final demand for rw consumption
       myX( (nx+1) : (nx+4*TT) ) = log(pack(q2_t( 2, 1, 1:2, 1:2, t0:(NT+1) ), .true.))
       nx = nx+4*TT

       ! final demand prices for gov and inv
       myX( (nx+1) : (nx+2*TT) ) = log(pack(pq_t(1, 2:3, t0:(NT+1) ), .true.))
       nx = nx+2*TT

       ! consumption prices
       myX( (nx+1) : (nx+4*TT) ) = log(pack(pc_t(1:2, 1:2, t0:(NT+1) ), .true.))
       nx = nx+4*TT

       ! bonds
       myX( (nx+1) : (nx+1*(TT-1) ) ) = pack(b_t( (t0+1):(NT+1) ), .true.)
       nx = nx+1*(TT-1)

       ! bond price
       myX( (nx+1) : (nx+1*(TT-1) ) ) = log(pack(qb_t( (t0):(NT) ), .true.))
       nx = nx+1*(TT-1)

       if(scenario .eq. 0) then ! baseline no-sg scenario
          ! first-period preference shock to pin down trade balance
          myX(nx+1) = log(om_t(iom,1))
          nx=nx+1

          ! second-period capital tax to pin down investment
          ! EDIT: investment wedge in period 2
          myX(nx+1) = log(iw_t(1,2))
          nx=nx+1

          if(fixed_inv_flag .eq. 1) then
             myX( (nx+1):(nx+(NT-1)) ) = log(iw_t(1,3:(NT+1)))
             nx = nx + (NT-1)   
          endif
       else if(scenario .eq. 1) then
          myX( (nx+1):(nx+(TSG1-TSG0+1)) ) = log(pack(om_t( iom, t0:TSG1 ),.true.))
          nx=nx+(TSG1-TSG0+1)

          if (jcurve_flag .eq. 1) then
             myX( (nx+1):(nx+(TSG1-TSG0+1)) ) = log(pack(tw_t( 2, t0:TSG1 ),.true.))
             nx=nx+(TSG1-TSG0+1)
          endif

          if(bk2009_flag .eq. 1) then
             myX( (nx+1):(nx+(TSG1-TSG0+1)) ) = log(pack(bkw_t( 1, 1, t0:TSG1 ),.true.))
             nx=nx+(TSG1-TSG0+1)
          endif

          if(fixed_inv_flag.eq.1) then
             myX( (nx+1):(nx+(NT-TSG0+1)) ) = log(iw_t(1,(TSG0+1):(NT+1)))
             nx = nx + (NT-TSG0+1)   
          endif

       endif

    else

       w_t(:,t0:(NT+1)) = exp(reshape(myX( (nx+1) : (nx+NC*TT) ), &
            (/NC,TT/) ))
       nx = nx+NC*TT

       if(k_adj_cost .eq. 0) then
          i_t(1,1:NS1(1),t0:(NT)) = reshape(myX( (nx+1) : (nx+NS1(1)*(TT-1)) ), &
               (/NS1(1),TT-1/) )
       else
          i_t(1,1:NS1(1),t0:(NT)) = exp(reshape(myX( (nx+1) : (nx+NS1(1)*(TT-1)) ), &
               (/NS1(1),TT-1/) ))
       endif
       nx = nx+NS1(1)*(TT-1)

       l_t(1,1:NS1(1),t0:(NT+1)) = exp(reshape(myX( (nx+1) : (nx+NS1(1)*TT) ), &
            (/NS1(1),TT/) ))
       nx = nx+NS1(1)*TT

       l_t(2,1:NS1(2),t0:(NT+1)) = exp(reshape(myX( (nx+1) : (nx+NS1(2)*TT) ), &
            (/NS1(2),TT/) ))
       nx = nx+NS1(2)*TT

       if(noio_flag .eq.0) then
          m2_t(1,1:NS1(1),1:2,1:2,t0:(NT+1)) = exp(reshape(myX( (nx+1) : (nx+NS1(1)*4*TT) ), &
               (/NS1(1),2,2,TT/) ))
       else
          m2_t(1,1:NS1(1),1:2,1:2,t0:(NT+1)) = (reshape(myX( (nx+1) : (nx+NS1(1)*4*TT) ), &
               (/NS1(1),2,2,TT/) ))
       endif
       nx = nx+NS1(1)*4*TT

       if(noio_flag .eq. 0) then
          m2_t(2,1:NS1(2),1:2,1:2,t0:(NT+1)) = exp(reshape(myX( (nx+1) : (nx+NS1(2)*4*TT) ), &
               (/NS1(2),2,2,TT/) ))
       else
          m2_t(2,1:NS1(2),1:2,1:2,t0:(NT+1)) = (reshape(myX( (nx+1) : (nx+NS1(2)*4*TT) ), &
               (/NS1(2),2,2,TT/) ))
       endif
       nx = nx+NS1(2)*4*TT

       py_t(1,1:NS1(1),t0:(NT+1)) = exp(reshape(myX( (nx+1) : (nx+NS1(1)*TT) ), &
            (/NS1(1),TT/) ))
       nx = nx+NS1(1)*TT

       py_t(2,1:NS1(2),t0:(NT+1)) = exp(reshape(myX( (nx+1) : (nx+NS1(2)*TT) ), &
            (/NS1(2),TT/) ))
       nx = nx+NS1(2)*TT

       q2_t(1, 1:2, 1:2, 1:2, t0:(NT+1)) = exp(reshape(myX( (nx+1) : (nx+2*4*TT) ), &
            (/2,2,2,TT/) ))
       nx = nx+2*4*TT

       q2_t(1, 3, 1:3, 1, t0:(NT+1)) = exp(reshape(myX( (nx+1) : (nx+3*TT) ), &
            (/3,TT/) ))
       nx = nx+3*TT

       q2_t(1, 3, 1:2, 2, t0:(NT+1)) = exp(reshape(myX( (nx+1) : (nx+2*TT) ), &
            (/2,TT/) ))
       nx = nx+2*TT

       q2_t(2, 1, 1:2, 1:2, t0:(NT+1)) = exp(reshape(myX( (nx+1) : (nx+4*TT) ), &
            (/2,2,TT/) ))
       nx = nx+4*TT     

       pq_t(1, 2:3, t0:(NT+1)) = exp(reshape(myX( (nx+1) : (nx+2*TT) ), &
            (/2,TT/) ))
       nx = nx+2*TT

       pc_t(1:2, 1:2, t0:(NT+1)) = exp(reshape(myX( (nx+1) : (nx+4*TT) ), &
            (/2,2,TT/) ))
       nx = nx+4*TT

       b_t( (t0+1):(NT+1) ) = myX( (nx+1) : (nx+1*(TT-1) ) )
       nx = nx+1*(TT-1)

       qb_t( (t0):(NT) ) = exp(myX( (nx+1) : (nx+1*(TT-1) ) ))
       nx = nx+1*(TT-1)

       if(scenario .eq. 0) then ! baseline no-sg scenario
          ! first-period preference shock to pin down trade balance
          om_t(iom,1) = exp(myX(nx+1))
          nx=nx+1

          ! second-period investment wedge
          iw_t(1,2) = exp(myX(nx+1))
          nx=nx+1

          if(fixed_inv_flag .eq. 1) then
             iw_t(1,3:(NT+1)) = exp(myX( (nx+1):(nx+(NT-1)) ))
             nx = nx + (NT-1)   
          endif

       else if(scenario .eq. 1) then
          om_t(iom, t0:TSG1) = exp(myX( (nx+1):(nx+(TSG1-TSG0+1)) ))
          nx=nx+(TSG1-TSG0+1)

          if(jcurve_flag .eq. 1) then
             tw_t(2, t0:TSG1) = exp(myX( (nx+1):(nx+(TSG1-TSG0+1)) ))
             nx=nx+(TSG1-TSG0+1)
          endif
          if(bk2009_flag .eq. 1) then
             bkw_t(1, 1, t0:TSG1) = exp(myX( (nx+1):(nx+(TSG1-TSG0+1)) ))
             nx=nx+(TSG1-TSG0+1)
          endif
          if(fixed_inv_flag.eq.1) then
             iw_t(1,(TSG0+1):(NT+1)) = exp(myX( (nx+1):(nx+(NT-TSG0+1)) ))
             nx = nx + (NT-TSG0+1)
          endif

       endif

    endif

    call assert1(nx.eq.NN,'nx != NF in vectorize_eqm_vars! nx = '//trim(str(nx))//', NN = '//trim(str(NN)))      

  end subroutine vectorize_eqm_vars

  ! ///////////////////////////////////////////////////////////////////////
  ! read/write seed files for solver guesses
  ! ///////////////////////////////////////////////////////////////////////
  subroutine write_seed(myX,fname)

    implicit none

    real(DP), dimension(:), intent(inout) :: myX
    character(len=*), intent(in) :: fname
    integer :: i

    open(unit=1, file=trim(OUTPATH)//trim(fname), form='formatted', & 
         action='write', status='replace')
    do i=1,size(myX)
       write(1,*) myX(i)
    enddo
    close(1)
  end subroutine write_seed

  subroutine read_seed(myX,fname)

    implicit none

    real(DP), dimension(:), intent(inout) :: myX
    character(len=*), intent(in) :: fname
    integer :: i, iostatus, nlines

    nlines=0
    open(unit=1, file=trim(OUTPATH)//trim(fname), form='formatted', & 
         action='read')
    do i=1,size(myX)
       read(1,*,iostat=iostatus) myX(i)
       if(iostatus .eq. 0) then
          nlines = nlines+1
          if(iostatus < 0) then
             ! if we don't have enough lines to fill up myX...
             if(nlines<size(myX)) then
                ! if we are in the fixed investment scenario, we are probably using a 
                ! seed from a previous run with variable investment, in which case we
                ! should initialize the investment wedges
                if(fixed_inv_flag .eq. 1) then
                   iw_t(1,2:(NT+1)) = ONE
                   exit
                ! otherwise there is something wrong and we should abort
                else
                   stop 'Error reading seed file! Not enough lines'
                endif
             end if
          end if
       endif
    enddo
    close(1)
    call vectorize_eqm_vars(myX,1)

  end subroutine read_seed

  ! ///////////////////////////////////////////////////////////////////////
  ! set initial guess for finding balanced growth path
  ! ///////////////////////////////////////////////////////////////////////
  subroutine set_initial_bgp_guess(myX)

    real(DP), dimension(:), intent(inout) :: myX
    integer :: t, i, s, f
    !real(DP) = tmp = 0.5_dp

    t = NT+1

    do i=1,NC
       w_t(i,t) = ONE
       do s=1,NS1(i)
          l_t(i,s,t)  = l0(i,s) * (lbar_ts(i,t)/lbar0(i))
          if(i.eq.1) then
             k_t(i,s,t) = k0(i,s) * gam_t(i,s,t)
          endif
          py_t(i,s,t) = ONE
          m2_t(i,s,:,:,t) = m02(i,s,:,:) * gam_t(i,s,t)
       end do
       pc_t(:,:,:) = ONE
       do f=1,NF1(i)
          pq_t(i,f,t) = ONE
          q2_t(i,f,:,:,t) = q02(i,f,:,:) * gam_t(i,f,t)
       enddo
    enddo
    call vectorize_bgp_vars(myX,0)

  end subroutine set_initial_bgp_guess

  ! ///////////////////////////////////////////////////////////////////////
  ! create initial guess for equilibrium
  ! ///////////////////////////////////////////////////////////////////////
  subroutine set_initial_eqm_guess(myX,retstat)
    real(DP), dimension(NEQM0), intent(inout) :: myX
    integer, intent(inout) :: retstat
    integer :: i,s,r,j,t,f
    real(DP) :: b

    ! first find balanced growth path associated with constantly-growing bonds
    b = b0*(gbgp**NT)
    call solve_bgp(b,retstat)
    if(retstat.eq.1) then
       stop 'Failed to create initial equilibrium guess! Return status from solve_bgp is 1!'
    endif
    call logspace(b_t,abs(b0),abs(b),NT+1)
    if(b0.lt.-TINY) then
       b_t(:) = -b_t(:)
    endif
    qb_t(:) = qb_t(NT+1)      

    ! now guess real variables grow exponentially between base period and the steady state,
    ! and prices grow linearly
    !do t=1,NT+1
       do i=1,NC
          call linspace(w_t(i,:),ONE,w_t(i,NT+1),NT+1)
          do s=1,NS1(i)
             if(i.eq.1) then
                call logspace(k_t(i,s,:),k0(i,s),k_t(i,s,NT+1),NT+1)
             endif
             call logspace(l_t(i,s,:),l0(i,s),l_t(i,s,NT+1),NT+1)
             call linspace(py_t(i,s,:),ONE,py_t(i,s,NT+1),NT+1)
             if(noio_flag .eq. 0) then
                do r=1,2
                   do j=1,NC
                      call logspace(m2_t(i,s,r,j,:),m02(i,s,r,j),m2_t(i,s,r,j,NT+1),NT+1)
                   enddo
                end do
             else
                m2_t(:,:,:,:,:) = ZERO
             endif
          enddo
          do f=1,NF1(i)
             if(f.gt.1) then
                call linspace(pq_t(i,f,:),ONE,pq_t(i,f,NT+1),NT+1)
             else
                call linspace(pc_t(i,1,:),ONE,pc_t(i,1,NT+1),NT+1)
                call linspace(pc_t(i,2,:),ONE,pc_t(i,2,NT+1),NT+1)
             endif
             do r=1,2
                do j=1,2
                   call logspace(q2_t(i,f,r,j,:),q02(i,f,r,j),q2_t(i,f,r,j,NT+1),NT+1)
                enddo
             enddo
          enddo
          if(i.eq.1) then
             call logspace(q2_t(i,3,3,1,:),q02(i,3,3,1),q2_t(i,3,3,1,NT+1),NT+1)
             call linspace(pq_t(i,3,:),ONE,pq_t(i,3,NT+1),NT+1)
          endif
       enddo
    !enddo

    ! now back out implied investment series
    do t=1,NT
       i_t(1,:,t) = k_t(1,:,t+1)-(ONE-delta)*k_t(1,:,t)
    enddo

    ! guess discount factor wedge is always one
    om_t(2,:) = ONE
    tw_t(2,:) = ONE

    ! capital income tax in period 2
    !tauk1 = tauk

    ! investment wedge in period 2
    iw_t(:,:) = ONE

    call vectorize_eqm_vars(myX,0)

    retstat=0

  end subroutine set_initial_eqm_guess

  ! ///////////////////////////////////////////////////////////////////////
  ! write balanced growth path equilibrium to file
  ! ///////////////////////////////////////////////////////////////////////
  subroutine write_bgp_vars()

    implicit none

    integer :: t,i,s,f

    t=NT+1

    open(unit=1, file=trim(OUTPATH)//'bgp.txt', form='formatted', &
         action='write', status='replace')

    call csv_write(1,'private bonds (exogenous state) ',.true.)
    call csv_write_dble(lun=1,value = b_t(t),advance=.true.)
    write(1,*)

    call csv_write(1,'government bonds ',.true.)
    call csv_write_dble(lun=1,value = bg_t(t),advance=.true.)
    write(1,*)

    call csv_write(1,'bond price',.true.)
    call csv_write(1,qb_t(t),.true.)
    write(1,*)

    call csv_write(1,'wages',.true.)
    call csv_write_dble_1d(lun=1,array=w_t(:,t),advance=.true.)
    write(1,*)

    call csv_write(1,'capital',.true.)
    call csv_write_dble_1d(lun=1,array=k_t(1,:,t),advance=.true.)
    write(1,*)

    call csv_write(1,'labor',.true.)
    do i=1,NC
       call csv_write_dble_1d(lun=1,array=l_t(i,:,t),advance=.true.)
    enddo
    write(1,*)

    call csv_write(1,'producer prices',.true.)
    do i=1,NC
       call csv_write_dble_1d(lun=1,array=py_t(i,:,t),advance=.true.)
    enddo
    write(1,*)

    call csv_write(1,'intermediate inputs',.true.)
    do i=1,NC
       do s=1,NS1(i)
          call csv_write_dble_2d(lun=1,array=m2_t(i,s,:,:,t))
       enddo
    enddo
    write(1,*)

    call csv_write(1,'gross output',.true.)
    call csv_write_dble_2d(lun=1,array=y_t(:,:,t))
    write(1,*)

    call csv_write(1,'labor productivity',.true.)
    call csv_write_dble_2d(lun=1,array=gam_t(:,:,t))
    write(1,*)

    call csv_write(1,'retailer prices',.true.)
    do i=1,NC
       call csv_write_dble_1d(lun=1,array=pq_t(i,:,t),advance=.true.)
    enddo
    write(1,*)

    call csv_write(1,'retail inputs',.true.)
    do i=1,NC
       do f=1,NF1(i)
          call csv_write_dble_2d(lun=1,array=q2_t(i,f,:,:,t))
       enddo
    enddo
    write(1,*)

    call csv_write(1,'net exports',.true.)
    call csv_write_dble_1d(lun=1,array=nxt_t(:,t))
    write(1,*)

    call csv_write(1,'nominal gdp',.true.)
    call csv_write_dble_1d(lun=1,array=ngdp_t(:,t))
    write(1,*)

    call csv_write(1,'real gdp',.true.)
    call csv_write_dble_1d(lun=1,array=rgdp_t(:,t))
    write(1,*)

    call csv_write(1,'goods labor share',.true.)
    call csv_write(lun=1,array=l_t(:,1,t)/ll_t(:,t))
    write(1,*)

    call csv_write(1,'K/GDP (US)',.true.)
    call csv_write_dble(1,kk_t(1,t)/rgdp_t(1,t),advance=.true.)
    write(1,*)

    call csv_write(1,'GDP per adult equivalent (1990 = 100)',.true.)
    call csv_write(1,(rgdp_t(:,t)/pope_ts(:,t))/(va0(:,1)+va0(:,2)+va0(:,3)))
    write(1,*)


    close(1)
  end subroutine write_bgp_vars

  ! ///////////////////////////////////////////////////////////////////////
  ! write equilibrium variables to csv file
  ! ///////////////////////////////////////////////////////////////////////
  subroutine write_eqm_vars(fname)
    implicit none
    character(len=*), intent(in) :: fname
    integer :: i,s,r,j,f,t

    !open file
    open(unit=1,file=trim(OUTPATH)//trim(fname),status='replace',action='write')

    ! first write columns labels
    call csv_write(1,'b',.false.)
    call csv_write(1,'bg',.false.)
    call csv_write(1,'qb',.false.)
    call csv_write(1,'rer',.false.)
    call csv_write(1,'rw',.false.)
    call csv_write(1,'rir',.false.)
    do i=1,NC
       call csv_write(1,'ll('//trim(str(i))//')',.false.)
       if(i.eq.1) then
          call csv_write(1,'g('//trim(str(i))//')',.false.)
          call csv_write(1,'kk('//trim(str(i))//')',.false.)
          call csv_write(1,'ii('//trim(str(i))//')',.false.)
          call csv_write(1,'pi('//trim(str(i))//')',.false.)
          call csv_write(1,'rk('//trim(str(i))//')',.false.)
          call csv_write(1,'iy('//trim(str(i))//')',.false.)
          call csv_write(1,'iw('//trim(str(i))//')',.false.)
       endif
       call csv_write(1,'w('//trim(str(i))//')',.false.)
       call csv_write(1,'ngdp('//trim(str(i))//')',.false.)
       call csv_write(1,'rgdp('//trim(str(i))//')',.false.)
       call csv_write(1,'om('//trim(str(i))//')',.false.)
       call csv_write(1,'tw('//trim(str(i))//')',.false.)
       call csv_write(1,'exty('//trim(str(i))//')',.false.)
       call csv_write(1,'imty('//trim(str(i))//')',.false.)
       call csv_write(1,'nxty('//trim(str(i))//')',.false.)
       call csv_write(1,'exfy('//trim(str(i))//')',.false.)
       call csv_write(1,'imfy('//trim(str(i))//')',.false.)
       call csv_write(1,'nxfy('//trim(str(i))//')',.false.)
       call csv_write(1,'exmy('//trim(str(i))//')',.false.)
       call csv_write(1,'immy('//trim(str(i))//')',.false.)
       call csv_write(1,'nxmy('//trim(str(i))//')',.false.)
       do s=1,NS1(i)
          call csv_write(1,'y('//trim(str(i))//'|'//trim(str(s))//')',.false.)
          call csv_write(1,'py('//trim(str(i))//'|'//trim(str(s))//')',.false.)
          call csv_write(1,'va('//trim(str(i))//'|'//trim(str(s))//')',.false.)
          call csv_write(1,'m('//trim(str(i))//'|'//trim(str(s))//')',.false.)
          call csv_write(1,'l('//trim(str(i))//'|'//trim(str(s))//')',.false.)
          if(i.eq.1) then
             call csv_write(1,'k('//trim(str(i))//'|'//trim(str(s))//')',.false.)
             call csv_write(1,'i('//trim(str(i))//'|'//trim(str(s))//')',.false.)
             call csv_write(1,'lshare('//trim(str(i))//'|'//trim(str(s))//')',.false.)
             call csv_write(1,'bkw('//trim(str(i))//'|'//trim(str(s))//')',.false.)
          endif
          call csv_write(1,'exsy('//trim(str(i))//'|'//trim(str(s))//')',.false.)
          call csv_write(1,'imsy('//trim(str(i))//'|'//trim(str(s))//')',.false.)
          call csv_write(1,'nxsy('//trim(str(i))//'|'//trim(str(s))//')',.false.)
          do r=1,2
             call csv_write(1,'m1('//trim(str(i))//'|'//trim(str(s))//'|'//trim(str(r))//')',.false.)
             do j=1,2
                call csv_write(1,'m2('//trim(str(i))//'|'//trim(str(s))//'|'//trim(str(r))//&
                     '|'//trim(str(j))//')',.false.)
             enddo
          enddo
       enddo
       do s=1,2
          call csv_write(1,'c('//trim(str(i))//'|'//trim(str(s))//')',.false.)
          call csv_write(1,'pc('//trim(str(i))//'|'//trim(str(s))//')',.false.)       
       enddo
       do f=2,NF1(i)
          call csv_write(1,'q('//trim(str(i))//'|'//trim(str(f))//')',.false.)
          call csv_write(1,'pq('//trim(str(i))//'|'//trim(str(f))//')',.false.)
          do r=1,2
             call csv_write(1,'q1('//trim(str(i))//'|'//trim(str(f))//'|'//trim(str(r))//')',.false.)
             do j=1,2
                call csv_write(1,'q2('//trim(str(i))//'|'//trim(str(f))//'|'//trim(str(r))//&
                     '|'//trim(str(j))//')',.false.)
             enddo
          enddo
          if(i.eq.1 .and. f.eq.3) then
             call csv_write(1,'q1('//trim(str(i))//'|'//trim(str(f))//'|'//trim(str(3))//')',.false.)
             call csv_write(1,'q2('//trim(str(i))//'|'//trim(str(f))//'|'//trim(str(3))//&
                  '|'//trim(str(1))//')',.false.)
          endif
       enddo
    enddo

    ! advance a line
    write(1,*)

    ! now write data
    do t=1,NT+1
       call csv_write(1,b_t(t),.false.)
       call csv_write(1,bg_t(t),.false.)
       call csv_write(1,qb_t(t),.false.)
       call csv_write(1,rer_t(t),.false.)
       call csv_write(1,rw_t(t),.false.)
       call csv_write(1,rir_t(t),.false.)
       do i=1,NC
          call csv_write(1,ll_t(i,t),.false.)
          if(i.eq.1) then
             call csv_write(1,g_t(i,t),.false.)
             call csv_write(1,kk_t(i,t),.false.)
             call csv_write(1,ii_t(i,t),.false.)
             call csv_write(1,pi_t(i,t),.false.)
             call csv_write(1,rk_t(i,t),.false.)
             call csv_write(1,iy_t(i,t),.false.)
             call csv_write(1,iw_t(i,t),.false.)
          endif
          call csv_write(1,w_t(i,t),.false.)
          call csv_write(1,ngdp_t(i,t),.false.)
          call csv_write(1,rgdp_t(i,t),.false.)
          call csv_write(1,om_t(i,t),.false.)
          call csv_write(1,tw_t(i,t),.false.)
          call csv_write(1,100.0*ext_t(i,t)/ngdp_t(i,t),.false.)
          call csv_write(1,100.0*imt_t(i,t)/ngdp_t(i,t),.false.)
          call csv_write(1,100.0*nxt_t(i,t)/ngdp_t(i,t),.false.)
          call csv_write(1,100.0*exf_t(i,t)/ngdp_t(i,t),.false.)
          call csv_write(1,100.0*imf_t(i,t)/ngdp_t(i,t),.false.)
          call csv_write(1,100.0*nxf_t(i,t)/ngdp_t(i,t),.false.)
          call csv_write(1,100.0*exm_t(i,t)/ngdp_t(i,t),.false.)
          call csv_write(1,100.0*imm_t(i,t)/ngdp_t(i,t),.false.)
          call csv_write(1,100.0*nxm_t(i,t)/ngdp_t(i,t),.false.)

          do s=1,NS1(i)
             call csv_write(1,y_t(i,s,t),.false.)
             call csv_write(1,py_t(i,s,t),.false.)
             call csv_write(1,va_t(i,s,t),.false.)
             call csv_write(1,m_t(i,s,t),.false.)              
             call csv_write(1,l_t(i,s,t),.false.)
             if(i.eq.1) then
                call csv_write(1,k_t(i,s,t),.false.)
                call csv_write(1,i_t(i,s,t),.false.)
                call csv_write(1,lshare_t(i,s,t),.false.)
                call csv_write(1,bkw_t(i,s,t),.false.)
             endif
             call csv_write(1,100.0*exs_t(i,s,t)/ngdp_t(i,t),.false.)
             call csv_write(1,100.0*ims_t(i,s,t)/ngdp_t(i,t),.false.)
             call csv_write(1,100.0*nxs_t(i,s,t)/ngdp_t(i,t),.false.)
             do r=1,2
                call csv_write(1,m1_t(i,s,r,t),.false.)
                do j=1,2
                   call csv_write(1,m2_t(i,s,r,j,t),.false.)
                enddo
             enddo
          enddo
          do s=1,2
             call csv_write(1,c_t(i,s,t),.false.)
             call csv_write(1,pc_t(i,s,t),.false.)
          enddo
          do f=2,NF1(i)
             call csv_write(1,q_t(i,f,t),.false.)
             call csv_write(1,pq_t(i,f,t),.false.)
             do r=1,2
                call csv_write(1,q1_t(i,f,r,t),.false.)
                do j=1,2
                   call csv_write(1,q2_t(i,f,r,j,t),.false.)
                enddo
             enddo
             if(i.eq.1 .and. f.eq.3) then
                call csv_write(1,q1_t(i,3,3,t),.false.)
                call csv_write(1,q2_t(i,3,3,1,t),.false.)
             endif
          enddo
       enddo
       write(1,*)
    enddo

    close(1)

  end subroutine write_eqm_vars

  ! ///////////////////////////////////////////////////////////////////////
  ! write only variables necessary for figures
  ! ///////////////////////////////////////////////////////////////////////
  subroutine write_vars_for_figs(fname)
    implicit none
    character(len=*), intent(in) :: fname
    integer :: t

    !open file
    open(unit=1,file=trim(OUTPATH)//trim(fname),status='replace',action='write')

    ! first write columns label
    write(1,*) 'year,tb,tbg,tbs,i,lg,lc,rer,r,rw,lpg,lps,lpc,gamg,gams,gamc'

    ! now write data
    do t=1,NT+1
       call csv_write(1,1991+t,.false.)
       call csv_write(1,100.0*nxt_t(1,t)/ngdp_t(1,t),.false.)
       call csv_write(1,100.0*nxs_t(1,1,t)/ngdp_t(1,t),.false.)
       call csv_write(1,100.0*nxs_t(1,2,t)/ngdp_t(1,t),.false.)
       call csv_write(1,iy_t(1,t),.false.)
       call csv_write(1,lshare_t(1,1,t),.false.)
       call csv_write(1,lshare_t(1,3,t),.false.)       
       call csv_write(1,rer_t(t),.false.)
       call csv_write(1,rir_t(t),.false.)
       call csv_write(1,rw_t(t),.false.)
       call csv_write(1,lp_t(1,1,t)/lp_t(1,1,1),.false.)
       call csv_write(1,lp_t(1,2,t)/lp_t(1,2,1),.false.)
       call csv_write(1,lp_t(1,3,t)/lp_t(1,3,1),.false.)
       call csv_write(1,gam_t(1,1,t),.false.)
       call csv_write(1,gam_t(1,2,t),.false.)
       call csv_write(1,gam_t(1,3,t),.false.)
       write(1,*)
    enddo

    close(1)

  end subroutine write_vars_for_figs

  ! ///////////////////////////////////////////////////////////////////////
  ! write only variables necessary for figures
  ! ///////////////////////////////////////////////////////////////////////
  subroutine write_exo_vars(fname)
    implicit none
    character(len=*), intent(in) :: fname
    integer :: t

    !open file
    open(unit=1,file=trim(OUTPATH)//trim(fname),status='replace',action='write')

    ! first write columns label
    write(1,*) 'year,gam_g,gam_s,gam_c,om,cg_pct,bg_pct,pope_us,popw_us,pope_rw,popw_rw,cbar_g,cbar_s,cbar_efrac_g,cbar_efrac_s'

    ! now write data
    do t=1,NT+1
       call csv_write(1,1991+t,.false.)
       call csv_write(1,gam_t(1,1,t),.false.)
       call csv_write(1,gam_t(1,2,t),.false.)
       call csv_write(1,gam_t(1,3,t),.false.)
       call csv_write(1,om_t(2,t),.false.)
       call csv_write(1,cg_pct_ts(scenario+1,t),.false.)
       call csv_write(1,bg_pct_ts(scenario+1,t),.false.)
       call csv_write(1,pope_ts(1,t),.false.)
       call csv_write(1,popw_ts(1,t),.false.)
       call csv_write(1,pope_ts(2,t),.false.)
       call csv_write(1,popw_ts(2,t),.false.)
       call csv_write(1,100*cbar_ts(1,1,t)/c_t(1,1,t),.false.)
       call csv_write(1,100*cbar_ts(1,2,t)/c_t(1,2,t),.false.)
       call csv_write(1,100* pc_t(1,1,t)*cbar_ts(1,1,t)/ (pc_t(1,1,t)*c_t(1,1,t) + pc_t(1,2,t)*c_t(1,2,t)) ,.false.)
       call csv_write(1,100* pc_t(1,2,t)*cbar_ts(1,2,t)/ (pc_t(1,1,t)*c_t(1,1,t) + pc_t(1,2,t)*c_t(1,2,t)) ,.false.)
       write(1,*)
    enddo

    close(1)

  end subroutine write_exo_vars

  ! ///////////////////////////////////////////////////////////////////////
  ! write equilibrium conditions (with labels) to file
  ! ///////////////////////////////////////////////////////////////////////
  subroutine write_eqm_conds(myF, fname)
    implicit none
    real(DP), dimension(:), intent(in) :: myF
    character(len=*), intent(in) :: fname
    integer :: i,s,r,j,f,t,t0,NN,nx

    !open file
    open(unit=1,file=trim(OUTPATH)//trim(fname),status='replace',action='write')

    if(scenario .eq. 0) then ! baseline scenario without saving glut
       t0 = 1
       if(fixed_inv_flag .eq. 0) then
          NN = NEQM0
       else
          NN = NEQM0fi
       endif
    else if(scenario .eq. 1) then ! saving glut
       t0 = TSG0
       if(jcurve_flag .eq. 1 .and. bk2009_flag .eq. 0) then
          NN = NEQM1jc
       else if(bk2009_flag .eq. 1 .and. jcurve_flag .eq. 0) then
          NN = NEQM1bkw
       else if(bk2009_flag .eq. 1 .and. jcurve_flag .eq. 1) then
          NN = NEQM1jcbkw
       elseif(fixed_inv_flag .eq. 1) then
          NN = NEQM1fi
       else
          NN = NEQM1
       endif
    else if(scenario .eq. 2) then ! sudde stop
       t0 = TSS0
       NN = NEQM2
    endif

    nx=0

    do t=t0,NT+1

       ! balance of payments
       call csv_write(1,'BoP('//trim(str(t))//')',.false.)
       call csv_write(1,myF(nx+1),.false.)
       write(1,*)
       nx=nx+1

       do i=1,NC
          do s=1,NS1(i)

             if(i.eq.1 .and. t.lt.NT+1) then
                call csv_write(1,'MPK(1|'//trim(str(s))//'|'//trim(str(t))//')',.false.)
                call csv_write(1,myF(nx+1),.false.)
                write(1,*)
                nx=nx+1
             endif

             call csv_write(1,'MPL('//trim(str(i))//'|'//trim(str(s))//'|'//trim(str(t))//')',.false.)
             call csv_write(1,myF(nx+1),.false.)
             write(1,*)
             nx=nx+1

             do r=1,2
                do j=1,NC
                   call csv_write(1,'MPM('//trim(str(i))//'|'//trim(str(s))//'|'&
                        //trim(str(r))//'|'//trim(str(j))//'|'//trim(str(t))//')',.false.)
                   call csv_write(1,myF(nx+1),.false.)
                   write(1,*)
                   nx=nx+1
                enddo
             enddo

             ! market clearing
             call csv_write(1,'Mktclear('//trim(str(i))//'|'//trim(str(s))//'|'//trim(str(t))//')',.false.)
             call csv_write(1,myF(nx+1),.false.)
             write(1,*)
             nx=nx+1
          enddo

          do s=1,2
             do j=1,NC
                ! mucg/mucs = pg/ps
                call csv_write(1,'MPQC('//trim(str(i))//'|'//trim(str(s))//'|'//trim(str(j))// &
                     '|'//trim(str(t))//')',.false.)
                call csv_write(1,myF(nx+1),.false.)
                write(1,*)
                nx=nx+1
             enddo
          enddo

          ! mucg/mucs = pg/ps
          call csv_write(1,'MUN('//trim(str(i))//'|'//trim(str(t))//')',.false.)
          call csv_write(1,myF(nx+1),.false.)
          write(1,*)
          nx=nx+1

          ! mun/mul = w/pc
          call csv_write(1,'MUN('//trim(str(i))//'|'//trim(str(t))//')',.false.)
          call csv_write(1,myF(nx+1),.false.)
          write(1,*)
          nx=nx+1

          ! euler equation
          if(t.lt.NT+1) then
             call csv_write(1,'Euler('//trim(str(i))//'|'//trim(str(t))//')',.false.)
             call csv_write(1,myF(nx+1),.false.)
             write(1,*)
             nx=nx+1
          endif
       enddo

       ! some US-specific things: foc for investment's use of construction, inv mkt clearing,
       ! price normalization, government consumption requirement

       ! marginal product of final demand = cost (for goods and services) for us gov and inv
       do f=2,NF1(1)
          do r=1,2
             do j=1,2
                call csv_write(1,'MPQ('//trim(str(1))//'|'//trim(str(f))//'|'&
                     //trim(str(r))//'|'//trim(str(j))//'|'//trim(str(t))//')',.false.)
                call csv_write(1,myF(nx+1),.false.)
                write(1,*)
                nx=nx+1
             enddo
          enddo
       enddo

       call csv_write(1,'MPQ(1|3|3|1|'//trim(str(t))//')',.false.)
       call csv_write(1,myF(nx+1),.false.)
       write(1,*)
       nx=nx+1

       call csv_write(1,'Mktclear-inv(1|'//trim(str(t))//')',.false.)
       call csv_write(1,myF(nx+1),.false.)
       write(1,*)
       nx=nx+1

       call csv_write(1,'Price-norm('//trim(str(t))//')',.false.)
       call csv_write(1,myF(nx+1),.false.)
       write(1,*)
       nx=nx+1

       call csv_write(1,'Mktclear-g(1|'//trim(str(t))//')',.false.)
       call csv_write(1,myF(nx+1),.false.)
       write(1,*)
       nx=nx+1

    enddo

    ! last few things...
    ! in baseline no-saving glut model, we match first-period trade balance and investment
    if(scenario .eq. 0) then
       call csv_write(1,'Match-tb(1)',.false.)
       call csv_write(1,myF(nx+1),.false.)
       write(1,*)
       nx=nx+1

       call csv_write(1,'Match-inv(1)',.false.)
       call csv_write(1,myF(nx+1),.false.)
       write(1,*)
       nx=nx+1

       if(fixed_inv_flag.eq.1) then
          do t=2,NT
             call csv_write(1,'Match_inv('//trim(str(t))//')',.false.)
             call csv_write(1,myF(nx+1),.false.)
             nx=nx+1
          enddo
       endif

       ! in saving glut model, we match trade balance for periods TSG0 through TSG1
    else if(scenario .eq. 1) then
       do t=t0,TSG1
          call csv_write(1,'Match-tb('//trim(str(t))//')',.false.)
          call csv_write(1,myF(nx+1),.false.)
          write(1,*)
          nx=nx+1
          if(jcurve_flag.eq.1) then
             call csv_write(1,'Match-rer('//trim(str(t))//')',.false.)
             call csv_write(1,myF(nx+1),.false.)
             write(1,*)
             nx=nx+1
          endif
          if(bk2009_flag .eq. 1) then
             call csv_write(1,'Match-lg('//trim(str(t))//')',.false.)
             call csv_write(1,myF(nx+1),.false.)
             write(1,*)
             nx=nx+1
          endif
       enddo

       if(fixed_inv_flag.eq.1) then
          do t=t0,NT
             call csv_write(1,'Match_inv('//trim(str(t))//')',.false.)
             call csv_write(1,myF(nx+1),.false.)
             nx=nx+1
          enddo
       endif

    endif

    call assert1(nx.eq.NN,'Incorrect number of equilibrium equations! nx = '&
         //trim(str(nx))//', NN = '//trim(str(NN)))

    close(1)
  end subroutine write_eqm_conds


  ! ///////////////////////////////////////////////////////////////////////
  ! set other equilibrium variables based on the main ones in the X vector
  ! ///////////////////////////////////////////////////////////////////////
  subroutine set_vars(t,bgp)

    implicit none

    integer, intent(in) :: t,bgp

    integer :: i, s, f, r, j, rr

    do i=1,NC

       ! initialize ngdp, rgdp, and exports/imports
       ngdp_t(i,t) = ZERO
       rgdp_t(i,t) = ZERO
       ext_t(i,t) = ZERO
       imt_t(i,t) = ZERO
       exf_t(i,t) = ZERO
       imf_t(i,t) = ZERO
       exm_t(i,t) = ZERO
       imm_t(i,t) = ZERO
       exs_t(i,:,t) = ZERO
       ims_t(i,:,t) = ZERO

       ! productivity shocks
       z_t(:,:,t) = ONE
       if(scenario .eq. 2) then
          if(t.eq.TSS0) then
             z_t(1,:,t) = 0.9_dp
          elseif(t.eq.TSS0+1) then
             z_t(1,:,t) = 0.95_dp
          endif
       endif

       ! production functions
       do s=1,NS1(i)
          do r=1,2
             m1_t(i,s,r,t) = prod_m1(i,s,r,m2_t(i,s,r,:,t))
          enddo
          m_t(i,s,t) = prod_m(i,s,m1_t(i,s,:,t))
          if(i.eq.1) then
             va_t(i,s,t) = prod_va_us(i,s,k_t(i,s,t),l_t(i,s,t),z_t(i,s,t)*gam_t(i,s,t))
          else
             va_t(i,s,t) = prod_va_rw(i,s,l_t(i,s,t),z_t(i,s,t)*gam_t(i,s,t))
          endif
          y_t(i,s,t) = prod_go(i,s,va_t(i,s,t),m_t(i,s,t))

          ngdp_t(i,t) = ngdp_t(i,t) + py_t(i,s,t) * y_t(i,s,t)
          rgdp_t(i,t) = rgdp_t(i,t) + y_t(i,s,t)
          rva_t(i,s,t) = py_t(i,s,1) * y_t(i,s,t)
          do r=1,2
             do j=1,2
                ngdp_t(i,t) = ngdp_t(i,t) - py_t(j,r,t) * m2_t(i,s,r,j,t)
                rgdp_t(i,t) = rgdp_t(i,t) - m2_t(i,s,r,j,t)
                rva_t(i,s,t) = rva_t(i,s,t) - py_t(j,r,1)*m2_t(i,s,r,j,t)
             enddo
          enddo
          lp_t(i,s,t) = rva_t(i,s,t)/l_t(i,s,t)

       enddo

       ! consumption
       do s=1,2
          c_t(i,s,t) = prod_q1(i,1,s,q2_t(i,1,s,:,t))
       enddo
       cpi_t(i,t) = (pc_t(i,1,t)*c0(i,1) + pc_t(i,2,t)*c0(i,2))/(c0(i,1)+c0(i,2))

       ! units of bonds
       if(i.eq.1) then
          pb_t(t) = cpi_t(i,t) ! US goods consumption
       endif

       ! final demand for us gov and inv
       if(i.eq.1) then
          do f=2,NF1(1)
             if(i.eq. 1 .and. f.eq.3) then
                rr=3
             else
                rr=2
             endif
             do r=1,rr
                q1_t(i,f,r,t) = prod_q1(i,f,r,q2_t(i,f,r,:,t))
             enddo
             q_t(i,f,t) = prod_q(i,f,q1_t(i,f,:,t))
             do r=1,rr
                do j=1,NC
                enddo
             enddo
          end do
       endif

       ! main aggregates
       ll_t(i,t) = sum(l_t(i,:,t))
       !pc_t(i,t) = pq_t(i,1,t)
       lshare_t(i,:,t) = 100.0_dp * l_t(i,:,t)/ll_t(i,t)
       if(i .eq. 1) then
          g_t(i,t) = q_t(i,2,t)
          kk_t(i,t) = sum(k_t(i,:,t))
          if(t.lt.NT+1) then
             if(t.eq.NT .or. k_adj_cost .eq. 0) then
                k_t(i,:,t+1) = (ONE-delta) * k_t(i,:,t) + i_t(i,:,t)
             else
                do s=1,NS1(1)
                   k_t(i,s,t+1) = phiK(i_t(i,s,t)/k_t(i,s,t)) * k_t(i,s,t) + (ONE-delta)*k_t(i,s,t)
                enddo
             endif
          endif
          if(t .eq. 1) then
             rk_t(i,t) = r0+delta
          else if(t .eq. NT+1) then
             qb_t(t) = pb_t(t)/(ONE+rbgp)
             if(bgp.eq.1) then
                rk_t(i,t) = pq_t(i,3,t)*pb_t(t)/qb_t(t) - (ONE-delta)*pq_t(i,3,t)
             else
                rk_t(i,t) = pq_t(i,3,t-1)*pb_t(t)/qb_t(t-1) - (ONE-delta)*pq_t(i,3,t)
             endif
             i_t(i,:,t) = (gbgp-ONE+delta) * k_t(i,:,t)
          else
             rk_t(i,t) = pq_t(i,3,t-1)*pb_t(t)/qb_t(t-1) - (ONE-delta)*pq_t(i,3,t)
          endif
          ii_t(i,t) = sum(i_t(i,:,t))
          pi_t(i,t) = pq_t(i,3,t)
          iy_t(i,t) = 100.0_dp * pi_t(i,t)*ii_t(i,t)/ngdp_t(i,t)
       endif

       if(i.eq.1) then
          j=2
       else
          j=1
       endif

       ! set exports
       do s=1,2
          ext_t(i,t) = ext_t(i,t) + py_t(i,s,t) * sum(m2_t(j,:,s,i,t))
          exm_t(i,t) = exm_t(i,t) + py_t(i,s,t) * sum(m2_t(j,:,s,i,t))
          exs_t(i,s,t) = py_t(i,s,t) * sum(m2_t(j,:,s,i,t))

          ext_t(i,t) = ext_t(i,t) + py_t(i,s,t) * sum(q2_t(j,:,s,i,t))
          exf_t(i,t) = exf_t(i,t) + py_t(i,s,t) * sum(q2_t(j,:,s,i,t))
          exs_t(i,s,t) = exs_t(i,s,t) + py_t(i,s,t) * sum(q2_t(j,:,s,i,t))
       enddo

    enddo

    ! set imports and net exports
    do i=1,NC
       if(i.eq.1) then
          j=2
       else
          j=1
       endif
       imt_t(i,t) = ext_t(j,t)
       imm_t(i,t) = exm_t(j,t)
       imf_t(i,t) = exf_t(j,t)
       do s=1,2
          ims_t(i,s,t) = exs_t(j,s,t)
       enddo
       nxt_t(i,t) = ext_t(i,t) - imt_t(i,t)
       nxf_t(i,t) = exf_t(i,t) - imf_t(i,t)
       nxm_t(i,t) = exm_t(i,t) - imm_t(i,t)
       do s=1,2
          nxs_t(i,s,t) = exs_t(i,s,t) - ims_t(i,s,t)
       enddo
    enddo

    ! real exchange rate, real wage ratio, real interest rate
    rer_t(t) = cpi_t(2,t)/cpi_t(1,t)
    rw_t(t) = (w_t(2,t)/cpi_t(2,t))/(w_t(1,t)/cpi_t(1,t))
    rir_t(t) = 100.0_dp * (pb_t(t)/qb_t(t) - ONE)

    ! government debt
    if(t.gt.1) then
       if(bgp.eq.0) then
          bg_t(t) = ngdp_t(1,t-1) * (bg_pct_ts(scenario+1,t-1)/100.0_dp)/qb_t(t-1)
       else
          bg_t(t) = (ngdp_t(1,t)/gbgp) * (bg_pct_ts(scenario+1,t-1)/100.0_dp)/qb_t(t)
       endif
    endif

    ! preference shocks
    if(scenario .eq. 0) then
       if(t.lt.NT+1) then
          om_t(:,t+1) = om_speed * om_t(:,t) + (ONE-om_speed) * ONE
       endif
    else if(scenario .eq. 1) then
       if(t.ge.TSG1 .and. t.lt.NT+1) then
          om_t(:,t+1) = om_speed * om_t(:,t) + (ONE-om_speed) * ONE
          if(jcurve_flag.eq.1) then
             tw_t(:,t+1) = om_speed * tw_t(:,t) + (ONE-om_speed) * ONE
          endif
          if(bk2009_flag .eq. 1) then ! assume these are permanent after 2012
             bkw_t(:,:,t+1) = bkw_t(:,:,t)
          endif
       endif
    else if(scenario .eq. 2) then
       om_t(:,t+1) = om_speed_ss * om_t(:,t) + (ONE-om_speed_ss) * ONE
    endif

  end subroutine set_vars

  ! ///////////////////////////////////////////////////////////////////////
  ! balanced growth path equations
  ! ///////////////////////////////////////////////////////////////////////
  subroutine eval_bgp_conds(myX,myF)

    implicit none

    real(DP), dimension(:), intent(inout) :: myX
    real(DP), dimension(:), intent(inout) :: myF

    integer :: nx, i, s, f, t, j, r

    t = NT+1
    call vectorize_bgp_vars(myX,1)
    call set_vars(t,1)
    b_t(NT+1) = bbgp

    nx=0

    ! balance of payments (1)
    myF(1) = bop(t)
    nx=nx+1

    do i=1,NC
       do s=1,NS1(i)

          ! marginal product of capital = rental rate (3)
          if(i.eq.1) then
             myF(nx+1) = mpk_rk_us(i,s,t)
             nx=nx+1
          endif

          ! marginal product of labor = wage (5)
          myF(nx+1) = mpl_w(i,s,t)
          nx=nx+1

          ! marginal product of intermediate = cost (20)
          do r=1,2
             do j=1,NC
                myF(nx+1) = mpm_py(i,s,r,j,t)
                nx=nx+1
             enddo
          enddo

          ! market clearing (5)
          myF(nx+1) = mkt_clear_y(i,s,t)
          nx=nx+1
       enddo

       do s=1,2
          do j=1,NC
             myF(nx+1) = mpc_py(i,s,j,t)
             nx=nx+1
          enddo
       enddo

       ! mucg/mucs = pg/ps (2)
       myF(nx+1) = mucg_mucs(i,t)
       nx=nx+1

       ! mun/mul = w/pc (2)
       myF(nx+1) = muc_mul(i,t)
       nx=nx+1
    enddo

    ! some US-specific things: foc for investment's use of construction, inv mkt clearing,
    ! price normalization, government consumption requirement (3)

    ! marginal product of final demand = cost (for goods and services) (16)
    i=1
    do f=2,3
       do r=1,2
          do j=1,2
             myF(nx+1) = mpq_py(i,f,r,j,t)
             nx=nx+1
          enddo
       enddo
    enddo
    myF(nx+1) = mpq_py(1,3,3,1,t)
    nx=nx+1
    myF(nx+1) = mkt_clear_i_us(1,t)
    nx=nx+1
    myF(nx+1) = price_norm(t)
    nx=nx+1
    myF(nx+1) = mkt_clear_g_us(t)
    nx=nx+1

    call assert1(nx.eq.NBGP,'Incorrect number of BGP equations! nx = '&
         //trim(str(nx))//', NBGP = '//trim(str(NBGP)))

  end subroutine eval_bgp_conds

  ! ///////////////////////////////////////////////////////////////////////
  ! wrapper for eval_bgp_conds that we can send to the solver
  ! ///////////////////////////////////////////////////////////////////////
  function bgp_func(myX)
    implicit none
    real(DP), dimension(:), intent(inout) :: myX
    real(DP), dimension(size(myX)) :: bgp_func
    call eval_bgp_conds(myX,bgp_func)
  end function bgp_func

  ! ///////////////////////////////////////////////////////////////////////
  ! solve for the balanced growth path!
  ! ///////////////////////////////////////////////////////////////////////
  subroutine solve_bgp(b,retstat)

    implicit none
    real(DP), intent(in) :: b
    integer, intent(inout) :: retstat
    real(DP), dimension(NBGP) :: X, F

    scenario = 0
    bbgp = b
    call set_initial_bgp_guess(X)
    if(eval_bgp_once_flag.eq.1) then
       call eval_bgp_conds(X,F)
       call write_array(F,'Fbgp.csv')
       call write_bgp_vars
    else
       !call eval_bgp_conds(X,F)
       !call write_array(F,'Fbgp0.csv')
       call newtons_method(bgp_func,X,5.0e-9_dp,500,NBGP,1,retstat)
       !call eval_bgp_conds(X,F)
       !call write_array(F,'Fbgp1.csv')
       call write_bgp_vars
       
    endif

  end subroutine solve_bgp

  ! ///////////////////////////////////////////////////////////////////////
  ! equilibrium conditions
  ! ///////////////////////////////////////////////////////////////////////
  subroutine eval_eqm_conds(myX,myF)

    implicit none

    real(DP), dimension(:), intent(inout) :: myX
    real(DP), dimension(:), intent(inout) :: myF

    integer :: nx, i, s, f, t, j, r, NN, t0, test

    test=-199

    if(scenario .eq. 0) then ! baseline scenario without saving glut
       t0 = 1
       if(fixed_inv_flag .eq. 0) then
          NN = NEQM0
       else
          NN = NEQM0fi
       endif
    else if(scenario .eq. 1) then ! saving glut
       t0 = TSG0
       if(jcurve_flag .eq. 1 .and. bk2009_flag .eq. 0) then
          NN = NEQM1jc
       elseif(bk2009_flag .eq. 1 .and. jcurve_flag .eq. 0) then
          NN = NEQM1bkw
       else if(bk2009_flag .eq. 1 .and. jcurve_flag .eq. 1) then
          NN = NEQM1jcbkw
       elseif(fixed_inv_flag .eq. 1) then
          NN = NEQM1fi
       else
          NN = NEQM1
       endif
    else if(scenario .eq. 2) then ! sudden stop
       t0 = TSS0
       NN = NEQM2
    endif

    ! first pull model variables out of myX array and set other vars in all time periods
    call vectorize_eqm_vars(myX,1)
    do t=t0,NT+1
       call set_vars(t,0)
    enddo
    bbgp = b_t(NT+1)

    ! now actually go through and evaluate equilibrium conditions
    nx=0

    do t=t0,NT+1

       ! balance of payments
       myF(nx+1) = bop(t)
       nx=nx+1

       do i=1,NC
          do s=1,NS1(i)

             ! marginal product of capital = rental rate
             if(i.eq.1 .and. t.lt.NT+1) then
                myF(nx+1) = mpk_rk_us(i,s,t+1)
                nx=nx+1
             endif

             ! marginal product of labor = wage
             myF(nx+1) = mpl_w(i,s,t)
             nx=nx+1

             ! marginal product of intermediate = cost
             do r=1,2
                do j=1,NC
                   myF(nx+1) = mpm_py(i,s,r,j,t)
                   nx=nx+1
                enddo
             enddo

             ! market clearing
             myF(nx+1) = mkt_clear_y(i,s,t)
             nx=nx+1
          enddo

          do s=1,2
             do j=1,NC
                myF(nx+1) = mpc_py(i,s,j,t)
                nx=nx+1
             enddo
          enddo

          ! mucg/mucs = pg/ps
          myF(nx+1) = mucg_mucs(i,t)
          nx=nx+1

          ! mun/mul = w/pc
          myF(nx+1) = muc_mul(i,t)
          nx=nx+1

          ! euler equation
          if(t.lt.NT+1) then
             myF(nx+1) = euler(i,t)
             nx=nx+1
          endif
       enddo

       ! some US-specific things: foc for investment's use of construction, inv mkt clearing,
       ! price normalization, government consumption requirement
       ! marginal product of final demand = cost (for goods and services)
       i=1
       do f=2,3
          do r=1,2
             do j=1,2
                myF(nx+1) = mpq_py(i,f,r,j,t)
                nx=nx+1
             enddo
          enddo
       enddo

       myF(nx+1) = mpq_py(1,3,3,1,t)
       nx=nx+1

       myF(nx+1) = mkt_clear_i_us(1,t)
       nx=nx+1

       myF(nx+1) = price_norm(t)
       nx=nx+1

       myF(nx+1) = mkt_clear_g_us(t)
       nx=nx+1
    enddo

    ! last few things...
    ! in baseline no-saving glut model, we match first-period trade balance and investment
    if(scenario .eq. 0) then
       myF(nx+1) = match_tbdata(1)
       nx=nx+1

       myF(nx+1) = match_inv(1)
       nx=nx+1

       if(fixed_inv_flag .eq. 1) then
          do t=2,NT
             myF(nx+1) = match_inv(t)
             nx=nx+1
          enddo
       endif

    ! in saving glut model, we match trade balance for periods TSG0 through TSG1
    else if(scenario .eq. 1) then
       do t=t0,TSG1
          myF(nx+1) = match_tbdata(t)
          nx=nx+1
          if(jcurve_flag .eq. 1) then
             myF(nx+1) = match_rerdata(t)
             nx=nx+1
          endif
          if(bk2009_flag .eq. 1) then
             myF(nx+1) = match_lg(t)
             nx=nx+1
          endif
       enddo

       if(fixed_inv_flag .eq. 1) then
          do t=t0,NT
             myF(nx+1) = match_inv(t)
             nx=nx+1
          enddo
       endif
    endif

    call assert1(nx.eq.NN,'Incorrect number of equilibrium equations! nx = '&
         //trim(str(nx))//', NN = '//trim(str(NN)))

  end subroutine eval_eqm_conds

  ! ///////////////////////////////////////////////////////////////////////
  ! wrapper for eqm_conds that we can send to the solver 
  ! ///////////////////////////////////////////////////////////////////////
  function eqm_func(myX)
    implicit none
    real(DP), dimension(:), intent(inout) :: myX
    real(DP), dimension(size(myX)) :: eqm_func
    call eval_eqm_conds(myX,eqm_func)
  end function eqm_func

  ! ///////////////////////////////////////////////////////////////////////
  ! solve for equilibrium
  ! ///////////////////////////////////////////////////////////////////////
  subroutine solve_eqm(scenariox,retstat)

    implicit none
    integer, intent(in) :: scenariox
    integer, intent(inout) :: retstat

    integer :: NN
    real(DP), dimension(:), allocatable :: X, F

    scenario = scenariox

    ! allocate memory: has to be dynamic since number of variables depends on scenario
    if(scenario .eq. 0) then ! baseline no-saving glut scenario
       if(fixed_inv_flag .eq. 0) then
          NN = NEQM0
       else
          NN = NEQM0fi
       endif
       b_t(1) = b0
       bg_t(1) = bg0
       k_t(1,:,1) = k0(1,:)
    else if(scenario .eq. 1) then ! saving glut scenario
       if(jcurve_flag .eq. 1 .and. bk2009_flag .eq. 0) then
          NN = NEQM1jc
       elseif(bk2009_flag .eq. 1 .and. jcurve_flag .eq. 0) then
          NN = NEQM1bkw
       else if(bk2009_flag .eq. 1 .and. jcurve_flag .eq. 1) then
          NN = NEQM1jcbkw
       elseif(fixed_inv_flag .eq. 1) then
          NN = NEQM1fi
       else
          NN = NEQM1
       endif
    else if(scenario .eq. 2) then ! sudden stop
       NN = NEQM2
    endif
    allocate(X(NN))
    allocate(F(NN))

    ! if we are in baseline scenario, either read initial guess from file or create one 
    ! from scratch depending on user input
    if(scenario .eq. 0) then
       if(bk2009_step3_flag .eq. 0 .and. jc_step3_flag .eq. 0) then
          if(read_seed0_flag .eq. 1) then
             if(nonhomo_prefs_flag .eq. 0) then
                call read_seed(X,'seed0.csv')
             elseif(noio_flag .eq. 1 .and. noio_leontief_flag .eq. 1) then
                call read_seed(X,'seed0_nh_noiol.csv')
             elseif(altio1_flag .eq. 1) then
                call read_seed(X,'seed0_nh_altio1.csv')
             elseif(saving_drought_flag .eq. 1) then
                call read_seed(X,'seed0_sd.csv')
             else
                call read_seed(X,'seed0_nh.csv')
             endif
          else
             call set_initial_eqm_guess(X,retstat)
          endif
       else
          call vectorize_eqm_vars(X,0)
       endif
       ! if we are in the saving glut scenario, read seed file if user requests it
       ! we allow control over the two seeds separately in case user needs to read
       ! seed for baseline model but not for saving glut model
    else if(scenario.eq.1) then
       if(read_seed1_flag .eq. 1) then
          call read_seed(X,'seed1'//trim(suffix)//'.csv')
       else
          call vectorize_eqm_vars(X,0)
       endif
    else if(scenario .eq. 2) then
       call vectorize_eqm_vars(X,0)
    endif

    ! now solve the model!
    if(eval_eqm_once_flag.eq.1) then
       call eval_eqm_conds(X,F)
       call write_array(F,'Feqm.csv')
       call write_eqm_vars('vars'//trim(str(scenario))//'.csv')
    else
       call newtons_method(eqm_func,X,5.0e-9_dp,50,NN,1,retstat)
       call eval_eqm_conds(X,F)
    endif

    ! write new seed file if user requested it (only if we succeeded in solving for the equilibrium)
    if(retstat .eq. 0) then
       call write_vars_for_figs('vars_figs'//trim(str(scenario))//trim(suffix)//'.csv')
       if(scenario .eq. 1) then
          call write_exo_vars('vars_exo'//trim(str(scenario))//trim(suffix)//'.csv')
       endif
       !call write_eqm_vars('vars_all'//trim(str(scenario))//trim(suffix)//'.csv')
       !call write_exo_vars('vars_exo'//trim(str(scenario))//trim(suffix)//'.csv')
       !call calc_shares
       !call write_shares('shares'//trim(str(scenario))//trim(suffix)//'.csv')
       !if (scenario .eq. 0) then
       !   call write_bgp_vars
       !   if(welfare_flag .eq. 1) then
       !      cg_aux_ts(:) = q_t(1,2,:)
       !      welfare_gsaved_flag = 1
       !   endif
       !end if
       if(scenario .eq. 0 .and. write_seed0_flag .eq. 1) then
          call write_seed(X,'seed0'//trim(suffix)//'.csv')
       else if(scenario .eq. 1 .and. write_seed1_flag .eq. 1) then
          call write_seed(X,'seed1'//trim(suffix)//'.csv')
       endif
    else
       call write_eqm_conds(F,'Feqm_err.csv')
       call write_eqm_vars('vars_err.csv')
    endif

    ! free memory
    deallocate(X)
    deallocate(F)

  end subroutine solve_eqm

  subroutine calc_shares()

    implicit none
    integer :: t

    do t=1,NT+1
       us_goods_fshare_t(t) = &
            100.0_dp * (&
            (py_t(1,1,t)*sum(q2_t(1,:,1,1,t))+py_t(2,1,t)*sum(q2_t(1,:,1,2,t)))/sum(pq_t(1,:,t)*q_t(1,:,t)))

       us_home_fshare_t(t) = &
            100.0_dp * (&
            (py_t(1,1,t)*sum(q2_t(1,:,1,1,t))+py_t(1,2,t)*sum(q2_t(1,:,2,1,t)))/sum(pq_t(1,:,t)*q_t(1,:,t)))

       us_home_fgshare_t(t) = &
            100.0_dp * (&
            (py_t(1,1,t)*sum(q2_t(1,:,1,1,t)))/(sum(py_t(1,1,t)*q2_t(1,:,1,1,t))+sum(py_t(2,1,t)*q2_t(1,:,1,2,t))))
            
       us_home_fsshare_t(t) = &
            100.0_dp * (&
            (py_t(1,2,t)*sum(q2_t(1,:,2,1,t)))/(sum(py_t(1,2,t)*q2_t(1,:,2,1,t))+sum(py_t(2,2,t)*q2_t(1,:,2,2,t))))

       us_goods_cshare_t(t) = &
            100.0_dp * (&
            pc_t(1,1,t)*c_t(1,1,t)/(pc_t(1,1,t)*c_t(1,1,t)+pc_t(1,2,t)*c_t(1,2,t)))

       us_home_cshare_t(t) = &
            100.0_dp * (&
            (py_t(1,1,t)*sum(q2_t(1,1:2,1,1,t))+py_t(1,2,t)*sum(q2_t(1,1:2,2,1,t)))/sum(pc_t(1,1:2,t)*c_t(1,1:2,t)))

       us_home_cgshare_t(t) = &
            100.0_dp * (&
            (py_t(1,1,t)*sum(q2_t(1,1:2,1,1,t)))/(sum(py_t(1,1,t)*q2_t(1,1:2,1,1,t))+sum(py_t(2,1,t)*q2_t(1,1:2,1,2,t))))

       us_home_csshare_t(t) = &
            100.0_dp * (&
            (py_t(1,2,t)*sum(q2_t(1,1:2,2,1,t)))/(sum(py_t(1,2,t)*q2_t(1,1:2,2,1,t))+sum(py_t(2,2,t)*q2_t(1,1:2,2,2,t))))

       us_goods_ishare_t(t) = &
            100.0_dp * (&
            (py_t(1,1,t)*(q2_t(1,3,1,1,t))+py_t(2,1,t)*(q2_t(1,3,1,2,t)))/(pq_t(1,3,t)*q_t(1,3,t)))

       us_home_ishare_t(t) = &
            100.0_dp * (&
            (py_t(1,1,t)*(q2_t(1,3,1,1,t))+py_t(1,2,t)*(q2_t(1,3,2,1,t)))/&
            ((pq_t(1,3,t)*q_t(1,3,t))-py_t(1,3,t)*q2_t(1,3,3,1,t)))

       us_home_igshare_t(t) = &
            100.0_dp * (&
            (py_t(1,1,t)*(q2_t(1,3,1,1,t)))/((py_t(1,1,t)*q2_t(1,3,1,1,t))+(py_t(2,1,t)*q2_t(1,3,1,2,t))))

       us_home_isshare_t(t) = &
            100.0_dp * (&
            (py_t(1,2,t)*(q2_t(1,3,2,1,t)))/((py_t(1,2,t)*q2_t(1,3,2,1,t))+(py_t(2,2,t)*q2_t(1,3,2,2,t))))

       us_m_yshare_t(t) = &
            100.0_dp * (&
            (py_t(1,1,t)*sum(m2_t(1,:,1,1,t)) + &
            py_t(1,2,t)*sum(m2_t(1,:,2,1,t)) + &
            py_t(1,3,t)*sum(m2_t(1,:,3,1,t)) + &
            py_t(2,1,t)*sum(m2_t(1,:,1,2,t)) + &
            py_t(2,2,t)*sum(m2_t(1,:,2,2,t))) / &
            sum(py_t(1,:,t)*y_t(1,:,t)))

       us_goods_mshare_t(t) = &
            100.0_dp * (&
            (py_t(1,1,t)*sum(m2_t(1,:,1,1,t)) + py_t(2,1,t)*sum(m2_t(1,:,1,2,t))) / &
            (py_t(1,1,t)*sum(m2_t(1,:,1,1,t)) + &
            py_t(1,2,t)*sum(m2_t(1,:,2,1,t)) + &
            py_t(1,3,t)*sum(m2_t(1,:,3,1,t)) + &
            py_t(2,1,t)*sum(m2_t(1,:,1,2,t)) + &
            py_t(2,2,t)*sum(m2_t(1,:,2,2,t))))

       us_home_mshare_t(t) = &
            100.0_dp * (&
            (py_t(1,1,t)*sum(m2_t(1,:,1,1,t)) + py_t(1,2,t)*sum(m2_t(1,:,2,1,t)) + py_t(1,3,t)*sum(m2_t(1,:,3,1,t))) / &
            (py_t(1,1,t)*sum(m2_t(1,:,1,1,t)) + &
            py_t(1,2,t)*sum(m2_t(1,:,2,1,t)) + &
            py_t(1,3,t)*sum(m2_t(1,:,3,1,t)) + &
            py_t(2,1,t)*sum(m2_t(1,:,1,2,t)) + &
            py_t(2,2,t)*sum(m2_t(1,:,2,2,t))))

       us_home_mgshare_t(t) = &
            100.0_dp * (&
            (py_t(1,1,t)*sum(m2_t(1,:,1,1,t))) / &
            (py_t(1,1,t)*sum(m2_t(1,:,1,1,t)) + &
            py_t(2,1,t)*sum(m2_t(1,:,1,2,t))))

       us_home_msshare_t(t) = &
            100.0_dp * (&
            (py_t(1,2,t)*sum(m2_t(1,:,2,1,t))) / &
            (py_t(1,2,t)*sum(m2_t(1,:,2,1,t)) + &
            py_t(2,2,t)*sum(m2_t(1,:,2,2,t))))

       rw_goods_yshare_t(t) = &
            100.0_dp * ( &
            (py_t(2,1,t)*y_t(2,1,t))/(sum(py_t(2,:,t)*y_t(2,:,t))))

       rw_share_worldy_t(t) = &
            100.0_dp * ( &
            (sum(py_t(2,:,t)*y_t(2,:,t))/(sum(py_t(:,:,t)*y_t(:,:,t)))))

       rw_yg_share_worldyg_t(t) = &
            100.0_dp * ( &
            (py_t(2,1,t)*y_t(2,1,t))/(sum(py_t(:,1,t)*y_t(:,1,t))))

       rw_ys_share_worldys_t(t) = &
            100.0_dp * ( &
            (py_t(2,2,t)*y_t(2,2,t))/(sum(py_t(:,2,t)*y_t(:,2,t))))

    enddo

  end subroutine calc_shares

  subroutine write_shares(fname)

    implicit none

    character(len=*), intent(in) :: fname
    integer :: t
    
    !open file
    open(unit=1,file=trim(OUTPATH)//trim(fname),status='replace',action='write')

    ! first write columns label
    call csv_write(1,'year',.false.)
    call csv_write(1,'us ky',.false.)
    call csv_write(1,'us goods fshare',.false.)
    call csv_write(1,'us home fshare',.false.)
    call csv_write(1,'us home fgshare',.false.)
    call csv_write(1,'us home fsshare',.false.)
    call csv_write(1,'us goods cshare',.false.)
    call csv_write(1,'us home cshare',.false.)
    call csv_write(1,'us home cgshare',.false.)
    call csv_write(1,'us home csshare',.false.)
    call csv_write(1,'us goods ishare',.false.)
    call csv_write(1,'us home ishare',.false.)
    call csv_write(1,'us home igshare',.false.)
    call csv_write(1,'us home isshare',.false.)
    call csv_write(1,'us m yshare',.false.)
    call csv_write(1,'us goods mshare',.false.)
    call csv_write(1,'us home mshare',.false.)
    call csv_write(1,'us home mgshare',.false.)
    call csv_write(1,'us home msshare',.false.)
    call csv_write(1,'rw goods yshare',.false.)
    call csv_write(1,'rw share worldy',.false.)
    call csv_write(1,'rw yg share worldyg',.false.)
    call csv_write(1,'rw ys share worldys',.false.)
    write(1,*)

    ! now write data
    do t=1,NT+1
       call csv_write(1,1991+t,.false.)
       call csv_write(1,kk_t(1,t)/rgdp_t(1,t),.false.)
       call csv_write(1,us_goods_fshare_t(t),.false.)
       call csv_write(1,us_home_fshare_t(t),.false.)
       call csv_write(1,us_home_fgshare_t(t),.false.)
       call csv_write(1,us_home_fsshare_t(t),.false.)
       call csv_write(1,us_goods_cshare_t(t),.false.)
       call csv_write(1,us_home_cshare_t(t),.false.)
       call csv_write(1,us_home_cgshare_t(t),.false.)
       call csv_write(1,us_home_csshare_t(t),.false.)
       call csv_write(1,us_goods_ishare_t(t),.false.)
       call csv_write(1,us_home_ishare_t(t),.false.)
       call csv_write(1,us_home_igshare_t(t),.false.)
       call csv_write(1,us_home_isshare_t(t),.false.)
       call csv_write(1,us_m_yshare_t(t),.false.)
       call csv_write(1,us_goods_mshare_t(t),.false.)
       call csv_write(1,us_home_mshare_t(t),.false.)
       call csv_write(1,us_home_mgshare_t(t),.false.)
       call csv_write(1,us_home_msshare_t(t),.false.)
       call csv_write(1,rw_goods_yshare_t(t),.false.)
       call csv_write(1,rw_share_worldy_t(t),.false.)
       call csv_write(1,rw_yg_share_worldyg_t(t),.false.)
       call csv_write(1,rw_ys_share_worldys_t(t),.false.)
       write(1,*)
    enddo

    close(1)
    
  end subroutine write_shares

  subroutine calc_real_income(flag)
    implicit none
    integer, intent(in) :: flag

    write(*,*) flag
    ! need to come back and redo this
    
  end subroutine calc_real_income

  ! ///////////////////////////////////////////////////////////////////////
  ! write production and retail parameters to text files
  ! ///////////////////////////////////////////////////////////////////////
  subroutine write_params

    implicit none

    integer :: i,s,r,f

    open(unit=1, file=trim(OUTPATH)//'params.txt', form='formatted', &
         action='write', status='replace')

    write(1,*) '///////////////////////////////////////////////////////////////////////////////////////'
    write(1,*) 'CALIBRATED PARAMETERS FOR "GLOBAL IMBALANCES AND STRUCTURAL CHANGE IN THE UNITED STATES'
    write(1,*) '///////////////////////////////////////////////////////////////////////////////////////'
    write(1,*)
    
    write(1,*) '---------------------------------------------------------------------------------------'
    write(1,*) 'PRODUCTION PARAMETERS:'
    write(1,*) '---------------------------------------------------------------------------------------'

    do i=1,NC
       write(1,*)
       write(1,*) 'Country '//trim(str(i))
       do s=1,NS1(i)
          write(1,*)
          write(1,*) 'Sector '//trim(str(s))
          
          write(1,*) 'A: '//trim(strd(A(i,s)))
          write(1,*) 'lambda: '//trim(strd(lam(i,s)))
          write(1,*) '1/(1-eta): '//trim(strd(1/(1-eta)))
          write(1,*) 'B: '//trim(strd(B(i,s)))
          write(1,*) 'alpha: '//trim(strd(alpha(s)))
          write(1,*) 'C: '//trim(strd(C(i,s)))
          write(1,*) 'pi: '//trim(strd(pi(i,s,1)))//', '&
               //trim(strd(pi(i,s,2)))//', '//trim(strd(pi(i,s,3)))
          write(1,*) '1/(1-xi): '//trim(strd(1/(1-xi)))
          write(1,*) 'D: '//trim(strd(D(i,s,1)))//', '&
               //trim(strd(D(i,s,2)))//', '//trim(strd(D(i,s,3)))
          do r=1,NS
             write(1,*) trim('mu(')//trim(str(i))//trim(',')//&
                  trim(str(s))//trim(',')//&
                  trim(str(r))//',:): '//&
                  trim(strd(mu(i,s,r,1)))//', '//trim(strd(mu(i,s,r,2)))
             write(1,*) '1/(1-zeta): '//trim(strd(1/(1-zeta(r))))
          enddo
       enddo
    enddo

    write(1,*)
    write(1,*) '---------------------------------------------------------------------------------------'
    write(1,*) 'FINAL DEMAND PARAMETERS FOR US GOV AND INV:'
    write(1,*) '---------------------------------------------------------------------------------------'

    write(1,*)
    i=1
    do f=2,3
       write(1,*)
       write(1,*) 'Final use category '//trim(str(f))

       write(1,*) 'G: '//trim(strd(G(i,f)))
       write(1,*) 'eps: '//trim(strd(eps(i,f,1)))//', '&
            //trim(strd(eps(i,f,2)))//', '//trim(strd(eps(i,f,3)))
       write(1,*) 'H: '//trim(strd(H(i,f,1)))//', '&
            //trim(strd(H(i,f,2)))//', '//trim(strd(H(i,f,3)))
       write(1,*) '1/(1-rho)'//trim(strd(1/(1-rho(f))))
       do r=1,NS
          write(1,*) trim('theta(')//trim(str(i))//trim(',')//&
               trim(str(f))//trim(',')//&
               trim(str(r))//',:): '//&
               trim(strd(theta(i,f,r,1)))//', '//trim(strd(theta(i,f,r,2)))
          write(1,*) '1/(1-sig): '//trim(strd(1/(1-sig(r))))
       enddo
    enddo

    write(1,*)
    write(1,*) '---------------------------------------------------------------------------------------'
    write(1,*) 'HOUSEHOLD AND CAPITAL FORMATION PARAMETERS'
    write(1,*) '---------------------------------------------------------------------------------------'
    write(1,*)
    write(1,*) 'delta: '//trim(strd(delta))
    write(1,*) 'tauk: '//trim(strd(tauk))
    write(1,*) 'iw0: '//trim(strd(iw_t(1,2)))
    write(1,*) 'rbgp: '//trim(strd(rbgp))
    write(1,*) 'beta: '//trim(strd(beta(1)))//', '//trim(strd(beta(2)))
    write(1,*) 'psi: '//trim(strd(psi))
    write(1,*) 'phi: '//trim(strd(phi(1)))//', '//trim(strd(phi(2)))
    write(1,*) 'eps(1,:): '//trim(strd(eps(1,1,1)))//', '//trim(strd(eps(1,1,2)))
    write(1,*) 'eps(2,:): '//trim(strd(eps(2,1,1)))//', '//trim(strd(eps(2,1,2)))
    write(1,*) 'H(1,:): '//trim(strd(H(1,1,1)))//', '&
         //trim(strd(H(1,1,2)))
    write(1,*) 'H(2,:): '//trim(strd(H(2,1,1)))//', '&
         //trim(strd(H(2,1,2)))
    write(1,*) 'cbar us: '//trim(strd(cbar0(1,1)))//', '//trim(strd(cbar0(1,2)))
    write(1,*) 'cbar us: '//trim(strd(cbar0(2,1)))//', '//trim(strd(cbar0(2,2)))

    close(1)

  end subroutine write_params

end module equilibrium
