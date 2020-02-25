program main

  use globals
  use calibrate
  use equilibrium

  implicit none

  integer :: i, nargs, retstat
  character(len=32) :: arg

  retstat=0
  
  ! ///////////////////////////////////////////////////////////////////////
  ! process user arguments
  ! ///////////////////////////////////////////////////////////////////////
  nargs = iargc()
  do i=1,nargs
     call getarg(i,arg)
     select case(arg)
     case('-ss')
        ss_flag = 1
     case('-nodemo')
        no_demo_flag = 1
        suffix = trim(suffix) // '_nodemo'
     case('-fixg')
        fixed_govt_flag = 1
        suffix  = trim(suffix) // '_fixg'
     case('-fixg2')
        fixed_govt2_flag = 1
        suffix  = trim(suffix) // '_fixg2'
     case('-asym')
        asym_growth_flag = 1
     case('-welfare')
        welfare_flag = 1
     case('-jc')
        jcurve_flag = 1
        suffix  = trim(suffix) // '_jc'
     case('-noio')
        noio_flag = 1
        suffix  = trim(suffix) // '_noio'
     case('-noio2')
        noio_flag = 1
        noio2_flag = 1
        suffix  = trim(suffix) // '_noio2'
     case('-noiol')
        noio_flag = 1
        noio_leontief_flag = 1
        suffix  = trim(suffix) // '_noiol'
     case('-nh')
        nonhomo_prefs_flag = 1
        suffix  = trim(suffix) // '_nh'
     case('-mst')
        more_svcs_trd_flag = 1
        suffix  = trim(suffix) // '_mst'
     case('-altio1')
        altio1_flag = 1
        suffix  = trim(suffix) // '_altio1'
     case('-fixl')
        fixed_labor_flag = 1
        suffix  = trim(suffix) // '_fixl'
     case('-fixi')
        fixed_inv_flag = 1
        suffix  = trim(suffix) // '_fixi'
     case('-sd')
        saving_drought_flag = 1
        k_adj_cost = 1
        iom=1
        suffix  = trim(suffix) // '_sd'
     case('-bkw')
        bk2009_flag = 1
        suffix  = trim(suffix) // '_bkw'
     case('-cobb1')
        cobb1_flag = 1
        suffix  = trim(suffix) // '_cobb1'
     case('-cobb2')
        cobb2_flag = 1
        suffix  = trim(suffix) // '_cobb2'
     case('-ack')
        k_adj_cost = 1
        !suffix  = trim(suffix) // '_ack'
     case('-rs0')
        read_seed0_flag = 1
     case('-rs1')
        read_seed1_flag = 1
     case('-ws0')
        write_seed0_flag = 1
     case('-ws1')
        write_seed1_flag = 1
     case('-homo')
        homotopy_flag =1
     case default
        if(arg .ne. '-help' .or. (jcurve_flag .eq. 1 .and. fixed_inv_flag .eq. 1) .or. &
             (fixed_inv_flag .eq. 1 .and. ss_flag .eq. 1) .or. &
             (noio_flag .eq. 1 .and. more_svcs_trd_flag .eq. 1) .or. &
             (more_svcs_trd_flag .eq. 1 .and. altio1_flag .eq. 1) .or. &
             (noio_flag .eq. 1 .and. altio1_flag .eq. 1) .or. &
             (fixed_inv_flag .eq. 1 .and. saving_drought_flag .eq. 1) .or. &
             (bk2009_flag .eq. 1 .and. (ss_flag .eq. 1 .or. fixed_inv_flag .eq. 1))) then
           write(*,*) 'INCORRECT COMMAND LINE ARGUMENT: ', arg
        endif
        write(*,*) 'VALID ARGUMENTS:'
        write(*,*) '-asym: asymmetric productivity growth across sectors'
        write(*,*) '-ss: sudden stop in 2015'
        write(*,*) '-jc: trade wedges to match RER (j-curve)'
        write(*,*) '-noio: no intermediate inputs, match GDP shares'
        write(*,*) '-noio2: no intermediate inputs, match GO shares'
        write(*,*) '-noiol: no-IO with Leontief preferences'
        write(*,*) '-nh: non-homothetic preferences'
        write(*,*) '-mst: IO matrix with more services trade'
        write(*,*) '-altio1: IO matrix with same GO/GDP ratio in all US sectors'
        write(*,*) '-nodemo: no demographic change'
        write(*,*) '-fixg: government consumption and debt fixed at 1992 fractions of GDP'
        write(*,*) '-fixg2: households anticipate changes in govt spendind and debt' 
        write(*,*) '-fixl: fixed labor supply'
        write(*,*) '-fixi: fixed investment rate'
        write(*,*) '-sd: domestic saving drought'
        write(*,*) '-bkw: Buera-Kaboski (2009) wedges to match structural change'
        write(*,*) '-cobb1: Cobb-Douglas for value added and intermediates'
        write(*,*) '-cobb2: Cobb-Douglas for intermediate goods and services'
        write(*,*) '-ack: concave capital formation'
        write(*,*) '-welfare: calculate real income indices (gov cons quantity fixed across scenarios)'
        write(*,*) '-rs0: read guess for baseline no-saving glut model'
        write(*,*) '-rs1: read guess for saving glut model'
        write(*,*) '-ws0: write guess for baseline no-saving glut model'
        write(*,*) '-ws1: write guess for saving glut model'
        write(*,*) '-homo: homotopy to get initial guess'
        write(*,*) '-help: repeat these options'
        write(*,*) 'user cannot select -fixi and -jc!'
        write(*,*) 'user cannot select -fixi and -ss!'
        write(*,*) 'user cannot select -fixi and -sd!'
        write(*,*) 'user can select only one of {-noio, -mst, -altio1, altio2}!'
        write(*,*) 'user cannot select -bkw and any of {-sd, -fixi, -ss}!'
        if(arg .ne. '-help') then
           write(*,*) 'ERROR! HALTING EXECUTION...'
        endif
        write(*,*)
        stop 'program terminated due to incorrect user input'
     end select
  enddo

  ! describe scenario
  write(*,*)
  write(*,*) '------------------------------------------------------------------------------------------'
  write(*,*) 'GLOBAL IMBALANCES AND STRUCTURAL CHANGE IN THE UNITED STATES (KEHOE, RUHL, AND STEINBERG)'
  write(*,*) 'February 2015 revision for Journal of Political Economy'
  write(*,*)
  write(*,*) 'Model scenario:'
  write(*,*) 'Base period is 1992'
  write(*,*) 'Unanticipated saving glut (or drought) begins in '//trim(str(1992+TSG0-1))
  if(saving_drought_flag .eq. 0) then
     write(*,*) 'Global saving glut: Rest-of-the-world discount factor shocks calibrated to match trade balance through '&
          //trim(str(1992+TSG1-1))
  else
     write(*,*) 'Domestic saving drought: U.S. discount factor shocks calibrated to match trade balance through '&
          //trim(str(1992+TSG1-1))
  endif
  if(asym_growth_flag .eq. 1) then
     write(*,*) 'Asymmetric productivity growth across sectors'
  else
     write(*,*) 'Productivity grows at same rate in all sectors'
  endif
  if(ss_flag .eq. 1) then
     write(*,*) 'Sudden stop in 2015'
  endif
  if(jcurve_flag .eq. 1) then
     write(*,*) 'Trade wedges to match RER during 1993-2012 (J-curve)'
  else
     write(*,*) 'No trade wedges, no J-curve'
  endif
  if(more_svcs_trd_flag .eq. 1) then
     write(*,*) 'IO matrix with more services trade'
  endif
  if(altio1_flag .eq. 1) then
     write(*,*) 'IO matrix with same GO/GDP in all US sectors'
  endif
  if(noio_flag .eq. 1 .and. noio2_flag .eq. 0) then
     write(*,*) 'No intermediate inputs, matching VA shares'
  endif
  if(noio_flag .eq. 1 .and. noio2_flag .eq. 1) then
     write(*,*) 'No intermediate inputs, matching GO shares'
  endif
  if(noio_leontief_flag .eq. 1) then
     write(*,*) 'Leontief consumption aggregator for no-IO model'
  endif
  if(no_demo_flag .eq. 1) then
     write(*,*) 'No demographic change'
  endif  
  if(fixed_govt_flag .eq. 1) then
     write(*,*) 'Government consumption and debt fixed at 1992 fractions of GDP'
  endif
  if(fixed_govt2_flag .eq. 1) then
     write(*,*) 'Households anticipate changes in govt spending and debt'
  endif
  if(welfare_flag .eq. 1) then
     write(*,*)  'Calculating real income indices (requires fixing gov cons quantities across scenarios)'
  endif
  if(fixed_labor_flag .eq. 1) then
     write(*,*) 'Labor supply fixed at 1992 level'
  endif
  if(fixed_inv_flag .eq. 1) then
     write(*,*) 'Investment rate fixed at 1992 level'
  endif
  if(bk2009_flag .eq. 1) then
     write(*,*) 'Buera-Kaboski (2009) wedges to match structural change data'
  endif
  if(cobb1_flag .eq. 1) then
     write(*,*) 'Cobb-Douglas in value added and intermediate'
  endif
  if(cobb2_flag .eq. 1) then
     write(*,*) 'Cobb-Douglas in intermediate goods and services'
  endif
  if(k_adj_cost .eq. 1) then
     write(*,*) 'Concave capital formation (Lucas and Prescott, 1971)'
  endif
  if(read_seed0_flag .eq. 1) then
     write(*,*) 'Reading initial guess for baseline no-saving glut scenario from file'
  endif
  if(read_seed1_flag .eq. 1) then
     write(*,*) 'Reading initial guess for saving glut scenario from file'
  endif
  if(write_seed0_flag .eq. 1) then
     write(*,*) 'Writing initial guess for baseline no-saving glut scenario to file'
  endif
  if(write_seed1_flag .eq. 1) then
     write(*,*) 'Writing initial guess for saving glut scenario to file'
  endif
  if(homotopy_flag .eq. 1) then
     write(*,*) 'Using homotopy on parameter values to find suitable initial guess'
  endif
  if(NT.gt.100) then
     long_flag = 1
     suffix = trim(suffix)//'_long'
     write(*,*) 'Longer time to BGP'
  endif
  write(*,*)

  ! ///////////////////////////////////////////////////////////////////////
  ! Step 1: Calibrate
  ! //////////////////////////////////////////////////////////////////////

  write(*,*) '------------------------------------------------------------------------------------------'
  write(*,*) 'STEP 0: CALIBRATING MODEL...'
  call load_iomat
  call load_ts_params
  call calibrate_all 
  call write_params
  call init_vars
  write(*,*)

  write(*,*) '------------------------------------------------------------------------------------------'
  write(*,*) 'STEP 1: BASELINE NO-SAVING GLUT SCENARIO...'
  write(*,*)
  eval_eqm_once_flag = 0
  call solve_eqm(0,retstat)
  call assert1(retstat .eq. 0,'Failed to solve for no-saving glut equilibrium!')
  call write_params
  if(welfare_flag .eq. 1) then
     call calc_real_income(1)
  endif
  write(*,*)

  if(homotopy_flag .eq. 0) then
     write(*,*) '------------------------------------------------------------------------------------------'
     write(*,*) 'STEP 2: SAVING GLUT SCENARIO...'
     write(*,*)
     call solve_eqm(1,retstat)
     call assert1(retstat .eq. 0,'Failed to solve for saving glut equilibrium!')
     if(welfare_flag .eq. 1) then
        call calc_real_income(0)
     endif
     write(*,*)

     if(bk2009_flag .eq. 1) then
        write(*,*) '------------------------------------------------------------------------------------------'
        write(*,*) 'STEP 3: REDOING NO-SAVING GLUT COUNTERFACTUAL WITH CALIBRATED BUERA-KABOSKI (2009) WEDGES...'
        write(*,*)
        bk2009_step3_flag = 1
        call solve_eqm(0,retstat)
        call assert1(retstat .eq. 0,'Failed to solve for no-saving glut equilibrium equilibrium!')
        write(*,*)

     elseif(jcurve_flag .eq. 1) then
        write(*,*) '------------------------------------------------------------------------------------------'
        write(*,*) 'STEP 3: REDOING NO-SAVING GLUT COUNTERFACTUAL WITH CALIBRATED TRADE WEDGES...'
        write(*,*)
        jc_step3_flag = 1
        call solve_eqm(0,retstat)
        call assert1(retstat .eq. 0,'Failed to solve for no-saving glut equilibrium equilibrium!')
        write(*,*)

     elseif(ss_flag .eq. 1) then
        write(*,*) '------------------------------------------------------------------------------------------'
        write(*,*) 'STEP 3: SUDDEN STOP SCENARIO...'
        write(*,*)
        call solve_eqm(2,retstat)
        call assert1(retstat .eq. 0,'Failed to solve for sudden stop equilibrium!')
        if(welfare_flag .eq. 1) then
           call calc_real_income(0)
        endif
        write(*,*)
     end if
  endif

  write(*,*) '------------------------------------------------------------------------------------------'
  write(*,*) 'Program complete!'
  write(*,*)
  
end program main
