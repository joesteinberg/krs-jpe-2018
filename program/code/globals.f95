module globals

  use csv_file
  implicit none
 
  ! ///////////////////////////////////////////////////////////////////////
  ! hardcoded parameters (needed to allocate memory for arrays)
  ! ///////////////////////////////////////////////////////////////////////

  integer, parameter :: DP = kind(1.0d0) ! system-specific DP setting
  real(DP), parameter :: ONE = 1.0_dp ! macro for unit double
  real(DP), parameter :: ZERO = 0.0_dp ! macro for zero
  real(DP), parameter :: TINY = 1.0e-13_dp ! macro for approximate zero
  real(DP), parameter :: TINYSQ = 1.0e-6_dp ! macro for tolerance in some cases
  integer, parameter :: NC = 2 ! number of countries
  integer, parameter :: NS = 3 ! number of production sectors
  integer, parameter :: NSI = 2 ! number of intermediate input sectors
  integer, parameter, dimension(2) :: NS1 = (/3,2/)  
  integer, parameter :: NF = 3 ! number of final use sectors
  integer, parameter, dimension(2) :: NF1 = (/3,1/)
  integer, parameter :: NBRANCHES = 1 ! number of branches in uncertainty tree (not used)
  integer, parameter :: TSG0 = 2 ! period in which saving glut begins
  integer, parameter :: TSG1 = 21 ! period in which saving glut ends
  integer, parameter :: TSS0 = 24 ! period of sudden stop start
  integer, parameter :: TSS1 = 25 ! period of sudden stop end
 
  character(len=512), parameter :: INPATH='/home/joe/Research/saving_glut_RR/program_stone_geary/input/'
  character(len=512), parameter :: OUTPATH='/home/joe/Research/saving_glut_RR/program_stone_geary/output/'
  
  ! NOTE: these are hardcoded for memory-allocation purposes... comment out first 3 lines and uncomment
  ! second 3 lines to run longer-horizon sensitivity analysis... MUST RECOMPILE AFTERWARDS AND START FROM SCRATCH
  ! IN FINDING SUITABLE INITIAL GUESS!
  integer, parameter :: NT = 100 ! number of periods before balanced growth
  integer, parameter :: TPC0 = 50 ! period where time series parameters begin to converge to BGP-consistent values
  integer, parameter :: TPC1 = 75 ! period where time series parameters reach BGP-consistent values
  !integer, parameter :: NT = 120 ! number of periods before balanced growth
  !integer, parameter :: TPC0 = 60 ! period where time series parameters begin to converge to BGP-consistent values
  !integer, parameter :: TPC1 = 90 ! period where time series parameters reach BGP-consistent values


  ! ///////////////////////////////////////////////////////////////////////
  ! non-hardcoded computational parameters
  ! ///////////////////////////////////////////////////////////////////////

  ! pseudo-hardcoded flags
  integer :: iom = 2
  integer :: bk2009_step3_flag = 0
  integer :: jc_step3_flag = 0
  integer :: eval_bgp_once_flag = 0
  integer :: eval_eqm_once_flag = 0
  integer :: force_small_step = 0
  integer :: welfare_gsaved_flag = 0

  ! user-specified flags
  integer :: cobb1_flag = 0
  integer :: cobb2_flag = 0
  integer :: welfare_flag = 0
  integer :: bk2009_flag = 0
  integer :: saving_drought_flag = 0
  integer :: k_adj_cost = 0
  integer :: no_demo_flag = 0
  integer :: fixed_govt_flag = 0
  integer :: fixed_govt2_flag = 0
  integer :: homotopy_flag = 0
  integer :: ss_flag = 0
  integer :: jcurve_flag = 0
  integer :: fixed_labor_flag = 0
  integer :: fixed_inv_flag = 0
  integer :: asym_growth_flag = 0
  integer :: lab_adj_cost_flag = 0
  integer :: cap_adj_cost_flag = 0
  integer :: biased_growth_flag = 0
  integer :: read_seed0_flag = 0
  integer :: read_seed1_flag = 0
  integer :: write_seed0_flag = 0
  integer :: write_seed1_flag = 0
  integer :: noio_flag = 0
  integer :: noio2_flag = 0
  integer :: noio_leontief_flag = 0
  integer :: more_svcs_trd_flag = 0
  integer :: altio1_flag = 0
  integer :: nonhomo_prefs_flag = 0
  integer :: nonhomo_prefs_approach = 0
  integer :: long_flag = 0

  character(len=24) :: suffix = ''

  ! others
  character(len=512) :: input_path, output_path

  ! initial investment (for reshuffling; US-only)
  real(DP), dimension(NS) :: xi0_rs

  ! ///////////////////////////////////////////////////////////////////////
  ! some utilities
  ! ///////////////////////////////////////////////////////////////////////
  contains

    ! boolean function returns NaN status
    function my_isnan(x)
      real(dp), intent(in) :: x
      logical :: my_isnan
      my_isnan = (x .ne. x)
    end function my_isnan

    character(len=20) function str(k)
      !   "Convert an integer to string."
      integer, intent(in) :: k
      write (str, *) k
      str = adjustl(str)
    end function str

    character(len=64) function strd(d)
      !   "Convert an integer to string."
      real(DP), intent(in) :: d
      write (strd, *) d
      strd = adjustl(strd)
    end function strd

    subroutine assert1(n1,string)
      character(len=*), intent(in) :: string
      logical, intent(in) :: n1
      if (.not. n1) then
         write(*,*) 'error: an assertaion failed with this tag: ', string
         stop 'program terminated by assert1'
      endif
    end subroutine assert1

    subroutine write_array(arr,fname)
      implicit none
      real(DP), dimension(:), intent(in) :: arr
      character(len=*), intent(in) :: fname
      integer :: i

      open(unit=1, file=trim(OUTPATH)//trim(fname), form='formatted', & 
           action='write', status='replace')
      do i=1,size(arr)
         call csv_write(1,arr(i),.true.)
      enddo
      close(1)
    end subroutine write_array

    subroutine linspace(arr,a,b,n)
      implicit none
      integer, intent(in) :: n
      real(DP), intent(in) :: a,b
      real(DP), dimension(n), intent(inout) :: arr
      real(DP) :: d
      integer :: i
      d = (b-a)/(dble(n-1))
      arr(1) = a
      do i=2,n
         arr(i) = arr(i-1)+d
         if(isnan(arr(i))) then
            write(*,*) a,b,d,arr(i)
            stop 'NaN detected in linspace! Check your inputs!'
         endif
      enddo
    end subroutine linspace

    subroutine logspace(arr,a,b,n)
      implicit none
      integer, intent(in) :: n
      real(DP), intent(in) :: a,b
      real(DP), dimension(n), intent(inout) :: arr
      call linspace(arr,log(a),log(b),n)
      arr(:) = exp(arr(:))
    end subroutine logspace

    real(DP) function stdev(X)
      implicit none
      real(DP), dimension(:), intent(in) :: X
      integer :: N
      real(DP) :: mean

      N = size(X)
      mean = sum(X(1:N)) / N 
      stdev = sqrt(sum((X(1:N)-mean)**2)/N) 
    end function stdev
  
end module globals
