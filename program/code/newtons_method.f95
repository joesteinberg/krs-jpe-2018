subroutine newtons_method(func, X, tol, maxit, nvars, verbose, retstat)

    ! THIS FUNCTION USES A SLIGHTLY MODIFIED VERSION OF NEWTON'S METHOD TO FIND THE ROOTS OF A VECTOR-VALUED 
    ! FUNCTION OF SEVERAL VARIABLES. I ATTEMPT TO IMPROVE GLOBAL CONVERGENCE BY REQUIRING THAT ALL STEPS
    ! REDUCE THE NORM OF THE TARGET FUNCTION. THIS HELPS PREVENT THE ROOT ESTIMATE FROM EXPLODING WHEN
    ! WE ARE FAR AWAY FROM THE ACTUAL ROOT.
    
    ! NOTE THAT THE DIMENSION OF THE FUNCTION VALUE MUST BE THE SAME AS THE DIMENSION OF THE VECTOR OF VARIABLES.
    ! MAKES USE OF EXTERNAL SUBROUTINES THAT CONTAIN FUNCTIONS THAT CALCULATE THE INVERSE OF A SQUARE MATRIX
    ! AND THE JACOBIAN MATRIX OF A VECTOR-VALUED FUNCTION.
    
    use globals, only: DP, my_isnan, force_small_step, str ! DOUBLE PRECISION TYPE AND NAN-CHECK UTILITY
    !use rw, only : write_array, write_matrix

    implicit none

    integer, intent(in) :: nvars, maxit, verbose ! DIMENSION OF VECTOR OF VARIABLES, FLAG FOR VERBOSE OUTPUT
    real(DP), intent(in) :: tol ! PRECISION OF SOLUTION
    real(DP), intent(inout), dimension(nvars) :: X ! INITIAL GUESS/SOLUTION OUTPUT
    integer, intent(inout) :: retstat

    !THIS INTERFACE PROVIDES ACCESS TO THE PARAMATER "func" WHICH IS THE VECTOR-VALUED FUNCTION OF WHICH WE WANT TO FIND THE ROOTS.
    !NOTE THAT IT IS VERY IMPORTANT TO ONLY USE FUNCTIONS WHOSE VALUES HAVE THE SAME DIMENSION AS THEIR INPUTS.
    interface
        function func(Y)
            use globals
            real(DP), intent(in), dimension(:) :: Y
            real(DP), dimension(size(X)) :: func
        end function func
    end interface

    integer :: iter=0, invstat=0!, lwork=0
    character(len=1) :: small_step
    character(len=60) :: fmt_str
    real(DP), parameter :: step=1.0e-9_DP
    real(DP) :: resnorm1, resnorm2=0.0_DP, t, r
    real(DP), dimension(nvars) :: F, Y, FF, dX
    !real(DP), dimension(nvars*nvars) :: work
    real(DP), dimension(nvars,nvars) :: JJ
    integer, dimension(nvars) :: ipiv

    write(*,*) 'Attempting Newtons method on system of '//trim(str(nvars))//' equations...'
    fmt_str = '(A4,3x,A10,3x,A10,3x,A10,3x,A10,3x,A10)'
    write(*,fmt_str) 'iter','norm1(F)','norm2(F)','max(F)','maxloc(F)','small step'
    fmt_str = '(I4,3x,ES10.3E3,3x,ES10.3E3,3x,ES10.3E3,3x,I10,3x,A10)'
    !fmt_str = '(I10, G10.4, G10.4, G10.4, A16)'

    !lwork=nvars*nvars
    retstat = 0
    iter=0
    do
        iter = iter + 1

        ! calculate function at current root estimate
        F = func(X)

        ! calculate norm of residual
        resnorm1 = sqrt(dot_product(F,F)/nvars)
        
        ! if we've hit the maximum number of iterations then it's time to quit
        if(iter == maxit) then
            write(*,*) 'Newtons method failed to converge given iteration limit.'
            write(*,*)
            retstat=1
            exit
        ! otherwise if something funny has happened, quit as well
        else if(my_isnan(resnorm1) .or. my_isnan(resnorm2) .or. maxval(F)<minval(F)) then
            write(*,*) 'Something has gone awry. Check for NaNs.'
            write(*,*) resnorm1, resnorm2, maxval(F), minval(F)
            write(*,*)
            retstat=1
            exit
        ! if we've converged, quit
        else if(maxval(F)<tol .and. minval(F)>-1.0_DP*tol) then
        !else if(resnorm1<tol) then
            write(*,*) 'Root found! norm(F) = ', resnorm1, ', max(F) = ', maxval(F), ', min(F) = ', minval(F)
            write(*,*)
            exit
        ! otherwise update the root estimate    
        else   
            ! first get the jacobian     
            call numerical_jacobian(func,X,step,nvars,nvars,JJ)
      
            ! invert it         
            !call inverse(nvars,JJ,invstat)
            !call DGETRF( nvars, nvars, JJ, nvars, ipiv, invstat)
            !call DGETRI(nvars, JJ, nvars, ipiv, work, lwork, invstat)
            dX(:) = F(:)
            
            if(force_small_step .eq. 0) then
               t = 1.0_DP
            else
               t = 0.1_DP
            endif

            call DGESV( nvars, 1, JJ, nvars, ipiv, dX, nvars, invstat )
            dX(:) = -t * dX(:)
            
            if(invstat .ne. 0) then
                write(*,*) 'Jacobian matrix is singular! U(i,i) = 0 for i = ', invstat
                write(*,*)
                retstat=1
                exit
            end if
           
            ! calculate the proposed newton step
            !if(iter .le. 3) then
            !    t = 1.0/resnorm1
            !endif
            !dX = -t*matmul(JJ,F)
            small_step = 'N' 

            ! make sure new root estimate reduces norm of function residual
            do
                ! calculate norm at new root estimate
                Y = X + dX
                FF = func(Y)
                resnorm2 = sqrt(dot_product(FF,FF)/nvars)               

                ! if NaNs crop up, first try moving 10% of the way to dX and exit
                if(my_isnan(resnorm2)) then
                   dX = 0.1*dX
                   Y = X+dX
                   FF = func(Y)
                   resnorm2 = sqrt(dot_product(FF,FF)/nvars)
                   if(my_isnan(resnorm2)) exit
                endif

                ! if we're okay, keep going
                if((my_isnan(resnorm2) .neqv. .true.) .and. (resnorm2 - resnorm1 < 0.0_DP)) exit

                ! otherwise reduce step size
                small_step = 'Y' 
                r = resnorm2/resnorm1
                t = (sqrt(1.0_DP + 6.0_DP*r)-1.0_DP)/(3.0_DP*r)

                dX = t*dX            
            end do

            X = Y
          
            ! if requested supply iteration information
            if(verbose .ne. 0) then
                !write(*,*) 'Iteration = ', iter, ', norm1(F) = ', resnorm1, 'norm2(F) = ', &
                !resnorm2, ', tol = ', tol, ' small step? (Y/N) = ', small_step
                write(*,fmt_str) iter,resnorm1,resnorm2,maxval(abs(F)),maxloc(abs(F)),small_step
            end if
        end if
    enddo

end subroutine newtons_method
