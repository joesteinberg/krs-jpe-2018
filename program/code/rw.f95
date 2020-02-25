module rw

  use globals
  use csv_file

  implicit none

contains

  ! --------------------------------------------------------------------
  subroutine write_array(array, fname)

    real(DP), intent(in), dimension(:) :: array
    character(len=*), intent(in) :: fname

    integer :: i

    open(unit=1, file=trim(PATH)//'output/'//fname, form='formatted', status='replace',action='write')
    do i=1,size(array)
       call csv_write(1,array(i),.true.)
    enddo
    close(unit=1)

  end subroutine write_array

  ! --------------------------------------------------------------------
  subroutine write_matrix(matrix, fname)

    real(DP), intent(in), dimension(:,:) :: matrix
    character(len=*), intent(in) :: fname

    integer :: i,j

    open(unit=2, file=trim(PATH)//'output/'//fname, form='formatted', status='replace',action='write')
    do i=1,size(matrix,1)
       do j=1,size(matrix,2)-1
          call csv_write(2,matrix(i,j),.false.)
       enddo
       call csv_write(2,matrix(i,size(matrix,2)),.true.)
    enddo
    close(unit=2)

  end subroutine write_matrix

  ! --------------------------------------------------------------------
  subroutine write_array_aux(array, unit)

    real(DP), intent(in), dimension(:) :: array
    integer, intent(in) :: unit

    integer :: i

    do i=1,size(array)-1
       call csv_write(unit,array(i),.false.)
    enddo
    call csv_write(unit,array(size(array)),.true.)

  end subroutine write_array_aux

  ! --------------------------------------------------------------------
  subroutine write_matrix_aux(matrix, unit)

    real(DP), intent(in), dimension(:,:) :: matrix
    integer, intent(in) :: unit

    integer :: i,j

    do i=1,size(matrix,1)
       do j=1,size(matrix,2)-1
          call csv_write(unit,matrix(i,j),.false.)
       enddo
       call csv_write(unit,matrix(i,size(matrix,2)),.true.)
    enddo

  end subroutine write_matrix_aux

  ! --------------------------------------------------------------------
  subroutine write_params()

    open(unit=3, file=trim(PATH)//'output/params.csv', status='replace', action='write')

    call csv_write(3,'beta',.true.)
    call write_array_aux(beta,3)

    call csv_write(3,'delta',.false.)
    call csv_write(3,delta,.true.)

    call csv_write(3,'rho',.false.)
    call csv_write(3,rho,.true.)

    call csv_write(3,'psi',.false.)
    call csv_write(3,psi,.true.)

    call csv_write(3,'eps',.true.)
    call write_array_aux(eps,3)

    call csv_write(3,'eta',.true.)
    call write_array_aux(eta,3)

    call csv_write(3,'alpha',.true.)
    call write_array_aux(alpha,3)

    call csv_write(3,'M',.true.)
    call write_matrix_aux(M,3)

    call csv_write(3,'mu',.true.)
    call write_matrix_aux(mu,3)

    call csv_write(3,'zeta',.true.)
    call write_matrix_aux(zeta,3)

    call csv_write(3,'DR',.true.)
    call write_matrix_aux(DR,3)

    call csv_write(3,'theta',.true.)
    call write_array_aux(theta,3)

    call csv_write(3,'tauk',.false.)
    call csv_write(3,tauk,.true.)

    call csv_write(3,'tauk1',.false.)
    call csv_write(3,tauk1,.true.)

    close(unit=3)

  end subroutine write_params

  
  subroutine write_all_vars(fname)

    ! write out csv file with all of main variables to make debugging easier

    character(len=*), intent(in) :: fname

    integer :: t, is

    open(unit=5,file=trim(PATH)//'output/'//fname, status='replace', action='write')

    ! .............................................
    ! prices
    do is=1,NS
       call csv_write(5,'p(1,'//trim(str(is))//')',.false.)       
    enddo
    do is=1,NS
       call csv_write(5,'pd(1,'//trim(str(is))//')',.false.)       
    enddo
    call csv_write(5,'pi',.false.)
    call csv_write(5,'w(1)',.false.)
    call csv_write(5,'qb',.false.)
    do is=1,NS
       call csv_write(5,'p(2,'//trim(str(is))//')',.false.)       
    enddo
    do is=1,NS
       call csv_write(5,'pd(2,'//trim(str(is))//')',.false.)       
    enddo
    call csv_write(5,'w(2)',.false.)
    call csv_write(5,'om',.false.)

    ! ................................................
    ! output
    do is=1,NS
       call csv_write(5,'y(1,'//trim(str(is))//')',.false.)       
    enddo
    do is=1,NS
       call csv_write(5,'yd(1,'//trim(str(is))//')',.false.)       
    enddo
    do is=1,NS
       call csv_write(5,'y(2,'//trim(str(is))//')',.false.)       
    enddo
    do is=1,NS
       call csv_write(5,'yd(2,'//trim(str(is))//')',.false.)       
    enddo
    call csv_write(5,'gdp(1)',.false.)   
    call csv_write(5,'tby(1)',.false.)       

    ! ................................................
    ! factors
    do is=1,NS
       call csv_write(5,'xk('//trim(str(is))//')',.false.)       
    enddo
    do is=1,NS
       call csv_write(5,'xl(1,'//trim(str(is))//')',.false.)       
    enddo
    do is=1,NS
       call csv_write(5,'xmpk('//trim(str(is))//')',.false.)       
    enddo
    do is=1,NS
       call csv_write(5,'xmpl('//trim(str(is))//')',.false.)       
    enddo
    do is=1,NS
       call csv_write(5,'xl(2,'//trim(str(is))//')',.false.)       
    enddo

    ! ..............................................
    ! intermediates
    do is=1,NS
       call csv_write(5,'zi('//trim(str(is))//')',.false.)
    enddo
    
    ! ..............................................
    ! final use
    do is=1,NS
       call csv_write(5,'c(1'//trim(str(is))//')',.false.)       
    enddo
    do is=1,NS
       call csv_write(5,'xi('//trim(str(is))//')',.false.)       
    enddo
    do is=1,NS
       call csv_write(5,'im(1'//trim(str(is))//')',.false.)       
    enddo
    do is=1,NS
       call csv_write(5,'ex(1'//trim(str(is))//')',.false.)
    enddo

    call csv_write(5,'b',.false.)
    do is=1,NS
       if(is.eq.NS) then
          call csv_write(5,'c(2'//trim(str(is))//')',.true.)       
       else
          call csv_write(5,'c(2'//trim(str(is))//')',.false.)       
       endif
    enddo

    do t=1,NT+1
       ! .............................................
       ! prices
       do is=1,NS
          call csv_write(5,p_ts(1,is,t),.false.)       
       enddo
       do is=1,NS
          call csv_write(5,pd_ts(1,is,t),.false.)       
       enddo
       call csv_write(5,pi_ts(t),.false.)
       call csv_write(5,w_ts(1,t),.false.)
       call csv_write(5,qb_ts(1,t),.false.)
       do is=1,NS
          call csv_write(5,p_ts(2,is,t),.false.)       
       enddo
       do is=1,NS
          call csv_write(5,pd_ts(2,is,t),.false.)       
       enddo
       call csv_write(5,w_ts(2,t),.false.)
       call csv_write(5,om_ts(t),.false.)

       ! ................................................
       ! output
       do is=1,NS
          call csv_write(5,y_ts(1,is,t),.false.)       
       enddo
       do is=1,NS
          call csv_write(5,yd_ts(1,is,t),.false.)       
       enddo
       do is=1,NS
          call csv_write(5,y_ts(2,is,t),.false.)       
       enddo
       do is=1,NS
          call csv_write(5,yd_ts(2,is,t),.false.)       
       enddo
       call csv_write(5,gdp_ts(1,t),.false.)   
       call csv_write(5,tby_ts(1,t),.false.)       

       ! ................................................
       ! factors
       do is=1,NS
          call csv_write(5,xk_ts(is,t),.false.)       
       enddo
       do is=1,NS
          call csv_write(5,xl_ts(1,is,t),.false.)       
       enddo
       do is=1,NS
          call csv_write(5,xmpk_ts(is,t),.false.)       
       enddo
       do is=1,NS
          call csv_write(5,xmpl_ts(is,t),.false.)       
       enddo
       do is=1,NS
          call csv_write(5,xl_ts(2,is,t),.false.)       
       enddo

       ! ..............................................
       ! intermediates
       do is=1,NS
          call csv_write(5,zi_ts(is,t),.false.)
       enddo

       ! ..............................................
       ! final use
       do is=1,NS
          call csv_write(5,c_ts(1,is,t),.false.)       
       enddo
       do is=1,NS
          call csv_write(5,xi_ts(is,t),.false.)       
       enddo
       do is=1,NS
          call csv_write(5,im_ts(1,is,t),.false.)       
       enddo       
       do is=1,NS
          call csv_write(5,ex_ts(1,is,t),.false.)       
       enddo       
       call csv_write(5,b_ts(1,t),.false.)
       do is=1,NS
          if(is.eq.NS) then
             call csv_write(5,c_ts(2,is,t),.true.)       
          else
             call csv_write(5,c_ts(2,is,t),.false.)       
          endif
       enddo

    enddo

    close(unit=5)

    return

  end subroutine write_all_vars


end module rw
