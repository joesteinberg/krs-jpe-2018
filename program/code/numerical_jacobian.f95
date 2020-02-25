subroutine numerical_jacobian(func, X, step, nx, nf, JJ)

  ! let's make this parallelizable!

  use globals, only: DP

  implicit none

  integer, intent(in) :: nx, nf
  real(DP), intent(in) :: step
  real(DP), intent(in), dimension(nx)  :: X
  real(DP), intent(inout), dimension(nf,nx) :: JJ

  interface
     function func(Y)
       use globals
       real(DP), intent(in), dimension(:) :: Y
       real(DP), dimension(size(X)) :: func
     end function func
  end interface

  integer :: i, j
  real(DP), dimension(nx) :: Xh, f, fh
  real(DP) :: relstep

  f(:)=func(X)
  JJ(:,:) = 0.0_DP

  do j=1, nx
     Xh(:)=X
     !relstep = (X(j)+1.0_DP)*step
     relstep = step
     Xh(j)=X(j) + relstep
     fh=func(Xh)
     do i=1, nf
        JJ(i,j)=(fh(i)-f(i))/relstep
     enddo
  enddo

end subroutine numerical_jacobian
