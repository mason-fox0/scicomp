module newtonsMethod
use, intrinsic :: iso_fortran_env
implicit none
!perform newton's method to find the root of a given function

integer, parameter, private :: dp = real64

contains

subroutine findRoot(func, df, guess, iterations, root, maxIter, tolerance)
implicit none

real(dp), intent(in) :: guess
integer, intent(out) :: iterations
integer, intent(in) :: maxIter
real(dp), external :: func, df
real(dp), intent(out) :: root
real(dp), intent(in) :: tolerance

real(dp) :: f, deriv, temp
integer :: i

temp = guess !root for prior iteration

do i = 1, maxIter
        f = func(temp)
        deriv = df(temp)
        root = temp - f / deriv !calculate new root

        if (abs(root - temp) < tolerance) then !convergence check
                exit
        end if

       temp = root
end do

if (i > maxIter) then
        write(*,*) "***Not Converged***"
end if

iterations = i-1

end subroutine
end module
