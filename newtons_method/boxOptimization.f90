program boxOptimization
use newtonsMethod !requires newtons_method_modular.f90
use iso_fortran_env
implicit none

!Problem: A piece of cardboard 'n' cm x 'm' cm is to be made into a
!box with no lid. A square with side length 'x' is removed from each
!corner to form the box. Determine the maximum volume of the box.
!Approach: Use newton's method to solve the optimization problem

integer, parameter :: sp = real32, dp = real64
real(dp) :: n = 100, m = 20
real(dp) :: x
real(dp) :: guess = 5
integer :: iter, maxIter = 10
real(dp) :: tolerance = 1E-12

call findRoot(dV, d2V, guess, iter, x, maxIter, tolerance) !critical point occurs where dV = 0
if (x > 0 .and. x < 2*m .and. x < 2*n) then
        write(*,*) "Number of Iterations: ", iter
        write(*,*) "Critical point: ", x
        write(*,*) "Max volume: ", volume(x)
else 
        write(*,*) "Method Failed. Try a different guess." !todo: make more robust (min vs max)
end if

contains

        real(dp) function volume(x)
                real(dp), intent(in) :: x
                volume = (n - 2*x) * (m - 2*x) * x
        end function

        real(dp) function dV(x)
                real(dp), intent(in) :: x
                dV = 4*x**3 - (n+m) * 2*x**2 + n*m * x 
        end function

        real(dp) function d2V(x)
                real(dp), intent(in) :: x
                d2V = 12*x**2 - 4*(n+m) * x + (n*m)
        end function

end program
