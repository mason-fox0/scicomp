module functions
!provides function list for use with newton's method subroutine
use, intrinsic :: iso_fortran_env
implicit none

integer, parameter, private :: dp = real64

contains

real(dp) function f1(x)
implicit none
real(dp), intent(in) :: x

f1 = cosh(x) - 5
end function

real(dp) function df1(x)
implicit none
real(dp), intent(in) :: x

df1 = sinh(x)
end function

real(dp) function f2(x)
implicit none
real(dp), intent(in) :: x

f2 = erf(x) !error function. NOTE: Root is zero. Newton's method should fail
end function

real(dp) function df2(x)
implicit none
real(dp), intent(in) :: x
real(dp), parameter :: pi = 4*datan(1.0_dp)

df2 = 2*exp(-x**2)/sqrt(pi)
end function

real(dp) function f3(x)
implicit none
real(dp), intent(in) :: x

f3 = bessel_jn(0, x) !bessel func of first kind, order zero
end function

real(dp) function df3(x)
implicit none
real(dp), intent(in) :: x

df3 = -bessel_jn(1, x) ! d(J_0(x))/dx = -J_1(x)
end function

end module
