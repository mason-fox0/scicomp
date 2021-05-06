module functions
!provides function list for use with newtons_method
use, intrinsic :: iso_fortran_env
implicit none

integer, parameter :: dp = real64
real(dp) :: x

contains

real(dp) function f1(x)
implicit none
f1 = cosh(x) - 5
end function

real(dp) function df1(x)
implicit none
df1 = sinh(x)
end function

real(dp) function f2(x)
implicit none
f2 = erf(x) !error function
end function

real(dp) function df2(x)
implicit none
real(dp), parameter :: pi = 4*datan(1.0_dp)
write(*,*) "Pi Precision Check: ", pi

df2 = 2*exp(-x**2)/sqrt(pi)
end function

real(dp) function f3(x)
implicit none
f3 = bessel_jn(0, x) !bessel func of first kind, order zero
end function

real(dp) function df3(x)
implicit none
df3 = -bessel_jn(1, x) ! d(J_0(x))/dx = -J_1(x)
end function

end module
