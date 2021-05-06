program newtonTest
use functions
use newtonsMethod
use, intrinsic :: iso_fortran_env

integer, parameter :: dp = real64
integer :: numFunctions, iter, maxIter = 25
real(dp) :: initial = 1.0, tolerance = 1E-12
real(dp) :: x_root
real(dp), dimension(3) :: actual
real :: error

!hardcoded based on wolfram alpha results, used for error calc
actual(1) = acosh(5.0)
actual(2) = 0.0
actual(3) = 2.4048255576957727686216319_dp

call findRoot(f1, df1, initial, iter, x_root, maxIter, tolerance)
error = (actual(1) - x_root) / actual(1)
write(*,*) "After ", iter, "iterations, the root of func1 is: ", x_root
write(*,*) "Actual: ", actual(1), " Error: ", error
write(*,*)

call findRoot(f2, df2, initial, iter, x_root, maxIter, tolerance) !expected to fail, root = 0
error = (actual(2) - x_root) / actual(2) 
write(*,*) "After ", iter, "iterations, the root of func2 is: ", x_root
write(*,*) "Actual: ", actual(2),  " Error: ", error
write(*,*)

call findRoot(f3, df3, initial, iter, x_root, maxIter, tolerance)
error = (actual(3) - x_root) / actual(3)
write(*,*) "After ", iter, "iterations, the root of func3 is: ", x_root
write(*,*) "Actual: ", actual(3), " Error: ", error
write(*,*)

end program
