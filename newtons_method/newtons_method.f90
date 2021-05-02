program newtonsMethod
implicit none

!perform newton's method to find the root of a given function
real :: initialGuess = 0, root, temp
integer :: maxIter = 50, i = 1
real, parameter :: tolerance = 0.00001, exactSoln = 4*atan(1.0)/6.0 !known ans to calculate error

temp = initialGuess !root for prior iteration

do i = 1, maxIter
        root = initialGuess - func(temp) / df(temp) !calculate new root
        write(*,*) "Iteration: ", i, " Value: ", root, " Error: ", root - exactSoln

        if (abs(root - temp) < tolerance) then !convergence check
                write(*,*) "Converged"
                exit
        end if

       temp = root
end do

end program


real function func(x) !calculates value of desired function at x
implicit none

real, intent(in) :: x
real, parameter :: pi = 4*atan(1.0)

func = tan(x-(pi/6.0)) 
end function


real function df(x) !calculates value of given derivative at x
implicit none

real, intent(in) :: x
real, parameter :: pi = 4*atan(1.0)

df = (1.0 / cos(x-(pi/6.0)))**2 !sec^2
end function
