program newtonsMethod
implicit none

!perform newton's method to find the root of a given function
double precision :: initialGuess = 0, root, temp
integer :: maxIter = 50, i
real, parameter :: tolerance = 1E-12
double precision, parameter :: exactSoln = 4*datan(1.0d0)/6.0 !known ans to calculate error

temp = initialGuess !root for prior iteration

do i = 1, maxIter
        root = temp - func(temp) / df(temp) !calculate new root
        write(*,*) "Iteration: ", i, " Value: ", root, " Error: ", root - exactSoln

        if (abs(root - temp) < tolerance) then !convergence check
                write(*,*) "Converged"
                exit
        end if

       temp = root
end do

contains

        double precision function func(x) !calculates value of desired function at x
        implicit none

        double precision, intent(in) :: x
        double precision, parameter :: pi = 4*datan(1.0d0)

        func = dtan(x-(pi/6.0)) 
        end function


        double precision function df(x) !calculates value of given derivative at x
        implicit none

        double precision, intent(in) :: x
        double precision, parameter :: pi = 4*datan(1.0d0)

        df = (1.0 / dcos(x-(pi/6.0)))**2 !sec^2
        end function
end program
