module newtonsMethod
implicit none

!perform newton's method to find the root of a given function
integer :: maxIter = 50
real, parameter :: tolerance = 1E-12

contains

subroutine findRoot(func, df, guess, iterations, root)
implicit none

double precision, intent(in) :: guess
integer, intent(out) :: iterations
double precision, external :: func, df
double precision, intent(out) :: root
double precision :: f, deriv, temp
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
