program LUSolve
use, intrinsic :: iso_fortran_env
use :: LUDecomp
implicit none

integer, parameter :: sp = real32
real, dimension(3,3) :: array = reshape((/1,1,1,1,4,3,2,4,5/), (/3,3/)

call gaussElim(array, 3, 3)

write(*,*) array

end program
