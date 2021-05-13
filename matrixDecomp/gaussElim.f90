module gaussElimination
!performs gaussian elimination with pivoting
use, intrinsic :: iso_fortran_env
sp = real_32

contains


subroutine gaussElim(matrix, numCols, numRows)
implicit none

real(sp), allocatable, dimension(:,:) :: matrix
integer, intent(in) :: numCols, numRows
integer :: i, largest, pivotRow

allocate(matrix(numRows, numCols))

do i = 1, numCols
        pivotRow = largestInCol(matrix, numCols, numRows, i)
        rowSwitch(matrix, i, pivotRow)

        !Todo: Perform Gaussian Elimination
end do

end subroutine gaussElim


subroutine rowSwitch(matrix, row1, row2) !switch row1 and row2 in matrix
implicit none

integer, intent(in) :: row1, row2
real(sp), allocatable, dimension(:) :: temp

allocate(temp(size(matrix, 2)) !allocate number of columns in matrix (length of a row)

temp = matrix(row2,:)
matrix(row2,:) = matrix(row1,:)
matrix(row1,:) = temp

end subroutine rowSwitch


integer function largestInCol(matrix, numCols, numRows, colCheck) result(rowIndex)
implicit none

integer, intent(in) :: numCols, numRows, colCheck
integer :: i
real(sp) :: largest = 0.0

rowIndex = -1

do i = 1, numRows
        if (abs(matrix(i,colCheck)) > largest) then
                largest = abs(matrix(i,colCheck))
                rowIndex = i
        end if
end do

end function largestInCol
end module gaussElimination
