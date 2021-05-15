module LUDecomp
!performs LU Decomposition via gaussian elimination with partial pivoting
use, intrinsic :: iso_fortran_env
integer :: sp = real_32

contains


subroutine gaussElim(matrix, numCols, numRows)
implicit none

real(sp), allocatable, dimension(:,:) :: matrix
integer, intent(in) :: numCols, numRows
integer :: i, j, k largest, pivotRow
real(sp), allocatable, dimension(:,:) :: elimMatrix

allocate(elimMatrix(numRows, numCols))

do i = 1, numCols-1
        pivotRow = largestInCol(matrix, numCols, numRows, i)
        
        if (pivotRow .ne. i) then
                rowSwitch(matrix, i, pivotRow)
        end if

        if (matrix(i,i) = 0) then
                cycle
        end if

        do j = i + 1, numCols
                elimMatrix(j,i) = matrix(j,i) / matrix(i,i)
        end do        

        do k = i + 1, numCols
                do j = i + 1, numCols
                        matrix(j,k) = matrix(j,k) - elimMatrix(j,i) * matrix(i,k)
                end do 
        end do
end do

deallocate(elimMatrix)

end subroutine gaussElim


subroutine rowSwitch(matrix, row1, row2) !switch row1 and row2 in matrix
implicit none

integer, intent(in) :: row1, row2
real(sp), allocatable, dimension(:) :: temp

allocate(temp(size(matrix, 2)) !allocate number of columns in matrix (length of a row)

temp = matrix(row2,:)
matrix(row2,:) = matrix(row1,:)
matrix(row1,:) = temp

deallocate(temp)

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
end module LUDecomp
