module DMESolve
!******************************************************************************
! AUTHOR 	:Black
! DATE		:2012年10月30日
! email		:1507912984@qq.com
! LICENSE	:You can do whatever you want.
!******************************************************************************
!	This file achieved the solution of fivediagmatrix and tridiagmatrix.
!More details of diagmatrix in the file named Matrix.f
use Matrix
implicit none
contains
!五对角和三对角矩阵方程的求解
function DMEResolve(in_mat,in_right)
	real,dimension(:),allocatable::DMEResolve
	real,dimension(:),allocatable::in_right
	type(DiagMatrix)::in_mat
	real::temp
	integer::I, MaxSize
	MaxSize=in_mat%MatSize
	allocate(DMEResolve(in_mat%MatSize))
!直接对矩阵进行操作，不再复制
!五对角矩阵，化简为三对角矩阵
if(in_mat%MatType==5)then
!第一次迭代，消除left_WW
	do I=3,MaxSize
		temp=in_mat%left_WW(I)/in_mat%left_W(I-1)
		in_mat%left_W(I)=in_mat%left_W(I)-in_mat%left_P(I-1)*temp
		in_mat%left_P(I)=in_mat%left_P(I)-in_mat%left_E(I-1)*temp
		in_mat%left_E(I)=in_mat%left_E(I)-in_mat%left_EE(I-1)*temp
		in_right(I)=in_right(I)-in_right(I-1)*temp
	end do
!第二次迭代，消除left_EE
	do I=MaxSize-2,1,-1
		temp=in_mat%left_EE(I)/in_mat%left_E(I+1)
		in_mat%left_E(I)=in_mat%left_E(I)-in_mat%left_P(I+1)*temp
		in_mat%left_P(I)=in_mat%left_P(I)-in_mat%left_W(I+1)*temp
		in_right(I)=in_right(I)-in_right(I+1)*temp
	end do
end if
!三对角矩阵求解
print*,in_mat%left_W
print*,in_mat%left_P
print*,in_mat%left_E
print*,in_right
!第一次迭代，消去left_W
	do I=2,MaxSize
		temp=in_mat%left_W(I)/in_mat%left_P(I-1)
		in_mat%left_P(I)=in_mat%left_P(I)- in_mat%left_E(I-1)*temp
		in_right(I)=in_right(I)- in_right(I-1)*temp
	end do
!第二次迭代，消去left_E
	do I=MaxSize-1,1,-1
		temp=in_mat%left_E(I)/in_mat%left_P(I+1)
		in_right(I)=in_right(I)- in_right(I+1)*temp
	end do
!计算结果
	do I=1,MaxSize
		DMEResolve(I)=in_right(I)/in_mat%left_P(I)
	end do
end function

end module
