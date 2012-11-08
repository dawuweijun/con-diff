module DMESolve
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
	print*,'The Size Of The Matrix To Solve Is :',MaxSize
!直接对矩阵进行操作，不再复制
!五对角矩阵，化简为三对角矩阵
if(in_mat%MatType==5)then
!第一次迭代，消除WW
do I=3,MaxSize
	temp=in_mat%left_WW(I)/in_mat%left_W(I-1)
	in_mat%left_W(I)=in_mat%left_W(I)-in_mat%left_P(I-1)*temp
	in_mat%left_P(I)=in_mat%left_P(I)-in_mat%left_E(I-1)*temp
	!注意，in_mat%left_E（MaxSize）应该为零，但是无所谓
	in_mat%left_E(I)=in_mat%left_E(I)-in_mat%left_EE(I-1)*temp
	in_right(I)=in_right(I)-in_right(I-1)*temp
end do
!第二次迭代，消除EE
end if
end function
end module
