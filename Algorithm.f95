module EquationSolve
use Matrix
implicit none


!*******************************************************************
!**********************三对角矩阵及其算法****************************
!*******************************************************************
contains
!统一接口
function MEResolve(in_mat,in_right)
	real,dimension(:),pointer::MEResolve
	type(DiagMatrix)::in_mat
	real,dimension(:),allocatable::in_right
	print*,"I am called here !"
end function
subroutine TDMAResolveTri(in_mat,in_right_b,out_ans)
	type(DiagMatrix),intent(in)::in_mat
	real,dimension(:),intent(in)::in_right_b
	real,dimension(:),allocatable,intent(out)::out_ans
	real,dimension(:),allocatable::temp
	integer MaxSize,I
	logical matreasonable
!判断矩阵的合理性	
if(IsMatReasonable(in_mat))then
	MaxSize=getMatSize(in_mat)
	if(size(in_right_b)==MaxSize) then
		allocate(out_ans(MaxSize),temp(MaxSize))
	else
		print*,"The Size Of The Vector Is Not Right: Quiting Now !"
		stop
	end if
else
	print*,"The Size Of The Mat Is Not Right: Quiting Now !"
	stop
end if
!TDMA Algorithm
!第一次迭代
temp(1)=in_mat%left_P(1)
out_ans(1)=in_right_b(1)
DO I=2,MaxSize
	temp(I)=in_mat%left_P(I)-in_mat%left_E(I-1)*in_mat%left_W(I)/temp(I-1)
	out_ans(I)=in_right_b(I)-out_ans(I-1)*in_mat%left_W(I)/temp(I-1)
END DO
CONTINUE
!回代
out_ans(MaxSize)=out_ans(MaxSize)/temp(MaxSize)
DO I=MaxSize-1,1
	out_ans(I)=(out_ans(I)-in_mat%left_E(I)*out_ans(I+1))/temp(I);
END DO
	deallocate(temp)
end subroutine
!*********************************************************************
!***************************五对角矩阵及其算法*************************
!*********************************************************************
subroutine TDMAResolveFive(in_mat,in_right_b,out_ans)
type(DiagMatrix),intent(in)::in_mat
	real,dimension(:),intent(in)::in_right_b
	real,dimension(:),allocatable,intent(out)::out_ans
	real,dimension(:),allocatable::temp_R
	type(DiagMatrix)::tempTriMat
	integer MaxSize,I
	logical matreasonable
	real temp
!判断矩阵的合理性
	if(IsMatReasonable(in_mat))then
		MaxSize=getMatSize(in_mat)
		if(size(in_right_b)/=MaxSize) then
			print*,"The Size Of The Vector Is Not Right: Quiting Now !"
			stop
		end if
	else
		print*,"The Size Of The Mat Is Not Right: Quiting Now !"
		stop
	end if
!分配临时内存
allocate(tempTriMat%left_W(MaxSize),&
		tempTriMat%left_P(MaxSize),&
		tempTriMat%left_E(MaxSize),&
		temp_R(MaxSize))
!复制矩阵中间三列和右侧向量
tempTriMat%left_W=in_mat%left_W
tempTriMat%left_P=in_mat%left_P
tempTriMat%left_E=in_mat%left_E
temp_R=in_right_b
!五对角矩阵求解过程如下
!第一次迭代，消除WW
do I=3,MaxSize
	temp=in_mat%left_WW(I)/tempTriMat%left_W(I-1)
	tempTriMat%left_W(I)=tempTriMat%left_W(I)-tempTriMat%left_P(I-1)*temp
	tempTriMat%left_P(I)=tempTriMat%left_P(I)-tempTriMat%left_E(I-1)*temp
	!注意，tempTriMat%left_E（MaxSize）应该为零，但是无所谓
	tempTriMat%left_E(I)=tempTriMat%left_E(I)-in_mat%left_EE(I-1)*temp
	temp_R(I)=temp_R(I)-temp_R(I-1)*temp
end do
!第二次迭代，消除EE
do I=MaxSize-2,1
	temp=in_mat%left_EE(I)/tempTriMat%left_E(I)
	tempTriMat%left_E(I)=tempTriMat%left_E(I)-tempTriMat%left_P(I+1)*temp
	tempTriMat%left_P(I)=tempTriMat%left_P(I)-tempTriMat%left_W(I+1)*temp
	temp_R(I)=temp_R(I)-temp_R(I+1)*temp
end do
!第三次，直接调用三对角矩阵求解算法
call TDMAResolveTri(tempTriMat,temp_R,out_ans)
!释放内存
deallocate(	tempTriMat%left_W,tempTriMat%left_P,tempTriMat%left_E,temp_R)
end subroutine
end module
