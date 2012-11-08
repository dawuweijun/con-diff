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
!in_mat%left_W(1)=0.0
!in_mat%left_E(MaxSize)=0.0
!call computeZeroFlag(in_mat)
print*,in_mat%left_W
print*,in_mat%left_P
print*,in_mat%left_E
print*,in_right
!第一次迭代，消去left_W
!print*,'Zero_Flag_left_W:',in_mat%Zero_Flag_left_W
if(in_mat%Zero_Flag_left_W==0) then!全部为真
	do I=2,MaxSize
		temp=in_mat%left_W(I)/in_mat%left_P(I-1)
		in_mat%left_P(I)=in_mat%left_P(I)- in_mat%left_E(I-1)*temp
		in_right(I)=in_right(I)- in_right(I-1)*temp
	end do
	in_mat%Zero_Flag_left_W=2
else if(in_mat%Zero_Flag_left_W==1)then!不全为真
	print*,"This Array Is Not All Zero or All Not Zero : "
	print*,in_mat%left_W
	print*,"Quiting."
end if
!
!print*,"after remove left_w"
!print*,in_mat%left_W
!print*,in_mat%left_P
!print*,in_mat%left_E
!print*,'in_right:',in_right
!第二次迭代，消去left_E
if(in_mat%Zero_Flag_left_E==0) then!全部为真
	do I=MaxSize-1,1,-1
		temp=in_mat%left_E(I)/in_mat%left_P(I+1)
		in_right(I)=in_right(I)- in_right(I+1)*temp
	end do
	in_mat%Zero_Flag_left_E=2
else if(in_mat%Zero_Flag_left_E==1)then!不全为真
	print*,"This Array Is Not All Zero or All Not Zero : "
	print*,in_mat%left_E
	print*,"Quiting."
end if
!计算结果
do I=1,MaxSize
	DMEResolve(I)=in_right(I)/in_mat%left_P(I)
end do
end function

end module
