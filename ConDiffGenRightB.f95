module ConDiffGenRightB
!******************************************************************************
! AUTHOR 	:Black
! DATE		:2012年10月30日
! email		:1507912984@qq.com
! LICENSE	:You can do whatever you want.
!******************************************************************************
!	This file achieved the generation of the vector in the right side of a 
!DiagMatrix Equation. For more information of diffrent scheme, go to the 
!file named ConDiffScheme1DUS.f. For more information about the matrix, go
!to the file named Matrix.f
!简化，根据边界条件文件，矩阵，格式选择计算右侧向量
!一维
use Matrix
use BoundaryDefine
use BoundaryLoad
implicit none
contains

subroutine ConDiffGenRightB1D(in_boundary,in_scheme,in_mat,in_right)
	type(BoundaryFirst1DSteady),intent(in)::in_boundary
!	type(Simple1DGrid),intent(in)::in_grid
	integer,intent(in)::in_scheme
	type(DiagMatrix)::in_mat
	real,dimension(:),allocatable::in_right
	integer::N
	if(in_scheme<0.or.in_scheme>5)then
		return
	end if
!说明，暂时不需加载GridFile，非均匀网格时需要加载GridFile
!分配内存
	N=in_mat%MatSize
	allocate(in_right(N))
!根据格式和矩阵计算rightB
!注意矩阵保存的系数除了P点外全部都已经反号了
select case(in_scheme)
!一阶精度，边界处理方法一模一样
case(0,1,2,3,4)
	in_right=0.0
	in_right(1)= -in_mat%left_W(1)*getPhiLeft(in_boundary)
	in_right(N)=-in_mat%left_E(N)*getPhiRight(in_boundary)
!二阶精度
case(5)
	in_right=0.0
	in_right(1)= -in_mat%left_W(1)*getPhiLeft(in_boundary)
	in_right(2)=-in_mat%left_WW(2)*getPhiLeft(in_boundary)
	in_right(N-1)=- in_mat%left_EE(N-1)*getPhiRight(in_boundary)
	in_right(N)=-in_mat%left_E(N)*getPhiRight(in_boundary)
end select

end subroutine
end module
