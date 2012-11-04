module ConDiffGenRightB
use Matrix
!简化，根据边界条件文件，矩阵，格式选择计算右侧向量
!一维
implicit none
contains
subroutine ConDiffGenRightB1D&
(in_boundary_path,in_grid_path,in_scheme,in_mat,in_right)
	real,dimension(:),allocatable::in_right
	character(128)::in_boundary_path,in_grid_path
	integer::in_scheme
	type(DiagMatrix)::in_mat
	if(in_scheme<0.or.in_scheme>6)then
		return
	end if
end subroutine
end module
