module ConDiffGenRightB
use Matrix
!简化，根据边界条件文件，矩阵，格式选择计算右侧向量
!一维一阶
function ConDiffGenRightB1D(in_boundary_file,in_grid_file,in_scheme,in_mat)
	real,dimension(:),allocatable::ConDiffGenRightB1D
	character(128)::in_boundary_file,in_grid_file
	integer::in_scheme
	type(DiagMatrix)::in_mat
end function
end module
