module ConDiffGenFD
!use Boundary
!use Grid

type FDPairs
	integer FDSize
	real,dimension(:),pointer::F
	real,dimension(:),pointer::D
end type

contains
!*********************************************************
!首先实现两个方法,只针对稳态问题
!*********************************************************
!根据边界条件文件和网格参数文件计算F,D

function GenFDPairs(in_boundary_path,in_grid_path)
	type(FDPairs)::GenFDPairs
	character(128),intent(in)::in_boundary_path,in_grid_path
end function
end module
