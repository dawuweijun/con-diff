module ConDiffGenFD
use BoundaryDefine
use BoundaryLoad
use GridDefine
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
	character(128)::errors
	integer::success
!只实现一维均匀网格
!定义网格参数
	type(Simple1DGrid)::s1dgrid
!只实现定fd条件,
!定义边界条件类型
	type(BoundaryFirst1DSteady)::f1dsboundary
!其他请自行扩展
	errors=''
!载入网格文件
	success=readGrid(s1dgrid,in_grid_path,errors)
	if(success/=0)then
		print*,"Errors When Loading Grid:",errors
		stop
	end if
!载入边界文件
	success=readBoundary(f1dsboundary,in_boundary_path,errors)
	if(success/=0)then
		print*,"Error When Loading Boundary:",errors
		stop
end function
end module
