module ConDiffGenFD
!******************************************************************************
! AUTHOR 	:Black
! DATE		:2012年10月30日
! email		:1507912984@qq.com
! LICENSE	:You can do whatever you want.
!******************************************************************************
!		This file achieved the generation of the number F and D. You should 
!	have noticd the difference of D between boundary nodes and inner nodes.
!边界
use BoundaryDefine
!网格
use GridDefine

implicit none
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

function GenFDPairs(in_boundary,in_grid)
	type(FDPairs)::GenFDPairs
	type(BoundaryFirst1DSteady),intent(in)::in_boundary
	type(Simple1DGrid),intent(in)::in_grid

	GenFDPairs%FDSize=in_grid%NumberOfPoints+1
	allocate(GenFDPairs%F(GenFDPairs%FDSize),GenFDPairs%D(GenFDPairs%FDSize))
!整体赋值
	GenFDPairs%F=in_boundary%Velocity*in_boundary%Density
	GenFDPairs%D=in_boundary%Gama*in_grid%NumberOfPoints/in_grid%Length
!边界处理
	GenFDPairs%D(1)=GenFDPairs%D(1)*2
	GenFDPairs%D(GenFDPairs%FDSize)=GenFDPairs%D(GenFDPairs%FDSize)*2
!完毕
end function
end module
