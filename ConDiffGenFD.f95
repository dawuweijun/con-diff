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
use BoundaryLoad
!网格
use GridDefine
use GridLoad

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
		print*,"Errors When Loading Grid : ",errors
		stop
	else
		print*,"Success In Loading The Grid File."
	end if
!载入边界文件
	success=readBoundary(f1dsboundary,in_boundary_path,errors)
	if(success/=0)then
		print*,"Error When Loading Boundary : ",errors
		stop
	else
		print*,"Success In Loading The Boundary File."
	end if
!给FDPairs分配内存
	GenFDPairs%FDSize=s1dgrid%NumberOfPoints+1
	allocate(GenFDPairs%F(GenFDPairs%FDSize),GenFDPairs%D(GenFDPairs%FDSize))
!整体赋值
	GenFDPairs%F=f1dsboundary%Velocity*f1dsboundary%Density
	GenFDPairs%D=f1dsboundary%Gama*s1dgrid%NumberOfPoints/s1dgrid%Length
!边界处理
	GenFDPairs%D(1)=GenFDPairs%D(1)*2
	GenFDPairs%D(GenFDPairs%FDSize)=GenFDPairs%D(GenFDPairs%FDSize)*2
!完毕
end function
end module
