 module GridDefine
!******************************************************************************
! AUTHOR 	:Black
! DATE		:2012年10月30日
! email		:1507912984@qq.com
! LICENSE	:You can do whatever you want.
!******************************************************************************
!		This file defined a grid of basic type and some methods for the
!problem this project concerned.
implicit none
!you may add you 2d and 3d grid in this file if possible.
!****************************************************************
!**********************一维均匀网格*************************
!****************************************************************
type Simple1DGrid
	character(128)::GridFilePath
	integer NumberOfPoints	!至少为1
	real Length		!计算区域长度，大于零
!	logical HafeVolume	!是否存在半个体积的计算单元
end type
contains
!****************************************************************
!**********************much more here****************************
!****************************************************************


!****************************************************************
!*****************一维非均匀网格，STL文件定义*********************
!****************************************************************
!由于stl文件格式暂时不熟悉，暂不实现
!type STL1DGrid,
!integer
!end type
end module
