module ConDiffGenPos
!******************************************************************************
! AUTHOR 	:Black
! DATE		:2012年10月30日
! email		:1507912984@qq.com
! LICENSE	:You can do whatever you want.
!******************************************************************************
!网格
use GridDefine

implicit none
contains

function GenPosVec(in_grid)
	real,dimension(:),allocatable::GenPosVec
	type(Simple1DGrid),intent(in)::in_grid
	integer::posSize,i
	real::deltX
!使用有限容积法离散
	posSize=in_grid%NumberOfPoints+2
	deltX=in_grid%Length/in_grid%NumberOfPoints
	allocate(GenPosVec(posSize))
!左边界面	
	GenPosVec(1)=0.0
!右边截面
	GenPosVec(posSize)=in_grid%Length
!中间点
	do i=2,posSize-1
		GenPosVec(i)=(i-1.5)*deltX
	end do
end function
end module
