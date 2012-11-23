module ConDiffGenPos
!******************************************************************************
! AUTHOR 	:Black
! DATE		:2012年10月30日
! email		:1507912984@qq.com
! LICENSE	:You can do whatever you want.
!******************************************************************************
!边界
use BoundaryDefine
use BoundaryLoad
!网格
use GridDefine
use GridLoad

implicit none
contains

function GenPosVec(in_grid_path)
	real,dimension(:),allocatable::GenPosVec
	character(128),intent(in)::in_grid_path
	character(128)::errors
	integer::success,posSize,i
	real deltX
!只实现一维均匀网格
!定义网格参数
	type(Simple1DGrid)::s1dgrid
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
!使用有限容积法离散
	posSize=s1dgrid%NumberOfPoints+2
	deltX=s1dgrid%Length/s1dgrid%NumberOfPoints
	allocate(GenPosVec(posSize))
!左边界面	
	GenPosVec(1)=0.0
!右边截面
	GenPosVec(posSize)=s1dgrid%Length
!中间点
	do i=2,posSize-1
		GenPosVec(i)=(i-1.5)*deltX
	end do
end function
end module
