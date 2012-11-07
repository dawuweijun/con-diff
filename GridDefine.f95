module GridDefine
implicit none
!you can add you 2d and 3d grid in this file
!****************************************************************
!**********************一维均匀网格*************************
!****************************************************************
type Simple1DGrid
	character(128)::GridFilePath
	integer NumberOfPoints			!至少为1
	real Length						!计算区域长度，大于零
!	logical HafeVolume				!是否存在半个体积的计算单元
end type
contains
!function BuildSimple1DGrid(in_length,in_pointnumber,in_hafeVolume)
!	type (Simple1DGrid)::BuildSimple1DGrid
!	real,intent(in)::in_length
!	integer ,intent(in)::in_pointnumber
!	logical ,intent (in)::in_hafeVolume
!	if ((in_pointnumber-1)<0.OR.in_length<0.OR.in_length=0) then
!		print *,"There Is Something Wrong about the Grid, Quiting !"
!		stop
!	end if
!	BuildSimple1DGrid%Length=in_length
!	BuildSimple1DGrid%NumberOfPoints=in_pointnumber
!	BuildSimple1DGrid%HafeVolume=in_hafeVolume
!end function

!function getDeltXS1DG(this)
!	type(Simple1DGrid),intent(in)::this
!	real::getDeltXS1DG
!	if this%HafeVolume then
!		getDeltXS1DG=this%Length/(this%NumberOfPoints+1)
!	else
!		getDeltXS1DG=this%Length/this%NumberOfPoints
!	end if
!end function
!function getGridSize(this)
!	type(Simple1DGrid),intent(in)::this
!	integer::getGridSize
!	getGridSize=this%NumberOfPoints
!end function
!****************************************************************
!**********************much more here****************************
!****************************************************************


!****************************************************************
!*****************一维非均匀网格，STL文件定义*********************
!****************************************************************
!由于stl文件格式暂时不熟悉，暂不实现
type STL1DGrid,
!integer
end type
end module
