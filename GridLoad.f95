module GridLoad
!******************************************************************************
! AUTHOR 	:Black
! DATE		:2012年10月30日
! email		:1507912984@qq.com
! LICENSE	:You can do whatever you want.
!******************************************************************************
!	This file achieved the methord for loading the grid defined in file
!named GridDefine.f. For More details how it work, go to the file named
!FileOperate.f
use GridDefine
use FileOperate
implicit none

interface readGrid
module procedure readGridS1D
end interface

contains
function readGridS1D(this,strFilePath,error)
	character(128),intent(in)::strFilePath
	type(Simple1DGrid),intent(inout)::this
	character(128),intent(inout)::error
	integer::readGridS1D,success
	character(128)::nameNumberOfPoints,nameLength
	character(128)::tempvalue
	this%GridFilePath=strFilePath
	nameNumberOfPoints='NumberOfPoints'
	nameLength='Length'
	error=''!清空错误缓存
!打开网格文件
    if(IsFileExist(strFilePath).neqv..true.)then
    	print*,"This GridFile Does Not Exist: Quiting ."
    	stop
    end if
	readGridS1D=0
	open(unit=24,file=strFilePath,status="old",action='read')
!NumberOfPoints
	success=getArgs(24,'!',nameNumberOfPoints,'=',tempvalue)
	if(success/=0)then
		error=error(1:len_trim(error))//'errs4NumberOfPoints;'
		readGridS1D=success
	else
		read(tempvalue,*) this%NumberOfPoints
	end if
!Length
	success=getArgs(24,'!',nameLength,'=',tempvalue)
	if(success/=0)then
		error=error(1:len_trim(error))//'errs4Length;'
		readGridS1D=success
	else
		read(tempvalue,*) this%Length
	end if
!关闭网格文件
	close(24)
end function
end module
