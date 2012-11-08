module GridLoad
use GridDefine
use FileOperate
implicit none

interface readGrid
module procedure readGridS1D
end interface

!type Simple1DGrid
!	character(128)::GridFilePath
!	integer NumberOfPoints			!至少为1
!	real Length						!计算区域长度，大于零
!	logical HafeVolume				!是否存在半个体积的计算单元
!end type

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
