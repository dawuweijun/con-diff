module BoundaryLoad
use BoundaryDefine
use FileOperate
implicit none
interface readBoundary
module procedure readBoundaryFile1DS
end interface
contains
!对流与扩散一维问题一类恒定边界
function readBoundaryFile1DS(this,strFilePath,error)
	character(128),intent(in)::strFilePath
	type(BoundaryFirst1DSteady),intent(inout)::this
	character(128),intent(inout)::error
	integer::readBoundaryFile1DS,success
	character(128)::namephi_left,namephi_right,nameVelocity,nameDensity,namegama
	character(128)::tempvalue
	this%BoundaryFilePath=strFilePath
	namephi_left='Phi_left'
	namephi_right='Phi_right'
	nameVelocity='Velocity'
	nameDensity='Density'
	namegama='Gama'
	error=''
!打开边界文件
	readBoundaryFile1DS=0
	open(unit=16,file=strFilePath,status="old",action='read')
!Phi_left
	success=getArgs(16,'!',namephi_left,'=',tempvalue)
	if(success/=0)then
		error=error(1:len_trim(error))//'errs4phi_left;'
		readBoundaryFile1DS=success
	else
		read(tempvalue,*) this%Phi_left
	end if
!Phi_right
	success=getArgs(16,'!',namephi_right,'=',tempvalue)
	if(success/=0)then
		error=error(1:len_trim(error))//'errs4phi_right;'
		readBoundaryFile1DS=success
	else
		read(tempvalue,*) this%Phi_right
	end if
!Velocity
	success=getArgs(16,'!',nameVelocity,'=',tempvalue)
	if(success/=0)then
		error=error(1:len_trim(error))//'errs4velocity;'
		readBoundaryFile1DS=success
	else
		read(tempvalue,*) this%Velocity
	end if
!Density
	success=getArgs(16,'!',nameDensity,'=',tempvalue)
	if(success/=0)then
		error=error(1:len_trim(error))//'errs4density;'
		readBoundaryFile1DS=success
	else
		read(tempvalue,*) this%Density
	end if
!Gama
	success=getArgs(16,'!',namegama,'=',tempvalue)
	if(success/=0)then
		error=error(1:len_trim(error))//'errs4gama;'
		readBoundaryFile1DS=success
	else
		read(tempvalue) this%Gama
	end if
!关闭边界文件
	close(16)
end function

function getBoundaryType(filepath)
	character(128)::getBoundaryType
	character(128)::filepath
end function
end module
