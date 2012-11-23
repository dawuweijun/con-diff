module ConDiffOutPut
!******************************************************************************
! AUTHOR 	:Black
! DATE		:2012年10月30日
! email		:1507912984@qq.com
! LICENSE	:You can do whatever you want.
!******************************************************************************
interface OutPut
	module procedure OutPut1D_ValDeltX,OutPut1D_ValPosPairs
end interface

contains

subroutine OutPut1D_ValDeltX(fileunit,nameVal,nameX,vecVal,pos1,deltX,scheme,error)
!
	integer,intent(in)::fileunit			!文件
	character(128),intent(in)::scheme
	character(8),intent(in)::nameX,nameVal
	real,dimension(:),intent(in)::vecVal
	real,intent(in)::pos1,deltX
	!integer,intent(in)::
	integer,intent(inout)::error
	real,dimension(:),allocatable::vecPos
	integer i
	error=0
!生成位置向量，调用OutPut1D_ValPosPairs
	allocate(vecPos(size(vecVal)))

	vecPos(1)=pos1
	do i=2,size(vecVal)
		vecPos(i)=vecPos(i-1)+deltX
	end do

	call OutPut1D_ValPosPairs(fileunit,nameVal,nameX,vecVal,vecPos,scheme,error)

end subroutine

subroutine OutPut1D_ValPosPairs(fileunit,nameVal,nameX,vecVal,vecPos,scheme,error)
!
	integer,intent(in)::fileunit			!文件
	character(128),intent(in)::scheme
	character(8),intent(in)::nameVal,nameX
	real,dimension(:),intent(in)::vecVal,vecPos
	!integer,intent(in)::scheme
	integer,intent(inout)::error
	integer i

print*,nameX
print*,nameVal

	error=0
!首先，判断 vecVal 与vecPos 长度是否相等
	if(size(vecVal)/=size(vecPos))then
		print *,"The lengths of the two vals are not equal for output!"
		print*,"Size of vecPos :",size(vecPos)
		print*,"Size of vecVal :",size(vecVal)
		error=1
		return
	end if

	write(fileunit,*)'TITLE="LINE"'
	write(fileunit,*)'VARIABLES= "',nameX,'","',nameVal,'"'
	do i=1,size(vecPos)
		write(fileunit,*)vecPos(i),vecVal(i)
	end do

end subroutine

end module ConDiffOutPut
