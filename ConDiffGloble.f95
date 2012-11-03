!************************模块说明*****************************
!定义，初始化全局变量，读取输入参数，解析输入文件
!*************************************************************
module ConDiffGloble
implicit none
!*******************输入文件参数格式说明***********************
!一维稳态，对流扩散问题，多种可选格式
!*************************************************************
type GlobleParameters
	character(128)::InPutFilePath		!输入文件路径，由命令行获取
	character(128)::BoundaryFilePath	!边界文件路径，由配置文件获取
	character(128)::GridInputFilePath	!网格文件路径，由配置文件获取
	character(128)::scheme				!格式选择，有配置文件获取
	character(128)::OutPutFileType			!输出类型，由配置文件获取
	character(128)::OutPutFilePath			!输出路径，由配置文件获取
end type

contains
!******************************************************************************
!获取主配置文件路径，并且读取其中参数
subroutine InitParameters(this)
	type (GlobleParameters),intent(in)::this
	integer::argc!the number of parameters from command line
	character(128)::args!for saving the parameter string
	argc=iargc() !
	if(argc/=2)then
		print*,"You should input the parameter file path . Quiting Now！"
		stop						!退出
	end if
	call getarg(2,this%InPutFilePath)
!文件是否存在
if (IsFileExist(this%InPutFilePath)) then
	call ReadConfFile(this,args)
else
	print*,"The Parameter file you selected dosen't exist:Quiting"
	stop
end if
!TODO:
end subroutine
!读取文件参数
subroutine ReadConfFile(this,strFilePath)
	character(128),intent(in)::strFilePath
	type(GlobleParameters),intent(in)::this
end subroutine
!获取边界文件路径
function getBoundaryFilePath(this)
	character(128)::getBoundaryFilePath
	type(GlobleParameters),intent(in)::this
	if (IsFileExist(this%BoundaryFilePath))then
		getBoundaryFilePath=this%BoundaryFilePath
	else
		getBoundaryFilePath=""
	end if
end function
!获取网格文件路径
function getGridInputFilePath(this)
	character(128)::getGridInputFilePath
	type(GlobleParameters),intent(in)::this
	if(IsFileExist(this%GridInputFilePath))then
		getGridInputFilePath=this%GridInputFilePath
	else
		getGridInputFilePath=""
	end if
end function
function getScheme(this)
type(GlobleParameters),intent(in)::this
integer::getScheme
end function
!获取输出文件类型
function getOutPutFileType(this)
	character(128)::getOutPutFileType
	type(GlobleParameters),intent(in)::this
!

end function
!获取输出文件路径
function GetOutPutFilePath(this)
	type(GlobleParameters),intent(in)::this
	character(128)::GetOutPutFilePath
	GetOutPutFilePath=this%OutPutFilePath
	if(IsFileExist(this%OutPutFilePath)) then
		GetOutPutFilePath=this%OutPutFilePath
	else
		GetOutPutFilePath=""
	endif
end function
!文件是否存在
function IsFileExist(filepath)
	logical::IsFileExist
	character(128),intent(in)::filepath
	integer::ierror,fileptr
	fileptr=1024
	open(unit=fileptr,file=filepath,status='old',action='read',iostat=ierror)
	if(ierror/=0) then
		IsFileExist=.false.
	else
		IsFileExist=.true.
	end if
	close(unit=fileptr)
end function
end module
