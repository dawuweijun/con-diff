!************************模块说明*****************************
!定义，初始化全局变量，读取输入参数，解析输入文件
!*************************************************************
module ConDiffGloble
use FileOperate
implicit none
!*******************输入文件参数格式说明***********************
!一维稳态，对流扩散问题，多种可选格式
!*************************************************************
type GlobleParameters
	character(128)::ConfiureFilePath		!输入文件路径，由命令行获取
	character(128)::BoundaryFilePath	!边界文件路径，由配置文件获取
	character(128)::GridInputFilePath	!网格文件路径，由配置文件获取
	character(128)::Scheme				!格式选择，有配置文件获取
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
	character(128)::errorInfo
	argc=iargc() !
	if(argc/=2)then
		print*,"You should input the parameter file path . Quiting Now！"
		stop						!退出
	end if
	call getarg(2,args)
!文件是否存在
if (IsFileExist(args).neqv..true.) then
	print*,"The Configure file you selected dosen't exist:Quiting"
	stop
end if
!TODO:文件存在，调用解析函数读入
	if(ReadConfFile(this,args,errorInfo)/=0) then
		print*,errorInfo !错误输出
		stop!退出程序
	else
		print*,"Success In Reading The Configure File!"
	end if
!文件读入成功
end subroutine
!读取文件参数
function ReadConfFile(this,strFilePath,error)
	character(128)::strFilePath
	type(GlobleParameters)::this
	character(128)::error
	integer::ReadConfFile,success
	character(128)::nameBoundaryPath,nameGridPath,&
	namescheme,nameOutPutType,nameOutPutPath
	ReadConfFile=0
	this%ConfiureFilePath=strFilePath
	error=""
	nameBoundaryPath='BoundaryFilePath'
	nameGridPath='GridInputFilePath'
	namescheme='Scheme'
	nameOutPutType='OutPutFileType'
	nameOutPutPath='OutPutFilePath'
!打开文件
	open(unit=8,file=this%ConfiureFilePath,status='old',action='read')
!边界条件
		 success=getArgs(8,'!',nameBoundaryPath,'=',this%BoundaryFilePath)
		 if(success/=0)then
		 	ReadConfFile=success
		 	errorInfo="errs 4 boundary;"
		 end if
!网格
		success=getArgs(8,'!',nameGridPath,'=',this%GridInputFilePath)
		if(success/=0)then
		 	ReadConfFile=success
		 	errorInfo=errorInfo(1:len_trim(errorInfo))//"errs 4 Grid;"
		end if
!格式选择
		success=getArgs(8,'!',namescheme,'=',this%Scheme)
		if(success/=0)then
		 	ReadConfFile=success
		 	errorInfo=errorInfo(1:len_trim(errorInfo))//"errs 4 Scheme;"
		end if
!输出类型
		success=getArgs(8,'!',nameOutPutType,'=',this%OutPutFileType)
		if(success/=0)then
		 	ReadConfFile=success
		 	errorInfo=errorInfo(1:len_trim(errorInfo))//"errs 4 OutPutType;"
		end if
!输出路径
		success=getArgs(8,'!',nameOutPutPath,'=',this%OutPutFilePath)
		if(success/=0)then
		 	ReadConfFile=success
		 	errorInfo=errorInfo(1:len_trim(errorInfo))//"errs 4 OutPutPath;"
		end if
	close(8)
end function
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
function getGridFilePath(this)
	character(128)::getGridFilePath
	type(GlobleParameters),intent(in)::this
	if(IsFileExist(this%GridInputFilePath))then
		getGridFilePath=this%GridInputFilePath
	else
		getGridFilePath=""
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
