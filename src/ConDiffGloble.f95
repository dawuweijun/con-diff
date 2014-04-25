module ConDiffGloble
!******************************************************************************
! AUTHOR 	:Black
! DATE		:2012年10月30日
! email		:1507912984@qq.com
! LICENSE	:You can do whatever you want.
!******************************************************************************
!	This file defined some global parameters necessary. For more details of
! how does it read parameter in, have a look at the file named FileOperate.f.
! For more information about the format of configure file, have a look at
! the file named test.conf.  
!******************************************************************************
use FileOperate
implicit none
!*******************输入文件参数格式说明****************************************
!一维稳态，对流扩散问题，多种可选格式
!******************************************************************************
type GlobleParameters
	character(128)::nameX			!
	character(128)::namePhi			!
	character(128)::ConfiureFilePath	!输入文件路径，由命令行获取
	character(128)::BoundaryFilePath	!边界文件路径，由配置文件获取
	character(128)::GridInputFilePath	!网格文件路径，由配置文件获取
	character(128)::Scheme			!格式选择，有配置文件获取
	character(128)::OutPutFileType		!输出类型，由配置文件获取
	character(128)::OutPutFilePath		!输出路径，由配置文件获取
end type

contains
!******************************************************************************
!获取主配置文件路径，并且读取其中参数
subroutine InitParameters(this,configurefile)
	type (GlobleParameters),intent(in)::this
	character(128)::configurefile
	character(128)::errorInfo
	
!TODO:文件存在，调用解析函数读入
	if(ReadConfFile(this,configurefile,errorInfo)/=0) then
		print*,errorInfo !错误输出
!		print*,this%OutPutFileType
!		print*,this%OutPutFilePath
		stop "***ERROR***:PASER CONFIGURE FILE"!退出程序
	else
		print*,"Success In Reading The Configure File!"
	end if
!文件读入成功
end subroutine
!读取文件参数
function ReadConfFile(this,strFilePath,errorInfo)
	character(128)::strFilePath
	type(GlobleParameters)::this
	character(128)::errorInfo
	integer::ReadConfFile,success
	character(128)::nameBoundaryPath,nameGridPath,&
	namescheme,nameOutPutType,nameOutPutPath,namenameX,namenamePhi
	ReadConfFile=0
	this%ConfiureFilePath=strFilePath
	errorInfo=""					!清空错误信息
	namenameX="nameX"
	namenamePhi="namePhi"
	nameBoundaryPath='BoundaryFilePath'
	nameGridPath='GridInputFilePath'
	namescheme='Scheme'
	nameOutPutType='OutPutFileType'
	nameOutPutPath='OutPutFilePath'
!打开文件
	open(unit=8,file=this%ConfiureFilePath,status='old',action='read')
!坐标轴名称
		success=getArgs(8,'!',namenameX,'=',this%nameX)
		if(success/=0)then
			ReadConfFile=success
		 	errorInfo=errorInfo(1:len_trim(errorInfo))//"errs4nameX;"
		end if
!变量名称
		success=getArgs(8,'!',namenamePhi,'=',this%namePhi)
		if(success/=0)then
			ReadConfFile=success
		 	errorInfo=errorInfo(1:len_trim(errorInfo))//"errs4namePhi;"
		end if
!边界条件
		success=getArgs(8,'!',nameBoundaryPath,'=',this%BoundaryFilePath)
		if(success/=0)then
			ReadConfFile=success
		 	errorInfo=errorInfo(1:len_trim(errorInfo))//"errs4boundary;"
		end if
!网格
		success=getArgs(8,'!',nameGridPath,'=',this%GridInputFilePath)
		if(success/=0)then
		 	ReadConfFile=success
		 	errorInfo=errorInfo(1:len_trim(errorInfo))//"errs4Grid;"
		end if
!格式选择
		success=getArgs(8,'!',namescheme,'=',this%Scheme)
		if(success/=0)then
		 	ReadConfFile=success
		 	errorInfo=errorInfo(1:len_trim(errorInfo))//"errs4Scheme;"
		end if
!输出类型
!		print*,"reading parameters for output type"
		success=getArgs(8,'!',nameOutPutType,'=',this%OutPutFileType)
		if(success/=0)then
		 	ReadConfFile=success
		 	errorInfo=errorInfo(1:len_trim(errorInfo))//"errs4OutPutType;"
		end if
!输出路径
		success=getArgs(8,'!',nameOutPutPath,'=',this%OutPutFilePath)
		if(success/=0)then
		 	ReadConfFile=success
		 	errorInfo=errorInfo(1:len_trim(errorInfo))//"errs4OutPutPath;"
		end if
	close(8)
end function

!获取x坐标轴名称，用于tecplot输出
function getNameX(this)
	character(8)::getNameX
	type(GlobleParameters),intent(in)::this
	if(len_trim(this%nameX)/=0) then
		getNameX=trim(this%nameX)
	endif
end function
!获取y轴变量名称，用于tecplot输出
function getNameVal(this)
	character(8)::getNameVal
	type(GlobleParameters),intent(in)::this
	if(len_trim(this%namePhi)/=0)then
		getNameVal=trim(this%namePhi)
	endif
end function

!获取边界文件路径
function getBoundaryFilePath(this)
	character(128)::getBoundaryFilePath
	type(GlobleParameters),intent(in)::this
	if (IsFileExist(this%BoundaryFilePath))then
		getBoundaryFilePath=this%BoundaryFilePath
	else
		print*,this%BoundaryFilePath,'Does Not Exsit.'
	end if
end function
!获取网格文件路径
function getGridFilePath(this)
	character(128)::getGridFilePath
	type(GlobleParameters),intent(in)::this
	if(IsFileExist(this%GridInputFilePath))then
		getGridFilePath=this%GridInputFilePath
	else
		print*,this%GridInputFilePath,'Does Not Exsit.'
	end if
end function
function getScheme(this)
	type(GlobleParameters),intent(in)::this
	integer::getScheme
	if(len_trim(this%Scheme)/=0)then
	read(this%Scheme,*)getScheme
	end if
end function
!获取输出文件类型
function getOutPutFileType(this)
	character(128)::getOutPutFileType
	type(GlobleParameters),intent(in)::this
!
	if(len_trim(this%OutPutFileType)/=0) then
		getOutPutFileType=this%OutPutFileType
	end if
end function
!获取输出文件路径
function GetOutPutFilePath(this)
	type(GlobleParameters),intent(in)::this
	character(128)::GetOutPutFilePath
!	GetOutPutFilePath=this%OutPutFilePath
!	if(IsFileExist(this%OutPutFilePath)) then
!		GetOutPutFilePath=this%OutPutFilePath
!	else
!		print*,this%OutPutFilePath,'Does Not Exsit.'
!	endif
	if(len_trim(this%OutPutFilePath)/=0)then
		GetOutPutFilePath=this%OutPutFilePath
	end if
end function
end module
