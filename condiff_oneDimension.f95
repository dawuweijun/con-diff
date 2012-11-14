!******************************************************************************
! AUTHOR 	:Black
! DATE		:2012年10月30日
! email		:1507912984@qq.com
! LICENSE	:You can do whatever you want.
!******************************************************************************
! 		This is the main programe for solving the one dimension 
!	convection-diffusion problem.
! INPUT		:Path of the parameters file.
program condiff
use ConDiffGloble
use Matrix
use ConDiffGenFD
use ConDiffSchemes1D
use ConDiffGenRightB
use DMESolve
implicit none
!*************************************************************
!定义全局变量
!*************************************************************
	type (GlobleParameters)::parameters
	real,dimension(:),allocatable::ans,right
	type(DiagMatrix)::mat			!矩阵
	integer in_scheme			!格式选择
	character(128) in_boundary_path		!边界文件路径
	character(128) in_grid_path		!网格文件路径
	integer::argc!the number of parameters from command line
	character(128)::args!for saving the parameter string
!******************************************************************************
!Read parameters from the command line
!******************************************************************************
	argc=iargc() !
	if(argc/=1)then
		print*,"You should input the parameter file path ."
		print*,"Like This:"
		print*,"Main test.conf"
		stop	!退出
	end if
	call getarg(1,args)
!文件是否存在
if (IsFileExist(args).neqv..true.) then
	print*,"The Configure file you selected dosen't exist:Quiting"
	stop
end if
!文件存在，调用解析函数读入

!******************************************************************************
!调用输入解析模块，解析输入文件
!******************************************************************************
	call InitParameters(parameters,args)
!******************************************************************************
!
!******************************************************************************
	in_scheme=getScheme(parameters)
	in_boundary_path=getBoundaryFilePath(parameters)
	in_grid_path=getGridFilePath(parameters)
!******************************************************************************
call ConDiffScheme1D(mat,in_scheme,GenFDPairs(in_boundary_path,in_grid_path))
!******************************************************************************
call ConDiffGenRightB1D(in_boundary_path,in_grid_path,in_scheme,mat,right)
!******************************************************************************
	ans=DMEResolve(mat,right)
	print*,ans
!call PutOut(输出格式,ans)
end program condiff
