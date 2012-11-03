! AUTHOR :Black
! DATE:2012年10月30日
! email:1507912984@qq.com
! LICENSE:
! This is main programe for solving the convection-diffusion problem.
! INPUT:the path of parameters file and the path of of out file.
program condiff
use EquationSolve
use ConDiffGloble
use BoundaryType
use BoundaryLoad
use Grid
use Matrix
implicit none
!*************************************************************
!定义全局变量
!*************************************************************
	type (GlobleParameters)::parameters
	real,dimension(:),allocatable::ans
!*************************************************************
!调用输入解析模块，解析输入文件
!*************************************************************
call InitParameters(parameters)
!
call ConDiffScheme1D(GenFDPairs(in_boundary_path,in_grid_path),in_scheme,mat)
call ConDiffGenRightB1D(in_boundary_file,in_grid_file,in_scheme,mat,right)
call MEResolve(mat,right,ans)
call PutOut(输出格式,ans)
end program condiff
