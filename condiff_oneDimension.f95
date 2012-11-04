! AUTHOR :Black
! DATE:2012年10月30日
! email:1507912984@qq.com
! LICENSE:
! This is main programe for solving the convection-diffusion problem.
! INPUT:the path of parameters file and the path of of out file.
include"ConDiffSchemes1DUS.f95"
include"Algorithm.f95"
include"ConDiffGloble.f95"
include"ConDiffGenFD.f95"
include"Matrix.f95"
include"ConDiffGenRightB.f95"
program condiff
use ConDiffGloble
use Matrix
use ConDiffGenFD
use ConDiffSchemes1D
use ConDiffGenRightB
use EquationSolve
implicit none
!*************************************************************
!定义全局变量
!*************************************************************
	type (GlobleParameters)::parameters
	real,dimension(:),allocatable::ans,right
	type(DiagMatrix)::mat
	integer in_scheme
	character(128) in_boundary_path
	character(128) in_grid_path
!*************************************************************
!调用输入解析模块，解析输入文件
!*************************************************************
	call InitParameters(parameters)
	in_scheme=getScheme(parameters)
	in_boundary_path=getBoundaryFilePath(parameters)
	in_grid_path=getGridFilePath(parameters)
	call ConDiffScheme1D(mat,in_scheme,GenFDPairs(in_boundary_path,in_grid_path))
	call ConDiffGenRightB1D(in_boundary_path,in_grid_path,in_scheme,mat,right)
	ans=MEResolve(mat,right)
!call PutOut(输出格式,ans)
end program condiff
