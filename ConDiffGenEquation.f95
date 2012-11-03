module ConDiffGenEquation
use ConDiffGenRightB
use Matrix
use ConDiffScheme
use Boundary!边界条件如何处理
!源项如何处理
implicit none
!*************************************************************

!*************************************************************

contains
!首先实现对流和扩散问题一维稳定，第一类边界条件
!in_scheme可选值为一阶精度：
!			0--------中心差分
!			1--------迎风格式
!			2--------混合格式
!			3--------指数格式
!			4--------乘方格式
!定F，D
subroutine ConDiffGenEquation1DSSPEFirst&
		(in_scheme,in_boundary,in_F,in_D,in_N,out_Mat,out_right)
	!type(Simple1DGrid),intent(in)::in_grid
	integer,intent(in)::in_scheme
	type(FirstBoundary1DSteady),intent(in)::in_boundary
	real,intent(in)::in_F,in_D
	integer,intent(in)::in_N!维数，等于节点个数
	type(TriDiagMatrix),intent(out)::out_Mat
	real,dimension(:),allocatable,intent(out)::out_right
	real phi_left,phi_right
	
	phi_left=getPhiLeft(in_boundary)
	phi_right=getPhiRight(in_boundary)
	
!分配内存
!	allocate(out_right(in_N))
!	out_Mat=BuildTriDiagMatrix(in_N),
!	此处不分配内存，格式生成时分配内存，防止重复分配
	select case(in_scheme)

	case(0)!中心差分
		!矩阵赋值
		call ConDiffSchemeCentralS1D&
		(out_Mat%left_A,out_Mat%left_B,out_Mat%left_C,in_F,in_D,in_N)
	case(1)!迎风格式
		!矩阵赋值
		call ConDiffSchemeUpwindS1D(out_Mat%left_A,out_Mat%left_B,out_Mat%left_C,in_F,in_D,in_N)
	case(2)!混合格式
		!矩阵赋值
		call ConDiffSchemeHybridS1D(out_Mat%left_A,out_Mat%left_B,out_Mat%left_C,in_F,in_D,in_N)
	case(3)!指数格式
		!矩阵赋值
		call ConDiffSchemeExpS1D(out_Mat%left_A,out_Mat%left_B,out_Mat%left_C,in_F,in_D,in_N)
	case(4)!乘方格式
		!矩阵赋值
		call ConDiffSchemePowerLawS1D(out_Mat%left_A,out_Mat%left_B,out_Mat%left_C,in_F,in_D,in_N)
	case default!未知格式，退出
	print*,"The Scheme Parameter Selected Is Not Supported,Quiting !"
		stop
	end case
	!矩阵变形
	out_Mat%left_A=-out_Mat%left_A
	out_Mat%left_C=-out_Mat%left_C
	!生成右侧向量
	call ConDiffGenRightB1DFirstNS(out_Mat,phi_left,phi_right,out_right,in_N)
end subroutine
!不定F，D
!TODO:暂不实现
subroutine ConDiffGenEquation1DSUSPEFirst&
		(in_scheme,in_boundary,in_F,in_D,out_Mat,out_right)
	integer,intent(in)::in_scheme
	type(FirstBoundary1DSteady),intent(in)::in_boundary!is it right?
	real,dimension(:),intent(in)::in_F,in_D
	type(TriDiagMatrix),intent(out)::out_Mat
	real,dimension(:),allocatable,intent(out)::out_right
 end subroutine
!二阶精度
!			5--------二阶迎风格式
!			6--------QUICK
!			>7-------暂时未实现
!定F，D
subroutine ConDiffGenEquation1DSSPESecond(&
		in_scheme,in_boundary,in_F,in_D,in_N,out_Mat,out_right)
	integer,intent(in)::in_scheme
	type(FirstBoundary1DSteady),intent(in)::in_boundary
	real,intent(in)::in_F,in_D
	integer,intent(in)::in_N
	type(FiveDiagMatrix),intent(out)::out_Mat
	real,dimension(:),allocatable,intent(out)::out_right
	real phi_left,phi_right
	
	phi_left=getPhiLeft(in_boundary)
	phi_right=getPhiRight(in_boundary)
	!
	select case(in_scheme)
	case(5)!迎风差分
	case(6)!QUICK格式
	!ConDiffSchemeStandQUICK1DSSPE(A_WW,A_W,A_P,A_E,A_EE,F,D,N)
	call ConDiffSchemeStandQUICK1D&
	(out_Mat%left_WW,out_Mat%left_W,out_Mat%left_P,out_Mat%left_E,out_Mat%left_EE,&
	in_F,in_D,in_N)
	case default!未知格式，退出
	print*,"The Scheme Parameter Selected Is Not Supported,Quiting !"
		stop
	end case
	!变换矩阵
	out_Mat%left_WW	=- out_Mat%left_WW
	out_Mat%left_W	=- out_Mat%left_W
	out_Mat%left_E	=- out_Mat%left_E
	out_Mat%left_EE	=- out_Mat%left_EE
	call ConDiffGenRightB1DSecondNS(out_Mat,phi_left,phi_right,out_right,in_N)
end subroutine
end module
