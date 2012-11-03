module ConDiffSchemes1D
use Matrix
use ConDiffGenFD
implicit none
!********************************************************************
!ONE DIMENSION，
!注意，F，D都是数组
!TODO:将F，D改为FDPairs
!********************************************************************
contains
!********************************************************************
!中心差分 central deferential
!********************************************************************
!定F，定D型，即F，D为定值
!F，D不定，考虑边界，F，D的长度应为界面数，及点数+1
function ConDiffSchemeCentral1D(in_FDPairs)
	type(DiagMatrix)::ConDiffSchemeCentral1D
	type(FDPairs)::in_FDPairs
	real,dimension(:),pointer::A_W,A_P,A_E
	real,dimension(:),pointer::F,D
	integer N,I
	
	F=>in_FDPairs%F
	D=>in_FDPairs%D
	A_W=>ConDiffSchemeCentral1D%left_W
	A_P=>ConDiffSchemeCentral1D%left_P
	A_P=>ConDiffSchemeCentral1D%left_E
!D_left=D(1),D_right=D(N+1)
!对于A_P(I),F_w=F(I),F_e=F(I+1),D_w=D(I),D_e=D(I+1)
	if (size(F)==size(D)) then
		N=size(F)-1
	else
		print*,"Errs in ConDiffSchemeCnetral''s paras:Quiting"
		stop
	end if
!分配内存
	allocate(A_W(1:N),A_P(1:N),A_E(1:N))
	ConDiffSchemeCentral1D%MatType=3
	ConDiffSchemeCentral1D%MatSize=N
	do I=1,N
		A_W(I)=-D(I)-F(I)*0.5
		A_E(I)=-D(I+1)+F(I+1)*0.5
		A_P(I)=-A_W(I)-A_E(I)+F(I+1)-F(I)
	end do
!注意，A_W(0)与A_E(N)不设为零，主要用来存储方程右侧系数Su
end function
!********************************************************************
!一阶向前差分 upwind scheme
!********************************************************************
!不定F，不定D型
function ConDiffSchemeUpwind1D(in_FDPairs)
	type(DiagMatrix)::CondiffSchemeUpWind1D
	type(FDPairs)::in_FDPairs
	real,dimension(:),pointer::A_W,A_P,A_E
	real,dimension(:),pointer::F,D
	integer N,I
	F=>in_FDPairs%F
	D=>in_FDPairs%D
	A_W=>ConDiffSchemeUpwind1D%left_W
	A_P=>ConDiffSchemeUpwind1D%left_P
	A_P=>ConDiffSchemeUpwind1D%left_E
	if(size(F)==size(D))then
		N=size(F)-1
	else
		print*,"Errs in CondiffSchemeUpWind''s paras:Quiting"
		stop
	end if
	allocate(A_W(1:N),A_P(1:N),A_E(1:N))
	CondiffSchemeUpWind1D%MatType=3
	ConDiffSchemeUpwind1D%MatSize=N

	do I=1,N
		A_W(I)=-D(I)-max(F(I),0.)
		A_E(I)=-D(I+1)-max(0.,-F(I+1))
		A_P(I)=-A_W(I)-A_E(I)+F(I+1)-F(I)
	end do
!注意，A_W(0)与A_E(N)不设为零，主要用来存储方程右侧系数Su
end function
!********************************************************************
!混合格式 hybrid scheme
!********************************************************************
!不定F，不定D型
function ConDiffSchemeHybrid1D(in_FDPairs)
	type(DiagMatrix)::ConDiffSchemeHybrid1D
	type(FDPairs)::in_FDPairs
	real,dimension(:),pointer::A_W,A_P,A_E
	real,dimension(:),pointer::F,D
	integer N,I
	F=>in_FDPairs%F
	D=>in_FDPairs%D
	A_W=>ConDiffSchemeHybrid1D%left_W
	A_P=>ConDiffSchemeHybrid1D%left_P
	A_P=>ConDiffSchemeHybrid1D%left_E

	if(size(F)==size(D)) then
		N=size(F)-1
	else
		print*,"Errs in CondiffSchemeUpWind''s paras:Quiting"
		stop
	end if
	allocate(A_W(1:N),A_P(1:N),A_E(1:N))
	ConDiffSchemeHybrid1D%MatType=3
	ConDiffSchemeHybrid1D%MatSize=N
!
	do I=1,N
		A_W(I)=-max(F(I),(D(I)+F(I)*0.5),0.)
		A_E(I)=-max(-F(I+1),(D(I+1)-F(I+1)*0.5),0.)
		A_P(I)=-A_W(I)-A_E(I)+F(I+1)-F(I)
	end do
!注意，A_W(0)与A_E(N)不设为零，主要用来存储方程右侧系数Su
end function
!********************************************************************
!指数格式
!********************************************************************
function ConDiffSchemeExp1D(in_FDPairs)
	type(DiagMatrix)::ConDiffSchemeExp1D
	type(FDPairs)::in_FDPairs
	real,dimension(:),pointer::A_W,A_P,A_E
	real,dimension(:),pointer::F,D
	integer N,I
	F=>in_FDPairs%F
	D=>in_FDPairs%D
	A_W=>ConDiffSchemeExp1D%left_W
	A_P=>ConDiffSchemeExp1D%left_P
	A_P=>ConDiffSchemeExp1D%left_E
	if(size(F)==size(D)) then
		N=size(F)-1
	else
		print*,"Errs in CondiffSchemeUpWind''s paras:Quiting"
		stop
	end if
	allocate(A_W(N),A_P(N),A_E(N))
	ConDiffSchemeExp1D%MatType=3
	ConDiffSchemeExp1D%MatSize=N
!注意，A_W(0)与A_E(N)不设为零，主要用来存储方程右侧系数Su
end function
!********************************************************************
!乘方格式 power-law scheme
!********************************************************************
!不定F，不定D型
function ConDiffSchemePowerLaw1D(in_FDPairs)
	type(DiagMatrix)::ConDiffSchemePowerLaw1D
	type(FDPairs)::in_FDPairs
	real,dimension(:),pointer::A_W,A_P,A_E
	real,dimension(:),pointer::F,D
	integer N,I
	real::temp
	real::temp_1
	real::temp_2
	F=>in_FDPairs%F
	D=>in_FDPairs%D
	A_W=>ConDiffSchemePowerLaw1D%left_W
	A_P=>ConDiffSchemePowerLaw1D%left_P
	A_P=>ConDiffSchemePowerLaw1D%left_E
	if(size(F)==size(D))then
		N=size(F)-1
	else
		print*,"Errs in CondiffSchemeUpWind''s paras:Quiting"
		stop
	end if
	allocate(A_W(1:N),A_P(1:N),A_E(1:N))
	ConDiffSchemePowerLaw1D%MatType=3
	ConDiffSchemePowerLaw1D%MatSize=N
	do I=1,N
		temp_1=1.-0.1*abs(F(I)/D(I))
		temp_2=1.-0.1*abs(F(I+1)/D(I+1))
		A_W(I)=-D(I)*max(0.,temp_1*temp_1*temp_1*temp_1*temp_1)
		A_E(I)=-D(I+1)*max(0.,temp_2*temp_2*temp_2*temp_2*temp_2)
		A_P(I)=-A_W(I)-A_E(I)+F(I+1)-F(I)				!整体赋值
	end do
!注意，A_W(0)与A_E(N)不设为零，主要用来存储方程右侧系数Su
end function
!********************************************************************
!二阶精度，定义稳态一维二阶精度统一接口
!********************************************************************

!********************************************************************
!二阶迎风格式
!********************************************************************

!********************************************************************
!标准 quick格式 QUICK scheme
!********************************************************************
subroutine ConDiffSchemeStandQUICK1DSUSPE(A_WW,A_W,A_P,A_E,A_EE,F,D)
	real,dimension(:),allocatable,intent(out)::A_WW,A_W,A_P,A_E,A_EE
	real,dimension(:),intent(in)::F,D
	integer N,I
	real A_temp,B_temp,C_temp,D_temp
	if(size(F)==size(D))then
		N=size(F)-1
	else
		print*,"Errs in CondiffSchemeUpWind''s paras:Quiting"
		stop
	end if
	allocate(A_WW(N),A_W(N),A_P(N),A_E(N),A_EE(N))
	do I=0,N
		A_temp=max(F(I+1),0.)
		B_temp=max(-F(I),0.)
		C_temp=max(-F(I+1),0.)
		D_temp=max(F(I),0.)
		A_WW(I)=0.125*D_temp
		A_W(I)=-D(I)-0.125*A_temp+0.375*B_temp-0.75*D_temp
		A_E(I)=-D(I+1)-0.125*B_temp+0.375*A_temp-0.75*C_temp
		A_EE(I)=0.125*C_temp
		A_P(I)=-A_WW(I)-A_W(I)-A_E(I)-A_EE(I)+F(I+1)-F(I)
	end do
!TODO：考虑边界处理方法
end subroutine
!********************************************************************
!one dimension ，unsteady
!********************************************************************


end module
