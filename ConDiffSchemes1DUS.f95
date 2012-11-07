module ConDiffSchemes1D
use Matrix
use ConDiffGenFD
implicit none
!********************************************************************
!ONE DIMENSION，steady
!********************************************************************

!一阶精度,定义稳态一维一阶精度统一接口
!中心差分格式

contains
subroutine ConDiffScheme1D(out_mat,in_scheme,in_FDPairs)
	type(DiagMatrix),intent(out)::out_mat
	integer,intent(in)::in_scheme
	type(FDPairs),intent(in)::in_FDPairs
!根据in_scheme的不同选择不同算法
	if(in_scheme<0)then
		return
	end if
!分配内存在此处，下面的子程序不再分配内存
end subroutine
!********************************************************************
!中心差分 central deferential
!********************************************************************
!F，D不定，考虑边界，F，D的长度应为界面数，及点数+1
subroutine ConDiffSchemeCnetral1DSUSPe(A_W,A_P,A_E,F,D)
	real,dimension(:),allocatable,intent(inout)::A_W,A_P,A_E
	real,dimension(:),intent(in)::F,D
	integer N,I
!D_left=D(1),D_right=D(N+1)
!对于A_P(I),F_w=F(I),F_e=F(I+1),D_w=D(I),D_e=D(I+1)
	if (size(F)==size(D)) then
		N=size(F)-1
	else
		print*,"Errs in ConDiffSchemeCnetral''s paras:Quiting"
		stop
	end if
!判断内存长度
	do I=1,N
		A_W(I)=D(I)+F(I)*0.5
		A_E(I)=D(I+1)-F(I+1)*0.5
		A_P(I)=A_W(I)+A_E(I)+F(I+1)-F(I)
	end do
!注意，A_W(0)与A_E(N)不设为零，主要用来存储方程右侧系数Su
end subroutine
!********************************************************************
!一阶向前差分 upwind scheme
!********************************************************************
!不定F，不定D型
subroutine ConDiffSchemeUpwind1DSUSPe(A_W,A_P,A_E,F,D)
	real,dimension(:),allocatable,intent(out)::A_W,A_P,A_E
	real,dimension(:),intent(in)::F,D
	integer N,I
	if(size(F)==size(D))then
		N=size(F)-1
	else
		print*,"Errs in CondiffSchemeUpWind''s paras:Quiting"
		stop
	end if
!判断内存长度
!
	do I=1,N
		A_W(I)=D(I)+max(F(I),0.)
		A_E(I)=D(I+1)+max(0.,-F(I+1))
		A_P(I)=A_W(I)+A_E(I)+F(I+1)-F(I)
	end do
!注意，A_W(0)与A_E(N)不设为零，主要用来存储方程右侧系数Su
end subroutine
!********************************************************************
!混合格式 hybrid scheme
!********************************************************************
!不定F，不定D型
subroutine ConDiffSchemeHybrid1DSUSPe(A_W,A_P,A_E,F,D)
	real,dimension(:),allocatable,intent(out)::A_W,A_P,A_E
	real,dimension(:),intent(in)::F,D
	integer N,I
	if(size(F)==size(D)) then
		N=size(F)-1
	else
		print*,"Errs in CondiffSchemeUpWind''s paras:Quiting"
		stop
	end if
!判断内存长度
!
	do I=1,N
		A_W(I)=max(F(I),(D(I)+F(I)*0.5),0.)
		A_E(I)=max(-F(I+1),(D(I+1)-F(I+1)*0.5),0.)
		A_P(I)=A_W(I)+A_E(I)+F(I+1)-F(I)
	end do
!注意，A_W(0)与A_E(N)不设为零，主要用来存储方程右侧系数Su
end subroutine
!********************************************************************
!指数格式
!********************************************************************
subroutine ConDiffSchemeExp1DSUSPe(A_W,A_P,A_E,F,D)
	real,dimension(:),allocatable,intent(out)::A_W,A_P,A_E
	real,dimension(:),intent(in)::F,D
	integer N,I
	if(size(F)==size(D)) then
		N=size(F)-1
	else
		print*,"Errs in CondiffSchemeUpWind''s paras:Quiting"
		stop
	end if
!判断内存长度
!注意，A_W(0)与A_E(N)不设为零，主要用来存储方程右侧系数Su
end subroutine
!********************************************************************
!乘方格式 power-law scheme
!********************************************************************
!不定F，不定D型
subroutine ConDiffSchemePowerLaw1DSUSPe(A_W,A_P,A_E,F,D)
	real,dimension(:),allocatable,intent(out)::A_W,A_P,A_E
	real,dimension(:),intent(in)::F,D
	integer N,I
	real::temp
	real::temp_1
	real::temp_2
	if(size(F)==size(D))then
		N=size(F)-1
	else
		print*,"Errs in CondiffSchemeUpWind''s paras:Quiting"
		stop
	end if
!判断内存长度
	do I=1,N
		temp_1=1.-0.1*abs(F(I)/D(I))
		temp_2=1.-0.1*abs(F(I+1)/D(I+1))
		A_W(I)=D(I)*max(0.,temp_1*temp_1*temp_1*temp_1*temp_1)
		A_E(I)=D(I+1)*max(0.,temp_2*temp_2*temp_2*temp_2*temp_2)
	end do
	do I=1,N
	A_P(I)=A_W(I)+A_E(I)+F(I+1)-F(I)				!整体赋值
	end do
!注意，A_W(0)与A_E(N)不设为零，主要用来存储方程右侧系数Su
end subroutine
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
!	allocate(A_WW(N),A_W(N),A_P(N),A_E(N),A_EE(N))
!判断内存长度
	do I=0,N
		A_temp=max(F(I+1),0.)
		B_temp=max(-F(I),0.)
		C_temp=max(-F(I+1),0.)
		D_temp=max(F(I),0.)
		A_WW(I)=-0.125*D_temp
		A_W(I)=D(I)+0.125*A_temp-0.375*B_temp+0.75*D_temp
		A_E(I)=D(I+1)+0.125*B_temp-0.375*A_temp+0.75*C_temp
		A_EE(I)=-0.125*C_temp
		A_P(I)=A_WW(I)+A_W(I)+A_E(I)+A_EE(I)+F(I+1)-F(I)
	end do
!TODO：考虑边界处理方法
end subroutine
!********************************************************************
!one dimension ，unsteady
!********************************************************************


end module
