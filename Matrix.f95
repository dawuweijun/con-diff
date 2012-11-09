module Matrix
implicit none
!删除三对角矩阵，统一使用五对角矩阵存储
type DiagMatrix
	integer MatType
	integer MatSize
!定义关于零的flag变量
!-1------------------未分配
!0-------------------没有零
!1-------------------部分为零
!2-------------------全为零
!	integer::Zero_Flag_left_WW
!	integer::Zero_Flag_left_W
!	integer::Zero_Flag_left_P
!	integer::Zero_Flag_left_E
!	integer::Zero_Flag_left_EE
	real,dimension(:),allocatable::left_WW
	real,dimension(:),allocatable::left_W
	real,dimension(:),allocatable::left_P
	real,dimension(:),allocatable::left_E
	real,dimension(:),allocatable::left_EE
end type
contains
!******************************************************************************
function BuildDiagMatrix(mat_type,mat_size)
	type(DiagMatrix)::BuildDiagMatrix
	integer,intent(in)::mat_type,mat_size
end function
!******************************************************************************
function BuildTriDiagMatrix(in_N)
type(DiagMatrix)::BuildTriDiagMatrix
integer,intent(in)::in_N
allocate(BuildTriDiagMatrix%left_W(in_N),&
			BuildTriDiagMatrix%left_P(in_N),&
			BuildTriDiagMatrix%left_E(in_N))
			BuildTriDiagMatrix%MatType=3
			BuildTriDiagMatrix%MatSize=in_N
			BuildTriDiagMatrix%left_W=0.0
			BuildTriDiagMatrix%left_P=0.0
			BuildTriDiagMatrix%left_E=0.0
!			BuildTriDiagMatrix%Zero_Flag_left_WW=-1
!			BuildTriDiagMatrix%Zero_Flag_left_W=2
!			BuildTriDiagMatrix%Zero_Flag_left_P=2
!			BuildTriDiagMatrix%Zero_Flag_left_E=2
!			BuildTriDiagMatrix%Zero_Flag_left_EE=-1
end function
!******************************************************************************
function BuildFiveDiagMatrix(in_N)
type(DiagMatrix)::BuildFiveDiagMatrix
integer::in_N
BuildFiveDiagMatrix%MatType=5
BuildFiveDiagMatrix%MatSize=in_N
allocate(BuildFiveDiagMatrix%left_WW(in_N),&
			BuildFiveDiagMatrix%left_W(in_N),&
			BuildFiveDiagMatrix%left_P(in_N),&
			BuildFiveDiagMatrix%left_E(in_N),&
			BuildFiveDiagMatrix%left_EE(in_N))
			BuildFiveDiagMatrix%MatType=5
			BuildFiveDiagMatrix%MatSize=in_N
			BuildFiveDiagMatrix%left_WW=0.0
			BuildFiveDiagMatrix%left_W=0.0
			BuildFiveDiagMatrix%left_P=0.0
			BuildFiveDiagMatrix%left_E=0.0
			BuildFiveDiagMatrix%left_EE=0.0
!			BuildFiveDiagMatrix%Zero_Flag_left_WW=2
!			BuildFiveDiagMatrix%Zero_Flag_left_W=2
!			BuildFiveDiagMatrix%Zero_Flag_left_P=2
!			BuildFiveDiagMatrix%Zero_Flag_left_E=2
!			BuildFiveDiagMatrix%Zero_Flag_left_EE=2
end function
!******************************************************************************
function IsMatReasonable(in_mat)
	type(DiagMatrix)::in_mat
	logical::IsMatReasonable
	IsMatReasonable=.false.
	select case(in_mat%MatType)
	case(3)
		IsMatReasonable=isArrySizeEqual2_3&
		(in_mat%MatSize,in_mat%left_W,in_mat%left_P,in_mat%left_E)
	case(5)
		IsMatReasonable=isArrySizeEqual2_5&
		(in_mat%MatSize,in_mat%left_WW,in_mat%left_W,&
		in_mat%left_P,in_mat%left_E,in_mat%left_EE)
	end select
end function
!******************************************************************************
function getMatSize(in_mat)
	integer::getMatSize
	type(DiagMatrix)::in_mat
	if(IsMatReasonable(in_mat)) then
		getMatSize=in_mat%MatSize
	else
		getMatSize=0
	end if
end function

function isArrySizeEqual2_2(length,arry_1,arry_2)
	logical isArrySizeEqual2_2
	integer,intent(in)::length
	real,dimension(:),allocatable,intent(in)::arry_1,arry_2
	if(size(arry_1)==length.and.size(arry_2)==length)then
		isArrySizeEqual2_2=.true.
	else
		isArrySizeEqual2_2=.false.
	end if
end function
function isArrySizeEqual2_3(length,arry_1,arry_2,arry_3)
	logical isArrySizeEqual2_3
	integer,intent(in)::length
	real,dimension(:),allocatable,intent(in)::arry_1,arry_2,arry_3
	if(size(arry_1)==length.and.&
		size(arry_2)==length.and.&
		size(arry_3)==length)then
		isArrySizeEqual2_3=.true.
	else
		isArrySizeEqual2_3=.false.
	end if
end function
function isArrySizeEqual2_5(length,arry_1,arry_2,arry_3,arry_4,arry_5)
	logical isArrySizeEqual2_5
	integer,intent(in)::length
	real,dimension(:),allocatable,intent(in)::&
	arry_1,arry_2,arry_3,arry_4,arry_5
	if(size(arry_1)==length.and.&
		size(arry_2)==length.and.&
		size(arry_3)==length.and.&
		size(arry_4)==length.and.&
		size(arry_5)==length) then
		isArrySizeEqual2_5=.true.
	else
		isArrySizeEqual2_5=.false.
	end if
end function
!subroutine computeZeroFlag(this)
!type(DiagMatrix)::this
!	if(this%MatType==5)then
!		this%Zero_Flag_left_WW=getArrayZeroFlag(this%left_WW)
!		this%Zero_Flag_left_EE=getArrayZeroFlag(this%left_EE)
!	end if
!	this%Zero_Flag_left_W=getArrayZeroFlag(this%left_W)
!	this%Zero_Flag_left_P=getArrayZeroFlag(this%left_P)
!	this%Zero_Flag_left_E=getArrayZeroFlag(this%left_E)
!
!end subroutine
!function getArrayZeroFlag(array)
!	integer::getArrayZeroFlag,I,zeroconter
!	real,dimension(:),allocatable,intent(in)::array
!	zeroconter=0
!	if(allocated(array))then
!		do I=1,size(array)
!			if(array(I)==0.0)then
!				zeroconter=zeroconter+1
!			end if
!		end do
!		if(zeroconter==0)then
!			getArrayZeroFlag=0
!		elseif (zeroconter/=size(array))then
!			getArrayZeroFlag=1
!		else
!			getArrayZeroFlag=2
!		end if
!	else
!		getArrayZeroFlag=2
!	end if
!end function
!subroutine printZeroFlag(this)
!	type(DiagMatrix)::this
!	print*,"Flag For left_WW : " ,this%Zero_Flag_left_WW
!	print*,"Flag For left_W  : " ,this%Zero_Flag_left_W
!	print*,"Flag For left_P  : " ,this%Zero_Flag_left_P
!	print*,"Flag For left_E  : " ,this%Zero_Flag_left_E
!	print*,"Flag For left_EE : " ,this%Zero_Flag_left_EE
!end subroutine
end module
