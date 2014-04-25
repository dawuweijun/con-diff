module Matrix
!******************************************************************************
! AUTHOR 	:Black
! DATE		:2012年10月30日
! email		:1507912984@qq.com
! LICENSE	:You can do whatever you want.
!******************************************************************************
!	This file defined an unified form of five diagmatrix and tridiagmatrix.
!More details for solving the diagmatrix equation in the file named
!DiagMatEqutionSolve.f.
implicit none
!删除三对角矩阵，统一使用五对角矩阵存储
type DiagMatrix
	integer MatType
	integer MatSize
	real,dimension(:),allocatable::left_WW
	real,dimension(:),allocatable::left_W
	real,dimension(:),allocatable::left_P
	real,dimension(:),allocatable::left_E
	real,dimension(:),allocatable::left_EE
end type

contains
!******************************************************************************
!创建一个三对角矩阵
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
end function
!******************************************************************************
!创建一个五对角矩阵
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
end function
!******************************************************************************
!判断矩阵是否合理
!******************************************************************************
function IsMatReasonable(in_mat)
	type(DiagMatrix)::in_mat
	logical::IsMatReasonable
	IsMatReasonable=.false.
	select case(in_mat%MatType)
	case(3)
		IsMatReasonable=isArraySizeEqual2_3&
		(in_mat%MatSize,in_mat%left_W,in_mat%left_P,in_mat%left_E)
	case(5)
		IsMatReasonable=isArraySizeEqual2_5&
		(in_mat%MatSize,in_mat%left_WW,in_mat%left_W,&
		in_mat%left_P,in_mat%left_E,in_mat%left_EE)
	end select
end function
!******************************************************************************
!获取矩阵的大小
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
!******************************************************************************
!判断两个数组是否相等
!******************************************************************************
function isArraySizeEqual2_2(length,Array_1,Array_2)
	logical isArraySizeEqual2_2
	integer,intent(in)::length
	real,dimension(:),allocatable,intent(in)::Array_1,Array_2
	if(size(Array_1)==length.and.size(Array_2)==length)then
		isArraySizeEqual2_2=.true.
	else
		isArraySizeEqual2_2=.false.
	end if
end function
!******************************************************************************
!判断三个数组是否相等
!******************************************************************************
function isArraySizeEqual2_3(length,Array_1,Array_2,Array_3)
	logical isArraySizeEqual2_3
	integer,intent(in)::length
	real,dimension(:),intent(in)::Array_1,Array_2,Array_3
	if(size(Array_1)==length.and.&
		size(Array_2)==length.and.&
		size(Array_3)==length)then
		isArraySizeEqual2_3=.true.
	else
		isArraySizeEqual2_3=.false.
	end if
end function
!******************************************************************************
!判断五个数组是否相等
!******************************************************************************
function isArraySizeEqual2_5(length,Array_1,Array_2,Array_3,Array_4,Array_5)
	logical isArraySizeEqual2_5
	integer,intent(in)::length
	real,dimension(:),intent(in)::&
	Array_1,Array_2,Array_3,Array_4,Array_5
	if(size(Array_1)==length.and.&
		size(Array_2)==length.and.&
		size(Array_3)==length.and.&
		size(Array_4)==length.and.&
		size(Array_5)==length) then
		isArraySizeEqual2_5=.true.
	else
		isArraySizeEqual2_5=.false.
	end if
end function
end module
