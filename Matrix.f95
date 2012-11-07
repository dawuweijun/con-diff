module Matrix
implicit none
!删除三对角矩阵，统一使用五对角矩阵存储
type DiagMatrix
	integer MatType
	integer MatSize
	real,dimension(:),pointer::left_WW
	real,dimension(:),pointer::left_W
	real,dimension(:),pointer::left_P
	real,dimension(:),pointer::left_E
	real,dimension(:),pointer::left_EE
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
function isArrySizeEqual23(length,arry_1,arry_2,arry_3)
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
end module
