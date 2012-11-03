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
		if (((size(in_mat%left_W)).eq.(size(in_mat%left_P))).and.&
			((size(in_mat%left_W)).eq.(size(in_mat%left_E))))then
			IsMatReasonable=.true.
		end if
	case(5)
		if (size(in_mat%left_WW).eq.size(in_mat%left_W).AND.&
		size(in_mat%left_WW).eq.size(in_mat%left_P).AND.&
		size(in_mat%left_WW).eq.size(in_mat%left_E).AND.&
		size(in_mat%left_WW).eq.size(in_mat%left_EE))then
		IsMatReasonable=.true.
		end if
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
end module
