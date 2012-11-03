module BoundaryLoad
use BoundaryDefine
implicit none

interface LoadBonadary
module procedure ReadConDiff1DFirstSBoundary
end interface
contains
!对流与扩散一维问题一类恒定边界

subroutine ReadConDiff1DFirstSBoundary(boundary,path)
	type(BoundaryFirst1DSteady),intent(out)::boundary
	character(128),intent(in)::path
end subroutine

function getBoundaryType(filepath)
	character(128)::getBoundaryType
	character(128)::filepath
end function
end module
