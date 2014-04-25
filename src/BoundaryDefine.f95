!******************************************************************************
! AUTHOR 	:Black
! DATE		:2012年10月30日
! email		:1507912984@qq.com
! LICENSE	:You can do whatever you want.
!******************************************************************************
!		This file defined a boundary of first type and some methods for the
!problem this project concerned. 
module BoundaryDefine
implicit none
type BoundaryFirst1DSteady
	character(128)::BoundaryFilePath
	real Phi_left
	real Phi_right
	real Velocity
	real Density
	real Gama
end type

contains
!******************************************************************************

function getDensity(this)
	type(BoundaryFirst1DSteady),intent(in)::this
	real::getDensity
	getDensity=this%Density
end function

function getPhiLeft(this)
	type(BoundaryFirst1DSteady),intent(in)::this
	real getPhiLeft
	getPhiLeft=this%Phi_left
end function

function getPhiRight(this)
	type(BoundaryFirst1DSteady),intent(in)::this
	real getPhiRight
	getPhiRight=this%Phi_right
end function

function getGama(this)
	type(BoundaryFirst1DSteady),intent(in)::this
	real getGama
	getGama=this%Gama
end function

end module
