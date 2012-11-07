module BoundaryDefine

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

subroutine getDensity(this)
	type(BoundaryFirst1DSteady),intent(in)::this
	real,intent(out)::getDensity
	getDensity=this%Density
end subroutine

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
