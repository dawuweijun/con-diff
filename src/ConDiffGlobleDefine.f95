module ConDiffGlobleDefine
	use ConDiffGloble
	use Matrix
	use ConDiffGenFD
	use ConDiffSchemes1D
	use ConDiffGenRightB
	use ConDiffGenPos
	use ConDiffOutPut
implicit none
	integer::argc		!the number of parameters from command line
	character(128)::args	!for saving the parameter string
	type (GlobleParameters)::myGloblePara
	!integer::myScheme
	type (BoundaryFirst1DSteady)::myBoundary
	real,dimension(:),allocatable::myRight
	real,dimension(:),allocatable::myAns
	type(FDPairs)::myFDPairs
	type (Simple1DGrid)::myGrid
	type(DiagMatrix)::myMat
end module
