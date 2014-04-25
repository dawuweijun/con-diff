set(SRCS
	condiff_oneDimension.f95
	Matrix.f95
	ConDiffGenFD.f95
	ConDiffGenRightB.f95
	ConDiffGenPos.f95
	ConDiffGloble.f95
	ConDiffGlobleDefine.f95
	ConDiffSchemes1D.f95
	ConDiffOutPut.f95
	FileOperate.f95
	BoundaryDefine.f95
	BoundaryLoad.f95
	GridDefine.f95
	GridLoad.f95
	DiagMatEquationSolve.f95
)

add_executable(main ${SRCS})
