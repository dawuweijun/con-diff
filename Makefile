##################################################################
#author:black
#email:1507912984@qq.com
#date:2012/11/06
##################################################################
compiler=gfortran
debugflag= -g -O3
sources = condiff_oneDimension.f95 \
		Matrix.f95 \
		ConDiffGenFD.f95 \
		ConDiffGenRightB.f95 \
		ConDiffGloble.f95 \
		ConDiffSchemes1DUS.f95 \
		FileOperate.f95 \
		BoundaryDefine.f95 \
		BoundaryLoad.f95 \
		GridDefine.f95 \
		GridLoad.f95 \
		DiagMatEquationSolve.f95

exeoutput= main
all:condiff_1d
clean:
	rm -f ./*~ ./*.mod $(exeoutput)
condiff_1d:
	$(compiler) $(debugflag) -o $(exeoutput) $(sources)
	
