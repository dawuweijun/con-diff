#******************************************************************************
# AUTHOR 	:Black
# DATE		:2012年10月30日
# email		:1507912984@qq.com
# LICENSE	:You can do whatever you want.
#******************************************************************************
#		This file achieved the autobuild of this project, you may change the 
#	compiler whatever you like. If you don't have a make tool just like GNU make,
#	this file is useless for you.
#		However, this file have some problems when make, what you should do is
#	just try more times. 
compiler=gfortran
debugflag= -g -O3
#debugflag= -O3
sources = condiff_oneDimension.f95 \
		Matrix.f95 \
		ConDiffGenFD.f95 \
		ConDiffGenRightB.f95 \
		ConDiffGenPos.f95 \
		ConDiffGloble.f95 \
		ConDiffGlobleDefine.f95 \
		ConDiffSchemes1D.f95 \
		ConDiffOutPut.f95 \
		FileOperate.f95 \
		BoundaryDefine.f95 \
		BoundaryLoad.f95 \
		GridDefine.f95 \
		GridLoad.f95 \
		DiagMatEquationSolve.f95

exeoutput= main
all:condiff_1d
clean:
	rm -f ./*~  $(exeoutput)
condiff_1d:
	$(compiler) $(debugflag) -o $(exeoutput) $(sources)
strip:	all
	strip $(exeoutput)
cleanall:clean
	rm -f ./*.mod
	
