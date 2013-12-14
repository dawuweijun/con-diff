!------------------------------------------------------------------------------
! 	AUTHOR 		:Black
! 	DATE		:2012年10月30日
! 	email		:1507912984@qq.com
! 	LICENSE		:You can do whatever you want.
!	声明：	
!		1,作者第一次使用Fortran写程序，对Fortran的多种特性把握不到位，以至
!	于本程序的架构比较糟糕，希望后来者加以改进。
!		2,另外，本作者的程序编写风格比较蹩脚，希望后来者切勿学习。
!		3,本人是一个GNU开源计划的实际受益者和忠诚的支持者，但是该程序只是本作者
!	课堂作业，并没有对本程序的架构和风格进行详细的推敲。本人认为：如果是在师弟师妹
!	面前献丑还值得，但是传至网络空间，就太丢人了。
!		4,开源代码中有很多优秀的计算流体力学程序可供各位后来者研究，各位后来者
!	请勿在此停留太多。
!	改进提示：	
!		1,程序中多处动态分配的内存都未进行销毁，此处千万要改正。
!		2,尽管此程序可以进行扩展的空间很大，但几乎没有是扩展的实际价值，仅供
!	初学者学习之用。
!		3,本程序中可供参考的内容比较少，本人推荐各位后来者详细查看六种格式的边
!	界处理，其他各处是否详细了解请自斟酌。
program condiff
	use ConDiffGloble
	use ConDiffGlobleDefine
	use Matrix
	use ConDiffGenFD
	use ConDiffSchemes1D
	use ConDiffGenRightB
	use ConDiffGenPos
	use ConDiffOutPut
	use GridLoad
	use DMESolve
implicit none
!------------------------------------------------------------------------------
!	定义全局变量
!------------------------------------------------------------------------------
	integer error				!用于错误返回
	real,dimension(:),allocatable::AnsForPrint
	integer i,success
	character(128)::errorInfo,outputfilename
!******************************************************************************
!Read parameters from the command line
!******************************************************************************
	argc=iargc() !
	if(argc/=1)then
		print*,"You should input the parameter file path ."
		print*,"Like This:"
		print*,"Main test.conf"
		stop	!退出
	end if
	call getarg(1,args)
!文件是否存在
if (IsFileExist(args).neqv..true.) then
	print*,"The Configure file you selected dosen't exist:Quiting"
	stop
end if
!文件存在，调用解析函数读入

!******************************************************************************
!调用输入解析模块，解析输入文件
!******************************************************************************
	call InitParameters(myGloblePara,args)
	success=readBoundary(myBoundary,myGloblePara%BoundaryFilePath,errorInfo)
	if(success/=0)then
		print *,"Errors For Load Boundary File :"
		print *,errorInfo
	endif
	success=readGrid(myGrid,myGloblePara%GridInputFilePath,errorInfo)
	if(success/=0)then
		print *,"Errors For Load Boundary File :"
		print *,errorInfo
	endif	
	myFDPairs=GenFDPairs(myBoundary,myGrid)
!******************************************************************************
call ConDiffScheme1D(myMat,myGloblePara%Scheme,myFDPairs)
!******************************************************************************
call ConDiffGenRightB1D(myBoundary,myGloblePara%Scheme,myMat,myRight)
!******************************************************************************
	myAns=DMEResolve(myMat,myRight)
!	打开输出文件


outputfilename=myGloblePara%OutPutFilePath(1:len_trim(myGloblePara%OutPutFilePath))//&
	"_Scheme_"//myGloblePara%Scheme(1:len_trim(myGloblePara%Scheme))//&
	".dat"
	open(20,file=outputfilename,action="write")
!注意，ans长度和位置列表的长度并不一致，这是由于边界界面是已知的
!为了保证输出时不因节点数的不同而存在太大差异的，应该将这两个界面也要加进去
	allocate(AnsForPrint(size(myAns)+2))

	AnsForPrint(1)=getPhiLeft(myBoundary)
	AnsForPrint(size(myAns)+2)=getPhiRight(myBoundary)
	do i=2,size(myAns)+1
		AnsForPrint(i)=myAns(i-1)
	end do

	call OutPut(20,myGloblePara%namePhi,myGloblePara%nameX,AnsForPrint,&
	GenPosVec(myGrid),myGloblePara%OutPutFileType,error)
	close(20)

	if(error/=0)then
		print*,"error ,while write ans to the file named ",outputfilename
	else
		print*,"Success ,see the ouput file : ",outputfilename
	endif

end program condiff
