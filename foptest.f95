include"FileOperate.f95"
program test
use FileOperate
	implicit none
	character(128) filepath
	character(128) argname,argvalue,nextarg,nextvalue
	integer success
	filepath='test.conf'
	argname='author'
	nextarg="nextline"
	open(unit=8,file=filepath,status="old",action='read')
	success=getArgs(8,'!',argname,'=',argvalue)
	print*,success,'author','=',argvalue
	success=getArgs(8,'!',nextarg,'=',argvalue)
	print*,success,'nextline','=',argvalue
	close(8)
end program
