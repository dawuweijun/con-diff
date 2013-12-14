module FileOperate
!******************************************************************************
! AUTHOR 	:Black
! DATE		:2012年10月30日
! email		:1507912984@qq.com
! LICENSE	:You can do whatever you want.
!******************************************************************************
!	This file achieved some basic function for dealing parameters file. You
!may reused them in your codes once you are going to read some parameters in
!a bit complex format. For more details about the parameters file format, go
!to the file named test.conf.
implicit none
  contains
!转换为大写字符串
function ToUperCase(string)
	character(128)::string,ToUperCase
	integer::I
	ToUperCase=string
	do i=1,len_trim(ToUperCase)
		if(ToUperCase(i:i)>='a'.and.ToUperCase(i:i)<='z') then
		ToUperCase(i:i)=ACHAR(IACHAR(ToUperCase(i:i))-32)
		end if
	end do
end function
!移除特定字符
function ReamoveAllChar(string,chartoremove)
	character(128)::ReamoveAllChar
	character(128),intent(in)::string
	character,intent(in)::chartoremove
	integer::i
	ReamoveAllChar=""
	!print*,len_trim(string),string,all right
	do i=1,len_trim(string)
		if(string(i:i)/=chartoremove) then
		!连接字符串，注意错误
		ReamoveAllChar=ReamoveAllChar(1:len_trim(ReamoveAllChar))//string(i:i)
		end if
	end do
end function
!分割字符串
subroutine SplitString(in_string,splitlabel,out_string_forward,out_string_back)
	character(128),intent(in)::in_string
	character(128),intent(inout)::out_string_forward,out_string_back
	character,intent(in)::splitlabel
	integer::i
	do i=1,len_trim(in_string)
		if(in_string(i:i)==splitlabel)then
			out_string_forward=in_string(1:i-1)!复制前一部分字符
			exit
		end if
	end do
	out_string_back=in_string(len_trim(out_string_forward)+2:len_trim(in_string))
	out_string_forward=trim(out_string_forward)
	out_string_back=trim(out_string_back)
end subroutine

function getArgs(FILEUNIT,notelabel,argname,splitlabel,inout_arg)
	integer::getArgs!
	integer,intent(in)::FILEUNIT
	character,intent(in)::notelabel,splitlabel
	character(128),intent(in)::argname
	character(128),intent(inout)::inout_arg
	character(128)::templine,tempname,tempvalue
	integer::status
	getArgs=-1
	rewind(FILEUNIT)!将文件重定位到文件开始
   	do
   		read(FILEUNIT,'(A)',iostat=status) templine
		if(status/=0) exit
   			if(templine(1:1)/=notelabel) then!非注释行
!移除所有空格
			templine=ReamoveAllChar(templine,' ')
!			print*,templine
!移除tab键
			templine=ReamoveAllChar(templine,'	')
!			print*,templine
!使用splitlabel分割字符串,获得变量名称和变量值字符串
			call SplitString(templine,splitlabel,tempname,tempvalue)
!获得大写变量名称
			tempname=ToUperCase(tempname)
			!相等，值不为空，取值，退出
			if (tempname==ToUperCase(argname))then
				if (len_trim(tempvalue)/=0)then
					inout_arg=trim(tempvalue)
					getArgs=0
					exit
				else
					print*,"The Value Of ",argname,"Is Null"
				end if
			end if
		end if
   	end do
end function
!文件是否存在
function IsFileExist(filepath)
	logical::IsFileExist
	character(128),intent(in)::filepath
	integer::ierror,fileptr
	fileptr=1024
	open(unit=fileptr,file=filepath,status='old',action='read',iostat=ierror)
	if(ierror/=0) then
		IsFileExist=.false.
	else
		IsFileExist=.true.
	end if
	close(unit=fileptr)
end function
end module
