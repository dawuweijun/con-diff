module FileOperate

contains
!转换为大写字符串
function ToUperCase(string)
	character(128)::string,ToUperCase
	integer::I
	ToUperCase=string
	do i=1,len(ToUperCase)
		if(ToUperCase(i:i)>='a'.and.ToUperCase(i:i)<='z') then
		ToUperCase(i:i)=ACHAR(IACHAR(ToUperCase(i:i))-32)
		end if
	end do
end function
!移除特定字符
function ReamoveAllChar(string,chartoremove)
	character(128)::string,ReamoveAllChar
	character::chartoremove
	integer::i
	ReamoveAllChar=""
	do i=1,len(string)
		if(string(i:i)/=chartoremove)&
		ReamoveAllChar=ReamoveAllChar//string(i:i)!链接字符串
	end do
end function
!分割字符串
subroutine SplitString(in_string,splitlabel,out_string_1,out_string_2)
	character(128)::in_string,out_string_1,out_string_2
	character::splitlabel
	integer::i
	out_string_1=""
	out_string_2=""
	do i=1,len(in_string)
		if(in_string(i:i)==splitlabel)exit
		out_string_1=out_string_2//in_string(i:i)!链接字符串
	end do
	out_string_2=out_string_1(len(out_string_1)+2:len(in_string))
end subroutine

function getArgs(filepath,notelabel,argname,splitlabel,out_arg)
	integer::getArgs!
	character(128)::filepath,notelabel,argname,splitlabel,out_arg
	character(128)::templine,tempname,tempvalue
	integer::status
	getArgs=-1
!首先，打开文件，
	open(UNIT=8,FILE=filepath,STATUS="old",action="read",&
	iostat=status)
   	do
   		read(3,*,iostat=status) templine
		if(status/=0) exit
   			if(templine(1:1)/=notelabel) then!非注释行
!移除所有空格
			templine=ReamoveAllChar(templine,' ')
!使用splitlabel分割字符串,获得变量名称和变量值字符串
			call SplitString(templine,splitlabel,tempname,tempvalue)
!获得大写变量名称
			tempname=ToUperCase(tempname)
			if (tempname==ToUperCase(argname))then!相等，取值，退出
				out_arg=tempvalue
				getArgs=0
				exit
			end if
		end if
   	end do
end function
end module
