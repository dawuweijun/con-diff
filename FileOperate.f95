module FileOperate
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
	character(128)::templine,tempname,tempvalue,templine_2
	integer::status
	getArgs=-1
   	do
   		read(FILEUNIT,'(A)',iostat=status) templine
		if(status/=0) exit
   			if(templine(1:1)/=notelabel) then!非注释行
!移除所有空格
			templine_2=ReamoveAllChar(templine,' ')
!使用splitlabel分割字符串,获得变量名称和变量值字符串
			call SplitString(templine,splitlabel,tempname,tempvalue)
!获得大写变量名称
			tempname=ToUperCase(tempname)
			if (tempname==ToUperCase(argname))then!相等，取值，退出
				inout_arg=trim(tempvalue)
				getArgs=0
				exit
			end if
		end if
   	end do
end function
end module
