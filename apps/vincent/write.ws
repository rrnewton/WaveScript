include "unix.ws"
include "stdlib.ws"

fopen  :: (String, String) -> FileDescr = foreign("fopen",  ["stdio.h"]);
fclose :: FileDescr -> () = foreign("fclose", ["stdio.h"]);
fwrite :: (Pointer "void*", Int, Int, FileDescr) -> Int = foreign("fwrite", ["stdio.h"])
fwrite_str :: (String, Int, Int, FileDescr) -> Int = foreign("fwrite", ["stdio.h"])
fwrite_arr :: (Array Int, Int, Int, FileDescr) -> Int = foreign("fwrite", ["stdio.h"])
write_header :: FileDescr -> () = foreign("write_header", ["save_data.c"])
set_file_name :: (Int, Int) -> String = foreign("set_file_name", ["save_data.c"])


fun save_data(data){
	
	data_list=Array:toList(ptrToArray(data,30));
	i=0;
	number_ = [];
	while not(List:ref(data_list,i)=='#') {						
					val = List:ref(data_list,i);
					number_ := val:::number_;
					i:=i+1;
				};
	i:=i+1;
	number_:=List:reverse(number_);
	numberstr=String:implode(number_);
	number=stringToInt(numberstr);	
	//print("list: "++number++"\n");
	sec_ = [];
	while not(List:ref(data_list,i)=='#') {						
					val = List:ref(data_list,i);
					sec_ := val:::sec_;
					i:=i+1;
				};
	i:=i+1;
	sec_:=List:reverse(sec_);
	secstr=String:implode(sec_);
	sec=stringToInt(secstr);	
	//print("list: "++sec++"\n");
	palette_ = [];
	while not(List:ref(data_list,i)=='#') {						
					val = List:ref(data_list,i);
					palette_ := val:::palette_;
					i:=i+1;
				};
	palette_:=List:reverse(palette_);
	palettestr=String:implode(palette_);
	palette=stringToInt(palettestr);	
	//print("list: "++palette++"\n");
	file_name=set_file_name(number,sec);
	print("File: "++file_name++"\n");
	f=fopen(file_name, "w");
	write_header(f);
	fwrite(data,1,3*640*480,f);
	fclose(f);
}


