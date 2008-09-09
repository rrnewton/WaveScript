include "unix.ws"
include "stdlib.ws"

fopen  :: (String, String) -> FileDescr = foreign("fopen",  ["stdio.h"]);
fclose :: FileDescr -> () = foreign("fclose", ["stdio.h"]);
fwrite :: (Pointer "void*", Int, Int, FileDescr) -> Int = foreign("fwrite", ["stdio.h"])
fwrite_str :: (String, Int, Int, FileDescr) -> Int = foreign("fwrite", ["stdio.h"])
fwrite_arr :: (Array Int, Int, Int, FileDescr) -> Int = foreign("fwrite", ["stdio.h"])


fun write_file(data){
	f=fopen("image.bmp","w");

/*	BMFormat = "BM";
    BMPHeaderLenght=3*640*480;
    ReservedBit = 0;
    DataOffset = 54;
   
    DataHeaderLenght = 40;
    Planes = 1;
    BPP = 24;
   
    Compression = 0;
   
    ImageSize = 0;
   
    ResX = 0;
    Resy = 0;
   
    ColorUsed =0;
    ColorImportant = 0;

	//fwrite_arr(bmp_header,1,1,f);*/
	b=fclose(f);
}

//main = iterate _ in timer(3.0){
//	write_file();
//	emit();
//}


