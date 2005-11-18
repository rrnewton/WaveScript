


DIR=`echo "(machine-type)" | chez -q`

## I'm just lifting it from libc, this is easier:
usleep_libc: usleep_libc.ss
	echo Building USLEEP_LIBC:
	$(RM) -rf $(DIR)
	mkdir $(DIR)
	(cd $(DIR); echo '(compile-file "../usleep_libc.ss" "usleep_libc.so")' | chez)
	if ! [ -d ../../build/$(DIR)/ ]; then mkdir ../../build/$(DIR)/; fi
	cp $(DIR)/usleep_libc.so ../../build/$(DIR)/

usleep: usleep.c
	echo Building USLEEP:
	$(RM) -rf $(DIR)
	mkdir $(DIR)
	gcc -fPIC -shared -o $(DIR)/usleep.o usleep.c 
	(cd $(DIR); echo '(compile-file "../usleep.ss" "usleep.so")' | chez)
	if ! [ -d ../../build/$(DIR)/ ]; then mkdir ../../build/$(DIR)/; fi
	cp $(DIR)/usleep.so ../../build/$(DIR)/

clean:
	$(RM) -rf *~ *.so *.o *.boot i3nt i3le ppcosx
