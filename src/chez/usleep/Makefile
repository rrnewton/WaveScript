



DIR=`echo "(machine-type)" | chez -q`

usleep: usleep.c
	echo Building USLEEP:
	$(RM) -rf $(DIR)
	mkdir $(DIR)
	gcc -fPIC -shared -o $(DIR)/usleep.o usleep.c 
	(cd $(DIR); echo '(compile-file "../usleep.ss" "usleep.so")' | chez)

clean:
	$(RM) -rf *~ *.so *.o *.boot i3nt i3le ppcosx
