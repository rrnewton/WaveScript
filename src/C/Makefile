


DIR=`echo "(machine-type)" | chez -q`
BUILD=../build/$(DIR)/
MACHINETYPE=`uname -s`-`uname -m | sed 's/ /_/'`
#TARGET=../build/$(DIR)/

all: usleep_default fftw

fftw: fftw.c
	echo Building Fftw...
	$(RM) -rf $(DIR)
	mkdir $(DIR)
	gcc -fPIC -shared -o $(DIR)/fftw.o fftw.c -I$(HOME)/bin/$(MACHINETYPE)/csv7.0a/boot/$(DIR) -lfftw3
	(cd $(DIR); echo '(compile-file "../fftw.ss" "fftw.so")' | chez)
	if ! [ -d $(BUILD) ]; then mkdir $(BUILD); fi
	cp $(DIR)/fftw.so $(BUILD)
	cp $(DIR)/fftw.o $(BUILD)

usleep_default: usleep_libc

## I'm just lifting it from libc, this is easier:
usleep_libc: usleep_libc.ss
	echo Building USLEEP_LIBC:
	$(RM) -rf $(DIR)
	mkdir $(DIR)
	(cd $(DIR); echo '(compile-file "../usleep_libc.ss" "usleep_libc.so")' | chez)
	if ! [ -d $(BUILD) ]; then mkdir $(BUILD); fi
	cp $(DIR)/usleep_libc.so $(BUILD)

usleep_wrapped: usleep.c
	echo Building USLEEP:
	$(RM) -rf $(DIR)
	mkdir $(DIR)
	gcc -fPIC -shared -o $(DIR)/usleep.o usleep.c 
	(cd $(DIR); echo '(compile-file "../usleep.ss" "usleep.so")' | chez)
	if ! [ -d $(BUILD) ]; then mkdir $(BUILD); fi
	cp $(DIR)/usleep.so $(BUILD)


clean:
	$(RM) -rf *~ *.so *.o *.boot 
	$(RM) -rf i3nt i3le ppcosx
	$(RM) -rf $(BUILD)/fftw.so $(BUILD)/usleep_libc.so $(BUILD)/usleep.so
