

CHEZ = `if (which chez > /dev/null); then echo chez; else echo ../depends/chez; fi`

DIR=`echo "(machine-type)" | chez -q`
BUILD=../build/$(DIR)/
MACHINETYPE=`uname -s`-`uname -m | sed 's/ /_/'`
#TARGET=../build/$(DIR)/

all: fftw.so usleep_default
	echo BUILT for $(DIR)

# This is VERY specific to my setup.
fftw.so: fftw.c fftw.ss
	@echo; echo "Building FFTW..."
	@echo "------------------------------------------------------------"
#	gcc -fPIC -shared -o fftw.o fftw.c -I$(HOME)/bin/$(MACHINETYPE)/csv7.0a/boot/$(DIR) -lfftw3
#	gcc -fPIC -shared -o fftw.o fftw.c -I$(HOME)/bin/$(MACHINETYPE)/csv7.3/boot/$(DIR) -lfftw3
#
# [2007.07.06] This isn't currently working under Mac OS:
	if [ `uname` == "Darwin" ]; \
	then  gcc -fPIC -dynamiclib -o fftw.o fftw.c -I$(REGIMENTD)/depends/petite_chez_scheme7.3/boot/$(DIR) -lfftw3; \
        else  gcc -fPIC -shared -o fftw.o fftw.c -I$(REGIMENTD)/depends/petite_chez_scheme7.3/boot/$(DIR) -lfftw3; \
	fi;
	(echo '(compile-file "fftw.ss" "fftw.so")' | $(CHEZ))
	if ! [ -d $(BUILD) ]; then mkdir $(BUILD); fi
	cp fftw.so $(BUILD)
	cp fftw.o $(BUILD)

usleep_default: usleep_libc.so

## I'm just lifting it from libc, this is easier:
usleep_libc.so: usleep_libc.ss
	@echo;
	@echo Building USLEEP_LIBC:
	@echo "------------------------------------------------------------"
	(echo '(compile-file "usleep_libc.ss" "usleep_libc.so")' | $(CHEZ))
	if ! [ -d $(BUILD) ]; then mkdir $(BUILD); fi
	cp usleep_libc.so $(BUILD)

usleep_wrapped: usleep.c
	echo Building USLEEP:
	$(RM) -rf $(DIR)
	mkdir $(DIR)
	gcc -fPIC -shared -o $(DIR)/usleep.o usleep.c 
	(cd $(DIR); echo '(compile-file "../usleep.ss" "usleep.so")' | $(CHEZ))
	if ! [ -d $(BUILD) ]; then mkdir $(BUILD); fi
	cp $(DIR)/usleep.so $(BUILD)


################ WAS DOING THIS WITH A SEPARATE BUILD DIRECTORY.  NOT ANYMORE:

# all: usleep_default fftw

# fftw: fftw.c
# 	@echo "Building FFTW..."
# 	@echo;echo "------------------------------------------------------------"
# 	$(RM) -rf $(DIR)
# 	mkdir $(DIR)
# 	gcc -fPIC -shared -o $(DIR)/fftw.o fftw.c -I$(HOME)/bin/$(MACHINETYPE)/csv7.0a/boot/$(DIR) -lfftw3
# 	(cd $(DIR); echo '(compile-file "../fftw.ss" "fftw.so")' | $(CHEZ))
# 	if ! [ -d $(BUILD) ]; then mkdir $(BUILD); fi
# 	cp $(DIR)/fftw.so $(BUILD)
# 	cp $(DIR)/fftw.o $(BUILD)

# usleep_default: usleep_libc

# ## I'm just lifting it from libc, this is easier:
# usleep_libc: usleep_libc.ss
# 	@echo Building USLEEP_LIBC:
# 	@echo;echo "------------------------------------------------------------"
# 	$(RM) -rf $(DIR)
# 	mkdir $(DIR)
# 	(cd $(DIR); echo '(compile-file "../usleep_libc.ss" "usleep_libc.so")' | $(CHEZ))
# 	if ! [ -d $(BUILD) ]; then mkdir $(BUILD); fi
# 	cp $(DIR)/usleep_libc.so $(BUILD)

# usleep_wrapped: usleep.c
# 	echo Building USLEEP:
# 	$(RM) -rf $(DIR)
# 	mkdir $(DIR)
# 	gcc -fPIC -shared -o $(DIR)/usleep.o usleep.c 
# 	(cd $(DIR); echo '(compile-file "../usleep.ss" "usleep.so")' | $(CHEZ))
# 	if ! [ -d $(BUILD) ]; then mkdir $(BUILD); fi
# 	cp $(DIR)/usleep.so $(BUILD)


clean:
	$(RM) -rf *~ *.so *.o *.boot 
	$(RM) -rf i3nt i3le ppcosx
	$(RM) -rf $(BUILD)/fftw.so $(BUILD)/usleep_libc.so $(BUILD)/usleep.so
