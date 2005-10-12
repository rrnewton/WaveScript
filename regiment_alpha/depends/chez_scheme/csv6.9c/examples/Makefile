# Unix make file to compile the examples.
# Compilation is not necessary since the examples may be loaded from
# source, but this gives an example of how to use make for Scheme.
# * To compile files not already compiled, type "make".  Only those
#   files in the object list below and not yet compiled will be compiled.
# * To compile all files, type "make all".  Only those files in the object
#   list below will be compiled.
# * To compile one file, say "fumble.ss", type "make fumble.so".  The
#   file need not be in the object list below.
# * To remove the object files, type "make clean".
# * To print the examples, type "make print".

src = def.ss edit.ss fact.ss fatfib.ss fft.so fib.ss freq.ss interpret.ss\
 macro.ss matrix.ss object.ss power.ss queue.ss rabbit.ss scons.ss setof.ss\
 socket.ss unify.ss

obj = def.so edit.so fact.so fatfib.so fft.so fib.so freq.so interpret.so\
 macro.so matrix.ss object.so power.so queue.so rabbit.so scons.so setof.so\
 socket.so unify.so

Scheme = scheme

.SUFFIXES:
.SUFFIXES: .ss .so
.ss.so: ; echo '(time (compile-file "$*"))' | ${Scheme}

needed:	${obj}

all: ; echo "(time (for-each compile-file (map symbol->string '(${src}))))" | ${Scheme}

clean: ; /bin/rm -f *.so

print: ; pr -f Makefile ${src} | lpr -JExamples
