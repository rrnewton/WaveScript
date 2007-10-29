


all: /tmp/dummyfile.bin

# This makse a ~40MB file.
# 
# I'm sure there's a better way to make a file full of nonsense.
# Could use /dev/something or other...
/tmp/dummyfile.bin:
	rm -f /tmp/one /tmp/two
	cp just_timer.ws /tmp/one
	for i in `seq 1 12`; do cat /tmp/one >> /tmp/two; cat /tmp/two >> /tmp/one; done
	mv /tmp/one /tmp/dummyfile.bin
	rm -f /tmp/two

clean:
	rm -f *~ __* /tmp/dummyfile.bin
