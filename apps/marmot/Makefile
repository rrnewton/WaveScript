


all:
	bunzip2 -f -k testdata.txt.bz2
	@echo
	@if [ -f "6sec_marmot_sample.raw" ]; then echo "Sample Marmot data in place."; else ./download_small_sample_data; fi
	@echo
	@echo "Need to run script update_locs_script.ss to generate nodelocs.ws!"
	@echo "Alternatively, link it to example_nodelocs.ws."

both: server client

server:
	wsmlton-x86ensbox run_ensbox_server_2and3.ws
	cp query.ensbox.mlton.exe NEWSERVER.exe

client:
	wsmlton-ensbox run_ensbox_node.ws
	cp query.ensbox.mlton.exe NEWNODE.exe


clean:
	rm -f query.*.c .tmp*.o *.pipe query.sml foo.out gmon.out pdump*
	rm -f *~ _* *.so \#* *.o
	rm loc.txt
