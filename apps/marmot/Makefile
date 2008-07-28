


all: nodelocs.ws
	@if [ -f "6sec_marmot_sample.raw" ]; then echo "Sample Marmot data in place."; else ./download_small_sample_data; fi
	cp 6sec_marmot_sample.raw temp.raw
	@echo "Duplicate the sample data 10 times"
	for i in 1 2 3 4 5 6 7 8 9; do cat temp.raw >> 6sec_marmot_sample.raw; done
	@echo
	bunzip2 -f -k testdata.txt.bz2
	@echo
	@echo "Need to run script update_locs_script.ss to generate nodelocs.ws!"
	@echo "Alternatively, link it to example_nodelocs.ws."

nodelocs.ws:
	cp example_nodelocs.ws nodelocs.ws

both: server client

server:
	wsmlton-x86ensbox run_ensbox_server_2and3.ws
	cp query.ensbox.mlton.exe NEWSERVER.exe

client:
	wsmlton-ensbox run_ensbox_node.ws
	cp query.ensbox.mlton.exe NEWNODE.exe

upload: 100 103 104 108 109 112 113 115

100:
	scp NEWNODE.exe root@192.168.11.100:/root/detector.exe
103:
	scp NEWNODE.exe root@192.168.11.103:/root/detector.exe
104:
	scp NEWNODE.exe root@192.168.11.104:/root/detector.exe
108:
	scp NEWNODE.exe root@192.168.11.108:/root/detector.exe
109:
	scp NEWNODE.exe root@192.168.11.109:/root/detector.exe
112:
	scp NEWNODE.exe root@192.168.11.112:/root/detector.exe
113:
	scp NEWNODE.exe root@192.168.11.113:/root/detector.exe
115:
	scp NEWNODE.exe root@192.168.11.115:/root/detector.exe

103-up: 103 104 108 109 112 113 115
104-up: 104 108 109 112 113 115
108-up: 108 109 112 113 115
109-up: 109 112 113 115
112-up: 112 113 115
113-up: 113 115

clean:
	rm -f query.*.c .tmp*.o *.pipe query.sml foo.out gmon.out pdump*
	rm -f *~ _* *.so \#* *.o
	rm loc.txt
