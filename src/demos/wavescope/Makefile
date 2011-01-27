

getdata: 6sec_marmot_sample.raw

6sec_marmot_sample.raw:
	./download_sample_marmot_data

scheme:
	./testall_demos.ss

clean:
	rm -rf query.* query_*.* .*.o ._* __* *.o WSQuery* WSMspAdc*
	if [ -f Makefile.tos2 ]; then make -f Makefile.tos2 clean; fi
	rm -f Makefile.tos2 progtelos

