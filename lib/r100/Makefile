

all:
	rm -f query.exe; 
	@echo PHONE is $(PHONE)
	(if wsc2 -gcc=arm-linux-gcc r100.ws && \
	    scp -P $(PHONEPORT) query.exe $(PHONE):; \
         then ssh -p $(PHONEPORT) $(PHONE) './query.exe -n 30'; \
              fi)
#              ssh $PHONE killall query.exe; fi)
