
all:
	@echo "     Building all gradient micro-benchmarks."
#	@sleep 1
	$(MAKE) inlined 

#	@sleep 3 
	$(MAKE) linked

	$(MAKE) etx

# Uses both processors:
par:
	($(MAKE) inlined &)
	($(MAKE) linked &)
	($(MAKE) etx &)
	$(MAKE) etx_noretry

# TODO: FINISH!!!

inlined:
	@echo "     Analyzing inlined (static) gradients."
	@echo "================================================================================"
	./reception_rate_vs_link_quality_w-w-out_acks.ss data/inlined.dat inlined

linked:
	@echo "     Analyzing linked (dynamic) gradients."
	@echo "================================================================================"
	./reception_rate_vs_link_quality_w-w-out_acks.ss data/linked.dat linked


etx:
	@echo "     Analyzing ETX gradients."
	@echo "================================================================================"
	./reception_rate_vs_link_quality_w-w-out_acks.ss data/etx.dat etx 50 10

etx_noretry:
	@echo "     Analyzing ETX gradients (with no retries) ."
	@echo "================================================================================"
	./reception_rate_vs_link_quality_w-w-out_acks.ss data/etx_noretry.dat etx 0 0


clean:
	rm -rf _* *~

