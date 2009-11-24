


./query.exe  -n 10000 > /tmp/output 

grep max /tmp/output | cut -b 7- > /tmp/mfcc-prefilt
grep cep /tmp/output | cut -b 7- > /tmp/mfcc-cep1
grep tck /tmp/output | cut -b 7- > /tmp/mfcc-trig
grep ewm /tmp/output | cut -b 7- > /tmp/mfcc-thresh

# gnuplot command
# plot "/tmp/mfcc-prefilt" with l, "~/lgr2" using 1:($2*4000) with l, "/tmp/mfcc-cep1" using 0:($2*100) with l, "/tmp/mfcc-thresh" w l, "/tmp/mfcc-trig" cc-cep1" using 0:($*100) with l, "/tmp/mfcc-thresh" w l, "/tmp/mfcc-trig" w impu1*100) with l, "/tmp/mfcc-thresh" w l, "/tmp/mfcc-trig" w impulses 
