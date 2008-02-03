

load "refcounts.gp"

plot 'refcounts.log' using ($4 / $3):xtic(1) title col
