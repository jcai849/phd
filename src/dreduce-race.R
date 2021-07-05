library(largeScaleR)
start(workers=1)

chunks <- distribute(1:2, 2)
dreduce("sum", chunks)
