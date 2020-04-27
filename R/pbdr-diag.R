suppressMessages(library(pbdDMAT))

init.grid()

x <- ddmatrix(1:4, ncol=2)
y <- diag(as.vector(x))
print(as.matrix(y))

finalize()
