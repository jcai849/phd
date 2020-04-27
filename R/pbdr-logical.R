suppressMessages(library(pbdDMAT))
init.grid()

x <- ddmatrix(TRUE)
y <- ddmatrix(FALSE)

print(x)
print(y)
print(x & y)
print(x | y)

run <- 0
while(as.matrix(x)){
	print("looping")
	run <- run + 1
	if (run > 2) break
}

finalize()
