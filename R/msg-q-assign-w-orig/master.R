#!/usr/bin/env R

source("shared.R")
source("messages.R")
source("chunk.R")

RSC <- redis.connect(host="localhost", port=6379L)
redis.rm(RSC, c("chunk1", as.character(1:10), "chunkID"))
chunk1 <- structure(new.env(), class = "chunk")
assign("ID", "chunk1", chunk1)

main <- function() {
	cat("Value of chunk1: ", format(chunk1), "\n")
	x <- chunkDo(expm1, chunk1)
	cat("Value of x: ", format(x), "\n")
	y <- chunkDo(log1p, x, wait=T)
	cat("Value of y: ", format(y), "\n")
}

main()
