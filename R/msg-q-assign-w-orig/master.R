#!/usr/bin/env R

source("shared.R")
source("messages.R")
source("chunk.R")

RSC <- redis.connect(host="localhost", port=6379L)
redis.rm(RSC, c("distChunk1", as.character(1:10), "JOB_ID", "CHUNK_ID"))
distChunk1 <- structure(new.env(), class = "distChunk")
chunkID(distChunk1) <- "distChunk1"

main <- function() {
	cat("Value of distChunk1: ", format(distChunk1), "\n")
	x <- chunkDo(expm1, distChunk1)
	cat("Value of x: ", format(x), "\n")
	y <- chunkDo(log1p, x, wait=T)
	cat("Value of y: ", format(y), "\n")
}

main()
