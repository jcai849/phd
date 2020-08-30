#!/usr/bin/env R

source("shared.R")

RSC <- redis.connect(host="localhost", port=6379L)
redis.rm(RSC, c("chunk1", as.character(1:10), "chunkID"))
chunk1 <- structure(new.env(), class = "chunk")
assign("chunkID", "chunk1", chunk1)

main <- function() {
	cat("Value of chunk1: ", format(chunk1), "\n")
	cat("Assigning exp(chunk1) - 1 as x, not waiting for response\n")
	x = assignFunAt(fun=expm1, chunk=chunk1, wait=F)
	cat("Value of x: ", format(x), "\n")
	cat("Assigning log(1 + x) as y, waiting for response\n")
	y = assignFunAt(fun=log1p, chunk=x, wait=T)
	cat("Value of y: ", format(y), "\n")
}

assignFunAt <- function(fun, chunk, wait=TRUE) {
	infoRef <- UUIDgenerate()
	sendMsg(op = "ASSIGN", fun = fun, chunk = chunk, 
		infoRef = infoRef, to = getChunkID(chunk))
	if (!wait) newChunk(infoRef)
	cat("Not waiting\n")
	newChunk(id = getChunkID(readMsg(infoRef, clear=TRUE)))
}

doFunAt <- function(fun, chunk) {
	infoRef <- UUIDgenerate()
	cat("Request to perform function ", format(fun), 
	    " on chunk ", getChunkID(chunk), "\n")
	sendMsg(op = "DOFUN", fun = fun, chunk = chunk, 
		infoRef = infoRef, to = getChunkID(chunk))
	getVal(readMsg(infoRef, clear=TRUE))
}

newChunk <- function(infoRef, id) {
	chunk <- new.env()
	if (!missing(id)) 
		assign("chunkID", id, chunk) else assign("infoRef", infoRef, chunk)
	class(chunk) <- "chunk"
	chunk
}

format.chunk <- function(x, ...) {
	obj <- doFunAt(identity, x)
	format(obj)
}

main()
