#!/usr/bin/env R

library(rediscc)
library(uuid)

RSC <- redis.connect(host="localhost", port=6379L)
redis.rm(RSC, "chunkID")
chunk1 <- structure("chunk1", class = "chunk")
redis.rm(RSC, "chunk1")
redis.rm(RSC, as.character(1:10))

main <- function() {
	cat("Value of chunk1: ", format(chunk1), "\n")
	cat("Assigning exp(chunk1) - 1 as x, not waiting for response")
	x = assignFunAt(fun=expm1, chunk=chunk1, wait=F)
	cat("Value of x: ", format(x), "\n")
	cat("Assigning log(1 + x) as y, waiting for response")
	y = assignFunAt(fun=log1p, chunk=x, wait=T)
	cat("Value of y: ", format(y), "\n")
}

assignFunAt <- function(fun, chunk, wait=TRUE) {
	id <- getChunkID()
	cat("ID is: ", format(id), "\n")
	returnAddr <- UUIDgenerate()
	sendMsg("ASSIGN", fun, chunk, returnAddr, id, ack = wait)
	if (wait) readReply(returnAddr)
	structure(id, class = "chunk")
}

doFunAt <- function(fun, chunk) {
	returnAddr <- UUIDgenerate()
	cat("Request to perform function ", format(fun), 
	    " on chunk ", chunk, "\n")
	sendMsg("DOFUN", fun, chunk, returnAddr)
	readReply(returnAddr)
}

getChunkID <- function() as.character(redis.inc(RSC, "chunkID"))

readReply <- function(addr, clear=TRUE) {
	cat("Awaiting response...\n")
	reply <- redis.pop(RSC, addr, timeout = Inf)
	cat("Received response\n")
	if (clear) redis.rm(RSC, addr)
	unserialize(charToRaw(reply))
}

sendMsg <- function(op, fun, chunk, returnAddr, id=NULL, ack=NULL) {
	msg <- newMsg(op, fun, chunk, id, 
		      ack, returnAddr)
	writeMsg(msg, chunk)
}

newMsg <- function(op, fun, chunk, id, ack, returnAddr) {
	structure(list(op = op, fun = fun, chunk = chunk, 
		       id = id, ack = ack, returnAddr = returnAddr),
		  class = "msg")
}

writeMsg <- function(msg, to) {
	serializedMsg <- rawToChar(serialize(msg, NULL, T))
	redis.push(RSC, to, serializedMsg)
	cat("wrote message: ", format(msg), 
	    " to queue belonging to chunk \"", to, "\"\n")
}

format.chunk <- function(x, ...) {
	obj <- doFunAt(identity, x)
	format(obj)
}

main()
