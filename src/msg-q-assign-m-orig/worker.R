#!/usr/bin/env R

library(rediscc)

RSC <- redis.connect(host="localhost", port=6379L)
chunk1 <- seq(10)
QUEUE <- "chunk1"

main <- function() {
	while (TRUE) {
		msg <- readMessage(QUEUE)
		cat("read message:", format(msg), "\n")
		switch(getOp(msg),
		       "ASSIGN" = {assignFun(getFun(msg), getChunk(msg),
					     getChunkID(msg))
				   if (getAck(msg)) 
				     writeMsg("Complete", getReturnAddr(msg))},
		       "DOFUN" = writeMsg(doFun(getFun(msg), getChunk(msg)),
					  getReturnAddr(msg)))
	}
}

assignFun <- function(fun, chunk, id) {
	val <- doFun(fun, chunk)
	assign(id, val, envir = .GlobalEnv)
	assign("QUEUE", c(QUEUE, id), envir = .GlobalEnv)
}

doFun <- function(fun, chunk) {
	do.call(fun, list(chunk))
}

getMsgField <- function(field) function(msg) msg[[field]]
getOp <- getMsgField("op"); getFun <- getMsgField("fun")
getChunk <- function(msg) get(getMsgField("chunk")(msg))
getChunkID <- getMsgField("id"); getAck <- getMsgField("ack")
getReturnAddr <- getMsgField("returnAddr")

readMessage <- function(queues) {
	serializedMsg <- redis.pop(RSC, queues, timeout=Inf)
	unserialize(charToRaw(serializedMsg))
}

writeMsg <- function(msg, to) {
	serializedMsg <- rawToChar(serialize(msg, NULL, T))
	redis.push(RSC, to, serializedMsg)
	cat("wrote message: ", format(msg), 
	    " to queue belonging to chunk \"", to, "\"\n")
}

main()
