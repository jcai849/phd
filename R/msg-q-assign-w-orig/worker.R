#!/usr/bin/env R

source("shared.R")

RSC <- redis.connect(host="localhost", port=6379L)
chunk1 <- seq(10)
QUEUE <- "chunk1"

main <- function() {
	while (TRUE) {
		msg <- readMsg(QUEUE)
		cat("read message:", format(msg), "\n")
		switch(getOp(msg),
		       "ASSIGN" = {id <- assignFun(getFun(msg), getChunk(msg))
			           sendMsg(chunkID = id, to = getInfoRef(msg))},
		       "DOFUN" = sendMsg(val = doFun(getFun(msg), getChunk(msg)),
					 to = getInfoRef(msg)))
	}
}

assignFun <- function(fun, chunk) {
	cat("Assigning...\n")
	id <- getChunkID()
	cat("Got ID ", format(id), "\n")
	val <- doFun(fun, chunk)
	assign(id, val, envir = .GlobalEnv)
	assign("QUEUE", c(QUEUE, id), envir = .GlobalEnv)
	id
}

doFun <- function(fun, chunk) {
	do.call(fun, list(chunk))
}

main()
