#!/usr/bin/env R

library(rediscc)

RSC <- redis.connect(host="localhost", port=6379L)
chunk1 <- seq(10)
QUEUE <- "chunk1"

main <- function() {
	while (TRUE) {
		msg <- readMessage(QUEUE)
		cat("read message:", format(msg), "\n")
		result <- doFun(msg)
		cat("result is: ", format(result), "\n")
		reply(result, getReturnAddr(msg))
	}
}

doFun <- function(msg) {
	fun <- getFun(msg); arg <- getArg(msg)
	do.call(fun, list(arg))
}

reply <- function(result, returnAddr) {
	replySock <- NULL
	while (is.null(replySock)) 
		replySock <- tryCatch(socketConnection(getHost(returnAddr),
						       getPort(returnAddr)),
			      error = function(e) {
				      cat("Failed to connect to return address",
					  ", trying again..\n")
				      NULL})
	cat("replying to request...\n")
	serialize(result, replySock)
	cat("replied\n")
	close(replySock)
}

getFun <- function(msg) msg$fun
getArg <- function(msg) get(msg$chunk)
getReturnAddr <- function(msg) msg$returnAddr
getHost <- function(addr) addr$host
getPort <- function(addr) addr$port

readMessage <- function(queues) {
	serializedMsg <- redis.pop(RSC, queues, timeout=Inf)
	unserialize(charToRaw(serializedMsg))
}

main()
