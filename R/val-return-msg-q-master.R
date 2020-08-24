#!/usr/bin/env R

library(rediscc)

RSC <- redis.connect(host="localhost", port=6379L)
SELF_ADDR <- list(host="localhost", port=12345L)
chunk <- structure("chunk1", class = "chunk")

main <- function() {
	doFunAt(fun=exp, chunk=chunk)
}

doFunAt <- function(fun, chunk, conn) {
	msg <- bquote(list(fun=.(fun),
			   chunk=.(chunk),
			   returnAddr=.(SELF_ADDR)))
	writeMsg(msg, chunk)
	cat("wrote message: ", format(msg), " to ", chunk, "\n")
	listenReply()
}

listenReply <- function() {
	replySock <- socketConnection(getHost(), getPort(), server=TRUE)
	response <- character(0)
	while (length(response) < 1) {
		response <- tryCatch(unserialize(replySock), 
				     error = function(e) {
		cat("no reply, trying again in 1 sec\n")
		Sys.sleep(1); NULL})
	}
	cat("received response: ", format(response), "\n")
	close(replySock)
	response
}

getHost <- function() SELF_ADDR$host
getPort <- function() SELF_ADDR$port

writeMsg <- function(msg, to) {
	serializedMsg <- rawToChar(serialize(msg, NULL, T))
	redis.push(RSC, to, serializedMsg)
}

main()
