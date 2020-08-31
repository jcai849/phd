#!/usr/bin/env R

source("shared.R")
source("messages.R")
source("chunk.R")

RSC <- redis.connect(host="localhost", port=6379L)
chunk1 <- seq(10)
QUEUE <- "chunk1"

main <- function() {
	while (TRUE) {
		msg <- readMsg(QUEUE)
		switch(op(msg),
		       "ASSIGN" = {id <- chunkDo(fun(msg), chunk(msg))
			           sendMsg(chunkID = id, to = infoRef(msg))},
		       "DOFUN" = sendMsg(val = chunkDo(fun(msg), chunk(msg), 
						       assign=FALSE),
					 to = infoRef(msg)))
	}
}

chunkDo.default <- function(what, x, assign=TRUE) {
	if (assign) {
		id <- chunkID()
		val <- chunkDo(what, x, assign=FALSE)
		cat("Assigning value", format(val), "to identifier", format(id), "\n")
		assign(id, val, envir = .GlobalEnv)
		assign("QUEUE", c(QUEUE, id), envir = .GlobalEnv)
		return(id)
	} else do.call(what, list(x))
}

main()
