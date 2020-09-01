#!/usr/bin/env R

source("shared.R")
source("messages.R")
source("chunk.R")

RSC <- redis.connect(host="localhost", port=6379L)
distChunk1 <- seq(10)
QUEUE <- "distChunk1"

main <- function() {
	repeat {
		m <- read.queue(QUEUE)
		switch(op(m),
		       "ASSIGN" = {cID <- chunkDo(fun(m), chunk(m))
			           send(CHUNK_ID = cID, to = jobID(m))},
		       "DOFUN" = send(VAL = chunkDo(fun(m), chunk(m), 
						    assign=FALSE),
				      to = jobID(m)))
	}
}

chunkDo.default <- function(what, x, assign=TRUE) {
	if (assign) {
		cID <- chunkID()
		val <- chunkDo(what, x, assign=FALSE)
		cat("Assigning value", format(val), "to identifier", format(cID), "\n")
		assign(cID, val, envir = .GlobalEnv)
		assign("QUEUE", c(QUEUE, cID), envir = .GlobalEnv)
		return(cID)
	} else do.call(what, list(x))
}

main()
