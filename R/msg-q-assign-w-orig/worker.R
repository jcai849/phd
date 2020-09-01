#!/usr/bin/env R

source("shared.R")
source("messages.R")
source("chunk.R")

RSC <- redis.connect(host="localhost", port=6379L)
distChunk1 <- seq(10)
QUEUE <- "distChunk1"

main <- function() {
	while (TRUE) {
		m <- read.queue(QUEUE)
		switch(op(m),
		       "ASSIGN" = {id <- chunkDo(fun(m), chunk(m))
			           send(chunkID = id, to = infoRef(m))},
		       "DOFUN" = send(val = chunkDo(fun(m), chunk(m), 
						       assign=FALSE),
					 to = infoRef(m)))
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
