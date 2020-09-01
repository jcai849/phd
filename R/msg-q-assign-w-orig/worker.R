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
		       "ASSIGN" = {cID <- do.call.chunk(what=fun(m), 
							chunkArg=chunk(m), 
							distArgs=dist(m), 
							staticArgs=static(m), 
							assign=TRUE)
			           send(CHUNK_ID = cID, to = jobID(m))},
		       "DOFUN" = {v <- do.call.chunk(what=fun(m), 
						     chunkArg=chunk(m), 
						     distArgs=dist(m), 
						     staticArgs=static(m), 
						     assign=FALSE)
			          send(VAL = v, to = jobID(m))})
	}
}

do.call.chunk <- function(what, chunkArg, distArgs, staticArgs, assign=TRUE) {
	if (assign) {
		cID <- chunkID()
		v <- do.call(what, list(chunkArg))
		cat("Assigning value", format(v), "to identifier", 
		    format(cID), "\n")
		assign(cID, v, envir = .GlobalEnv)
		assign("QUEUE", c(QUEUE, cID), envir = .GlobalEnv)
		return(cID)
	} else do.call(what, list(chunkArg))
}

main()
