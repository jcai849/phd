library(rediscc)

# Generics and setters

distChunk <- function(x, ...) {
	if (missing(x)) {
		dc <- new.env()
		class(dc) <- "distChunk"
		return(dc)
	}
	UseMethod("distChunk", x)
}

chunkID <- function(x, ...) {
	if (missing(x)) {
		cID <- as.character(redis.inc(RSC, "CHUNK_ID"))
		class(cID) <- "chunkID"
		return(cID)
	}
	UseMethod("chunkID", x)
}

`chunkID<-` <- function(x, value) {
	assign("CHUNK_ID", value, x)
	x
}

jobID <- function(x, ...) {
	if (missing(x)) {
		jID <- as.character(redis.inc(RSC, "JOB_ID"))
		class(jID) <- "jobID"
		return(jID)
	}
	UseMethod("jobID", x)
}

`jobID<-` <- function(x, value) {
	assign("JOB_ID", value, x)
	x
}

chunk <- function(x, ...) UseMethod("chunk", x)

dist <- function(x, ...) UseMethod("dist", x)

dist.default <- stats::dist

# jobID methods

distChunk.jobID <- function(x, ...) {
	dc <- distChunk()
	jobID(dc) <- x
	dc
}

# chunkID methods

distChunk.chunkID <- function(x, ...) {
	dc <- distChunk()
	chunkID(dc) <- x
	dc
}
