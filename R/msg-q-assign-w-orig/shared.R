library(rediscc)
library(uuid)

# Generics and setters

chunk <- function(x, ...) UseMethod("chunk", x)

distChunk <- function(x, ...) {
	if (missing(x)) {
		c <- new.env()
		class(c) <- "distChunk"
		return(c)
	}
	UseMethod("distChunk", x)
}

chunkDo <- function(what, x, wait=FALSE, assign=TRUE) 
	UseMethod("chunkDo", x)

chunkID <- function(x, ...) {
	if (missing(x)) {
		ID <- as.character(redis.inc(RSC, "chunkID"))
		class(ID) <- "chunkID"
		return(ID)
	}
	UseMethod("chunkID", x)
}

`chunkID<-` <- function(x, value) {
	assign("ID", value, x)
	x
}

infoRef <- function(x, ...) {
	if (missing(x)) {
		ref <- UUIDgenerate()
		class(ref) <- "infoRef"
		return(ref)
	}
	UseMethod("infoRef", x)
}

`infoRef<-` <- function(x, value) {
	assign("ref", value, x)
	x
}

# infoRef methods

distChunk.infoRef <- function(x, ...) {
	c <- distChunk()
	infoRef(c) <- x
	c
}

# chunkID methods

distChunk.chunkID <- function(x, ...) {
	c <- distChunk()
	chunkID(c) <- x
	c
}
