library(rediscc)
library(uuid)

# Generics and setters

chunk <- function(x, ...) {
	if (missing(x)) {
		c <- new.env()
		class(c) <- "chunk"
		return(c)
	}
	UseMethod("chunk", x)
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

chunk.infoRef <- function(x, ...) {
	c <- chunk()
	infoRef(c) <- x
	c
}

# chunkID methods

chunk.chunkID <- function(x, ...) {
	c <- chunk()
	chunkID(c) <- x
	c
}
