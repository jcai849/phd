library(rediscc)
library(uuid)

# Attain Chunk ID

getChunkID <- function(x, ...) {
	if (missing(x)) return(as.character(redis.inc(RSC, "chunkID")))
	UseMethod("getChunkID", x)
}

getChunkID.chunk <- function(x, ...) {
	if (! exists("chunkID", x)) { 
		infoRef <- get("infoRef", x)
		cat("chunkID not yet associated with chunk; checking infoRef ", infoRef, "\n")
		id <- redis.pop(RSC, infoRef, timeout = Inf)
		cat("chunkID \"", id, "\" found; associating...\n")
		assign("chunkID", id, x)
		redis.rm(infoRef)
	}
	get("chunkID", x)
}

# messaging functions

sendMsg <- function(...) {
	items <- list(...)
	msg <- do.call(newMsg, items)
	writeMsg(msg, items$to)
}

newMsg <- function(...) {
	structure(list(...), class = "msg")
}

writeMsg <- function(msg, to) {
	serializedMsg <- rawToChar(serialize(msg, NULL, T))
	redis.push(RSC, to, serializedMsg)
	cat("wrote message: ", format(msg), 
	    " to queue belonging to chunk \"", to, "\"\n")
}

readMsg <- function(queues, clear = FALSE) {
	cat("Awaiting message...\n")
	serializedMsg <- redis.pop(RSC, queues, timeout=Inf)
	if (clear) redis.rm(RSC, queues)
	msg <- unserialize(charToRaw(serializedMsg))
	cat("Received message: ", format(msg), "\n")
	msg
}

# message field accessors

getMsgField <- function(field) function(x, ...) x[[field]]
getOp <- getMsgField("op"); getFun <- getMsgField("fun")
getAck <- getMsgField("ack"); getVal <- getMsgField("val")
getChunkID.msg <- getMsgField("chunkID"); getInfoRef <- getMsgField("infoRef")
getChunk <- function(x, ...) get(getChunkID(getMsgField("chunk")(x)))

