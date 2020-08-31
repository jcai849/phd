# messaging functions

newMsg <- function(...) {
	structure(list(...), class = "msg")
}

sendMsg <- function(..., to) {
	items <- list(...)
	msg <- do.call(newMsg, items)
	writeMsg(msg, to)
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

chunk.msg <- function(x, ...) get(chunkID(msgField("chunk")(x)))
msgField <- function(field) function(x, ...) x[[field]]
op <- msgField("op"); fun <- msgField("fun")
val <- msgField("val"); chunkID.msg <- msgField("chunkID"); 
infoRef.msg <- msgField("infoRef")
