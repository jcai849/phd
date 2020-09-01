# messaging functions

msg <- function(...) {
	structure(list(...), class = "msg")
}

send <- function(..., to) {
	items <- list(...)
	m <- do.call(msg, items)
	write.msg(m, to)
}

write.msg <- function(m, to) {
	serializedMsg <- rawToChar(serialize(m, NULL, T))
	redis.push(RSC, to, serializedMsg)
	cat("wrote message: ", format(m), 
	    " to queue belonging to chunk \"", to, "\"\n")
}

read.queue <- function(queue, clear = FALSE) {
	cat("Awaiting message...\n")
	serializedMsg <- redis.pop(RSC, queue, timeout=Inf)
	if (clear) redis.rm(RSC, queue)
	m <- unserialize(charToRaw(serializedMsg))
	cat("Received message: ", format(m), "\n")
	m
}

# message field accessors

chunk.msg <- function(x, ...) get(chunkID(msgField("chunk")(x)))
msgField <- function(field) function(x, ...) x[[field]]
op <- msgField("op"); fun <- msgField("fun")
val <- msgField("val"); chunkID.msg <- msgField("chunkID"); 
infoRef.msg <- msgField("infoRef")
