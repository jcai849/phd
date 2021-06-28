library(rediscc)

REDIS_HOST <- "localhost"
REDIS_PORT <- 6379L
QUEUE <- "chunk"
MSG <- quote({ chunk + 1})

main <- function() {
	rsc <- redis.connect(REDIS_HOST, REDIS_PORT)
	writeMsg(MSG, to=QUEUE, conn=rsc)
}

writeMsg <- function(msg, to, conn) {
	serializedMsg <- rawToChar(serialize(msg, NULL, T))
	redis.push(conn, to, serializedMsg)
}

main()
