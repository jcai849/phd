library(rediscc)

REDIS_HOST <- "LOCALHOST"
REDIS_PORT <- 6379L
QUEUE <- "chunk"
chunk <- seq(10)

main <- function() {
	rsc <- redis.connect(REDIS_HOST, REDIS_PORT)
	while (TRUE) {
		msg <- readMessage(QUEUE, conn=rsc)
		print(eval(msg))
	}
}

readMessage <- function(queues, conn) {
	serializedMsg <- redis.pop(conn, queues, timeout=Inf)
	unserialize(charToRaw(serializedMsg))
}

main()
