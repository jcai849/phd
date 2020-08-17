library(RSclient)
library(rediscc)

REDIS_SERVER_HOST <- "hdp"
INITIATOR_HOST <- "hdp"
MSG_DETECTOR_HOST <- "hadoop1"
FORWARDER_HOST <- "hadoop2"

main.direct <- function() {
	initiatorNode <- newInitiatorNode()
	msgDetectorNode <- newMsgDetectorNode()
	ping(to=msgDetectorNode, from=initiatorNode)
	exit(msgDetectorNode)
}

newInitiatorNode <- function(initiatorHost=INITIATOR_HOST,
			 redisServerHost=REDIS_SERVER_HOST) {
	rc <- redis.connect(redisServerHost)
	redis.set(rc, "INITIATOR_HOST", initiatorHost)
	initiatorNode <- list(rc=rc, host=initiatorHost)
	class(initiatorNode) <- c("initiatorNode", "node")
	initiatorNode
}

newMsgDetectorNode <- function(msgDetectorHost=MSG_DETECTOR_HOST,
			   redisServerHost=REDIS_SERVER_HOST,
			   response="pong") {
	rsc <- RS.connect(msgDetectorHost)
	msgDetectorNodeMain <- substitute({
		library(rediscc)
		rc <- redis.connect(redisServerHost)
		initiatorHost <- redis.get(rc, "INITIATOR_HOST")
		while (TRUE) {
			redis.pop(rc, msgDetectorHost, timeout=Inf)
			redis.push(rc, initiatorHost, response)
	}},
	list(redisServerHost=redisServerHost,
	     msgDetectorHost=msgDetectorHost,
	     response=response))
	eval(bquote(RS.eval(rsc, .(msgDetectorMain), wait = FALSE)))
	msgDetectorNode <- list(rsc=rsc, host=msgDetectorHost)
	class(msgDetectorNode) <- c("msgDetectorNode", "node")
	msgDetectorNode
}

ping <- function(to, from, via, msg="ping") {
	msg <- as.character(msg)
	if (missing(via)) {
		redis.push(from$rc, to$host, msg)
		cat(sprintf("sending message \"%s\" to host \"%s\"...\n",
			    msg, to$host))
	} else {
		redis.push(from$rc, via$host, paste0("SENDTO",to$hoste, 
						     "MSG", msg))
		cat(sprintf("sending message \"%s\" to host \"%s\" via host \"%s\"...\n",
		    msg, to$host, via$host))
	}
	response <- redis.pop(from$rc, from$host, timeout=Inf)
	cat(sprintf("received message \"%s\"...\n",
		    response))
}

exit <- function(...) lapply(list(...), function(node) RS.close(node$rsc))

main.indirect <- function() {
	initiatorNode <- newInitiatorNode()
	forwarderNode <- newForwarderNode()
	msgDetectorNode <- newMsgDetectorNode()
	ping(to=msgDetectorNode, from=initiatorNode, via=forwarderNode)
	exit(msgDetectorNode, forwarderNode)
}

newForwarderNode <- function(forwarderHost=FORWARDER_HOST,
			     redisServerHost=REDIS_SERVER_HOST) {
	rsc <- RS.connect(forward)
	forwarderNodeMain <- substitute({
		library(rediscc)
		rc <- redis.connect(redisServerHost)
		while (TRUE) {
			mail <- redis.pop(rc, forwarderHost, timeout=Inf)
			m <- regmatches(mail, 
					regexec("^SENDTO(.*?)MSG(.*)", mail))
			nextHost <- m[[1]][2]; msg <- m[[1]][3]
			redis.push(rc, nextHost, msg)
	}},
	list(redisServerHost=redisServerHost,
	     forwarderHost=forwarderHost))
	eval(bquote(RS.eval(rsc, .(msgDetectorMain), wait = FALSE)))
	forwarderNode <- list(rsc=rsc, host=msgDetectorHost)
	class(forwarderNode) <- c("forwarderNode", "node")
	forwarderNode
}
