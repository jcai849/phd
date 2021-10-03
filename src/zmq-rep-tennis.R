library(rzmq)

context <- init.context()

sl <- lapply(0:9, function(i) {
		     s <- init.socket(context, "ZMQ_REP")
		     bind.socket(s, paste0("tcp://*:1234", i))
		     s
})

for (r in 1:10) {
	toread <- poll.socket(sl, lapply(sl, function(...) "read"), timeout=-1L)
	msgsfrom <- lapply(sl[unlist(toread)], receive.socket)
	lapply(msgsfrom, function(m) cat("received:\t", m, '\n'))
	msgto <- "RETURNED!"
	lapply(sl[unlist(toread)], function(s) {
		       cat("sending:\t", msgto, '\n')
		       send.socket(s, msgto)
})
}
