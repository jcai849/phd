library(rzmq)

context <- init.context()

sl <- lapply(0:9, function(i) {
		     s <- init.socket(context, "ZMQ_REQ")
		     connect.socket(s, paste0("tcp://localhost:1234", i))
		     s
})

for (r in 1:10) {
	i <- sample((0:9)+1, 1)
	msgto <- "SERVE!"
	cat("sending:\t", msgto, '\n')
	send.socket(sl[[i]], msgto)
	msgfrom <- receive.socket(sl[[i]])
	cat("received:\t", msgfrom, '\n')
}
