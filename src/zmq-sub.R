library(rzmq)

context <- init.context()
socket <- init.socket(context, "ZMQ_SUB")
connect.socket(socket, "tcp://localhost:12345")
subscribe(socket, '')
sin <- file("stdin")
while ({cat("RECEIVE?\n"); readChar(sin, 1); T}) {
	cat("Message: ", receive.socket(socket), "\n")
}
