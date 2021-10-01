library(rzmq)

context <- init.context()
socket <- init.socket(context, "ZMQ_PUB")
bind.socket(socket, "tcp://*:12345")
sin <- file("stdin")
while ({cat("SEND?\n"); readChar(sin, 1); T}) {
	send.socket(socket, "Hello from PUB")
}
