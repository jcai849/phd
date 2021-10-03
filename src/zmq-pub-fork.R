library(rzmq)

context <- init.context()
socket <- init.socket(context, "ZMQ_PUB")
bind.socket(socket, "tcp://*:12345")

syncsock <- init.socket(context, "ZMQ_REQ")
connect.socket(syncsock, "tcp://localhost:123456")
send.socket(syncsock, '')
receive.socket(syncsock)

for (i in 1:1000) {
	send.socket(socket, i)
}
