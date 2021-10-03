library(rzmq)
library(parallel)

context <- init.context()
socket <- init.socket(context, "ZMQ_SUB")
connect.socket(socket, "tcp://localhost:12345")
subscribe(socket, '')

mcparallel(Sys.sleep(3))
Sys.sleep(1)

syncsock <- init.socket(context, "ZMQ_REP")
bind.socket(syncsock, "tcp://*:123456")
receive.socket(syncsock)
send.socket(syncsock, '')

for (i in 1:1000) {
	msg <- receive.socket(socket)
	print(msg)
}
