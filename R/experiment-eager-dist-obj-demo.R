hosts <- paste0("hadoop", 1:8)

rsc <- make_cluster(hosts)
x = send(1:100, rsc)

peek(rsc[1])
peek(rsc)

a = send(0:100, rsc)
b = send(400:300, rsc)
c = a - b
c.local = receive(a) - receive(b)
all.equal(receive(c), c.local)

a. = 0:100
c. = a - b
all.equal(receive(c.), c.local)

b. = 400:300
c.. = a - b.
all.equal(receive(c..), c.local)

y = send(seq(10), rsc)
z = send(3, rsc)

id <- getUUID()
k = eval(bquote(RS.eval(rsc[[1]], receive(.(a), remote = TRUE))))

kill_servers(hosts)
