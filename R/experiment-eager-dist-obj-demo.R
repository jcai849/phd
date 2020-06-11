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

y = x / 2
receive(y)

z1 = send(1, rsc)
z1

z2 = send(1, rsc, align_to=x)
z2

try(receive(x - z1))
receive(x - z2)

kill_servers(hosts)
