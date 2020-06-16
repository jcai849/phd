# Cluster creation

hosts <- paste0("hadoop", 1:8)
rsc <- make_cluster(hosts)
peek(rsc)

# Distributed Vector Coercion

v1 <- as.distributedr(1:150, rsc)
v2 <- as.distributed(151:300, rsc)
v3 <- as.distributed(1:150 %% 2 == 0, rsc)
v4 <- send(1, rsc, align_to=v1)
v5 <- send(1, rsc, align_to=v3)
v1
receive(v1)
receive(v3)

# Distributed Vector Operations

o1 <- v1 / v2
o2 <- v1 - v2
o3 <- v1 + 1
o4 <- v2 + v4
o5 <- v3 | TRUE
o1
receive(o1)

# Distributed Vector Indexing

i1 <- v1[1]
i2 <- v1[30:50]
i3 <- v1[v3]
##i4 <- v1[1:150 %% 2 == 0]
i3
receive(i3)
receive(i2)

peek(i1$host)

# Distributed Data Frame Coercion
## TODO: creation of reader directly from remotes

d1 <- as.distributed(iris, rsc)

# Distributed Data Frame Indexing

d1[,]
d1[,3]
d1[,3:5]
d1[,"Sepal.Length"]
#d1[,as.distributed.vector("Sepal.Length", rsc)]
#d1[,as.distributed.vector(c(T,F,T,F,F), rsc)]

d1[3, ]
d1[3, 3]
d1[3, 3:5]
d1[3, "Sepal.Length"]
#d1[3, as.distributed.vector("Sepal.Length", rsc)]
#d1[3, as.distributed.vector(c(T,F,T,F,F), rsc)]

d1[3:80, ]
d1[3:80, 3]
d1[3:80, 3:5]
d1[3:80, "Sepal.Length"]
#d1[3:80, as.distributed.vector("Sepal.Length", rsc)]
#d1[3:80, as.distributed.vector(c(T,F,T,F,F), rsc)]

#d1[v1, ]
#d1[v1, 3]
#d1[v1, 3:5]
#d1[v1, "Sepal.Length"]
#d1[v1, as.distributed.vector("Sepal.Length", rsc)]
#d1[v1, as.distributed.vector(c(T,F,T,F,F), rsc)]

d1[v3, ]
d1[v3, 3]
d1[v3, 3:5]
d1[v3, "Sepal.Length"]
#d1[v3, as.distributed.vector("Sepal.Length", rsc)]
#d1[v3, as.distributed.vector(c(T,F,T,F,F), rsc)]

## d1[1:150 %% 2 == 0, ]
## d1[1:150 %% 2 == 0, 3]
## d1[1:150 %% 2 == 0, 3:5]
## d1[1:150 %% 2 == 0, "Sepal.Length"]
## d1[1:150 %% 2 == 0, as.distributed.vector("Sepal.Length", rsc)]
## d1[1:150 %% 2 == 0, as.distributed.vector(c(T,F,T,F,F), rsc)]

# Close

kill_servers(hosts)
