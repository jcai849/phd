# Cluster creation

hosts <- paste0("hadoop", 1:8)
kill_servers(hosts)
rsc <- make_cluster(hosts)
RS.eval(rsc[[1]], ls())

# Distributed Vector Coercion

(v1 <- as.distributed(1:150, rsc))
(v2 <- as.distributed(151:300, rsc))
(v3 <- as.distributed(1:150 %% 2 == 0, rsc))
(v4 <- as.distributed(1, rsc, align_to=v1))
(v5 <- as.distributed(1, rsc, align_to=v3))
(v6 <- as.distributed(structure(1:26, names = letters), rsc))
v6[]
RS.eval(rsc[[1]], ls())

# Distributed Vector Operations

(o1 <- v1 / v2)
(o2 <- v1 - v2)
(o3 <- v1 + 1)
(o4 <- v2 + v4)
(o5 <- v3 | TRUE)
(1 + v1)
(o1[])

# Distributed Vector Indexing

(i1 <- v1[1])
(i2 <- v1[30:50])
(i3 <- v1[v3])
(i4 <- v1[1:150 %% 2 == 0])

# Distributed Data Frame Coercion

(d1 <- as.distributed(iris, rsc))

# Distributed Data Frame Indexing

d1[,]
d1[,3]
d1[,3:5]
d1[,"Sepal.Length"]
#d1[,as.distributed("Sepal.Length", rsc)]
#d1[,as.distributed(c(T,F,T,F,F), rsc)]

d1[3, ]
d1[3, 3]
d1[3, 3:5]
d1[3, "Sepal.Length"]
#d1[3, as.distributed("Sepal.Length", rsc)]
#d1[3, as.distributed(c(T,F,T,F,F), rsc)]

d1[3:80, ]
d1[3:80, 3]
d1[3:80, 3:5]
d1[3:80, "Sepal.Length"]
#d1[3:80, as.distributed("Sepal.Length", rsc)]
#d1[3:80, as.distributed(c(T,F,T,F,F), rsc)]

#d1[v1, ] # it works but it shouldn't!!
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

d1[1:150 %% 2 == 0, ]
d1[1:150 %% 2 == 0, 3]
d1[1:150 %% 2 == 0, 3:5]
d1[1:150 %% 2 == 0, "Sepal.Length"]
## d1[1:150 %% 2 == 0, as.distributed.vector("Sepal.Length", rsc)]
## d1[1:150 %% 2 == 0, as.distributed.vector(c(T,F,T,F,F), rsc)]

d1$Sepal.Length
d1[["Sepal.Length"]]
d1[[1]]

# dataframe utilities

dim(d1)
nrow(d1)
ncol(d1)

# list functions with dataframes

as.list(d1)
lapply(d1, receive)

# vector utilities

unique(d1$Species)
table(d1$Species)
d1$Species %in% c("virginica", "setosa")
head(d1[d1$Species %in% c("virginica", "setosa"),][])

# gc

RS.eval(rsc[[1]], ls())
rm(list = c(paste0("v", 1:6), paste0("o", 1:5), paste0("i", 1:4), "d1"))
gc()
RS.eval(rsc[[1]], ls())

# Close

kill_servers(hosts)
