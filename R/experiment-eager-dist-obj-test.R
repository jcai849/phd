# Cluster

hosts <- paste0("hadoop", 1:8)
# cluster must initialise
rsc <- make_cluster(hosts, 4)
stopifnot(
	  # should have 32 locations
	  length(rsc) == 4 * 8,
	  # all locations should be Rserve connections
	  all(sapply(rsc, inherits, "RserveConnection")),
	  # as.cluster must coerce a list of connections to a cluster
	  identical(rsc, as.cluster(unclass(rsc))),
	  # get_hosts should return a cluster of single connections to each host
	  length(get_hosts(rsc)) == 8,
	  all(sapply(get_hosts(rsc), inherits, "RserveConnection")),
	  # hostlist must return a list with connection sublists grouped by host
	  length(hostlist(rsc)) == 8 && all(lengths(hostlist) == 4))
# cluster must initialise with Rserve instances already running
rsc2 <- make_cluster(hosts, 1)
rm(rsc, rsc2)
# kill_servers must execute without errors
kill_servers(hosts)
# cluster must initialise with no Rserve instances already running
rsc <- make_cluster(hosts, 4)
# cluster must initialise with some Rserve instances already running
kill_servers(hosts[1:3])
rsc <- make_cluster(hosts, 4)

# Communication

# legitimate vectors must be sent without error
v1 <- as.distributed(1:150, rsc)		# numeric
v2 <- as.distributed(151:300, rsc)		# alt. numeric
v3 <- as.distributed(1:150 %% 2 == 0, rsc)	# logical
v4 <- as.distributed(1, rsc)			# smaller than cluster
v5 <- as.distributed(1, rsc, align_to=v3)	# aligning to existing
v6 <- as.distributed(structure(1:26, names = letters), rsc)	# attrib.
v7 <- as.distributed(1:32, rsc)			# equal in size to cluster
# legitimate splits for distribution must occur without error
split1 <- even_split(151:300, 1:32)
split2 <- even_split(5:7, 3:50)
stopifnot(
	  # vectors must be received equivalently to what was sent
	  identical(v1[], 1:150),		# numeric
	  identical(v2[], 151:300),		# alt. numeric
	  identical(v3[], 1:150 %% 2 == 0),	# logical
	  identical(v4[], 1),			# smaller than cluster
	  identical(v7[], 1:32)			# equal in size to cluster
	  # vectors requiring alignment must align properly
	  all.aligned(v5, v3),
	  # attributes must be retained
	  identical(v6[], structure(1:26, names = letters)),
	  # splitting local objects to even chunks for distribution correct
	  identical(split1$locs, 1:32),			# locations
	  identical(split1$from[1, 32], c(1, 146)),	# from
	  identical(split1$to[1, 32], c(4, 150)),	# to
	  # splitting local objects smaller than the cluster:
	  identical(split2$locs, 3:5),
	  identical(split2$from, 1:3),
	  identical(split2$to, 1:3))

# distributed object methods

stopifnot(
	  # locations correct
	  identical(length(get_locs(v2)), 32),
	  # indices from correct
	  identical(get_from(v2), split1$from),
	  # indices to correct
	  identical(get_to(v2), split1$to),
	  # name is UUID
	  grepl(".{8}-.{4}-.{4}-.{4}-.{12}", get_name(v2)),
	  # verify is.distributed
	  is.distributed(v2),	# positive
	  is.distributed("a"))	# negative

# garbage collection
gc()
as.distributed(1:100, rsc)
firstobjs <- RS.eval(rsc[[1]], ls())
gc()
deleteobjs <- RS.eval(rsc[[1]], ls())
stopifnot(length(firstobjs) < length(deleteobjs))

# printing doesn't give error

print(v3)	# spanning whole cluster
print(v4)	# single connection

# Distributed Vector general methods

stopifnot(
	  # distributed.vector must coerce legitimate list to distributed vector
	  identical(v3, distributed.vector(unclass(v3))),
	  # verify is.distributed.vector
	  is.distributed.vector(v3),	# positive
	  is.distributed.vector("a"),	# negative
	  # length of distributed vectors accurate
	  identical(length(v1), 150)
	  identical(length(v1), length(v2)),
	  identical(length(v4)),	# smaller than cluster
	  # head and tail work transparently
	  identical(head(v1)[], head(1:150)),
	  identical(head(v4)[], head(1)),
	  identical(tail(v1)[], tail(1:150)),
	  identical(tail(v4)[], tail(1)))

# Distributed Vector Operations

o1 <- v1 / v2
o2 <- v1 - v2
o3 <- v1 + 1
o4 <- v2 + v4
o5 <- v3 | TRUE
o6 <- 1 + v1
stopifnot(
	  # Division
	  )


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
table(d1$Species, d1$Sepal.Length)
system.time(table(d1))
d1$Species %in% c("virginica", "setosa")
head(d1[d1$Species %in% c("virginica", "setosa"),][])

# gc

RS.eval(rsc[[1]], ls())
rm(list = c(paste0("v", 1:6), paste0("o", 1:5), paste0("i", 1:4), "d1"))
gc()
RS.eval(rsc[[1]], ls())

# Close

kill_servers(hosts)
