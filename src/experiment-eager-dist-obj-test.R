source("R/experiment-eager-dist-obj.R")

# Cluster

hosts <- paste0("hadoop", 1:8)
# cluster must initialise
cluster <- make_cluster(hosts, 4)
stopifnot(
	  # should have 32 locations
	  length(cluster) == 4 * 8,
	  # all locations should be Rserve connections
	  all(sapply(cluster, inherits, "RserveConnection")),
	  # as.cluster must coerce a list of connections to a cluster
	  identical(cluster, as.cluster(unclass(cluster))),
	  # get_hosts should return a cluster of single connections to each host
	  length(get_hosts(cluster)) == 8,
	  all(sapply(get_hosts(cluster), inherits, "RserveConnection")),
	  # hostlist must return a list with connection sublists grouped by host
	  length(hostlist(cluster)) == 8 && 
		  all(lengths(hostlist(cluster)) == 4),
	  # is.cluster detects cluster class
	  is.cluster(cluster), 	# +ve
	  !is.cluster("a"))	# -ve
# cluster must initialise with Rserve instances already running
cluster2 <- make_cluster(hosts, 1)
rm(cluster, cluster2)
# kill_servers must execute without errors
kill_servers(hosts)
# cluster must initialise with no Rserve instances already running
cluster <- make_cluster(hosts, 4)
# cluster must initialise with some Rserve instances already running
kill_servers(hosts[1:3])
cluster <- make_cluster(hosts, 4)

# Vector Communication

# legitimate vectors must be sent without error
a1 <- 1:150
a2 <- 151:300
a3 <- 1:150 %% 2 == 0
v1 <- as.distributed(1:150, cluster)				# numeric
v2 <- as.distributed(151:300, cluster)				# alt. numeric
v3 <- as.distributed(1:150 %% 2 == 0, cluster)			# logical
v4 <- as.distributed(1, cluster)				# obj < cluster
v5 <- as.distributed(1:32, cluster)				# obj == cluster
v6 <- as.distributed(structure(1:26, names = letters), cluster)	# attributes
v7 <- as.distributed(a1, cluster)				# symbol

# vectors must be received equivalently to what was sent
stopifnot(
	  identical(v1[], a1),					# numeric
	  identical(v2[], a2),			                # alt. numeric
	  identical(v3[], a3),			                # logical
	  identical(v4[], 1),			                # obj < cluster
	  identical(v5[], 1:32),		                # obj == cluster
	  identical(v6[], structure(1:26, names = letters)),    # attributes
	  identical(v7[], a1))                                  # symbol

# distributed object methods

stopifnot(
	  # locations correct
	  identical(length(get_locs(v2)), 32L),
	  # size correct
	  identical(class(get_size(v2)), "integer"),
	  # name is UUID
	  grepl(".{8}-.{4}-.{4}-.{4}-.{12}", get_name(v2)),
	  # test for distributed is accurate
	  is.distributed(v2),	# positive
	  !is.distributed("a"))	# negative

# garbage collection occurs on the remote end upon object removal

gc()
as.distributed(1:100, cluster)
firstobjs <- RS.eval(cluster[[1]], ls())
gc()
deleteobjs <- RS.eval(cluster[[1]], ls())
stopifnot(length(firstobjs) > length(deleteobjs))

# printing doesn't give error

print(v3)	# spanning whole cluster
print(v4)	# single connection

# Distributed Vector general methods

stopifnot(
	  # verify is.distributed.vector
	  is.distributed.vector(v3),	# positive
	  !is.distributed.vector("a"),	# negative
	  # length of distributed vectors accurate
	  identical(length(v1), 150L),
	  identical(length(v1), length(v2)),
	  identical(length(v4), 1L),	# smaller than cluster
	  # head and tail work transparently
	  identical(head(v1)[], head(1:150)),	# head < vector length
	  identical(head(v4)[], head(1)),	# head > vector length
	  identical(tail(v1)[], tail(1:150)),	# tail < vector length
	  identical(tail(v4)[], tail(1)))	# tail > vector length

# Distributed Vector Operations

# legitimate operations must take place without error
o1 <- v1 + v2	# addition between two vectors
o2 <- v2 + v1	# alternate argument positioning
o3 <- v1 / v2	# division between two vectors
o4 <- v2 / v1	# alternate argument positioning
o5 <- -v2	# negation
o6 <- v1 + 1	# addition with a scalar
o7 <- 1 + v1	# alternate argument positioning

# the value of the operations must be accurate
stopifnot(
	  identical(o1[], a1 + a2),	# addition between two vectors
	  identical(o2[], a2 + a1),     # alternate argument positioning
	  identical(o3[], a1 / a2),     # division between two vectors
	  identical(o4[], a2 / a1),     # alternate argument positioning
	  identical(o5[], -a2),         # negation
	  identical(o6[], a1 + 1),      # addition with a scalar
	  identical(o7[], 1 + a1))      # alternate argument positioning

# Distributed Vector Indexing

# legitimate subsetting operations must take place without error
i1 <- v1[1]			# singular numeric value
i2 <- v1[30:50]			# consecutive range of num vals
i3 <- v1[c(1, 50, 100)]		# ordered non-consec. num vals
i4 <- v1[v3]			# aligned distributed vector 
i5 <- v1[1:150 %% 2 == 0]	# local logical vector

# the value of the subsets must be correct
stopifnot(
	  identical(i1[], a1[1]),		# singular numeric value
	  identical(i2[], a1[30:50]),           # consecutive range of num vals
	  identical(i3[], a1[c(1, 50, 100)]),	# ordered non-consec. num vals
	  identical(i4[], a1[a3]),              # aligned distributed vector 
	  identical(i5[], a1[a3]))              # local logical vector

# Distributed Data Frames

# legitimate data frames must be sent without error
ad1 <- data.frame(a = letters, 	b = 1:26, 
		  row.names = LETTERS, stringsAsFactors = TRUE)
ad2 <- data.frame(a = 1)
d1 <- as.distributed(iris, cluster)	# no rownames, > cluster
d2 <- as.distributed(mtcars, cluster)	# rownames, == cluster
d3 <- as.distributed(ad1, cluster)	# < cluster

# legitimate splits for distribution must occur without error
dfsplit1 <- even_split(nrow(iris), 1:32)	# larger than cluster
dfsplit2 <- even_split(nrow(mtcars), 1:32)	# equal to cluster
dfsplit3 <- even_split(nrow(d3), 1:32)		# smaller than cluster

# data frames must be received equivalently to what was sent
stopifnot(
	  identical(d1[], iris),	# no rownames, > cluster
	  identical(d2[], mtcars),      # rownames, == cluster
	  identical(d3[], ad1),         # < cluster
	  # splitting local objects to even chunks for distribution correct
	  # larger than cluster
	  identical(dfsplit1$locs, 1:32),			# locations
	  identical(dfsplit1$from[c(1, 32)], c(1L, 146L)),	# from
	  identical(dfsplit1$to[c(1, 32)], c(4L, 150L)),	# to
	  # equal to cluster
	  identical(dfsplit2$locs, 1:32),			# locations
	  identical(dfsplit2$from[c(1, 32)], c(1L, 32L)),	# from
	  identical(dfsplit2$to[c(1, 32)], c(1L, 32L)),		# to
	  # smaller than cluster
	  identical(dfsplit3$locs, 1:26),			# locations
	  identical(dfsplit3$from[c(1, 26)], c(1L, 26L)),	# from
	  identical(dfsplit3$to[c(1, 26)], c(1L, 26L)))		# to

# TODO: single row data frame
# d4 <- as.distributed(ad2, cluster)	# single row
# identical(d4[], ad2),
# print(d4)	# single connection
# identical(dim(d4), dim(ad2)),		# dim << cluster
# identical(head(d4)[], head(ad2)),	# head > df rows
# identical(tail(d4)[], tail(ad2)),	# tail > df rows

# distributed object methods

stopifnot(
	  # locations correct
	  identical(length(get_locs(d1)), 32L),
	  # size correct
	  identical(get_size(d1), dfsplit1$from),
	  # name is UUID
	  grepl(".{8}-.{4}-.{4}-.{4}-.{12}", get_name(d1)),
	  # test for distributed is accurate
	  is.distributed(d1),	# positive
	  !is.distributed("a"))	# negative

# distributed Data Frame printing doesn't give error

print(d1)	# spanning whole cluster

# Distributed Data Frame general methods

stopifnot(
	  # verify is.distributed.data.frame
	  is.distributed.data.frame(d1),	# positive
	  !is.distributed.data.frame("a"),	# negative
	  # dimensions of distributed data frames accurate
	  identical(nrow(d1), nrow(iris)),	# nrow
	  identical(ncol(d1), ncol(iris)),	# ncol
	  identical(dim(d1), dim(iris)),	# dim
	  # head and tail work transparently
	  identical(head(d1)[], head(iris)),	# head < df rows
	  identical(tail(d1)[], tail(iris)),	# tail < df rows
	  identical(names(d1), names(iris)))	# names	

# legitimate subset operations must take place without error, with correct vals
stopifnot(
	  identical(d1[,3][], iris[,3]),	# row missing, col num
	  identical(d1[,3:5][], iris[,3:5]),	# row missing, col vec
	  identical(d1[,"Sepal.Length"][],	# row missing, col char.
		    iris[,"Sepal.Length"]),
	  identical(d1[3, ][], iris[3,]),	# row num, col missing
	  identical(d1[3, 3][], iris[3, 3]),	# row num, col num
	  identical(d1[3, 3:5][], iris[3, 3:5]),# row num, col vec
	  identical(d1[3, "Sepal.Length"][],	# row num, col char
		    iris[3, "Sepal.Length"]),
	  identical(d1[3:80, ][], iris[3:80,]),	# row vec, col missing
	  identical(d1[3:80, 3][], 		# row vec, col num
		    iris[3:80, 3]),
	  identical(d1[3:80, 3:5][],		# row vec, col vec
		    iris[3:80, 3:5]),
	  identical(d1[3:80, "Sepal.Length"][],	# row vec, col char
		    iris[3:80, "Sepal.Length"]),
	  identical(d1[v3, ][], iris[a3,]),	# row dist logical, col missing
	  identical(d1[v3, 3][], iris[a3, 3]),	# row dist logical, col num
	  identical(d1[v3, 3:5][],		# row dist logical, col vec
		    iris[a3, 3:5]),
	  identical(d1[v3, "Sepal.Length"][],	# row dist logical, col char
		    iris[a3, "Sepal.Length"]),
	  identical(d1[1:150 %% 2 == 0, ][], 	# row logical, col missing
		    iris[a3,]),	
	  identical(d1[1:150 %% 2 == 0, 3][],	# row logical, col num
		    iris[a3, 3]),	
	  identical(d1[1:150 %% 2 == 0, 3:5][],	# row logical, col vec
		    iris[a3, 3:5]),
	  identical(d1[1:150 %% 2 == 0, "Sepal.Length"][],# row logical col char
		    iris[a3, "Sepal.Length"]))

# other subset combinations
#d1[,as.distributed("Sepal.Length", cluster)]
#d1[,as.distributed(c(T,F,T,F,F), cluster)]
#d1[3, as.distributed("Sepal.Length", cluster)]
#d1[3, as.distributed(c(T,F,T,F,F), cluster)]
#d1[3:80, as.distributed("Sepal.Length", cluster)]
#d1[3:80, as.distributed(c(T,F,T,F,F), cluster)]
#d1[v1, ] 
#d1[v1, 3]
#d1[v1, 3:5]
#d1[v1, "Sepal.Length"]
#d1[v1, as.distributed.vector("Sepal.Length", cluster)]
#d1[v1, as.distributed.vector(c(T,F,T,F,F), cluster)]
#d1[v3, as.distributed.vector("Sepal.Length", cluster)]
#d1[v3, as.distributed.vector(c(T,F,T,F,F), cluster)]
#d1[1:150 %% 2 == 0, as.distributed.vector("Sepal.Length", cluster)]
#d1[1:150 %% 2 == 0, as.distributed.vector(c(T,F,T,F,F), cluster)]

# Alternate means of subsetting must return equivalent values to a non-dist. df
stopifnot(
	  identical(d1$Sepal.Length[], 		# `$` subsetting
		    iris$Sepal.Length),
	  identical(d1[["Sepal.Length"]][],	# `[[` subsetting by character
		    iris[["Sepal.Length"]]),
	  identical(d1[[1]][], iris[[1]]))	# `[[` subsetting by numeric

# list functions with dataframes must function equivalently to non-dist. df
stopifnot(
	  is.list(as.list(d1)),		 	# coercion to list produces list
	  identical(lapply(d1[,1:4], receive),	# lapply functions correctly
		    lapply(iris[,1:4], identity)))

# TODO: factor
# identical(lapply(d1, receive),	# lapply functions correctly
# 	  lapply(iris, identity)))

# further distributed data frame methods
# should be entirely equivalent to their non-distributed complements
stopifnot(
	  identical(unique(d1$Sepal.Width),		# unique
		    unique(iris$Sepal.Width)),
	  identical(table(d1$Sepal.Width),		# table
		    table(iris$Sepal.Width)),
	  identical(table(d1$Sepal.Width, 		# 2D table
			  d1$Sepal.Length),
		    table(iris$Sepal.Width,
			  iris$Sepal.Length)),
	  identical(receive(d1$Sepal.Width %in%		# %in%
			    c(3.0, 3.2)),
		    iris$Sepal.Width %in%
			    c(3.0, 3.2)))

# Close

kill_servers(hosts)

# distributed data frame resulting from read.distributed.csv
# create split csv file
cluster <- make_cluster("localhost", 4)
reflist <- even_split(iris, 1:4)
tmpdirectory <- tempdir()
lapply(1:4, function(i)
       write.table(iris[seq(reflist$from[i], reflist$to[i]), 1:4],
		 paste0(tmpdirectory, "/iris", i, ".csv"),
		 sep = ",", dec = ".", qmethod = "double",
		 row.names = FALSE, col.names = FALSE))

cols <- sapply(iris[,1:4], class)

# reading in a legitimate distributed csv must not raise any errors
rdf1 <- read.distributed.csv(cluster, paste0(tmpdir, "/*"),
			    header = FALSE, 
			    col.names = names(cols),
			    colClasses = as.vector(cols))

# distributed.data.frame resulting from reading in csv must be equivalent to one
# produced through sending a local data frame
df1 <- as.distributed(iris[,1:4], cluster)
stopifnot(all.aligned(rdf1, df1))

# the value received must be identical to the original local data
stopifnot(identical(rdf1[], iris[,1:4]))

# close
kill_servers("localhost")

# Distributed data frames at scale

hosts <- paste0("hadoop", 1:8)
cluster <- make_cluster(hosts, 4)

# reading in a legitimate distributed csv must not raise any errors
cols = c("Year"="integer","Month"="integer","DayofMonth"="integer",
	 "DayOfWeek"="integer","DepTime"="integer","CRSDepTime"="integer",
	 "ArrTime"="integer","CRSArrTime"="integer",
	 "UniqueCarrier"="character","FlightNum"="integer","TailNum"="character",
	 "ActualElapsedTime"="integer","CRSElapsedTime"="integer",
	 "AirTime"="integer","ArrDelay"="integer", "DepDelay"="integer",
	 "Origin"="character","Dest"="character","Distance"="integer",
	 "TaxiIn"="integer","TaxiOut"="integer", "Cancelled"="integer",
	 "CancellationCode"="character","Diverted"="integer",
	 "CarrierDelay"="integer","WeatherDelay"="integer","NASDelay"="integer",
	 "SecurityDelay"="integer","LateAircraftDelay"="integer")
rdf2 <- read.distributed.csv(cluster,
			     paths = "~/flights-chunk-*",
			     header = FALSE,
			     col.names = names(cols),
			     colClasses = as.vector(cols))

stopifnot(
	  identical(dim(rdf2),				# dimensions
		    c(118914458L, 29L)),
	  identical(rdf2[c(1, nrow(rdf2)),"Year"][], 	# indexing
		    c(1987L, 2008L)),
	  identical(nrow(rdf2[rdf2$Year == 1987L,]), 	# indexing logical
		    1311826L),
	  identical(as.vector(table(rdf2$Year)[1]),	# table
		    1311826L))

kill_servers(hosts)
