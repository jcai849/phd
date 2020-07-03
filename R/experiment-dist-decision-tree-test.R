# Local Decision Tree

X <- mtcars[,c("vs", "am", "gear", "carb")]
y <- mtcars$cyl
tree <- decision_tree(X, y)
predict(tree, X)

# Distributed Decision Tree

hosts <- paste0("hadoop", 1:8)
kill_servers(hosts)
rsc <- make_cluster(hosts)

dX <- as.distributed(mtcars[,c("vs", "am", "gear", "carb")], rsc)
dy <- as.distributed(mtcars$cyl, rsc)
dtree <- decision_tree(dX, dy)
predict(dtree, X)

# Big Distributed Decision Tree

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
big <- read.distributed.csv(file = "~/flights-chunk.csv",
			    header = FALSE,
			   col.names = names(cols),
			   colClasses = as.vector(cols),
			    to = rsc)
nrow(big)
big[100000000,][]
bigX <- big[,c("Month", "DayOfWeek")]
bigy <- big$Diverted
bigtree <- decision_tree(bigX, bigy)
biggertree <- decision_tree(bigX, bigy, max_depth=2, threshold = 0)

kill_servers(hosts)
