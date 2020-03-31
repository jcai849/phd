## Until the node class and generics are working properly, assume each
## environment element of the cluster list is a node, with their data
## being contained and evaluated in their environment

## e.g.
    
cluster <- lapply(1:10,
                  function(x)new.env(parent = baseenv()))

for (node in cluster) {
    with(node,
         {x <- 1})
}

## 1. Initialise nodes

n.nodes <- 10
cluster <- lapply(1:n.nodes,
                  function(x)new.env(parent = as.environment("package:stats")))

## 2. Distribute data to nodes
## generate
n.data <- 100
data.init <- rbinom(100, n.data, 0.6)
data.dist <- split(data.init, 1:n.data %% n.nodes)
## distribute
invisible(mapply(assign, "data", data.dist, cluster))
lapply(cluster, ls.str)

## 3. find approximate median (middle actual number) using
## median-of-medians (guaranteed to be between 30 and 70th percentile)

mid.i <- function(length) ifelse(length %% 2 == 0,
                                 length/2,
                                 ceiling(length/2))
invisible(lapply(cluster, function(env) assign("mid.i", mid.i, envir=env)))

expr.mid <- quote(sort(data)[mid.i(length(data))])
node.meds <- sapply(cluster, function(x)eval(expr.mid, envir = x))

assign("node.meds", node.meds, envir = cluster[[1]])
expr.mid.of.mid <- eval(substitute(substitute(expr.mid, list(data = quote(node.meds))), list(expr.mid = expr.mid)))
eval(expr.mid.of.mid,
     cluster[[1]])

sort(node.meds)[mid.i(length(node.meds))]
sort(data)[mid.i(length(data))]


################################################
## Map Reduce Mean
################################################

n.nodes <- 10
cluster <- lapply(1:n.nodes,
                  function(x)new.env(parent = as.environment("package:stats")))

## 2. Distribute data to nodes
## generate
n.data <- 100
data.init <- rbinom(100, n.data, 0.6)
data.dist <- split(data.init, 1:n.data %% n.nodes)
## distribute
invisible(mapply(assign, "data", data.dist, cluster))
lapply(cluster, ls.str)

## 3. find sums and lengths

expr.sum <- quote(sum(data))
node.sums <- sapply(cluster, function(x)eval(expr.sum, envir = x))

expr.length <- quote(length(data))
node.lengths <- sapply(cluster, function(x)eval(expr.length, envir = x))

assign("sums", node.sums, envir = cluster[[1]])
assign("lengths", node.lengths, envir = cluster[[1]])
expr.mean <- quote(sum(sums)  / sum(lengths))
eval(expr.mean,
     cluster[[1]])

mean(data.init)
