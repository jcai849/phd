library(RSclient)
library(uuid)

hosts <- paste0("hadoop", 1:8)

do_servers <- function(command) 
	function(hosts) 
		lapply(hosts, function(host)
				       system(paste("ssh", host, command),
					      wait = FALSE))

start_servers <- do_servers("R CMD Rserve --vanilla")
kill_servers <- do_servers("killall Rserve")
connect_servers <- function(hosts) sapply(hosts, 
					  function(host) RS.connect(host),
					  simplify = FALSE,
					  USE.NAMES = TRUE)

make_cluster <- function(hosts){
	try(start_servers(hosts))
	servers <- connect_servers(hosts)
	lapply(servers, function(node) {class(node) <- c("node", class(node))})
	class(servers) <- c("cluster", class(servers))
	servers
}

send <- function(to, obj, id = UUIDgenerate()){
	UseMethod("send", to)
}

send.node <- function(to, obj, id = UUIDgenerate()){
	RS.assign(to, id, obj)
	dist_ref <- list(host = to, name = id, from = 0, to = length(obj))
	class(dist_ref) <- "distributed.object"
	dist_ref
}

send.cluster <- function(to, obj, id = UUIDgenerate()){
	clustsize <- length(to)
	objsize <- length(obj)
	objelems <- seq(length(obj))
	splits <- split(obj, cumsum(floor(objelems %% (objsize / clustsize)) == 1))
	names(splits) <- names(to)
	lapply(seq(clustsize), function(i) send(to[[i]], splits[[i]], id))
	dist_ref <- list(host = to, name = id, from = sapply(splits, head, 1), to = sapply(splits, tail, 1))
	class(dist_ref) <- "distributed.object"
	dist_ref
}

rsc <- make_cluster(hosts)
send(rsc[[1]], 1:100)
RS.eval(rsc[[2]], quote({ls()}))
RS.eval(rsc[[2]], quote({get(ls())}))
x = send(rsc, 1:100)
