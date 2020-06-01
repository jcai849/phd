library(RSclient)
library(uuid)

do_servers <- function(command) 
	function(hosts) 
		lapply(hosts, function(host)
				       system(paste("ssh", host, command),
					      wait = FALSE))

start_servers <- do_servers("R CMD Rserve --vanilla")
kill_servers <- do_servers("killall Rserve") # Find a cleaner way to do this!!
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

send <- function(to, obj){
	id <- UUIDgenerate()
	UseMethod("send", to)
}

send.node <- function(to, obj){
	RS.assign(to, id, obj)
	dist_ref <- list(host = to, name = id, from = 0, to = length(obj))
	if (is.vector(obj)) {
		class(dist_ref) <- c("distributed.vector", class(dist_ref))
	}
	class(dist_ref) <- c("distributed.object", class(dist_ref))
	dist_ref
}

send.cluster <- function(to, obj){
	clustsize <- length(to)
	objsize <- length(obj)
	objelems <- seq(length(obj))
	spliton <- floor(objelems %% (objsize / clustsize)) == 1
	splits <- split(obj,
			cumsum(spliton))
	names(splits) <- names(to)
	lapply(seq(clustsize), function(i) RS.assign(to[[i]], id, splits[[i]]))
	dist_ref <- list(host = to, name = id,
			 from = which(spliton), 
			 to = c(which(spliton)[-1] - 1, objsize))
	if (is.vector(obj)) {
		class(dist_ref) <- c("distributed.vector", class(dist_ref))
	}
	class(dist_ref) <- c("distributed.object", class(dist_ref))
	dist_ref
}

Ops.distributed.vector <- function(e1, e2) {
	id <- UUIDgenerate()
	lapply(e1$host, function(hostname) eval(
	    bquote(RS.eval(.(hostname),  # is this R or lisp??
			    quote(assign(.(id), 
					 do.call(.(.Generic), 
					  list(get(.(e1$name)),
					       get(.(e2$name))))))))
	))
	e1$name <- id
	e1
}

##################################  DEMO  ######################################

hosts <- paste0("hadoop", 1:8)
rsc <- make_cluster(hosts)
x = send(rsc, 1:100)
RS.eval(rsc[[2]], quote(ls()))
RS.eval(rsc[[2]], quote({get(ls()[length(ls())])})) # is this R or APL?

a = send(rsc, 100:0)
b = send(rsc, 400:300)
c = a + b
a.on.2 = eval(bquote(RS.eval(rsc[[2]], quote(get(.(a$name))))))
b.on.2 = eval(bquote(RS.eval(rsc[[2]], quote(get(.(b$name))))))
c.on.2 = eval(bquote(RS.eval(rsc[[2]], quote(get(.(c$name))))))

a.on.2 + b.on.2
c.on.2

all.equal(a.on.2 + b.on.2, c.on.2)

kill_servers(hosts)
