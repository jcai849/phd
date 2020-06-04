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
	e1.classes <- class(e1)
	e2.classes <- class(e2)
	if (("distributed.vector" %in% e1.classes) &
	    ("distributed.vector" %in% e2.classes)){
		if (all.equal(a$host, b$host) &
		    all.equal(e1$to, e2$to) &
		    all.equal(e1$from, e2$from)) {
			lapply(e1$host, function(hostname) eval(
			    bquote(RS.eval(.(hostname),
				    quote(assign(.(id), 
						 do.call(.(.Generic), 
						  list(get(.(e1$name)),
						       get(.(e2$name))))))))))
			e1$name <- id
			return(e1)
		} else {
			do.call(.Generic, match_host_indices(e1, e2))
		}
	} else if (!("distributed.vector" %in% e1.classes)) {
		e1 <- send(e2$host, e1)
		do.call(.Generic, list(e1, e2))
	} else if (!("distributed.vector" %in% e2.classes)) {
		e2 <- send(e1$host, e2)
		do.call(.Generic, list(e1, e2))
	}
}

# returns a list of the two objects now with matching elements on matching hosts
match_host_indices <- function(e1, e2){
	
}

receive <- function(obj) {
	UseMethod("receive", obj)
}

receive.distributed.object <- function(obj) {
	recv <- lapply(obj$host, function(hostname) eval(bquote(RS.eval(.(hostname),
							   quote(get(.(obj$name)))))))
	unsplit(recv, rep(seq(length(recv)), 1 + obj$to - obj$from))
}

peek <- function(loc) {
	UseMethod("peek", loc)
}

peek.cluster <- function(loc) {
	hosts <- names(loc)
	sapply(hosts, function(hostname) {
		       sapply(RS.eval(rsc[[hostname]], quote(ls())),
						function(uuid) {
							eval(bquote(RS.eval(.(rsc[[hostname]]), 
									    quote(get(.(uuid))))))
						},
						simplify = FALSE, USE.NAMES = TRUE),
			 }
	simplify = FALSE, USE.NAMES = TRUE)
}

peek.node <- function(loc) {
	sapply(RS.eval(loc, quote(ls())), 
	       function(uuid) eval(bquote(RS.eval(.(loc), quote(get(.(uuid)))))),
	simplify = FALSE, USE.NAMES = TRUE)
}

length.distributed.vector <- function(x) max(x$to)

##################################  DEMO  ######################################

hosts <- paste0("hadoop", 1:8)

rsc <- make_cluster(hosts)
x = send(rsc, 1:100)

peek(rsc[[hosts[1]]])
peek(rsc)

a = send(rsc, 0:100)
b = send(rsc, 400:300)
c = a - b
c.local = receive(a) - receive(b)
all.equal(receive(c), c.local)

a. = 0:100
c. = a - b
all.equal(receive(c.), c.local)

b. = 400:300
c.. = a - b.
all.equal(receive(c..), c.local)


kill_servers(hosts)
