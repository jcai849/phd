library(RSclient)
library(uuid)

do_servers <- function(command) 
	function(hosts) 
		lapply(hosts, function(host)
				       system(paste("ssh", host, command),
					      wait = FALSE))

start_servers <- do_servers("R CMD Rserve --vanilla")
kill_servers <- do_servers("killall Rserve") # Find a cleaner way to do this!!
connect_servers <- function(hosts) {
	sapply(hosts, 
	       function(host) {
		       RS.connect(host)
	       }, simplify = FALSE, USE.NAMES = TRUE)
}

make_cluster <- function(hosts){
	try(start_servers(hosts))
	Sys.sleep(2)
	servers <- connect_servers(hosts)
#	lapply(servers, function(host) {
#	     lapply(c("RSclient", "uuid"),
#	      function(lib) eval(bquote(RS.eval(host, library(.(lib))))))
#	     lapply(c("receive", 
#			"receive.distributed.object", 
#			"connect_servers"),
#	      function(fun) eval(bquote(RS.assign(host, fun, .(get(fun))))))
#	       })
	class(servers) <- c("cluster", class(servers))
	servers
}

`[.cluster` <- function(x, i){
		classes <- class(x)
		class(x) <- NULL
		out <- x[i]
		class(out) <- classes
		out
}

send <- function(obj, to, align_to=NULL){
		id <- UUIDgenerate()
		clustsize <- length(to)
		objsize <- length(obj)
		objelems <- seq(length(obj))
		spliton <- if (clustsize < objsize) {
			bucketsto <- cumsum(rep(objsize / clustsize, clustsize))
			bucketsfrom <- c(0, bucketsto[-clustsize])
			facsplit = sapply(objelems,
			       function(i) which(i > bucketsfrom &
						 i <= bucketsto))
			c(TRUE, !{c(NA, facsplit[-objsize]) - facsplit == 0}[-1])
		} else rep(TRUE, objsize)
		splits <- split(obj,
				cumsum(spliton))
		allsplitrefs <- seq(length(splits))
		names(splits) <- names(to)[allsplitrefs]
		lapply(seq(length(splits)),
		       function(i) RS.assign(to[[i]], id, splits[[i]]))
		dist_ref <- list(host = to[allsplitrefs], name = id,
				 from = which(spliton), 
				 to = c(which(spliton)[-1] - 1, objsize))
		if (is.vector(obj)) {
			class(dist_ref) <- c("distributed.vector", 
					     class(dist_ref))
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
		if (all.equal(e1$host, e2$host) &
		    all.equal(e1$to, e2$to) &
		    all.equal(e1$from, e2$from)) {
			lapply(e1$host, function(hostname) eval(
			    bquote(RS.eval(hostname,
			     quote(assign(.(id), 
				 do.call(.(.Generic), 
				  list(get(.(e1$name)),
				       get(.(e2$name))))))))))
			e1$name <- id
			return(e1)
		} else if (length(e1) == length(e2) |
			   length(e1) > length(e2)) {
			do.call(.Generic, list(e1, align(e2, e1)))
		} else if (length(e1) < length(e2)) {
			do.call(.Generic, list(align(e1, e2), e2))
		}
	} else if (!("distributed.vector" %in% e1.classes)) {
		e1 <- send(e1, e2$host, align_to=e2)
		do.call(.Generic, list(e1, e2))
	} else if (!("distributed.vector" %in% e2.classes)) {
		e2 <- send(e2, e1$host, align_to=e1)
		do.call(.Generic, list(e1, e2))
	}
}

align <- function(from, to){
	stop("TODO: Implement align")
}

receive <- function(obj, remote=FALSE) {
	UseMethod("receive", obj)
}

receive.distributed.object <- function(obj, remote=FALSE) {
	if (remote) {
		conn <- connect_servers(names(obj$host))
		on.exit(lapply(conn, RS.close))
	} else {
		conn <- obj$host
	}
	recv <- lapply(conn, function(hostname) {
			       eval(bquote(RS.eval(hostname,
						   quote(get(.(obj$name))))))
			   })
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
			simplify = FALSE, USE.NAMES = TRUE)
		 },
	simplify = FALSE, USE.NAMES = TRUE)
}

length.distributed.vector <- function(x) max(x$to)
