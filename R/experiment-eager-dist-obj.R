# Depends 

library(RSclient)
library(parallel)
library(uuid)

# Parallel setup

options(mc.cores=detectCores())

# Cluster

do_servers <- function(command) 
	function(hosts) 
		mclapply(hosts, function(host)
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

peek <- function(loc) {
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


# Communication

send <- function(obj, to, align_to=NULL){
	id <- UUIDgenerate()
	clustsize <- length(to)
	objsize <- if (is.data.frame(obj) |
		       is.matrix(obj)) nrow(obj) else length(obj)
	objelems <- seq(objsize)
	if (is.null(align_to)) {
		spliton <- if (clustsize < objsize) {
			bucketsto <- cumsum(rep(objsize / 
						clustsize, clustsize))
			bucketsfrom <- c(0, bucketsto[-clustsize])
			facsplit = sapply(objelems,
					  function(i) which(i > bucketsfrom &
							    i <= bucketsto))
			c(TRUE, !{c(NA, facsplit[-objsize]) -
			  facsplit == 0}[-1])
		} else rep(TRUE, objsize)
		splits <- split(obj,
				cumsum(spliton))
		allsplitrefs <- seq(length(splits))
		names(splits) <- names(to)[allsplitrefs]
		lapply(seq(length(splits)),
		       function(i) RS.assign(to[[i]], id, splits[[i]]))
		reflist <- alist(host = to[allsplitrefs], name = id,
				 from = which(spliton), 
				 to = c(which(spliton)[-1] - 1, objsize))
	} else if (objsize == 1) {
		lapply(align_to$host,
		       function(x) RS.assign(x, id, obj))
		reflist <- alist(host = align_to$host,
				 name = id,
				 from = rep(1, length(align_to$host)),
				 to = rep(1, length(align_to$host)))
	} else {
		stop("Recycling not yet implemented for length(obj) > 1")
	}
	if (is.vector(obj)) return(do.call(distributed.vector, reflist))
	if (is.data.frame(obj)) return(do.call(distributed.data.frame, reflist))
	do.call(distributed.object, reflist)
}

as.distributed <- send

receive <- function(obj, remote=FALSE) {
	UseMethod("receive", obj)
}

receive.distributed.object <- function(obj) {
	conn <- obj$host
	recv <- lapply(conn, function(hostname) {
			       eval(bquote(RS.eval(hostname,
						   get(.(obj$name)))))
			   })
	unsplit(recv, rep(seq(length(recv)), 1 + obj$to - obj$from))
}

align <- function(from, to){
	stop("Distributed objects not aligned; Implement `align`!")
}

all.aligned <- function(x, y) {
	length(x) == length(y) &
		all.equal(x$host, y$host) &
		all(x$from == y$from) &
		all(x$to == y$to)
}

# Distributed classes

distributed.class <- function(classname){
	function(host, name, from, to) {
	dist_ref <- list(host = host,
			 name = name,
			 from = from,
			 to = to)
	class(dist_ref) <- c(classname, "distributed.object",
			     class(dist_ref))
	dist_ref
	}
}

distributed.object <- distributed.class(NULL)

is.distributed.class <- function(classname) {
	function(x) classname %in% class(x)
}

dist_subset <- function(subset_type, id, x, i, j=NULL) {
	subset_type <- substitute(subset_type)
	lapply(x$host, function(host) {
	      tosub <- bquote(RS.eval(host, 
				      {assign(.(id), .(subset_type)); NULL}))
	      eval(eval(substitute(substitute(tosub,
			    list(xname = x$name,
				 iname = if (is.vector(i)) NULL else i$name,
				 jname = if (is.vector(j)) NULL else j$name,
				 i = i,
				 j = j)),
			 list(tosub = tosub))))})
	all.locs <- sapply(x$host, function(host) {
	       eval(bquote(RS.eval(host,
			   do.call(if (is.data.frame(get(.(id))) |
			       is.matrix(get(.(id)))) "nrow" else "length",
				   list(get(.(id)))))))
	   })
	nonemptylocs <- sapply(all.locs, function(v) v != 0)
	locs <- all.locs[nonemptylocs]
	list(host = x$host[nonemptylocs],
	      name = id,
	      from = cumsum(c(1,locs[-length(locs)])),
	      to = cumsum(locs))
}

num_subset <- function(subset_type, id, x, i, j=NULL){
	subset_type <- substitute(subset_type)
	hostnums <- sapply(i, 
	   function(y) which.min({y - x$from}[(y - x$from) >= 0]))
	vals_on_host <- table(hostnums)
	hostsize <- x$to - x$from + 1
	hosts <- x$host[as.numeric(names(vals_on_host))]
	selectionlist <- as.list(vals_on_host)
	if (length(vals_on_host) == 1) {
	firstfrom <- 1 + i[1] - x$from[as.numeric(names(vals_on_host))]
	first <- seq(firstfrom,
		     firstfrom + length(i) - 1)
	}
	if (length(vals_on_host) > 1) {
	first <- seq(hostsize[as.numeric(names(vals_on_host[1]))] -
		     vals_on_host[1] + 1,
		     hostsize[as.numeric(names(vals_on_host[1]))])
	selectionlist[[1]] <- first
	last <- seq(vals_on_host[length(vals_on_host)])
	selectionlist[[length(selectionlist)]] <- last
	}
	selectionlist[[1]] <- first
	if (length(vals_on_host) > 2) {
	    lapply(2:{length(vals_on_host) - 1},
	   function(y) selectionlist[[y]] <<- seq(vals_on_host[y]))
	}
	mapply(function(host, selection) {
	      tosub <- bquote(RS.eval(host, 
			   assign(.(id), .(subset_type))))
	      eval(eval(substitute(substitute(tosub,
			    list(xname = x$name,
				 iname = if (is.vector(i)) NULL else i$name,
				 jname = if (is.vector(j)) NULL else j$name,
				 i = i,
				 j = j,
				 selection = selection)),
			 list(tosub = tosub))))
	   }, hosts, selectionlist)
	list(host = hosts,
	     name = id,
	     from = cumsum(c(1, 
			     sapply(selectionlist, 
				    length)[-length(selectionlist)])),
	     to = cumsum(sapply(selectionlist,
				length)))
}

# Distributed vector methods

distributed.vector <- distributed.class("distributed.vector")

is.distributed.vector <- is.distributed.class("distributed.vector")

length.distributed.vector <- function(x) max(x$to)

Ops.distributed.vector <- function(e1, e2) {
	id <- UUIDgenerate()
	e1.classes <- class(e1)
	e2.classes <- class(e2)
	if (("distributed.vector" %in% e1.classes) &
	    ("distributed.vector" %in% e2.classes)){
		if (all.equal(e1$host, e2$host) &
		    ((all(e1$to  == e2$to) &
		      all(e1$from == e2$from)) |
		     (length(e1) == 1 |
		      length(e2) == 1))) {
			lapply(e1$host, function(hostname) eval(bquote(
				RS.eval(hostname,
				quote({assign(.(id), 
				  do.call(.(.Generic), 
				list(get(.(e1$name)),
				     get(.(e2$name))))); NULL})))))
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

`[.distributed.vector` <- function(x, i){
	id <- UUIDgenerate()
	if (is.distributed.vector(i)){
		if (!all.aligned(x, i)) {
	stop("Subsetting by non-logical distributed vector not yet implemented")
		} else dist_ref <- dist_subset(get(xname)[get(iname)],
					       id = id, x = x, i = i)
	} else if (is.numeric(i)) {#assuming ordered & continuous numeric
	    dist_ref <- num_subset(get(xname)[selection],
				    id = id, x = x, i = i)
	} else stop(paste("Unrecognised class for i. Your class: ", 
			  paste(class(i), collapse = ", ")))
	do.call(distributed.vector, dist_ref)
}

# distributed.data.frame methods

read.distributed.csv <- function(..., to){
	id <- UUIDgenerate()
	lapply(to, function(host) {
	       eval(bquote(
		   RS.eval(host, {assign(.(id),
		       do.call(read.csv,
			       .(list(...)))); NULL})))
	})
	locs <- sapply(to, function(host) {
	       eval(bquote(RS.eval(host,
			   do.call(if (is.data.frame(get(.(id))) |
			       is.matrix(get(.(id)))) "nrow" else "length",
				   list(get(.(id)))))))
	})
	distributed.data.frame(host = to,
	      name = id,
	      from = cumsum(c(1,locs[-length(locs)])),
	      to = cumsum(locs))
}

distributed.data.frame <- distributed.class("distributed.data.frame")

is.distributed.data.frame <- is.distributed.class("distributed.data.frame")

names.distributed.data.frame <- function(x) {
	eval(bquote(RS.eval(x$host[[1]], names(get(.(x$name))))))
}

`[.distributed.data.frame` <- function(x, i=NULL, j=NULL){
	id <- UUIDgenerate()
	if (is.distributed.vector(i)) {
		if (is.distributed.vector(j)) {
		dist_ref <- dist_subset(get(xname)[get(iname), get(jname)],
					id = id, x = x, i = i, j = j)
		} else if (is.null(j)) {
		dist_ref <- dist_subset(get(xname)[get(iname),],
					id = id, x = x, i = i, j = j)
		} else {
		dist_ref <- dist_subset(get(xname)[get(iname),j],
					id = id, x = x, i = i, j = j)
		}
	} else if (is.numeric(i)) {
		if (is.distributed.vector(j)) {
	        dist_ref <- num_subset(get(xname)[selection,get(jname)],
				       id = id, x = x, i = i)
		} else if (is.null(j)) {
	        dist_ref <- num_subset(get(xname)[selection,],
				       id = id, x = x, i = i)
		} else {
		dist_ref <- num_subset(get(xname)[selection,j],
				       id = id, x = x, i = i)
		}
	} else if (is.null(i)) {
		if (is.distributed.vector(j)) {
		dist_ref <- dist_subset(get(xname)[, get(jname)],
					id = id, x = x, i = i, j = j)
		} else if (is.null(j)) {
		dist_ref <- dist_subset(get(xname)[,],
					id = id, x = x, i = i, j = j)
		} else {
		dist_ref <- dist_subset(get(xname)[,j],
					id = id, x = x, i = i, j = j)
		}
	} else stop(paste("Unrecognised class for i. Your class: ", 
			  paste(class(i), collapse = ", ")))

	if (eval(bquote(RS.eval(.(dist_ref$host[[1]]),
				is.data.frame(get(.(id))))))) {
		do.call(distributed.data.frame, dist_ref)
	} else {
		do.call(distributed.vector, dist_ref)
	}
}
