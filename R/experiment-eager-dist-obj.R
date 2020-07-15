# Depends 

library(RSclient)
library(uuid)

# Cluster

make_cluster <- function(hosts = "localhost", conns = 1, ...) {
	uninitd <- unlist(do_servers("pgrep Rserve")(hosts)) != 0 
	if (sum(uninitd) > 0) start_servers(hosts[uninitd])
	Sys.sleep(2)
	as.cluster(connect_servers(hosts, conns))
}

connect_servers <- function(hosts, conns) {
	connections <- unlist(mapply(function(host, conns)
				     replicate(conns, RS.connect(host)),
				     host = hosts, conns, SIMPLIFY = FALSE))
	names(connections) <- unlist(mapply(rep, hosts, conns))
	connections
}

do_servers <- function(command, ...) {
	addl_args <- list(...)
	gc()
	function(hosts) 
		lapply(hosts, function(host) 
		       do.call(system, 
			       c(list(command = paste("ssh", host, command)),
				 addl_args)))
}

start_servers <- do_servers("R CMD Rserve --vanilla", wait = FALSE)

kill_servers <- do_servers("killall Rserve", wait = FALSE)

get_hosts <- function(cluster) { # one connection per host
	cluster[match(unique(names(cluster)), names(cluster))]
}

as.cluster <- function(x) UseMethod("as.cluster", x)

as.cluster.list <- function(x) {
	lapply(x, function(conn) 
	       if (!inherits(conn, "RserveConnection"))
		       stop("list not coercible to a cluster"))
	class(x) <- "cluster"
	x
}

as.cluster.cluster <- identity

is.cluster <- is.distributed.class("cluster")

hostlist <- function(cluster) {
	sapply(unique(names(cluster)), 
	       function(host) cluster[names(cluster) %in% host],
	simplify = FALSE)
}

# Communication

distributed.do.call <- function(what, args, cluster = NULL,
				assign = FALSE, collect = FALSE,
				recycle = TRUE) {
	stopifnot(is.list(args),
		  is.function(what),
		  if (!is.null(cluster)) is.cluster(cluster))

	args.locs <- prep.args.locs(args, cluster, recycle = recycle)
	args <- args.locs$args
	locs <- args.locs$locs

	if (collect && assign) {
		id <- UUIDgenerate()
		lapply(locs, function(loc)
		       eval(bquote(RS.eval(loc, 
				      assign(.(id),
					     do.call(.(what), .(args))),
				      wait = FALSE))))
		return(list(id = id, vals = lapply(locs, RS.collect)))
	}
	if (assign) {
		id <- UUIDgenerate()
		lapply(locs, function(loc)
		       eval(bquote(RS.eval(loc, 
				      {assign(.(id),
					      do.call(.(what), .(args)))
				      NULL},
				      wait = FALSE))))
		lapply(locs, RS.collect)
		return(id)
	}
	if (collect) {
		lapply(locs, function(loc)
		       eval(bquote(RS.eval(loc, do.call(.(what), .(args)),
				      wait = FALSE))))
		return(lapply(locs, RS.collect))
	}
	lapply(locs, function(loc)
	       eval(bquote(RS.eval(loc,
			      {do.call(.(what), .(args))
			      NULL}, 
			      wait = FALSE))))
	lapply(locs, RS.collect)
	return()
}

Summary.distributed.vector <- function(..., na.rm = FALSE) 
	do.call(.Generic,
		distributed.do.call(.Generic, 
				    c(list(...), list(na.rm = na.rm)),
				    collect = TRUE))

# NB. incorrect for cumsum and other 4th group Math
Math.distributed.vector <- function(x, ...)
	distributed.vector(locs = get_locs(x),
			   name = distributed.do.call(.Generic,
						      c(x, list(...)),
						      assign = TRUE),
			   from = get_from(x),
			   to = get_to(x))

prep.args.locs <- function(args, cluster, recycle) {
	are.dist <- sapply(args, is.distributed)
	if (is.null(cluster) && !any(are.dist)) stop("no distributed info")
	if (!is.null(cluster) && any(are.dist)) 
		args[are.dist] <- do.call(align,
					  c(args[are.dist], 
					    list(recycle = recycle,
						 cluster = cluster)))
	if (any(are.dist)) {
		aligned <- do.call(all.aligned, args[are.dist])
		if (!aligned) 
			args[are.dist] <- do.call(align,
						  c(args[are.dist],
						    list(recycle = recycle)))
		args[are.dist] <- lapply(args[are.dist],
				    bquote(get(.(get_name(arg)))))
		locs <- get_locs(args[are.dist][[1]])
	}
	if (!is.null(cluster))
		locs <- cluster 
	list(args, locs)
}

send <- function(obj, cluster=NULL, align_with=NULL){
	if (is.null(cluster) && is.null(align_with)) 
		stop("one of 'cluster' or 'align_with' required")
	id <- UUIDgenerate()
	clustsize <- length(cluster)
	objtype <- if (is.data.frame(obj) ||
		       is.matrix(obj)) "non-atomic" else "atomic"
	objsize <- switch(objtype,
			  "atomic" = length,
			  "non-atomic" = nrow)(obj)

	reflist <- if (is.null(align_with)) {
		c(even_split(objsize, cluster), name = id)
	} else align(obj, align_with = align_with)	

	lapply(seq(length(reflist$locs)),
	       function(i) {
		       tosend <- switch(objtype,
					"atomic" = obj[seq(reflist$from[i],
							   reflist$to[i])],
					"non-atomic" = obj[seq(reflist$from[i], 
							       reflist$to[i]),])
		       RS.assign(reflist$locs[[i]], reflist$name, tosend)})
	if (is.vector(obj)) return(do.call(distributed.vector, reflist))
	if (is.data.frame(obj)) return(do.call(distributed.data.frame, reflist))
	do.call(distributed.object, reflist)
}

even_split <- function(objsize, dest) {
	destsize <- length(dest)
	spliton <- if (destsize < objsize) {
		bucketsto <- cumsum(rep(objsize / 
					destsize, destsize))
		bucketsfrom <- c(0, bucketsto[-destsize])
		facsplit = sapply(seq(objsize),
				  function(i) which(i > bucketsfrom &
						    i <= bucketsto))
		c(TRUE, !{c(NA, facsplit[-objsize]) -
		  facsplit == 0}[-1])
	} else rep(TRUE, objsize)
	list(locs = dest[seq(sum(spliton))],
	     from = which(spliton), 
	     to = as.integer(c(which(spliton)[-1] - 1, objsize)))
}

as.distributed <- send

receive <- function(obj, remote=FALSE) {
	UseMethod("receive", obj)
}

dist_receive <- function(joinf)
	function(obj) {
	conn <- structure(get_locs(obj), names = NULL)
	recv <- lapply(conn, function(hostname) {
			       eval(bquote(RS.eval(hostname,
						   get(.(get_name(obj))))))})
	do.call(joinf, recv)
}

# returns aligned dist obj if given dist objs
# returns locs, to, from if given local to align with dist.
align <- function(..., recycle = FALSE, align_with = NULL, cluster = NULL){
	stop("Distributed objects not aligned; Implement `align`!")
	if (objsize == max(get_to(align_with))) 
		list(locs = get_locs(align_with),
		     name = id,
		     from = get_from(align_with),
		     to = get_to(align_with))
}

all.aligned <- function(...) {
	objs <- list(...)
	if (length(objs) < 2) return(TRUE)
	bi.aligned <- function(x, y) {
	identical(get_locs(x), get_locs(y)) &&
		identical(length(x), length(y)) &&
		identical(get_from(x), get_from(y)) &&
		identical(get_to(x), get_to(y))
	}
	if (length(objs) == 2) return(bi.aligned(objs[1], objs[2]))
	return(
	all(mapply(function, 
		   x = objs[-1], y = objs[-length(objs)])))
} 




# Distributed classes

distributed.class <- function(classname){
	function(locs, name, from, to) {
		stopifnot(is.cluster(locs), is.character(name),
			  is.integer(from), is.integer(to))
		slots <- list(locs = locs, name = name,
			      from = from, to = to)
	dist_ref <- new.env()
	lapply(names(slots),
	       function(name) assign(name, slots[[name]], envir = dist_ref))
	# set as global function within a function
	# tryCatch removal, failure results in adding to a list of later removals
	cleanup <- function(e) lapply(get_locs(e), function(host) {
			      eval(bquote(RS.eval(host, rm(.(get_name(e))))))})
	reg.finalizer(dist_ref, cleanup)
	class(dist_ref) <- c(classname, "distributed.object", class(dist_ref))
	dist_ref
	}
}

get_ref_slot <- function(slot) function(ref) get(slot, envir = ref)
get_locs <- get_ref_slot("locs")
get_name <- get_ref_slot("name")
get_to <- get_ref_slot("to")
get_from <- get_ref_slot("from")

distributed.object <- distributed.class(NULL)

is.distributed.class <- function(classname) {
	function(x) inherits(x, classname)
}

is.distributed <- is.distributed.class("distributed.object")

# Distributed Subsetting

eval_subset_template <- function(loc, subset_template, id, x, i, j) {
	# subset_template to be given as quoted language variations of x[i,j]
	eval(eval(substitute(substitute(
		      RS.eval(loc,
			      {assign(id, subset_template); NULL},
			      wait = FALSE),
		    list(x = substitute(get(x), list(x = get_name(x))),
			 i = if (is.distributed(i)) 
				 substitute(get(i), 
					    list(i = get_name(i))) else i,
			 j = if (is.distributed(j))
				 substitute(get(j),
					    list(j = get_name(j))) else j)),
			 list(id = id,
			      subset_template = subset_template))))
	RS.collect(loc)
	NULL
}

dist_subset <- function(subset_template, x, i, j=NULL) {
	id <- UUIDgenerate()
	subset_template <- substitute(subset_template)
	locs <- get_locs(x)

	lapply(locs, eval_subset_template, 
	       subset_template, id, x, i, j)

	lapply(locs, function(host) {
	       eval(bquote(RS.eval(host,
			   do.call(if (is.data.frame(get(.(id))) ||
				       is.matrix(get(.(id))))
					   "nrow" else "length",
				   list(get(.(id)))),
				   wait = FALSE)))})
	all.locs <- sapply(locs, RS.collect)
	nonemptylocs <- all.locs != 0
	locs <- all.locs[nonemptylocs]

	list(locs = get_locs(x)[nonemptylocs],
	      name = id,
	      from = cumsum(c(1,locs[-length(locs)])),
	      to = cumsum(locs))
}

generate_num_selection <- function(x, i){
	which.loc <- rowSums(outer(i, get_from(x), "-") >= 0)
	allselections <- i - get_from(x)[which.loc] + 1
	locs <- unique(which.loc)
	list(locs = locs, 
	     selections = lapply(locs, function(loc)
				 allselections[which.loc == loc]))
}

num_subset <- function(subset_template, x, i, j=NULL){
	id <- UUIDgenerate()
	subset_template <- substitute(subset_template)
	selectionlist <- generate_num_selection(x, i)
	locs <- get_locs(x)[selectionlist$locs]
	selections <- selectionlist$selections

	mapply(function(loc, selection) 
	       eval_subset_template(loc, subset_template, 
				    id, x, selection, j),
	       locs, selections)

	list(locs = locs,
	     name = id,
	     from = cumsum(c(1, lengths(selections)[-length(selections)])),
	     to = cumsum(lengths(selections)))
}

dist_print <- function(type, components, measurename, measure) {
	function(x, elements = 6L, ...) {
	cat(sprintf(paste0("A ", type, " of ", measurename, " %s\n"),
		    paste(measure(x), collapse = " x ")))
	cat(paste0("Header ", components, ":\n\n"))
	print(receive(head(x, elements)))
	cat("\n")
	nlocs <- length(get_locs(x))
	connfmt <- paste0("Distributed over %d connection", 
			  if (nlocs > 1) "s " else " ")
	hostnames <- as.list(names(get_hosts(get_locs(x))))
	hostfmt <- paste0("on ",
		      switch(as.character(length(hostnames)),
			     "1" = "host %s",
			     "2" = "hosts %s and %s",
			     "3" = "hosts %s, %s, and %s",
			     "host %s and %d others"),
		      "\n")
	addl <- if (length(hostnames) > 3) 
		c(hostnames[1], length(hostnames) - 1) else hostnames
	cat(paste0(sprintf(connfmt, nlocs),
		   do.call(sprintf,
			   c(list(fmt = hostfmt), as.list(addl)))))
	}
}

# Distributed vector methods

distributed.vector <- distributed.class("distributed.vector")

is.distributed.vector <- is.distributed.class("distributed.vector")

length.distributed.vector <- function(x) max(get_to(x))

head.distributed.vector <- function(x, n = 6L, ...) x[seq(min(n, length(x)))]

tail.distributed.vector <- function(x, n = 6L, ...) 
	x[seq(length(x) - min(n, length(x)) + 1, length(x))]

print.distributed.vector <- dist_print("Distributed Vector", 
				       "elements", "Length", length)

receive.distributed.vector <- dist_receive(c)

Ops.distributed.vector <- function(e1, e2=NULL) {
	id <- UUIDgenerate()
	if (is.null(e2)) {
			lapply(get_locs(e1), function(host) eval(bquote(
				RS.eval(host,
				{assign(.(id), 
					do.call(.(.Generic), 
						list(get(.(get_name(e1))))));
				NULL},
				wait = FALSE))))
			lapply(get_locs(e1), RS.collect)
			return(distributed.vector(locs = get_locs(e1),
						  name = id,
						  from = get_from(e1),
						  to = get_to(e1)))
	}
	if (is.distributed.vector(e1) && 
	    is.distributed.vector(e2)){
		if (all.aligned(e1, e2) ||
		    (length(e1) == 1 || length(e2) == 1)) {
			lapply(get_locs(e1), function(host) eval(bquote(
				RS.eval(host,
				{assign(.(id), 
					do.call(.(.Generic), 
						list(get(.(get_name(e1))),
						     get(.(get_name(e2)))))); 
				NULL},
					wait = FALSE))))
			lapply(get_locs(e1), RS.collect)
			return(distributed.vector(locs = get_locs(e1),
					   name = id,
					   from = if (length(e2) == 1)
						   get_from(e1) else
							   get_from(e2),
					   to = if (length(e2) == 1) 
						   get_to(e1) else
							   get_to(e2)))
		} else if (length(e1) == length(e2) ||
			   length(e1) > length(e2)) {
			do.call(.Generic, list(e1, align(e2, e1)))
		} else # (length(e1) < length(e2)) 
			do.call(.Generic, list(align(e1, e2), e2))
	} else if (!is.distributed.vector(e1)) {
		e1 <- send(e1, get_locs(e2), align_to=e2)
		do.call(.Generic, list(e1, e2))
	} else { # (!("distributed.vector" %in% e2.classes)) {
		e2 <- send(e2, get_locs(e1), align_to=e1)
		do.call(.Generic, list(e1, e2))
	}
}

`[.distributed.vector` <- function(x, i=NULL){
	if (is.null(i)) {
		cat("receiving distributed vector...\n"); return(receive(x))}
	if (is.logical(i)) 
		return(x[as.distributed(i, align_to=x)])

	dist_ref <- 
	if (is.distributed.vector(i)){
		if (!all.aligned(x, i)) {
	stop(paste0("Subsetting by non-logical distributed vector not yet implemented",
		    "x-from: ", paste0(get_from(x)),
		    "i-from: ", paste0(get_from(i)),))
		} else dist_subset(x[i], x = x, i = i)
	} else if (is.numeric(i)) { num_subset(x[i], x = x, i = i)
	} else stop(paste("Unrecognised class for i. Your class: ", 
			  paste(class(i), collapse = ", ")))

	do.call(distributed.vector, dist_ref)
}

`%gin%` <- `%in%`

`%in%` <- function(x, table) UseMethod("%in%", x)

`%in%.default` <- function(x, table) x %gin% table

`%in%.distributed.vector` <- function(x, table) {
	id <- UUIDgenerate()
	lapply(get_locs(x),
	       function(host) eval(bquote(RS.eval(host, 
			  {assign(.(id), 
				  get(.(get_name(x))) %in% .(table));
			  NULL},
			  wait = FALSE))))
	lapply(get_locs(x), RS.collect)
	distributed.vector(host = get_locs(x),
			   name = id, 
			   from = get_from(x),
			   to = get_to(x))
}

combine <- function(...) UseMethod("combine", list(...)[[1L]])

combine.table <- function(...) {
	tabs <- list(...)
	chunknames <- lapply(tabs, dimnames)
	stopifnot(all(lengths(chunknames) == length(chunknames[[1]])))
	groupedvarnames <- lapply(seq(length(chunknames[[1]])),
			      function(i) lapply(chunknames,
						 function(chunk) chunk[[i]]))
	wholenames <- structure(lapply(groupedvarnames,
		       function(names) sort(unique(do.call(c, names)))),
			  names = names(chunknames[[1]]))
	
	wholearray <- array(0L, dim = lengths(wholenames, use.names = FALSE),
			    dimnames = wholenames)

	# the metalinguistics are for the sake of producing subsets of varying
	# number of arguments, according to the array dimension - can't be done
	# otherwise

	lapply(seq(length(tabs)), function(i)
	       {eval(substitute(wholearray_sub <<- wholearray_sub + tab_chunk, 
		   list(wholearray_sub =  do.call(call, c(list("["),
			x = quote(quote(wholearray)),
			unname(chunknames[[i]]))),
			tab_chunk = substitute(tabs[[i]], list(i = i)))))
	NULL})
	as.table(wholearray)
}

gtable <- table

table <- function(...) UseMethod("table", list(...)[[1]])

table.default <- function(...) do.call(gtable, list(...))

# assumes no other arguments
table.distributed.object <- function(...){
	refids <- sapply(list(...), function(x) get_name(x))
	lapply(get_locs(list(...)[[1]]),
	     function(host) eval(bquote(RS.eval(host,
		do.call(table, 
			lapply(.(refids),
				 function(refid) get(refid))),
						wait = FALSE))))
	nodecounts <- lapply(get_locs(list(...)[[1]]), RS.collect)
	do.call(combine.table, nodecounts)
}

#returns non-distributed
unique.distributed.vector <- function(x) {
	lapply(get_locs(x),
	       function(host) eval(bquote(RS.eval(host,
						  unique(get(.(get_name(x)))), 
						  wait = FALSE))))
	unique(unlist(lapply(get_locs(x), RS.collect)))
}

# distributed.data.frame methods

read.distributed.csv <- function(cluster, paths,  ...) {
	id <- UUIDgenerate()
	hosts <- get_hosts(cluster)
	lapply(hosts, function(conn)
	       eval(bquote(RS.eval(conn, Sys.glob(.(paths)),
				   wait = FALSE))))
	hostfiles <- lapply(hosts, RS.collect)
	hostconns <- hostlist(cluster)
	mapply(function(h, l, n) {
	       if (n < 1) stop(paste0("No files detected at host: ", h))
	       if (n > l) stop(paste0(
			 "More files than connections at host: ", h))},
	       names(hostconns), lengths(hostconns), lengths(hostfiles))

	destinations <- list()
	lapply(names(hostfiles), function(hostname) {
	       tosend <- even_split(length(hostfiles[[hostname]]), 
				    hostconns[[hostname]])
	       destinations <<- c(destinations, tosend$locs)
	       lapply(seq(length(tosend$locs)), function(i)
		      eval(bquote(RS.eval(tosend$locs[[i]],{
		     assign(.(id), 
			    do.call(read.csv,
			   .(c(file = hostfiles[[hostname]][i],
			       list(...)))));
		     NULL},
				     wait = FALSE))))})

	loc.rows <- sapply(destinations, function(conn) {
			       RS.collect(conn)
			       eval(bquote(RS.eval(conn,
						   nrow(get(.(id))))))})

	distributed.data.frame(locs = as.cluster(destinations),
	      name = id,
	      from = as.integer(cumsum(c(1,loc.rows[-length(loc.rows)]))),
	      to = as.integer(cumsum(loc.rows)))
}

distributed.data.frame <- distributed.class("distributed.data.frame")

is.distributed.data.frame <- is.distributed.class("distributed.data.frame")

names.distributed.data.frame <- function(x) {
	eval(bquote(RS.eval(get_locs(x)[[1]], 
			    names(get(.(get_name(x)))))))
}

print.distributed.data.frame <- dist_print("Distributed Data Frame",
					   "rows", "Dimension", dim)

receive.distributed.data.frame <- dist_receive(rbind)

head.distributed.data.frame <- function(x, n = 6L, ...) x[seq(min(n, nrow(x))),]

tail.distributed.data.frame <- function(x, n = 6L, ...) 
	x[seq(nrow(x) - min(n, nrow(x)) + 1, nrow(x)),]

`[.distributed.data.frame` <- function(x, i=NULL, j=NULL){
	if (is.null(i) && is.null(j)){
		cat("receiving distributed data frame...\n"); return(receive(x))}
	if (is.logical(i)) return(x[as.distributed(i, align_to = x), j])

	dist_ref <-
	if (is.distributed.vector(i)) {
		if (is.null(j)) { dist_subset(x[i, ], x = x, i = i, j = j)
		} else dist_subset(x[i, j], x = x, i = i, j = j)
	} else if (is.null(i)) {
		dist_subset(x[,j], x = x, i = i, j = j)
	} else if (is.numeric(i)) {
		if (is.null(j)) { num_subset(x[i,], x = x, i = i, j = j)
		} else  num_subset(x[i,j], x = x, i = i, j = j) 
	} else stop(paste("Unrecognised class for i. Your class: ", 
			  paste(class(i), collapse = ", ")))

	if (eval(bquote(RS.eval(.(dist_ref[["locs"]][[1]]),
				is.data.frame(get(.(dist_ref$name))))))) {
		do.call(distributed.data.frame, dist_ref)
	} else {
		do.call(distributed.vector, dist_ref)
	}
}

`[[.distributed.data.frame` <- function(x, i){
	id <- UUIDgenerate()
	lapply(get_locs(x),
	       function(host) eval(bquote(RS.eval(host, 
			  {assign(.(id), get(.(get_name(x)))[[.(i)]]); NULL},
					  wait = FALSE))))
	lapply(get_locs(x), RS.collect)
	distributed.vector(locs = get_locs(x), 
			 name = id, 
			 from = get_from(x), 
			 to = get_to(x))
}

dim.distributed.data.frame <- function(x) c(max(get_to(x)),
				    eval(bquote(RS.eval(get_locs(x)[[1]], 
						ncol(get(.(get_name(x))))))))

as.list.distributed.data.frame <- function(x, ...) sapply(names(x),
					  function(colname) x[[colname]],
					  simplify = FALSE, USE.NAMES = TRUE)

`$.distributed.data.frame` <- function(x, name) x[[name]]
