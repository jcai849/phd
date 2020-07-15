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

is.distributed.class <- function(classname) {
	function(x) inherits(x, classname)
}

is.cluster <- is.distributed.class("cluster")

`[.cluster` <- function(x, i) structure(unclass(x)[i], class = "cluster")

hostlist <- function(cluster) {
	sapply(unique(names(cluster)), 
	       function(host) cluster[names(cluster) %in% host],
	simplify = FALSE)
}

# Communication
# args include both distributed and local to be coerced to distributed and all
# aligned
# addl.args don't need to be coerced to distributed, such as na.rm = FALSE
distributed.do.call <- function(what, args = list(), addl.args = list(), 
				cluster = NULL, assign = FALSE,
				collect = FALSE, recycle = TRUE) {
	stopifnot(is.list(args),
		  is.list(addl.args),
		  is.function(what) || is.character(what),
		  is.null(cluster)  || is.cluster(cluster))

	args.locs <- prep.args.locs(args, cluster, recycle = recycle)
	args <- args.locs$args
	locs <- args.locs$locs

	if (collect && assign) {
		id <- UUIDgenerate()
		lapply(locs, function(loc)
		       eval(bquote(RS.eval(loc, 
				      assign(.(id),
					     do.call(.(what), 
						     .(c(args, addl.args)))),
				      wait = FALSE))))
		return(list(id = id, vals = lapply(locs, RS.collect)))
	}
	if (assign) {
		id <- UUIDgenerate()
		lapply(locs, function(loc)
		       eval(bquote(RS.eval(loc, 
				      {assign(.(id),
					      do.call(.(what),
						      .(c(args, addl.args))))
				      NULL},
				      wait = FALSE))))
		lapply(locs, RS.collect)
		return(distributed.from.ext(id, locs))
	}
	if (collect) {
		lapply(locs, function(loc)
		       eval(bquote(RS.eval(loc, do.call(.(what),
							.(c(args, addl.args))),
				      wait = FALSE))))
		return(lapply(locs, RS.collect))
	}
	lapply(locs, function(loc)
	       eval(bquote(RS.eval(loc,
			      {do.call(.(what), .(c(args, addl.args)))
			      NULL}, 
			      wait = FALSE))))
	lapply(locs, RS.collect)
	return()
}

prep.args.locs <- function(args, cluster, recycle) {
	are.dist <- sapply(args, is.distributed)
	stopifnot(xor(is.cluster(cluster), any(are.dist)),
		  !is.cluster(cluster) && !any(are.dist))

	if (is.cluster(cluster) && identical(args, list()))
		return(list(args = args, locs = cluster))
	if (all(are.dist)){
		align_with <- which.max(sapply(args, length))
		args <- lapply(args, align, align_with = args[[align_with]])
	}
	if (!(any(are.dist))) stop("need get.alignment for non-dist objs in prep")
	locs <- get_locs(args[[1]])
	args <- lapply(args, function(arg)
		       bquote(get(.(get_name(arg)))))
	list(args = args, locs = locs)
}

as.distributed <- function(obj, cluster=NULL, align_with=NULL){
	if (is.null(cluster) && is.null(align_with)) 
		stop("one of 'cluster' or 'align_with' required")
	id <- UUIDgenerate()
	clustsize <- length(cluster)
	objsize <- {if (is.data.frame(obj) ||
		       is.matrix(obj)) nrow else length}(obj)
	reflist <- if (is.null(align_with)) {
		within(even_split(objsize, cluster),
		       {locs <- as.cluster(locs)
		       name <- id})
	} else stop("need get.alignment subroutine in as.distributed")

	lapply(seq(length(reflist$locs)),
	       function(i) {
		       tosend <- if (is.data.frame(obj) || is.matrix(obj)){
			       obj[seq(reflist$from[i], reflist$to[i]),]
		       } else obj[seq(reflist$from[i],reflist$to[i])]
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
	     from = as.integer(which(spliton)), 
	     to = as.integer(c(which(spliton)[-1] - 1, objsize)))
}

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

# returns aligned dist obj 
align <- function(x, align_with = NULL, recycle = TRUE,  cluster = NULL){
	stop("Distributed objects not aligned; Implement `align`!")
	if (objsize == max(get_to(align_with))) 
		list(locs = get_locs(align_with),
		     name = id,
		     from = get_from(align_with),
		     to = get_to(align_with))
}

all.aligned <- function(...) {
	objs <- list(...)
	stopifnot(all(sapply(objs, is.distributed)))
	if (length(objs) < 2) return(TRUE)
	bi.aligned <- function(x, y) {
	identical(get_locs(x), get_locs(y)) &&
		identical(length(x), length(y)) &&
		identical(get_from(x), get_from(y)) &&
		identical(get_to(x), get_to(y))
	}
	if (length(objs) == 2) return(bi.aligned(objs[[1]], objs[[2]]))
	return(all(mapply(bi.aligned,
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

is.distributed <- is.distributed.class("distributed.object")

distributed.from.ext <- function(id, locs) {
	obj_class <- eval(bquote(RS.eval(locs[[1]],
					 is.vector(get(.(id))))))
	measure <- if (obj_class) length else nrow
	create_obj <- if (obj_class)
		distributed.vector else distributed.data.frame
	eval(bquote(lapply(locs, function(loc)
			   RS.eval(loc,
				   .(measure)(get(.(id))),
				   wait = FALSE))))
	obj_lengths <- sapply(locs, RS.collect)
	return(create_obj(name = id,
			  locs = locs,
			  to = cumsum(obj_lengths),
			  from = cumsum(c(1,
					  obj_lengths[-length(obj_lengths)]))))

}
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
	      from = cumsum(c(1L,locs[-length(locs)])),
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
	     from = cumsum(c(1L, lengths(selections)[-length(selections)])),
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

Summary.distributed.vector <- function(..., na.rm = FALSE) 
	do.call(.Generic,
		distributed.do.call(.Generic, 
				    list(...),
				    list(na.rm = na.rm)
				    collect = TRUE))

# NB. incorrect for cumsum and other 4th group Math
Math.distributed.vector <- function(x, ...)
	distributed.do.call(.Generic, list(x), list(...), assign = TRUE)

receive.distributed.vector <- dist_receive(c)

Ops.distributed.vector <- function(e1, e2) {
	if (missing(e2)) {
		distributed.do.call(.Generic, list(e1 = e1), assign = TRUE)
	} else 
		distributed.do.call(.Generic, list(e1 = e1, e2 = e2),
				    assign = TRUE)
}

`[.distributed.vector` <- function(x, i=NULL){
	if (is.null(i)) {
		cat("receiving distributed vector...\n"); return(receive(x))}
	if (is.logical(i)) 
		return(x[as.distributed(i, align_with=x)])

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
	if (is.logical(i)) return(x[as.distributed(i, align_with = x), j])

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
