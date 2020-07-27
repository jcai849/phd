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

# what: function, as per do.call
# 3 forms of args (each differs based on their locational treatment):
# dist:	local or distributed objects to be aligned to the same location, 
# 	including recycling
# static: local objects to be sent to every location exactly as is
# map: 	local list containing lists of the same length as the number of
#	locations, each sublist containing lists to be iterated upon with
#	each location, as in a slightly stricter mapply

distributed.do.call <- function(what, args.dist = list(),
				args.static = list(), args.map = list(),
				cluster = NULL, assign = FALSE,
				collect = FALSE, recycle = TRUE) {
	stopifnot(is.list(args.dist), is.list(args.static), 
		  is.list(args.map), all(sapply(args.map, is.list)),
		  is.function(what) || is.character(what),
		  is.null(cluster)  || is.cluster(cluster),
		  !(assign && collect))
	if (! identical(args.dist, list())) {
		args.dist.locs <- prep.args.locs(args.dist, cluster,
						 recycle = recycle)
		args.dist <- args.dist.locs$args
		locs <- args.dist.locs$locs
	} else locs <- cluster
	stopifnot(identical(length(locs), as.vector(lengths(args.map))) || 
		  identical(args.map, list()))
	dist.eval <- function(expr) {
		expr <- substitute(expr)
		eval(substitute(do.call(mapply,
					c(list(function(loc, ...)
					      eval(bquote(RS.eval(loc, expr,
							   wait = FALSE)))),
					  loc = list(locs), args.map,
					  list(SIMPLIFY = FALSE))),
				list(expr = expr)))
	}
	if (assign) {
	id <- UUIDgenerate()
	dist.eval({assign(.(id),
	                   do.call(.(what),
				   .(c(args.dist, args.static, list(...)))));
		   df <- is.data.frame(get(.(id)));
		   list(df, if (df) nrow(get(.(id))) else length(get(.(id))))})
	size.type <- lapply(locs, RS.collect)
	size <- sapply(size.type, function(x) x[[2]])
	create.dist <- if (any(sapply(size.type, function(x) x[[1]])))
		distributed.data.frame else distributed.vector
	return(create.dist(name = id,
			   locs = locs[size > 0],
			   size = size[size > 0]))
	} else if (collect) {
		dist.eval(do.call(.(what), 
				  .(c(args.dist, args.static, list(...)))))
		return(lapply(locs, RS.collect))
	}
	dist.eval({do.call(.(what), .(c(args.dist, args.static, list(...))));
	           NULL})
	lapply(locs, RS.collect)
	return()
}

prep.args.locs <- function(args, cluster, recycle) {
	are.dist <- sapply(args, is.distributed)
	stopifnot(xor(is.cluster(cluster), any(are.dist)))
	if (is.cluster(cluster) && identical(args, list()))
		return(list(args = args, locs = cluster))
	if (! (all(are.dist) && do.call(all.aligned, args))) {
		align_with <- if (any(are.dist)) {
			match(which.max(sapply(args[are.dist],
			       function(x) {if (is.distributed.vector(x))
				       length else nrow}(x))),
			      cumsum(are.dist))
		} else  which.max(sapply(args, function(x) {
						 if (is.data.frame(x))
							 nrow else length}(x)))
		if (!is.distributed(args[[align_with]]))
			args[[align_with]] <- as.distributed(args[[align_with]],
							     cluster)
		args <- lapply(args, align,
			       align_with = args[[align_with]])
	}
	locs <- get_locs(args[[1]])
	args <- lapply(args, function(arg) as.symbol(get_name(arg)))
	list(args = args, locs = locs)
}

as.distributed <- function(obj, align_with, recycle = FALSE) {
	if (is.distributed(align_with)) {
		return(align(obj, align_with, recycle = recycle))
	} else if (is.cluster(align_with)) {
		send.def <- even_split(obj, cluster)
		locs <- as.cluster(send.def$locs)
		chunks <- send.def$chunks
	} else stop("no information to align with")
	distributed.do.call("identity", args.map = list(chunks), 
			    cluster = locs, assign = TRUE, 
			    recycle = recycle)
}

# returns aligned dist obj 
# x: dist/local, align_with: dist
align <- function(x, align_with = NULL, recycle = TRUE) UseMethod("align", x)

align.distributed.object <- function(x, align_with = NULL, recycle = TRUE) {
	if (identical(x, align_with)) return(x)
	stop("implement alignment for distributed objects!")
}

align.data.frame <- function(x, align_with = NULL, recycle = TRUE) {
	if (identical(x, align_with)) return(x)
	locs <- get_locs(align_with)
	indices <- gen.indices(x, align_with)
	chunks <- lapply(seq(nrow(indices)), function(index) 
		      rbind(x[seq(indices[index,"from1"], 
				  indices[index, "to1"]),],
			    if (!is.na(indices[index, "from2"]))
			    x[seq(indices[index,"from2"], 
				  indices[index,"to2"]),] else NULL))
	name <- send(chunks, locs)
	distributed.data.frame(name = id,
			       locs = locs,
			       size = sapply(chunks, nrow))

}

align.default <- function(x, align_with = NULL, recycle = TRUE) {
	if (identical(x, align_with)) return(x)
	locs <- get_locs(align_with)
	indices <- gen.indices(x, align_with)
	chunks <- lapply(seq(nrow(indices)), function(index) 
			   c(x[seq(indices[index,"from1"], 
					indices[index,"to1"])],
			     if (!is.na(indices[index,"from2"]))
			     x[seq(indices[index,"from2"],
				   indices[index,"to2"])] else NULL))
	name <- send(chunks, locs)
	distributed.vector(name = name,
			   locs = locs,
			   size = sapply(chunks, length))
}

send <- function(chunks, locs) {
	id <- UUIDgenerate()
	mapply(function(loc, val)
	       RS.assign(loc, id, val, wait = FALSE),
	       locs, chunks)
	lapply(locs, RS.collect)
	return(id)
}

# return indices
gen.indices <- function(x, align_with) {
	measure <- if (is.data.frame(x)) nrow else length
	cap <- get_size(align_with)
	indices <- matrix(nrow = length(cap), ncol = 4,
			  dimnames = list(NULL, 
					  c("from1", "to1", "from2", "to2")))
	prior <- cumsum(c(1, cap[-length(cap)]))
	firsts <- prior %% measure(x)
	indices[,"from1"] <- ifelse(firsts == 0, measure(x), firsts)
	indices[,"to1"] <- pmin(indices[,"from1"] + cap - 1, measure(x))
	complete <- indices[,"to1"] - indices[,"from1"] + 1 == cap |
		indices[,"to1"] - indices[,"from1"] + 1 == measure(x)
	indices[!complete,"from2"] <- 1
	indices[!complete,"to2"] <- pmin(indices[!complete,"from1"] - 1,
					 cap[!complete] - 
					 (indices[!complete,"to1"] -
					  indices[!complete,"from1"] + 1))
	indices
}

all.aligned <- function(...) {
	objs <- list(...)
	stopifnot(all(sapply(objs, is.distributed)))
	if (length(objs) < 2) return(TRUE)
	bi.aligned <- function(x, y) {
	identical(get_locs(x), get_locs(y)) &&
		identical(length(x), length(y)) &&
		identical(get_size(x), get_size(y))
	}
	if (length(objs) == 2) return(bi.aligned(objs[[1]], objs[[2]]))
	return(all(mapply(bi.aligned,
			  x = objs[-1], y = objs[-length(objs)])))
} 

even_split <- function(obj, dest) {
	objsize <- {if (is.data.frame(obj)) nrow else length}(obj)
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
	     chunks = split(obj, cumsum(spliton)))
}

receive <- function(obj, remote=FALSE) {
	UseMethod("receive", obj)
}

dist_receive <- function(joinf) function(obj) 
	do.call(joinf, structure(distributed.do.call("identity",
				args.dist = list(obj), collect = TRUE),
		      names = NULL))

# Distributed classes


distributed.class <- function(classname){
	function(locs, name, size) {
		stopifnot(is.cluster(locs), is.character(name),
			  is.integer(size))
		slots <- list(locs = locs, name = name,
			      size = size)
	dist_ref <- new.env()
	lapply(names(slots),
	       function(name) assign(name, slots[[name]], envir = dist_ref))
	reg.finalizer(dist_ref, cleanup)
	class(dist_ref) <- c(classname, "distributed.object", class(dist_ref))
	dist_ref
	}
}

cleanupcore <- function() {
	cleanlist <- list()
	function(obj){
		cleanlist <<- c(cleanlist, 
				list(list(locs = get_locs(obj), 
					  name = get_name(obj))))
		for (i in seq(length(cleanlist)))
		     tryCatch({distributed.do.call("rm",
			     args.static = list(cleanlist[[i]]$name),
			     cluster = list(cleanlist[[i]]$locs));
			     cleanlist <<- cleanlist[-i]},
			     error = function(e) NULL)
	}
}

cleanup <- cleanupcore()

get_ref_slot <- function(slot) function(ref) get(slot, envir = ref)
get_locs <- get_ref_slot("locs")
get_name <- get_ref_slot("name")
get_size <- get_ref_slot("size")

distributed.object <- distributed.class(NULL)

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
	      size = locs)
}

generate_num_selection <- function(x, i){
	which.loc <- colSums(outer(cumsum(get_size(x)), i, "-") < 0) + 1
	allselections <- i - cumsum(c(1L, get_size(x)))[which.loc] + 1L
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
	     size = lengths(selections))
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

length.distributed.vector <- function(x) max(cumsum(get_size(x)))

head.distributed.vector <- function(x, n = 6L, ...) x[seq(min(n, length(x)))]

tail.distributed.vector <- function(x, n = 6L, ...) 
	x[seq(length(x) - min(n, length(x)) + 1, length(x))]

print.distributed.vector <- dist_print("Distributed Vector", 
				       "elements", "Length", length)

Summary.distributed.vector <- function(..., na.rm = FALSE) 
	do.call(.Generic,
		distributed.do.call(.Generic, 
				    args.dist = list(...),
				    args.static = list(na.rm = na.rm),
				    collect = TRUE))

# NB. incorrect for cumsum and other 4th group Math
Math.distributed.vector <- function(x, ...)
	distributed.do.call(.Generic, 
			    args.dist = list(x), 
			    args.static = list(...), 
			    assign = TRUE)

receive.distributed.vector <- dist_receive(joinf = c)

Ops.distributed.vector <- function(e1, e2) {
	if (missing(e2)) {
		distributed.do.call(.Generic, args.dist = list(e1 = e1),
				    assign = TRUE)
	} else 
		distributed.do.call(.Generic, args.dist = list(e1 = e1, e2 = e2),
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
	stop("Subsetting by non-logical distributed vector not yet implemented")
		} else dist_subset(x[i], x = x, i = i)
	} else if (is.numeric(i)) { num_subset(x[i], x = x, i = i)
	} else stop(paste("Unrecognised class for i. Your class: ", 
			  paste(class(i), collapse = ", ")))

	do.call(distributed.vector, dist_ref)
}

`%in%` <- function(x, table) UseMethod("%in%", x)

`%in%.default` <- function(x, table) match(x, table, nomatch = 0) > 0

`%in%.distributed.vector` <- function(x, table) 
	distributed.do.call("%in%", args.dist = list(x),
			    args.static = list(table), assign = TRUE)

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

table.distributed.object <- function(...)
	do.call(combine.table,
		distributed.do.call("table", args.dist = list(...),
				    collect = T))

#returns non-distributed
unique.distributed.object <- function(x) 
	unique(distributed.do.call("unique", args.dist = list(x),
				   assign = TRUE)[])

# distributed.data.frame methods

read.distributed.csv <- function(cluster, paths,  ...) {
	id <- UUIDgenerate()
	hosts <- get_hosts(cluster)
	hostfiles <- distributed.do.call("Sys.glob", args.static = list(paths),
					 cluster = cluster, collect = TRUE)
	connfile = unlist(lapply(unique(names(hostfiles)), function(name)
		      lapply(seq(length(hostfiles[names(hostfiles) %in% name])),
			     function(i) 
				     hostfiles[[name]][i])), recursive = FALSE)
	distributed.do.call("read.csv", args.static = list(...),
			    args.map = list(file = connfile), cluster = cluster,
			    assign = TRUE)
}

distributed.data.frame <- distributed.class("distributed.data.frame")

is.distributed.data.frame <- is.distributed.class("distributed.data.frame")

names.distributed.data.frame <- function(x)
	unlist(distributed.do.call("names", args.dist = list(x[1,]), 
				   collect = TRUE), use.names = FALSE)

print.distributed.data.frame <- dist_print("Distributed Data Frame",
					   "rows", "Dimension", dim)

receive.distributed.data.frame <- dist_receive(joinf = rbind)

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

`[[.distributed.data.frame` <- function(x, i)
	distributed.do.call("[[", args.dist = list(x),
			    args.static = list(i), assign = TRUE)

dim.distributed.data.frame <- function(x) 
	c(max(cumsum(get_size(x))), 
	  unlist(distributed.do.call("ncol", args.dist = list(x[1,]),
				     collect = TRUE), use.names = FALSE))

as.list.distributed.data.frame <- function(x, ...) 
	sapply(names(x), function(colname) x[[colname]],
	       simplify = FALSE, USE.NAMES = TRUE)

`$.distributed.data.frame` <- function(x, name) x[[name]]
