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

# Communication

send <- function(obj, to=NULL, align_to=NULL){
	if (is.null(to) && is.null(align_to)) 
		stop("one of `to` or `align_to` required")
	id <- UUIDgenerate()
	clustsize <- length(to)
	objtype <- if (is.data.frame(obj) ||
		       is.matrix(obj)) "non-atomic" else "atomic"
	objsize <- switch(objtype,
			  "atomic" = length,
			  "non-atomic" = nrow)(obj)
	if (is.null(align_to)) {
		spliton <- if (clustsize < objsize) {
			bucketsto <- cumsum(rep(objsize / 
						clustsize, clustsize))
			bucketsfrom <- c(0, bucketsto[-clustsize])
			facsplit = sapply(seq(objsize),
					  function(i) which(i > bucketsfrom &
							    i <= bucketsto))
			c(TRUE, !{c(NA, facsplit[-objsize]) -
			  facsplit == 0}[-1])
			} else rep(TRUE, objsize)
		reflist <- list(hosts = to[seq(sum(spliton))], name = id,
				 from = which(spliton), 
				 to = c(which(spliton)[-1] - 1, objsize))
	} else if (objsize == max(get_to(align_to))) {
		reflist <- list(hosts = get_hosts(align_to),
				name = id,
				from = get_from(align_to),
				to = get_to(align_to))
	} else if (objsize == 1) {
		reflist <- list(hosts = get_hosts(align_to),
				 name = id,
				 from = rep(1, length(get_hosts(align_to))),
				 to = rep(1, length(get_hosts(align_to))))
	} else {
		stop("Recycling not yet implemented for length(obj) > 1")
	}
	lapply(seq(length(reflist$hosts)),
	       function(i) {
		       tosend <- switch(objtype,
					"atomic" = obj[seq(reflist$from[i],
							   reflist$to[i])],
					"non-atomic" = obj[seq(reflist$from[i], 
							       reflist$to[i]),])
		       RS.assign(reflist$hosts[[i]], reflist$name, tosend)
	       })
	if (is.vector(obj)) return(do.call(distributed.vector, reflist))
	if (is.data.frame(obj)) return(do.call(distributed.data.frame, reflist))
	do.call(distributed.object, reflist)
}

as.distributed <- send

receive <- function(obj, remote=FALSE) {
	UseMethod("receive", obj)
}

dist_receive <- function(joinf)
	function(obj) {
	conn <- structure(get_hosts(obj), names = NULL)
	recv <- lapply(conn, function(hostname) {
			       eval(bquote(RS.eval(hostname,
						   get(.(get_name(obj))))))})
	do.call(joinf, recv)
}

align <- function(from, to){
	stop("Distributed objects not aligned; Implement `align`!")
}

all.aligned <- function(x, y) {
	length(x) == length(y) &&
		identical(get_hosts(x), get_hosts(y)) &&
		all(get_from(x) == get_from(y)) &&
		all(get_to(x) == get_to(y))
}

# Distributed classes

distributed.class <- function(classname){
	function(hosts, name, from, to) {
	contents <- list(hosts = hosts,
			 name = name,
			 from = from,
			 to = to)
	dist_ref <- new.env()
	lapply(names(contents),
	       function(name) assign(name,
				     contents[[name]], 
				     envir = dist_ref))
	cleanup <- function(e) lapply(get_hosts(e), function(host) {
			      eval(bquote(RS.eval(host, rm(.(get_name(e))))))})
	reg.finalizer(dist_ref, cleanup)
	class(dist_ref) <- c(classname, "distributed.object", class(dist_ref))
	dist_ref
	}
}

get_ref_content <- function(content) function(ref) get(content, envir = ref)
get_hosts <- get_ref_content("hosts")
get_name <- get_ref_content("name")
get_to <- get_ref_content("to")
get_from <- get_ref_content("from")

distributed.object <- distributed.class(NULL)

is.distributed.class <- function(classname) {
	function(x) classname %in% class(x)
}

is.distributed <- is.distributed.class("distributed.object")

eval_subset_template <- function(host, subset_template, id, x, i, j) {
	eval(eval(substitute(substitute(
		      RS.eval(host,
			      {assign(id, subset_template); NULL}),
		    list(x = substitute(get(x), list(x = get_name(x))),
			 i = if (is.distributed(i)) 
				 substitute(get(i), 
					    list(i = get_name(i))) else i,
			 j = if (is.distributed(j))
				 substitute(get(j),
					    list(j = get_name(j))) else j)),
			 list(id = id,
			      subset_template = subset_template))))
	NULL
}

dist_subset <- function(subset_template, x, i, j=NULL) {
	id <- UUIDgenerate()
	subset_template <- substitute(subset_template)
	hosts <- get_hosts(x)

	lapply(hosts, eval_subset_template, 
	       subset_template, id, x, i, j)

	all.locs <- sapply(hosts, function(host) {
	       eval(bquote(RS.eval(host,
			   do.call(if (is.data.frame(get(.(id))) ||
				       is.matrix(get(.(id))))
					   "nrow" else "length",
				   list(get(.(id)))))))})
	nonemptylocs <- all.locs != 0
	locs <- all.locs[nonemptylocs]

	list(hosts = get_hosts(x)[nonemptylocs],
	      name = id,
	      from = cumsum(c(1,locs[-length(locs)])),
	      to = cumsum(locs))
}

generate_num_selection <- function(x, i){
	hostnums <- sapply(i, 
	   function(y) which.min({y - get_from(x)}[(y - get_from(x)) >= 0]))
	vals_on_host <- table(hostnums)
	hostsize <- get_to(x) - get_from(x) + 1
	hosts <- get_hosts(x)[as.numeric(names(vals_on_host))]

	selections <- as.list(vals_on_host)

	if (length(vals_on_host) == 1) {
	firstfrom <- 1 + i[1] - get_from(x)[as.numeric(names(vals_on_host))]
	first <- seq(firstfrom,
		     firstfrom + length(i) - 1)
	}
	if (length(vals_on_host) > 1) {
	first <- seq(hostsize[as.numeric(names(vals_on_host[1]))] -
		     vals_on_host[1] + 1,
		     hostsize[as.numeric(names(vals_on_host[1]))])
	selections[[1]] <- first
	last <- seq(vals_on_host[length(vals_on_host)])
	selections[[length(selections)]] <- last
	}
	selections[[1]] <- first
	if (length(vals_on_host) > 2) {
	    lapply(2:{length(vals_on_host) - 1},
	   function(y) selections[[y]] <<- seq(vals_on_host[y]))
	}
	
	list(selections = selections, hosts = hosts)
}

num_subset <- function(subset_template, x, i, j=NULL){
	id <- UUIDgenerate()
	subset_template <- substitute(subset_template)
	selectionlist <- generate_num_selection(x, i)
	hosts <- selectionlist$hosts
	selections <- selectionlist$selections

	mapply(function(host, selection) 
	       eval_subset_template(host, subset_template, 
				    id, x, selection, j),
	       hosts, selections)

	list(hosts = hosts,
	     name = id,
	     from = cumsum(c(1, 
			     sapply(selections, 
				    length)[-length(selections)])),
	     to = cumsum(sapply(selections,
				length)))
}

dist_print <- function(type, components, measurename, measure) {
	function(x, elements = 6L, ...) {
	cat(sprintf(paste0("A ", type, " of ", measurename, " %s\n"),
		    paste(measure(x), collapse = " x ")))
	cat(paste0("Header ", components, ":\n\n"))
	print(receive(head(x, elements)))
	cat("\n")
	hostnames <- as.list(names(get_hosts(x)))
	fmt <- paste0("Distributed over ",
		      switch(as.character(length(hostnames)),
			     "1" = "node %s",
			     "2" = "nodes %s and %s",
			     "3" = "nodes %s, %s, and %s",
			     "node %s and %d others"),
		      "\n")
	addl <- if (length(hostnames) > 3) 
		c(hostnames[1], length(hostnames) - 1) else hostnames
	cat(do.call(sprintf, c(list(fmt = fmt), as.list(addl))))
	}
}

# Distributed vector methods

distributed.vector <- distributed.class("distributed.vector")

is.distributed.vector <- is.distributed.class("distributed.vector")

length.distributed.vector <- function(x) max(get_to(x))

head.distributed.vector <- function(x, n = 6L, ...) x[seq(min(n, length(x)))]

tail.distributed.vector <- function(x, n = 6L, ...) 
	x[{length(x)-min(n, length(x) - 1)}:length(x)]

print.distributed.vector <- dist_print("Distributed Vector", 
				       "elements", "Length", length)
receive.distributed.vector <- dist_receive(c)

Ops.distributed.vector <- function(e1, e2=NULL) {
	id <- UUIDgenerate()
	if (is.null(e2)) {
			lapply(get_hosts(e1), function(host) eval(bquote(
				RS.eval(host,
				quote({assign(.(id), 
				  do.call(.(.Generic), 
				list(get(.(get_name(e1)))))); NULL}),
				wait = FALSE))))
			lapply(get_hosts(e1), RS.collect)
			return(distributed.vector(hosts = get_hosts(e1),
					   name = id,
					   from = get_from(e1),
					   to = get_to(e1)))
	}
	if (is.distributed.vector(e1) && 
	    is.distributed.vector(e2)){
		if (identical(get_hosts(e1), get_hosts(e2)) &&
		    ((all(get_to(e1) == get_to(e2)) &&
		      all(get_from(e1) == get_from(e2))) ||
		     (length(e1) == 1 ||
		      length(e2) == 1))) {
			lapply(get_hosts(e1), function(host) eval(bquote(
				RS.eval(host,
				quote({assign(.(id), 
				  do.call(.(.Generic), 
				list(get(.(get_name(e1))),
				     get(.(get_name(e2)))))); NULL}),
					wait = FALSE))))
			lapply(get_hosts(e1), RS.collect)
			return(distributed.vector(hosts = get_hosts(e1),
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
		e1 <- send(e1, get_hosts(e2), align_to=e2)
		do.call(.Generic, list(e1, e2))
	} else { # (!("distributed.vector" %in% e2.classes)) {
		e2 <- send(e2, get_hosts(e1), align_to=e1)
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
	stop("Subsetting by non-logical distributed vector not yet implemented")
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
	lapply(get_hosts(x),
	       function(host) eval(bquote(RS.eval(host, 
			  {assign(.(id), 
				  get(.(get_name(x))) %in% .(table)); NULL},
						  wait = FALSE))))
	lapply(get_hosts(x), RS.collect)
	dist_ref <- list(host = get_hosts(x),
			 name = id, 
			 from = get_from(x),
			 to = get_to(x))
	do.call(distributed.vector, dist_ref)
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
	
	wholearray <- array(0, dim = sapply(wholenames, length),
			    dimnames = wholenames)

	as.table(Reduce(`+`, lapply(seq(length(tabs)), function(i)
	       do.call(`[<-`, c(x = list(wholearray), 
			lapply(seq(length(wholenames)), function(j)
			  wholenames[[j]] %in% chunknames[[i]][[j]]),
			    value = list(tabs[[i]]))))))
}

gtable <- table

table <- function(...) UseMethod("table", list(...)[[1]])

table.default <- function(...) do.call(gtable, list(...))

# assumes no other arguments
table.distributed.object <- function(...){
	refids <- sapply(list(...), function(x) get_name(x))
	nodecounts <- lapply(get_hosts(list(...)[[1]]),
	     function(host) eval(bquote(RS.eval(host,
		do.call(table, 
			lapply(.(refids),
				 function(refid) get(refid)))))))
	do.call(combine, nodecounts)
}

#returns non-distributed
unique.distributed.vector <- function(x) {
	unique(unlist(lapply(get_hosts(x),
			     function(host) eval(bquote(RS.eval(host,
				  unique(get(.(get_name(x))))))))))
}
# distributed.data.frame methods

read.distributed.csv <- function(..., to){
	id <- UUIDgenerate()
	lapply(to, function(host) {
	       eval(bquote(
		   RS.eval(host, {assign(.(id),
		       do.call(read.csv,
			       .(list(...)))); NULL},
			   wait = FALSE)))
	})
	lapply(to, RS.collect)
	locs <- sapply(to, function(host) {
	       eval(bquote(RS.eval(host,
				   nrow(get(.(id))))))
	})
	distributed.data.frame(hosts = to,
	      name = id,
	      from = cumsum(c(1,locs[-length(locs)])),
	      to = cumsum(locs))
}

distributed.data.frame <- distributed.class("distributed.data.frame")

is.distributed.data.frame <- is.distributed.class("distributed.data.frame")

names.distributed.data.frame <- function(x) {
	eval(bquote(RS.eval(get_hosts(x)[[1]], 
			    names(get(.(get_name(x)))))))
}

print.distributed.data.frame <- dist_print("Distributed Data Frame",
					   "rows", "Dimension", dim)

receive.distributed.data.frame <- dist_receive(rbind)

head.distributed.data.frame <- function(x, n = 6L, ...) x[seq(min(n, nrow(x))),]

tail.distributed.data.frame <- function(x, n = 6L, ...) 
	x[{nrow(x)-min(n, nrow(x) - 1)}:nrow(x),]

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

	if (eval(bquote(RS.eval(.(dist_ref[["hosts"]][[1]]),
				is.data.frame(get(.(dist_ref$name))))))) {
		do.call(distributed.data.frame, dist_ref)
	} else {
		do.call(distributed.vector, dist_ref)
	}
}

`[[.distributed.data.frame` <- function(x, i){
	id <- UUIDgenerate()
	lapply(get_hosts(x),
	       function(host) eval(bquote(RS.eval(host, 
			  {assign(.(id), get(.(get_name(x)))[[.(i)]]); NULL},
					  wait = FALSE))))
	lapply(get_hosts(x), RS.collect)
	dist_ref <- list(hosts = get_hosts(x), 
			 name = id, 
			 from = get_from(x), 
			 to = get_to(x))
	do.call(distributed.vector, dist_ref)
}

dim.distributed.data.frame <- function(x) c(max(get_to(x)),
				    eval(bquote(RS.eval(get_hosts(x)[[1]], 
						ncol(get(.(get_name(x))))))))

as.list.distributed.data.frame <- function(x, ...) sapply(names(x),
					  function(colname) x[[colname]],
					  simplify = FALSE, USE.NAMES = TRUE)

`$.distributed.data.frame` <- function(x, name) x[[name]]
