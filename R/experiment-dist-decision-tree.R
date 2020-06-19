`%addjoin%` <- function(x, y){
	xyr <- rownames(x)[rownames(x) %in% rownames(y)]
	xyc <- colnames(x)[colnames(x) %in% colnames(y)]
	xruniq <- rownames(x)[!rownames(x) %in% rownames(y)]
	xcuniq <- colnames(x)[!colnames(x) %in% colnames(y)]
	yruniq <- rownames(y)[!rownames(y) %in% rownames(x)]
	ycuniq <- colnames(y)[!colnames(y) %in% colnames(x)]
	allr <- c(xyr, xruniq, yruniq)
	allc <- c(xyc, xcuniq, ycuniq)

	out <- matrix(nrow = length(allr),
		      ncol = length(allc),
		      dimnames = list(allr, allc))
	out[xruniq, ycuniq] <- 0
	out[yruniq, xcuniq] <- 0
	out[xyr, xyc] <- x[xyr, xyc] + y[xyr, xyc]
	out[xyr, xcuniq] <- x[xyr, xcuniq]
	out[xruniq, xyc] <- x[xruniq, xyc]
	out[xruniq, xcuniq] <- x[xruniq, xcuniq]
	out[xyr, ycuniq] <- y[xyr, ycuniq]
	out[yruniq, xyc] <- y[yruniq, xyc]
	out[yruniq, ycuniq] <- y[yruniq, ycuniq]
	out
}

transpose <- function(l){
	top <- unique(unlist(lapply(l, names)))
	sapply(top, 
	       function(i) lapply(l, 
				  function(j) j[[i]]),
		simplify = FALSE, USE.NAMES = TRUE)
}

gtable <- function(...) UseMethod("gtable", list(...)[[1]])

gtable.default <- function(...) do.call(table, list(...))

# assumes no other arguments
gtable.distributed.vector <- function(...){
	names <- sapply(list(...) function(x) x$name)
	nodecounts <- lapply(y$host,
	       function(host) eval(bquote(RS.eval(host,
		  do.call(table, 
			  c(lapply(.(names),
				  function(name) do.call(get, name))))))))
	lapply(transpose(nodecounts), 
				function(feature) Reduce(`%addjoin%`, feature))
}

qgen <- function(X){
	uniques <- apply(X, 2, unique)
	unlist(lapply(names(uniques), function(uniquecol) {
		       lapply(uniques[[uniquecol]], function(element) {
			      substitute(X[,colname] %in% element,
					 list(colname = uniquecol,
					      element = element))})}))
}

gensubset <- function(question, x, side = "L"){
	if (is.distributed.data.frame(x) | 
	    is.data.frame(x) | is.matrix(x)) {
	switch(side,
	       L = eval(substitute(x[question,],
				  list(question = question))),
               R = eval(substitute(x[!question,],
				  list(question = question))))
	       # include dataframe options!!
	} else switch(side,
	       L = eval(substitute(x[question],
				  list(question = question))),
               R = eval(substitute(x[!question],
				  list(question = question))))
}

impurity <- function(counts, measure="gini") {
	switch(measure,
	       gini = 1 - sum((counts / sum(counts))^2))
}

GoQ <- function(y, question, impurity_measure="gini"){
	L = gensubset(question, y, side = "L")
	R = gensubset(question, y, side = "R")
	impurity(gtable(y), impurity_measure) - 
		((length(L) / length(y)) * impurity(gtable(L),
						    impurity_measure)) - 
		((length(R) / length(y)) * impurity(gtable(R),
						    impurity_measure))
}

dist_decision_tree <- function(X, y, max_depth = 2, 
			       impurity_measure = "gini", threshold = 0.1){
	maketree <- function(...){
		counts <- gtable(y)
		if (impurity(counts, 
			     measure = impurity_measure) < threshold |
		    max_depth <= 1 | length(y) < 2) return(counts / sum(counts))
		questions <- qgen(X)
		qgoodness <- sapply(questions, function(question)
				    GoQ(y, question, impurity_measure))
		bestq <- questions[[which.min(qgoodness)]]
		return(list(bestq,
			    Recall(gensubset(bestq, X, "L"),
				   gensubset(bestq, y, "L"),
				   max_depth = max_depth - 1,
				   impurity_measure = impurity_measure,
				   threshold = threshold),
			    Recall(gensubset(bestq, X, "R"),
				   gensubset(bestq, y, "R"),
				   max_depth = max_depth - 1,
				   impurity_measure = impurity_measure,
				   threshold = threshold)))}
	tree <- maketree(X, y, max_depth, impurity_measure, threshold)
	class(tree) <- "decision.tree"
	tree
}
