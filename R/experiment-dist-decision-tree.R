gini_imp <- function(count) {
	colsums.count <- colSums(count)
	p <- apply(count, 1, function(x) x / colsums.count)
	1 - rowSums(p^2)
}

weighted_gini_imp <- function(count){
	pclass <- colSums(count) / sum(count)
	sum(pclass * gini_imp(count))
}


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
	
decision_tree.distributed.data.frame <- function(X, y, max_depth = 4){
}


gini_node <- function(X, y, gini_threshold = 0.4, max_depth = 4){
	nodecounts <- lapply(X$host,
	       function(host) eval(bquote(RS.eval(host, 
						  apply(get(.(X$name)),
							2,
							table
							get(.(y$name)))))))
	featurecounts <- lapply(transpose(nodecounts), 
				function(feature) Reduce(`%addjoin%`, feature))
	featuresplit <- which.min(sapply(featurecounts, weighted_gini_imp))
	# levelsplit incorrect
	levelsplit <- which.min(gini_imp(featurecounts[[featuresplit]]))
	classprobs <- colSums(featurecounts[[featuresplit]]) / 
			sum(featurecounts[[featuresplit]])
	treenode <- list(split_feature = names(featuresplit), 
			 class_probs = classprobs)
	if (maxdepth == 1 | 
	    featurecounts[[featuresplit]] < gini_threshold){
		attr(treenode, "nodetype") <- "leaf"
		return(treenode)
	} else return(c(treenode, 
			list(left = Recall(X[X[[names(featuresplit)]] == 
					   names(levelsplit),],
					   y[X[[names(featuresplit)]] == 
					     names(levelsplit)],
					      gini_threshold=gini_threshold,
					      max_depth = max_depth - 1),
			     right = Recall(X[X[[names(featuresplit)]] != 
					   names(levelsplit),],
					   y[X[[names(featuresplit)]] != 
					     names(levelsplit)],
					      gini_threshold=gini_threshold,
					      max_depth = max_depth - 1))))
}
