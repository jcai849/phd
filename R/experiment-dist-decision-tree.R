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

count <- function(X, y) {
	nodecounts <- lapply(X$host,
	       function(host) eval(bquote(RS.eval(host, 
						  apply(get(.(X$name)),
							2,
							table
							get(.(y$name)))))))
	lapply(transpose(nodecounts), 
				function(feature) Reduce(`%addjoin%`, feature))
}

qgen <- function(counts){
# Generate logical vectors to filter X and y
}

gensplit <- function(question, X, y){
# Apply logical vector `question` to filter X and y into L and R (yes and no) binary fashion
# return list of Left and Right splits, each of which have X and y
}

GoS <- function(split){
# take output of gensplit, assess count() and run some impurity measure on the output of count()
}

splitstop <- function(counts, threshold){
# asses counts, determining if purity threshold met at counts (node)
}

assignclass <- function(counts){
# assess counts, determining the class probabilities
}

dist_decision_tree <- function(X, y, max_depth = 4, 
			       impurity_measure = "gini", threshold = 0.8){
	maketree <- function(X, y, max_depth = max_depth,
			     impurity_measure = impurity_measure, 
			     threshold = 0.8){
		jointcounts <- count(X,y)
		if (splitstop(jointcounts, threshold) |
		    max_depth == 1) return(assignclass(jointcounts))
		questions <- qgen(jointcounts)
		splits <- lapply(questions, gensplit, X, y)
		bestsplit <- which.max(sapply(splits, GoS))
		# delete unnecessary splits - memory leak imminent
		return(list(questions[[bestsplit]],
			    Recall(splits[[bestsplit]]$L$X,
				   splits[[bestsplit]]$L$y,
				   max_depth - 1),
			    Recall(splits[[bestsplit]]$R$X,
				   splits[[bestsplit]]$R$y,
				   max_depth - 1)))}
	tree <- maketree()
	class(tree) <- "decision.tree"
	tree
}
