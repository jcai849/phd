decision_tree <- function(X, y, max_depth = 4, 
			       impurity_measure = "gini", threshold = 0.1){

	counts <- rowSums(table(y, X[,1]))
	if (impurity(counts) < threshold | max_depth <= 1 | length(y) < 2)
		return(counts / sum(counts))

	splitinfo <- get_best_split(X, y, impurity_measure)
	splitpoint <- splitinfo$splitpoint
	splitvec <- splitinfo$splitvec

	return(structure(list(splitpoint,
			      Recall(X[splitvec,],
				     y[splitvec],
				     max_depth = max_depth - 1,
				     impurity_measure = impurity_measure,
				     threshold = threshold),
			      Recall(X[!splitvec,],
				     y[!splitvec],
				     max_depth = max_depth - 1,
				     impurity_measure = impurity_measure,
				     threshold = threshold)),
			 class = "dist.decision.tree"))
}

get_best_split <- function(X, y, impurity_measure = "gini") {
	tabs <- sapply(X, function(Xn) table(y, Xn))
	tabs <- tabs[sapply(tabs, function(tab) ncol(tab) > 1)]
	rowsplits <- sapply(tabs, function(tab) {
		t <- rowSums(tab)
		tL <- tab
		tR <- apply(tab, 2, function(x) t - x)
		itR <- apply(tR, 2, impurity, impurity_measure)
		itL <- apply(tL, 2, impurity, impurity_measure)
		it <- impurity(t)
		pL <- colSums(tL) / sum(tab)
		pR <- colSums(tR) / sum(tab)
		GoS <- it - pL*itL - pR*itR
		whichbest <- which.max(GoS)
		matrix(c(GoS[whichbest], whichbest), nrow = 2,
		       dimnames = list(c("impurity", "index"), NULL))})
	colsplit <- names(which.max(rowsplits[1,]))

	splitpoint <- colnames(tabs[[colsplit]])[rowsplits[2, colsplit]]
	names(splitpoint) <- colsplit
	mode(splitpoint) <- mode(X[1,colsplit][])
	splitvec <- X[,colsplit] %in% splitpoint
	list(splitpoint = splitpoint,
	     splitvec = splitvec)

}

allcombn <- function(x) {
	combos <- unlist(lapply(seq(x),
		      function(y) unlist(apply(combn(x, y), 2, list),
					 recursive = FALSE)),
	       recursive = FALSE)
	combos[-length(combos)]
}

impurity <- function(counts, measure="gini") {
	switch(measure,
	       gini = 1 - sum((counts / sum(counts))^2),
	       stop("only gini implemented"))
}

# L = y, R = n
predict.dist.decision.tree <- function(object, X){
	if (nrow(X) == 1) {
		if (eval(object[[1]])) {
			if (is.table(object[[2]])) {
				object[[2]]
			} else {
				predict(object[[2]], X) 
		}} else {
			if (is.table(object[[3]])) {
				object[[3]]
			} else {
				predict(object[[3]], X) 
	}}} else {
		lapply(seq(nrow(X)),
		       function(rownum) predict(object, X[rownum,]))
	}
}
