allcombn <- function(x) {
	combos <- unlist(lapply(seq(x),
		      function(y) unlist(apply(combn(x, y), 2, list),
					 recursive = FALSE)),
	       recursive = FALSE)
	combos[-length(combos)]
}


qgen <- function(X, acc = 5){
	uniques <- lapply(X, unique)
	subsets <- lapply(uniques[sapply(uniques, length) > 2],
		  function(unq) {
	       if (length(unq) < acc) {
			       allcombn(unq)
		       } else unq })
	unlist(lapply(names(subsets), function(subsetcol) {
		       lapply(subsets[[subsetcol]], function(element) {
			      substitute(X[,colname] %in% element,
					 list(colname = subsetcol,
					      element = element))})}))
}

gensubset <- function(question, x, X, side = "L"){
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
	       gini = 1 - sum((counts / sum(counts))^2),
	       stop("only gini implemented"))
}

GoQ <- function(y, X, question, impurity_measure="gini"){
	L = gensubset(question, y, X, side = "L")
	R = gensubset(question, y, X, side = "R")
	impurity(table(y), impurity_measure) - 
		((length(L) / length(y)) * impurity(table(L),
						    impurity_measure)) - 
		((length(R) / length(y)) * impurity(table(R),
						    impurity_measure))
}

dist_decision_tree <- function(X, y, max_depth = 4, 
			       impurity_measure = "gini", threshold = 0.1){
		counts <- table(y)
		if (impurity(counts, 
			     measure = impurity_measure) < threshold |
		    max_depth <= 1 | length(y) < 2) return(counts / sum(counts))
		questions <- qgen(X)
		qgoodness <- sapply(questions, function(question) {
					    gc()
					    GoQ(y, X, 
						question, impurity_measure)})
		bestq <- questions[[which.max(qgoodness)]]
		if (is.null(bestq)) return(counts / sum(counts))
		return(structure(list(bestq,
			    Recall(gensubset(bestq, X, X, "L"),
				   gensubset(bestq, y, X, "L"),
				   max_depth = max_depth - 1,
				   impurity_measure = impurity_measure,
				   threshold = threshold),
			    Recall(gensubset(bestq, X, X, "R"),
				   gensubset(bestq, y, X, "R"),
				   max_depth = max_depth - 1,
				   impurity_measure = impurity_measure,
				   threshold = threshold)),
		       class = "dist.decision.tree"))
}

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
