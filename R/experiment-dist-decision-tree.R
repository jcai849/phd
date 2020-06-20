allcombn <- function(x) {
	unlist(lapply(seq(x),
		      function(y) unlist(apply(combn(x, y), 2, list),
					 recursive = FALSE)),
	       recursive = FALSE)
}


qgen <- function(X, acc = 5){
	uniques <- lapply(X, unique)
	subsets <- lapply(uniques, function(unq) {
		       if (length(unq) < acc) {
			       allcombn(unq)
		       } else unq })
	unlist(lapply(names(subsets), function(subsetcol) {
		       lapply(subsets[[subsetcol]], function(element) {
			      substitute(X[,colname] %in% element,
					 list(colname = subsetcol,
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

dist_decision_tree <- function(X, y, max_depth = 4, 
			       impurity_measure = "gini", threshold = 0.1){
		counts <- gtable(y)
		if (impurity(counts, 
			     measure = impurity_measure) < threshold |
		    max_depth <= 1 | length(y) < 2) return(counts / sum(counts))
		questions <- qgen(X)
		qgoodness <- sapply(questions, function(question)
				    GoQ(y, question, impurity_measure))
		bestq <- questions[[which.max(qgoodness)]]
		return(structure(list(bestq,
			    Recall(gensubset(bestq, X, "L"),
				   gensubset(bestq, y, "L"),
				   max_depth = max_depth - 1,
				   impurity_measure = impurity_measure,
				   threshold = threshold),
			    Recall(gensubset(bestq, X, "R"),
				   gensubset(bestq, y, "R"),
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
