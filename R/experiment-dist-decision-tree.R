count_gini <- function(x, y){
	tapply(x, y, function(x) tapply(x, x, length))
}

count_all_gini <- function(X, y){
	apply(X, 2, function(x) count_gini(x, y))
}

gini_imp <- function(counts) {
	pmis <- sapply(counts, function(x) 1 - sum((x / sum(x))^2))
	classcounts <- sapply(counts, sum)
	pclass <- classcounts / sum(classcounts)
	sum(pclass * pmis)
}

gini_all_imp <- function(counts) {
	sapply(counts, gini_imp)
}

combine_counts <- function(counts){
	stop("Implement this!")
}

send(rsc, count_gini)
send(rsc, count_all_gini)
