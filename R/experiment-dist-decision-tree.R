count_all_gini <- function(X, y){
	apply(X, 2, table, y)
}

gini_imp <- function(count) {
	colsums.count <- colSums(count)
	p <- apply(count, 1, function(x) x / colsums.count)
	pmis <- 1 - rowSums(p^2)
	pclass <- colsums.count / sum(count)
	sum(pclass * pmis)
}

gini_all_imp <- function(counts) {
	sapply(counts, gini_imp)
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
}
	
	
combine_counts <- function(counts){
	Reduce(%addjoin%, counts)
}

X = split(iris[,!names(iris) %in% "Species"], 1:3)
y = split(iris$Species, 1:3)
counts <- lapply(1:3, function(i) count_all_gini(X[[i]], y[[i]]))



send(rsc, count_gini)
send(rsc, count_all_gini)


# x = matrix(1:6, byrow=T, nrow = 3, dimnames = list(c("a","b","c"), c("x","y")))
# y = matrix(1:6, byrow=T, nrow = 3, dimnames = list(c("a","d","e"), c("x","z")))
