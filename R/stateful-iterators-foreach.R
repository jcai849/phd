library(iterators)
library(foreach)
library(doParallel)

# Try a different cluster
cl <- makeCluster(2)
registerDoParallel(cl)

addsone <- function(start, to) {
	nextEl <- function(){
		start <<- start + 1
		if (start >= to) {
			stop('StopIteration')
		}
		start}
	obj <- list(nextElem=nextEl)
	class(obj) <- c('addsone', 'abstractiter', 'iter')
	obj
}

it <- addsone(1, 10)
nextElem(it)

system.time(foreach(i = addsone(1, 1000), .combine = c) %do% i)
system.time(foreach(i = addsone(1, 1000), .combine = c) %dopar% i)
