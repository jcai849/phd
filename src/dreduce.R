dreduce <- function(f, x, init, right = FALSE, accumulate = FALSE, ...) {
	Reduce(dreducable(f, ...), chunkRef(x), init, right, accumulate)
}

dreducable <- function(f, ...) {
	function(x, y) {
		do.ccall(f, list(x, y), target = y, ...)
	}
}
