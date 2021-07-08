dlm <- function(formula, data, weights=NULL, sandwich=FALSE) {
	stopifnot(largeScaleR::is.distObjRef(data))
	chunks <- largeScaleR::chunkRef(data)
	stopifnot(length(chunks) > 0L)
	dblm <- dbiglm(formula, chunks[[1]], weights, sandwich)
	if (length(chunks) != 1L)
		largeScaleR::dreduce(f="biglm::update.biglm",
				     x=largeScaleR::distObjRef(chunks[-1]),
				     init=dblm)
	else init
}

dbiglm <- function(formula, data, weights=NULL, sandwich=FALSE) {
	stopifnot(largeScaleR::is.chunkRef(data))
	sys.call <- largeScaleR::currCallFun(-1)
	largeScaleR::do.ccall(what="biglm::biglm",
			      args=list(formula=largeScaleR::envBase(formula),
					data=data,
					weights=largeScaleR::envBase(weights),
					sandwich=sandwich),
			      target=data,
			      insert=list(sys.call=largeScaleR::envBase(sys.call)))
}
