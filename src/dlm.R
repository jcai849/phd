dlm <-
function(formula, data, weights=NULL, sandwich=FALSE) {
        stopifnot(largeScaleR::is.distObjRef(data))
        chunks <- largeScaleR::chunkRef(data)
        stopifnot(length(chunks) > 0L)
	sys.call <- largeScaleR::currCallFun()
        init <- largeScaleR::do.ccall("biglm::biglm",
                         list(formula=largeScaleR::envBase(formula),
                              data=chunks[[1]],
                              weights=largeScaleR::envBase(weights),
                              sandwich=sandwich),
                         target=chunks[[1]],
                         insert=list(sys.call=largeScaleR::envBase(sys.call)))
        if (length(chunks) != 1L)
		largeScaleR::dreduce("biglm::update.biglm",
				     largeScaleR::distObjRef(chunks[-1]), init)
        else init
}
