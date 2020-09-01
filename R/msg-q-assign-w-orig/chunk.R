# distChunk methods

jobID.distChunk <- function(x, ...) get("JOB_ID", x)

chunkID.distChunk <- function(x, ...) {
	if (! exists("CHUNK_ID", x)) { 
		jID <- jobID(x)
		cat("chunkID not yet associated with distChunk; checking jobID ", jID, "\n")
		cID <- chunkID(read.queue(jID, clear=TRUE))
		cat("chunkID \"", format(cID), "\" found; associating...\n")
		chunkID(x) <- cID
	}
	get("CHUNK_ID", x)
}

chunkDo.distChunk <- function(what, x, assign=TRUE, wait=FALSE) {
	jID <- jobID()
	cat("Request to perform function ", format(what), " on distChunk ",
	    chunkID(x), " with assignment: ", format(assign), "\n")
	send(OP = if (assign) "ASSIGN" else "DOFUN", FUN = what, CHUNK = x, 
		JOB_ID = jID, to = chunkID(x))
	# object creation
	dc <- if (assign) {
		if (!wait){
			cat("not waiting, using job ID", format(jID), "\n")
			distChunk(jID) 
		} else {
			cat("waiting...\n")
			distChunk(chunkID(read.queue(jID, clear=TRUE)))
	} } else {
		cat("taking the value\n")
		val(read.queue(jID, clear=TRUE))
	}
	dc	
}

format.distChunk <- function(x, ...) {
	c <- chunkDo(identity, x, assign=FALSE)
	format(c)
}
