# Chunk methods

infoRef.distChunk <- function(x, ...) get("ref", x)

chunkID.distChunk <- function(x, ...) {
	if (! exists("ID", x)) { 
		ref <- infoRef(x)
		cat("chunkID not yet associated with distChunk; checking infoRef ", ref, "\n")
		ID <- chunkID(read.queue(ref, clear=TRUE))
		cat("chunkID \"", format(ID), "\" found; associating...\n")
		chunkID(x) <- ID
	}
	get("ID", x)
}

chunkDo.distChunk <- function(what, x, assign=TRUE, wait=FALSE) {
	ref <- infoRef()
	cat("Request to perform function ", format(what), " on distChunk ",
	    chunkID(x), " with assignment: ", format(assign), "\n")
	send(op = if (assign) "ASSIGN" else "DOFUN", fun = what, chunk = x, 
		infoRef = ref, to = chunkID(x))
	x <- if (assign) {
		if (!wait){
			cat("not waiting, using reference", format(ref), "\n")
			distChunk(ref) 
		} else {
			cat("waiting...\n")
			distChunk(chunkID(read.queue(ref, clear=TRUE)))
	} } else {
		cat("taking the value\n")
		val(read.queue(ref, clear=TRUE))
	}
	x
}

format.distChunk <- function(x, ...) {
	obj <- chunkDo(identity, x, assign=FALSE)
	format(obj)
}
