# Chunk methods

infoRef.chunk <- function(x, ...) get("ref", x)

chunkID.chunk <- function(x, ...) {
	if (! exists("ID", x)) { 
		ref <- infoRef(x)
		cat("chunkID not yet associated with chunk; checking infoRef ", ref, "\n")
		ID <- chunkID(readMsg(ref, clear=TRUE))
		cat("chunkID \"", format(ID), "\" found; associating...\n")
		chunkID(x) <- ID
	}
	get("ID", x)
}

chunkDo.chunk <- function(what, x, assign=TRUE, wait=FALSE) {
	ref <- infoRef()
	cat("Request to perform function ", format(what), " on chunk ",
	    chunkID(x), " with assignment: ", format(assign), "\n")
	sendMsg(op = if (assign) "ASSIGN" else "DOFUN", fun = what, chunk = x, 
		infoRef = ref, to = chunkID(x))
	x <- if (assign) {
		if (!wait){
			cat("not waiting, using reference", format(ref), "\n")
			chunk(ref) 
		} else {
			cat("waiting...\n")
			chunk(chunkID(readMsg(ref, clear=TRUE)))
	} } else {
		cat("taking the value\n")
		val(readMsg(ref, clear=TRUE))
	}
	x
}

format.chunk <- function(x, ...) {
	obj <- chunkDo(identity, x, assign=FALSE)
	format(obj)
}

