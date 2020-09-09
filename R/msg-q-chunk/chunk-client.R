#### Initialisation 

library(distObj)
distInit(verbose=T)

# Clear any previous examples
rediscc::redis.rm(conn(), c("chunkRef1", paste0("C", 1:10), paste0("J", 1:10), 
		"JOB_ID", "CHUNK_ID"))

# Create new example object
chunk1 <- structure(new.env(), class = "chunkRef")
chunkID(chunk1) <- structure("chunk1", class="chunkID")
preview(chunk1) <- 1:5
resolution(chunk1) <- "RESOLVED"

chunk1

 ##### Assign Chunk Reference

x <- do.call.chunkRef(what="expm1", chunkArg=chunk1)

##### Assign Chunk Reference with Failure

y <- do.call.chunkRef("as.Date", x)

##### Local Operations while Chunks Resolve

expm1(1:10)

##### Preview of Successful Chunk

x

##### Value of Successful Chunk

do.call.chunkRef("identity", x, assign=FALSE)

##### Resolution of Unsuccessful Chunk

resolve(y)
