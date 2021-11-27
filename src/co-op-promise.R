get_args <- function(request)
    promise_all( # create a promise representing the completion of all the following promises in the list
        lapply(prerequisites(request),
               function(chunk)
                       store(key=id(chunk), # cache each item under the chunk id
                             val=promise(function(resolve, reject) # create a promise representing emergence of the chunk
                                              resolve(emerge(chunk)))))) # Must run in parallel else blocks process
compute <- function(computation, args)
    store(key=id(request),
          val=then(args,
                   onFulfilled=function(value)
                           promise(function(resolve, reject)
                               tryCatch(resolve(do.call(computation(request), value)), # optimally runs in parallel
                                                error=reject))))

repeat {
    request <- nonblocking_read()
    if (anything_read(request)) {
        args <- get_args(request) # non-blocking return with a promise to a list of all args
        compute(computation, args) # returns immediately with a promise to the computation, already stored
        if (response_needed(request)) {
            then(result,
                 onFulfilled=function(value) send(address(request), value)) # returns immediately with
            }
    }
}
