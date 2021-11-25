# Co-operative multitasking
# Promises

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

# Async/await

# N.B. await sleep immediately prior to non-blocking io is roughly equivalent to awaits on async io

main <- async(function() repeat {
    await(async_sleep(0))
    request <- nonblocking_read()
    if (anything_read(request))
        create_task(process_request(request))
})

process_request <- async(function(request) {
    args <- await_lapply(prereqs(request), emerge)
    result <- tryCatch(do.call(computation(request), args), error=identity) # todo: parallel optimisation
    if (response_needed(request))
        send(address(request),result)
})

emerge <- async(function(id) {
    val <- NULL
    while (is.null(val)) {
        await(async_sleep(0))
        val <- get_from_local_cache()
        if (is.null(val))
            val <- non_blocking_get_from_other_host() # a la aiohttp
    }
    store(id, val)
    val
})

async_run(main())

main2 <- async(function() repeat {
    request <- await(async_read(in_sock)) # c.f. aiohttp
    if (anything_read(request))
        create_task(process_request(request))
})

emerge2 <- async(function(id) {
    val <- async_gather(async_get_from_local_cache(id),
                        async_get_from_other_host(id))
    store(id, val)
    val
})

# Communicating Sequential Processes

main <- function() {

    go(receiver)
    repeat {
        select(
            from(receiver) {
                go(worker)
                to(worker, request)
            },
            from(worker) {
                to(store, resolution)
                if (response_needed(request)) to(original, resolution)
            }
        )
    }
}

receiver <- function() {
    from(outside_world) {
        to(main)
    }
}

worker <- function(request) {
    prereq_count <- length(prerequisites(request))
    prereqs <- list()
    lapply(prerequisites(request), go(emerger))
    total_responses <- 0
    while(prereq_count < total_responses) {
    from(emerger) {
    	prereqs <- c(prereqs, emerged)
    	to(store, emerged)
    	total_responses <- total_responses + 1
        }
    }
    do.call(computation(request), prereqs)
}

store <- function() {
    storage <- new.env()
    from(main) {
        assign(id, storage)
    }
}

