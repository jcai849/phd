# N.B. await sleep immediately prior to non-blocking io is roughly equivalent to awaits on async io

main <- async(function() repeat { # this spins; see main2 for a more efficient implementation
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

emerge <- async(function(id) { # Also spinning
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

# Non-spinning; relying on some external async io functions

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
