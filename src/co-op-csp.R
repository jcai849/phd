main <- function() {

    server <- channel()
    storage <- channel()
    waiting_worker <- channel()
    complete_worker <- channel()
    spawn(routine=serve, channel=server)
    spawn(routine=store, channel=storage)
    repeat {
        select(
            server = function(request) {
                spawn(routine=work, channel=waiting_worker)
                send(channel=waiting_worker, value=c(request, complete_worker))
            },
            complete_worker = function(resolution) {
                send(channel=storage, value=value(resolution))
                if (response_needed(resolution))
                    send(channel=return_address(resolution),
                         value=value(resolution))
            }
        )
    }
}

serve <- function(channel) {
    outside_world <- socket_init()
    repeat send(channel, receive(outside_world))
}

work <- function(channel) {
    send_to_emerger <- function(x) {
        emerger <- channel()
        spawn(routine=emerge, channel=emerger)
        send(channel=emerger, value=x)
        emerger
    }
    request_and_main <- receive(channel)
    request <- request_and_main[[1]]
    main <- request_and_main[[2]]
    prereq_count <- length(prerequisites(request))
    emergers <- lapply(prerequisites(request), send_to_emerger)
    prereqs <- select(list=emergers)
    result <- do.call(computation(request), prereqs)
    send(main, result)
}

store <- function(channel) {
    storage <- new.env()
    repeat {
        item <- receive(channel)
        assign(id(item), value(item), storage)
    }
}

emerge <- function(channel) {
    item <- receive(channel)
    send(channel, GET(id(item), address(item)))
}
