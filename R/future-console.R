library(future)

multicore.console <- function(){
    get.input <- function(){
        cat("Type \"e\" to enter an expression for",
            "evaluation \nand \"r\" to see",
            "resolved expressions\n", sep="")
        readline()
    }

    send.expr <- function(){
        cat("Multicore Console> ")
        input <- readline()
        futs[[i]] <<- future(eval(str2expression(input)))
        cat("\nResolving as: ", as.character(i), "\n")
    }

    see.resolved <- function(){
        for (i in 1:length(futs)){
            if (is(futs[[i]], "Future") &
                resolved(futs[[i]])) {
                cat("Resolved: ", as.character(i), " ")
                print(value(futs[[i]]))
            }
        }
    }
    
    plan(multicore)
    futs <- list()
    i <- 1
    while(TRUE){
        input <- get.input()
        if (input == "e") {
            send.expr()
            i <- i + 1
        } else if (input == "r") {
            see.resolved()
        } else {
            cat("Try again")
        }
    }
}

multicore.console()
