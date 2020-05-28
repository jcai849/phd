do_servers <- function(command) 
	function(hosts) 
		lapply(hosts, function(host)
				       system(paste("ssh", host, command),
					      wait = FALSE))

start_servers <- do_servers("R CMD Rserve --vanilla")
kill_servers <- do_servers("killall Rserve")
connect_servers <- function(hosts) sapply(hosts, 
					  function(host) RS.connect(host),
					  simplify = FALSE,
					  USE.NAMES = TRUE)

make_cluster <- function(hosts){
	servers <- start_servers(hosts)

