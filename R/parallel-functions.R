## Functions for installing R packages with makeCluster and parallel processing.



## Create full dependency list for CRAN packets given as a parameter vector.

# Parameters:
# pgks - Packets to be installed in vector.
# available - Matrix composed with available.packages -function from parallel -packet.
find.cran.dependencies <- function( pkgs, available ) {

	## Test if all packages can be found from available
	err <- try( utils:::.make_dependency_list(pkgs, available, recursive = TRUE) )

	if( is(err,'try-error')) {
		stop("Error: Some packages can't be found from available packages.")
		}

	## Make dependency list for packages
	dependency.list <- utils:::.make_dependency_list(pkgs, available, recursive = TRUE)
	dependency.list <- lapply(dependency.list, function(x) x[x %in% pkgs])

	combined.list <- pkgs

	## Create combined package list with dependencies and existing pkgs -list
	combined.list <- pkgs

	for( i in seq_along(dependency.list) ) {
		combined.list <- unlist( c( combined.list, dependency.list[[i]] ))
		}

	## Remove duplicates
	combined.list <- unique( combined.list )

	## If all packages in dependency list are not in pkgs -list, use recursion and add them.
	if( any( !combined.list %in% pkgs ) ) {

		## Call function recursively to get the dependencies for all packets
		dependency.list <- find.cran.dependencies( combined.list, available )
		}


	## Return dependency list
	return( dependency.list )

}


## Function to install one CRAN package on one node
cran.submit <- function(cluster, node, one.package) {
	parallel:::sendCall(cluster[[node]], install.packages, list(pkgs=one.package, repos=repo.cran), tag = one.package)
	}

## While-loop function to check if nodes & packages are ready
# and install them with submit -function.
# Used by cran.install.packages and bioc.install.packages

install.rest.of.packages <- function( dependency.list, packages.done, available.nodes, cluster, packages ) {
	while(length(packages.done) < length(packages)) {

		# Get one result for sendCall from node
		result <- parallel:::recvOneResult(cluster)

		# Add node for available workers
		available.nodes <- c(available.nodes, result$node)

		# Add package to done and remove it from DL
		packages.done <- c(packages.done, result$tag)

		# Add package to OK -list (OK if dependencies are done)
		dependencies.ok <- unlist(lapply(dependency.list, function(x) all(x %in% packages.done) ))

		# If installed.packages is still empty, start loop from beginning
		if (!any(dependencies.ok)) next

		# Get packets that has their dependencies ready
		dependencies.ready <- names(dependency.list)[dependencies.ok]

		# How many packages are ready to be installed and how many available workers
		packages.to.install <- min(length(dependencies.ready), length(available.nodes)) # >= 1

		# Use submit function for each pkg to sendCall to nodes
		if ( packages.to.install != 0 ) {
			for (i in 1:packages.to.install)
			{
				cran.submit(cluster, available.nodes[i], dependencies.ready[i])
			}
		}


		# Remove used nodes from available workers
		available.nodes <- available.nodes[-(1:packages.to.install)]

		# Remove packages which was sent to workers
		dependency.list <- dependency.list[!names(dependency.list) %in% dependencies.ready[1:packages.to.install]]

	}
}



#' Function to install multiple CRAN packages using parallel instances.
#' Parameters:
#' @param packages - Packages to be installed in vector
#' @param parallel.installs - Number of parallel installs
#' @export
install_in_parallel <- function( packages, parallel.installs ) {

	## Load needed packages
	require(parallel)
	require(methods)

  repo.cran <- "https://cloud.r-project.org"

	## Remove old logfile
	unlink("cran_install_log.txt")

	## Set up available packages and get their dependencies
	available <- available.packages()
	dependency.list <- find.cran.dependencies( packages, available )

	# if amount of nodes is smaller than packages to be installed..
	parallel.installs <- min(parallel.installs, length(names(dependency.list)))

	## Create local cluster
	cluster <- makeCluster(parallel.installs, type = "FORK", outfile = "cran_install_log.txt")

	## How many dependencies are for each package
	dependencies <- sapply(dependency.list, length)

	## Packages that are ready to be installed (have no dependencies)
	packages.ready <- names(dependency.list[dependencies == 0L])

	## Packages already installed
	packages.done <- character()

	## How many packages are ready to be installed
	waiting.packages <- length(packages.ready)

	## For loop to install n packages
	for (i in 1:min(waiting.packages, parallel.installs)) {
		cran.submit(cluster, i, packages.ready[i])
		}

	## Remove packages that are already ready to be installed
	dependency.list <- dependency.list[!names(dependency.list) %in% packages.ready[1:min(waiting.packages, parallel.installs)]]

	## Available workers
	available.nodes <- if(waiting.packages < parallel.installs) (waiting.packages+1L):parallel.installs else integer()

	## Call function to install rest of the packets
	install.rest.of.packages( dependency.list, packages.done, available.nodes, cluster, packages)

	## Stop cluster
	stopCluster(cluster)

}
