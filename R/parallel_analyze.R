
# read command line arguments
# 1st should be name of RDS file containing a character array of packages
#     to install, and
# 2nd should be no. of cores to take advantage of for parallelism
args <- commandArgs(trailingOnly=TRUE)

package_names <- readRDS(args[1])
num_cores <- as.numeric(args[2])

require(typethat)
require(doMC)
require(foreach)

typethat::prep_for_parallel(num_cores)

capture <- foreach (i = 1:length(package_names)) %dopar% {
  tryCatch({
    usePackage(package_names[i])
    typethat::type_a_package(package_names[i])
  }, error = function(e) {
    # something failed, lets just continue
    print("Top level error dealing with package: ")
    print(e)
  })
}
