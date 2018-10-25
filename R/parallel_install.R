# Script to install a set of packages in parallel.
# Use parallel_analysis.R after to analyze the installed packages.

# determine the path of the executing script
initial.options <- commandArgs(trailingOnly = FALSE)
file.arg.name <- "--file="
script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
script.basename <- dirname(script.name)
# Make path name absolute
script.basename <- normalizePath(script.basename)


# need to make sure these files are present
source(paste(script.basename, "/smip.R", sep=""));
source(paste(script.basename, "/parallel-functions.R", sep=""));

# read command line arguments
# 1st should be name of RDS file containing a character array of packages
#     to install, and
# 2nd should be no. of cores to take advantage of for parallelism
args <- commandArgs(trailingOnly=TRUE)

package_names <- readRDS(args[1])
num_cores <- as.numeric(args[2])

#
# requirements
#
require(parallel) # v
require(methods)  # for parallel

repo.cran = "https://mirrors.nic.cz/R/"

r <- getOption("repos")
r["CRAN"] <- "https://mirrors.nic.cz/R/"
options(repos = r)

smart.install.packages(package="tcltk", mirror=repo.cran)
install_in_parallel(package_names, num_cores)
