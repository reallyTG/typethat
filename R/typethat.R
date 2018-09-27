# # #
# TODO Sept 21st 2018
#
# [1] Best way to automate?
# [1.1] Is there a cluster/supercomputer/server/something like that?
# [1.2] Need the code to be more robust, some packages don't actually work
#       that well, as in the exported data might be ill-formed, or the
#       analysis might not work so well on the exported data.
# [1.2.1] One issue is with needing to eval arguments to figure out their type.
# [2] Do we want a different trace/decorator, or more instrumentation?
# [2.1] Right now the decoration is only happening on.exit(). Do we need a more
#       granular analysis?
# [3] Should we start thinking about a possible type system?
# [3.1] How would it fit with Julia's?
#
# A paper looking at arguments of functions:
# Dynamic analysis of types of functions Python?
#
# How many functions are monomorphic? Only one type signature.
# Can we typecheck monomorphic functions? Need tool to analyze the code.
#
# For polymorphic functions, what do we need in the TS to capture them with
# some elegance.
#
# If we had an infinite TS, what would go into it?
#
# Data frames themselves probably have a more complicated type. Can go deeper
# that the surface tag.
#
# # #


# typethat usage...
# result <-
#   type_from_package(
#     "Rvmmin",
#     types="all",
#     output_dir="tmp"
#   )

#' Extract function argument type information from a specified package.
#'
#' @export
#
type_from_package <- function(pkgs_to_trace, pkgs_to_run=pkgs_to_trace,
                             types=c("examples", "tests", "vignettes", "all"),
                             filter=NULL,
                             quiet=TRUE,
                             skipgen=FALSE,
                             ...) {

  # commented out for now, can think about how this should work.
  # throwing out trace data is not ideal for big programs
  #
  # first, set up directory structure
  # if directory doesnt exist, make it:
  # dir.create("tmp", showWarnings=FALSE)

  # if it did exist, clean it:
  # unlink("./tmp/*")

  # first, generate the trace results
  # cant prune if exporting

  cat(" > Generating ... \n")

  if (!skipgen) {
    genthat::gen_from_package( pkgs_to_trace, pkgs_to_run,
                      types, action="export", filter, prune_tests=FALSE, quiet,
                      output_dir=paste("tmp/", pkgs_to_trace, sep=""), ...)
  }

  cat(" > Typing ... \n")

  # next, analyze the results
  # > first, get list of files
  lof <- list.files(paste("./tmp/", pkgs_to_trace, "/", sep=""), recursive=TRUE)

  # > second, look at the list of files, and tally up the
  #   results of the trace
  if (length(lof) > 1) {
    # there are files to analyze
    res <- type_trace_all_tally( lof, paste("./tmp/", pkgs_to_trace, "/", sep=""))
  } else {
    # there are no files
  }
}


#' Extract type information from many packages, and consolidate information.
#' e.g. how many functions have an argument with more than one type?
#' @param package_names the list of package names to type.
#' @param clean should we clean up the type_res directory?
#' @export
# type all packages in directory, tally results
# TODO: make this not install
#
type_all_packages <- function(package_names, clean=FALSE) {

  # tell genthat where to find the package sources
  options(genthat.source_paths="packages")

  # install and uninstall package names

  # first, set up directory structure
  # if directory doesnt exist, make it:
  dir.create("type_res", showWarnings=FALSE)

  # if it did exist, clean it:
  if (clean)
    unlink("./type_res/*")

  for (i in 1:length(package_names)) {
    # is this the best way?
    # get package stuff

    # package installation might fail. if it does, we should skip this package

    tryCatch({

      cat(".\n.\n.\n > Loading Package ... \n.\n.\n.\n")

      use_package(package_names[i])

      cat(".\n.\n.\n > Package Loaded \n > Typing Package ... \n.\n.\n.\n")

      # do the thing
      this_res <- type_from_package(package_names[i])

      cat(".\n.\n.\n > Package Typed \n > Writing, and continuing \n.\n.\n.\n")

      if (!is.null(this_res)) {
        # write(this_res, paste("./type_res/result_", package_names[i], sep=""))
        saveRDS(this_res, paste("./type_res/result_", package_names[i], ".RDS", sep=""))
      } else {
        # something bad happened with the package, but thats not our fault
      }

      # remove the package? dont want it to take up space
      # remove.packages(package_names[i])
    }, error = function(e) {
      # something failed, lets just continue
    })
  }
}

# TODO Change name, masked from devtools
# for downloading packages properly
# adapted from http://www.salemmarafi.com/code/install-r-package-automatically/
#' @export
use_package <- function(p)
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE, repos="https://cloud.r-project.org")
  require(p, character.only = TRUE)
}

#' Analyze all .RDS files (output from type_all_packages) in a specified dir.
#' NOTE: only caring about typeof at this time
#' @export
#
analyze_all <- function(path) {

  # get files to ... get
  # this ought to be type_res
  lof <- list.files(path)

  # get names from lof
  package_names <- c()

  q <- 1
  for (f in lof) {
    # 8 is the index of the character following result_
    # nchar(f) - 4 to get rid of .RDS
    package_names[q] <- substring(f, 8, nchar(f) - 4)
    q = q + 1
  }

  # prepare output as a list
  res <- list()

  res$num_mono = 0
  res$num_mono_opt = 0
  res$num_poly_simpl = 0
  res$num_poly_simpl_opt = 0
  res$num_poly_compl = 0
  res$num_poly_compl_opt = 0
  res$num_poly_compl_other_opt = 0

  poly_compls <- list()

  for (i in 1:length(lof)) {
    # look @ the file, do analysis
    look_at_me <- readRDS(paste(path, lof[i], sep="/"))
    one_look <- analyze_type_information(look_at_me)

    # get counts
    simpler <- simplify_analysis(one_look)
    counts <- simpler[[1]]
    polycs <- simpler[[2]]
    if (length(polycs) != 0) {
      for (j in 1:length(polycs)) {
        polycs[[j]]$pkg_name <- package_names[i]
      }
    }

    res$num_mono = res$num_mono + counts$mono
    res$num_mono_opt = res$num_mono_opt + counts$mono_o
    res$num_poly_simpl = res$num_poly_simpl + counts$polys
    res$num_poly_simpl_opt = res$num_poly_simpl_opt + counts$polys_o
    res$num_poly_compl = res$num_poly_compl + counts$polyc
    res$num_poly_compl_opt = res$num_poly_compl_opt + counts$polyc_o
    res$num_poly_compl_other_opt = res$num_poly_compl_other_opt + counts$polyc_o_o

    poly_compls <- c(poly_compls, polycs)
  }

  list(res, poly_compls)

}

#' @export
simplify_analysis <- function(analysis) {

  res <- list()

  num_mono <- 0
  num_mono_opt <- 0
  num_poly_simpl <- 0
  num_poly_simpl_opt <- 0
  num_poly_compl <- 0
  num_poly_compl_opt <- 0
  num_poly_compl_w_other_opt <- 0

  poly_compl_usage <- list()

  p <- 1
  for (i in 1:length(analysis)) {
    if (analysis[[i]][[1]]$morphicity == "monomorphic") { # 1 for type
      num_mono = num_mono + 1
    } else if (analysis[[i]][[1]]$morphicity == "polymorphic") {
      if (analysis[[i]][[1]]$polymorphicity == "simple polymorphic") {
        num_poly_simpl = num_poly_simpl + 1
      } else if (analysis[[i]][[1]]$polymorphicity == "optional simple polymorphic") {
        num_poly_simpl_opt = num_poly_simpl_opt + 1
      } else if (analysis[[i]][[1]]$polymorphicity == "optional monomorphic") {
        num_mono_opt = num_mono_opt + 1
      } else {
        # it's complicated, we want the usage always but count as appropriate
        if (analysis[[i]][[1]]$polymorphicity == "complex polymorphic") {
          num_poly_compl = num_poly_compl + 1
        } else if (analysis[[i]][[1]]$polymorphicity == "optional complex polymorphic") {
          num_poly_compl_opt = num_poly_compl_opt + 1
        } else {
          num_poly_compl_w_other_opt = num_poly_compl_w_other_opt + 1
        }

        poly_compl_usage[[p]] <- analysis[[i]][[1]]$usage
        poly_compl_usage[[p]] <- c(poly_compl_usage[[p]], name=names(analysis)[i])
        p <- p + 1
      }
    }
  }

  res$mono = num_mono
  res$mono_o = num_mono_opt
  res$polys = num_poly_simpl
  res$polys_o = num_poly_simpl_opt
  res$polyc = num_poly_compl
  res$polyc_o = num_poly_compl_opt
  res$polyc_o_o = num_poly_compl_w_other_opt


  list(res, poly_compl_usage)
}

#' analyze_type_information takes a tally and lists some things about it:
#' [1] how many easy monomorphic, hard monomorphic, and polymorphic functions
#'     are there
#' [2] for easy monomorphic functions, suggestions for annotation
#'
#' Types of ``morphicity'':
#' monomorphic
#' optional monomorphic
#' simple polymorphic
#' optional simple polymorphic
#' complex polymorphic
#' optional polymorphic
#' complex polymorphic with other optional argument
#'
#' @param tally the tally to analyze, use readRDS on type_from_package output
#' @export
#
analyze_type_information <- function(tally) {
  # tally[[1]] is all types w/ boxes per fun
  # tally[[2]] is all classes w/ " " "
  # tally[[3]] is all modes w/ " " "
  # tally[[4]] is all storage.modes w/ " " "

  # to simplify, lets grab the function names
  fun_names <- names(tally[[1]])

  res <- list()
  which_one <- c("type") # , "class", "mode", "storage.mode")

  # these are the easy polymorphic types (and classes, modes)
  # baked together in this delicious array
  simple_polymorphic_types <- c("integer", "double", "numeric", "complex")

  # how many are monomorphic?
  for (q in 1:length(which_one)) {
    for (i in 1:length(fun_names)) {

      if (length(tally[[q]][[i]]) == 0) {
        # single argument function
        # TODO: must be monomorphic ... ?
        res[[fun_names[i]]][[which_one[q]]]$morphicity <- "monomorphic"
        next
      }

      # count up arguments. if more arguments than list, its polymorphic
      arg_count <- 0
      for (j in 1:length(tally[[q]][[i]])) {
        for (k in 1:length(tally[[q]][[i]][[j]])) {
          arg_count <- arg_count + 1
        }
      }

      if (arg_count == length(tally[[q]][[i]])) {
        res[[fun_names[i]]][[which_one[q]]]$morphicity <- "monomorphic"
      } else {
        res[[fun_names[i]]][[which_one[q]]]$morphicity <- "polymorphic"
        res[[fun_names[i]]][[which_one[q]]]$usage <- tally[[q]][[i]]

        # we can say a bit more about polymorphic functions.
        arg_polyc <- list()
        ap_iter <- 1

        for (j in 1:length(tally[[q]][[i]])) {

          is_null <- FALSE

          # look @ all argument lists which have more than 1 type
          if (length(tally[[q]][[i]][[j]]) > 1) {

            # check for NULL
            if ("NULL" %in% tally[[q]][[i]][[j]]) {
              is_null <- TRUE
              # remove it for now
              for (z in 1:length(tally[[q]][[i]][[j]])) {
                if (tally[[q]][[i]][[j]][[z]] == "NULL") {
                  # remove the NULL
                  # tally[[q]][[i]][[j]][[z]] <- NULL
                  # need list.remove
                  tally[[q]][[i]][[j]] <- list.remove(tally[[q]][[i]][[j]], c(z, z))
                  break
                }
              }

              # check the len now
              if (length(tally[[q]][[i]][[j]]) == 1) {
                # it was just an optional argument
                arg_polyc[ap_iter] <- "optional monomorphic"
                ap_iter = ap_iter + 1
                next
              }
            }

            inter <- intersect(tally[[q]][[i]][[j]], simple_polymorphic_types)
            if (length(inter) == length(tally[[q]][[i]][[j]])) {
              if (is_null) {
                arg_polyc[ap_iter] <- "optional simple polymorphic"
              } else {
                arg_polyc[ap_iter] <- "simple polymorphic"
              }
              ap_iter = ap_iter + 1
            } else {
              # ok, not just numerics
              if (is_null) {
                arg_polyc[ap_iter] <- "optional complex polymorphic"
              } else {
                arg_polyc[ap_iter] <- "complex polymorphic"
              }
              ap_iter = ap_iter + 1
            }
          }
        }

        # ok, time to judge the function
        # look at arg_polyc
        if (length(arg_polyc) == 1) {
          res[[fun_names[i]]][[which_one[q]]]$polymorphicity <- arg_polyc[1]
        } else {
          # here, we need to distill arg_polyc down into a single statement
          if ("optional complex polymorphic" %in% arg_polyc) {
            # at least one argument is complex, and optional
            res[[fun_names[i]]][[which_one[q]]]$polymorphicity <- "optional complex polymorphic"
          } else if ("complex polymorphic" %in% arg_polyc) {
            # check if any are optional
            found_optional <- FALSE
            for (p in 1:length(arg_polyc)) {
              if (regexpr("optional", arg_polyc[p]) > -1) {
                # there's an optional argument
                res[[fun_names[i]]][[which_one[q]]]$polymorphicity <- "complex polymorphic with other optional argument"
                found_optional <- TRUE
                break
              }
            }
            if (!found_optional) {
              res[[fun_names[i]]][[which_one[q]]]$polymorphicity <- "complex polymorphic"
            } # no else, since we already set it
          } else if ("simple optional polymorphic" %in% arg_polyc) {
            # it's not optional complex polymorphic, and it's not complex polymorphic either, so it's either
            # simple optional or simple w/o option
            res[[fun_names[i]]][[which_one[q]]]$polymorphicity <- "simple optional polymorphic"
          } else if ("simple polymorphic" %in% arg_polyc) {
            # at this point, it has to be simple
            res[[fun_names[i]]][[which_one[q]]]$polymorphicity <- "simple polymorphic"
          } else if ("optional monomorphic" %in% arg_polyc) {
            res[[fun_names[i]]][[which_one[q]]]$polymorphicity <- "optional monomorphic"
          } else {
            # this should never be reached
            res[[fun_names[i]]][[which_one[q]]]$polymorphicity <- "weird?"
          }
        }
      }
      # make sure to tag the usage w/ the polymorphicity
      res[[fun_names[i]]][[which_one[q]]]$usage$polymorphicity <-
      res[[fun_names[i]]][[which_one[q]]]$polymorphicity
    }
  }
  res
}

#' print_tally_results will display the type, class, mode, and storage.mode
#' of all arguments of all functions in a specified tally. It is intended to
#' be human-readable.
#' @param tally the tally to print
#' @export
#
print_tally_results <- function(tally) {

  fnames <- names( tally[[1]])

  for (i in 1:length(fnames)) {
    name <- fnames[i]
    cat(
      "------------------------\n",
      "Function:",
      name,
      "\n"
    )

    if (length(names(tally[[1]][[i]])) == 1) {
      cat("> No argument function.\n")
      # don't skip b/c return value
    }

    cat(
      "> Types\n"
    )

    for (j in 1:length(names(tally[[1]][[i]]))) {
      if (j == length(names(tally[[1]][[i]]))) {
        cat(
          "[Return value]: ",
          string_for_possibly_array( tally[[1]][[i]][[j]]),
          "\n",
          sep=""
        )
      } else {
        cat(
          "Argument [",
          names(tally[[1]][[i]])[j],
          "]: ",
          string_for_possibly_array( tally[[1]][[i]][[j]]),
          "\n",
          sep=""
        )
      }
    }

    cat(
      "> Classes\n"
    )

    for (j in 1:length(names(tally[[2]][[i]]))) {
      if (j == length(names(tally[[1]][[i]]))) {
        cat(
          "[Return value]: ",
          string_for_possibly_array( tally[[2]][[i]][[j]]),
          "\n",
          sep=""
        )
      } else {
        cat(
          "Argument [",
          names(tally[[2]][[i]])[j],
          "]: ",
          string_for_possibly_array( tally[[2]][[i]][[j]]),
          "\n",
          sep=""
        )
      }
    }

    cat(
      "> Mode\n"
    )

    for (j in 1:length(names(tally[[3]][[i]]))) {
      if (j == length(names(tally[[1]][[i]]))) {
        cat(
          "[Return value]: ",
          string_for_possibly_array( tally[[3]][[i]][[j]]),
          "\n",
          sep=""
        )
      } else {
        cat(
          "Argument [",
          names(tally[[3]][[i]])[j],
          "]: ",
          string_for_possibly_array( tally[[3]][[i]][[j]]),
          "\n",
          sep=""
        )
      }
    }

    cat(
      "> Storage Mode\n"
    )

    for (j in 1:length(names(tally[[4]][[i]]))) {
      if (j == length(names(tally[[1]][[i]]))) {
        cat(
          "[Return value]: ",
          string_for_possibly_array( tally[[4]][[i]][[j]]),
          "\n",
          sep=""
        )
      } else {
        cat(
          "Argument [",
          names(tally[[4]][[i]])[j],
          "]: ",
          string_for_possibly_array( tally[[4]][[i]][[j]]),
          "\n",
          sep=""
        )
      }
    }
  }
}

string_for_possibly_array <- function(plos) {
  out <- ""
  for (i in 1:length(plos)) {
    out <- paste(out, plos[i], sep = "")
    if (i == length(plos)) {
      break;
    }
    out <- paste(out, ", ", sep = "")
  }
  out
}

# Garbo from analyze_type_information
# older stuff
# # this is polymorphic
# inter <- intersect(tally[[q]][[i]][[j]], simple_polymorphic_types)
# if (length(inter) == length(tally[[q]][[i]][[j]])) {
#   res[[fun_names[i]]][[which_one[q]]]$polymorphicity <- "simple polymorphic"
# } else {
#   res[[fun_names[i]]][[which_one[q]]]$polymorphicity <- "complex polymorphic"
#
#   # is there a NULL?
#   if ("NULL" %in% tally[[q]][[i]][[j]]) {
#     # ok, with NULL in there we are surely dealing with optional
#     # find it
#     for (z in 1:length(tally[[q]][[i]][[j]])) {
#       if (tally[[q]][[i]][[j]][[z]] == "NULL") {
#         # remove the NULL
#         tally[[q]][[i]][[j]][[z]] <- NULL
#         break
#       }
#     }
#     # what does the item look like now?
#     inter <- intersect(tally[[q]][[i]][[j]], simple_polymorphic_types)
