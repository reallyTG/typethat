# typethat usage...
# result <-
#   type_from_package(
#     "Rvmmin",
#     types="all",
#     output_dir="tmp"
#   )

# GLOBALS
test <- "test"

# Type names
poly_type_names <- list(
  single      = "singleton",
  mono        = "monomorphic",
  simpl_nume  = "simple numeric polymorphic",
  nume        = "numeric polymorphic",
  func        = "function polymorphic",
  compl       = "complex polymorphic",
  listy       = "listy",
  indexy      = "indexy"
)

func_poly_type_names <- list(
  multi_simpl   = "multiple arguments with simple polymorphism",
  multi_compl   = "multiple arguments with complex polymorphism",
  stopping      = "monomorphic with possibly NULL return value",
  single        = "singleton",
  mono          = "monomorphic", # <| these are for $morphicity
  poly          = "polymorphic", # <|
  listy         = "list-like polymorphism"
)

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

  if (!quiet)
    cat(" > Generating ... \n")

  if (!skipgen) {
    genthat::gen_from_package( pkgs_to_trace, pkgs_to_run,
                      types, action="export", filter, prune_tests=FALSE, quiet,
                      output_dir=paste("tmp/", pkgs_to_trace, sep=""), ...)
  }

  if (!quiet)
    cat(" > Typing ... \n")

  # next, analyze the results
  # > first, get list of files
  lof <- list.files(paste("./tmp/", pkgs_to_trace, "/", sep=""), recursive=TRUE)

  # > second, look at the list of files, and tally up the
  #   results of the trace
  if (length(lof) > 0) {
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
#
type_all_packages <- function(package_names, clean=FALSE, skipgen=FALSE,
                              use_rev_deps=FALSE, quiet=TRUE) {

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

      if (!quiet)
        cat(".\n.\n.\n > Loading Package: ", package_names[i], " ... \n.\n.\n.\n", sep="")

      usePackage(package_names[i])

      if (!quiet)
        cat(".\n.\n.\n > Package Loaded \n > Typing Package ... \n.\n.\n.\n")

      # do the thing
      if (use_rev_deps) {
        # list reverse dependencies
        # select default CRAN mirror, this should stop the following call from
        # prompting the user. modify this i guess if you want a different
        # mirror
        local({ r <- getOption("repos")
                r["CRAN"] <- "http://cran.r-project.org"
                options(repos=r)})

        rdeps <- tools::package_dependencies(package_names[i],
                 which=c("Depends", "Imports", "LinkingTo", "Suggests"),
                 reverse=TRUE, recursive=FALSE)
        rdeps <- unlist(rdeps, use.names=FALSE)

        if (length(rdeps) > 0) {
          for (pname in rdeps) {
            usePackage(pname)
          }
        }

        # run
        this_res <- type_from_package(package_names[i],
                                      pkgs_to_run=c(package_names[i], rdeps),
                                      skipgen=skipgen)
      } else {
        # just run
        this_res <- type_from_package(package_names[i], skipgen=skipgen)
      }

      if (!quiet)
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
      if (!quiet) {
        print("Top level error dealing with package: ")
        print(e)
      }
    })
  }
}

# |
# |
# TODO Before calling type_a_package
# install and uninstall package names
# first, set up directory structure
# if directory doesnt exist, make it:
# dir.create("type_res", showWarnings=FALSE)
# usePackage(package_names[i]) \forall i
# |
# |

#' Transform :: take a count result from an analysis, and transform it
#'   to a data.frame that can be used to plot.
#' @param ana_res the *whole* result of a call to analyze_all
#' @export
transform_results <- function(ana_res) {
  # get the counts
  counts <- ana_res[[1]]

  the_names <- names(counts)
  the_vals <- unname(unlist(counts))

  data.frame(type=the_names, num=the_vals)
}

#' Give Only Polymorphic :: take a data.frame (from transform_results) and give
#'   back a data frame where the `singleton` and `monomorphic` fields are gone.
#' @param df result from transform_results
#' @export
give_only_polymorphic <- function(df) {
  remove_me <- c()
  for (i in 1:length(df[,1])) { # for i in rows
    if (func_poly_type_names$single == df[i,1] ||
        func_poly_type_names$mono   == df[i,1]) {
      remove_me <- c(remove_me, i)
    }
  }
  df[-remove_me,]
}

#' Plot a bar chart: uses results from transform_results and give_only_polymorphic
#' @export
plot_bar <- function(df) {
  bp <- ggplot(df, aes(x=type, y=num, color=type)) +
        geom_bar(stat="identity", fill="white")

  # make the bar chart
  pie <- bp + coord_flip() + theme(legend.position="none")
}

#' Plot a pie chart: uses results from transform_results and give_only_polymorphic
#' @export
plot_pie <- function(df) {
  bp <- ggplot(df, aes(x="", y=num, fill=type)) +
        geom_bar(width=1, stat="identity")

  # make the pie chart
  pie <- bp + coord_polar("y", start=0)
}

#' Function which takes a list of package_names (for big parallel call to
#' type_a_package), and prepares for the call.
#'
#' @export
#
prep_for_parallel <- function(num_cores=1) {
  # tell genthat where to find the package sources
  options(genthat.source_paths="packages")

  # first, set up directory structure
  # if directory doesnt exist, make it:
  dir.create("type_res", showWarnings=FALSE)

  registerDoMC(num_cores)
  # gonna do this elsewhere
  # get each package
  # for (name in package_names) {
  #   usePackage(name)
  # }
}

#' Type just one package. Use this with foreach ... %dopar% if you want
#' parallelism.
#' @export
#
type_a_package <- function(package_name, clean=FALSE, skipgen=FALSE, quiet=TRUE) {

  tryCatch({

    # get do it
    this_res <- type_from_package(package_name, clean=clean, skipgen=skipgen, quiet=quiet)

    if (!is.null(this_res)) {
      saveRDS(this_res, paste("./type_res/result_", package_name, ".RDS", sep=""))
    } else {
      print("result was NULL")
    }

    # remove the package? dont want it to take up space
    # remove.packages(package_names[i])
  }, error = function(e) {
    # something failed, lets just continue
    if (!quiet) {
      print("Top level error dealing with package: ")
      print(e)
    }
  })
}

# for downloading packages properly
# adapted from http://www.salemmarafi.com/code/install-r-package-automatically/
#' @export
usePackage <- function(p)
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE, repos="https://cloud.r-project.org")
  require(p, character.only = TRUE)
}

#' Analyze all .RDS files (output from type_all_packages) in a specified dir.
#' @param path path to the results of type_all_packages (this should be something
#'             of the form */type_res)
#' @param type the type of info you want. currently either "type" or "class"
#' @export
#
analyze_all <- function(path, type="type", display="some", per_arg=FALSE) {

  if (substring(path, nchar(path)) == "/") {
    path = substring(path, 0, nchar(path)-1)
  }

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

  if (!per_arg) {

    # TODO: This has been phased out. Might be worth revisting this concept
    #       and possibly cleaning things up.

    # for (name in func_poly_type_names) {
    #   res[[name]] <- 0
    # }

    # res$num_singles = 0
    # res$num_mono = 0
    # res$num_mono_opt = 0
    # res$num_stopping = 0 # NEW
    # res$num_poly_simpl = 0 # split this into simple & complex
    # res$num_poly_simpl_opt = 0
    # res$listy = 0
    # res$indexy = 0
    # res$num_poly_compl = 0
    # res$num_poly_compl_opt = 0
    # res$num_poly_compl_other_opt = 0
  } else {

    #
    # TODO: Need a better system for this. Right now, only works b/c the lists
    #       are in the same order.

    res$num_singles = 0
    res$num_mono = 0
    res$num_mono_opt = 0
    res$num_indexy = 0
    res$num_opt_indexy = 0
    res$num_listy = 0
    res$num_opt_listy = 0
    res$num_poly_num = 0
    res$num_poly_num_opt = 0
    res$num_poly_simpl_num = 0
    res$num_poly_simpl_num_opt = 0
    res$num_poly_fun = 0
    res$num_poly_fun_opt = 0
    res$num_poly_compl = 0
    res$num_poly_compl_opt = 0

    pretty <- list(
      num_singles               = poly_type_names$single,
      num_mono                  = poly_type_names$mono,
      num_mono_opt              = paste("optional", poly_type_names$mono),
      num_indexy                = poly_type_names$indexy,
      num_opt_indexy            = paste("optional", poly_type_names$indexy),
      num_listy                 = poly_type_names$listy,
      num_opt_listy             = paste("optional", poly_type_names$listy),
      num_poly_num              = poly_type_names$nume,
      num_poly_num_opt          = paste("optional", poly_type_names$nume),
      num_poly_simpl_num        = poly_type_names$simpl_nume,
      num_poly_simpl_num_opt    = paste("optional", poly_type_names$simpl_nume),
      num_poly_fun              = poly_type_names$func,
      num_poly_fun_opt          = paste("optional", poly_type_names$func),
      num_poly_compl            = poly_type_names$compl,
      num_poly_compl_opt        = paste("optional", poly_type_names$compl)
    )
  }

  poly_compls <- list()

  for (i in 1:length(lof)) {
    # look @ the file, do analysis
    look_at_me <- readRDS(paste(path, lof[i], sep="/"))

    # this will get results per function, if we care instead about how args are
    # used only, we need something slighly different

    if (!per_arg) {
      # per function results are desired
      one_look <- analyze_type_information(look_at_me, type)

      # get counts
      simpler <- simplify_analysis(one_look, display)
      counts <- simpler[[1]]
      polycs <- simpler[[2]]
      if (length(polycs) != 0) {
        for (j in 1:length(polycs)) {
          polycs[[j]]$pkg_name <- package_names[i]
        }
      }

      count_names <- names(counts)
      for (i in 1:length(counts)) {
        if (is.null(res[[count_names[i]]])) {
          res[[count_names[i]]] <- 0
        }
        res[[count_names[i]]] <- res[[count_names[i]]] + counts[[i]]
      }

      # for (name in func_poly_type_names) {
      #   res[[name]] <- res[[name]] + counts[[name]]
      # }

      # res$num_singles = res$num_singles + counts$singles
      # res$num_mono = res$num_mono + counts$mono
      # res$num_mono_opt = res$num_mono_opt + counts$mono_o
      # res$num_stopping = res$num_stopping + counts$stopping
      # res$num_poly_simpl = res$num_poly_simpl + counts$polys
      # res$num_poly_simpl_opt = res$num_poly_simpl_opt + counts$polys_o
      # res$listy = res$listy + counts$listy
      # res$indexy = res$indexy + counts$indexy
      # res$num_poly_compl = res$num_poly_compl + counts$polyc
      # res$num_poly_compl_opt = res$num_poly_compl_opt + counts$polyc_o
      # res$num_poly_compl_other_opt = res$num_poly_compl_other_opt + counts$polyc_o_o

      poly_compls <- c(poly_compls, polycs)

    } else {
      # here, we want to get results per argument
      one_look <- analyze_argument_type_information(look_at_me, type, package_names[i])

      # count everyone up
      res$num_singles = res$num_singles + one_look$singletons
      res$num_mono = res$num_mono + one_look$monomorphics
      res$num_mono_opt = res$num_mono_opt + one_look$optional_monomorphics
      res$num_indexy = res$num_indexy + one_look$indexy
      res$num_opt_indexy = res$num_opt_indexy + one_look$optional_indexy
      res$num_listy = res$num_listy + one_look$listy
      res$num_opt_listy = res$num_opt_listy + one_look$optional_listy
      res$num_poly_num = res$num_poly_num + one_look$numeric_polymorphic
      res$num_poly_num_opt = res$num_poly_num_opt + one_look$optional_numeric_polymorphic
      res$num_poly_simpl_num = res$num_poly_simpl_num + one_look$simple_numeric_polymorphic
      res$num_poly_simpl_num_opt = res$num_poly_simpl_num_opt + one_look$optional_simple_numeric_polymorphic
      res$num_poly_fun = res$num_poly_fun + one_look$function_polymorphic
      res$num_poly_fun_opt = res$num_poly_fun_opt + one_look$optional_function_polymorphic
      res$num_poly_compl = res$num_poly_compl + one_look$complex_polymorphic
      res$num_poly_compl_opt = res$num_poly_compl_opt + one_look$optional_complex_polymorphic

      # save the complex polymorphic arguments for inspection
      poly_compls <- c(poly_compls, one_look$compl_usages)
    }
  }

  if (per_arg) {
    names(res) <- pretty
  }

  list(res, poly_compls)

}

#' only call with aggregate type analysis
#' @export
simplify_analysis <- function(analysis, display="some") {

  res <- list()
  poly_compl_usage <- list()

  # initialize all relevant fields in res to be ready for tallying
  # for (i in 1:length(analysis)) {
  #   if (analysis[[i]][[1]]$morphicity == func_poly_type_names$poly) {
  #     res[[analysis[[i]][[1]]$polymorphicity]] <- 0
  #   }
  # }

  # for (name in func_poly_type_names) {
  #   res[[name]] <- 0
  # }

  p <- 1
  for (i in 1:length(analysis)) {
    if (!analysis[[i]][[1]]$morphicity == func_poly_type_names$poly) {
      # must be single or mono
      if (is.null(res[[analysis[[i]][[1]]$morphicity]])) {
        res[[analysis[[i]][[1]]$morphicity]] <- 0
      }
      res[[analysis[[i]][[1]]$morphicity]] <- res[[analysis[[i]][[1]]$morphicity]] + 1
    } else {

      # get it in the out
      poly_compl_usage[[p]] <- analysis[[i]][[1]]$usage
      poly_compl_usage[[p]] <- c(poly_compl_usage[[p]], name=names(analysis)[i])
      p <- p + 1

      # TODO: Why do we need the [[1]] here?
      if (is.null(res[[analysis[[i]][[1]]$polymorphicity[[1]]]])) {
        res[[analysis[[i]][[1]]$polymorphicity[[1]]]] <- 0
      }
      res[[analysis[[i]][[1]]$polymorphicity[[1]]]] <- res[[analysis[[i]][[1]]$polymorphicity[[1]]]] + 1

      # for (name in func_poly_type_names) {
      #   if (analysis[[i]][[1]]$polymorphicity == name) {
      #     res[[name]] <- res[[name]] + 1
      #     break
      #   }
      # }
    }
  }

  list(res, poly_compl_usage)
}

#' Get all trace results for a particular kind of polymorphism.
#' @param lot result from analyze_all
#' @export
get_all_for_kind <- function(lot, kind) {
  out <- list()
  inde <- 1
  for (i in 1:length(lot[[2]])) {
    if (lot[[2]][[i]]$polymorphicity[[1]] == kind) {
      out[[inde]] <- lot[[2]][[i]]
      inde <- inde + 1
    }
  }
  out
}

# this function should not exist
# we ought to find a better way to do this
app <- function(alist, aitem) {
  alist[[length(alist) + 1]] <- aitem
  alist
}

analyze_argument_type_information <- function(tally, type="type", pkgname="") {
  # ^ called for each package result
  # tally[[1]] is all types w/ boxes per fun
  # tally[[2]] is all classes w/ " " "
  # tally[[3]] is all modes w/ " " "
  # tally[[4]] is all storage.modes w/ " " "

  # to simplify, lets grab the function names
  fun_names <- names(tally[[1]])

  q = 1 # type is default, in q = 1
  if (type == "class") {
    q = 2 # if class specified, get 2
  } else if (type == "mode") {
    q = 3
  } else if (type == "storage.mode") {
    q = 4
  }

  which_one = c("type", "class", "mode", "storage.mode")

  # simple numeric polymorphic: can be either indexy or numeric
  simple_numeric_polymorphic_types <- c("integer", "double")
  simple_numeric_polymorphic_classes <- c("numeric", "integer") # monomorphic in terms of class now

  # these are the easy polymorphic types (and classes, modes)
  # baked together in this delicious array
  numeric_polymorphic_types <- c("integer", "double", "complex", "logical") # "numeric" ?
  numeric_polymorphic_classes <- c("numeric", "integer", "complex", "logical")

  # also this one
  function_polymorphic_types <- c("closure", "builtin", "special")
  function_polymorphic_classes <- c("function") # monomorphic in terms of class now

  # also ...
  list_index_types <- c("integer", "double", "character")
  list_index_classes <- c("numeric", "integer", "character")

  if (q == 2) { # class
    simple_numeric_polymorphic_types  <- simple_numeric_polymorphic_classes
    numeric_polymorphic_types         <- numeric_polymorphic_classes
    function_polymorphic_types        <- function_polymorphic_classes
    list_index_types                  <- list_index_classes
  }

  # we will be producing a list of lists, each containing arguments with
  # the specified polymorphicity

  singletons <- list() #
  monomorphics <- list() #
  optional_monomorphics <- list() #
  indexy <- list() #
  optional_indexy <- list() #
  listy <- list() #
  optional_listy <- list() #
  simple_numeric_polymorphic <- list() #
  optional_simple_numeric_polymorphic <- list() #
  numeric_polymorphic <- list() #
  optional_numeric_polymorphic <- list() #
  function_polymorphic <- list() #
  optional_function_polymorphic <- list() #
  complex_polymorphic <- list() #
  optional_complex_polymorphic <- list() #

  compl_usages <- list() #

  # for each package in the function
  for (i in 1:length(fun_names)) {

    if (length(tally[[q]][[i]]) == 0) {
      next
    }

    arg_names <- names(tally[[q]][[i]])

    # process singletons first
    if (fun_names[i] %in% tally[[6]]) { # [[6]] has singleton funs
      # add each argument to singletons
      for (j in 1:length(tally[[q]][[i]])) {
        singletons[[length(singletons) + 1]] <- paste(pkgname, fun_names[i], arg_names[[j]], sep="::")
      }
      next # skip to next function
    }

    # at this point, we know that the function we're looking at isn't a singleton.
    # iterate over all arguments
    for (j in 1:length(tally[[q]][[i]])) {
      arg_usage <- tally[[q]][[i]][[j]]
      arg_name <- arg_names[[j]]
      arg_name_to_store <- paste(pkgname, fun_names[i], arg_name, sep="::")

      # deal with monomorphic
      if (length(arg_usage) == 1) {
        monomorphics[[length(monomorphics) + 1]] <- arg_name_to_store
        next # next arg
      }

      is_null <- FALSE

      # is there NULL?
      # check for NULL
      if ("NULL" %in% arg_usage) { # && arg_name != "retv") {
        is_null <- TRUE
        # remove it for now
        for (z in 1:length(arg_usage)) {
          if (arg_usage[[z]] == "NULL") {
            # remove the NULL
            arg_usage <- rlist::list.remove(arg_usage, c(z, z))
            break # move on
          }
        }

        # check the len now
        if (length(arg_usage) == 1) {
          # it was just an optional argument
          optional_monomorphics[[length(optional_monomorphics) + 1]] <- arg_name_to_store
          next # next arg
        }
      }

      # is it simple numeric?
      inter <- intersect(arg_usage, simple_numeric_polymorphic_types)
      if (length(inter) == length(arg_usage)) {
        if (is_null) {
          optional_simple_numeric_polymorphic <- app(optional_simple_numeric_polymorphic, arg_name_to_store)
        } else {
          simple_numeric_polymorphic <- app(simple_numeric_polymorphic, arg_name_to_store)
        }
        next
      }

      # is it more complex numeric?
      inter <- intersect(arg_usage, numeric_polymorphic_types)
      if (length(inter) == length(arg_usage)) {
        if (is_null) {
          optional_numeric_polymorphic <- app(optional_numeric_polymorphic, arg_name_to_store)
        } else {
          numeric_polymorphic <- app(numeric_polymorphic, arg_name_to_store)
        }
        next
      }

      # is it indexy?
      inter <- intersect(arg_usage, list_index_types)
      if (length(inter) == length(arg_usage)) {
        if (is_null) {
          optional_indexy <- app(optional_indexy, arg_name_to_store)
        } else {
          indexy <- app(indexy, arg_name_to_store)
        }
        next
      }

      # is it function-y?
      inter <- intersect(arg_usage, function_polymorphic_types)
      if (length(inter) == length(arg_usage)) {
        if (is_null) {
          optional_function_polymorphic <- app(optional_function_polymorphic, arg_name_to_store)
        } else {
          function_polymorphic <- app(function_polymorphic, arg_name_to_store)
        }
        next
      }

      # is it listy?
      # does it contain list?
      if ("list" %in% arg_usage) {
        # remove it for now
        for (z in 1:length(arg_usage)) {
          if (arg_usage[[z]] == "list") {
            # remove the list
            arg_usage <- rlist::list.remove(arg_usage, c(z, z))
            break
          }
        }

        # check the len now
        if (length(arg_usage) == 1) {
          # its just list or one other thing. FIXME:
          # there is a "good chance" that this is a list OR vector.
          # we need a smarter way to catch this
          if (is_null) {
            optional_listy <- app(optional_listy, arg_name_to_store)
          } else {
            listy <- app(listy, arg_name_to_store)
          }
          next
        }
      }

      # if we're here, it means that we haven't stumbled on anything quite yet
      # lets just call it complex, and move on
      if (is_null) {
        optional_complex_polymorphic <- app(optional_complex_polymorphic, arg_name_to_store)
      } else {
        complex_polymorphic <- app(complex_polymorphic, arg_name_to_store)
      }

      # print("Complex Polymorphic:")
      # print(arg_name_to_store)
      # print(arg_usage)
      # print("--------------------")

      usage_report <- c(arg_name_to_store)
      usage_report <- c(usage_report, tally[[q]][[i]][[j]])

      compl_usages[[length(compl_usages) + 1]] <- usage_report
    }
  }

  # return
  list(singletons=length(singletons),
  monomorphics=length(monomorphics),
  optional_monomorphics=length(optional_monomorphics),
  indexy=length(indexy),
  optional_indexy=length(optional_indexy),
  listy=length(listy),
  optional_listy=length(optional_listy),
  simple_numeric_polymorphic=length(simple_numeric_polymorphic),
  optional_simple_numeric_polymorphic=length(optional_simple_numeric_polymorphic),
  numeric_polymorphic=length(numeric_polymorphic),
  optional_numeric_polymorphic=length(optional_numeric_polymorphic),
  function_polymorphic=length(function_polymorphic),
  optional_function_polymorphic=length(optional_function_polymorphic),
  complex_polymorphic=length(complex_polymorphic),
  optional_complex_polymorphic=length(optional_complex_polymorphic),
  compl_usages=compl_usages)

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
#' @param type the type of info to get. either "type" or "class".
#' @export
#
analyze_type_information <- function(tally, type="type") {
  # tally[[1]] is all types w/ boxes per fun
  # tally[[2]] is all classes w/ " " "
  # tally[[3]] is all modes w/ " " "
  # tally[[4]] is all storage.modes w/ " " "

  # to simplify, lets grab the function names
  fun_names <- names(tally[[1]])

  res <- list()
  q = 1 # type is default, in q = 1
  if (type == "class") {
    q = 2 # if class specified, get 2
  } else if (type == "mode") {
    q = 3
  } else if (type == "storage.mode") {
    q = 4
  }

  which_one = c("type", "class", "mode", "storage.mode")

  # simple numeric polymorphic: can be either indexy or numeric
  simple_numeric_polymorphic_types <- c("integer", "double")
  simple_numeric_polymorphic_classes <- c("numeric", "integer") # monomorphic in terms of class now

  # these are the easy polymorphic types (and classes, modes)
  # baked together in this delicious array
  numeric_polymorphic_types <- c("integer", "double", "complex", "logical") # "numeric" ?
  numeric_polymorphic_classes <- c("numeric", "integer", "complex", "logical")

  # also this one
  function_polymorphic_types <- c("closure", "builtin", "special")
  function_polymorphic_classes <- c("function") # monomorphic in terms of class now

  # also ...
  list_index_types <- c("integer", "double", "character")
  list_index_classes <- c("numeric", "integer", "character")

  if (q == 2) { # class
    simple_numeric_polymorphic_types  <- simple_numeric_polymorphic_classes
    numeric_polymorphic_types         <- numeric_polymorphic_classes
    function_polymorphic_types        <- function_polymorphic_classes
    list_index_types                  <- list_index_classes
  }

  # how many are monomorphic?
  for (i in 1:length(fun_names)) {

    if (fun_names[i] %in% tally[[6]]) { # [[6]] has singleton funs
      # if its not null, its true by construction
      res[[fun_names[i]]][[which_one[q]]]$morphicity <- func_poly_type_names$single
      next
    }

    if (length(tally[[q]][[i]]) == 0) {
      # single argument function
      res[[fun_names[i]]][[which_one[q]]]$morphicity <- func_poly_type_names$mono
      next
    }

    # count up arguments. if more arguments than list, its polymorphic
    arg_count <- 0
    for (j in 1:length(tally[[q]][[i]])) {
      for (k in 1:length(tally[[q]][[i]][[j]])) {
        arg_count <- arg_count + 1
      }
    }
    # TODO: This needs to change depending on if the user specified "type" or
    # "class" or wte. Right not it only works (well) for "type".
    if (arg_count == length(tally[[q]][[i]])) {
      res[[fun_names[i]]][[which_one[q]]]$morphicity <- func_poly_type_names$mono
    } else {
      res[[fun_names[i]]][[which_one[q]]]$morphicity <- func_poly_type_names$poly
      res[[fun_names[i]]][[which_one[q]]]$usage <- tally[[q]][[i]]

      # we can say a bit more about polymorphic functions.
      arg_polyc <- list()
      ap_iter <- 1

      skipForStopping <- FALSE

      if (arg_count == length(tally[[q]][[i]]) + 1) {
        # here, arg_count is one more than the length, meaning only one argument
        # is polymorphic. If it's retv, and retv has NULL, we have stopping
        # monomorphism.
        for (j in 1:length(tally[[q]][[i]])) {
          arg_name <- attributes(tally[[1]][[i]])$names[j]
          if (arg_name == "retv") {
            # its the return val
            if (length(tally[[q]][[i]][[j]]) == 2) {
              res[[fun_names[i]]][[which_one[q]]]$polymorphicity <- func_poly_type_names$stopping
              res[[fun_names[i]]][[which_one[q]]]$usage$polymorphicity <-
              res[[fun_names[i]]][[which_one[q]]]$polymorphicity
              skipForStopping <- TRUE
              break
            }
          }
        }
      }

      if (skipForStopping)
        next

      for (j in 1:length(tally[[q]][[i]])) {

        is_null <- FALSE

        # look @ all argument lists which have more than 1 type
        if (length(tally[[q]][[i]][[j]]) > 1) {

          # don't care about optionality for retv
          arg_name <- attributes(tally[[1]][[i]])$names[j]

          # check for NULL
          if ("NULL" %in% tally[[q]][[i]][[j]] &&
              arg_name != "retv") { # make sure not
            is_null <- TRUE
            # remove it for now
            for (z in 1:length(tally[[q]][[i]][[j]])) {
              if (tally[[q]][[i]][[j]][[z]] == "NULL") {
                # remove the NULL
                tally[[q]][[i]][[j]] <- rlist::list.remove(tally[[q]][[i]][[j]], c(z, z))
                break
              }
            }

            # check the len now
            if (length(tally[[q]][[i]][[j]]) == 1) {
              # it was just an optional argument
              arg_polyc[ap_iter] <- paste("optional", poly_type_names$mono) # "optional monomorphic"
              ap_iter = ap_iter + 1
              next
            }
          }

          # check if the argument is "numeric"
          inter <- intersect(tally[[q]][[i]][[j]], simple_numeric_polymorphic_types)
          if (length(inter) == length(tally[[q]][[i]][[j]])) {
            if (is_null) {
              arg_polyc[ap_iter] <- paste("optional", poly_type_names$simpl_nume)# "optional simple numeric"
            } else {
              arg_polyc[ap_iter] <- poly_type_names$simpl_nume # "simple numeric"
            }
            ap_iter = ap_iter + 1
            next
          }

          # check if the argument is "numeric"
          inter <- intersect(tally[[q]][[i]][[j]], simple_polymorphic_types)
          if (length(inter) == length(tally[[q]][[i]][[j]])) {
            if (is_null) {
              arg_polyc[ap_iter] <- paste("optional", poly_type_names$nume) # "optional numeric"
            } else {
              arg_polyc[ap_iter] <- poly_type_names$nume # "numeric"
            }
            ap_iter = ap_iter + 1
            next
          }

          # check if argument is "indexy"
          inter <- intersect(tally[[q]][[i]][[j]], list_index_types)
          if (length(inter) == length(tally[[q]][[i]][[j]])) {
            if (is_null) {
              arg_polyc[ap_iter] <- paste("optional", poly_type_names$indexy) # "optional indexy"
            } else {
              arg_polyc[ap_iter] <- poly_type_names$indexy # "indexy"
            }
            ap_iter = ap_iter + 1
            next
          }

          # check if arg is function
          inter <- intersect(tally[[q]][[i]][[j]], function_polymorphic_types)
          if (length(inter) == length(tally[[q]][[i]][[j]])) {
            if (is_null) {
              arg_polyc[ap_iter] <- paste("optional", poly_type_names$func) # "optional function polymorphic"
            } else {
              arg_polyc[ap_iter] <- poly_type_names$func # "function polymorphic"
            }
            ap_iter = ap_iter + 1
            next
          }

          # check if the argument is "listy"
          # does it contain list?
          if ("list" %in% tally[[q]][[i]][[j]]) {
            # remove it for now
            for (z in 1:length(tally[[q]][[i]][[j]])) {
              if (tally[[q]][[i]][[j]][[z]] == "list") {
                # remove the list
                tally[[q]][[i]][[j]] <- rlist::list.remove(tally[[q]][[i]][[j]], c(z, z))
                break
              }
            }

            # check the len now
            if (length(tally[[q]][[i]][[j]]) == 1) {
              # its just list or one other thing. FIXME:
              # there is a "good chance" that this is a list OR vector.
              # we need a smarter way to catch this
              if (is_null) {
                arg_polyc[ap_iter] <- paste("optional", poly_type_names$listy) # "optional listy"
              } else {
                arg_polyc[ap_iter] <- poly_type_names$listy # "listy"
              }
              ap_iter = ap_iter + 1
              next
            }
          }

          # catch all for complex case
          if (is_null) {
            arg_polyc[ap_iter] <- paste("optional", poly_type_names$compl) # "optional complex polymorphic"
          } else {
            arg_polyc[ap_iter] <- poly_type_names$compl # "complex polymorphic"
          }
          ap_iter = ap_iter + 1
        }
      }

      # ok, time to judge the function
      # look at arg_polyc, if there is only one polymorphic argument then that's the
      # polymorphism of the function
      if (length(arg_polyc) == 1) {
        res[[fun_names[i]]][[which_one[q]]]$polymorphicity <- arg_polyc[1]
      } else {
        if ( paste("optional", poly_type_names$compl) %in% arg_polyc ||
             poly_type_names$compl %in% arg_polyc) {
          res[[fun_names[i]]][[which_one[q]]]$polymorphicity <- func_poly_type_names$multi_compl
        } else if (0 == length(setdiff(arg_polyc, c(poly_type_names$listy, poly_type_names$indexy,
                   paste("optional", poly_type_names$listy), paste("optional", poly_type_names$indexy))))) {
          res[[fun_names[i]]][[which_one[q]]]$polymorphicity <- func_poly_type_names$listy
        } else {
          res[[fun_names[i]]][[which_one[q]]]$polymorphicity <- func_poly_type_names$multi_simpl
        }
      }

      # } else {
      #   # here, we need to distill arg_polyc down into a single statement
      #   if ("optional complex polymorphic" %in% arg_polyc) {
      #     # at least one argument is complex, and optional
      #     res[[fun_names[i]]][[which_one[q]]]$polymorphicity <- "optional complex polymorphic"
      #   } else if ("complex polymorphic" %in% arg_polyc) {
      #     # check if any are optional
      #     found_optional <- FALSE
      #     for (p in 1:length(arg_polyc)) {
      #       if (regexpr("optional", arg_polyc[p]) > -1) {
      #         # there's an optional argument
      #         res[[fun_names[i]]][[which_one[q]]]$polymorphicity <- "complex polymorphic with other optional argument"
      #         found_optional <- TRUE
      #         break
      #       }
      #     }
      #     if (!found_optional) {
      #       res[[fun_names[i]]][[which_one[q]]]$polymorphicity <- "complex polymorphic"
      #     } # no else, since we already set it
      #   } else if ("simple optional polymorphic" %in% arg_polyc) {
      #     # it's not optional complex polymorphic, and it's not complex polymorphic either, so it's either
      #     # simple optional or simple w/o option
      #     res[[fun_names[i]]][[which_one[q]]]$polymorphicity <- "simple optional polymorphic"
      #   } else if ("optional numeric" %in% arg_polyc) {
      #     res[[fun_names[i]]][[which_one[q]]]$polymorphicity <- "optional numeric polymorphic"
      #   } else if ("numeric" %in% arg_polyc) {
      #     res[[fun_names[i]]][[which_one[q]]]$polymorphicity <- "numeric polymorphic"
      #   } else if ("optional simple numeric" %in% arg_polyc) {
      #     res[[fun_names[i]]][[which_one[q]]]$polymorphicity <- "optional simple numeric polymorphic"
      #   } else if ("simple numeric" %in% arg_polyc) {
      #     res[[fun_names[i]]][[which_one[q]]]$polymorphicity <- "simple numeric polymorphic"
      #   } else if ("optional monomorphic" %in% arg_polyc) {
      #     res[[fun_names[i]]][[which_one[q]]]$polymorphicity <- "optional monomorphic"
      #   } else if ("listy" %in% arg_polyc) {
      #     res[[fun_names[i]]][[which_one[q]]]$polymorphicity <- "listy"
      #   } else if ("indexy" %in% arg_polyc) {
      #     res[[fun_names[i]]][[which_one[q]]]$polymorphicity <- "indexy"
      #   } else {
      #     # this should never be reached
      #     res[[fun_names[i]]][[which_one[q]]]$polymorphicity <- "weird?"
      #   }
      # }
    }
    # make sure to tag the usage w/ the polymorphicity
    res[[fun_names[i]]][[which_one[q]]]$usage$polymorphicity <-
    res[[fun_names[i]]][[which_one[q]]]$polymorphicity
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

# Old stuff from simplify_analysis
# res <- list()
#
# num_singles <- 0
# num_mono <- 0
# num_mono_opt <- 0
# num_stopping <- 0
# num_nume <- 0
# num_nume_opt <- 0
# num_nume_simpl <- 0
# num_nume_simpl_opt <- 0
# num_func_poly <- 0
# num_func_poly_opt <- 0
# num_listy <- 0
# num_indexy <- 0
# num_poly_compl <- 0
# num_poly_compl_opt <- 0
# num_poly_compl_w_other_opt <- 0
#
# poly_compl_usage <- list()
#
# p <- 1
# for (i in 1:length(analysis)) {
#   if (analysis[[i]][[1]]$morphicity == "singleton") {
#     num_singles = num_singles + 1
#   } else if (analysis[[i]][[1]]$morphicity == "monomorphic") { # 1 for type
#     num_mono = num_mono + 1
#   } else if (analysis[[i]][[1]]$morphicity == "polymorphic") {
#     if (analysis[[i]][[1]]$polymorphicity == "simple polymorphic") {
#       num_poly_simpl = num_poly_simpl + 1
#     } else if (analysis[[i]][[1]]$polymorphicity == "optional simple polymorphic") {
#       num_poly_simpl_opt = num_poly_simpl_opt + 1
#     } else if (analysis[[i]][[1]]$polymorphicity == "optional monomorphic") {
#       num_mono_opt = num_mono_opt + 1
#     } else if (analysis[[i]][[1]]$polymorphicity == "listy") {
#       num_listy = num_listy + 1
#     } else if (analysis[[i]][[1]]$polymorphicity == "indexy") {
#       num_indexy = num_indexy + 1
#     } else {
#       # it's complicated, we want the usage always but count as appropriate
#       if (analysis[[i]][[1]]$polymorphicity == "complex polymorphic") {
#         num_poly_compl = num_poly_compl + 1
#       } else if (analysis[[i]][[1]]$polymorphicity == "stopping monomorphic") {
#         num_stopping = num_stopping + 1
#       } else if (analysis[[i]][[1]]$polymorphicity == "optional complex polymorphic") {
#         num_poly_compl_opt = num_poly_compl_opt + 1
#       } else {
#         num_poly_compl_w_other_opt = num_poly_compl_w_other_opt + 1
#       }
#
#       if (display == "some") {
#         poly_compl_usage[[p]] <- analysis[[i]][[1]]$usage
#         poly_compl_usage[[p]] <- c(poly_compl_usage[[p]], name=names(analysis)[i])
#         p <- p + 1
#       }
#     }
#     if (display == "all") {
#       poly_compl_usage[[p]] <- analysis[[i]][[1]]$usage
#       poly_compl_usage[[p]] <- c(poly_compl_usage[[p]], name=names(analysis)[i])
#       p <- p + 1
#     }
#   }
# }
#
# res$singles = num_singles
# res$mono = num_mono
# res$mono_o = num_mono_opt
# res$stopping = num_stopping
# res$polys = num_poly_simpl
# res$polys_o = num_poly_simpl_opt
# res$listy = num_listy
# res$indexy = num_indexy
# res$polyc = num_poly_compl
# res$polyc_o = num_poly_compl_opt
# res$polyc_o_o = num_poly_compl_w_other_opt
#
#
# list(res, poly_compl_usage)
