#
type_trace_args <- function(a_trace) {
	for (i in 1:length(a_trace$args)) {
		print( typeof( eval( a_trace$args[[i]], a_trace$globals)))
	}
}

#
# display the types (on function exit) of arguments and return
# of a function specified in the trace [a_trace]
#
type_trace_args_display <- function(a_trace) {
	# display the function name
	cat( "traced fun: ", a_trace$fun, "\n----\n")
	cat( "args\n")
	# display argument names and types
	arg_names <- names(a_trace$args)
        for (i in 1:length(a_trace$args)) {
		the_thing <- eval( a_trace$args[[i]], a_trace$globals)
                cat( arg_names[i],
		     ": ",
		     typeof( the_thing),
		     ", ",
		     class( the_thing),
		     "\n",
			 	 sep="")
        }

	# display return type
	cat( "\nret: ", typeof( eval( a_trace$retv)),
		  	", ", class( eval( a_trace$retv)),  "\n\n", sep="")

}

#' Tally up a trace.
#' @param a_trace the trace to analyze
#' @param tally the tally to add the analysis to
#'

type_trace_args_tally <- function(a_trace) {
	# fun name
	fun_name <- a_trace$fun

	# want arg names
	arg_names <- names(a_trace$args)

  # build list of types and classes
	# result <- matrix(NA, length(arg_names), 3)
	result <- list()
  arg_types <- list()
	arg_classes <- list()
	arg_mode <- list()
	arg_storage_mode <- list()

	if (length(arg_names) > 0) {
		# there were things to look at

		for (i in 1:length(a_trace$args)) {

			# tryCatch required to handle errors caused by bad evals --
			# some packages have failing builds, and sometimes evaluating their
			# function arguments (even in the environment the function is tested
		  # in...) causes a crash, handled by this block.
			tryCatch({
				the_thing <- eval( a_trace$args[[i]], a_trace$globals)

				arg_types[[i]] <- typeof(the_thing)
				arg_classes[[i]] <- class(the_thing)
				arg_mode[[i]] <- mode(the_thing)
				arg_storage_mode[[i]] <- storage.mode(the_thing)

				result[[arg_names[i]]] <- c(arg_types[[i]], arg_classes[[i]], arg_mode[[i]], arg_storage_mode[[i]])
			}, error = function(e) {})

		}
	} else {
		# function had no arguments, nothing to do
	}

	# some tests are ill-formed, and cause runtime failures when we run them
	# one particular issue is arg_names which are blank, due to some error on the
	# test author's part. we can avoid that with this trycatch.
	result$retv <- c(	typeof(a_trace$retv), class(a_trace$retv),
										mode(a_trace$retv), storage.mode(a_trace$retv))

	# make sure function name is loaded
	attributes(result)$fun <- fun_name

	# return
	result

}

#
# compute all used arguments from a list of tallies
#
get_used_args <- function(lot) {

	# union all arg names

	used_args <- list()
	for (i in 1:length(lot)) {
		these_names <- names(lot[[i]]) # need to go 1:len-1 b/c of file_path saving
		used_args[[attributes(lot[[i]])$fun]] <- union(these_names[1:length(these_names)-1], used_args[[attributes(lot[[i]])$fun]])
	}

	# display
	used_args
}

#
# aggregate all results from a list of tallies
#
aggregate_tally_results <- function(lot) {

	# return vals
	type_agg <- list()          # type info
	class_agg <- list()         # class info
	mode_agg <- list()          # mode info
	storage_mode_agg <- list()  # storage mode info

	file_info <- list()			    # info of usage per file

	# per set of iteration tallies
	this_type_agg <- list()
	this_class_agg <- list()
	this_mode_agg <- list()
	this_storage_mode_agg <- list()

	# get the used argument names
  used_args <- get_used_args(lot)
	# we deal with return separately

	fname <- attributes(lot[[1]])$fun # get first name

	# look through lot, at each used arg, and get all types and classes
	# +1 for dumb hack...
	for (i in 1:length(lot)+1) {
		# lot[[i]]'s used args

		if (i == length(lot)+1) {
			# save last results
			type_agg[[fname]] <- this_type_agg
			class_agg[[fname]] <- this_class_agg
			mode_agg[[fname]] <- this_mode_agg
			storage_mode_agg[[fname]] <- this_storage_mode_agg

			# were out of bounds, break here
			break;
		}

		this_used <- used_args[[attributes(lot[[i]])$fun]]

		if (attributes(lot[[i]])$fun != fname) {

			type_agg[[fname]] <- this_type_agg
			class_agg[[fname]] <- this_class_agg
			mode_agg[[fname]] <- this_mode_agg
			storage_mode_agg[[fname]] <- this_storage_mode_agg

			this_type_agg <- list()
			this_class_agg <- list()
			this_mode_agg <- list()
			this_storage_mode_agg <- list()

			fname <- attributes(lot[[i]])$fun # next name
		}

		if (length(this_used) > 0) {
			# there are used arguments
			for (j in 1:length(this_used)) {

				this_type_class <- lot[[i]][[this_used[j]]]

				if (is.null(this_type_class)) {
					next;
				}

				ta_put <- union(this_type_agg[[this_used[[j]]]], this_type_class[1])
				ca_put <- union(this_class_agg[[this_used[[j]]]], this_type_class[2])
				mo_put <- union(this_mode_agg[[this_used[[j]]]], this_type_class[3])
				st_put <- union(this_storage_mode_agg[[this_used[[j]]]], this_type_class[4])

				this_type_agg[[this_used[[j]]]] <- ta_put
				this_class_agg[[this_used[[j]]]] <- ca_put
				this_mode_agg[[this_used[[j]]]] <- mo_put
				this_storage_mode_agg[[this_used[[j]]]] <- st_put

				if (is.null(file_info[[fname]][[this_used[j]]][[this_type_class[1]]])) {
					file_info[[fname]][[this_used[j]]][[this_type_class[1]]] <- list()
				}

				# file_info[[paste(fname, this_used[j], sep="_")]][[this_type_class[1]]] <-
				# union(file_info[[paste(fname, this_used[j], sep="_")]][[this_type_class[1]]], lot[[i]]$file_path)

				file_info[[fname]][[this_used[j]]][[this_type_class[1]]] <-
				union(file_info[[fname]][[this_used[j]]][[this_type_class[1]]], lot[[i]]$file_path)
			}

		} else {
			# there are not used arguments, nothing to do
		}

	}
	list( type_usage=type_agg, class_usage=class_agg,
			  mode_usage=mode_agg, storage_mode_usage=storage_mode_agg,
			  file_argument_usage=file_info)
}

#
#' tally all traces pointed to by the specified file names
#'
#' data format :
#' res[[x]][y] : res is the result
#'							  x is the test #
#'								y is param name
#' @export
type_trace_all_tally <- function(file_names, path_to_dir) {

	all_results <- list()
	fname <- ""

	# debug
	print(path_to_dir)

	for (i in 1:length(file_names)) {

		# get path to file
		f_path <- paste( path_to_dir, file_names[i], sep="")

		# read file, get trace
		this_trace <- readRDS( f_path)

		fname <- this_trace$fun

		# emit trace information
	  trace_result <- type_trace_args_tally( this_trace)
		trace_result$file_path = f_path

		all_results[[i]] <- trace_result

	}

	# all_results

	aggr_res <- aggregate_tally_results( all_results)

}

#
# analyze all traces pointed to by the specified file names
#
type_trace_all <- function(file_names, path_to_dir) {

	for (i in 1:length(file_names)) {

		# get path to file
		f_path <- paste( path_to_dir, file_names[i], sep="")

		# read file, get trace
		this_trace <- readRDS( f_path)

		# emit trace information
	  type_trace_args_display( this_trace)

	}
}
