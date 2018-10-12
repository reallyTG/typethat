# typethat
R Package for Dynamic Type Analysis

Oct 2018 // Work in Progress

# Goals:

1. Analyze function usage in R packages;

2. Suggest type annotations for R functions;

3. Inform R compiler of function usage to ease early optimizations;

4. Inform design of a type system for R.

# Usage:

For batch type analysis, start with the `type_all_packages` function:

```R
type_all_packages(c("Rvmmin", "unpivotr")) # you may specify multiple packages
```

Output of the type analysis can be found in the `type_res` directory (which can be found in the working directory of the R instance that ran the previous command).
You can then read individual result files with:

```R
res_rvmmin <- readRDS("type_res/result_Rvmmin.RDS")
res_unpivotr <- readRDS("type_res/result_unpivotr.RDS")
```

Batch analysis of the output files is accomplished via the following:

```R
# here, type_res is the directory containing the results
analysis_res <- analyze_all("type_res")

# to look at the "morphicity" of functions:
morphicity_counts <- analysis_res[[1]]

# to look at usage of more complicated functions:
polymorphic_functions <- analysis_res[[2]]
```

If profiles for single packages are of interest, consider the following functions:

```R
# get output of one package
res_rvmmin <- readRDS("type_res/result_Rvmmin.RDS")

# get function profiles
rvmmin_profiles <- analyze_type_information(res_rvmmin)

# for a more readable version, try:
rvmmin_simplified <- simplify_analysis(rvmmin_profiles)
```

Note: R has several notions of "type" for a given value `x`, revealed through the `typeof(x)`, `class(x)`, `mode(x)`, and `storage.mode(x)` functions.
While our later analyses currently focus on the `typeof` notion of type, our initial type analysis does produce `class`, `mode`, and `storage.mode` information, which can be found in the files in the `type_res` directory.
