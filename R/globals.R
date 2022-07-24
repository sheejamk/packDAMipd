## Required to avoid the note/warning with RMD check "no visible binding for
## global variable"
#' @importFrom utils globalVariables
utils::globalVariables(
  names = c(
    "cost_data.df", "trace_data.df", "utility_data.df",
    "table_param.df", "trial_data.df", "adl_scoring.df",
    "promis3a_scoring.df", "blank.df"
  ),
  package = "packDAMipd", add = FALSE
)

