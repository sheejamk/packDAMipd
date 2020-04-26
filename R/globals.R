#' Required to avoid the note/warning with RMD check "no visible binding for global variable"
#' @import utils
utils::globalVariables(
  names = c(
    "cost_data", "trace_data", "utility_data",
    "table_param", "trial_data", "adl_scoring"
  ),
  package = "packDAMipd", add = TRUE
)
