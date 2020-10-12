#######################################################################
#' Function to find the keyword for generating random numbers the  distribution
#' @param text name of the probability distribution
#' @return the keyword that should be used in R for generating random numbers
#' @examples
#' find_keyword_rand_generation("gamma")
#' @export
#' @details
#' This function returns the keyword for generating random number using
#' a keyword provided that is generally used for prob distribution
#' (but R might require a different keyword)
find_keyword_rand_generation <- function(text) {
  # text can not be null or NA
  if (is.null(text)) {
    stop("text should not be null")
  }else{
    if (is.na(text)) stop("text should not be NA")
  }
  text <- trimws(toupper(text))
  keyword <- NULL
  if (text == "LOGNORMAL" || text == "LOG NORMAL") {
    keyword <- "rlnorm"
  }
  if (text == "GAMMA") {
    keyword <- "rgamma"
  }
  if (text == "BINOMIAL") {
    keyword <- "rbinom"
  }
  if (text == "BETA") {
    keyword <- "rbeta"
  }
  if (text == "UNIFORM") {
    keyword <- "runif"
  }
  if (text == "GAUSSIAN" || text == "NORMAL") {
    keyword <- "rnorm"
  }
  if (text == "WEIBULL") {
    keyword <- "rweibull"
  }
  if (text == "EXPONENTIAL" || text == "EXPO") {
    keyword <- "rexp"
  }
  if (text == "POISSON") {
    keyword <- "rpois"
  }
  if (is.null(keyword)) {
    stop("Error - Didnt find a suitable probability distribution")
  }
  return(keyword)
}
#######################################################################
#' Function to find the parameters that determine the probability distribution
#' @param name_distri name of the probability distribution
#' @return the parameters that determine the distribution
#' @examples
#' find_required_parameter_combs("gamma")
#' @export
#' @details
#' For each of the probability distribution we require certain parameters
#' and this function provides that required list of parameters.
find_required_parameter_combs <- function(name_distri) {
  # name_distri can not be null or NA
  if (is.null(name_distri)) {
    stop("name of the distribution should not be null")
  }else{
    if (is.na(name_distri)) stop("name of the distribution should not be NA")
  }
  text <- trimws(toupper(name_distri))
  keyword <- NULL
  if (text == "LOGNORMAL" || text == "LOG NORMAL") {
    keyword <- list(c("meanlog", "sdlog"), c("mean", "sd"), c("mean", "sdlog"),
                    c("meanlog", "sd"))
  }
  if (text == "GAMMA") {
    keyword <- c("shape", "rate")
  }
  if (text == "BINOMIAL") {
    keyword <- c("size", "prob")
  }
  if (text == "BETA") {
    keyword <- c("shape1", "shape2")
  }
  if (text == "UNIFORM") {
    keyword <- c("min", "max")
  }
  if (text == "GAUSSIAN" || text == "NORMAL") {
    keyword <- c("mean", "sd")
  }
  if (text == "WEIBULL") {
    keyword <- c("shape", "scale")
  }
  if (text == "EXPONENTIAL" || text == "EXPO") {
    keyword <- c("rate")
  }
  if (text == "POISSON") {
    keyword <- c("lambda")
  }
  if (is.null(keyword)) {
    stop("Error -Didnt find a suitable probability distribution")
  }
  return(keyword)
}
#######################################################################
#' Function to return the two parameters from a given expression separated by comma,
#' @param expr an expression
#' @return parameters in the expression expr
#' @examples
#' get_name_value_probdistrb_def("gamma(mean = 10, sd =1)")
#' @export
#' @details
#' It will return the parameters of the distribution separated by commas
#' and given in usual notation as brackets. It will identify those in between
#' first occurrence of "( "and last occurrence of ")"
#' and from the characters in between search for comma to indicate different
#' parameters then it will extract (from those extracted parameters separated
#' by commas) that on the left side of "equal" sign
#' get_name_value_probdistrb_def("gamma(mean = sqrt(2), b =17)") will be ok
#' but get_name_value_probdistrb_def("gamma(shape, scale")) and
#' get_name_value_probdistrb_def("gamma(shape =1 & scale =1")) will show error
get_name_value_probdistrb_def <- function(expr) {
  # expr can not be null or NA
  if (is.null(expr)) {
    stop("expression should not be null")
  }else{
    if (is.na(expr)) stop("expression should not be NA")
  }
  # Find the position of last bracket
  posi_last <- sort(unlist(stringr::str_locate_all(expr, "\\)")))
  if (length(posi_last) == 0)
    stop("Expecting the expression with open and closed brackets")
  last <- posi_last[length(posi_last)]
  # Find the position of first bracket
  posi_first <- sort(unlist(stringr::str_locate_all(expr, "\\(")))
  first <- posi_first[1]
  # Find the parameters separated by commas that determine the distribution
  res <- stringr::str_sub(expr, start = first + 1, end = last - 1)
  # Find the position of comma
  posi <- sort(unlist(stringr::str_locate_all(res, ",")))
  expr_found <- list()
  final <- length(posi) + 1
  i <- 1
  # Now get the expressions between those commas
  while (i <= final) {
    if (i == 1) {
      start <- 1
    } else {
      start <- posi[i - 1] + 1
    }
    if (i == final) {
      end <- nchar(res)
    } else {
      if (i == 1) {
        end <- posi[i + 1] - 1
      } else {
        end <- posi[i] - 1
      }
    }
    this_param <- trimws(stringr::str_sub(res, start = start, end = end))
    expr_found <- append(expr_found, this_param)
    i <- i + 2
  }

  final2 <- length(expr_found)
  param_found <- list()
  values_found <- list()
  for (i in 1:final2) {
    # Now get the position of  equal sign and get the parameter name
    posi <- unlist(stringr::str_locate_all(unlist(expr_found[i]), "="))
    if (length(posi) != 2 || posi[1] != posi[2]) {
      stop("Error - this expression has more than one equal sign or none found")
    }
    this_param <- trimws(stringr::str_sub(expr_found[i], start = 1,
                                          end = posi[1] - 1))
    param_found <- append(param_found, this_param)
    this_value <- eval(parse(text = expr_found[i]))
    if (!is.numeric(this_value)) {
      stop("Error - the parameter = value expression could not be evaluated")
    }
    # contains the values of the parameters between the commas, but provided as
    # name value pairs
    values_found <- append(values_found, this_value)
  }
  param_found <- unlist(param_found)
  values_found <- unlist(values_found)
  return(structure(list(params = param_found, values = values_found)))
}
#######################################################################
#' Function to find the keyword for regression methods
#' @param text regression method
#' @param additional_info additional information required
#' @return the keyword that should be used in R for regression analysis
#' @examples
#' find_keyword_regression_method("linear")
#' @export
#' @details
#' This function returns the keyword to use in regression methods.
#' For example linear regression requires lm in R
#' some regression methods require additional info and it has to be
#' provided
find_keyword_regression_method <- function(text, additional_info = NA) {
  # text can not be null or NA
  if (is.null(text)) {
    stop("text should not be null")
  }else{
    if (is.na(text)) stop("text should not be NA")
  }
  text <- trimws(toupper(text))
  additional_words <- trimws(toupper(additional_info))
  keyword <- NULL
  if (text == "LINEAR REGRESSION" | text == "LINEAR") {
    keyword <- "lm"
  }
  if (text == "LOGISTIC REGRESSION" | text == "LOGISTIC") {
    keyword <- "glm"
  }
  if (text == "MULTILEVEL MODELLING" | text == "MULTILEVEL") {
    keyword <- "lmer"
  }
  if (text == "SURVIVAL" | text == "SURVIVAL ANALYSIS") {
    if (is.na(additional_info)) {
      stop("Need to provide additional info for survival analysis -
           hazard model or KM")
    }
    km_match <- c("KAPLAN-MEIER", "KM", "FLEMING-HARRINGTON", "FH2", "FH")
    index <- unlist(lapply(km_match, match, additional_words))
    if (any(!is.na(index))) {
      keyword <- "survival::survfit"
    }
    cox_match <- c("COX-PROPORTIONAL-HAZARD", "COX PROPORTIONAL HAZARD",
                   "COX-PH", "COX PH", "COXPH")
    index <- unlist(lapply(cox_match, match, additional_words))
    if (any(!is.na(index))) {
      keyword <- "survival::coxph"
    }
    pa_match <- c("PARAMETRIC REGRESSION", "PARAMETRIC")
    index <- unlist(lapply(pa_match, match, additional_words))
    if (any(!is.na(index))) {
      keyword <- "flexsurv::flexsurvreg"
    }
  }
  if (is.null(keyword)) {
    stop("No corresponding regression methods found")
  }
  return(keyword)
}
#######################################################################
#' Function to check if the parameters are sufficient to define a distribution
#' and if not see if we can estimate the parameters from the given parameters,
#' e.g. for gamma distribution, shape and rate can be estimated from mean and sd
#' @param params_found given parameters for generating random numbers
#' @param required_params the required parameter by the stats package
#' @param the_expr the expression that contain the definition
#' @param distr_key the keyword used to generate random numbers in stats package
#' @return the parameters required
#' @examples
#' check_estimate_required_params("gamma(mean = 10 ,sd = 1)", "gamma")
#' @export
#' @keywords internal
#' @details
#' if the distribution is gamma and the parameters are mean and sd
#' we can estimate shape and rate from mean and sd.
#' This function is not usually needed by the user
check_estimate_required_params <- function(the_expr, distr_key) {
  # distr_key can not be null or NA
  if (is.null(distr_key)) {
    stop("distr key should not be null")
  }else{
    if (is.na(distr_key)) stop("distr key should not be NA")
  }
  distr_key <- trimws(toupper(distr_key))
  # the_expr can not be null or NA
  if (is.null(the_expr)) {
    stop("expression should not be null")
  }else{
    if (is.na(the_expr)) stop("expression should not be NA")
  }
  names_values <- get_name_value_probdistrb_def(the_expr)
  params_found <- names_values$params
  values_found <- names_values$values
  reqd_params <- find_required_parameter_combs(distr_key)
  if (distr_key == "GAMMA") {
    if (sum(params_found == c("mean", "sd")) == 2) {
      this_shape <- (values_found[1] / values_found[2])^2
      this_rate <- values_found[1] / (values_found[2]^2)
      result <- structure(list(shape = this_shape, rate = this_rate))
      return(result)
    }else{
      stop("Error - distribution is gamma while parameters are not mean and sd")
    }
  } else {
    print("For the distributions other than gamma,the code is not equipped to
          estimate the parameters")
  }
}
#' #######################################################################
#' Function to get the expression, analyse it for what distribution it is,
#' then check if it has all parameters requires,
#' if not estimate the required parameters from given parameters,
#' and substitute it for actual expression that can
#' be used with R stats package
#' @param the_expr given parameters for generating ransom numbers
#' @return the keyword that should be used in R for generating random numbers
#' @examples
#' check_estimate_substitute_proper_params("gamma(mean = 10 ,sd=1)")
#' @export
#' @keywords internal
#' @details
#' Get the expression of probability distribution with parameters, get the
#' parameters that are found, then find the required params and if they are ok
#' if not try if they can be estimated. Then it will substitute for proper
#' parameters in the way R is expecting to perform
check_estimate_substitute_proper_params <- function(the_expr) {
  # the_expr can not be null or NA
  if (is.null(the_expr)) {
    stop("expression should not be null")
  }else{
    if (is.na(the_expr)) stop("expression should not be NA")
  }

  location <- stringr::str_locate(the_expr, "\\(")
  if (sum(!is.na(location) == 0))
    stop("Error - expression is expected to have open brackets")

  distr_key <- substr(the_expr, 1, location[1] - 1)
  rand_key <- find_keyword_rand_generation(distr_key)
  required_params <- find_required_parameter_combs(distr_key)
  params_found <- get_name_value_probdistrb_def(the_expr)$params
  index <- 0
  for (i in length(required_params)) {
    index <- match(params_found, unlist(required_params))
    if (sum(!is.na(index)) == length(index)) {
      i <- length(required_params)
    }
  }
  if (sum(is.na(index)) == length(index)) {
    required_params <- unlist(required_params)
    estim_params <- check_estimate_required_params(the_expr, distr_key)
    if (sum(is.na(estim_params)) != 0) {
      stop("Check parameters that define the probability distribution")
    }
    create_expr <- ""
    for (i in seq_len(length(required_params))) {
      create_expr <- paste(create_expr, " ", required_params[i], " = ",
                           estim_params[i], sep = "")
      if (i != length(required_params)) {
        create_expr <- paste(create_expr, ",", sep = "")
      }
    }
    the_real_expr <- paste(rand_key, "(1,", create_expr, ")", sep = "")
  } else {
    remaining <- stringr::str_split(the_expr, "\\(")[[1]][2]
    the_real_expr <- paste(rand_key, "(1, ", remaining, sep = "")
  }
  return(the_real_expr)
}
#######################################################################
#' Function to get extension of a file name
#' @param filename name of a file
#' @return the extension
#' @examples
#' get_extension_file("data.txt")
#' @export
#' @details
#' if there is no "." character returns error
#' else returns last characters those after string split using "."
get_extension_file <- function(filename) {
  ex <- strsplit(basename(filename), split = "\\.")[[1]]
  if (length(ex[-1]) == 0) {
    stop("The character '.'  was not found ")
  } else {
    return(ex[-1])
  }
}

#######################################################################
#' Function to load the file containing trial data and return it
#' @param file, name of the file in full
#' @return trial data if success, else -1
#' @examples
#' load_trial_data(system.file("extdata", "trial_data.csv",
#'   package = "packDAMipd"
#' ))
#' @export
load_trial_data <- function(file = NULL) {
  # Load trial data from file input or stored in package
  if (!is.null(file)) {
    if (IPDFileCheck::test_file_exist_read(file) == 0) {
      if (get_extension_file(file) == "txt") {
        df_trial_data <- read.table(
          file = file, header = TRUE, sep = "\t", quote = "\"",
          dec = ",", fill = TRUE, na.strings = c(""), as.is = 1:4, stringsAsFactors = FALSE
        )
      }
      if (get_extension_file(file) == "csv") {
        df_trial_data <- read.csv(file = file, header = TRUE, stringsAsFactors = FALSE, fileEncoding = "latin1")
      }
      if (get_extension_file(file) == "dta") {
        df_trial_data <- foreign::read.dta(file = file)
      }
    } else {
      stop("Error in reading given file")
    }
  } else {
    df_trial_data <- trial_data
  }
  return(df_trial_data)
}
#######################################################################
#' Make a string of covariates using addition sign
#' @param covariates covariates
#' @return expression of covariates
#' @keywords internal
#' @examples
#' make_string_covariates(c("open", "grade"))
#' @export
make_string_covariates <- function(covariates) {
  covariates_list <- list()
  if (sum(is.na(covariates)) == 0) {
    for (i in seq_len(length(covariates))) {
      if (i == length(covariates)) {
        covariates_list <- paste(covariates_list, paste(covariates[i], sep = ""))
      } else {
        covariates_list <- paste(covariates_list, paste(covariates[i], " +", sep = ""))
      }
    }
  } else {
    covariates_list <- NA
  }
  covariates_list <- trimws(covariates_list)
  return(covariates_list)
}
