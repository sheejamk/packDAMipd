#######################################################################
#' Function to get extension of a filename
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
    warning("No character . found ")
  } else {
    return(ex[-1])
  }
}
#######################################################################
#' Function to add the probabilities in a vector
#' @param probs probabilities
#' @return sum of probabilities if success else -1
#' @examples
#' add_probabilities(c(0.6, 0.2))
#' @export
#' @details
#' if probabilites not numeric or fall out of range (0 and 1)
#' return error else add the probabilites together
add_probabilities <- function(probs) {
  # test if the probs are less than zero or not numeric
  test1 <- any(probs < 0) || any(probs > 1)
  test2 <- is.numeric(probs)
  if (test1 == FALSE && test2 == TRUE) {
    sum_prob <- sum(probs)
    return(sum_prob)
  } else {
    stop("Error in probability values")
  }
}
#######################################################################
#' Function to check the sum of probabilities in a vector
#' @param avector vector of probabilities
#' @return sum of probabilities if success else error
#' @examples
#' checksum_rowprob(c(0.6, 0.4))
#' @export
#' @details
#' returns the sum if the sum is 1 else error
checksum_rowprob <- function(avector) {
  if (add_probabilities(avector) == 1) {
    return(sum(avector))
  } else {
    stop("Error - row sum should be 1 in transition probability matrix")
  }
}
#######################################################################
#' Function to return random number given a distribution and parameters
#' @param prob_distbn a probability distribution and vector of parameters
#' @param param  param values to define the probability distribution
#' @return random number according to the specified distribution
#' @examples
#' random_number_prob_distbn("beta", c(0.6, 0.2))
#' @importFrom stats rbeta rgamma rlnorm rnorm runif
#' @export
#' @details
#' Returns a random number accroding to the given distribution
#' and corresponding parameters
#' For uniform distributin with no parametes, this will be std uniform
#' other wise will check if param 1 is less than param 2
#' If not for unifrom distribution, number of parameters should be minimum of two
#' and also the parameters should be numeric
random_number_prob_distbn <- function(prob_distbn, param = NULL) {
  if (is.null(param) && prob_distbn == "unif") {
    this_rand <- runif(1)
  } else {
    if (is.numeric(param) == TRUE) {
      if (length(param) >= 2) {
        if (prob_distbn == "unif") {
          if (param[1] > param[2]) {
            stop("Error - Minimum value (param[1]) should be less than maximum value (param[2])")
          } else {
            this_rand <- runif(1, param[1], param[2])
          }
        }
        if (prob_distbn == "beta") {
          this_rand <- rbeta(1, param[1], param[2])
        }
        if (prob_distbn == "gamma") {
          this_rand <- rgamma(1, param[1], param[2])
        }
        if (prob_distbn == "normal") {
          this_rand <- rnorm(1, param[1], param[2])
        }
        if (prob_distbn == "normal") {
          this_rand <- rlnorm(1, param[1], param[2])
        }
        return(this_rand)
      } else {
        stop("Error - Minimum 2 parameters required to define a probabiity distribution")
      }
    } else {
      stop("Error - Probability distribution parameters not numeric ")
    }
  }
}

#######################################################################
#' Function to return a list of parameters given
#' @param ... any parameters set of name value pairs expected
#' @return a list of parameters
#' @examples
#' define_parameters(rr = 1)
#' @export
#' @details
#' To return a list of parameters
#' For using with assign_parameters() just list or enumarate the
#' parameters, do not use c() or list() to create a data type list
define_parameters <- function(...) {
  param_list <- list(...)
  return(param_list)
}
#######################################################################
#' Function to assign the values of nested parameters from the parameter list
#' @param param_list list of parameters, some of which can be nested
#' @return list of assigned parameters
#' @examples
#' param_list <- define_parameters(
#'   cost_direct_med_A = 1701, cost_comm_care_A = 1055,
#'   cost_direct_med_B = 1774, cost_comm_care_B = 1278, cost_direct_med_C = 6948,
#'   cost_comm_care_C = 2059, cost_zido = 2456, cost_health_A = "cost_direct_med_A + cost_comm_care_A",
#'   cost_health_B = "cost_direct_med_B+ cost_comm_care_B",
#'   cost_health_C = "cost_direct_med_C + cost_comm_care_C", cost_drug = "cost_zido"
#' )
#' assign_parameters(param_list)
#' @export
#' @details
#' The parameter list should be a list of parameters in the form
#' name value pairs. If the name value pairs is given as a string it throws
#' error as in assign_parameters(c("cost_A=100", "a=10"))
#' or even if you use assign_parameters(define_parameters(c("cost_A=100", "a=10")))
#' but this will be ok if you use the below forms
#' assign_list2 <- c(a=10, cost_A = "a+100", cost_B =10)
#' assign_parameters(assign_list2) OR
#' param_list <- define_parameters(a=10, cost_A= "a+100", cost_B =10)
#' assign_list <- assign_parameters(param_list)
#' Also for nested parameters, remember to give the parameters in order so that
#' at run time, the parameters can be evaluated
#' for example, assign_list =define_parameters(cost_A="a+100", a=10)
#' assign_parameters(assign_list) will throw an error, while
#'  assign_list = define_parameters( a=10, cost_A="a+100")
#'  assign_parameters(assign_list) will successfully assign parameters
#'  as the parameters 'a' is visible before the calculation of 'cost_A'
#'  Another thing to note is that while using define_parameters, just enumaerte them,
#'  no need to create as a list by using c() or list function
assign_parameters <- function(param_list) {
  list_len <- length(param_list)
  assigned_list <- list()
  names_assigned_list <- list()
  j <- 1
  while (j <= list_len) {
    this_name <- names(param_list[j])
    this_value <- param_list[[this_name]]
    if (is.numeric(this_value)) {
      assign(names(param_list[j]), this_value)
      assigned_list <- append(assigned_list, this_value)
      names_assigned_list <- append(names_assigned_list, names(param_list[j]))
    } else {
      if (is.character(this_value)) {
        string_this_value <- toString(this_value)
        string_this_value_evalu <- eval(parse(text = string_this_value))
        if (!is.numeric(string_this_value_evalu)) {
          stop("Error - the evaluation should bring a numerical value")
        } else {
          assign(names(param_list[j]), eval(parse(text = string_this_value)))
          assigned_list <- append(assigned_list, eval(parse(text = string_this_value)))
          names_assigned_list <- append(names_assigned_list, names(param_list[j]))
        }
      }
    }
    j <- j + 1
  }
  names(assigned_list) <- names_assigned_list
  return(assigned_list)
}

#######################################################################
#' Function to return parameters with in a expression containing operators
#' @param expr an expression
#' @return parameters in the expression expr
#' @examples
#' find_parameters_btn_operators("a+b")
#' @export
#' @details
#' This function returns the parameters between the operators
#' if the state value or probabiltites are defined as expressions, we need
#' to extract the parameters and then assign
#' First the position of all operators are found and then return the parameters
#' separated by those operators
#' This happens only for one level
#' find_parameters_btn_operators("a+b") provides a and b
#' but  for find_parameters_btn_operators("mean(a,b)+b") provides mean(a,b) and b
find_parameters_btn_operators <- function(expr) {
  parsed <- toString(parse(text = expr))
  characters <- c("\\+", "-", "\\*", "/", "%%", "%/%", "\\^", "<", ">", "<=", ">=", "==", "!=", "&", "\\|", "!")
  posi <- sort(unlist(stringr::str_locate_all(parsed, characters)))
  params <- list()
  final <- length(posi) + 1
  i <- 1
  while (i <= final) {
    if (i == 1) {
      start <- 1
    } else {
      start <- posi[i - 1] + 1
    }
    if (i == final) {
      end <- nchar(parsed)
    } else {
      if (i == 1) {
        end <- posi[i + 1] - 1
      } else {
        end <- posi[i] - 1
      }
    }
    params <- append(params, trimws(stringr::str_sub(parsed, start = start, end = end)))
    i <- i + 2
  }
  params <- unlist(params)
  params_noempty_string <- params[!params == ""]
  return(params_noempty_string)
}

#######################################################################
#' Function to return parameters from a given expression separated by comma,
#' @param expr an expression
#' @return parameters in the expression expr
#' @examples
#' find_param_from_def_prob_distrbn("gamma(mean = 10, sd =1)")
#' @export
#' @details
#' It will return the parameters of the distribution separated by commas
#' and given in usual notation as brackets
#' It will identify those in betweeen first occurance of "( "and last occurance of ")"
#' and from the charcters in between seach for comma to indicate different parameters
#' then it will extract (from those extracted paramters separated by commas)
#' that on the left side of "equal" sign
#' find_param_from_def_prob_distrbn("gamma(mean = sqrt(2), b =17)") will be ok
#' but find_param_from_def_prob_distrbn("gamma(shape, scale")) and
#' find_param_from_def_prob_distrbn("gamma(shape =1 & scale =1")) will throw error
find_param_from_def_prob_distrbn <- function(expr) {
  posi_last <- sort(unlist(stringr::str_locate_all(expr, "\\)")))
  last <- posi_last[length(posi_last)]
  posi_first <- sort(unlist(stringr::str_locate_all(expr, "\\(")))
  first <- posi_first[1]
  res <- stringr::str_sub(expr, start = first + 1, end = last - 1)
  posi <- sort(unlist(stringr::str_locate_all(res, ",")))
  final <- length(posi) + 1
  expr_found <- list()
  i <- 1
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
  for (i in 1:final2) {
    posi <- unlist(stringr::str_locate_all(unlist(expr_found[i]), "="))
    if (length(posi) != 2 || posi[1] != posi[2]) {
      stop("Error - this expression has more than one equal sign or none found")
    }
    this_param <- trimws(stringr::str_sub(expr_found[i], start = 1, end = posi[1] - 1))
    param_found <- append(param_found, this_param)
  }
  param_found <- unlist(param_found)
  return(param_found)
}
#######################################################################
#' Function to find the parameters that determine the given probability distribution
#' @param name_distri name of the probability distribution
#' @return the parameters that determine the distribution
#' @examples
#' find_required_parameter_combs("gamma")
#' @export
#' @details
#' For each of the probability distribution we require certain parameters and this function provides
#' that required list of parameters.
find_required_parameter_combs <- function(name_distri) {
  text <- trimws(toupper(name_distri))
  keyword <- NULL
  if (text == "LOGNORMAL" || text == "LOG NORMAL") {
    keyword <- list(c("meanlog", "sdlog"), c("mean", "sd"), c("mean", "sdlog"), c("meanlog", "sd"))
  }
  if (text == "GAMMA") {
    keyword <- c("shape", "rate")
  }
  if (text == "BINOMIAL" || text == "BI NORMAL") {
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
  return(keyword)
}
#######################################################################
#' Function to find the keyword for generating random numbers for given probability distribution
#' @param text name of the probability distribution
#' @return the keyword that should be used in R for generating random numbers
#' @examples
#' find_keyword_rand_generation("gamma")
#' @export
#' @details
#' This function returns the keyword for generting random number using a keyword provided
#' that is generally used for prob distirbution (but R might require a different keyword)
find_keyword_rand_generation <- function(text) {
  text <- trimws(toupper(text))
  keyword <- NULL
  if (text == "LOGNORMAL" || text == "LOG NORMAL") {
    keyword <- "rlnorm"
  }
  if (text == "GAMMA") {
    keyword <- "rgamma"
  }
  if (text == "BINOMIAL" || text == "BI NORMAL") {
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
    stop("Error -Didnt find a suitable probability distribution")
  }
  return(keyword)
}
#######################################################################
#' Function to check if the parameters are sufficient to define a probability distribution
#' and if not see if we can estimate the parameters from the given parameters, e.g. for
#' gamma distribution, shape and rate can be estimated from mean and sd
#' @param params_found given parameters for generating random numbers
#' @param required_params the required parameter by the R code (stats package)
#' @param the_expr the expression that contain the definition
#' @param distr_key the keyword used for generating random numbers in R stats package
#' @return the parameters required
#' @examples
#' check_estimating_required_params(
#'   c("mean", "sd"), c("shape", "rate"),
#'   "gamma(mean =10 ,sd=1)", "gamma"
#' )
#' @export
#' @keywords internal
#' @details
#' This function is not usually needed by the user
check_estimating_required_params <- function(params_found, required_params, the_expr, distr_key) {
  distr_key <- trimws(toupper(distr_key))
  posi_last <- sort(unlist(stringr::str_locate_all(the_expr, "\\)")))
  last <- posi_last[length(posi_last)]
  posi_first <- sort(unlist(stringr::str_locate_all(the_expr, "\\(")))
  first <- posi_first[1]
  res <- stringr::str_sub(the_expr, start = first + 1, end = last - 1)
  posi <- sort(unlist(stringr::str_locate_all(res, ",")))
  final <- length(posi) + 1
  values_found <- list()
  i <- 1
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
    this_value <- eval(parse(text = this_param))
    if (!is.numeric(this_value)) {
      stop("Error - the parameter = value expression could not be evaluated")
    }
    values_found <- append(values_found, this_value)
    i <- i + 2
  }
  values_found <- unlist(values_found)
  if (distr_key == "GAMMA" & sum(params_found == c("mean", "sd")) == 2) {
    shape <- (values_found[1] / values_found[2])^2
    rate <- values_found[1] / (values_found[2]^2)
    esti_reqd_params <- c(shape, rate)
    return(esti_reqd_params)
  } else {
    return(0)
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
#' Get the expression of probability distribution with parameters,
#' get the parameters that are foud, then dind the required params and if they are ok
#' if not try if they can be estimated. Then it will substitue for proper
#' parameters in the way R is expecting to perform
check_estimate_substitute_proper_params <- function(the_expr) {
  location <- stringr::str_locate(the_expr, "\\(")
  distr_key <- substr(the_expr, 1, location[1] - 1)
  rand_key <- find_keyword_rand_generation(distr_key)
  required_params <- find_required_parameter_combs(distr_key)
  params_found <- find_param_from_def_prob_distrbn(the_expr)
  index <- 0
  for (i in length(required_params)) {
    index <- match(params_found, unlist(required_params))
    if (sum(!is.na(index)) == length(index)) {
      i <- length(required_params)
    }
  }
  if (sum(is.na(index)) == length(index)) {
    required_params <- unlist(required_params)
    estim_params <- check_estimating_required_params(params_found, required_params, the_expr, distr_key)
    if (sum(estim_params) == 0) {
      stop("Check parameters that define the probability distribution")
    }
    create_expr <- ""
    for (i in 1:length(required_params)) {
      create_expr <- paste(create_expr, " ", required_params[i], " = ", estim_params[i], sep = "")
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
      stop("Need to provide additional info for survival analysis - hazard model or KM")
    }
    km_match <- c("KAPLAN-MEIER", "KM", "FLEMING-HARRINGTON", "FH2", "FH")
    index <- unlist(lapply(km_match, match, additional_words))
    if (any(!is.na(index))) {
      keyword <- "survival::survfit"
    }
    cox_match <- c("COX-PROPORTIONAL-HAZARD", "COX PROPORTIONAL HAZARD", "COX-PH", "COX PH", "COXPH")
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
#' Function to find the keyword for survreg distribution
#' @param text distribution
#' @return the keyword - the name of distribution
#' @examples
#' find_survreg_distribution("weibull")
#' @export
#' @details
#' For surveg method, find the distribution
find_survreg_distribution <- function(text) {
  text <- trimws(toupper(text))
  keyword <- NULL
  if (text == "EXPONENTIAL" | text == "EXPO") {
    keyword <- "exponential"
  }
  if (text == "WEIBULL") {
    keyword <- "weibull"
  }
  if (text == "GAUSSIAN") {
    keyword <- "gaussian"
  }
  if (text == "LOGGAUSSIAN" | text == "LOG GAUSSIAN") {
    keyword <- "loggaussian"
  }
  if (text == "RAYLEIGH") {
    keyword <- "rayleigh"
  }
  if (text == "LOGISTIC") {
    keyword <- "logistic"
  }
  if (text == "LOGNORMAL" | text == "LOG NORMAL") {
    keyword <- "lognormal"
  }
  if (text == "LOG LOGISTIC" | text == "LOGLOGISTIC") {
    keyword <- "loglogistic"
  }
  if (is.null(keyword)) {
    stop("Error - Survreg - family of distribution not matching  ")
  }
  return(keyword)
}
#######################################################################
#' Function to find the keyword for family of distribution in glm
#' @param text distribution
#' @return the keyword - the name of distribution
#' @examples
#' find_glm_distribution("gamma")
#' @export
#' @details
#' Find the family for glm method
find_glm_distribution <- function(text) {
  text <- trimws(toupper(text))
  keyword <- NULL
  if (text == "BINOMIAL" | text == "BI NOMIAL") {
    keyword <- "binomial"
  }
  if (text == "GAUSSIAN" | text == "NORMAL") {
    keyword <- "gaussian"
  }
  if (text == "GAMMA") {
    keyword <- "gamma"
  }
  if (text == "INVERSE.GAUSSIAN" | text == "INVERSE GAUSSIAN" | text == "INVERSE_GAUSSIAN") {
    keyword <- "inverse.gaussian"
  }
  if (text == "POISSON") {
    keyword <- "poisson"
  }
  if (text == "QUASI") {
    keyword <- "quasi"
  }
  if (text == "QUASI BINOMIAL" | text == "QUASI_BINOMIAL") {
    keyword <- "quasibinomial"
  }
  if (text == "QUASI POISSON" | text == "QUASI_POISSON") {
    keyword <- "quasipoisson"
  }
  if (is.null(keyword)) {
    stop("Error - glm - family of distribution not found  ")
  }
  return(keyword)
}
#######################################################################
#' Form expression to use with lm()
#' @param param_to_be_estimated  parameter of interest
#' @param indep_var the independent variable (column name in data file)
#' @param covariates list of covariates
#' @param interaction boolean value to indicate interaction in the case of linear regression,
#' false by default
#' @return the formula for lm
#' @examples
#' formula <- form_expression_lm("gre", indep_var = "gpa", covariates = NA, interaction = FALSE)
#' @export
#' @details
#' This function helps to create the expression for liner regression model
#' it takes care of covariates and interaction
form_expression_lm <- function(param_to_be_estimated, indep_var, covariates, interaction) {
  if (length(covariates) == 0 | sum(is.na(covariates)) == length(covariates)) {
    # no need to check for interaction
    fmla <- paste("lm(", param_to_be_estimated, " ~ ", indep_var, ", data = dataset )", sep = "")
    short_fmla <- paste(" ~ ", indep_var, sep = "")
  } else {
    expre <- paste(covariates[1], sep = "")
    i <- 2
    while (i <= length(covariates)) {
      this <- paste(covariates[i], sep = "")
      if (interaction) {
        expre <- paste(expre, this, sep = " * ")
      } else {
        expre <- paste(expre, this, sep = " + ")
      }
      i <- i + 1
    }
    fmla <- paste("lm(", param_to_be_estimated, " ~ ", expre, " + ", indep_var, ", data = dataset )", sep = "")
    short_fmla <- paste(" ~ ", expre, " + ", indep_var, ")", sep = "")
  }
  expressions <- list(formula = fmla, short_formula = short_fmla)
  return(expressions)
}
#######################################################################
#' Form expression to use with glm()
#' @param param_to_be_estimated  parameter of interest
#' @param indep_var the independent variable (column name in data file)
#' @param family distribution name  eg. for logistic regression -binomial
#' @param covariates list of covariates
#' @param interaction boolean value to indicate interaction in the case of generalised linear models,
#' false by default
#' @param naaction action to be taken with the missing values
#' @param link link function if not the default for each family
#' @return the formula for glm
#' @examples
#' formula <- form_expression_glm("admit",
#'   indep_var = "gre", family = "binomial",
#'   covariates = c("gpa", "rank"), interaction = FALSE, naaction = "na.omit", link = NA
#' )
#' @export
#' @details
#' Form expression for the method glm
form_expression_glm <- function(param_to_be_estimated, indep_var, family, covariates, interaction, naaction, link) {
  if (is.na(family)) {
    stop("Error - information on distribution is missing")
  } else {
    this_dist <- find_glm_distribution(family)
  }
  if (!is.na(link)) {
    link <- check_link_glm(this_dist, link)
    family_def <- paste(this_dist, "(link = ", link, ")", sep = "")
  } else {
    family_def <- paste(this_dist, sep = "")
  }
  if (length(covariates) == 0 | sum(is.na(covariates)) == length(covariates)) {
    # no need to check for interaction
    fmla <- paste("glm(", param_to_be_estimated, " ~ ", indep_var, ", family = ", family_def,
      ", data = dataset, na.action =", naaction, ")",
      sep = ""
    )
    short_fmla <- paste(" ~ ", indep_var, sep = "")
  } else {
    expre <- paste(covariates[1], sep = "")
    i <- 2
    while (i <= length(covariates)) {
      this <- paste(covariates[i], sep = "")
      if (interaction) {
        expre <- paste(expre, this, sep = " * ")
      } else {
        expre <- paste(expre, this, sep = " + ")
      }
      i <- i + 1
    }
    fmla <- paste("glm(", param_to_be_estimated, " ~ ", expre, " + ", indep_var, ", family = ", family_def,
      ", data = dataset, na.action =", naaction, ")",
      sep = ""
    )
    short_fmla <- paste(" ~ ", expre, " + ", indep_var, sep = "")
  }
  expressions <- list(formula = fmla, short_formula = short_fmla)
  return(expressions)
}

#######################################################################
#' Function to find the keyword for family of distribution in glm
#' @param family family of distribution
#' @param link function to be used
#' @return the link if they can be accepted else error
#' @examples
#' check_link_glm("gaussian", "identity")
#' @export
#' @details
#' Check and get the link function for the method glm
check_link_glm <- function(family, link) {
  family <- tolower(trimws(toupper(family)))
  link <- tolower(trimws(toupper(link)))
  if (family == "gaussian") {
    link_accept <- c("identity", "log", "inverse")
  }
  if (family == "binomial") {
    link_accept <- c("logit", "probit", "cauchit", "log", "loglog")
  }
  if (family == "gamma") {
    link_accept <- c("inverse", "identity", "log")
  }
  if (family == "poisson") {
    link_accept <- c("sqrt", "identity", "log")
  }
  if (family == "inverse.gaussian") {
    link_accept <- c("1/mu^2", "identity", "log", "inverse")
  }
  if (family == "quasi") {
    link_accept <- c("logit", "probit", "cloglog", "identity", "inverse", "log", "1/mu^2", "sqrt")
  }
  if (family == "quasibinomial") {
    link_accept <- c("logit", "probit", "cloglog", "identity", "inverse", "log", "1/mu^2", "sqrt")
  }
  if (family == "quasipoisson") {
    link_accept <- c("logit", "probit", "cloglog", "identity", "inverse", "log", "1/mu^2", "sqrt")
  }
  matching <- match(link, link_accept)
  if (is.na(matching)) {
    stop(paste("Error - link given- ", link, " can not be accepted by family - ", family, sep = ""))
  } else {
    return(link)
  }
}
#######################################################################
#' Form expression to use with mixed models
#' @param param_to_be_estimated column name of dependent variable
#' @param dataset a dataframe
#' @param fix_eff names of variables as fixed effect predictors
#' @param fix_eff_interact_vars, if interaction -true
#' @param random_intercept_vars, names of variables for random intercept
#' @param nested_intercept_vars_pairs, those of the random intercept variables with nested effect
#' @param cross_intercept_vars, those of the random intercept variables with crossed effect
#' @param uncorrel_slope_intercept_pairs, variables with correlated intercepts
#' @param random_slope_intercept_pairs, random slopes intercept pairs - this is alist of paired variables
#' @param family, family of distribution for non gaussian distribution of predicted variable
#' @param link, link function for the variance
#' @return result regression result with plot if success and -1, if failure
#' @examples
#' dt <- read.table("http://bayes.acs.unt.edu:8083/BayesContent/class/Jon/R_SC/Module9/lmm.data.txt",
#'   header = TRUE, sep = ",", na.strings = "NA", dec = ".", strip.white = TRUE
#' )
#' formula <- form_expression_mixed_model("extro",
#'   dataset = dataset,
#'   fix_eff = c("open", "agree", "social"),
#'   fix_eff_interact_vars = NULL,
#'   random_intercept_vars = c("school", "class"),
#'   nested_intercept_vars_pairs = list(c("school", "class")),
#'   cross_intercept_vars = NULL,
#'   uncorrel_slope_intercept_pairs = NULL,
#'   random_slope_intercept_pairs = NULL, family = "binomial", link = NA
#' )
#' @export
#' @details
#' Form the expression for mixed model
#'
form_expression_mixed_model <- function(param_to_be_estimated, dataset, fix_eff, fix_eff_interact_vars,
                                        random_intercept_vars, nested_intercept_vars_pairs, cross_intercept_vars,
                                        uncorrel_slope_intercept_pairs, random_slope_intercept_pairs, family, link) {
  if (!is.na(family)) {
    this_dist <- find_glm_distribution(family)
    if (!is.na(link)) {
      link <- check_link_glm(this_dist, link)
      family_def <- paste(this_dist, "(link = ", link, ")", sep = "")
    } else {
      family_def <- paste(this_dist, sep = "")
    }
  }
  if (is.null(nested_intercept_vars_pairs) & is.null(cross_intercept_vars)) {
    stop("Error - the random intercept variables have to be either nested or cross, both can not be null")
  }
  expression <- paste(param_to_be_estimated, "~")
  if (is.null(fix_eff)) {
    fix_eff <- 1
    expression <- paste(expression, fix_eff, "+")
  }
  if (!is.null(fix_eff)) {
    no_interaction_var <- setdiff(fix_eff, fix_eff_interact_vars)
    i <- 1
    all_vars <- ""
    while (i <= length(no_interaction_var)) {
      this <- paste(no_interaction_var[i], "+ ")
      all_vars <- paste(all_vars, this)
      i <- i + 1
    }
    expression <- paste(expression, all_vars, sep = "")
    i <- 1
    all_interact_vars <- ""
    while (i <= length(fix_eff_interact_vars)) {
      if (i == length(fix_eff_interact_vars)) {
        this <- paste(fix_eff_interact_vars[i], "+ ")
      } else {
        this <- paste(fix_eff_interact_vars[i], "* ")
      }
      all_interact_vars <- paste(all_interact_vars, this)
      i <- i + 1
    }
    expression <- paste(expression, all_interact_vars, sep = "")
  }
  if (!is.null(random_intercept_vars)) {
    if (!is.null(cross_intercept_vars)) {
      char <- "+ "
      i <- 1
      all_interact_vars <- " "
      while (i <= length(cross_intercept_vars)) {
        if (!is.null(uncorrel_slope_intercept_pairs)) {
          index_uncorrel <- match(cross_intercept_vars[i], unlist(uncorrel_slope_intercept_pairs))
        } else {
          index_uncorrel <- NA
        }
        if (length(index_uncorrel) != 0 & sum(is.na(index_uncorrel)) > 0) {
          uncorrel_ind <- "1 + "
        } else {
          uncorrel_ind <- "0 + "
        }
        index <- match(cross_intercept_vars[i], unlist(random_slope_intercept_pairs))
        if (length(index) == 0 | is.na(index) | index == 0) {
          slope <- 1
        } else {
          slope <- random_slope_intercept_pairs[index / 2][[1]][index - 1]
        }
        if (i == length(cross_intercept_vars)) {
          this <- paste("(", uncorrel_ind, slope, "|", cross_intercept_vars[i], ") ")
        } else {
          this <- paste("(", uncorrel_ind, slope, "|", cross_intercept_vars[i], ")", char)
        }
        all_interact_vars <- paste(all_interact_vars, this)
        i <- i + 1
      }
      expression <- paste(expression, all_interact_vars, sep = "")
    }
    if (!is.null(nested_intercept_vars_pairs)) {
      char <- "+ "
      i <- 1
      if (is.null(cross_intercept_vars)) {
        all_interact_vars <- " "
      } else {
        all_interact_vars <- "+"
      }
      while (i <= length(nested_intercept_vars_pairs)) {
        if (!is.null(uncorrel_slope_intercept_pairs)) {
          index_uncorrel <- match(unlist(nested_intercept_vars_pairs[i]), unlist(uncorrel_slope_intercept_pairs))
        } else {
          index_uncorrel <- NA
        }
        if (length(index_uncorrel) != 0 & sum(is.na(index_uncorrel)) > 0) {
          uncorrel_ind <- "1 + "
        } else {
          uncorrel_ind <- "0 + "
        }
        index <- match(nested_intercept_vars_pairs[i], unlist(random_slope_intercept_pairs))
        if (length(index) == 0 | is.na(index) | index == 0) {
          slope <- 1
        } else {
          slope <- random_slope_intercept_pairs[index / 2][[1]][index - 1]
        }
        if (i == length(nested_intercept_vars_pairs)) {
          this <- paste("(", uncorrel_ind, slope, "|", nested_intercept_vars_pairs[i][[1]][1], ":", nested_intercept_vars_pairs[i][[1]][2], ")")
        } else {
          this <- paste("(", uncorrel_ind, slope, "|", nested_intercept_vars_pairs[i][[1]][1], ":", nested_intercept_vars_pairs[i][[1]][2], ")", char)
        }
        all_interact_vars <- paste(all_interact_vars, this)
        i <- i + 1
      }
      expression <- paste(expression, all_interact_vars, sep = "")
    }
  }
  if (!is.na(family)) {
    expression <- paste(expression, ", family = ", family_def, sep = "")
  }
  if (sys.nframe() > 1) {
    caller <- deparse(sys.calls()[[sys.nframe() - 1]])
    this_caller <- caller[1]
    index <- stringr::str_locate(this_caller, "\\(")[1]
    method_name <- substr(this_caller, 1, index[1] - 1)
    if (method_name == "use_linear_mixed_model") {
      expression <- paste("lme4::lmer(", expression, ", data = dataset)", sep = "")
    }
    if (method_name == "use_generalised_linear_mixed_model") {
      expression <- paste("lme4::glmer(", expression, ", data = dataset, control = lme4::glmerControl(optimizer = \"bobyqa\"), nAGQ = 10)", sep = "")
    }
  } else {
    expression <- paste("lme4::lmer(", expression, ", data = dataset)", sep = "")
  }
  return(expression)
}
#######################################################################
#' Convert frequency medication to given basis
#' @param freq_given given frequency
#' @param basis given basis, default is daily
#' @return converted frequency
#' @examples
#' convert_freq_diff_basis("once daily")
#' convert_freq_diff_basis("bd", "week")
#' convert_freq_diff_basis("Every 4 days", "day")
#' @export
convert_freq_diff_basis <- function(freq_given, basis = "day") {
  freq_req_basis <- NULL
  if (!is.null(freq_given)) {
    freq_given <- trimws(tolower(freq_given))
  }
  if (rlang::is_empty(freq_given) |
    any(is.na(freq_given)) |
    is.null(freq_given) |
    any(freq_given == "null") |
    any(freq_given == "Null")) {
    freq_req_basis <- NA
  } else {
    if (freq_given == "once in a day" |
      freq_given == "once a day" |
      freq_given == "daily" |
      freq_given == "once daily" |
      freq_given == "one in a day" |
      freq_given == "one a day" |
      freq_given == "one daily") {
      freq_req_basis <- 1
    } else {
      if (freq_given == "twice in a day" |
        freq_given == "twice a day" |
        freq_given == "twice daily" |
        freq_given == "bd" |
        freq_given == "b.d" |
        freq_given == "two in a day" |
        freq_given == "two a day" |
        freq_given == "two daily" |
        freq_given == "b.i.d" |
        freq_given == "bid") {
        freq_req_basis <- 2
      } else {
        if (freq_given == "thrice in a day" |
          freq_given == "thrice a day" |
          freq_given == "thrice daily" |
          freq_given == "tds" |
          freq_given == "t.d.s" |
          freq_given == "three in a day" |
          freq_given == "three a day" |
          freq_given == "three daily" |
          freq_given == "tid" |
          freq_given == "t.i.d" |
          freq_given == "three times a day" |
          freq_given == "three times in a day" |
          freq_given == "three times daily") {
          freq_req_basis <- 3
        } else {
          if (freq_given == "four in a day" |
            freq_given == "four a day" |
            freq_given == "four daily"
          | freq_given == "four times a day" |
            freq_given == "four times in a day" |
            freq_given == "four times daily") {
            freq_req_basis <- 4
          } else {
            if (freq_given == "five in a day" |
              freq_given == "five a day" |
              freq_given == "five daily"
            | freq_given == "five times a day" |
              freq_given == "five times in a day" |
              freq_given == "five times daily") {
              freq_req_basis <- 5
            } else {
              if (freq_given == "six in a day" |
                freq_given == "six a day" |
                freq_given == "six daily"
              | freq_given == "six times a day" |
                freq_given == "six times in a day" |
                freq_given == "six times daily") {
                freq_req_basis <- 6
              } else {
                if (freq_given == "seven in a day" |
                  freq_given == "seven a day" |
                  freq_given == "seven daily"
                | freq_given == "seven times a day" |
                  freq_given == "seven times in a day" |
                  freq_given == "seven times daily") {
                  freq_req_basis <- 7
                } else {
                  if (freq_given == "eight in a day" |
                    freq_given == "eight a day" |
                    freq_given == "eight daily"
                  | freq_given == "eight times a day" |
                    freq_given == "eight times in a day" |
                    freq_given == "eight times daily") {
                    freq_req_basis <- 8
                  } else {
                    if (freq_given == "nine in a day" |
                      freq_given == "nine a day" |
                      freq_given == "nine daily"
                    | freq_given == "nine times a day" |
                      freq_given == "nine times in a day" |
                      freq_given == "nine times daily") {
                      freq_req_basis <- 9
                    } else {
                      if (freq_given == "ten in a day" |
                        freq_given == "ten a day" |
                        freq_given == "ten daily"
                      | freq_given == "ten times a day" |
                        freq_given == "ten times in a day" |
                        freq_given == "ten times daily") {
                        freq_req_basis <- 10
                      } else {
                        if (freq_given == "eleven in a day" |
                          freq_given == "eleven a day" |
                          freq_given == "eleven daily"
                        | freq_given == "eleven times a day" |
                          freq_given == "eleven times in a day" |
                          freq_given == "eleven times daily") {
                          freq_req_basis <- 11
                        } else {
                          if (freq_given == "twelve in a day" |
                            freq_given == "twelve a day" |
                            freq_given == "twelve daily"
                          | freq_given == "twelve times a day" |
                            freq_given == "twelve times in a day" |
                            freq_given == "twelve times daily") {
                            freq_req_basis <- 12
                          } else {
                            if (freq_given == "once in every 2 days" |
                              freq_given == "once every 2 days" |
                              freq_given == "every 2 days" |
                              freq_given == "every 2 days") {
                              freq_req_basis <- 0.5
                            } else {
                              if (freq_given == "once in every 3 days" |
                                freq_given == "once every 3 days" |
                                freq_given == "every 3 days" |
                                freq_given == "every 3 days") {
                                freq_req_basis <- 1 / 3
                              } else {
                                if (freq_given == "once in every 4 days" |
                                  freq_given == "once every 4 days" |
                                  freq_given == "every 4 days" |
                                  freq_given == "every 4 days") {
                                  freq_req_basis <- 1 / 4
                                } else {
                                  if (freq_given == "once in every 5 days" |
                                    freq_given == "once every 5 days" |
                                    freq_given == "every 5 days" |
                                    freq_given == "every 5 days") {
                                    freq_req_basis <- 1 / 5
                                  } else {
                                    if (freq_given == "once in every 6 days" |
                                      freq_given == "once every 6 days" |
                                      freq_given == "every 6 days" |
                                      freq_given == "every 6 days") {
                                      freq_req_basis <- 1 / 6
                                    } else {
                                      if (freq_given == "once in every 7 days" |
                                        freq_given == "once every 7 days" |
                                        freq_given == "every 7 days" |
                                        freq_given == "every 7 days") {
                                        freq_req_basis <- 1 / 7
                                      } else {
                                        if (freq_given == "once a week" |
                                          freq_given == "once weekly" |
                                          freq_given == "once in a week" |
                                          freq_given == "once in week" |
                                          freq_given == "weekly once") {
                                          freq_req_basis <- 1 / 7
                                        } else {
                                          if (freq_given == "twice a week" |
                                            freq_given == "twice in a week" |
                                            freq_given == "two times in a week" |
                                            freq_given == "weekly twice" |
                                            freq_given == "weekly two times" |
                                            freq_given == "twice weekly" |
                                            freq_given == "two times weekly") {
                                            freq_req_basis <- 2 / 7
                                          } else {
                                            if (freq_given == "thrice in a week" |
                                              freq_given == "three times in a week" |
                                              freq_given == "thrice a week" |
                                              freq_given == "three times in week" |
                                              freq_given == "weekly thrice" |
                                              freq_given == "weekly three times" |
                                              freq_given == "thrice weekly" |
                                              freq_given == "three times weekly") {
                                              freq_req_basis <- 3 / 7
                                            } else {
                                              if (freq_given == "four times in a week" |
                                                freq_given == "four times in week" |
                                                freq_given == "four times a week" |
                                                freq_given == "weekly four times" |
                                                freq_given == "four times a week" |
                                                freq_given == " four times weekly") {
                                                freq_req_basis <- 4 / 7
                                              } else {
                                                if (freq_given == "five times in a week" |
                                                  freq_given == "five times in week" |
                                                  freq_given == "five times a week" |
                                                  freq_given == "weekly five times" |
                                                  freq_given == "five times weekly ") {
                                                  freq_req_basis <- 5 / 7
                                                } else {
                                                  if (freq_given == "six times in a week" |
                                                    freq_given == "six times in week" |
                                                    freq_given == "six times a week" |
                                                    freq_given == "weekly six times" |
                                                    freq_given == "six times weekly") {
                                                    freq_req_basis <- 6 / 7
                                                  } else {
                                                    if (freq_given == "seven times in a week" |
                                                      freq_given == "seven times in week" |
                                                      freq_given == "seven times a week" |
                                                      freq_given == "weekly seven times" |
                                                      freq_given == "seven times weekly ") {
                                                      freq_req_basis <- 7 / 7
                                                    } else {
                                                      if (freq_given == "every 1 hour " |
                                                        freq_given == "every one hour"
                                                      | freq_given == "every hour" |
                                                        freq_given == "once hourly"
                                                      | freq_given == "hourly once" |
                                                        freq_given == "hourly one time") {
                                                        freq_req_basis <- 24
                                                      } else {
                                                        if (freq_given == "every 2 hour" |
                                                          freq_given == "every two hour" |
                                                          freq_given == "every 2 hours" |
                                                          freq_given == "every two hours") {
                                                          freq_req_basis <- 12
                                                        } else {
                                                          if (freq_given == "every 3 hour" |
                                                            freq_given == "every three hour" |
                                                            freq_given == "every 3 hours" |
                                                            freq_given == "every three hours") {
                                                            freq_req_basis <- 8
                                                          } else {
                                                            if (freq_given == "every 4 hour" |
                                                              freq_given == "every four hour" |
                                                              freq_given == "every 4 hours" |
                                                              freq_given == "every four hours") {
                                                              freq_req_basis <- 6
                                                            } else {
                                                              if (freq_given == "every 6 hour" | freq_given == "every six hour" |
                                                                freq_given == "every 6 hours" | freq_given == "every six hours") {
                                                                freq_req_basis <- 4
                                                              } else {
                                                                if (freq_given == "every 12 hour" |
                                                                  freq_given == "every twelve hour" |
                                                                  freq_given == "every 12 hours" |
                                                                  freq_given == "every twelve hours") {
                                                                  freq_req_basis <- 2
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  basis <- tolower(basis)
  if (basis == "hour" & !is.na(freq_req_basis)) {
    freq_req_basis <- freq_req_basis / 24
  }
  if (basis == "week" & !is.na(freq_req_basis)) {
    freq_req_basis <- freq_req_basis * 7
  }
  if (basis == "month" & !is.na(freq_req_basis)) {
    freq_req_basis <- freq_req_basis * 30
  }
  if (basis == "year" & !is.na(freq_req_basis)) {
    freq_req_basis <- freq_req_basis * 365
  }
  if (is.null(freq_req_basis)) {
    warning("Something went wrong- couldnt convert the frquency to the correct basis")
  } else {
    return(freq_req_basis)
  }
}
#######################################################################
#' Convert unit strength to given basis
#' @param given_unit given unit
#' @param basis given basis, default is "mg"
#' @return converted unit
#' @examples
#' convert_unit_diff_basis("mg")
#' convert_unit_diff_basis("mg", "mcg")
#' convert_unit_diff_basis("l", "ml")
#' @export
convert_unit_diff_basis <- function(given_unit, basis = "mg") {
  unit_req_basis <- NULL
  if (!is.null(given_unit)) {
    given_unit <- trimws(tolower(given_unit))
  }
  if (rlang::is_empty(given_unit) | any(is.na(given_unit)) | length(given_unit) == 0 |
    identical(given_unit, "") | is.null(given_unit) |
    any(given_unit == "null") | any(given_unit == "Null")) {
    unit_req_basis <- NA
  } else {
    if (basis == "mg") {
      if (given_unit == "mg" | given_unit == "milli gram" | given_unit == "milligram") {
        unit_req_basis <- 1
      }
      if (given_unit == "g" | given_unit == "gm") {
        unit_req_basis <- 1000
      }
      if (given_unit == "kg" | given_unit == "k.g" | given_unit == "kilo gram") {
        unit_req_basis <- 1e6
      }
      if (given_unit == "mcg" | given_unit == "micro gram" | given_unit == "microgram") {
        unit_req_basis <- 0.001
      }
    } else {
      if (basis == "ml") {
        if (given_unit == "l" | given_unit == "litre" | given_unit == "liter") {
          unit_req_basis <- 1000
        }
        if (given_unit == "mcl" | given_unit == "micro litre" | given_unit == "microlitre" |
          given_unit == "micro liter" | given_unit == "microliter") {
          unit_req_basis <- 0.001
        }
      }
    }
  }
  if (is.null(unit_req_basis)) {
    warning("Something went wrong- couldnt convert the frquency to the correct basis")
  } else {
    return(unit_req_basis)
  }
}
#######################################################################
#' Convert weight per time  to given basis
#' @param given_unit given unit
#' @param basis given basis, default is "mg"
#' @return converted unit
#' @examples
#' convert_wtpertimediff_basis("mg/day")
#' convert_wtpertimediff_basis("mcg/day")
#' convert_wtpertimediff_basis("mg/hour")
#' @export
convert_wtpertimediff_basis <- function(given_unit, basis = "mcg/hour") {
  if (!is.null(given_unit)) {
    given_unit <- trimws(tolower(given_unit))
  }
  unit_req_basis <- NULL
  if (rlang::is_empty(given_unit) | any(is.na(given_unit)) | length(given_unit) == 0
  | identical(given_unit, "") | is.null(given_unit)
  | any(given_unit == "null") | any(given_unit == "Null")) {
    unit_req_basis <- NA
  } else {
    index <- stringr::str_locate(given_unit, "/")
    given_wt <- stringr::str_sub(given_unit, 1, index[1] - 1)
    given_time <- stringr::str_sub(given_unit, index[2] + 1, nchar(given_unit))
    basis_index <- stringr::str_locate(basis, "/")
    basis_wt <- stringr::str_sub(basis, 1, basis_index[1] - 1)
    basis_time <- stringr::str_sub(basis, basis_index[2] + 1, nchar(basis))
    unit_req_wt <- 1
    unit_req_time <- 1
    if (basis_wt == "mg" | basis_wt == "milli gram" | basis_wt == "milligram") {
      if (given_wt == "mcg" | given_wt == "micro gram" | given_wt == "microgram") {
        unit_req_wt <- 1 / 1000
      }
      if (given_wt == "gram" | given_wt == "gm") {
        unit_req_wt <- 1000
      }
    }
    if (basis_wt == "mcg" | basis_wt == "micro gram" | basis_wt == "microgram") {
      if (given_wt == "mg" | given_wt == "milli gram" | given_wt == "milligram") {
        unit_req_wt <- 1000
      }
      if (given_wt == "gram" | given_wt == "gm") {
        unit_req_wt <- 1e6
      }
    }
    if (basis_wt == "gram" | basis_wt == "gm") {
      if (given_wt == "mg" | given_wt == "milli gram" | given_wt == "milligram") {
        unit_req_wt <- 1 / 1000
      }
      if (given_wt == "mcg" | given_wt == "micro gram" | given_wt == "microgram") {
        unit_req_wt <- 1 / 1e6
      }
    }
    if (basis_time == "day") {
      if (given_time == "hour" | basis_time == "hr") {
        unit_req_time <- 1 / 24
      }
      if (given_time == "sec") {
        unit_req_time <- 1 / (24 * 3600)
      }
      if (given_time == "minute" | given_time == "min") {
        unit_req_time <- 1 / (24 * 60)
      }
    }
    if (basis_time == "hour" | basis_time == "hr") {
      if (given_time == "day") {
        unit_req_time <- 24
      }
      if (given_time == "sec") {
        unit_req_time <- 1 / 3600
      }
      if (given_time == "minute" | given_time == "min") {
        unit_req_time <- 1 / 60
      }
    }
    if (basis_time == "minute" | basis_time == "min") {
      if (given_time == "day") {
        unit_req_time <- 24 * 60
      }
      if (given_time == "sec") {
        unit_req_time <- 1 / 60
      }
      if (given_time == "hour" | given_time == "hr") {
        unit_req_time <- 60
      }
    }
    if (basis_time == "sec") {
      if (given_time == "day") {
        unit_req_time <- 24 * 3600
      }
      if (given_time == "minute" | basis_time == "min") {
        unit_req_time <- 60
      }
      if (given_time == "hour" | given_time == "hr") {
        unit_req_time <- 3600
      }
    }
    unit_req_basis <- unit_req_wt / unit_req_time
  }
  if (is.null(unit_req_basis)) {
    warning("Something went wrong- couldnt convert the frquency to the correct basis")
  } else {
    return(unit_req_basis)
  }
}
#######################################################################
#' Convert period to given basis
#' @param given_time given time
#' @param basis_time given basis, default is "day"
#' @return converted unit
#' @examples
#' convert_to_given_timeperiod("4 weeks")
#' convert_to_given_timeperiod("a month")
#' convert_to_given_timeperiod("1 week")
#' @export
convert_to_given_timeperiod <- function(given_time, basis_time = "day") {
  if (!is.null(given_time)) {
    given_time <- trimws(tolower(given_time))
  }
  unit_req_basis <- NULL
  if (rlang::is_empty(given_time) | any(is.na(given_time)) | length(given_time) == 0 |
    identical(given_time, "")
  | any(given_time == "null") | any(given_time == "Null")) {
    unit_req_basis <- NA
  } else {
    index <- stringr::str_locate(given_time, " ")
    first_part <- stringr::str_sub(given_time, 1, index[1] - 1)
    if (!is.numeric(first_part)) {
      out <- tryCatch(
        {
          words2number::to_number(first_part)
        },
        error = function(cond) {
          if (first_part == "a" | first_part == "an" | first_part == "one") {
            out <- 1
          }
        }
      )
    } else {
      out <- as.numeric(first_part)
    }
    sec_part <- stringr::str_sub(given_time, index[2] + 1, nchar(given_time))
    if (basis_time == "day" | basis_time == "days") {
      if (sec_part == "day" | sec_part == "days") {
        unit_req_time <- 1
      }
      if (sec_part == "week" | sec_part == "weeks") {
        unit_req_time <- 7
      }
      if (sec_part == "month" | sec_part == "months") {
        unit_req_time <- 30
      }
      if (sec_part == "year" | sec_part == "years") {
        unit_req_time <- 365
      }
      if (sec_part == "hour" | sec_part == "hr" | sec_part == "hours") {
        unit_req_time <- 1 / 24
      }
      if (sec_part == "minute" | sec_part == "min" | sec_part == "minutes") {
        unit_req_time <- 1 / (24 * 60)
      }
      if (sec_part == "sec" | sec_part == "second" | sec_part == "seconds") {
        unit_req_time <- 1 / (24 * 60 * 60)
      }
    }
    if (basis_time == "month" | basis_time == "months") {
      if (sec_part == "month" | sec_part == "months") {
        unit_req_time <- 1
      }
      if (sec_part == "week" | sec_part == "weeks") {
        unit_req_time <- 1 / 4
      }
      if (sec_part == "day" | sec_part == "days") {
        unit_req_time <- 1 / 30
      }
      if (sec_part == "year" | sec_part == "years") {
        unit_req_time <- 12
      }
      if (sec_part == "hour" | sec_part == "hr" | sec_part == "hours") {
        unit_req_time <- 1 / (30 * 24)
      }
      if (sec_part == "minute" | sec_part == "min" | sec_part == "minutes") {
        unit_req_time <- 1 / (30 * 24 * 60)
      }
      if (sec_part == "sec" | sec_part == "second" | sec_part == "seconds") {
        unit_req_time <- 1 / (30 * 24 * 60 * 60)
      }
    }
    if (basis_time == "week" | basis_time == "weeks") {
      if (sec_part == "week" | sec_part == "weeks") {
        unit_req_time <- 1
      }
      if (sec_part == "month" | sec_part == "months") {
        unit_req_time <- 4
      }
      if (sec_part == "day" | sec_part == "days") {
        unit_req_time <- 1 / 7
      }
      if (sec_part == "year" | sec_part == "years") {
        unit_req_time <- 52.1429
      }
      if (sec_part == "hour" | sec_part == "hr" | sec_part == "hours") {
        unit_req_time <- 1 / (7 * 24)
      }
      if (sec_part == "minute" | sec_part == "min" | sec_part == "minutes") {
        unit_req_time <- 1 / (7 * 24 * 60)
      }
      if (sec_part == "sec" | sec_part == "second" | sec_part == "seconds") {
        unit_req_time <- 1 / (7 * 24 * 60 * 60)
      }
    }
    if (basis_time == "hour" | basis_time == "hr" | basis_time == "hours") {
      if (sec_part == "hour" | sec_part == "hr" | sec_part == "hours") {
        unit_req_time <- 1
      }
      if (sec_part == "month" | sec_part == "months") {
        unit_req_time <- 24 * 30
      }
      if (sec_part == "day" | sec_part == "days") {
        unit_req_time <- 24
      }
      if (sec_part == "year" | sec_part == "years") {
        unit_req_time <- 24 * 365
      }
      if (sec_part == "week" | sec_part == "weeks") {
        unit_req_time <- 7 * 24
      }
      if (sec_part == "minute" | sec_part == "min" | sec_part == "minutes") {
        unit_req_time <- 1 / (60)
      }
      if (sec_part == "sec" | sec_part == "second" | sec_part == "seconds") {
        unit_req_time <- 1 / (60 * 60)
      }
    }
    unit_req_basis <- out * unit_req_time
  }
  return(unit_req_basis)
}
#######################################################################
#' Convert volume per time  to given basis
#' @param given_unit given unit
#' @param basis given basis, default is "mg"
#' @return converted unit
#' @examples
#' convert_volpertimediff_basis("ml/day")
#' convert_volpertimediff_basis("mcl/day")
#' convert_volpertimediff_basis("ml/hour")
#' @export
convert_volpertimediff_basis <- function(given_unit, basis = "ml/hour") {
  if (!is.null(given_unit)) {
    given_unit <- trimws(tolower(given_unit))
  }
  unit_req_basis <- NULL
  if (rlang::is_empty(given_unit) | any(is.na(given_unit)) | length(given_unit) == 0
  | identical(given_unit, "")
  | any(given_unit == "null") | any(given_unit == "Null")) {
    unit_req_basis <- NA
  } else {
    index <- stringr::str_locate(given_unit, "/")
    given_vol <- stringr::str_sub(given_unit, 1, index[1] - 1)
    given_time <- stringr::str_sub(given_unit, index[2] + 1, nchar(given_unit))
    basis_index <- stringr::str_locate(basis, "/")
    basis_vol <- stringr::str_sub(basis, 1, basis_index[1] - 1)
    basis_time <- stringr::str_sub(basis, basis_index[2] + 1, nchar(basis))
    unit_req_vol <- 1
    unit_req_time <- 1
    if (basis_vol == "ml" |
      basis_vol == "milli liter" | basis_vol == "milliliter" |
      basis_vol == "milli litre" | basis_vol == "millilitre") {
      if (given_vol == "ml" |
        given_vol == "milli liter" | given_vol == "milliliter" |
        given_vol == "milli litre" | given_vol == "millilitre") {
        unit_req_vol <- 1
      }
      if (given_vol == "micro liter" | given_vol == "micro litre" |
        given_vol == "microliter" | given_vol == "microlitre" |
        given_vol == "mcl") {
        unit_req_vol <- 1 / 1000
      }
      if (given_vol == "l" | given_vol == "liter" | given_vol == "litre") {
        unit_req_vol <- 1000
      }
    }
    if (basis_vol == "micro liter" | basis_vol == "micro litre" |
      basis_vol == "microliter" | basis_vol == "microlitre" |
      basis_vol == "mcl") {
      if (given_vol == "micro liter" | given_vol == "micro litre" |
        given_vol == "microliter" | given_vol == "microlitre" |
        given_vol == "mcl") {
        unit_req_vol <- 1
      }
      if (given_vol == "ml" |
        given_vol == "milli liter" | given_vol == "milliliter" |
        given_vol == "milli litre" | given_vol == "millilitre") {
        unit_req_vol <- 1000
      }
      if (given_vol == "l" | given_vol == "liter" | given_vol == "litre") {
        unit_req_vol <- 1e6
      }
    }
    if (basis_vol == "l" | basis_vol == "liter" | basis_vol == "litre") {
      if (given_vol == "l" | given_vol == "liter" | given_vol == "litre") {
        unit_req_vol <- 1
      }
      if (given_vol == "ml" |
        given_vol == "milli liter" | given_vol == "milliliter" |
        given_vol == "milli litre" | given_vol == "millilitre") {
        unit_req_vol <- 1 / 1000
      }
      if (basis_vol == "micro liter" | basis_vol == "micro litre" |
        basis_vol == "microliter" | basis_vol == "microlitre" |
        basis_vol == "mcl") {
        unit_req_vol <- 1 / 1e6
      }
    }
    if (basis_time == "day" | basis_time == "days") {
      if (given_time == "day" | basis_time == "days") {
        unit_req_time <- 1
      }
      if (given_time == "hour" | given_time == "hours" | given_time == "hr") {
        unit_req_time <- 1 / 24
      }
      if (given_time == "sec" | given_time == "second" | given_time == "seconds") {
        unit_req_time <- 1 / (24 * 3600)
      }
      if (given_time == "minute" | given_time == "minutes" | given_time == "min") {
        unit_req_time <- 1 / (24 * 60)
      }
    }
    if (basis_time == "hour" | basis_time == "hours" | basis_time == "hr") {
      if (given_time == "hour" | given_time == "hours" | given_time == "hr") {
        unit_req_time <- 1
      }
      if (given_time == "day" | given_time == "days") {
        unit_req_time <- 24
      }
      if (given_time == "sec" | given_time == "second" | given_time == "seconds") {
        unit_req_time <- 1 / 3600
      }
      if (given_time == "minute" | given_time == "minutes" | given_time == "min") {
        unit_req_time <- 1 / 60
      }
    }
    if (basis_time == "minute" | basis_time == "minutes" | basis_time == "min") {
      if (given_time == "minute" | given_time == "minutes" | given_time == "min") {
        unit_req_time <- 1
      }
      if (given_time == "day" | given_time == "days") {
        unit_req_time <- 24 * 60
      }
      if (given_time == "sec" | given_time == "second" | given_time == "seconds") {
        unit_req_time <- 1 / 60
      }
      if (given_time == "hour" | given_time == "hours" | given_time == "hr") {
        unit_req_time <- 60
      }
    }
    if (basis_time == "sec" | basis_time == "second" | basis_time == "seconds") {
      if (given_time == "sec" | given_time == "second" | given_time == "seconds") {
        unit_req_time <- 1
      }
      if (given_time == "day" | given_time == "days") {
        unit_req_time <- 24 * 3600
      }
      if (given_time == "minute" | given_time == "minutes" | given_time == "min") {
        unit_req_time <- 60
      }
      if (given_time == "hour" | given_time == "hours" | given_time == "hr") {
        unit_req_time <- 3600
      }
    }
    unit_req_basis <- unit_req_vol / unit_req_time
  }
  if (is.null(unit_req_basis)) {
    warning("Something went wrong- couldnt convert the frquency to the correct basis")
  } else {
    return(unit_req_basis)
  }
}
#######################################################################
#' Convert unit of time to another
#' @param given_time given unit of time
#' @param basis given basis, default is "day"
#' @return converted unit
#' @examples
#' convert_time_unit("day")
#' convert_time_unit("sec")
#' convert_time_unit("sec", "minute")
#' @export
convert_time_unit <- function(given_time, basis = "day") {
  if (!is.null(given_time)) {
    given_time <- trimws(tolower(given_time))
  }
  unit_req_time <- NULL
  if (rlang::is_empty(given_time) | any(is.na(given_time)) | length(given_time) == 0
  | identical(given_time, "")
  | any(given_time == "null") | any(given_time == "Null")) {
    unit_req_time <- NA
  } else {
    if (basis == "year" | basis == "years") {
      if (given_time == "year" | given_time == "years") {
        unit_req_time <- 1
      }
      if (given_time == "month" | given_time == "months") {
        unit_req_time <- 12
      }
      if (given_time == "week" | given_time == "weeks") {
        unit_req_time <- 52
      }
      if (given_time == "day" | given_time == "days") {
        unit_req_time <- 365.25
      }
      if (given_time == "hour" | given_time == "hr" | given_time == "hours") {
        unit_req_time <- 30 * 24
      }
      if (given_time == "sec" | given_time == "second" | given_time == "seconds") {
        unit_req_time <- 30 * 24 * 3600
      }
      if (given_time == "minute" | given_time == "minutes" | given_time == "min") {
        unit_req_time <- 30 * 24 * 60
      }
    }
    if (basis == "month" | basis == "months") {
      if (given_time == "month" | given_time == "months") {
        unit_req_time <- 1
      }
      if (given_time == "year" | given_time == "years") {
        unit_req_time <- 1 / 12
      }
      if (given_time == "week" | given_time == "weeks") {
        unit_req_time <- 4
      }
      if (given_time == "day" | given_time == "days") {
        unit_req_time <- 30
      }
      if (given_time == "hour" | given_time == "hr" | given_time == "hours") {
        unit_req_time <- 30 * 24
      }
      if (given_time == "sec" | given_time == "second" | given_time == "seconds") {
        unit_req_time <- 30 * 24 * 3600
      }
      if (given_time == "minute" | given_time == "minutes" | given_time == "min") {
        unit_req_time <- 30 * 24 * 60
      }
    }
    if (basis == "week" | basis == "weeks") {
      if (given_time == "week" | given_time == "weeks") {
        unit_req_time <- 1
      }
      if (given_time == "year" | given_time == "years") {
        unit_req_time <- 1 / 52
      }
      if (given_time == "month" | given_time == "months") {
        unit_req_time <- 1 / 4
      }
      if (given_time == "day" | given_time == "days") {
        unit_req_time <- 7
      }
      if (given_time == "hour" | given_time == "hr" | given_time == "hours") {
        unit_req_time <- 7 * 24
      }
      if (given_time == "sec" | given_time == "second" | given_time == "seconds") {
        unit_req_time <- 7 * 24 * 3600
      }
      if (given_time == "minute" | given_time == "minutes" | given_time == "min") {
        unit_req_time <- 7 * 24 * 60
      }
    }
    if (basis == "day" | basis == "days") {
      if (given_time == "day" | given_time == "days") {
        unit_req_time <- 1
      }
      if (given_time == "year" | given_time == "years") {
        unit_req_time <- 1 / 365.25
      }
      if (given_time == "month" | given_time == "months") {
        unit_req_time <- 1 / 30
      }
      if (given_time == "week" | given_time == "weeks") {
        unit_req_time <- 1 / 7
      }
      if (given_time == "hour" | given_time == "hr" | given_time == "hours") {
        unit_req_time <- 1 / 24
      }
      if (given_time == "sec" | given_time == "second" | given_time == "seconds") {
        unit_req_time <- 1 / (24 * 3600)
      }
      if (given_time == "minute" | given_time == "minutes" | given_time == "min") {
        unit_req_time <- 1 / (24 * 60)
      }
    }
    if (basis == "hour" | basis == "hr" | basis == "hours") {
      if (given_time == "hour" | given_time == "hr" | given_time == "hours") {
        unit_req_time <- 1
      }
      if (given_time == "year" | given_time == "years") {
        unit_req_time <- 1 / (365.25 * 24)
      }
      if (given_time == "month" | given_time == "months") {
        unit_req_time <- 1 / (30 * 24)
      }
      if (given_time == "week" | given_time == "weeks") {
        unit_req_time <- 1 / (7 * 24)
      }
      if (given_time == "day" | given_time == "days") {
        unit_req_time <- 24
      }
      if (given_time == "sec" | given_time == "second" | given_time == "seconds") {
        unit_req_time <- 1 / 3600
      }
      if (given_time == "minute" | given_time == "minutes" | given_time == "min") {
        unit_req_time <- 1 / 60
      }
    }
    if (basis == "minute" | basis == "minutes" | basis == "min") {
      if (given_time == "minute" | given_time == "minutes" | given_time == "min") {
        unit_req_time <- 1
      }
      if (given_time == "year" | given_time == "years") {
        unit_req_time <- 1 / (365.25 * 24 * 60)
      }
      if (given_time == "month" | given_time == "months") {
        unit_req_time <- 1 / (30 * 24 * 60)
      }
      if (given_time == "week" | given_time == "weeks") {
        unit_req_time <- 1 / (7 * 24 * 60)
      }
      if (given_time == "day" | given_time == "days") {
        unit_req_time <- 24 * 60
      }
      if (given_time == "sec" | given_time == "second" | given_time == "seconds") {
        unit_req_time <- 1 / 60
      }
      if (given_time == "hour" | given_time == "hr" | given_time == "hours") {
        unit_req_time <- 60
      }
    }
    if (basis == "sec" | basis == "second" | basis == "seconds") {
      if (given_time == "sec" | given_time == "second" | given_time == "seconds") {
        unit_req_time <- 1
      }
      if (given_time == "year" | given_time == "years") {
        unit_req_time <- 1 / (365.25 * 24 * 60 * 60)
      }
      if (given_time == "month" | given_time == "months") {
        unit_req_time <- 1 / (30 * 24 * 60 * 60)
      }
      if (given_time == "week" | given_time == "weeks") {
        unit_req_time <- 1 / (7 * 24 * 60 * 60)
      }
      if (given_time == "day" | given_time == "days") {
        unit_req_time <- 24 * 3600
      }
      if (given_time == "minute" | given_time == "minutes" | given_time == "min") {
        unit_req_time <- 60
      }
      if (given_time == "hour" | given_time == "hr" | given_time == "hours") {
        unit_req_time <- 3600
      }
    }
  }
  if (is.null(unit_req_time)) {
    return(-1)
  } else {
    return(unit_req_time)
  }
}
##########################################################################################################
#######################################################################
#' Convert volume  to given basis
#' @param given_unit given unit
#' @param basis given basis, default is "mg"
#' @return converted unit
#' @examples
#' convert_volume_basis("ml", "liter")
#' @export
convert_volume_basis <- function(given_unit, basis = "ml") {
  if (!is.null(given_unit)) {
    given_unit <- trimws(tolower(given_unit))
  }
  unit_req_basis <- NULL
  if (rlang::is_empty(given_unit) | any(is.na(given_unit)) | length(given_unit) == 0
  | identical(given_unit, "")
  | any(given_unit == "null") | any(given_unit == "Null")) {
    unit_req_vol <- NA
  } else {
    unit_req_vol <- 1
    if (basis == "ml" |
      basis == "milli liter" | basis == "milliliter" |
      basis == "milli litre" | basis == "millilitre") {
      if (given_unit == "micro liter" | given_unit == "micro litre" |
        given_unit == "microliter" | given_unit == "microlitre" |
        given_unit == "mcl") {
        unit_req_vol <- 1 / 1000
      }
      if (given_unit == "l" | given_unit == "liter" | given_unit == "litre") {
        unit_req_vol <- 1000
      }
    }
    if (basis == "micro liter" | basis == "micro litre" |
      basis == "microliter" | basis == "microlitre" |
      basis == "mcl") {
      if (given_unit == "l" | given_unit == "liter" | given_unit == "litre") {
        unit_req_vol <- 1e6
      }
      if (given_unit == "ml" |
        given_unit == "milli liter" | given_unit == "milliliter" |
        given_unit == "milli litre" | given_unit == "millilitre") {
        unit_req_vol <- 1000
      }
    }
    if (basis == "l" | basis == "liter" | basis == "litre") {
      if (given_unit == "ml" |
        given_unit == "milli liter" | given_unit == "milliliter" |
        given_unit == "milli litre" | given_unit == "millilitre") {
        unit_req_vol <- 1 / 1000
      }

      if (given_unit == "micro liter" | given_unit == "micro litre" |
        given_unit == "microliter" | given_unit == "microlitre" |
        given_unit == "mcl") {
        unit_req_vol <- 1 / 1e6
      }
    }
    if (is.null(unit_req_vol)) {
      return(-1)
    } else {
      return(unit_req_vol)
    }
  }
}
#' #######################################################################

