#' Function to get extension of a filename
#' @param file name of a file
#' @return the extension
#' @examples get_extension("data.txt")
#' @export
get_extension <- function(file){
  ex <- strsplit(basename(file), split = "\\.")[[1]]
  return(ex[-1])
}
#######################################################################
#' Function to add the probabilities in a vector
#' @param probs probabilities
#' @return sum of probabilities if success else -1
#' @examples add_probabilities(c(0.6,0.2))
#' @export
add_probabilities <-  function(probs) {
  #test if the probs are less than zero or not numeric
  test1 <-  any(probs < 0) || any(probs > 1)
  test2 <-  is.numeric(probs)
  if (test1 ==  FALSE && test2 ==  TRUE) {
    sum_prob <-  sum(probs)
    return(sum_prob)
  }else{
    stop("Error in probability values")
  }
}
#######################################################################
#' Function to check the sum of probabilities in a vector
#' @param avector vector of probabilities
#' @return sum of probabilities if success else error
#' @examples checksum_rowprob(c(0.6,0.4))
#' @export
checksum_rowprob <-  function(avector) {
  if (add_probabilities(avector) ==  1) {
    return(sum(avector))
  }else{
    stop("Error - row sum should be 1 in transition probability matrix")
  }
}
#######################################################################
#' Function to return random number given a distribution and parameters
#' @param prob_distbn a probability distribution and vector of parameters
#' @param param  param values to define the probability distribution
#' @return random number according to the specified distribution
#' @examples random_number_prob_distbn("beta",c(0.6,0.2))
#' @importFrom stats rbeta rgamma rlnorm rnorm runif
#' @export
random_number_prob_distbn <-  function(prob_distbn, param = NULL) {
  if (is.null(param) && prob_distbn ==  "unif") {
    this_rand <-  runif(1)
  }else{
    if (is.numeric(param) ==  TRUE) {
      if (length(param) >= 2) {
        if (prob_distbn ==  "unif") {
          if (param[1] > param[2]) {
          stop("Error - Minimum value (param[1]) should be less than maximum value (param[2])")
          }else{
            this_rand <-  runif(1, param[1], param[2])
          }
        }
        if (prob_distbn ==  "beta")
          this_rand <- rbeta(1, param[1], param[2])
        if (prob_distbn ==  "gamma")
          this_rand <- rgamma(1, param[1], param[2])
        if (prob_distbn ==  "normal")
          this_rand <- rnorm(1, param[1], param[2])
        if (prob_distbn ==  "normal")
          this_rand <- rlnorm(1, param[1], param[2])
        return(this_rand)

      }else{
        stop("Error - Minimum 2 parameters required to define a probabiity distribution")
      }

    }else{
      stop("Error - Probability distribution parameters not numeric ")
    }
  }
}

#######################################################################
#' Function to return a list of parameters given
#' @param ... any parameters set of name value pairs expected
#' @return a list of parameters
#' @examples define_parameters(rr=1)
#' @export
define_parameters <-  function(...){
  param_list = list(...)
  return(param_list)
}
#######################################################################
#' Function to return parameters with in a expression containing operators
#' @param expr an expression
#' @return parameters in the expression expr
#' @examples find_parameters_btn_operators("a+b")
#' @export
find_parameters_btn_operators <-  function(expr){
  parsed <-  toString(parse(text = expr))
  characters <-  c("\\+","-","\\*","/","%%","%/%","\\^","<",">","<=",">=","==","!=","&","\\|","!","&&","\\|\\|")
  posi <-  unlist(stringr::str_locate_all(parsed,characters))
  params <- list()
  final <-  length(posi)/2 + 1
  for (i in 1:final) {
    if (i ==  1) {
      start = 1
    }else{
      start = posi[i - 1] + 1
    }
    if (i ==  final) {
      end = nchar(parsed)
    }else{
      end = posi[i] - 1
    }
    params <- append(params, trimws(stringr::str_sub(parsed, start = start, end = end)))
  }
  return(params)
}

#######################################################################
#' Function to return parameters from a given expression separated by comma,
#' @param expr an expression
#' @return parameters in the expression expr
#' @examples find_param_from_def_prob_distrbn("gamma(mean = 10, sd =1)")
#' @export
find_param_from_def_prob_distrbn <- function(expr){
  res <- regmatches(expr, regexec('\\((.*?)\\)', expr))[[1]][2]
  posi <-  unlist(stringr::str_locate_all(res,","))
  final = length(posi)/2 + 1
  expr_found = list()
  for (i in 1:final) {
    if (i ==  1) {
      start = 1
    }else{
      start = posi[i] + 1
    }
    if (i ==  final) {
      end = nchar(res)
    }else{
      end = posi[i] - 1
    }
    this_param <- trimws(stringr::str_sub(res, start = start, end = end))
    expr_found <- append(expr_found, this_param)
  }
  final2 <-  length(expr_found)
  param_found <- list()
  for (i in 1:final2) {
    posi <-  unlist(stringr::str_locate_all(unlist(expr_found[i]),"="))
    if (length(posi) != 2 || posi[1] != posi[2]) {
      stop("Error - this expression has more than one equal sign")
    }
    this_param <- trimws(stringr::str_sub(expr_found[i], start = 1, end = posi[1 ] - 1))
    param_found <- append(param_found, this_param)
  }
  param_found <- unlist(param_found)
  return(param_found)
}
#######################################################################
#' Function to find the parameters that determine the given probability distribution
#' @param name_distri name of the probability distribution
#' @return the parameters that determine the distribution
#' @examples find_required_parameter_combs("gamma")
#' @export
find_required_parameter_combs <- function(name_distri){
  text <- trimws(toupper(name_distri))
  keyword <- NULL
  if (text ==  "LOGNORMAL" || text == "LOG NORMAL")
    keyword = list(c("meanlog", "sdlog"), c("mean", "sd"), c("mean", "sdlog"), c("meanlog", "sd"))
  if (text ==  "GAMMA")
    keyword = list(c("shape", "rate"))
  if (text ==  "BINOMIAL" || text ==  "BI NORMAL")
    keyword = list(c("size", "prob"))
  if (text ==  "BETA")
    keyword = list(c("shape1", "shape2"))
  if (text ==  "UNIFORM")
    keyword = list(c("min", "max"))
  if (text  ==  "GAUSSIAN" || text == "NOMRAL")
    keyword = list(c("mean", "sd"))
  if (text  ==  "WEIBULL")
    keyword = list(c("shape", "scale"))
  if (text  ==  "EXPONENTIAL" || text == "EXPO")
    keyword = list(c("rate"))
  if (text  ==  "POISSON")
    keyword =  list(c("lambda"))
  return(keyword)
}
#######################################################################
#' Function to find the keyword for generating random numbers for given probability distribution
#' @param text name of the probability distribution
#' @return the keyword that should be used in R for generating random numbers
#' @examples find_keyword_rand_generation("gamma")
#' @export
find_keyword_rand_generation <- function(text){
  text <- trimws(toupper(text))
  keyword <- NULL
  if (text  ==  "LOGNORMAL" || text  ==  "LOG NORMAL")
    keyword = "rlnorm"
  if (text  ==  "GAMMA")
    keyword = "rgamma"
  if (text  ==  "BINOMIAL" || text  ==  "BI NORMAL")
    keyword = "rbinom"
  if (text  ==  "BETA")
    keyword = "rbeta"
  if (text  ==  "UNIFORM")
    keyword = "runif"
  if (text  ==  "GAUSSIAN" || text  ==  "NOMRAL")
    keyword = "rnorm"
  if (text  ==  "WEIBULL")
    keyword = "rweibull"
  if (text  ==  "EXPONENTIAL" || text == "EXPO")
    keyword = "rexp"
  if (text  ==  "POISSON")
    keyword = "rpois"
  if (is.null(keyword))
    stop("Error -Didnt find a suitable probability distribution")
  return(keyword)
}
#######################################################################
#' Function to check if the parameters are sufficient to define a probability distribution and if not
#' see if we can estimate the parameters from the given parameters, e.g. for gamma distribution, shape and rate
#' can be estimated from mean and sd
#' @param params_found given parameters for generating ransom numbers
#' @param required_params the required parameter by the R code (stats package)
#' @param the_expr the expression that contain the definition
#' @param distr_key the keyword used for generating random numbers in R stats package e.g. rgamma for gamma distribution
#' @return the keyword that should be used in R for generating random numbers
#' @examples check_estimating_required_params(c("mean,sd"),c("shape", "rate"), "gamma(mean =10 ,sd=1)", "gamma")
#' @export
check_estimating_required_params <- function(params_found,required_params, the_expr,distr_key){
  distr_key <- trimws(toupper(distr_key))
  res <- regmatches(the_expr, regexec('\\((.*?)\\)', the_expr))[[1]][2]
  posi <-  unlist(stringr::str_locate_all(res,","))
  final = length(posi)/2 + 1
  values_found = list()
  for (i in 1:final) {
    if (i  ==  1) {
      start = 1
    }else{
      start = posi[i] + 1
    }
    if (i  ==  final) {
      end = nchar(res)
    }else{
      end = posi[i] - 1
    }
    this_param <- trimws(stringr::str_sub(res, start = start, end = end))
    this_value = eval(parse(text = this_param))
    values_found <- append(values_found, this_value)
  }
  values_found <- unlist(values_found)
  if (distr_key  ==  "GAMMA" && sum(params_found ==  c("mean","sd")) == 2) {
    shape = (values_found[1]/values_found[2]) ^ 2
    rate = values_found[1]/(values_found[2] ^ 2)
    esti_reqd_params <-  c(shape,rate)
    return(esti_reqd_params)
  }else{
    return(0)
  }
}
#######################################################################
#' Function to get the expression, analyse it for what distribution it is, then check if it has all parameters requires,
#' if not estimate the required parameters from given parameters, and substitute it for actual expression that can
#' be used with R stats package
#' @param the_expr given parameters for generating ransom numbers
#' @return the keyword that should be used in R for generating random numbers
#' @examples check_estimate_substitute_proper_params("gamma(mean = 10 ,sd=1)")
#' @export
check_estimate_substitute_proper_params <- function(the_expr){
  location <- stringr::str_locate(the_expr,"\\(")
  distr_key <- substr(the_expr, 1, location[1] - 1)
  rand_key <- find_keyword_rand_generation(distr_key)
  required_params <- find_required_parameter_combs(distr_key)
  params_found <- find_param_from_def_prob_distrbn(the_expr)
  index = 0
  for (i in length(required_params)) {
    index <- match(params_found,unlist(required_params[i]))
    if (sum(!is.na(index))  ==  length(index))
      i = length(required_params)
  }
  if (sum(is.na(index)) == length(index)) {
    required_params <- unlist(required_params[1])
    estim_params <- check_estimating_required_params(params_found,required_params, the_expr,distr_key)
    if (sum(estim_params)  ==  0)
      stop("Check parameters that define the probability distribution")
    create_expr = ""

    for (i in 1:length(required_params)) {
      create_expr <-  paste(create_expr, " ", required_params[i], "=", estim_params[i], sep = "")
      if (i != length(required_params))
        create_expr <-  paste(create_expr,",",sep = "")
    }
    the_real_expr <- paste(rand_key,"(1,", create_expr,")", sep = "")
  }else{
    remaining = stringr::str_split(the_expr,"\\(")[[1]][2]
    the_real_expr = paste(rand_key,"(1,",remaining, sep = "")
  }
  return(the_real_expr)
}
#######################################################################
#' Function to find the keyword for regression methods
#' @param text regression method
#' @param additional_info additional information required
#' @return the keyword that should be used in R for regression analysis
#' @examples find_keyword_regression_method("linear")
#' @export
find_keyword_regression_method <- function(text, additional_info = NA){
  text <- trimws(toupper(text))
  additional_words <- trimws(toupper(additional_info))
  keyword <- NULL
  if (text  ==  "LINEAR REGRESSION" | text  ==  "LINEAR")
    keyword = "lm"
  if (text  ==  "LOGISTIC REGRESSION" | text  ==  "LOGISTIC")
    keyword = "glm"
  if (text  ==  "MULTILEVEL MODELLING" | text  ==  "MULTILEVEL")
    keyword = "lmer"
  if (text  ==  "SURVIVAL" | text  ==  "SURVIVAL ANALYSIS") {
    if (is.na(additional_info))
      stop("Need to provide additional info for survival analysis - hazard model or KM")
    km_match <- c("KAPLAN-MEIER","KM","FLEMING-HARRINGTON", "FH2","FH")
    index <- unlist(lapply(km_match,match, additional_words))
    if (any(!is.na(index)))
      keyword = "survival::survfit"
    cox_match <- c("COX-PROPORTIONAL-HAZARD","COX PROPORTIONAL HAZARD","COX-PH", "COX PH","COXPH")
    index <- unlist(lapply(cox_match,match, additional_words))
    if (any(!is.na(index)))
      keyword = "survival::coxph"
    pa_match <- c("PARAMETRIC REGRESSION", "PARAMETRIC")
    index <- unlist(lapply(pa_match,match, additional_words))
    if (any(!is.na(index)))
      keyword = "flexsurv::flexsurvreg"
  }
  return(keyword)
}
#######################################################################
#' Function to find the keyword for survreg distribution
#' @param text distribution
#' @return the keyword - the name of distribution
#' @examples find_survreg_distribution("weibull")
#' @export
find_survreg_distribution <- function(text){
  text <- trimws(toupper(text))
  keyword <- NULL
  if (text  ==  "EXPONENTIAL" | text  ==  "EXPO")
    keyword = "exponential"
  if (text  ==  "WEIBULL")
    keyword = "weibull"
  if (text  ==  "GAUSSIAN")
    keyword = "gaussian"
  if (text  ==  "LOGGAUSSIAN" | text  ==  "LOG GAUSSIAN")
    keyword = "loggaussian"
  if (text  ==  "RAYLEIGH")
    keyword = "rayleigh"
  if (text  ==  "LOGISTIC")
    keyword = "logistic"
  if (text  ==  "LOGNORMAL" | text  ==  "LOG NORMAL")
    keyword = "lognormal"
  if (text  ==  "LOG LOGISTIC" | text  ==  "LOGLOGISTIC")
    keyword = "loglogistic"
  if (is.null(keyword))
    stop("Error - Survreg - family of distribution not matching  ")
  return(keyword)
}
#######################################################################

#' Function to find the keyword for family of distribution in glm
#' @param text distribution
#' @return the keyword - the name of distribution
#' @examples find_glm_distribution("weibull")
#' @export
find_glm_distribution <- function(text){
  text <- trimws(toupper(text))
  keyword <- NULL
  if (text  ==  "EXPONENTIAL" | text  ==  "EXPO")
    keyword = "exponential"
  if (text  ==  "WEIBULL")
    keyword = "weibull"
  if (text  ==  "GAUSSIAN")
    keyword = "gaussian"
  if (text  ==  "LOGISTIC")
    keyword = "logistic"
  if (text  ==  "LOGNORMAL" | text  ==  "LOG NORMAL")
    keyword = "lognormal"
  if (text  ==  "LOG LOGISTIC" | text  ==  "LOGLOGISTIC")
    keyword = "loglogistic"
  if (text  ==  "BINOMIAL" | text  ==  "BI NOMIAL")
    keyword = "binomial"
  if (text  ==  "POISSON")
    keyword = "poisson"

  return(keyword)
}
#######################################################################
#' Form expression to use with lm()
#' @param param_to_be_estimated  parameter of interest
#' @param indep_var the independent variable (column name in data file)
#' @param covariates list of covariates - calculations to be done before passing
#' @param interaction boolean value to indicate interaction in the case of linear regression,
#' false by default
#' @return the results of the regression analysis
#' @examples
#' formula = form_expression_lm("gre", dataset = mydata, indep_var = "gpa", covariates = NA, interaction = FALSE)
form_expression_lm <- function(param_to_be_estimated, indep_var, covariates, interaction){
  if (length(covariates) == 0 | sum(is.na(covariates)) == length(covariates)) {
    # no need to check for interaction
    fmla <- stats::as.formula(paste(param_to_be_estimated, paste("~"), paste(indep_var, collapse = "+")))
  }else{
    expre = paste(covariates[1], sep = "")
    i = 2
    while (i <= length(covariates)) {
      this = paste(covariates[i], sep = "")
      expre = paste(expre,this, sep = "+")
      i = i + 1
    }
    if (interaction == FALSE) {
      fmla <- stats::as.formula(paste(param_to_be_estimated, paste("~"), paste(expre, "+", sep = ""),
                                      paste(indep_var,collapse = "+")))
     }else{
      expre = paste(covariates[1], sep = "")
      i = 2
      while (i <= length(covariates)) {
        this = paste(covariates[i], sep = "")
        expre = paste(expre,this, sep = "*")
        i = i + 1
      }
      fmla <- stats::as.formula(paste(param_to_be_estimated, paste("~"), paste(expre, "*", sep = ""),
                                      paste(indep_var,collapse = "*")))
     }
  }
  return(fmla)
}
