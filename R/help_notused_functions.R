#######################################################################
#' Function to add the probabilities in a vector
#' @param probs probabilities
#' @return sum of probabilities if success else -1
#' @examples
#' add_probabilities(c(0.6, 0.2))
#' @export
#' @details
#' if probabilities not numeric or fall out of range (0 and 1)
#' return error else add the probabilities together
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
#' Returns a random number according to the given distribution
#' and corresponding parameters
#' For uniform distribution with no parameters, this will be std uniform
#' other wise will check if param 1 is less than param 2
#' If not for uniform distribution, number of parameters should be minimum of
#'  two and also the parameters should be numeric
random_number_prob_distbn <- function(prob_distbn, param = NULL) {
  if (is.null(param) && prob_distbn == "unif") {
    this_rand <- runif(1)
  } else {
    if (is.numeric(param) == TRUE) {
      if (length(param) >= 2) {
        if (prob_distbn == "unif") {
          if (param[1] > param[2]) {
            stop("Error - Minimum value (param[1]) should be less than maximum
                 value (param[2])")
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
        stop("Error - Minimum 2 parameters required to define a
             probabiity distribution")
      }
    } else {
      stop("Error - Probability distribution parameters not numeric ")
    }
  }
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
  if (rlang::is_empty(given_unit) | any(is.na(given_unit)) |
      length(given_unit) == 0 | identical(given_unit, "")
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
    warning("Something went wrong- couldnt convert the frquency to
            the correct basis")
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
  if (rlang::is_empty(given_time) | any(is.na(given_time)) |
      length(given_time) == 0| identical(given_time, "") |
      any(given_time == "null") | any(given_time == "Null")) {
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





