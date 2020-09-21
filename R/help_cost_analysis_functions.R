##########################################################################################################
#' Function to check the variable null or NA
#' @param variable name of variable to check
#' @return -1 or -2 as error, else return 0 as success
#' @export
check_null_na <- function(variable) {
  #Error - variable can not be NULL or NA
  if (is.null(variable)) {
    return(-1)
  }else{
    if (is.na(variable)) return(-2)
    else return(0)
  }
}
##########################################################################################################
#' Function to check the variable null or NA
#' @param word word for the number
#' @return return the number
#' @details
#' https://stackoverflow.com/questions/18332463/convert-written-number-to-number-in-r
#' @export
word2num <- function(word){
  wsplit <- strsplit(tolower(word)," ")[[1]]
  one_digits <- list(zero = 0, one = 1, two = 2, three = 3, four = 4, five = 5,
                     six = 6, seven = 7, eight = 8, nine = 9)
  teens <- list(eleven = 11, twelve = 12, thirteen = 13, fourteen = 14, fifteen = 15,
                sixteen = 16, seventeen = 17, eighteen = 18, nineteen = 19)
  ten_digits <- list(ten = 10, twenty = 20, thirty = 30, forty = 40, fifty = 50,
                     sixty = 60, seventy = 70, eighty = 80, ninety = 90)
  doubles <- c(teens,ten_digits)
  out <- 0
  i <- 1
  while (i <= length(wsplit)) {
    if (i != 1 && wsplit[i] == "and")
      i = i + 1
    j <- 1
    if (i == 1 && wsplit[i] == "hundred")
      temp <- 100
    else if (i == 1 && wsplit[i] == "thousand")
      temp <- 1000
    else if (wsplit[i] %in% names(one_digits))
      temp <- as.numeric(one_digits[wsplit[i]])
    else if (wsplit[i] %in% names(teens))
      temp <- as.numeric(teens[wsplit[i]])
    else if (wsplit[i] %in% names(ten_digits))
      temp <- (as.numeric(ten_digits[wsplit[i]]))
    if (i < length(wsplit) && wsplit[i + 1] == "hundred") {
      if (i > 1 && wsplit[i - 1] %in% c("hundred","thousand"))
        out <- out + 100*temp
      else
        out <- 100*(out + temp)
      j <- 2
    }
    else if (i < length(wsplit) && wsplit[i + 1] == "thousand") {
      if (i > 1 && wsplit[i - 1] %in% c("hundred","thousand"))
        out <- out + 1000*temp
      else
        out <- 1000*(out + temp)
      j <- 2
    }
    else if (i < length(wsplit) && wsplit[i + 1] %in% names(doubles)) {
      temp <- temp*100
      out <- out + temp
    }
    else{
      out <- out + temp
    }
    i <- i + j
  }
  return(list(word,out))
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
#' @importFrom rlang is_empty
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
  if (is.null(freq_req_basis))
    stop("Given frequency is not identifiable ")
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
    stop("Something wrong-couldnt convert the frquency to correct basis")
  } else {
    return(freq_req_basis)
  }
}
##################################################
#' Convert unit strength to given basis
#' @param given_unit given unit
#' @param basis given basis, default is "mg"
#' @return converted unit
#' @examples
#' convert_weight_diff_basis("mg")
#' convert_weight_diff_basis("kilogram", "micro gram")
#' @export
convert_weight_diff_basis <- function(given_unit, basis = "mg") {
  unit_req_basis <- NULL
  if (!is.null(given_unit)) {
    given_unit <- trimws(tolower(given_unit))
  }
  nospace_unit = tolower(gsub("[[:space:]]", "", given_unit))
  basis = tolower(gsub("[[:space:]]", "", basis))
  if (nospace_unit != "gram"  & nospace_unit != "milligram" &
      nospace_unit != "gm"  & nospace_unit != "mg" &
      nospace_unit != "microgram"  & nospace_unit != "microgm" &
      nospace_unit != "mcg" &
      nospace_unit != "kilogram"  & nospace_unit != "kilo"  &
      nospace_unit != "kg") {
    stop("given unit is not of weight")
  }
  if (rlang::is_empty(nospace_unit) | any(is.na(nospace_unit)) |
      length(nospace_unit) == 0 | identical(nospace_unit, "") | is.null(nospace_unit) |
      any(nospace_unit == "null") | any(nospace_unit == "Null")) {
    unit_req_basis <- NA
  } else {
    if (basis == "mcg" | basis == "microgram"
        | basis == "microgm") {
      if (nospace_unit == "mg" | nospace_unit == "milligram") {
        unit_req_basis <- 1e3
      }
      if (nospace_unit == "g" | nospace_unit == "gm"
          | nospace_unit == "gram") {
        unit_req_basis <- 1e6
      }
      if (nospace_unit == "kg" | nospace_unit == "kilo"
          | nospace_unit == "kilogram") {
        unit_req_basis <- 1e9
      }
      if (nospace_unit == "mcg" | nospace_unit == "microgram"
          | nospace_unit == "microgm") {
        unit_req_basis <- 1
      }
    }
    if (basis == "mg" | basis == "milligram") {
      if (nospace_unit == "mg" | nospace_unit == "milligram") {
        unit_req_basis <- 1
      }
      if (nospace_unit == "g" | nospace_unit == "gm"
          | nospace_unit == "gram") {
        unit_req_basis <- 1000
      }
      if (nospace_unit == "kg" | nospace_unit == "kilo"
          | nospace_unit == "kilogram") {
        unit_req_basis <- 1e6
      }
      if (nospace_unit == "mcg" | nospace_unit == "microgram"
          | nospace_unit == "microgm") {
        unit_req_basis <- 0.001
      }
    }
    if (basis == "gram" | basis == "gm") {
      if (nospace_unit == "mg" | nospace_unit == "milligram") {
        unit_req_basis <- 1e-3
      }
      if (nospace_unit == "g" | nospace_unit == "gm"
          | nospace_unit == "gram") {
        unit_req_basis <- 1
      }
      if (nospace_unit == "kg" | nospace_unit == "kilo"
          | nospace_unit == "kilogram") {
        unit_req_basis <- 1e3
      }
      if (nospace_unit == "mcg" | nospace_unit == "microgram"
          | nospace_unit == "microgm") {
        unit_req_basis <- 1e-6
      }
    }
    if (basis == "kg" | basis == "kilo"
        | basis == "kiloram") {
      if (nospace_unit == "mg" | nospace_unit == "milligram") {
        unit_req_basis <- 1e-6
      }
      if (nospace_unit == "g" | nospace_unit == "gm"
          | nospace_unit == "gram") {
        unit_req_basis <- 1e-3
      }
      if (nospace_unit == "kg" | nospace_unit == "kilo"
          | nospace_unit == "kilogram") {
        unit_req_basis <- 1
      }
      if (nospace_unit == "mcg" | nospace_unit == "microgram"
          | nospace_unit == "microgm") {
        unit_req_basis <- 1e-9
      }
    }
  }
  if (is.null(unit_req_basis)) {
    stop("Something wrong - couldnt convert the weight to the correct basis")
  } else {
    return(unit_req_basis)
  }
}

#######################################################################
#' Convert volume  to given basis
#' @param given_unit given unit
#' @param basis given basis, default is "ml"
#' @return converted unit
#' @examples
#' convert_volume_basis("ml", "liter")
#' @export
convert_volume_basis <- function(given_unit, basis = "ml") {
  if (!is.null(given_unit)) {
    given_unit <- trimws(tolower(given_unit))
  }
  unit_req_vol <- NULL
  given_unit = tolower(gsub("[[:space:]]", "", given_unit))
  basis = tolower(gsub("[[:space:]]", "", basis))
  if (given_unit != "liter"  & given_unit != "l" &
      given_unit != "milliliter" &  given_unit != "microliter"
      &  given_unit != "mcl"  & given_unit != "ml"  ) {
    stop("given unit is not of volume")
  }
  if (rlang::is_empty(given_unit) | any(is.na(given_unit)) |
      length(given_unit) == 0 | identical(given_unit, "") |
      any(given_unit == "null") | any(given_unit == "Null")) {
    unit_req_vol <- NA
  } else {
    unit_req_vol <- 1
    if (basis == "ml" | basis == "milliliter" ) {
      if ( given_unit == "microliter" | given_unit == "mcl") {
        unit_req_vol <- 1 / 1000
      }
      if (given_unit == "l" | given_unit == "liter") {
        unit_req_vol <- 1000
      }
    }
    if (basis == "microliter" | basis == "mcl") {
      if (given_unit == "l" | given_unit == "liter") {
        unit_req_vol <- 1e6
      }
      if (given_unit == "ml" | given_unit == "milliliter") {
        unit_req_vol <- 1000
      }
    }
    if (basis == "l" | basis == "liter") {
      if (given_unit == "ml" |
          given_unit == "milliliter" ) {
        unit_req_vol <- 1 / 1000
      }

      if (given_unit == "microliter" |  given_unit == "mcl") {
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
    basis_wt = tolower(gsub("[[:space:]]", "", basis_wt))
    basis_time = tolower(gsub("[[:space:]]", "", basis_time))
    unit_req_wt <- 1
    unit_req_time <- 1
    if (basis_wt == "mg" | basis_wt == "milligram") {
      if (given_wt == "mcg" | given_wt == "microgram") {
        unit_req_wt <- 1 / 1000
      }
      if (given_wt == "gram" | given_wt == "gm") {
        unit_req_wt <- 1000
      }
    }
    if (basis_wt == "mcg" | basis_wt == "microgram") {
      if (given_wt == "mg" | given_wt == "milligram") {
        unit_req_wt <- 1000
      }
      if (given_wt == "gram" | given_wt == "gm") {
        unit_req_wt <- 1e6
      }
    }
    if (basis_wt == "gram" | basis_wt == "gm") {
      if (given_wt == "mg" |  given_wt == "milligram") {
        unit_req_wt <- 1 / 1000
      }
      if (given_wt == "mcg" |  given_wt == "microgram") {
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
    result = sum(is.na(suppressWarnings(is.numeric(first_part))))
    if (result > 0) {
      out <- tryCatch(
        {
          as.numeric(unlist(word2num(first_part))[2])
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
    if (is.null(sec_part) | is.na(sec_part)) {
      stop("Error - missing the unit of time")
    }
    unit_req_time = NULL
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
    if (is.null(unit_req_time)) {
      stop("Error in converting the unit")
    }
    unit_req_basis <- out * unit_req_time
  }
  return(unit_req_basis)
}


#### earlier version fo costing patches
#' ##########################################################################################################
#' #' Function to estimate the cost of patches taken given individual level
#' #' data and cost data
#' #' @param ind_part_data IPD
#' #' @param name_med name of the medication taken as patch
#' #' @param dose_med dose of medication
#' #' @param dose_unit unit of strength
#' #' @param no_taken how many taken
#' #' @param freq_taken frequency of medication
#' #' @param basis_time basis for time
#' #' @param unit_cost_data  unit costs data
#' #' @param unit_cost_column column name of unit cost in unit_cost_data
#' #' @param cost_calculated_in column name of unit where the cost is calculated
#' #' @param strength_expressed_in column name of strength in the unit cost data
#' #' @param list_period_timepoint list of time period at each timepoint if they are coded
#' #' @param list_of_code_names if names are coded, give the code:name pairs
#' #' @param list_of_code_freq if frequency is coded, give the code:frequency pairs
#' #' @param list_of_code_dose_unit if unit is coded, give the code:unit pairs
#' #' @param equiv_dose if cost per equivalent doses are to be calculated, provide equiv_dose
#' #' @return the calculated cost of patches along with original data
#' #' @examples
#' #' med_costs_file <- system.file("extdata", "average_unit_costs_med.csv", package = "packDAMipd")
#' #' data_file <- system.file("extdata", "resource_use_p.csv", package = "packDAMipd")
#' #' ind_part_data <- load_trial_data(data_file)
#' #' med_costs <- load_trial_data(med_costs_file)
#' #' res <- microcosting_patches(
#' #'   ind_part_data, "Drug", "patch_strength", "patch_dose_unit", "patch_no_taken",
#' #'   "patch_frequency", "day", med_costs, "UnitCost", "StrengthUnit", "Strength",
#' #'   list(c("4 weeks", "1 week"), c(1, 2)), NULL, NULL, NULL, "patch_equiv_dose"
#' #' )
#' #' @export
#' #' @details
#' #' Assumes individual level data has name of medication, dose, dose unit, number taken,
#' #' frequency taken, and basis time
#' #' Assumes unit cost data contains the name of medication, form/type, strength,
#' #' unit of strength (or the unit in which the cost calculated), preparation,
#' #'  unit cost, size and size unit
#' #' (in which name, forms, size, size unit, and preparation  are not passed on)
#'
#' microcosting_patches <- function(ind_part_data,
#'                                  name_med, dose_med, dose_unit,
#'                                  no_taken, freq_taken, basis_time,
#'                                  unit_cost_data, unit_cost_column,
#'                                  cost_calculated_in, strength_expressed_in,
#'                                  list_period_timepoint = NULL,
#'                                  list_of_code_names = NULL,
#'                                  list_of_code_freq = NULL,
#'                                  list_of_code_dose_unit = NULL,
#'                                  equiv_dose = NULL) {
#'   #Error - data should not be NULL
#'   if (is.null(ind_part_data) | is.null(unit_cost_data))
#'     stop("data should not be NULL")
#'
#'   #Checking if the required parameters are NULL or NA
#'   variables_check = list(name_med, dose_med, dose_unit,
#'                          no_taken, freq_taken, basis_time, unit_cost_column,
#'                          cost_calculated_in, strength_expressed_in)
#'   results = sapply(variables_check, check_null_na)
#'   names_check = c("name_med","dose_med", "dose_unit",
#'                   "no_taken", "freq_taken", "basis_time", "unit_cost_column",
#'                   "cost_calculated_in", "strength_expressed_in")
#'   if (any(results !=0)) {
#'     indices = which(results < 0)
#'     stop(paste("Error - the variables can not be NULL or NA, check the variable(s)", names_check[indices]))
#'   }
#'
#'   # check columns exist in individual data
#'   info_list <- c(name_med, dose_med, dose_unit, no_taken, freq_taken)
#'   checks <- sapply(info_list, IPDFileCheck::check_column_exists, ind_part_data)
#'   if (sum(checks) != 0) {
#'     stop("Atleast one of the required columns in individual data not found")
#'   }
#'
#'   # check columns exist in unit cost  data
#'   info_list <- c(unit_cost_column, cost_calculated_in, strength_expressed_in)
#'   checks <- sapply(info_list, IPDFileCheck::check_column_exists, unit_cost_data)
#'   if (sum(checks) != 0) {
#'     stop("Atleast one of the required columns in unit cost data not found")
#'   }
#'
#'   # get colnames for name, form, dosage and unit
#'   name_pattern <- c("name", "drug", "medication", "med")
#'   bool_res <- unlist(lapply(name_pattern, IPDFileCheck::check_colno_pattern_colname, colnames(unit_cost_data)))
#'   if (any(bool_res)) {
#'     name_ind <- which(bool_res == TRUE)
#'   } else {
#'     stop("Error- Name of the medication is not found. Please use name, drug, medication, or med to indicate the medication")
#'   }
#'
#'   form_pattern <- c("form", "drug form", "patch/tablet/liquid", "type")
#'   bool_res <- unlist(lapply(form_pattern, IPDFileCheck::check_colno_pattern_colname, colnames(unit_cost_data)))
#'   if (any(bool_res)) {
#'     form_ind <- which(bool_res == TRUE)
#'   } else {
#'     stop("Error- Form of the medication is not found. Please use form, drug form, patch/tablet/liquid, or type to indicate
#'          the type of medication")
#'   }
#'   name_col_no <- IPDFileCheck::get_colno_pattern_colname(name_pattern[name_ind],
#'                                                          colnames(unit_cost_data))
#'   form_col_no <- IPDFileCheck::get_colno_pattern_colname(form_pattern[form_ind],
#'                                                          colnames(unit_cost_data))
#'   dosage_col_no <- IPDFileCheck::get_columnno_fornames(unit_cost_data, strength_expressed_in)
#'   unit_col_no <- IPDFileCheck::get_columnno_fornames(unit_cost_data, cost_calculated_in)
#'
#'   # if the codes are being used for name, dosage, frequency and time period
#'   # list_of_code_names is a list of (list of codes and list of names)
#'   # if they are valid, assign the names to codes, read the code from data
#'   # and read the corresponding names using the earlier assignment
#'   if (!is.null(list_of_code_names) & sum(is.na(list_of_code_names)) ==0) {
#'     name_and_code <- stats::setNames(as.list(list_of_code_names[[1]]), list_of_code_names[[2]])
#'     ipd_codes <- ind_part_data[[name_med]]
#'     name_from_code <- name_and_code[ipd_codes]
#'   } else {
#'     name_from_code <- ind_part_data[[name_med]]
#'   }
#'   # get the frequency from code, as same as name
#'   if (!is.null(list_of_code_freq) & sum(is.na(list_of_code_freq)) ==0) {
#'     freq_code <- stats::setNames(as.list(list_of_code_freq[[1]]), list_of_code_freq[[2]])
#'     ipd_codes <- ind_part_data[[freq_taken]]
#'     freq_desc_from_code <- freq_code[ipd_codes]
#'
#'     # but as the basis time might be different convert the frequency to given basis
#'     freq_given_basis <- unlist(lapply(freq_desc_from_code, convert_freq_diff_basis, basis_time))
#'   } else {
#'     freq_given_basis <- unlist(lapply(ind_part_data[[freq_taken]], convert_freq_diff_basis, basis_time))
#'   }
#'
#'   if (!is.null(list_of_code_dose_unit) & sum(is.na(list_of_code_dose_unit)) ==0) {
#'     unit_and_code <- stats::setNames(as.list(list_of_code_dose_unit[[1]]), list_of_code_dose_unit[[2]])
#'     ipd_codes <- ind_part_data[[dose_unit]]
#'     unit_from_code <- unit_and_code[ipd_codes]
#'   } else {
#'     unit_from_code <- ind_part_data[[dose_unit]]
#'   }
#'   timepoint_details <- get_timepoint_details(ind_part_data)
#'   if (is.null(list_period_timepoint)) {
#'     period_desc_from_code <- ind_part_data[[timepoint_details$name]]
#'   } else {
#'     if (sum(is.na(list_period_timepoint)!=0)) {
#'       stop("Error - list of code and time points can not be NA")
#'     }
#'     period_code <- stats::setNames(as.list(list_period_timepoint[[1]]), list_period_timepoint[[2]])
#'     timepoint_codes <- ind_part_data[[timepoint_details$name]]
#'     period_desc_from_code <- period_code[timepoint_codes]
#'   }
#'
#'   # if dose equivalent is null, it will assume as 1 or read from the data
#'   if (is.null(equiv_dose)) {
#'     equiv_dose_div <- 1
#'   } else {
#'     equiv_dose_div <- ind_part_data[[equiv_dose]]
#'   }
#'   list_total_med_basis <- list()
#'   list_total_cost_basis <- list()
#'   list_total_cost_basis_equiv_dose <- list()
#'   list_total_cost_timeperiod <- list()
#'   list_total_cost_timeperiod_equiv_dose <- list()
#'   for (i in 1:nrow(ind_part_data)) {
#'     # get name and dose for each individual
#'     name_medication <- name_from_code[i]
#'     dose_medication <- ind_part_data[[dose_med]][i]
#'     if (!is.null(dose_medication) & !is.na(dose_medication)) {
#'       # get number taken
#'       how_many_taken <- ind_part_data[[no_taken]][i]
#'       # get the frequency given the basis time
#'       freq_multiplier <- freq_given_basis[i]
#'       if (is.na(freq_multiplier)) {
#'         stop("Error - frequency multiplier can not be NA")
#'       }
#'       # get the unit of dose
#'       this_unit <- unit_from_code[i]
#'       # get the data with the name of medication matches to the given name in the unit cost data
#'       subset1 <- unit_cost_data[toupper(unit_cost_data[[name_col_no]]) == toupper(name_medication),]
#'       if (nrow(subset1) != 0) {
#'         # get one more subset data with form of medication is patch or patches
#'         subset2 <- subset1[subset1[form_col_no] == "Patch" | subset1[form_col_no] == "Patches", ]
#'         if (nrow(subset2) == 0)
#'           stop("Error - no matched records of Patched found")
#'       }else{
#'         stop("Error - no matched records found with the given name of medication")
#'       }
#'       # by now the cost data should have all the rows correspoding to given medication of given type
#'       # get the unit used for costing - it has to be uniquely identifiable other wise error
#'       unit_used_costing <- unique(subset2[[unit_col_no]])
#'       if (length(unit_used_costing) != 1) {
#'         stop("unit used for costing patches is not unique !!!")
#'       }
#'       # estimate the total medication taken given the time basis
#'       total_med_basis <- how_many_taken * freq_multiplier
#'
#'       # now convert the unit of dose to the dose used for costing
#'       unit_multiplier <- convert_wtpertimediff_basis(this_unit, unit_used_costing)
#'       if (unit_multiplier != 1) {
#'         stop("The unit used is not same as that used in calculating costs")
#'       }
#'
#'       #  check for the correct dose and unit used for costing
#'       if (any(subset2[[dosage_col_no]] == dose_medication)) {
#'         unit_cost_med_prep <- subset2[subset2[[dosage_col_no]] == dose_medication &
#'                                         subset2[[unit_col_no]] == unit_used_costing, ][[unit_cost_column]]
#'       } else {
#'         stop("The used dosage is not in costing table")
#'       }
#'       # total cost for the basis time is then the product of total med and unit cost and the multiplier
#'       total_cost_basis <- total_med_basis * unit_cost_med_prep * unit_multiplier
#'       # find the total cost for the equivalent dose given
#'       total_cost_basis_equiv_dose <- total_cost_basis / equiv_dose_div[i]
#'
#'       # now the total cost we want probably is for certaini time period, so find that time periods
#'       period_given_basis <- convert_to_given_timeperiod(period_desc_from_code[i], basis_time)
#'       # now find the total cost for that time period
#'       total_cost_timeperiod <- total_cost_basis * period_given_basis
#'       #now find the total cost for that time period for equivalen dose
#'       total_cost_timeperiod_equiv_dose <- total_cost_timeperiod / equiv_dose_div[i]
#'     } else {
#'       total_med_basis <- NA
#'       total_cost_basis <- NA
#'       total_cost_basis_equiv_dose <- NA
#'       total_cost_timeperiod <- NA
#'       total_cost_timeperiod_equiv_dose <- NA
#'     }
#'     list_total_med_basis <- append(list_total_med_basis, total_med_basis)
#'     list_total_cost_basis <- append(list_total_cost_basis, total_cost_basis)
#'     list_total_cost_basis_equiv_dose <- append(list_total_cost_basis_equiv_dose, total_cost_basis_equiv_dose)
#'     list_total_cost_timeperiod <- append(list_total_cost_timeperiod, total_cost_timeperiod)
#'     list_total_cost_timeperiod_equiv_dose <- append(list_total_cost_timeperiod_equiv_dose, total_cost_timeperiod_equiv_dose)
#'   }
#'   ind_part_data[["tot_basis_patches"]] <- unlist(list_total_med_basis)
#'   ind_part_data[["totcost_patches_basis"]] <- unlist(list_total_cost_basis)
#'   ind_part_data[["totcost_patches_basis_equiv_dose"]] <- unlist(list_total_cost_basis_equiv_dose)
#'   ind_part_data[["totcost_timeperiod_patches"]] <- unlist(list_total_cost_timeperiod)
#'   ind_part_data[["totcost_patches_timeperiod_equiv_dose"]] <- unlist(list_total_cost_timeperiod_equiv_dose)
#'
#'   return(ind_part_data)
#' }
