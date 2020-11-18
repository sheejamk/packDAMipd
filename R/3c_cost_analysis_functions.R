##############################################################################
#' Function to estimate the cost of tablets and patches taken (from IPD)
#' @param form the form of medication either tablet or patch
#' @param ind_part_data IPD
#' @param name_med name of medication
#' @param dose_med dose of medication
#' @param dose_unit unit of strength
#' @param no_taken how many taken
#' @param freq_taken frequency of medication
#' @param basis_time basis for time
#' @param unit_cost_data  unit costs data
#' @param unit_cost_column column name of unit cost in unit_cost_data
#' @param cost_calculated_in column name of unit where the cost is calculated
#' @param strength_expressed_in column name of strength in the unit cost data
#' @param list_period_timepoint list of time period at each timepoint if coded
#' @param list_of_code_names if names is coded, give the code:name pairs
#' @param list_of_code_freq if frequency is coded, give the
#' code:frequency pairs
#' @param list_of_code_dose_unit if unit is coded, give the code:unit pairs
#' @param equiv_dose if cost per equivalent doses are to be calculates,
#' provide equiv_dose
#' @return the calculated cost of tablets along with original data
#' @examples
#' freq_desc <- c(
#'   "Once a day", "Twice a day", "Three times a day",
#'   "Four times a day", "Five times a day", "Six times a day",
#'   "Seven times a day", "Eight times a day", "Nine times a day",
#'   "Ten times a day", "Once every 2 days", "Once every 3 days",
#'   "Once every 4 days", "Once every 5 days", "Once every 6 days",
#'   "Once a week", "Twice a week", "Thrice a week", "Four times a week"
#' )
#' codes <- c(seq(1:19))
#' this_list <- list(freq_desc, codes)
#' med_costs_file <- system.file("extdata", "average_unit_costs_med.csv",
#' package = "packDAMipd")
#' data_file <- system.file("extdata", "resource_use_t.csv",
#' package = "packDAMipd")
#' ind_part_data <- load_trial_data(data_file)
#' med_costs <- load_trial_data(med_costs_file)
#' res <- microcosting_tablets_patches("tablets",
#'   ind_part_data, "Name", "tab_dosage", "tab_dosage_unit", "tab_no_taken",
#'   "tab_frequency", "day", med_costs, "UnitCost",
#'   "StrengthUnit", "Strength", list(c("4 weeks", "1 week"), c(1, 2)),
#'   list(c("Buprenorphine", "Morphine"), c(1, 2)), this_list,
#'   list(c("mcg", "mg", "gm"), c(1, 2, 3)), "tab_equiv_dose"
#' )
#' @export
#' @details
#' Assumes individual level data has name of medication, dose, dose unit,
#' number taken, frequency taken, and basis time
#' Assumes unit cost data contains the name of medication, form/type,
#' strength, unit of strength (or the unit in which the cost calculated),
#' preparation, unit cost, size and size unit
#' (in which name, forms, size, size unit, and preparation  are not passed on)
microcosting_tablets_patches <- function(form, ind_part_data,
                                         name_med, dose_med, dose_unit,
                                         no_taken, freq_taken, basis_time,
                                         unit_cost_data, unit_cost_column,
                                         cost_calculated_in,
                                         strength_expressed_in,
                                         list_period_timepoint = NULL,
                                         list_of_code_names = NULL,
                                         list_of_code_freq = NULL,
                                         list_of_code_dose_unit = NULL,
                                         equiv_dose = NULL) {

  #Error - data should not be NULL
  if (is.null(ind_part_data) | is.null(unit_cost_data))
    stop("data should not be NULL")

  #Checking if the required parameters are NULL or NA
  variables_check <- list(form, name_med, dose_med, dose_unit,
                         no_taken, freq_taken, basis_time, unit_cost_column,
                         cost_calculated_in, strength_expressed_in)
  results <- sapply(variables_check, check_null_na)
  names_check <- c("form", "name_med", "dose_med", "dose_unit",
                  "no_taken", "freq_taken", "basis_time", "unit_cost_column",
                  "cost_calculated_in", "strength_expressed_in")
  if (any(results != 0)) {
    indices <- which(results < 0)
    stop(paste("Error - the variables can not be NULL or NA,
               check the variable(s)", names_check[indices]))
  }

  if (toupper(form) != "PATCH" & toupper(form) != "PATCHES" &
     toupper(form) != "TABLET" & toupper(form) != "TABLETS")
    stop("Form of medication should be patch or tablet")

  # check columns exist in individual data
  info_list <- c(name_med, dose_med, dose_unit, no_taken, freq_taken)
  checks <- sapply(info_list, IPDFileCheck::check_column_exists, ind_part_data)
  if (sum(checks) != 0) {
    stop("Atleast one of the required columns not found")
  }

  # check columns exist in unit cost  data
  info_list <- c(unit_cost_column, cost_calculated_in, strength_expressed_in)
  checks <- sapply(info_list, IPDFileCheck::check_column_exists, unit_cost_data)
  if (sum(checks) != 0) {
    stop("Atleast one of the required columns in unit cost data not found")
  }
  # get colnames for name, form, dosage and unit
  name_pattern <- c("name", "drug", "medication", "med")
  bool_res <- unlist(lapply(name_pattern,
                            IPDFileCheck::check_colno_pattern_colname,
                            colnames(unit_cost_data)))
  if (any(bool_res)) {
    name_ind <- which(bool_res == TRUE)
  } else {
    stop("Error- Name of the medication is not found. Please use name,
         drug, medication, or med to indicate the medication")
  }
  name_col_no <- IPDFileCheck::get_colno_pattern_colname(name_pattern[name_ind],
                                                colnames(unit_cost_data))

  form_pattern <- c("form", "drug form", "patch/tablet", "type")
  bool_res <- unlist(lapply(form_pattern,
                            IPDFileCheck::check_colno_pattern_colname,
                            colnames(unit_cost_data)))
  if (any(bool_res)) {
    form_ind <- which(bool_res == TRUE)
  } else {
    stop("Error- Form of the medication is not found. Please use form,
    drug form, patch/tablet, or type to indicate
         the type of medication")
  }
  form_col_no <- IPDFileCheck::get_colno_pattern_colname(form_pattern[form_ind],
                                                  colnames(unit_cost_data))
  unit_col_no <- IPDFileCheck::get_columnno_fornames(unit_cost_data,
                                                     cost_calculated_in)
  dosage_col_no <- IPDFileCheck::get_columnno_fornames(unit_cost_data,
                                                       strength_expressed_in)

  # if the codes are being used for name, dosage, frequency and time period
  # list_of_code_names is a list of (list of codes and list of names)
  # if they are valid, assign the names to codes, read the code from data
  # and read the corresponding names using the earlier assignment
  if (!is.null(list_of_code_names) & sum(is.na(list_of_code_names)) == 0) {
    name_and_code <- stats::setNames(as.list(list_of_code_names[[1]]),
                                     list_of_code_names[[2]])
    ipd_codes <- ind_part_data[[name_med]]
    name_from_code <- name_and_code[ipd_codes]
    index <- which(is.na(ipd_codes))
    if (length(index) > 0)
      name_from_code[index] <- NA
  } else {
    name_from_code <- ind_part_data[[name_med]]
  }
  if (is.null(unlist(name_from_code)) | sum(is.na(name_from_code)) != 0) {
    stop("Error - name_from_code can not be null - check the input for
         list of names and codes")
  }
  # get the frequency from code, as same as name
  if (!is.null(list_of_code_freq) & sum(is.na(list_of_code_freq)) == 0) {
    freq_code <- stats::setNames(as.list(list_of_code_freq[[1]]),
                                 list_of_code_freq[[2]])
    ipd_codes <- ind_part_data[[freq_taken]]
    freq_desc_from_code <- freq_code[ipd_codes]
    index <- which(is.na(ipd_codes))
    if (length(index) > 0)
      freq_desc_from_code[index] <- NA

    # but as the basis time might be different convert the frequency
    # to given basis
    freq_given_basis <- unlist(lapply(freq_desc_from_code,
                                      convert_freq_diff_basis, basis_time))
  } else {
    freq_given_basis <- unlist(lapply(ind_part_data[[freq_taken]],
                                      convert_freq_diff_basis, basis_time))
  }

  if (sum(is.na(unlist(freq_given_basis))) == length(freq_given_basis)) {
    stop("Error - freq_given_basis can not be null - check the
         input for list of frequency")
  }
  if (!is.null(list_of_code_dose_unit) &
      sum(is.na(list_of_code_dose_unit)) == 0) {
    unit_and_code <- stats::setNames(as.list(list_of_code_dose_unit[[1]]),
                                     list_of_code_dose_unit[[2]])
    ipd_codes <- ind_part_data[[dose_unit]]
    unit_from_code <- unit_and_code[ipd_codes]
    index <- which(is.na(ipd_codes))
    if (length(index) > 0)
      unit_from_code[index] <- NA

  } else {
    unit_from_code <- ind_part_data[[dose_unit]]
  }
  if (is.null(unlist(unit_from_code))) {
    stop("Error - unit_from_code can not be null - check the input for
         list of codes and names")
  }

  timepoint_details <- get_timepoint_details(ind_part_data)
  if (is.null(list_period_timepoint)) {
    period_desc_from_code <- ind_part_data[[timepoint_details$name]]
  } else {
    if (sum(is.na(list_period_timepoint) != 0)) {
      stop("Error - list of code and time points can not be NA")
    }
    period_code <- stats::setNames(as.list(list_period_timepoint[[1]]),
                                   list_period_timepoint[[2]])
    timepoint_codes <- ind_part_data[[timepoint_details$name]]
    period_desc_from_code <- period_code[timepoint_codes]
    index <- which(is.na(timepoint_codes))
    if (length(index) > 0)
      period_desc_from_code[index] <- NA

  }
  if (is.null(unlist(period_desc_from_code))  |
      sum(is.na(period_desc_from_code)) != 0) {
    stop("Error - period_desc_from_code can not be null -
         check the input for list of period")
  }
  if (is.null(equiv_dose)) {
    equiv_dose_div <- 1
  } else {
    equiv_dose_div <- ind_part_data[[equiv_dose]]
  }
  list_total_med_basis <- list()
  list_total_cost_basis <- list()
  list_total_cost_basis_equiv_dose <- list()
  list_total_cost_timeperiod <- list()
  list_total_cost_timeperiod_equiv_dose <- list()
  for (i in 1:nrow(ind_part_data)) {
    name_medication <- name_from_code[i]
    dose_medication <- ind_part_data[[dose_med]][i]
    if (!is.null(dose_medication) & !is.na(dose_medication)) {
      how_many_taken <- ind_part_data[[no_taken]][i]
      freq_multiplier <- freq_given_basis[i]
      this_unit <- unit_from_code[i]
      subset1 <- unit_cost_data[toupper(unit_cost_data[[name_col_no]])
                                == toupper(name_medication), ]
      if (toupper(form) == "TABLET" | toupper(form) == "TABLETS")
        subset2 <- subset1[subset1[form_col_no] == "Tablet" |
                             subset1[form_col_no] == "Tablets" |
                             subset1[form_col_no] == "tablet" |
                             subset1[form_col_no] == "tablets", ]
      else
        subset2 <- subset1[subset1[form_col_no] == "Patch" |
                             subset1[form_col_no] == "Patches" |
                             subset1[form_col_no] == "patch" |
                             subset1[form_col_no] == "patches", ]

      unit_used_costing <- unique(subset2[[unit_col_no]])
      if (length(unit_used_costing) != 1) {
        stop("unit used for costing tablets is not unique !!!")
      }
      total_med_basis <- how_many_taken * freq_multiplier
      if (toupper(form) == "TABLET" | toupper(form) == "TABLETS")
        unit_multiplier <- convert_weight_diff_basis(this_unit,
                                                     unit_used_costing)
      else
        unit_multiplier <- convert_wtpertimediff_basis(this_unit,
                                                       unit_used_costing)
      if (is.na(unit_multiplier))
        stop("The unit is not identifiable to convert that used for costing")

      if (any(subset2[[dosage_col_no]] == dose_medication)) {
        unit_cost_med_prep <- subset2[subset2[[dosage_col_no]]
                                   == dose_medication &
                                  subset2[[unit_col_no]]
                                  == unit_used_costing, ][[unit_cost_column]]
      } else {
        stop("The used dosage is not in costing table")
      }
      total_cost_basis <- total_med_basis * unit_cost_med_prep * unit_multiplier
      total_cost_basis_equiv_dose <- total_cost_basis / equiv_dose_div[i]
      period_given_basis <- convert_to_given_timeperiod(period_desc_from_code[i],
                                                        basis_time)

      total_cost_timeperiod <- total_cost_basis * period_given_basis
      total_cost_timeperiod_equiv_dose <-
        total_cost_timeperiod / equiv_dose_div[i]
    } else {
      total_med_basis <- NA
      total_cost_basis <- NA
      total_cost_basis_equiv_dose <- NA
      total_cost_timeperiod <- NA
      total_cost_timeperiod_equiv_dose <- NA
    }
    if (toupper(form) == "TABLET" | toupper(form) == "TABLETS")
      keywd <- "tablets"
    else
      keywd <- "patches"

    list_total_med_basis <- append(list_total_med_basis, total_med_basis)
    list_total_cost_basis <- append(list_total_cost_basis, total_cost_basis)
    list_total_cost_basis_equiv_dose <- append(list_total_cost_basis_equiv_dose,
                                               total_cost_basis_equiv_dose)
    list_total_cost_timeperiod <- append(list_total_cost_timeperiod,
                                         total_cost_timeperiod)
    list_total_cost_timeperiod_equiv_dose <-
      append(list_total_cost_timeperiod_equiv_dose,
                                          total_cost_timeperiod_equiv_dose)
  }
  this_name <- paste("totmed_basis_", keywd, sep = "")
  ind_part_data[[this_name]] <- unlist(list_total_med_basis)
  this_name <- paste("totcost_basis_", keywd, sep = "")
  ind_part_data[[this_name]] <- unlist(list_total_cost_basis)
  this_name <- paste("totcost_basis_equiv_dose_", keywd, sep = "")
  ind_part_data[[this_name]] <- unlist(list_total_cost_basis_equiv_dose)
  this_name <- paste("totcost_timeperiod_", keywd, sep = "")
  ind_part_data[[this_name]] <- unlist(list_total_cost_timeperiod)
  this_name <- paste("totcost_timeperiod_equiv_dose_", keywd, sep = "")
  ind_part_data[[this_name]] <- unlist(list_total_cost_timeperiod_equiv_dose)
  return(ind_part_data)
}

##############################################################################
#' Function to estimate the cost of liquids taken (from IPD)
#' @param ind_part_data IPD
#' @param name_med name of medication
#' @param dose_med dose of medication
#' @param dose_unit unit of strength
#' @param bottle_size_unit unit which size of the bottle is expressed
#' @param bottle_size how many taken
#' @param bottle_remain frequency of medication
#' @param unit_cost_data  unit costs data
#' assumes unit cost data contains the columns, name of medication,
#' form/type, strength,
#' unit of strength, preparation, unitcost, size and size unit
#' (in which except name and form are to be passed in)
#' @param unit_cost_column column name of unit cost in unit_cost_data
#' @param cost_calculated_in column name of unit where the cost is calculated
#' @param strength_column column name of strength in the unit cost data
#' @param list_period_timepoint list of time period at each timepoint
#' @param preparation  preparation of liquid that is used
#' @param list_of_code_names if names is coded, give the code:name pairs
#' @param list_of_code_dose_unit if unit is coded, give the code:unit pairs
#' @param list_of_code_bottle_size list of codes for the bottle sizes fi used
#' @param equiv_dose if cost per equivalent doses are to be calculates,
#' provide equiv_dose
#' @param basis_time time needed to convert , default is day
#' @return the calculated cost of liquids along with original data
#' @examples
#' med_costs_file <- system.file("extdata", "average_unit_costs_med.csv",
#' package = "packDAMipd")
#' data_file <- system.file("extdata", "resource_use_l.csv",
#' package = "packDAMipd")
#' ind_part_data <- load_trial_data(data_file)
#' med_costs <- load_trial_data(med_costs_file)
#' res <- microcosting_liquids(
#'   ind_part_data, "Drug", "liq_dosage", "liquid_dose_unit",
#'   "liquid_bottle_size", "liquid_bottle_remain_time", NULL,
#'   med_costs, "UnitCost", "SizeUnit", "Strength", NULL, NULL, NULL,
#'   NULL, NULL, "liquid_equiv_dose", "day"
#' )
#' @importFrom tm removeNumbers
#' @export
microcosting_liquids <- function(ind_part_data,
                                 name_med, dose_med, dose_unit,
                                 bottle_size, bottle_remain,
                                 bottle_size_unit = NULL,
                                 unit_cost_data, unit_cost_column,
                                 cost_calculated_in, strength_column,
                                 list_period_timepoint,
                                 preparation = NULL,
                                 list_of_code_names = NULL,
                                 list_of_code_dose_unit = NULL,
                                 list_of_code_bottle_size = NULL,
                                 equiv_dose = NULL, basis_time = "day") {
  #Error - data should not be NULL
  if (is.null(ind_part_data) | is.null(unit_cost_data))
    stop("data should not be NULL")
  #Checking if the required parameters are NULL or NA
  variables_check <- list(name_med, dose_med, dose_unit,
                         bottle_size, bottle_remain, basis_time,
                         unit_cost_column,
                         cost_calculated_in, strength_column)
  results <- sapply(variables_check, check_null_na)
  names_check <- c("name_med", "dose_med", "dose_unit",
                  "bottle_size", "bottle_remain", "basis_time",
                  "unit_cost_column",
                  "cost_calculated_in", "strength_column")
  if (any(results != 0)) {
    indices <- which(results < 0)
    stop(paste("Error - the variables can not be NULL or NA, check the
               variable(s)",
               names_check[indices]))
  }
  if (is.null(equiv_dose)) {
    equiv_dose_div <- 1
  } else {
    equiv_dose_div <- ind_part_data[[equiv_dose]]
  }
  # get colnames for name, form, dosage and unit
  name_pattern <- c("name", "drug", "medication", "med")
  bool_res <- unlist(lapply(name_pattern,
                            IPDFileCheck::check_colno_pattern_colname,
                            colnames(unit_cost_data)))
  if (any(bool_res)) {
    name_ind <- which(bool_res == TRUE)
  } else {
    stop("Error- Name of the medication is not found. Please use name, drug,
         medication, or med to indicate the medication")
  }
  name_col_no <- IPDFileCheck::get_colno_pattern_colname(name_pattern[name_ind],
                                                  colnames(unit_cost_data))

  form_pattern <- c("form", "drug form", "patch/tablet/liquid/injection",
                    "type")
  bool_res <- unlist(lapply(form_pattern,
                            IPDFileCheck::check_colno_pattern_colname,
                            colnames(unit_cost_data)))
  if (any(bool_res)) {
    form_ind <- which(bool_res == TRUE)
  } else {
    stop("Error- Form of the medication is not found. Please use form,
    drug form, patch/tablet/liquid, or type to indicate
         the type of medication")
  }
  form_col_no <- IPDFileCheck::get_colno_pattern_colname(form_pattern[form_ind],
                                                  colnames(unit_cost_data))

  unit_col_no <- IPDFileCheck::get_columnno_fornames(unit_cost_data,
                                                     cost_calculated_in)
  dosage_col_no <- IPDFileCheck::get_columnno_fornames(unit_cost_data,
                                                       strength_column)
  size_col_no <- IPDFileCheck::get_columnno_fornames(unit_cost_data, "size")

  prepar_pattern <- c("preparation")
  bool_res <- unlist(lapply(prepar_pattern,
                            IPDFileCheck::check_colno_pattern_colname,
                            colnames(unit_cost_data)))
  if (!is.null(preparation)) {
    if (any(bool_res)) {
      prepar_ind <- which(bool_res == TRUE)
    }
    prepar_col_no <- IPDFileCheck::get_columnno_fornames(unit_cost_data,
                                                prepar_pattern[prepar_ind])
  }

  # if the codes are being used for name, dosage, frequency an time period
  if (!is.null(list_of_code_names)) {
    name_and_code <- stats::setNames(as.list(list_of_code_names[[1]]),
                                     list_of_code_names[[2]])
    ipd_codes <- ind_part_data[[name_med]]
    name_from_code <- name_and_code[ipd_codes]
    index <- which(is.na(ipd_codes))
    if (length(index) > 0)
      name_from_code[index] <- NA

  } else {
    name_from_code <- ind_part_data[[name_med]]
  }
  if (is.null(unlist(name_from_code))) {
    stop("Error - name_from_code can not be null - check the input for list
         of names and codes")
  }
  if (!is.null(list_of_code_bottle_size)) {
    size_and_code <- stats::setNames(as.list(list_of_code_bottle_size[[1]]),
                                     list_of_code_bottle_size[[2]])
    ipd_codes <- ind_part_data[[bottle_size]]
    size_from_code <- size_and_code[ipd_codes]
    index <- which(is.na(ipd_codes))
    if (length(index) > 0)
      size_from_code[index] <- NA
  } else {
    size_from_code <- ind_part_data[[bottle_size]]
  }
  if (is.null(unlist(size_from_code))) {
    stop("Error - size_from_code can not be null - check the input for list
         of size and codes")
  }

  if (!is.null(list_of_code_dose_unit)) {
    unit_and_code <- stats::setNames(as.list(list_of_code_dose_unit[[1]]),
                                     list_of_code_dose_unit[[2]])
    ipd_codes <- ind_part_data[[dose_unit]]
    unit_from_code <- unit_and_code[ipd_codes]
    index <- which(is.na(ipd_codes))
    if (length(index) > 0)
      unit_from_code[index] <- NA
  } else {
    unit_from_code <- ind_part_data[[dose_unit]]
  }

  if (is.null(unlist(unit_from_code))) {
    stop("Error - unit_from_code can not be null - check the input for list of
         codes and names")
  }
  timepoint_details <- get_timepoint_details(ind_part_data)
  if (is.null(list_period_timepoint)) {
    period_desc_from_code <- ind_part_data[[timepoint_details$name]]
  } else {
    period_code <- stats::setNames(as.list(list_period_timepoint[[1]]),
                                   list_period_timepoint[[2]])
    timepoint_codes <- ind_part_data[[timepoint_details$name]]
    period_desc_from_code <- period_code[timepoint_codes]
    index <- which(is.na(timepoint_codes))
    if (length(index) > 0)
      period_desc_from_code[index] <- NA
  }
  if (is.null(unlist(period_desc_from_code))) {
    stop("Error - period_desc_from_code can not be null - check the input for
         list of period")
  }
  list_total_bottle_timeperiod <- list()
  list_total_bottle_timeperiod_equiv_dose <- list()
  list_total_cost_timeperiod <- list()
  list_total_cost_timeperiod_equiv_dose <- list()
  for (i in 1:nrow(ind_part_data)) {
    name_medication <- name_from_code[i]
    dose_medication <- ind_part_data[[dose_med]][i]
    if (!is.null(dose_medication) & !is.na(dose_medication)) {
      this_size <- size_from_code[i]
      remain_time <- ind_part_data[[bottle_remain]][i]
      subset1 <- unit_cost_data[toupper(unit_cost_data[[name_col_no]])
                                == toupper(name_medication), ]
      subset2 <- subset1[subset1[form_col_no] == "Liquid" |
                           subset1[form_col_no] == "Liquids", ]
      unit_used_costing <- unique(subset2[[unit_col_no]])
      if (length(unit_used_costing) != 1) {
        stop("unit used for costing tablets is not unique !!!")
      }
      if (!is.null(preparation)) {
        this_preparation <- ind_part_data[[preparation]][i]
      }
      if (is.null(bottle_size_unit)) {
        size_unit <- trimws(tm::removeNumbers(unlist(this_size)))
        matches <- regmatches(this_size, gregexpr("[[:digit:]]+", this_size))
        size_number <- as.numeric(unlist(matches))
      } else {
        size_unit <- ind_part_data[[bottle_size_unit]]
        size_number <- this_size
      }
      size_multiplier <- convert_volume_basis(size_unit, unit_used_costing)
      if (is.na(size_multiplier))
        stop("The unit is not identifiable to convert that used for costing")

      total_med_for_remain_time <- size_number * size_multiplier

      if (any(subset2[[dosage_col_no]] == dose_medication)) {
        if (!is.null(preparation)) {
          unit_cost_med_prep <- subset2[subset2[[dosage_col_no]] == dose_medication &
                                subset2[[prepar_col_no]] == this_preparation &
                                subset2[[size_col_no]] == size_number &
              subset2[[unit_col_no]] == unit_used_costing, ][[unit_cost_column]]
        } else {
          unit_cost_med_prep <- subset2[subset2[[dosage_col_no]] == dose_medication &
                                subset2[[size_col_no]] == size_number &
                                subset2[[unit_col_no]] ==
                                unit_used_costing, ][[unit_cost_column]]
        }
      } else {
        stop("The used dosage is not in costing table")
      }
      if (unit_cost_med_prep <= 0 | is.null(unit_cost_med_prep) |
          is.na(unit_cost_med_prep))
        stop("Error - unit cost for the medicine is not valid")
      cost_bottle <- unit_cost_med_prep
      period_given_basis <- convert_to_given_timeperiod(period_desc_from_code[i],
                                                        basis_time)
      bottle_used_time_given_basis <- convert_to_given_timeperiod(remain_time,
                                                                  basis_time)
      med_required_timeperiod <- (total_med_for_remain_time / bottle_used_time_given_basis) * period_given_basis
      no_bottle_timeperiod <- ceiling(med_required_timeperiod / size_number)
      # need to check if what it actually mean by equivalent dose
      # 1mg/ml of liquid equivalent to mg of morphine ?
      no_bottle_timeperiod_equiv_dose <- no_bottle_timeperiod / equiv_dose_div[i]
      total_cost_timeperiod <- no_bottle_timeperiod * cost_bottle
      total_cost_timeperiod_equiv_dose <- total_cost_timeperiod / equiv_dose_div[i]
    } else {
      no_bottle_timeperiod <- NA
      no_bottle_timeperiod_equiv_dose <- NA
      total_cost_timeperiod <- NA
      total_cost_timeperiod_equiv_dose <- NA
    }
    list_total_bottle_timeperiod <- append(list_total_bottle_timeperiod,
                                           no_bottle_timeperiod)
    list_total_bottle_timeperiod_equiv_dose <- append(list_total_bottle_timeperiod_equiv_dose,
                                              no_bottle_timeperiod_equiv_dose)
    list_total_cost_timeperiod <- append(list_total_cost_timeperiod,
                                         total_cost_timeperiod)
    list_total_cost_timeperiod_equiv_dose <- append(list_total_cost_timeperiod_equiv_dose,
                                          total_cost_timeperiod_equiv_dose)
  }
  ind_part_data[["tot_timeperiod_bottle"]] <- unlist(list_total_bottle_timeperiod)
  ind_part_data[["tot_timeperiod_equiv_dose_bottle"]] <- unlist(list_total_bottle_timeperiod_equiv_dose)
  ind_part_data[["totcost_timeperiod_liquids"]] <- unlist(list_total_cost_timeperiod)
  ind_part_data[["totcost_timeperiod_equiv_dose_liquids"]] <- unlist(list_total_cost_timeperiod_equiv_dose)
  return(ind_part_data)
}
##############################################################################
#' Function to estimate the cost of resource use taken (from IPD)
#' @param ind_part_data IPD
#' @param name_use_col name of the column containing resource use
#' @param each_length_num_use list of column names that shows
#' length/number of  repeated use eg. hospital admission
#' @param each_use_provider_indicator list of column names that shows the
#' bool indicators for the use of resource if this
#' is to be included for the particular provider, say an nhs hospital use
#' @param unit_length_use the column name that contains how many or
#' how long used
#' @param unit_cost_data  unit costs data where the assumption is that
#' the unit cost for resources
#' such as hospital use, gp visit are listed in column resource/resource
#' use with unit costs in another
#' column and the units calculated as in another column
#' @param name_use_unit_cost name of resource use (the column name in
#' the unit cost
#' data is assumed to be name/resource/type etc) in unit cost data
#' @param unit_cost_column column name of unit cost in unit_cost_data
#' @param cost_calculated_in column name of unit where the cost is calculated
#' @param list_code_use_indicator if the column name_use_col  shows codes
#' to indicate the resource use provide the list of codes and resource use
#' for eg., list(c("yes", "no", c(1,2)))
#' @param list_code_provider_indicator  column each_use_provider_indicator
#' shows codes to indicate the resource use
#' provide the list of codes and resource use for eg.,
#' list(c("yes", "no", c(1,2)))
#' @return the calculated cost of resource uses along with original data
#' @examples
#' costs_file <- system.file("extdata", "costs_resource_use.csv",
#' package = "packDAMipd")
#' datafile <- system.file("extdata", "resource_use_hc_2.csv",
#' package = "packDAMipd")
#' ind_part_data <- load_trial_data(datafile)
#' unit_cost_data <- load_trial_data(costs_file)
#' res <- costing_resource_use(
#'   ind_part_data[1, ],
#'   "hospital_admission_1",
#'   list("length_1", "length_2"),
#'   list("nhs_1", "nhs_2"),
#'   "day",
#'   unit_cost_data, "Inpatient hospital admissions", "UnitCost",
#'   "UnitUsed",
#'   NULL, NULL
#' )
#' @export
costing_resource_use <- function(ind_part_data,
                                 name_use_col,
                                 each_length_num_use = NULL,
                                 each_use_provider_indicator = NULL,
                                 unit_length_use = "day",
                                 unit_cost_data,
                                 name_use_unit_cost,
                                 unit_cost_column,
                                 cost_calculated_in,
                                 list_code_use_indicator = NULL,
                                 list_code_provider_indicator = NULL) {
  # Error - data should not be NULL
  if (is.null(ind_part_data) | is.null(unit_cost_data))
    stop("data should not be NULL")

  # check columns exist in individual data
  info_list <- c(name_use_col)
  checks <- sapply(info_list, IPDFileCheck::check_column_exists, ind_part_data)
  if (sum(checks) != 0) {
    stop("Any one of the required columns can not be found in IPD")
  }
  # if the use is indicated by a code in the data
  if (is.null(list_code_use_indicator)) {
    use_ind_from_code <- toupper(ind_part_data[[name_use_col]])
  } else {
    use_code <- stats::setNames(as.list(list_code_use_indicator[[1]]),
                                list_code_use_indicator[[2]])
    use_from_data <- ind_part_data[[name_use_col]]
    use_ind_from_code <- toupper(use_code[use_from_data])

    index <- which(is.na(use_from_data))
    if (length(index) > 0)
      use_ind_from_code[index] <- NA
  }

  # check length of each_length_use, each_number_use,
  # each_use_provider_indicator to be same
  if (!is.null(each_length_num_use) & !is.null(each_use_provider_indicator)) {
    if (length(each_length_num_use) != length(each_use_provider_indicator)) {
      stop("length of each event of resource use should be equal to
           length of use from provider indicator")
    }
  }

  # check columns exist in unit cost data
  info_list <- c(unit_cost_column, cost_calculated_in)
  checks <- sapply(info_list, IPDFileCheck::check_column_exists, unit_cost_data)
  if (sum(checks) != 0) {
    stop("Any one of the required columns can not be found in unit cost data")
  }
  # find the column name under which each resource use is listed
  name_pattern <- c("name", "resource", "resource use", "type of resource")
  bool_res <- unlist(lapply(name_pattern,
                            IPDFileCheck::check_colno_pattern_colname,
                            colnames(unit_cost_data)))
  if (any(bool_res)) {
    name_ind <- which(bool_res == TRUE)
  } else {
    stop("Error- Name of the resource use is not found. Please use name,
    resource, resource use,
         or type of resource to indicate the medication")
  }
  name_col_no <- IPDFileCheck::get_colno_pattern_colname(name_pattern[name_ind],
                                                  colnames(unit_cost_data))

  # initialising the result columns
  column_names <- colnames(ind_part_data)
  new_col <- paste("totcost_", name_use_col, sep = "")
  if (is.na(match(new_col, column_names))) {
    ind_part_data[[new_col]] <- NA
  }
  # if the name of the resource use in unit cost is not null, find the subset of
  # data with that resource use
  # or get the column name with the resource use given at the ith row of the ind
  # data (these might be
  # the resource use where it is not needed to specify the use by a yes/no and
  # rather by number of contacts)
  for (i in 1:nrow(ind_part_data)) {
    if (!is.null(name_use_unit_cost)) {
      subset1 <- unit_cost_data[toupper(unit_cost_data[[name_col_no]]) ==
                                  toupper(name_use_unit_cost), ]
    } else {
      subset1 <- unit_cost_data[toupper(unit_cost_data[[name_col_no]]) ==
        toupper(ind_part_data[[name_use_col]][i]), ]
    }
    # if no matching rows found, throw error
    if (nrow(subset1) < 1) {
      stop("Error- Name of resource use in unit cost data not found")
    }
    # get the unit cost of that resource use
    unit_cost <- subset1[[unit_cost_column]]

    # if they need to indicate that use is to be accounted e.g NHS hospital
    if (!is.null(name_use_unit_cost)) {
      # identify, yes or true as indicator for use
      ind <- match(toupper(use_ind_from_code[i]), c("YES", "TRUE"))
    } else {
      ind <- 1
    }
    # get the unit expressed
    if (!is.na(ind) & ind > 0) {
      if (IPDFileCheck::check_column_exists(unit_length_use,
                                            ind_part_data) != 0) {
        uni_expr <- unit_length_use
      } else {
        uni_expr <- ind_part_data[[unit_length_use]][i]
      }
      total_length_num <- 0
      # each_length_num_use is the column names where it is given the
      # length of use
      for (m in seq_len(length(each_length_num_use))) {
        # check each column exists
        if (IPDFileCheck::check_column_exists(each_length_num_use[m],
                                              ind_part_data) == 0) {
          # check use corresponding to the provider is indicated
          if (IPDFileCheck::check_column_exists(each_use_provider_indicator[m],
                                                ind_part_data) == 0) {
            # if the use corresponding to the provider is coded,
            # then use the code
            if (is.null(list_code_provider_indicator)) {
              this_column_name <- unlist(each_use_provider_indicator[m])
              use_desc_from_code <- toupper(ind_part_data[[this_column_name]][i])
            } else {
              # else read it from ind data
              use_code <- stats::setNames(as.list(list_code_provider_indicator[[1]]),
                                          list_code_provider_indicator[[2]])
              this_column_name <- unlist(each_use_provider_indicator[m])
              use_from_data <- ind_part_data[[this_column_name]][i]
              use_desc_from_code <- toupper(use_code[use_from_data])
              index <- which(is.na(use_from_data))
              if (length(index) > 0)
                use_desc_from_code[index] <- NA
            }
          } else {
            if (!is.null(list_code_provider_indicator)) {
              stop("Error - column do not exist and given codes for provider
                   indicator")
            } else {
              use_desc_from_code <- "YES"
            }
          }
          # for multiple uses of resource, add the numbers together
          if (use_desc_from_code == "YES" | use_desc_from_code == "TRUE") {
            this_column_name <- unlist(each_length_num_use[m])
            total_length_num <- total_length_num + ind_part_data[[this_column_name]][i]
          }
        }
      }
      # find the cost calculated subset
      cost_calculatedin <- subset1[[cost_calculated_in]]
      if (cost_calculatedin == uni_expr | cost_calculatedin == paste("per",
                                                            uni_expr)) {
        total_cost <- total_length_num * unit_cost
      } else {
        stop("units of resource use expressed and calculated are different")
      }
      ind_part_data[[new_col]][i] <- total_cost
    }
  }
  return(ind_part_data)
}
