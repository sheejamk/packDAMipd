
##############################################################################
#' Function to estimate the cost of tablets and patches taken (from IPD)
#' @param ind_part_data IPD
#' @param name_med name of medication
#' @param brand_med brand name of medication if revealed
#' @param dose_med dose of medication used
#' @param unit_med unit of medication ; use null if its along with the dose
#' @param bottle_size size of the bottle used
#' @param bottle_size_unit unit of bottle volume
#' @param bottle_lasts how long the bottle lasted
#' @param bottle_lasts_unit time unit of how long the bottle lasted
#' @param preparation_dose dose if preparation is given
#' @param preparation_unit unit of preparatio dose
#' @param timeperiod time period for cost calculation
#' @param unit_cost_data  unit costs data
#' @param unit_cost_column column name of unit cost in unit_cost_data
#' @param cost_calculated_per column name of unit where the cost is calculated
#' @param strength_column column column name that has strength of medication
#' @param list_of_code_names if names is coded, give the code:name pairs,
#' optional
#' @param list_of_code_brand if brand names  are coded, give the
#' code:brand pairs, optional
#' @param list_of_code_dose_unit if unit is coded, give the code:unit pairs,
#' optional
#' @param list_of_code_bottle_size_unit  list of bottle size units and codes
#' @param list_of_code_bottle_lasts_unit list of time of bottle lasts and codes
#' @param list_preparation_dose_unit list of preparation dose units and codes
#' @param eqdose_covtab table to get the conversion factor for equivalent
#' doses, optional
#' @param basis_strength_unit strength unit to be taken as basis
#' required for total medication calculations
#' @return the calculated cost of tablets along with original data
#' @examples
#' med_costs_file <- system.file("extdata", "average_unit_costs_med_brand.csv",
#' package = "packDAMipd")
#' data_file <- system.file("extdata", "medication_liq.xlsx",
#' package = "packDAMipd")
#' ind_part_data <- load_trial_data(data_file)
#' med_costs <- load_trial_data(med_costs_file)
#' conv_file <- system.file("extdata", "Med_calc.xlsx",package = "packDAMipd")
#' table <- load_trial_data(conv_file)
#' res <- microcosting_liquids_wide(
#' ind_part_data = ind_part_data, name_med = "liq_name", brand_med =  NULL,
#' dose_med = "liq_strength", unit_med = NULL, bottle_size = "liq_bottle_size",
#' bottle_size_unit = NULL, bottle_lasts = "liq_lasts", bottle_lasts_unit = NULL,
#' preparation_dose = NULL, preparation_unit = NULL, timeperiod = "4 months",
#' unit_cost_data = med_costs, unit_cost_column = "UnitCost",
#' cost_calculated_per = "Basis", strength_column = "Strength",
#' list_of_code_names = NULL, list_of_code_brand = NULL,
#' list_of_code_dose_unit = NULL, list_of_code_bottle_size_unit = NULL,
#' list_of_code_bottle_lasts_unit = NULL,list_preparation_dose_unit = NULL,
#' eqdose_covtab = table, basis_strength_unit = NULL)
#' @export
#' @importFrom dplyr %>%
microcosting_liquids_wide <- function(ind_part_data,
                                      name_med,
                                      brand_med = NULL,
                                      dose_med,
                                      unit_med = NULL,
                                      bottle_size,
                                      bottle_size_unit = NULL,
                                      bottle_lasts,
                                      bottle_lasts_unit = NULL,
                                      preparation_dose = NULL,
                                      preparation_unit = NULL,
                                      timeperiod,
                                      unit_cost_data,
                                      unit_cost_column,
                                      cost_calculated_per,
                                      strength_column,
                                      list_of_code_names = NULL,
                                      list_of_code_brand = NULL,
                                      list_of_code_dose_unit = NULL,
                                      list_of_code_bottle_size_unit = NULL,
                                      list_of_code_bottle_lasts_unit = NULL,
                                      list_preparation_dose_unit = NULL,
                                      eqdose_covtab = NULL,
                                      basis_strength_unit = NULL) {
  internal_basis_time = "day"
  # check the form as liquids
  words <- c("liquid", "liq", "injection", "inject", "solution", "ampoule",
             "liquids", "injections", "solutions", "ampoules")
  generated_list <- generate_wt_vol_units()
  wt_per_vol_units <- generated_list$weight_per_vol
  time_units <-  generate_wt_time_units()$time_units

  #Error - data should not be NULL
  if (is.null(ind_part_data) | is.null(unit_cost_data))
    stop("data should not be NULL")

  #Checking if the required parameters are NULL or NA
  variables_check <- list(name_med, dose_med,
                          bottle_size, bottle_lasts,
                          timeperiod, unit_cost_column,
                          cost_calculated_per, strength_column)
  results <- sapply(variables_check, check_null_na)
  names_check <- c("name_med", "dose_med", "bottle_size", "bottle_lasts",
                   "timeperiod", "unit_cost_column",
                   "cost_calculated_per", "strength_column")
  if (any(results != 0)) {
    indices <- which(results < 0)
    stop(paste("Error - the variables can not be NULL or NA,
               check the variable(s)", names_check[indices]))
  }
  # if null,keep proper strength unit and time unit
  if (!is.null(basis_strength_unit)) {
    if (is.na(basis_strength_unit)) {
      basis_strength_unit <- "mg/ml"
      basis_wt_unit <- "mg"
      basis_vol_unit <- "ml"
    } else {
      if (!(basis_strength_unit %in% wt_per_vol_units))
        stop("Basis strength unit is not valid")
      index <- stringr::str_locate(basis_strength_unit, "/")
      basis_wt_unit <- stringr::str_sub(basis_strength_unit, 1, index[1] - 1)
      basis_vol_unit <- stringr::str_sub(basis_strength_unit, index[2] + 1,
                                         nchar(basis_strength_unit))
    }
  } else {
    basis_strength_unit <- "mg/ml"
    basis_wt_unit <- "mg"
    basis_vol_unit <- "ml"
  }

  ## Check the columns in IPD and get the columns
  brand_check <- return0_if_not_null_na(brand_med)
  unit_med_check <-  return0_if_not_null_na(unit_med)
  bottle_size_unit_check <- return0_if_not_null_na(bottle_size_unit)
  bottle_lasts_unit_check <- return0_if_not_null_na(bottle_lasts_unit)
  preparation_dose_check <- return0_if_not_null_na(preparation_dose)
  preparation_unit_check <- return0_if_not_null_na(preparation_unit)

  check_list <- c(unit_med_check, brand_check, bottle_size_unit_check,
                  bottle_lasts_unit_check, preparation_dose_check,
                  preparation_unit_check)
  partial_list <- c(name_med, dose_med, bottle_size, bottle_lasts)
  another_list <- list(unit_med, brand_med, bottle_size_unit, bottle_lasts_unit,
                    preparation_dose, preparation_unit)
  another_list[check_list == -1] <- -1
  info_list <- unlist(append(partial_list, another_list))

  ipd_cols_exists <- list()
  for (i in seq_len(length(info_list))) {
    if (info_list[i] != -1) {
      check <- IPDFileCheck::check_column_exists(info_list[i], ind_part_data)
      if (sum(check) != 0) {
        res <- grep(info_list[i], colnames(ind_part_data))
        if (length(res) == 0)
          stop("Atleast one of the required columns not found")
        ipd_cols_exists[length(ipd_cols_exists) + 1] <- list(res)
      }
    } else {
      ipd_cols_exists[length(ipd_cols_exists) + 1] <- -1
    }
  }
  names_med_ipd_cols <- unlist(ipd_cols_exists[1])
  doses_med_ipd_cols <- unlist(ipd_cols_exists[2])
  bottle_size_ipd_cols <- unlist(ipd_cols_exists[3])
  bottle_lasts_ipd_cols <- unlist(ipd_cols_exists[4])
  if (unit_med_check != -1)
    unit_med_ipd_cols  <- unlist(ipd_cols_exists[5])
  if (brand_check != -1)
    brand_med_ipd_cols  <- unlist(ipd_cols_exists[6])
  if (bottle_size_unit_check != -1)
    bottle_size_unit_ipd_cols  <- unlist(ipd_cols_exists[7])
  if (bottle_lasts_unit_check != -1)
    bottle_lasts_unit_ipd_cols  <- unlist(ipd_cols_exists[8])
  if (preparation_dose_check != -1)
    preparation_dose_ipd_cols  <- unlist(ipd_cols_exists[9])
  if (preparation_unit_check != -1)
    preparation_unit_ipd_cols  <- unlist(ipd_cols_exists[10])

  # check columns exist in unit cost  data
  info_list <- c(unit_cost_column, cost_calculated_per, strength_column)
  checks <- sapply(info_list, IPDFileCheck::check_column_exists, unit_cost_data)
  if (sum(checks) != 0) {
    stop("Atleast one of the required columns in unit cost data not found")
  }

  ## if the information is coded
  names_from_ipd_code <- encode_codes_data(list_of_code_names,
                                           names_med_ipd_cols, ind_part_data)
  if (is.null(unlist(names_from_ipd_code)) |
      sum(is.na(unlist(names_from_ipd_code))) ==
      length(unlist(names_from_ipd_code))) {
    stop("Error - name_from_code can not be null - check the input for
         list of names and codes")
  }

  if (unit_med_check == -1) {
    med_ipd_dose <- ind_part_data %>%
      dplyr::select(dplyr::all_of(doses_med_ipd_cols))
    med_dose_unlist <- unlist(med_ipd_dose)
    unit_from_ipd_code <- gsub("[0-9\\.]", "", med_dose_unlist)
    unit_from_ipd_code <- matrix(unit_from_ipd_code,
                                 nrow = dim(med_ipd_dose)[1])
    colnames(unit_from_ipd_code) <- colnames(med_ipd_dose)
    unit_from_ipd_code <- as.data.frame(unit_from_ipd_code)
  } else {
    med_ipd_dose <- ind_part_data %>%
      dplyr::select(dplyr::all_of(doses_med_ipd_cols))
    med_ipd_dose <- as.data.frame(med_ipd_dose)
    unit_from_ipd_code <- encode_codes_data(list_of_code_dose_unit,
                                            unit_med_ipd_cols, ind_part_data)
    if (is.null(unlist(unit_from_ipd_code)) |
        sum(is.na(unlist(unit_from_ipd_code))) ==
        length(unlist(unit_from_ipd_code))) {
      stop("Error - unit_from_code can not be null - check the input for
         list of units")
    }
  }

  if (bottle_lasts_unit_check == -1) {
    bottle_lasts_ipd <- ind_part_data %>%
      dplyr::select(dplyr::all_of(bottle_lasts_ipd_cols))
    bottle_lasts_unit_unlist <- unlist(bottle_lasts_ipd)
    unit_from_lasts <- gsub("[0-9\\.]", "", bottle_lasts_unit_unlist)
    unit_from_lasts <- matrix(unit_from_lasts,
                              nrow = dim(bottle_lasts_ipd)[1])
    colnames(unit_from_lasts) <- colnames(bottle_lasts_ipd)
    bottle_lasts_unit_from_ipd_code <- as.data.frame(unit_from_lasts)
  } else {
    bottle_lasts_ipd <- ind_part_data %>%
      dplyr::select(dplyr::all_of(bottle_lasts_ipd_cols))
    bottle_lasts_unit_from_ipd_code <-
      encode_codes_data(list_of_code_bottle_lasts_unit,
                        bottle_lasts_unit_ipd_cols, ind_part_data)
    if (is.null(unlist(bottle_lasts_unit_from_ipd_code)) |
        sum(is.na(unlist(bottle_lasts_unit_from_ipd_code))) ==
        length(unlist(bottle_lasts_unit_from_ipd_code))) {
      stop("Error - size_unit_from_code can not be null - check the input for
           bottle lasts unit code")
    }
  }

  if (bottle_size_unit_check == -1) {
    bottle_size_ipd <- ind_part_data %>%
      dplyr::select(dplyr::all_of(bottle_size_ipd_cols))
    bottle_size_unlist <- unlist(bottle_size_ipd)
    unit_from_size <- gsub("[0-9]+", "", bottle_size_unlist)
    unit_from_size <- matrix(unit_from_size,
                             nrow = dim(bottle_size_ipd)[1])
    colnames(unit_from_size) <- colnames(bottle_size_ipd)
    bottle_size_unit_from_ipd_code <- as.data.frame(unit_from_size)
  } else {
    bottle_size_ipd <- ind_part_data %>%
      dplyr::select(dplyr::all_of(bottle_size_ipd_cols))
    bottle_size_unit_from_ipd_code <-
      encode_codes_data(list_of_code_bottle_size_unit,
                        bottle_size_unit_ipd_cols,
                        ind_part_data)
    if (is.null(unlist(bottle_size_unit_from_ipd_code)) |
        sum(is.na(unlist(bottle_size_unit_from_ipd_code))) ==
        length(unlist(bottle_size_unit_from_ipd_code))) {
      stop("Error - size_unit_from_code can not be null - check the input for
         bottel size unit code")
    }
  }

  if (brand_check != -1) {
    brand_from_ipd_code <- encode_codes_data(list_of_code_brand,
                                             brand_med_ipd_cols, ind_part_data)
    if (is.null(unlist(brand_from_ipd_code)) |
        sum(is.na(unlist(brand_from_ipd_code))) ==
        length(unlist(brand_from_ipd_code))) {
      stop("Error - size_unit_from_code can not be null - check the input for
         brand  code")
    }
  }

  # dose is specified as 2mg/ml unit is not separate, no coding required
  if (preparation_dose_check != -1 & preparation_unit_check == -1) {
    preparation_ipd_dose <- ind_part_data %>%
      dplyr::select(dplyr::all_of(preparation_dose_ipd_cols))
    preparation_dose_unlist <- unlist(preparation_ipd_dose)
    prepare_unit_from_ipd_code <- gsub("[0-9\\.]", "", preparation_dose_unlist)
    prepare_unit_from_ipd_code <- matrix(prepare_unit_from_ipd_code,
                                         nrow = dim(preparation_ipd_dose)[1])
    colnames(prepare_unit_from_ipd_code) <- colnames(preparation_ipd_dose)
    prepare_unit_from_ipd_code <- as.data.frame(prepare_unit_from_ipd_code)
  }
  if (preparation_dose_check != -1 & preparation_unit_check != -1) {
    preparation_ipd_dose <- ind_part_data %>%
      dplyr::select(dplyr::all_of(preparation_dose_ipd_cols))
    preparation_ipd_dose <- as.data.frame(preparation_ipd_dose)
    prepare_unit_from_ipd_code <- encode_codes_data(list_preparation_dose_unit,
                                    preparation_unit_ipd_cols, ind_part_data)
  }

  # get column names for name, form, dosage and unit from unit cost data
  name_pattern <- c("name", "drug", "medication", "med", "patch")
  form_pattern <- c("form", "drug form", "patch/tablet", "type")
  size_pattern <- c("size", "packsize")
  size_unit_pattern <- c("vol", "volume")
  brand_pattern <- c("brand", "brand name", "trade", "trade name")
  preparation_pattern <- c("preparation", "prepare", "make")

  name_cost_col_no <- get_col_multiple_pattern(name_pattern, unit_cost_data)
  form_cost_col_no <- get_col_multiple_pattern(form_pattern, unit_cost_data)
  size_pack_cost_col_no <-
    get_col_multiple_pattern(size_pattern, unit_cost_data)
  size_unit_cost_col_no <-
    get_col_multiple_pattern(size_unit_pattern, unit_cost_data)
  unit_cost_col_no <- IPDFileCheck::get_columnno_fornames(unit_cost_data,
                                                          cost_calculated_per)
  dosage_cost_col_no <- IPDFileCheck::get_columnno_fornames(unit_cost_data,
                                                            strength_column)
  if (brand_check != -1) {
    brand_cost_col_no <- get_col_multiple_pattern(brand_pattern,
                                                   unit_cost_data)
  }
  if (preparation_dose_check != -1) {
    preparation_cost_col_no <-  get_col_multiple_pattern(preparation_pattern,
                                                          unit_cost_data)
  }

  ## information from equivalent dose tables

  if (is.null(eqdose_covtab)) {
    conversion_factor <- 1
    eqdose_check <- -1
  } else {
    if (typeof(eqdose_covtab) != "closure" & typeof(eqdose_covtab) != "list") {
      if (is.na(eqdose_covtab)) {
        eqdose_check <- -1
        conversion_factor <- 1
      }
    } else {
      eqdose_check <- 0
      name_pattern <- c("name", "drug", "medication", "med")
      form_pattern <- c("form", "type")
      dose_unit_pattern <- c("unit", "dose unit", "dose_unit", "unit of dose",
                             "unit dose")
      conv_factor_pattern <- c("conversion factor", "conversion_factor",
                               "conv_factor", "factor conversion",
                               "factor_conversion")

      drug_col_conv_table <- get_col_multiple_pattern(name_pattern,
                                                       eqdose_covtab)

      form_col_conv_table <- get_col_multiple_pattern(form_pattern,
                                                       eqdose_covtab)
      dose_unit_col_conv_table <- get_col_multiple_pattern(dose_unit_pattern,
                                                            eqdose_covtab)
      conv_factor_col_conv_table <-
        get_col_multiple_pattern(conv_factor_pattern, eqdose_covtab)

    }
  }

  list_total_cost_basis <- list()
  list_total_med_period <- list()
  list_total_med_wt_period <- list()
  list_total_cost_period <- list()
  list_total_med_equiv_dose_period <- list()
  list_total_cost_per_equiv_period <- list()
  for (i in 1:nrow(ind_part_data)) {
    name_ipd <- names_from_ipd_code[i, ]
    dose_ipd <- med_ipd_dose[i, ]
    unit_dose_ipd <- unit_from_ipd_code[i, ]

    bot_lasts_ipd <- bottle_lasts_ipd[i, ]
    bot_lasts_unit_ipd <- bottle_lasts_unit_from_ipd_code[i, ]
    bot_size_ipd <- bottle_size_ipd[i, ]
    bot_size_unit_ipd <- bottle_size_unit_from_ipd_code[i, ]

    if (brand_check != -1)
      brand_ipd <- brand_from_ipd_code[i, ]
    if (preparation_dose_check != -1)
      prepare_dose_ipd <- preparation_ipd_dose[i, ]
    if (preparation_unit_check != -1)
      prepare_unit_ipd <- prepare_unit_from_ipd_code[i, ]
    no_na_dose_ipd <- dose_ipd[!is.na(dose_ipd)]
    no_na_name_ipd <- name_ipd[!is.na(name_ipd)]
    if (length(no_na_name_ipd) != length(no_na_dose_ipd))
      stop("number of doses and number of medications should be equal")
    if (is.null(name_ipd)) {
      med_valid_check <- -1
    } else {
      if (sum(is.na(unname(name_ipd))) >= length(name_ipd))
        med_valid_check <- -1
      else
        med_valid_check <- 0
    }
    if (med_valid_check != -1) {
      total_cost_basis <- 0
      total_med_period <- 0
      total_med_wt_period <- 0
      total_cost_period <- 0
      total_med_equiv_dose_period <- 0
      total_cost_per_equiv_period <- 0
      for (j in seq_len(length(name_ipd))) {
        if (!is.null(name_ipd[j]) & !is.na(name_ipd[j])) {
          #(name, form, brand, dose, preparation,and volume of bottle)
          match_name <- return_equal_str_col(name_cost_col_no, unit_cost_data,
                                             name_ipd[j])
          match_form <- return_equal_liststring_col(form_cost_col_no,
                                                    match_name, words)
          if (brand_check != -1) {
            match_form_brand <- return_equal_str_col(brand_cost_col_no,
                                                     match_form, brand_ipd[j])
            if (nrow(match_form_brand) < 1)
              stop("Did not find matching brand name of medication")
          } else {
            match_form_brand <- match_form
          }
          # get the unit of doses from the ipd
          if (unit_med_check == -1)
            dose_num_val_ipd <-
            as.numeric(stringr::str_extract(dose_ipd[j], "\\d+\\.*\\d*"))
          else
            dose_num_val_ipd <- as.numeric(dose_ipd[j])
          dose_in_ipd <- paste(dose_num_val_ipd, unit_dose_ipd[j], sep = "")


          strength_unit_cost <- trimws(gsub("[0-9\\.]", "",
                                            match_form[[dosage_cost_col_no]]))
          strength_val_cost <-
            as.numeric(stringr::str_extract(match_form[[dosage_cost_col_no]],
                                            "\\d+\\.*\\d*"))

          # if the unit cost are listed for different unit of doses say
          # mg/ml or g/ml
          # choose the right one after finding the multiplier which is 1.

          dose_in_cost_data <- paste(strength_val_cost, strength_unit_cost,
                                     sep = "")

          if (any(dose_in_cost_data == dose_in_ipd)) {
            match_form_brand_unit <-
              match_form_brand[dose_in_cost_data ==  dose_in_ipd, ]

          } else {
            stop("The used dosage is not in costing table")
          }

          basis_str_unit_multiply <- convert_wtpervoldiff_basis(unit_dose_ipd[j],
                                                    basis_strength_unit)
          # get the unit of preparation from the ipd
          if (preparation_dose_check != -1) {
            if (preparation_unit_check == -1) {
              ipd_preparation_dose <- prepare_dose_ipd[j]
            } else {
              prepare_dose_val <- (prepare_dose_ipd[j])
              prepare_dose_unit_val <- prepare_unit_ipd[j]

              index_slash <- stringr::str_locate(prepare_dose_val, "/")
              first_dose <- stringr::str_sub(prepare_dose_val, 1,
                                             index_slash[1] - 1)

              second_dose <- stringr::str_sub(prepare_dose_val,
                                  index_slash[2] + 1, nchar(prepare_dose_val))

              index <- stringr::str_locate(prepare_dose_unit_val, "/")
              wt_unit <- stringr::str_sub(prepare_dose_unit_val, 1,
                                          index[1] - 1)

              vol_unit <- stringr::str_sub(prepare_dose_unit_val,
                                  index[2] + 1, nchar(prepare_dose_unit_val))

              ipd_preparation_dose <- paste(first_dose, wt_unit, "/",
                                            second_dose, vol_unit, sep = "")

            }
            match_form_brand_unit_prepare <-
              match_form_brand_unit[match_form_brand_unit[preparation_cost_col_no] ==
                                      ipd_preparation_dose, ]
          } else {
            match_form_brand_unit_prepare <- match_form_brand_unit
          }
          unit_used_costing <-
            unique(match_form_brand_unit_prepare[[unit_cost_col_no]])
          if (unit_used_costing == "per bottle") {
            bottle_vol_cost <-
              as.numeric(match_form_brand_unit_prepare[size_pack_cost_col_no])
            bottle_vol_unit_cost <-
              match_form_brand_unit_prepare[size_unit_cost_col_no]

            bottle_volandunit_cost <- paste(bottle_vol_cost,
                                            bottle_vol_unit_cost, sep = "")

            if (bottle_size_unit_check == -1)
              bottle_size_num_val_ipd <-
              as.numeric(stringr::str_extract(bot_size_ipd[j], "\\d+\\.*\\d*"))
            else
              bottle_size_num_val_ipd <- as.numeric(bot_size_ipd[j])
            bottle_size_in_ipd <- paste(bottle_size_num_val_ipd,
                                        bot_size_unit_ipd[j], sep = "")

            if (any(bottle_volandunit_cost == bottle_size_in_ipd)) {
              match_form_brand_unit_prepare_size <-
                match_form_brand_unit_prepare[bottle_volandunit_cost ==
                                                bottle_size_in_ipd, ]
              uni_cost_per_bottle <-
                match_form_brand_unit_prepare_size[[unit_cost_column]]
            } else {
              stop("The used vol and unit of bottle is not in costing table")
            }

          } else {
            stop("Error- liquids needs to be costed per bottle")
          }

          if (eqdose_check != -1) {
            temp <- return_equal_str_col(drug_col_conv_table, eqdose_covtab,
                                         name_ipd[j])
            tempa <- return_equal_liststring_listcol(form_col_conv_table, temp,
                                                     words)
            unit_conv_table <- tempa[[dose_unit_col_conv_table]]
            unit_converts <-
              unlist(lapply(unit_conv_table, convert_wtpervoldiff_basis,
                            unit_dose_ipd[j]))
            temp2 <- tempa[which(unit_converts == 1), ]
            if (nrow(temp2) < 1)
              stop("The unit in the conversion table is not correct or
                   can not be checked")
            conver_factor <- temp2[[conv_factor_col_conv_table]]
            if (!is.numeric(conver_factor)) {
              if (conver_factor == "N/A" | is.na(conver_factor)) {
                conversion_factor <- 1
              } else {
                check_num <- suppressWarnings(as.numeric(conver_factor))
                if (is.na(check_num))
                  conversion_factor <-
                    as.numeric(stringr::str_extract(conver_factor,
                                                    "\\d+\\.*\\d*"))
                else
                  conversion_factor <- as.numeric(conver_factor)
              }
            }
          }
          if (bottle_lasts_unit_check == -1)
            bottle_lasts_num_val_ipd <-
            as.numeric(stringr::str_extract(bot_lasts_ipd[j], "\\d+\\.*\\d*"))
          else
            bottle_lasts_num_val_ipd <- as.numeric(bot_lasts_ipd[j])
          ipd_bottle_lasts <- paste(bottle_lasts_num_val_ipd,
                                    bot_lasts_unit_ipd[j], sep = " ")
          basis_time_multiply <- convert_to_given_timeperiod(ipd_bottle_lasts,
                                                             internal_basis_time)
          if (basis_time_multiply > 1)
            no_bottles_used_basis <- 1
          else
            no_bottles_used_basis <- ceiling(1 / basis_time_multiply)

          vol_unit_multiplier <- convert_volume_basis(bot_size_unit_ipd[j],
                                                        basis_vol_unit)
          index <- stringr::str_locate(unit_dose_ipd[j], "/")
          this_wt_unit <- stringr::str_sub(unit_dose_ipd[j], 1, index[1] - 1)

          wt_unit_multiplier <- convert_weight_diff_basis(this_wt_unit,
                                                            basis_wt_unit)

          # no of bottle times the unit cost
          cost_basis <- no_bottles_used_basis * uni_cost_per_bottle

          time_multiplier <- convert_to_given_timeperiod(timeperiod,
                                                         internal_basis_time)
          actual_no_bottles_basis = 1 / basis_time_multiply
          bottles_taken_period <- ceiling(actual_no_bottles_basis * time_multiplier)

          #2 mg/ml dose 10 bottles of certain volume med in strength unit
          med_str_period <-  dose_num_val_ipd * bottles_taken_period *
                                                  basis_str_unit_multiply

          med_wt_period <- med_str_period * bottle_size_num_val_ipd *
                        vol_unit_multiplier * wt_unit_multiplier

          cost_period <- bottles_taken_period * uni_cost_per_bottle
          med_str_equiv_period <- med_str_period * conversion_factor
          cost_per_equiv_period  <- cost_period / med_str_equiv_period

        } else {
          cost_basis <- 0
          med_str_period <- 0
          med_wt_period <- 0
          cost_period <- 0
          med_str_equiv_period <- 0
          cost_per_equiv_period <- 0

        }
        total_cost_basis <- total_cost_basis + cost_basis
        total_med_period <- total_med_period + med_str_period
        total_med_wt_period <- total_med_wt_period + med_wt_period
        total_cost_period <- total_cost_period + cost_period
        total_med_equiv_dose_period <- total_med_equiv_dose_period +
          med_str_equiv_period
        total_cost_per_equiv_period <- total_cost_per_equiv_period +
          cost_per_equiv_period
      }
    } else {
      total_cost_basis <- NA
      total_med_period <- NA
      total_med_wt_period <- NA
      total_cost_period <- NA
      total_med_equiv_dose_period <- NA
      total_cost_per_equiv_period <- NA
    }
    keywd <- "liquid"
    list_total_cost_basis <- append(list_total_cost_basis, total_cost_basis)
    list_total_med_period <- append(list_total_med_period,total_med_period)
    list_total_med_wt_period <- append(list_total_med_wt_period,
                                       total_med_wt_period)
    list_total_cost_period <- append(list_total_cost_period, total_cost_period)
    list_total_med_equiv_dose_period <- append(list_total_med_equiv_dose_period,
                                               total_med_equiv_dose_period)
    list_total_cost_per_equiv_period <- append(list_total_cost_per_equiv_period,
                                               total_cost_per_equiv_period)
  }
  this_name <- paste("totcost_basis_", keywd, sep = "")
  ind_part_data[[this_name]] <- unlist(list_total_cost_basis)
  this_name <- paste("totmed_period_", keywd, sep = "")
  ind_part_data[[this_name]] <- unlist(list_total_med_period)
  this_name <- paste("totmed_wt_period_", keywd, sep = "")
  ind_part_data[[this_name]] <- unlist(list_total_med_wt_period)
  this_name <- paste("totcost_period_", keywd, sep = "")
  ind_part_data[[this_name]] <- unlist(list_total_cost_period)
  this_name <- paste("totmed_per_equiv_period_", keywd, sep = "")
  ind_part_data[[this_name]] <- unlist(list_total_med_equiv_dose_period)
  this_name <- paste("totcost_per_equiv_period_", keywd, sep = "")
  ind_part_data[[this_name]] <- unlist(list_total_cost_per_equiv_period)
  return(ind_part_data)
}
###############################################################################
#' Function to estimate the cost of patches taken (from IPD)
#' @param ind_part_data IPD
#' @param name_med name of medication
#' @param brand_med brand name of medication if revealed
#' @param dose_med dose of medication used
#' @param unit_med unit of medication ; use null if its along with the dose
#' @param no_taken how many taken
#' @param freq_taken frequency of medication
#' @param timeperiod time period for cost calculation
#' @param unit_cost_data  unit costs data
#' @param unit_cost_column column name of unit cost in unit_cost_data
#' @param cost_calculated_per column name of unit where the cost is calculated
#' @param strength_column column column name that contain strength of
#' medication
#' @param list_of_code_names if names is coded, give the code:name pairs,
#' optional
#' @param list_of_code_freq if frequency is coded, give the
#' code:frequency pairs, optional
#' @param list_of_code_dose_unit if unit is coded, give the code:unit pairs,
#' optional
#' @param list_of_code_brand if brand names  are coded, give the code:brand
#' pairs, optional
#' @param eqdose_cov_tab table to get the conversion factor for equivalent
#' doses, optional
#' @param basis_strength_unit strength unit to be taken as basis
#' required for total medication calculations
#' @return the calculated cost of tablets along with original data
#' @examples
#' med_costs_file <- system.file("extdata", "average_unit_costs_med_brand.csv",
#' package = "packDAMipd")
#' data_file <- system.file("extdata", "medication.xlsx",
#' package = "packDAMipd")
#' ind_part_data <- load_trial_data(data_file)
#' med_costs <- load_trial_data(med_costs_file)
#' conv_file <- system.file("extdata", "Med_calc.xlsx", package = "packDAMipd")
#' table <- load_trial_data(conv_file)
#' res <- microcosting_patches_wide(
#' ind_part_data = ind_part_data, name_med = "patch_name",
#' brand_med = "patch_brand", dose_med = "patch_strength", unit_med = NULL,
#' no_taken = "patch_no_taken", freq_taken = "patch_frequency",
#' timeperiod = "4 months", unit_cost_data = med_costs,
#' unit_cost_column = "UnitCost", cost_calculated_per  = "Basis",
#' strength_column = "Strength", list_of_code_names = NULL,
#' list_of_code_freq = NULL, list_of_code_dose_unit = NULL,
#' list_of_code_brand = NULL, eqdose_cov_tab = table,
#' basis_strength_unit = "mcg/hr")
#' @export
#' @details
#' Assumes individual level data has name of medication, dose, dose unit,
#' number taken, frequency taken, and basis time
#' Assumes unit cost data contains the name of medication, form/type,
#' strength, unit of strength (or the unit in which the cost calculated),
#' preparation, unit cost, size and size unit
#' (in which name, forms, size, size unit, and preparation  are not passed on)
#'  @importFrom dplyr %>%

#'  a patient use 1 mg/hr patches 5 patches once a week
#'  that patch comes in a pack of 4 with cost £2.50
#'  we want to estimate the cost for 3 months
#'  that means amount of medication
#'  3 months = 21 weeks
#'  number of patches taken = 21*5 = 105 patches
#'  packs = (105/4) almost 27 packs
#'  cost = 27*2.50

microcosting_patches_wide <- function(ind_part_data,
                                      name_med,
                                      brand_med = NULL,
                                      dose_med,
                                      unit_med = NULL,
                                      no_taken, freq_taken,
                                      timeperiod,
                                      unit_cost_data,
                                      unit_cost_column,
                                      cost_calculated_per,
                                      strength_column,
                                      list_of_code_names = NULL,
                                      list_of_code_freq = NULL,
                                      list_of_code_dose_unit = NULL,
                                      list_of_code_brand = NULL,
                                      eqdose_cov_tab = NULL,
                                      basis_strength_unit = NULL) {
  internal_basis_time <- "day"
  generated_list <- generate_wt_time_units()
  wt_per_time_units <- generated_list$weight_per_times
  wt_units <- generated_list$weight_units
  time_units <-  generated_list$time_units
  #Error - data should not be NULL
  if (is.null(ind_part_data) | is.null(unit_cost_data))
    stop("data should not be NULL")

  #Checking if the required parameters are NULL or NA
  variables_check <- list(name_med, dose_med,
                          no_taken, freq_taken,
                          timeperiod, unit_cost_column,
                          cost_calculated_per, strength_column)
  results <- sapply(variables_check, check_null_na)
  names_check <- c("name_med", "dose_med",
                   "no_taken", "freq_taken",
                   "timeperiod", "unit_cost_column",
                   "cost_calculated_per", "strength_column")
  if (any(results != 0)) {
    indices <- which(results < 0)
    stop(paste("Error - the variables can not be NULL or NA,
               check the variable(s)", names_check[indices]))
  }
  if (!is.null(basis_strength_unit)) {
    if (is.na(basis_strength_unit))
      basis_strength_unit <- "mcg/hr"
  } else {
    basis_strength_unit <- "mcg/hr"
  }
  if (!(basis_strength_unit %in% wt_per_time_units))
    stop("the basis strength unit is not valid")

  index <- stringr::str_locate(basis_strength_unit, "/")
  basis_wt_unit <- stringr::str_sub(basis_strength_unit, 1, index[1] - 1)
  basis_time_unit <- stringr::str_sub(basis_strength_unit, index[2] + 1,
                                      nchar(basis_strength_unit))

  brand_check <- return0_if_not_null_na(brand_med)
  unit_med_check <-  return0_if_not_null_na(unit_med)

  check_list <- c(unit_med_check, brand_check)
  partial_list <- c(name_med, dose_med, no_taken, freq_taken)
  another_list <- list(unit_med, brand_med)
  another_list[check_list == -1] <- -1
  info_list <- unlist(append(partial_list, another_list))

  ipd_cols_exists <- list()
  for (i in seq_len(length(info_list))) {
    if (info_list[i] != -1) {
      check <- IPDFileCheck::check_column_exists(info_list[i], ind_part_data)
      if (sum(check) != 0) {
        res <- grep(info_list[i], colnames(ind_part_data))
        if (length(res) == 0)
          stop("Atleast one of the required columns not found")
        ipd_cols_exists[length(ipd_cols_exists) + 1] <- list(res)
      }
    } else {
      ipd_cols_exists[length(ipd_cols_exists) + 1] <- -1
    }
  }
  names_med_cols <- unlist(ipd_cols_exists[1])
  doses_med_cols <- unlist(ipd_cols_exists[2])
  numbers_taken_cols <- unlist(ipd_cols_exists[3])
  freq_taken_cols <- unlist(ipd_cols_exists[4])
  if (unit_med_check != -1)
    unit_med_cols  <- unlist(ipd_cols_exists[5])
  if (brand_check != -1)
    brand_med_cols  <- unlist(ipd_cols_exists[6])

  # if the codes are being used for name, dosage, frequency and time period
  # list_of_code_names is a list of (list of codes and list of names)
  # if they are valid, assign the names to codes, read the code from data
  # and read the corresponding names using the earlier assignment
  names_from_code <- encode_codes_data(list_of_code_names, names_med_cols,
                                       ind_part_data)
  if (is.null(unlist(names_from_code)) | sum(is.na(unlist(names_from_code))) ==
      length(unlist(names_from_code))) {
    stop("Error - name_from_code can not be null - check the input for
         list of names and codes")
  }
  freq_desc_from_code <- encode_codes_data(list_of_code_freq, freq_taken_cols,
                                           ind_part_data)
  freq_given_basis_list <- as.list(as.data.frame(t(freq_desc_from_code)))
  freq_given_basis <- list()
  for (i in seq_len(length(freq_given_basis_list))) {
    this_list <- unlist(freq_given_basis_list[i])
    this_row <- lapply(this_list, convert_freq_diff_basis, internal_basis_time)
    freq_given_basis <- append(freq_given_basis, this_row)
  }
  freq_given_basis <- unlist(freq_given_basis)
  freq_given_basis <- matrix(freq_given_basis,
                             nrow = dim(freq_desc_from_code)[1], byrow = TRUE)
  colnames(freq_given_basis) <- colnames(freq_desc_from_code)
  freq_given_basis <- as.data.frame(freq_given_basis)

  if (brand_check != -1) {
    brand_and_code <- encode_codes_data(list_of_code_brand, brand_med_cols,
                                        ind_part_data)
    if (is.null(unlist(brand_and_code)) | sum(is.na(unlist(brand_and_code))) ==
        length(unlist(brand_and_code))) {
      stop("Error - size_unit_from_code can not be null - check the input for
         brand  code")
    }
  }
  if (unit_med_check == -1) {
    med_dose <- ind_part_data %>% dplyr::select(dplyr::all_of(doses_med_cols))
    med_dose_unlist <- unlist(med_dose)
    unit_from_code <- gsub("[0-9\\.]", "", med_dose_unlist)
    unit_from_code <- matrix(unit_from_code,
                             nrow = dim(med_dose)[1])
    colnames(unit_from_code) <- colnames(med_dose)
    unit_from_code <- as.data.frame(unit_from_code)
  } else {
    med_dose <- ind_part_data %>% dplyr::select(dplyr::all_of(doses_med_cols))
    med_dose <- as.data.frame(med_dose)
    unit_from_code <- encode_codes_data(list_of_code_dose_unit, unit_med_cols,
                                        ind_part_data)
  }
  if (is.null(unlist(unit_from_code)) | sum(is.na(unlist(unit_from_code))) ==
      length(unlist(unit_from_code))) {
    stop("Error - unit_from_code can not be null - check the input for
         list of units")
  }

  # check columns exist in unit cost  data
  info_list <- c(unit_cost_column, cost_calculated_per, strength_column)
  checks <- sapply(info_list, IPDFileCheck::check_column_exists, unit_cost_data)
  if (sum(checks) != 0) {
    stop("Atleast one of the required columns in unit cost data not found")
  }
  # get column names for name, form, dosage and unit
  name_pattern <- c("name", "drug", "medication", "med", "patch")
  name_col_no <- get_col_multiple_pattern(name_pattern, unit_cost_data)

  form_pattern <- c("form", "drug form", "type")
  form_col_no <- get_col_multiple_pattern(form_pattern, unit_cost_data)

  unit_col_no <- IPDFileCheck::get_columnno_fornames(unit_cost_data,
                                                     cost_calculated_per)
  dosage_col_no <- IPDFileCheck::get_columnno_fornames(unit_cost_data,
                                                       strength_column)
  if (brand_check != -1) {
    brand_pattern <- c("brand", "brand name", "trade", "trade name")
    brand_col_no <- get_col_multiple_pattern(brand_pattern, unit_cost_data)
    size_pattern <- c("size", "pack size")
    size_pack_col_no <- get_col_multiple_pattern(size_pattern, unit_cost_data)
  }
  if (is.null(eqdose_cov_tab)) {
    conversion_factor <- 1
    eqdose_check <- -1
  } else {
    if (typeof(eqdose_cov_tab) != "closure" &
        typeof(eqdose_cov_tab) != "list") {
      if (is.na(eqdose_cov_tab)) {
        eqdose_check <- -1
        conversion_factor <- 1
      }
    } else {
      eqdose_check <- 0
      name_pattern <- c("name", "drug", "medication", "med")
      drug_col_conv_table <- get_col_multiple_pattern(name_pattern,
                                                      eqdose_cov_tab)

      form_pattern <- c("form", "type")
      form_col_conv_table <- get_col_multiple_pattern(form_pattern,
                                                      eqdose_cov_tab)

      dose_unit_pattern <- c("unit", "dose unit", "dose_unit", "unit of dose",
                             "unit dose")
      dose_unit_col_conv_table <- get_col_multiple_pattern(dose_unit_pattern,
                                                           eqdose_cov_tab)

      conv_factor_pattern <- c("conversion factor", "conversion_factor",
                               "conv_factor", "factor conversion",
                               "factor_conversion")
      conv_factor_col <- get_col_multiple_pattern(conv_factor_pattern,
                                                  eqdose_cov_tab)
    }
  }
  list_total_med_str_period <- list()
  list_total_med_wt_period <- list()
  list_total_med_equiv_dose_period <- list()
  list_total_cost_period <- list()
  list_total_cost_per_equiv_period <- list()
  for (i in 1:nrow(ind_part_data)) {
    name_medication <- names_from_code[i, ]
    if (brand_check != -1)
      brand_medication <- brand_and_code[i, ]
    dose_medication <- med_dose[i, ]
    if (length(dose_medication) != length(name_medication))
      stop("number of doses and number of medications should be equal")
    if (is.null(name_medication)) {
      medication_valid_check <- -1
    } else {
      if (sum(is.na(unname(name_medication))) >= length(name_medication))
        medication_valid_check <- -1
      else
        medication_valid_check <- 0
    }
    if (medication_valid_check != -1) {
      how_many_taken <- as.numeric(ind_part_data[i, numbers_taken_cols])
      freq_multiplier_basis <- freq_given_basis[i, ]
      freq_multiplier_basis <-
        freq_multiplier_basis[!is.na(freq_multiplier_basis)]
      this_unit <- unit_from_code[i, ]
      this_unit <- this_unit[!is.na(this_unit)]
      total_med_str_period <- 0
      total_med_wt_period <- 0
      total_med_equiv_dose_period <-  0
      total_cost_period <-  0
      total_cost_per_equiv_period <-  0
      for (j in seq_len(length(name_medication))) {
        if (!is.null(name_medication[j]) & !is.na(name_medication[j])) {
          subset1 <- return_equal_str_col(name_col_no,
                                          unit_cost_data, name_medication[j])
          subset2 <- subset1[subset1[form_col_no] == "Patch" |
                               subset1[form_col_no] == "Patches" |
                               subset1[form_col_no] == "patch" |
                               subset1[form_col_no] == "patches", ]
          if (brand_check != -1) {
            subset2 <- return_equal_str_col(brand_col_no,
                                            subset2, brand_medication[j])
            if (nrow(subset2) < 1)
              stop("Did not find matching brand name of medication")
          }
          if (unit_med_check == -1)
            dose_num_val <-
            as.numeric(stringr::str_extract(dose_medication[j],
                                            "\\d+\\.*\\d*"))
          else
            dose_num_val <- as.numeric(dose_medication[j])

          if (eqdose_check != -1) {
            temp <- return_equal_str_col(drug_col_conv_table, eqdose_cov_tab,
                                         name_medication[j])
            words <- c("patch", "patches")
            tempa <- return_equal_liststring_listcol(form_col_conv_table, temp,
                                                     words)

            unit_conv_table <- tempa[[dose_unit_col_conv_table]]
            unit_converts <-
              unlist(lapply(unit_conv_table, convert_wtpertimediff_basis,
                            this_unit[j]))

            unit_same <- which(unit_converts == 1)
            temp2 <- tempa[unit_same, ]
            if (nrow(temp2) < 1)
              stop("The unit in the conversion table is not correct or
                   can not be checked")
            conver_factor <- temp2[[conv_factor_col]]
            if (!is.numeric(conver_factor)) {
              if (conver_factor == "N/A" | is.na(conver_factor)) {
                conversion_factor <- 1
              } else {
                check_num <- suppressWarnings(as.numeric(conver_factor))
                if (is.na(check_num))
                  conversion_factor <-
                    as.numeric(stringr::str_extract(conver_factor,
                                                    "\\d+\\.*\\d*"))
                else
                  conversion_factor <- as.numeric(conver_factor)
              }
            } else {
              conversion_factor <- as.numeric(conver_factor)
            }
          }
          strength_unit_cost <- trimws(gsub("[0-9\\.]", "",
                                            subset2[[dosage_col_no]]))
          strength_val_cost <-
            as.numeric(stringr::str_extract(subset2[[dosage_col_no]],
                                            "\\d+\\.*\\d*"))

          dose_in_ipd <- paste(dose_num_val, this_unit[j], sep = "")
          strength_unit_multiplier <- c()

          basis_str_unit_multiply <- convert_wtpertimediff_basis(this_unit[j],
                                                        basis_strength_unit)

          for (i in seq_len(length(strength_unit_cost))) {
            unit_multiply <- convert_wtpertimediff_basis(this_unit[j],
                                                         strength_unit_cost[i])
            strength_unit_multiplier <- append(strength_unit_multiplier,
                                               unit_multiply)
          }

           strength_unit_cost[which(strength_unit_multiplier == 1)] <-
            this_unit[j]
          dose_in_cost_data <- paste(strength_val_cost,
                                     strength_unit_cost, sep = "")
          if (any(dose_in_cost_data == dose_in_ipd)) {
            subset3 <- subset2[dose_in_cost_data ==  dose_in_ipd, ]
            unit_cost_med_prep <- subset3[[unit_cost_column]]
          } else {
            stop("The used dosage is not in costing table")
          }
          strength_unit_multip <- strength_unit_multiplier[dose_in_cost_data ==
                                                             dose_in_ipd]
          unit_used_costing <- unique(subset3[[unit_col_no]])
          if (brand_check != -1) {
            if (unit_used_costing == "per pack" |
                unit_used_costing == "per package" |
                unit_used_costing == "pack" |
                unit_used_costing == "package") {
              pack_size <- as.numeric(subset3[size_pack_col_no])
            } else {
              pack_size <- 1
            }
          } else {
            pack_size <- 1
          }
          # number of patches taken for the base unit of time internally
          # it is a day
          no_taken_basis <- how_many_taken[j] * freq_multiplier_basis[j]
          #5 patch once a week- 4months=4*30days=120/7 weeks = (120/7)*5 =
          # 85.7 patches =22 packs

          time_multiplier <- convert_to_given_timeperiod(timeperiod,
                                                         internal_basis_time)
          number_taken_period <- no_taken_basis * time_multiplier
          packs_taken_period <- ceiling(number_taken_period / pack_size)

          #85.71429 patches 1mg/hr for 120 days
          med_str_period <-  dose_num_val * number_taken_period *
                                                   basis_str_unit_multiply
          #10 patches 2 mg/hr * 2 * 48hours (2 days)
          time_multi <- convert_to_given_timeperiod(timeperiod, basis_time_unit)
          med_wt_period <- dose_num_val * number_taken_period * time_multi

          cost_period <- packs_taken_period * unit_cost_med_prep
          med_str_equiv_period <- med_str_period * conversion_factor
          cost_per_equiv_period  <- cost_period / med_str_equiv_period

        } else {
          med_str_period <- 0
          cost_period <- 0
          med_str_equiv_period <- 0
          cost_per_equiv_period <- 0

        }
        total_med_str_period <- total_med_str_period + med_str_period
        total_med_wt_period <- total_med_wt_period + med_wt_period
        total_med_equiv_dose_period <- total_med_equiv_dose_period +
          med_str_equiv_period
        total_cost_period <- total_cost_period + cost_period
        total_cost_per_equiv_period <- total_cost_per_equiv_period +
          cost_per_equiv_period
      }
    } else {
      total_med_str_period <- NA
      total_med_equiv_dose_period <- NA
      total_med_wt_period <- NA
      total_cost_period <- NA
      total_cost_per_equiv_period <- NA
    }
    keywd <- "patches"
    list_total_med_str_period <- append(list_total_med_str_period,
                                        total_med_str_period)
    list_total_med_wt_period <- append(list_total_med_wt_period,
                                        total_med_wt_period)
    list_total_med_equiv_dose_period <- append(list_total_med_equiv_dose_period,
                                               total_med_equiv_dose_period)
    list_total_cost_period <- append(list_total_cost_period,
                                     total_cost_period)
    list_total_cost_per_equiv_period <- append(list_total_cost_per_equiv_period,
                                               total_cost_per_equiv_period)
  }

  this_name <- paste("totmed_period_", keywd, sep = "")
  ind_part_data[[this_name]] <- unlist(list_total_med_str_period)
  this_name <- paste("totmed_wt_period_", keywd, sep = "")
  ind_part_data[[this_name]] <- unlist(list_total_med_wt_period)
  this_name <- paste("totmed_equiv_period_", keywd, sep = "")
  ind_part_data[[this_name]] <- unlist(list_total_med_equiv_dose_period)

  this_name <- paste("totcost_period_", keywd, sep = "")
  ind_part_data[[this_name]] <- unlist(list_total_cost_period)

  this_name <- paste("totcost_per_equiv_period_", keywd, sep = "")
  ind_part_data[[this_name]] <- unlist(list_total_cost_per_equiv_period)
  return(ind_part_data)
}
##############################################################################
#' Function to estimate the cost of tablets taken (from IPD)
#' @param ind_part_data IPD
#' @param name_med name of medication
#' @param brand_med brand name of medication if revealed
#' @param dose_med dose of medication used
#' @param unit_med unit of medication ; use null if its along with the dose
#' @param no_taken how many taken
#' @param freq_taken frequency of medication
#' @param timeperiod time period for cost calculation
#' @param unit_cost_data  unit costs data
#' @param unit_cost_column column name of unit cost in unit_cost_data
#' @param cost_calculated_per column name of unit where the cost is calculated
#' @param strength_column column column name that contain strength of
#' medication
#' @param list_of_code_names if names is coded, give the code:name pairs,
#' optional
#' @param list_of_code_freq if frequency is coded, give the
#' code:frequency pairs, optional
#' @param list_of_code_dose_unit if unit is coded, give the code:unit pairs,
#' optional
#' @param list_of_code_brand if brand names  are coded, give the code:brand
#' pairs, optional
#' @param eqdose_cov_tab table to get the conversion factor for equivalent
#' doses, optional
#' @param basis_strength_unit strength unit to be taken as basis
#' required for total medication calculations
#' @return the calculated cost of tablets along with original data
#' @examples
#' med_costs_file <- system.file("extdata", "average_unit_costs_med_brand.csv",
#' package = "packDAMipd")
#' data_file <- system.file("extdata", "medication_all.xlsx",
#' package = "packDAMipd")
#' ind_part_data <- load_trial_data(data_file)
#' med_costs <- load_trial_data(med_costs_file)
#' conv_file <- system.file("extdata", "Med_calc.xlsx",package = "packDAMipd")
#' table <- load_trial_data(conv_file)
#' res <- microcosting_tablets_wide(ind_part_data = ind_part_data,
#' name_med = "tab_name", brand_med = "tab_brand", dose_med = "tab_strength",
#' unit_med = "tab_str_unit", no_taken = "tab_no_taken",
#' freq_taken = "tab_frequency",timeperiod = "2 months",
#' unit_cost_data = med_costs,unit_cost_column = "UnitCost",
#' cost_calculated_per  = "Basis", strength_column = "Strength",
#' list_of_code_names = NULL, list_of_code_freq = NULL,
#' list_of_code_dose_unit = NULL, eqdose_cov_tab = table,
#' basis_strength_unit = "mg")
#' @export
#' @details
#' Assumes individual level data has name of medication, dose, dose unit,
#' number taken, frequency taken, and basis time
#' Assumes unit cost data contains the name of medication, form/type,
#' strength, unit of strength (or the unit in which the cost calculated),
#' preparation, unit cost, size and size unit
#' (in which name, forms, size, size unit, and preparation  are not passed on)
#'  @importFrom dplyr %>%
microcosting_tablets_wide <- function(ind_part_data,
                                      name_med,
                                      brand_med = NULL,
                                      dose_med,
                                      unit_med = NULL,
                                      no_taken, freq_taken,
                                      timeperiod,
                                      unit_cost_data,
                                      unit_cost_column,
                                      cost_calculated_per,
                                      strength_column,
                                      list_of_code_names = NULL,
                                      list_of_code_freq = NULL,
                                      list_of_code_dose_unit = NULL,
                                      list_of_code_brand = NULL,
                                      eqdose_cov_tab = NULL,
                                      basis_strength_unit = NULL) {
  internal_basis_time <- "day"
  generated_list <- generate_wt_time_units()
  wt_units <- generated_list$weight_units
  #Error - data should not be NULL
  if (is.null(ind_part_data) | is.null(unit_cost_data))
    stop("data should not be NULL")

  #Checking if the required parameters are NULL or NA
  variables_check <- list(name_med, dose_med,
                          no_taken, freq_taken,
                          timeperiod, unit_cost_column,
                          cost_calculated_per, strength_column)
  results <- sapply(variables_check, check_null_na)
  names_check <- c("name_med", "dose_med",
                   "no_taken", "freq_taken",
                   "timeperiod", "unit_cost_column",
                   "cost_calculated_per", "strength_column")
  if (any(results != 0)) {
    indices <- which(results < 0)
    stop(paste("Error - the variables can not be NULL or NA,
               check the variable(s)", names_check[indices]))
  }
  if (!is.null(basis_strength_unit)) {
    if (is.na(basis_strength_unit))
      basis_strength_unit <- "mg"
  } else {
    basis_strength_unit <- "mg"
  }
  if (!(basis_strength_unit %in% wt_units))
    stop("the basis strength unit is not valid")

  basis_wt_unit <- basis_strength_unit

  brand_check <- return0_if_not_null_na(brand_med)
  unit_med_check <-  return0_if_not_null_na(unit_med)

  check_list <- c(unit_med_check, brand_check)
  partial_list <- c(name_med, dose_med, no_taken, freq_taken)
  another_list <- list(unit_med, brand_med)
  another_list[check_list == -1] <- -1
  info_list <- unlist(append(partial_list, another_list))

  ipd_cols_exists <- list()
  for (i in seq_len(length(info_list))) {
    if (info_list[i] != -1) {
      check <- IPDFileCheck::check_column_exists(info_list[i], ind_part_data)
      if (sum(check) != 0) {
        res <- grep(info_list[i], colnames(ind_part_data))
        if (length(res) == 0)
          stop("Atleast one of the required columns not found")
        ipd_cols_exists[length(ipd_cols_exists) + 1] <- list(res)
      }
    } else {
      ipd_cols_exists[length(ipd_cols_exists) + 1] <- -1
    }
  }
  names_med_cols <- unlist(ipd_cols_exists[1])
  doses_med_cols <- unlist(ipd_cols_exists[2])
  numbers_taken_cols <- unlist(ipd_cols_exists[3])
  freq_taken_cols <- unlist(ipd_cols_exists[4])
  if (unit_med_check != -1)
    unit_med_cols  <- unlist(ipd_cols_exists[5])
  if (brand_check != -1)
    brand_med_cols  <- unlist(ipd_cols_exists[6])

  # if the codes are being used for name, dosage, frequency and time period
  # list_of_code_names is a list of (list of codes and list of names)
  # if they are valid, assign the names to codes, read the code from data
  # and read the corresponding names using the earlier assignment
  names_from_code <- encode_codes_data(list_of_code_names, names_med_cols,
                                       ind_part_data)
  if (is.null(unlist(names_from_code)) | sum(is.na(unlist(names_from_code))) ==
      length(unlist(names_from_code))) {
    stop("Error - name_from_code can not be null - check the input for
         list of names and codes")
  }
  freq_desc_from_code <- encode_codes_data(list_of_code_freq, freq_taken_cols,
                                           ind_part_data)
  freq_given_basis_list <- as.list(as.data.frame(t(freq_desc_from_code)))
  freq_given_basis <- list()
  for (i in seq_len(length(freq_given_basis_list))) {
    this_list <- unlist(freq_given_basis_list[i])
    this_row <- lapply(this_list, convert_freq_diff_basis, internal_basis_time)
    freq_given_basis <- append(freq_given_basis, this_row)
  }
  freq_given_basis <- unlist(freq_given_basis)
  freq_given_basis <- matrix(freq_given_basis,
                             nrow = dim(freq_desc_from_code)[1], byrow = TRUE)
  colnames(freq_given_basis) <- colnames(freq_desc_from_code)
  freq_given_basis <- as.data.frame(freq_given_basis)

  if (brand_check != -1) {
    brand_and_code <- encode_codes_data(list_of_code_brand, brand_med_cols,
                                        ind_part_data)
    if (is.null(unlist(brand_and_code)) | sum(is.na(unlist(brand_and_code))) ==
        length(unlist(brand_and_code))) {
      stop("Error - brand_and_code can not be null - check the input for
         brand  code")
    }
  }
  if (unit_med_check == -1) {
    med_dose <- ind_part_data %>% dplyr::select(dplyr::all_of(doses_med_cols))
    med_dose_unlist <- unlist(med_dose)
    unit_from_code <- gsub("[0-9\\.]", "", med_dose_unlist)
    unit_from_code <- matrix(unit_from_code,
                             nrow = dim(med_dose)[1])
    colnames(unit_from_code) <- colnames(med_dose)
    unit_from_code <- as.data.frame(unit_from_code)
  } else {
    med_dose <- ind_part_data %>% dplyr::select(dplyr::all_of(doses_med_cols))
    med_dose <- as.data.frame(med_dose)
    unit_from_code <- encode_codes_data(list_of_code_dose_unit, unit_med_cols,
                                        ind_part_data)
  }
  if (is.null(unlist(unit_from_code)) | sum(is.na(unlist(unit_from_code))) ==
      length(unlist(unit_from_code))) {
    stop("Error - unit_from_code can not be null - check the input for
         list of units")
  }

  # check columns exist in unit cost  data
  info_list <- c(unit_cost_column, cost_calculated_per, strength_column)
  checks <- sapply(info_list, IPDFileCheck::check_column_exists, unit_cost_data)
  if (sum(checks) != 0) {
    stop("Atleast one of the required columns in unit cost data not found")
  }
  # get column names for name, form, dosage and unit
  name_pattern <- c("name", "drug", "medication", "med", "patch")
  name_col_no <- get_col_multiple_pattern(name_pattern, unit_cost_data)

  form_pattern <- c("form", "drug form", "type")
  form_col_no <- get_col_multiple_pattern(form_pattern, unit_cost_data)

  unit_col_no <- IPDFileCheck::get_columnno_fornames(unit_cost_data,
                                                     cost_calculated_per)
  dosage_col_no <- IPDFileCheck::get_columnno_fornames(unit_cost_data,
                                                       strength_column)
  if (brand_check != -1) {
    brand_pattern <- c("brand", "brand name", "trade", "trade name")
    brand_col_no <- get_col_multiple_pattern(brand_pattern, unit_cost_data)
    size_pattern <- c("size", "pack size")
    size_pack_col_no <- get_col_multiple_pattern(size_pattern, unit_cost_data)
  }
  if (is.null(eqdose_cov_tab)) {
    conversion_factor <- 1
    eqdose_check <- -1
  } else {
    if (typeof(eqdose_cov_tab) != "closure" &
        typeof(eqdose_cov_tab) != "list") {
      if (is.na(eqdose_cov_tab)) {
        eqdose_check <- -1
        conversion_factor <- 1
      }
    } else {
      eqdose_check <- 0
      name_pattern <- c("name", "drug", "medication", "med")
      drug_col_conv_table <- get_col_multiple_pattern(name_pattern,
                                                      eqdose_cov_tab)

      form_pattern <- c("form", "type")
      form_col_conv_table <- get_col_multiple_pattern(form_pattern,
                                                      eqdose_cov_tab)

      dose_unit_pattern <- c("unit", "dose unit", "dose_unit", "unit of dose",
                             "unit dose")
      dose_unit_col_conv_table <- get_col_multiple_pattern(dose_unit_pattern,
                                                           eqdose_cov_tab)

      conv_factor_pattern <- c("conversion factor", "conversion_factor",
                               "conv_factor", "factor conversion",
                               "factor_conversion")
      conv_factor_col <- get_col_multiple_pattern(conv_factor_pattern,
                                                  eqdose_cov_tab)
    }
  }
  list_total_med_str_period <- list()
  list_total_med_equiv_dose_period <- list()
  list_total_cost_period <- list()
  list_total_cost_per_equiv_period <- list()
  for (i in 1:nrow(ind_part_data)) {
    name_medication <- names_from_code[i, ]
    if (brand_check != -1)
      brand_medication <- brand_and_code[i, ]
    dose_medication <- med_dose[i, ]
    if (length(dose_medication) != length(name_medication))
      stop("number of doses and number of medications should be equal")
    if (is.null(name_medication)) {
      medication_valid_check <- -1
    } else {
      if (sum(is.na(unname(name_medication))) >= length(name_medication))
        medication_valid_check <- -1
      else
        medication_valid_check <- 0
    }
    if (medication_valid_check != -1) {
      how_many_taken <- as.numeric(ind_part_data[i, numbers_taken_cols])
      freq_multiplier_basis <- freq_given_basis[i, ]
      freq_multiplier_basis <-
        freq_multiplier_basis[!is.na(freq_multiplier_basis)]
      this_unit <- unit_from_code[i, ]
      this_unit <- this_unit[!is.na(this_unit)]
      total_med_str_period <- 0
      total_med_equiv_dose_period <-  0
      total_cost_period <-  0
      total_cost_per_equiv_period <-  0
      for (j in seq_len(length(name_medication))) {
        if (!is.null(name_medication[j]) & !is.na(name_medication[j])) {
          subset1 <- return_equal_str_col(name_col_no,
                                          unit_cost_data, name_medication[j])
          subset2 <- subset1[subset1[form_col_no] == "Tablet" |
                               subset1[form_col_no] == "Tablets" |
                               subset1[form_col_no] == "tablet" |
                               subset1[form_col_no] == "tablets", ]
          if (brand_check != -1) {
            subset2 <- return_equal_str_col(brand_col_no,
                                            subset2, brand_medication[j])
            if (nrow(subset2) < 1)
              stop("Did not find matching brand name of medication")
          }
          if (unit_med_check == -1)
            dose_num_val <-
            as.numeric(stringr::str_extract(dose_medication[j],
                                            "\\d+\\.*\\d*"))
          else
            dose_num_val <- as.numeric(dose_medication[j])

          if (eqdose_check != -1) {
            temp <- return_equal_str_col(drug_col_conv_table, eqdose_cov_tab,
                                         name_medication[j])
            words <- c("tablet", "tablets")
            tempa <- return_equal_liststring_listcol(form_col_conv_table, temp,
                                                     words)

            unit_conv_table <- tempa[[dose_unit_col_conv_table]]
            unit_converts <-
              unlist(lapply(unit_conv_table, convert_weight_diff_basis,
                            this_unit[j]))

            unit_same <- which(unit_converts == 1)
            temp2 <- tempa[unit_same, ]
            if (nrow(temp2) < 1)
              stop("The unit in the conversion table is not correct or
                   can not be checked")
            conver_factor <- temp2[[conv_factor_col]]
            if (!is.numeric(conver_factor)) {
              if (conver_factor == "N/A" | is.na(conver_factor)) {
                conversion_factor <- 1
              } else {
                check_num <- suppressWarnings(as.numeric(conver_factor))
                if (is.na(check_num))
                  conversion_factor <-
                    as.numeric(stringr::str_extract(conver_factor,
                                                    "\\d+\\.*\\d*"))
                else
                  conversion_factor <- as.numeric(conver_factor)
              }
            } else {
              conversion_factor <- as.numeric(conver_factor)
            }
          }
          strength_unit_cost <- trimws(gsub("[0-9\\.]", "",
                                            subset2[[dosage_col_no]]))
          strength_val_cost <-
            as.numeric(stringr::str_extract(subset2[[dosage_col_no]],
                                            "\\d+\\.*\\d*"))

          dose_in_ipd <- paste(dose_num_val, this_unit[j], sep = "")
          strength_unit_multiplier <- c()

          basis_str_unit_multiply <- convert_weight_diff_basis(this_unit[j],
                                                      basis_strength_unit)
          for (i in seq_len(length(strength_unit_cost))) {
            unit_multiply <- convert_weight_diff_basis(this_unit[j],
                                                       strength_unit_cost[i])
            strength_unit_multiplier <- append(strength_unit_multiplier,
                                               unit_multiply)
          }

          if (sum(is.na(strength_unit_multiplier)) != 0)
            stop("The unit is not identifiable to convert for costing")
          strength_unit_cost[which(strength_unit_multiplier == 1)] <-
            this_unit[j]
          dose_in_cost_data <- paste(strength_val_cost,
                                     strength_unit_cost, sep = "")
          if (any(dose_in_cost_data == dose_in_ipd)) {
            subset3 <- subset2[dose_in_cost_data ==  dose_in_ipd, ]
            if (nrow(subset3) != 1)
              stop("Error - atleast one row should be existing ")
            unit_cost_med_prep <- subset3[[unit_cost_column]]
          } else {
            stop("The used dosage is not in costing table")
          }
          strength_unit_multip <- strength_unit_multiplier[dose_in_cost_data ==
                                                             dose_in_ipd]
          unit_used_costing <- unique(subset3[[unit_col_no]])
          if (brand_check != -1) {
            if (unit_used_costing == "per pack" |
                unit_used_costing == "per package" |
                unit_used_costing == "pack" |
                unit_used_costing == "package") {
              pack_size <- as.numeric(subset3[size_pack_col_no])
            } else {
              pack_size <- 1
            }
          } else {
            pack_size <- 1
          }
          # number of tablets taken for the base unit of time internally
          #it is a day
          no_taken_basis <- how_many_taken[j] * freq_multiplier_basis[j]

          time_multiplier <- convert_to_given_timeperiod(timeperiod,
                                                         internal_basis_time)
          number_taken_period <- no_taken_basis * time_multiplier
          packs_taken_period <- ceiling(number_taken_period / pack_size)

          med_str_period <-  dose_num_val * number_taken_period *
            basis_str_unit_multiply
          cost_period <- packs_taken_period * unit_cost_med_prep
          med_str_equiv_period <- med_str_period * conversion_factor
          cost_per_equiv_period  <- cost_period / med_str_equiv_period

        } else {
          med_str_period <- 0
          cost_period <- 0
          med_str_equiv_period <- 0
          cost_per_equiv_period <- 0

        }
        total_med_str_period <- total_med_str_period + med_str_period
        total_med_equiv_dose_period <- total_med_equiv_dose_period +
          med_str_equiv_period
        total_cost_period <- total_cost_period + cost_period
        total_cost_per_equiv_period <- total_cost_per_equiv_period +
          cost_per_equiv_period
      }
    } else {
      total_med_str_period <- 0
      total_med_equiv_dose_period <- NA
      total_cost_period <- NA
      total_cost_per_equiv_period <- NA
    }
    keywd <- "tablets"
    list_total_med_str_period <- append(list_total_med_str_period,
                                        total_med_str_period)
    list_total_med_equiv_dose_period <- append(list_total_med_equiv_dose_period,
                                               total_med_equiv_dose_period)
    list_total_cost_period <- append(list_total_cost_period,
                                     total_cost_period)
    list_total_cost_per_equiv_period <- append(list_total_cost_per_equiv_period,
                                               total_cost_per_equiv_period)
  }

  this_name <- paste("totmed_period_", keywd, sep = "")
  ind_part_data[[this_name]] <- unlist(list_total_med_str_period)

  this_name <- paste("totmed_equiv_period_", keywd, sep = "")
  ind_part_data[[this_name]] <- unlist(list_total_med_equiv_dose_period)

  this_name <- paste("totcost_period_", keywd, sep = "")
  ind_part_data[[this_name]] <- unlist(list_total_cost_period)

  this_name <- paste("totcost_per_equiv_period_", keywd, sep = "")
  ind_part_data[[this_name]] <- unlist(list_total_cost_per_equiv_period)
  return(ind_part_data)
}

#'###########################################################################
#' #' Function to estimate the cost of tablets and patches taken (from IPD)
#' #' using a IPD data of long format
#' #' @param the_columns columns that are to be used to convert the data
#' #' from long to wide
#' #' @param ind_part_data IPD
#' #' @param name_med name of medication
#' #' @param brand_med brand name of medication if revealed
#' #' @param dose_med dose of medication used
#' #' @param unit_med unit of medication ; use null if its along with the dose
#' #' @param no_taken how many taken
#' #' @param freq_taken frequency of medication
#' #' @param timeperiod time period for cost calculation
#' #' @param unit_cost_data  unit costs data
#' #' @param unit_cost_column column name of unit cost in unit_cost_data
#' #' @param cost_calculated_per column name of unit in the cost is calculated
#' #' @param strength_column column column name that contain strength of
#' #' medication
#' #' @param list_of_code_names if names is coded, give the code:name pairs,
#' #' optional
#' #' @param list_of_code_freq if frequency is coded, give the
#' #' code:frequency pairs, optional
#' #' @param list_of_code_dose_unit if unit is coded, give the code:unit pairs,
#' #' optional
#' #' @param list_of_code_brand if brand names  are coded, give the code:brand
#' #' pairs, optional
#' #' @param eqdose_cov_tab table to get the conversion factor for equivalent
#' #' doses, optional
#' #' @param basis_strength_unit strength unit to be taken as basis
#' #' required for total medication calculations
#' #' @return the calculated cost of tablets along with original data
#' #' @examples
#' #' med_costs_file <- system.file("extdata", "average_unit_costs_med_brand.csv",
#' #' package = "packDAMipd")
#' #' data_file <- system.file("extdata", "medication.xlsx",
#' #' package = "packDAMipd")
#' #' ind_part_data <- load_trial_data(data_file)
#' #' ind_part_data_long <- gather(ind_part_data, condition, measurement,
#' #' patch_name_1:liq_lasts_3)
#' #' med_costs <- load_trial_data(med_costs_file)
#' #' conv_file <- system.file("extdata", "Med_calc.xlsx", package = "packDAMipd")
#' #' table <- load_trial_data(conv_file)
#' #' res <- microcosting_patches_long(the_columns = patch_name_1:liq_lasts_3,
#' #' ind_part_data = ind_part_data_long, name_med = "patch_name",
#' #' brand_med = "patch_brand", dose_med = "patch_strength", unit_med = NULL,
#' #' no_taken = "patch_no_taken", freq_taken = "patch_frequency",
#' #' timeperiod = "4 months", unit_cost_data = med_costs,
#' #' unit_cost_column = "UnitCost", cost_calculated_per  = "Basis",
#' #' strength_column = "Strength", list_of_code_names = NULL,
#' #' list_of_code_freq = NULL, list_of_code_dose_unit = NULL,
#' #' list_of_code_brand = NULL, eqdose_cov_tab = table,
#' #' basis_strength_unit = "mcg/hr")
#' #' @export
#' #' @importFrom tidyr gather
#' #' @importFrom tidyr spread_
#' microcosting_patches_long <- function(the_columns, ind_part_data_long,
#'                                               name_med,
#'                                               brand_med = NULL,
#'                                               dose_med,
#'                                               unit_med = NULL,
#'                                               no_taken, freq_taken,
#'                                               timeperiod,
#'                                               unit_cost_data,
#'                                               unit_cost_column,
#'                                               cost_calculated_per,
#'                                               strength_column,
#'                                               list_of_code_names = NULL,
#'                                               list_of_code_freq = NULL,
#'                                               list_of_code_dose_unit = NULL,
#'                                               list_of_code_brand = NULL,
#'                                               eqdose_cov_tab = NULL,
#'                                               basis_strength_unit = NULL) {
#'   #Error - data should not be NULL
#'   if (is.null(ind_part_data_long) | is.null(unit_cost_data))
#'     stop("data should not be NULL")
#'
#'   ind_part_data_wide <- tidyr::spread_(ind_part_data_long, the_columns[1],
#'                                        the_columns[2])
#'
#'   result <- microcosting_patches_wide(ind_part_data_wide,
#'                                     name_med,
#'                                     brand_med = NULL,
#'                                     dose_med,
#'                                     unit_med = NULL,
#'                                     no_taken, freq_taken,
#'                                     timeperiod,
#'                                     unit_cost_data,
#'                                     unit_cost_column,
#'                                     cost_calculated_per,
#'                                     strength_column,
#'                                     list_of_code_names = NULL,
#'                                     list_of_code_freq = NULL,
#'                                     list_of_code_dose_unit = NULL,
#'                                     list_of_code_brand = NULL,
#'                                     eqdose_cov_tab = NULL,
#'                                     basis_strength_unit = NULL)
#' }
