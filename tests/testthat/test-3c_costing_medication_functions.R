###############################################################################
context("testing microcosting patches")
test_that("testing microcosting patches", {
  med_costs_file <- system.file("extdata", "average_unit_costs_med_brand.csv",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "medication.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)
  conv_file <- system.file("extdata", "Med_calc.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)
  #using the package price when the brand name of the medication is known
  res <- microcosting_patches_wide(ind_part_data = ind_part_data,
                                           name_med = "patch_name",
                                           brand_med = "patch_brand",
                                           dose_med = "patch_strength",
                                           unit_med = NULL,
                                           no_taken = "patch_no_taken",
                                           freq_taken = "patch_frequency",
                                           timeperiod = "4 months",
                                           unit_cost_data = med_costs,
                                           unit_cost_column = "UnitCost",
                                           cost_calculated_per  = "Basis",
                                           strength_column = "Strength",
                                           list_of_code_names = NULL,
                                           list_of_code_freq = NULL,
                                           list_of_code_dose_unit = NULL,
                                           list_of_code_brand = NULL,
                                           eqdose_cov_tab = table,
                                           basis_strength_unit = "mcg/hr")


  expect_equal(res$totmed_period_patches, c(1285.714, 2880), tolerance = 1e-3)
  expect_equal(res$totcost_period_patches, c(1081.30, 604.32), tolerance = 1e-3)

  expect_error(microcosting_patches_wide(ind_part_data = ind_part_data,
                                   name_med = "patch_name",
                                   brand_med = "patch_brand",
                                   dose_med = "patch_strength",
                                   unit_med = NULL,
                                   no_taken = "patch_no_taken",
                                   freq_taken = "patch_frequency",
                                   timeperiod = "4 months",
                                   unit_cost_data = med_costs,
                                   unit_cost_column = "UnitCost",
                                   cost_calculated_per  = "Basis",
                                   strength_column = "Strength",
                                   list_of_code_names = NULL,
                                   list_of_code_freq = NULL,
                                   list_of_code_dose_unit = NULL,
                                   list_of_code_brand = NULL,
                                   eqdose_cov_tab = table,
                                   basis_strength_unit = "jg/hr"))

  expect_error(microcosting_patches_wide(ind_part_data = ind_part_data,
                                         name_med = "patch_name",
                                         brand_med = "patch_brand",
                                         dose_med = "patch_strength",
                                         unit_med = NULL,
                                         no_taken = "patch_no_taken",
                                         freq_taken = "patch_frequency",
                                         timeperiod = "4 months",
                                         unit_cost_data = med_costs,
                                         unit_cost_column = "UnitCost",
                                         cost_calculated_per  = "Basis",
                                         strength_column = "Strength",
                                         list_of_code_names = NULL,
                                         list_of_code_freq = NULL,
                                         list_of_code_dose_unit = NULL,
                                         list_of_code_brand = NULL,
                                         eqdose_cov_tab = table,
                                         basis_strength_unit = "mg/b"))

  res <- microcosting_patches_wide(ind_part_data = ind_part_data,
                                            name_med = "patch_name",
                                            brand_med = "patch_brand",
                                            dose_med = "patch_strength",
                                            unit_med = NA,
                                            no_taken = "patch_no_taken",
                                            freq_taken = "patch_frequency",
                                            timeperiod = "4 months",
                                            unit_cost_data = med_costs,
                                            unit_cost_column = "UnitCost",
                                            cost_calculated_per  = "Basis",
                                            strength_column = "Strength",
                                            list_of_code_names = NULL,
                                            list_of_code_freq = NULL,
                                            list_of_code_dose_unit = NULL,
                                            list_of_code_brand = NULL,
                                            eqdose_cov_tab = NULL,
                                            basis_strength_unit = "mcg/hr")

  expect_equal(res$totmed_period_patches, c(1285.714, 2880), tolerance = 1e-3)

  res <- microcosting_patches_wide(ind_part_data = ind_part_data,
                                            name_med = "patch_name",
                                            brand_med = "patch_brand",
                                            dose_med = "patch_strength",
                                            unit_med = NA,
                                            no_taken = "patch_no_taken",
                                            freq_taken = "patch_frequency",
                                            timeperiod = "4 months",
                                            unit_cost_data = med_costs,
                                            unit_cost_column = "UnitCost",
                                            cost_calculated_per  = "Basis",
                                            strength_column = "Strength",
                                            list_of_code_names = NULL,
                                            list_of_code_freq = NULL,
                                            list_of_code_dose_unit = NULL,
                                            list_of_code_brand = NULL,
                                            eqdose_cov_tab = NA,
                                            basis_strength_unit = "mcg/hr")

  expect_equal(res$totmed_period_patches, c(1285.714, 2880), tolerance = 1e-3)


  # no cols with drug names
  conv_file <- system.file("extdata", "Med_calc_nodrugcol.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)
  expect_error(microcosting_patches_wide(ind_part_data = ind_part_data,
                                            name_med = "patch_name",
                                            brand_med = "patch_brand",
                                            dose_med = "patch_strength",
                                            unit_med = NA,
                                            no_taken = "patch_no_taken",
                                            freq_taken = "patch_frequency",
                                            timeperiod = "4 months",
                                            unit_cost_data = med_costs,
                                            unit_cost_column = "UnitCost",
                                            cost_calculated_per  = "Basis",
                                            strength_column = "Strength",
                                            list_of_code_names = NULL,
                                            list_of_code_freq = NULL,
                                            list_of_code_dose_unit = NULL,
                                            list_of_code_brand = NULL,
                                            eqdose_cov_tab = table,
                                            basis_strength_unit = "mcg/hr"))

  # no column for form in conversion table
  conv_file <- system.file("extdata", "Med_calc_noformcol.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)
  expect_error(microcosting_patches_wide(ind_part_data = ind_part_data,
                                            name_med = "patch_name",
                                            brand_med = "patch_brand",
                                            dose_med = "patch_strength",
                                            unit_med = NA,
                                            no_taken = "patch_no_taken",
                                            freq_taken = "patch_frequency",
                                            timeperiod = "4 months",
                                            unit_cost_data = med_costs,
                                            unit_cost_column = "UnitCost",
                                            cost_calculated_per  = "Basis",
                                            strength_column = "Strength",
                                            list_of_code_names = NULL,
                                            list_of_code_freq = NULL,
                                            list_of_code_dose_unit = NULL,
                                            list_of_code_brand = NULL,
                                            eqdose_cov_tab = table,
                                            basis_strength_unit = "mcg/hr"))
 # no unit column in conversion table
  conv_file <- system.file("extdata", "Med_calc_nounitcol.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)

  expect_error(microcosting_patches_wide(ind_part_data = ind_part_data,
                                            name_med = "patch_name",
                                            brand_med = "patch_brand",
                                            dose_med = "patch_strength",
                                            unit_med = NA,
                                            no_taken = "patch_no_taken",
                                            freq_taken = "patch_frequency",
                                            timeperiod = "4 months",
                                            unit_cost_data = med_costs,
                                            unit_cost_column = "UnitCost",
                                            cost_calculated_per  = "Basis",
                                            strength_column = "Strength",
                                            list_of_code_names = NULL,
                                            list_of_code_freq = NULL,
                                            list_of_code_dose_unit = NULL,
                                            list_of_code_brand = NULL,
                                            eqdose_cov_tab = table,
                                            basis_strength_unit = "mcg/hr"))
  #no conversion factor column in conversion table
  conv_file <- system.file("extdata", "Med_calc_noconvfactorcol.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)

  expect_error(microcosting_patches_wide(ind_part_data = ind_part_data,
                                                  name_med = "patch_name",
                                                  brand_med = "patch_brand",
                                                  dose_med = "patch_strength",
                                                  unit_med = NA,
                                                  no_taken = "patch_no_taken",
                                                freq_taken = "patch_frequency",
                                                  timeperiod = "4 months",
                                                  unit_cost_data = med_costs,
                                                  unit_cost_column = "UnitCost",
                                              cost_calculated_per  = "Basis",
                                                  strength_column = "Strength",
                                                  list_of_code_names = NULL,
                                                  list_of_code_freq = NULL,
                                                  list_of_code_dose_unit = NULL,
                                                  list_of_code_brand = NULL,
                                                  eqdose_cov_tab = table,
                                              basis_strength_unit = "mcg/hr"))


  conv_file <- system.file("extdata", "Med_calc.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)

  res <- microcosting_patches_wide(ind_part_data = ind_part_data,
                                            name_med = "patch_name",
                                            brand_med = "patch_brand",
                                            dose_med = "patch_strength",
                                            unit_med = NA,
                                            no_taken = "patch_no_taken",
                                            freq_taken = "patch_frequency",
                                            timeperiod = "4 months",
                                            unit_cost_data = med_costs,
                                            unit_cost_column = "UnitCost",
                                            cost_calculated_per  = "Basis",
                                            strength_column = "Strength",
                                            list_of_code_names = NULL,
                                            list_of_code_freq = NULL,
                                            list_of_code_dose_unit = NULL,
                                            list_of_code_brand = NULL,
                                            eqdose_cov_tab = table,
                                            basis_strength_unit = "mcg/hr")

  expect_equal(res$totmed_period_patches, c(1285.714, 2880), tolerance = 1e-3)

  expect_error(microcosting_patches_wide(ind_part_data = ind_part_data,
                                            name_med = "patch_name",
                                            brand_med = "patch_brand",
                                            dose_med = "patch_strength",
                                            unit_med = NULL,
                                            no_taken = "patch_no_taken",
                                            freq_taken = "patch_frequency",
                                            timeperiod = "4 months",
                                            unit_cost_data = med_costs,
                                            unit_cost_column = "UnitCost",
                                            cost_calculated_per  = "Basis",
                                            strength_column = "Strength",
                                            list_of_code_names = NULL,
                                            list_of_code_freq = NULL,
                                            list_of_code_dose_unit = NULL,
                                            eqdose_cov_tab = table,
                                            basis_strength_unit = "mg"))

  expect_error(microcosting_patches_wide(ind_part_data = NULL,
                                            name_med = "patch_name",
                                            brand_med = "patch_brand",
                                            dose_med = "patch_strength",
                                            unit_med = NULL,
                                            no_taken = "patch_no_taken",
                                            freq_taken = "patch_frequency",
                                            timeperiod = "4 months",
                                            unit_cost_data = med_costs,
                                            unit_cost_column = "UnitCost",
                                            cost_calculated_per  = "Basis",
                                            strength_column = "Strength",
                                            list_of_code_names = NULL,
                                            list_of_code_freq = NULL,
                                            list_of_code_dose_unit = NULL,
                                            eqdose_cov_tab = table,
                                            basis_strength_unit = "mcg/hr"))

  expect_error(microcosting_patches_wide(ind_part_data = ind_part_data,
                                            name_med = NULL,
                                            brand_med = "patch_brand",
                                            dose_med = "patch_strength",
                                            unit_med = NULL,
                                            no_taken = "patch_no_taken",
                                            freq_taken = "patch_frequency",
                                            timeperiod = "4 months",
                                            unit_cost_data = med_costs,
                                            unit_cost_column = "UnitCost",
                                            cost_calculated_per  = "Basis",
                                            strength_column = "Strength",
                                            list_of_code_names = NULL,
                                            list_of_code_freq = NULL,
                                            list_of_code_dose_unit = NULL,
                                            eqdose_cov_tab = table,
                                            basis_strength_unit = "mcg/hr"))


  expect_error(microcosting_patches_wide(ind_part_data = ind_part_data,
                                            name_med = "patch_name",
                                            brand_med = "patch_brand",
                                            dose_med = NULL,
                                            unit_med = NULL,
                                            no_taken = "patch_no_taken",
                                            freq_taken = "patch_frequency",
                                            timeperiod = "4 months",
                                            unit_cost_data = med_costs,
                                            unit_cost_column = "UnitCost",
                                            cost_calculated_per  = "Basis",
                                            strength_column = "Strength",
                                            list_of_code_names = NULL,
                                            list_of_code_freq = NULL,
                                            list_of_code_dose_unit = NULL,
                                            eqdose_cov_tab = table,
                                            basis_strength_unit = "mcg/hr"))

  expect_error(microcosting_patches_wide(ind_part_data = ind_part_data,
                                            name_med = "patch_name",
                                            brand_med = "patch_brand",
                                            dose_med = "patch_strength",
                                            unit_med = NULL,
                                            no_taken = NULL,
                                            freq_taken = "patch_frequency",
                                            timeperiod = "4 months",
                                            unit_cost_data = med_costs,
                                            unit_cost_column = "UnitCost",
                                            cost_calculated_per  = "Basis",
                                            strength_column = "Strength",
                                            list_of_code_names = NULL,
                                            list_of_code_freq = NULL,
                                            list_of_code_dose_unit = NULL,
                                            eqdose_cov_tab = table,
                                            basis_strength_unit = "mcg/hr"))

  expect_error(microcosting_patches_wide(ind_part_data = ind_part_data,
                                            name_med = "patch_name",
                                            brand_med = "patch_brand",
                                            dose_med = "patch_strength",
                                            unit_med = NULL,
                                            no_taken = "patch_no_taken",
                                            freq_taken = NULL,
                                            timeperiod = "4 months",
                                            unit_cost_data = med_costs,
                                            unit_cost_column = "UnitCost",
                                            cost_calculated_per  = "Basis",
                                            strength_column = "Strength",
                                            list_of_code_names = NULL,
                                            list_of_code_freq = NULL,
                                            list_of_code_dose_unit = NULL,
                                            eqdose_cov_tab = table,
                                            basis_strength_unit = "mcg/hr"))

  expect_error(microcosting_patches_wide(ind_part_data = ind_part_data,
                                            name_med = "patch_name",
                                            brand_med = "patch_brand",
                                            dose_med = "patch_strength",
                                            unit_med = NULL,
                                            no_taken = "patch_no_taken",
                                            freq_taken = "patch_frequency",
                                            timeperiod = NULL,
                                            unit_cost_data = med_costs,
                                            unit_cost_column = "UnitCost",
                                            cost_calculated_per  = "Basis",
                                            strength_column = "Strength",
                                            list_of_code_names = NULL,
                                            list_of_code_freq = NULL,
                                            list_of_code_dose_unit = NULL,
                                            eqdose_cov_tab = table,
                                            basis_strength_unit = "mcg/hr"))

  expect_error(microcosting_patches_wide(ind_part_data = ind_part_data,
                                            name_med = "patch_name",
                                            brand_med = "patch_brand",
                                            dose_med = "patch_strength",
                                            unit_med = NULL,
                                            no_taken = "patch_no_taken",
                                            freq_taken = "patch_frequency",
                                            timeperiod = "4 months",
                                            unit_cost_data = NULL,
                                            unit_cost_column = "UnitCost",
                                            cost_calculated_per  = "Basis",
                                            strength_column = "Strength",
                                            list_of_code_names = NULL,
                                            list_of_code_freq = NULL,
                                            list_of_code_dose_unit = NULL,
                                            eqdose_cov_tab = table,
                                            basis_strength_unit = "mcg/hr"))

  expect_error(microcosting_patches_wide(ind_part_data = ind_part_data,
                                            name_med = "patch_name",
                                            brand_med = "patch_brand",
                                            dose_med = "patch_strength",
                                            unit_med = NULL,
                                            no_taken = "patch_no_taken",
                                            freq_taken = "patch_frequency",
                                            timeperiod = "4 months",
                                            unit_cost_data = med_costs,
                                            unit_cost_column = NULL,
                                            cost_calculated_per  = "Basis",
                                            strength_column = "Strength",
                                            list_of_code_names = NULL,
                                            list_of_code_freq = NULL,
                                            list_of_code_dose_unit = NULL,
                                            eqdose_cov_tab = table,
                                            basis_strength_unit = "mcg/hr"))

  expect_error(microcosting_patches_wide(ind_part_data = ind_part_data,
                                            name_med = "patch_name",
                                            brand_med = "patch_brand",
                                            dose_med = "patch_strength",
                                            unit_med = NULL,
                                            no_taken = "patch_no_taken",
                                            freq_taken = "patch_frequency",
                                            timeperiod = "4 months",
                                            unit_cost_data = med_costs,
                                            unit_cost_column = "UnitCost",
                                            cost_calculated_per  = NULL,
                                            strength_column = "Strength",
                                            list_of_code_names = NULL,
                                            list_of_code_freq = NULL,
                                            list_of_code_dose_unit = NULL,
                                            eqdose_cov_tab = table,
                                            basis_strength_unit = "mcg/hr"))
  expect_error(microcosting_patches_wide(ind_part_data = ind_part_data,
                                            name_med = "patch_name",
                                            brand_med = "patch_brand",
                                            dose_med = "patch_strength",
                                            unit_med = NULL,
                                            no_taken = "patch_no_taken",
                                            freq_taken = "patch_frequency",
                                            timeperiod = "4 months",
                                            unit_cost_data = med_costs,
                                            unit_cost_column = "UnitCost",
                                            cost_calculated_per  = "Basis",
                                            strength_column = NULL,
                                            list_of_code_names = NULL,
                                            list_of_code_freq = NULL,
                                            list_of_code_dose_unit = NULL,
                                            eqdose_cov_tab = table,
                                            basis_strength_unit = "mcg/hr"))


  # no brand information
  med_costs_file <- system.file("extdata", "average_unit_costs_med.csv",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "medication.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)
  conv_file <- system.file("extdata", "Med_calc.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)
  res <- microcosting_patches_wide(ind_part_data = ind_part_data,
                                            name_med = "patch_name",
                                            brand_med = NULL,
                                            dose_med = "patch_strength",
                                            unit_med = NULL,
                                            no_taken = "patch_no_taken",
                                            freq_taken = "patch_frequency",
                                            timeperiod = "4 months",
                                            unit_cost_data = med_costs,
                                            unit_cost_column = "UnitCost",
                                            cost_calculated_per  = "Basis",
                                            strength_column = "Strength",
                                            list_of_code_names = NULL,
                                            list_of_code_freq = NULL,
                                            list_of_code_dose_unit = NULL,
                                            eqdose_cov_tab = table,
                                            basis_strength_unit = NULL)

  expect_equal(res$totmed_period_patches, c(1285.714, 2880), tolerance = 1e-3)

  res <- microcosting_patches_wide(ind_part_data = ind_part_data,
                                            name_med = "patch_name",
                                            brand_med = NA,
                                            dose_med = "patch_strength",
                                            unit_med = NULL,
                                            no_taken = "patch_no_taken",
                                            freq_taken = "patch_frequency",
                                            timeperiod = "4 months",
                                            unit_cost_data = med_costs,
                                            unit_cost_column = "UnitCost",
                                            cost_calculated_per  = "Basis",
                                            strength_column = "Strength",
                                            list_of_code_names = NULL,
                                            list_of_code_freq = NULL,
                                            list_of_code_dose_unit = NULL,
                                            eqdose_cov_tab = table,
                                            basis_strength_unit = NULL)
  expect_equal(res$totmed_period_patches, c(1285.714, 2880), tolerance = 1e-3)


  res <- microcosting_patches_wide(ind_part_data = ind_part_data,
                                            name_med = "patch_name",
                                            brand_med = NULL,
                                            dose_med = "patch_strength",
                                            unit_med = NULL,
                                            no_taken = "patch_no_taken",
                                            freq_taken = "patch_frequency",
                                            timeperiod = "4 months",
                                            unit_cost_data = med_costs,
                                            unit_cost_column = "UnitCost",
                                            cost_calculated_per  = "Basis",
                                            strength_column = "Strength",
                                            list_of_code_names = NULL,
                                            list_of_code_freq = NULL,
                                            list_of_code_dose_unit = NULL,
                                            eqdose_cov_tab = table,
                                            basis_strength_unit = NA)
  expect_equal(res$totmed_period_patches, c(1285.714, 2880), tolerance = 1e-3)



  expect_error(microcosting_patches_wide(ind_part_data = ind_part_data,
                                                  name_med = "patch_name",
                                                  brand_med = NULL,
                                                  dose_med = "patch_strength",
                                                  unit_med = NULL,
                                                  no_taken = "patch_no_taken",
                                              freq_taken = "patch_frequency",
                                                  timeperiod = "4 months",
                                                  unit_cost_data = med_costs,
                                                  unit_cost_column = "UnitCost",
                                              cost_calculated_per  = "Basis",
                                                  strength_column = "Strength",
                                                  list_of_code_names = NULL,
                                                  list_of_code_freq = NULL,
                                                  list_of_code_dose_unit = NULL,
                                                  eqdose_cov_tab = table,
                                                  basis_strength_unit = "NA"))


  med_costs_file <- system.file("extdata",
                                "average_unit_costs_med_brand_errorbrand.csv",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "medication.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)
  conv_file <- system.file("extdata", "Med_calc.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)
  #using the package price when the brand name of the medication is known
  expect_error(microcosting_patches_wide(ind_part_data = ind_part_data,
                                            name_med = "patch_name",
                                            brand_med = "patch_brand",
                                            dose_med = "patch_strength",
                                            unit_med = NULL,
                                            no_taken = "patch_no_taken",
                                            freq_taken = "patch_frequency",
                                            timeperiod = "4 months",
                                            unit_cost_data = med_costs,
                                            unit_cost_column = "UnitCost",
                                            cost_calculated_per  = "Basis",
                                            strength_column = "Strength",
                                            list_of_code_names = NULL,
                                            list_of_code_freq = NULL,
                                            list_of_code_dose_unit = NULL,
                                            list_of_code_brand = NULL,
                                            eqdose_cov_tab = table,
                                            basis_strength_unit = "mcg/hr"))

  ## Information is coded  in the data file
  med_costs_file <- system.file("extdata", "average_unit_costs_med_brand.csv",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "medication_with_codes.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)
  conv_file <- system.file("extdata", "Med_calc.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)
  res <- microcosting_patches_wide(ind_part_data = ind_part_data,
                                            name_med = "patch_name",
                                            brand_med = "patch_brand",
                                            dose_med = "patch_strength",
                                            unit_med = "patch_unit_str",
                                            no_taken = "patch_no_taken",
                                            freq_taken = "patch_frequency",
                                            timeperiod = "4 months",
                                            unit_cost_data = med_costs,
                                            unit_cost_column = "UnitCost",
                                            cost_calculated_per  = "Basis",
                                            strength_column = "Strength",
                                            list_of_code_names =
                              list(c(1, 2), c("Buprenorphine", "Fentanyl")),
                                            list_of_code_freq =
              list(c(1, 2, 3), c("once a day", "twice a day", "once a week")),
                                            list_of_code_dose_unit =
              list(c(1, 2), c("mcg/hr", "mcg/day")),
              list_of_code_brand = list( c(1, 2, 3),
                                         c("BuTrans", "Fencino", "Butec")),
                                           eqdose_cov_tab = table,
                                            basis_strength_unit = "mcg/hr")


  expect_equal(res$totmed_period_patches, c(1285.714, 2880), tolerance = 1e-3)

  expect_error(microcosting_patches_wide(
         ind_part_data = ind_part_data,
                                           name_med = "patch_name",
                                           brand_med = "patch_brand",
                                           dose_med = "patch_strength",
                                           unit_med = NULL,
                                           no_taken = "patch_no_taken",
                                           freq_taken = "patch_frequency",
                                           timeperiod = "4 months",
                                           unit_cost_data = med_costs,
                                           unit_cost_column = "UnitCost",
                                           cost_calculated_per  = "Basis",
                                          strength_column = "Strength",
                                          list_of_code_names =
                                          list(c(1, 2),
                                               c("Buprenorphine", "Fentanyl")),
                                           list_of_code_freq =
                                            list(c(1, 2, 3),
                                                 c("once a day", "twice a day",
                                                  "once a week")),
                                           list_of_code_dose_unit =
                                             list(c(1, 2),
                                                  c("mcg/hr", "mcg/day")),
                                           list_of_code_brand =
                                             list( c(1, 2, 3),c("BuTrans", "Fencino",
                                                    "Butec")),
                                           eqdose_cov_tab = table,
                                           basis_strength_unit = "mcg/hr"))


   med_costs_file <- system.file("extdata",
                                 "average_unit_costs_med_brand_nodosage.csv",
                                 package = "packDAMipd")
   data_file <- system.file("extdata", "medication.xlsx",
                            package = "packDAMipd")
   ind_part_data <- load_trial_data(data_file)
   med_costs <- load_trial_data(med_costs_file)
   conv_file <- system.file("extdata", "Med_calc.xlsx",
                            package = "packDAMipd")
   table <- load_trial_data(conv_file)
   #using the package price when the brand name of the medication is known
   expect_error(microcosting_patches_wide(ind_part_data = ind_part_data,
                                            name_med = "patch_name",
                                            brand_med = "patch_brand",
                                            dose_med = "patch_strength",
                                            unit_med = NULL,
                                            no_taken = "patch_no_taken",
                                            freq_taken = "patch_frequency",
                                            timeperiod = "4 months",
                                            unit_cost_data = med_costs,
                                            unit_cost_column = "UnitCost",
                                            cost_calculated_per  = "Basis",
                                            strength_column = "Strength",
                                            list_of_code_names = NULL,
                                            list_of_code_freq = NULL,
                                            list_of_code_dose_unit = NULL,
                                            list_of_code_brand = NULL,
                                            eqdose_cov_tab = table,
                                            basis_strength_unit = "mcg/hr"))

   med_costs_file <- system.file("extdata",
                      "average_unit_costs_med_brand_unitnotidentify.csv",
                                 package = "packDAMipd")
   data_file <- system.file("extdata", "medication.xlsx",
                            package = "packDAMipd")
   ind_part_data <- load_trial_data(data_file)
   med_costs <- load_trial_data(med_costs_file)
   conv_file <- system.file("extdata", "Med_calc.xlsx",
                            package = "packDAMipd")
   table <- load_trial_data(conv_file)
   #using the package price when the brand name of the medication is known
   expect_error(microcosting_patches_wide(ind_part_data = ind_part_data,
                                                  name_med = "patch_name",
                                                  brand_med = "patch_brand",
                                                  dose_med = "patch_strength",
                                                  unit_med = NULL,
                                                  no_taken = "patch_no_taken",
                                              freq_taken = "patch_frequency",
                                                  timeperiod = "4 months",
                                                  unit_cost_data = med_costs,
                                                  unit_cost_column = "UnitCost",
                                              cost_calculated_per  = "Basis",
                                                  strength_column = "Strength",
                                                  list_of_code_names = NULL,
                                                  list_of_code_freq = NULL,
                                              list_of_code_dose_unit = NULL,
                                                  list_of_code_brand = NULL,
                                                  eqdose_cov_tab = table,
                                              basis_strength_unit = "mcg/hr"))


   med_costs_file <- system.file("extdata", "average_unit_costs_med_brand.csv",
                                 package = "packDAMipd")
   data_file <- system.file("extdata", "medication_nopatchname.xlsx",
                            package = "packDAMipd")
   ind_part_data <- load_trial_data(data_file)
   med_costs <- load_trial_data(med_costs_file)
   conv_file <- system.file("extdata", "Med_calc.xlsx",
                            package = "packDAMipd")
   table <- load_trial_data(conv_file)

   #using the package price when the brand name of the medication is known
   expect_error(microcosting_patches_wide(ind_part_data = ind_part_data,
                                    name_med = "patch_name",
                                    brand_med = "patch_brand",
                                    dose_med = "patch_strength",
                                    unit_med = NULL,
                                    no_taken = "patch_no_taken",
                                    freq_taken = "patch_frequency",
                                    timeperiod = "4 months",
                                    unit_cost_data = med_costs,
                                    unit_cost_column = "UnitCost",
                                    cost_calculated_per  = "Basis",
                                    strength_column = "Strength",
                                    list_of_code_names = NULL,
                                    list_of_code_freq = NULL,
                                    list_of_code_dose_unit = NULL,
                                    list_of_code_brand = NULL,
                                    eqdose_cov_tab = table,
                                    basis_strength_unit = "mcg/hr"))

   med_costs_file <- system.file("extdata", "average_unit_costs_med_brand_nounitcostcol.csv",
                                 package = "packDAMipd")
   data_file <- system.file("extdata", "medication.xlsx",
                            package = "packDAMipd")
   ind_part_data <- load_trial_data(data_file)
   med_costs <- load_trial_data(med_costs_file)
   conv_file <- system.file("extdata", "Med_calc.xlsx",
                            package = "packDAMipd")
   table <- load_trial_data(conv_file)

   #using the package price when the brand name of the medication is known
   expect_error(microcosting_patches_wide(ind_part_data = ind_part_data,
                                          name_med = "patch_name",
                                          brand_med = "patch_brand",
                                          dose_med = "patch_strength",
                                          unit_med = NULL,
                                          no_taken = "patch_no_taken",
                                          freq_taken = "patch_frequency",
                                          timeperiod = "4 months",
                                          unit_cost_data = med_costs,
                                          unit_cost_column = "UnitCost",
                                          cost_calculated_per  = "Basis",
                                          strength_column = "Strength",
                                          list_of_code_names = NULL,
                                          list_of_code_freq = NULL,
                                          list_of_code_dose_unit = NULL,
                                          list_of_code_brand = NULL,
                                          eqdose_cov_tab = table,
                                          basis_strength_unit = "mcg/hr"))

   med_costs_file <- system.file("extdata", "average_unit_costs_med_brand.csv",
                                 package = "packDAMipd")
   data_file <- system.file("extdata", "medication.xlsx",
                            package = "packDAMipd")
   ind_part_data <- load_trial_data(data_file)
   med_costs <- load_trial_data(med_costs_file)
   conv_file <- system.file("extdata", "Med_calc_unit_notright.xlsx",
                            package = "packDAMipd")
   table <- load_trial_data(conv_file)

   #using the package price when the brand name of the medication is known
   expect_error(microcosting_patches_wide(ind_part_data = ind_part_data,
                                          name_med = "patch_name",
                                          brand_med = "patch_brand",
                                          dose_med = "patch_strength",
                                          unit_med = NULL,
                                          no_taken = "patch_no_taken",
                                          freq_taken = "patch_frequency",
                                          timeperiod = "4 months",
                                          unit_cost_data = med_costs,
                                          unit_cost_column = "UnitCost",
                                          cost_calculated_per  = "Basis",
                                          strength_column = "Strength",
                                          list_of_code_names = NULL,
                                          list_of_code_freq = NULL,
                                          list_of_code_dose_unit = NULL,
                                          list_of_code_brand = NULL,
                                          eqdose_cov_tab = table,
                                          basis_strength_unit = "mcg/hr"))

   conv_file <- system.file("extdata", "Med_calc_testNA.xlsx",
                            package = "packDAMipd")
   table <- load_trial_data(conv_file)

   #using the package price when the brand name of the medication is known
   microcosting_patches_wide(ind_part_data = ind_part_data,
                                          name_med = "patch_name",
                                          brand_med = "patch_brand",
                                          dose_med = "patch_strength",
                                          unit_med = NULL,
                                          no_taken = "patch_no_taken",
                                          freq_taken = "patch_frequency",
                                          timeperiod = "4 months",
                                          unit_cost_data = med_costs,
                                          unit_cost_column = "UnitCost",
                                          cost_calculated_per  = "Basis",
                                          strength_column = "Strength",
                                          list_of_code_names = NULL,
                                          list_of_code_freq = NULL,
                                          list_of_code_dose_unit = NULL,
                                          list_of_code_brand = NULL,
                                          eqdose_cov_tab = table,
                                          basis_strength_unit = "mcg/hr")
   expect_equal(res$totmed_period_patches, c(1286, 2880), tolerance = 1e-1)


   med_costs_file <- system.file("extdata", "average_unit_costs_med_brand.csv",
                                 package = "packDAMipd")
   data_file <- system.file("extdata", "medication_all.xlsx",
                            package = "packDAMipd")
   ind_part_data <- load_trial_data(data_file)
   med_costs <- load_trial_data(med_costs_file)
   conv_file <- system.file("extdata", "Med_calc.xlsx",
                            package = "packDAMipd")
   table <- load_trial_data(conv_file)

   #using the package price when the brand name of the medication is known
   res <- microcosting_patches_wide(ind_part_data = ind_part_data,
                                          name_med = "patch_name",
                                          brand_med = "patch_brand",
                                          dose_med = "patch_strength",
                                          unit_med = NULL,
                                          no_taken = "patch_no_taken",
                                          freq_taken = "patch_frequency",
                                          timeperiod = "4 months",
                                          unit_cost_data = med_costs,
                                          unit_cost_column = "UnitCost",
                                          cost_calculated_per  = "Basis",
                                          strength_column = "Strength",
                                          list_of_code_names = NULL,
                                          list_of_code_freq = NULL,
                                          list_of_code_dose_unit = NULL,
                                          list_of_code_brand = NULL,
                                          eqdose_cov_tab = table,
                                          basis_strength_unit = "mcg/hr")
   expect_equal(res$totmed_period_patches, c(1286, NA), tolerance = 1e-3)

###############################################################################

   med_costs_file <- system.file("extdata", "average_unit_costs_med_brand.csv",
                                 package = "packDAMipd")
   data_file <- system.file("extdata", "medication_all.xlsx",
                            package = "packDAMipd")
   ind_part_data <- load_trial_data(data_file)
   med_costs <- load_trial_data(med_costs_file)
   conv_file <- system.file("extdata", "Med_calc.xlsx",
                            package = "packDAMipd")
   table <- load_trial_data(conv_file)
   #using the package price when the brand name of the medication is known

   res <- microcosting_tablets_wide(ind_part_data = ind_part_data,
                                            name_med = "tab_name",
                                            brand_med = "tab_brand",
                                            dose_med = "tab_strength",
                                            unit_med = "tab_str_unit",
                                            no_taken = "tab_no_taken",
                                            freq_taken = "tab_frequency",
                                            timeperiod = "2 months",
                                            unit_cost_data = med_costs,
                                            unit_cost_column = "UnitCost",
                                            cost_calculated_per  = "Basis",
                                            strength_column = "Strength",
                                            list_of_code_names = NULL,
                                            list_of_code_freq = NULL,
                                            list_of_code_dose_unit = NULL,
                                            eqdose_cov_tab = table,
                                            basis_strength_unit = "mg")
  expect_equal(res$totmed_period_tablets, c(608.5714, 0), tolerance = 1e-3)

  expect_error(microcosting_tablets_wide(ind_part_data = NULL,
                                   name_med = "tab_name",
                                   brand_med = "tab_brand",
                                   dose_med = "tab_strength",
                                   unit_med = "tab_str_unit",
                                   no_taken = "tab_no_taken",
                                   freq_taken = "tab_frequency",
                                   timeperiod = "2 months",
                                   unit_cost_data = med_costs,
                                   unit_cost_column = "UnitCost",
                                   cost_calculated_per  = "Basis",
                                   strength_column = "Strength",
                                   list_of_code_names = NULL,
                                   list_of_code_freq = NULL,
                                   list_of_code_dose_unit = NULL,
                                   eqdose_cov_tab = table,
                                   basis_strength_unit = "mg"))

  expect_error(microcosting_tablets_wide(ind_part_data = ind_part_data,
                                         name_med = NULL,
                                         brand_med = "tab_brand",
                                         dose_med = "tab_strength",
                                         unit_med = "tab_str_unit",
                                         no_taken = "tab_no_taken",
                                         freq_taken = "tab_frequency",
                                         timeperiod = "2 months",
                                         unit_cost_data = med_costs,
                                         unit_cost_column = "UnitCost",
                                         cost_calculated_per  = "Basis",
                                         strength_column = "Strength",
                                         list_of_code_names = NULL,
                                         list_of_code_freq = NULL,
                                         list_of_code_dose_unit = NULL,
                                         eqdose_cov_tab = table,
                                         basis_strength_unit = "mg"))

  med_costs_file <- system.file("extdata", "average_unit_costs_med_brand.csv",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "medication_all_no_tabname.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)
  conv_file <- system.file("extdata", "Med_calc.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)
  #using the package price when the brand name of the medication is known

  expect_error(microcosting_tablets_wide(ind_part_data = ind_part_data,
                                   name_med = "tab_name",
                                   brand_med = "tab_brand",
                                   dose_med = "tab_strength",
                                   unit_med = "tab_str_unit",
                                   no_taken = "tab_no_taken",
                                   freq_taken = "tab_frequency",
                                   timeperiod = "2 months",
                                   unit_cost_data = med_costs,
                                   unit_cost_column = "UnitCost",
                                   cost_calculated_per  = "Basis",
                                   strength_column = "Strength",
                                   list_of_code_names = NULL,
                                   list_of_code_freq = NULL,
                                   list_of_code_dose_unit = NULL,
                                   eqdose_cov_tab = table,
                                   basis_strength_unit = "mg"))

   med_costs_file <- system.file("extdata", "average_unit_costs_med_brand.csv",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "medication.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)
  conv_file <- system.file("extdata", "Med_calc.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)
  #using the package price when the brand name of the medication is known
  res <- microcosting_tablets_wide(ind_part_data = ind_part_data,
                                            name_med = "tab_name",
                                            brand_med = "tab_brand",
                                            dose_med = "tab_strength",
                                            unit_med = NULL,
                                            no_taken = "tab_no_taken",
                                            freq_taken = "tab_frequency",
                                            timeperiod = "2 months",
                                            unit_cost_data = med_costs,
                                            unit_cost_column = "UnitCost",
                                            cost_calculated_per  = "Basis",
                                            strength_column = "Strength",
                                            list_of_code_names = NULL,
                                            list_of_code_freq = NULL,
                                            list_of_code_dose_unit = NULL,
                                            eqdose_cov_tab = table,
                                            basis_strength_unit = "mg")

  expect_equal(res$totmed_period_tablets, c(608.5714, 48), tolerance = 1e-3)

  res <- microcosting_tablets_wide(ind_part_data = ind_part_data,
                                            name_med = "tab_name",
                                            brand_med = "tab_brand",
                                            dose_med = "tab_strength",
                                            unit_med = NULL,
                                            no_taken = "tab_no_taken",
                                            freq_taken = "tab_frequency",
                                            timeperiod = "2 months",
                                            unit_cost_data = med_costs,
                                            unit_cost_column = "UnitCost",
                                            cost_calculated_per  = "Basis",
                                            strength_column = "Strength",
                                            list_of_code_names = NULL,
                                            list_of_code_freq = NULL,
                                            list_of_code_dose_unit = NULL,
                                            eqdose_cov_tab = table,
                                            basis_strength_unit = NULL)
  expect_equal(res$totmed_period_tablets, c(608.5714, 48), tolerance = 1e-3)

  res <- microcosting_tablets_wide(ind_part_data = ind_part_data,
                                            name_med = "tab_name",
                                            brand_med = "tab_brand",
                                            dose_med = "tab_strength",
                                            unit_med = NULL,
                                            no_taken = "tab_no_taken",
                                            freq_taken = "tab_frequency",
                                            timeperiod = "2 months",
                                            unit_cost_data = med_costs,
                                            unit_cost_column = "UnitCost",
                                            cost_calculated_per  = "Basis",
                                            strength_column = "Strength",
                                            list_of_code_names = NULL,
                                            list_of_code_freq = NULL,
                                            list_of_code_dose_unit = NULL,
                                            eqdose_cov_tab = table,
                                            basis_strength_unit = NA)
  expect_equal(res$totmed_period_tablets, c(608.5714, 48), tolerance = 1e-3)


  expect_error(microcosting_tablets_wide(ind_part_data = ind_part_data,
                                            name_med = "tab_name",
                                            brand_med = "tab_brand",
                                            dose_med = "tab_strength",
                                            unit_med = NULL,
                                            no_taken = "tab_no_taken",
                                            freq_taken = "tab_frequency",
                                            timeperiod = "2 months",
                                            unit_cost_data = med_costs,
                                            unit_cost_column = "UnitCost",
                                            cost_calculated_per  = "Basis",
                                            strength_column = "Strength",
                                            list_of_code_names = NULL,
                                            list_of_code_freq = NULL,
                                            list_of_code_dose_unit = NULL,
                                            eqdose_cov_tab = table,
                                            basis_strength_unit = "NA"))

  med_costs_file <- system.file("extdata",
                  "average_unit_costs_med_brand_brandcolname_error.csv",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "medication.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)

  expect_error(microcosting_tablets_wide(ind_part_data = ind_part_data,
                                     name_med = "tab_name",
                                     brand_med = "tab_brand",
                                     dose_med = "tab_strength",
                                     unit_med = NULL,
                                     no_taken = "tab_no_taken",
                                     freq_taken = "tab_frequency",
                                     timeperiod = "2 months",
                                     unit_cost_data = med_costs,
                                     unit_cost_column = "UnitCost",
                                     cost_calculated_per  = "Basis",
                                     strength_column = "Strength",
                                     list_of_code_names = NULL,
                                     list_of_code_freq = NULL,
                                     list_of_code_dose_unit = NULL,
                                     eqdose_cov_tab = table,
                                     basis_strength_unit = NA))


  med_costs_file <- system.file("extdata",
                        "average_unit_costs_med_brand_sizecol_error.csv",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "medication.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)

  expect_error(microcosting_tablets_wide(ind_part_data = ind_part_data,
                                     name_med = "tab_name",
                                     brand_med = "tab_brand",
                                     dose_med = "tab_strength",
                                     unit_med = NULL,
                                     no_taken = "tab_no_taken",
                                     freq_taken = "tab_frequency",
                                     timeperiod = "2 months",
                                     unit_cost_data = med_costs,
                                     unit_cost_column = "UnitCost",
                                     cost_calculated_per  = "Basis",
                                     strength_column = "Strength",
                                     list_of_code_names = NULL,
                                     list_of_code_freq = NULL,
                                     list_of_code_dose_unit = NULL,
                                     eqdose_cov_tab = table,
                                     basis_strength_unit = NA))
  #no brand name for tablets
  med_costs_file <- system.file("extdata", "average_unit_costs_med.csv",
                                package = "packDAMipd")
  med_costs <- load_trial_data(med_costs_file)
  res <- microcosting_tablets_wide(ind_part_data = ind_part_data,
                                            name_med = "tab_name",
                                            brand_med = NULL,
                                            dose_med = "tab_strength",
                                            unit_med = NULL,
                                            no_taken = "tab_no_taken",
                                            freq_taken = "tab_frequency",
                                            timeperiod = "2 months",
                                            unit_cost_data = med_costs,
                                            unit_cost_column = "UnitCost",
                                            cost_calculated_per  = "Basis",
                                            strength_column = "Strength",
                                            list_of_code_names = NULL,
                                            list_of_code_freq = NULL,
                                            list_of_code_dose_unit = NULL,
                                            eqdose_cov_tab = table,
                                            basis_strength_unit = "mg")
  expect_equal(res$totmed_period_tablets, c(608.5714, 48), tolerance = 1e-3)



  med_costs_file <- system.file("extdata",
                              "average_unit_costs_med_nounitcost.csv",
                                package = "packDAMipd")
  med_costs <- load_trial_data(med_costs_file)

  expect_error(microcosting_tablets_wide(ind_part_data = ind_part_data,
                                           name_med = "tab_name",
                                           brand_med = NULL,
                                           dose_med = "tab_strength",
                                           unit_med = NULL,
                                           no_taken = "tab_no_taken",
                                           freq_taken = "tab_frequency",
                                           timeperiod = "2 months",
                                           unit_cost_data = med_costs,
                                           unit_cost_column = "UnitCost",
                                           cost_calculated_per  = "Basis",
                                           strength_column = "Strength",
                                           list_of_code_names = NULL,
                                           list_of_code_freq = NULL,
                                           list_of_code_dose_unit = NULL,
                                           eqdose_cov_tab = table,
                                           basis_strength_unit = "mg"))


  med_costs_file <- system.file("extdata", "average_unit_costs_med.csv",
                                package = "packDAMipd")
  med_costs <- load_trial_data(med_costs_file)
  data_file <- system.file("extdata", "medication_namemissing.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  expect_error(microcosting_tablets_wide(ind_part_data = ind_part_data,
                                           name_med = "tab_name",
                                           brand_med = NULL,
                                           dose_med = "tab_strength",
                                           unit_med = "tab_str_unit",
                                           no_taken = "tab_no_taken",
                                           freq_taken = "tab_frequency",
                                           timeperiod = "2 months",
                                           unit_cost_data = med_costs,
                                           unit_cost_column = "UnitCost",
                                           cost_calculated_per  = "Basis",
                                           strength_column = "Strength",
                                           list_of_code_names = NULL,
                                           list_of_code_freq = NULL,
                                           list_of_code_dose_unit = NULL,
                                           eqdose_cov_tab = table,
                                           basis_strength_unit = "mg"))

  med_costs_file <- system.file("extdata",
                              "average_unit_costs_med_namecolname_error.csv",
                                package = "packDAMipd")
  med_costs <- load_trial_data(med_costs_file)
  data_file <- system.file("extdata", "medication.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  expect_error(microcosting_tablets_wide(ind_part_data = ind_part_data,
                                                 name_med = "tab_name",
                                                 brand_med = NULL,
                                                 dose_med = "tab_strength",
                                                 unit_med = NULL,
                                                 no_taken = "tab_no_taken",
                                                 freq_taken = "tab_frequency",
                                                 timeperiod = "2 months",
                                                 unit_cost_data = med_costs,
                                             unit_cost_column = "UnitCost",
                                              cost_calculated_per  = "Basis",
                                                 strength_column = "Strength",
                                                 list_of_code_names = NULL,
                                                 list_of_code_freq = NULL,
                                                 list_of_code_dose_unit = NULL,
                                                 eqdose_cov_tab = table,
                                                 basis_strength_unit = "mg"))

  med_costs_file <- system.file("extdata",
                                "average_unit_costs_med_formcolname_error.csv",
                                package = "packDAMipd")
  med_costs <- load_trial_data(med_costs_file)
  data_file <- system.file("extdata", "medication.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  expect_error(microcosting_tablets_wide(ind_part_data = ind_part_data,
                                                 name_med = "tab_name",
                                                 brand_med = NULL,
                                                 dose_med = "tab_strength",
                                                 unit_med = NULL,
                                                 no_taken = "tab_no_taken",
                                                 freq_taken = "tab_frequency",
                                                 timeperiod = "2 months",
                                                 unit_cost_data = med_costs,
                                                 unit_cost_column = "UnitCost",
                                              cost_calculated_per  = "Basis",
                                                 strength_column = "Strength",
                                                 list_of_code_names = NULL,
                                                 list_of_code_freq = NULL,
                                                 list_of_code_dose_unit = NULL,
                                                 eqdose_cov_tab = table,
                                                 basis_strength_unit = "mg"))

  ## Information is coded  in the data file
  med_costs_file <- system.file("extdata", "average_unit_costs_med_brand.csv",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "medication_with_codes.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)
  conv_file <- system.file("extdata", "Med_calc.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)
  res <- microcosting_tablets_wide(ind_part_data = ind_part_data,
                            name_med = "tab_name",
                            brand_med = "tab_brand",
                            dose_med = "tab_strength",
                            unit_med = "tab_unit",
                            no_taken = "tab_no_taken",
                            freq_taken = "tab_frequency",
                            timeperiod = "4 months",
                            unit_cost_data = med_costs,
                            unit_cost_column = "UnitCost",
                            cost_calculated_per  = "Basis",
                            strength_column = "Strength",
                            list_of_code_names =
                              list(c(1, 2), c("Buprenorphine", "Fentanyl")),
                            list_of_code_freq =
                            list(c(1, 2, 3),
                                 c("once a day", "twice a day", "once a week")),
                            list_of_code_dose_unit =
                                list(c(1, 2), c("mcg", "mg")),
                            list_of_code_brand = list(c(1, 2), c("Buprenorphine",
                                                                 "Temgesic")),
                            eqdose_cov_tab = table,
                            basis_strength_unit = "mg")


  expect_equal(res$totmed_period_tablets, c(1217, 96), tolerance = 1e-1)

  res <- microcosting_tablets_wide(ind_part_data = ind_part_data,
                                   name_med = "tab_name",
                                   brand_med = "tab_brand",
                                   dose_med = "tab_strength",
                                   unit_med = "tab_unit",
                                   no_taken = "tab_no_taken",
                                   freq_taken = "tab_frequency",
                                   timeperiod = "4 months",
                                   unit_cost_data = med_costs,
                                   unit_cost_column = "UnitCost",
                                   cost_calculated_per  = "Basis",
                                   strength_column = "Strength",
                                   list_of_code_names =
                                     list(c(1, 2),c("Buprenorphine", "Fentanyl")),
                                   list_of_code_freq =
                                     list(c(1, 2, 3), c("once a day", "twice a day",
                                                        "once a week")),
                                   list_of_code_dose_unit =
                                     list(c(1, 2), c("mcg", "mg")),
                                   list_of_code_brand = list(c(1, 2),
                                                c("Buprenorphine","Temgesic")),
                                   eqdose_cov_tab = NULL,
                                   basis_strength_unit = "mg")

  res <- microcosting_tablets_wide(ind_part_data = ind_part_data,
                                   name_med = "tab_name",
                                   brand_med = "tab_brand",
                                   dose_med = "tab_strength",
                                   unit_med = "tab_unit",
                                   no_taken = "tab_no_taken",
                                   freq_taken = "tab_frequency",
                                   timeperiod = "4 months",
                                   unit_cost_data = med_costs,
                                   unit_cost_column = "UnitCost",
                                   cost_calculated_per  = "Basis",
                                   strength_column = "Strength",
                                   list_of_code_names =
                                     list(c(1, 2),c("Buprenorphine", "Fentanyl")),
                                   list_of_code_freq =
                                     list(c(1, 2, 3),
                                   c("once a day", "twice a day","once a week"),
                                          ),
                                   list_of_code_dose_unit =
                                     list(c(1, 2),c("mcg", "mg")),
                                   list_of_code_brand = list(c(1, 2),
                                      c("Buprenorphine","Temgesic")),
                                   eqdose_cov_tab = NA,
                                   basis_strength_unit = "mg")

})
###############################################################################
################################################################################
med_costs_file <- system.file("extdata", "average_unit_costs_med_brand.csv",
                              package = "packDAMipd")
data_file <- system.file("extdata", "medication_liq.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc.xlsx",
                         package = "packDAMipd")
table <- load_trial_data(conv_file)
#using the package price when the brand name of the me
res <- microcosting_liquids_wide(
                                 ind_part_data = ind_part_data,
                                 name_med = "liq_name",
                                 brand_med =  NULL,
                                 dose_med = "liq_strength",
                                 unit_med = NULL,
                                 bottle_size = "liq_bottle_size",
                                 bottle_size_unit = NULL,
                                 bottle_lasts = "liq_lasts",
                                 bottle_lasts_unit = NULL,
                                 preparation_dose = NULL,
                                 preparation_unit = NULL,
                                 timeperiod = "4 months",
                                 unit_cost_data = med_costs,
                                 unit_cost_column = "UnitCost",
                                 cost_calculated_per = "Basis",
                                 strength_column = "Strength",
                                 list_of_code_names = NULL,
                                 list_of_code_brand = NULL,
                                 list_of_code_dose_unit = NULL,
                                 list_of_code_bottle_size_unit = NULL,
                                 list_of_code_bottle_lasts_unit = NULL,
                                 list_preparation_dose_unit = NULL,
                                 eqdose_covtab = table,
                                 basis_strength_unit = NULL)

expect_equal(res$totmed_period_liquid, c(22, 9),
             tolerance = 1e-3)



expect_error(microcosting_liquids_wide(
                          ind_part_data = NULL,
                          name_med = "liq_name",
                          brand_med =  NULL,
                          dose_med = "liq_strength",
                          unit_med = NULL,
                          bottle_size = "liq_bottle_size",
                          bottle_size_unit = NULL,
                          bottle_lasts = "liq_lasts",
                          bottle_lasts_unit = NULL,
                          preparation_dose = NULL,
                          preparation_unit = NULL,
                          timeperiod = "4 months",
                          unit_cost_data = med_costs,
                          unit_cost_column = "UnitCost",
                          cost_calculated_per = "Basis",
                          strength_column = "Strength",
                          list_of_code_names = NULL,
                          list_of_code_brand = NULL,
                          list_of_code_dose_unit = NULL,
                          list_of_code_bottle_size_unit = NULL,
                          list_of_code_bottle_lasts_unit = NULL,
                          list_preparation_dose_unit = NULL,
                          eqdose_covtab = table,
                          basis_strength_unit = NULL
                          ))

expect_error(microcosting_liquids_wide(
                          ind_part_data = ind_part_data,
                          name_med = NULL,
                          brand_med =  NULL,
                          dose_med = "liq_strength",
                          unit_med = NULL,
                          bottle_size = "liq_bottle_size",
                          bottle_size_unit = NULL,
                          bottle_lasts = "liq_lasts",
                          bottle_lasts_unit = NULL,
                          preparation_dose = NULL,
                          preparation_unit = NULL,
                          timeperiod = "4 months",
                          unit_cost_data = med_costs,
                          unit_cost_column = "UnitCost",
                          cost_calculated_per = "Basis",
                          strength_column = "Strength",
                          list_of_code_names = NULL,
                          list_of_code_brand = NULL,
                          list_of_code_dose_unit = NULL,
                          list_of_code_bottle_size_unit = NULL,
                          list_of_code_bottle_lasts_unit = NULL,
                          list_preparation_dose_unit = NULL,
                          eqdose_covtab = table,
                          basis_strength_unit = NULL
                          ))

expect_error(microcosting_liquids_wide(
                          ind_part_data = ind_part_data,
                          name_med = "liq_name",
                          brand_med =  NULL,
                          dose_med = NULL,
                          unit_med = NULL,
                          bottle_size = "liq_bottle_size",
                          bottle_size_unit = NULL,
                          bottle_lasts = "liq_lasts",
                          bottle_lasts_unit = NULL,
                          preparation_dose = NULL,
                          preparation_unit = NULL,
                          timeperiod = "4 months",
                          unit_cost_data = med_costs,
                          unit_cost_column = "UnitCost",
                          cost_calculated_per = "Basis",
                          strength_column = "Strength",
                          list_of_code_names = NULL,
                          list_of_code_brand = NULL,
                          list_of_code_dose_unit = NULL,
                          list_of_code_bottle_size_unit = NULL,
                          list_of_code_bottle_lasts_unit = NULL,
                          list_preparation_dose_unit = NULL,
                          eqdose_covtab = table,
                          basis_strength_unit = NULL
                          ))

expect_error(microcosting_liquids_wide(
                          ind_part_data = ind_part_data,
                          name_med = "liq_name",
                          brand_med =  NULL,
                          dose_med = "liq_strength",
                          unit_med = NULL,
                          bottle_size = NULL,
                          bottle_size_unit = NULL,
                          bottle_lasts = "liq_lasts",
                          bottle_lasts_unit = NULL,
                          preparation_dose = NULL,
                          preparation_unit = NULL,
                          timeperiod = "4 months",
                          unit_cost_data = med_costs,
                          unit_cost_column = "UnitCost",
                          cost_calculated_per = "Basis",
                          strength_column = "Strength",
                          list_of_code_names = NULL,
                          list_of_code_brand = NULL,
                          list_of_code_dose_unit = NULL,
                          list_of_code_bottle_size_unit = NULL,
                          list_of_code_bottle_lasts_unit = NULL,
                          list_preparation_dose_unit = NULL,
                          eqdose_covtab = table,
                          basis_strength_unit = NULL
                          ))

expect_error(microcosting_liquids_wide(
                          ind_part_data = ind_part_data,
                          name_med = "liq_name",
                          brand_med =  NULL,
                          dose_med = "liq_strength",
                          unit_med = NULL,
                          bottle_size = NULL,
                          bottle_size_unit = NULL,
                          bottle_lasts = "liq_lasts",
                          bottle_lasts_unit = NULL,
                          preparation_dose = NULL,
                          preparation_unit = NULL,
                          timeperiod = "4 months",
                          unit_cost_data = med_costs,
                          unit_cost_column = "UnitCost",
                          cost_calculated_per = "Basis",
                          strength_column = "Strength",
                          list_of_code_names = NULL,
                          list_of_code_brand = NULL,
                          list_of_code_dose_unit = NULL,
                          list_of_code_bottle_size_unit = NULL,
                          list_of_code_bottle_lasts_unit = NULL,
                          list_preparation_dose_unit = NULL,
                          eqdose_covtab = table,
                          basis_strength_unit = NULL
                          ))

expect_error(microcosting_liquids_wide(
                          ind_part_data = ind_part_data,
                          name_med = "liq_name",
                          brand_med =  NULL,
                          dose_med = "liq_strength",
                          unit_med = NULL,
                          bottle_size = "liq_bottle_size",
                          bottle_size_unit = NULL,
                          bottle_lasts = NULL,
                          bottle_lasts_unit = NULL,
                          preparation_dose = NULL,
                          preparation_unit = NULL,
                          timeperiod = "4 months",
                          unit_cost_data = med_costs,
                          unit_cost_column = "UnitCost",
                          cost_calculated_per = "Basis",
                          strength_column = "Strength",
                          list_of_code_names = NULL,
                          list_of_code_brand = NULL,
                          list_of_code_dose_unit = NULL,
                          list_of_code_bottle_size_unit = NULL,
                          list_of_code_bottle_lasts_unit = NULL,
                          list_preparation_dose_unit = NULL,
                          eqdose_covtab = table,
                          basis_strength_unit = NULL
                          ))

expect_error(microcosting_liquids_wide(
                          ind_part_data = ind_part_data,
                          name_med = "liq_name",
                          brand_med =  NULL,
                          dose_med = "liq_strength",
                          unit_med = NULL,
                          bottle_size = "liq_bottle_size",
                          bottle_size_unit = NULL,
                          bottle_lasts = "liq_lasts",
                          bottle_lasts_unit = NULL,
                          preparation_dose = NULL,
                          preparation_unit = NULL,
                          timeperiod = NULL,
                          unit_cost_data = med_costs,
                          unit_cost_column = "UnitCost",
                          cost_calculated_per = "Basis",
                          strength_column = "Strength",
                          list_of_code_names = NULL,
                          list_of_code_brand = NULL,
                          list_of_code_dose_unit = NULL,
                          list_of_code_bottle_size_unit = NULL,
                          list_of_code_bottle_lasts_unit = NULL,
                          list_preparation_dose_unit = NULL,
                          eqdose_covtab = table,
                          basis_strength_unit = NULL
                          ))

expect_error(microcosting_liquids_wide(
                          ind_part_data = ind_part_data,
                          name_med = "liq_name",
                          brand_med =  NULL,
                          dose_med = "liq_strength",
                          unit_med = NULL,
                          bottle_size = "liq_bottle_size",
                          bottle_size_unit = NULL,
                          bottle_lasts = "liq_lasts",
                          bottle_lasts_unit = NULL,
                          preparation_dose = NULL,
                          preparation_unit = NULL,
                          timeperiod = "4 months",
                          unit_cost_data = NULL,
                          unit_cost_column = "UnitCost",
                          cost_calculated_per = "Basis",
                          strength_column = "Strength",
                          list_of_code_names = NULL,
                          list_of_code_brand = NULL,
                          list_of_code_dose_unit = NULL,
                          list_of_code_bottle_size_unit = NULL,
                          list_of_code_bottle_lasts_unit = NULL,
                          list_preparation_dose_unit = NULL,
                          eqdose_covtab = table,
                          basis_strength_unit = NULL
                          ))

expect_error(microcosting_liquids_wide(
                          ind_part_data = ind_part_data,
                          name_med = "liq_name",
                          brand_med =  NULL,
                          dose_med = "liq_strength",
                          unit_med = NULL,
                          bottle_size = "liq_bottle_size",
                          bottle_size_unit = NULL,
                          bottle_lasts = "liq_lasts",
                          bottle_lasts_unit = NULL,
                          preparation_dose = NULL,
                          preparation_unit = NULL,
                          timeperiod = "4 months",
                          unit_cost_data = med_costs,
                          unit_cost_column = NULL,
                          cost_calculated_per = "Basis",
                          strength_column = "Strength",
                          list_of_code_names = NULL,
                          list_of_code_brand = NULL,
                          list_of_code_dose_unit = NULL,
                          list_of_code_bottle_size_unit = NULL,
                          list_of_code_bottle_lasts_unit = NULL,
                          list_preparation_dose_unit = NULL,
                          eqdose_covtab = table,
                          basis_strength_unit = NULL
                          ))

expect_error(microcosting_liquids_wide(
                          ind_part_data = ind_part_data,
                          name_med = "liq_name",
                          brand_med =  NULL,
                          dose_med = "liq_strength",
                          unit_med = NULL,
                          bottle_size = "liq_bottle_size",
                          bottle_size_unit = NULL,
                          bottle_lasts = "liq_lasts",
                          bottle_lasts_unit = NULL,
                          preparation_dose = NULL,
                          preparation_unit = NULL,
                          timeperiod = "4 months",
                          unit_cost_data = med_costs,
                          unit_cost_column = "UnitCost",
                          cost_calculated_per = NULL,
                          strength_column = "Strength",
                          list_of_code_names = NULL,
                          list_of_code_brand = NULL,
                          list_of_code_dose_unit = NULL,
                          list_of_code_bottle_size_unit = NULL,
                          list_of_code_bottle_lasts_unit = NULL,
                          list_preparation_dose_unit = NULL,
                          eqdose_covtab = table,
                          basis_strength_unit = NULL
                          ))

expect_error(microcosting_liquids_wide(
                          ind_part_data = ind_part_data,
                          name_med = "liq_name",
                          brand_med =  NULL,
                          dose_med = "liq_strength",
                          unit_med = NULL,
                          bottle_size = "liq_bottle_size",
                          bottle_size_unit = NULL,
                          bottle_lasts = "liq_lasts",
                          bottle_lasts_unit = NULL,
                          preparation_dose = NULL,
                          preparation_unit = NULL,
                          timeperiod = "4 months",
                          unit_cost_data = med_costs,
                          unit_cost_column = "UnitCost",
                          cost_calculated_per = "Basis",
                          strength_column = NULL,
                          list_of_code_names = NULL,
                          list_of_code_brand = NULL,
                          list_of_code_dose_unit = NULL,
                          list_of_code_bottle_size_unit = NULL,
                          list_of_code_bottle_lasts_unit = NULL,
                          list_preparation_dose_unit = NULL,
                          eqdose_covtab = table,
                          basis_strength_unit = NULL
                          ))

res <- microcosting_liquids_wide(
                          ind_part_data = ind_part_data,
                          name_med = "liq_name",
                          brand_med =  NULL,
                          dose_med = "liq_strength",
                          unit_med = NULL,
                          bottle_size = "liq_bottle_size",
                          bottle_size_unit = NULL,
                          bottle_lasts = "liq_lasts",
                          bottle_lasts_unit = NULL,
                          preparation_dose = NULL,
                          preparation_unit = NULL,
                          timeperiod = "4 months",
                          unit_cost_data = med_costs,
                          unit_cost_column = "UnitCost",
                          cost_calculated_per = "Basis",
                          strength_column = "Strength",
                          list_of_code_names = NULL,
                          list_of_code_brand = NULL,
                          list_of_code_dose_unit = NULL,
                          list_of_code_bottle_size_unit = NULL,
                          list_of_code_bottle_lasts_unit = NULL,
                          list_preparation_dose_unit = NULL,
                          eqdose_covtab = NULL,
                          basis_strength_unit = NULL
                          )
expect_equal(res$totmed_period_liquid, c(22, 9), tolerance = 1e-3)



res <- microcosting_liquids_wide(ind_part_data = ind_part_data,
                                       name_med = "liq_name",
                                       brand_med =  NULL,
                                       dose_med = "liq_strength",
                                       unit_med = NULL,
                                       bottle_size = "liq_bottle_size",
                                       bottle_size_unit = NULL,
                                       bottle_lasts = "liq_lasts",
                                       bottle_lasts_unit = NULL,
                                       preparation_dose = NULL,
                                       preparation_unit = NULL,
                                       timeperiod = "4 months",
                                       unit_cost_data = med_costs,
                                       unit_cost_column = "UnitCost",
                                       cost_calculated_per = "Basis",
                                       strength_column = "Strength",
                                       list_of_code_names = NULL,
                                       list_of_code_brand = NULL,
                                       list_of_code_dose_unit = NULL,
                                       list_of_code_bottle_size_unit = NULL,
                                       list_of_code_bottle_lasts_unit = NULL,
                                       list_preparation_dose_unit = NULL,
                                       eqdose_covtab = NULL,
                                       basis_strength_unit = NA)
expect_equal(res$totmed_period_liquid, c(22, 9), tolerance = 1e-3)
res <- microcosting_liquids_wide(
                                 ind_part_data = ind_part_data,
                                 name_med = "liq_name",
                                 brand_med =  NULL,
                                 dose_med = "liq_strength",
                                 unit_med = NULL,
                                 bottle_size = "liq_bottle_size",
                                 bottle_size_unit = NULL,
                                 bottle_lasts = "liq_lasts",
                                 bottle_lasts_unit = NULL,
                                 preparation_dose = NULL,
                                 preparation_unit = NULL,
                                 timeperiod = "4 months",
                                 unit_cost_data = med_costs,
                                 unit_cost_column = "UnitCost",
                                 cost_calculated_per = "Basis",
                                 strength_column = "Strength",
                                 list_of_code_names = NULL,
                                 list_of_code_brand = NULL,
                                 list_of_code_dose_unit = NULL,
                                 list_of_code_bottle_size_unit = NULL,
                                 list_of_code_bottle_lasts_unit = NULL,
                                 list_preparation_dose_unit = NULL,
                                 eqdose_covtab = NULL,
                                 basis_strength_unit = "mg/ml")
expect_equal(res$totmed_period_liquid, c(22, 9), tolerance = 1e-3)
expect_error(microcosting_liquids_wide(
                                 ind_part_data = ind_part_data,
                                 name_med = "liq_name",
                                 brand_med =  NULL,
                                 dose_med = "liq_strength",
                                 unit_med = NULL,
                                 bottle_size = "liq_bottle_size",
                                 bottle_size_unit = NULL,
                                 bottle_lasts = "liq_lasts",
                                 bottle_lasts_unit = NULL,
                                 preparation_dose = NULL,
                                 preparation_unit = NULL,
                                 timeperiod = "4 months",
                                 unit_cost_data = med_costs,
                                 unit_cost_column = "UnitCost",
                                 cost_calculated_per = "Basis",
                                 strength_column = "Strength",
                                 list_of_code_names = NULL,
                                 list_of_code_brand = NULL,
                                 list_of_code_dose_unit = NULL,
                                 list_of_code_bottle_size_unit = NULL,
                                 list_of_code_bottle_lasts_unit = NULL,
                                 list_preparation_dose_unit = NULL,
                                 eqdose_covtab = NULL,
                                 basis_strength_unit = "mg/hh"))

med_costs_file <- system.file("extdata", "average_unit_costs_med_brand.csv",
                              package = "packDAMipd")
data_file <- system.file("extdata", "medication_liq_codes.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc.xlsx",
                         package = "packDAMipd")
table <- load_trial_data(conv_file)
res <- microcosting_liquids_wide(
                          ind_part_data = ind_part_data,
                          name_med = "liq_name",
                          brand_med =  "liq_brand",
                          dose_med = "liq_strength",
                          unit_med = "str_unit",
                          bottle_size = "liq_bottle_size",
                          bottle_size_unit = "bottle_unit",
                          bottle_lasts = "liq_lasts",
                          bottle_lasts_unit = "lasts_unit",
                          preparation_dose = "liq_pre",
                          preparation_unit = "pre_unit",
                          timeperiod = "4 months",
                          unit_cost_data = med_costs,
                          unit_cost_column = "UnitCost",
                          cost_calculated_per = "Basis",
                          strength_column = "Strength",
                          list_of_code_names =
                            list(c(1, 2), c("Morphine", "Oxycodone")),
                          list_of_code_brand =
                            list(c(1, 2), c("Oramorph", "Oxycodone")),
                          list_of_code_dose_unit =
                            list(c(1, 2), c("mg/ml", "g/ml")),
                          list_of_code_bottle_size_unit =
                            list(c(1, 2), c("ml", "l")),
                          list_of_code_bottle_lasts_unit =
                            list(c(1, 2, 3), c("days", "weeks", "months")),
                          list_preparation_dose_unit =
                            list(c(1, 2), c("mg/ml", "g/ml")),
                          eqdose_covtab = NULL,
                          basis_strength_unit = "mg/ml")

expect_equal(res$totmed_period_liquid, c(22, 9), tolerance = 1e-3)


med_costs_file <- system.file("extdata", "average_unit_costs_med_brand.csv",
                              package = "packDAMipd")
data_file <- system.file("extdata", "medication_liq_codes_notprep.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc.xlsx",
                         package = "packDAMipd")
table <- load_trial_data(conv_file)
res <- microcosting_liquids_wide(
                                 ind_part_data = ind_part_data,
                                 name_med = "liq_name",
                                 brand_med =  "liq_brand",
                                 dose_med = "liq_strength",
                                 unit_med = "str_unit",
                                 bottle_size = "liq_bottle_size",
                                 bottle_size_unit = "bottle_unit",
                                 bottle_lasts = "liq_lasts",
                                 bottle_lasts_unit = "lasts_unit",
                                 preparation_dose = "liq_pre",
                                 preparation_unit = NULL,
                                 timeperiod = "4 months",
                                 unit_cost_data = med_costs,
                                 unit_cost_column = "UnitCost",
                                 cost_calculated_per = "Basis",
                                 strength_column = "Strength",
                                 list_of_code_names =
                                   list(c(1, 2), c("Morphine", "Oxycodone")),
                                 list_of_code_brand =
                                   list( c(1, 2), c("Oramorph", "Oxycodone")),
                                 list_of_code_dose_unit =
                                   list(c(1, 2), c("mg/ml", "g/ml")),
                                 list_of_code_bottle_size_unit =
                                   list(c(1, 2), c("ml", "l")),
                                 list_of_code_bottle_lasts_unit =
                                   list(c(1, 2, 3),c("days", "weeks", "months")),
                                 list_preparation_dose_unit = NULL,
                                 eqdose_covtab = NULL,
                                 basis_strength_unit = "mg/ml")

expect_equal(res$totmed_period_liquid, c(22, 9), tolerance = 1e-3)

res <- microcosting_liquids_wide(
                                 ind_part_data = ind_part_data,
                                 name_med = "liq_name",
                                 brand_med =  "liq_brand",
                                 dose_med = "liq_strength",
                                 unit_med = "str_unit",
                                 bottle_size = "liq_bottle_size",
                                 bottle_size_unit = "bottle_unit",
                                 bottle_lasts = "liq_lasts",
                                 bottle_lasts_unit = "lasts_unit",
                                 preparation_dose = "liq_pre",
                                 preparation_unit = NULL,
                                 timeperiod = "4 months",
                                 unit_cost_data = med_costs,
                                 unit_cost_column = "UnitCost",
                                 cost_calculated_per = "Basis",
                                 strength_column = "Strength",
                                 list_of_code_names =
                                   list(c(1, 2), c("Morphine", "Oxycodone")),
                                 list_of_code_brand =
                                   list(c(1, 2),c("Oramorph", "Oxycodone")),
                                 list_of_code_dose_unit =
                                   list(c(1, 2),c("mg/ml", "g/ml")),
                                 list_of_code_bottle_size_unit =
                                   list(c(1, 2), c("ml", "l")),
                                 list_of_code_bottle_lasts_unit =
                                   list(c(1, 2, 3), c("days", "weeks", "months")),
                                 list_preparation_dose_unit = NULL,
                                 eqdose_covtab = NA,
                                 basis_strength_unit = "mg/ml")

expect_equal(res$totmed_period_liquid, c(22, 9), tolerance = 1e-3)


data_file <- system.file("extdata",
                         "medication_liq_codes_namenotsameasdose.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)

expect_error(microcosting_liquids_wide(
                                 ind_part_data = ind_part_data,
                                 name_med = "liq_name",
                                 brand_med =  "liq_brand",
                                 dose_med = "liq_strength",
                                 unit_med = "str_unit",
                                 bottle_size = "liq_bottle_size",
                                 bottle_size_unit = "bottle_unit",
                                 bottle_lasts = "liq_lasts",
                                 bottle_lasts_unit = "lasts_unit",
                                 preparation_dose = "liq_pre",
                                 preparation_unit = "pre_unit",
                                 timeperiod = "4 months",
                                 unit_cost_data = med_costs,
                                 unit_cost_column = "UnitCost",
                                 cost_calculated_per = "Basis",
                                 strength_column = "Strength",
                                 list_of_code_names =
                                   list(c(1, 2),c("Morphine", "Oxycodone")),
                                 list_of_code_brand =
                                   list(c(1, 2), c("Oramorph", "Oxycodone")),
                                 list_of_code_dose_unit =
                                   list(c(1, 2), c("mg/ml", "g/ml")),
                                 list_of_code_bottle_size_unit =
                                   list(c(1, 2), c("ml", "l")),
                                 list_of_code_bottle_lasts_unit =
                                   list(c(1, 2, 3), c("days", "weeks", "months")),
                                 list_preparation_dose_unit =
                                   list(c(1, 2), c("mg/ml", "g/ml")),
                                 eqdose_covtab = NA,
                                 basis_strength_unit = "mg/ml"))

med_costs_file <-
  system.file("extdata", "average_unit_costs_med_brand_errorbrand.csv",
                              package = "packDAMipd")
data_file <- system.file("extdata", "medication_liq.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc.xlsx",
                         package = "packDAMipd")
table <- load_trial_data(conv_file)
#using the package price when the brand name of the me
expect_error(microcosting_liquids_wide(
                                 ind_part_data = ind_part_data,
                                 name_med = "liq_name",
                                 brand_med =  "liq_brand",
                                 dose_med = "liq_strength",
                                 unit_med = NULL,
                                 bottle_size = "liq_bottle_size",
                                 bottle_size_unit = NULL,
                                 bottle_lasts = "liq_lasts",
                                 bottle_lasts_unit = NULL,
                                 preparation_dose = NULL,
                                 preparation_unit = NULL,
                                 timeperiod = "4 months",
                                 unit_cost_data = med_costs,
                                 unit_cost_column = "UnitCost",
                                 cost_calculated_per = "Basis",
                                 strength_column = "Strength",
                                 list_of_code_names = NULL,
                                 list_of_code_brand = NULL,
                                 list_of_code_dose_unit = NULL,
                                 list_of_code_bottle_size_unit = NULL,
                                 list_of_code_bottle_lasts_unit = NULL,
                                 list_preparation_dose_unit = NULL,
                                 eqdose_covtab = table,
                                 basis_strength_unit = NULL
                                 ))

med_costs_file <-
  system.file("extdata", "average_unit_costs_med_brand.csv",
              package = "packDAMipd")
data_file <- system.file("extdata", "medication_liq_liqlastsbeforeday.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc.xlsx",
                         package = "packDAMipd")
table <- load_trial_data(conv_file)
#using the package price when the brand name of the me
res <- microcosting_liquids_wide(
     ind_part_data = ind_part_data,
                                       name_med = "liq_name",
                                       brand_med =  "liq_brand",
                                       dose_med = "liq_strength",
                                       unit_med = NULL,
                                       bottle_size = "liq_bottle_size",
                                       bottle_size_unit = NULL,
                                       bottle_lasts = "liq_lasts",
                                       bottle_lasts_unit = NULL,
                                       preparation_dose = NULL,
                                       preparation_unit = NULL,
                                       timeperiod = "4 months",
                                       unit_cost_data = med_costs,
                                       unit_cost_column = "UnitCost",
                                       cost_calculated_per = "Basis",
                                       strength_column = "Strength",
                                       list_of_code_names = NULL,
                                       list_of_code_brand = NULL,
                                       list_of_code_dose_unit = NULL,
                                       list_of_code_bottle_size_unit = NULL,
                                       list_of_code_bottle_lasts_unit = NULL,
                                       list_preparation_dose_unit = NULL,
                                       eqdose_covtab = table,
                                       basis_strength_unit = NULL
                                       )
expect_equal(res$totmed_period_liquid, c(482, 9), tolerance = 1e-3)


med_costs_file <- system.file("extdata", "average_unit_costs_med_brand.csv",
                              package = "packDAMipd")
data_file <- system.file("extdata", "medication_liq_nonamecol.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc.xlsx",
                         package = "packDAMipd")
table <- load_trial_data(conv_file)
#using the package price when the brand name of the me
expect_error(microcosting_liquids_wide(
  ind_part_data = ind_part_data,
  name_med = "liq_name",
  brand_med =  NULL,
  dose_med = "liq_strength",
  unit_med = NULL,
  bottle_size = "liq_bottle_size",
  bottle_size_unit = NULL,
  bottle_lasts = "liq_lasts",
  bottle_lasts_unit = NULL,
  preparation_dose = NULL,
  preparation_unit = NULL,
  timeperiod = "4 months",
  unit_cost_data = med_costs,
  unit_cost_column = "UnitCost",
  cost_calculated_per = "Basis",
  strength_column = "Strength",
  list_of_code_names = NULL,
  list_of_code_brand = NULL,
  list_of_code_dose_unit = NULL,
  list_of_code_bottle_size_unit = NULL,
  list_of_code_bottle_lasts_unit = NULL,
  list_preparation_dose_unit = NULL,
  eqdose_covtab = table,
  basis_strength_unit = NULL))

med_costs_file <- system.file("extdata", "average_unit_costs_med_brand_nounitcostcol.csv",
                              package = "packDAMipd")
data_file <- system.file("extdata", "medication_liq.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc.xlsx",
                         package = "packDAMipd")
table <- load_trial_data(conv_file)
#using the package price when the brand name of the me
expect_error(microcosting_liquids_wide(
  ind_part_data = ind_part_data,
  name_med = "liq_name",
  brand_med =  NULL,
  dose_med = "liq_strength",
  unit_med = NULL,
  bottle_size = "liq_bottle_size",
  bottle_size_unit = NULL,
  bottle_lasts = "liq_lasts",
  bottle_lasts_unit = NULL,
  preparation_dose = NULL,
  preparation_unit = NULL,
  timeperiod = "4 months",
  unit_cost_data = med_costs,
  unit_cost_column = "UnitCost",
  cost_calculated_per = "Basis",
  strength_column = "Strength",
  list_of_code_names = NULL,
  list_of_code_brand = NULL,
  list_of_code_dose_unit = NULL,
  list_of_code_bottle_size_unit = NULL,
  list_of_code_bottle_lasts_unit = NULL,
  list_preparation_dose_unit = NULL,
  eqdose_covtab = table,
  basis_strength_unit = NULL))

med_costs_file <- system.file("extdata", "average_unit_costs_med_brand_nomatchingdose.csv",
                              package = "packDAMipd")
data_file <- system.file("extdata", "medication_liq.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc.xlsx",
                         package = "packDAMipd")
table <- load_trial_data(conv_file)
#using the package price when the brand name of the me
expect_error(microcosting_liquids_wide(
  ind_part_data = ind_part_data,
  name_med = "liq_name",
  brand_med =  "liq_brand",
  dose_med = "liq_strength",
  unit_med = NULL,
  bottle_size = "liq_bottle_size",
  bottle_size_unit = NULL,
  bottle_lasts = "liq_lasts",
  bottle_lasts_unit = NULL,
  preparation_dose = NULL,
  preparation_unit = NULL,
  timeperiod = "4 months",
  unit_cost_data = med_costs,
  unit_cost_column = "UnitCost",
  cost_calculated_per = "Basis",
  strength_column = "Strength",
  list_of_code_names = NULL,
  list_of_code_brand = NULL,
  list_of_code_dose_unit = NULL,
  list_of_code_bottle_size_unit = NULL,
  list_of_code_bottle_lasts_unit = NULL,
  list_preparation_dose_unit = NULL,
  eqdose_covtab = table,
  basis_strength_unit = NULL))



med_costs_file <- system.file("extdata", "average_unit_costs_med_brand_bottlevol_notmatching.csv",
                              package = "packDAMipd")
data_file <- system.file("extdata", "medication_liq.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc.xlsx",
                         package = "packDAMipd")
table <- load_trial_data(conv_file)
#using the package price when the brand name of the me
expect_error(microcosting_liquids_wide(
  ind_part_data = ind_part_data,
  name_med = "liq_name",
  brand_med =  "liq_brand",
  dose_med = "liq_strength",
  unit_med = NULL,
  bottle_size = "liq_bottle_size",
  bottle_size_unit = NULL,
  bottle_lasts = "liq_lasts",
  bottle_lasts_unit = NULL,
  preparation_dose = NULL,
  preparation_unit = NULL,
  timeperiod = "4 months",
  unit_cost_data = med_costs,
  unit_cost_column = "UnitCost",
  cost_calculated_per = "Basis",
  strength_column = "Strength",
  list_of_code_names = NULL,
  list_of_code_brand = NULL,
  list_of_code_dose_unit = NULL,
  list_of_code_bottle_size_unit = NULL,
  list_of_code_bottle_lasts_unit = NULL,
  list_preparation_dose_unit = NULL,
  eqdose_covtab = table,
  basis_strength_unit = NULL))

med_costs_file <- system.file("extdata", "average_unit_costs_med_brand_notcostedperbottle.csv",
                              package = "packDAMipd")
data_file <- system.file("extdata", "medication_liq.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc.xlsx",
                         package = "packDAMipd")
table <- load_trial_data(conv_file)
#using the package price when the brand name of the me
expect_error(microcosting_liquids_wide(
  ind_part_data = ind_part_data,
  name_med = "liq_name",
  brand_med =  "liq_brand",
  dose_med = "liq_strength",
  unit_med = NULL,
  bottle_size = "liq_bottle_size",
  bottle_size_unit = NULL,
  bottle_lasts = "liq_lasts",
  bottle_lasts_unit = NULL,
  preparation_dose = NULL,
  preparation_unit = NULL,
  timeperiod = "4 months",
  unit_cost_data = med_costs,
  unit_cost_column = "UnitCost",
  cost_calculated_per = "Basis",
  strength_column = "Strength",
  list_of_code_names = NULL,
  list_of_code_brand = NULL,
  list_of_code_dose_unit = NULL,
  list_of_code_bottle_size_unit = NULL,
  list_of_code_bottle_lasts_unit = NULL,
  list_preparation_dose_unit = NULL,
  eqdose_covtab = table,
  basis_strength_unit = NULL))

med_costs_file <- system.file("extdata", "average_unit_costs_med_brand.csv",
                              package = "packDAMipd")
data_file <- system.file("extdata", "medication_liq_bottlevol_unitdiff.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc.xlsx",
                         package = "packDAMipd")
table <- load_trial_data(conv_file)
#using the package price when the brand name of the me
expect_error(microcosting_liquids_wide(
  ind_part_data = ind_part_data,
  name_med = "liq_name",
  brand_med =  "liq_brand",
  dose_med = "liq_strength",
  unit_med = NULL,
  bottle_size = "liq_bottle_size",
  bottle_size_unit = NULL,
  bottle_lasts = "liq_lasts",
  bottle_lasts_unit = NULL,
  preparation_dose = NULL,
  preparation_unit = NULL,
  timeperiod = "4 months",
  unit_cost_data = med_costs,
  unit_cost_column = "UnitCost",
  cost_calculated_per = "Basis",
  strength_column = "Strength",
  list_of_code_names = NULL,
  list_of_code_brand = NULL,
  list_of_code_dose_unit = NULL,
  list_of_code_bottle_size_unit = NULL,
  list_of_code_bottle_lasts_unit = NULL,
  list_preparation_dose_unit = NULL,
  eqdose_covtab = table,
  basis_strength_unit = NULL))
med_costs_file <- system.file("extdata", "average_unit_costs_med_brand.csv",
                              package = "packDAMipd")
data_file <- system.file("extdata", "medication_liq.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc_unit_notright.xlsx",
                         package = "packDAMipd")
table <- load_trial_data(conv_file)
#using the package price when the brand name of the me
expect_error(microcosting_liquids_wide(
  ind_part_data = ind_part_data,
  name_med = "liq_name",
  brand_med =  "liq_brand",
  dose_med = "liq_strength",
  unit_med = NULL,
  bottle_size = "liq_bottle_size",
  bottle_size_unit = NULL,
  bottle_lasts = "liq_lasts",
  bottle_lasts_unit = NULL,
  preparation_dose = NULL,
  preparation_unit = NULL,
  timeperiod = "4 months",
  unit_cost_data = med_costs,
  unit_cost_column = "UnitCost",
  cost_calculated_per = "Basis",
  strength_column = "Strength",
  list_of_code_names = NULL,
  list_of_code_brand = NULL,
  list_of_code_dose_unit = NULL,
  list_of_code_bottle_size_unit = NULL,
  list_of_code_bottle_lasts_unit = NULL,
  list_preparation_dose_unit = NULL,
  eqdose_covtab = table,
  basis_strength_unit = NULL))


med_costs_file <- system.file("extdata", "average_unit_costs_med_brand.csv",
                              package = "packDAMipd")
data_file <- system.file("extdata", "medication_all.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc.xlsx",
                         package = "packDAMipd")
table <- load_trial_data(conv_file)
#using the package price when the brand name of the me
res <- microcosting_liquids_wide(
  ind_part_data = ind_part_data,
  name_med = "liq_name",
  brand_med =  "liq_brand",
  dose_med = "liq_strength",
  unit_med = NULL,
  bottle_size = "liq_bottle_size",
  bottle_size_unit = NULL,
  bottle_lasts = "liq_lasts",
  bottle_lasts_unit = NULL,
  preparation_dose = NULL,
  preparation_unit = NULL,
  timeperiod = "4 months",
  unit_cost_data = med_costs,
  unit_cost_column = "UnitCost",
  cost_calculated_per = "Basis",
  strength_column = "Strength",
  list_of_code_names = NULL,
  list_of_code_brand = NULL,
  list_of_code_dose_unit = NULL,
  list_of_code_bottle_size_unit = NULL,
  list_of_code_bottle_lasts_unit = NULL,
  list_preparation_dose_unit = NULL,
  eqdose_covtab = table,
  basis_strength_unit = NULL)
expect_equal(res$totmed_period_liquid, c(22, NA), tolerance = 1e-3)
###############################################################################
###############################################################################
context("testing microcosting patches when data being long format")
test_that("testing microcosting patches when data being long format", {
  med_costs_file <- system.file("extdata", "average_unit_costs_med_brand.csv",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "medication.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)
  conv_file <- system.file("extdata", "Med_calc.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)

  names <- colnames(ind_part_data)
  ending <- length(names)
  ind_part_data_long <- tidyr::gather(ind_part_data, measurement, value,
                          names[2]:names[ending], factor_key = TRUE)

  the_columns <- c("measurement", "value")

  #using the package price when the brand name of the medication is known
  res <- microcosting_patches_long(the_columns,
                                   ind_part_data_long = ind_part_data_long,
                                   name_med = "patch_name",
                                   brand_med = "patch_brand",
                                   dose_med = "patch_strength",
                                   unit_med = NULL,
                                   no_taken = "patch_no_taken",
                                   freq_taken = "patch_frequency",
                                   timeperiod = "4 months",
                                   unit_cost_data = med_costs,
                                   unit_cost_column = "UnitCost",
                                   cost_calculated_per  = "Basis",
                                   strength_column = "Strength",
                                   list_of_code_names = NULL,
                                   list_of_code_freq = NULL,
                                   list_of_code_dose_unit = NULL,
                                   list_of_code_brand = NULL,
                                   eqdose_cov_tab = table,
                                   basis_strength_unit = "mcg/hr")

  index =  which(res$measurment == "totmed_period_patches")[1]

  expect_equal(as.numeric(res$value[index]), 1285.714,tolerance = 1e-3)

  expect_error(microcosting_patches_long(the_columns,
                            ind_part_data_long = NULL,
                            name_med = "patch_name",
                            brand_med = "patch_brand",
                            dose_med = "patch_strength",
                            unit_med = NULL,
                            no_taken = "patch_no_taken",
                            freq_taken = "patch_frequency",
                            timeperiod = "4 months",
                            unit_cost_data = med_costs,
                            unit_cost_column = "UnitCost",
                            cost_calculated_per  = "Basis",
                            strength_column = "Strength",
                            list_of_code_names = NULL,
                            list_of_code_freq = NULL,
                            list_of_code_dose_unit = NULL,
                            list_of_code_brand = NULL,
                            eqdose_cov_tab = table,
                            basis_strength_unit = "mcg/hr"))


})
###############################################################################
###############################################################################

context("testing microcosting liquids when data being long format")
test_that("testing microcosting liquids when data being long format", {
  med_costs_file <- system.file("extdata", "average_unit_costs_med_brand.csv",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "medication_liq.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)
  conv_file <- system.file("extdata", "Med_calc.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)

  names <- colnames(ind_part_data)
  ending <- length(names)
  ind_part_data_long <- tidyr::gather(ind_part_data, measurement, value,
                                      names[2]:names[ending], factor_key = TRUE)
  the_columns <- c("measurement", "value")
  res <- microcosting_liquids_long(the_columns,
    ind_part_data_long = ind_part_data_long,
    name_med = "liq_name",
    brand_med =  NULL,
    dose_med = "liq_strength",
    unit_med = NULL,
    bottle_size = "liq_bottle_size",
    bottle_size_unit = NULL,
    bottle_lasts = "liq_lasts",
    bottle_lasts_unit = NULL,
    preparation_dose = NULL,
    preparation_unit = NULL,
    timeperiod = "4 months",
    unit_cost_data = med_costs,
    unit_cost_column = "UnitCost",
    cost_calculated_per = "Basis",
    strength_column = "Strength",
    list_of_code_names = NULL,
    list_of_code_brand = NULL,
    list_of_code_dose_unit = NULL,
    list_of_code_bottle_size_unit = NULL,
    list_of_code_bottle_lasts_unit = NULL,
    list_preparation_dose_unit = NULL,
    eqdose_covtab = table,
    basis_strength_unit = NULL)

  index =  which(res$measurment == "totmed_period_liquid")[1]

  expect_equal(as.numeric(res$value[index]), 22,tolerance = 1e-3)


  expect_error(microcosting_liquids_long(the_columns,
                            ind_part_data_long = NULL,
                            name_med = "liq_name",
                            brand_med =  NULL,
                            dose_med = "liq_strength",
                            unit_med = NULL,
                            bottle_size = "liq_bottle_size",
                            bottle_size_unit = NULL,
                            bottle_lasts = "liq_lasts",
                            bottle_lasts_unit = NULL,
                            preparation_dose = NULL,
                            preparation_unit = NULL,
                            timeperiod = "4 months",
                            unit_cost_data = med_costs,
                            unit_cost_column = "UnitCost",
                            cost_calculated_per = "Basis",
                            strength_column = "Strength",
                            list_of_code_names = NULL,
                            list_of_code_brand = NULL,
                            list_of_code_dose_unit = NULL,
                            list_of_code_bottle_size_unit = NULL,
                            list_of_code_bottle_lasts_unit = NULL,
                            list_preparation_dose_unit = NULL,
                            eqdose_covtab = table,
                            basis_strength_unit = NULL))

})
###############################################################################
###############################################################################

context("testing microcosting tablets when data being long format")
test_that("testing microcosting tablets when data being long format", {
med_costs_file <- system.file("extdata", "average_unit_costs_med_brand.csv",
                              package = "packDAMipd")
data_file <- system.file("extdata", "medication_all.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc.xlsx",
                         package = "packDAMipd")
table <- load_trial_data(conv_file)

names <- colnames(ind_part_data)
ending <- length(names)
ind_part_data_long <- tidyr::gather(ind_part_data, measurement, value,
                                    names[2]:names[ending], factor_key = TRUE)
the_columns <- c("measurement", "value")
res <- microcosting_tablets_long(the_columns,
                                 ind_part_data_long = ind_part_data_long,
                                 name_med = "tab_name",
                                 brand_med = "tab_brand",
                                 dose_med = "tab_strength",
                                 unit_med = "tab_str_unit",
                                 no_taken = "tab_no_taken",
                                 freq_taken = "tab_frequency",
                                 timeperiod = "2 months",
                                 unit_cost_data = med_costs,
                                 unit_cost_column = "UnitCost",
                                 cost_calculated_per  = "Basis",
                                 strength_column = "Strength",
                                 list_of_code_names = NULL,
                                 list_of_code_freq = NULL,
                                 list_of_code_dose_unit = NULL,
                                 eqdose_cov_tab = table,
                                 basis_strength_unit = "mg")

index =  which(res$measurment == "totmed_period_tablets")[1]

expect_equal(as.numeric(res$value[index]), 608.57,tolerance = 1e-3)

expect_error(microcosting_tablets_long(the_columns,
                                 ind_part_data_long = NULL,
                                 name_med = "tab_name",
                                 brand_med = "tab_brand",
                                 dose_med = "tab_strength",
                                 unit_med = "tab_str_unit",
                                 no_taken = "tab_no_taken",
                                 freq_taken = "tab_frequency",
                                 timeperiod = "2 months",
                                 unit_cost_data = med_costs,
                                 unit_cost_column = "UnitCost",
                                 cost_calculated_per  = "Basis",
                                 strength_column = "Strength",
                                 list_of_code_names = NULL,
                                 list_of_code_freq = NULL,
                                 list_of_code_dose_unit = NULL,
                                 eqdose_cov_tab = table,
                                 basis_strength_unit = "mg"))
})
