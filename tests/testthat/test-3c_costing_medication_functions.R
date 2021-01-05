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
  res <- microcosting_tablets_patches_wide(form = "patch",
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
                                           list_of_code_names = NULL,
                                           list_of_code_freq = NULL,
                                           list_of_code_dose_unit = NULL,
                                           list_of_code_brand = NULL,
                                           eqdose_cov_tab = table,
                                           basis_time = "day",
                                           basis_strength_unit = "mcg/hr")


  expect_equal(res$totmed_basis_patches, c(10.71429, 24.00000), tolerance = 1e-3)
  expect_equal(res$totcost_basis_patches, c(49.15,12.59), tolerance = 1e-3)
  expect_equal(res$totcost_period_patches, c(1081.30, 604.32), tolerance = 1e-3)

  res <- microcosting_tablets_patches_wide(form = "patch",
                                            ind_part_data = ind_part_data,
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
                                            basis_time = "day",
                                            basis_strength_unit = "mcg/hr")

  expect_equal(res$totmed_basis_patches, c(10.71429, 24.00000), tolerance = 1e-3)
  res <- microcosting_tablets_patches_wide(form = "patch",
                                            ind_part_data = ind_part_data,
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
                                            basis_time = "day",
                                            basis_strength_unit = "mcg/hr")

  expect_equal(res$totmed_basis_patches, c(10.71429, 24.00000), tolerance = 1e-3)


  # no cols with drug names
  conv_file <- system.file("extdata", "Med_calc_nodrugcol.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)
  expect_error(microcosting_tablets_patches_wide(form = "patch",
                                            ind_part_data = ind_part_data,
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
                                            basis_time = "day",
                                            basis_strength_unit = "mcg/hr"))

  # no column for form in conversion table
  conv_file <- system.file("extdata", "Med_calc_noformcol.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)
  expect_error(microcosting_tablets_patches_wide(form = "patch",
                                            ind_part_data = ind_part_data,
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
                                            basis_time = "day",
                                            basis_strength_unit = "mcg/hr"))
 # no unit column in conversion table
  conv_file <- system.file("extdata", "Med_calc_nounitcol.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)

  expect_error(microcosting_tablets_patches_wide(form = "patch",
                                            ind_part_data = ind_part_data,
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
                                            basis_time = "day",
                                            basis_strength_unit = "mcg/hr"))
  #no conversion factor column in conversion table
  conv_file <- system.file("extdata", "Med_calc_noconvfactorcol.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)

  expect_error(microcosting_tablets_patches_wide(form = "patch",
                                                  ind_part_data = ind_part_data,
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
                                                  basis_time = "day",
                                                  basis_strength_unit = "mcg/hr"))


  conv_file <- system.file("extdata", "Med_calc.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)

  res <- microcosting_tablets_patches_wide(form = "patch",
                                            ind_part_data = ind_part_data,
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
                                            basis_time = "day",
                                            basis_strength_unit = "mcg/hr")

  expect_equal(res$totmed_basis_patches, c(10.71429, 24.00000), tolerance = 1e-3)

  expect_error(microcosting_tablets_patches_wide(form = NULL,
                                            ind_part_data = ind_part_data,
                                            name_med = "patch_name",
                                            brand_med = "patch_brand",
                                            dose_med = "patch_strength",
                                            unit_med = NULL,
                                            no_taken = "patch_no_taken",
                                            freq_taken = "patch_frequency",
                                            basis_time = "day",
                                            timeperiod = "4 months",
                                            unit_cost_data = med_costs,
                                            unit_cost_column = "UnitCost",
                                            cost_calculated_per  = "Basis",
                                            strength_column = "Strength",
                                            list_of_code_names = NULL,
                                            list_of_code_freq = NULL,
                                            list_of_code_dose_unit = NULL,
                                            eqdose_cov_tab = table,
                                            basis_time = "day",
                                            basis_strength_unit = "mg"))

  expect_error(microcosting_tablets_patches_wide(form = "patch",
                                            ind_part_data = NULL,
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
                                            basis_time = "day",
                                            basis_strength_unit = "mcg/hr"))

  expect_error(microcosting_tablets_patches_wide(form = "patch",
                                            ind_part_data = ind_part_data,
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
                                            basis_time = "day",
                                            basis_strength_unit = "mcg/hr"))


  expect_error(microcosting_tablets_patches_wide(form = "patch",
                                            ind_part_data = ind_part_data,
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
                                            basis_time = "day",
                                            basis_strength_unit = "mcg/hr"))

  expect_error(microcosting_tablets_patches_wide(form = "patch",
                                            ind_part_data = ind_part_data,
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
                                            basis_time = "day",
                                            basis_strength_unit = "mcg/hr"))

  expect_error(microcosting_tablets_patches_wide(form = "patch",
                                            ind_part_data = ind_part_data,
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
                                            basis_time = "day",
                                            basis_strength_unit = "mcg/hr"))

  expect_error(microcosting_tablets_patches_wide(form = "patch",
                                            ind_part_data = ind_part_data,
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
                                            basis_time = "day",
                                            basis_strength_unit = "mcg/hr"))

  expect_error(microcosting_tablets_patches_wide(form = "patch",
                                            ind_part_data = ind_part_data,
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
                                            basis_time = "day",
                                            basis_strength_unit = "mcg/hr"))

  expect_error(microcosting_tablets_patches_wide(form = "patch",
                                            ind_part_data = ind_part_data,
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
                                            basis_time = "day",
                                            basis_strength_unit = "mcg/hr"))

  expect_error(microcosting_tablets_patches_wide(form = "patch",
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
                                            cost_calculated_per  = NULL,
                                            strength_column = "Strength",
                                            list_of_code_names = NULL,
                                            list_of_code_freq = NULL,
                                            list_of_code_dose_unit = NULL,
                                            eqdose_cov_tab = table,
                                            basis_time = "day",
                                            basis_strength_unit = "mcg/hr"))
  expect_error(microcosting_tablets_patches_wide(form = "patch",
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
                                            strength_column = NULL,
                                            list_of_code_names = NULL,
                                            list_of_code_freq = NULL,
                                            list_of_code_dose_unit = NULL,
                                            eqdose_cov_tab = table,
                                            basis_time = "day",
                                            basis_strength_unit = "mcg/hr"))

  expect_error(microcosting_tablets_patches_wide(form = "tt",
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
                                                  list_of_code_names = NULL,
                                                  list_of_code_freq = NULL,
                                                  list_of_code_dose_unit = NULL,
                                                  eqdose_cov_tab = table,
                                                  basis_time = "day",
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
  res <- microcosting_tablets_patches_wide(form = "patch",
                                            ind_part_data = ind_part_data,
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
                                            basis_time = NULL,
                                            basis_strength_unit = NULL)
  expect_equal(res$totmed_basis_patches, c(10.71429, 24.00000), tolerance = 1e-3)
  expect_equal(res$totcost_basis_patches, c(16.29, 40.00), tolerance = 1e-3)
  expect_equal(res$totcost_period_patches, c(1400.94, 4800.00), tolerance = 1e-3)

  res <- microcosting_tablets_patches_wide(form = "patch",
                                            ind_part_data = ind_part_data,
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
                                            basis_time = NULL,
                                            basis_strength_unit = NULL)
  expect_equal(res$totmed_basis_patches, c(10.71429, 24.00000), tolerance = 1e-3)


  res <- microcosting_tablets_patches_wide(form = "patch",
                                            ind_part_data = ind_part_data,
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
                                            basis_time = NA,
                                            basis_strength_unit = NA)
  expect_equal(res$totmed_basis_patches, c(10.71429, 24.00000), tolerance = 1e-3)
  expect_equal(res$totcost_basis_patches, c(16.29, 40.00), tolerance = 1e-3)
  expect_equal(res$totcost_period_patches, c(1400.94, 4800.00), tolerance = 1e-3)


  expect_error(microcosting_tablets_patches_wide(form = "patch",
                                            ind_part_data = ind_part_data,
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
                                            basis_time = "NA",
                                            basis_strength_unit = NA))

  expect_error(microcosting_tablets_patches_wide(form = "patch",
                                                  ind_part_data = ind_part_data,
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
                                                  basis_time = NA,
                                                  basis_strength_unit = "NA"))


  med_costs_file <- system.file("extdata", "average_unit_costs_med_brand_errorbrand.csv",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "medication.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)
  conv_file <- system.file("extdata", "Med_calc.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)
  #using the package price when the brand name of the medication is known
  expect_error(microcosting_tablets_patches_wide(form = "patch",
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
                                            list_of_code_names = NULL,
                                            list_of_code_freq = NULL,
                                            list_of_code_dose_unit = NULL,
                                            list_of_code_brand = NULL,
                                            eqdose_cov_tab = table,
                                            basis_time = "day",
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
  res <- microcosting_tablets_patches_wide(form = "patch",
                                            ind_part_data = ind_part_data,
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
                              list(c("Buprenorphine", "Fentanyl"), c(1, 2)),
                                            list_of_code_freq =
              list(c("once a day", "twice a day", "once a week"), c(1, 2, 3)),
                                            list_of_code_dose_unit =
                list(c("mcg/hr", "mcg/day"), c(1, 2)),
              list_of_code_brand = list(c("BuTrans", "Fencino", "Butec"), c(1, 2, 3)),
                                           eqdose_cov_tab = table,
                                            basis_time = "day",
                                            basis_strength_unit = "mcg/hr")


  expect_equal(res$totmed_basis_patches, c(10.71429, 24.00000), tolerance = 1e-3)
  expect_equal(res$totcost_basis_patches, c(49.15, 12.59), tolerance = 1e-3)
  expect_equal(res$totcost_period_patches, c(1081.30, 604.32), tolerance = 1e-3)


   expect_error(microcosting_tablets_patches_wide(form = "patch",
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
                                             list(c("Buprenorphine", "Fentanyl"), c(1, 2)),
                                           list_of_code_freq =
                                             list(c("once a day", "twice a day", "once a week"), c(1, 2, 3)),
                                           list_of_code_dose_unit =
                                             list(c("mcg/hr", "mcg/day"), c(1, 2)),
                                           list_of_code_brand =
                                             list(c("BuTrans", "Fencino", "Butec"), c(1, 2, 3)),
                                           eqdose_cov_tab = table,
                                           basis_time = "day",
                                           basis_strength_unit = "mcg/hr"))


   med_costs_file <- system.file("extdata", "average_unit_costs_med_brand_nodosage.csv",
                                 package = "packDAMipd")
   data_file <- system.file("extdata", "medication.xlsx",
                            package = "packDAMipd")
   ind_part_data <- load_trial_data(data_file)
   med_costs <- load_trial_data(med_costs_file)
   conv_file <- system.file("extdata", "Med_calc.xlsx",
                            package = "packDAMipd")
   table <- load_trial_data(conv_file)
   #using the package price when the brand name of the medication is known
   expect_error(microcosting_tablets_patches_wide(form = "patch",
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
                                            list_of_code_names = NULL,
                                            list_of_code_freq = NULL,
                                            list_of_code_dose_unit = NULL,
                                            list_of_code_brand = NULL,
                                            eqdose_cov_tab = table,
                                            basis_time = "day",
                                            basis_strength_unit = "mcg/hr"))

   med_costs_file <- system.file("extdata", "average_unit_costs_med_brand_unitnotidentify.csv",
                                 package = "packDAMipd")
   data_file <- system.file("extdata", "medication.xlsx",
                            package = "packDAMipd")
   ind_part_data <- load_trial_data(data_file)
   med_costs <- load_trial_data(med_costs_file)
   conv_file <- system.file("extdata", "Med_calc.xlsx",
                            package = "packDAMipd")
   table <- load_trial_data(conv_file)
   #using the package price when the brand name of the medication is known
   expect_error(microcosting_tablets_patches_wide(form = "patch",
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
                                                  list_of_code_names = NULL,
                                                  list_of_code_freq = NULL,
                                                  list_of_code_dose_unit = NULL,
                                                  list_of_code_brand = NULL,
                                                  eqdose_cov_tab = table,
                                                  basis_time = "day",
                                                  basis_strength_unit = "mcg/hr"))


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
   res <- microcosting_tablets_patches_wide(form = "tablets",
                                            ind_part_data = ind_part_data,
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
                                            basis_time = "day",
                                            basis_strength_unit = "mg")
    expect_equal(res$totmed_basis_tablets, c(10.14286, NA), tolerance = 1e-3)

  ###############################################################################
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
  res <- microcosting_tablets_patches_wide(form = "tablets",
                                            ind_part_data = ind_part_data,
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
                                            basis_time = "day",
                                            basis_strength_unit = "mg")

  expect_equal(res$totmed_equiv_basis_tablets, c(355, 28), tolerance = 1e-3)
  expect_equal(res$totcost_per_equiv_period_tablets, c(0.0186019, 0.0150000), tolerance = 1e-3)
  expect_equal(res$totcost_period_tablets, c(42.88, 25.20), tolerance = 1e-3)

  res <- microcosting_tablets_patches_wide(form = "tablets",
                                            ind_part_data = ind_part_data,
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
                                            basis_time = "day",
                                            basis_strength_unit = NULL)
  expect_equal(res$totmed_equiv_basis_tablets, c(355, 28), tolerance = 1e-3)
  expect_equal(res$totcost_per_equiv_period_tablets, c(0.0186019, 0.0150000), tolerance = 1e-3)
  expect_equal(res$totcost_period_tablets, c(42.88, 25.20), tolerance = 1e-3)

  res <- microcosting_tablets_patches_wide(form = "tablets",
                                            ind_part_data = ind_part_data,
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
                                            basis_time = "day",
                                            basis_strength_unit = NA)
  expect_equal(res$totmed_equiv_basis_tablets, c(355, 28), tolerance = 1e-3)
  expect_equal(res$totcost_per_equiv_period_tablets, c(0.0186019, 0.0150000), tolerance = 1e-3)
  expect_equal(res$totcost_period_tablets, c(42.88, 25.20), tolerance = 1e-3)


  expect_error(microcosting_tablets_patches_wide(form = "tablets",
                                            ind_part_data = ind_part_data,
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
                                            basis_time = "day",
                                            basis_strength_unit = "NA"))
  med_costs_file <- system.file("extdata", "average_unit_costs_med_brand_brandcolname_error.csv",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "medication.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)

  expect_error(microcosting_tablets_patches_wide(form = "tablets",
                                     ind_part_data = ind_part_data,
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
                                     basis_time = "day",
                                     basis_strength_unit = NA))


  med_costs_file <- system.file("extdata", "average_unit_costs_med_brand_sizecol_error.csv",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "medication.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)

  expect_error(microcosting_tablets_patches_wide(form = "tablets",
                                     ind_part_data = ind_part_data,
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
                                     basis_time = "day",
                                     basis_strength_unit = NA))
  #no brand name for tablets
  med_costs_file <- system.file("extdata", "average_unit_costs_med.csv",
                                package = "packDAMipd")
  med_costs <- load_trial_data(med_costs_file)
  res <- microcosting_tablets_patches_wide(form = "tablets",
                                            ind_part_data = ind_part_data,
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
                                            basis_time = "day",
                                            basis_strength_unit = "mg")
  expect_equal(res$totmed_equiv_basis_tablets, c(355, 28), tolerance = 1e-3)



  med_costs_file <- system.file("extdata", "average_unit_costs_med_nounitcost.csv",
                                package = "packDAMipd")
  med_costs <- load_trial_data(med_costs_file)

  expect_error(microcosting_tablets_patches_wide(form = "tablets",
                                           ind_part_data = ind_part_data,
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
                                           basis_time = "day",
                                           basis_strength_unit = "mg"))


  med_costs_file <- system.file("extdata", "average_unit_costs_med.csv",
                                package = "packDAMipd")
  med_costs <- load_trial_data(med_costs_file)
  data_file <- system.file("extdata", "medication_namemissing.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  expect_error(microcosting_tablets_patches_wide(form = "tablets",
                                           ind_part_data = ind_part_data,
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
                                           basis_time = "day",
                                           basis_strength_unit = "mg"))

  med_costs_file <- system.file("extdata", "average_unit_costs_med_namecolname_error.csv",
                                package = "packDAMipd")
  med_costs <- load_trial_data(med_costs_file)
  data_file <- system.file("extdata", "medication.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  expect_error(microcosting_tablets_patches_wide(form = "tablets",
                                                 ind_part_data = ind_part_data,
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
                                                 basis_time = "day",
                                                 basis_strength_unit = "mg"))

  med_costs_file <- system.file("extdata", "average_unit_costs_med_formcolname_error.csv",
                                package = "packDAMipd")
  med_costs <- load_trial_data(med_costs_file)
  data_file <- system.file("extdata", "medication.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  expect_error(microcosting_tablets_patches_wide(form = "tablets",
                                                 ind_part_data = ind_part_data,
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
                                                 basis_time = "day",
                                                 basis_strength_unit = "mg"))


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
res <- microcosting_liquids_wide(form = "liquid",
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
                                 basis_strength_unit = NULL,
                                 basis_time = NULL)

expect_equal(res$totmed_basis_liquid, c(54.16667, 17.85714), tolerance = 1e-3)
expect_equal(res$totcost_basis_liquid, c(8.08, 6.18), tolerance = 1e-3)
expect_equal(res$totmed_equiv_basis_liquid, c(56.25, 26.78571), tolerance = 1e-3)

expect_error(microcosting_liquids_wide(form = NULL,
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
                                 basis_strength_unit = NULL,
                                 basis_time = NULL))

expect_error(microcosting_liquids_wide(form = "liquid",
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
                          basis_strength_unit = NULL,
                          basis_time = NULL))

expect_error(microcosting_liquids_wide(form = "liquid",
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
                          basis_strength_unit = NULL,
                          basis_time = NULL))

expect_error(microcosting_liquids_wide(form = "liquid",
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
                          basis_strength_unit = NULL,
                          basis_time = NULL))

expect_error(microcosting_liquids_wide(form = "liquid",
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
                          basis_strength_unit = NULL,
                          basis_time = NULL))

expect_error(microcosting_liquids_wide(form = "liquid",
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
                          basis_strength_unit = NULL,
                          basis_time = NULL))

expect_error(microcosting_liquids_wide(form = "liquid",
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
                          basis_strength_unit = NULL,
                          basis_time = NULL))

expect_error(microcosting_liquids_wide(form = "liquid",
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
                          basis_strength_unit = NULL,
                          basis_time = NULL))

expect_error(microcosting_liquids_wide(form = "liquid",
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
                          basis_strength_unit = NULL,
                          basis_time = NULL))

expect_error(microcosting_liquids_wide(form = "liquid",
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
                          basis_strength_unit = NULL,
                          basis_time = NULL))

expect_error(microcosting_liquids_wide(form = "liquid",
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
                          basis_strength_unit = NULL,
                          basis_time = NULL))

expect_error(microcosting_liquids_wide(form = "liquid",
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
                          basis_strength_unit = NULL,
                          basis_time = NULL))

res <- microcosting_liquids_wide(form = "liquid",
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
                          basis_strength_unit = NULL,
                          basis_time = NULL)
expect_equal(res$totmed_basis_liquid, c(54.16667, 17.85714), tolerance = 1e-3)


expect_error(microcosting_liquids_wide(form = "hu",
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
                          basis_strength_unit = NULL,
                          basis_time = NULL))


res <- microcosting_liquids_wide(form = "liquid",
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
                                       basis_strength_unit = NA,
                                       basis_time = NA)
expect_equal(res$totmed_basis_liquid, c(54.16667, 17.85714), tolerance = 1e-3)
res <- microcosting_liquids_wide(form = "liquid",
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
                                 basis_strength_unit = "mg/ml",
                                 basis_time = "day")
expect_equal(res$totmed_basis_liquid, c(54.16667, 17.85714), tolerance = 1e-3)

expect_error(microcosting_liquids_wide(form = "liquid",
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
                                 basis_strength_unit = "mg/hh",
                                 basis_time = "day"))

med_costs_file <- system.file("extdata", "average_unit_costs_med_brand.csv",
                              package = "packDAMipd")
data_file <- system.file("extdata", "medication_liq_codes.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc.xlsx",
                         package = "packDAMipd")
table <- load_trial_data(conv_file)
res <- microcosting_liquids_wide(form = "liquid",
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
                            list(c("Morphine", "Oxycodone"), c(1, 2)),
                          list_of_code_brand =
                            list(c("Oramorph", "Oxycodone"), c(1, 2)),
                          list_of_code_dose_unit =
                            list(c("mg/ml", "g/ml"), c(1, 2)),
                          list_of_code_bottle_size_unit =
                            list(c("ml", "l"), c(1, 2)),
                          list_of_code_bottle_lasts_unit =
                            list(c("days", "weeks", "months"), c(1, 2, 3)),
                          list_preparation_dose_unit =
                            list(c("mg/ml", "g/ml"), c(1, 2)),
                          eqdose_covtab = NULL,
                          basis_strength_unit = "mg/ml",
                          basis_time = "day")

expect_equal(res$totmed_basis_liquid, c(54.16667, 17.85714), tolerance = 1e-3)
expect_equal(res$totcost_basis_liquid, c(8.08, 6.18), tolerance = 1e-3)
expect_equal(res$totmed_equiv_basis_liquid, c(54.16667, 17.85714), tolerance = 1e-3)


med_costs_file <- system.file("extdata", "average_unit_costs_med_brand.csv",
                              package = "packDAMipd")
data_file <- system.file("extdata", "medication_liq_codes_notprep.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc.xlsx",
                         package = "packDAMipd")
table <- load_trial_data(conv_file)
res <- microcosting_liquids_wide(form = "liquid",
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
                                   list(c("Morphine", "Oxycodone"), c(1, 2)),
                                 list_of_code_brand =
                                   list(c("Oramorph", "Oxycodone"), c(1, 2)),
                                 list_of_code_dose_unit =
                                   list(c("mg/ml", "g/ml"), c(1, 2)),
                                 list_of_code_bottle_size_unit =
                                   list(c("ml", "l"), c(1, 2)),
                                 list_of_code_bottle_lasts_unit =
                                   list(c("days", "weeks", "months"), c(1, 2, 3)),
                                 list_preparation_dose_unit = NULL,
                                 eqdose_covtab = NULL,
                                 basis_strength_unit = "mg/ml",
                                 basis_time = "day")

expect_equal(res$totmed_basis_liquid, c(54.16667, 17.85714), tolerance = 1e-3)
expect_equal(res$totcost_basis_liquid, c(8.08, 6.18), tolerance = 1e-3)
expect_equal(res$totmed_equiv_basis_liquid, c(54.16667, 17.85714), tolerance = 1e-3)

res <- microcosting_liquids_wide(form = "liquid",
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
                                   list(c("Morphine", "Oxycodone"), c(1, 2)),
                                 list_of_code_brand =
                                   list(c("Oramorph", "Oxycodone"), c(1, 2)),
                                 list_of_code_dose_unit =
                                   list(c("mg/ml", "g/ml"), c(1, 2)),
                                 list_of_code_bottle_size_unit =
                                   list(c("ml", "l"), c(1, 2)),
                                 list_of_code_bottle_lasts_unit =
                                   list(c("days", "weeks", "months"), c(1, 2, 3)),
                                 list_preparation_dose_unit = NULL,
                                 eqdose_covtab = NA,
                                 basis_strength_unit = "mg/ml",
                                 basis_time = "day")

expect_equal(res$totmed_basis_liquid, c(54.16667, 17.85714), tolerance = 1e-3)
expect_equal(res$totcost_basis_liquid, c(8.08, 6.18), tolerance = 1e-3)
expect_equal(res$totmed_equiv_basis_liquid, c(54.16667, 17.85714), tolerance = 1e-3)


data_file <- system.file("extdata", "medication_liq_codes_namenotsameasdose.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)

expect_error(microcosting_liquids_wide(form = "liquid",
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
                                   list(c("Morphine", "Oxycodone"), c(1, 2)),
                                 list_of_code_brand =
                                   list(c("Oramorph", "Oxycodone"), c(1, 2)),
                                 list_of_code_dose_unit =
                                   list(c("mg/ml", "g/ml"), c(1, 2)),
                                 list_of_code_bottle_size_unit =
                                   list(c("ml", "l"), c(1, 2)),
                                 list_of_code_bottle_lasts_unit =
                                   list(c("days", "weeks", "months"), c(1, 2, 3)),
                                 list_preparation_dose_unit =
                                   list(c("mg/ml", "g/ml"), c(1, 2)),
                                 eqdose_covtab = NA,
                                 basis_strength_unit = "mg/ml",
                                 basis_time = "day"))

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
expect_error(microcosting_liquids_wide(form = "liquid",
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
                                 basis_strength_unit = NULL,
                                 basis_time = NULL))

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
res <- microcosting_liquids_wide(form = "liquid",
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
                                       basis_strength_unit = NULL,
                                       basis_time = NULL)
expect_equal(res$totmed_basis_liquid, c(1204, 17.85714), tolerance = 1e-3)


data_file <- system.file("extdata", "medication_liq_codes.xlsx",
package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
data_column_nos = c(2,12)
list_of_code_names = list(c("Morphine", "Oxycodone"), c(1, 2))
encode_codes_data(list_of_code_names, data_column_nos, ind_part_data)
