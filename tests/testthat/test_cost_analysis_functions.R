###############################################################################
context("testing microcosting patches")
test_that("testing microcosting patches", {
  med_costs_file <- system.file("extdata", "average_unit_costs_med.csv", package = "packDAMipd")
  data_file <- system.file("extdata", "resource_use_p.csv", package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)
  example1 <- ind_part_data[1, ]
  # example 1 Buprenorphine 5nos, 20 mcg/hour patch taken once a week with MED of 100
  # cost should be calculated for 1 week use
  # Buprenorphine 20 mcg/h cost is £14.37 per patch per strength
  # basis time is day
  # total patch usage per day =(5)/7 mcg/hour =0.714
  # total cost patch usage per day = (5)/7 mcg/hour * 14.37 GBP per patch = 10.26
  # total cost patch usage per week = (10.26 * 7)= 71.85

  res <- microcosting_patches(
    example1, "Name", "patch_strength", "patch_dose_unit", "patch_no_taken",
    "patch_frequency", "day", med_costs, "UnitCost", "StrengthUnit", "Strength",
    list(c("4 weeks", "1 week"), c(1, 2)), list(c("Buprenorphine", "Morphine"), c(1, 2)),
    NULL, NULL, "patch_equiv_dose"
  )
  expect_equal(res$tot_basis_patches, 0.714, tolerance = 1e-3)
  expect_equal(res$totcost_patches_basis, 10.26, tolerance = 1e-3)
  expect_equal(res$totcost_timeperiod_patches, 71.825, tolerance = 1e-3)

  # example 2 Buprenorphine 7nos, 10 mcg/hour patch taken twice a day with MED of 6
  # cost should be calculated for 4 weeks use
  # Buprenorphine 10 mcg/h cost is 7.89 per patch
  # basis time is day
  # total patch numbers usage per day = (7)*2  = 14
  # total cost patch usage per day = (7)*2 *7.89  GBP = 110.46
  # total cost patch usage for 4 weeks = 110.46*4*7  * = 3092.88
  # total cost patch usage for 4 weeks per MED =  3092.88/6 = 515.48
  example2 <- ind_part_data[11, ]
  res <- microcosting_patches(
    example2, "Name", "patch_strength", "patch_dose_unit", "patch_no_taken",
    "patch_frequency", "day", med_costs, "UnitCost", "StrengthUnit", "Strength",
    list(c("4 weeks", "1 week"), c(1, 2)), list(c("Buprenorphine", "Morphine"), c(1, 2)),
    NULL, NULL, "patch_equiv_dose"
  )
  expect_equal(res$tot_basis_patches, 14, tolerance = 1e-3)
  expect_equal(res$totcost_patches_basis, 110.425, tolerance = 1e-3)
  expect_equal(res$totcost_timeperiod_patches, 3091.9, tolerance = 1e-3)
  expect_equal(res$totcost_patches_timeperiod_equiv_dose, 515.3167, tolerance = 1e-3)
  #  the column name "strength" not in IPD
  expect_error(microcosting_patches(
    ind_part_data, "Name", "strength", "patch_dose_unit", "patch_no_taken",
    "patch_frequency", "day", med_costs, "UnitCost", "StrengthUnit", "Strength",
    list(c("4 weeks", "1 week"), c(1, 2)), list(c("Buprenorphine", "Morphine"), c(1, 2)),
    list(c("Once a day", "twice a day", "once weekly"), c(1, 2, 3)),
    list(c("mcg/hr", "mg/day", "mg/hr"), c(1, 2, 3)), "patch_equiv_dose"
  ))
  #  frequency is not coded, but given exaclty
  expect_error(microcosting_patches(
    ind_part_data, "Name", "patch_strength", "patch_dose_unit", "patch_no_taken",
    "patch_frequency", "day", med_costs, "UnitCost", "StrengthUnit", "Strength",
    list(c("4 weeks", "1 week"), c(1, 2)), list(c("Buprenorphine", "Morphine"), c(1, 2)),
    list(c("Once a day", "twice a day", "once weekly"), c(1, 2, 3)),
    NULL, "patch_equiv_dose"
  ))

  #  dose unit is not coded, and given exacty in IPD
  expect_error(microcosting_patches(
    example2, "Name", "patch_strength", "patch_dose_unit", "patch_no_taken",
    "patch_frequency", "day", med_costs, "UnitCost", "StrengthUnit", "Strength",
    list(c("4 weeks", "1 week"), c(1, 2)), list(c("Buprenorphine", "Morphine"), c(1, 2)),
    NULL, list(c("mcg/hr", "mg/day", "mg/hr"), c(1, 2, 3)), "patch_equiv_dose"
  ))

  #  time period should not be NULL
  expect_error(microcosting_patches(
    ind_part_data, "Name", "patch_strength", "patch_dose_unit", "patch_no_taken",
    "patch_frequency", "day", med_costs, "UnitCost", "StrengthUnit", "Strength",
    NULL, list(c("Buprenorphine", "Morphine"), c(1, 2)),
    list(c("Once a day", "twice a day", "once weekly"), c(1, 2, 3)),
    list(c("mcg/hr", "mg/day", "mg/hr"), c(1, 2, 3)), "patch_equiv_dose"
  ))
  #  column with name "dose" not in unit cost data
  expect_error(microcosting_patches(
    ind_part_data, "Name", "patch_strength", "patch_dose_unit", "patch_no_taken",
    "patch_frequency", "hour", med_costs, "UnitCost", "StrengthUnit", "dose",
    list(c("4 weeks", "1 week"), c(1, 2)), list(c("Buprenorphine", "Morphine"), c(1, 2)),
    list(c("Once a day", "twice a day", "once weekly"), c(1, 2, 3)),
    list(c("mcg/hr", "mg/day", "mg/hr"), c(1, 2, 3)), "patch_equiv_dose"
  ))


  error_costs_file <- system.file("extdata", "costs_error.csv", package = "packDAMipd")
  datafile <- system.file("extdata", "resource_use_patches.csv", package = "packDAMipd")
  ind_part_data <- load_trial_data(datafile)
  med_costs <- load_trial_data(error_costs_file)
  # cost file in wrong format
  expect_error(microcosting_patches(
    ind_part_data, "Name", "patch_strength", "patch_dose_unit", "patch_no_taken",
    "patch_frequency", "day", med_costs, "UnitCost", "StrengthUnit", "Strength",
    list(c("4 weeks", "1 week"), c(1, 2)), list(c("Buprenorphine", "Morphine"), c(1, 2)),
    list(c("Once a day", "twice a day", "once weekly"), c(1, 2, 3)),
    list(c("mcg/hr", "mg/day", "mg/hr"), c(1, 2, 3)), "patch_equiv_dose"
  ))
})

###############################################################################
context("testing microcosting tablets")
test_that("testing microcosting tablets", {
  med_costs_file <- system.file("extdata", "average_unit_costs_med.csv", package = "packDAMipd")
  data_file <- system.file("extdata", "resource_use_t.csv", package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)
  freq_desc <- c(
    "Once a day", "Twice a day", "Three times a day",
    "Four times a day", "Five times a day", "Six times a day",
    "Seven times a day", "Eight times a day", "Nine times a day",
    "Ten times a day", "Once every 2 days", "Once every 3 days",
    "Once every 4 days", "Once every 5 days", "Once every 6 days",
    "Once a week", "Twice a week", "Thrice a week", "Four times a week"
  )
  codes <- c(seq(1:19))
  this_list <- list(freq_desc, codes)
  example1 <- ind_part_data[1, ]
  # Buprenorphine 1 tablet 8mg once a day with MED 40
  # total tablet a day = 8 mg
  # unit cost of Buprenorphine  8 mg is 2.52 per tablet
  # total cost a day =  2.52
  # total cost for 1 week = 2.52 *7 = 17.64
  # total cost for 1 week per MED = 17.64/40= 0.441

  res <- microcosting_tablets(
    example1, "Drug", "tab_dosage", "tab_dosage_unit", "tab_no_taken",
    "tab_frequency", "day", med_costs, "UnitCost",
    "StrengthUnit", "Strength", list(c("4 weeks", "1 week"), c(1, 2)),
    NULL, this_list,
    list(c("mcg", "mg", "gm"), c(1, 2, 3)), "tab_equiv_dose"
  )
  expect_equal(res$tot_basis_tablets, 1, tolerance = 1e-3)
  expect_equal(res$totcost_tablets_basis, 2.52, tolerance = 1e-3)
  expect_equal(res$totcost_timeperiod_tablets, 17.64, tolerance = 1e-3)
  expect_equal(res$totcost_tablets_timeperiod_equiv_dose, 0.441, tolerance = 1e-3)

  expect_error(microcosting_tablets(
    example1, "Drug", "tab_dosage", "tab_dosage_unit", "tab_no_taken",
    "tab_frequency", "day", med_costs, "UnitCost",
    "StrengthUnit", "Strength", list(c("4 weeks", "1 week"), c(1, 2)),
    NULL, NULL,
    list(c("mcg", "mg", "gm"), c(1, 2, 3)), "tab_equiv_dose"
  ))

  expect_error(microcosting_tablets(
    example1, "Drug", "tab_dosage", NULL, "tab_no_taken",
    "tab_frequency", "day", med_costs, "UnitCost",
    "StrengthUnit", "Strength", list(c("4 weeks", "1 week"), c(1, 2)),
    NULL, this_list,
    list(c("mcg", "mg", "gm"), c(1, 2, 3)), "tab_equiv_dose"
  ))

  expect_error(microcosting_tablets(
    example1, "Drug", "tab_dosage", "tab_dosage_unit", NULL,
    "tab_frequency", "day", med_costs, "UnitCost",
    "StrengthUnit", "Strength", list(c("4 weeks", "1 week"), c(1, 2)),
    NULL, this_list,
    list(c("mcg", "mg", "gm"), c(1, 2, 3)), "tab_equiv_dose"
  ))
})
###############################################################################

context("testing microcosting liquids")
test_that("testing microcosting liquids", {
  med_costs_file <- system.file("extdata", "average_unit_costs_med.csv", package = "packDAMipd")
  data_file <- system.file("extdata", "resource_use_l.csv", package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)
  example1 <- ind_part_data[1, ]
  # Morphine liquid 2 mg/ml bottle of size 100 (assuming ml) lasted for 2 weeks
  # cost of bottle is 2.08
  # no of bottles used = 2
  # total cost for 4 weeks = 2*2.08
  res <- microcosting_liquids(
    example1, "Drug", "liq_dosage", "liquid_dose_unit",
    "liquid_bottle_size", "liquid_bottle_remain_time", NULL,
    med_costs, "UnitCost", "SizeUnit",
    "Strength",
    NULL,
    NULL,
    NULL,
    NULL, NULL, "liquid_equiv_dose", "day"
  )
  expect_equal(res$tot_bottle_timeperiod, 2, tolerance = 1e-3)
  expect_equal(res$totcost_timeperiod_liquids, 4.16, tolerance = 1e-3)
})
###############################################################################
context("testing costing resource use")
test_that("testing costing resource use", {
  costs_file <- system.file("extdata", "costs_resource_use.csv", package = "packDAMipd")
  datafile <- system.file("extdata", "resource_use_hc_2.csv", package = "packDAMipd")
  ind_part_data <- load_trial_data(datafile)
  unit_cost_data <- load_trial_data(costs_file)
  # Someone admitted to in patient hospital for 1 day (nhs hospital)
  # another day not in NHS hospital
  # unit_cost_data shows inpatient hospital admission cost £20
  # another patient admitted to in patient hospital for 2 days first time,
  # then 2 days another time- so total 4 times -£80
  res <- costing_resource_use(
    ind_part_data[1, ],
    "hospital_admission_1",
    list("length_1", "length_2"),
    list("nhs_1", "nhs_2"),
    "day",
    unit_cost_data, "Inpatient hospital admissions", "UnitCost",
    "UnitUsed",
    NULL, NULL
  )

  expect_equal(res$totcost_hospital_admission_1, 20, tolerance = 1e-3)

  res <- costing_resource_use(
    ind_part_data[2, ],
    "hospital_admission_1",
    list("length_1", "length_2"),
    list("nhs_1", "nhs_2"),
    "day",
    unit_cost_data, "Inpatient hospital admissions", "UnitCost",
    "UnitUsed",
    NULL, NULL
  )
  expect_equal(res$totcost_hospital_admission_1, 80, tolerance = 1e-3)



  expect_error(costing_resource_use(
    ind_part_data[2, ],
    "hospital_admission_1",
    list("length_1", "length_2"),
    list("nhs_1"),
    "day",
    unit_cost_data, "Inpatient hospital admissions", "UnitCost",
    "UnitUsed",
    NULL, NULL
  ))

  datafile <- system.file("extdata", "resource_use_hc_2_codes.csv", package = "packDAMipd")
  ind_part_data <- load_trial_data(datafile)
  # Someone admitted to in patient hospital for 1 day (nhs hospital)
  # another day not in NHS hospital
  # unit_cost_data shows inpatient hospital admission cost £20
  # another patient admitted to in patient hospital for 2 days first time,
  # then 2 days another time- so total 4 times -£80
  # testing if we use all codes
  res <- costing_resource_use(
    ind_part_data[1, ],
    "hospital_admission_1",
    list("length_1", "length_2"),
    list("nhs_1", "nhs_2"),
    "day",
    unit_cost_data, "Inpatient hospital admissions", "UnitCost",
    "UnitUsed",
    list(c("yes", "no"), c(1, 2)), list(c("yes", "no"), c(1, 2))
  )
  expect_equal(res$totcost_hospital_admission_1, 20, tolerance = 1e-3)

  res <- costing_resource_use(
    ind_part_data[1, ],
    "daycare",
    list("number"),
    list("nhs_visit"),
    "visit",
    unit_cost_data, "Day care admission", "UnitCost",
    "UnitUsed",
    NULL, list(c("yes", "no"), c(1, 2))
  )
  expect_equal(res$totcost_daycare, 45, tolerance = 1e-3)
  expect_error(costing_resource_use(
    ind_part_data[1, ],
    "daycare",
    list("number"),
    list("visit"),
    "visit",
    unit_cost_data, "Day care admission", "UnitCost",
    "UnitUsed",
    NULL, list(c("yes", "no"), c(1, 2))
  ))
  res <- costing_resource_use(
    ind_part_data[1, ],
    "other_contact",
    list("number_of_contacts"),
    NULL, "visit",
    unit_cost_data, NULL, "UnitCost",
    "UnitUsed",
    NULL, NULL
  )
  expect_equal(res$totcost_other_contact, 39.23, tolerance = 1e-3)


  res <- costing_resource_use(
    ind_part_data[2, ],
    "other_contact",
    list("number_of_contacts"),
    NULL, "hour",
    unit_cost_data, NULL, "UnitCost",
    "UnitUsed",
    NULL, NULL
  )
  # 3 contacts of pratcise nurse- unit is per hour.
  expect_equal(res$totcost_other_contact, 126, tolerance = 1e-3)
})
