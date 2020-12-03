###############################################################################
context("testing microcosting patches")
test_that("testing microcosting patches", {
  med_costs_file <- system.file("extdata", "average_unit_costs_med.csv",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "resource_use_p.csv",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)
  example1 <- ind_part_data[1, ]
  # example 1 Buprenorphine 5nos, 20 mcg/hour patch taken once a week
  # with MED of 100
  # cost should be calculated for 1 week use
  # Buprenorphine 20 mcg/h cost is £14.37 per patch per strength
  # basis time is day
  # total patch usage per day =(5)/7 mcg/hour =0.714
  # total cost patch usage per day = (5)/7 mcg/hour * 14.37 GBP
  # per patch = 10.26
  # total cost patch usage per week = (10.26 * 7)= 71.85
  res <- microcosting_tablets_patches("patch",
    example1, "Name", "patch_strength", "patch_dose_unit", "patch_no_taken",
    "patch_frequency", "day", med_costs, "UnitCost", "StrengthUnit",
    "Strength",
    list(c("4 weeks", "1 week"), c(1, 2)),
    list(c("Buprenorphine", "Morphine"), c(1, 2)),
    NULL, NULL, "patch_equiv_dose"
  )
  expect_equal(res$totmed_basis_patches, 0.714, tolerance = 1e-3)
  expect_equal(res$totcost_basis_patches, 10.26, tolerance = 1e-3)
  expect_equal(res$totcost_timeperiod_patches, 71.825, tolerance = 1e-3)

  # example 2 Buprenorphine 7nos, 10 mcg/hour patch taken twice a day
  # with MED of 6
  # cost should be calculated for 4 weeks use
  # Buprenorphine 10 mcg/h cost is 7.89 per patch
  # basis time is day
  # total patch numbers usage per day = (7)*2  = 14
  # total cost patch usage per day = (7)*2 *7.89  GBP = 110.46
  # total cost patch usage for 4 weeks = 110.46*4*7  * = 3092.88
  # total cost patch usage for 4 weeks per MED =  3092.88/6 = 515.48
  example2 <- ind_part_data[11, ]
  res <- microcosting_tablets_patches("patches",
    example2, "Name", "patch_strength", "patch_dose_unit", "patch_no_taken",
    "patch_frequency", "day", med_costs, "UnitCost", "StrengthUnit",
    "Strength",
    list(c("4 weeks", "1 week"), c(1, 2)),
    list(c("Buprenorphine", "Morphine"), c(1, 2)),
    NULL, NULL, "patch_equiv_dose"
  )
  expect_equal(res$totmed_basis_patches, 14, tolerance = 1e-3)
  expect_equal(res$totcost_basis_patches, 110.425, tolerance = 1e-3)
  expect_equal(res$totcost_timeperiod_patches, 3091.9, tolerance = 1e-3)
  expect_equal(res$totcost_timeperiod_equiv_dose_patches, 515.48,
               tolerance = 1e-3)

  #  invalid form
  expect_error(microcosting_tablets_patches("capsule",
                                      example2, "Name", "patch_strength",
                                      "patch_dose_unit", "patch_no_taken",
                                      "patch_frequency", "day", med_costs,
                                      "UnitCost", "StrengthUnit", "Strength",
                                      list(c("4 weeks", "1 week"), c(1, 2)),
                              list(c("Buprenorphine", "Morphine"), c(1, 2)),
                                      NULL, NULL, "patch_equiv_dose"
  ))
  #  the column name "strength" not in IPD
  expect_error(microcosting_tablets_patches("patches",
    ind_part_data, "Name", "strength", "patch_dose_unit",
    "patch_no_taken",
    "patch_frequency", "day", med_costs, "UnitCost", "StrengthUnit",
    "Strength",
    list(c("4 weeks", "1 week"), c(1, 2)),
    list(c("Buprenorphine", "Morphine"), c(1, 2)),
    list(c("Once a day", "twice a day", "once weekly"), c(1, 2, 3)),
    list(c("mcg/hr", "mg/day", "mg/hr"), c(1, 2, 3)), "patch_equiv_dose"
  ))

  #  frequency is not coded, but given exactly
  expect_error(microcosting_tablets_patches("patches",
    ind_part_data, "Name", "patch_strength", "patch_dose_unit",
    "patch_no_taken",
    "patch_frequency", "day", med_costs, "UnitCost", "StrengthUnit",
    "Strength",
    list(c("4 weeks", "1 week"), c(1, 2)),
    list(c("Buprenorphine", "Morphine"), c(1, 2)),
    list(c("Once a day", "twice a day", "once weekly"), c(1, 2, 3)),
    NULL, "patch_equiv_dose"
  ))

  #  dose unit is not coded, and given exactly in IPD
  expect_error(microcosting_tablets_patches("patches",
    example2, "Name", "patch_strength", "patch_dose_unit", "patch_no_taken",
    "patch_frequency", "day", med_costs, "UnitCost", "StrengthUnit",
    "Strength",
    list(c("4 weeks", "1 week"), c(1, 2)),
    list(c("Buprenorphine", "Morphine"), c(1, 2)),
    NULL, list(c("mcg/hr", "mg/day", "mg/hr"), c(1, 2, 3)),
    "patch_equiv_dose"
  ))

  #  frequency should not be NULL
  expect_error(microcosting_tablets_patches("patches",
    ind_part_data, "Name", "patch_strength", "patch_dose_unit",
    "patch_no_taken",
    "patch_frequency", "day", med_costs, "UnitCost", "StrengthUnit", "Strength",
    NULL, list(c("Buprenorphine", "Morphine"), c(1, 2)),
    list(c("Once a day", "twice a day", "once weekly"), c(1, 2, 3)),
    list(c("mcg/hr", "mg/day", "mg/hr"), c(1, 2, 3)), "patch_equiv_dose"
  ))
  #  column with name "dose" not in unit cost data
  expect_error(microcosting_tablets_patches("patches",
    ind_part_data, "Name", "patch_strength", "patch_dose_unit",
    "patch_no_taken",
    "patch_frequency", "hour", med_costs, "UnitCost", "StrengthUnit", "dose",
    list(c("4 weeks", "1 week"), c(1, 2)),
    list(c("Buprenorphine", "Morphine"), c(1, 2)),
    list(c("Once a day", "twice a day", "once weekly"), c(1, 2, 3)),
    list(c("mcg/hr", "mg/day", "mg/hr"), c(1, 2, 3)), "patch_equiv_dose"
  ))

  # list of codes and names can be  NA, but the unit cost should be unique
  expect_error(microcosting_tablets_patches("patches",
                        ind_part_data, "Name", "patch_strength",
                        "patch_dose_unit", "patch_no_taken",
                        "patch_frequency", "day", med_costs, "UnitCost",
                        "StrengthUnit", "Strength",
                        list(c("4 weeks", "1 week"), c(1, 2)), NA,
                         NULL, NULL, "patch_equiv_dose"
  ))

  error_costs_file <- system.file("extdata", "costs_error.csv",
                                  package = "packDAMipd")
  datafile <- system.file("extdata", "resource_use_patches.csv",
                          package = "packDAMipd")
  ind_part_data <- load_trial_data(datafile)
  med_costs <- load_trial_data(error_costs_file)
  # cost file in wrong format - form of medication column not exists
  expect_error(microcosting_tablets_patches("patches",
    ind_part_data, "Name", "patch_strength", "patch_dose_unit",
    "patch_no_taken",
    "patch_frequency", "day", med_costs, "UnitCost", "StrengthUnit",
    "Strength",
    list(c("4 weeks", "1 week"), c(1, 2)),
    list(c("Buprenorphine", "Morphine"), c(1, 2)),
    list(c("Once a day", "twice a day", "once weekly"), c(1, 2, 3)),
    list(c("mcg/hr", "mg/day", "mg/hr"), c(1, 2, 3)), "patch_equiv_dose"
  ))

  # null ind_part_data
  expect_error(microcosting_tablets_patches("patches",
    NULL, "Name", "patch_strength", "patch_dose_unit", "patch_no_taken",
    "patch_frequency", "day", med_costs, "UnitCost",
    "StrengthUnit", "Strength",
    list(c("4 weeks", "1 week"), c(1, 2)),
    list(c("Buprenorphine", "Morphine"), c(1, 2)),
    NULL, NULL, "patch_equiv_dose"
  ))

  # null name of medication
  expect_error(microcosting_tablets_patches("patches",
    ind_part_data, NULL, "patch_strength", "patch_dose_unit",
    "patch_no_taken", "patch_frequency", "day", med_costs, "UnitCost",
    "StrengthUnit", "Strength", list(c("4 weeks", "1 week"), c(1, 2)),
    list(c("Buprenorphine", "Morphine"), c(1, 2)),
    NULL, NULL, "patch_equiv_dose"
  ))

  # NA unit cost column
  expect_error(microcosting_tablets_patches("patches",
    ind_part_data, "Name", "patch_strength", "patch_dose_unit",
    "patch_no_taken",
    "patch_frequency", "day", med_costs, NA, "StrengthUnit",
    "Strength",
    list(c("4 weeks", "1 week"), c(1, 2)),
    list(c("Buprenorphine", "Morphine"), c(1, 2)),
    NULL, NULL, "patch_equiv_dose"
  ))

  noname_med_file <- system.file("extdata",
                                 "average_unit_costs_med_nodrugname.csv",
                                 package = "packDAMipd")
  med_costs <- load_trial_data(noname_med_file)
  #no name of medication in costs file
  expect_error(microcosting_tablets_patches("patches",
                            ind_part_data, "Name", "patch_strength",
                            "patch_dose_unit", "patch_no_taken",
                            "patch_frequency", "day", med_costs, "UnitCost",
                            "StrengthUnit", "Strength",
                            list(c("4 weeks", "1 week"), c(1, 2)),
                            list(c("Buprenorphine", "Morphine"), c(1, 2)),
                            NULL, NULL, "patch_equiv_dose"))

  nullcol_medname_file <- system.file("extdata",
                                "average_unit_costs_med_nullcolmedname.csv",
                                 package = "packDAMipd")
  med_costs <- load_trial_data(nullcol_medname_file)
  data_file_nmedcode <- system.file("extdata", "resource_use_p_nomedcode.csv",
                                    package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file_nmedcode)
  #no name of medication in costs file
  expect_error(microcosting_tablets_patches("patches",
                                            ind_part_data, "Name",
                                            "patch_strength",
                                            "patch_dose_unit",
                                            "patch_no_taken",
                                            "patch_frequency", "day",
                                            med_costs, "UnitCost",
                                            "StrengthUnit", "Strength",
                                            list(c("4 weeks", "1 week"),
                                                 c(1, 2)),
                                            NULL, NULL, NULL,
                                            "patch_equiv_dose"))


  data_file_nmedcode <- system.file("extdata", "resource_use_p_notpcode.csv",
                                    package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file_nmedcode)
  #no period desc in individual data
  expect_error(microcosting_tablets_patches("patches",
                                            ind_part_data, "Name",
                                            "patch_strength",
                                            "patch_dose_unit",
                                            "patch_no_taken",
                                            "patch_frequency", "day",
                                            med_costs, "UnitCost",
                                            "StrengthUnit", "Strength",
                                            NULL, NULL, NULL, NULL,
                                            "patch_equiv_dose"))

  med_costs_file <- system.file("extdata", "average_unit_costs_med.csv",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "resource_use_p_tpgiven.csv",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)
  example1 <- ind_part_data[1, ]
  res <- microcosting_tablets_patches("patch",
                                      example1, "Name", "patch_strength",
                                      "patch_dose_unit", "patch_no_taken",
                                      "patch_frequency", "day", med_costs,
                                      "UnitCost", "StrengthUnit", "Strength",
                                      NULL,
                                      list(c("Buprenorphine", "Morphine"),
                                                 c(1, 2)),
                                      NULL, NULL, "patch_equiv_dose"
  )
  expect_equal(res$totmed_basis_patches, 0.7142857)
  expect_equal(res$totcost_timeperiod_patches, 143.7)
  # list of code and time points can not be NA
  expect_error(microcosting_tablets_patches("patch",
                                      example1, "Name", "patch_strength",
                                      "patch_dose_unit", "patch_no_taken",
                                      "patch_frequency", "day", med_costs,
                                      "UnitCost", "StrengthUnit", "Strength",
                                      NA, list(c("Buprenorphine", "Morphine"),
                                               c(1, 2)),
                                      NULL, NULL, "patch_equiv_dose"))


  med_costs_file <- system.file("extdata", "average_unit_costs_med.csv",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "resource_use_p_dosage_notincosting.csv",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)
  expect_error(microcosting_tablets_patches("patches",
                               ind_part_data[1, ], "Drug", "patch_strength",
                               "patch_dose_unit",
                               "patch_no_taken",
                               "patch_frequency", "day", med_costs, "UnitCost",
                                "StrengthUnit", "Strength",
                               list(c("4 weeks", "1 week"), c(1, 2)),
                                NULL, NULL,
                                NULL, "patch_equiv_dose"))
  # patch dosage not in costing
  expect_error(microcosting_tablets_patches("patches",
                                            ind_part_data[2, ], "Drug",
                                            "patch_strength", "patch_dose_unit",
                                            "patch_no_taken",
                                            "patch_frequency", "day",
                                            med_costs, "UnitCost",
                                            "StrengthUnit", "Strength",
                                            list(c("4 weeks", "1 week"),
                                                 c(1, 2)),
                                            NULL, NULL,
                                            NULL, "patch_equiv_dose"))

})

###############################################################################
context("testing microcosting tablets")
test_that("testing microcosting tablets", {
  med_costs_file <- system.file("extdata", "average_unit_costs_med.csv",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "resource_use_t.csv",
                           package = "packDAMipd")
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
  res <- microcosting_tablets_patches("tablet",
    example1, "Drug", "tab_dosage", "tab_dosage_unit", "tab_no_taken",
    "tab_frequency", "day", med_costs, "UnitCost",
    "StrengthUnit", "Strength", list(c("4 weeks", "1 week"), c(1, 2)),
    NULL, this_list,
    list(c("mcg", "mg", "gm"), c(1, 2, 3)), "tab_equiv_dose"
  )

  expect_equal(res$totmed_basis_tablets, 1, tolerance = 1e-3)
  expect_equal(res$totcost_basis_tablets, 2.52, tolerance = 1e-3)
  expect_equal(res$totcost_timeperiod_tablets, 17.64, tolerance = 1e-3)
  expect_equal(res$totcost_timeperiod_equiv_dose_tablets, 0.441,
               tolerance = 1e-3)

  data_file <- system.file("extdata", "resource_use_tandp.csv",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  res <- microcosting_tablets_patches("tablet",
                                      ind_part_data, "Drug", "tab_dosage",
                                      "tab_dosage_unit", "tab_no_taken",
                                      "tab_frequency", "day", med_costs,
                                      "UnitCost",
                                      "StrengthUnit", "Strength",
                                      list(c("4 weeks", "1 week"), c(1, 2)),
                                      NULL, this_list,
                                      list(c("mcg", "mg", "gm"), c(1, 2, 3)),
                                      "tab_equiv_dose"
  )
  expect_equal(res$totmed_basis_tablets, c(1, 6, 12, 24, NA, NA, NA),
               tolerance = 1e-3)

  #equivalent dose NULL no issues
  res <- microcosting_tablets_patches("tablet",
                                      example1, "Drug", "tab_dosage",
                                      "tab_dosage_unit", "tab_no_taken",
                                      "tab_frequency", "day", med_costs,
                                      "UnitCost",
                                      "StrengthUnit", "Strength",
                                      list(c("4 weeks", "1 week"), c(1, 2)),
                                      NULL, this_list,
                                      list(c("mcg", "mg", "gm"), c(1, 2, 3)),
                                      NULL
  )

  expect_equal(res$totmed_basis_tablets, 1, tolerance = 1e-3)

  # Error -
  # frequency code is in the IPD but not given while function call
  expect_error(microcosting_tablets_patches("tablets",
    example1, "Drug", "tab_dosage", "tab_dosage_unit", "tab_no_taken",
    "tab_frequency", "day", med_costs, "UnitCost",
    "StrengthUnit", "Strength", list(c("4 weeks", "1 week"), c(1, 2)),
    NULL, NULL,
    list(c("mcg", "mg", "gm"), c(1, 2, 3)), NULL
  ))
  # Error - dose unit is null
  expect_error(microcosting_tablets_patches("tablet",
    example1, "Drug", "tab_dosage", NULL, "tab_no_taken",
    "tab_frequency", "day", med_costs, "UnitCost",
    "StrengthUnit", "Strength", list(c("4 weeks", "1 week"), c(1, 2)),
    NULL, this_list,
    list(c("mcg", "mg", "gm"), c(1, 2, 3)), "tab_equiv_dose"
  ))
  # Error - no taken is null
  expect_error(microcosting_tablets_patches("tablet",
    example1, "Drug", "tab_dosage", "tab_dosage_unit", NULL,
    "tab_frequency", "day", med_costs, "UnitCost",
    "StrengthUnit", "Strength", list(c("4 weeks", "1 week"), c(1, 2)),
    NULL, this_list,
    list(c("mcg", "mg", "gm"), c(1, 2, 3)), "tab_equiv_dose"
  ))
  data_file <- system.file("extdata", "resource_use_t_wtnotincosting.csv",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  # used dosage is not in costing table
  expect_error(microcosting_tablets_patches("tablet",
                    ind_part_data, "Drug", "tab_dosage", "tab_dosage_unit",
                    "tab_no_taken",
                     "tab_frequency", "day", med_costs, "UnitCost",
                      "StrengthUnit", "Strength", list(c("4 weeks", "1 week"),
                                                       c(1, 2)),
                      NULL, this_list,
                      NULL, "tab_equiv_dose")
  )
  med_costs_file <- system.file("extdata",
                "average_unit_costs_med_incorrect_unit_tablet.csv",
                package = "packDAMipd")
  data_file <- system.file("extdata", "resource_use_t.csv",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)
  # used dosage is not in costing table
  expect_error(microcosting_tablets_patches("tablets",
                                            ind_part_data, "Drug",
                                            "patch_dosage", "tab_dosage_unit",
                                            "tab_no_taken",
                                            "tab_frequency", "day", med_costs,
                                            "UnitCost",
                                            "StrengthUnit", "Strength",
                                            list(c("4 weeks", "1 week"),
                                                 c(1, 2)),
                                            NULL, this_list,
                                            NULL, "tab_equiv_dose"))


})
###############################################################################

context("testing microcosting liquids")
test_that("testing microcosting liquids", {
  med_costs_file <- system.file("extdata", "average_unit_costs_med.csv",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "resource_use_l.csv",
                           package = "packDAMipd")
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
    med_costs, "UnitCost", "SizeUnit", "Strength", NULL,
     NULL, NULL, NULL, NULL, "liquid_equiv_dose", "day")

  expect_equal(res$tot_timeperiod_bottle, 2, tolerance = 1e-3)
  expect_equal(res$totcost_timeperiod_liquids, 4.16, tolerance = 1e-3)
  # no equivalent dose given - no issues
  res <- microcosting_liquids(
    example1, "Drug", "liq_dosage", "liquid_dose_unit",
    "liquid_bottle_size", "liquid_bottle_remain_time", NULL,
    med_costs, "UnitCost", "SizeUnit", "Strength", NULL,
    NULL, NULL, NULL, NULL, NULL, "day")

  expect_equal(res$tot_timeperiod_bottle, 2, tolerance = 1e-3)
  # NULL data
  expect_error(microcosting_liquids(
    NULL, "Drug", "liq_dosage", "liquid_dose_unit",
    "liquid_bottle_size", "liquid_bottle_remain_time", NULL,
    med_costs, "UnitCost", "SizeUnit", "Strength", NULL,
    NULL, NULL, NULL, NULL, "liquid_equiv_dose", "day"))

  # NULL name medicine
  expect_error(microcosting_liquids(
    example1, NULL, "liq_dosage", "liquid_dose_unit",
    "liquid_bottle_size", "liquid_bottle_remain_time", NULL,
    med_costs, "UnitCost", "SizeUnit", "Strength", NULL,
    NULL, NULL, NULL, NULL, "liquid_equiv_dose", "day"))

  # name medicine col not in cost data
  med_costs_file <- system.file("extdata",
                                "average_unit_costs_med_nodrugname.csv",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "resource_use_l.csv",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)

  expect_error(microcosting_liquids(
    ind_part_data, "Drug", "liq_dosage", "liquid_dose_unit",
    "liquid_bottle_size", "liquid_bottle_remain_time", NULL,
    med_costs, "UnitCost", "SizeUnit", "Strength", NULL, NULL,
    list(c("Buprenorphine", "Morphine"), c(1, 2)),
    NULL, NULL, "liquid_equiv_dose", "day"))


  # form medicine col not in cost data
  med_costs_file <- system.file("extdata", "average_unit_costs_med_noform.csv",
                                package = "packDAMipd")
  med_costs <- load_trial_data(med_costs_file)
  expect_error(microcosting_liquids(
    ind_part_data, "Drug", "liq_dosage", "liquid_dose_unit",
    "liquid_bottle_size", "liquid_bottle_remain_time", NULL,
    med_costs, "UnitCost", "SizeUnit", "Strength", NULL, NULL,
    list(c("Buprenorphine", "Morphine"), c(1, 2)), NULL, NULL,
    "liquid_equiv_dose", "day"))

  med_costs_file <- system.file("extdata", "average_unit_costs_med.csv",
                                package = "packDAMipd")
  med_costs <- load_trial_data(med_costs_file)
  data_file <- system.file("extdata", "resource_use_l2.csv",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  result <- microcosting_liquids(
    ind_part_data[3, ], "Name", "liq_dosage", "liquid_dose_unit",
    "liquid_bottle_size", "liquid_bottle_remain_time", NULL,
    med_costs, "UnitCost", "SizeUnit", "Strength",
    NULL, NULL, list(c("Buprenorphine", "Morphine"), c(1, 2)),
    NULL, NULL, "liquid_equiv_dose", "day")

  expect_equal(result$totcost_timeperiod_liquids, 1423.5)
  # preparation not null
  data_file <- system.file("extdata", "resource_use_l_prep.csv",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  result <- microcosting_liquids(
    ind_part_data[3, ], "Name", "liq_dosage", "liquid_dose_unit",
    "liquid_bottle_size", "liquid_bottle_remain_time", NULL,
    med_costs, "UnitCost", "SizeUnit", "Strength",
    NULL, "Preparation", list(c("Buprenorphine", "Morphine"), c(1, 2)),
    NULL, NULL, "liquid_equiv_dose", "day")
  expect_equal(result$totcost_timeperiod_liquids, 1423.5)

  # name from code not null
  data_file <- system.file("extdata", "resource_use_l_noname.csv",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  expect_error(microcosting_liquids(
    ind_part_data[3, ], "Name", "liq_dosage", "liquid_dose_unit",
    "liquid_bottle_size", "liquid_bottle_remain_time", NULL,
    med_costs, "UnitCost", "SizeUnit", "Strength",
    NULL, "Preparation", list(c("Buprenorphine", "Morphine"), c(1, 2)),
    NULL, NULL, "liquid_equiv_dose", "day"))

  data_file <- system.file("extdata", "resource_use_l_bottlesize.csv",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  expect_error(microcosting_liquids(
    ind_part_data[3, ], "Name", "liq_dosage", "liquid_dose_unit",
    "liquid_bottle_size", "liquid_bottle_remain_time", NULL,
    med_costs, "UnitCost", "SizeUnit", "Strength",
    NULL, NULL, list(c("Buprenorphine", "Morphine"), c(1, 2)),
    NULL, list(c("100ml", "300ml", "120ml"), c(1, 2, 3)),
    "liquid_equiv_dose", "day"))
  data_file <- system.file("extdata", "resource_use_l_bottlesize_unitsep.csv",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)

  microcosting_liquids(
    ind_part_data[3, ], "Name", "liq_dosage", "liquid_dose_unit",
    "liquid_bottle_size", "liquid_bottle_remain_time", "bottle_size_unit",
    med_costs, "UnitCost", "SizeUnit", "Strength",
    NULL, NULL, list(c("Buprenorphine", "Morphine"), c(1, 2)),
    NULL, NULL,
    "liquid_equiv_dose", "day")
   expect_equal(result$totcost_timeperiod_liquids, 1423.5)

  data_file <- system.file("extdata", "resource_use_l_doseunit.csv",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  expect_error(microcosting_liquids(
    ind_part_data[3, ], "Name", "liq_dosage", "liquid_dose_unit",
    "liquid_bottle_size", "liquid_bottle_remain_time", NULL,
    med_costs, "UnitCost", "SizeUnit", "Strength",
    NULL, NULL, list(c("Buprenorphine", "Morphine"), c(1, 2)),
    list(c("mg/ml", "g/l"), c(1, 2)), NULL,
    "liquid_equiv_dose", "day"))

  data_file <- system.file("extdata", "resource_use_l_timeperiod.csv",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  expect_error(microcosting_liquids(
    ind_part_data[1, ], "Name", "liq_dosage", "liquid_dose_unit",
    "liquid_bottle_size", "liquid_bottle_remain_time", NULL,
    med_costs, "UnitCost", "SizeUnit", "Strength",
    list(c("4 weeks", "2 months"), c(1, 2)), NULL,
    list(c("Buprenorphine", "Morphine"), c(1, 2)),
    NULL, NULL,
    "liquid_equiv_dose", "day"))

  # unit of costing not unique
  med_costs_file <- system.file("extdata",
                                "average_unit_costs_med_unitnotunique.csv",
                                package = "packDAMipd")
  med_costs <- load_trial_data(med_costs_file)
  data_file <- system.file("extdata", "resource_use_l.csv",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  expect_error(microcosting_liquids(
    ind_part_data, "Name", "liq_dosage", "liquid_dose_unit",
    "liquid_bottle_size", "liquid_bottle_remain_time", NULL,
    med_costs, "UnitCost", "SizeUnit", "Strength",
    NULL, NULL,
    list(c("Buprenorphine", "Morphine"), c(1, 2)),
    NULL, NULL,
    "liquid_equiv_dose", "day"))

  # dosage not in unit costs
  med_costs_file <- system.file("extdata",
                    "average_unit_costs_med_l_dosagenotin_unitcosts.csv",
                    package = "packDAMipd")
  med_costs <- load_trial_data(med_costs_file)
  data_file <- system.file("extdata", "resource_use_l.csv",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  expect_error(microcosting_liquids(
    ind_part_data, "Name", "liq_dosage", "liquid_dose_unit",
    "liquid_bottle_size", "liquid_bottle_remain_time", NULL,
    med_costs, "UnitCost", "SizeUnit", "Strength",
    NULL, NULL,
    list(c("Buprenorphine", "Morphine"), c(1, 2)),
    NULL, NULL,
    "liquid_equiv_dose", "day"))

  # all including liquid, tablet, patches
  med_costs_file <- system.file("extdata", "average_unit_costs_med.csv",
                                package = "packDAMipd")
  med_costs <- load_trial_data(med_costs_file)
  data_file <- system.file("extdata", "resource_use_l_t_p.csv",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  res <- microcosting_liquids(
    ind_part_data, "Name", "liq_dosage", "liquid_dose_unit",
    "liquid_bottle_size", "liquid_bottle_remain_time", NULL,
    med_costs, "UnitCost", "SizeUnit", "Strength",
    list(c("4 weeks", "2 months"), c(1, 2)), NULL,
    list(c("Buprenorphine", "Morphine"), c(1, 2)),
    NULL, NULL,
    "liquid_equiv_dose", "day")
  expect_equal(res$totcost_timeperiod_liquids, c(10.40, 13.14, 234.00, NA,
                                                 NA, NA, NA, NA, NA, NA))

})
