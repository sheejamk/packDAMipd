# med_costs_file = system.file("extdata", "average_unit_costs_med.csv", package = "packDAMipd")
# data_file = system.file("extdata", "resource_use_p.csv", package = "packDAMipd")
# ind_part_data = load_trial_data(data_file)
# med_costs = load_trial_data(med_costs_file)
#
# ind_part_data = ind_part_data
# name_med = "Name"
# dose_med = "patch_strength"
# dose_unit = "patch_dose_unit"
# no_taken = "patch_no_taken"
# freq_taken = "patch_frequency"
# basis_time =  "day"
# unit_cost_data =  med_costs
# unit_cost_column =  "UnitCost"
# cost_calculated_in = "StrengthUnit"
# strength_expressed_in = "Strength"
# list_period_timepoint = list(c("4 weeks", "1 week"), c(1,2))
# list_of_code_names = list(c("Buprenorphine", "Morphine"), c(1,2))
# list_of_code_freq = NULL
# list_of_code_dose_unit = NULL
# equiv_dose = "patch_equiv_dose"
#
# res = microcosting_patches(ind_part_data,"Name", "patch_strength", "patch_dose_unit","patch_no_taken",
#                            "patch_frequency",  "day", med_costs, "UnitCost", "StrengthUnit","Strength",
#                            list(c("4 weeks", "1 week"), c(1,2)), list(c("Buprenorphine", "Morphine"), c(1,2)),
#                            NULL, NULL, "patch_equiv_dose")
# res
