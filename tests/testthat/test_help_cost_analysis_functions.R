###############################################################################
context("testing checking null or NA")
test_that("testing checking null or NA", {
  expect_equal(check_null_na(NULL), -1)
  expect_equal(check_null_na(NA), -2)
  expect_equal(check_null_na("not null"), 0)

})
###############################################################################
context("testing converting frequency to different basis")
test_that("testing converting frequency to different basis", {
  expect_equal(convert_freq_diff_basis("once daily"),1)
  expect_equal(convert_freq_diff_basis("once weekly"),1/7)
  expect_equal(convert_freq_diff_basis("twice a day", "week"),14)
  expect_error(convert_freq_diff_basis("twiceday", "week"))
})
###############################################################################
context("testing converting unit to different basis")
test_that("testing converting unit to different basis", {
  expect_equal(convert_unit_diff_basis("mg"),1)
  expect_equal(convert_unit_diff_basis("gm"),1000)
  expect_equal(convert_unit_diff_basis("l", "ml"),1000)
  expect_error(convert_unit_diff_basis("twiceday", "mg"))
})
