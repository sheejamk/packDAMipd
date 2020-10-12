###############################################################################
context("testing checking null or NA")
test_that("testing checking null or NA", {
  expect_equal(check_null_na(NULL), -1)
  expect_equal(check_null_na(NA), -2)
  expect_equal(check_null_na("not null"), 0)

})
###############################################################################
context("testing ocnverting from word to number")
test_that("testing ocnverting from word to number", {
  expect_equal(as.numeric(unlist(word2num("ninety eight"))[2]), 98)
  expect_equal(as.numeric(unlist( word2num("one hundred and eight"))[2]), 108)

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
  expect_equal(convert_weight_diff_basis("mg"),1)
  expect_equal(convert_weight_diff_basis("gm"),1000)
  expect_error(convert_weight_diff_basis("l", "ml"))
  expect_error(convert_weight_diff_basis("twiceday", "mg"))
})
###############################################################################
context("testing converting volume to different basis")
test_that("testing converting volume to different basis", {
  expect_equal(convert_volume_basis("ml"),1)
  expect_equal(convert_volume_basis("l"),1000)
  expect_equal(convert_volume_basis("l", "ml"),1000)
  expect_error(convert_volume_basis("mg"))
})
###############################################################################
context("testing converting weight per time to different basis")
test_that("testing converting weight per time to different basis", {
  expect_equal(convert_wtpertimediff_basis("mg/day"),41.6667, tol = 1e-3)
  expect_equal(convert_wtpertimediff_basis("mcg/hr"),1)
  expect_equal(convert_wtpertimediff_basis("mg/day", "mcg/day"),1000)
  expect_error(convert_wtpertimediff_basis("mg"))
})
###############################################################################
context("testing converting to different time period")
test_that("testing converting to given time period", {
  expect_equal(convert_to_given_timeperiod("4 weeks"),28)
  expect_equal(convert_to_given_timeperiod("2 months"),60)
  expect_equal(convert_to_given_timeperiod("1 year", "day"),365)
  expect_error(convert_to_given_timeperiod("mg"))
})
