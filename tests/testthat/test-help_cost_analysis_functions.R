
###############################################################################
context("testing ocnverting from word to number")
test_that("testing ocnverting from word to number", {
  expect_equal(as.numeric(unlist(word2num("ninety eight"))[2]), 98)
  expect_equal(as.numeric(unlist(word2num("one hundred and eight"))[2]), 108)
  expect_equal(as.numeric(unlist(word2num("hundred and eight"))[2]), 108)
  answer <- word2num("thousand two hundred and eight")
  expect_equal(as.numeric(unlist(answer)[2]), 1208)
  expect_equal(as.numeric(unlist(word2num("seventeen"))[2]), 17)
  answer <- word2num("two thousand two hundred and eight")
  expect_equal(as.numeric(unlist(answer)[2]), 2208)
  answer <- word2num("five thousand two hundred and eight")
  expect_equal(as.numeric(unlist(answer)[2]), 5208)
  answer <- word2num("five thousand two hundred and forty one")
  expect_equal(as.numeric(unlist(answer)[2]), 5241)
  answer <- word2num("five hundred four thousand and forty one")
  expect_equal(as.numeric(unlist(answer)[2]), 4541)
  answer <- word2num("forty one and five hundred")
  expect_equal(as.numeric(unlist(answer)[2]), 4600)
  answer <- word2num("one forty one")
  expect_equal(as.numeric(unlist(answer)[2]), 141)
})
###############################################################################
context("testing converting frequency to different basis")
test_that("testing converting frequency to different basis", {
  expect_equal(convert_freq_diff_basis("once daily"), 1)
  expect_equal(convert_freq_diff_basis("once weekly"), 1 / 7)
  expect_equal(convert_freq_diff_basis("twice a day", "week"), 14)
  expect_equal(convert_freq_diff_basis("twiceday", "week"), NA)
  expect_error(convert_freq_diff_basis(NULL, "day"))
  expect_equal(convert_freq_diff_basis("tid", "day"), 3)
  expect_equal(convert_freq_diff_basis("four daily", "day"), 4)
  expect_equal(convert_freq_diff_basis("five times daily", "day"), 5)
  expect_equal(convert_freq_diff_basis("six times in a day", "day"), 6)
  expect_equal(convert_freq_diff_basis("seven a day", "day"), 7)
  expect_equal(convert_freq_diff_basis("eight times a day", "day"), 8)
  expect_equal(convert_freq_diff_basis("nine times a day", "day"), 9)
  expect_equal(convert_freq_diff_basis("ten times a day", "day"), 10)
  expect_equal(convert_freq_diff_basis("eleven times a day", "day"), 11)
  expect_equal(convert_freq_diff_basis("twelve times a day", "day"), 12)
  expect_equal(convert_freq_diff_basis("every 2 days", "day"), 0.5)
  expect_equal(convert_freq_diff_basis("every 3 days", "day"), 1 / 3)
  expect_equal(convert_freq_diff_basis("every 4 days", "day"), 1 / 4)
  expect_equal(convert_freq_diff_basis("every 5 days", "day"), 1 / 5)
  expect_equal(convert_freq_diff_basis("every 6 days", "day"), 1 / 6)
  expect_equal(convert_freq_diff_basis("every 7 days", "day"), 1 / 7)
  expect_equal(convert_freq_diff_basis("twice a week", "day"), 2 / 7)
  expect_equal(convert_freq_diff_basis("thrice a week", "day"), 3 / 7)
  expect_equal(convert_freq_diff_basis("four times a week", "day"), 4 / 7)
  expect_equal(convert_freq_diff_basis("five times a week", "day"), 5 / 7)
  expect_equal(convert_freq_diff_basis("six times a week", "day"), 6 / 7)
  expect_equal(convert_freq_diff_basis("weekly seven times", "day"),  1)
  expect_equal(convert_freq_diff_basis("every hour", "day"),  24)
  expect_equal(convert_freq_diff_basis("every two hours", "day"),  12)
  expect_equal(convert_freq_diff_basis("every 3 hours", "day"),  8)
  expect_equal(convert_freq_diff_basis("every four hour", "day"),  6)
  expect_equal(convert_freq_diff_basis("every six hours", "day"),  4)
  expect_equal(convert_freq_diff_basis("every 12 hours", "day"),  2)
  expect_error(convert_freq_diff_basis("every 12 hours", "time"))
  expect_error(convert_freq_diff_basis(NULL, "day"))
  expect_equal(convert_freq_diff_basis(NA, "day"), NA)
  expect_equal(convert_freq_diff_basis("every 12 hours", "hour"), 1 / 12)
  expect_equal(convert_freq_diff_basis("every week", "month"), 4.2857,
               tol = 1e-4)
  expect_equal(convert_freq_diff_basis("once a day", "week"), 7)
  expect_equal(convert_freq_diff_basis("once a day", "year"), 365)
})

###############################################################################
context("testing converting unit to different basis")
test_that("testing converting unit to different basis", {
  expect_equal(convert_weight_diff_basis("mg"), 1)
  expect_equal(convert_weight_diff_basis("gm"), 1000)
  expect_error(convert_weight_diff_basis("l", "ml"))
  expect_error(convert_weight_diff_basis("twiceday", "mg"))
  expect_equal(convert_weight_diff_basis("mg", "mcg"), 1000)
  expect_equal(convert_weight_diff_basis("gm", "mcg"), 1e6)
  expect_equal(convert_weight_diff_basis("kg", "mcg"), 1e9)
  expect_equal(convert_weight_diff_basis("mcg", "mcg"), 1)
  expect_equal(convert_weight_diff_basis("kg", "mg"), 1e6)
  expect_equal(convert_weight_diff_basis("mcg", "mg"), 0.001)
  expect_equal(convert_weight_diff_basis("mcg", "mg"), 0.001)
  expect_equal(convert_weight_diff_basis("mg", "gm"), 0.001)
  expect_equal(convert_weight_diff_basis("g", "gm"), 1)
  expect_equal(convert_weight_diff_basis("kg", "gm"), 1e3)
  expect_equal(convert_weight_diff_basis("mcg", "gm"), 1e-6)
  expect_equal(convert_weight_diff_basis("mg", "kg"), 1e-6)
  expect_equal(convert_weight_diff_basis("g", "kg"), 1e-3)
  expect_equal(convert_weight_diff_basis("kg", "kg"), 1)
  expect_equal(convert_weight_diff_basis("mcg", "kg"), 1e-9)
  expect_error(convert_weight_diff_basis(NULL, "kg"))
  expect_error(convert_weight_diff_basis("NULL", "kg"))
})
###############################################################################
context("testing converting volume to different basis")
test_that("testing converting volume to different basis", {
  expect_equal(convert_volume_basis("ml"), 1)
  expect_equal(convert_volume_basis("l"), 1000)
  expect_equal(convert_volume_basis("l", "ml"), 1000)
  expect_error(convert_volume_basis("mg"))
  expect_equal(convert_volume_basis("mcl", "ml"), 1 / 1000)
  expect_equal(convert_volume_basis("l", "mcl"), 1e6)
  expect_equal(convert_volume_basis("ml", "mcl"), 1000)
  expect_equal(convert_volume_basis("ml", "l"), 1 / 1000)
  expect_equal(convert_volume_basis("mcl", "l"), 1 / 1e6)
  expect_error(convert_volume_basis(NULL, "l"))
})
###############################################################################
context("testing converting weight per time to different basis")
test_that("testing converting weight per time to different basis", {
  expect_equal(convert_wtpertimediff_basis("mg/day"), 41.6667, tol = 1e-3)
  expect_equal(convert_wtpertimediff_basis("mcg/hr"), 1)
  expect_equal(convert_wtpertimediff_basis("mg/day", "mcg/day"), 1000)
  expect_error(convert_wtpertimediff_basis("mg"))
  expect_error(convert_wtpertimediff_basis(NULL))
  expect_equal(convert_wtpertimediff_basis("mcg/day", "mg/day"), 1 / 1000)
  expect_equal(convert_wtpertimediff_basis("gm/day", "mg/day"), 1000)
  expect_equal(convert_wtpertimediff_basis("mg/day", "mg/day"), 1)
  expect_equal(convert_wtpertimediff_basis("gm/day", "mcg/day"), 1e6)
  expect_equal(convert_wtpertimediff_basis("mg/day", "gm/day"), 1 / 1000)
  expect_equal(convert_wtpertimediff_basis("mcg/day", "gm/day"), 1 / 1e6)
  expect_equal(convert_wtpertimediff_basis("gm/day", "gm/day"), 1)
  expect_equal(convert_wtpertimediff_basis("mcg/hour", "mcg/day"), 24)
  expect_equal(convert_wtpertimediff_basis("mcg/sec", "mcg/day"),
               (24 * 3600))
  expect_equal(convert_wtpertimediff_basis("mcg/minute", "mcg/day"),
               (24 * 60))
  expect_equal(convert_wtpertimediff_basis("mcg/sec", "mcg/hour"),
               (3600))
  expect_equal(convert_wtpertimediff_basis("mcg/minute", "mcg/hour"),
               (60))
  expect_equal(convert_wtpertimediff_basis("mcg/d", "mcg/min"),
               1 / (24 * 60))
  expect_equal(convert_wtpertimediff_basis("mcg/sec", "mcg/minute"),
                60)
  expect_equal(convert_wtpertimediff_basis("mcg/hour", "mcg/minute"),
               1 / 60)
  expect_equal(convert_wtpertimediff_basis("mcg/minute", "mcg/minute"),
               1)
  expect_equal(convert_wtpertimediff_basis("mcg/day", "mcg/sec"),
               1 / (24 * 3600))
  expect_equal(convert_wtpertimediff_basis("mcg/minute", "mcg/sec"),
               1 / 60)
  expect_equal(convert_wtpertimediff_basis("mcg/hour", "mcg/sec"),
               1 / 3600)
  expect_equal(convert_wtpertimediff_basis("mcg/sec", "mcg/sec"),
               1)
  expect_equal(convert_wtpertimediff_basis(""), NA)

})
###############################################################################
context("testing converting to different time period")
test_that("testing converting to given time period", {
  expect_equal(convert_to_given_timeperiod("4 weeks"), 28)
  expect_equal(convert_to_given_timeperiod("2 months"), 60)
  expect_equal(convert_to_given_timeperiod("1 year", "day"), 365)
  expect_error(convert_to_given_timeperiod("mg"))
  expect_equal(convert_to_given_timeperiod("an year"), 365)
  expect_error(convert_to_given_timeperiod("an year", NULL))
  expect_error(convert_to_given_timeperiod(NULL))
  expect_equal(convert_to_given_timeperiod(""), NA)
  expect_error(convert_to_given_timeperiod("4 "))
  expect_error(convert_to_given_timeperiod("4 null"))
  expect_equal(convert_to_given_timeperiod("a day"), 1)
  expect_equal(convert_to_given_timeperiod("an hour"), 1 / 24)
  expect_equal(convert_to_given_timeperiod("one second"), 1 / (24 * 60 * 60))
  expect_equal(convert_to_given_timeperiod("ten minutes"), 10 / (24 * 60))
  expect_equal(convert_to_given_timeperiod("ten months"), 300)
  expect_equal(convert_to_given_timeperiod("an year", "month"), 12)
  expect_equal(convert_to_given_timeperiod("ten months", "month"), 10)
  expect_equal(convert_to_given_timeperiod("ten weeks", "month"), 10 / 4)
  expect_equal(convert_to_given_timeperiod("ten days", "month"), 10 / 30)
  expect_equal(convert_to_given_timeperiod("ten hours", "month"),
               10 / (30 * 24))
  expect_equal(convert_to_given_timeperiod("five minute", "month"),
               5 / (30 * 24 * 60))
  expect_equal(convert_to_given_timeperiod("five seconds", "month"),
               5 / (30 * 24 * 3600))
  expect_equal(convert_to_given_timeperiod("five weeks", "week"), 5)
  expect_equal(convert_to_given_timeperiod("five months", "week"), 5 * 4)
  expect_equal(convert_to_given_timeperiod("five days", "week"), 5 / 7)
  expect_equal(convert_to_given_timeperiod("five years", "week"), 5 * 52.1429)
  expect_equal(convert_to_given_timeperiod("five minutes", "week"),
               5 / (7 * 24 * 60))
  expect_equal(convert_to_given_timeperiod("five seconds", "week"),
               5 / (7 * 24 * 3600))

  expect_equal(convert_to_given_timeperiod("five hour", "weeks"),
               5 / (7 * 24))
  expect_equal(convert_to_given_timeperiod("eight hours", "hour"), 8)
  expect_equal(convert_to_given_timeperiod("eight months", "hour"),
               8 * 24 * 30)
  expect_equal(convert_to_given_timeperiod("eight days", "hour"), 8 * 24)
  expect_equal(convert_to_given_timeperiod("eight years", "hour"),
               8 * 24 * 365)
  expect_equal(convert_to_given_timeperiod("eight weeks", "hour"), 8 * 24 * 7)
  expect_equal(convert_to_given_timeperiod("eight minutes", "hour"), 8 / 60)
  expect_equal(convert_to_given_timeperiod("eight second", "hour"), 8 / 3600)



})
