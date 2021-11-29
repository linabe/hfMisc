dors_data <- prep_sosdata(dors_data, registry = "dors", impute = FALSE)
rs_data <- left_join(rs_data, dors_data, by = "id") %>%
  mutate(censdtm = pmin(lubridate::ymd("2015-11-30"), deathdtm))

rs_data_test <- create_deathvar(
  cohortdata = rs_data,
  name = "cv",
  orsakkod = " I| R34"
)

expect_that(sum(rs_data_test$sos_out_deathcv == 1), equals(142))

rs_data_test <- create_deathvar(
  cohortdata = rs_data,
  name = "cvfac",
  orsakkod = " I| R34",
  valsclass = "fac"
)

expect_that(sum(rs_data_test$sos_out_deathcvfac == "Yes"), equals(142))


rs_data_test <- create_deathvar(
  cohortdata = rs_data,
  indexdate = indexdtm,
  name = "cv2",
  orsakkod = " I| R34",
  orsakvar = ULORSAK,
  censdate = deathdtm,
  deathdate = deathdtm,
  calctimetodeath = TRUE
)

expect_that(sum(rs_data_test$sos_out_deathcv2 == 1), equals(142))
expect_that(mean(rs_data_test$sos_outtime_deathcv2), equals(659.54))

rs_data_test <- create_deathvar(
  cohortdata = rs_data,
  indexdate = indexdtm,
  name = "cv3",
  orsakkod = " I| R34",
  orsakvar = ULORSAK,
  censdate = censdtm,
  deathdate = deathdtm,
  calctimetodeath = TRUE
)

expect_that(sum(rs_data_test$sos_out_deathcv3 == 1), equals(109))
expect_that(mean(rs_data_test$sos_outtime_deathcv3), equals(638.832))

rs_data_test <- create_deathvar(
  cohortdata = rs_data,
  indexdate = indexdtm,
  name = "cv",
  orsakkod = " I| R34",
  orsakvar = ULORSAK,
  censdate = censdtm,
  deathdate = deathdtm
)

expect_that(sum(rs_data_test$sos_out_deathcv == 1), equals(109))

rs_data_test <- create_deathvar(
  cohortdata = rs_data,
  indexdate = indexdtm,
  name = "cv",
  orsakkod = " I| R34",
  orsakvar = ULORSAK,
  censdate = censdtm,
  deathdate = DODSDAT,
  calctimetodeath = TRUE
)

expect_that(sum(rs_data_test$sos_out_deathcv == 1), equals(142))

rs_data_test <- create_deathvar(
  cohortdata = rs_data,
  name = "cv3",
  orsakkod = " I| R34",
  orsakvar = ULORSAK
)

expect_that(sum(rs_data_test$sos_out_deathcv3 == 1), equals(142))
