dors_data_test <- prep_sosdata(dors_data, registry = "dors")

dors_data_dup <- bind_rows(dors_data_test, dors_data_test)


expect_warning(
  create_deathvar(
    sosdata = dors_data_dup,
    cohortdata = rs_data,
    patid = id,
    indexdate = indexdtm,
    orsakkod = " I",
    name = "cv",
    warnings = TRUE
  ),
  "sosdata has duplicated id this might cause unexpected results."
)

rs_data_ulorsak <- left_join(rs_data,
                             dors_data_test,
                             by = "id"
)

rs_data_test <- create_deathvar(
  cohortdata = rs_data_ulorsak,
  name = "cv",
  orsakkod = " I| R34"
)

expect_that(sum(rs_data_test$sos_out_deathcv == 1), equals(145))

rs_data_test <- create_deathvar(
  cohortdata = rs_data_ulorsak,
  name = "cvfac",
  orsakkod = " I| R34",
  valsclass = "fac"
)

expect_that(sum(rs_data_test$sos_out_deathcvfac == "yes"), equals(145))


rs_data_test <- create_deathvar(
  sosdata = dors_data_test,
  cohortdata = rs_data,
  patid = id,
  indexdate = indexdtm,
  name = "cv2",
  orsakkod = " I| R34",
  orsakvar = ULORSAK,
  censdate = deathdtm,
  calctimetodeath = TRUE
)

expect_that(sum(rs_data_test$sos_out_deathcv2 == 1), equals(145))
expect_that(mean(rs_data_test$sos_outtime_deathcv2), equals(659.54))


rs_data_test <- create_deathvar(
  sosdata = dors_data_test,
  cohortdata = rs_data,
  patid = id,
  name = "cv3",
  orsakkod = " I| R34",
  orsakvar = ULORSAK
)

expect_that(sum(rs_data_test$sos_out_deathcv3 == 1), equals(145))


expect_warning(
  rs_data_test <- create_deathvar(
    cohortdata = rs_data_ulorsak,
    name = "cvfac",
    orsakkod = " I| R34",
    valsclass = "fac",
    calctimetodeath = TRUE
  ),
  "sosdata has not been supplied and therefore arguments censdate, indexdate, patid and calctimetodeath will have no affect."
)

expect_warning(
  rs_data_test <- create_deathvar(
    cohortdata = rs_data_ulorsak,
    name = "cvfac",
    orsakkod = " I| R34",
    valsclass = "fac",
    patid = lopnr
  ),
  "sosdata has not been supplied and therefore arguments censdate, indexdate, patid and calctimetodeath will have no affect."
)
