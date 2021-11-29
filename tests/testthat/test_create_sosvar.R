
sos_data_test <- prep_sosdata(sos_data, utdatum = TRUE, evar = "ekod")

expect_error(
  create_sosvar(
    sosdata = sos_data_test,
    cohortdata = rs_data,
    patid = lopnr
  ),
  "lopnr does not exist in sosdata"
)


expect_error(
  create_sosvar(
    sosdata = sos_data_test %>% rename(lopnr = id),
    cohortdata = rs_data,
    patid = lopnr
  ),
  "lopnr does not exist in cohortdata"
)


expect_error(
  create_sosvar(
    sosdata = sos_data_test,
    cohortdata = rs_data,
    patid = id,
    indexdate = indexdatetest
  ),
  "indexdatetest does not exist in cohortdata"
)


expect_error(
  create_sosvar(
    sosdata = sos_data_test,
    cohortdata = rs_data,
    patid = id,
    indexdate = indexdtm,
    sosdate = sosdatetest
  ),
  "sosdatetest does not exist in sosdata"
)


expect_warning(
  create_sosvar(
    sosdata = sos_data_test %>% mutate(deathdtm = 1),
    cohortdata = rs_data,
    patid = id,
    indexdate = indexdtm,
    sosdate = sosdtm,
    type = "com",
    diakod = " I",
    name = "cv",
    warnings = TRUE
  ),
  "cohortdata and sosdata have overlapping columns. Only id should be the same. This might cause unexpected results."
)

expect_error(create_sosvar(
  sosdata = sos_data_test,
  cohortdata = rs_data,
  patid = id,
  indexdate = indexdtm,
  sosdate = sosdtm,
  type = "Test",
  diakod = " I",
  name = "cv"
))


expect_error(
  create_sosvar(
    sosdata = sos_data_test,
    cohortdata = rs_data,
    patid = id,
    indexdate = indexdtm,
    sosdate = sosdtm,
    type = "out",
    diakod = " I",
    name = "cv"
  ),
  "censdate is needed for variables of type out."
)

expect_error(
  create_sosvar(
    sosdata = sos_data_test,
    cohortdata = rs_data,
    patid = id,
    indexdate = indexdtm,
    sosdate = sosdtm,
    type = "out",
    name = "cv",
    censdate = "deathdtm"
  ),
  "Either dia, op or ekod must be specified."
)


expect_warning(
  test_data <- create_sosvar(
    sosdata = sos_data_test,
    cohortdata = rs_data,
    patid = id,
    indexdate = indexdtm,
    sosdate = sosdtm,
    type = "out",
    name = "cv",
    diakod = " I",
    censdate = deathdtm,
    warnings = TRUE
  ),
  "id is not unique in cohortdata. Output data will be for unique id and indexdtm."
)

test_data <- test_data %>%
  group_by(id) %>%
  slice(1) %>%
  ungroup()

expect_warning(
  create_sosvar(
    sosdata = sos_data_test,
    cohortdata = test_data,
    patid = id,
    indexdate = indexdtm,
    sosdate = sosdtm,
    type = "out",
    name = "cv",
    diakod = " I",
    censdate = deathdtm,
    warnings = TRUE
  ),
  "sos_out_cv already exists in cohortdata. This might cause unexpected results."
)


expect_warning(
  create_sosvar(
    sosdata = sos_data_test,
    cohortdata = rs_data,
    patid = id,
    indexdate = indexdtm,
    sosdate = sosdtm,
    type = "com",
    name = "cv",
    diakod = " I",
    stoptime = 365.25,
    warnings = TRUE
  ),
  "stoptime for comorbidity is not negative."
)

rs_data_test <- create_sosvar(
  sosdata = sos_data_test,
  cohortdata = rs_data,
  patid = id,
  indexdate = indexdtm,
  sosdate = sosdtm,
  type = "com",
  name = "cv1y",
  diakod = " I",
  stoptime = -365.25,
  warning = FALSE
)


rs_data_test <- create_sosvar(
  sosdata = sos_data_test,
  cohortdata = rs_data_test,
  patid = id,
  indexdate = indexdtm,
  sosdate = sosdtm,
  type = "out",
  name = "cv",
  diakod = " I",
  stoptime = 365,
  censdate = deathdtm,
  warning = FALSE
)


rs_data_test <- create_sosvar(
  sosdata = sos_data_test,
  cohortdata = rs_data_test,
  patid = id,
  indexdate = indexdtm,
  sosdate = sosdtm,
  type = "out",
  name = "cv_HDIA",
  diakod = " I",
  diavar = HDIA,
  censdate = deathdtm,
  warning = FALSE
)

rs_data_test <- create_sosvar(
  sosdata = sos_data_test,
  cohortdata = rs_data_test,
  patid = id,
  indexdate = indexdtm,
  sosdate = sosdtm,
  type = "out",
  name = "noncv",
  diakod = " I",
  diakodneg = TRUE,
  diavar = HDIA,
  censdate = deathdtm,
  warning = FALSE
)

expect_that(sum(rs_data_test$sos_com_cv1y == 1), equals(246))

expect_that(sum(rs_data_test$sos_out_cv == 1), equals(192))

expect_that(sum(rs_data_test$sos_out_cv_HDIA == 1), equals(208))

expect_that(sum(rs_data_test$sos_outtime_cv), equals(122121))

expect_that(sum(rs_data_test$sos_out_noncv == 1), equals(53))

# number of

rs_data_test <- create_sosvar(
  sosdata = sos_data_test,
  cohortdata = rs_data,
  patid = id,
  indexdate = indexdtm,
  sosdate = sosdtm,
  noof = TRUE,
  type = "out",
  name = "repI",
  diakod = " I",
  diavar = DIA_all,
  censdate = deathdtm,
  warning = FALSE
)

expect_that(rs_data_test %>% count(sos_out_repI), equals(data.frame(sos_out_repI = c(0, 1, 2, 3, 4, 5, 6), n = as.integer(c(275, 117, 49, 36, 14, 8, 1)))))

# start and stopdate

rs_data_test <- create_sosvar(
  sosdata = sos_data_test,
  cohortdata = rs_data,
  patid = id,
  indexdate = indexdtm,
  sosdate = sosdtm,
  type = "out",
  name = "cv_HDIA_ss",
  diakod = " I",
  diavar = HDIA,
  stoptime,
  censdate = deathdtm,
  warning = FALSE,
  starttime = 0,
  stoptime = 2 * 365
)

expect_that(sum(rs_data_test$sos_out_cv_HDIA_ss == 1), equals(205))

expect_that(sum(rs_data_test$sos_outtime_cv_HDIA_ss), equals(184433))

rs_data_test <- create_sosvar(
  sosdata = sos_data_test,
  cohortdata = rs_data,
  patid = id,
  indexdate = indexdtm,
  sosdate = sosdtm,
  noof = TRUE,
  type = "out",
  name = "repI_ss",
  diakod = " I",
  diavar = DIA_all,
  censdate = deathdtm,
  warning = FALSE,
  starttime = 0,
  stoptime = 2 * 365
)

expect_that(sum(rs_data_test$sos_out_repI_ss), equals(402))

# test comduration

rs_data_test <- create_sosvar(
  sosdata = sos_data_test,
  cohortdata = rs_data,
  patid = id,
  indexdate = indexdtm,
  sosdate = sosdtm,
  comduration = TRUE,
  type = "com",
  name = "r",
  diakod = " R34",
  diavar = DIA_all,
  warning = FALSE,
  starttime = 0
)

expect_that(sum(rs_data_test$sos_comdur_r, na.rm = TRUE), equals(1414180))

rs_data_test <- create_sosvar(
  sosdata = sos_data_test,
  cohortdata = rs_data,
  patid = id,
  indexdate = indexdtm,
  sosdate = sosdtm,
  comduration = TRUE,
  type = "com",
  name = "r",
  diakod = " R34",
  diavar = DIA_all,
  warning = FALSE,
  starttime = -100
)

expect_that(sum(rs_data_test$sos_comdur_r, na.rm = TRUE), equals(1376680))

rs_data_test <- create_sosvar(
  sosdata = sos_data_test,
  cohortdata = rs_data,
  patid = id,
  indexdate = indexdtm,
  sosdate = sosdtm,
  comduration = TRUE,
  type = "com",
  name = "r",
  diakod = " R34",
  diavar = DIA_all,
  warning = FALSE,
  stoptime = -500
)

expect_that(sum(rs_data_test$sos_comdur_r, na.rm = TRUE), equals(38645))

# check noof and stoptime
## unique

rs_data_unik <- rs_data %>% group_by(id) %>% slice(1) %>% ungroup()

rs_data_test <- create_sosvar(
  sosdata = sos_data_test,
  cohortdata = rs_data_unik,
  patid = id,
  indexdate = indexdtm,
  sosdate = sosdtm,
  noof = TRUE,
  stoptime = 2 * 365,
  type = "out",
  name = "r34",
  diakod = " R34",
  diavar = HDIA,
  censdate = deathdtm,
  warning = FALSE
)

expect_that(rs_data_test %>% count(sos_out_r34), equals(tibble(sos_out_r34 = c(0, 1, 2), n = as.integer(c(281, 32, 6)))))

## not unique

rs_data_test <- create_sosvar(
  sosdata = sos_data_test,
  cohortdata = rs_data,
  patid = id,
  indexdate = indexdtm,
  sosdate = sosdtm,
  noof = TRUE,
  stoptime = 2 * 365,
  type = "out",
  name = "r34",
  diakod = " R34",
  diavar = HDIA,
  censdate = deathdtm,
  warning = FALSE
)

expect_that(rs_data_test %>% count(sos_out_r34), equals(data.frame(sos_out_r34 = c(0, 1, 2), n = as.integer(c(446, 47, 7)))))
