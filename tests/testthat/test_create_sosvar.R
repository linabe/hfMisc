
sos_data_test <- prep_sosdata(sos_data, utdatum = TRUE)

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
  stoptime,
  censdate = deathdtm,
  warning = FALSE
)

expect_that(sum(rs_data_test$sos_com_cv1y == 1), equals(222))

expect_that(sum(rs_data_test$sos_out_cv == 1), equals(179))

expect_that(sum(rs_data_test$sos_out_cv_HDIA == 1), equals(201))

expect_that(sum(rs_data_test$sos_outtime_cv), equals(124467))


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

expect_that(rs_data_test %>% count(sos_out_repI), equals(data.frame(sos_out_repI = c(0, 1, 2, 3, 4, 5, 7), n = as.integer(c(287, 97, 76, 27, 8, 4, 1)))))

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

expect_that(sum(rs_data_test$sos_out_cv_HDIA_ss == 1), equals(197))

expect_that(sum(rs_data_test$sos_outtime_cv_HDIA_ss), equals(185369))

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

expect_that(sum(rs_data_test$sos_out_repI_ss), equals(368))
