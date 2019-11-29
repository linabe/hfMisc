library(purrr)
library(dplyr)
library(lubridate)
library(stringi)
library(rlang)
library(testthat)
library(hfmisc)


sos_data_test <- prep_sosdata(sos_data)

expect_error(
  do_sosvar(
    sosdata = sos_data_test,
    cohortdata = rs_data,
    patid = lopnr
  ),
  "lopnr does not exist in sosdata"
)


expect_error(
  do_sosvar(
    sosdata = sos_data_test %>% rename(lopnr = id),
    cohortdata = rs_data,
    patid = lopnr
  ),
  "lopnr does not exist in cohortdata"
)


expect_error(
  do_sosvar(
    sosdata = sos_data_test,
    cohortdata = rs_data,
    patid = id,
    indexdate = indexdatetest
  ),
  "indexdatetest does not exist in cohortdata"
)


expect_error(
  do_sosvar(
    sosdata = sos_data_test,
    cohortdata = rs_data,
    patid = id,
    indexdate = indexdtm,
    sosdate = sosdatetest
  ),
  "sosdatetest does not exist in sosdata"
)


expect_warning(
  do_sosvar(
    sosdata = sos_data_test %>% mutate(deathdtm = 1),
    cohortdata = rs_data,
    patid = id,
    indexdate = indexdtm,
    sosdate = sosdtm,
    type = "mh",
    diakod = " I",
    name = "cv",
    warnings = TRUE
  ),
  "cohortdata and sosdata have overlapping columns. Only id should be the same. This might cause unexpected results."
)

expect_error(do_sosvar(
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
  do_sosvar(
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
  do_sosvar(
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
  test_data <- do_sosvar(
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
  "id is not unique in cohortdata. Output data
    will be for unique id and indexdtm."
)

test_data <- test_data %>%
  group_by(id) %>%
  slice(1) %>%
  ungroup()

expect_warning(
  do_sosvar(
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
  do_sosvar(
    sosdata = sos_data_test,
    cohortdata = rs_data,
    patid = id,
    indexdate = indexdtm,
    sosdate = sosdtm,
    type = "mh",
    name = "cv",
    diakod = " I",
    stoptime = 365.25,
    warnings = TRUE
  ),
  "stoptime for comorbidity is not negative."
)

rs_data_test <- do_sosvar(
  sosdata = sos_data_test,
cohortdata = rs_data,
patid = id,
indexdate = indexdtm,
sosdate = sosdtm,
type = "mh",
name = "cv1y",
diakod = " I",
stoptime = -365.25
)


rs_data_test <- do_sosvar(
  sosdata = sos_data_test,
  cohortdata = rs_data_test,
  patid = id,
  indexdate = indexdtm,
  sosdate = sosdtm,
  type = "out",
  name = "cv",
  diakod = " I",
  stoptime = 365.25,
  censdate = deathdtm
)


rs_data_test <- do_sosvar(
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
  censdate = deathdtm
)

expect_that(sum(rs_data_test$sos_mh_cv1y == 1), equals(222))

expect_that(sum(rs_data_test$sos_out_cv == 1), equals(179))

expect_that(sum(rs_data_test$sos_out_cv_HDIA == 1), equals(201))

expect_that(sum(rs_data_test$sos_outtime_cv), equals(222021))
