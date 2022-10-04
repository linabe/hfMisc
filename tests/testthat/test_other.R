
# tests for create_crvar

expect_warning(
  rs_data <- create_crvar(rs_data, "xvar_4_num"),
  "Variable is not a character or factor and will not be returned. Use forcenum = TRUE to proceed"
)

# tests for create_medvar

expect_error(
  rs_data <- create_medvar(atc = "^C07A", medname = "bbl", cohortdata = rs_data, meddata = med_data, id = "id"),
  "id is not unique. Supply additional id variable."
)

rs_data_test <- create_medvar(atc = "^C07A", medname = "bbl", cohortdata = rs_data, meddata = med_data, id = c("id", "indexdtm"))
expect_that(sum(rs_data_test$sos_lm_bbl), equals(169))
