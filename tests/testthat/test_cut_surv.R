
rs_data_test <- rs_data %>%
  mutate(
    eventtest = if_else(out_death_num == 0, 2, 1),
    eventtest2 = if_else(out_death_char == "No", "Tjollahop", "Tjollahej"),
  )

expect_error(
  cut_surv(rs_data, indexdtm, outtime_death, at = 365, rename = "1yr"),
  "event is Date and no censval is given."
)

expect_error(
  cut_surv(rs_data_test, eventtest, outtime_death, at = 365, rename = "1yr"),
  "event is not 0, 1 and no censval is given."
)

expect_error(
  cut_surv(rs_data_test, eventtest2, outtime_death, at = 365, rename = "1yr"),
  "event is not No, Yes and no censval is given."
)

rs_data_test <- cut_surv(rs_data, out_death_fac, outtime_death, at = 365, rename = "1yr")
expect_that(nrow(rs_data_test %>% filter(out_death_fac1yr == "Yes")), equals(101))

rs_data_test <- cut_surv(rs_data, out_death_char, outtime_death, at = 365, rename = "1yr")
expect_that(nrow(rs_data_test %>% filter(out_death_char1yr == "Yes")), equals(101))

rs_data_test <- cut_surv(rs_data, out_death_num, outtime_death, at = 365, rename = "1yr")
expect_that(nrow(rs_data_test %>% filter(out_death_num1yr == 1)), equals(101))
expect_that(sum(rs_data_test %>% pull(outtime_death1yr)), equals(163402))

rs_data_test <- cut_surv(rs_data, out_death_num, outtime_death, at = 365)
expect_that(nrow(rs_data_test %>% filter(out_death_num == 1)), equals(101))
