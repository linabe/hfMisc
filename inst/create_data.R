library(lubridate)
library(dplyr)
library(tidyr)

set.seed(734895789)


# Create cohortdata -------------------------------------------------------

rs_data <- data.frame(
  id = sample(500, 500, replace = TRUE),
  indexdtm = as.Date(sample(365 * 3, 500,
    replace = TRUE
  ),
  origin = as.Date("2012-01-01")
  )
)

deathdate <- data.frame(
  id = sample(500, 350, replace = FALSE),
  tmp_deathdtm = as.Date(sample(365 * 3, 350, replace = TRUE),
    origin = as.Date("2012-01-01")
  )
)

rs_data <- left_join(rs_data,
  deathdate,
  by = "id"
) %>%
  mutate(
    deathdtm = case_when(
      difftime(tmp_deathdtm, indexdtm, units = "days") >= 0 ~ tmp_deathdtm,
      TRUE ~ ymd("2015-12-31")
    ),
    outtime_death = as.numeric(deathdtm - indexdtm),
    out_death_num = if_else(deathdtm == ymd("2015-12-31"), 0, 1),
    out_death_char = if_else(out_death_num == 0, "No", "Yes"),
    out_death_fac = factor(out_death_num, levels = 0:1, labels = c("No", "Yes")),
    out_hosphf = rbinom(n = n(), size = 1, prob = 0.7),
    xvar_4_num = rbinom(n = n(), size = 3, prob = 0.15),
    xvar_2_fac = factor(rbinom(n = n(), size = 1, prob = 0.15), levels = 0:1, labels = c("No", "Yes"))
  ) %>%
  dplyr::select(-tmp_deathdtm) %>%
  arrange(id, indexdtm)


# Create sosdata ----------------------------------------------------------

set.seed(734895789)

sos_data <- data.frame(
  id = sample(400, 5500,
    replace = TRUE
  ),
  INDATUM = as.Date(sample(365 * 15, 5500,
    replace = TRUE
  ), origin = as.Date("2000-01-01")),
  HDIA = sample(c("I50 C50", "F45", "R34 I54", "I45", "I66", "I"), 5500,
    replace = TRUE
  ),
  DIA01 = sample(c("I50 C50", "F45", "R34 I54", "I45", "I66", "I"), 5500,
    replace = TRUE
  ),
  OP01 = sample(c("TNX", "DKW00", "KGH20", "", "I50"), 5500,
    replace = TRUE
  ),
  OP02 = sample(c("TNX", "DKW00", "KGH20", "", "I50"), 5500,
    replace = TRUE
  ),
  ekod1 = sample(c("Y80", "", "W5", "I50"), 5500,
    replace = TRUE
  ),
  ekod2 = sample(c("Y80", "", "W5", "I50"), 5500,
    replace = TRUE
  )
) %>%
  mutate(UTDATUM = INDATUM + sample(1:20, 5500,
    replace = TRUE
  )) %>%
  mutate(AR = year(UTDATUM)) %>%
  mutate_if(is.factor, as.character) %>%
  arrange(id, UTDATUM) %>%
  select(
    id, INDATUM, UTDATUM, HDIA, DIA01, OP01, OP02,
    ekod1, ekod2, UTDATUM, AR
  )


# Create deathdata --------------------------------------------------------

set.seed(734895789)

dors_data <- rs_data %>%
  rename(DODSDAT = deathdtm) %>%
  filter(DODSDAT != ymd("2015-12-31")) %>%
  select(id, DODSDAT) %>%
  group_by(id) %>%
  slice(1) %>%
  ungroup() %>%
  sample_n(size = 100) %>%
  mutate(ULORSAK = sample(c("I50", "F45", "R34", "I45", "I66", "I"), 100,
    replace = TRUE
  )) %>%
  arrange(id)

save(rs_data, sos_data, dors_data, file = "./data/data.rda")


# Create meddata ----------------------------------------------------------

set.seed(734895789)

med_data <- rs_data %>%
  select(id, indexdtm) %>%
  mutate(ATC = sample(c("C07A", "C01BC04", "C08DB01"), n(),
    replace = TRUE
  ))

save(rs_data, sos_data, dors_data, med_data, file = "./data/data.rda")
