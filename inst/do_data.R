library(lubridate)
library(tidyverse)

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
  mutate(deathdtm = case_when(
    difftime(tmp_deathdtm, indexdtm, units = "days") >= 0 ~ tmp_deathdtm,
    TRUE ~ ymd("2015-12-31")
  )) %>%
  dplyr::select(-tmp_deathdtm) %>%
  arrange(id, indexdtm)


# Create sosdata ----------------------------------------------------------

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
  select(id, INDATUM, UTDATUM, HDIA, DIA01, OP01, OP02,
         ekod1, ekod2, UTDATUM, AR)

save(rs_data, sos_data, file = "./data/data.rda")
