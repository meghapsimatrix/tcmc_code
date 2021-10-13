source("project/functions/the_libraries.R")
library(purrr)
source("CodeToArchive/balance/functions/overall.R")

# load the data with centered covariates
load("derived files/2017_18_Cohort/cont/cen_dat.RData")
load("derived files/2017_18_Cohort/cont/outcome_m_eq.RData")
load("derived files/2017_18_Cohort/cont/ss_dat.RData")


load("derived files/2017_18_Cohort/cont/clean_dat.RData")


# Create Dat ------------------------------------------------------------------


table(clean_dat$sem)

factors <- list(
  id2 = cen_dat$id2,
  sem = c("2018_3", "2018_1", "2019_2", "2019_3", "2019_1")
)

analysis_dat <- expand.grid(factors) 


dev_dat <- clean_dat %>%
  filter(ssDev == "1" | ssPre %in% c("DMAT", "DMTH", "DSMA"))

dev_dat %>%
  group_by(ssDev, ssPre) %>% 
  count()

cbm00s_join <- 
  left_join(analysis_dat, dev_dat, by = c("id2", "sem")) %>%
  mutate(
    sem = factor(sem, levels = factors$sem, labels = 0:4),
    sem = as.numeric(as.character(sem))
  )

table(cbm00s_join$sem, useNA = "ifany")

table(cbm00s_join$ssPre, useNA = "ifany")



dups <- cbm00s_join %>%
  group_by(id2) %>%
  filter(duplicated(sem) | duplicated(sem, fromLast = TRUE)) %>% 
  ungroup()


# Just enrollment ---------------------------------------------------------


dat <- cbm00s_join %>%
  group_by(id2) %>%
  distinct(sem, .keep_all = TRUE) %>%
  ungroup() %>%
  mutate(en = ifelse(is.na(type), 0, 1))

table(dat$en, useNA = "ifany")



dat <- dat %>%
  group_by(id2) %>%
  mutate(cum_sum = cumsum(en)) %>%
  ungroup() %>%
  mutate(y = as.integer(cum_sum > 0))


# dat %>%
#   dplyr::select(id2, type, ssFICE, ssPre, sem, en, cum_sum, y) %>%
#   View()


dat <- left_join(dat, cen_dat, by = "id2")


# dat %>% dplyr::select(id2, ssPre, ssNum, ssDev) %>% filter(!is.na(ssDev)) %>% View()



# Analysis ----------------------------------------------------------------

library(sandwich)
library(lmtest)
library(broom)

dev_enroll_results <- dat %>%
  group_by(sem) %>%
  do(lin_prob(., outcome = "Dev Enroll", HC = "HC0"))

dev_enroll_overall <- dev_enroll_results %>%
  do(extract_overall(., outcome = "Dev Enroll"))

save(dev_enroll_results, dev_enroll_overall, file = "derived files/2017_18_Cohort/cont/results_contemporaneous_dev_enroll.RData")

