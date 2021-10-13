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


math_dat <- clean_dat %>%
  filter(ssDev == "0") %>%
  filter(!(ssPre %in% c("DMAT", "DMTH", "DSMA")))

table(math_dat$ssPre)


cbm00s_join <- 
  left_join(analysis_dat, math_dat, by = c("id2", "sem")) %>%
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
  ungroup() %>%
  mutate(pass = ifelse(ssGrade %in% c("1", "2", "3", "4", "8"), 1, 0))


dups <- dat %>%
  group_by(id2) %>%
  filter(duplicated(sem) | duplicated(sem, fromLast = TRUE)) %>% 
  ungroup()


dat <- dat %>%
  group_by(id2) %>%
  arrange(desc(pass)) %>%
  distinct(sem, .keep_all = TRUE) %>%
  ungroup() %>%
  arrange(sem) %>%
  group_by(id2) %>%
  mutate(cum_sum = cumsum(pass)) %>%
  ungroup() %>%
  mutate(y = as.integer(cum_sum > 0))


# dat %>%
#   dplyr::select(id2, type, ssFICE, ssPre, sem, pass, cum_sum, y) %>%
#   View()


dat <- left_join(dat, cen_dat, by = "id2")


check <- dat %>% dplyr::select(id2, ssPre, ssNum, ssDev, ssGrade, pass, cum_sum, y, sem)




# Analysis ----------------------------------------------------------------

library(sandwich)
library(lmtest)
library(broom)


math_pass_results <- dat %>%
  group_by(sem) %>%
  do(lin_prob(., outcome = "Math Pass", HC = "HC0"))

math_pass_overall <- math_pass_results %>%
  do(extract_overall(., outcome = "Math Pass"))

save(math_pass_results, math_pass_overall, file = "derived files/2017_18_Cohort/cont/results_contemporaneous_math_pass.RData")
