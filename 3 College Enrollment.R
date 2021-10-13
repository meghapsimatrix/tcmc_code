# enrollment into college

library(purrr)

source("project/functions/the_libraries.R")
source("CodeToArchive/balance/functions/overall.R")


# cbm all data 
load("derived files/2017_18_Cohort/cont/cbm_all.RData")


# load the data with centered covariates
load("derived files/2017_18_Cohort/cont/cen_dat.RData")
load("derived files/2017_18_Cohort/cont/outcome_m_eq.RData")
load("derived files/2017_18_Cohort/cont/ss_dat.RData")


factors <- list(
  id2 = cen_dat$id2,
  sem = c("2018_3","2018_1", "2019_2", "2019_3", "2019_1")
)

analysis_dat <- expand.grid(factors) 
  

cen_join <- left_join(analysis_dat, cbm_all, by = c("id2", "sem")) 

cen_join <- 
  cen_join %>%
  mutate(
    sem = factor(sem, levels = factors$sem, labels = 0:4),
    sem = as.numeric(as.character(sem))
  )

table(cen_join$sem, useNA = "ifany")


dups <- 
  cen_join %>%
  group_by(id2) %>%
  filter(duplicated(sem) | duplicated(sem, fromLast = TRUE)) %>% 
  ungroup()


check_dups <- semi_join(cen_join, dups, by = "id2")





# Create the outcome ------------------------------------------------------


dat <- 
  cen_join %>%
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
#   dplyr::select(id2, type, stufice, sem, en, cum_sum, y) %>%
#   View()

dat <- left_join(dat, cen_dat, by = "id2")

save(cen_join, file = "derived files/2017_18_Cohort/cont/college_en_dat.RData")

# Analysis ----------------------------------------------------------------

# model

# cti_lm <- lm(eq, data = dat, weights = gbm_wt)
# results <- coeftest(cti_lm, vcov = vcovHC, type = "HC2")


# results 


names(dat)

library(sandwich)
library(lmtest)
library(broom)


enroll_results <- dat %>%
  group_by(sem) %>%
  do(lin_prob(., outcome = "Enroll College", HC = "HC0"))

enroll_overall <- enroll_results %>%
  do(extract_overall(., outcome = "Enroll College"))


save(enroll_results, enroll_overall, file = "derived files/2017_18_Cohort/cont/results_contemporaneous_college_enroll.RData")


# check residuals
# examine residual plot

dat$res <-  cti_lm$residuals
dat$pred <-  fitted.values(cti_lm)

dat <- dat %>%
  mutate(treat = as.factor(ifelse(D == 1, "TCMC", "Comparison")))


#dat$logit_ps <- with(dat, log(es.mean.ATT/ (1 - es.mean.ATT)))

# predicted against ps
dat %>%
  ggplot(aes(x = es.mean.ATT, y = pred, color = treat)) +
  geom_point(alpha = .2) +
  geom_smooth(se = F) +
  ggtitle("College Enrollment") +
  labs(x = "Propensity Scores", y = "Predicted Values", color = "") +
  #ylim(c(0,1.2)) + 
  theme_minimal()

# res values against ps
dat %>%
  ggplot(aes(x = es.mean.ATT, y = res, color = treat)) +
  geom_point(alpha = .2) +
  geom_smooth(se = F) +
  ggtitle("College Enrollment") +
  labs(x = "Propensity Scores", y = "Residuals", color = "") +
  theme_minimal()

















