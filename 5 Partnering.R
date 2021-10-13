source("project/functions/the_libraries.R")
source("CodeToArchive/balance/functions/overall.R")

# cbm all data 
load("derived files/2017_18_Cohort/cont/cbm_all.RData")


# load the data with centered covariates
load("derived files/2017_18_Cohort/cont/cen_dat.RData")
load("derived files/2017_18_Cohort/cont/outcome_m_eq.RData")
load("derived files/2017_18_Cohort/cont/ss_dat.RData")
load("derived files/2017_18_Cohort/cont/college_en_dat.RData")


load("derived files/balance 2019/cont/all_fice.RData")

# fice_dat <- all_fice %>% 
#   dplyr::select(-district) %>%
#   distinct(., .keep_all = TRUE)
# 
# 
# dist <- all_fice %>% dplyr::select(instfice, district)

# Partner Data ------------------------------------------------------------

enrollment_counts <- 
  cen_join %>%
  left_join(cen_dat, by = "id2") %>%
  select(id2, sem, DISTNAME, instfice = stufice) %>%
  left_join(all_fice, by = c("instfice", "DISTNAME" = "district")) %>%
  filter(!is.na(instname)) %>%
  group_by(DISTNAME, instname) %>%
  count()

enrollment_counts %>%
  anti_join(all_fice, by = c("instname", "DISTNAME" = "district"))
all_fice %>%
  anti_join(enrollment_counts, by = c("instname", "district" = "DISTNAME"))
enrollment_counts %>%
  full_join(all_fice, by = c("instname", "DISTNAME" = "district")) %>%
  arrange(DISTNAME, instname) %>%
  View()

dat <- 
  cen_join %>%
  left_join(cen_dat, by = "id2") %>%
  select(id2, sem, DISTNAME, instfice = stufice) %>%
  left_join(all_fice, by = c("instfice", "DISTNAME" = "district")) %>%
  group_by(id2, sem) %>%
  summarise(
    en = as.numeric(any(!is.na(instname))),
    instfice = paste(unique(instfice), collapse = "; "),
    instname = paste(unique(instname), collapse = "; ")
  ) %>%
  left_join(cen_dat, by = "id2")

## previous way of calculating enrollments
## was not correct because it counted any community college enrollment as partner enrollment
#
# dat <- 
#   cen_join %>%
#   left_join(cen_dat, by = "id2") %>%
#   ungroup() %>%
#   mutate(type = ifelse(type %in% "c", "c", NA),
#          stufice = ifelse(type %in% "c", stufice, NA)) %>%
#   rename(instfice = stufice) %>%
#   left_join(all_fice, by = c("instfice", "DISTNAME" = "district")) %>%
#   group_by(id2, sem) %>%
#   distinct(id2, .keep_all = TRUE) %>%
#   ungroup() %>%
#   mutate(instname = ifelse(str_detect(instname, "LONE STAR"), "LONE STAR", instname)) %>%
#   mutate(en = ifelse(is.na(instfice), 0, 1))


dups <- 
  dat %>%
  group_by(id2) %>%
  filter(duplicated(sem) | duplicated(sem, fromLast = TRUE)) %>% 
  ungroup()

dat <- 
  dat %>%
  group_by(id2) %>%
  mutate(cum_sum = cumsum(en)) %>%
  ungroup() %>%
  mutate(y = as.integer(cum_sum > 0))


# Analysis ----------------------------------------------------------------

library(lmtest)
library(sandwich)
library(broom)


# results 

partner_results <- dat %>%
  group_by(sem) %>%
  do(lin_prob(., outcome = "Partner", HC = "HC0"))

partner_overall <- partner_results %>%
  do(extract_overall(., outcome = "Partner"))

save(partner_results, partner_overall, file = "derived files/2017_18_Cohort/cont/results_contemporaneous_enroll_partner.RData")


# examine residual plot

dat <- clean_dat

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
  ggtitle("Enrollment: Partner Colleges") +
  labs(x = "Propensity Scores", y = "Predicted Values", color = "") +
  #ylim(c(0,1.2)) + 
  theme_minimal()

# res values against ps
dat %>%
  ggplot(aes(x = es.mean.ATT, y = res, color = treat)) +
  geom_point(alpha = .2) +
  geom_smooth(se = F) +
  ggtitle("Enrollment: Partner Colleges") +
  labs(x = "Propensity Scores", y = "Residuals", color = "") +
  theme_minimal()


