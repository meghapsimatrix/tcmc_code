library(purrr)
source("project/functions/the_libraries.R")

load("derived files/2017_18_Cohort/cont/cen_dat.RData")
load("derived files/2017_18_Cohort/cont/vars.RData")
load("derived files/2017_18_Cohort/cont/outcome_m_eq.RData")


source("CodeToArchive/balance/functions/overall.R")



#sum(ss_dat$n)

(ss_dat <- cen_dat %>%
    group_by(CAMPNAME, D) %>%
    summarize(n = n()) %>%
    ungroup() %>%
    rename(Campus = CAMPNAME) %>%
    filter(D == 1))


(overall_dat <- cen_dat %>%
    group_by(D) %>%
    summarize(n = n()) %>%
    ungroup())

write_csv(overall_dat, "overall_ss.csv")

save(ss_dat, file = "derived files/2017_18_Cohort/cont/ss_dat.RData")

# High School Same Year ---------------------------------------------------



# read in the graduation 
grad_dat_18 <- read_tsv("NewFilesReleased/TEA/p_graduate18.txt")
grad_dat_19 <- read_csv("NewFilesReleased/TEA/p_graduate19.txt")

grad_dat <- 
  bind_rows(
    grad_dat_18 %>% dplyr::select(id2, GRADDATE) %>% mutate(g_date = 18), 
    grad_dat_19 %>% dplyr::select(id2, GRADDATE) %>% mutate(g_date = 19)
  ) %>%
  filter(!is.na(id2)) %>%
  distinct(id2, g_date, .keep_all = TRUE) %>%
  as_tibble() %>%
  mutate(id2 = as.character(id2))

# join the graduation data set and create the outcome variable 
analysis_dat <- 
  cen_dat %>%
  mutate(id2 = as.character(id2)) %>%
  left_join(grad_dat, by = "id2") 


table(is.na(analysis_dat$GRADDATE)) 
table(analysis_dat$GRADDATE)
table(analysis_dat$g_date)

# dups
nrow(analysis_dat %>% filter(duplicated(id2)))

analysis_dat %>% 
  filter(duplicated(id2) | duplicated(id2, fromLast = TRUE)) %>% 
  select(id2, GRADDATE)


analysis_dat <- 
  analysis_dat %>%
  mutate(
    y = ifelse(g_date == 18, 1, 0),
    y = ifelse(is.na(g_date), 0, y)
  )

table(analysis_dat$y)

# Analysis ----------------------------------------------------------------

dat <- analysis_dat

library(lmtest)
library(sandwich)
library(broom)

# model
cti_lm <- lm(eq, data = dat, weights = gbm_wt)
results <- coeftest(cti_lm, vcov = vcovHC, type = "HC0")

# results 
grad_res <- lin_prob(dat, form = eq, outcome = "HS Grad 18", HC = "HC0") 
hs_overall <- extract_overall(grad_res, outcome = "HS Grad 18")

# save(grad_res, hs_overall, file = "derived files/2017_18_Cohort/cont/results_contemporaneous.RData")

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
  ggtitle("HS Grad") +
  labs(x = "Propensity Scores", y = "Predicted Values", color = "") +
  #ylim(c(0,1.2)) + 
  theme_minimal()

# res values against ps
dat %>%
  ggplot(aes(x = es.mean.ATT, y = res, color = treat)) +
  geom_point(alpha = .2) +
  geom_smooth(se = F) +
  ggtitle("HS Grad") +
  labs(x = "Propensity Scores", y = "Residuals", color = "") +
  theme_minimal()




# 2 year  -----------------------------------------------------------------

dat <- dat %>%
  mutate(y = ifelse(is.na(g_date), 0, 1)) 

table(dat$y)

# results 
grad_res_19 <- lin_prob(dat, form = eq, outcome = "HS Grad 19", HC = "HC0") 
hs_overall_19 <- extract_overall(grad_res_19, outcome = "HS Grad 19")



HS_grad_res <- bind_rows(grad_res, grad_res_19)
HS_grad_overall <- bind_rows(hs_overall, hs_overall_19)

save(HS_grad_res, HS_grad_overall, file = "derived files/2017_18_Cohort/cont/results_contemporaneous_HS_grad.RData")
