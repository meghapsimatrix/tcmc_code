library(purrr)
source("project/functions/the_libraries.R")
library(dplyr)


# the data without the extreme propensity scores
load("derived files/2017_18_Cohort/cont/gbm_res_across.RData")


final_dat <- final_res %>%
  mutate(LEP_ATTEND = as.numeric(LEP_ATTEND),
         id2 = as.character(id2)) 

# take out irrelevant vars - take out id2 so the dummy coding doesn't mess up

covs <- final_dat %>%
  dplyr::select(-c(MATHEMATICS_GRADE_7_2_13, MATHEMATICS_GRADE_7_A_13))


str(covs)


final_dat_dc <- dummy.data.frame(covs, names = c("SEX", "ETHNIC", "ALGEBRA_I_ALG_1_2_14", "ALGEBRA_I_ALG_1_2_15", "ALGEBRA_II_ALG2_2_16",
                                                 "ALGEBRA_II_ALG2_2_17", "GEOMETRY_GEOM_2_15", "GEOMETRY_GEOM_2_16", "MATHEMATICS_GRADE_8_2_14",
                                                 "MATHEMATICS_GRADE_8_A_14", "PRECALCULUS_PRE_CALC_2_17", "at_risk_enroll_imp",
                                                 "immigrant_enroll_imp", "speced_ever_imp", "gifted_ever_imp", "atrisk_ever_imp",
                                                 "immigrant_ever_imp"))




save(final_dat_dc, file = "derived files/2017_18_Cohort/cont/cont_dum_dat.RData")


# function to center at the mean

center_at <- function(x, subset){
  x - mean(x[subset])
}



# group by the campus and then center

vars <- names(final_dat_dc)[5:95]

save(vars, file = "derived files/2017_18_Cohort/cont/cont_vars.RData")


center <- function(dat) {
  
  dat %>%
    mutate_at(vars, center_at, subset = dat$D == 1)
  
}

cen_dat <- final_dat_dc %>%
  group_by(CAMPNAME) %>%
  do(center(.)) %>%
  ungroup() 


# verify centering

cen_dat %>%
  filter(D == 1) %>%
  group_by(CAMPNAME) %>%
  summarise_at(vars(vars), mean) %>%
  summarise_at(vars(vars), ~ max(abs(.))) %>%
  gather() %>%
  summarise(
    min = min(value), 
    max = max(value)
  )

cen_dat %>%
  group_by(DISTNAME, CAMPNAME) %>%
  summarise(
    N = n(),
    Trt = sum(D),
    Comp = sum(1L - D)
  )

(eq <- paste0("y ~ 0 + CAMPNAME + D:CAMPNAME + D:(", paste(vars, collapse = " + "), ") +",  paste(vars, collapse = " + ")))



save(cen_dat, file = "derived files/2017_18_Cohort/cont/cen_dat.RData")
save(vars, file = "derived files/2017_18_Cohort/cont/vars.RData")
save(eq, file = "derived files/2017_18_Cohort/cont/outcome_m_eq.RData")
