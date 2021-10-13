source("project/functions/the_libraries.R")
load("derived files/balance 2019/cont/cont_dum_dat.RData")

glimpse(cont_dum_dat)



# Common Support ----------------------------------------------------------



# just get the common support ones
cont_dum_dat <- 
  cont_dum_dat %>%
  filter(Common_support == TRUE)



# Center ------------------------------------------------------------------



# function to center at the mean
center_at <- function(x, subset){
  x - mean(x[subset])
}



# group by the campus and then center

vars <- names(cont_dum_dat)[5:93]

save(vars, file = "derived files/balance 2019/cont/cont_vars.RData")


center <- function(dat){
  
  dat %>%
    mutate_at(vars, center_at, subset = dat$D == 1)
  
}


cen_dat <- cont_dum_dat %>%
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


glimpse(cen_dat)

table(cen_dat$Common_support)



# Outcome model -----------------------------------------------------------



(eq <- paste0("y ~ 0 + CAMPNAME + D:CAMPNAME + D:(", paste(vars, collapse = " + "), ") +",  paste(vars, collapse = " + ")))



# Sample size for overall estimate ----------------------------------------



(ss_dat <- cen_dat %>%
  group_by(CAMPNAME, D) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  rename(Campus = CAMPNAME) %>%
  filter(D == 1))



save(cen_dat, ss_dat, file = "derived files/balance 2019/cont/cen_dat.RData")

save(eq, file = "derived files/balance 2019/cont/outcome_m_eq.RData")
