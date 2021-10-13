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
load("derived files/2017_18_Cohort/cont/college_en_dat.RData")




# Function for sectors ----------------------------------------------------


res_sector <- function(dat = cen_join, all_dat = cen_dat, col, outcome){
  

  
  dat <- dat %>%
    mutate(type = ifelse(type %in% col, "col", NA)) %>%
    group_by(id2) %>%
    distinct(sem, .keep_all = TRUE) %>%
    ungroup() %>%
    mutate(en = ifelse(is.na(type), 0, 1))
  
  
  
  
  dat <- dat %>%
    group_by(id2) %>%
    mutate(cum_sum = cumsum(en)) %>%
    ungroup() %>%
    mutate(y = as.integer(cum_sum > 0))
  
  
  dat <- left_join(dat, all_dat, by = "id2")
  
  
  res <- dat %>%
    group_by(sem) %>%
    do(lin_prob(., outcome = outcome, HC = "HC0"))
  
  return(res)
  
}


comm_res <- res_sector(col = "c", outcome = "Community")
univ_res <- res_sector(col = "u", outcome = "University")
ind_res <- res_sector(col = "i", outcome = "Independent")

comm_over <- extract_overall(comm_res, "Community")
univ_over <- extract_overall(univ_res, "University")
ind_over <- extract_overall(ind_res, "Independent")


enroll_sector_res <- bind_rows(comm_res, univ_res, ind_res)
enroll_sector_over <- 
  bind_rows(comm_over, univ_over, ind_over) %>%
  mutate(p = 2 * (1 - pnorm(abs(Difference)/SE)))

save(enroll_sector_res, enroll_sector_over, file = "derived files/2017_18_Cohort/cont/results_contemporaneous_enroll_sector.RData")
