source("NewFilesReleased/Masking-functions.R")
source("project/functions/the_libraries.R")

load("derived files/2017_18_Cohort/cont/gbm_res_across.RData")
load("derived files/2017_18_Cohort/cont/cont_dum_dat.RData")
load("derived files/2017_18_Cohort/cont/cont_vars.RData")

cont_dum_dat <- final_dat_dc
analysis_dat <- cont_dum_dat
  

ss_cont <- cont_dum_dat %>% 
  group_by(D) %>% 
  summarize(n = n())


analysis_dat %>% 
  group_by(D) %>% 
  summarize(n = n())

#write_csv(ss_cont, "ss_cont.csv")

# take the means  ---------------------------------------------------------




means <- cont_dum_dat %>%
  group_by(D) %>%
  summarize_at(vars, funs(mean, sd)) 




m <- means %>%
  gather(var, type, -D)

m <- m %>%
  mutate(term = case_when(str_detect(m$var, "mean") & m$D == 0 ~ "Control M",
                          str_detect(m$var, "mean")& m$D == 1 ~ "Treatment M",
                          str_detect(m$var, "sd") & m$D == 0 ~ "Control SD",
                          str_detect(m$var, "sd") & m$D == 1 ~ "Treatment SD")) %>%
  mutate(type = round(type, 3))

final <- m %>%
  mutate(var = str_replace_all(var, "_mean", ""),
         var = str_replace_all(var, "_sd", "")) %>%
  dplyr::select(var, term, type) %>%
  spread(term, type)



control_n <- table(cont_dum_dat$D)[1]
trt_n <- table(cont_dum_dat$D)[2]

# classes -----------------------------------------------------------------

classes <- final_res %>% 
  mutate_at(vars(ALGEBRA_I_ALG_1_2_14:PRECALCULUS_PRE_CALC_2_17), funs(as.character)) %>%
  gather(course, res, ALGEBRA_I_ALG_1_2_14:PRECALCULUS_PRE_CALC_2_17) %>%
  mutate(course = factor(course),
         res = factor(res)) %>%
  group_by(course, D) %>%
  summarize(
    N = n(),
    res = pct_fct(res)
  ) %>%
  unnest(res) %>%
  ungroup() %>%
  gather(type, val, did_not_take:pass) %>%
  mutate(D = ifelse(D == 1, "Treatment M", "Control M")) %>%
  dplyr::select(-N) %>%
  spread(D, val) %>%
  mutate(course = as.character(course)) %>%
  unite("var", c(1, 2), sep = "") 

classes %>%
  View()



# categorical -----------------------------------------------------------

check <- c("A1_SSC_missing", rep("atrisk_enroll", 3),
           rep("atrisk_ever", 3), 
           rep("econ", 4), 
           rep("ethnic", 7),
           "gifted",
           rep("gifted_ever", 3),
           rep("immigrant_enroll", 3),
           rep("immigrant_ever", 3),
           "LEP",
           rep("SEX", 2),
           "speced",
           rep("speced_ever", 3), "track_missing")

(masked_dat <- 
  final %>%
  filter(str_detect(var, "economic|SEX|ETHNIC|LEP|atrisk|at_risk|speced|gifted|immigrant|A1_SSC_missing|track_missing")) %>%
  filter(!str_detect(var, "yrs")) %>%
  mutate(main_var = check) %>%
  group_by(main_var) %>%
  mutate(
    `Control M` = mask_pct(`Control M`, N = control_n),
    `Treatment M` = mask_pct(`Treatment M`, N = trt_n),
    `Control SD` = NA,
    `Treatment SD` = NA
  ))

# continuous -----------------------------------------------------------

cont <- final %>%
  filter(!(var %in% masked_dat$var)) %>%
  filter(!(var %in% classes$var))



cont_masked_dat <- masked_dat
cont_classes <- classes
cont_cont <- cont


save(cont_masked_dat, cont_classes, cont_cont, file = "derived files/2017_18_Cohort/cont/cont_msd.RData")


write_csv(cont_masked_dat, "Results for export/updated 2018 results/cont_masked_dat.csv")
write_csv(cont_classes, "Results for export/updated 2018 results/cont_classes.csv")
write_csv(cont_cont, "Results for export/updated 2018 results/cont_cont.csv")
