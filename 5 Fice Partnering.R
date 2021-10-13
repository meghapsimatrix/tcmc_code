source("project/functions/the_libraries.R")
source("CodeToArchive/balance/functions/overall.R")


partner_dat <- read_csv("CodeToArchive/2017_18_Cohort/Contemporaneous/6_outcome_analysis/fice/fice_year2.csv")

partner_dat %>%
  filter(c_college == "San Jacinto Community College")

partner_dat <- 
  partner_dat %>%
  mutate(c_college = if_else(c_college == "San Jacinto Community College", "San Jacinto College", c_college))


# obtaining the fice codes
fice_dat <- read_csv("E:/master_data/ERC_THECB Documents/Fice_Codes/fice_instnames16.csv")

fice_dat %>%
  filter(str_detect(instname, "SAN JACINTO"))

fice_dat <- 
  fice_dat %>%
  mutate(instlegalname = if_else(str_detect(instlegalname, "^San Jacinto College"), "San Jacinto College", instlegalname))


f_dat <- left_join(partner_dat, fice_dat, by = c("c_college" = "instlegalname"))

f_dat %>%
  filter(is.na(instname))

aldine <- 
  fice_dat %>%
  filter(str_detect(instlegalname, "Lone Star")) %>%
  mutate(district = "ALDINE ISD") %>%
  dplyr::select(district, c_college = instlegalname, instfice:instname)


all_fice <- 
  bind_rows(f_dat, aldine) %>%
  filter(!is.na(instname))


save(all_fice, file = "derived files/balance 2019/cont/all_fice.RData")
