source("project/functions/the_libraries.R")
library(purrr)

# 2018+ college enrollment
files <- list.files("NewFilesReleased/THECB", pattern = "cbm001_fy(18_summ|19|20)", recursive = TRUE, full.names = TRUE)

# Community ---------------------------------------------------------------

c_cbm <- files[str_detect(files, "c_cbm")]
dlims <- c("\t","\t","\t",",",",",",")
identical(length(c_cbm), length(dlims))

# read_delim(c_cbm[1], delim = "\t", n_max = 100)

c_cbm_dat <- map2_dfr(c_cbm, dlims, read_delim, .id = "file")

c_cbm_dat <- 
  c_cbm_dat %>%
  mutate(type = "c") %>%
  mutate(non_degree_seeking = NA,
         sch = as.numeric(stutotsch) - as.numeric(stuschhs),
         dual_credit = as.numeric(stuschhs) > 0 & sch == 0)

c_cbm_clean <- 
  c_cbm_dat %>%
  filter(!dual_credit) %>%
  dplyr::select(file, id2, type, stufice, stusem, stuyear)

c_cbm_clean %>%
  group_by(file, stuyear, stusem) %>%
  count()

# Health ------------------------------------------------------------------

h_cbm <- files[str_detect(files, "h_cbm")]
dlims <- c("\t","\t","\t",",",",")
identical(length(h_cbm), length(dlims))

h_cbm_dat <- map2_df(h_cbm, dlims, 
                     ~ read_delim(.x, delim = .y) %>% 
                       mutate(stucip = as.character(stucip)), 
                     .id = "file")


h_cbm_dat <- 
  h_cbm_dat %>%
  mutate(type = "h") %>%
  mutate(non_degree_seeking = FALSE,
         sch = as.numeric(stutotsch),
         dual_credit = FALSE)

h_cbm_clean <- 
  h_cbm_dat %>%
  filter(!dual_credit, !non_degree_seeking) %>%
  dplyr::select(file, id2, type, stufice, stusem, stuyear)

h_cbm_clean %>%
  group_by(file, stuyear, stusem) %>%
  count()

# Independent Colleges ----------------------------------------------------

i_cbm <- files[str_detect(files, "i_cbm")]
dlims <-  c("\t","\t",",")
identical(length(i_cbm), length(dlims))

i_cbm_dat <- map2_df(i_cbm, dlims, read_delim, .id = "file")

i_cbm_dat <- 
  i_cbm_dat %>%
  mutate(type = "i") %>%
  mutate(non_degree_seeking = prvclass == "U",
         sch = as.numeric(prvsch),
         dual_credit = FALSE)


i_cbm_clean <- 
  i_cbm_dat %>%
  filter(!dual_credit, !non_degree_seeking) %>%
  dplyr::select(file, id2 = ID2, type, stufice = prvfice, stusem = prvsem, stuyear = prvyear)

i_cbm_clean %>%
  group_by(file, stuyear, stusem) %>%
  count()


# Public universities -----------------------------------------------------

u_cbm <- files[str_detect(files, "u_cbm")]
dlims <- c("\t","\t","\t",",",",")
identical(length(u_cbm), length(dlims))

u_cbm_dat <- map2_df(u_cbm, dlims, read_delim, .id = "file")

u_cbm_dat <- 
  u_cbm_dat %>%
  mutate(type = "u") %>%
  mutate(
    non_degree_seeking = stunondeg == 1,
    sch = as.numeric(stutotsch) - as.numeric(stuschhs),
    dual_credit = as.numeric(stuschhs) > 0 & sch == 0
  )

u_cbm_clean <- 
  u_cbm_dat %>%
  filter(!dual_credit, !non_degree_seeking) %>%
  dplyr::select(file, id2, type, stufice, stusem, stuyear)

u_cbm_clean %>%
  group_by(file, stuyear, stusem) %>%
  count()

# Combine across sectors -----------------------------------------------------

cbm_all <- bind_rows(c_cbm_clean, h_cbm_clean, i_cbm_clean, u_cbm_clean)

cbm_all %>%
  group_by(type, stuyear, stusem) %>%
  count() %>%
  spread(type, n)

cbm_all <- 
  cbm_all %>%
  mutate(stusem = ifelse(stusem == 4, 3, stusem)) %>%
  unite(sem, stuyear, stusem)

cbm_all %>%
  group_by(type, sem) %>%
  count() %>%
  spread(type, n)


save(cbm_all, file = "derived files/2017_18_Cohort/cont/cbm_all.RData")
