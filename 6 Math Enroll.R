library(purrr)
source("project/functions/the_libraries.R")
source("CodeToArchive/balance/functions/overall.R")

# cbm all data 
# cbm all data 
load("derived files/2017_18_Cohort/cont/cbm_all.RData")


# load the data with centered covariates
load("derived files/2017_18_Cohort/cont/cen_dat.RData")
load("derived files/2017_18_Cohort/cont/outcome_m_eq.RData")
load("derived files/2017_18_Cohort/cont/ss_dat.RData")

# Read cbm00s data --------------------------------------------------------


files <- list.files("NewFilesReleased/THECB", pattern = "cbm00s_fy(18_summ|19_|20_)", recursive = TRUE, full.names = TRUE)

# read in data, all in character format

read_dat <- function(path, join_dat = select(cen_dat, id2), n_max = Inf) {
  
  dat <- read_tsv(path, n_max = 1)
  delim <- if (ncol(dat) == 1) "," else "\t"
  
  cols <- ncol(read_delim(path, delim = delim, n_max = 1))
  col_types <- paste(rep("c", cols), collapse = "")
  
  dat <- 
    read_delim(path, delim = delim, col_types = col_types, n_max = n_max) %>%
    semi_join(join_dat, by = "id2")
  
  return(dat)
  
}

  
all_dat <- 
  tibble(file = files) %>%
  mutate(
    type = str_sub(file, 24, 24),
    dat = map(file, read_dat),
    file = str_sub(file, 24, -5),
  ) %>%
  unnest(cols = dat) %>% 
  mutate(
    ssSem = ifelse(ssSem == 4, 3, ssSem),
    Begin = str_sub(ssBegin, 1, 6),
    End = str_sub(ssEnd, 1, 6),
  ) %>%
  unite(sem, ssRYear, ssSem)

# Prefixes ---------------------------------------------------------

all_dat %>%
  group_by(file, sem) %>%
  count()

prefs <- c("MATH", "DSMA", "MATD", "DMAT", "TMTH", "DMTH", "MABR", "DEVL",
           "STAT", "MTH", "SMTE", "M", "SDS", "MAT", "STA")

clean_dat <- 
  all_dat %>%
  filter(ssPre %in% prefs) %>%
  filter(!(ssInstr %in% c("2", "5")), # take out lab and co-op, lab was also creating a lot of dups
         ssHSstat != "1",  # 1 is dual credit, 0 is not a hs student, 2 not a hs graduate course for college cred %>%
         ssSCH != "0")

save(clean_dat, file = "derived files/2017_18_Cohort/cont/clean_dat.RData")


load("derived files/2017_18_Cohort/cont/clean_dat.RData")


prefixes <- 
  clean_dat %>%
  dplyr::select(id2, type, ssPre) %>%
  left_join(cen_dat %>% dplyr::select(id2, D), by = "id2")



prefixes <- prefixes %>%
  filter(ssPre %in% prefs)
  

pref_count <- prefixes %>%
  group_by(ssPre) %>%
  summarize(n = n())


pref_count_trt <- prefixes %>%
  group_by(ssPre, D) %>%
  summarize(n = n())



# Create Dat ------------------------------------------------------------------


table(clean_dat$sem)

factors <- list(
  id2 = cen_dat$id2,
  sem = c("2018_3", "2018_1", "2019_2", "2019_3", "2019_1")
)

analysis_dat <- expand.grid(factors) 


math_dat <- 
  clean_dat %>%
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



dups <- 
  cbm00s_join %>%
  group_by(id2) %>%
  filter(duplicated(sem) | duplicated(sem, fromLast = TRUE)) %>% 
  ungroup()



# Just enrollment ---------------------------------------------------------


dat <- cbm00s_join %>%
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


# dat %>% dplyr::select(id2, type, ssFICE, ssPre, sem, en, cum_sum, y)

dat <- left_join(dat, cen_dat, by = "id2")

# dat %>% dplyr::select(id2, ssPre, ssNum, ssDev) %>% filter(!is.na(ssDev)) %>% View()


# Analysis ----------------------------------------------------------------

library(sandwich)
library(lmtest)
library(broom)



math_enroll_results <- 
  dat %>%
  group_by(sem) %>%
  do(lin_prob(., outcome = "Math Enroll", HC = "HC0"))

math_enroll_overall <- math_enroll_results %>%
  do(extract_overall(., outcome = "Math Enroll"))

save(math_enroll_results, math_enroll_overall, file = "derived files/2017_18_Cohort/cont/results_contemporaneous_math_enroll.RData")

