source("project/functions/the_libraries.R")
library(purrr)
load("derived files/2017_18_Cohort/analytic_dem_course.RData")

# taking out the alt file
staar_files <- list.files(path = "NewFilesReleased/TAASandTAKSandSTAAR", pattern = "staareoca1", recursive = TRUE, full.names = TRUE)[c(6:11, 13:15)]


# Read in the staar files
read_staar <- function(path) {
  
  dat <- read_tsv(path) %>% 
    filter(ID2 %in% analytic_dem_course$id2) %>% 
    mutate(file = tools::file_path_sans_ext(basename(path))) %>%
    dplyr::select(ID2, ID1, GRADE, YEAR, A1_RAW, A1_SSC, A1_SCODE, A1_TESTVER, file)
  
  return(dat)
}


staar_dat <- staar_files %>%
  map_df(read_staar)


staar_files_new <- list.files(path = "NewFilesReleased/TAASandTAKSandSTAAR", pattern = "staareoca1", recursive = TRUE, full.names = TRUE)[c(17:20)]

read_staar_new <- function(path, sep) {
  
  dat <- read_delim(path, delim = sep) %>% 
    filter(ID2 %in% analytic_dem_course$id2) %>% 
    mutate(file = tools::file_path_sans_ext(basename(path))) %>%
    dplyr::select(ID2, ID1, GRADE, YEAR, A1_RAW, A1_SSC, A1_SCODE, A1_TESTVER, file)
  
  return(dat)
}


# check yr1 and yr2

yr1 <- read_staar_new(staar_files_new[1], sep = ",")
yr2 <- read_staar_new(staar_files_new[2], sep = "\t")
yr3 <- read_staar_new(staar_files_new[3], sep = "\t")
yr4 <- read_staar_new(staar_files_new[4], sep = "\t")



#dat <- read_tsv(staar_files[12])

#load("derived files/balance exercise/staar_dat_analytic.RData")

staar_dat_analytic <- bind_rows(staar_dat, yr1, yr2, yr3, yr4)



save(staar_dat_analytic, file = "derived files/2017_18_Cohort/staar_dat_analytic.RData")


