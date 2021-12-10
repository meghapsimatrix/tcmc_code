source("project/functions/the_libraries.R")
load("derived files/2017_18_Cohort/analytic_dem_course.RData")
load("derived files/2017_18_Cohort/staar_dat_analytic.RData")


# filter so that it's only A1 ScODE = S and A1 TESTVER = S
staar_clean <- staar_dat_analytic %>%
  filter(A1_SCODE == "S", A1_TESTVER == "S")



# look at duplicates
staar_clean %>%
  summarize(n_distinct(ID2))

dups <- staar_clean %>%
  filter(duplicated(ID2) | duplicated(ID2, fromLast = TRUE))




# look at small A1 scores
check <- staar_clean %>%
  filter(as.numeric(A1_SSC) < 1000)




staar_no_dups <- staar_clean %>%
 # filter(file != "staareoca1_alt2_apr16") %>%  # the scales are weird compared to the rest of the data 
  mutate(A1_SSC = as.numeric(A1_SSC)) %>%
  separate(file, into = c("file", "date")) %>%
  mutate(month = str_sub(date, 1, 3),
         yr = as.integer(paste0("20", str_sub(date, 4, 5))))

staar_no_dups <- staar_no_dups %>%
  mutate(mth = case_when(staar_no_dups$month == "dec" ~ 12,
                         staar_no_dups$month == "jul" ~ 7,
                         staar_no_dups$month == "may" ~ 5,
                         staar_no_dups$month == "jun" ~ 5,
                         staar_no_dups$month == "spr" ~ 3)) %>%
  mutate(y = paste(yr, mth, sep = "-")) %>%
  mutate(y = ymd(y, truncated = 1)) %>%
  group_by(ID2) %>%
  filter(y == min(y)) %>%
  ungroup()



dups_2 <- staar_no_dups %>%
  filter(duplicated(ID2) | duplicated(ID2, fromLast = TRUE))


staar_final <- staar_no_dups

save(staar_final, file = "derived files/2017_18_Cohort/staar_final.RData")


# join with analytic dat

analytic_final <- left_join(analytic_dem_course, staar_final, by= c("id2" = "ID2"))

analytic_final <- analytic_final %>%
  rename(ID1 = ID1.x, ID1_staar = ID1.y, GRADE = GRADE.x, GRADE_staar = GRADE.y)

save(analytic_final, file = "derived files/2017_18_Cohort/analytic_final.RData")


