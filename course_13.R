source("project/functions/the_libraries.R")
library(tidyverse)
library(lubridate)

load("derived files/2017_18_Cohort/analytic_dat_track.RData")
source("CodeToArchive/tracking/track course enrollment/course_enroll_function.R")

# math classes
classes <- readRDS("derived files/track courses/classes.RData")

math_classes <- 
  classes %>%
  filter(str_detect(COURSE_NAME, "ALGEBRA_|GEOMETRY|CALCULUS|GRADE_7|GRADE_8")) %>%
  filter(!str_detect(COURSE_NAME, "MULTIVARIABLE|MODERN")) %>%
  filter(!(SERVICE %in% c("03100505", "03100507", "03100605", "03100607", "03100705", "03100707")))


# read in demographic data from a year ago and filter so that it contains students from analytic sample
year <- "13"
p_attend_demog_laf <- connect_data(path = paste0("NewFilesReleased/TEA/p_attend_demog", year, ".txt"), sep = "\t")

p_attend_demog <- subset_analytic(p_attend_demog_laf, analytic_dat_track)


dups <- p_attend_demog %>%
  filter(duplicated(id2)| duplicated(id2, fromLast = TRUE))


# read in the course data and filter it so that it contains students from analytic and are taking math classes
# delete people who have the entire row duplicated
p_course_laf <- connect_data(path = paste0("NewFilesReleased/TEA/p_course_complete", year, ".txt"), sep = "\t")


p_course <- subset_analytic(p_course_laf, analytic_dat_track) %>%
  filter(SERVICE %in% math_classes$SERVICE) %>%
  distinct(., .keep_all = TRUE)

p_course <- p_course %>%
  mutate(COURSE_RESULT = case_when(p_course$COURSE_RESULT == "1" ~ "pass",
                                   p_course$COURSE_RESULT == "2" ~ "fail",
                                   p_course$COURSE_RESULT == "3" ~ "incomplete")) 

gc()


# save 
# save 
saveRDS(p_attend_demog, paste0("derived files/2017_18_Cohort/p_attend_demog_", year, ".RData"))
saveRDS(p_course, paste0("derived files/2017_18_Cohort//p_course_", year, ".RData"))


# load
p_attend_demog <- readRDS(paste0("derived files/2017_18_Cohort/p_attend_demog_", year, ".RData"))
p_course <- readRDS(paste0("derived files/2017_18_Cohort/p_course_", year, ".RData"))

# Check Dups --------------------------------------------------------------------------------------------------------------------------------------

# check duplicated id
dups_1 <- p_course %>%
  group_by(SERVICE, COURSE_SEQ) %>%
  filter(duplicated(id2) | duplicated(id2, fromLast = TRUE)) %>%
  arrange(id2, SERVICE, CLASS_ID)

# there were students who took the same class during the same year/semester
# makes sense to keep the last record
# below- joining classes- so we have names of classes 
# below- uniting class-courseseq and by id2/class keeping the record with the max date


p_course_new <- left_join(p_course, classes, by = "SERVICE") %>%
  unite(class, c(COURSE_NAME, COURSE_SEQ)) %>%
  mutate(date = mdy(STUDENT_END_DATE)) %>%
  group_by(id2, class) %>%
  filter(date == max(date)) %>%
  ungroup() 

check <- p_course_new %>%
  dplyr::select(id2, STUDENT_END_DATE, date)


# check people who have the same end date for the two classes- which to keep??
dups_2 <- p_course_new %>%
  ungroup() %>%
  group_by(class) %>%
  filter(duplicated(id2) | duplicated(id2, fromLast = TRUE))

# people who have duplicated id by class and result. they took the same class and got the same result with the same end date
#just pick one arbitrarily
dups_3 <- p_course_new %>%
  ungroup() %>%
  group_by(class, COURSE_RESULT) %>%
  filter(duplicated(id2) | duplicated(id2, fromLast = TRUE))


# just keep one record arbitrarily
p_course_new <- p_course_new %>%
  ungroup() %>%
  group_by(class, COURSE_RESULT) %>%
  distinct(id2, .keep_all = TRUE)

# people who took the same class ended at the same time but got different results- drop the result that had fail

dups_4 <- p_course_new %>%
  ungroup() %>%
  group_by(class) %>%
  filter(duplicated(id2) | duplicated(id2, fromLast = TRUE))

# already deleted people with same results- this will identify people with different results
# remove if duplicated and the record is fail
p_course_clean <- p_course_new %>%
  ungroup() %>%
  group_by(id2) %>%
  mutate(dups = duplicated(class) | duplicated(class, fromLast = TRUE)) %>%
  filter(!(dups == TRUE & COURSE_RESULT == "fail")) # marks the duplicated and the original cases 


dups_5 <- p_course_clean %>%
  ungroup() %>%
  group_by(class) %>%
  filter(duplicated(id2) | duplicated(id2, fromLast = TRUE))


# course data ----------------------------------------------------------------------------------------------------------------------------------

# Spread 
p_course_sp <- p_course_clean %>%
  dplyr::select(id2, class, COURSE_RESULT) %>%
  spread(class, COURSE_RESULT)

sapply(p_course_sp, function(x) sum(is.na(x)))


# join the demographic data from the year and the course data 
# if they are missing in any of the courses indicate that they did not take the course with 0

(var_names <- names(p_course_sp)[-1])

demog_dat <- tibble(id2 = p_attend_demog[, "id2"]) %>%
  distinct(., .keep_all = TRUE)

demog_course <- left_join(demog_dat, p_course_sp, by = "id2") %>%
  mutate_at(var_names, funs(replace(., is.na(.), "did_not_take")))


# then left join analytic_dat and demog_course - if they are still missing then Missing

a_dat <- data_frame(id2 = analytic_dat_track[, "id2"])

course_dat_13 <- left_join(a_dat, demog_course, by = "id2") %>%
  mutate_at(var_names, funs(replace(., is.na(.), "missing"))) %>%
  mutate(year = year)

check_enrollment <- course_dat_13 %>%
  gather(course, status, -id2) %>%
  group_by(course, status) %>%
  summarize(n = n_distinct(id2)) %>%
  mutate(prop = n/sum(n))

save(course_dat_13, file =  paste0("derived files/2017_18_Cohort/course_tracking/tracked_data_courses/course_dat_", year, ".RData"))
