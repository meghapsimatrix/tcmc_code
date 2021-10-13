library(tidyverse)

load("derived files/2017_18_Cohort/cont/results_contemporaneous_HS_grad.RData")
load("derived files/2017_18_Cohort/cont/results_contemporaneous_college_enroll.RData")
load("derived files/2017_18_Cohort/cont/results_contemporaneous_enroll_sector.RData")
load("derived files/2017_18_Cohort/cont/results_contemporaneous_enroll_partner.RData")
load("derived files/2017_18_Cohort/cont/results_contemporaneous_math_enroll.RData")
load("derived files/2017_18_Cohort/cont/results_contemporaneous_dev_enroll.RData")
load("derived files/2017_18_Cohort/cont/results_contemporaneous_math_pass.RData")
load("derived files/2017_18_Cohort/cont/results_contemporaneous_dev_pass.RData")


results_contemporaneous <- bind_rows(
  HS_grad_res,
  enroll_results,
  enroll_sector_res,
  partner_results,
  math_enroll_results,
  dev_enroll_results,
  math_pass_results,
  dev_pass_results
)

overall_contemporaneous <- bind_rows(
  HS_grad_overall,
  enroll_overall,
  enroll_sector_over,
  partner_overall,
  math_enroll_overall,
  dev_enroll_overall,
  math_pass_overall,
  dev_pass_overall
)

write_csv(overall_contemporaneous, 
          path = "Results for export/updated 2018 results/overall_effects_2017-18_contemporaneous.csv")

write_csv(results_contemporaneous, 
          path = "Results for export/updated 2018 results/school_effects_2017-18_contemporaneous.csv")
