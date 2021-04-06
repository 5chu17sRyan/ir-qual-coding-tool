library(tidyverse)

test_survey <- read.csv("C:/Users/ryans/OneDrive/Desktop/Spring 2021/Senior Seminar/ir-qual-coding-tool/Test Data/durhamnc-2011-resident-survey/2011-resident-survey_1.csv", sep=";") %>%
  select(q2_1st_most, q7_1st_most, q9_1st_maintenance, q18_use_to_get_information,
           q20_kind_of_programs_like_more_on_dtv8, q25_like_best, q26_least)

write.csv(test_survey, "C:/Users/ryans/OneDrive/Desktop/Spring 2021/Senior Seminar/ir-qual-coding-tool/test_survey")