library(rvest)
library(purrr)
library(tidyverse)

ir_survey_schedule <- "https://www.kenyon.edu/offices-and-services/office-of-institutional-research/conducting-research-at-kenyon/survey-calendar/"

ir_surveys <- ir_survey_schedule %>% 
  read_html() %>% 
  html_nodes("table") 

student_surveys <- ir_surveys %>%
  purrr::pluck(1) %>%
  html_table() %>%
  filter(Dates != Title) %>%
  select(Title)

faculty_surveys <- ir_surveys %>%
  purrr::pluck(2) %>%
  html_table() %>%
  filter(Dates != Title) %>%
  select(Title)

alumni_surveys <- ir_surveys %>%
  purrr::pluck(3) %>%
  html_table() %>%
  filter(Dates != Title) %>%
  select(Title)

staff_surveys <- ir_surveys %>%
  purrr::pluck(4) %>%
  html_table(fill = T)
staff_surveys <- staff_surveys[,1:2] %>%
  filter(Dates != Title) %>%
  select(Title)
  
survey_frequency <- rbind(student_surveys, faculty_surveys, alumni_surveys, staff_surveys) %>%
  table() 
survey_frequency <- as.data.frame(survey_frequency) %>%
  arrange(desc(Freq))

head(survey_frequency)

