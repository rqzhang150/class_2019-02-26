library(tidyverse)
library(gt)
library(readxl)
library(janitor)

spring_2018_original <- read_excel("class_enrollment_summary_by_term_03.06.18.xlsx", skip = 3)
spring_2018_clean <- spring_2018_original %>% 
  clean_names() %>% 
  filter(!is.na(course_name))

spring_2019_original <- read_excel("class_enrollment_summary_by_term_2.26.19.xlsx", skip = 3)
spring_2019_clean <- spring_2019_original %>% 
  clean_names() %>% 
  filter(!is.na(course_name))

spring_2018_clean %>% 
  left_join(spring_2019_clean, by = c("course_name"), 
            suffix = c("_2018", "_2019")) %>% 
  filter(course_department_2018 == course_department_2019,
         u_grad_2018 > 5,
         u_grad_2019 > 5) %>% 
  mutate(enrollment_change = u_grad_2019 - u_grad_2018) %>% 
  select(course_title_2019, course_name, u_grad_2018, u_grad_2019, enrollment_change) %>% 
  arrange(enrollment_change) %>% 
  slice(1:10) %>% 
  gt() %>% 
  tab_header(
    title = "Biggest Enrollment Decreases in Spring 2019"
  ) %>% 
  cols_label(
    course_title_2019 = "Number",
    course_name = "Name",
    u_grad_2018 = "2018",
    u_grad_2019 = "2019",
    enrollment_change = "Change"
  )

